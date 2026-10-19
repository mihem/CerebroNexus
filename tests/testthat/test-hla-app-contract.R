hla_inst_file <- function(...) {
  installed <- system.file(..., package = "CerebroNexus")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }
  testthat::test_path("../../inst", ...)
}

## ---- HLA core resolution in every launch mode -------------------------- ##
## The core is authored only in R/hla_*.R. core_shim.R binds it into the module
## scope from whichever source the launch mode provides: the checkout's R/
## (repository launch), Cerebro.options$.hla_core (launchCerebro), or the
## generated viewer/hla_tcr_motifs/core/ (createShinyApp bundle). A function
## the module calls by bare name but the shim cannot bind surfaces only as
## "could not find function" in a running app, so pin each path here.

hla_shim_env <- function(options) {
  env <- new.env(parent = globalenv())
  env$Cerebro.options <- options
  sys.source(hla_inst_file("viewer/hla_tcr_motifs/core_shim.R"), env)
  env
}

## An app root holding only what createShinyApp() writes for the HLA core.
hla_bundle_root <- function(envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  core <- file.path(root, "viewer/hla_tcr_motifs/core")
  dir.create(core, recursive = TRUE)
  writeLines(.viewerHLACoreSource(), file.path(core, "hla_core.R"))
  root
}

test_that("the handed-over HLA core is every object R/hla_*.R defines", {
  r_dir <- testthat::test_path("../../R")
  testthat::skip_if_not(
    dir.exists(r_dir),
    "R/ source tree not present (installed-package layout)"
  )
  defined <- unlist(lapply(
    list.files(r_dir, pattern = "^hla_.*[.]R$", full.names = TRUE),
    function(file) {
      exprs <- parse(file, keep.source = FALSE)
      unlist(lapply(exprs, function(e) {
        if (
          is.call(e) &&
            (identical(e[[1L]], as.name("<-")) ||
              identical(e[[1L]], as.name("="))) &&
            is.name(e[[2L]])
        ) {
          as.character(e[[2L]])
        }
      }))
    }
  ))
  # .viewerHLACore() selects by the hla_ / HLA_ prefix, so a core object
  # without it would be missing from installed launches and bundles, and a
  # prefixed object defined elsewhere would be shipped without its file.
  expect_setequal(names(.viewerHLACore()), defined)
})

test_that("core shim binds locally without polluting globalenv", {
  global_names <- c("hla_detect_chains", "hla_descriptive_feature_overlap")
  for (nm in global_names) {
    if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = nm, envir = .GlobalEnv)
    }
  }

  local_env <- hla_shim_env(list(cerebro_root = hla_bundle_root()))

  for (nm in global_names) {
    expect_true(exists(nm, envir = local_env, inherits = FALSE))
    expect_false(exists(nm, envir = .GlobalEnv, inherits = FALSE))
  }
})

test_that("repository launch sources the checkout's R/ without the package", {
  repo_root <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)
  app_root <- file.path(repo_root, "inst")
  shim_path <- file.path(app_root, "viewer/hla_tcr_motifs/core_shim.R")
  # runApp("inst") sets cerebro_root to inst/, whose sibling R/ holds the core.
  # Run it in a --vanilla subprocess to prove no CerebroNexus install is
  # needed. Under R CMD check there is no source tree to launch from.
  testthat::skip_if_not(
    file.exists(shim_path) && dir.exists(file.path(repo_root, "R")),
    "source tree not present (installed-package layout)"
  )
  expression <- paste0(
    "e <- new.env(parent = globalenv()); ",
    "e$Cerebro.options <- list(cerebro_root = ",
    deparse(app_root),
    "); ",
    "sys.source(",
    deparse(shim_path),
    ", envir = e); ",
    "missing <- setdiff(",
    paste(deparse(names(.viewerHLACore())), collapse = ""),
    ", ls(e, all.names = TRUE)); ",
    "if (length(missing)) stop('unbound: ', toString(missing))"
  )
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(expression)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }

  expect_equal(status, 0L, info = paste(output, collapse = "\n"))
})

test_that("installed launch binds the namespace core launchCerebro hands over", {
  core <- .viewerHLACore()
  env <- hla_shim_env(list(
    cerebro_root = withr::local_tempdir(),
    .hla_core = core
  ))

  for (name in names(core)) {
    expect_identical(env[[name]], core[[name]], info = name)
  }
})

test_that("an exported app's generated core reproduces the namespace core", {
  root <- hla_bundle_root()
  source_lines <- readLines(file.path(
    root,
    "viewer/hla_tcr_motifs/core/hla_core.R"
  ))
  # The bundle runs without the package, so the generated source must not
  # reach for it.
  expect_false(any(grepl(
    "CerebroNexus::|getFromNamespace|requireNamespace|asNamespace|loadNamespace",
    source_lines
  )))

  env <- hla_shim_env(list(cerebro_root = root))
  core <- .viewerHLACore()
  for (name in names(core)) {
    expected <- core[[name]]
    actual <- env[[name]]
    if (is.function(expected)) {
      expect_identical(environment(actual), env, info = name)
      expected <- utils::removeSource(expected)
      actual <- utils::removeSource(actual)
    }
    expect_true(
      identical(actual, expected, ignore.environment = TRUE),
      info = paste(name, "changed when deparsed into the bundle")
    )
  }
})

test_that("core shim fails loudly when no HLA core is reachable", {
  root <- file.path(withr::local_tempdir(), "app")
  dir.create(root)
  expect_error(
    hla_shim_env(list(cerebro_root = root)),
    "HLA core not found"
  )
})

## ---- shipped demo contracts ------------------------------------------- ##
## One demo ships: real single cells, real paired TCR, real published donor
## genotypes. It makes claims the UI depends on, so if a rebuild drops one the
## page silently changes meaning -- donor-level counting reverts to
## sample-level, or the antigen-selection disclosure disappears while the
## carrier contrast remains on screen.

hla_sc_demo <- function() {
  path <- hla_inst_file("extdata/examples/demo_hla_tcr_dextramer.crb")
  testthat::skip_if_not(file.exists(path), "single-cell demo not built")
  readCerebro(path)
}

test_that("shipped demo declares antigen selection, cells and its receptor key", {
  ti <- hla_sc_demo()$technical_info
  # The repertoire was sorted for dextramer binding: not an unbiased sample of
  # the donors' repertoires, and the page prints so above the Associations.
  expect_equal(ti$tcr_selection, "antigen-selected")
  expect_true(nzchar(ti$tcr_selection_detail))
  expect_equal(ti$observation_unit, "cell")
  # This source identifies a receptor by V gene AND CDR3; CDR3-only nodes would
  # fuse receptors the source counts separately.
  expect_equal(ti$receptor_key, "v_gene+cdr3")
  # Declared, so class-based filtering never has to infer the lineage column.
  expect_equal(ti$lineage_column, "cell_type")
})

test_that("shipped demo HLA is genotyped and covers every sample", {
  crb <- hla_sc_demo()
  ht <- crb$getHLATyping()
  # Published (table S1), measured independently of these cells -- which is what
  # lets a carrier / non-carrier contrast mean anything here.
  expect_true(all(ht$source_type == "genotyped"))
  expect_setequal(unique(ht$sample), names(crb$getImmuneRepertoire()))
  # Sorted CD8+ T cells, so Class I only. The Class I x Class II pair scope is
  # gated on hla_pair_available() and therefore stays hidden on this demo.
  expect_setequal(unique(ht$locus), c("HLA-A", "HLA-B"))
})

test_that("shipped demo carries donor ids, so counting is donor-level", {
  ht <- hla_sc_demo()$getHLATyping()
  expect_false(any(is.na(ht$donor_id)))
  units <- hla_analysis_unit_map(ht, unique(ht$sample))
  expect_equal(unique(units$unit_type), "donor")
})

test_that("shipped demo yields a readable TRB motif network on real sequences", {
  # The whole argument for the page: a Hamming-1 CDR3 network is legible on an
  # ANTIGEN-SELECTED repertoire. An unselected one is sparse -- the real-sequence
  # predecessor gave 4 nodes in 2 motifs. Measured here: 157 nodes in 31 motifs,
  # largest 36. Assert well under those so a rebuild is not brittle, but far
  # above the near-empty scatter this replaced.
  crb <- hla_sc_demo()
  seg <- hla_parse_ir_segments(crb$getImmuneRepertoire(), "TRB")
  nodes <- hla_aggregate_cdr3_nodes(seg, by_v = TRUE)
  m <- hla_build_motif_groups(nodes, by_v = TRUE)$motif_df
  in_motif <- m[m$motif_size >= 2L, ]
  expect_gt(nrow(in_motif), 100L)
  expect_gte(length(unique(in_motif$motif_group)), 20L)
  expect_gte(max(in_motif$motif_size), 10L)
  # Isolated CDR3s must still dominate: a repertoire where everything clusters
  # would be its own kind of lie.
  expect_gt(nrow(m) - nrow(in_motif), nrow(in_motif))
})

test_that("shipped demo measures real genes for every cell", {
  crb <- hla_sc_demo()
  # Unlike the bulk cohort this replaced, these are sequenced cells: the matrix
  # must be non-empty and aligned to the metadata.
  expect_gt(nrow(crb$expression), 0L)
  expect_equal(ncol(crb$expression), nrow(crb$meta_data))
})

test_that("shipped demo keeps its expression block sparse", {
  # Normalized single-cell expression is ~90% zeros, and every other demo this
  # package ships is a dgCMatrix. A dense block here cost 184 MiB of session
  # memory and 4.5 MiB of installed package for nothing, so it is a size
  # regression worth failing on rather than a style preference.
  crb <- hla_sc_demo()
  expect_s4_class(crb$expression, "CsparseMatrix")
})

test_that("every shipped observation is paired alpha/beta", {
  # The vignette and NEWS both call this demo paired ab. combineTCR() writes
  # CTaa as "<alpha>_<beta>" and puts the literal string NA on a side it could
  # not resolve, so a non-empty CTaa is NOT evidence of pairing -- an earlier
  # build filtered on nzchar() alone and shipped 1,493 single-chain cells under
  # a "paired" label.
  ir <- hla_sc_demo()$getImmuneRepertoire()
  ctaa <- unlist(lapply(ir, function(x) x$CTaa), use.names = FALSE)
  parts <- strsplit(ifelse(is.na(ctaa), "", ctaa), "_", fixed = TRUE)
  paired <- vapply(
    parts,
    function(p) {
      length(p) == 2L && all(nzchar(p)) && !any(p %in% c("NA", "None"))
    },
    logical(1)
  )
  expect_true(all(paired))
})

test_that("shipped demo labels dextramer calls as reagent calls, not specificity", {
  # 10x's binarized flags are RAW BINDER CALLS. Naming them `antigen` /
  # `restricting_allele` asserts a validated peptide specificity and a presenting
  # allele that this data does not establish -- for most cells the bound
  # reagent's restriction is not even in the donor's published genotype.
  md <- hla_sc_demo()$getMetaData()
  expect_true(all(
    c(
      "dextramer_antigen",
      "dextramer_peptide",
      "dextramer_allele",
      "restriction_in_genotype"
    ) %in%
      colnames(md)
  ))
  # The over-claiming names must not come back.
  expect_false(any(c("antigen", "restricting_allele") %in% colnames(md)))
})

test_that("shipped demo exposes its cross-reactivity instead of describing it", {
  # restriction_in_genotype is the evidence column that keeps the binder calls
  # honest INSIDE the app: colour the projection by it and the noise is visible.
  # It is also declared as a group, so it reaches the network and its table.
  crb <- hla_sc_demo()
  md <- crb$getMetaData()
  expect_true(all(md$restriction_in_genotype %in% c("yes", "no", "unknown")))
  # If this ever came out clean, the calls would have stopped being raw 10x
  # calls and the documentation would need rewriting -- so assert the caveat.
  expect_gt(sum(md$restriction_in_genotype == "no"), 0L)
  expect_true("restriction_in_genotype" %in% crb$getGroups())
  # And the caveat travels with the object, not only in the vignette.
  expect_match(
    crb$technical_info$tcr_selection_detail,
    "RAW BINDER CALLS",
    fixed = TRUE
  )
})

test_that("an incompletely called locus is unknown, never a confirmed negative", {
  # Table S1 publishes ONE HLA-B allele for donors 1 and 2, so their second B
  # copy could be anything -- a B-restricted binder call there is undecidable,
  # not off-genotype. A two-state column would have to call it "no", inventing a
  # confirmed negative out of missing data. Same rule the carrier logic uses:
  # only a locus called at two copies can rule an allele out.
  crb <- hla_sc_demo()
  md <- crb$getMetaData()
  ht <- crb$getHLATyping()
  expect_gt(sum(md$restriction_in_genotype == "unknown"), 0L)

  for (i in seq_len(nrow(md))) {
    if (!identical(md$restriction_in_genotype[i], "no")) {
      next
    }
    locus <- hla_allele_locus(md$dextramer_allele[i])
    n_copies <- sum(ht$sample == md$sample[i] & ht$locus == locus)
    # every "no" must stand on a completely called locus
    expect_gte(n_copies, 2L)
  }
  # and every "unknown" must be a call the genotype genuinely cannot settle
  unknown <- md[md$restriction_in_genotype == "unknown", , drop = FALSE]
  if (nrow(unknown) > 0) {
    carried <- paste(ht$sample, ht$allele)
    expect_false(any(
      paste(unknown$sample, unknown$dextramer_allele) %in% carried
    ))
  }
})

test_that("the shipped demo's selection is a caveat the page can actually show", {
  crb <- hla_sc_demo()
  shiny::testServer(
    function(input, output, session) {
      data_set <- function() crb
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        environment(),
        names = c("HLA_SELECTION_CAVEATS", "hla_selection_caveat")
      )
    },
    {
      caveat <- hla_selection_caveat()
      expect_true(length(caveat) > 0)
      expect_match(
        paste(unlist(caveat), collapse = " "),
        "ascertainment",
        ignore.case = TRUE
      )
    }
  )
})

test_that("the shipped demo ships its CC-BY attribution beside the data", {
  # data-raw/DATASETS.md holds the provenance but is .Rbuildignore'd, so an
  # installed user would otherwise receive the CC-BY data with no licensing
  # record. The attribution file lives in extdata so it installs with the demo.
  att <- hla_inst_file("extdata/examples/demo_hla_tcr_dextramer.ATTRIBUTION.md")
  expect_true(file.exists(att))
  txt <- viewer_test_text(att)
  expect_match(txt, "CC-BY", fixed = TRUE)
  expect_match(txt, "abf5835", fixed = TRUE) # the source paper
})

test_that("the network caption states area encoding and the cap", {
  shiny::testServer(
    function(input, output, session) {
      hla_require_active_page <- function() invisible(NULL)
      hla_has_deps <- function() TRUE
      hla_segments <- function() data.frame(cdr3 = c("AAA", "AAB"))
      hla_motif_graph <- function() igraph::make_ring(2)
      hla_unit_noun <- function() "cell"
      hla_param <- function(name, default) default
      hla_motif_n_clusters <- function() 1L
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "visualizations.R"),
        environment(),
        names = c("HLA_MOTIF_MAX_LEGEND_CLUSTERS", "output$hla_motif_note")
      )
    },
    {
      text <- output$hla_motif_note$html
      expect_match(text, "Node AREA", fixed = TRUE)
      expect_match(text, as.character(HLA_NODE_MAX_EXACT), fixed = TRUE)
      expect_match(text, "same size", fixed = TRUE)
    }
  )
})

test_that("the page's nav gate scans every sample, like the core does", {
  # The IR module's detect_chains() stops after three samples. The HLA page is
  # gated on chains being present, and it is also the only route to its own
  # Data & QC tab -- so a cohort whose TCR happens to start at sample four would
  # be locked out of a page that could analyse it.
  late <- list(
    s1 = data.frame(CTgene = NA_character_, stringsAsFactors = FALSE),
    s2 = data.frame(CTgene = NA_character_, stringsAsFactors = FALSE),
    s3 = data.frame(CTgene = NA_character_, stringsAsFactors = FALSE),
    s4 = data.frame(
      CTgene = "TRAV1-2.TRAJ33.TRAC_TRBV19.TRBD1.TRBJ2-7.TRBC2",
      stringsAsFactors = FALSE
    )
  )
  expect_setequal(hla_detect_chains(late), c("TRA", "TRB"))

  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  expect_true(scope$viewerHasTcrRepertoire(late))
})

test_that("every scrolling table keeps its cells on one line", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      typing <- hla_normalize_typing(
        data.frame(
          sample = c("s1", "s2"),
          locus = "HLA-A",
          allele = c("HLA-A*02:01", "HLA-A*01:01"),
          donor_id = c("d1", "d2")
        ),
        source_type = "genotyped"
      )
      hla_active_typing <- function() typing
      hla_has_typing <- function() TRUE
      hla_ir_samples <- function() c("s1", "s2")
      hla_unit_noun <- function() "cell"
      hla_association_consumer_ready <- function() FALSE
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data_qc.R"),
        scope,
        names = c(
          "output$hla_coverage_table",
          "output$hla_normalized_preview",
          "output$hla_donor_mapping_preview",
          "output$hla_download_normalized"
        )
      )
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "associations.R"),
        scope,
        names = c(
          "hla_overlap_live_table",
          "hla_overlap_summary_skeleton",
          "hla_overlap_breadth_skeleton",
          "hla_overlap_summary_data",
          "hla_overlap_breadth_data",
          "hla_overlap_summary_proxy",
          "hla_overlap_table_proxy",
          "output$hla_allele_matrix"
        )
      )
    },
    {
      for (id in c(
        "hla_coverage_table",
        "hla_normalized_preview",
        "hla_donor_mapping_preview",
        "hla_overlap_summary",
        "hla_overlap_table",
        "hla_allele_matrix"
      )) {
        table <- jsonlite::fromJSON(output[[id]])$x
        expect_true(table$options$scrollX, info = id)
        expect_match(table$container, "nowrap", fixed = TRUE, info = id)
      }
      downloaded <- read.csv(
        output$hla_download_normalized,
        check.names = FALSE,
        na.strings = ""
      )
      expect_equal(downloaded$allele, scope$typing$allele)
      expect_equal(downloaded$donor_id, c("d1", "d2"))
    }
  )
})

test_that("Viewer Pack first frames bypass cold HLA preparation", {
  withr::local_package("magrittr")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      available_crb_files <- list(selected = "tiny")
      data_set <- function() TRUE
      hla_active_chain <- function() "TRB"
      first <- list(
        filter_levels = list(sample = "s1"),
        filter_groups = "sample",
        initial_samples = "s1",
        by_v = FALSE,
        node_meta_cols = "sample",
        lineage_col = NULL,
        available_cols = "sample",
        segments = data.frame(sample = "s1", cdr3 = "AAA"),
        graph_raw = igraph::make_ring(3)
      )
      viewerPackCurrent <- function() "packed"
      viewerPackHlaFirstFrame <- function(pack, chain) {
        stopifnot(identical(pack, "packed"), identical(chain, "TRB"))
        if (isTRUE(input$corrupt)) NULL else first
      }
      viewerPackHlaSegments <- function(pack, chain) first$segments
      hla_ir_annotated <- function() stop("cold metadata hydration")
      hla_unfiltered_segments <- function() stop("cold segment parsing")
      hla_segments_fallback <- function() {
        data.frame(sample = "s2", cdr3 = "BBB")
      }
      hla_node_meta_cols <- function() "sample"
      hla_celltype_col <- function() NA_character_
      hla_default_min_nodes <- function() 2L
      hla_params_ready <- function() TRUE
      hla_motif_graph_cached <- function() igraph::make_empty_graph()
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        scope,
        names = c(
          "hla_bindCache",
          "hla_param",
          "hla_first_frame",
          "hla_packed_segments",
          "hla_filter_groups",
          "hla_filter_levels",
          "hla_initial_samples",
          "hla_default_filter_selections",
          "hla_filter_input_id",
          "hla_filter_selections",
          "hla_first_frame_filters_match",
          "hla_by_v_default",
          "hla_segments",
          "hla_first_frame_graph_matches",
          "hla_motif_graph"
        )
      )
    },
    {
      expect_equal(hla_filter_levels(), list(sample = "s1"))
      expect_equal(hla_packed_segments(), scope$first$segments)
      expect_equal(hla_segments(), scope$first$segments)
      expect_equal(igraph::vcount(hla_motif_graph()), 3L)
      session$setInputs(hla_group_filter_TRB_sample = "s2")
      expect_equal(hla_segments()$cdr3, "BBB")
      expect_equal(igraph::vcount(hla_motif_graph()), 0L)
    }
  )
})
