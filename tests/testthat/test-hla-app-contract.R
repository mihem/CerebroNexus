hla_inst_file <- function(...) {
  installed <- system.file(..., package = "CerebroNexus")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }
  testthat::test_path("../../inst", ...)
}

# The body of hla_params_ready(), so the gate's contract can be asserted against
# the gate itself rather than against a window of characters that happens to
# follow its name. Lazy up to the first line-start "})".
hla_params_ready_src <- function(data_src) {
  m <- regmatches(
    data_src,
    regexpr(
      "hla_params_ready <- reactive\\(\\{[\\s\\S]{0,1500}?\\n\\}\\)",
      data_src,
      perl = TRUE
    )
  )
  testthat::expect_length(m, 1L)
  m
}

test_that("HLA Associations is wired to a frozen motif feature", {
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/associations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(src, "hla_feature_type")
  expect_match(src, "hla_feature_id")
  expect_match(src, "hla_descriptive_feature_overlap", fixed = TRUE)
  expect_match(src, "hla_overlap_table")
  expect_match(src, "hla_allele_matrix")
})

test_that("Associations takes its features from the allele-independent graph", {
  # Sourcing the drawn graph here would let the allele scope nominate the very
  # motif the allele is then compared on. Guarded statically because the wiring
  # is a Shiny reactive: the unit suite cannot see which graph it reads.
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/associations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    src,
    "hla_feature_catalog <- reactive\\(\\{[\\s\\S]{0,120}hla_global_motif_graph\\(\\)",
    perl = TRUE
  )
  expect_no_match(
    src,
    "hla_feature_catalog <- reactive\\(\\{[\\s\\S]{0,120}g <- hla_motif_graph\\(\\)",
    perl = TRUE
  )
})

test_that("the allele scope cache key carries the carrier set, not just a name", {
  # Two typings can name the same allele and disagree on who carries it. If the
  # key is the allele name alone, the second upload serves the graph cached from
  # the first one's carriers while every caption around it updates.
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    src,
    "hla_scope_key <- reactive\\(\\{[\\s\\S]{0,600}hla_carriers_of\\(",
    perl = TRUE
  )
})

test_that("Data and QC exposes normalized and donor mapping previews", {
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/data_qc.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(src, "hla_normalized_preview")
  expect_match(src, "hla_donor_mapping_preview")
  expect_match(src, "hla_download_normalized")
})

test_that("motif network exposes a stable selected-node detail panel", {
  ui <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/UI.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  viz <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(ui, "hla_node_details")
  expect_match(viz, "hla_selected_node_id")
  expect_match(viz, "visEvents")
})

test_that("core shim binds locally without polluting globalenv", {
  local_env <- new.env(parent = globalenv())
  # The shim reads Cerebro.options[["cerebro_root"]] to locate its bundled core/
  # directory; supply it so the source path resolves in this isolated env.
  local_env$Cerebro.options <- list(cerebro_root = hla_inst_file())
  global_names <- c("hla_detect_chains", "hla_descriptive_feature_overlap")
  for (nm in global_names) {
    if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = nm, envir = .GlobalEnv)
    }
  }

  source(
    hla_inst_file("shiny/v1.4/hla_tcr_motifs/core_shim.R"),
    local = local_env
  )

  expect_true(exists("hla_detect_chains", envir = local_env, inherits = FALSE))
  expect_true(exists(
    "hla_descriptive_feature_overlap",
    envir = local_env,
    inherits = FALSE
  ))
  expect_false(exists(
    "hla_detect_chains",
    envir = .GlobalEnv,
    inherits = FALSE
  ))
  expect_false(exists(
    "hla_descriptive_feature_overlap",
    envir = .GlobalEnv,
    inherits = FALSE
  ))
})

test_that("bundled core shim resolves without an installed package", {
  repo_root <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)
  app_root <- file.path(repo_root, "inst")
  shim_path <- file.path(
    app_root,
    "shiny/v1.4/hla_tcr_motifs/core_shim.R"
  )
  # The shim sources its bundled core/ copies with no package on the search
  # path -- the exact condition of a createShinyApp bundle. Run it in a --vanilla
  # subprocess to prove no CerebroNexus install is needed. Under R CMD check
  # the source `inst/` tree is not at this path, so there is nothing to test --
  # skip rather than fail on the missing file.
  testthat::skip_if_not(
    file.exists(shim_path),
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
    "stopifnot(",
    "exists('hla_distinct_colors', envir = e, inherits = FALSE), ",
    "exists('hla_descriptive_feature_overlap', envir = e, inherits = FALSE)",
    ")"
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

## ---- shipped demo contracts ------------------------------------------- ##
## One demo ships: real single cells, real paired TCR, real published donor
## genotypes. It makes claims the UI depends on, so if a rebuild drops one the
## page silently changes meaning -- donor-level counting reverts to
## sample-level, or the antigen-selection disclosure disappears while the
## carrier contrast remains on screen.

hla_sc_demo <- function() {
  path <- hla_inst_file("extdata/v1.4/demo_hla_tcr_dextramer.crb")
  testthat::skip_if_not(file.exists(path), "single-cell demo not built")
  readRDS(path)
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
  # The caveat above the Associations tables is keyed on technical_info$
  # tcr_selection. This demo declares "antigen-selected", which was NOT a
  # recognised key -- so the object declared a selection and the page rendered
  # nothing, while the vignette told users to read a note that did not exist.
  crb <- hla_sc_demo()
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  key <- crb$technical_info$tcr_selection
  expect_match(
    src,
    paste0(
      "HLA_SELECTION_CAVEATS <- list\\([\\s\\S]{0,4000}\"",
      key,
      "\" = list\\("
    ),
    perl = TRUE
  )
  # Independent genotypes remove circularity, not ascertainment -- the wording
  # has to keep those apart rather than declaring the contrast clean.
  expect_match(crb$technical_info$tcr_selection_detail, "ASCERTAINMENT")
})

test_that("the shipped demo ships its CC-BY attribution beside the data", {
  # data-raw/DATASETS.md holds the provenance but is .Rbuildignore'd, so an
  # installed user would otherwise receive the CC-BY data with no licensing
  # record. The attribution file lives in extdata so it installs with the demo.
  att <- hla_inst_file("extdata/v1.4/demo_hla_tcr_dextramer.ATTRIBUTION.md")
  expect_true(file.exists(att))
  txt <- paste(readLines(att, warn = FALSE), collapse = "\n")
  expect_match(txt, "CC-BY", fixed = TRUE)
  expect_match(txt, "abf5835", fixed = TRUE) # the source paper
})

## ---- node colours must not be handed to vis-network's group palette --- ##

test_that("motif network nodes carry no group column", {
  # vis-network auto-registers unknown groups and paints them from its own
  # default palette, overriding the per-node colour: this silently rendered 246
  # of 430 nodes in vis defaults (a "Mixed" node drawn #FFFF00) while the data
  # said otherwise. Nothing consumes the column - the legend is built by hand
  # with useGroups = FALSE - so it must stay out unless visGroups() registers
  # every level.
  viz <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  node_df <- regmatches(
    viz,
    regexpr(
      "nodes <- data\\.frame\\([\\s\\S]{0,400}?\\n  \\)",
      viz,
      perl = TRUE
    )
  )
  expect_length(node_df, 1L)
  expect_no_match(node_df, "\\bgroup\\s*=", perl = TRUE)
  expect_match(node_df, "color\\s*=\\s*node_color", perl = TRUE)
  # If group ever comes back, it must come with an explicit registration.
  if (grepl("group\\s*=\\s*group_raw", viz, perl = TRUE)) {
    expect_match(viz, "visGroups", perl = TRUE)
  }
})

## ---- core_shim sources every core file, byte-for-byte ------------------ ##
## The shim sys.source()s a hardcoded list of the bundled core/ files -- no
## namespace fallback, so the bundle never names CerebroNexus. A core file
## present in R/ but missing from that list is never sourced, so its functions
## surface as "could not find function" in a running app while unit tests (which
## reach R/ directly) stay green. Pin the file list here, and pin the bundled
## copies byte-for-byte against R/ in the next test -- together they guarantee
## every R/hla_*.R function is reachable at runtime, in every launch mode.

test_that("core_shim sources every R/hla_*.R core file", {
  shim <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/core_shim.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  core_files <- basename(list.files(
    testthat::test_path("../../R"),
    pattern = "^hla_.*[.]R$"
  ))
  # Doc-only anchors carry no runtime symbols; everything else must be sourced.
  core_files <- setdiff(core_files, character(0))
  missing <- core_files[
    !vapply(
      core_files,
      function(f) grepl(paste0('"', f, '"'), shim, fixed = TRUE),
      logical(1)
    )
  ]
  expect_equal(missing, character(0))
})

test_that("bundled HLA core/ is byte-identical to the R/ source", {
  r_dir <- testthat::test_path("../../R")
  core_dir <- hla_inst_file("shiny/v1.4/hla_tcr_motifs/core")
  # The shim sources these copies at runtime; they exist only so the module is
  # self-contained in a createShinyApp bundle (no R/, no package). If they drift
  # from R/, the bundle silently runs stale core code. This guard needs the
  # package R/ source tree, absent under the installed-package test layout --
  # skip there rather than fail.
  testthat::skip_if_not(
    dir.exists(r_dir) &&
      length(list.files(r_dir, pattern = "^hla_.*[.]R$")) > 0,
    "R/ source tree not present (installed-package layout)"
  )
  core_files <- c(
    "hla_typing.R",
    "hla_motif_core.R",
    "hla_association_core.R",
    "hla_visual_helpers.R",
    "hla_export.R"
  )
  for (f in core_files) {
    inst_copy <- file.path(core_dir, f)
    r_src <- file.path(r_dir, f)
    expect_true(
      file.exists(inst_copy),
      info = paste("missing bundled core copy:", f)
    )
    expect_identical(
      readLines(inst_copy, warn = FALSE),
      readLines(r_src, warn = FALSE),
      info = paste0(
        "inst/shiny/v1.4/hla_tcr_motifs/core/",
        f,
        " drifted from R/",
        f,
        " -- re-copy R/hla_*.R into the module's core/ directory."
      )
    )
  }
})

## ---- illustrated guide ------------------------------------------------- ##

test_that("the panel info button is wired to a sourced guide", {
  # The trap that produced the export 500 last time: a new module file that the
  # server never sources is invisible to unit tests and only fails in a running
  # app. Pin button -> handler -> source.
  ui <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/UI.R"), warn = FALSE),
    collapse = "\n"
  )
  server <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  guide <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/help_guide.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(ui, "cerebroInfoButton\\(\"hla_visualizations_info\"\\)")
  expect_match(server, "help_guide\\.R", perl = TRUE)
  expect_match(guide, "observeEvent\\(input\\$hla_visualizations_info")
})

test_that("the guide covers every tab the page actually shows", {
  # A page tab with no guide entry is how a guide rots: the tab ships, the guide
  # silently does not mention it.
  ui <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/UI.R"), warn = FALSE),
    collapse = "\n"
  )
  guide <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/help_guide.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  page_tabs <- c("Motif Network", "HLA Associations", "Data & QC")
  for (tab in page_tabs) {
    expect_match(ui, sprintf('tabPanel\\(\n?\\s*"%s"', tab), perl = TRUE)
  }
  # The guide names them (case-insensitively: its rail says "Motif network").
  # The guide tab that explains a page tab carries that tab's exact name.
  expect_match(guide, "Motif Network", fixed = TRUE)
  expect_match(guide, "HLA Associations", fixed = TRUE)
  expect_match(guide, "Data & QC", fixed = TRUE)
})

test_that("the guide states the page's evidence ceiling", {
  # This page's whole framing is co-occurrence, not restriction. If the guide
  # ever loses that, it starts teaching the opposite of the UI's own subtitle.
  guide <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/help_guide.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(guide, "never confirmed restriction", ignore.case = TRUE)
  # The six-class-I-allele ambiguity is the structural reason the ceiling
  # exists. Matched loosely across markup: the claim is what must survive, not
  # one phrasing of it. (An exact-string version of this broke the moment the
  # prose gained inline emphasis, which is a rewrite, not a regression.)
  expect_match(guide, "six[\\s\\S]{0,80}class I", perl = TRUE)
  # The two orthogonal "Mixed" axes are the page's most confusable thing.
  expect_match(guide, "Orthogonal axes", ignore.case = TRUE)
})

test_that("guide schematics are self-contained inline SVG", {
  # A strict-CSP page and an offline .crb viewer both break on external assets.
  guide <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/help_guide.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(guide, "<svg viewBox=", fixed = TRUE)
  expect_no_match(guide, "<img ", fixed = TRUE)
  # The SVG xmlns is a namespace IDENTIFIER, never fetched, so it does not count
  # as an external asset; strip it before looking for real remote references.
  external <- gsub("http://www.w3.org/2000/svg", "", guide, fixed = TRUE)
  expect_no_match(external, "https?://", perl = TRUE)
})

## ---- node size encoding ------------------------------------------------ ##

test_that("the renderer sets node size itself, never via vis `value`", {
  # vis-network maps `value` linearly onto the RADIUS, so a node table carrying
  # `value = clone_count` renders area as count^2 (measured: counts 1..6 at
  # radii 8..40, a 25x area spread for 6x the cells). The radius must come from
  # hla_node_radius() instead, and `scaling` must not reappear alongside it.
  viz <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  node_df <- regmatches(
    viz,
    regexpr(
      "nodes <- data\\.frame\\([\\s\\S]{0,500}?\\n  \\)",
      viz,
      perl = TRUE
    )
  )
  expect_length(node_df, 1L)
  # The display-only multiplier rides along; the radius is still set from the
  # clone count through hla_node_radius(), never from a `value` scaling.
  expect_match(
    node_df,
    "size = hla_node_radius\\(clone_count, node_scale\\)",
    perl = TRUE
  )
  expect_no_match(node_df, "\\bvalue\\s*=", perl = TRUE)
  expect_no_match(viz, "scaling = list\\(min", perl = TRUE)
})

test_that("the network caption states area encoding and the cap", {
  # A caption reading "node size = number of cells" invites the reader to
  # compare areas proportionally past the cap, where that is false.
  viz <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(viz, "Node AREA", fixed = TRUE)
  expect_match(viz, "HLA_NODE_MAX_EXACT", fixed = TRUE)
})

## ---- the guide must teach the colours the app actually draws ----------- ##

test_that("guide palette constants match the renderer's scales", {
  # The guide invented its own hues for the MHC-context axis once, and so taught
  # colours the app never draws. A schematic that disagrees with the plot is
  # worse than no schematic: the reader trusts it.
  read_src <- function(f) {
    paste(readLines(hla_inst_file(f), warn = FALSE), collapse = "\n")
  }
  viz <- read_src("shiny/v1.4/hla_tcr_motifs/visualizations.R")
  guide <- read_src("shiny/v1.4/hla_tcr_motifs/help_guide.R")

  hex <- function(src, name) {
    m <- regmatches(
      src,
      regexpr(
        sprintf("\"%s\"\\s*=\\s*\"(#[0-9a-fA-F]{6})\"", name),
        src,
        perl = TRUE
      )
    )
    sub(".*\"(#[0-9a-fA-F]{6})\".*", "\\1", m)
  }
  guide_hex <- function(name) {
    m <- regmatches(
      guide,
      regexpr(sprintf("%s <- \"(#[0-9a-fA-F]{6})\"", name), guide, perl = TRUE)
    )
    sub(".*\"(#[0-9a-fA-F]{6})\".*", "\\1", m)
  }

  expect_equal(guide_hex("HLA_GUIDE_CARRIER"), hex(viz, "Carrier"))
  expect_equal(guide_hex("HLA_GUIDE_NONCARRIER"), hex(viz, "Non-carrier"))
  expect_equal(guide_hex("HLA_GUIDE_CLASS_I"), hex(viz, "Class I"))
  expect_equal(guide_hex("HLA_GUIDE_CLASS_II"), hex(viz, "Class II"))

  # The sample-origin hues are the renderer's own first three, and the guide
  # drew RColorBrewer Set2 until this was pinned.
  block <- regmatches(
    guide,
    regexpr("HLA_GUIDE_SAMPLE <- c\\([^)]*\\)", guide, perl = TRUE)
  )
  sample_hues <- unlist(regmatches(block, gregexpr("#[0-9a-fA-F]{6}", block)))
  expect_equal(sample_hues, unname(hla_distinct_colors(c("a", "b", "c"))))
})

test_that("the guide draws no unnamed colour", {
  # Every hue in a schematic must trace to a named constant, so a scale cannot
  # drift away from the renderer one raw hex at a time. Greys and tints are
  # chrome (backgrounds, rules, arrows), not data levels.
  guide <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/help_guide.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  body <- substring(guide, regexpr("hla_guide_svg_mismatch", guide))
  drawn <- unlist(regmatches(
    body,
    gregexpr("(fill|stroke)='#[0-9a-fA-F]{6}'", body)
  ))
  drawn <- toupper(sub(".*'(#[0-9a-fA-F]{6})'.*", "\\1", drawn))
  chrome <- toupper(c(
    "#ececec",
    "#e2e2e2",
    "#fdeae0",
    "#f0cdb8",
    "#e0a58a",
    "#c2410c",
    "#f4f4f5",
    "#fff8ec",
    "#fafafa",
    "#cfcfcf",
    "#ddd",
    "#fff"
  ))
  expect_equal(setdiff(drawn, chrome), character(0))
})

test_that("the carrier and MHC-context scales share no hue but grey", {
  # They are orthogonal axes. Look-alike colours across them invite the reader
  # to connect a carrier to a CD8 cell, which is the one inference this page
  # must not suggest. The no-information grey is the exception: it says the same
  # thing on both.
  viz <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  grab <- function(block) {
    b <- regmatches(
      viz,
      regexpr(sprintf("%s <- c\\([^)]*\\)", block), viz, perl = TRUE)
    )
    unlist(regmatches(b, gregexpr("#[0-9a-fA-F]{6}", b)))
  }
  carrier <- grab("HLA_CARRIER_COLORS")
  context <- grab("HLA_CONTEXT_COLORS")
  expect_length(carrier, 4L)
  expect_length(context, 4L)
  shared <- intersect(tolower(carrier), tolower(context))
  expect_equal(shared, "#b8bcc4") # the neutral grey, and nothing else
})

test_that("MHC context is a fixed scale, not a data-order palette", {
  # It used to fall through to hla_distinct_colors(), which assigns colours in
  # whatever order levels happen to appear among the nodes: Class I could be
  # blue on one data set and red on the next.
  viz <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(viz, "HLA_CONTEXT_LEVELS", fixed = TRUE)
  expect_match(
    viz,
    "intersect(HLA_CONTEXT_LEVELS, unique(group_raw))",
    fixed = TRUE
  )
  expect_match(viz, "HLA_CONTEXT_COLORS[levels_ord]", fixed = TRUE)
})

test_that("the page's nav gate scans every sample, like the core does", {
  # The IR module's detect_chains() stops after three samples. The HLA page is
  # gated on chains being present, and it is also the only route to its own
  # Data & QC tab -- so a cohort whose TCR happens to start at sample four would
  # be locked out of a page that could analyse it. The core already scans all
  # samples; the gate must use the core.
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

  src <- paste(
    readLines(hla_inst_file("shiny/v1.4/shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    src,
    "\"hla_tcr_motifs\",[\\s\\S]{0,1500}hla_detect_chains\\(getImmuneRepertoire\\(\\)\\)",
    perl = TRUE
  )
})

test_that("node colouring is offered from the declared groupings", {
  # The colour list used to be inferred: every metadata column that happened to
  # be a string with >1 value. That offered whatever the upstream pipeline left
  # behind (orig.ident, RNA_snn_res.0.6) as though it were biology, and let one
  # data set advertise different groupings on two pages. getGroups() is the
  # object's own answer and the Groups page's source; this page must not hold a
  # second opinion.
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    src,
    "hla_color_meta_cols <- reactive\\(\\{[\\s\\S]{0,300}getGroups\\(\\)",
    perl = TRUE
  )
  # A data set may DECLARE any column as its lineage column, so the declared
  # check still reads from all available columns. But INFERENCE is limited to
  # the declared groupings: scoring every column let an identifier value like
  # "CD8_case" be taken for a lineage and silently reshape HLA scope filtering.
  expect_match(
    src,
    "hla_celltype_col <- reactive\\(\\{[\\s\\S]{0,60}hla_available_cols\\(\\)",
    perl = TRUE
  )
  expect_match(
    src,
    "hla_celltype_col <- reactive\\(\\{[\\s\\S]{0,1400}candidates <- hla_color_meta_cols\\(\\)",
    perl = TRUE
  )
})

test_that("the lineage column is found by its labels, not by its name", {
  # A general-purpose viewer cannot assume the annotation is called cell_type /
  # cell_type_fine: it may be `annotation`, `azimuth_l2`, `predicted.id`. The
  # labels are what carry the lineage, and hla_lineage_column_score() reads
  # those -- it is hla_lineage_context() plus the exclusion of labels that name
  # an experimental condition ("anti-CD4"), which is unit-tested in the core.
  src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    src,
    "hla_celltype_col <- reactive\\(\\{[\\s\\S]{0,1600}hla_lineage_column_score\\(",
    perl = TRUE
  )
  # ...and a candidate must clear a real share, so one stray "CD4" cannot win.
  expect_match(
    src,
    "hla_celltype_col <- reactive\\(\\{[\\s\\S]{0,1800}HLA_LINEAGE_MIN_SHARE",
    perl = TRUE
  )
  # A data set may still declare it outright, like observation_unit does.
  expect_match(
    src,
    "hla_celltype_col <- reactive\\(\\{[\\s\\S]{0,900}lineage_column",
    perl = TRUE
  )
  expect_no_match(
    src,
    "hla_celltype_col <- reactive\\(\\{[\\s\\S]{0,400}\"cell_type_fine\" %in% cols",
    perl = TRUE
  )
})

test_that("every scrolling table keeps its cells on one line", {
  # A table that scrolls sideways must not ALSO wrap: the column gets squeezed
  # and an identifier breaks mid-token ("HLA-" / "A"), which reads as two values.
  # DataTables ships the `nowrap` class for exactly this pairing.
  for (f in c("associations.R", "data_qc.R")) {
    src <- paste(
      readLines(
        hla_inst_file(file.path("shiny/v1.4/hla_tcr_motifs", f)),
        warn = FALSE
      ),
      collapse = "\n"
    )
    calls <- regmatches(
      src,
      gregexpr("DT::datatable\\((?:[^()]|\\([^()]*\\))*\\)", src, perl = TRUE)
    )[[1]]
    scrolling <- calls[grepl("scrollX = TRUE", calls, fixed = TRUE)]
    expect_gt(length(scrolling), 0)
    for (call in scrolling) {
      expect_match(
        call,
        "nowrap",
        info = paste0(f, ": scrollX without nowrap in ", substr(call, 1, 60))
      )
    }
  }
})

test_that("scope guards test for 'all', never for one scope's name", {
  # Both the feature guard and its on-screen explanation must cover EVERY
  # allele-selected scope. Written as `!= "allele"` they read the same until a
  # scope is added -- and then the new one silently walks past both: its graph
  # goes back to nominating features, with no notice that it did.
  data_src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"), warn = FALSE),
    collapse = "\n"
  )
  assoc_src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/associations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  # The scope guard lives on the RAW build (hla_global_motif_graph_raw_cached):
  # it is the expensive, allele-independent build that must come from unscoped
  # segments, and the cheap min-size filter downstream inherits that. The scope
  # test itself is unchanged — it just sits on the raw reactive now.
  expect_match(
    data_src,
    "hla_global_motif_graph_raw_cached <- reactive\\(\\{[\\s\\S]{0,200}identical\\(hla_scope_mode\\(\\), \"all\"\\)",
    perl = TRUE
  )
  # Pinned against the name the logic actually lives under, so this cannot pass
  # by matching a wrapper that never mentions a scope at all.
  expect_no_match(
    data_src,
    "hla_global_motif_graph_raw_cached <- reactive\\(\\{[\\s\\S]{0,200}!identical\\(hla_scope_mode\\(\\), \"allele\"\\)",
    perl = TRUE
  )
  expect_match(
    assoc_src,
    "!identical\\(hla_scope_mode\\(\\), \"all\"\\)",
    perl = TRUE
  )
})

test_that("the parameter gate stays OUTSIDE the cached graph reactives", {
  # Both heavy graphs are gated on hla_params_ready() so the page does not build
  # and draw once against hla_param()'s fallbacks and then again for real the
  # moment output$hla_parameters_ui's inputs report.
  #
  # The gate must sit in the uncached wrapper. req() raises a silent condition,
  # and inside a bindCache body that condition is a value the cache may store
  # under the current key — every later hit on that key would then replay the
  # stop instead of building. Structural, and silent when broken: the page would
  # simply go blank for whichever parameters were live when it first opened.
  data_src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"), warn = FALSE),
    collapse = "\n"
  )
  # Each cached reactive is bindCache'd and must not gate. Both the expensive raw
  # builds and the cheap finalize layers are cached, so all four are pinned.
  for (nm in c(
    "hla_motif_graph_raw_cached",
    "hla_motif_graph_cached",
    "hla_global_motif_graph_raw_cached",
    "hla_global_motif_graph_cached"
  )) {
    expect_match(
      data_src,
      paste0(nm, " <- reactive\\(\\{[\\s\\S]{0,400}hla_bindCache\\("),
      perl = TRUE,
      info = paste(nm, "must be the bindCache'd reactive")
    )
    expect_no_match(
      data_src,
      paste0(nm, " <- reactive\\(\\{[\\s\\S]{0,200}req\\("),
      perl = TRUE,
      info = paste(nm, "must not req() inside the cached body")
    )
  }
  # ...and each public reactive gates before reaching its cache.
  for (nm in c("hla_motif_graph", "hla_global_motif_graph")) {
    expect_match(
      data_src,
      paste0(nm, " <- reactive\\(\\{\\s*req\\(hla_params_ready\\(\\)\\)"),
      perl = TRUE,
      info = paste(nm, "must gate on hla_params_ready() first")
    )
  }
  # The gate must decide on hla_color_by with is.null(), and must never req() it:
  # that input's default value is "" (colour by motif cluster), which req()
  # treats as missing, so the network would never draw until the user happened to
  # pick a colouring. Asserted as "tests it, but not with req()" rather than
  # against one spelling — `!is.null(x) &&` and `if (is.null(x)) return(FALSE)`
  # are the same contract.
  expect_match(
    hla_params_ready_src(data_src),
    "is\\.null\\(input\\$hla_color_by\\)",
    perl = TRUE
  )
  expect_no_match(
    hla_params_ready_src(data_src),
    "req\\(input\\$hla_color_by\\)",
    perl = TRUE
  )
})

test_that("the parameter panel renders once, then updates in place", {
  # Finding #8: output$hla_parameters_ui both CREATED its controls and READ its
  # own inputs to seed them, so every scope / colour / checkbox change tore the
  # whole panel down and rebuilt it (transient NULLs, lost selectize state). The
  # panel must render once from the DATA and update controls in place -- the
  # discipline the allele pickers already follow.
  settings_src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/settings.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  data_src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"), warn = FALSE),
    collapse = "\n"
  )

  # The colour-by choices are a shared reactive, not rebuilt inline inside the
  # picker's own renderUI.
  expect_match(data_src, "hla_color_by_choices <- reactive\\(", perl = TRUE)

  # The panel seeds its own inputs under isolate(), so setting them does not
  # invalidate the renderUI that owns them.
  expect_match(
    settings_src,
    "selected = isolate\\(hla_param\\(\"hla_scope\"",
    perl = TRUE
  )
  expect_match(
    settings_src,
    "selected = isolate\\(hla_param\\(\"hla_color_by\"",
    perl = TRUE
  )
  expect_match(
    settings_src,
    "value = isolate\\([\\s\\S]{0,20}hla_param\\(\"hla_by_v\"",
    perl = TRUE
  )

  # The one real cross-control coupling (scope decides whether "MHC context" or
  # "Pair class" is offered) moved to an observer that updates the picker in
  # place instead of rebuilding the panel.
  expect_match(
    settings_src,
    "observeEvent\\([\\s\\S]{0,20}hla_color_by_choices\\(\\)",
    perl = TRUE
  )
  expect_match(
    settings_src,
    "updateSelectizeInput\\([\\s\\S]{0,40}\"hla_color_by\"",
    perl = TRUE
  )
})

test_that("the page's allele lives outside both pickers", {
  # There is ONE allele for the whole page, and it must not be stored in either
  # picker's input. The network's picker sits in a conditionalPanel, so Shiny
  # suspends it whenever the network is neither allele-scoped nor carrier-
  # coloured — and updateSelectInput() addressed to a control that is not
  # rendered is silently dropped. With the value living there, picking an allele
  # on the Associations tab in that state wrote to nothing, and the network's
  # picker later seeded itself from choices[1] and dragged Associations back.
  # Measured before this fix: HLA-B*07:02 replaced by HLA-A*02:01, silently.
  data_src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"), warn = FALSE),
    collapse = "\n"
  )
  assoc_src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/associations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  settings_src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/settings.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  # A reactiveVal cannot be suspended; an input can.
  expect_match(
    data_src,
    "hla_allele_state <- reactiveVal\\(",
    perl = TRUE
  )
  # hla_color_allele() reads the shared value, never a picker's input.
  gate <- regmatches(
    data_src,
    regexpr(
      "hla_color_allele <- reactive\\(\\{[\\s\\S]{0,600}?\\n\\}\\)",
      data_src,
      perl = TRUE
    )
  )
  expect_length(gate, 1L)
  expect_match(gate, "hla_allele_state\\(\\)", perl = TRUE)
  expect_no_match(gate, "input\\$hla_color_allele", perl = TRUE)
  expect_no_match(gate, "input\\$hla_association_allele", perl = TRUE)

  # Both pickers seed from the shared value rather than from the top of the list.
  expect_match(
    assoc_src,
    "\"hla_association_allele\"[\\s\\S]{0,900}?selected = isolate\\(hla_color_allele\\(\\)\\)",
    perl = TRUE
  )
  expect_match(
    settings_src,
    "\"hla_color_allele\"[\\s\\S]{0,900}?selected = isolate\\(hla_color_allele\\(\\)\\)",
    perl = TRUE
  )
  # ...and the Associations numbers come from the shared allele, not from the
  # picker that only exists while that tab is open.
  expect_match(
    assoc_src,
    "hla_overlap_table <- reactive\\(\\{[\\s\\S]{0,400}?allele <- hla_color_allele\\(\\)",
    perl = TRUE
  )
})

test_that("the parameter gate covers the conditional allele pickers", {
  # The controls in output$hla_parameters_ui are only half the story. The allele
  # pickers live in SEPARATE uiOutputs inside conditionalPanels, so Shiny keeps
  # them suspended until the panel appears — which is the same instant the user
  # selects the scope or colouring that needs them. They therefore report one
  # flush AFTER the thing that reveals them, and a gate that only waits for the
  # main panel lets the page build against hla_color_allele()'s fallback, draw
  # it, and redraw it when the picker lands. Measured before this: two widgets
  # on one scope change.
  #
  # This is the third place the same shape of bug appeared (first open, allele
  # scope, pair scope), which is why it is pinned rather than just fixed.
  data_src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"), warn = FALSE),
    collapse = "\n"
  )
  gate <- hla_params_ready_src(data_src)

  # The single allele: a build parameter in "allele" scope, a display parameter
  # under carrier colouring.
  expect_match(gate, "input\\$hla_color_allele", perl = TRUE)
  expect_match(gate, "hla_carrier", perl = TRUE)
  # The pair scope's two pickers.
  expect_match(gate, "input\\$hla_pair_allele_i", perl = TRUE)
  expect_match(gate, "input\\$hla_pair_allele_ii", perl = TRUE)
  # ...but never wait for a picker the data set cannot offer: that uiOutput
  # renders a bare message and creates no input, so the gate would never open.
  expect_match(gate, "hla_allele_choices\\(\\)", perl = TRUE)
})

test_that("the pair scope is offered only when both classes can be picked", {
  src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"), warn = FALSE),
    collapse = "\n"
  )
  # A pair with no lineage to sort cells by is undefined, not narrower.
  expect_match(
    src,
    "hla_pair_available <- reactive\\(\\{[\\s\\S]{0,200}hla_celltype_col\\(\\)",
    perl = TRUE
  )
  expect_match(
    src,
    "hla_pair_available <- reactive\\(\\{[\\s\\S]{0,300}Class II",
    perl = TRUE
  )
})

test_that("the Network data tab exposes grain radio, table and download", {
  ui_src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/UI.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ui_src, "tabPanel\\([\\s\\S]{0,40}\"Network data\"", perl = TRUE)
  # The grain radio is rendered SERVER-side so its second label can follow the
  # declared observation unit: a bulk repertoire's rows are analysis units, not
  # cells. UI.R only carries the placeholder.
  expect_match(
    ui_src,
    "uiOutput\\([\\s\\S]{0,20}\"hla_table_grain_ui\"",
    perl = TRUE
  )
  tbl_src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/network_table.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    tbl_src,
    "radioButtons\\([\\s\\S]{0,40}\"hla_table_grain\"",
    perl = TRUE
  )
  # both the row label and the count column come from the declared unit
  expect_match(tbl_src, "getObservationUnit\\(\\)\\$singular", perl = TRUE)
  expect_match(tbl_src, "getObservationUnit\\(\\)\\$plural", perl = TRUE)
  expect_no_match(tbl_src, "clone_count = \"cells\"", fixed = TRUE)
  expect_match(
    ui_src,
    "dataTableOutput\\([\\s\\S]{0,20}\"hla_network_table\"",
    perl = TRUE
  )
  expect_match(
    ui_src,
    "downloadButton\\([\\s\\S]{0,20}\"hla_network_download\"",
    perl = TRUE
  )
})

test_that("the network table reads the graph and the segments, not the render cap", {
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/network_table.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  # node view from the SAME graph object the network draws
  expect_match(src, "hla_network_table_data <- reactive\\(", perl = TRUE)
  expect_match(src, "hla_motif_graph\\(\\)", perl = TRUE)
  expect_match(src, "as_data_frame\\([\\s\\S]{0,40}\"vertices\"", perl = TRUE)
  # cell view from the scoped per-cell rows
  expect_match(src, "hla_scoped_segments\\(\\)", perl = TRUE)
  # ...but filtered to the cells BEHIND the graph's nodes, not every scoped
  # cell: the graph drops singletons / sub-min-size motifs, so scoped segments
  # are a superset. Keep only rows whose node is a vertex of the current graph.
  expect_match(src, "V\\(g\\)\\$name", perl = TRUE)
  # switched by the grain input
  expect_match(src, "input\\$hla_table_grain", perl = TRUE)
  # NOT bound by the render cap (this is data, not canvas)
  expect_no_match(src, "HLA_MOTIF_MAX_RENDER", perl = TRUE)
  # sourced into the module
  server_src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(server_src, "network_table\\.R", perl = TRUE)
})

test_that("the network table carries the data set's own annotations", {
  # A fixed column whitelist showed only what this file happened to name, so a
  # demo whose whole point is a per-cell antigen / presenting allele had those
  # columns silently dropped -- while the vignette said the table contained
  # them. Both grains must append the DECLARED metadata columns, which is the
  # same set the network already colours by, so a new annotation column needs
  # no edit here.
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/network_table.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    src,
    "hla_network_table_meta_cols <- reactive\\([\\s\\S]{0,200}hla_node_meta_cols\\(\\)",
    perl = TRUE
  )
  # both grains, not just the node view
  expect_match(
    src,
    "HLA_NETWORK_TABLE_CELL_COLS,[\\s\\S]{0,40}hla_network_table_meta_cols\\(\\)",
    perl = TRUE
  )
  expect_match(
    src,
    "HLA_NETWORK_TABLE_NODE_COLS,[\\s\\S]{0,40}hla_network_table_meta_cols\\(\\)",
    perl = TRUE
  )
  # the annotations must not be able to duplicate a structural column
  expect_match(src, "!duplicated\\(names\\(map\\)\\)", perl = TRUE)
  # the wide sample list stays at the right edge, after the annotations
  expect_match(
    src,
    "hla_network_table_meta_cols\\(\\),[\\s\\S]{0,60}HLA_NETWORK_TABLE_NODE_TAIL_COLS",
    perl = TRUE
  )
  # a node aggregates cells, so an annotation that varies within it must not be
  # reported as that node's value
  expect_match(src, "hla_mark_mixed_nodes <- function", perl = TRUE)
  expect_match(
    src,
    "map\\[\\[\"clone_count\"\\]\\][\\s\\S]{0,300}hla_mark_mixed_nodes\\(",
    perl = TRUE
  )
})

test_that("the network table renders and downloads the current view", {
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/network_table.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    src,
    "output\\$hla_network_table <- DT::renderDataTable",
    perl = TRUE
  )
  expect_match(
    src,
    "output\\$hla_network_download <- downloadHandler",
    perl = TRUE
  )
  # the download writes the SAME reactive the table renders
  expect_match(
    src,
    "downloadHandler\\([\\s\\S]{0,400}hla_network_table_data\\(\\)",
    perl = TRUE
  )
})

test_that("the motif network cannot zoom out below its initial fit", {
  vis_src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  js_src <- paste(
    readLines(hla_inst_file("shiny/v1.4/www/hla_motifs.js"), warn = FALSE),
    collapse = "\n"
  )
  # zoom is button-only -- scroll/pinch zoom is off, so nothing shrinks the
  # network past the opening fit
  expect_match(vis_src, "zoomView = FALSE", perl = TRUE)
  # the render captures the initial (fit) scale as the floor and greys the
  # zoom-out button out once the network sits at it
  expect_match(vis_src, "hlaMinScale", perl = TRUE)
  expect_match(vis_src, "hla-mb-btn--off", perl = TRUE)
  # the modebar zoom-out button respects the same floor
  expect_match(js_src, "hlaMinScale", perl = TRUE)
  # the layout fills the whole (wide) plot area, not a centred square
  expect_match(vis_src, "type = \"full\"", perl = TRUE)
})

test_that("a colour change recolours in place, without re-rendering", {
  data_src <- paste(
    readLines(hla_inst_file("shiny/v1.4/hla_tcr_motifs/data.R"), warn = FALSE),
    collapse = "\n"
  )
  vis_src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  # (d) the nodes carry EVERY colourable column, so a colour switch never re-keys
  # the graph cache -- the column is already on the node.
  expect_match(
    data_src,
    "hla_node_meta_cols <- reactive\\(\\{[\\s\\S]{0,900}hla_color_meta_cols\\(\\)",
    perl = TRUE
  )
  # (a) a one-way readiness latch, so the renderer does not depend on the
  # colour-reading hla_params_ready() gate.
  expect_match(data_src, "hla_ready_latch <- reactiveVal\\(", perl = TRUE)
  # (b) the renderer gates on the latch and reads the coloured visnet ISOLATED,
  # so a colour change does not invalidate it.
  expect_match(vis_src, "req\\(hla_ready_latch\\(\\)\\)", perl = TRUE)
  expect_match(vis_src, "isolate\\(hla_visnet\\(\\)\\)", perl = TRUE)
  # (c) colour is pushed onto the existing network in place via a proxy.
  expect_match(vis_src, "visNetworkProxy\\(", perl = TRUE)
  expect_match(vis_src, "visUpdateNodes\\(", perl = TRUE)
})

test_that("the network table is nowrap-scrollable and truncates samples on hover", {
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/network_table.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  # cells never wrap; the table scrolls horizontally instead
  expect_match(src, "nowrap", perl = TRUE)
  expect_match(src, "scrollX = TRUE", perl = TRUE)
  # the multi-value samples column is truncated to the first value in DISPLAY
  # only (a DataTables render), so search / sort / CSV keep the full list
  expect_match(src, "columnDefs", perl = TRUE)
  expect_match(src, "split\\(','\\)\\[0\\]", perl = TRUE)
})

test_that("the samples column cannot inject HTML", {
  # DataTables escaping is deliberately OFF for the samples column so the render
  # can emit a <span>. A sample name is arbitrary .crb data, so if the cell were
  # assembled by string concatenation a value like
  #   <img src=x onerror=alert(document.domain)>
  # would run in the app's origin. The render must therefore build DOM nodes and
  # let the browser escape both the text and the title attribute.
  src <- paste(
    readLines(
      hla_inst_file("shiny/v1.4/hla_tcr_motifs/network_table.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  # built as DOM, escaped by the browser
  expect_match(src, "document\\.createElement\\('span'\\)", perl = TRUE)
  expect_match(src, "\\.textContent =", perl = TRUE)
  expect_match(src, "\\.title = full", perl = TRUE)
  expect_match(src, "return span\\.outerHTML", perl = TRUE)
  # and NOT by pasting the value into an HTML string, in either branch
  expect_no_match(src, "'<span title=", fixed = TRUE)
  expect_no_match(src, "&quot;", fixed = TRUE)
  expect_no_match(
    src,
    "if \\(parts\\.length <= 1\\) \\{ return data; \\}",
    perl = TRUE
  )
})
