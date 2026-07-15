hla_inst_file <- function(...) {
  installed <- system.file(..., package = "cerebroAppLite")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }
  testthat::test_path("../../inst", ...)
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

test_that("source-tree core shim does not require a freshly installed package", {
  repo_root <- normalizePath(testthat::test_path("../.."), mustWork = TRUE)
  app_root <- file.path(repo_root, "inst")
  shim_path <- file.path(
    app_root,
    "shiny/v1.4/hla_tcr_motifs/core_shim.R"
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

test_that("bundled HLA demo is disclosed as fully fabricated", {
  # This demo used to carry REAL sequences with a synthetic receptor-to-cell
  # linkage. It is now fabricated end to end, which is a strictly stronger
  # claim: the label and registry must not still read as "real data, synthetic
  # wiring", or a reader would trust the sequences.
  app <- paste(readLines(hla_inst_file("app.R"), warn = FALSE), collapse = "\n")

  datasets_path <- testthat::test_path("../../data-raw/DATASETS.md")
  if (file.exists(datasets_path)) {
    datasets <- paste(readLines(datasets_path, warn = FALSE), collapse = "\n")
    expect_match(datasets, "fully synthetic", ignore.case = TRUE)
  }
  expect_match(app, "FULLY FABRICATED", ignore.case = TRUE)
  expect_match(app, "Synthetic cohort[\\s\\S]{0,40}fixture", perl = TRUE)
})

## ---- single-cell fixture contracts ------------------------------------ ##
## Its whole reason to exist is a motif network that is dense enough to read,
## which only holds because the families were designed in. If a rebuild loses
## that, the page silently reverts to the near-empty scatter this replaced.

hla_sc_demo <- function() {
  path <- hla_inst_file("extdata/v1.4/demo_hla_tcr.crb")
  testthat::skip_if_not(file.exists(path), "single-cell demo not built")
  readRDS(path)
}

test_that("single-cell fixture declares synthetic selection and cell units", {
  ti <- hla_sc_demo()$technical_info
  expect_equal(ti$tcr_selection, "synthetic")
  expect_equal(ti$observation_unit, "cell")
  expect_equal(ti$receptor_key, "v_gene+cdr3")
  expect_true(nzchar(ti$tcr_selection_detail))
})

test_that("single-cell fixture HLA is synthetic and covers every sample", {
  crb <- hla_sc_demo()
  ht <- crb$getHLATyping()
  expect_true(all(ht$source_type == "synthetic"))
  expect_setequal(unique(ht$sample), names(crb$getImmuneRepertoire()))
  # Only the loci the page enforces.
  expect_setequal(
    unique(ht$locus),
    c("HLA-A", "HLA-B", "HLA-C", "HLA-DRB1")
  )
})

test_that("single-cell fixture yields a readable TRB motif network", {
  crb <- hla_sc_demo()
  seg <- hla_parse_ir_segments(crb$getImmuneRepertoire(), "TRB")
  nodes <- hla_aggregate_cdr3_nodes(seg, by_v = TRUE)
  m <- hla_build_motif_groups(nodes, by_v = TRUE)$motif_df
  in_motif <- m[m$motif_size >= 2L, ]
  # The predecessor produced 4 nodes in 2 motifs. Assert an order of magnitude
  # more, and a spread of sizes rather than a pile of identical pairs.
  expect_gt(nrow(in_motif), 300L)
  expect_gte(length(unique(in_motif$motif_group)), 15L)
  expect_gte(max(in_motif$motif_size), 40L)
  # Isolated CDR3s must still dominate: a repertoire where everything clusters
  # would be its own kind of lie.
  expect_gt(nrow(m) - nrow(in_motif), nrow(in_motif))
})

## ---- shipped demo contracts ------------------------------------------- ##
## The bulk demo makes claims the UI depends on. If a rebuild drops one, the
## page silently changes meaning: donor-level counting reverts to sample-level,
## or the positive-control disclosure disappears while the contrast remains.

hla_bulk_demo <- function() {
  path <- hla_inst_file("extdata/v1.4/demo_hla_tcr_bulk.crb")
  testthat::skip_if_not(file.exists(path), "bulk demo not built")
  readRDS(path)
}

test_that("bulk demo declares its association-conditioned selection", {
  ti <- hla_bulk_demo()$technical_info
  expect_equal(ti$tcr_selection, "association-conditioned")
  expect_true(nzchar(ti$tcr_selection_detail))
})

test_that("bulk demo declares a V-gene+CDR3 receptor key", {
  # Its CDR3s recur across V families, so CDR3-only nodes would fuse receptors
  # the source counts separately.
  expect_equal(hla_bulk_demo()$technical_info$receptor_key, "v_gene+cdr3")
})

test_that("bulk demo carries donor ids, so counting is donor-level", {
  ht <- hla_bulk_demo()$getHLATyping()
  expect_false(any(is.na(ht$donor_id)))
  units <- hla_analysis_unit_map(ht, unique(ht$sample))
  expect_equal(unique(units$unit_type), "donor")
})

test_that("bulk demo HLA is real, and measures no genes", {
  crb <- hla_bulk_demo()
  expect_true(all(crb$getHLATyping()$source_type == "genotyped"))
  # Bulk: no transcriptome. A 0-row matrix states that; NULL would break
  # ncol()/nrow() call sites.
  expect_equal(nrow(crb$expression), 0L)
  expect_equal(ncol(crb$expression), nrow(crb$meta_data))
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

## ---- core_shim covers every core file and symbol ---------------------- ##
## The shim has TWO paths: a repository launch sys.source()s a hardcoded file
## list, while an installed launch pulls names from the namespace. A gap in
## either one is invisible to unit tests (which load the whole package) and
## only shows up as "could not find function" in a running app, on one launch
## mode. Pin both.

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

test_that("core_shim binds every package function the module calls", {
  mod_dir <- hla_inst_file("shiny/v1.4/hla_tcr_motifs")
  mod <- list.files(mod_dir, pattern = "[.]R$", full.names = TRUE)
  src <- unlist(lapply(mod, readLines, warn = FALSE))
  called <- unique(unlist(regmatches(
    src,
    gregexpr("hla_[a-zA-Z0-9_]+(?=[(])", src, perl = TRUE)
  )))

  pkg_files <- list.files(
    testthat::test_path("../../R"),
    pattern = "^hla_.*[.]R$",
    full.names = TRUE
  )
  pkg <- unlist(lapply(pkg_files, readLines, warn = FALSE))
  defined <- unique(sub(
    "^([a-zA-Z0-9_.]+) <- function.*$",
    "\\1",
    grep("^hla_[a-zA-Z0-9_.]+ <- function", pkg, value = TRUE)
  ))

  shim <- paste(
    readLines(file.path(mod_dir, "core_shim.R"), warn = FALSE),
    collapse = "\n"
  )
  need <- intersect(called, defined)
  missing <- need[
    !vapply(
      need,
      function(f) grepl(paste0('"', f, '"'), shim, fixed = TRUE),
      logical(1)
    )
  ]
  expect_equal(missing, character(0))
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
  expect_match(node_df, "size = hla_node_radius\\(clone_count\\)", perl = TRUE)
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
