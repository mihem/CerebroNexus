source_expressions <- function(path) {
  parse(viewer_test_path(path))
}

call_class <- function(expression) {
  if (!is.call(expression)) {
    return(NULL)
  }
  arguments <- as.list(expression)[-1L]
  classes <- arguments[names(arguments) == "class"]
  if (length(classes) != 1L || !is.character(classes[[1L]])) {
    return(NULL)
  }
  classes[[1L]]
}

find_call_by_class <- function(expression, class_name) {
  classes <- call_class(expression)
  if (
    is.call(expression) &&
      class_name %in% strsplit(if (is.null(classes)) "" else classes, " ")[[1L]]
  ) {
    return(expression)
  }
  if (!is.recursive(expression)) {
    return(NULL)
  }
  children <- as.list(expression)
  for (index in seq_along(children)) {
    if (rlang::is_missing(children[[index]])) {
      next
    }
    child <- children[[index]]
    match <- find_call_by_class(child, class_name)
    if (!is.null(match)) {
      return(match)
    }
  }
  NULL
}

contains_call <- function(expression, name, first_argument = NULL) {
  if (is.call(expression) && identical(as.character(expression[[1L]]), name)) {
    if (is.null(first_argument)) {
      return(TRUE)
    }
    if (
      length(expression) >= 2L &&
        is.character(expression[[2L]]) &&
        identical(expression[[2L]], first_argument)
    ) {
      return(TRUE)
    }
  }
  if (!is.recursive(expression)) {
    return(FALSE)
  }
  children <- as.list(expression)
  for (index in seq_along(children)) {
    if (rlang::is_missing(children[[index]])) {
      next
    }
    child <- children[[index]]
    if (contains_call(child, name, first_argument)) {
      return(TRUE)
    }
  }
  FALSE
}

find_assignment_rhs <- function(expression, left_hand_side) {
  if (
    is.call(expression) &&
      identical(as.character(expression[[1L]]), "<-") &&
      identical(paste(deparse(expression[[2L]]), collapse = ""), left_hand_side)
  ) {
    return(expression[[3L]])
  }
  if (!is.recursive(expression)) {
    return(NULL)
  }
  children <- as.list(expression)
  for (index in seq_along(children)) {
    if (rlang::is_missing(children[[index]])) {
      next
    }
    match <- find_assignment_rhs(children[[index]], left_hand_side)
    if (!is.null(match)) {
      return(match)
    }
  }
  NULL
}

test_that("standard visualization toolbars own selection status", {
  pages <- c(
    "overview/UI_projection.R",
    "gene_expression/UI_projection.R",
    "trajectory/projection.R",
    "spatial/UI_projection.R",
    "trekker/UI.R",
    "hla_tcr_motifs/UI.R"
  )
  for (page in pages) {
    toolbar <- find_call_by_class(
      source_expressions(page),
      "cerebro-viz-toolbar"
    )
    expect_false(is.null(toolbar), info = page)
    expect_true(
      contains_call(toolbar, "cerebroSelectionStatus"),
      info = page
    )
  }

  ir_toolbar <- find_call_by_class(
    source_expressions("immune_repertoire/UI.R"),
    "cerebro-viz-toolbar"
  )
  expect_true(
    contains_call(ir_toolbar, "uiOutput", "ir_selection_status_UI")
  )
})

test_that("Linked views uses the shared header and owns selection status", {
  source <- source_expressions("coordinated_views/UI.R")
  source_text <- paste(deparse(source), collapse = "\n")
  toolbar <- find_call_by_class(source, "cv-topbar")

  expect_true(contains_call(source, "cerebroVizPageHeader"))
  expect_match(source_text, 'meta_id = "cv-meta"', fixed = TRUE)
  expect_false(is.null(toolbar))
  expect_false(is.null(find_call_by_class(toolbar, "cv-status-slot")))
})

test_that("specialized controls live in More settings", {
  spatial_settings <- source_expressions(
    "spatial/UI_projection_main_parameters.R"
  )
  spatial_main <- find_assignment_rhs(
    spatial_settings,
    'output[["spatial_projection_main_parameters_UI"]]'
  )
  spatial_background <- find_assignment_rhs(
    spatial_settings,
    'output[["spatial_projection_background_selector_UI"]]'
  )
  spatial_toolbar <- find_call_by_class(
    source_expressions("spatial/UI_projection.R"),
    "cerebro-viz-toolbar"
  )

  expect_false(
    contains_call(
      spatial_main,
      "selectInput",
      "spatial_projection_background_image"
    )
  )
  expect_true(
    contains_call(
      spatial_background,
      "selectInput",
      "spatial_projection_background_image"
    )
  )
  expect_true(
    contains_call(
      spatial_toolbar,
      "uiOutput",
      "spatial_projection_background_selector_UI"
    )
  )

  hla_settings <- source_expressions("hla_tcr_motifs/settings.R")
  hla_main <- find_assignment_rhs(hla_settings, "output$hla_parameters_ui")
  hla_more <- find_assignment_rhs(
    hla_settings,
    "output$hla_more_parameters_ui"
  )

  expect_false(contains_call(hla_main, "sliderInput", "hla_min_nodes"))
  expect_true(contains_call(hla_more, "sliderInput", "hla_min_nodes"))
})

test_that("visualization chrome uses compact shared sizing", {
  css <- paste(
    readLines(viewer_test_path("www/custom.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(css, "--c-toolbar-control-height: 34px", fixed = TRUE)
  expect_match(
    css,
    ".cerebro-viz-toolbar > .cerebro-selection-status-slot",
    fixed = TRUE
  )
  expect_match(
    css,
    ".coordviews-page .cv-topbar > .cv-status-slot",
    fixed = TRUE
  )
})

test_that("shared cell-view chrome keeps only compact plot gutters", {
  css <- paste(
    readLines(viewer_test_path("www/coordviews.css"), warn = FALSE),
    collapse = "\n"
  )
  js <- paste(
    readLines(viewer_test_path("www/cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(css, "gap: 8px", fixed = TRUE)
  expect_match(css, "border-radius: 10px; padding: 8px", fixed = TRUE)
  expect_match(js, "var chromeX = 18;", fixed = TRUE)
  expect_match(
    js,
    paste0(
      "var padL = axed ? 36 : 10, padB = axed ? 24 : 10, ",
      "padT = 10, padR = 10;"
    ),
    fixed = TRUE
  )
})

test_that("legends use one scrolling row outside the visualization", {
  linked <- source_expressions("coordinated_views/UI.R")
  linked_text <- paste(deparse(linked), collapse = "\n")
  linked_panes <- find_call_by_class(linked, "cv-panes")
  expect_true(is.null(find_call_by_class(linked_panes, "cv-legend")))
  expect_lt(
    regexpr('id = "cv-legend"', linked_text, fixed = TRUE)[[1]],
    regexpr('class = "cv-panes"', linked_text, fixed = TRUE)[[1]]
  )

  spatial <- source_expressions("spatial/UI_projection.R")
  spatial_surface <- find_call_by_class(spatial, "spatial-viz-surface")
  expect_true(
    contains_call(
      spatial_surface,
      "textOutput",
      "spatial_projection_morans_i"
    )
  )
  expect_true(
    contains_call(
      spatial_surface,
      "cerebroCellViewOutput",
      "spatial_projection"
    )
  )

  hla <- source_expressions("hla_tcr_motifs/UI.R")
  hla_tab <- find_call_by_class(hla, "hla-motif-tab")
  expect_true(contains_call(hla_tab, "uiOutput", "hla_legend_ui"))
  expect_true(
    contains_call(hla_tab, "cerebroCellViewOutput", "hla_motif_network")
  )

  css <- paste(
    c(
      readLines(viewer_test_path("www/coordviews.css"), warn = FALSE),
      readLines(viewer_test_path("www/hla_motifs.css"), warn = FALSE)
    ),
    collapse = "\n"
  )
  expect_match(css, "flex-wrap: nowrap", fixed = TRUE)
  expect_match(css, "overflow-x: auto", fixed = TRUE)
  expect_match(css, ".hla-legend-row:not(:empty)", fixed = TRUE)
  expect_match(
    css,
    "\\.hla-plot-wrap \\{[^}]*background: var\\(--c-surface\\)",
    perl = TRUE
  )
  expect_false(grepl(".cv-legend-overlay", css, fixed = TRUE))
  expect_false(grepl(".hla-legend-overlay", css, fixed = TRUE))
  expect_match(css, ".spatial-moran-overlay", fixed = TRUE)

  js <- paste(
    readLines(viewer_test_path("www/cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(js, "cerebro-cell-view-legend", fixed = TRUE)
})

test_that("Immune repertoire keeps its status row across subtabs", {
  source <- paste(
    readLines(
      viewer_test_path("immune_repertoire/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(source, "Selection is available in Clonal UMAP", fixed = TRUE)
})

test_that("immune analysis pages share the compact tab strip", {
  hla <- paste(
    readLines(viewer_test_path("hla_tcr_motifs/UI.R"), warn = FALSE),
    collapse = "\n"
  )
  repertoire <- paste(
    readLines(
      viewer_test_path("immune_repertoire/visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  css <- paste(
    readLines(viewer_test_path("www/custom.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(hla, 'class = "cerebro-analysis-tabs"', fixed = TRUE)
  expect_match(repertoire, 'class = "cerebro-analysis-tabs"', fixed = TRUE)
  expect_match(css, ".cerebro-analysis-tabs > .nav-tabs", fixed = TRUE)
})
