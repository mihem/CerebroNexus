review_source <- function(...) {
  paste(readLines(viewer_test_path(...), warn = FALSE), collapse = "\n")
}

test_that("sharing is available without a selected cohort", {
  ui <- review_source("shiny_UI.R")
  linked_ui <- review_source("coordinated_views", "UI.R")
  state <- review_source("www", "specialist-view-state.js")
  config <- review_source("www", "coordviews-config.js")

  expect_no_match(ui, 'style = "display:none"', fixed = TRUE)
  expect_no_match(
    linked_ui,
    'class = paste(\n          "cv-config-open',
    fixed = TRUE
  )
  expect_no_match(state, "Select at least one cell first.", fixed = TRUE)
  expect_match(config, "exportReady = !!ready;", fixed = TRUE)
  expect_no_match(
    config,
    "Select at least one cell before sharing this view",
    fixed = TRUE
  )
  expect_no_match(
    review_source("coordinated_views", "config.R"),
    "!length(selected_cells)",
    fixed = TRUE
  )
})

test_that("Immune Repertoire exposes sharing only with its visible canvas", {
  ui <- review_source("immune_repertoire", "UI.R")
  visualizations <- review_source("immune_repertoire", "visualizations.R")
  canvas <- review_source("www", "cell_views.js")

  expect_match(
    ui,
    "input.ir_tabs == 'Clonal UMAP' && input.ir_p_umap_group_by == ''",
    fixed = TRUE
  )
  expect_match(
    visualizations,
    'cerebroSelectionStatus(\n    "ir_clonalUMAP_projection"',
    fixed = TRUE
  )
  expect_match(
    ui,
    'cerebroShareButton("ir_clonalUMAP_projection")',
    fixed = TRUE
  )
  expect_no_match(visualizations, 'output[["ir_share_UI"]]', fixed = TRUE)
  expect_match(canvas, "} else if (singleActive) {", fixed = TRUE)
})

test_that("missing genes retain compact validation feedback", {
  ui <- review_source("gene_expression", "UI_projection.R")
  feedback <- review_source("gene_expression", "out_genes_displayed.R")

  expect_match(ui, 'uiOutput("expression_genes_displayed")', fixed = TRUE)
  expect_match(feedback, 'genes_to_display_missing', fixed = TRUE)
  expect_match(feedback, 'class = "cerebro-gene-validation"', fixed = TRUE)
})

test_that("composition dragging captures and reliably releases its pointer", {
  javascript <- review_source("www", "viewer-shell.js")

  expect_match(javascript, "setPointerCapture(event.pointerId)", fixed = TRUE)
  expect_match(
    javascript,
    "releasePointerCapture(drag.pointerId)",
    fixed = TRUE
  )
  expect_match(
    javascript,
    'window.addEventListener("blur", finish)',
    fixed = TRUE
  )
  expect_match(
    javascript,
    'document.addEventListener("lostpointercapture"',
    fixed = TRUE
  )
})

test_that("HLA modebar does not consume legend width", {
  css <- review_source("www", "hla_motifs.css")

  expect_match(css, "margin: 0 0 2px;", fixed = TRUE)
  expect_match(css, "position: absolute; top: 8px; right: 8px;", fixed = TRUE)
  expect_match(css, "opacity: 0; pointer-events: none;", fixed = TRUE)
  expect_match(css, ".hla-plot-wrap:hover .hla-modebar", fixed = TRUE)
  expect_match(css, ".hla-modebar:focus-within", fixed = TRUE)
})

test_that("compact controls remain readable", {
  css <- review_source("www", "custom.css")
  linked_css <- review_source("www", "coordviews.css")

  expect_match(css, "font-size: 12.5px;", fixed = TRUE)
  expect_match(css, "font-size: 13px;", fixed = TRUE)
  expect_match(linked_css, "font-size: 13px;", fixed = TRUE)
})

test_that("Share and Settings use one right-aligned toolbar layout", {
  ui <- review_source("shiny_UI.R")
  css <- review_source("www", "custom.css")

  expect_match(ui, "cerebroToolbarActions <- function", fixed = TRUE)
  expect_match(
    css,
    ".cerebro-toolbar-actions {",
    fixed = TRUE
  )
  expect_match(
    css,
    ".cerebro-toolbar-actions .cerebro-more-btn",
    fixed = TRUE
  )
  for (path in list(
    c("overview", "UI_projection.R"),
    c("spatial", "UI_projection.R"),
    c("gene_expression", "UI_projection.R"),
    c("trajectory", "projection.R"),
    c("immune_repertoire", "UI.R"),
    c("hla_tcr_motifs", "UI.R"),
    c("coordinated_views", "UI.R")
  )) {
    expect_match(
      do.call(review_source, as.list(path)),
      "cerebroToolbarActions(",
      fixed = TRUE,
      info = paste(path, collapse = "/")
    )
  }
})

test_that("HLA reports readiness when its network is drawn", {
  javascript <- review_source("www", "hla_motifs.js")

  expect_match(javascript, "function reportState()", fixed = TRUE)
  expect_match(
    javascript,
    "if (changed && network) reportState();",
    fixed = TRUE
  )
})

test_that("HLA modebar exposes selection clearing consistently", {
  ui <- review_source("hla_tcr_motifs", "UI.R")
  javascript <- review_source("www", "cell_views.js")

  expect_match(
    ui,
    'cerebroSelectionStatus(',
    fixed = TRUE
  )
  expect_match(
    javascript,
    "if (singleAct === 'clear') clearSingleSelection(singleId);",
    fixed = TRUE
  )
})

test_that("HLA selection zoom derives a viewport from selected nodes", {
  ui <- review_source("hla_tcr_motifs", "UI.R")
  javascript <- review_source("www", "cell_views.js")

  expect_match(ui, 'cerebroSelectionStatus(', fixed = TRUE)
  expect_match(javascript, "function zoomToSelection(p)", fixed = TRUE)
  expect_match(
    javascript,
    "if (singleAct === 'focus') toggleSelectionFocus(singleId);",
    fixed = TRUE
  )
})

test_that("shared selection actions focus and clear the same viewport", {
  ui <- review_source("shiny_UI.R")
  linked_ui <- review_source("coordinated_views", "UI.R")
  javascript <- review_source("www", "cell_views.js")

  expect_match(ui, 'tags$span("Focus")', fixed = TRUE)
  expect_match(linked_ui, 'id = "cv-zsel"', fixed = TRUE)
  expect_match(
    javascript,
    "if (singleAct === 'focus') toggleSelectionFocus(singleId);",
    fixed = TRUE
  )
  expect_match(javascript, "if (selectionZoomed) resetZoom();", fixed = TRUE)
})
