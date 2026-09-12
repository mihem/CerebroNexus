test_that("gene expression summary modes keep their intended series", {
  helper <- viewer_test_path(
    "gene_expression",
    "func_expression_summary.R"
  )
  expect_true(file.exists(helper))
  if (!file.exists(helper)) {
    return(invisible())
  }
  source(helper, local = TRUE)

  combined <- expressionSummarySpec(
    "combined",
    c("MS4A1", "CD3D")
  )
  expect_identical(combined$kind, "mean")
  expect_identical(combined$series[[1]]$genes, c("MS4A1", "CD3D"))

  separate <- expressionSummarySpec(
    "separate",
    c("MS4A1", "CD3D")
  )
  expect_identical(separate$kind, "separate")
  expect_identical(
    vapply(separate$series, `[[`, character(1), "label"),
    c("MS4A1", "CD3D")
  )

  expect_identical(
    expressionSummarySpec(
      "separate",
      paste0("Gene", seq_len(10))
    )$kind,
    "mean"
  )
  expect_identical(
    expressionSummarySpec(
      "separate",
      c("MS4A1", "CD3D"),
      dimensions = 3
    )$kind,
    "mean"
  )

  rgb <- expressionSummarySpec(
    "rgb",
    c("MS4A1", "CD3D"),
    list(r = "MS4A1", g = "CD3D", b = "")
  )
  expect_identical(rgb$kind, "rgb")
  expect_identical(
    vapply(rgb$series, `[[`, character(1), "label"),
    c("R · MS4A1", "G · CD3D")
  )
  expect_identical(
    vapply(rgb$series, `[[`, character(1), "genes"),
    c("MS4A1", "CD3D")
  )
})

test_that("RGB summaries preserve repeated channels and omit empty ones", {
  helper <- viewer_test_path(
    "gene_expression",
    "func_expression_summary.R"
  )
  skip_if_not(file.exists(helper))
  source(helper, local = TRUE)

  rgb <- expressionSummarySpec(
    "rgb",
    c("MS4A1", "CD3D"),
    list(r = "MS4A1", g = "MS4A1", b = "CD3D")
  )
  expect_identical(
    vapply(rgb$series, `[[`, character(1), "label"),
    c("R · MS4A1", "G · MS4A1", "B · CD3D")
  )
})

test_that("RGB expression reads all channels in one backend call", {
  scope <- new.env(parent = globalenv())
  scope$reactive <- shiny::reactive
  scope$req <- shiny::req
  scope$withProgress <- function(expr, ...) force(expr)
  scope$incProgress <- function(...) NULL
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  scope$input <- shiny::reactiveValues(
    expression_projection_genes_in_separate_panels = "rgb"
  )

  values <- matrix(
    c(1, 2, 3, 4, 5, 6),
    nrow = 3,
    dimnames = list(c("g1", "g2", "g3"), c("c1", "c2"))
  )
  reads <- 0L
  dataset <- new.env(parent = emptyenv())
  dataset$expression <- values
  dataset$getExpressionMatrix <- function(cells = NULL, genes = NULL) {
    reads <<- reads + 1L
    values[genes, cells, drop = FALSE]
  }
  scope$data_set <- shiny::reactive(dataset)
  scope$getGeneNames <- function() rownames(values)
  scope$expression_projection_cells_to_show <- shiny::reactive(c(1L, 2L))
  scope$expression_projection_coordinates <- shiny::reactive(data.frame(
    x = c(0, 1),
    y = c(1, 0)
  ))
  scope$expression_selected_genes <- shiny::reactive(list(
    genes_to_display_present = c("g1", "g2", "g3"),
    rgb_genes = list(r = "g1", g = "g2", b = "g3")
  ))

  sys.source(
    viewer_test_path("gene_expression", "func_expression_summary.R"),
    envir = scope
  )
  sys.source(
    viewer_test_path(
      "gene_expression",
      "obj_projection_expression_levels.R"
    ),
    envir = scope
  )
  levels <- shiny::isolate(scope$expression_projection_expression_levels())

  expect_identical(reads, 1L)
  expect_equal(levels, list(r = c(1, 4), g = c(2, 5), b = c(3, 6)))
})

test_that("RGB violin outliers use the channel color", {
  source(
    viewer_test_path("plotting_functions.R"),
    local = TRUE
  )
  source(
    viewer_test_path("gene_expression", "func_expression_summary.R"),
    local = TRUE
  )

  plot <- plotExpressionSummary(
    list(list(
      label = "R · MS4A1",
      key = "r",
      genes = "MS4A1",
      color = "#dc2626",
      values = c(0, 0, 0, 1, 4)
    )),
    factor(rep("sample_1", 5)),
    c(sample_1 = "#f59e0b")
  )
  trace <- plotly::plotly_build(plot)$x$data[[1]]

  expect_identical(trace$marker$color, "#dc2626")
})

test_that("gene expression panels follow gene, selection, and display mode", {
  skip_if_not_installed("shinytest2")
  inst_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(inst_dir))
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = "gene_expression_panel_modes",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())

  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  app$wait_for_js(
    "document.getElementById('expression_genes_input') !== null",
    timeout = 20000
  )
  expect_false(app$get_js(
    "document.querySelector('#expression_details_selected_cells_UI h3') !== null"
  ))
  expect_false(app$get_js(
    "document.querySelector('#expression_in_selected_cells_UI h3') !== null"
  ))

  viewer_set_selectize(app, "expression_genes_input", "MS4A1")
  app$wait_for_idle(timeout = 60000)
  app$wait_for_js(
    "document.querySelector('#expression_by_group_UI h3') !== null",
    timeout = 20000
  )
  expect_false(app$get_js(
    "document.querySelector('#expression_by_gene_UI h3') !== null"
  ))

  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_projection_cell_view_host canvas:not(.cv-mini)') !== null"
    ),
    timeout = 20000
  )
  app$wait_for_js(
    paste0(
      "window.cerebroCellViews?.captureState(",
      "'expression_projection') !== null"
    ),
    timeout = 20000
  )
  drag <- app$get_js(paste0(
    "(() => {",
    "const host=document.getElementById(",
    "'expression_projection_cell_view_host');",
    "host.querySelector('.cv-tbtn[data-act=\"box\"]').click();",
    "const r=host.querySelector('canvas:not(.cv-mini)').getBoundingClientRect();",
    "return {x1:r.left+r.width*.08,y1:r.top+r.height*.08,",
    "x2:r.right-r.width*.08,y2:r.bottom-r.height*.08};",
    "})()"
  ))
  viewer_drag_mouse(app, drag$x1, drag$y1, drag$x2, drag$y2)
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_in_selected_cells_UI h3') !== null"
    ),
    timeout = 20000
  )
  expect_true(app$get_js(
    "document.querySelector('#expression_details_selected_cells_UI h3') !== null"
  ))
  app$run_js(paste0(
    "document.querySelector(",
    "'#expression_projection_cell_view_host .cv-clear-btn').click()"
  ))
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_in_selected_cells_UI h3') === null"
    ),
    timeout = 20000
  )

  viewer_set_selectize(
    app,
    "expression_genes_input",
    c("MS4A1", "CD3D")
  )
  app$wait_for_js(
    paste0(
      "document.getElementById(",
      "'expression_projection_genes_in_separate_panels')?.disabled === false"
    ),
    timeout = 10000
  )
  app$set_inputs(
    expression_projection_genes_in_separate_panels = "separate",
    wait_ = FALSE
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('expression_by_group')?.innerText",
      ".includes('MS4A1')"
    ),
    timeout = 20000
  )
  panel_text <- app$get_js(
    "document.getElementById('expression_by_group').innerText"
  )
  expect_true(grepl("MS4A1", panel_text, fixed = TRUE))
  expect_true(grepl("CD3D", panel_text, fixed = TRUE))

  app$set_inputs(
    expression_projection_genes_in_separate_panels = "rgb",
    wait_ = FALSE
  )
  app$wait_for_js(
    "document.getElementById('expression_rgb_gene_r') !== null",
    timeout = 10000
  )
  viewer_set_selectize(app, "expression_rgb_gene_r", "MS4A1")
  viewer_set_selectize(app, "expression_rgb_gene_g", "CD3D")
  viewer_set_selectize(app, "expression_rgb_gene_b", "")
  app$wait_for_js(
    paste0(
      "(() => {const text=document.getElementById(",
      "'expression_by_group')?.innerText || '';",
      "return text.includes('R · MS4A1') && ",
      "text.includes('G · CD3D') && !text.includes('B ·');})()"
    ),
    timeout = 20000
  )
})
