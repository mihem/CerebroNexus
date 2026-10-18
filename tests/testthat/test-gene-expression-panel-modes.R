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

test_that("Viewer BPCells reads preserve values and canonical cell order", {
  skip_if_not_installed("BPCells")
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  source <- matrix(
    c(0, 1.25, -2, 4, 0, 5.5, 3, 6, NA_real_, 7, 8, 9),
    nrow = 3L,
    dimnames = list(c("g1", "g2", "g3"), c("c1", "c2", "c3", "c4"))
  )
  iterable <- methods::as(
    methods::as(Matrix::Matrix(source, sparse = TRUE), "CsparseMatrix"),
    "IterableMatrix"
  )
  directory <- withr::local_tempdir()
  for (row_major in c(FALSE, TRUE)) {
    path <- file.path(directory, if (row_major) "row" else "column")
    if (row_major) {
      BPCells::transpose_storage_order(iterable, outdir = path)
    } else {
      BPCells::write_matrix_dir(iterable, dir = path)
    }
    dataset <- new.env(parent = emptyenv())
    dataset$expression <- BPCells::open_matrix_dir(path)
    reads <- 0L
    dataset$getExpressionMatrix <- function(cells, genes) {
      reads <<- reads + 1L
      source[genes, cells, drop = FALSE]
    }
    dataset$getExpressionRow <- function(gene, cells) {
      reads <<- reads + 1L
      as.numeric(source[gene, cells])
    }
    for (cells in list(1:4, c(4L, 1L, 3L), 2L)) {
      expect_identical(
        scope$viewerExpressionValues(dataset, cells, c("g3", "g1")),
        list(
          g3 = as.numeric(source["g3", cells]),
          g1 = as.numeric(source["g1", cells])
        )
      )
      expect_identical(
        scope$viewerExpressionRow(dataset, cells, "g2"),
        as.numeric(source["g2", cells])
      )
    }
    expect_identical(reads, 0L)
    expect_identical(
      scope$viewerExpressionValues(dataset, c(4L, 1L, 4L), c("g3", "g1")),
      list(g3 = c(9, -2, 9), g1 = c(7, 0, 7))
    )
    expect_identical(reads, 1L)
    expect_identical(dimnames(dataset$expression), dimnames(source))
    expect_identical(as.matrix(dataset$expression), source)
    expect_identical(
      scope$viewerExpressionValues(dataset, c("c4", "c1"), c("g2", "g1")),
      list(g2 = c(8, 1.25), g1 = c(7, 0))
    )
    expect_identical(reads, 2L)
    expect_null(scope$viewerBpcellsValues(dataset, c(NA_integer_, 1L), "g1"))
    expect_null(scope$viewerBpcellsValues(dataset, c(0L, 1L), "g1"))
    expect_null(scope$viewerBpcellsValues(dataset, 1.5, "g1"))
    expect_null(scope$viewerBpcellsValues(dataset, 1L, "missing"))
  }
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

test_that("gene summary hosts change only when their layout changes", {
  shiny::testServer(
    function(input, output, session) {
      groups <- reactiveVal(c("sample", "cluster"))
      summary <- reactiveVal(NULL)
      reads <- 0L
      expression_summary_data <- function() {
        reads <<- reads + 1L
        req(summary())
        summary()
      }
      getGroups <- function() groups()
      expression_projection_summary_ready <- function() TRUE
      cerebroBox <- shiny::div
      boxTitle <- function(x) x
      cerebroInfoButton <- function(...) NULL
      scope <- environment()
      sys.source(
        viewer_test_path("gene_expression", "UI_expression_by_group.R"),
        scope
      )
      sys.source(
        viewer_test_path("gene_expression", "UI_expression_by_gene.R"),
        scope
      )
    },
    {
      session$setInputs(sidebar = "overview")
      expect_identical(reads, 0L)
      summary(list(genes = "A", series = list(1)))
      session$setInputs(
        sidebar = "geneExpression",
        expression_summary_viewport_request = 1
      )
      first <- output$expression_by_group_UI
      expect_match(first$html, "400px", fixed = TRUE)
      session$setInputs(expression_by_group_selected_group = "cluster")
      summary(list(genes = "B", series = list(2)))
      session$flushReact()
      expect_identical(output$expression_by_group_UI, first)
      expect_identical(input$expression_by_group_selected_group, "cluster")
      summary(list(genes = c("A", "B"), series = list(1, 2)))
      session$flushReact()
      gene_host <- output$expression_by_gene_UI
      expect_match(gene_host$html, "expression_by_gene", fixed = TRUE)
      summary(list(genes = c("C", "D"), series = list(3, 4)))
      session$flushReact()
      expect_identical(output$expression_by_gene_UI, gene_host)
      expect_identical(output$expression_by_group_UI, first)
      summary(list(genes = LETTERS[1:7], series = as.list(1:7)))
      session$flushReact()
      expect_match(output$expression_by_group_UI$html, "900px", fixed = TRUE)
      expect_match(
        output$expression_by_group_UI$html,
        'value="cluster" selected',
        fixed = TRUE
      )
      groups(c("batch", "condition"))
      session$flushReact()
      expect_match(
        output$expression_by_group_UI$html,
        'value="batch" selected',
        fixed = TRUE
      )
      summary(NULL)
      session$flushReact()
      expect_null(expression_summary_group_height())
      expect_false(expression_summary_show_genes())
    }
  )
})

## Summaries update only while visible (expression_summary_viewport_request).
## Scroll on every poll: the page may still grow and push the gate back out.
viewer_show_expression_summary <- function(app) {
  app$wait_for_js(
    paste0(
      "(() => {document.getElementById('expression_summary_gate')",
      "?.scrollIntoView();",
      "return Shiny.shinyapp.$inputValues",
      ".expression_summary_viewport_request === true;})()"
    ),
    timeout = 60000
  )
}

## Wait for summary content. A repaint (e.g. switching to RGB) can resize the
## projection after the summary was scrolled into view and push it out again,
## where it intentionally stops updating; keep it in view while waiting.
viewer_wait_expression_summary <- function(app, condition) {
  app$wait_for_js(
    paste0(
      "(() => {document.getElementById('expression_summary_gate')",
      "?.scrollIntoView();",
      "const text=document.getElementById('expression_by_group')?.innerText||'';",
      "return (",
      condition,
      ");})()"
    ),
    timeout = 60000
  )
}

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

  app$run_js(paste0(
    "window.genePanelErrors = [];",
    "window.addEventListener('error', e => genePanelErrors.push(e.message));",
    "window.addEventListener('unhandledrejection', ",
    "e => genePanelErrors.push(String(e.reason)));"
  ))

  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  app$wait_for_js(
    "document.getElementById('expression_genes_input') !== null",
    timeout = 60000
  )
  expect_false(app$get_js(
    "document.querySelector('#expression_details_selected_cells_UI h3') !== null"
  ))
  expect_false(app$get_js(
    "document.querySelector('#expression_in_selected_cells_UI h3') !== null"
  ))

  viewer_set_selectize(app, "expression_genes_input", "MS4A1")
  app$wait_for_js(
    paste0(
      "String(window.Shiny?.shinyapp?.$inputValues?",
      ".expression_projection_rendered_key || '').includes('MS4A1')"
    ),
    timeout = 60000
  )
  viewer_show_expression_summary(app)
  app$wait_for_js(
    "document.querySelector('#expression_by_group_UI h3') !== null",
    timeout = 60000
  )
  expect_false(app$get_js(
    "document.querySelector('#expression_by_gene_UI h3') !== null"
  ))

  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_projection_cell_view_host canvas:not(.cv-mini)') !== null"
    ),
    timeout = 60000
  )
  drag <- app$get_js(paste0(
    "(() => {const host=document.getElementById(",
    "'expression_projection_cell_view_host');",
    "host.scrollIntoView();",
    "host.querySelector('.cv-tbtn[data-act=\"box\"]').click();",
    "const r=host.querySelector(",
    "'.cv-canvas-wrap > canvas:not(.cv-mini)').getBoundingClientRect();",
    "return {x1:r.left+r.width*.08,y1:r.top+r.height*.08,",
    "x2:r.right-r.width*.08,y2:r.bottom-r.height*.08};})()"
  ))
  viewer_drag_mouse(app, drag$x1, drag$y1, drag$x2, drag$y2)
  app$wait_for_js(
    paste0(
      "(window.Shiny?.shinyapp?.$inputValues?",
      ".expression_projection_persistent_selection?.ids?.length || 0) > 0"
    ),
    timeout = 60000
  )
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_in_selected_cells_UI h3') !== null"
    ),
    timeout = 60000
  )
  expect_true(app$get_js(
    "document.querySelector('#expression_details_selected_cells_UI h3') !== null"
  ))
  app$click(selector = "#expression_projection_clear_selection")
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_in_selected_cells_UI h3') === null"
    ),
    timeout = 60000
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
      "(() => {const panes = Array.from(document.querySelectorAll(",
      "'#expression_projection_cell_view_host .cv-pane:not(.cv-hidden)'));",
      "return panes.length === 2 && panes.every(p => ",
      "Number(p.querySelector('.cv-canvas-wrap > canvas:not(.cv-mini)')",
      "?.dataset.pointCount) > 0) && ",
      "String(Shiny.shinyapp.$inputValues.expression_projection_rendered_key)",
      ".startsWith('separate');})()"
    ),
    timeout = 20000
  )
  ## The box selection above scrolled the projection into view.
  viewer_wait_expression_summary(app, "text.includes('MS4A1')")
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
  viewer_wait_expression_summary(
    app,
    paste0(
      "text.includes('R · MS4A1') && text.includes('G · CD3D') && ",
      "!text.includes('B ·')"
    )
  )
  expect_length(app$get_js("genePanelErrors"), 0L)
})
