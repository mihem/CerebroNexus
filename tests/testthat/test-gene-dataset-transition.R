test_that("dataset changes bypass pending gene edits and remap canonical names immediately", {
  shiny::testServer(function(input, output, session) {
    sys.source(viewer_test_path("utility_functions.R"), environment())
    list_of_genes <- reactiveVal(c("A", "B"))
    sys.source(viewer_test_path("gene_expression", "obj_selected_genes.R"), environment())
  }, {
    session$setInputs(expression_analysis_mode = "Gene(s)", expression_genes_input = "A")
    expect_identical(as.character(expression_selected_genes()$genes_to_display_present), "A")
    session$setInputs(expression_genes_input = c("A", "B"))
    expect_identical(as.character(expression_selected_genes()$genes_to_display_present), "A")
    list_of_genes(c("a", "b", "C"))
    session$flushReact()
    expect_identical(as.character(expression_selected_genes()$genes_to_display_present), c("a", "b"))
    list_of_genes(c("B", "C"))
    session$flushReact()
    expect_identical(as.character(expression_selected_genes()$genes_to_display_present), "B")
    expect_identical(expression_selected_genes()$genes_to_display_missing, "A")
    session$elapse(1000)
    expect_identical(as.character(expression_selected_genes()$genes_to_display_present), "B")
  })
})

test_that("mounted gene selections and completion survive a dataset round trip", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  inst_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(inst_dir))
  app <- shinytest2::AppDriver$new(inst_dir, name = "gene_dataset_transition",
    height = 700, width = 1440, load_timeout = 60000)
  withr::defer(app$stop())
  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  genes <- c("AAK1", "ABCB9", "AC004687.1", "AC009123.1")
  viewer_set_selectize(app, "expression_genes_input", genes)
  app$set_inputs(expression_projection_genes_in_separate_panels = "separate", wait_ = FALSE)
  settled <- paste0("(() => {const painted=Shiny.shinyapp.$inputValues.expression_projection_rendered_state;",
    "return painted && CerebroDatasetContext.accepts(painted) && painted.render_key.startsWith('separate') && ",
    "document.getElementById('cerebro-dataset-loading').hidden && ",
    "!document.getElementById('expression_feedback').classList.contains('is-pending');})()")
  app$wait_for_js(settled, timeout = 30000)
  app$run_js(paste0("window.geneChanges=[];window.geneErrors=[];",
    "jQuery(document).on('change.geneRoundTrip','#expression_genes_input',function(){",
    "geneChanges.push(this.selectize.getValue().slice())});",
    "window.addEventListener('error',e=>geneErrors.push(e.message));"))
  for (dataset in c("Mouse brain (Visium)", "PBMC - Full (T+B)")) {
    old_context <- app$get_js("CerebroDatasetContext.current()")
    app$set_inputs(crb_file_selector = dataset, wait_ = FALSE)
    app$wait_for_js(sprintf("CerebroDatasetContext.current()?.generation > %d", old_context$generation),
      timeout = 30000)
    app$wait_for_js(settled, timeout = 30000)
    expect_identical(tolower(unlist(app$get_js(
      "document.getElementById('expression_genes_input').selectize.getValue()"))), tolower(genes))
  }
  expect_true(all(lengths(app$get_js("geneChanges")) == 4L))
  expect_equal(app$get_js("document.querySelectorAll('#expression_projection_cell_view_host .cv-pane:not(.cv-hidden)').length"), 4)
  expect_length(app$get_js("geneErrors"), 0L)
  ## Summary work follows current visibility, including after a previous visit.
  app$run_js("document.getElementById('expression_summary_gate').scrollIntoView()")
  app$wait_for_js("Shiny.shinyapp.$inputValues.expression_summary_viewport_request === true", timeout = 30000)
  app$run_js("window.scrollTo(0,0);document.querySelector('.content-wrapper').scrollTop=0")
  app$wait_for_js("Shiny.shinyapp.$inputValues.expression_summary_viewport_request === false", timeout = 30000)
})

test_that("summaries wait for visibility without recomputing on scroll alone", {
  shiny::testServer(function(input, output, session) {
    scope <- environment()
    reads <- 0L
    expression_projection_summary_ready <- function() TRUE
    expression_projection_data <- function() data.frame(cluster = c("a", "b"))
    expression_summary_data <- reactive({
      input$revision
      list(series = list(list(values = c(0, 1))))
    })
    reactive_colors <- function() list(cluster = c(a = "red", b = "blue"))
    plotExpressionSummary <- function(...) {
      reads <<- reads + 1L
      plotly::plot_ly(x = 1:2, y = 1:2)
    }
    viewer_server_load(viewer_server_statements("gene_expression", "UI_expression_by_group.R"),
      scope, names = "output$expression_by_group")
  }, {
    session$setInputs(expression_summary_viewport_request = FALSE, revision = 1,
      expression_by_group_selected_group = "cluster")
    expect_error(output$expression_by_group, class = "shiny.silent.error")
    expect_identical(scope$reads, 0L)
    session$setInputs(expression_summary_viewport_request = TRUE)
    original <- output$expression_by_group
    expect_identical(scope$reads, 1L)
    session$setInputs(expression_summary_viewport_request = FALSE)
    expect_identical(output$expression_by_group, original)
    session$setInputs(expression_summary_viewport_request = TRUE)
    expect_identical(output$expression_by_group, original)
    expect_identical(scope$reads, 1L)
    session$setInputs(expression_summary_viewport_request = FALSE, revision = 2)
    expect_error(output$expression_by_group, class = "shiny.silent.error")
    expect_identical(scope$reads, 1L)
    session$setInputs(expression_summary_viewport_request = TRUE)
    output$expression_by_group
    expect_identical(scope$reads, 2L)
  })
})

test_that("a dataset switch notifies gene consumers once", {
  shiny::testServer(function(input, output, session) {
    scope <- environment()
    sys.source(viewer_test_path("utility_functions.R"), environment())
    list_of_genes <- reactiveVal(c("A", "B"))
    sys.source(viewer_test_path("gene_expression", "obj_selected_genes.R"), environment())
    runs <- 0L
    observe({
      expression_selected_genes()
      runs <<- runs + 1L
    })
  }, {
    session$setInputs(expression_analysis_mode = "Gene(s)", expression_genes_input = "A")
    session$elapse(1000)
    before <- scope$runs
    list_of_genes(c("A", "B", "C"))
    session$flushReact()
    session$elapse(1000)
    expect_identical(scope$runs, before)
    list_of_genes(c("a", "b"))
    session$flushReact()
    session$elapse(1000)
    expect_identical(scope$runs, before + 1L)
    expect_identical(as.character(expression_selected_genes()$genes_to_display_present), "a")
  })
})
