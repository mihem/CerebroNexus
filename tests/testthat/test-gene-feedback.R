test_that("Gene feedback uses canonical keys without preparing expression", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      key_function <- viewer_r_assignment(
        viewer_test_path(
          "gene_expression",
          "obj_projection_expression_levels.R"
        ),
        "expressionProjectionRenderKey"
      )
      eval(key_function, scope)
      viewer_loaded_dataset_context <- function() {
        list(epoch = "e", dataset_key = "A", generation = 1L)
      }
      expression_selected_genes_input <- reactive(list(
        genes_to_display_present = input$canonical
      ))
      expression_projection_expression_levels <- function() {
        stop("must not prepare expression")
      }
      getMetaData <- function() stop("must not load metadata")
      sent <- list()
      session$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type, message)
      }
      sys.source(viewer_test_path("gene_expression", "event_feedback.R"), scope)
    },
    {
      session$setInputs(
        sidebar = "geneExpression",
        expression_analysis_mode = "Gene(s)",
        expression_projection_genes_in_separate_panels = "single",
        expression_genes_input = "gad1",
        canonical = "Gad1"
      )
      message <- tail(scope$sent, 1)[[1]]
      expect_identical(message[[1]], "cerebro_gene_feedback")
      expect_identical(message[[2]]$controls$expression_genes_input, "gad1")
      expect_identical(
        message[[2]]$render_key,
        expressionProjectionRenderKey(
          list(genes_to_display_present = "Gad1"),
          "single"
        )
      )
      session$setInputs(
        expression_genes_input = "missing",
        canonical = character()
      )
      expect_identical(
        tail(scope$sent, 1)[[1]][[2]]$render_key,
        expressionProjectionRenderKey(
          list(genes_to_display_present = character()),
          "single"
        )
      )
      count <- length(scope$sent)
      session$setInputs(sidebar = "overview", canonical = "other")
      expect_length(scope$sent, count)
    }
  )
})

test_that("Gene feedback survives rapid input, missing genes and dataset generations", {
  expect_node_fixture(
    "gene-feedback.js",
    viewer_test_path("www", "gene-feedback.js"),
    pass = "Gene feedback assertions passed"
  )
})
