test_that("Gene progress closes when hidden and does not reopen painted keys", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      sys.source(
        viewer_test_path("gene_expression", "func_expression_summary.R"),
        scope
      )
      expression_projection_cells_to_show <- reactive({
        input$revision
        1:2
      })
      expression_projection_coordinates <- function() {
        data.frame(x = 1:2, y = 2:1)
      }
      expression_selected_genes <- function() {
        list(genes_to_display_present = "g1")
      }
      getGeneNames <- function() "g1"
      data_set <- function() NULL
      root <- getOption(
        "gene.lifecycle.source",
        viewer_test_path("gene_expression")
      )
      sys.source(file.path(root, "obj_projection_expression_levels.R"), scope)
      reads <- 0L
      expressionProjectionCachedRow <- function(...) {
        reads <<- reads + 1L
        c(0, 2)
      }
    },
    {
      session$setInputs(
        sidebar = "geneExpression",
        revision = 1,
        expression_projection_genes_in_separate_panels = "single",
        expression_projection_render_request = list(nonce = 1)
      )
      expect_identical(expression_projection_expression_levels(), c(0, 2))
      expect_false(is.null(scope$expression_projection_progress_handle))
      key <- expression_projection_render_key()
      ## A stale acknowledgement cannot close a newer operation's progress.
      session$setInputs(expression_projection_rendered_key = "obsolete")
      expect_identical(scope$expression_projection_progress_key, key)
      session$setInputs(sidebar = "overview")
      expect_null(scope$expression_projection_progress_handle)
      expression_projection_expression_levels()
      expect_identical(scope$reads, 1L)
      session$setInputs(sidebar = "geneExpression")
      expression_projection_expression_levels()
      expect_identical(scope$reads, 1L)
      ## A later request snapshot also must not invalidate expression values.
      session$setInputs(expression_projection_render_request = list(nonce = 2))
      expression_projection_expression_levels()
      expect_identical(scope$reads, 1L)
      session$setInputs(expression_projection_rendered_key = key, revision = 2)
      expression_projection_expression_levels()
      expect_null(scope$expression_projection_progress_handle)
      expect_identical(scope$reads, 2L)
      ## No request yet, or hidden recalculation, must never open global progress.
      session$setInputs(
        expression_projection_render_request = NULL,
        revision = 3,
        expression_projection_rendered_key = "obsolete"
      )
      expression_projection_expression_levels()
      expect_null(scope$expression_projection_progress_handle)
      session$setInputs(
        sidebar = "overview",
        revision = 4,
        expression_projection_render_request = list(nonce = 3)
      )
      expression_projection_expression_levels()
      expect_null(scope$expression_projection_progress_handle)
    }
  )
})

test_that("Gene rejects obsolete group columns and clears rather than retaining output", {
  root <- getOption(
    "gene.lifecycle.source",
    viewer_test_path("gene_expression")
  )
  expressions <- parse(file.path(root, "UI_expression_by_group.R"))
  assignment <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1]], as.name("<-")) &&
        is.call(x[[2]]) &&
        identical(x[[2]][[1]], as.name("[[")) &&
        identical(x[[2]][[3]], "expression_by_group")
    },
    as.list(expressions)
  )[[1]]
  body <- assignment[[3]][[2]]
  env <- new.env(parent = globalenv())
  env$req <- shiny::req
  env$input <- list(
    expression_summary_viewport_request = 1,
    expression_by_group_selected_group = "old_group"
  )
  env$expression_projection_summary_ready <- function() TRUE
  env$expression_projection_data <- function() {
    data.frame(new_group = c("a", "b"))
  }
  env$expression_summary_data <- function() {
    list(series = list(list(values = c(0, 1))))
  }
  env$reactive_colors <- function() list(new_group = c(a = "red", b = "blue"))
  env$plotExpressionSummary <- function(series, groups, colors) groups
  error <- tryCatch(eval(body, env), error = identity)
  expect_s3_class(error, "shiny.silent.error")
  expect_false(inherits(error, "shiny.output.cancel"))
  env$input$expression_by_group_selected_group <- "new_group"
  expect_identical(eval(body, env), c("a", "b"))
  env$expression_projection_data <- function() {
    data.frame(new_group = I(matrix(1:4, nrow = 2)))
  }
  expect_error(eval(body, env), class = "shiny.silent.error")
})
