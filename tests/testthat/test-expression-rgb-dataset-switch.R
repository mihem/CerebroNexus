test_that("RGB channels track canonical spelling across dataset switches", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  shiny::testServer(
    function(input, output, session) {
      list_of_genes <- shiny::reactiveVal(c("Snap25", "Gad1"))
      debounceAfterFirst <- scope$debounceAfterFirst
      `%||%` <- function(x, y) if (is.null(x)) y else x
      sys.source(
        viewer_test_path("gene_expression", "obj_selected_genes.R"),
        environment()
      )
    },
    {
      session$setInputs(
        expression_analysis_mode = "Gene(s)",
        expression_projection_genes_in_separate_panels = "rgb",
        expression_rgb_gene_r = "Snap25",
        expression_rgb_gene_g = "Snap25",
        expression_rgb_gene_b = "Gad1"
      )
      expect_identical(
        expression_selected_genes_input()$rgb_genes,
        list(r = "Snap25", g = "Snap25", b = "Gad1")
      )
      list_of_genes(c("SNAP25", "GAD1"))
      session$flushReact()
      selected <- expression_selected_genes_input()
      expect_identical(
        selected$rgb_genes,
        list(r = "SNAP25", g = "SNAP25", b = "GAD1")
      )
      expect_setequal(
        as.character(selected$genes_to_display_present),
        c("SNAP25", "GAD1")
      )
      source(
        viewer_test_path("gene_expression", "func_expression_summary.R"),
        local = TRUE
      )
      spec <- expressionSummarySpec(
        "rgb",
        selected$genes_to_display_present,
        selected$rgb_genes
      )
      expect_length(spec$series, 3L)
      list_of_genes("SNAP25")
      session$flushReact()
      expect_identical(
        expression_selected_genes_input()$rgb_genes,
        list(r = "SNAP25", g = "SNAP25", b = NULL)
      )
      list_of_genes("OTHER")
      session$flushReact()
      expect_identical(
        expression_selected_genes_input()$rgb_genes,
        list(r = NULL, g = NULL, b = NULL)
      )
    }
  )
})

test_that("an empty expression summary is a valid empty widget", {
  source(
    viewer_test_path("gene_expression", "func_expression_summary.R"),
    local = TRUE
  )
  expect_s3_class(
    plotExpressionSummary(list(), character(), character()),
    "plotly"
  )
})

test_that("gene choice refresh preserves canonical selections and unknown input", {
  source(viewer_test_path("utility_functions.R"), local = TRUE)
  expect_identical(
    canonicalGeneSelection(
      c("Snap25", "Snap25", "missing"),
      c("GAD1", "SNAP25")
    ),
    c("SNAP25", "SNAP25", "missing")
  )
  expect_null(canonicalGeneSelection(NULL, c("GAD1", "SNAP25")))
  expect_identical(canonicalGeneSelection(character(), "SNAP25"), character())
})

test_that("mounted Gene selectors do not restore old input on return", {
  callbacks <- list()
  sent <- list()
  testthat::local_mocked_bindings(
    later = function(func, ...) {
      callbacks[[length(callbacks) + 1L]] <<- func
    },
    .package = "later"
  )
  shiny::testServer(
    function(input, output, session) {
      sys.source(viewer_test_path("utility_functions.R"), environment())
      data_set <- shiny::reactiveVal(list(genes = c("Gad1", "Slc17a7")))
      getGeneNames <- function() data_set()$genes
      updateSelectizeInput <- function(
        session,
        inputId,
        choices,
        selected,
        ...
      ) {
        sent[[length(sent) + 1L]] <<- list(
          choices = choices,
          selected = selected
        )
      }
      serverSideGeneSelector(
        session,
        "gene",
        canonical_selection = TRUE,
        refresh_on_activation = FALSE,
        active = function() identical(input$sidebar, "geneExpression")
      )
    },
    {
      session$setInputs(sidebar = "geneExpression", gene = "Gad1")
      lapply(callbacks, function(f) f())
      n <- length(sent)
      expect_gt(n, 0L)
      old_callbacks <- callbacks
      session$setInputs(sidebar = "coordinated_views")
      session$setInputs(sidebar = "geneExpression", gene = "Slc17a7")
      lapply(old_callbacks, function(f) f())
      expect_length(sent, n)
      expect_identical(input$gene, "Slc17a7")
      session$setInputs(gene = "")
      session$setInputs(sidebar = "coordinated_views")
      session$setInputs(sidebar = "geneExpression")
      expect_length(sent, n)
      session$setInputs(sidebar = "coordinated_views")
      data_set(list(genes = "NEW"))
      session$flushReact()
      expect_length(sent, n)
      session$setInputs(sidebar = "geneExpression")
      expect_gt(length(sent), n)
      expect_identical(tail(sent, 1)[[1]]$choices, "NEW")
      n <- length(sent)
      lapply(old_callbacks, function(f) f())
      expect_length(sent, n)
    }
  )
})

test_that("obsolete selector retries cannot replace a new dataset's choices", {
  callbacks <- list()
  recorded <- new.env(parent = emptyenv())
  recorded$sent <- list()
  testthat::local_mocked_bindings(
    later = function(func, ...) {
      callbacks[[length(callbacks) + 1L]] <<- func
    },
    .package = "later"
  )
  shiny::testServer(
    function(input, output, session) {
      sys.source(viewer_test_path("utility_functions.R"), environment())
      data_set <- shiny::reactiveVal(list(genes = c("Snap25", "Gad1")))
      getGeneNames <- function() data_set()$genes
      updateSelectizeInput <- function(
        session,
        inputId,
        choices,
        selected,
        ...
      ) {
        recorded$sent[[length(recorded$sent) + 1L]] <- list(
          choices = choices,
          selected = selected
        )
      }
      serverSideGeneSelector(session, "gene", canonical_selection = TRUE)
    },
    {
      session$setInputs(gene = "Snap25")
      old_callbacks <- callbacks
      data_set(list(genes = c("SNAP25", "GAD1")))
      session$flushReact()
      lapply(tail(callbacks, 2L), function(f) f())
      n <- length(recorded$sent)
      expect_gt(n, 0L)
      expect_identical(recorded$sent[[n]]$selected, "SNAP25")
      lapply(old_callbacks, function(f) f())
      expect_length(recorded$sent, n)
      expect_setequal(recorded$sent[[n]]$choices, c("SNAP25", "GAD1"))
      session$setInputs(gene = "")
      lapply(tail(callbacks, 2L), function(f) f())
      expect_length(recorded$sent, n)
      session$setInputs(gene = NULL)
      data_set(list(genes = "NEW"))
      session$flushReact()
      session$setInputs(gene = "")
      n <- length(recorded$sent)
      lapply(tail(callbacks, 2L), function(f) f())
      expect_gt(length(recorded$sent), n)
      expect_identical(tail(recorded$sent, 1L)[[1L]]$choices, "NEW")
    }
  )
})

test_that("a single-shot gene selector registers even when the selection moved", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  updates <- list()
  scope$updateSelectizeInput <- function(
    session,
    inputId,
    choices,
    selected,
    ...
  ) {
    updates[[length(updates) + 1L]] <<- list(
      choices = choices,
      selected = selected
    )
  }
  shiny::testServer(
    function(input, output, session) {
      data_set <- shiny::reactiveVal("first")
      environment(scope$serverSideGeneSelector) <- environment()
      updateSelectizeInput <- scope$updateSelectizeInput
      canonicalGeneSelection <- scope$canonicalGeneSelection
      scope$serverSideGeneSelector(
        session,
        "expression_rgb_gene_r",
        choices = function() c("SNAP25", "GAD1"),
        retry = FALSE,
        canonical_selection = TRUE,
        refresh_on_activation = FALSE
      )
    },
    {
      session$setInputs(expression_rgb_gene_r = "")
      session$flushReact()
      ## An RGB seed changes the channel before the delayed registration.
      session$setInputs(expression_rgb_gene_r = "Snap25")
      Sys.sleep(0.4)
      later::run_now(0)
      expect_length(updates, 1L)
      expect_identical(updates[[1]]$selected, "SNAP25")
      expect_identical(updates[[1]]$choices, c("GAD1", "SNAP25"))
    }
  )
})
