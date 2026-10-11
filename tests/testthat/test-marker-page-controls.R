test_that("marker selectors start together and follow valid method choices", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      catalog <- reactiveVal(list(
        A = c("cluster", "sample"),
        B = "batch",
        C = character()
      ))
      sent <- list()
      session$sendInputMessage <- function(id, message) {
        sent[[length(sent) + 1L]] <<- list(id = id, message = message)
      }
      getMethodsForMarkerGenes <- function() names(catalog())
      getGroupsWithMarkerGenes <- function(method) catalog()[[method]]
      getMetaData <- function() stop("control creation must not read metadata")
      sys.source(viewer_test_path("marker_genes", "select_content.R"), scope)
    },
    {
      session$flushReact()
      initial <- output$marker_genes_select_method_and_table_UI$html
      expect_match(initial, 'id="marker_genes_selected_method"', fixed = TRUE)
      expect_match(initial, 'id="marker_genes_selected_table"', fixed = TRUE)
      expect_match(initial, 'value="cluster" selected', fixed = TRUE)
      session$setInputs(
        marker_genes_selected_method = "A",
        marker_genes_selected_table = "sample"
      )
      expect_identical(tail(scope$sent, 1)[[1]]$message$value, "sample")
      session$setInputs(marker_genes_selected_method = "B")
      expect_identical(tail(scope$sent, 1)[[1]]$message$value, "batch")
      expect_identical(
        output$marker_genes_select_method_and_table_UI$html,
        initial
      )
      session$setInputs(marker_genes_selected_method = "C")
      expect_length(tail(scope$sent, 1)[[1]]$message$value, 0L)
      before <- length(scope$sent)
      session$setInputs(marker_genes_selected_method = "stale-method")
      expect_length(scope$sent, before)
      catalog(list(D = "new-group"))
      session$flushReact()
      updated <- output$marker_genes_select_method_and_table_UI$html
      expect_match(updated, 'value="D" selected', fixed = TRUE)
      expect_match(updated, 'value="new-group" selected', fixed = TRUE)
      expect_no_match(updated, 'value="cluster"', fixed = TRUE)
      catalog(list())
      session$flushReact()
      expect_match(
        output$marker_genes_select_method_and_table_UI$html,
        "No marker-gene methods available.",
        fixed = TRUE
      )
    }
  )
})

test_that("marker result shell distinguishes missing and empty results", {
  expressions <- as.list(parse(viewer_test_path("marker_genes", "table.R")))
  result_ui <- Filter(
    function(e) {
      is.call(e) &&
        identical(e[[1L]], quote(`<-`)) &&
        grepl(
          'marker_genes_table_or_text_UI',
          paste(deparse(e[[2L]]), collapse = ""),
          fixed = TRUE
        )
    },
    expressions
  )[[1L]]
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      catalog <- reactiveVal(list())
      result <- reactiveVal("no_markers_found")
      reads <- 0L
      getMethodsForMarkerGenes <- function() names(catalog())
      getGroupsWithMarkerGenes <- function(method) catalog()[[method]]
      getMarkerGenes <- function(...) {
        reads <<- reads + 1L
        result()
      }
      cerebroResultTableControls <- function(...) p("Table controls")
      eval(result_ui, scope)
    },
    {
      session$flushReact()
      expect_match(
        output$marker_genes_table_or_text_UI$html,
        "not included",
        fixed = TRUE
      )
      expect_identical(scope$reads, 0L)
      catalog(list(A = character()))
      session$setInputs(marker_genes_selected_method = "A")
      expect_match(
        output$marker_genes_table_or_text_UI$html,
        "No marker-gene table",
        fixed = TRUE
      )
      expect_identical(scope$reads, 0L)
      catalog(list(A = "cluster"))
      session$setInputs(marker_genes_selected_table = "cluster")
      expect_match(
        output$marker_genes_table_or_text_UI$html,
        "No marker genes meet",
        fixed = TRUE
      )
      result(NULL)
      session$flushReact()
      expect_match(
        output$marker_genes_table_or_text_UI$html,
        "No marker-gene table",
        fixed = TRUE
      )
      result(data.frame(gene = "Gad1"))
      session$flushReact()
      expect_match(
        output$marker_genes_table_or_text_UI$html,
        "Table controls",
        fixed = TRUE
      )
      before <- scope$reads
      session$setInputs(marker_genes_selected_table = "stale-group")
      expect_error(
        output$marker_genes_table_or_text_UI,
        class = "shiny.silent.error"
      )
      expect_identical(scope$reads, before)
    }
  )
})
