test_that("mean-only expression groups are reachable without expression reads", {
  shiny::testServer(
    function(input, output, session) {
      getGroupsWithMostExpressedGenes <- function() "percent-group"
      getGroupsWithMeanExpression <- function() c("mean-only", "percent-group")
      getMetaData <- function() stop("must not read cells for control choices")
      sys.source(
        viewer_test_path("most_expressed_genes", "select_group.R"),
        environment()
      )
    },
    {
      html <- output$most_expressed_genes_select_group_UI$html
      expect_match(html, 'value="mean-only"', fixed = TRUE)
      expect_equal(
        lengths(regmatches(
          html,
          gregexpr('value="percent-group"', html, fixed = TRUE)
        )),
        1L
      )
    }
  )
})

test_that("enrichment method changes preserve valid choices and reject stale methods", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sent <- NULL
      session$sendInputMessage <- function(id, message) sent <<- message
      getMethodsForEnrichedPathways <- function() c("Enrichr", "GSVA", "empty")
      getGroupsWithEnrichedPathways <- function(method) {
        switch(
          method,
          Enrichr = c("cluster", "sample"),
          GSVA = "sample",
          empty = character()
        )
      }
      viewer_result_page_load("enriched_pathways", scope)
    },
    {
      session$flushReact()
      initial <- output$enriched_pathways_select_method_and_table_UI$html
      expect_match(
        initial,
        'id="enriched_pathways_selected_table"',
        fixed = TRUE
      )
      session$setInputs(
        enriched_pathways_selected_method = "Enrichr",
        enriched_pathways_selected_table = "sample"
      )
      session$setInputs(enriched_pathways_selected_method = "GSVA")
      expect_identical(scope$sent$value, "sample")
      expect_identical(
        output$enriched_pathways_select_method_and_table_UI$html,
        initial
      )
      session$setInputs(enriched_pathways_selected_method = "empty")
      expect_length(scope$sent$value, 0L)
      before <- scope$sent
      session$setInputs(
        enriched_pathways_selected_method = "old-dataset-method"
      )
      expect_identical(scope$sent, before)
    }
  )
})

test_that("stored expression metric preference survives a grouping change", {
  expressions <- as.list(parse(viewer_test_path(
    "most_expressed_genes",
    "table.R"
  )))
  wanted <- c(
    "most_expressed_genes_available_groups",
    "most_expressed_genes_selected_tables",
    "most_expressed_genes_selected_table",
    "most_expressed_genes_metric_description",
    'output[["most_expressed_genes_table_or_text_UI"]]'
  )
  definitions <- Filter(
    function(e) {
      is.call(e) &&
        as.character(e[[1L]])[1] %in% c("<-", "=") &&
        viewer_server_key(e[[2L]]) %in%
          vapply(
            wanted,
            function(key) viewer_server_key(parse(text = key)[[1]]),
            character(1)
          )
    },
    expressions
  )
  shiny::testServer(
    function(input, output, session) {
      getGroupsWithMostExpressedGenes <- function() c("A", "B")
      getGroupsWithMeanExpression <- function() c("A", "B", "mean-only")
      viewerGetMostExpressedGenes <- function(group) {
        data.frame(group = "x", gene = "Gad1", pct = 0.5)
      }
      getMeanExpression <- function(group) {
        data.frame(group = "x", gene = "Gad1", mean_expr = 1.5)
      }
      for (definition in definitions) {
        eval(definition, environment())
      }
    },
    {
      session$setInputs(
        most_expressed_genes_selected_group = "A",
        most_expressed_genes_metric_type = "mean_expr"
      )
      session$setInputs(most_expressed_genes_selected_group = "B")
      expect_match(
        output$most_expressed_genes_table_or_text_UI$html,
        'value="mean_expr" selected',
        fixed = TRUE
      )
      session$setInputs(most_expressed_genes_selected_group = "mean-only")
      expect_match(
        output$most_expressed_genes_table_or_text_UI$html,
        'value="mean_expr" selected',
        fixed = TRUE
      )
      expect_no_match(
        output$most_expressed_genes_table_or_text_UI$html,
        'value="pct"',
        fixed = TRUE
      )
      session$setInputs(most_expressed_genes_selected_group = "stale")
      expect_error(
        most_expressed_genes_selected_tables(),
        class = "shiny.silent.error"
      )
    }
  )
})

test_that("ambiguous legacy pct values reach the table without unit conversion", {
  expressions <- as.list(parse(viewer_test_path(
    "most_expressed_genes",
    "table.R"
  )))
  wanted <- c(
    "most_expressed_genes_available_groups",
    "most_expressed_genes_selected_tables",
    "most_expressed_genes_selected_table",
    'output[["most_expressed_genes_table"]]'
  )
  definitions <- Filter(
    function(e) {
      is.call(e) &&
        as.character(e[[1L]])[1] %in% c("<-", "=") &&
        viewer_server_key(e[[2L]]) %in%
          vapply(
            wanted,
            function(key) viewer_server_key(parse(text = key)[[1]]),
            character(1)
          )
    },
    expressions
  )
  shiny::testServer(
    function(input, output, session) {
      captured <- new.env(parent = emptyenv())
      getGroupsWithMostExpressedGenes <- function() c("fraction", "percent")
      getGroupsWithMeanExpression <- function() character()
      getMetaData <- function() stop("must not materialize metadata")
      viewerGetMostExpressedGenes <- function(group) {
        data.frame(
          group = "x",
          gene = c("A", "B"),
          pct = if (group == "fraction") c(0.000123456789, 0.5) else c(0.5, 50)
        )
      }
      cerebroFilterResultRows <- function(results_df, ...) results_df
      cerebroResultTable <- function(
        results_df,
        ...,
        number_formatting = TRUE,
        columns_percentage = NULL
      ) {
        captured$result <- list(
          data = results_df,
          formatting = number_formatting,
          percentage = columns_percentage
        )
        DT::datatable(results_df)
      }
      for (definition in definitions) {
        eval(definition, environment())
      }
    },
    {
      session$setInputs(
        most_expressed_genes_selected_group = "fraction",
        most_expressed_genes_metric_type = "pct",
        most_expressed_genes_table_filter_switch = TRUE
      )
      invisible(output$most_expressed_genes_table)
      expect_identical(
        captured$result$data[["Stored score"]],
        c(0.000123456789, 0.5)
      )
      expect_false(captured$result$formatting)
      expect_null(captured$result$percentage)
      session$setInputs(most_expressed_genes_selected_group = "percent")
      invisible(output$most_expressed_genes_table)
      expect_identical(captured$result$data[["Stored score"]], c(0.5, 50))
      expect_false(captured$result$formatting)
      expect_null(captured$result$percentage)
    }
  )
})

test_that("Abundance waits for dynamic controls before choosing a renderer", {
  definition <- viewer_r_assignment(
    viewer_test_path("immune_repertoire", "visualizations.R"),
    "output.ir_plot_clonalAbundance"
  )
  # Evaluate the production renderPlotly body, without the output/cache wrapper.
  scope <- new.env(parent = environment())
  scope$req <- shiny::req
  scope$req_plot_space <- function(...) NULL
  scope$ir_data <- function() list(sample = data.frame(CTgene = "a"))
  scope$parameters <- list(chain = NULL, cloneCall = NULL, groupBy = NULL)
  scope$ir_params <- function() scope$parameters
  scope$ir_param <- function(id, default) default
  scope$ir_order_by <- function() NULL
  scope$req_scRepertoire <- function() {
    stop("must not send unbound controls upstream")
  }
  scope$ir_clonecall_col <- identity
  scope$ir_clonal_abundance_counts <- function(data, clone_col) clone_col
  scope$ir_clonal_abundance_plotly <- identity
  render <- eval(
    call("function", pairlist(), definition[[3L]][[2L]][[2L]]),
    scope
  )
  expect_error(render(), class = "shiny.silent.error")
  scope$parameters$chain <- "both"
  expect_error(render(), class = "shiny.silent.error")
  scope$parameters$cloneCall <- "gene"
  expect_identical(render(), "gene")
  scope$parameters$cloneCall <- character()
  expect_error(render(), class = "shiny.silent.error")
})
