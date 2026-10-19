test_that("HLA graphs recover from unbound controls and cache by the actual cohort", {
  withr::local_package("magrittr")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      available_crb_files <- reactiveValues(selected = "dataset-A")
      data_set <- reactive(if (isTRUE(input$loaded)) "loaded" else NULL)
      hla_first_frame_graph_matches <- function() FALSE
      hla_first_frame <- function() NULL
      hla_scope_mode <- function() input$scope %||% "all"
      hla_active_chain <- function() input$chain %||% "TRB"
      hla_by_v_default <- function() FALSE
      hla_node_meta_cols <- function() "sample"
      hla_default_min_nodes <- function() 1L
      hla_filter_key <- function() input$filter %||% "all"
      hla_color_allele <- function() "HLA-A*02:01"
      hla_celltype_col <- function() input$lineage %||% "cell_type"
      hla_allele_choices <- function() {
        if (isTRUE(input$no_alleles)) {
          character()
        } else {
          c("HLA-A*02:01", "HLA-DRB1*01:01")
        }
      }
      hla_active_typing <- reactive({
        hla_normalize_typing(
          data.frame(
            sample = c("s1", "s2"),
            locus = "HLA-A",
            allele = if (isTRUE(input$swap_carriers)) {
              c("HLA-A*01:01", "HLA-A*02:01")
            } else {
              c("HLA-A*02:01", "HLA-A*01:01")
            }
          ),
          source_type = "genotyped"
        )
      })
      hla_segments <- reactive({
        rows <- data.frame(sample = c("s1", "s2"), cdr3 = c("AAA", "BBB"))
        if (identical(hla_filter_key(), "s1")) {
          rows <- rows[rows$sample == "s1", , drop = FALSE]
        }
        rows
      })
      builds <- 0L
      hla_build_graph_raw_from <- function(segments) {
        builds <<- builds + 1L
        segments
      }
      hla_finalize_motif_graph <- function(raw, min_nodes, show_isolated) {
        list(cdr3 = raw$cdr3, minimum = min_nodes, isolated = show_isolated)
      }
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        scope,
        names = c(
          "hla_param",
          "hla_page_active",
          "hla_class_allele_choices",
          "hla_pair_available",
          "hla_pair_allele_i",
          "hla_pair_allele_ii",
          "hla_scoped_segments",
          "hla_scope_key",
          "hla_params_ready",
          "hla_bindCache",
          "hla_min_nodes_debounced",
          "hla_graph_cache_key",
          "hla_motif_graph_raw_cached",
          "hla_motif_graph_cached",
          "hla_motif_graph",
          "hla_global_motif_graph_raw_cached",
          "hla_global_motif_graph_cached",
          "hla_global_motif_graph"
        )
      )
    },
    {
      session$setInputs(sidebar = "hla_tcr_motifs", loaded = FALSE)
      expect_error(hla_motif_graph(), class = "shiny.silent.error")
      expect_identical(scope$builds, 0L)
      session$setInputs(
        hla_color_by = "",
        hla_min_nodes = 1L,
        hla_by_v = FALSE,
        hla_show_isolated = FALSE
      )
      session$elapse(500)
      expect_error(hla_motif_graph(), class = "shiny.silent.error")
      session$setInputs(loaded = TRUE)
      expect_identical(hla_motif_graph()$cdr3, c("AAA", "BBB"))
      expect_identical(scope$builds, 1L)
      session$setInputs(hla_color_by = "sample", hla_min_nodes = 2L)
      session$elapse(500)
      expect_identical(hla_motif_graph()$minimum, 2L)
      expect_identical(scope$builds, 1L)
      session$setInputs(scope = "allele", hla_color_allele = NULL)
      expect_error(hla_motif_graph(), class = "shiny.silent.error")
      expect_identical(scope$builds, 1L)
      session$setInputs(hla_color_allele = "HLA-A*02:01")
      expect_identical(hla_motif_graph()$cdr3, "AAA")
      expect_identical(scope$builds, 2L)
      session$setInputs(swap_carriers = TRUE)
      expect_identical(hla_motif_graph()$cdr3, "BBB")
      expect_identical(scope$builds, 3L)
      expect_identical(hla_global_motif_graph()$cdr3, c("AAA", "BBB"))
      session$setInputs(scope = "all", filter = "s1")
      expect_identical(hla_motif_graph()$cdr3, "AAA")
      before <- scope$builds
      scope$available_crb_files$selected <- "dataset-B"
      session$flushReact()
      expect_identical(hla_motif_graph()$cdr3, "AAA")
      expect_gt(scope$builds, before)
      session$setInputs(chain = "TRA")
      expect_identical(hla_motif_graph()$cdr3, "AAA")
      expect_gt(scope$builds, before + 1L)
      session$setInputs(
        scope = "pair",
        hla_pair_allele_i = NULL,
        hla_pair_allele_ii = NULL
      )
      expect_false(hla_params_ready())
      session$setInputs(hla_pair_allele_i = "HLA-A*02:01")
      expect_false(hla_params_ready())
      session$setInputs(hla_pair_allele_ii = "HLA-DRB1*01:01")
      expect_true(hla_params_ready())
      expect_true(hla_pair_available())
      session$setInputs(lineage = NA_character_)
      expect_false(hla_pair_available())
      session$setInputs(
        no_alleles = TRUE,
        hla_pair_allele_i = NULL,
        hla_pair_allele_ii = NULL
      )
      expect_true(hla_params_ready())
      expect_false(hla_pair_available())
    }
  )
})
