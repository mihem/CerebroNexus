test_that("HLA settings stay idle off-page and update choices without rebuilding controls", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      scans <- renders <- 0L
      updates <- list()
      session$sendInputMessage <- function(inputId, message) {
        updates[[inputId]] <<- message
      }
      hla_require_active_page <- function() {
        req(identical(input$sidebar, "hla_tcr_motifs"))
      }
      hla_tcr_chains <- function() {
        renders <<- renders + 1L
        c("TRA", "TRB")
      }
      hla_active_chain <- function() "TRB"
      hla_has_analyzable_allele <- hla_pair_available <- function() TRUE
      hla_param <- function(name, default) {
        value <- input[[name]]
        if (is.null(value)) default else value
      }
      hla_scope_mode <- function() hla_param("hla_scope", "all")
      hla_usable_color_cols <- function() "sample"
      hla_celltype_col <- function() "cell_type"
      hla_ir_samples <- function() {
        scans <<- scans + 1L
        c("donor-a", "donor-b")
      }
      hla_default_min_nodes <- function() {
        scans <<- scans + 1L
        2L
      }
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        scope,
        names = c("hla_color_by_choices", "hla_min_nodes_debounced")
      )
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "settings.R"),
        scope,
        names = c("HLA_TWO_LINE_RENDER", "output$hla_parameters_ui"),
        events = "hla_color_by_choices"
      )
    },
    {
      session$setInputs(sidebar = "overview")
      session$elapse(500)
      expect_identical(scope$scans, 0L)
      expect_identical(scope$renders, 0L)
      session$setInputs(sidebar = "hla_tcr_motifs")
      original <- output$hla_parameters_ui$html
      expect_match(original, 'id="hla_color_by"', fixed = TRUE)
      expect_identical(scope$renders, 1L)
      expect_gt(scope$scans, 0L)
      session$setInputs(
        hla_scope = "pair",
        hla_color_by = "mhc_context",
        hla_by_v = TRUE
      )
      expect_identical(output$hla_parameters_ui$html, original)
      expect_identical(scope$renders, 1L)
      expect_identical(scope$updates$hla_color_by$value, "cluster")
      expect_match(
        scope$updates$hla_color_by$options,
        "pair_allele",
        fixed = TRUE
      )
      expect_no_match(
        scope$updates$hla_color_by$options,
        "mhc_context",
        fixed = TRUE
      )
      session$setInputs(hla_scope = "all", hla_color_by = "sample")
      expect_identical(scope$updates$hla_color_by$value, "sample")
      expect_identical(scope$renders, 1L)
      session$setInputs(sidebar = "overview")
      before <- scope$scans
      session$setInputs(hla_scope = "pair", hla_min_nodes = NULL)
      session$elapse(500)
      expect_identical(scope$scans, before)
    }
  )
})

test_that("HLA network renders only on its requested subtab with a current context", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      reads <- 0L
      sent <- list()
      context <- list(epoch = "session", dataset_key = "tiny", generation = 1L)
      viewer_loaded_dataset_context <- function() context
      hla_active_chain <- function() input$chain
      hla_tcr_chains <- function() "TRB"
      hla_first_frame_graph_matches <- function() FALSE
      hla_ready_latch <- function() isTRUE(input$ready)
      hla_min_nodes_debounced <- function() 2L
      available_crb_files <- list(selected = "tiny")
      hla_visnet <- function() {
        reads <<- reads + 1L
        list(
          layout = matrix(c(1, 2, 3, 4), 2),
          nodes = data.frame(
            node_key = c("a", "b"),
            size = c(2, 3),
            color = c("red", "blue")
          ),
          edges = data.frame(from = 1L, to = 2L),
          legend_title = "Motif",
          hover_columns = list(CDR3 = c("AAA", "AAT"))
        )
      }
      cerebroFeedbackControls <- function(...) list()
      cerebroSendViewFeedback <- function(...) NULL
      cerebroCellViewRender <- function(...) {
        sent[[length(sent) + 1L]] <<- list(...)
        1L
      }
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "visualizations.R"),
        scope,
        names = c(
          "hla_motif_last_render_signature",
          "hla_motif_last_feedback_token"
        ),
        events = "hla_motif_network_render_request"
      )
    },
    {
      request <- list(dataset_context = context)
      hidden_states <- list(
        off_page = list(
          sidebar = "overview",
          hla_tabs = "Motif Network",
          chain = "TRB",
          ready = TRUE,
          hla_motif_network_render_request = request
        ),
        other_tab = list(
          sidebar = "hla_tcr_motifs",
          hla_tabs = "HLA Associations"
        ),
        no_request = list(
          hla_tabs = "Motif Network",
          hla_motif_network_render_request = NULL
        ),
        stale_request = list(
          hla_motif_network_render_request = list(
            dataset_context = modifyList(
              context,
              list(dataset_key = "old", generation = 0L)
            )
          )
        ),
        wrong_chain = list(
          hla_motif_network_render_request = request,
          chain = "IGH"
        ),
        not_ready = list(chain = "TRB", ready = FALSE)
      )
      for (state in names(hidden_states)) {
        do.call(session$setInputs, hidden_states[[state]])
        expect_identical(scope$reads, 0L, info = state)
      }
      session$setInputs(ready = TRUE)
      expect_gt(scope$reads, 0L)
      expect_length(scope$sent, 1L)
      session$setInputs(
        hla_motif_network_render_request = c(request, list(nonce = 2))
      )
      expect_length(scope$sent, 1L)
      expect_identical(scope$sent[[1]]$data$selection_key, c("a", "b"))
      expect_identical(scope$sent[[1]]$data$x, c(1, 2))
      expect_identical(scope$sent[[1]]$data$point_sizes, c(4, 6))
      expect_false(scope$sent[[1]]$data$reset_axes)
      expect_identical(scope$sent[[1]]$extra$edges, list(from = 1L, to = 2L))
      expect_identical(scope$sent[[1]]$hover$columns$CDR3, c("AAA", "AAT"))
    }
  )
})

test_that("both HLA association table consumers wait for a visible, valid feature", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      reads <- c(summary = 0L, breadth = 0L)
      sent <- list()
      session$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type, message)
      }
      hla_color_allele <- function() input$allele
      hla_allele_choices <- function() "A*02:01"
      hla_selected_feature_members <- function() {
        if (isTRUE(input$feature_ready)) {
          data.frame(cdr3 = "AAA")
        } else {
          data.frame()
        }
      }
      hla_overlap_summary_data <- function() {
        reads["summary"] <<- reads["summary"] + 1L
        data.frame(n = 1L)
      }
      hla_overlap_breadth_data <- function() {
        reads["breadth"] <<- reads["breadth"] + 1L
        data.frame(n = 2L)
      }
      hla_unit_noun <- function() "cell"
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "associations.R"),
        scope,
        names = c(
          "hla_association_page_requested",
          "hla_association_consumer_ready",
          "hla_overlap_live_table",
          "hla_overlap_summary_skeleton",
          "hla_overlap_breadth_skeleton",
          "hla_overlap_summary_proxy",
          "hla_overlap_table_proxy"
        )
      )
    },
    {
      session$setInputs(
        sidebar = "overview",
        hla_tabs = "HLA Associations",
        allele = "A*02:01",
        feature_ready = TRUE,
        hla_associations_render_request = 1
      )
      expect_equal(unname(scope$reads), c(0L, 0L))
      session$setInputs(sidebar = "hla_tcr_motifs", hla_tabs = "Motif Network")
      expect_equal(unname(scope$reads), c(0L, 0L))
      session$setInputs(
        hla_tabs = "HLA Associations",
        hla_associations_render_request = NULL
      )
      expect_equal(unname(scope$reads), c(0L, 0L))
      session$setInputs(hla_associations_render_request = 1, allele = "invalid")
      expect_equal(unname(scope$reads), c(0L, 0L))
      session$setInputs(allele = "A*02:01", feature_ready = FALSE)
      expect_equal(unname(scope$reads), c(0L, 0L))
      session$setInputs(feature_ready = TRUE)
      expect_true(all(scope$reads > 0L))
      expect_length(scope$sent, 2L)
      before <- scope$reads
      session$setInputs(sidebar = "overview", feature_ready = FALSE)
      expect_identical(scope$reads, before)
    }
  )
})

test_that("HLA association features come from the full graph and survive scope changes", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      graph <- igraph::make_ring(3)
      attrs <- list(
        name = c("a", "b", "c"),
        cdr3 = c("AAA", "AAT", "CCC"),
        v_gene = rep("TRBV1", 3),
        motif_group = c("m1", "m1", "m2")
      )
      for (name in names(attrs)) {
        graph <- igraph::set_vertex_attr(graph, name, value = attrs[[name]])
      }
      hla_global_motif_graph <- function() graph
      hla_motif_graph <- function() {
        stop("Association used the allele-filtered graph")
      }
      hla_motif_graph_ok <- function(g) igraph::vcount(g) > 0L
      hla_param <- function(name, default) {
        x <- input[[name]]
        if (is.null(x)) default else x
      }
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "associations.R"),
        scope,
        names = c(
          "hla_association_page_requested",
          "hla_feature_catalog",
          "hla_selected_feature_members"
        )
      )
    },
    {
      session$setInputs(
        sidebar = "hla_tcr_motifs",
        hla_tabs = "HLA Associations",
        hla_associations_render_request = 1,
        hla_feature_type = "motif",
        hla_feature_id = "m1"
      )
      expect_identical(hla_feature_catalog()$node_id, c("a", "b", "c"))
      expect_identical(hla_selected_feature_members()$cdr3, c("AAA", "AAT"))
      session$setInputs(hla_scope = "allele", hla_color_allele = "A*02:01")
      expect_identical(hla_selected_feature_members()$cdr3, c("AAA", "AAT"))
      session$setInputs(hla_feature_type = "node")
      expect_error(hla_selected_feature_members(), class = "shiny.silent.error")
      session$setInputs(hla_feature_id = "c")
      expect_identical(hla_selected_feature_members()$cdr3, "CCC")
    }
  )
})
