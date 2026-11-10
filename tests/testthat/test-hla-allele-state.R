test_that("HLA tabs retain one allele and use it for both network colours and overlap counts", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("stringdist")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      updates <- list()
      session$sendInputMessage <- function(inputId, message) {
        updates[[inputId]] <<- message
      }
      typing <- hla_normalize_typing(
        data.frame(
          sample = rep(c("s1", "s2"), each = 2),
          locus = "HLA-A",
          allele = c("HLA-A*02:01", "HLA-A*11:01", "HLA-A*01:01", "HLA-A*11:01")
        ),
        source_type = "genotyped"
      )
      hla_allele_choices <- reactive({
        if (isTRUE(input$new_dataset)) {
          c("HLA-A*11:01")
        } else {
          c("HLA-A*02:01", "HLA-A*01:01")
        }
      })
      hla_param <- function(name, default) {
        x <- input[[name]]
        if (is.null(x)) default else x
      }
      hla_scope_mode <- function() "all"
      hla_has_typing <- hla_has_deps <- function() TRUE
      hla_active_typing <- function() typing
      hla_ir_samples <- function() c("s1", "s2")
      hla_segments <- function() {
        data.frame(
          sample = c("s1", "s1", "s2"),
          cdr3 = c("CASSL", "OTHER", "OTHER")
        )
      }
      hla_selected_feature_members <- function() {
        data.frame(cdr3 = "CASSL", v_gene = "TRBV1")
      }
      hla_by_v_default <- function() FALSE
      hla_active_chain <- function() "TRB"
      hla_unit_noun <- function() "cell"
      hla_celltype_col <- function() NA_character_
      hla_selection_caveat <- function() NULL
      hla_first_frame_graph_snapshot <- function() NULL
      graph <- igraph::make_ring(2)
      attrs <- list(
        name = c("node1", "node2"),
        cdr3 = c("CASSL", "OTHER"),
        cluster = c(1, 1),
        clone_count = c(1, 1),
        samples_all = list("s1", "s2"),
        layout_x = c(0, 1),
        layout_y = c(1, 0)
      )
      for (name in names(attrs)) {
        graph <- igraph::set_vertex_attr(graph, name, value = attrs[[name]])
      }
      hla_motif_graph <- reactive(graph)
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        scope,
        names = c("hla_allele_state", "hla_color_allele"),
        events = c(
          "hla_color_allele",
          "hla_association_allele",
          "hla_allele_state"
        )
      )
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "settings.R"),
        scope,
        names = c("HLA_TWO_LINE_RENDER", "output$hla_color_allele_ui")
      )
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "associations.R"),
        scope,
        names = c(
          "hla_label_hint",
          "hla_association_page_requested",
          "hla_association_consumer_ready",
          "hla_overlap_table",
          "output$hla_associations_ui"
        )
      )
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "visualizations.R"),
        scope,
        names = c(
          "HLA_MOTIF_MAX_LEGEND_CLUSTERS",
          "HLA_CARRIER_LEVELS",
          "HLA_CARRIER_COLORS",
          "HLA_CONTEXT_LEVELS",
          "HLA_CONTEXT_COLORS",
          "hla_build_motif_visnet",
          "hla_visnet"
        )
      )
    },
    {
      session$setInputs(
        sidebar = "hla_tcr_motifs",
        hla_tabs = "HLA Associations",
        hla_associations_render_request = 1,
        hla_color_by = "hla_carrier"
      )
      first <- hla_visnet()
      expect_identical(first$legend_title, "HLA-A*02:01")
      first_table <- hla_overlap_table()
      expect_identical(
        first_table$hla_status[first_table$analysis_unit == "s1"],
        "carrier"
      )
      session$setInputs(hla_association_allele = "HLA-A*01:01")
      expect_identical(hla_visnet()$legend_title, "HLA-A*01:01")
      expect_identical(hla_visnet()$nodes$color, rev(first$nodes$color))
      second_table <- hla_overlap_table()
      expect_identical(
        second_table$hla_status[second_table$analysis_unit == "s1"],
        "non-carrier"
      )
      expect_identical(
        second_table$hla_status[second_table$analysis_unit == "s2"],
        "carrier"
      )
      expect_identical(
        second_table$n_feature_cells,
        first_table$n_feature_cells
      )
      expect_identical(scope$updates$hla_color_allele$value, "HLA-A*01:01")
      # Removing a hidden picker must not erase the user's choice.
      session$setInputs(
        hla_tabs = "Motif Network",
        hla_association_allele = "",
        hla_color_allele = NULL
      )
      expect_identical(hla_color_allele(), "HLA-A*01:01")
      session$setInputs(hla_color_allele = "HLA-A*02:01")
      expect_identical(
        scope$updates$hla_association_allele$value,
        "HLA-A*02:01"
      )
      session$setInputs(hla_tabs = "HLA Associations", hla_color_allele = "")
      expect_identical(hla_overlap_table(), first_table)
      expect_identical(hla_visnet()$nodes$color, first$nodes$color)
      # Newly created controls seed from the current shared selection.
      session$setInputs(
        hla_association_allele = "HLA-A*01:01",
        new_dataset = TRUE
      )
      expect_identical(hla_color_allele(), "HLA-A*11:01")
      expect_identical(hla_visnet()$legend_title, "HLA-A*11:01")
      expect_true(all(hla_overlap_table()$hla_status == "carrier"))
      expect_match(
        output$hla_color_allele_ui$html,
        'value="HLA-A*11:01" selected',
        fixed = TRUE
      )
      expect_match(
        output$hla_associations_ui$html,
        'value="HLA-A*11:01" selected',
        fixed = TRUE
      )
    }
  )
})
