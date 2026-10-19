test_that("HLA node colours, sizes and hover data survive vertex reordering", {
  scope <- new.env(parent = asNamespace("CerebroNexus"))
  viewer_server_load(
    viewer_server_statements("hla_tcr_motifs", "visualizations.R"),
    scope,
    names = c(
      "HLA_MOTIF_MAX_LEGEND_CLUSTERS",
      "HLA_CARRIER_LEVELS",
      "HLA_CARRIER_COLORS",
      "HLA_CONTEXT_LEVELS",
      "HLA_CONTEXT_COLORS",
      "hla_build_motif_visnet"
    )
  )
  graph <- igraph::make_ring(4)
  attrs <- list(
    name = letters[1:4],
    cdr3 = paste0("CASS", letters[1:4]),
    cluster = rep(1, 4),
    clone_count = c(1, 4, 16, 1000),
    samples_all = as.list(paste0("s", 1:4)),
    layout_x = 1:4,
    layout_y = 4:1,
    mhc_context = c("Class I", "Class II", "Mixed", "Unknown")
  )
  for (name in names(attrs)) {
    graph <- igraph::set_vertex_attr(graph, name, value = attrs[[name]])
  }
  built <- scope$hla_build_motif_visnet(graph, color_by = "mhc_context")
  expect_equal(built$nodes$size, hla_node_radius(attrs$clone_count))
  expect_false(any(c("group", "value") %in% names(built$nodes)))
  expect_equal(
    built$nodes$color,
    unname(scope$HLA_CONTEXT_COLORS[attrs$mhc_context])
  )
  reversed <- scope$hla_build_motif_visnet(
    igraph::permute(graph, 4:1),
    color_by = "mhc_context"
  )
  expect_equal(
    reversed$nodes$color[match(built$nodes$node_key, reversed$nodes$node_key)],
    built$nodes$color
  )
  counts <- Filter(
    function(x) identical(x$label, "Clone size"),
    built$hover_columns
  )
  expect_length(counts, 1L)
  expect_equal(counts[[1]]$values, attrs$clone_count)
  expect_equal(
    intersect(
      tolower(scope$HLA_CONTEXT_COLORS),
      tolower(scope$HLA_CARRIER_COLORS)
    ),
    "#b8bcc4"
  )
})

test_that("the HLA help action renders offline diagrams and evidence limits", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      modal <- NULL
      showModal <- function(ui) modal <<- ui
      sys.source(viewer_test_path("hla_tcr_motifs", "help_guide.R"), scope)
    },
    {
      session$setInputs(hla_visualizations_info = 1)
      doc <- xml2::read_html(as.character(scope$modal))
      text <- gsub("\\s+", " ", xml2::xml_text(doc))
      for (tab in c("Motif Network", "HLA Associations", "Data & QC")) {
        expect_match(text, tab, fixed = TRUE)
      }
      expect_match(text, "never confirmed restriction", ignore.case = TRUE)
      expect_match(text, "six.{0,80}class I", ignore.case = TRUE)
      expect_match(text, "Orthogonal axes", ignore.case = TRUE)
      expect_gt(length(xml2::xml_find_all(doc, ".//svg")), 0)
      references <- xml2::xml_attr(xml2::xml_find_all(doc, ".//*[@src]"), "src")
      expect_false(any(grepl("^https?://", references)))
      fills <- xml2::xml_attr(xml2::xml_find_all(doc, ".//*[@fill]"), "fill")
      for (color in c(
        scope$HLA_GUIDE_CARRIER,
        scope$HLA_GUIDE_NONCARRIER,
        scope$HLA_GUIDE_CLASS_I,
        scope$HLA_GUIDE_CLASS_II,
        scope$HLA_GUIDE_SAMPLE
      )) {
        expect_true(tolower(color) %in% tolower(fills))
      }
      scales <- new.env(parent = globalenv())
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "visualizations.R"),
        scales,
        names = c("HLA_CARRIER_COLORS", "HLA_CONTEXT_COLORS")
      )
      expect_equal(
        scope$HLA_GUIDE_CARRIER,
        unname(scales$HLA_CARRIER_COLORS["Carrier"])
      )
      expect_equal(
        scope$HLA_GUIDE_NONCARRIER,
        unname(scales$HLA_CARRIER_COLORS["Non-carrier"])
      )
      expect_equal(
        scope$HLA_GUIDE_CLASS_I,
        unname(scales$HLA_CONTEXT_COLORS["Class I"])
      )
      expect_equal(
        scope$HLA_GUIDE_CLASS_II,
        unname(scales$HLA_CONTEXT_COLORS["Class II"])
      )
      expect_equal(
        scope$HLA_GUIDE_SAMPLE,
        unname(hla_distinct_colors(c("a", "b", "c")))
      )
    }
  )
})

test_that("HLA network tables and CSV follow graph membership and observation units", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      graph <- igraph::make_empty_graph(2)
      attrs <- list(
        name = c("V1::AAA", "V2::BBB"),
        cdr3 = c("AAA", "BBB"),
        v_gene = c("V1", "V2"),
        clone_count = c(2, 1),
        cluster = c(1, 1),
        annotation = c("x", "z"),
        annotation_dist = c("2 types: x (1), y (1)", "1 type: z (1)"),
        samples_all = c("s1,s2", "s3")
      )
      for (name in names(attrs)) {
        graph <- igraph::set_vertex_attr(graph, name, value = attrs[[name]])
      }
      hla_motif_graph <- function() if (isTRUE(input$empty)) NULL else graph
      hla_node_meta_cols <- function() c("sample", "cdr3", "annotation")
      hla_param <- function(name, default) input[[name]] %||% default
      hla_by_v_default <- function() TRUE
      getObservationUnit <- function() {
        list(singular = "analysis unit", plural = "analysis units")
      }
      hla_scoped_segments <- function() {
        data.frame(
          sample = c("s1", "s2", "s3", "s4"),
          cdr3 = c("AAA", "AAA", "BBB", "AAA"),
          v_gene = c("V1", "V1", "V2", "V3"),
          annotation = letters[1:4]
        )
      }
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "network_table.R"),
        scope,
        names = c(
          "HLA_NETWORK_TABLE_NODE_COLS",
          "HLA_NETWORK_TABLE_NODE_TAIL_COLS",
          "HLA_NETWORK_TABLE_CELL_COLS",
          "hla_network_table_meta_cols",
          "hla_mark_mixed_nodes",
          "hla_network_table_grain",
          "hla_network_table_data",
          "output$hla_table_grain_ui",
          "output$hla_network_table",
          "output$hla_network_download"
        )
      )
    },
    {
      rows <- hla_network_table_data()
      expect_equal(rows$CDR3, c("AAA", "BBB"))
      expect_equal(rows[["analysis units"]], c(2, 1))
      expect_equal(rows$annotation, c("2 types: x (1), y (1)", "z"))
      expect_equal(sum(names(rows) == "CDR3"), 1L)
      expect_equal(tail(names(rows), 1), "samples")
      expect_match(
        output$hla_table_grain_ui$html,
        "By analysis unit",
        fixed = TRUE
      )
      expect_equal(
        read.csv(output$hla_network_download, check.names = FALSE),
        rows
      )
      widget <- jsonlite::fromJSON(output$hla_network_table)
      expect_true(widget$x$options$scrollX)
      expect_match(widget$x$container, "nowrap", fixed = TRUE)
      sample_render <- widget$x$options$columnDefs$render[[1]]
      session$setInputs(hla_table_grain = "cell")
      rows <- hla_network_table_data()
      expect_equal(rows$sample, c("s1", "s2", "s3"))
      expect_equal(rows$annotation, letters[1:3])
      expect_equal(
        read.csv(output$hla_network_download, check.names = FALSE),
        rows
      )
      session$setInputs(empty = TRUE)
      expect_null(hla_network_table_data())
      # Keep table/CSV checks above active on CRAN; only the DOM probe needs Chrome.
      skip_if_no_chrome()
      browser <- chromote::ChromoteSession$new()
      on.exit(browser$close(), add = TRUE)
      malicious <- '<img src=x onerror="window.injected=true">'
      result <- browser$Runtime$evaluate(
        paste0(
          "(() => { const render = ",
          sample_render,
          "; const value = ",
          jsonlite::toJSON(malicious, auto_unbox = TRUE),
          "; const host = document.createElement('div');",
          "host.innerHTML = render(value, 'display', []); const single = host.textContent === value && !host.querySelector('img');",
          "host.innerHTML = render(value + ',second', 'display', []); return {single, title:host.firstChild.title,",
          "text:host.textContent, images:host.querySelectorAll('img').length, raw:render(value, 'filter', [])}; })()"
        ),
        returnByValue = TRUE
      )$result$value
      expect_true(result$single)
      expect_equal(result$title, paste0(malicious, ",second"))
      expect_equal(result$text, paste0(malicious, "\u2026"))
      expect_equal(result$images, 0)
      expect_equal(result$raw, malicious)
    }
  )
})

test_that("HLA metadata discovery uses declared biology and chain-specific filters", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      hla_first_frame <- function() NULL
      hla_active_chain <- function() input$chain %||% "TRB"
      getGroups <- function() {
        if (isTRUE(input$no_groups)) {
          "condition"
        } else {
          c("annotation", "condition", "missing")
        }
      }
      data_set <- function() {
        list(technical_info = list(lineage_column = input$declared))
      }
      hla_ir_annotated <- function() {
        list(data.frame(
          sample = c("s1", "s2"),
          annotation = c("CD4 T", "CD8 T"),
          condition = c("anti-CD4", "CD8_case"),
          cell_type = c("wrong", "wrong")
        ))
      }
      hla_initial_samples <- function() c("s1", "s2")
      viewer_server_load(
        viewer_server_statements("hla_tcr_motifs", "data.R"),
        scope,
        names = c(
          "hla_available_cols",
          "hla_color_meta_cols",
          "hla_celltype_col",
          "hla_filter_groups",
          "hla_filter_levels",
          "hla_default_filter_selections",
          "hla_filter_input_id",
          "hla_filter_selections",
          "hla_ir_filtered"
        )
      )
    },
    {
      expect_equal(hla_color_meta_cols(), c("annotation", "condition"))
      expect_identical(hla_celltype_col(), "annotation")
      session$setInputs(no_groups = TRUE)
      expect_identical(hla_celltype_col(), NA_character_)
      session$setInputs(declared = "cell_type")
      expect_identical(hla_celltype_col(), "cell_type")
      session$setInputs(hla_group_filter_TRB_sample = "s1")
      expect_equal(hla_ir_filtered()[[1]]$sample, "s1")
      session$setInputs(chain = "TRA")
      expect_equal(hla_ir_filtered()[[1]]$sample, c("s1", "s2"))
      session$setInputs(hla_group_filter_TRA_sample = "s2")
      expect_equal(hla_ir_filtered()[[1]]$sample, "s2")
      session$setInputs(chain = "TRB")
      expect_equal(hla_ir_filtered()[[1]]$sample, "s1")
    }
  )
})
