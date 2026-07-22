##----------------------------------------------------------------------------##
## HLA & TCR Motifs — "Network data" table
##
## Presentation only. Node view = the vertices of the SAME graph object the
## Motif Network draws (hla_motif_graph()), so the table matches the picture by
## construction and is NOT bound by the render cap -- the graph is built even
## when too large to draw. Cell view = the per-cell scoped rows (hla_scoped_
## segments()) that feed the graph. No new data logic; both reuse existing
## reactives, so the table cannot drift from the graph.
##----------------------------------------------------------------------------##

## STRUCTURAL columns -> display names, per grain: the ones this page computes
## itself, which every data set has in common. What a data set means -- its own
## annotations -- is NOT listed here; it is carried dynamically by
## hla_network_table_meta_cols() below, so a demo whose point is a per-cell
## antigen or presenting allele shows those columns without this file naming
## them. Columns absent from a given dataset/scope are dropped (intersect with
## what the data actually carries), the same defensive pattern as
## hla_node_meta_cols().
HLA_NETWORK_TABLE_NODE_COLS <- c(
  cdr3 = "CDR3",
  v_gene = "V",
  j_gene = "J",
  # Replaced per data set with the declared observation unit (see below); never
  # hard-coded to "cells", which is wrong for a bulk repertoire.
  clone_count = "observations",
  cluster = "motif cluster",
  pair_allele = "allele side",
  mhc_context = "MHC context"
)
## Appended after the declared metadata, so the wide "samples" list stays at the
## right edge of the table rather than pushing the annotations off screen.
HLA_NETWORK_TABLE_NODE_TAIL_COLS <- c(
  samples_all = "samples",
  sample_origin = "sample origin"
)
HLA_NETWORK_TABLE_CELL_COLS <- c(
  sample = "sample",
  cdr3 = "CDR3",
  v_gene = "V",
  j_gene = "J",
  pair_allele = "allele side",
  mhc_context = "MHC context"
)

## The data set's OWN annotations, in the object's declared order. This is the
## same set the network already carries onto its nodes and colours by
## (hla_node_meta_cols()), so the table shows exactly what the picture can show,
## and a new annotation column needs no change here.
##
## `sample` is excluded because both grains already report it: the node grain
## through `samples_all` / `sample_origin`, the cell grain as a structural
## column. The per-column `_dist` companions the aggregation adds are left out
## too -- they are packed distribution strings meant for tooltips, not cells in
## a table.
hla_network_table_meta_cols <- reactive({
  cols <- setdiff(hla_node_meta_cols(), "sample")
  stats::setNames(cols, cols)
})

## A node is a set of cells, so its annotation is a DISTRIBUTION, not a value.
## The aggregation stores the modal value in `<col>` and the full tally in
## `<col>_dist` ("2 types: Influenza (5), CMV (2)"). Showing only the mode makes
## a heterogeneous node look categorically assigned -- on the shipped TRB graph
## several nodes carry more than one reagent antigen, and some carry both
## genotype-status values, all rendered as if settled.
##
## Where a node is homogeneous the plain value is shown. Where it is not, the
## cell becomes the distribution string itself: it names the mixture, keeps the
## counts, survives the CSV export, and needs no extra column. This follows the
## page's existing habit of reporting a spanning node as such rather than
## breaking the tie (see hla_context_summary()'s "Mixed").
hla_mark_mixed_nodes <- function(df, cols) {
  for (col in intersect(cols, colnames(df))) {
    dist_col <- paste0(col, "_dist")
    if (!dist_col %in% colnames(df)) {
      next
    }
    n_types <- suppressWarnings(as.integer(
      sub("^([0-9]+) type.*$", "\\1", df[[dist_col]])
    ))
    mixed <- !is.na(n_types) & n_types > 1L
    df[[col]][mixed] <- df[[dist_col]][mixed]
  }
  df
}

hla_network_table_grain <- reactive({
  g <- input$hla_table_grain
  if (is.null(g) || !nzchar(g)) "node" else g
})

## The grain picker lives here, not in UI.R, because the second grain is one row
## per OBSERVATION UNIT -- a cell only when the data set declares it so. For the
## bulk repertoire those rows are analysis units, and calling them cells would
## contradict the unit noun used everywhere else on the page. The stored VALUE
## stays "cell" so the reactives and the CSV name are unchanged; only the label
## follows the declared unit.
output$hla_table_grain_ui <- renderUI({
  unit <- getObservationUnit()$singular
  tagList(
    radioButtons(
      "hla_table_grain",
      "Rows:",
      choices = stats::setNames(
        c("node", "cell"),
        c("By motif (node)", paste0("By ", unit))
      ),
      selected = isolate(hla_network_table_grain()),
      inline = TRUE
    ),
    tags$p(
      class = "text-muted",
      style = "font-size: 12px;",
      sprintf(
        paste(
          "The rows behind the network shown on Motif Network, under the",
          "current chain / scope / allele / min-size filters. 'By motif' is",
          "one row per CDR3 node; 'By %s' is one row per %s."
        ),
        unit,
        unit
      )
    )
  )
})

## The data frame currently shown, already column-selected and renamed. NULL
## when there is nothing in scope / no graph, which the render treats as empty.
##
## BOTH grains are anchored on the SAME graph object the network draws, so the
## table can never show more than the picture. The node view is its vertices;
## the cell view is the scoped cells whose node survived into the graph -- the
## graph aggregates to unique CDR3 and drops singletons / sub-min-size motifs,
## so hla_scoped_segments() (every cell in scope) is a superset that must be
## filtered to graph membership, or "By cell" would list cells behind nodes the
## network never showed.
hla_network_table_data <- reactive({
  g <- hla_motif_graph()
  if (!hla_motif_graph_ok(g)) {
    return(NULL)
  }
  if (identical(hla_network_table_grain(), "cell")) {
    seg <- hla_scoped_segments()
    if (is.null(seg) || nrow(seg) == 0) {
      return(NULL)
    }
    # Node id = cdr3, or v_gene::cdr3 under "Split motifs by V gene" -- the same
    # key hla_aggregate_cdr3_nodes() uses for V(g)$name. Build it the same way
    # so the membership test matches the graph exactly.
    by_v <- isTRUE(hla_param("hla_by_v", hla_by_v_default()))
    seg_key <- if (by_v) {
      paste(seg$v_gene, seg$cdr3, sep = "::")
    } else {
      seg$cdr3
    }
    df <- seg[seg_key %in% igraph::V(g)$name, , drop = FALSE]
    map <- c(HLA_NETWORK_TABLE_CELL_COLS, hla_network_table_meta_cols())
  } else {
    df <- igraph::as_data_frame(g, what = "vertices")
    map <- c(
      HLA_NETWORK_TABLE_NODE_COLS,
      hla_network_table_meta_cols(),
      HLA_NETWORK_TABLE_NODE_TAIL_COLS
    )
    # clone_count counts whatever the data set declares as its observation unit.
    # Hard-coding "cells" mislabels a bulk repertoire, whose rows are analysis
    # units, not cells -- the same reason hla_unit_noun() exists.
    map[["clone_count"]] <- getObservationUnit()$plural
    # A node aggregates cells, so an annotation that varies within it must say
    # so rather than report its mode as if it were the node's value.
    df <- hla_mark_mixed_nodes(df, names(hla_network_table_meta_cols()))
  }
  # A declared annotation that collides with a structural name (a `cdr3` column
  # in the metadata, say) must not produce two columns of the same name.
  map <- map[!duplicated(names(map))]
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  keep <- intersect(names(map), colnames(df))
  out <- df[, keep, drop = FALSE]
  colnames(out) <- unname(map[keep])
  rownames(out) <- NULL
  out
})

output$hla_network_table <- DT::renderDataTable({
  df <- hla_network_table_data()
  if (is.null(df)) {
    df <- data.frame(Note = "No rows in the current network scope.")
  }
  # The multi-value "samples" column (a comma-separated donor list) is shown as
  # the first sample + an ellipsis, with the full list on hover via a title
  # attribute. Display only -- the render returns the raw value for every other
  # DataTables type, so search / sort and the CSV export keep the full list.
  # Only that column is left un-escaped (it emits a <span>); every other column
  # stays HTML-escaped.
  #
  # Because DataTables escaping is off for this column, the cell is built as DOM
  # nodes and never by string concatenation. A sample name is arbitrary data
  # coming from the .crb, so a value like `<img src=x onerror=...>` pasted into
  # an HTML string would execute in the app's origin. Assigning to .textContent
  # and .title makes the browser escape both the text and the attribute for us,
  # which is why outerHTML is safe to return here.
  s_idx <- which(colnames(df) == "samples")
  col_defs <- list()
  escape_cols <- seq_len(ncol(df))
  if (length(s_idx) == 1) {
    escape_cols <- which(colnames(df) != "samples")
    col_defs <- list(list(
      targets = s_idx - 1L,
      render = DT::JS(
        "function(data, type, row) {",
        "  if (type !== 'display' || data === null) { return data; }",
        "  var full = String(data);",
        "  var span = document.createElement('span');",
        "  if (full.indexOf(',') === -1) {",
        "    span.textContent = full;",
        "  } else {",
        "    span.title = full;",
        "    span.textContent = full.split(',')[0] + '\\u2026';",
        "  }",
        "  return span.outerHTML;",
        "}"
      )
    ))
  }
  DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    escape = escape_cols,
    class = "display nowrap",
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      columnDefs = col_defs
    )
  )
})

output$hla_network_download <- downloadHandler(
  filename = function() {
    paste0("hla_network_", hla_network_table_grain(), ".csv")
  },
  content = function(file) {
    df <- hla_network_table_data()
    if (is.null(df)) {
      df <- data.frame()
    }
    utils::write.csv(df, file, row.names = FALSE, na = "")
  }
)
