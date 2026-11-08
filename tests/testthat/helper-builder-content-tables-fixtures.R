builder_table_source_runtime <- function(local = parent.frame()) {
  file <- builder_table_inst_path("builder", "content_tables.R")
  if (nzchar(file) && file.exists(file)) {
    sys.source(file, envir = local)
  }
  invisible(file)
}

builder_table_context <- function() {
  list(
    cells = paste0("cell", seq_len(4L)),
    features = c("CD3D", "MS4A1", "NKG7"),
    metadata = list(
      columns = c("cell_type", "sample"),
      identity_valid = TRUE
    ),
    assays = list(RNA = list(exportable = TRUE)),
    default_assay = "RNA",
    groups = list(candidates = c("cell_type", "sample")),
    reductions = list(),
    source = list(type = "example", location = "fixture")
  )
}

builder_table_object <- function(misc = list()) {
  object <- SeuratObject::pbmc_small
  methods::slot(object, "misc") <- misc
  object
}

builder_table_marker <- function(group = "cell_type") {
  stats::setNames(
    data.frame(
      group = c("B", "T"),
      gene = c("MS4A1", "CD3D"),
      avg_log2FC = c(2.1, 1.7),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    c(group, "gene", "avg_log2FC")
  )
}

builder_table_most <- function(group = "cell_type") {
  stats::setNames(
    data.frame(
      group = c("B", "T"),
      gene = c("MS4A1", "CD3D"),
      pct = c(90, 85),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    c(group, "gene", "pct")
  )
}

builder_table_mean <- function(group = "cell_type") {
  stats::setNames(
    data.frame(
      group = c("B", "T"),
      gene = c("MS4A1", "CD3D"),
      mean_expr = c(2.5, 1.8),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    c(group, "gene", "mean_expr")
  )
}

builder_table_enrichment <- function(group = "cell_type") {
  stats::setNames(
    data.frame(
      group = c("B", "T"),
      term = c("BCR signaling", "TCR signaling"),
      score = c(8.2, 7.5),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    c(group, "Term", "Combined.Score")
  )
}

builder_table_trajectory <- function(cells = c("cell1", "cell3")) {
  meta <- data.frame(
    DR_1 = c(0.1, 0.8),
    DR_2 = c(0.2, 0.7),
    pseudotime = c(0, 1),
    state = factor(c("1", "2"), levels = c("1", "2")),
    row.names = cells,
    check.names = FALSE
  )
  edges <- data.frame(
    source = "n1",
    target = "n2",
    weight = 1,
    source_dim_1 = 0.1,
    source_dim_2 = 0.2,
    target_dim_1 = 0.8,
    target_dim_2 = 0.7,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(meta = meta, edges = edges)
}

builder_table_bomb <- function(value, sentinel) {
  structure(
    value,
    class = c("builder_table_bomb", class(value)),
    sentinel = sentinel
  )
}
