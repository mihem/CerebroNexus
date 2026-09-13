#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "usage: prepare_viewer_1m_pages.R REPO_ROOT INPUT_CRB OUTPUT_CRB",
    call. = FALSE
  )
}

root <- normalizePath(args[[1L]], mustWork = TRUE)
input <- normalizePath(args[[2L]], mustWork = TRUE)
output <- normalizePath(args[[3L]], mustWork = FALSE)
if (!identical(dirname(input), dirname(output))) {
  stop(
    "INPUT_CRB and OUTPUT_CRB must share a directory and sidecar.",
    call. = FALSE
  )
}
if (identical(input, output)) {
  stop("OUTPUT_CRB must not overwrite INPUT_CRB.", call. = FALSE)
}

devtools::load_all(root, quiet = TRUE)
object <- readCerebro(input)
metadata <- object$getMetaData()
if (nrow(metadata) != 1000000L) {
  stop("The page benchmark requires exactly 1,000,000 cells.", call. = FALSE)
}
cells <- as.character(metadata$cell_barcode)

projection_names <- object$availableProjections()
if (!length(projection_names)) {
  stop("The benchmark CRB needs a two-dimensional projection.", call. = FALSE)
}
projection_name <- projection_names[[1L]]
projection <- object$getProjection(projection_name)
if (ncol(projection) < 2L) {
  stop("The benchmark CRB needs a two-dimensional projection.", call. = FALSE)
}
trajectory_meta <- data.frame(
  DR_1 = projection[[1L]],
  DR_2 = projection[[2L]],
  pseudotime = (seq_along(cells) - 1) / (length(cells) - 1),
  state = factor((seq_along(cells) - 1L) %% 10L + 1L),
  row.names = cells
)
centers <- aggregate(
  trajectory_meta[c("DR_1", "DR_2")],
  list(state = trajectory_meta$state),
  mean
)
trajectory_edges <- data.frame(
  source_dim_1 = head(centers$DR_1, -1L),
  source_dim_2 = head(centers$DR_2, -1L),
  target_dim_1 = tail(centers$DR_1, -1L),
  target_dim_2 = tail(centers$DR_2, -1L)
)
object$addTrajectory(
  "monocle2",
  "synthetic_1m",
  list(meta = trajectory_meta, edges = trajectory_edges)
)

encode_aa <- function(ids, width = 8L) {
  alphabet <- strsplit("ACDEFGHIKLMNPQRSTVWY", "", fixed = TRUE)[[1L]]
  vapply(
    ids - 1L,
    function(value) {
      positions <- (value %/% 20^(seq_len(width) - 1L)) %% 20L + 1L
      paste0(alphabet[positions], collapse = "")
    },
    character(1)
  )
}

n_receptors <- 100000L
n_samples <- 32L
receptor_cells <- cells[
  floor(seq(0, length(cells) - 1L, length.out = n_receptors)) + 1L
]
sample_id <- sprintf(
  "sample_%02d",
  (seq_len(n_receptors) - 1L) %% n_samples + 1L
)
clone_id <- (seq_len(n_receptors) - 1L) %% 2048L + 1L
tra <- paste0("CAV", encode_aa(clone_id + 37L), "F")
trb <- paste0("CASS", encode_aa(clone_id), "F")
repertoire <- data.frame(
  barcode = receptor_cells,
  CTgene = "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1",
  CTnt = NA_character_,
  CTaa = paste(tra, trb, sep = "_"),
  CTstrict = paste(
    "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1",
    paste(tra, trb, sep = "_"),
    sep = ";"
  ),
  sample = sample_id,
  stringsAsFactors = FALSE
)
repertoire <- split(repertoire, repertoire$sample)
repertoire <- lapply(repertoire, function(table) {
  table$sample <- NULL
  rownames(table) <- NULL
  table
})
object$addImmuneRepertoire(repertoire)

sample_names <- names(repertoire)
hla <- data.frame(
  sample = sample_names,
  donor_id = sample_names,
  `HLA-A_1` = rep(c("02:01", "01:01"), length.out = n_samples),
  `HLA-A_2` = rep(c("03:01", "24:02"), length.out = n_samples),
  `HLA-B_1` = rep(c("07:02", "08:01"), length.out = n_samples),
  `HLA-B_2` = rep(c("44:02", "15:01"), length.out = n_samples),
  `HLA-DRB1_1` = rep(c("04:01", "03:01"), length.out = n_samples),
  `HLA-DRB1_2` = rep(c("07:01", "15:01"), length.out = n_samples),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
object$addHLATyping(
  hla,
  source_type = "synthetic",
  typing_method = "deterministic benchmark fixture"
)

saveCerebro(object, output)
message("Wrote 1M page benchmark CRB: ", output)
