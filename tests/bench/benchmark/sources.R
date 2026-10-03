# Shared configuration and helpers for the backend benchmark.
# Source this file from tests or cli.R.

# ---- sources.R ----
# Remote data source registry for the expression-backend benchmark.
#
# Every source is a public HDF5 file. Metadata inspection uses the rhdf5 ROS3
# virtual file driver and transfers only the ranges it needs. A benchmark run
# downloads the complete file into its marked scratch directory so all timed
# source reads are local and network throughput is excluded from measurements.
#
# Numbers in the comments were measured with `cli.R inspect`.

BENCH_SOURCES <- list(
  # 10x Genomics 1.3 M mouse brain cells (E18), the canonical large-scale
  # scRNA-seq reference. Raw integer counts, CSC over cells.
  # 1,306,127 cells x 27,998 genes, nnz 2,624,828,308, 3.93 GB remote.
  mouse_brain_e18 = list(
    label = "10x mouse brain E18",
    accession = "GSE93421; SRP096558",
    landing_page = paste0(
      "https://www.10xgenomics.com/datasets/",
      "1-3-million-brain-cells-from-e-18-mice-2-standard-1-3-0"
    ),
    kind = "tenx",
    url = paste0(
      "https://cf.10xgenomics.com/samples/cell-exp/1.3.0/1M_neurons/",
      "1M_neurons_filtered_gene_bc_matrices_h5.h5"
    ),
    group = "mm10",
    organism = "mm10",
    slot = "counts",
    expected_bytes = 4216018749,
    expected_sha256 = paste0(
      "255a36ee92de25cb3568faa2c27d31fe",
      "6d0db30f285c5c977be8d6245de14044"
    ),
    full_cells = 1306127,
    preview_cells = 400e3,
    # Shared scale tiers. The complete 1,306,127-cell matrix is
    # measured separately by full and is never labelled as 1m.
    tiers = c(1e3, 10e3, 50e3, 100e3, 500e3, 1e6),
    comparison_tiers = c(1e3, 10e3, 50e3, 100e3, 500e3, 1e6)
  ),

  # CELLxGENE Discover: population-scale cross-disorder atlas of the human
  # prefrontal cortex (HBCC cohort). Normalised float values, CSR over cells.
  # 1,486,324 cells x 34,176 genes, nnz 6,111,732,728, 14.15 GB remote.
  human_pfc_hbcc = list(
    label = "human PFC cross-disorder (HBCC)",
    dataset_id = "d27fb144-f105-46c2-b36f-f51421f74e4e",
    collection_id = "84ce6837-548d-4a1f-919f-0bc0d9a3952f",
    doi = "10.1038/s41597-025-04687-5",
    landing_page = paste0(
      "https://cellxgene.cziscience.com/collections/",
      "84ce6837-548d-4a1f-919f-0bc0d9a3952f"
    ),
    kind = "h5ad",
    url = paste0(
      "https://datasets.cellxgene.cziscience.com/",
      "d27fb144-f105-46c2-b36f-f51421f74e4e.h5ad"
    ),
    organism = "hg38",
    slot = "data",
    expected_bytes = 14150526668,
    expected_sha256 = paste0(
      "aeca0480ab8941a7e4cf6b0ff6dc8c",
      "5f9d0de376466d65ca8198dc873f1cb16f"
    ),
    full_cells = 1486324,
    preview_cells = 300e3,
    # The same fixed tiers are used for direct scale comparison. The complete
    # 1,486,324-cell matrix remains a separate full observation.
    tiers = c(1e3, 10e3, 50e3, 100e3, 500e3, 1e6),
    comparison_tiers = c(1e3, 10e3, 50e3, 100e3, 500e3, 1e6)
  ),

  # Same collection, MSSM cohort: 4,140,453 cells, 33.6 GB remote. Opt-in via
  # BENCH_SOURCES_EXTRA=human_pfc_mssm because probing it alone streams more
  # than the other two sources combined.
  human_pfc_mssm = list(
    label = "human PFC cross-disorder (MSSM)",
    kind = "h5ad",
    url = paste0(
      "https://datasets.cellxgene.cziscience.com/",
      "0e853475-e298-4b09-881a-ed0b60d5a8c9.h5ad"
    ),
    organism = "hg38",
    slot = "data",
    expected_bytes = 36077725286,
    tiers = c(50e3, 200e3),
    comparison_tiers = 50e3,
    opt_in = TRUE
  )
)

# Which sources run by default.
bench_active_sources <- function() {
  extra <- strsplit(Sys.getenv("BENCH_SOURCES_EXTRA", ""), "[,[:space:]]+")[[1]]
  extra <- extra[nzchar(extra)]
  keep <- vapply(BENCH_SOURCES, function(s) !isTRUE(s$opt_in), logical(1))
  active <- names(BENCH_SOURCES)[keep | names(BENCH_SOURCES) %in% extra]
  only <- strsplit(Sys.getenv("BENCH_SOURCES_ONLY", ""), "[,[:space:]]+")[[1]]
  only <- only[nzchar(only)]
  if (!length(only)) {
    return(active)
  }
  unknown <- setdiff(only, names(BENCH_SOURCES))
  if (length(unknown)) {
    stop("unknown BENCH_SOURCES_ONLY value: ", paste(unknown, collapse = ", "))
  }
  intersect(active, only)
}
