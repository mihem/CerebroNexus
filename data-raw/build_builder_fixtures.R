#!/usr/bin/env Rscript
## Build the Complete Viewer input; build_builder_spatial_fixtures.R builds the other two.

arguments <- commandArgs(trailingOnly = FALSE)
script_argument <- grep("^--file=", arguments, value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run this script with Rscript from the repository root.", call. = FALSE)
}
script <- normalizePath(sub("^--file=", "", script_argument), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
if (!identical(normalizePath(getwd(), mustWork = TRUE), repo)) {
  stop(
    "Run this script from the CerebroNexus repository root: ",
    repo,
    call. = FALSE
  )
}

stabilize_fixture <- function(object) {
  fixed_time <- as.POSIXct("2026-08-06 00:00:00", tz = "UTC")
  if (length(object@commands)) {
    for (name in names(object@commands)) {
      object@commands[[name]]@time.stamp <- fixed_time
    }
  }
  object
}

make_overview_plot <- function() {
  plot_data <- data.frame(
    condition = c("baseline", "treated"),
    n = c(120, 60)
  )
  ggplot2::ggplot(plot_data, ggplot2::aes(condition, n)) +
    ggplot2::geom_col()
}

add_fixture_section <- function(object, name, cells, origin, span) {
  coordinates <- data.frame(
    x = stats::runif(length(cells), 0, span[1L]) + origin[1L],
    y = stats::runif(length(cells), 0, span[2L]) + origin[2L],
    cell = cells
  )
  object[[name]] <- SeuratObject::CreateFOV(
    coords = list(
      centroids = SeuratObject::CreateCentroids(coordinates)
    ),
    type = "centroids",
    assay = "RNA",
    key = paste0(tolower(name), "_")
  )
  object
}

make_gene_lists <- function(marker_genes) {
  list(
    epithelial_program = unname(marker_genes$Epithelial),
    myeloid_program = unname(marker_genes$Macrophage),
    t_cell_program = unname(marker_genes[["T cell"]])
  )
}

make_trajectory <- function(cells, umap, states) {
  state_levels <- unique(as.character(states))
  endpoints <- vapply(
    state_levels,
    function(state) {
      which(as.character(states) == state)[[1L]]
    },
    integer(1)
  )
  edge_indices <- seq_len(length(state_levels) - 1L)
  list(
    monocle2 = list(
      lineage = list(
        meta = data.frame(
          DR_1 = unname(umap[cells, 1L]),
          DR_2 = unname(umap[cells, 2L]),
          pseudotime = seq(0, 1, length.out = length(cells)),
          state = factor(as.character(states), levels = state_levels),
          row.names = cells,
          check.names = FALSE
        ),
        edges = data.frame(
          source = state_levels[edge_indices],
          target = state_levels[edge_indices + 1L],
          weight = rep(1, length(edge_indices)),
          source_dim_1 = unname(umap[endpoints[edge_indices], 1L]),
          source_dim_2 = unname(umap[endpoints[edge_indices], 2L]),
          target_dim_1 = unname(umap[endpoints[edge_indices + 1L], 1L]),
          target_dim_2 = unname(umap[endpoints[edge_indices + 1L], 2L]),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )
    )
  )
}

make_immune_repertoire <- function(cells, samples) {
  split_cells <- split(cells, samples)
  chains <- c("TRA", "TRB", "IGH")
  genes <- c(
    TRA = "TRAV1-2.TRAJ33",
    TRB = "TRBV7-9.TRBJ2-7",
    IGH = "IGHV3-23.IGHJ4"
  )
  stats::setNames(
    lapply(seq_along(split_cells), function(sample_index) {
      sample_cells <- split_cells[[sample_index]]
      chain <- rep(chains, length.out = length(sample_cells))
      data.frame(
        barcode = sample_cells,
        CTgene = unname(genes[chain]),
        CTnt = sprintf("NT_%02d_%03d", sample_index, seq_along(sample_cells)),
        CTaa = sprintf("CASS_%02d_%03d", sample_index, seq_along(sample_cells)),
        CTstrict = sprintf(
          "%s_%02d_%03d",
          chain,
          sample_index,
          seq_along(sample_cells)
        ),
        stringsAsFactors = FALSE
      )
    }),
    names(split_cells)
  )
}

make_hla_typing <- function(samples, donors) {
  typing <- expand.grid(
    sample = unique(samples),
    locus = c("HLA-A", "HLA-B", "HLA-C"),
    copy = 1:2,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  donor_by_sample <- stats::setNames(donors, samples)
  typing$donor_id <- unname(donor_by_sample[typing$sample])
  allele_index <- match(typing$locus, c("HLA-A", "HLA-B", "HLA-C"))
  typing$allele <- sprintf(
    "%s*%02d:%02d",
    typing$locus,
    allele_index * 2L + typing$copy,
    typing$copy
  )
  typing$resolution <- "two-field"
  typing$source_type <- "synthetic"
  typing$typing_method <- "deterministic fixture"
  typing$source_reference <- "complete_viewer_data"
  typing[, c(
    "sample",
    "donor_id",
    "locus",
    "copy",
    "allele",
    "resolution",
    "source_type",
    "typing_method",
    "source_reference"
  )]
}

make_complete_viewer_data_fixture <- function() {
  patient_id <- rep(c("patient_a", "patient_b", "patient_c"), each = 60L)
  section_id <- c(
    rep(c("section_a_1", "section_a_2"), each = 30L),
    rep(
      c(
        "section_b_1",
        "section_b_2",
        "section_b_3"
      ),
      each = 20L
    ),
    rep("section_c_1", 60L)
  )
  fov_id <- paste0(section_id, "_fov_1")
  sample_id <- unname(c(
    section_a_1 = "xenium_a_1",
    section_a_2 = "xenium_a_2",
    section_b_1 = "xenium_b_1",
    section_b_2 = "xenium_b_2",
    section_b_3 = "xenium_b_3",
    section_c_1 = "xenium_c_1"
  )[section_id])
  condition <- unname(c(
    patient_a = "baseline",
    patient_b = "treated",
    patient_c = "baseline"
  )[patient_id])
  cell_types <- c(
    "Epithelial",
    "Fibroblast",
    "Macrophage",
    "T cell",
    "B cell"
  )
  cells <- sprintf("%s_cell_%03d", patient_id, seq_along(patient_id))
  cell_type <- rep(cell_types, length.out = length(cells))
  cluster <- paste0("cluster_", match(cell_type, cell_types))
  region <- unname(c(
    Epithelial = "epithelial_zone",
    Fibroblast = "stroma",
    Macrophage = "immune_zone",
    `T cell` = "immune_zone",
    `B cell` = "immune_zone"
  )[cell_type])
  marker_genes <- list(
    Epithelial = c("EPCAM", "KRT8", "KRT18", "KRT19"),
    Fibroblast = c("COL1A1", "COL1A2", "DCN", "LUM"),
    Macrophage = c("CD68", "LYZ", "CD163", "CSF1R"),
    `T cell` = c("CD3D", "CD3E", "TRBC1", "IL7R"),
    `B cell` = c("MS4A1", "CD79A", "CD37", "CD74")
  )
  genes <- c(
    unlist(marker_genes, use.names = FALSE),
    sprintf("GENE%03d", 1:144)
  )
  lambda <- matrix(0.12, nrow = length(genes), ncol = length(cells))
  rownames(lambda) <- genes
  colnames(lambda) <- cells
  for (type in cell_types) {
    lambda[marker_genes[[type]], cell_type == type] <- 5
  }
  counts <- Matrix::Matrix(
    matrix(stats::rpois(length(lambda), lambda), nrow = nrow(lambda)),
    sparse = TRUE,
    dimnames = list(genes, cells)
  )
  object <- SeuratObject::CreateSeuratObject(counts = counts)
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object$patient_id <- factor(patient_id, levels = unique(patient_id))
  object$section_id <- factor(section_id, levels = unique(section_id))
  object$fov_id <- factor(fov_id, levels = unique(fov_id))
  object$sample_id <- factor(sample_id, levels = unique(sample_id))
  object$condition <- factor(condition, levels = unique(condition))
  object$cell_type <- factor(cell_type, levels = cell_types)
  object$cluster <- factor(cluster, levels = unique(cluster))
  object$region <- factor(region, levels = unique(region))
  object$Phase <- factor(
    rep(c("G1", "S", "G2M"), length.out = length(cells)),
    levels = c("G1", "S", "G2M")
  )

  cluster_index <- match(cell_type, cell_types)
  centers <- matrix(
    c(-4, -2, 4, -2, -2, 4, 4, 4, 0, 0),
    ncol = 2,
    byrow = TRUE
  )
  umap <- centers[cluster_index, , drop = FALSE] +
    matrix(stats::rnorm(length(cells) * 2L, sd = 0.7), ncol = 2L)
  tsne <- centers[cluster_index, , drop = FALSE] *
    1.4 +
    matrix(stats::rnorm(length(cells) * 2L, sd = 0.9), ncol = 2L)
  pca <- cbind(
    umap,
    matrix(stats::rnorm(length(cells) * 3L), ncol = 3L)
  )
  rownames(umap) <- rownames(tsne) <- rownames(pca) <- cells
  colnames(umap) <- c("UMAP_1", "UMAP_2")
  colnames(tsne) <- c("TSNE_1", "TSNE_2")
  colnames(pca) <- paste0("PC_", 1:5)
  object[["pca"]] <- SeuratObject::CreateDimReducObject(
    pca,
    key = "PC_",
    assay = "RNA"
  )
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    umap,
    key = "UMAP_",
    assay = "RNA"
  )
  object[["tsne"]] <- SeuratObject::CreateDimReducObject(
    tsne,
    key = "TSNE_",
    assay = "RNA"
  )

  fov_names <- unique(fov_id)
  for (index in seq_along(fov_names)) {
    fov <- fov_names[[index]]
    fov_cells <- cells[fov_id == fov]
    object <- add_fixture_section(
      object,
      fov,
      fov_cells,
      origin = c((index - 1L) * 1200, (index %% 2L) * 250),
      span = c(950, 720)
    )
  }
  object@misc$spatial_coordinate_system <- stats::setNames(
    lapply(fov_names, function(fov) {
      list(
        unit = "micron",
        origin = "top-left",
        x_direction = "right",
        y_direction = "down"
      )
    }),
    fov_names
  )

  marker_rows <- do.call(
    rbind,
    lapply(cell_types, function(type) {
      data.frame(
        cell_type = type,
        gene = marker_genes[[type]],
        avg_log2FC = seq(2.4, 1.2, length.out = length(marker_genes[[type]])),
        stringsAsFactors = FALSE
      )
    })
  )
  most_rows <- marker_rows[, c("cell_type", "gene")]
  most_rows$pct <- rep(c(96, 91, 86, 81), length.out = nrow(most_rows))
  mean_rows <- marker_rows[, c("cell_type", "gene")]
  mean_rows$mean_expr <- rep(
    c(4.8, 3.6, 2.7, 1.9),
    length.out = nrow(mean_rows)
  )
  enrichment_rows <- data.frame(
    cell_type = cell_types,
    Term = paste(cell_types, "signature"),
    Combined.Score = seq(12, 8, length.out = length(cell_types)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  object@misc$gene_lists <- make_gene_lists(marker_genes)
  object@misc$marker_genes <- list(wilcox = list(cell_type = marker_rows))
  object@misc$most_expressed_genes <- list(cell_type = most_rows)
  object@misc$mean_expression <- list(cell_type = mean_rows)
  object@misc$enriched_pathways <- list(
    enrichr = list(cell_type = enrichment_rows)
  )
  object@misc$trajectories <- make_trajectory(cells, umap, cluster)
  object@misc$extra_material <- list(
    tables = list(
      cohort_summary = data.frame(
        patient_id = c("patient_a", "patient_b", "patient_c"),
        condition = c("baseline", "treated", "baseline"),
        stringsAsFactors = FALSE
      )
    ),
    plots = list(
      overview = make_overview_plot()
    )
  )
  object@misc$immune_repertoire <- make_immune_repertoire(cells, sample_id)
  object@misc$hla_typing <- make_hla_typing(
    unique(sample_id),
    unname(c(
      xenium_a_1 = "patient_a",
      xenium_a_2 = "patient_a",
      xenium_b_1 = "patient_b",
      xenium_b_2 = "patient_b",
      xenium_b_3 = "patient_b",
      xenium_c_1 = "patient_c"
    )[unique(sample_id)])
  )
  object@misc$hla_typing_source_type <- "synthetic"

  positioned <- seq(1L, length(cells), by = 2L)
  positioned_cells <- cells[positioned]
  purity <- stats::runif(length(positioned), 0.45, 0.98)
  object@misc$trekker <- list(
    meta = list(
      n_cells_full = length(cells),
      n_cells = length(positioned),
      n_genes_obj = length(genes),
      unit = "um",
      coord_source = "synthetic_xenium",
      r = 25,
      seurat = TRUE,
      generated = "2026-08-10T00:00:00+0000"
    ),
    qc = list(
      sample_id = "xenium_omnibus",
      assay = "synthetic_xenium",
      tile_id = "all_fovs",
      eps = "25",
      min_sb = "5",
      total_nuclei = length(cells),
      in_lib = length(cells),
      pct_in_lib = 100,
      pct_valid_sb = 100,
      positioned = length(positioned),
      pct_positioned = 100 * length(positioned) / length(cells),
      conf = length(positioned),
      pct_conf = 100 * length(positioned) / length(cells),
      pct_2plus = 0,
      o_1 = length(positioned),
      n_0 = 0L,
      n_1 = length(positioned),
      n_2 = 0L,
      n_3 = 0L,
      n_4p = 0L,
      salv_2 = 0L,
      salv_3 = 0L
    ),
    barcodes = positioned_cells,
    x = unname(round(stats::runif(length(positioned), 0, 4200), 2)),
    y = unname(round(stats::runif(length(positioned), 0, 3200), 2)),
    ux = unname(round(umap[positioned, 1L], 3)),
    uy = unname(round(umap[positioned, 2L], 3)),
    clusters = unname(cluster_index[positioned] - 1L),
    celltype = cell_types,
    fields = list(
      spatial_purity = list(
        v = unname(as.integer(round(purity * 255))),
        max = 1,
        label = "Spatial purity",
        desc = "Synthetic neighbour agreement."
      )
    ),
    conf = list(
      prop_top = unname(round(stats::runif(length(positioned), 0.7, 0.99), 3)),
      prop_noise = unname(round(stats::runif(length(positioned), 0, 0.2), 3)),
      sb_total = rep(100L, length(positioned)),
      sb_umi_top = rep(80L, length(positioned))
    ),
    moran = lapply(seq_len(8L), function(index) {
      list(
        rank = as.integer(index),
        gene = genes[[index]],
        I = 0.8 - index * 0.04
      )
    }),
    evidence = list(),
    qc_examples = list()
  )
  object
}

write_tissue_png <- function(path, width, height) {
  gx <- matrix(rep(seq_len(width), each = height), nrow = height)
  gy <- matrix(rep(seq_len(height), times = width), nrow = height)
  band <- 0.5 + 0.35 * sin(gy / height * 6 * pi) * cos(gx / width * 2 * pi)
  vignette <- 1 -
    0.5 *
      (((gx - width / 2) / width)^2 +
        ((gy - height / 2) / height)^2) *
      4
  vignette[vignette < 0] <- 0
  red <- pmin(1, band * vignette * 1.05)
  green <- pmin(1, band * vignette * 0.75)
  blue <- pmin(1, band * vignette * 0.95)
  png::writePNG(
    array(c(red, green, blue), dim = c(height, width, 3)),
    path
  )
  invisible(path)
}

write_tissue_jpeg <- function(path, width, height) {
  gx <- matrix(rep(seq_len(width), each = height), nrow = height)
  gy <- matrix(rep(seq_len(height), times = width), nrow = height)
  red <- 0.35 + 0.45 * sin(gx / width * pi)^2
  green <- 0.2 + 0.55 * cos(gy / height * 2 * pi)^2
  blue <- 0.3 + 0.4 * sin((gx + gy) / (width + height) * 3 * pi)^2
  jpeg::writeJPEG(array(c(red, green, blue), dim = c(height, width, 3)), path)
  invisible(path)
}

write_fixture_tables <- function(output) {
  utils::write.csv(
    data.frame(gene = c("CD3D", "CD3E", "TRBC1", "IL7R"), score = 4:1),
    file.path(output, "marker_t_cells.csv"),
    row.names = FALSE
  )
  utils::write.table(
    data.frame(gene = c("MS4A1", "CD79A", "CD37", "CD74"), score = 4:1),
    file.path(output, "marker_b_cells.tsv"),
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  writexl::write_xlsx(
    list(
      Epithelial = data.frame(gene = c("EPCAM", "KRT8"), score = c(4, 3)),
      Fibroblast = data.frame(gene = c("COL1A1", "DCN"), score = c(4, 3)),
      Macrophage = data.frame(gene = c("CD68", "LYZ"), score = c(4, 3))
    ),
    file.path(output, "marker_panels.xlsx")
  )
  utils::write.csv(
    data.frame(
      patient_id = c("patient_a", "patient_b", "patient_c"),
      condition = c("baseline", "treated", "baseline"),
      age = c(34L, 51L, 43L)
    ),
    file.path(output, "supplementary_clinical.csv"),
    row.names = FALSE
  )
  writexl::write_xlsx(
    list(
      Clinical = data.frame(
        patient_id = c("patient_a", "patient_b", "patient_c"),
        response = c("stable", "partial", "stable")
      ),
      QC = data.frame(
        sample_id = unique(as.character(object$sample_id)),
        median_counts = seq(800L, 1300L, length.out = 6L)
      )
    ),
    file.path(output, "supplementary_tables.xlsx")
  )
  invisible(output)
}

output_arguments <- commandArgs(trailingOnly = TRUE)
if (length(output_arguments) > 1L) {
  stop("Pass at most one fixture output directory.", call. = FALSE)
}
output <- if (length(output_arguments)) {
  output_arguments[[1L]]
} else {
  file.path("inst", "builder", "fixtures", "complete_viewer")
}

dir.create(output, recursive = TRUE, showWarnings = FALSE)
set.seed(2026L)
object <- stabilize_fixture(make_complete_viewer_data_fixture())
fixture_path <- file.path(output, "complete_viewer_data.rds")
saveRDS(object, fixture_path, version = 3L, compress = FALSE)
if (!methods::is(readRDS(fixture_path), "Seurat")) {
  stop(
    "The permanent Complete Viewer data fixture did not round-trip as Seurat."
  )
}

images <- data.frame(
  name = c(
    "section_a_1_he.png",
    "section_a_1_dapi.png",
    "section_a_2_he.png",
    "section_a_2_dapi.png",
    "section_b_1_he.png",
    "section_b_1_if.jpg",
    "section_b_1_pas.png"
  ),
  width = c(320L, 280L, 300L, 260L, 360L, 340L, 320L),
  height = c(240L, 300L, 260L, 320L, 220L, 280L, 240L),
  stringsAsFactors = FALSE
)
for (index in seq_len(nrow(images))) {
  writer <- if (identical(tools::file_ext(images$name[[index]]), "jpg")) {
    write_tissue_jpeg
  } else {
    write_tissue_png
  }
  writer(
    file.path(output, images$name[[index]]),
    images$width[[index]],
    images$height[[index]]
  )
}
write_fixture_tables(output)
unlink(file.path(
  output,
  c(
    "patient_a_section_1.png",
    "patient_a_section_2.png",
    "patient_b_section_1.png",
    "patient_b_section_2.png",
    "patient_b_section_3.png",
    "section_b_1_if.png"
  )
))
cat(
  "Wrote Complete Viewer data Seurat fixture and twelve sidecars to ",
  output,
  "\n"
)
