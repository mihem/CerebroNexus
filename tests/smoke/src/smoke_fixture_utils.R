SMOKE_SYNTHETIC_VERSION <- "2026-05-25-v4"

smoke_fixture_manifest_path <- function(data_dir = "data") {
  file.path(data_dir, ".synthetic_fixtures_manifest")
}

smoke_fixture_profile <- function() {
  scale_raw <- Sys.getenv("SMOKE_SYNTH_SCALE", unset = "1")
  scale <- suppressWarnings(as.numeric(scale_raw))
  if (!is.finite(scale) || scale <= 0) {
    scale <- 1
  }

  scale_int <- function(x) {
    max(50L, as.integer(round(x * scale)))
  }

  list(
    myeloid   = list(genes = scale_int(4000), cells = scale_int(8000),  density = 0.025),
    fibro_ctrl = list(genes = scale_int(2500), cells = scale_int(6000),  density = 0.030),
    fibro_ms  = list(genes = scale_int(2500), cells = scale_int(6000),  density = 0.030),
    pbmc_vdj  = list(genes = scale_int(5000), cells = scale_int(15000), density = 0.008),
    pbmc_all  = list(genes = scale_int(38606), cells = scale_int(147756), density = 0.02)
  )
}

random_sparse_counts <- function(gene_names, cell_ids, density, lambda = 5) {
  n_genes <- length(gene_names)
  n_cells <- length(cell_ids)
  nnz <- max(1L, as.integer(round(as.double(n_genes) * as.double(n_cells) * density)))
  mat <- Matrix::sparseMatrix(
    i = sample.int(n_genes, size = nnz, replace = TRUE),
    j = sample.int(n_cells, size = nnz, replace = TRUE),
    x = stats::rpois(nnz, lambda = lambda) + 1,
    dims = c(n_genes, n_cells),
    giveCsparse = TRUE
  )
  mat <- methods::as(mat, "dgCMatrix")
  rownames(mat) <- gene_names
  colnames(mat) <- cell_ids
  mat
}

add_marker_signal <- function(counts, celltypes, marker_map, signal_fraction = 0.7, lambda = 8) {
  gene_index <- stats::setNames(seq_len(nrow(counts)), rownames(counts))
  cell_groups <- split(seq_along(celltypes), celltypes)
  spike_i <- integer(0)
  spike_j <- integer(0)
  spike_x <- numeric(0)

  for (celltype in names(marker_map)) {
    marker_genes <- intersect(marker_map[[celltype]], rownames(counts))
    target_cells <- cell_groups[[celltype]]
    if (length(marker_genes) == 0 || is.null(target_cells) || length(target_cells) == 0) {
      next
    }

    n_signal <- max(1L, as.integer(round(length(target_cells) * signal_fraction)))
    selected_cells <- sort(sample(target_cells, size = min(length(target_cells), n_signal)))

    for (gene in marker_genes) {
      spike_i <- c(spike_i, rep.int(gene_index[[gene]], length(selected_cells)))
      spike_j <- c(spike_j, selected_cells)
      spike_x <- c(spike_x, stats::rpois(length(selected_cells), lambda = lambda) + 1)
    }
  }

  if (length(spike_x) == 0) {
    return(counts)
  }

  counts + Matrix::sparseMatrix(
    i = spike_i,
    j = spike_j,
    x = spike_x,
    dims = dim(counts),
    dimnames = dimnames(counts)
  )
}

cluster_embeddings <- function(cell_ids, cluster_ids, seed, prefix) {
  set.seed(seed)
  clusters <- unique(cluster_ids)
  centers <- stats::setNames(seq_along(clusters), clusters)
  xy <- vapply(seq_along(cell_ids), function(i) {
    center_idx <- centers[[cluster_ids[[i]]]]
    c(
      stats::rnorm(1, mean = center_idx * 3.2, sd = 0.45),
      stats::rnorm(1, mean = center_idx * 1.8, sd = 0.45)
    )
  }, numeric(2))
  xy <- t(xy)
  rownames(xy) <- cell_ids
  colnames(xy) <- c(paste0(prefix, "_1"), paste0(prefix, "_2"))
  xy
}

add_reductions <- function(object, cluster_ids, seed, assay) {
  umap <- cluster_embeddings(
    cell_ids = colnames(object),
    cluster_ids = cluster_ids,
    seed = seed,
    prefix = "UMAP"
  )
  pca <- cluster_embeddings(
    cell_ids = colnames(object),
    cluster_ids = cluster_ids,
    seed = seed + 100,
    prefix = "PC"
  )

  object[["umap"]] <- Seurat::CreateDimReducObject(
    embeddings = umap,
    key = "UMAP_",
    assay = assay
  )
  object[["pca"]] <- Seurat::CreateDimReducObject(
    embeddings = pca,
    key = "PC_",
    assay = assay
  )
  object
}

write_placeholder_jpg <- function(path, label, seed) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::jpeg(path, width = 900, height = 650, quality = 85)
  on.exit(grDevices::dev.off(), add = TRUE)

  set.seed(seed)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::rect(0, 0, 1, 1, col = "#f5efe6", border = NA)
  for (i in seq_len(14)) {
    x0 <- stats::runif(1, 0.02, 0.82)
    y0 <- stats::runif(1, 0.05, 0.78)
    w <- stats::runif(1, 0.08, 0.22)
    h <- stats::runif(1, 0.05, 0.18)
    graphics::rect(
      x0,
      y0,
      min(0.98, x0 + w),
      min(0.95, y0 + h),
      col = grDevices::adjustcolor(sample(c("#d08c60", "#d8b48a", "#8a5a44", "#f2d7b5"), 1), 0.6),
      border = NA
    )
  }
  graphics::text(0.05, 0.95, label, adj = c(0, 1), cex = 1.6, font = 2, col = "#5c3b2e")
  graphics::text(0.05, 0.89, "synthetic histology placeholder", adj = c(0, 1), cex = 0.9, col = "#6a5147")
}

save_qs <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  qs::qsave(object, path, preset = "balanced")
}

make_gene_names <- function(core_genes, total_genes, prefix) {
  fillers_needed <- max(0L, total_genes - length(core_genes))
  c(core_genes, paste0(prefix, sprintf("%04d", seq_len(fillers_needed))))
}

build_myeloid_fixture <- function(spec) {
  cell_ids <- paste0("myeloid_cell_", seq_len(spec$cells))
  clusters <- sample(
    c("inflamMono", "CAM", "IFN-CAM", "Granulo", "Mast"),
    size = spec$cells,
    replace = TRUE,
    prob = c(0.32, 0.22, 0.18, 0.18, 0.10)
  )
  gene_names <- make_gene_names(
    c("LST1", "FCN1", "S100A8", "TYMP", "IFITM3", "CST3", "IL1B", "MS4A7", "CTSS", "FCGR3A", "GAPDH", "ACTB"),
    spec$genes,
    "MYE"
  )
  counts <- random_sparse_counts(gene_names, cell_ids, density = spec$density, lambda = 4)

  meta <- data.frame(
    sample_id = sample(c("DM_01", "DM_02", "DM_03"), spec$cells, replace = TRUE),
    condition = sample(c("Ctrl", "MS"), spec$cells, replace = TRUE, prob = c(0.55, 0.45)),
    biobank = sample(c("BioA", "BioB"), spec$cells, replace = TRUE),
    annotated_myeloid = clusters,
    row.names = cell_ids,
    stringsAsFactors = FALSE
  )

  object <- Seurat::CreateSeuratObject(
    counts = counts,
    assay = "SCT",
    meta.data = meta
  )
  object <- Seurat::NormalizeData(object, assay = "SCT", verbose = FALSE)
  object@meta.data$nCount_RNA <- object@meta.data$nCount_SCT
  object@meta.data$nFeature_RNA <- object@meta.data$nFeature_SCT
  object <- add_reductions(object, cluster_ids = clusters, seed = 101, assay = "SCT")
  object
}

build_spatial_fixture <- function(spec, cohort_label) {
  cell_ids <- paste0(cohort_label, "_spot_", seq_len(spec$cells))
  clusters <- sample(
    c("duraFibro1-3", "duraFibro3", "bordFibro", "duraFibro4"),
    size = spec$cells,
    replace = TRUE,
    prob = c(0.35, 0.25, 0.20, 0.20)
  )
  gene_names <- make_gene_names(
    c("COL1A1", "COL3A1", "DCN", "LUM", "COL6A3", "RGS5", "TAGLN", "VWF", "GAPDH", "ACTB"),
    spec$genes,
    "FIB"
  )
  counts <- random_sparse_counts(gene_names, cell_ids, density = spec$density, lambda = 6)

  meta <- data.frame(
    sample_id = sample(paste0(cohort_label, c("_ROI1", "_ROI2")), spec$cells, replace = TRUE),
    annotated_manuscript = clusters,
    row.names = cell_ids,
    stringsAsFactors = FALSE
  )

  object <- Seurat::CreateSeuratObject(
    counts = counts,
    assay = "Xenium",
    meta.data = meta
  )
  object <- Seurat::NormalizeData(object, assay = "Xenium", verbose = FALSE)
  object <- add_reductions(object, cluster_ids = clusters, seed = if (cohort_label == "Ctrl") 202 else 303, assay = "Xenium")

  coords <- data.frame(
    x = stats::runif(spec$cells, min = 0, max = 1000),
    y = stats::runif(spec$cells, min = 0, max = 850),
    cell = cell_ids,
    stringsAsFactors = FALSE
  )
  centroids <- SeuratObject::CreateCentroids(coords = coords)
  object[[paste0(cohort_label, "_fov")]] <- SeuratObject::CreateFOV(
    coords = centroids,
    type = "centroids",
    assay = "Xenium"
  )
  object
}

build_pbmc_vdj_fixture <- function(spec) {
  cell_ids <- paste0("PBMC1002_", seq_len(spec$cells))
  celltypes <- sample(
    c("T cell", "B cell", "NK cell", "Mono"),
    size = spec$cells,
    replace = TRUE,
    prob = c(0.42, 0.22, 0.18, 0.18)
  )
  gene_names <- make_gene_names(
    c("CD3D", "TRBC1", "MS4A1", "CD79A", "NKG7", "GNLY", "LST1", "FCGR3A", "GAPDH", "ACTB"),
    spec$genes,
    "PBM"
  )
  counts <- random_sparse_counts(gene_names, cell_ids, density = spec$density, lambda = 3)
  counts <- add_marker_signal(
    counts,
    celltypes,
    marker_map = list(
      "T cell" = c("CD3D", "TRBC1"),
      "B cell" = c("MS4A1", "CD79A"),
      "NK cell" = c("NKG7", "GNLY"),
      "Mono" = c("LST1", "FCGR3A")
    ),
    signal_fraction = 0.8,
    lambda = 10
  )

  meta <- data.frame(
    celltype_merged.l1 = celltypes,
    timepoint = "Post",
    treatment = sample(c("Vehicle", "Drug"), spec$cells, replace = TRUE),
    sample = "PBMC_1002",
    orig.ident = "PBMC_1002",
    row.names = cell_ids,
    stringsAsFactors = FALSE
  )

  repertoire_mask <- stats::runif(spec$cells) < 0.55
  ctgene <- rep(NA_character_, spec$cells)
  ctgene[repertoire_mask & celltypes == "T cell"] <- sample(c("TRAV1-2;TRBV20-1", "TRAV12-2;TRBV7-9", "TRAV26-1;TRBV5-1"), sum(repertoire_mask & celltypes == "T cell"), replace = TRUE)
  ctgene[repertoire_mask & celltypes == "B cell"] <- sample(c("IGHV3-23;IGKV1-39", "IGHV4-34;IGLV2-14", "IGHV1-69;IGKV3-20"), sum(repertoire_mask & celltypes == "B cell"), replace = TRUE)
  meta$CTgene <- ctgene
  meta$CTnt <- ifelse(is.na(ctgene), NA_character_, paste0("nt_", sample(100000:999999, spec$cells, replace = TRUE)))
  meta$CTaa <- ifelse(is.na(ctgene), NA_character_, paste0("CASS", sample(LETTERS, spec$cells, replace = TRUE), sample(100:999, spec$cells, replace = TRUE), "EQYF"))
  meta$CTstrict <- ifelse(is.na(ctgene), NA_character_, paste0("strict_", sample(1000:9999, spec$cells, replace = TRUE)))

  object <- Seurat::CreateSeuratObject(
    counts = counts,
    assay = "RNA",
    meta.data = meta
  )
  object <- add_reductions(object, cluster_ids = celltypes, seed = 404, assay = "RNA")
  object
}

build_pbmc_all_fixture <- function(spec) {
  cell_ids <- paste0("PBMC_all_", seq_len(spec$cells))
  celltypes <- sample(
    c("T cell", "B cell", "NK cell", "Mono", "DC"),
    size = spec$cells,
    replace = TRUE,
    prob = c(0.38, 0.18, 0.18, 0.18, 0.08)
  )
  gene_names <- make_gene_names(
    c("CD3D", "MS4A1", "NKG7", "GNLY", "LST1", "FCGR3A", "HLA-DRA", "FCER1A", "GAPDH", "ACTB", "MALAT1"),
    spec$genes,
    "ALL"
  )
  counts <- random_sparse_counts(gene_names, cell_ids, density = spec$density, lambda = 3)
  counts <- add_marker_signal(
    counts,
    celltypes,
    marker_map = list(
      "T cell" = c("CD3D"),
      "B cell" = c("MS4A1"),
      "NK cell" = c("NKG7", "GNLY"),
      "Mono" = c("LST1", "FCGR3A"),
      "DC" = c("HLA-DRA", "FCER1A")
    ),
    signal_fraction = 0.75,
    lambda = 9
  )

  meta <- data.frame(
    celltype_merged.l1 = celltypes,
    timepoint = sample(c("Pre", "Post"), spec$cells, replace = TRUE),
    sample = sample(c("PBMC_1001", "PBMC_1002", "PBMC_1003", "PBMC_1004"), spec$cells, replace = TRUE),
    row.names = cell_ids,
    stringsAsFactors = FALSE
  )

  object <- Seurat::CreateSeuratObject(
    counts = counts,
    assay = "RNA",
    meta.data = meta
  )
  object <- add_reductions(object, cluster_ids = celltypes, seed = 505, assay = "RNA")
  object
}

write_marker_table <- function(path) {
  markers <- data.frame(
    cluster = c("inflamMono", "CAM", "IFN-CAM", "Granulo", "Mast"),
    gene = c("LST1", "FCN1", "IFITM3", "S100A8", "CST3"),
    avg_log2FC = c(1.8, 1.5, 1.7, 1.9, 1.2),
    pct.1 = c(0.82, 0.75, 0.78, 0.84, 0.71),
    pct.2 = c(0.21, 0.24, 0.19, 0.18, 0.22),
    p_val_adj = c(1e-12, 5e-10, 3e-11, 8e-14, 4e-09),
    stringsAsFactors = FALSE
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(markers, path, overwrite = TRUE)
}

write_manifest <- function(data_dir, backup_dir, profile) {
  manifest <- c(
    paste0("version=", SMOKE_SYNTHETIC_VERSION),
    paste0("created_at=", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("backup_dir=", if (is.null(backup_dir)) "" else backup_dir),
    paste0("myeloid=", profile$myeloid$genes, "x", profile$myeloid$cells),
    paste0("fibro_ctrl=", profile$fibro_ctrl$genes, "x", profile$fibro_ctrl$cells),
    paste0("fibro_ms=", profile$fibro_ms$genes, "x", profile$fibro_ms$cells),
    paste0("pbmc_vdj=", profile$pbmc_vdj$genes, "x", profile$pbmc_vdj$cells),
    paste0("pbmc_all=", profile$pbmc_all$genes, "x", profile$pbmc_all$cells)
  )
  writeLines(manifest, smoke_fixture_manifest_path(data_dir))
}

ensure_smoke_fixtures <- function(data_dir = "data", force = FALSE, verbose = TRUE) {
  manifest_path <- smoke_fixture_manifest_path(data_dir)
  existing_manifest <- if (file.exists(manifest_path)) readLines(manifest_path, warn = FALSE) else character(0)
  version_ok <- any(existing_manifest == paste0("version=", SMOKE_SYNTHETIC_VERSION))
  is_previous_synthetic <- any(grepl("^version=", existing_manifest))

  if (dir.exists(data_dir) && version_ok && !force) {
    if (verbose) {
      message("[smoke] Synthetic fixtures already available in ", data_dir)
    }
    return(invisible(list(data_dir = data_dir, backup_dir = NULL, regenerated = FALSE)))
  }

  backup_dir <- NULL
  if (dir.exists(data_dir) && !version_ok) {
    existing_entries <- setdiff(list.files(data_dir, all.files = TRUE, no.. = TRUE), ".DS_Store")
    if (length(existing_entries) > 0 && !is_previous_synthetic) {
      backup_dir <- paste0(data_dir, "_private_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      if (verbose) {
        message("[smoke] Renaming existing data directory to ", backup_dir)
      }
      ok <- file.rename(data_dir, backup_dir)
      if (!ok) {
        stop("Failed to rename existing data directory to ", backup_dir, call. = FALSE)
      }
    } else if (length(existing_entries) > 0 && verbose) {
      message("[smoke] Rebuilding previous synthetic fixtures in ", data_dir)
    }
  }

  if (dir.exists(data_dir)) {
    unlink(file.path(data_dir, "*"), recursive = TRUE, force = TRUE)
  } else {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  profile <- smoke_fixture_profile()

  suppressPackageStartupMessages({
    library(Matrix)
    library(Seurat)
    library(SeuratObject)
    library(qs)
    library(openxlsx)
  })

  set.seed(1234567)
  myeloid <- build_myeloid_fixture(profile$myeloid)
  fibro_ctrl <- build_spatial_fixture(profile$fibro_ctrl, cohort_label = "Ctrl")
  fibro_ms <- build_spatial_fixture(profile$fibro_ms, cohort_label = "MS")
  pbmc_vdj <- build_pbmc_vdj_fixture(profile$pbmc_vdj)
  pbmc_all <- build_pbmc_all_fixture(profile$pbmc_all)

  save_qs(myeloid, file.path(data_dir, "39_shinyApp", "39.0_samples_seurat_reclustered_myeloid_subset.qs"))
  save_qs(fibro_ctrl, file.path(data_dir, "39_shinyApp", "10.3.1_sc_Ctrl_duraFibro_both_integrated.qs"))
  save_qs(fibro_ms, file.path(data_dir, "39_shinyApp", "10.3.1_sc_MS_duraFibro_both_integrated.qs"))
  save_qs(pbmc_vdj, file.path(data_dir, "tcr_bcr", "seurat_PBMC_1002_Post_VDJ.qs"))
  save_qs(pbmc_all, file.path(data_dir, "21_S04_seurat_integrated_STACAS_standard_pipeline.qs"))

  write_marker_table(file.path(data_dir, "39_shinyApp", "36.1_myeloid_Cluster_Markers.xlsx"))
  write_placeholder_jpg(file.path(data_dir, "39_shinyApp", "Xenium_Ctrl_ROI_HE.jpg"), "Ctrl dura fibro", seed = 616)
  write_placeholder_jpg(file.path(data_dir, "39_shinyApp", "Xenium_MS_ROI_HE.jpg"), "MS dura fibro", seed = 717)

  write_manifest(data_dir = data_dir, backup_dir = backup_dir, profile = profile)

  if (verbose) {
    message("[smoke] Synthetic fixtures ready in ", data_dir)
  }

  invisible(list(data_dir = data_dir, backup_dir = backup_dir, regenerated = TRUE, profile = profile))
}

# ── Shared conversion helpers ────────────────────────────────────────────────

smoke_xlsx_to_csv <- function(xlsx_path, tmp_dir = file.path(tempdir(), "smoke_convert")) {
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  csv_path <- file.path(tmp_dir, sub("\\.xlsx$", ".csv", basename(xlsx_path)))
  if (!file.exists(csv_path)) {
    df <- openxlsx::read.xlsx(xlsx_path)
    utils::write.csv(df, csv_path, row.names = FALSE)
  }
  csv_path
}

smoke_prepare_spatial <- function(obj) {
  colnames(obj@meta.data)[colnames(obj@meta.data) == "nCount_Xenium"]   <- "nCount_RNA"
  colnames(obj@meta.data)[colnames(obj@meta.data) == "nFeature_Xenium"] <- "nFeature_RNA"
  obj
}

smoke_compare_size <- function(crb_path, backend_label, embedded_dir = "result/10_convert_embedded") {
  stem <- basename(crb_path)
  embedded_crb <- file.path(embedded_dir, stem)
  crb_size <- file.info(crb_path)$size

  if (backend_label == "h5") {
    h5_sibling <- sub("\\.crb$", ".h5", crb_path)
    h5_size <- if (file.exists(h5_sibling)) file.info(h5_sibling)$size else NA_real_
    cat(sprintf("  h5 crb     : %s bytes\n", format(crb_size, big.mark = ",")))
    if (!is.na(h5_size)) {
      cat(sprintf("  .h5 sibling: %s bytes\n", format(h5_size, big.mark = ",")))
      cat(sprintf("  total      : %s bytes\n", format(crb_size + h5_size, big.mark = ",")))
    }
    if (file.exists(embedded_crb)) {
      emb_size <- file.info(embedded_crb)$size
      cat(sprintf("  embedded crb (for comparison): %s bytes\n", format(emb_size, big.mark = ",")))
      if (!is.na(h5_size)) {
        cat(sprintf("  reduction vs embedded: %.0f%%\n",
                    (1 - (crb_size + h5_size) / emb_size) * 100))
      }
    }
  } else {
    cat(sprintf("  %s crb: %s bytes\n", backend_label, format(crb_size, big.mark = ",")))
    if (file.exists(embedded_crb)) {
      emb_size <- file.info(embedded_crb)$size
      cat(sprintf("  embedded crb: %s bytes\n", format(emb_size, big.mark = ",")))
      cat(sprintf("  reduction: %.0f%%\n", (1 - crb_size / emb_size) * 100))
    }
  }
}

# Returns a list of 5 fixture configs. Each entry has:
#   seurat_path, assay, slot, experiment_name, organism,
#   groups, groups_naming, marker_file, jpg_from, patterns
smoke_fixture_configs <- function() {
  marker_xlsx <- "data/39_shinyApp/36.1_myeloid_Cluster_Markers.xlsx"

  list(
    myeloid = list(
      seurat_path     = "data/39_shinyApp/39.0_samples_seurat_reclustered_myeloid_subset.qs",
      assay           = "SCT",
      slot            = "data",
      experiment_name = "Dura Mater - Myeloid cells snRNaseq",
      organism        = "Human",
      groups          = c("sample_id", "condition", "biobank", "annotated_myeloid"),
      groups_naming   = list(sample_id = "sample", annotated_myeloid = "cluster"),
      marker_file     = marker_xlsx,
      patterns        = "myeloid.*\\.crb$"
    ),
    fibro_ctrl = list(
      seurat_path     = "data/39_shinyApp/10.3.1_sc_Ctrl_duraFibro_both_integrated.qs",
      assay           = "Xenium",
      slot            = "data",
      experiment_name = "Ctrl Dura Mater - Fibroblasts spatialseq",
      organism        = "Human",
      groups          = c("annotated_manuscript", "sample_id"),
      groups_naming   = list(annotated_manuscript = "cluster", sample_id = "sample"),
      is_spatial      = TRUE,
      jpg_from        = "data/39_shinyApp/Xenium_Ctrl_ROI_HE.jpg",
      patterns        = "Ctrl.*\\.crb$"
    ),
    fibro_ms = list(
      seurat_path     = "data/39_shinyApp/10.3.1_sc_MS_duraFibro_both_integrated.qs",
      assay           = "Xenium",
      slot            = "data",
      experiment_name = "MS Dura Mater - Fibroblasts spatialseq",
      organism        = "Human",
      groups          = c("annotated_manuscript", "sample_id"),
      groups_naming   = list(annotated_manuscript = "cluster", sample_id = "sample"),
      is_spatial      = TRUE,
      jpg_from        = "data/39_shinyApp/Xenium_MS_ROI_HE.jpg",
      patterns        = "MS.*\\.crb$"
    ),
    pbmc_vdj = list(
      seurat_path     = "data/tcr_bcr/seurat_PBMC_1002_Post_VDJ.qs",
      assay           = "RNA",
      slot            = "counts",
      experiment_name = "PBMC 1002 Post TCR/BCR",
      organism        = "Human PBMC",
      groups          = c("celltype_merged.l1", "timepoint"),
      patterns        = "PBMC_1002.*\\.crb$"
    ),
    pbmc_all = list(
      seurat_path     = "data/21_S04_seurat_integrated_STACAS_standard_pipeline.qs",
      assay           = "RNA",
      slot            = "counts",
      experiment_name = "PBMC All Samples TCR/BCR",
      organism        = "Human PBMC",
      groups          = c("celltype_merged.l1", "timepoint", "sample"),
      patterns        = "All_Samples.*\\.crb$"
    )
  )
}

smoke_convert_fixtures <- function(backend = c("embedded", "bpcells", "h5"),
                                   result_dir = NULL, verbose = TRUE) {
  backend <- match.arg(backend)

  if (is.null(result_dir)) {
    dir_map <- c(embedded = "result/10_convert_embedded",
                 bpcells  = "result/11_convert_bpcells",
                 h5       = "result/12_convert_h5")
    result_dir <- dir_map[[backend]]
  }

  if (dir.exists(result_dir)) {
    unlink(file.path(result_dir, "*"), recursive = TRUE, force = TRUE)
  } else {
    dir.create(result_dir, recursive = TRUE)
  }

  # Only load extra deps when actually needed
  pkgs <- c("dplyr", "Seurat", "openxlsx")
  if (backend == "bpcells") pkgs <- c(pkgs, "BPCells")
  if (backend == "h5")      pkgs <- c(pkgs, "HDF5Array")

  pkg_root <- file.path(dirname(getwd()), "..")
  suppressPackageStartupMessages({
    devtools::load_all(pkg_root, quiet = TRUE)
    for (p in pkgs) library(p, character.only = TRUE)
  })

  options(width = 100)
  set.seed(1234567)

  mode_args <- if (backend != "embedded") {
    list(expression_matrix_mode = backend)
  } else {
    list()
  }

  cfgs <- smoke_fixture_configs()
  n <- length(cfgs)
  labels <- c("Myeloid snRNA-seq",
              "Ctrl Fibroblasts spatial",
              "MS Fibroblasts spatial",
              "PBMC 1002 Post TCR/BCR",
              "PBMC All Samples")
  names(labels) <- names(cfgs)

  if (verbose) cat(sprintf("=== Seurat -> Cerebro (%s mode) ===\n\n", backend))

  for (i in seq_along(cfgs)) {
    name <- names(cfgs)[[i]]
    cfg <- cfgs[[name]]

    if (verbose) cat(sprintf("[%d/%d] %s\n", i, n, labels[[name]]))

    obj <- qs::qread(cfg$seurat_path)
    if (isTRUE(cfg$is_spatial)) {
      obj <- smoke_prepare_spatial(obj)
    }

    args <- c(
      list(seurat_file    = obj,
           result_dir     = result_dir,
           assay          = cfg$assay,
           slot           = cfg$slot,
           experiment_name = cfg$experiment_name,
           organism       = cfg$organism,
           groups         = cfg$groups),
      if (!is.null(cfg$groups_naming)) list(groups_naming = cfg$groups_naming),
      if (!is.null(cfg$marker_file)) {
        list(marker_file = smoke_xlsx_to_csv(cfg$marker_file))
      },
      mode_args
    )

    do.call(convertSeuratToCerebro, args)

    if (!is.null(cfg$jpg_from)) {
      file.copy(cfg$jpg_from, file.path(result_dir, basename(cfg$jpg_from)))
    }

    crb <- list.files(result_dir, pattern = cfg$patterns, full.names = TRUE)[1]
    if (!is.na(crb)) smoke_compare_size(crb, backend)
    if (verbose) cat("\n")
  }

  if (verbose) {
    cat(sprintf("=== All %s conversions complete ===\n", backend))
    cat("Output:", result_dir, "\nArtifacts:\n")
    for (f in list.files(result_dir, recursive = TRUE)) {
      cat(sprintf("  %s\n", f))
    }
  }

  invisible(result_dir)
}