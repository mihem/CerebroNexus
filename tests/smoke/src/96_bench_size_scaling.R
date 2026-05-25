#!/usr/bin/env Rscript
# 96 — Size-scaling bench: synthetic sparse matrices across 3 fixture sizes
#
# Goal: validate two hypotheses with a single self-contained run:
#   (1) "h5 disk wins only at scale" — i.e. h5 sibling is *larger* than
#       embedded crb on small/dense fixtures, smaller on large sparse ones.
#   (2) "BPCells stays huge unless integer bit-packing is triggered" — i.e.
#       BPCells's val file is uncompressed double by default; converting
#       the matrix to uint32_t before write_matrix_dir() collapses it.
#
# For each (size × backend) combination:
#   - generate a synthetic Seurat object with Poisson(1.5)+1 sparse counts
#   - export via exportFromSeurat() (or a custom bpcells_int wrapper)
#   - record .crb size, sibling size, total disk, export wall-clock
#
# Sizes (cells × genes), density chosen to mimic scRNA-seq counts:
#   small:  500 × 2,000      (10% — close to example.crb regime)
#   medium: 50,000 × 20,000  (5%)
#   large:  500,000 × 20,000 (5%) — OPTIONAL, ~20-40 min, ~8 GB peak RAM
#
# Backends:
#   embedded     — saveRDS dgCMatrix inside .crb
#   bpcells      — exporter default (no convert_matrix_type, val stays double)
#   bpcells_int  — bpcells + BPCells::convert_matrix_type("uint32_t") before
#                  write_matrix_dir(), to probe whether the BPCells size could
#                  be reduced if exportFromSeurat were updated. NOT the default.
#   h5           — exporter default (HDF5Array::writeTENxMatrix, gzip level 4)
#
# Output: result/96_bench_size_scaling/summary.csv
#
# Run (small + medium only):
#   Rscript src/96_bench_size_scaling.R
# Include large (~20-40 min):
#   BENCH_INCLUDE_LARGE=1 Rscript src/96_bench_size_scaling.R

set.seed(42)
pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(Matrix)
  library(SeuratObject)
  library(Seurat)
  library(BPCells)
})

result_dir <- "result/96_bench_size_scaling"
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

sizes <- list(
  small  = list(ncells = 500L,    ngenes = 2000L,  density = 0.10),
  medium = list(ncells = 50000L,  ngenes = 20000L, density = 0.05)
)
if (nzchar(Sys.getenv("BENCH_INCLUDE_LARGE"))) {
  sizes$large <- list(ncells = 500000L, ngenes = 20000L, density = 0.05)
}

make_seurat <- function(ncells, ngenes, density) {
  cat(sprintf(
    "  building synthetic counts (%d × %d, density %.0f%%) ... ",
    ngenes, ncells, density * 100
  ))
  t0 <- Sys.time()
  mat <- Matrix::rsparsematrix(
    nrow = ngenes, ncol = ncells,
    density = density,
    rand.x = function(n) rpois(n, lambda = 1.5) + 1L
  )
  rownames(mat) <- paste0("Gene", seq_len(ngenes))
  colnames(mat) <- paste0("Cell", seq_len(ncells))
  obj <- SeuratObject::CreateSeuratObject(counts = mat)
  obj$sample <- sample(letters[1:3], ncol(obj), replace = TRUE)
  obj$cluster <- sample(paste0("c", 1:5), ncol(obj), replace = TRUE)
  # Fabricate a UMAP embedding so exportFromSeurat's dimreduc check passes
  # without paying for a real RunPCA + RunUMAP on a synthetic matrix.
  umap_coord <- matrix(rnorm(ncells * 2), ncells, 2)
  colnames(umap_coord) <- c("UMAP_1", "UMAP_2")
  rownames(umap_coord) <- colnames(obj)
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = umap_coord,
    key = "UMAP_",
    assay = "RNA"
  )
  cat(sprintf(
    "done (%.1fs)\n",
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ))
  obj
}

bpcells_int_export <- function(obj, file) {
  # Run the normal bpcells export first to get a tagged .crb + sibling,
  # then overwrite the sibling dir with an integer-typed BPCells matrix
  # to probe whether the bit-packing path produces a smaller payload.
  cerebroAppLite::exportFromSeurat(
    object = obj,
    file = file,
    assay = "RNA",
    slot = "counts",
    experiment_name = "synthetic",
    organism = "synthetic",
    groups = c("sample", "cluster"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    expression_matrix_mode = "bpcells",
    verbose = FALSE
  )
  bpc_dir <- sub("\\.crb$", ".bpcells", file)
  if (dir.exists(bpc_dir)) unlink(bpc_dir, recursive = TRUE)
  mat <- SeuratObject::GetAssayData(obj, layer = "counts")
  m_bp <- BPCells::convert_matrix_type(
    methods::as(mat, "IterableMatrix"),
    type = "uint32_t"
  )
  BPCells::write_matrix_dir(mat = m_bp, dir = bpc_dir)
}

bench_one_export <- function(name, ncells, ngenes, mode, obj) {
  dir <- file.path(result_dir, sprintf("%s_%s", name, mode))
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  crb <- file.path(dir, sprintf("%s.crb", name))

  t0 <- Sys.time()
  if (mode == "bpcells_int") {
    bpcells_int_export(obj, crb)
  } else {
    cerebroAppLite::exportFromSeurat(
      object = obj,
      file = crb,
      assay = "RNA",
      slot = "counts",
      experiment_name = name,
      organism = "synthetic",
      groups = c("sample", "cluster"),
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      expression_matrix_mode = mode,
      verbose = FALSE
    )
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  crb_mb <- file.info(crb)$size / 1024^2
  sib_pattern <- if (mode %in% c("bpcells", "bpcells_int")) {
    "\\.bpcells$"
  } else if (mode == "h5") {
    "\\.h5$"
  } else {
    NA
  }
  sib_mb <- if (is.na(sib_pattern)) {
    NA_real_
  } else {
    sib_path <- list.files(dir, pattern = sib_pattern, full.names = TRUE)
    if (length(sib_path) == 0) {
      NA_real_
    } else if (dir.exists(sib_path[1])) {
      sum(
        file.info(
          list.files(sib_path[1], recursive = TRUE, full.names = TRUE)
        )$size
      ) /
        1024^2
    } else {
      file.info(sib_path[1])$size / 1024^2
    }
  }
  total_mb <- crb_mb + ifelse(is.na(sib_mb), 0, sib_mb)
  data.frame(
    size = name,
    ncells = ncells,
    ngenes = ngenes,
    backend = mode,
    crb_mb = round(crb_mb, 2),
    sibling_mb = round(sib_mb, 2),
    total_mb = round(total_mb, 2),
    export_secs = round(elapsed, 2)
  )
}

backends <- c("embedded", "bpcells", "bpcells_int", "h5")

rows <- list()
for (sz_name in names(sizes)) {
  sz <- sizes[[sz_name]]
  cat(sprintf(
    "\n=== size '%s' (%d cells × %d genes, density %.0f%%) ===\n",
    sz_name,
    sz$ncells,
    sz$ngenes,
    sz$density * 100
  ))
  obj <- make_seurat(sz$ncells, sz$ngenes, sz$density)
  for (mode in backends) {
    cat(sprintf("  [%-11s] ... ", mode))
    r <- tryCatch(
      bench_one_export(sz_name, sz$ncells, sz$ngenes, mode, obj),
      error = function(e) {
        cat(sprintf("FAILED: %s\n", conditionMessage(e)))
        data.frame(
          size = sz_name,
          ncells = sz$ncells,
          ngenes = sz$ngenes,
          backend = mode,
          crb_mb = NA_real_,
          sibling_mb = NA_real_,
          total_mb = NA_real_,
          export_secs = NA_real_
        )
      }
    )
    if (!is.na(r$total_mb)) {
      cat(sprintf(
        "crb=%6.1f MB, sib=%s, total=%7.1f MB (%5.1fs)\n",
        r$crb_mb,
        ifelse(is.na(r$sibling_mb), "       —", sprintf("%6.1f MB", r$sibling_mb)),
        r$total_mb,
        r$export_secs
      ))
    }
    rows[[length(rows) + 1]] <- r
  }
  rm(obj)
  gc(verbose = FALSE)
}

results <- do.call(rbind, rows)
out_csv <- file.path(result_dir, "summary.csv")
write.csv(results, out_csv, row.names = FALSE)
cat(sprintf("\nwrote %s\n\n", out_csv))
print(results)
