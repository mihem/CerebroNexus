## A 10x Atera object as Seurat 6's LoadAtera() returns it exports like a
## Xenium one (see the Spatial analysis article). Built in memory: no Atera
## files are needed.
skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")

## What LoadAtera(segmentations = "cell", molecule.coordinates = TRUE,
## genes = ...) leaves, as seen on 10x's tiny Atera dataset: counts in an
## `Atera` assay, codeword/control assays next to it, one FOV "fov" with
## centroids, segmentation and the molecules of the requested genes,
## `azimuth_*` labels in the metadata, and in Misc() the run metadata and a
## live handle to the transcript file, plus the morphology entry that
## `morphology.image = TRUE` adds. (By default LoadAtera() reads the
## centroids only.)
atera_like_object <- function(n_cells = 30L, n_genes = 40L) {
  set.seed(11)
  cells <- sprintf("aaab%04d-1", seq_len(n_cells))
  genes <- paste0("Gene", seq_len(n_genes))
  counts <- methods::as(
    matrix(
      stats::rpois(n_genes * n_cells, 3),
      n_genes,
      n_cells,
      dimnames = list(genes, cells)
    ),
    "dgCMatrix"
  )
  object <- SeuratObject::CreateSeuratObject(counts, assay = "Atera")
  control <- function(prefix) {
    SeuratObject::CreateAssay5Object(
      counts = methods::as(
        matrix(
          stats::rpois(3L * n_cells, 0.2),
          3L,
          n_cells,
          dimnames = list(paste0(prefix, 1:3), cells)
        ),
        "dgCMatrix"
      )
    )
  }
  object[["ControlProbe"]] <- control("NegControlProbe-")
  object[["BlankCodeword"]] <- control("BLANK-")

  centroids <- data.frame(
    x = rep(seq(10, 290, length.out = 6L), length.out = n_cells),
    y = rep(seq(10, 190, length.out = 5L), each = 6L)[seq_len(n_cells)],
    cell = cells
  )
  segmentation <- do.call(
    rbind,
    lapply(seq_len(n_cells), function(i) {
      data.frame(
        x = centroids$x[i] + c(-4, 4, 4, -4),
        y = centroids$y[i] + c(-4, -4, 4, 4),
        cell = cells[i]
      )
    })
  )
  molecules <- data.frame(
    x = stats::runif(200L, 0, 300),
    y = stats::runif(200L, 0, 200),
    gene = sample(genes, 200L, replace = TRUE)
  )
  object[["fov"]] <- SeuratObject::CreateFOV(
    coords = list(
      centroids = SeuratObject::CreateCentroids(centroids),
      segmentations = SeuratObject::CreateSegmentation(segmentation)
    ),
    type = c("centroids", "segmentations"),
    molecules = SeuratObject::CreateMolecules(molecules),
    assay = "Atera"
  )
  object$azimuth_broad <- rep(
    c("Podocyte", "Tubule", "Immune"),
    length.out = n_cells
  )
  object$azimuth_score <- stats::runif(n_cells)
  SeuratObject::Misc(object, "run_metadata") <- list(panel = "WTA")
  SeuratObject::Misc(object, "atera.molecules") <- list(
    handle = new.env(),
    cache = molecules,
    fov = "fov"
  )
  SeuratObject::Misc(object, "atera.morphology") <- list(
    path = "morphology.ome.tif"
  )
  object
}

test_that("an Atera object exports like a Xenium one", {
  object <- atera_like_object()
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      stats::rnorm(2L * ncol(object)),
      ncol(object),
      2L,
      dimnames = list(colnames(object), c("umap_1", "umap_2"))
    ),
    key = "umap_",
    assay = "Atera"
  )

  out <- withr::local_tempdir()
  expect_no_error(convertSeuratToCerebro(
    object,
    out,
    assay = "Atera",
    nUMI = "nCount_Atera",
    nGene = "nFeature_Atera",
    organism = "hg",
    groups = "azimuth_broad",
    add_most_expressed_genes = FALSE,
    verbose = FALSE
  ))
  crb <- list.files(out, "\\.crb$", full.names = TRUE)
  expect_length(crb, 1L)
  skip_if(length(crb) != 1L, "conversion failed; see the failure above")
  data <- readCerebro(crb)

  ## Cells, per-cell totals and expression of the Atera assay
  meta <- data$getMetaData()
  expect_setequal(data$getCellNames(), colnames(object))
  expect_equal(
    unname(meta$nUMI[match(colnames(object), data$getCellNames())]),
    unname(object$nCount_Atera)
  )
  expect_true("azimuth_broad" %in% data$getGroups())
  expression <- data$getExpressionMatrix(genes = c("Gene1", "Gene2"))
  reference <- as.matrix(SeuratObject::LayerData(
    object,
    assay = "Atera",
    layer = "data"
  )[c("Gene1", "Gene2"), colnames(expression)])
  expect_equal(as.matrix(expression), reference, ignore_attr = TRUE)

  ## The FOV: centroids, segmentation and molecules
  expect_identical(names(data$spatial), "fov")
  spatial <- data$getSpatialData("fov")
  expect_equal(nrow(spatial$coordinates), ncol(object))
  expect_setequal(unique(spatial$boundaries$cell_barcode), colnames(object))
  expect_equal(spatial$molecules$total_count, 200L)
  expect_equal(nrow(spatial$molecules$data), 200L)
  expect_false(spatial$molecules$truncated)
})

test_that("the Builder picks the Atera count columns on its own", {
  builder_profile_source_runtime()
  builder_repo_source("inspect.R")
  profile <- describe_seurat(atera_like_object())$assay_profiles$Atera
  expect_identical(profile$nUMI, "nCount_Atera")
  expect_identical(profile$nGene, "nFeature_Atera")
})
