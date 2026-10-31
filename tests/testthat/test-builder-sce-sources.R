## SingleCellExperiment and AnnData files as Builder sources.
##
## They enter through builder_read_object(), which hands everything downstream a
## Seurat object, so the inspect / profile / export paths are the ones Seurat
## datasets already use.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")
skip_if_not_installed("SeuratObject")

builder_repo_source("io.R", local = globalenv())
builder_profile_source_runtime(local = globalenv())
builder_repo_source("inspect.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())

fixture <- function(name) testthat::test_path("fixtures", "anndata", name)
quiet <- function(x) suppressWarnings(suppressMessages(x))

test_that("the format registry claims AnnData files next to the Seurat ones", {
  extensions <- builder_dataset_extensions()
  expect_true(all(c("rds", "qs2", "qs", "h5ad", "h5mu") %in% extensions))
  expect_identical(builder_match_format("a/b/data.h5ad")$id, "h5ad")
  expect_identical(builder_match_format("data.H5MU")$id, "h5mu")
})

test_that("an h5ad file is read as a Seurat object with its reductions and results", {
  result <- quiet(builder_read_object(fixture("small_csr.h5ad")))
  expect_null(result$error)
  expect_s4_class(result$object, "Seurat")
  expect_identical(result$format, "h5ad")
  expect_equal(ncol(result$object), 120)
  expect_true(all(c("PCA", "UMAP") %in% names(result$object@reductions)))
  expect_true("marker_genes" %in% names(result$object@misc))
  expect_true(all(
    c("cell_type", "donor") %in% colnames(result$object@meta.data)
  ))
})

test_that("a SingleCellExperiment saved as .rds or .qs2 is read the same way", {
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  path <- file.path(withr::local_tempdir(), "sce.rds")
  saveRDS(sce, path)
  from_rds <- builder_read_object(path)
  expect_null(from_rds$error)
  expect_s4_class(from_rds$object, "Seurat")
  expect_identical(colnames(from_rds$object), colnames(sce))

  skip_if_not_installed("qs2")
  qs_path <- file.path(dirname(path), "sce.qs2")
  qs2::qs_save(sce, qs_path)
  from_qs <- builder_read_object(qs_path)
  expect_null(from_qs$error)
  expect_s4_class(from_qs$object, "Seurat")
})

test_that("objects that are neither Seurat nor SingleCellExperiment are still refused", {
  path <- file.path(withr::local_tempdir(), "number.rds")
  saveRDS(1L, path)
  expect_identical(
    builder_read_object(path)$error,
    "The file contains a integer object, not a Seurat object."
  )
})

test_that("counts, data layers and spatial coordinates are placed by what they hold", {
  sce <- quiet(readH5adAsSCE(fixture("small_spatial.h5ad")))
  seurat <- convertSCEToSeurat(sce)
  expect_identical(SeuratObject::Layers(seurat), "counts")
  ## coordinates become a spatial image with the cells' centroids
  expect_identical(SeuratObject::Images(seurat), "spatial")
  expect_false("spatial" %in% names(seurat@reductions))
  tissue <- SeuratObject::GetTissueCoordinates(seurat[["spatial"]])
  expect_equal(
    unname(tissue$x[match(colnames(sce), tissue$cell)]),
    unname(SingleCellExperiment::reducedDim(sce, "spatial")[, 1])
  )

  SummarizedExperiment::assay(
    sce,
    "logcounts"
  ) <- log1p(SummarizedExperiment::assay(sce, "X"))
  SummarizedExperiment::assay(sce, "counts") <- SummarizedExperiment::assay(
    sce,
    "X"
  )
  both <- convertSCEToSeurat(sce)
  expect_setequal(SeuratObject::Layers(both), c("counts", "data"))
})

test_that("an AnnData source keeps the md5 of the file, not of its snapshot", {
  path <- fixture("small_csr.h5ad")
  inspected <- quiet(builder_adapter_inspect(builder_seurat_file_adapter(path)))
  ## the h5ad is converted, so no snapshot is a copy of it
  expect_null(inspected$snapshot_source)
  expect_identical(
    inspected$source_md5,
    unname(as.character(tools::md5sum(path)))
  )
})

test_that("an AnnData file goes through the Builder's own inspect path", {
  adapter <- builder_seurat_file_adapter(fixture("small_csr.h5ad"))
  expect_identical(adapter$format, "h5ad")
  inspected <- quiet(builder_adapter_inspect(adapter))
  profile <- inspected$profile
  expect_s3_class(profile, "builder_dataset_profile")
  expect_identical(profile$identity$cells$count, 120L)
  expect_true(profile$identity$cells$valid)
  expect_true(all(
    c("cell_type", "donor") %in% names(profile$groups$legacy$levels)
  ))
})

test_that("the same data as Seurat and as AnnData gives the same profile shape", {
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  converted <- convertSCEToSeurat(sce)
  source <- list(
    type = "example",
    location = "x",
    fingerprint = "f",
    format = "rds"
  )
  a <- quiet(builder_dataset_profile(converted, source))
  ## the Seurat object a user would have built by hand from the same parts
  manual <- SeuratObject::CreateSeuratObject(
    SeuratObject::LayerData(converted, "counts"),
    meta.data = converted@meta.data
  )
  b <- quiet(builder_dataset_profile(manual, source))
  expect_identical(names(a), names(b))
  expect_identical(a$schema_version, b$schema_version)
  expect_identical(a$identity$cells$ids, b$identity$cells$ids)
  expect_identical(a$identity$features$ids, b$identity$features$ids)
  expect_identical(names(a$groups), names(b$groups))
})

test_that("an exported .crb from the converted object matches the direct conversion", {
  skip_if_not_installed("Seurat")
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  seurat <- convertSCEToSeurat(sce)
  out <- withr::local_tempdir()
  via_seurat <- file.path(out, "via_seurat.crb")
  quiet(exportFromSeurat(
    seurat,
    assay = "RNA",
    slot = "counts",
    file = via_seurat,
    experiment_name = "x",
    organism = "hg",
    groups = c("cell_type", "donor"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    verbose = FALSE,
    viewer_binary = "never"
  ))
  direct <- quiet(convertSCEToCerebro(
    sce,
    file.path(out, "direct"),
    assay = "X",
    groups = c("cell_type", "donor"),
    viewer_binary = "never",
    verbose = FALSE,
    .source_name = "small.h5ad"
  ))
  a <- readCerebro(via_seurat)
  b <- readCerebro(direct)
  expect_identical(a$getMetaData()$cell_barcode, b$getMetaData()$cell_barcode)
  expect_identical(sort(a$getGroups()), sort(b$getGroups()))
  expect_setequal(names(a$projections), names(b$projections))
  expect_identical(names(a$marker_genes), names(b$marker_genes))
  expect_identical(names(a$marker_genes[[1]]), names(b$marker_genes[[1]]))
})

## ---------------------------------------------------------------------------
## the whole Builder pipeline: file -> inspect -> snapshot -> plan -> build
## ---------------------------------------------------------------------------

builder_e2e_source_runtime(local = globalenv())

build_through_builder <- function(path, id, configure = identity) {
  inspected_object <- quiet(builder_read_object(path))$object
  record <- list(
    id = id,
    label = id,
    make = function() list(object = inspected_object)
  )
  entry <- builder_e2e_entry(record)
  root <- withr::local_tempdir(.local_envir = parent.frame())
  snapshot <- builder_snapshot_seurat(
    inspected_object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  entry$snapshot <- snapshot
  entry <- configure(entry)
  plan <- builder_freeze_plan(
    list(entry),
    file.path(root, "release"),
    make_app = FALSE
  )
  expect_null(plan$error)
  stage <- file.path(root, "stage")
  dir.create(stage)
  result <- quiet(builder_execute_plan(
    plan,
    stage,
    stats::setNames(list(snapshot), id)
  ))
  expect_identical(result$state, "success", info = result$error %||% "")
  list(result = result, entry = entry, crb = qs2::qs_read(result$built[[1L]]))
}

test_that("an AnnData file builds a .crb through the Builder, with its results", {
  skip_if_not_installed("qs2")
  built <- build_through_builder(fixture("small_csr.h5ad"), "h5ad_small")
  crb <- built$crb
  expect_s3_class(crb, "Cerebro")
  expect_equal(nrow(crb$getMetaData()), 120)
  expect_true(all(c("cell_type", "donor") %in% crb$getGroups()))
  expect_true(length(crb$projections) >= 1L)
  expect_true(length(crb$marker_genes) >= 1L)
})

test_that("a spatial AnnData file builds a spatial section through the Builder", {
  skip_if_not_installed("qs2")
  built <- build_through_builder(fixture("small_spatial.h5ad"), "h5ad_spatial")
  expect_true(length(built$crb$spatial) >= 1L)
})

test_that("scanpy / scVelo trajectories are offered and built by the Builder", {
  skip_if_not_installed("qs2")
  inspected_trajectories <- quiet(builder_read_object(
    fixture("small_trajectory.h5ad")
  ))$object@misc$trajectories
  built <- build_through_builder(
    fixture("small_trajectory.h5ad"),
    "h5ad_trajectory",
    configure = function(entry) {
      ## what the Configure page records when the user keeps these trajectories
      entry$settings$included_trajectories <- list(
        scvelo = names(inspected_trajectories$scvelo)[1L],
        paga = names(inspected_trajectories$paga)[1L]
      )
      entry$settings$default_trajectory <- list(
        method = "scvelo",
        name = names(inspected_trajectories$scvelo)[1L]
      )
      entry
    }
  )
  methods <- built$crb$getMethodsForTrajectories()
  expect_true(all(c("paga", "scvelo") %in% methods))
  expect_true(length(built$crb$getNamesOfTrajectories("scvelo")) >= 1L)
})

test_that("a scirpy repertoire is offered and built by the Builder", {
  skip_if_not_installed("qs2")
  built <- build_through_builder(fixture("small_airr.h5ad"), "h5ad_airr")
  expect_true(
    length(built$crb$immune_repertoire) >= 1L ||
      length(built$crb$getImmuneRepertoire()) >= 1L
  )
})

test_that("a normalised-only file still gets per-cell count columns", {
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  SummarizedExperiment::assay(sce, "counts") <- NULL
  SummarizedExperiment::assay(sce, "X") <- log1p(
    SummarizedExperiment::assay(sce, "X") / 3 + 0.37
  )
  seurat <- convertSCEToSeurat(sce)
  expect_identical(SeuratObject::Layers(seurat), "data")
  expect_true(all(seurat$nCount_RNA > 0))
  expect_true(all(seurat$nFeature_RNA > 0))
})

test_that("gene names with underscores survive Seurat and the Builder", {
  skip_if_not_installed("Seurat")
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  genes <- rownames(sce)
  genes[1:2] <- c("Blank_1", "APOBEC3A_B")
  rownames(sce) <- genes
  seurat <- quiet(convertSCEToSeurat(sce))
  ## Seurat's constructors rewrite them; the converter puts the file's names back
  expect_identical(rownames(seurat)[1:2], c("Blank_1", "APOBEC3A_B"))
  expect_identical(
    rownames(SeuratObject::LayerData(seurat, "counts"))[1:2],
    c("Blank_1", "APOBEC3A_B")
  )
  out <- file.path(withr::local_tempdir(), "x.crb")
  quiet(exportFromSeurat(
    seurat,
    assay = "RNA",
    slot = "counts",
    file = out,
    experiment_name = "x",
    organism = "hg",
    groups = "cell_type",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    verbose = FALSE,
    viewer_binary = "never"
  ))
  crb <- readCerebro(out)
  expect_setequal(crb$getGeneNames(), genes)
})

test_that("the same data as AnnData, SCE and Seurat gives the same .crb through the Builder", {
  skip_if_not_installed("qs2")
  dir <- withr::local_tempdir()
  sce <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  saveRDS(sce, file.path(dir, "sce.rds"))
  ## a Seurat object built by hand from the same parts, not by the converter
  counts <- methods::as(
    SummarizedExperiment::assay(sce, "counts"),
    "CsparseMatrix"
  )
  meta <- as.data.frame(SummarizedExperiment::colData(sce))
  hand <- quiet(SeuratObject::CreateSeuratObject(counts, meta.data = meta))
  for (name in SingleCellExperiment::reducedDimNames(sce)) {
    embedding <- SingleCellExperiment::reducedDim(sce, name)
    key <- paste0(tolower(name), "_")
    dimnames(embedding) <- list(
      colnames(sce),
      paste0(key, seq_len(ncol(embedding)))
    )
    hand[[name]] <- SeuratObject::CreateDimReducObject(
      embedding,
      key = key,
      assay = "RNA"
    )
  }
  saveRDS(hand, file.path(dir, "seurat.rds"))

  routes <- c(
    h5ad = fixture("small_csr.h5ad"),
    sce = file.path(dir, "sce.rds"),
    seurat = file.path(dir, "seurat.rds")
  )
  crbs <- Map(
    function(path, id) build_through_builder(path, id)$crb,
    routes,
    names(routes)
  )
  reference <- crbs$h5ad
  for (name in c("sce", "seurat")) {
    other <- crbs[[name]]
    expect_identical(
      other$getMetaData()$cell_barcode,
      reference$getMetaData()$cell_barcode,
      info = name
    )
    expect_identical(
      other$getGeneNames(),
      reference$getGeneNames(),
      info = name
    )
    expect_equal(
      as.matrix(other$getExpressionMatrix()),
      as.matrix(reference$getExpressionMatrix()),
      info = name
    )
    expect_identical(
      sort(other$getGroups()),
      sort(reference$getGroups()),
      info = name
    )
    for (projection in names(reference$projections)) {
      expect_equal(
        unname(as.matrix(other$getProjection(projection))),
        unname(as.matrix(reference$getProjection(projection))),
        info = paste(name, projection)
      )
    }
  }
})

test_that("Visium sections reach the .crb as separate spatial entries through the Builder", {
  skip_if_not_installed("qs2")
  ## the Builder wants a projection to lead with: give the sections one
  sce <- quiet(readH5adAsSCE(fixture("small_visium.h5ad")))
  SingleCellExperiment::reducedDim(
    sce,
    "UMAP"
  ) <- SingleCellExperiment::reducedDim(sce, "spatial") / 1000
  path <- file.path(withr::local_tempdir(), "visium.rds")
  saveRDS(sce, path)
  built <- build_through_builder(path, "h5ad_visium")
  crb <- built$crb
  expect_identical(names(crb$spatial), c("section1", "section2"))
  s1 <- crb$getSpatialData("section1", FALSE)
  s2 <- crb$getSpatialData("section2", FALSE)
  expect_equal(c(nrow(s1$coordinates), nrow(s2$coordinates)), c(50, 49))
  ## the same coordinates as the direct conversion
  direct <- readCerebro(quiet(convertH5adToCerebro(
    fixture("small_visium.h5ad"),
    withr::local_tempdir(),
    groups = c("library_id", "layer"),
    viewer_binary = "never",
    verbose = FALSE
  )))
  d1 <- direct$getSpatialData("spatial [section1]", FALSE)
  expect_equal(
    s1$coordinates[order(rownames(s1$coordinates)), c("x", "y")],
    d1$coordinates[order(rownames(d1$coordinates)), c("x", "y")]
  )
  ## pictures are not embedded (the Builder only takes external ones) but are kept
  expect_length(s1$histology_images, 0L)
})

test_that("tissue pictures stay with their section on the Seurat object", {
  sce <- quiet(readH5adAsSCE(fixture("small_visium.h5ad")))
  seurat <- convertSCEToSeurat(sce)
  expect_identical(SeuratObject::Images(seurat), c("section1", "section2"))
  expect_null(seurat@misc$cerebro_spatial_images)
  expect_identical(
    names(seurat@misc$cerebro_source_images),
    c("section1", "section2")
  )
  expect_identical(
    seurat@misc$cerebro_source_images$section1$hires$histology_image_bounds,
    sce@metadata$spatial_images$section1$hires$histology_image_bounds
  )
})

test_that("tissue pictures that come with the file become Builder images and ship with the app", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("png")
  sce <- quiet(readH5adAsSCE(fixture("small_visium.h5ad")))
  SingleCellExperiment::reducedDim(
    sce,
    "UMAP"
  ) <- SingleCellExperiment::reducedDim(sce, "spatial") / 1000
  dir <- withr::local_tempdir()
  path <- file.path(dir, "visium.rds")
  saveRDS(sce, path)

  ## the import worker's side: the bytes of the pictures cross over, nothing else
  inspected <- quiet(builder_adapter_inspect(builder_seurat_file_adapter(path)))
  payload <- inspected$source_images
  expect_identical(names(payload), c("section1", "section2"))
  expect_identical(names(payload$section1), "hires")
  expect_identical(names(payload$section2), "lowres")
  expect_identical(rawToChar(payload$section1$hires$bytes[2:4]), "PNG")

  ## the main process's side: files and records like an upload's
  images <- builder_source_image_settings(payload, file.path(dir, "images"))
  record <- images$section1$hires
  expect_true(file.exists(record$source_path))
  expect_true(record$flip_y)
  expect_identical(record$section_id, "section1")
  expect_identical(record$image_label, "hires")
  expect_equal(record$base_bounds, payload$section1$hires$bounds)
  expect_identical(
    record$source_content_md5,
    unname(as.character(tools::md5sum(record$source_path)))
  )

  ## and the plan accepts them and the built app carries them
  entry <- builder_e2e_entry(list(
    id = "visium",
    label = "visium",
    make = function() list(object = inspected$object)
  ))
  entry$settings$images <- images
  snapshot <- builder_snapshot_seurat(
    inspected$object,
    file.path(dir, "snapshot"),
    available_bytes = 2^40
  )
  entry$snapshot <- snapshot
  plan <- builder_freeze_plan(
    list(entry),
    file.path(dir, "release"),
    make_app = TRUE,
    app_options = list(launch_browser = FALSE)
  )
  expect_null(plan$error)
  stage <- file.path(dir, "stage")
  dir.create(stage)
  result <- quiet(builder_execute_plan(plan, stage, list(visium = snapshot)))
  expect_identical(result$state, "success", info = result$error %||% "")
  assets <- list.files(file.path(result$app_dir, "spatial-assets"))
  expect_length(assets, 2L)
  expect_true(all(endsWith(assets, ".png")))
})

## ---------------------------------------------------------------------------
## SpatialData (as a zip) and antibody features
## ---------------------------------------------------------------------------

zip_store <- function(store, zip_file) {
  skip_if_not_installed("zip")
  withr::with_dir(
    dirname(store),
    zip::zip(zip_file, basename(store), mode = "mirror")
  )
  zip_file
}

test_that("a SpatialData store as a .zip is a Builder source and builds one section per region", {
  skip_if_not_installed("qs2")
  skip_if_not_installed("arrow")
  skip_if_not_installed("png")
  zip_file <- zip_store(
    normalizePath(fixture("small_sd_visium.zarr")),
    file.path(withr::local_tempdir(), "visium.zarr.zip")
  )
  expect_identical(builder_match_format(zip_file)$id, "spatialdata")

  adapter <- builder_seurat_file_adapter(zip_file)
  inspected <- quiet(builder_adapter_inspect(adapter))
  expect_s3_class(inspected$profile, "builder_dataset_profile")
  expect_equal(inspected$profile$identity$cells$count, 54)
  expect_identical(names(inspected$source_images), c("sec1", "sec2"))

  built <- build_through_builder(zip_file, "spatialdata_visium")
  expect_identical(names(built$crb$spatial), c("sec1", "sec2"))
  expect_equal(nrow(built$crb$getSpatialData("sec1", FALSE)$coordinates), 30)
})

test_that("a zip that holds no SpatialData store is refused with a clear message", {
  skip_if_not_installed("zip")
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  writeLines("hello", file.path(dir, "note.txt"))
  zip_file <- file.path(withr::local_tempdir(), "other.zip")
  withr::with_dir(dir, zip::zip(zip_file, "note.txt"))
  expect_match(builder_read_object(zip_file)$error, "no SpatialData store")
})

test_that("antibody features of an h5mu file reach the .crb through the Builder", {
  skip_if_not_installed("qs2")
  built <- build_through_builder(fixture("small_mudata.h5mu"), "h5mu_cite")
  genes <- built$crb$getGeneNames()
  expect_equal(length(genes), 60 + 4)
  expect_identical(genes[grepl("^prot:", genes)], paste0("prot:P", 0:3))
})

test_that("a SingleCellExperiment's ADT alternative experiment is added, spike-ins are not", {
  sce <- quiet(readH5adAsSCE(
    fixture("small_mudata.h5mu"),
    modality = "gex",
    extra_modalities = "prot"
  ))
  SingleCellExperiment::altExp(sce, "ADT") <- SingleCellExperiment::altExp(
    sce,
    "prot"
  )
  SingleCellExperiment::altExp(sce, "ERCC") <- SingleCellExperiment::altExp(
    sce,
    "prot"
  )
  SingleCellExperiment::altExp(sce, "prot") <- NULL
  expect_identical(builder_extra_feature_names(sce), "ADT")
  seurat <- quiet(convertSCEToSeurat(
    sce,
    extra_features = builder_extra_feature_names(sce)
  ))
  expect_equal(sum(grepl("^ADT:", rownames(seurat))), 4)
  expect_equal(sum(grepl("^ERCC:", rownames(seurat))), 0)
  ## without the argument nothing is added
  expect_equal(nrow(quiet(convertSCEToSeurat(sce))), 60)
})
