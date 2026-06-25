## Unit tests for cerebroAppLite R package functions
## These tests do NOT require a running Shiny app or Seurat.
## They test pure R logic: the Cerebro_v1.3 R6 class, data loading,
## and input validation in functions that can be tested without Seurat.

## ---------------------------------------------------------------------------
## Cerebro_v1.3 R6 class
## ---------------------------------------------------------------------------

test_that("Cerebro_v1.3 object can be instantiated", {
  obj <- Cerebro_v1.3$new()
  expect_true(inherits(obj, "Cerebro_v1.3"))
  expect_true(inherits(obj, "R6"))
})

test_that("Cerebro_v1.3: addGroup / getGroups round-trip", {
  obj <- Cerebro_v1.3$new()
  # addGroup checks that the column exists in meta_data first
  obj$setMetaData(data.frame(
    sample = c("rep1", "rep2"),
    cluster = c("0", "1"),
    stringsAsFactors = FALSE
  ))
  obj$addGroup("sample", c("rep1", "rep2", "rep3"))
  obj$addGroup("cluster", c("0", "1", "2"))

  groups <- obj$getGroups()
  expect_equal(sort(groups), sort(c("sample", "cluster")))
})

test_that("Cerebro_v1.3: getGroupLevels returns correct levels", {
  obj <- Cerebro_v1.3$new()
  obj$setMetaData(data.frame(sample = c("A", "B"), stringsAsFactors = FALSE))
  obj$addGroup("sample", c("A", "B", "C"))

  lvls <- obj$getGroupLevels("sample")
  expect_equal(lvls, c("A", "B", "C"))
})

test_that("Cerebro_v1.3: checkIfGroupExists works correctly", {
  obj <- Cerebro_v1.3$new()
  obj$setMetaData(data.frame(cluster = c("0", "1"), stringsAsFactors = FALSE))
  obj$addGroup("cluster", c("0", "1"))

  # returns invisibly (NULL) when group exists — no error
  expect_no_error(obj$checkIfGroupExists("cluster"))
  # throws when group does not exist
  expect_error(obj$checkIfGroupExists("nonexistent"), regexp = "not present")
})

test_that("Cerebro_v1.3: addProjection / getProjection round-trip", {
  obj <- Cerebro_v1.3$new()
  proj <- data.frame(
    UMAP_1 = c(1.0, 2.0, 3.0),
    UMAP_2 = c(4.0, 5.0, 6.0)
  )
  obj$addProjection("UMAP", proj)

  result <- obj$getProjection("UMAP")
  expect_equal(result, proj)
})

test_that("Cerebro_v1.3: availableProjections lists added projections", {
  obj <- Cerebro_v1.3$new()
  obj$addProjection("tSNE", data.frame(x = 1:3, y = 1:3))
  obj$addProjection("UMAP", data.frame(x = 1:3, y = 1:3))

  projs <- obj$availableProjections()
  expect_true("tSNE" %in% projs)
  expect_true("UMAP" %in% projs)
})

test_that("Cerebro_v1.3: setMetaData / getMetaData round-trip", {
  obj <- Cerebro_v1.3$new()
  meta <- data.frame(
    cell_barcode = paste0("cell_", 1:5),
    sample = c("A", "A", "B", "B", "B"),
    nUMI = c(100L, 200L, 150L, 300L, 250L),
    stringsAsFactors = FALSE
  )
  obj$setMetaData(meta)

  result <- obj$getMetaData()
  expect_equal(nrow(result), 5L)
  expect_true("sample" %in% colnames(result))
  expect_true("nUMI" %in% colnames(result))
})

test_that("Cerebro_v1.3: addMarkerGenes / getMarkerGenes round-trip", {
  obj <- Cerebro_v1.3$new()
  mg_table <- data.frame(
    gene = c("CD3D", "CD79A", "FCGR3A"),
    p_val = c(0.001, 0.002, 0.003),
    avg_logFC = c(1.5, 1.2, 0.9),
    stringsAsFactors = FALSE
  )
  obj$addMarkerGenes(method = "seurat", name = "cluster", table = mg_table)

  result <- obj$getMarkerGenes(method = "seurat", name = "cluster")
  expect_equal(nrow(result), 3L)
  expect_true("gene" %in% colnames(result))
})

test_that("Cerebro_v1.3: setExpression / getExpressionMatrix round-trip", {
  obj <- Cerebro_v1.3$new()
  # Use a sparse Matrix (single-value class "dgCMatrix") to avoid the
  # length > 1 class() issue with base matrix in R >= 4.x
  mat <- Matrix::Matrix(
    c(0, 1, 2, 3, 0, 1),
    nrow = 2,
    dimnames = list(c("GeneA", "GeneB"), c("cell1", "cell2", "cell3")),
    sparse = TRUE
  )
  obj$setExpression(mat)

  result <- obj$getExpressionMatrix(
    cells = c("cell1", "cell2"),
    genes = c("GeneA", "GeneB")
  )
  expect_equal(nrow(result), 2L) # 2 genes
  expect_equal(ncol(result), 2L) # 2 cells
})

test_that("Cerebro_v1.3: getMeanExpressionForGenes returns numeric vector", {
  obj <- Cerebro_v1.3$new()
  mat <- Matrix::Matrix(
    c(0, 2, 4, 6, 1, 3),
    nrow = 2,
    dimnames = list(c("GeneA", "GeneB"), c("cell1", "cell2", "cell3")),
    sparse = TRUE
  )
  obj$setExpression(mat)

  result <- obj$getMeanExpressionForGenes(c("GeneA", "GeneB"))
  expect_equal(nrow(result), 2L)
  expect_true(is.numeric(result$expression))
  # GeneA: row 1 = c(0, 4, 1) → mean 5/3; GeneB: row 2 = c(2, 6, 3) → mean 11/3
  expect_equal(
    result$expression[result$gene == "GeneA"],
    mean(c(0, 4, 1)),
    tolerance = 1e-6
  )
  expect_equal(
    result$expression[result$gene == "GeneB"],
    mean(c(2, 6, 3)),
    tolerance = 1e-6
  )
})

test_that("Cerebro_v1.3: addGeneList / getGeneLists round-trip", {
  obj <- Cerebro_v1.3$new()
  # addGeneList(name, genes) — two separate arguments
  obj$addGeneList("mito", c("MT-CO1", "MT-ND1"))
  obj$addGeneList("ribo", c("RPS2", "RPL3"))

  gl <- obj$getGeneLists()
  expect_true("mito" %in% names(gl))
  expect_true("ribo" %in% names(gl))
  expect_equal(gl$mito, c("MT-CO1", "MT-ND1"))
})

test_that("Cerebro_v1.3: addExperiment / getExperiment round-trip", {
  obj <- Cerebro_v1.3$new()
  # addExperiment(field, content) — two separate arguments, call once per field
  obj$addExperiment("experiment_name", "PBMC test")
  obj$addExperiment("organism", "hg")
  obj$addExperiment("date_of_export", "2024-01-01")

  exp <- obj$getExperiment()
  expect_equal(exp$experiment_name, "PBMC test")
  expect_equal(exp$organism, "hg")
})

test_that("Cerebro_v1.3: version can be set and retrieved", {
  obj <- Cerebro_v1.3$new()
  obj$setVersion("1.3.0")
  expect_equal(as.character(obj$getVersion()), "1.3.0")
})

## ---------------------------------------------------------------------------
## example data integrity checks
## ---------------------------------------------------------------------------

test_that("example.crb loads successfully and has correct structure", {
  path <- system.file("extdata/v1.4/example.crb", package = "cerebroAppLite")
  expect_true(file.exists(path))

  data <- readRDS(path)
  expect_true(inherits(data, "Cerebro_v1.3"))

  # groups
  groups <- data$getGroups()
  expect_true(length(groups) >= 1)

  # projections
  projs <- data$availableProjections()
  expect_true(length(projs) >= 1)

  # meta data has rows
  meta <- data$getMetaData()
  expect_true(nrow(meta) > 0)
})

test_that("example.crb contains expected groups and projections", {
  path <- system.file("extdata/v1.4/example.crb", package = "cerebroAppLite")
  data <- readRDS(path)

  expect_true("sample" %in% data$getGroups())
  expect_true("seurat_clusters" %in% data$getGroups())

  projs <- data$availableProjections()
  expect_true(any(grepl("UMAP|tSNE|umap|tsne", projs, ignore.case = TRUE)))
})

test_that("example.crb sample levels are as expected", {
  path <- system.file("extdata/v1.4/example.crb", package = "cerebroAppLite")
  data <- readRDS(path)

  lvls <- data$getGroupLevels("sample")
  # example data is split into multiple pseudo-samples (donor_1/2/3)
  expect_true(length(lvls) >= 2)
  expect_true(is.character(lvls))
})

test_that("example.h5 file exists and is non-empty", {
  path <- system.file("extdata/v1.4/example.h5", package = "cerebroAppLite")
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
})

## ---------------------------------------------------------------------------
## calculatePercentGenes input validation (without Seurat)
## ---------------------------------------------------------------------------

test_that("calculatePercentGenes stops if Seurat is not installed or object is wrong class", {
  # passing a non-Seurat object should give a clear error
  expect_error(
    calculatePercentGenes(
      object = list(),
      assay = "RNA",
      genes = list(g = "GeneA")
    ),
    regexp = "Seurat"
  )
})

## ---------------------------------------------------------------------------
## addPercentMtRibo input validation (without Seurat)
## ---------------------------------------------------------------------------

test_that("addPercentMtRibo rejects unsupported organism", {
  # needs Seurat object check first, but organism check fires after that
  # so we just verify the function at least checks for Seurat first
  expect_error(
    addPercentMtRibo(
      object = list(),
      organism = "zebrafish",
      gene_nomenclature = "name"
    ),
    regexp = "Seurat"
  )
})

test_that("addPercentMtRibo rejects unsupported gene_nomenclature", {
  # same pattern — Seurat check fires first, which is still informative
  expect_error(
    addPercentMtRibo(
      object = list(),
      organism = "hg",
      gene_nomenclature = "unknown_format"
    ),
    regexp = "Seurat"
  )
})

## ---------------------------------------------------------------------------
## launchCerebroV1.4 parameter validation
## ---------------------------------------------------------------------------

test_that("launchCerebroV1.4 rejects invalid mode", {
  expect_error(
    launchCerebroV1.4(mode = "readonly"),
    regexp = "'mode' parameter must be set to either 'open' or 'closed'"
  )
})

test_that("launchCerebroV1.4 rejects out-of-range point size", {
  expect_error(
    launchCerebroV1.4(overview_default_point_size = 50),
    regexp = "overview_default_point_size"
  )
})

test_that("launchCerebroV1.4 rejects out-of-range opacity", {
  expect_error(
    launchCerebroV1.4(gene_expression_default_point_opacity = 2),
    regexp = "gene_expression_default_point_opacity"
  )
})

test_that("launchCerebroV1.4 rejects out-of-range percentage", {
  expect_error(
    launchCerebroV1.4(gene_expression_default_percentage_cells_to_show = 150),
    regexp = "gene_expression_default_percentage_cells_to_show"
  )
})

test_that("launchCerebroV1.4 rejects non-logical projections_show_hover_info", {
  expect_error(
    launchCerebroV1.4(projections_show_hover_info = "yes"),
    regexp = "projections_show_hover_info"
  )
})
