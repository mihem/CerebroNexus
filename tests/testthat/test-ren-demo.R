ren_demo_script <- function() {
  testthat::test_path("..", "bench", "prepare_viewer_ren_data.R")
}

test_that("the Ren demo registers the full published atlas", {
  helper <- viewer_test_path("million_cell_demo.R")
  env <- new.env(parent = baseenv())
  sys.source(helper, envir = env)

  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = c(Small = 5),
    point_opacity = c(Small = 1),
    percentage_cells_to_show = 100
  )
  expect_identical(env$viewerAddRenDemo(original, ""), original)

  root <- tempfile("cerebro-ren-demo-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  crb <- file.path(root, "ren.crb")
  file.create(crb)
  dir.create(file.path(root, "ren.bpcells"))
  file.create(file.path(root, "ren.bpcells", "shape"))

  configured <- env$viewerAddRenDemo(original, crb)
  label <- "Ren et al. COVID-19 atlas (1.46M + TCR/BCR)"
  expect_identical(
    unname(configured$crb_file_to_load[label]),
    normalizePath(crb)
  )
  expect_equal(unname(configured$percentage_cells_to_show[label]), 100)
  expect_equal(unname(configured$expression_point_size[label]), 2)
  expect_equal(unname(configured$expression_point_opacity[label]), 1)
})

test_that("the Ren preparation preserves published counts and paired chains", {
  script <- ren_demo_script()
  skip_if_not(
    file.exists(script),
    "benchmark tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  manifest <- env$.viewerRenFiles()
  expect_identical(manifest$archive$size, 14333004385)
  expect_identical(manifest$annotation$size, 70354925)
  expect_identical(manifest$tcr$size, 155752190)
  expect_identical(manifest$bcr$size, 204004592)

  tcr <- data.frame(
    cellBarcode = c("cell-1", "cell-2"),
    sampleID = c("sample-1", "sample-1"),
    TCRA_vgene = c("TRAV1", NA),
    TCRA_dgene = NA_character_,
    TCRA_jgene = c("TRAJ1", NA),
    TCRA_cgene = c("TRAC", NA),
    TCRA_cdr3nt = c("AAA", NA),
    TCRA_cdr3aa = c("CAAF", NA),
    TCRB_vgene = c("TRBV1", "TRBV2"),
    TCRB_dgene = c("TRBD1", "TRBD1"),
    TCRB_jgene = c("TRBJ1", "TRBJ2"),
    TCRB_cgene = c("TRBC1", "TRBC1"),
    TCRB_cdr3nt = c("CCC", "GGG"),
    TCRB_cdr3aa = c("CBBF", "CCCF"),
    stringsAsFactors = FALSE
  )
  canonical <- env$.viewerRenCanonicalRepertoire(tcr, "TCR")
  expect_named(
    canonical,
    c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict", "sample")
  )
  expect_identical(canonical$CTnt, c("AAA_CCC", "GGG"))
  expect_identical(canonical$CTaa, c("CAAF_CBBF", "CCCF"))
  expect_false(any(grepl("NA", canonical$CTgene, fixed = TRUE)))
})

test_that("the Ren h5ad selectors accept standard AnnData paths", {
  skip_if_not_installed("rhdf5")
  script <- ren_demo_script()
  skip_if_not(file.exists(script), "benchmark tree not present")
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  h5ad <- tempfile(fileext = ".h5ad")
  on.exit(unlink(h5ad), add = TRUE)
  rhdf5::h5createFile(h5ad)
  rhdf5::h5write(matrix(1:6, nrow = 2), h5ad, "X")
  rhdf5::h5createGroup(h5ad, "obsm")
  rhdf5::h5write(matrix(1:6, nrow = 3), h5ad, "obsm/X_umap")

  expect_identical(env$.viewerRenMatrixGroup(h5ad), "X")
  projection <- env$.viewerRenProjection(h5ad, c("c1", "c2", "c3"))
  expect_identical(rownames(projection), c("c1", "c2", "c3"))
  expect_identical(attr(projection, "source"), "/obsm/X_umap")
  expect_identical(env$.viewerRenProjectionName(projection), "umap")

  rhdf5::h5delete(h5ad, "obsm/X_umap")
  rhdf5::h5write(matrix(7:12, nrow = 3), h5ad, "obsm/X_tsne")
  projection <- env$.viewerRenProjection(h5ad, c("c1", "c2", "c3"))
  expect_identical(attr(projection, "source"), "/obsm/X_tsne")
  expect_identical(env$.viewerRenProjectionName(projection), "tsne")
})

test_that("the Ren preparation keeps large categorical and repertoire data lazy", {
  script <- ren_demo_script()
  skip_if_not(file.exists(script), "benchmark tree not present")
  source <- paste(readLines(script), collapse = "\n")

  expect_match(source, ".viewerRenVersion <- 2L", fixed = TRUE)
  expect_match(source, "metadata[categorical] <- lapply", fixed = TRUE)
  expect_match(source, "immune_repertoire.qs2", fixed = TRUE)
  expect_match(source, "immune_repertoire_backend", fixed = TRUE)
  expect_false(grepl("addImmuneRepertoire(repertoire)", source, fixed = TRUE))
})

test_that("run-demo prepares both full-scale datasets", {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  skip_if_not(
    file.exists(launcher),
    "repository-only demo launcher is not installed with the package"
  )
  skip_if_not(
    file.exists(launcher),
    "repository-only demo launcher is not included in the built package"
  )
  script <- paste(readLines(launcher), collapse = "\n")
  expect_match(script, "prepareViewer1mBenchmarkData()", fixed = TRUE)
  expect_match(script, "prepareViewerRenDemoData()", fixed = TRUE)
  expect_match(script, "CEREBRO_REN_DEMO_CRB", fixed = TRUE)
})

test_that("Ren preparation preserves prior datasets on failure and upgrade", {
  script <- ren_demo_script()
  skip_if_not(file.exists(script), "benchmark tree not present")
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  root <- tempfile("ren-versioned-")
  dir.create(file.path(root, "ren", "cerebro"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  legacy <- file.path(root, "ren", "cerebro", "legacy-matrix")
  writeLines("preserve legacy data", legacy)
  paths <- env$.viewerRenPaths(root)
  dir.create(dirname(paths$crb))
  writeLines("preserve incomplete current data", paths$crb)
  env$.viewerRenRequire <- function(...) NULL
  env$.viewerRenFiles <- function() list()
  env$.viewerRenExtractH5ad <- function(...) NULL
  env$.viewerRenBuildObject <- function(paths) {
    writeLines("partial build", paths$crb)
    stop("simulated preparation failure")
  }
  expect_error(
    env$prepareViewerRenDemoData(root),
    "simulated preparation failure"
  )
  expect_identical(readLines(legacy), "preserve legacy data")
  expect_identical(readLines(paths$crb), "preserve incomplete current data")
  expect_length(list.files(paths$root, pattern = "stage-"), 0L)
  env$.viewerRenBuildObject <- function(paths) {
    writeLines("new data", paths$crb)
    dir.create(paths$sidecar)
    file.create(file.path(paths$sidecar, "immune_repertoire.qs2"))
    writeLines("2", paths$stamp)
  }
  expect_identical(env$prepareViewerRenDemoData(root), paths$crb)
  expect_identical(readLines(legacy), "preserve legacy data")
  expect_identical(readLines(paths$crb), "new data")
  env$.viewerRenBuildObject <- function(...) stop("must reuse complete cache")
  expect_identical(env$prepareViewerRenDemoData(root), paths$crb)
})
