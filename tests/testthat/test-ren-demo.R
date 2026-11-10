ren_demo_script <- function() {
  testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_viewer_ren_data.R"
  )
}

## The Ren script uses the shared download helpers next to it.
ren_demo_source <- function(script, env) {
  sys.source(file.path(dirname(script), "download_helpers.R"), envir = env)
  sys.source(script, envir = env)
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
  file.create(file.path(root, "ren.h5"))

  configured <- env$viewerAddRenDemo(original, crb)
  label <- "Ren et al. COVID-19 atlas (1.46M + TCR/BCR)"
  expect_identical(
    unname(configured$crb_file_to_load[label]),
    normalizePath(crb)
  )
  expect_equal(unname(configured$percentage_cells_to_show[label]), 100)
  expect_null(configured$expression_point_size)

  unlink(file.path(root, "ren.h5"))
  expect_error(
    env$viewerAddRenDemo(original, crb),
    "CEREBRO_REN_DEMO_CRB must name a .crb with an adjacent .h5 file.",
    fixed = TRUE
  )
})

test_that("the Ren preparation preserves published counts and paired chains", {
  script <- ren_demo_script()
  skip_if_not(
    file.exists(script),
    "benchmark tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  ren_demo_source(script, env)

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
  ren_demo_source(script, env)

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
  ## Slow.
  skip_on_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("qs2")
  script <- ren_demo_script()
  skip_if_not(file.exists(script), "benchmark tree not present")
  env <- new.env(parent = globalenv())
  ren_demo_source(script, env)
  cells <- paste0("cell-", seq_len(1462702L))
  annotations <- data.frame(
    cellName = cells,
    sampleID = "sample",
    celltype = "T",
    majorType = "lymphocyte"
  )
  local_mocked_bindings(
    fread = function(...) annotations,
    .package = "data.table"
  )
  clinical_names <- c(
    "sample",
    "geo_accession",
    "patient",
    "dataset",
    "tissue",
    "severity",
    "stage",
    "age",
    "sex",
    "days_after_symptom_onset",
    "sars_cov_2",
    "outcome",
    "comorbidities",
    "bcr_sequenced",
    "tcr_sequenced"
  )
  clinical <- as.data.frame(setNames(
    as.list(rep("sample", length(clinical_names))),
    clinical_names
  ))
  env$.viewerRenReadClinical <- function(...) clinical
  chosen <- cells[c(2, 1)]
  metadata <- env$.viewerRenReadCells("annotation", "clinical", chosen)
  expect_identical(metadata$cell_barcode, chosen)
  expect_identical(rownames(metadata), chosen)
  expect_true(all(vapply(metadata[-1], is.factor, logical(1))))
  expect_identical(as.character(metadata$cell_type), c("T", "T"))
  expect_error(
    env$.viewerRenReadCells("annotation", "clinical", "missing"),
    "barcodes do not match"
  )

  root <- withr::local_tempdir()
  paths <- env$.viewerRenPaths(root)
  dir.create(dirname(paths$crb), recursive = TRUE)
  env$.viewerRenWriteSidecar <- function(...) {
    matrix(numeric(), 0, length(cells), dimnames = list(NULL, cells))
  }
  env$.viewerRenReadCells <- function(...) metadata
  env$.viewerRenProjection <- function(...) {
    structure(
      data.frame(x = 1:2, y = 3:4, row.names = chosen),
      source = "/obsm/X_umap"
    )
  }
  repertoire <- list(
    sample = data.frame(
      barcode = chosen,
      CTgene = c("TRAV1_TRBV2", "TRAV1_TRBV2")
    )
  )
  env$.viewerRenRepertoire <- function(...) repertoire
  object <- new.env(parent = emptyenv())
  for (method in c(
    "setVersion",
    "addExperiment",
    "addParameters",
    "setExpression",
    "setExpressionBackend",
    "setMetaData",
    "addGroup",
    "addProjection",
    "addExtraTable"
  )) {
    object[[method]] <- function(...) invisible(NULL)
  }
  object$addImmuneRepertoire <- function(...) {
    stop("must not hydrate repertoire")
  }
  env$Cerebro <- list(new = function() object)
  saved <- NULL
  env$saveCerebro <- function(value, path) {
    saved <<- value
    file.create(path)
  }
  expect_identical(env$.viewerRenBuildObject(paths), paths$crb)
  expect_identical(saved, object)
  backend <- object$immune_repertoire_backend
  expect_identical(backend$type, "file")
  expect_identical(backend$file, "cerebro_ren_covid_1.46m.immune.qs2")
  expect_identical(backend$samples, "sample")
  expect_setequal(backend$chains, c("TRA", "TRB"))
  ## The repertoire sidecar sits next to the CRB and the H5 file.
  sidecar <- file.path(dirname(paths$crb), backend$file)
  expect_identical(normalizePath(sidecar), normalizePath(paths$immune))
  expect_identical(qs2::qs_read(sidecar), repertoire)
  expect_identical(backend$md5, unname(tools::md5sum(sidecar)))
  expect_null(object$immune_repertoire)
})

test_that("Ren preparation preserves prior datasets on failure and upgrade", {
  script <- ren_demo_script()
  skip_if_not(file.exists(script), "benchmark tree not present")
  env <- new.env(parent = globalenv())
  ren_demo_source(script, env)
  root <- tempfile("ren-versioned-")
  dir.create(file.path(root, "ren", "cerebro"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  legacy <- file.path(root, "ren", "cerebro", "legacy-matrix")
  writeLines("preserve legacy data", legacy)
  paths <- env$.viewerRenPaths(root)
  dir.create(dirname(paths$crb))
  writeLines("preserve incomplete current data", paths$crb)
  env$.demoRequire <- function(...) NULL
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
    file.create(paths$sidecar)
    file.create(paths$immune)
    writeLines("2", paths$stamp)
  }
  expect_identical(env$prepareViewerRenDemoData(root), paths$crb)
  expect_identical(readLines(legacy), "preserve legacy data")
  expect_identical(readLines(paths$crb), "new data")
  env$.viewerRenBuildObject <- function(...) stop("must reuse complete cache")
  expect_identical(env$prepareViewerRenDemoData(root), paths$crb)
})
