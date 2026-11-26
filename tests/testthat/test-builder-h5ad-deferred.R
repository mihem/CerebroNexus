## Slow Builder integration tests (app sourcing): CI only.
skip_on_cran()
skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

## A large h5ad is opened without its matrices and read in full by the Build.

builder_deferred_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  for (file in sort(list.files(
    testthat::test_path("..", "..", "R"),
    pattern = "[.][Rr]$",
    full.names = TRUE
  ))) {
    sys.source(file, envir = runtime)
  }
  for (file in c(
    "io.R",
    "spatial.R",
    "profile.R",
    "inspect.R",
    "worker.R",
    "adapters.R",
    "analysis.R",
    "extras.R",
    "state.R",
    "app_bundle.R",
    "build.R"
  )) {
    builder_repo_source(file, local = runtime)
  }
  runtime
}

runtime <- builder_deferred_runtime()
read_quietly <- function(...) {
  suppressWarnings(suppressMessages(runtime$builder_read_object(...)))
}
deferred_of <- function(object) object@misc$.cerebro_deferred_h5ad

test_that("large h5ad files are opened without their matrices", {
  path <- anndata_test_fixture("small_csr.h5ad")

  full <- read_quietly(path, metadata_only = FALSE)
  expect_null(full$error)
  expect_null(deferred_of(full$object))

  shell <- read_quietly(path, metadata_only = TRUE)
  expect_null(shell$error)
  expect_identical(
    deferred_of(shell$object)$file,
    normalizePath(path, winslash = "/")
  )
  expect_identical(
    length(SeuratObject::LayerData(shell$object, layer = "counts")@x),
    0L
  )

  ## The size threshold decides when the caller does not.
  withr::with_options(list(cerebro.builder.h5ad_metadata_only_bytes = 0), {
    expect_false(is.null(deferred_of(read_quietly(path)$object)))
  })
  withr::with_options(list(cerebro.builder.h5ad_metadata_only_bytes = Inf), {
    expect_null(deferred_of(read_quietly(path)$object))
  })

  ## A layout the metadata-only read cannot stand in for is read in full.
  dense <- read_quietly(
    anndata_test_fixture("small_dense.h5ad"),
    metadata_only = TRUE
  )
  expect_null(dense$error)
  expect_null(deferred_of(dense$object))
})

test_that("the Build reads a metadata-only dataset in full and checks it", {
  source <- anndata_test_fixture("small_csr.h5ad")
  path <- withr::local_tempfile(fileext = ".h5ad")
  file.copy(source, path, copy.date = TRUE)
  shell <- read_quietly(path, metadata_only = TRUE)$object
  full <- read_quietly(path, metadata_only = FALSE)$object

  hydrated <- runtime$.builder_build_hydrate_deferred(shell)
  expect_null(deferred_of(hydrated))
  expect_identical(hydrated@meta.data, full@meta.data)
  for (layer in SeuratObject::Layers(full)) {
    expect_identical(
      SeuratObject::LayerData(hydrated, layer = layer),
      SeuratObject::LayerData(full, layer = layer)
    )
  }
  expect_gt(length(SeuratObject::LayerData(hydrated, layer = "counts")@x), 0L)
  ## The QC totals the shell showed are those of the full read.
  for (column in grep(
    "^n(Count|Feature)_",
    colnames(full@meta.data),
    value = TRUE
  )) {
    expect_equal(shell@meta.data[[column]], full@meta.data[[column]])
  }

  ## A dataset whose shape differs from the full read is refused.
  altered <- shell
  altered$extra_column <- 1
  expect_error(
    runtime$.builder_build_hydrate_deferred(altered),
    "does not match the dataset that was inspected"
  )

  ## So is a file that changed since it was opened.
  Sys.setFileTime(path, Sys.time() + 3600)
  expect_error(
    runtime$.builder_build_hydrate_deferred(shell),
    "changed or moved"
  )
})

test_that("preparing a metadata-only dataset leaves no placeholders", {
  path <- anndata_test_fixture("small_csr.h5ad")
  shell <- read_quietly(path, metadata_only = TRUE)$object
  item <- list(
    included_projections = names(shell@reductions),
    included_trajectories = list(),
    default_trajectory = NULL,
    assay = SeuratObject::DefaultAssay(shell),
    layer = "counts"
  )
  prepared <- runtime$.builder_build_prepare(shell, item)
  expect_null(deferred_of(prepared))
  expect_gt(length(SeuratObject::LayerData(prepared, layer = "counts")@x), 0L)
})

test_that("the backend size estimate counts the matrices the Build will read", {
  path <- anndata_test_fixture("small_csr.h5ad")
  shell <- read_quietly(path, metadata_only = TRUE)$object
  full <- read_quietly(path, metadata_only = FALSE)$object
  stored <- vapply(
    SeuratObject::Layers(full),
    function(layer) length(SeuratObject::LayerData(full, layer = layer)@x),
    integer(1)
  )
  deferred <- deferred_of(shell)
  expect_identical(
    sort(unname(deferred$stored_values)),
    sort(as.numeric(stored))
  )
  expect_identical(
    runtime$.builder_deferred_matrix_bytes(shell),
    sum(deferred$stored_values * 12 + (ncol(shell) + 1) * 4)
  )
  expect_identical(runtime$.builder_deferred_matrix_bytes(full), 0)
})
