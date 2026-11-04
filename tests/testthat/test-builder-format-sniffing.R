## Dataset files whose name has no extension the Builder reads (a `targets`
## store object, a renamed download) are recognised by their first bytes.

builder_repo_source("io.R", local = globalenv())

sniff_dir <- function() withr::local_tempdir(.local_envir = parent.frame())

write_bytes <- function(path, bytes) {
  writeBin(as.raw(bytes), path)
  path
}

test_that("every RDS compression is recognised without an extension", {
  dir <- sniff_dir()
  object <- list(a = 1:10, b = letters)
  compressions <- list(
    gzip = list(compress = TRUE),
    bzip2 = list(compress = "bzip2"),
    xz = list(compress = "xz"),
    none = list(compress = FALSE),
    ascii = list(compress = FALSE, ascii = TRUE)
  )
  if (getRversion() >= "4.4.0") {
    zstd <- file.path(dir, "probe")
    if (
      !inherits(
        try(saveRDS(1, zstd, compress = "zstd"), silent = TRUE),
        "try-error"
      )
    ) {
      compressions$zstd <- list(compress = "zstd")
    }
  }
  for (name in names(compressions)) {
    path <- file.path(dir, paste0("object_", name))
    do.call(saveRDS, c(list(object, path), compressions[[name]]))
    expect_identical(builder_sniff_format(path), "rds", info = name)
    expect_identical(builder_match_format(path)$id, "rds", info = name)
  }
  native <- file.path(dir, "native")
  connection <- file(native, "wb")
  serialize(object, connection, xdr = FALSE)
  close(connection)
  expect_identical(builder_sniff_format(native), "rds")
})

test_that("qs2, qs, AnnData, MuData and SpatialData zip are recognised by content", {
  dir <- sniff_dir()
  skip_if_not_installed("qs2")
  qs2_path <- file.path(dir, "object_qs2")
  qs2::qs_save(list(a = 1), qs2_path)
  expect_identical(builder_sniff_format(qs2_path), "qs2")

  ## qs writes 0B 0E 0A 0C, then reserved bytes (qs src/qs_common.h).
  qs_path <- write_bytes(
    file.path(dir, "object_qs"),
    c(0x0b, 0x0e, 0x0a, 0x0c, rep(0, 12))
  )
  expect_identical(builder_sniff_format(qs_path), "qs")
  ## qs2's qdata container (0B 0E 0A CD) cannot hold a Seurat object.
  qd_path <- write_bytes(
    file.path(dir, "object_qd"),
    c(0x0b, 0x0e, 0x0a, 0xcd, rep(0, 12))
  )
  expect_null(builder_sniff_format(qd_path))

  zip_path <- write_bytes(
    file.path(dir, "store"),
    c(0x50, 0x4b, 0x03, 0x04, rep(0, 26))
  )
  expect_identical(builder_sniff_format(zip_path), "spatialdata")

  ## MuData marks its HDF5 user block before the signature at offset 512.
  userblock <- c(
    charToRaw("MuData (format-version=0.1.0;creator=muon)"),
    raw(512L - 42L)
  )
  mudata_path <- write_bytes(
    file.path(dir, "mudata_userblock"),
    c(userblock, 0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a, raw(16L))
  )
  expect_identical(builder_sniff_format(mudata_path), "h5mu")

  skip_if_not_installed("rhdf5")
  anndata <- file.path(dir, "anndata")
  file.copy(test_path("fixtures", "anndata", "small_dense.h5ad"), anndata)
  expect_identical(builder_sniff_format(anndata), "h5ad")
  expect_identical(builder_match_format(anndata)$id, "h5ad")
  mudata <- file.path(dir, "mudata")
  file.copy(test_path("fixtures", "anndata", "small_mudata.h5mu"), mudata)
  expect_identical(builder_sniff_format(mudata), "h5mu")
  expect_identical(builder_match_format(mudata)$id, "h5mu")
})

test_that("an HDF5 file that is neither AnnData nor MuData asks for the extension", {
  skip_if_not_installed("rhdf5")
  path <- file.path(sniff_dir(), "matrix_store")
  rhdf5::h5createFile(path)
  rhdf5::h5createGroup(path, "counts")
  rhdf5::h5closeAll()
  expect_identical(builder_sniff_format(path), "hdf5")
  expect_null(builder_match_format(path))
  expect_null(builder_path_format_capability("matrix_store", content = path))
  expect_identical(
    builder_unrecognised_format_message("matrix_store", path),
    paste0(
      "matrix_store is an HDF5 file, but its content does not show whether ",
      "it holds AnnData or MuData. Save it with the .h5ad or .h5mu extension."
    )
  )
})

test_that("a wrong extension falls back to content; a supported one is kept", {
  dir <- sniff_dir()
  wrong <- file.path(dir, "sc_tcr.dat")
  saveRDS(list(a = 1), wrong)
  expect_identical(builder_match_format(wrong)$id, "rds")
  expect_identical(
    builder_path_format_capability("sc_tcr.dat", content = wrong)$id,
    "rds"
  )
  expect_true(
    builder_path_format_capability("sc_tcr.dat", content = wrong)$available
  )
  ## Without content only the name counts.
  expect_null(builder_path_format_capability("sc_tcr.dat"))

  skip_if_not_installed("qs2")
  misnamed <- file.path(dir, "object.rds")
  qs2::qs_save(list(a = 1), misnamed)
  expect_identical(builder_match_format(misnamed)$id, "rds")
  expect_identical(
    builder_path_format_capability("object.rds", content = misnamed)$id,
    "rds"
  )
})

test_that("unrecognised content fails with a message naming the formats", {
  dir <- sniff_dir()
  garbage <- write_bytes(file.path(dir, "sc_tcr"), as.raw(0:255))
  text <- file.path(dir, "notes.dat")
  writeLines(c("A", "plain text that only looks like an ASCII header"), text)
  fake_gzip <- write_bytes(file.path(dir, "broken"), c(0x1f, 0x8b, 0x08, 0x00))
  empty <- file.path(dir, "empty")
  file.create(empty)
  for (path in c(garbage, text, fake_gzip, empty)) {
    expect_null(builder_sniff_format(path), info = path)
    expect_null(builder_match_format(path), info = path)
  }
  expect_null(builder_sniff_format(file.path(dir, "missing")))
  expect_null(builder_sniff_format(dir))

  expect_identical(
    builder_unrecognised_format_message("sc_tcr", garbage),
    paste0(
      "sc_tcr has no file extension and its content is not a supported ",
      "format (RDS, qs2, h5ad, h5mu, SpatialData zip). Save it with one of ",
      "these extensions: .rds, .qs2, .h5ad, .h5mu, .zip."
    )
  )
  expect_match(
    builder_unrecognised_format_message("notes.dat", text),
    "^notes\\.dat has an unsupported \\.dat extension and its content"
  )
  expect_identical(
    builder_read_object(garbage)$error,
    builder_unrecognised_format_message("sc_tcr", garbage)
  )
})

test_that("only the first bytes are read to recognise a file", {
  runtime <- new.env(parent = globalenv())
  sys.source(builder_test_inst_path("builder", "io.R"), envir = runtime)
  requested <- integer()
  runtime$readBin <- function(con, what, n = 1L, ...) {
    requested <<- c(requested, n)
    base::readBin(con, what, n = n, ...)
  }
  dir <- sniff_dir()
  large <- file.path(dir, "large")
  writeBin(as.raw(rep(0x41, 1e6)), large)
  rds <- file.path(dir, "large_rds")
  saveRDS(seq_len(1e6), rds)
  expect_null(runtime$builder_sniff_format(large))
  expect_identical(runtime$builder_sniff_format(rds), "rds")
  expect_true(length(requested) >= 3L)
  expect_true(all(requested <= 4096L))
})

test_that("an extensionless Seurat RDS is read and described like a .rds", {
  skip_if_not_installed("SeuratObject")
  path <- file.path(sniff_dir(), "sc_tcr")
  saveRDS(SeuratObject::pbmc_small, path)
  result <- builder_read_object(path)
  expect_null(result$error)
  expect_s4_class(result$object, "Seurat")
  expect_identical(result$format, "RDS")
})

test_that("a project stores an extensionless source under its real format", {
  runtime <- new.env(parent = globalenv())
  for (file in c("io.R", "project.R")) {
    sys.source(builder_test_inst_path("builder", file), envir = runtime)
  }
  dir <- sniff_dir()
  rds <- file.path(dir, "sc_tcr")
  saveRDS(list(a = 1), rds)
  expect_identical(
    runtime$builder_project_source_storage_name("sc_tcr", content = rds),
    "source.rds"
  )
  expect_identical(
    runtime$builder_project_source_storage_name("data.h5ad", content = rds),
    "source.h5ad"
  )
  expect_identical(
    runtime$builder_project_source_storage_name("sc_tcr"),
    "source.rds"
  )
  skip_if_not_installed("qs2")
  qs2_path <- file.path(dir, "sc_bcr")
  qs2::qs_save(list(a = 1), qs2_path)
  expect_identical(
    runtime$builder_project_source_storage_name("sc_bcr", content = qs2_path),
    "source.qs2"
  )
})
