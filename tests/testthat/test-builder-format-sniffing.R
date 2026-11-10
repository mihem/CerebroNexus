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

test_that("an ASCII RDS saved on Windows (CRLF line ends) is recognised", {
  ## saveRDS(ascii = TRUE) on Windows writes "A\r\n3\r\n..."; build the same
  ## bytes on any platform.
  dir <- sniff_dir()
  ascii <- serialize(list(a = 1:3, b = "x"), NULL, ascii = TRUE)
  newline <- which(ascii == as.raw(0x0a))
  crlf <- unlist(lapply(seq_along(ascii), function(i) {
    if (i %in% newline) as.raw(c(0x0d, 0x0a)) else ascii[[i]]
  }))
  expect_identical(crlf[1:4], as.raw(c(0x41, 0x0d, 0x0a, 0x33)))

  plain <- write_bytes(file.path(dir, "windows_ascii"), crlf)
  expect_identical(builder_sniff_format(plain), "rds")
  expect_identical(builder_match_format(plain)$id, "rds")

  gz <- file.path(dir, "windows_ascii_gzip")
  connection <- gzfile(gz, "wb")
  writeBin(crlf, connection)
  close(connection)
  expect_identical(builder_sniff_format(gz), "rds")

  ## A text file that only starts with "A" is still not an RDS.
  text <- write_bytes(file.path(dir, "text"), charToRaw("A\r\nhello\r\n"))
  expect_null(builder_sniff_format(text))
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
  file.copy(anndata_test_fixture("small_dense.h5ad"), anndata)
  expect_identical(builder_sniff_format(anndata), "h5ad")
  expect_identical(builder_match_format(anndata)$id, "h5ad")
  mudata <- file.path(dir, "mudata")
  file.copy(anndata_test_fixture("small_mudata.h5mu"), mudata)
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

test_that("loom, h5Seurat and 10x HDF5 files get advice for their own kind", {
  skip_if_not_installed("rhdf5")
  dir <- sniff_dir()
  make <- function(name, groups) {
    path <- file.path(dir, name)
    rhdf5::h5createFile(path)
    for (group in groups) {
      rhdf5::h5createGroup(path, group)
    }
    rhdf5::h5closeAll()
    path
  }
  loom <- make("cells.loom", c("matrix", "row_attrs", "col_attrs", "layers"))
  h5seurat <- make("cells.h5seurat", c("assays", "meta.data", "cell.names"))
  tenx_v3 <- make("filtered.h5", c("matrix", "matrix/barcodes"))
  tenx_v2 <- make("filtered_v2.h5", c("GRCh38", "GRCh38/barcodes"))
  expect_match(
    builder_unrecognised_format_message("cells.loom", loom),
    "^cells\\.loom is a loom file.*\\.rds\\.$"
  )
  expect_match(
    builder_unrecognised_format_message("cells.h5seurat", h5seurat),
    "is an h5Seurat file.*LoadH5Seurat"
  )
  for (path in c(tenx_v3, tenx_v2)) {
    message <- builder_unrecognised_format_message(basename(path), path)
    expect_match(message, "10x Genomics count matrix.*Read10X_h5", info = path)
    expect_false(grepl(".h5ad", message, fixed = TRUE), info = path)
  }
  expect_identical(
    builder_read_object(loom)$error,
    builder_unrecognised_format_message("cells.loom", loom)
  )
})

test_that("empty, truncated and multi-object files say what is wrong", {
  dir <- sniff_dir()
  empty <- file.path(dir, "empty.rds")
  file.create(empty)
  expect_identical(
    builder_read_object(empty)$error,
    "The file is empty (0 bytes)."
  )
  whole <- file.path(dir, "whole.rds")
  saveRDS(as.list(seq_len(5000)), whole, compress = FALSE)
  truncated <- file.path(dir, "truncated.rds")
  writeBin(readBin(whole, "raw", n = 2000L), truncated)
  expect_match(
    builder_read_object(truncated)$error,
    "^Could not read with RDS: the file ends early\\. It looks truncated"
  )

  skip_if_not_installed("SeuratObject")
  several <- file.path(dir, "several.rds")
  saveRDS(rep(list(SeuratObject::pbmc_small), 8L), several)
  expect_identical(
    builder_read_object(several)$error,
    paste0(
      "The file contains a list of 8 Seurat objects, not one Seurat object. ",
      "The Builder reads one Seurat object per file: save each object to ",
      "its own file and add those files."
    )
  )
  mixed <- file.path(dir, "mixed.rds")
  saveRDS(list(1), mixed)
  expect_match(
    builder_read_object(mixed)$error,
    "^The file contains a list of 1 object, not one Seurat object\\."
  )
})

test_that("an unreadable file is reported as such", {
  skip_on_os("windows")
  dir <- sniff_dir()
  path <- file.path(dir, "locked.rds")
  saveRDS(list(a = 1), path)
  Sys.chmod(path, "000")
  withr::defer(Sys.chmod(path, "644"))
  skip_if(identical(unname(file.access(path, 4L)), 0L), "running as root")
  expect_match(
    builder_read_object(path)$error,
    "^The file cannot be opened for reading\\."
  )
  expect_error(
    builder_copy_file_verified(path, file.path(dir, "copy")),
    "The file cannot be opened for reading.",
    fixed = TRUE
  )
})

test_that("content beats a wrong extension; a supported one is kept", {
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

  ## A qs2 file named .rds is read as qs2.
  skip_if_not_installed("qs2")
  misnamed <- file.path(dir, "object.rds")
  qs2::qs_save(list(a = 1), misnamed)
  expect_identical(builder_match_format(misnamed)$id, "qs2")
  expect_identical(
    builder_path_format_capability("object.rds", content = misnamed)$id,
    "qs2"
  )
  ## Without content only the name counts.
  expect_identical(builder_path_format_capability("object.rds")$id, "rds")
  expect_match(
    builder_read_object(misnamed)$error,
    "^The file contains a list of 1 object"
  )
  skip_if_not_installed("SeuratObject")
  seurat <- file.path(dir, "seurat.rds")
  qs2::qs_save(SeuratObject::pbmc_small, seurat)
  read <- builder_read_object(seurat)
  expect_null(read$error)
  expect_identical(read$format, "qs2")
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

test_that("a picked file without an extension is labelled by its format", {
  runtime <- new.env(parent = globalenv())
  sys.source(builder_test_inst_path("builder", "extras.R"), envir = runtime)
  label <- runtime$builder_file_type_label
  expect_identical(label("sc_tcr", "application/octet-stream", "rds"), "RDS")
  expect_identical(label("sc_tcr", "application/octet-stream"), "FILE")
  expect_identical(label("sc_tcr", "application/x-hdf5"), "X-HDF5")
  expect_identical(
    label("object.qs2", "application/octet-stream", "rds"),
    "QS2"
  )
})
