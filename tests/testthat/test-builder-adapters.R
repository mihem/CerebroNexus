builder_repo_source("io.R", local = globalenv())

.builder_adapter_source_contracts <- function() {
  builder_profile_source_runtime(local = globalenv())
  builder_repo_source("inspect.R", local = globalenv())
  builder_repo_source("adapters.R", local = globalenv())
}

.builder_adapter_source_contracts()

test_that("snapshot contracts read legacy Assay slots directly", {
  object <- SeuratObject::pbmc_small
  expect_true(inherits(object[["RNA"]], "Assay"))
  layer_data_called <- FALSE

  contracts <- .builder_snapshot_layer_contracts(
    object,
    .layer_data = function(...) {
      layer_data_called <<- TRUE
      stop("legacy Assay should not call LayerData")
    }
  )

  expect_false(layer_data_called)
  expect_length(contracts, 0L)
})

.builder_normalize_profile_sources <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  value_names <- names(value)
  for (index in seq_along(value)) {
    if (
      length(value_names) >= index &&
        identical(value_names[[index]], "source")
    ) {
      value[index] <- list(list(normalized = TRUE))
    } else {
      value[index] <- list(.builder_normalize_profile_sources(value[[index]]))
    }
  }
  value
}

test_that("file and example adapters converge after loading", {
  object <- SeuratObject::pbmc_small
  path <- file.path(withr::local_tempdir(), "pbmc.rds")
  saveRDS(object, path)

  from_file <- builder_adapter_inspect(
    builder_seurat_file_adapter(path)
  )
  from_example <- builder_adapter_inspect(
    builder_example_adapter("pbmc_small", object)
  )

  expect_s4_class(from_file$object, "Seurat")
  expect_s4_class(from_example$object, "Seurat")
  expect_s3_class(from_file$profile, "builder_dataset_profile")
  expect_identical(from_file$profile$schema_version, 2L)
  expect_identical(
    .builder_normalize_profile_sources(from_file$profile),
    .builder_normalize_profile_sources(from_example$profile)
  )
  expect_identical(from_file$legacy_profile, from_example$legacy_profile)
  expect_identical(from_file$levels, from_example$levels)
  expect_identical(from_file$format, "RDS")
  expect_identical(from_example$format, "Built-in example")
  expect_identical(from_file$source$type, "file")
  expect_identical(from_example$source$type, "example")
  expect_identical(
    from_file$matrix_summary,
    list(sparse = TRUE, storage = "memory")
  )
})

test_that("QS reads use bounded physical-core parallelism", {
  skip_if_not_installed("qs")
  path <- tempfile(fileext = ".qs")
  file.create(path)
  withr::defer(unlink(path))
  calls <- list()
  testthat::local_mocked_bindings(
    detectCores = function(logical = TRUE) 16L,
    .package = "parallel"
  )
  testthat::local_mocked_bindings(
    qread = function(file, use_alt_rep = FALSE, strict = FALSE, nthreads = 1L) {
      calls <<- list(file = file, strict = strict, nthreads = nthreads)
      SeuratObject::pbmc_small
    },
    .package = "qs"
  )

  result <- builder_read_object(path)

  expect_identical(
    calls,
    list(file = path, strict = TRUE, nthreads = 16L)
  )
  expect_s4_class(result$object, "Seurat")
})

test_that("parallel QS reads preserve Seurat objects", {
  skip_if_not_installed("qs")
  path <- tempfile(fileext = ".qs")
  withr::defer(unlink(path))
  qs::qsave(SeuratObject::pbmc_small, path, nthreads = 2L)

  result <- builder_read_object(path)

  expect_null(result$error)
  expect_identical(result$object, SeuratObject::pbmc_small)
})

test_that("QS snapshots use strict reads and stable file identity", {
  skip_if_not_installed("qs")
  root <- withr::local_tempdir()
  source <- file.path(root, "source.qs")
  qs::qsave(SeuratObject::pbmc_small, source, nthreads = 2L)
  inspected <- builder_adapter_inspect(builder_seurat_file_adapter(source))
  frozen <- .builder_snapshot_seurat_impl(
    inspected$object,
    file.path(root, "snapshot"),
    available_bytes = 2^40,
    source = inspected$snapshot_source
  )
  expect_identical(frozen$snapshot$serialization, "qs")
  expect_identical(
    unname(tools::md5sum(frozen$snapshot$object_file)),
    unname(tools::md5sum(source))
  )

  testthat::with_mocked_bindings(
    {
      expect_true(.builder_snapshot_owned(frozen$snapshot))
      expect_identical(
        builder_open_snapshot(frozen$snapshot),
        inspected$object
      )
    },
    md5sum = function(...) stop("unexpected QS MD5"),
    .package = "tools"
  )

  object_file <- frozen$snapshot$object_file
  modified_at <- file.info(object_file)$mtime[[1L]]
  connection <- file(object_file, open = "r+b")
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  seek(connection, where = 128L, origin = "start")
  byte <- readBin(connection, what = "raw", n = 1L)
  seek(connection, where = 128L, origin = "start")
  writeBin(as.raw(bitwXor(as.integer(byte), 1L)), connection)
  close(connection)
  Sys.setFileTime(object_file, modified_at)
  expect_false(.builder_snapshot_owned(frozen$snapshot))

  changed_state <- .builder_file_source_state(object_file)
  changed_state$location <- NULL
  frozen$snapshot$object_state <- changed_state
  marker_path <- .builder_snapshot_marker_path(frozen$snapshot$path)
  marker <- readRDS(marker_path)
  marker$object_state <- changed_state
  saveRDS(marker, marker_path)
  expect_error(
    builder_open_snapshot(frozen$snapshot),
    "object file is unreadable"
  )
})

test_that("a saved source MD5 is deferred to snapshot verification", {
  root <- withr::local_tempdir()
  writers <- list(
    rds = function(path) saveRDS(SeuratObject::pbmc_small, path)
  )
  if (requireNamespace("qs", quietly = TRUE)) {
    writers$qs <- function(path) {
      qs::qsave(SeuratObject::pbmc_small, path, nthreads = 2L)
    }
  }
  expect_error(
    .builder_seurat_file_adapter(
      file.path(root, "missing.rds"),
      expected_md5 = "not-an-md5"
    ),
    "does not exist",
    fixed = TRUE
  )

  for (format in names(writers)) {
    path <- file.path(root, paste0("source.", format))
    writers[[format]](path)
    md5 <- unname(tools::md5sum(path))
    adapter <- .builder_seurat_file_adapter(
      path,
      expected_md5 = toupper(md5),
      snapshot_reusable = TRUE
    )

    expect_identical(adapter$expected_md5, md5)
    expect_match(adapter$fingerprint, paste0(md5, "$"))
    wrong <- .builder_seurat_file_adapter(
      path,
      expected_md5 = paste(rep("0", 32L), collapse = ""),
      snapshot_reusable = TRUE
    )
    inspected <- builder_adapter_inspect(wrong)
    target <- file.path(root, paste0("wrong-", format, ".snapshot"))
    expect_error(
      .builder_snapshot_seurat_impl(
        inspected$object,
        target,
        available_bytes = 2^40,
        source = inspected$snapshot_source
      ),
      "does not match its saved MD5",
      fixed = TRUE
    )
    expect_false(dir.exists(target))
  }

  path <- file.path(root, "valid.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  expect_error(
    .builder_seurat_file_adapter(path, expected_md5 = "not-an-md5"),
    "expected Seurat source MD5 is invalid"
  )
})

test_that("snapshot planning ownership avoids a second full content digest", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source.rds")
  saveRDS(SeuratObject::pbmc_small, source)
  inspected <- builder_adapter_inspect(builder_seurat_file_adapter(source))
  frozen <- .builder_snapshot_seurat_impl(
    inspected$object,
    file.path(root, "snapshot"),
    available_bytes = 2^40,
    source = inspected$snapshot_source
  )

  expect_true(.builder_snapshot_owned(
    frozen$snapshot,
    verify_content = FALSE,
    .digest_file = function(...) stop("unexpected planning digest")
  ))
  expect_true(.builder_snapshot_owned(
    frozen$snapshot,
    .digest_file = function(path) frozen$snapshot$object_md5
  ))

  Sys.setFileTime(
    frozen$snapshot$object_file,
    file.info(frozen$snapshot$object_file)$mtime[[1L]] + 2
  )
  expect_false(.builder_snapshot_owned(
    frozen$snapshot,
    verify_content = FALSE,
    .digest_file = function(...) stop("unexpected planning digest")
  ))
})

test_that("plain RDS and QS2 sources become immutable snapshots without reserialization", {
  root <- withr::local_tempdir()
  writers <- list(
    rds = function(object, path) saveRDS(object, path)
  )
  if (requireNamespace("qs2", quietly = TRUE)) {
    writers$qs2 <- function(object, path) qs2::qs_save(object, path)
  }

  for (format in names(writers)) {
    source <- file.path(root, paste0("source.", format))
    writers[[format]](SeuratObject::pbmc_small, source)
    inspected <- builder_adapter_inspect(
      builder_seurat_file_adapter(source)
    )
    expect_null(inspected$snapshot_source$expected_md5)
    frozen <- .builder_snapshot_seurat_impl(
      inspected$object,
      file.path(root, paste0(format, ".snapshot")),
      available_bytes = 2^40,
      source = inspected$snapshot_source
    )

    expect_identical(frozen$snapshot$serialization, format)
    expect_identical(
      unname(tools::md5sum(frozen$snapshot$object_file)),
      unname(tools::md5sum(source))
    )
    expect_identical(
      builder_open_snapshot(frozen$snapshot),
      inspected$object
    )
    if (identical(format, "rds")) {
      legacy <- frozen$snapshot
      legacy$serialization <- NULL
      marker_path <- .builder_snapshot_marker_path(legacy$path)
      marker <- readRDS(marker_path)
      marker$serialization <- NULL
      saveRDS(marker, marker_path)
      expect_true(.builder_snapshot_owned(legacy))
      expect_identical(builder_open_snapshot(legacy), inspected$object)
    }
    changed_loader <- frozen$snapshot
    changed_loader$serialization <- if (format == "rds") "qs2" else "rds"
    expect_false(.builder_snapshot_owned(changed_loader))
  }

  saveRDS(1L, .builder_snapshot_marker_path(frozen$snapshot$path))
  expect_false(.builder_snapshot_owned(frozen$snapshot))
})

test_that("retained snapshots reserve disk headroom before copying", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source.rds")
  saveRDS(SeuratObject::pbmc_small, source)
  inspected <- builder_adapter_inspect(
    builder_seurat_file_adapter(source)
  )
  copied <- FALSE
  runtime <- environment(.builder_snapshot_seurat_impl)
  original_copy <- .builder_snapshot_copy
  assign(
    ".builder_snapshot_copy",
    function(...) {
      copied <<- TRUE
      original_copy(...)
    },
    envir = runtime
  )
  on.exit(
    assign(".builder_snapshot_copy", original_copy, envir = runtime),
    add = TRUE
  )

  expect_error(
    .builder_snapshot_seurat_impl(
      inspected$object,
      file.path(root, "snapshot"),
      available_bytes = file.info(source)$size + 1024^3 - 1,
      source = inspected$snapshot_source
    ),
    "1 GiB"
  )
  expect_false(copied)
  expect_false(dir.exists(file.path(root, "snapshot")))
  expect_invisible(.builder_snapshot_check_budget(
    8000 * 1024^2,
    10000 * 1024^2
  ))
})

test_that("snapshot opening rejects a marker changed after ownership validation", {
  root <- withr::local_tempdir()
  snapshot <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  runtime <- environment(builder_open_snapshot)
  original <- .builder_snapshot_marker
  assign(".builder_snapshot_marker", function(snapshot) 1L, envir = runtime)
  on.exit(
    assign(".builder_snapshot_marker", original, envir = runtime),
    add = TRUE
  )

  expect_error(builder_open_snapshot(snapshot), "integrity check failed")
})

test_that("snapshot opening hashes content only once", {
  root <- withr::local_tempdir()
  snapshot <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  md5sum <- tools::md5sum
  calls <- 0L

  reopened <- testthat::with_mocked_bindings(
    builder_open_snapshot(snapshot),
    md5sum = function(...) {
      calls <<- calls + 1L
      md5sum(...)
    },
    .package = "tools"
  )

  expect_s4_class(reopened, "Seurat")
  expect_identical(calls, 1L)
})

test_that("file adapters clear a materialized stale cache without executing it", {
  object <- SeuratObject::pbmc_small
  sentinel <- file.path(withr::local_tempdir(), "sentinel")
  cache <- data.frame(
    layer = "counts",
    path = tempfile(),
    class = "matrix",
    pkg = "base",
    fxn = paste0(
      "function(x) { file.create(",
      dQuote(sentinel),
      "); matrix(0, 1, 1) }"
    ),
    assay = "RNA",
    stringsAsFactors = FALSE
  )
  object@tools[["SaveSeuratRds"]] <- cache
  path <- file.path(withr::local_tempdir(), "cached.rds")
  saveRDS(object, path)

  inspected <- builder_adapter_inspect(builder_seurat_file_adapter(path))
  expect_s4_class(inspected$object, "Seurat")
  expect_null(.builder_saved_cache(inspected$object))
  expect_null(inspected$snapshot_source)
  expect_false(file.exists(sentinel))
})

test_that("example adapters use the same safe cache cleanup path", {
  object <- SeuratObject::pbmc_small
  sentinel <- file.path(withr::local_tempdir(), "example-sentinel")
  object@tools[["SaveSeuratRds"]] <- data.frame(
    layer = "counts",
    path = tempfile(),
    class = "matrix",
    pkg = "base",
    fxn = paste0(
      "function(x) { file.create(",
      dQuote(sentinel),
      "); matrix(0, 1, 1) }"
    ),
    assay = "RNA",
    stringsAsFactors = FALSE
  )

  inspected <- builder_adapter_inspect(
    builder_example_adapter("cached-example", object)
  )
  expect_null(.builder_saved_cache(inspected$object))
  expect_false(file.exists(sentinel))
})

## A Seurat object whose counts live in an HDF5 file under `root`.
builder_hdf5_backed_object <- function(root) {
  matrix <- builder_profile_matrix(paste0("c", 1:6))
  on_disk <- HDF5Array::writeHDF5Array(
    matrix,
    filepath = file.path(root, "source.h5"),
    name = "counts"
  )
  dimnames(on_disk) <- dimnames(matrix)
  SeuratObject::CreateSeuratObject(counts = on_disk)
}

test_that("file adapters reject an incomplete cache stub without executing it", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  object <- builder_hdf5_backed_object(root)
  stub_path <- file.path(root, "stub.rds")
  SaveSeuratRds <- SeuratObject::SaveSeuratRds
  suppressWarnings(SaveSeuratRds(object, stub_path, move = FALSE))
  sentinel <- file.path(root, "sentinel")
  stub <- readRDS(stub_path)
  stub@tools$SaveSeuratRds$fxn <- paste0(
    "function(x) { file.create(",
    dQuote(sentinel),
    "); matrix(0, 1, 1) }"
  )
  saveRDS(stub, stub_path)

  expect_error(
    builder_adapter_inspect(builder_seurat_file_adapter(stub_path)),
    "incomplete SaveSeuratRds cache stub"
  )
  expect_false(file.exists(sentinel))
})

test_that("loaded HDF5-backed objects can be saved, adapted and snapshotted", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  object <- builder_hdf5_backed_object(root)
  seurat_path <- file.path(root, "saved.rds")
  SaveSeuratRds <- SeuratObject::SaveSeuratRds
  suppressWarnings(SaveSeuratRds(object, seurat_path, move = FALSE))
  loaded <- SeuratObject::LoadSeuratRds(seurat_path)
  saveRDS(loaded, seurat_path)

  inspected <- builder_adapter_inspect(
    builder_seurat_file_adapter(seurat_path)
  )
  expect_null(.builder_saved_cache(inspected$object))
  expect_null(inspected$snapshot_source)
  expect_identical(
    inspected$source_md5,
    unname(tools::md5sum(seurat_path))
  )
  expect_identical(inspected$matrix_summary$storage, "h5")
  snapshot <- builder_snapshot_seurat(
    inspected$object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  expect_s4_class(builder_open_snapshot(snapshot), "Seurat")
})

test_that("adapter inputs fail closed", {
  expect_error(builder_seurat_file_adapter(tempfile()), "does not exist")
  expect_error(builder_seurat_file_adapter(tempdir()), "regular file")
  expect_error(builder_example_adapter("", SeuratObject::pbmc_small), "id")
  expect_error(builder_example_adapter("bad", list()), "Seurat")
  expect_error(builder_adapter_inspect(list()), "adapter")

  path <- file.path(withr::local_tempdir(), "source.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  changed_format <- builder_seurat_file_adapter(path)
  changed_format$reader$id <- "qs2"
  expect_error(
    builder_adapter_inspect(changed_format),
    "format adapter changed"
  )
  changed_format$location <- NULL
  expect_error(
    builder_adapter_inspect(changed_format),
    "format adapter changed"
  )
})

test_that("file adapters reject sources replaced after adapter creation", {
  root <- withr::local_tempdir()
  path <- file.path(root, "source.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  adapter <- builder_seurat_file_adapter(path)
  writeBin(charToRaw("replacement"), path)

  expect_error(builder_adapter_inspect(adapter), "changed since")
})

test_that("file adapters reject sources changed while they are read", {
  root <- withr::local_tempdir()
  path <- file.path(root, "source.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  adapter <- builder_seurat_file_adapter(path)
  original_mtime <- file.info(path)$mtime
  adapter_env <- environment(builder_adapter_inspect)
  original <- .builder_adapter_after_read
  assign(
    ".builder_adapter_after_read",
    function(adapter) {
      bytes <- readBin(
        adapter$location,
        what = "raw",
        n = file.info(adapter$location)$size
      )
      bytes[[length(bytes)]] <- as.raw(bitwXor(
        as.integer(bytes[[length(bytes)]]),
        1L
      ))
      writeBin(bytes, adapter$location)
      Sys.setFileTime(adapter$location, original_mtime)
    },
    envir = adapter_env
  )
  on.exit(
    assign(".builder_adapter_after_read", original, envir = adapter_env),
    add = TRUE
  )

  expect_error(builder_adapter_inspect(adapter), "changed while")
})

test_that("adapter and snapshot public interfaces stay narrow", {
  expect_identical(names(formals(builder_seurat_file_adapter)), "path")
  expect_identical(names(formals(builder_example_adapter)), c("id", "object"))
  expect_identical(names(formals(builder_adapter_inspect)), "adapter")
  expect_identical(
    names(formals(builder_snapshot_seurat)),
    c("object", "snapshot_dir", "available_bytes")
  )
  expect_identical(names(formals(builder_open_snapshot)), "snapshot")
})

test_that("registration publishes a snapshot before exposing an object", {
  root <- withr::local_tempdir()
  path <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  objects <- new.env(parent = emptyenv())
  snapshots <- new.env(parent = emptyenv())
  globals <- c(
    ".builder_objects",
    ".builder_snapshots",
    ".builder_snapshot_root"
  )
  prior <- lapply(globals, function(name) {
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      list(present = TRUE, value = get(name, envir = globalenv()))
    } else {
      list(present = FALSE)
    }
  })
  names(prior) <- globals
  on.exit(
    {
      for (name in globals) {
        if (prior[[name]]$present) {
          assign(name, prior[[name]]$value, envir = globalenv())
        } else if (exists(name, envir = globalenv(), inherits = FALSE)) {
          rm(list = name, envir = globalenv())
        }
      }
    },
    add = TRUE
  )
  assign(".builder_objects", objects, envir = globalenv())
  assign(".builder_snapshots", snapshots, envir = globalenv())
  assign(".builder_snapshot_root", root, envir = globalenv())

  adapter <- builder_seurat_file_adapter(path)
  result <- .builder_register_adapter(adapter, "ds1")
  unlink(path)

  expect_s3_class(result$dataset_profile, "builder_dataset_profile")
  expect_null(result$dataset_profile$identity$cells$ids)
  expect_true(builder_axis_identity_valid(
    result$dataset_profile$identity$cells$axis_identity
  ))
  expect_identical(
    result$matrix_summary$estimated_bytes,
    result$snapshot$closure_bytes
  )
  expect_identical(result$snapshot$source_state, adapter$source_state)
  expect_s4_class(get("ds1", envir = objects), "Seurat")
  snapshot <- get("ds1", envir = snapshots)
  expect_true(dir.exists(snapshot$path))
  expect_s4_class(builder_open_snapshot(snapshot), "Seurat")
})

test_that("registration reuses an owned snapshot for an unchanged fingerprint", {
  root <- withr::local_tempdir()
  path <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  adapter <- builder_seurat_file_adapter(path)
  objects <- new.env(parent = emptyenv())
  snapshots <- new.env(parent = emptyenv())
  cache <- new.env(parent = emptyenv())
  globals <- c(
    ".builder_objects",
    ".builder_snapshots",
    ".builder_snapshot_cache",
    ".builder_snapshot_root"
  )
  prior <- lapply(globals, function(name) {
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      list(present = TRUE, value = get(name, envir = globalenv()))
    } else {
      list(present = FALSE)
    }
  })
  names(prior) <- globals
  on.exit(
    {
      for (name in globals) {
        if (prior[[name]]$present) {
          assign(name, prior[[name]]$value, envir = globalenv())
        } else if (exists(name, envir = globalenv(), inherits = FALSE)) {
          rm(list = name, envir = globalenv())
        }
      }
    },
    add = TRUE
  )
  assign(".builder_objects", objects, envir = globalenv())
  assign(".builder_snapshots", snapshots, envir = globalenv())
  assign(".builder_snapshot_cache", cache, envir = globalenv())
  assign(".builder_snapshot_root", root, envir = globalenv())

  first <- .builder_register_adapter(adapter, "ds1")
  second <- .builder_register_adapter(builder_seurat_file_adapter(path), "ds2")

  expect_false(first$cache_hit)
  expect_true(second$cache_hit)
  expect_identical(second$snapshot$path, first$snapshot$path)
  expect_s4_class(get("ds2", envir = objects), "Seurat")
})

test_that("file fingerprints change when content changes", {
  root <- withr::local_tempdir()
  path <- file.path(root, "source.rds")
  bytes <- as.raw(rep(1L, 131072L))
  writeBin(bytes, path)
  first <- builder_seurat_file_adapter(path)$fingerprint
  info <- file.info(path)

  bytes[[65537L]] <- as.raw(2L)
  writeBin(bytes, path)
  Sys.setFileTime(path, info$mtime)
  second <- builder_seurat_file_adapter(path)$fingerprint

  expect_false(identical(first, second))
})

.builder_wait_for_worker <- function(worker, timeout = 30) {
  deadline <- Sys.time() + timeout
  repeat {
    polled <- builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      return(list(worker = worker, result = polled$result))
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the Builder worker.")
    }
  }
}

test_that("real worker loads, fresh-builds and drops an owned snapshot", {
  skip_if_not_installed("callr")
  builder_repo_source("worker.R", local = globalenv())
  builder_repo_source("session.R", local = globalenv())
  root <- withr::local_tempdir()
  source <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, source)
  started <- builder_session_start(
    builder_profile_inst_path("builder")
  )
  expect_null(started$error)
  worker <- started$worker
  on.exit(try(worker$process$close(), silent = TRUE), add = TRUE)

  builder_session_load(worker, "ds1", source)
  loaded <- .builder_wait_for_worker(worker)
  worker <- loaded$worker
  expect_true(loaded$result$done)
  expect_null(loaded$result$value$error)
  expected_backends <- list(h5 = requireNamespace("HDF5Array", quietly = TRUE))
  expect_identical(
    loaded$result$value$backend_capabilities,
    list(build = expected_backends, viewer = expected_backends)
  )
  snapshot <- loaded$result$value$snapshot
  worker <- builder_worker_register_snapshot(worker, "ds1", snapshot)
  unlink(source)
  expect_false(file.exists(source))

  live_object <- worker$process$run(function() {
    get("ds1", envir = get(".builder_objects", envir = globalenv()))
  })
  expect_s4_class(live_object, "Seurat")
  out <- file.path(root, "out")
  dir.create(out)
  source_snapshot_identity <- c(
    list(available = TRUE, snapshot = snapshot, source = list()),
    snapshot
  )
  object <- SeuratObject::pbmc_small
  plan <- structure(
    list(
      make_app = FALSE,
      app_contract_version = 0L,
      app_options = list(enabled = FALSE),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      ),
      items = list(list(
        id = "ds1",
        name = "PBMC",
        filename = "pbmc.crb",
        organism = "hg",
        assay = "RNA",
        layer = "data",
        included_groups = "groups",
        included_projections = "tsne",
        analyses = character(),
        analysis_dependency_graph = list(),
        artifact_identity = list(
          schema_version = 2L,
          cells = SeuratObject::Cells(object),
          features = rownames(object[["RNA"]]),
          group_levels = list(
            groups = sort(unique(as.character(object$groups)))
          ),
          projections = "tsne",
          source_metadata = c(
            "cell_barcode",
            "groups",
            "nCount_RNA",
            "nFeature_RNA"
          ),
          metadata = c("cell_barcode", "groups", "nUMI", "nGene"),
          spatial_sections = character()
        ),
        tables = list(),
        images = list(),
        nUMI = "nCount_RNA",
        nGene = "nFeature_RNA",
        default_group = "groups",
        metadata_policy = list(
          included = c(
            "cell_barcode",
            "groups",
            "nCount_RNA",
            "nFeature_RNA"
          )
        ),
        expression_backend = "embedded",
        sidecars = character(),
        manifest = list(),
        viewer_page_expectations = list(
          visible_conditional = character(),
          hidden_conditional = character()
        ),
        source_snapshot_identity = source_snapshot_identity
      ))
    ),
    class = c("builder_build_plan", "list")
  )
  builder_session_build(worker, plan)
  built <- .builder_wait_for_worker(worker, timeout = 60)
  worker <- built$worker
  expect_true(built$result$done)
  expect_null(built$result$value$error)
  expect_true(built$result$value$publishable)
  expect_length(built$result$value$built, 1L)
  expect_true(file.exists(built$result$value$built[[1L]]))
  expect_true(startsWith(
    normalizePath(built$result$value$built[[1L]], winslash = "/"),
    normalizePath(worker$snapshot_root, winslash = "/")
  ))
  expect_false(file.exists(file.path(out, "pbmc.crb")))

  builder_session_drop(worker, "ds1")
  dropped <- .builder_wait_for_worker(worker)
  worker <- dropped$worker
  expect_identical(dropped$result$value, TRUE)
  expect_true(.builder_snapshot_owned(snapshot))
  worker <- builder_worker_release_snapshot(
    worker,
    "ds1",
    .builder_worker_identity(snapshot)
  )
  state <- worker$process$run(function() {
    snapshots <- get(".builder_snapshots", envir = globalenv())
    root <- get(".builder_snapshot_root", envir = globalenv())
    list(
      registered = exists("ds1", envir = snapshots, inherits = FALSE),
      remaining = list.files(root, all.files = TRUE, no.. = TRUE)
    )
  })
  expect_false(state$registered)
  expect_true(any(startsWith(state$remaining, "builder-build-stage-")))
  expect_length(worker$snapshot_registry, 0L)
})

.builder_local_worker_globals <- function(values, envir = parent.frame()) {
  names <- c(names(values), ".builder_object_ledger")
  prior <- lapply(names, function(name) {
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      list(present = TRUE, value = get(name, envir = globalenv()))
    } else {
      list(present = FALSE)
    }
  })
  names(prior) <- names
  withr::defer(
    for (name in names) {
      if (prior[[name]]$present) {
        assign(name, prior[[name]]$value, envir = globalenv())
      } else if (exists(name, envir = globalenv(), inherits = FALSE)) {
        rm(list = name, envir = globalenv())
      }
    },
    envir = envir
  )
  if (exists(".builder_object_ledger", envir = globalenv(), inherits = FALSE)) {
    rm(list = ".builder_object_ledger", envir = globalenv())
  }
  for (name in names(values)) {
    assign(name, values[[name]], envir = globalenv())
  }
  invisible(TRUE)
}

test_that("worker objects are evicted least-recently-used within the budget", {
  objects <- new.env(parent = emptyenv())
  mb <- 1024^2
  .builder_local_worker_globals(list(
    .builder_objects = objects,
    .builder_object_budget = 20 * mb
  ))
  for (id in c("a", "b", "c")) {
    value <- numeric(1024^2)
    assign(id, value, envir = objects)
    .builder_worker_touch_object(id, value)
  }
  # 3 x 8 MB exceeds 20 MB: the least recently used object goes first.
  .builder_worker_touch_object("a")
  evicted <- .builder_worker_evict_objects(keep = "a", reserve_bytes = 0)
  expect_identical(evicted, "b")
  expect_setequal(ls(objects), c("a", "c"))

  # Making room for a decode releases more, but never the kept object.
  evicted <- .builder_worker_evict_objects(keep = "a", reserve_bytes = 30 * mb)
  expect_identical(evicted, "c")
  expect_identical(ls(objects), "a")

  # A dataset dropped elsewhere is forgotten by the ledger.
  rm("a", envir = objects)
  expect_length(.builder_worker_evict_objects(reserve_bytes = 0), 0L)
  expect_length(.builder_worker_object_ledger()$bytes, 0L)
})

test_that("a zero budget keeps only the dataset in use", {
  objects <- new.env(parent = emptyenv())
  .builder_local_worker_globals(list(
    .builder_objects = objects,
    .builder_object_budget = 0
  ))
  for (id in c("a", "b")) {
    assign(id, id, envir = objects)
    .builder_worker_touch_object(id, id)
  }
  expect_identical(.builder_worker_evict_objects(keep = "b"), "a")
  expect_identical(.builder_worker_evict_objects(), "b")
})

test_that("revisiting a resident dataset neither reopens nor rehashes it", {
  root <- withr::local_tempdir()
  objects <- new.env(parent = emptyenv())
  snapshots <- new.env(parent = emptyenv())
  .builder_local_worker_globals(list(
    .builder_objects = objects,
    .builder_snapshots = snapshots,
    .builder_snapshot_root = root,
    .builder_object_budget = 2^40
  ))
  for (id in c("a", "b")) {
    path <- file.path(root, paste0(id, ".rds"))
    saveRDS(SeuratObject::pbmc_small, path)
    .builder_register_adapter(builder_seurat_file_adapter(path), id)
  }
  expect_setequal(ls(objects), c("a", "b"))

  digests <- 0L
  testthat::local_mocked_bindings(
    md5sum = function(files) {
      digests <<- digests + 1L
      stop("unexpected content hash")
    },
    .package = "tools"
  )
  expect_s4_class(.builder_worker_object("a"), "Seurat")
  expect_s4_class(.builder_worker_object("b"), "Seurat")
  expect_identical(digests, 0L)

  # An evicted dataset reopens from its own snapshot by stat and marker only.
  rm("a", envir = objects)
  expect_s4_class(.builder_worker_object("a"), "Seurat")
  expect_identical(digests, 0L)
  expect_setequal(ls(objects), c("a", "b"))
  # The public reopen still verifies content.
  expect_error(builder_open_snapshot(get("a", envir = snapshots)))
  expect_identical(digests, 1L)
})

test_that("releasing an owned snapshot does not rehash its content", {
  root <- withr::local_tempdir()
  snapshot <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "release.snapshot"),
    available_bytes = 2^40
  )
  testthat::local_mocked_bindings(
    md5sum = function(files) stop("unexpected content hash"),
    .package = "tools"
  )
  expect_true(.builder_snapshot_release(snapshot))
  expect_false(dir.exists(snapshot$path))
})
