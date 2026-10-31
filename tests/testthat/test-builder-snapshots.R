builder_repo_source("io.R")
builder_repo_source("adapters.R")

.builder_snapshot_object <- function(matrix) {
  SeuratObject::CreateSeuratObject(counts = matrix)
}

.builder_snapshot_dense <- function() {
  Matrix::Matrix(
    matrix(
      seq_len(48),
      nrow = 8,
      dimnames = list(paste0("g", 1:8), paste0("c", 1:6))
    ),
    sparse = TRUE
  )
}

.builder_snapshot_cache <- function(path) {
  stub <- readRDS(file.path(path, "object.rds"))
  stub@tools[["SaveSeuratRds"]]
}

.builder_expect_cache_contained <- function(snapshot) {
  cache <- .builder_snapshot_cache(snapshot$path)
  expect_true(nrow(cache) > 0L)
  members <- unlist(strsplit(cache$path, ",", fixed = TRUE), use.names = FALSE)
  root <- normalizePath(snapshot$path, winslash = "/", mustWork = TRUE)
  targets <- normalizePath(members, winslash = "/", mustWork = TRUE)
  expect_true(all(startsWith(targets, paste0(root, "/"))))
}

.builder_cache_fixture <- function(path, pkg = "HDF5Array") {
  data.frame(
    layer = "counts",
    path = path,
    class = "fixture",
    pkg = pkg,
    fxn = "identity",
    assay = "RNA",
    stringsAsFactors = FALSE
  )
}

test_that("embedded snapshots are private, atomic and reopenable", {
  root <- withr::local_tempdir()
  target <- file.path(root, "embedded.snapshot")
  snapshot <- builder_snapshot_seurat(
    .builder_snapshot_object(.builder_snapshot_dense()),
    target,
    available_bytes = 2^40
  )

  expect_true(dir.exists(target))
  expect_false(any(grepl("stage", list.files(root), fixed = TRUE)))
  expect_s4_class(builder_open_snapshot(snapshot), "Seurat")
  expect_identical(snapshot$path, normalizePath(target, winslash = "/"))
  if (.Platform$OS.type != "windows") {
    expect_identical(
      as.integer(file.info(target)$mode),
      strtoi("700", base = 8L)
    )
    files <- list.files(target, recursive = TRUE, full.names = TRUE)
    expect_true(all(
      as.integer(file.info(files)$mode) == strtoi("600", base = 8L)
    ))
  }
})

test_that("snapshot ownership is bound after permissions settle", {
  root <- withr::local_tempdir()
  runtime <- environment(builder_snapshot_seurat)
  original_permissions <- get(
    ".builder_snapshot_permissions",
    envir = runtime,
    inherits = FALSE
  )
  assign(
    ".builder_snapshot_permissions",
    function(path) {
      original_permissions(path)
      object_file <- file.path(path, "object.rds")
      Sys.setFileTime(object_file, file.info(object_file)$mtime - 60)
      invisible(TRUE)
    },
    envir = runtime
  )
  withr::defer(assign(
    ".builder_snapshot_permissions",
    original_permissions,
    envir = runtime
  ))

  snapshot <- builder_snapshot_seurat(
    .builder_snapshot_object(.builder_snapshot_dense()),
    file.path(root, "permissions.snapshot"),
    available_bytes = 2^40
  )

  expect_true(.builder_snapshot_owned(snapshot))
  expect_s4_class(builder_open_snapshot(snapshot), "Seurat")
})

test_that("in-memory snapshots keep the inspected object without a stub read-back", {
  root <- withr::local_tempdir()
  object <- .builder_snapshot_object(.builder_snapshot_dense())
  read_backs <- 0L
  frozen <- local({
    testthat::local_mocked_bindings(
      readRDS = function(...) {
        read_backs <<- read_backs + 1L
        stop("An in-memory snapshot must not read the stub back.")
      },
      .package = "base"
    )
    .builder_snapshot_seurat_impl(
      object,
      file.path(root, "mem.snapshot"),
      available_bytes = 2^40
    )
  })

  expect_identical(read_backs, 0L)
  expect_identical(frozen$object, object)
  expect_s4_class(builder_open_snapshot(frozen$snapshot), "Seurat")
  expect_equal(
    as.matrix(SeuratObject::LayerData(
      builder_open_snapshot(frozen$snapshot),
      layer = "counts"
    )),
    as.matrix(.builder_snapshot_dense())
  )
})

## A Seurat object whose counts live in an HDF5 file under `root`.
.builder_snapshot_hdf5_object <- function(root, name = "counts") {
  source <- file.path(root, "source.h5")
  on_disk <- HDF5Array::writeHDF5Array(
    .builder_snapshot_dense(),
    filepath = source,
    name = name
  )
  dimnames(on_disk) <- dimnames(.builder_snapshot_dense())
  list(object = .builder_snapshot_object(on_disk), source = source)
}

test_that("file-backed snapshots read the stub once and re-link to the backing", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  fixture <- .builder_snapshot_hdf5_object(root)
  object <- fixture$object
  source <- fixture$source
  original_readRDS <- base::readRDS
  read_backs <- 0L
  frozen <- local({
    testthat::local_mocked_bindings(
      readRDS = function(file, refhook = NULL) {
        read_backs <<- read_backs + 1L
        original_readRDS(file, refhook)
      },
      .package = "base"
    )
    .builder_snapshot_seurat_impl(
      object,
      file.path(root, "hdf5.snapshot"),
      available_bytes = 2^40
    )
  })
  unlink(source)

  expect_identical(read_backs, 1L)
  expect_equal(
    as.matrix(SeuratObject::LayerData(frozen$object, layer = "counts")),
    as.matrix(.builder_snapshot_dense())
  )
  .builder_expect_cache_contained(frozen$snapshot)
  expect_s4_class(builder_open_snapshot(frozen$snapshot), "Seurat")
})

test_that("BPCells matrices are rejected before snapshot publication", {
  skip_if_not_installed("BPCells")
  root <- withr::local_tempdir()
  source <- file.path(root, "raw")
  suppressWarnings(BPCells::write_matrix_dir(
    methods::as(.builder_snapshot_dense(), "IterableMatrix"),
    source
  ))
  cases <- list(
    directory = BPCells::open_matrix_dir(source),
    memory = BPCells::write_matrix_memory(
      methods::as(.builder_snapshot_dense(), "IterableMatrix")
    )
  )

  for (name in names(cases)) {
    target <- file.path(root, paste0(name, ".snapshot"))
    expect_error(
      builder_snapshot_seurat(
        .builder_snapshot_object(cases[[name]]),
        target,
        available_bytes = 2^40
      ),
      "BPCells matrix",
      info = name
    )
    expect_false(dir.exists(target), info = name)
  }
})

test_that("HDF5 backing data is closed over before its source is replaced", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  source <- file.path(root, "source.h5")
  on_disk <- HDF5Array::writeHDF5Array(
    .builder_snapshot_dense(),
    filepath = source,
    name = "counts  batch:+1"
  )
  dimnames(on_disk) <- dimnames(.builder_snapshot_dense())
  object <- .builder_snapshot_object(on_disk)
  snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "hdf5.snapshot"),
    available_bytes = 2^40
  )
  unlink(source)
  writeBin(charToRaw("replacement"), source)

  reopened <- builder_open_snapshot(snapshot)
  expect_equal(
    as.matrix(SeuratObject::LayerData(reopened, layer = "counts")),
    as.matrix(.builder_snapshot_dense())
  )
  .builder_expect_cache_contained(snapshot)
})

test_that("HDF5Array delayed operations fail closed before publication", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  source <- file.path(root, "raw.h5")
  raw <- HDF5Array::writeHDF5Array(
    .builder_snapshot_dense(),
    filepath = source,
    name = "counts"
  )
  dimnames(raw) <- dimnames(.builder_snapshot_dense())
  target <- file.path(root, "transformed.snapshot")

  expect_error(
    builder_snapshot_seurat(
      .builder_snapshot_object(raw * 2),
      target,
      available_bytes = 2^40
    ),
    "unsupported HDF5Array|delayed operation"
  )
  expect_false(dir.exists(target))
})

test_that("embedded DelayedArray matrices remain snapshot-compatible", {
  skip_if_not_installed("DelayedArray")
  root <- withr::local_tempdir()
  embedded <- DelayedArray::DelayedArray(
    as.matrix(.builder_snapshot_dense())
  ) *
    2
  snapshot <- builder_snapshot_seurat(
    .builder_snapshot_object(embedded),
    file.path(root, "delayed-memory.snapshot"),
    available_bytes = 2^40
  )

  expect_null(.builder_snapshot_cache(snapshot$path))
  expect_equal(
    as.matrix(SeuratObject::LayerData(
      builder_open_snapshot(snapshot),
      layer = "counts"
    )),
    as.matrix(.builder_snapshot_dense()) * 2
  )
})

test_that("snapshot creation rejects unsafe paths and insufficient space", {
  object <- .builder_snapshot_object(.builder_snapshot_dense())
  root <- withr::local_tempdir()
  existing <- file.path(root, "existing.snapshot")
  dir.create(existing)

  expect_error(
    builder_snapshot_seurat(object, existing, available_bytes = 2^40),
    "already exists"
  )
  expect_true(dir.exists(existing))
  expect_error(
    builder_snapshot_seurat(
      object,
      file.path(root, "small.snapshot"),
      available_bytes = 1
    ),
    "free space"
  )
  expect_false(dir.exists(file.path(root, "small.snapshot")))

  source <- file.path(root, "source")
  dir.create(source)
  saveRDS(object, file.path(source, "object.rds"))
  link <- file.path(root, "object-link.rds")
  if (
    isTRUE(suppressWarnings(
      file.symlink(file.path(source, "object.rds"), link)
    ))
  ) {
    expect_error(builder_seurat_file_adapter(link), "symbolic link")
  }

  linked_parent <- file.path(root, "linked-parent")
  if (isTRUE(suppressWarnings(file.symlink(source, linked_parent)))) {
    expect_error(
      builder_seurat_file_adapter(file.path(linked_parent, "object.rds")),
      "symbolic link"
    )
  }
})

test_that("free-space queries support POSIX and Windows volumes", {
  posix <- .builder_snapshot_space_query("/tmp/space here", os_type = "unix")
  expect_identical(posix$command, "df")
  expect_identical(posix$multiplier, 1024)
  expect_match(paste(posix$args, collapse = " "), "space here", fixed = TRUE)

  windows <- .builder_snapshot_space_query(
    "C:\\Builder Snapshots",
    os_type = "windows"
  )
  expect_true(windows$command %in% c("powershell", "pwsh"))
  expect_identical(windows$multiplier, 1)
  expect_match(paste(windows$args, collapse = " "), "Get-PSDrive", fixed = TRUE)

  fake_runner <- function(command, args, stdout, stderr) "4294967296"
  expect_identical(
    .builder_snapshot_available_bytes(
      "C:\\Builder Snapshots",
      os_type = "windows",
      runner = fake_runner
    ),
    4294967296
  )
})

test_that("layer cache validation rejects incomplete and ambiguous closures", {
  root <- withr::local_tempdir()
  missing <- file.path(root, "missing")
  expect_error(
    .builder_snapshot_validate_cache(.builder_cache_fixture(missing)),
    "dependency is missing"
  )

  comma_path <- file.path(root, "real,comma")
  writeLines("x", comma_path)
  expect_error(
    .builder_snapshot_validate_cache(.builder_cache_fixture(comma_path)),
    "contains a comma"
  )

  one <- file.path(root, "one")
  two <- file.path(root, "two")
  writeLines("one", one)
  writeLines("two", two)
  validated <- .builder_snapshot_validate_cache(
    .builder_cache_fixture(paste(one, two, sep = ","))
  )
  expect_identical(validated$members[[1L]], c(one, two))

  adapter_env <- environment(.builder_snapshot_validate_cache)
  assign(
    "requireNamespace",
    function(package, quietly) !identical(package, "HDF5Array"),
    envir = adapter_env
  )
  on.exit(rm("requireNamespace", envir = adapter_env), add = TRUE)
  expect_error(
    .builder_snapshot_validate_cache(.builder_cache_fixture(one)),
    "missing package"
  )
})

test_that("layer cache validation rejects links inside a backing tree", {
  root <- withr::local_tempdir()
  backing <- file.path(root, "backing")
  dir.create(backing)
  outside <- file.path(root, "outside")
  writeLines("private", outside)
  if (
    isTRUE(suppressWarnings(
      file.symlink(outside, file.path(backing, "linked"))
    ))
  ) {
    expect_error(
      .builder_snapshot_validate_cache(.builder_cache_fixture(backing)),
      "backing tree contains a symbolic link"
    )
  } else {
    succeed("Symbolic links are unavailable on this host.")
  }
})

test_that("opening rejects a changed snapshot before cache code can run", {
  root <- withr::local_tempdir()
  snapshot <- builder_snapshot_seurat(
    .builder_snapshot_object(.builder_snapshot_dense()),
    file.path(root, "tampered.snapshot"),
    available_bytes = 2^40
  )
  sentinel <- file.path(root, "sentinel")
  object <- readRDS(snapshot$object_file)
  object@tools[["SaveSeuratRds"]] <- data.frame(
    layer = "counts",
    path = snapshot$object_file,
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
  saveRDS(object, snapshot$object_file)

  expect_error(builder_open_snapshot(snapshot), "owner record|integrity")
  expect_false(file.exists(sentinel))
})

test_that("opening rejects non-allowlisted cache loaders without executing", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  snapshot <- builder_snapshot_seurat(
    .builder_snapshot_hdf5_object(root)$object,
    file.path(root, "loader.snapshot"),
    available_bytes = 2^40
  )
  sentinel <- file.path(root, "sentinel")
  object <- readRDS(snapshot$object_file)
  object@tools$SaveSeuratRds$fxn <- paste0(
    "function(x) { file.create(",
    dQuote(sentinel),
    "); x }"
  )
  saveRDS(object, snapshot$object_file)
  marker_path <- file.path(snapshot$path, ".builder-owner.rds")
  marker <- readRDS(marker_path)
  marker$object_md5 <- unname(tools::md5sum(snapshot$object_file))
  object_state <- .builder_file_source_state(snapshot$object_file)
  object_state$location <- NULL
  marker$object_state <- object_state
  snapshot$object_md5 <- marker$object_md5
  snapshot$object_state <- object_state
  saveRDS(marker, marker_path)

  expect_error(builder_open_snapshot(snapshot), "loader is not allowed")
  expect_false(file.exists(sentinel))
})

test_that("a target appearing before publication is preserved", {
  object <- .builder_snapshot_object(.builder_snapshot_dense())
  root <- withr::local_tempdir()
  target <- file.path(root, "raced.snapshot")
  adapter_env <- environment(builder_snapshot_seurat)
  original <- .builder_snapshot_after_copy
  assign(
    ".builder_snapshot_after_copy",
    function() dir.create(target),
    envir = adapter_env
  )
  on.exit(
    assign(".builder_snapshot_after_copy", original, envir = adapter_env),
    add = TRUE
  )

  expect_error(
    builder_snapshot_seurat(object, target, available_bytes = 2^40),
    "appeared during creation"
  )
  expect_true(dir.exists(target))
})

test_that("a foreign directory replacing a published snapshot is preserved", {
  object <- .builder_snapshot_object(.builder_snapshot_dense())
  root <- withr::local_tempdir()
  target <- file.path(root, "replaced.snapshot")
  adapter_env <- environment(builder_snapshot_seurat)
  original <- .builder_snapshot_after_publish
  assign(
    ".builder_snapshot_after_publish",
    function(snapshot) {
      unlink(snapshot$path, recursive = TRUE, force = TRUE)
      dir.create(snapshot$path)
      writeLines("foreign", file.path(snapshot$path, "foreign.txt"))
      stop("Injected post-publication failure.")
    },
    envir = adapter_env
  )
  on.exit(
    assign(".builder_snapshot_after_publish", original, envir = adapter_env),
    add = TRUE
  )

  expect_error(
    builder_snapshot_seurat(object, target, available_bytes = 2^40),
    "Injected post-publication failure"
  )
  expect_identical(readLines(file.path(target, "foreign.txt")), "foreign")
})

test_that("a changed published object revokes cleanup authority", {
  object <- .builder_snapshot_object(.builder_snapshot_dense())
  root <- withr::local_tempdir()
  target <- file.path(root, "changed-object.snapshot")
  adapter_env <- environment(builder_snapshot_seurat)
  original <- .builder_snapshot_after_publish
  assign(
    ".builder_snapshot_after_publish",
    function(snapshot) {
      writeBin(charToRaw("changed"), snapshot$object_file)
      stop("Injected changed-object failure.")
    },
    envir = adapter_env
  )
  on.exit(
    assign(".builder_snapshot_after_publish", original, envir = adapter_env),
    add = TRUE
  )

  expect_error(
    builder_snapshot_seurat(object, target, available_bytes = 2^40),
    "Injected changed-object failure"
  )
  expect_true(dir.exists(target))
  expect_identical(
    readBin(file.path(target, "object.rds"), "raw", n = 7L),
    charToRaw("changed")
  )
})

test_that("release isolates then restores a raced foreign replacement", {
  root <- withr::local_tempdir()
  snapshot <- builder_snapshot_seurat(
    .builder_snapshot_object(.builder_snapshot_dense()),
    file.path(root, "release-race.snapshot"),
    available_bytes = 2^40
  )
  adapter_env <- environment(builder_snapshot_seurat)
  original <- .builder_cleanup_after_check
  assign(
    ".builder_cleanup_after_check",
    function(path) {
      unlink(path, recursive = TRUE, force = TRUE)
      dir.create(path)
      writeLines("foreign", file.path(path, "foreign.txt"))
    },
    envir = adapter_env
  )
  on.exit(
    assign(".builder_cleanup_after_check", original, envir = adapter_env),
    add = TRUE
  )

  expect_false(.builder_snapshot_release(snapshot))
  expect_identical(
    readLines(file.path(snapshot$path, "foreign.txt")),
    "foreign"
  )
})

test_that("failed stage cleanup preserves a raced foreign replacement", {
  root <- withr::local_tempdir()
  target <- file.path(root, "stage-race.snapshot")
  adapter_env <- environment(builder_snapshot_seurat)
  original_copy <- .builder_snapshot_after_copy
  original_cleanup <- .builder_cleanup_after_check
  assign(
    ".builder_snapshot_after_copy",
    function() stop("Injected stage failure."),
    envir = adapter_env
  )
  assign(
    ".builder_cleanup_after_check",
    function(path) {
      if (startsWith(basename(path), ".s-")) {
        unlink(path, recursive = TRUE, force = TRUE)
        dir.create(path)
        writeLines("foreign", file.path(path, "foreign.txt"))
      }
    },
    envir = adapter_env
  )
  on.exit(
    {
      assign(".builder_snapshot_after_copy", original_copy, envir = adapter_env)
      assign(
        ".builder_cleanup_after_check",
        original_cleanup,
        envir = adapter_env
      )
    },
    add = TRUE
  )

  expect_error(
    builder_snapshot_seurat(
      .builder_snapshot_object(.builder_snapshot_dense()),
      target,
      available_bytes = 2^40
    ),
    "Injected stage failure"
  )
  foreign <- list.files(
    root,
    pattern = "foreign[.]txt$",
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(foreign, 1L)
  expect_identical(readLines(foreign), "foreign")
})

test_that("backing changes during copy prevent publication", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  fixture <- .builder_snapshot_hdf5_object(root)
  object <- fixture$object
  source <- fixture$source
  adapter_env <- environment(builder_snapshot_seurat)
  original <- .builder_snapshot_after_copy
  assign(
    ".builder_snapshot_after_copy",
    function() cat("changed", file = source, append = TRUE),
    envir = adapter_env
  )
  on.exit(
    assign(".builder_snapshot_after_copy", original, envir = adapter_env),
    add = TRUE
  )
  target <- file.path(root, "changed.snapshot")

  expect_error(
    builder_snapshot_seurat(object, target, available_bytes = 2^40),
    "changed while it was copied"
  )
  expect_false(dir.exists(target))
})

test_that("snapshot failure does not leave a partial or replace a target", {
  object <- .builder_snapshot_object(.builder_snapshot_dense())
  root <- withr::local_tempdir()
  target <- file.path(root, "failed.snapshot")
  adapter_env <- environment(builder_snapshot_seurat)
  original <- .builder_snapshot_after_copy
  assign(
    ".builder_snapshot_after_copy",
    function() stop("Injected snapshot failure."),
    envir = adapter_env
  )
  on.exit(
    assign(".builder_snapshot_after_copy", original, envir = adapter_env),
    add = TRUE
  )

  expect_error(
    builder_snapshot_seurat(object, target, available_bytes = 2^40),
    "Injected snapshot failure"
  )
  expect_false(file.exists(target))
  expect_false(any(grepl("stage", list.files(root), fixed = TRUE)))
})

test_that("free space is queried before the snapshot object is written", {
  lines <- readLines(
    builder_profile_inst_path("builder", "adapters.R"),
    warn = FALSE
  )
  implementation <- grep(
    ".builder_snapshot_seurat_impl <-",
    lines,
    fixed = TRUE
  )[1L]
  query <- grep(
    ".builder_snapshot_available_bytes(parent)",
    lines,
    fixed = TRUE
  )[1L]
  save <- grep(
    ".builder_snapshot_save_stub(object, stub_path)",
    lines,
    fixed = TRUE
  )[1L]
  expect_true(implementation < query && query < save)
})

test_that("minimum headroom failure occurs before SaveSeuratRds", {
  root <- withr::local_tempdir()
  target <- file.path(root, "no-space.snapshot")
  adapter_env <- environment(builder_snapshot_seurat)
  original <- .builder_snapshot_save_stub
  called <- FALSE
  assign(
    ".builder_snapshot_save_stub",
    function(object, path) {
      called <<- TRUE
      stop("SaveSeuratRds should not run.")
    },
    envir = adapter_env
  )
  on.exit(
    assign(".builder_snapshot_save_stub", original, envir = adapter_env),
    add = TRUE
  )

  expect_error(
    builder_snapshot_seurat(
      .builder_snapshot_object(.builder_snapshot_dense()),
      target,
      available_bytes = 1024^3 - 1
    ),
    "1 GiB headroom"
  )
  expect_false(called)
  expect_false(dir.exists(target))
})

test_that("conservative object estimate is checked before SaveSeuratRds", {
  root <- withr::local_tempdir()
  target <- file.path(root, "estimated-space.snapshot")
  object <- .builder_snapshot_object(.builder_snapshot_dense())
  estimate <- as.numeric(object.size(object))
  adapter_env <- environment(builder_snapshot_seurat)
  original <- .builder_snapshot_save_stub
  called <- FALSE
  assign(
    ".builder_snapshot_save_stub",
    function(object, path) {
      called <<- TRUE
      stop("SaveSeuratRds should not run.")
    },
    envir = adapter_env
  )
  on.exit(
    assign(".builder_snapshot_save_stub", original, envir = adapter_env),
    add = TRUE
  )

  expect_error(
    builder_snapshot_seurat(
      object,
      target,
      available_bytes = 1024^3 + floor(estimate / 2)
    ),
    "free space"
  )
  expect_false(called)
  expect_false(dir.exists(target))
})
