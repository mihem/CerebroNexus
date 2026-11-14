test_that("a background process writes the cell-name file once per cell order", {
  skip_if_not_installed("mirai")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  scope <- new.env(parent = globalenv())
  ## .viewerMirai(), which shiny_server.R takes from crb_cache.R.
  sys.source(viewer_test_path("crb_cache.R"), scope)
  sys.source(viewer_test_path("core", "viewer_pack.R"), scope)
  viewer_server_load(
    viewer_server_statements("shiny_server.R"),
    scope,
    names = c(
      ".viewer_cell_names_files",
      ".viewer_cell_names_job",
      "viewerCellNamesCache",
      "viewerCellNamesKept",
      "viewerCellNamesFile"
    )
  )
  pack <- list(
    manifest = list(cell_order_fingerprint = "md5-order:test"),
    canonical_order = TRUE,
    cells = c("c1", paste0("c", intToUtf8(233L))),
    cell_count = 2L
  )
  entry <- scope$viewerCellNamesFile(pack)
  result <- mirai::call_mirai(entry$task)$data
  expect_identical(result, list(cells = 2L, bytes = 7L))
  connection <- gzfile(entry$file, "rb")
  on.exit(close(connection), add = TRUE)
  expect_identical(
    readBin(connection, "raw", 64L),
    c(
      charToRaw("c1"),
      as.raw(0L),
      charToRaw(enc2utf8(paste0("c", intToUtf8(233L)))),
      as.raw(0L)
    )
  )
  ## Every later session of the process reuses the same file.
  expect_identical(scope$viewerCellNamesFile(pack), entry)
  expect_null(scope$viewerCellNamesFile(list(manifest = list())))
})

test_that("the cell-name resource describes the finished file", {
  skip_if_not_installed("mirai")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  scope <- new.env(parent = globalenv())
  ## .viewerMirai(), which shiny_server.R takes from crb_cache.R.
  sys.source(viewer_test_path("crb_cache.R"), scope)
  sys.source(viewer_test_path("core", "viewer_pack.R"), scope)
  viewer_server_load(
    viewer_server_statements("shiny_server.R"),
    scope,
    names = c(
      ".viewer_cell_names_files",
      ".viewer_cell_names_job",
      "viewerCellNamesCache",
      "viewerCellNamesKept",
      "viewerCellNamesFile"
    )
  )
  viewer_server_load(
    viewer_server_statements("shiny_server.R", server = TRUE),
    scope,
    names = "viewerCellNamesResource"
  )
  pack <- list(
    manifest = list(cell_order_fingerprint = "md5-order:resource"),
    canonical_order = TRUE,
    cells = c("a", "b", "c"),
    cell_count = 3L
  )
  scope$viewerPackCurrent <- function() pack
  scope$viewerStaticResourcePrefix <- function(kind, directory) "cell-names-x"
  scope$viewerCanonicalResourceIdentity <- function(pack) list(identity = "p")
  scope$input <- list()
  scope$isolate <- shiny::isolate
  ## The session never waits for the background process; the test does.
  mirai::call_mirai(scope$viewerCellNamesFile(pack)$task)
  descriptor <- scope$viewerCellNamesResource()
  expect_identical(descriptor$protocol, "canonical-cell-names-v1")
  expect_identical(descriptor$cells, 3L)
  expect_identical(descriptor$bytes, 6L)
  expect_match(descriptor$url, "^cell-names-x/md5-order_resource[.]bin[.]gz$")
  expect_identical(descriptor$identity, "p")
  ## A browser that could not read the file gets the names in messages.
  scope$input <- list(cell_view_names_resource_failed = descriptor$url)
  expect_null(scope$viewerCellNamesResource())
})

test_that("names go in messages while the cell-name file is written", {
  ## Timing-sensitive; kills a background process.
  skip_on_cran()
  skip_if_not_installed("mirai")
  scope <- new.env(parent = globalenv())
  viewer_server_load(
    viewer_server_statements("shiny_server.R", server = TRUE),
    scope,
    names = "viewerCellNamesResource"
  )
  entry <- new.env(parent = emptyenv())
  entry$task <- mirai::mirai(Sys.sleep(20))
  on.exit(mirai::stop_mirai(entry$task), add = TRUE)
  scope$viewerPackCurrent <- function() list()
  scope$viewerCellNamesFile <- function(pack) entry

  started <- proc.time()[["elapsed"]]
  expect_null(scope$viewerCellNamesResource())
  expect_lt(proc.time()[["elapsed"]] - started, 5)
  ## A later frame looks again.
  expect_null(entry$result)
  expect_true(inherits(entry$task, "mirai"))
})

cell_names_scope <- function() {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("crb_cache.R"), scope)
  sys.source(viewer_test_path("core", "viewer_pack.R"), scope)
  viewer_server_load(
    viewer_server_statements("shiny_server.R"),
    scope,
    names = c(
      ".viewer_cell_names_files",
      ".viewer_cell_names_job",
      "viewerCellNamesCache",
      "viewerCellNamesKept",
      "viewerCellNamesFile",
      "viewerCellNamesRead",
      "viewerCellNamesPending"
    )
  )
  scope
}

test_that("the cell-name file is kept for the next process of the same cells", {
  skip_if_not_installed("mirai")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  cells <- c("kept-1", paste0("kept-", intToUtf8(233L)), "kept-3")
  pack <- list(
    manifest = list(cell_order_fingerprint = "md5-order:kept"),
    canonical_order = TRUE,
    cells = cells,
    cell_count = 3L,
    cache = new.env(parent = emptyenv())
  )
  first <- cell_names_scope()
  entry <- first$viewerCellNamesFile(pack)
  result <- mirai::call_mirai(entry$task)$data
  expect_identical(result$cells, 3L)
  kept <- file.path(first$viewerCellNamesCache(), basename(entry$file))
  expect_true(file.exists(kept))
  expect_identical(
    readRDS(sub("[.]bin[.]gz$", ".rds", kept))$fingerprint,
    "md5-order:kept"
  )

  ## The next process: no background process, the file is there at once.
  unlink(entry$file)
  second <- cell_names_scope()
  again <- second$viewerCellNamesFile(pack)
  expect_null(again$task)
  expect_identical(again$result, result)
  expect_true(file.exists(again$file))
  expect_false(second$viewerCellNamesPending(pack))

  ## Some names are cut out of the file; only those strings are made.
  expect_identical(
    second$viewerCellNamesRead(pack, c(3L, 1L, 2L)),
    cells[c(3L, 1L, 2L)]
  )
  expect_identical(second$viewerCellNamesRead(pack, 2L), cells[2L])
  expect_null(second$viewerCellNamesRead(pack, c(1L, 4L)))
  ## and through the Pack's reader, which has not read the cell order for it
  pack$cache <- new.env(parent = emptyenv())
  expect_identical(
    second$viewerPackCellBarcodesAt(pack, c(2L, 3L)),
    cells[c(2L, 3L)]
  )
  expect_false(exists(
    "common/cell_order.qs2",
    envir = pack$cache,
    inherits = FALSE
  ))

  ## Kept for other cells, or a file that is not the one described: not used.
  third <- cell_names_scope()
  other <- pack
  other$cell_count <- 4L
  other$cells <- c(cells, "kept-4")
  unlink(entry$file)
  expect_null(third$viewerCellNamesKept("md5-order:kept", 4L, entry$file))
  writeBin(as.raw(1:3), kept)
  expect_null(third$viewerCellNamesKept("md5-order:kept", 3L, entry$file))
  expect_false(file.exists(entry$file))
})

test_that("a view's request for names waits for the cell-name file", {
  ## Timing-sensitive; kills a background process.
  skip_on_cran()
  skip_if_not_installed("mirai")
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), scope)
  viewer_server_load(
    viewer_server_statements("shiny_server.R"),
    scope,
    names = c(".viewer_cell_names_files", "viewerCellNamesPending")
  )
  viewer_server_load(
    viewer_server_statements("shiny_server.R", server = TRUE),
    scope,
    names = c(
      "viewer_aux_waiting",
      "viewerAuxRequestReceive",
      "viewerAuxRequestRelease"
    )
  )
  pack <- list(
    manifest = list(cell_order_fingerprint = "md5-order:pending"),
    canonical_order = TRUE
  )
  scope$viewerPackCurrent <- function() pack
  answered <- character()
  scope$cerebroCellViewAuxRequest <- function(request) {
    answered <<- c(answered, request$id)
  }

  ## no file is being written: answered at once
  expect_false(scope$viewerCellNamesPending(pack))
  expect_false(scope$viewerCellNamesPending(NULL))
  expect_true(scope$viewerAuxRequestReceive(list(id = "first")))
  expect_identical(answered, "first")

  ## while it is written the requests wait, in order
  entry <- new.env(parent = emptyenv())
  entry$task <- mirai::mirai(Sys.sleep(20))
  on.exit(try(mirai::stop_mirai(entry$task), silent = TRUE), add = TRUE)
  assign("md5-order:pending", entry, envir = scope$.viewer_cell_names_files)
  expect_true(scope$viewerCellNamesPending(pack))
  expect_false(scope$viewerAuxRequestReceive(list(id = "second")))
  expect_false(scope$viewerAuxRequestReceive(list(id = "third")))
  expect_false(scope$viewerAuxRequestRelease())
  expect_identical(answered, "first")

  ## the process has ended (here: stopped), and they are answered
  mirai::stop_mirai(entry$task)
  mirai::call_mirai(entry$task)
  expect_false(scope$viewerCellNamesPending(pack))
  expect_true(scope$viewerAuxRequestRelease())
  expect_identical(answered, c("first", "second", "third"))
  expect_true(scope$viewerAuxRequestRelease())
  expect_true(scope$viewerAuxRequestReceive(list(id = "fourth")))
  expect_identical(answered, c("first", "second", "third", "fourth"))
})

test_that("the same files keep one resource URL for the session", {
  scope <- new.env(parent = globalenv())
  scope$viewer_resource_by_directory <- new.env(parent = emptyenv())
  ## No configured data set shares these directories: each gets the session's
  ## own prefix (test-shared-resources.R covers the shared ones).
  sys.source(viewer_test_path("core", "shared_resources.R"), scope)
  scope$viewer_shared_resource_roots <- character()
  viewer_server_load(
    viewer_server_statements("shiny_server.R", server = TRUE),
    scope,
    names = c(
      "viewer_resource_prefixes",
      "viewer_resource_counts",
      "viewerRegisterStaticResource",
      "viewerStaticResourcePrefix"
    )
  )
  scope$session <- list(token = "tok")
  first <- withr::local_tempdir()
  second <- withr::local_tempdir()
  names_prefix <- scope$viewerStaticResourcePrefix("cell-names", first)
  ## Asked again for every frame: a new prefix would be a new URL, and the
  ## browser would download and keep the 1.46M cell names once more.
  expect_identical(
    scope$viewerStaticResourcePrefix("cell-names", first),
    names_prefix
  )
  expect_false(identical(
    scope$viewerStaticResourcePrefix("cell-names", second),
    names_prefix
  ))
  expect_false(identical(
    scope$viewerStaticResourcePrefix("projection", first),
    names_prefix
  ))
  expect_length(scope$viewer_resource_prefixes, 3L)
  for (prefix in scope$viewer_resource_prefixes) {
    shiny::removeResourcePath(prefix)
  }
})
