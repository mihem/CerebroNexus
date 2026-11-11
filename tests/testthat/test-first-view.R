## The first view of the Projection page: what a page reads to preview the plot
## before its session exists (inst/viewer/core/shared_resources.R, drawn by
## inst/viewer/www/first-view.js).

## The process-level functions as a process that has just started has them:
## nothing registered, nothing published. (The directory of published first
## views belongs to the R process, so one test would find another's files.)
first_view_env <- function() {
  env <- new.env()
  sys.source(viewer_test_path("core", "shared_resources.R"), env)
  unlink(env$viewerFirstViewDirectory(), recursive = TRUE)
  env
}

## A Viewer Pack directory with the two files a first view names.
first_view_pack <- function(root, cells = 4L) {
  pack <- file.path(normalizePath(root, winslash = "/"), "data.viewer")
  dir.create(file.path(pack, "projections"), recursive = TRUE)
  dir.create(file.path(pack, "metadata"))
  writeBin(raw(cells * 8L), file.path(pack, "projections", "001.bin"))
  writeBin(raw(cells), file.path(pack, "metadata", "002.codes.bin"))
  writeLines("{}", file.path(pack, "manifest.json"))
  pack
}

## The first frame of the Projection page as cerebroCellViewRender() has it
## before it is packed, with the Pack files under their shared prefixes.
first_view_message <- function(env, pack, register = function(...) NULL) {
  prefix <- function(kind, directory) {
    env$viewerSharedResourcePrefix(
      kind,
      file.path(pack, directory),
      pack,
      register
    )
  }
  list(
    id = "overview_projection",
    render_token = 1,
    meta = list(color_type = "categorical", color_variable = "cluster"),
    data = list(
      color = list(I("#FFC312"), I("#C4E538")),
      point_size = 5,
      point_opacity = 1,
      point_line = list(),
      x_range = list(),
      y_range = list(),
      projection_resource = list(
        url = paste0(prefix("projection", "projections"), "/001.bin"),
        cells = 4L,
        dimensions = 2L,
        dtype = "float32",
        bytes = 32
      ),
      categorical_resource = list(
        url = paste0(prefix("metadata", "metadata"), "/002.codes.bin"),
        cells = 4L,
        dtype = "uint8",
        bytes = 4,
        levels = list("a", "b"),
        code_map = list(-1L, 0L, 1L)
      ),
      client_percentage = TRUE,
      percentage_cells_to_show = 100,
      n = 4L
    )
  )
}

test_that("a page address gives the file name the browser computes", {
  env <- first_view_env()
  ## "/" + "\n": first-view.js builds the same bytes from window.location
  expect_identical(env$viewerFirstViewKey("/", ""), "2f0a")
  expect_identical(env$viewerFirstViewKey("/", "?page=projection"), "2f0a")
  expect_identical(env$viewerFirstViewKey(NULL, NULL), "2f0a")
  expect_identical(
    env$viewerFirstViewKey("/app/", "?dataset=T%20cells&page=projection"),
    paste(
      as.character(charToRaw("/app/\nT cells")),
      collapse = ""
    )
  )
  ## another data set, another file
  expect_false(identical(
    env$viewerFirstViewKey("/", "?dataset=a"),
    env$viewerFirstViewKey("/", "?dataset=b")
  ))
  expect_match(env$viewerFirstViewKey("/café", ""), "^[0-9a-f]+$")
})

test_that("only the default view of all cells from shared files is described", {
  env <- first_view_env()
  pack <- first_view_pack(withr::local_tempdir())
  message <- first_view_message(env, pack)

  view <- env$viewerFirstViewPayload(message)
  expect_identical(view$version, 1L)
  expect_identical(view$cells, 4L)
  expect_identical(unclass(view$colors), c("#FFC312", "#C4E538"))
  expect_identical(unclass(view$code_map), c(-1L, 0L, 1L))
  expect_identical(view$projection$url, message$data$projection_resource$url)
  expect_identical(view$projection$bytes, 32)
  expect_identical(view$categories$bytes, 4)
  json <- jsonlite::fromJSON(
    jsonlite::toJSON(view, auto_unbox = TRUE, digits = NA)
  )
  expect_identical(json$colors, c("#FFC312", "#C4E538"))
  expect_identical(json$point_size, 5L)

  refused <- function(change) {
    changed <- change(message)
    is.null(env$viewerFirstViewPayload(changed))
  }
  ## another view, a continuous colour, a subset, a border, fixed axes
  expect_true(refused(function(m) {
    m$id <- "spatial_projection"
    m
  }))
  expect_true(refused(function(m) {
    m$meta$color_type <- "continuous"
    m
  }))
  expect_true(refused(function(m) {
    m$data$percentage_cells_to_show <- 50
    m
  }))
  expect_true(refused(function(m) {
    m$data$point_line <- list(color = "rgb(196,196,196)", width = 1)
    m
  }))
  expect_true(refused(function(m) {
    m$data$x_range <- c(-10, 10)
    m
  }))
  ## three dimensions, coordinates sent in the frame, a session's own prefix
  expect_true(refused(function(m) {
    m$data$projection_resource$dimensions <- 3L
    m
  }))
  expect_true(refused(function(m) {
    m$data$projection_resource <- NULL
    m
  }))
  expect_true(refused(function(m) {
    m$data$categorical_resource$url <- "cerebro-metadata-0123abcd-1/002.codes.bin"
    m
  }))
  ## colours that are not one per level
  expect_true(refused(function(m) {
    m$data$color <- list(I("#FFC312"))
    m
  }))
})

test_that("a first frame is published once and kept for later processes", {
  cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache)
  env <- first_view_env()
  pack <- first_view_pack(withr::local_tempdir())
  basis <- env$viewerFirstViewBasis(list(cerebro_version = "1", point_size = 5))
  message <- first_view_message(env, pack)

  file <- env$viewerFirstViewCapture(
    message,
    "/",
    "?page=projection",
    pack,
    basis
  )
  expect_identical(basename(file), "2f0a.json")
  published <- jsonlite::fromJSON(file)
  expect_identical(
    published$projection$url,
    message$data$projection_resource$url
  )
  kept <- list.files(env$viewerFirstViewCache(), full.names = TRUE)
  expect_length(kept, 1L)
  entry <- jsonlite::fromJSON(kept, simplifyVector = FALSE)
  ## kept by the files' place in the Pack, not by this process's URLs
  expect_null(entry$view$projection$url)
  expect_identical(entry$view$projection$path, "projections/001.bin")
  expect_identical(entry$view$categories$path, "metadata/002.codes.bin")
  expect_identical(entry$pack, pack)

  ## the same frame from the next session writes nothing
  Sys.setFileTime(file, Sys.time() - 3600)
  written <- file.mtime(file)
  env$viewerFirstViewCapture(message, "/", "?page=projection", pack, basis)
  expect_identical(file.mtime(file), written)
  ## a frame a preview cannot stand in for publishes nothing
  message$data$percentage_cells_to_show <- 50
  expect_null(
    env$viewerFirstViewCapture(message, "/", "?dataset=x", pack, basis)
  )
  expect_length(list.files(env$viewerFirstViewDirectory()), 1L)
})

test_that("a new process publishes what an earlier one kept", {
  cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache)
  earlier <- first_view_env()
  pack <- first_view_pack(withr::local_tempdir())
  options <- list(cerebro_version = "1", point_size = 5)
  basis <- earlier$viewerFirstViewBasis(options)
  earlier$viewerFirstViewCapture(
    first_view_message(earlier, pack),
    "/",
    "",
    pack,
    basis
  )
  unlink(earlier$viewerFirstViewDirectory(), recursive = TRUE)

  restore <- function(roots = pack, basis_now = basis) {
    env <- first_view_env()
    registered <- character()
    written <- env$viewerFirstViewRestore(
      roots,
      basis_now,
      function(prefix, directory) {
        registered[[prefix]] <<- directory
      }
    )
    unlink(env$viewerFirstViewDirectory(), recursive = TRUE)
    list(env = env, written = written, registered = registered)
  }

  ## The description is published again when the process starts, with the
  ## URLs of this process, and both Pack directories are made reachable.
  env <- first_view_env()
  registered <- character()
  written <- env$viewerFirstViewRestore(
    pack,
    basis,
    function(prefix, directory) {
      registered[[prefix]] <<- directory
    }
  )
  expect_identical(basename(written), "2f0a.json")
  view <- jsonlite::fromJSON(written)
  expect_match(
    view$projection$url,
    "^cerebro-projection-[a-z0-9]{24}-1/001[.]bin$"
  )
  expect_match(
    view$categories$url,
    "^cerebro-metadata-[a-z0-9]{24}-1/002[.]codes[.]bin$"
  )
  expect_null(view$projection$path)
  expect_identical(view$colors, c("#FFC312", "#C4E538"))
  expect_identical(view$code_map, c(-1L, 0L, 1L))
  expect_setequal(
    normalizePath(unname(registered), winslash = "/"),
    file.path(pack, c("projections", "metadata"))
  )
  expect_setequal(
    names(registered),
    dirname(c(view$projection$url, view$categories$url))
  )
  ## and the session of that page sends the frame the description promised
  expect_identical(
    jsonlite::fromJSON(env$viewerFirstViewCapture(
      first_view_message(env, pack),
      "/",
      "",
      pack,
      basis
    )),
    view
  )
  unlink(env$viewerFirstViewDirectory(), recursive = TRUE)

  ## Not with other launch options or another Viewer version, not for a data
  ## set that is not configured, and not after the Pack was built again.
  expect_length(
    restore(
      basis_now = earlier$viewerFirstViewBasis(utils::modifyList(
        options,
        list(point_size = 2)
      ))
    )$written,
    0L
  )
  expect_length(restore(roots = character())$written, 0L)
  expect_length(restore(roots = paste0(pack, "-other"))$written, 0L)
  Sys.setFileTime(file.path(pack, "manifest.json"), Sys.time() + 60)
  expect_length(restore()$written, 0L)
})

test_that("descriptions nothing has renewed for two months are removed", {
  cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache)
  env <- first_view_env()
  directory <- env$viewerFirstViewCache()
  dir.create(directory, recursive = TRUE)
  stale <- file.path(directory, "stale.json")
  recent <- file.path(directory, "recent.json")
  writeLines("{}", stale)
  writeLines("{}", recent)
  Sys.setFileTime(stale, Sys.time() - 61 * 24 * 3600)

  pack <- first_view_pack(withr::local_tempdir())
  env$viewerFirstViewCapture(
    first_view_message(env, pack),
    "/",
    "",
    pack,
    env$viewerFirstViewBasis(list())
  )
  expect_false(file.exists(stale))
  expect_true(file.exists(recent))
  expect_length(list.files(directory), 2L)
})

test_that("early prefixes get their headers when the web server exists", {
  env <- first_view_env()
  pack <- first_view_pack(withr::local_tempdir())
  testthat::local_mocked_bindings(
    addResourcePath = function(prefix, directoryPath) invisible(NULL),
    .package = "shiny"
  )
  env$viewerSharedResourceRegisterEarly(
    "cerebro-projection-x-1",
    file.path(pack, "projections")
  )
  ## nothing to set them on yet: they stay pending
  testthat::local_mocked_bindings(
    getShinyOption = function(name, default = NULL) default,
    .package = "shiny"
  )
  expect_false(env$viewerSharedResourceHeaders())

  paths <- NULL
  testthat::local_mocked_bindings(
    getShinyOption = function(name, default = NULL) {
      list(setStaticPath = function(.list) paths <<- .list)
    },
    .package = "shiny"
  )
  expect_true(env$viewerSharedResourceHeaders())
  expect_named(paths, "cerebro-projection-x-1")
  headers <- paths[[1L]]$options$headers
  expect_identical(headers[["Content-Encoding"]], "identity")
  expect_match(headers[["Cache-Control"]], "immutable")
  ## once
  expect_false(env$viewerSharedResourceHeaders())
})

## The first frame of a data set without a Viewer Pack: it carries its cells.
first_view_inline_message <- function(cells = 5000L) {
  set.seed(1)
  list(
    id = "overview_projection",
    render_token = 1,
    meta = list(color_type = "categorical", color_variable = "cluster"),
    data = list(
      x = I(round(stats::runif(cells, -10, 15), 3)),
      y = I(round(stats::runif(cells, -8, 18), 3)),
      color = list(I("#FFC312"), I("#C4E538"), I("#12CBC4")),
      canonical_group = I(c(NA, rep_len(0:2, cells - 1L))),
      point_size = 5,
      point_opacity = 1,
      point_line = list(),
      x_range = I(c(-11, 16)),
      y_range = I(c(-9, 19)),
      n = cells
    ),
    dataset_identity = list(cell_count = cells)
  )
}

test_that("a frame that carries its cells is described with two files", {
  env <- first_view_env()
  message <- first_view_inline_message()
  inline <- env$viewerFirstViewInline(message)
  expect_identical(inline$view$cells, 5000L)
  expect_identical(unclass(inline$view$code_map), 0:2)
  expect_identical(unclass(inline$view$x_range), c(-11, 16))
  expect_identical(unclass(inline$view$y_range), c(-9, 19))
  ## x and y of each cell side by side; a cell without a group has code 255
  expect_identical(
    inline$xy[1:2],
    c(message$data$x[[1L]], message$data$y[[1L]])
  )
  expect_length(inline$xy, 10000L)
  expect_identical(inline$codes[1:4], as.raw(c(255, 0, 1, 2)))

  directory <- withr::local_tempdir()
  view <- env$viewerFirstViewWriteInline(inline, "2f0a", directory)
  expect_match(
    view$projection$url,
    "^cerebro-first-view/2f0a[.][0-9a-f]{16}[.]xy[.]bin$"
  )
  expect_match(
    view$categories$url,
    "^cerebro-first-view/2f0a[.][0-9a-f]{16}[.]codes[.]bin$"
  )
  expect_identical(view$projection$bytes, 40000)
  expect_identical(view$categories$bytes, 5000)
  ## little-endian float32, as the browser reads a Float32Array
  written <- readBin(
    file.path(directory, basename(view$projection$url)),
    "numeric",
    n = 10000L,
    size = 4L,
    endian = "little"
  )
  expect_equal(written, inline$xy, tolerance = 1e-6)
  expect_identical(
    readBin(file.path(directory, basename(view$categories$url)), "raw", 5000L),
    inline$codes
  )
  ## the same cells again: the same files; other cells: the old files go
  expect_identical(
    env$viewerFirstViewWriteInline(inline, "2f0a", directory),
    view
  )
  expect_length(list.files(directory), 2L)
  inline$xy[[1L]] <- inline$xy[[1L]] + 1
  changed <- env$viewerFirstViewWriteInline(inline, "2f0a", directory)
  expect_false(identical(changed$projection$url, view$projection$url))
  expect_setequal(
    list.files(directory),
    basename(c(changed$projection$url, changed$categories$url))
  )

  ## fewer than 4,096 cells arrive as one vector per group
  nested <- first_view_inline_message(6L)
  nested$data$canonical_group <- NULL
  nested$data$x <- list(I(c(1, 2, 3)), I(c(4, 5)), I(6))
  nested$data$y <- list(I(c(10, 20, 30)), I(c(40, 50)), I(60))
  inline <- env$viewerFirstViewInline(nested)
  expect_identical(inline$xy, c(1, 10, 2, 20, 3, 30, 4, 40, 5, 50, 6, 60))
  expect_identical(inline$codes, as.raw(c(0, 0, 0, 1, 1, 2)))

  refused <- function(change) {
    is.null(env$viewerFirstViewInline(change(message)))
  }
  ## a subset of the data set, three dimensions, a border, a continuous colour,
  ## coordinates the browser already holds, more groups than a byte counts
  expect_true(refused(function(m) {
    m$dataset_identity$cell_count <- 6000L
    m
  }))
  expect_true(refused(function(m) {
    m$data$z <- m$data$x
    m
  }))
  expect_true(refused(function(m) {
    m$data$point_line <- list(color = "rgb(196,196,196)", width = 1)
    m
  }))
  expect_true(refused(function(m) {
    m$meta$color_type <- "continuous"
    m
  }))
  expect_true(refused(function(m) {
    m$shared_projection <- "UMAP"
    m$data$x <- NULL
    m$data$y <- NULL
    m
  }))
  expect_true(refused(function(m) {
    m$data$color <- rep(list(I("#FFC312")), 256L)
    m
  }))
  expect_true(refused(function(m) {
    m$data$x_range <- I(c(3, 3))
    m
  }))
})

test_that("an inline first view is published for a configured data set only", {
  cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = cache)
  env <- first_view_env()
  source <- file.path(
    normalizePath(withr::local_tempdir(), winslash = "/"),
    "data.crb"
  )
  writeBin(raw(8), source)
  basis <- env$viewerFirstViewBasis(list(cerebro_version = "1"))
  message <- first_view_inline_message()

  ## an upload, or a frame of a session whose data set is not configured
  expect_null(env$viewerFirstViewCapture(message, "/", "", character(), basis))
  expect_length(list.files(env$viewerFirstViewDirectory()), 0L)

  file <- env$viewerFirstViewCapture(
    message,
    "/",
    "",
    character(),
    basis,
    source
  )
  view <- jsonlite::fromJSON(file)
  expect_identical(view$cells, 5000L)
  expect_identical(view$x_range, c(-11L, 16L))
  expect_true(all(file.exists(file.path(
    env$viewerFirstViewDirectory(),
    basename(c(view$projection$url, view$categories$url))
  ))))
  ## kept with its two files
  kept <- list.files(env$viewerFirstViewCache())
  expect_length(kept, 3L)
  expect_length(grep("[.]json$", kept), 1L)

  ## a new process publishes it again, files included
  unlink(env$viewerFirstViewDirectory(), recursive = TRUE)
  later <- first_view_env()
  written <- later$viewerFirstViewRestore(
    character(),
    basis,
    function(...) stop("no Pack directory is involved"),
    source
  )
  expect_identical(basename(written), "2f0a.json")
  expect_identical(jsonlite::fromJSON(written), view)
  expect_true(all(file.exists(file.path(
    later$viewerFirstViewDirectory(),
    basename(c(view$projection$url, view$categories$url))
  ))))
  ## and the frame of that page's session describes itself in the same words
  before <- file.mtime(written)
  Sys.setFileTime(written, Sys.time() - 3600)
  later$viewerFirstViewCapture(message, "/", "", character(), basis, source)
  expect_lt(as.numeric(file.mtime(written)), as.numeric(before))
  unlink(later$viewerFirstViewDirectory(), recursive = TRUE)

  ## not for another launch, a data set that is not configured, or a CRB file
  ## written again
  restore <- function(basis_now = basis, sources = source) {
    env <- first_view_env()
    written <- env$viewerFirstViewRestore(
      character(),
      basis_now,
      function(...) NULL,
      sources
    )
    unlink(env$viewerFirstViewDirectory(), recursive = TRUE)
    written
  }
  expect_length(
    restore(basis_now = env$viewerFirstViewBasis(list(cerebro_version = "2"))),
    0L
  )
  expect_length(restore(sources = character()), 0L)
  Sys.setFileTime(source, Sys.time() + 60)
  expect_length(restore(), 0L)
})
