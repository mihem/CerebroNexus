test_that("run-demo loads the worktree and avoids a busy default port", {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  skip_if_not(file.exists(launcher), "repository-only launcher")
  withr::local_envvar(c(CEREBRO_1M_DEMO_CRB = NA, CEREBRO_REN_DEMO_CRB = NA))
  root <- normalizePath(testthat::test_path("..", ".."))
  datasets <- tempfile(c("one-", "ren-"))
  file.create(datasets)
  withr::defer(unlink(datasets))
  loaded <- NULL
  testthat::local_mocked_bindings(
    load_all = function(path, ...) loaded <<- path,
    .package = "pkgload"
  )
  ports <- 0L
  testthat::local_mocked_bindings(
    randomPort = function(...) {
      ports <<- ports + 1L
      if (length(list(...))) {
        stop("port in use")
      }
      18765L
    },
    .package = "httpuv"
  )
  launched <- NULL
  testthat::local_mocked_bindings(
    runApp = function(appDir, host, port, launch.browser) {
      launched <<- list(
        appDir = appDir,
        host = host,
        port = port,
        browser = launch.browser
      )
    },
    .package = "shiny"
  )
  for (ren_only in c(FALSE, TRUE)) {
    prepared <- character()
    packs <- character()
    scope <- new.env(parent = globalenv())
    scope$commandArgs <- function(trailingOnly) {
      if (!trailingOnly) {
        paste0("--file=", normalizePath(launcher))
      } else {
        c("--no-browser", if (ren_only) "--ren-only")
      }
    }
    scope$source <- function(file, local) {
      scope$.viewerRenVersion <- 2L
      scope$prepareViewer1mBenchmarkData <- function() {
        prepared <<- c(prepared, "one")
        datasets[[1]]
      }
      scope$prepareViewerRenDemoData <- function() {
        prepared <<- c(prepared, "ren")
        datasets[[2]]
      }
      ## The launcher builds a Viewer Pack for each demo data set it prepares.
      scope$prepareDemoViewerPack <- function(crb) {
        packs <<- c(packs, basename(crb))
        invisible(crb)
      }
    }
    sys.source(launcher, scope)
    expect_identical(loaded, root)
    expect_identical(prepared, if (ren_only) "ren" else c("one", "ren"))
    expect_identical(
      packs,
      basename(if (ren_only) datasets[[2]] else datasets)
    )
    expect_identical(
      Sys.getenv("CEREBRO_REN_DEMO_CRB"),
      normalizePath(datasets[[2]])
    )
    expect_identical(
      Sys.getenv("CEREBRO_1M_DEMO_CRB"),
      if (ren_only) "" else normalizePath(datasets[[1]])
    )
    expect_identical(
      launched,
      list(
        appDir = file.path(root, "inst"),
        host = "127.0.0.1",
        port = 18765L,
        browser = FALSE
      )
    )
  }
  expect_identical(ports, 4L)
})

test_that("demo preparation reuses valid packs and rebuilds missing or stale first frames", {
  helper <- testthat::test_path("..", "bench", "prepare_viewer_pack.R")
  skip_if_not(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, env)
  result <- list(valid = FALSE)
  builds <- 0L
  testthat::local_mocked_bindings(
    validateViewerPack = function(file, full) {
      expect_true(full)
      result
    },
    buildViewerPack = function(file, viewer_binary, overwrite) {
      expect_identical(file, "fixture.crb")
      expect_identical(viewer_binary, "always")
      expect_true(overwrite)
      builds <<- builds + 1L
      "fixture.viewer"
    },
    .package = "CerebroNexus"
  )
  expect_message(env$prepareDemoViewerPack("fixture.crb"), "Preparing Viewer Pack")
  expect_identical(builds, 1L)
  result <- list(valid = TRUE, path = "fixture.viewer", manifest = list(
    hla_chains = c("TRA", "TRB"),
    assets = data.frame(path = c("hla_tcr/TRA.first.qs2", "hla_tcr/TRB.first.qs2"))
  ))
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 1L)
  result$manifest$assets <- result$manifest$assets[1L, , drop = FALSE]
  expect_message(env$prepareDemoViewerPack("fixture.crb"), "Preparing Viewer Pack")
  expect_identical(builds, 2L)
  result$manifest$hla_chains <- character()
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 2L)
})
