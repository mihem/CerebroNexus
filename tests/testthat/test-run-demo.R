run_demo <- function(options, scope = new.env(parent = globalenv())) {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  testthat::skip_if_not(file.exists(launcher), "repository-only launcher")
  scope$commandArgs <- function(trailingOnly) {
    if (trailingOnly) options else paste0("--file=", normalizePath(launcher))
  }
  sys.source(launcher, scope)
}

test_that("run-demo prepares the data sets --data names and opens the Viewer", {
  keys <- c("1m", "ren", "cosmx", "tcell", "xenium-prime")
  variables <- paste0("CEREBRO_", toupper(gsub("-", "_", keys)), "_DEMO_CRB")
  withr::local_envvar(stats::setNames(rep(NA, length(keys)), variables))
  datasets <- stats::setNames(tempfile(paste0(keys, "-")), keys)
  file.create(datasets)
  withr::defer(unlink(datasets))
  root <- normalizePath(testthat::test_path("..", ".."))
  loaded <- NULL
  testthat::local_mocked_bindings(
    load_all = function(path, ...) loaded <<- path,
    .package = "pkgload"
  )
  ## The default port is busy: the launcher falls back to a random one.
  testthat::local_mocked_bindings(
    randomPort = function(...) {
      if (length(list(...))) stop("port in use") else 18765L
    },
    .package = "httpuv"
  )
  testthat::local_mocked_bindings(
    runApp = function(appDir, host, port, launch.browser) {
      launched <<- list(appDir, host, port, launch.browser)
    },
    .package = "shiny"
  )
  runs <- list(
    list(options = character(), expected = "1m"),
    list(options = "--data=cosmx", expected = "cosmx"),
    list(options = "--data=xenium-prime", expected = "xenium-prime"),
    list(options = "--data=tcell,ren,1m", expected = c("1m", "ren", "tcell"))
  )
  fake <- function(key) {
    function() {
      prepared <<- c(prepared, key)
      datasets[[key]]
    }
  }
  for (run in runs) {
    prepared <- packs <- character()
    launched <- NULL
    scope <- new.env(parent = globalenv())
    scope$source <- function(file, local) {
      scope$prepareViewer1mBenchmarkData <- fake("1m")
      scope$prepareViewerRenDemoData <- fake("ren")
      scope$prepareViewerCosMxDemoData <- fake("cosmx")
      scope$prepareViewerTcellDemoData <- fake("tcell")
      scope$prepareViewerXeniumPrimeDemoData <- fake("xenium-prime")
      ## A Viewer Pack for each prepared data set.
      scope$prepareDemoViewerPack <- function(crb) {
        packs <<- c(packs, basename(crb))
      }
    }
    run_demo(run$options, scope)
    label <- paste(c("run-demo", run$options), collapse = " ")
    expect_identical(loaded, root, label = label)
    expect_identical(prepared, run$expected, label = label)
    names <- basename(datasets[run$expected])
    expect_identical(packs, as.vector(names), label = label)
    expect_identical(
      unname(Sys.getenv(variables)),
      unname(ifelse(keys %in% run$expected, normalizePath(datasets), "")),
      label = label
    )
    expect_identical(
      launched,
      list(file.path(root, "inst"), "127.0.0.1", 18765L, TRUE),
      label = label
    )
  }
})

test_that("run-demo rejects unknown options and data sets", {
  for (options in list("--everything", "--data=mouse", "--data=")) {
    expect_error(
      run_demo(options),
      "Usage: Rscript run-demo.R [--data=1m,ren,cosmx,tcell,xenium-prime] [--prepare-only]",
      fixed = TRUE
    )
  }
})

test_that("demo preparation reuses valid packs and rebuilds missing or stale first frames", {
  helper <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_viewer_pack.R"
  )
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
  expect_message(
    env$prepareDemoViewerPack("fixture.crb"),
    "Preparing Viewer Pack"
  )
  expect_identical(builds, 1L)
  result <- list(
    valid = TRUE,
    path = "fixture.viewer",
    manifest = list(
      hla_chains = c("TRA", "TRB"),
      assets = data.frame(
        path = c("hla_tcr/TRA.first.v3.qs2", "hla_tcr/TRB.first.v3.qs2")
      )
    )
  )
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 1L)
  result$manifest$assets <- result$manifest$assets[1L, , drop = FALSE]
  expect_message(
    env$prepareDemoViewerPack("fixture.crb"),
    "Preparing Viewer Pack"
  )
  expect_identical(builds, 2L)
  result$manifest$hla_chains <- character()
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 2L)
})
