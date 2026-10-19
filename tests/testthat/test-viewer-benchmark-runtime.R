viewer_benchmark_scope <- function() {
  file <- test_path("..", "bench", "benchmark_viewer_1m_pages.R")
  skip_if_not(file.exists(file), "benchmark tree not present")
  scope <- new.env(parent = globalenv())
  sys.source(test_path("..", "bench", "viewer_1m_page_protocol.R"), scope)
  sys.source(test_path("..", "bench", "lib", "viewer_launch.R"), scope)
  for (expression in parse(file)) {
    if (
      is.call(expression) &&
        as.character(expression[[1]])[1] %in% c("<-", "=") &&
        is.call(expression[[3]]) &&
        identical(expression[[3]][[1]], as.name("function"))
    ) {
      eval(expression, scope)
    }
  }
  scope
}

test_that("page benchmarks reject stale readiness and count actual websocket bytes", {
  skip_if_no_chrome()
  scope <- viewer_benchmark_scope()
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$close(), add = TRUE)
  js <- function(code) {
    result <- browser$Runtime$evaluate(code, returnByValue = TRUE)
    if (!is.null(result$exceptionDetails)) {
      stop(result$exceptionDetails$exception$description)
    }
    result$result$value
  }
  app <- list(run_js = js, get_js = js)
  js(
    "document.body.innerHTML='<button id=go>Open</button><div id=shiny-tab-overview class=active><div id=host><canvas data-point-count=2></canvas></div></div>';window.__cerebroPageBenchExpectedFingerprint='fp';window.emitReady=detail=>window.dispatchEvent(new CustomEvent('cerebro:specialist-state',{detail}));"
  )
  page <- scope$canvas_page(
    "overview",
    "#host",
    "overview_projection",
    expected_points = 2
  )
  scope$arm_and_click_page(app, page, "#go")
  expect_false(js(scope$page_active_js(page)))
  for (detail in list(
    list(
      viewId = "overview_projection",
      benchmarkGeneration = 0,
      datasetFingerprint = "fp",
      renderedPointCount = 2
    ),
    list(
      viewId = "overview_projection",
      benchmarkGeneration = 1,
      datasetFingerprint = "old",
      renderedPointCount = 2
    ),
    list(
      viewId = "overview_projection",
      benchmarkGeneration = 1,
      datasetFingerprint = "fp",
      renderedPointCount = 1
    )
  )) {
    js(sprintf("emitReady(%s)", jsonlite::toJSON(detail, auto_unbox = TRUE)))
    expect_false(js(scope$page_active_js(page)))
  }
  js(
    "emitReady({viewId:'overview_projection',benchmarkGeneration:1,datasetFingerprint:'fp',renderedPointCount:2})"
  )
  expect_true(js(scope$page_active_js(page)))
  expect_true(js(page$correctness))
  js("document.querySelector('canvas').dataset.pointCount='3'")
  expect_false(js(page$correctness))
  scope$arm_and_click_page(app, page, "#go")
  js(
    "emitReady({viewId:'overview_projection',benchmarkGeneration:1,datasetFingerprint:'fp',renderedPointCount:2})"
  )
  expect_false(js(scope$page_active_js(page)))

  js(
    "window.socket=new EventTarget();socket.sent=[];socket.send=function(data){this.sent.push(data)};window.originalSend=socket.send;window.Shiny={shinyapp:{$socket:socket}};"
  )
  scope$skip_socket_meter <- FALSE
  scope$start_socket_meter(app)
  expect_error(scope$start_socket_meter(app), "already active")
  js(
    "socket.send('汉字');socket.send(new Uint8Array([1,2,3]));socket.dispatchEvent(new MessageEvent('message',{data:'é'}));socket.dispatchEvent(new MessageEvent('message',{data:new Blob(['abcd'])}));"
  )
  expect_equal(scope$stop_socket_meter(app), list(sent = 9, received = 6))
  expect_true(js("socket.send===originalSend && socket.sent.length===2"))
  expect_error(scope$stop_socket_meter(app), "not active")
  scope$start_socket_meter(app)
  scope$cleanup_socket_meter(app)
  expect_true(js(
    "socket.send===originalSend && window.__cerebroPageBenchSocketMeter===null"
  ))
  scope$skip_socket_meter <- TRUE
  scope$start_socket_meter(app)
  expect_equal(scope$stop_socket_meter(app), list(sent = 0, received = 0))
})

test_that("benchmark browser retries clean up failures and preserve real errors", {
  scope <- viewer_benchmark_scope()
  scope$browser_start_attempts <- 3L
  starts <- stops <- 0L
  error_message <- "Chrome debugging port not open after 10 seconds"
  ready <- list(ready = TRUE)
  testthat::local_mocked_bindings(
    AppDriver = list(new = function(...) {
      starts <<- starts + 1L
      if (starts == 1L) {
        error <- simpleError(error_message)
        error$app <- list(stop = function() stops <<- stops + 1L)
        stop(error)
      }
      ready
    }),
    .package = "shinytest2"
  )
  result <- scope$start_app_driver("app", "test")
  expect_identical(result$app, ready)
  expect_equal(result$attempts, 2)
  expect_equal(stops, 1)
  expect_length(result$errors, 1)
  starts <- stops <- 0L
  error_message <- "application failed to initialize"
  expect_error(
    scope$start_app_driver("app", "test"),
    error_message,
    fixed = TRUE
  )
  expect_equal(starts, 1)
  expect_equal(stops, 1)
})

test_that("page benchmarks launch installed candidates by default", {
  scope <- viewer_benchmark_scope()
  candidate <- c(label = "current", root = "/src/cerebro", library = "/tmp/lib")
  installed <- scope$viewer_app_lines(candidate, "/data/1m.crb", "installed")
  expect_identical(
    installed[[1L]],
    'library(CerebroNexus, lib.loc = "/tmp/lib")'
  )
  expect_false(any(grepl("load_all", installed, fixed = TRUE)))
  expect_true(any(grepl(
    '"1M pages" = "/data/1m.crb"',
    installed,
    fixed = TRUE
  )))
  source_tree <- scope$viewer_app_lines(candidate, "/data/1m.crb", "load_all")
  expect_identical(
    source_tree[[1L]],
    'devtools::load_all("/src/cerebro", quiet = TRUE)'
  )
  expect_error(scope$validate_viewer_load_mode("source"), "VIEWER_BENCH_LOAD")
})

test_that("page benchmarks read the Data info cell count inside the overview", {
  skip_if_no_chrome()
  scope <- viewer_benchmark_scope()
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$close(), add = TRUE)
  js <- function(code) {
    browser$Runtime$evaluate(code, returnByValue = TRUE)$result$value
  }
  expect_null(js(scope$data_info_cells_js()))
  js(paste0(
    "document.body.innerHTML='<div id=load_data_overview><div class=dataset-stat ",
    "id=load_data_number_of_cells><strong>1,000,000</strong> <span>Cells</span>",
    "</div></div>'"
  ))
  expect_match(js(scope$data_info_cells_js()), "1,000,000", fixed = TRUE)
})

test_that("browser benchmarks share installed launches and Data info waits", {
  scope <- viewer_benchmark_scope()
  candidate <- c(label = "a b", root = "/src/a", library = "/lib/a")
  expect_identical(
    scope$viewer_load_line(candidate, "installed"),
    'library(CerebroNexus, lib.loc = "/lib/a")'
  )
  expect_identical(
    scope$viewer_load_line(candidate, "load_all"),
    'devtools::load_all("/src/a", quiet = TRUE)'
  )
  ## Source-tree mode installs nothing.
  candidates <- list(a = c(label = "a", root = "/src/a"))
  expect_identical(
    scope$prepare_viewer_candidates(candidates, "load_all"),
    candidates
  )
  waited <- character()
  app <- list(
    wait_for_js = function(script, timeout) waited <<- c(waited, script),
    get_js = function(script) "1,000,000 Cells"
  )
  expect_identical(
    scope$wait_for_data_info_cells(app, "1,000,000"),
    "1,000,000 Cells"
  )
  expect_length(waited, 1L)
  expect_error(scope$wait_for_data_info_cells(app, "2,000,000"), "2,000,000")
  expect_identical(scope$wait_for_data_info_cells(app), "1,000,000 Cells")
})
