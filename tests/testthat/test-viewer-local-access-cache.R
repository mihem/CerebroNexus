test_that("the public cached shell is served off-thread at the original root", {
  skip_if_not_installed("httr")
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "kept_document.R"), env)
  # This request deliberately runs synchronously on R's main thread. A page
  # requiring an R callback would time out rather than return these bytes.
  directory <- withr::local_tempdir()
  html <- charToRaw(
    "<!DOCTYPE html><html><body>generic cached shell</body></html>"
  )
  writeBin(html, file.path(directory, "index.html"))
  port <- httpuv::randomPort()
  server <- httpuv::startServer(
    "127.0.0.1",
    port,
    list(
      call = function(req) {
        list(status = 403L, headers = list(), body = "denied")
      },
      onWSOpen = function(ws) ws$close()
    )
  )
  withr::defer(server$stop())
  env$viewerDocumentBundleDirectory <- withr::local_tempdir
  env$viewerDocumentServe(server, directory)
  root <- server$getStaticPaths()[["/"]]
  expect_true(root$options$indexhtml)
  expect_true(file.exists(file.path(root$path, "index.html")))
  expect_identical(
    list.files(root$path),
    "index.html"
  )
  response <- viewer_local_access_get(
    sprintf("http://127.0.0.1:%d/", port),
    httr::timeout(2)
  )
  expect_identical(response$status_code, 200L)
  expect_identical(response$content, html)
  expect_identical(response$headers[["referrer-policy"]], "no-referrer")
  expect_identical(response$headers[["cache-control"]], "no-cache")
})

test_that("protected first-view URLs and temporary files do not use public aliases", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "shared_resources.R"), env)
  public <- env$viewerFirstViewDirectory()
  token <- strrep("cd", 32)
  env$.viewer_shared_resources$access_token <- token
  private <- env$viewerFirstViewDirectory()
  expect_false(identical(public, private))
  expect_identical(dirname(public), dirname(private))
  crb <- withr::local_tempfile(fileext = ".crb")
  writeBin(raw(8), crb)
  prefixes <- character()
  env$viewerFirstViewPublishKept(
    list(crb_file_to_load = crb, cerebro_version = "8.4.5"),
    register = function(...) invisible(NULL),
    serve = function(prefix, directory) prefixes <<- c(prefixes, prefix)
  )
  expect_identical(prefixes, paste0("cerebro-first-view-", token))
})

test_that("the preview client uses authorized JSON and binary URLs", {
  expect_node_fixture(
    "viewer-local-access-preview.cjs",
    viewer_test_path("www", "first-view.js"),
    pass = "PASS protected preview URLs"
  )
})

test_that("the query token permits uploads, downloads and reloads with the public cached page", {
  skip_on_cran()
  skip_if_no_chrome()
  app <- viewer_local_access_process(cached = TRUE)
  expect_identical(app$initial$status_code, 200L)
  expect_match(
    app$authorized_url,
    paste0("?token=", app$token),
    fixed = TRUE
  )
  browser <- chromote::ChromoteSession$new()
  withr::defer(browser$close())
  browser$Page$navigate(app$authorized_url)
  ready <- paste0(
    "new Promise(resolve => { const deadline = Date.now() + 15000; ",
    "const check = () => { ",
    "const pong = document.querySelector('#pong'); ",
    "const download = document.querySelector('#download'); ",
    "if (pong && pong.textContent === 'pong:' && download && ",
    "download.getAttribute('href')) return resolve(true); ",
    "if (Date.now() > deadline) return resolve(false); ",
    "setTimeout(check, 50); }; check(); })"
  )
  expect_true(
    browser$Runtime$evaluate(
      ready,
      awaitPromise = TRUE,
      returnByValue = TRUE
    )$result$value
  )
  download <- browser$Runtime$evaluate(
    "new URL(document.querySelector('#download').getAttribute('href'), location.href).href",
    returnByValue = TRUE
  )$result$value
  expect_identical(
    trimws(rawToChar(viewer_local_access_get(download)$content)),
    "authorized download"
  )
  upload <- withr::local_tempfile(fileext = ".txt")
  writeLines("trusted test file", upload)
  document <- browser$DOM$getDocument()
  input <- browser$DOM$querySelector(document$root$nodeId, "#upload")
  browser$DOM$setFileInputFiles(files = list(upload), nodeId = input$nodeId)
  uploaded <- browser$Runtime$evaluate(
    paste0(
      "new Promise(resolve => { const deadline = Date.now() + 10000; ",
      "const check = () => { ",
      "if (document.querySelector('#uploaded').textContent.length > 0) ",
      "return resolve(document.querySelector('#uploaded').textContent); ",
      "if (Date.now() > deadline) return resolve(''); ",
      "setTimeout(check, 50); }; check(); })"
    ),
    awaitPromise = TRUE,
    returnByValue = TRUE
  )$result$value
  expect_identical(uploaded, basename(upload))
  loaded <- browser$Page$loadEventFired(wait_ = FALSE)
  browser$Page$reload()
  browser$wait_for(loaded)
  expect_true(
    browser$Runtime$evaluate(
      ready,
      awaitPromise = TRUE,
      returnByValue = TRUE
    )$result$value
  )
  expect_identical(viewer_local_access_get(app$address)$status_code, 200L)
  expect_true(viewer_local_access_socket(app, "")$closed)
})
