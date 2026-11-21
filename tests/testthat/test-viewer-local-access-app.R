skip_on_cran()

test_that("a public Viewer shell cannot start unauthorized WebSocket sessions", {
  app <- viewer_local_access_process()
  expect_identical(app$initial$status_code, 200L)
  expect_match(app$token, "^[0-9a-f]{64}$")
  wrong <- paste0("?token=", strrep("0", 64))
  expect_identical(
    viewer_local_access_get(paste0(app$address, wrong))$status_code,
    200L
  )
  expect_identical(
    viewer_local_access_get(paste0(app$address, "?token=%00"))$status_code,
    200L
  )
  expect_identical(
    viewer_local_access_get(
      app$address,
      httr::add_headers(Host = "attacker.example")
    )$status_code,
    200L
  )
  authorized <- viewer_local_access_get(paste0(
    app$address,
    "?token=",
    app$token
  ))
  expect_identical(authorized$status_code, 200L)
  expect_identical(authorized$headers[["referrer-policy"]], "no-referrer")
  for (query in c("", wrong, "?token=%00")) {
    probe <- viewer_local_access_socket(app, query)
    expect_true(probe$opened)
    expect_false(identical(probe$origin, sub("/$", "", app$address)))
    expect_true(probe$closed)
    expect_null(probe$values)
    expect_length(probe$errors, 0L)
  }
  for (attempt in seq_len(2)) {
    probe <- viewer_local_access_socket(app, paste0("?token=", app$token))
    expect_false(probe$closed)
    expect_identical(probe$values$pong, "pong:socket")
  }
})

test_that("closed and explicitly public Viewers retain tokenless access", {
  for (settings in list(list(mode = "closed"), list(require_token = FALSE))) {
    app <- do.call(viewer_local_access_process, settings)
    expect_identical(app$initial$status_code, 200L)
    expect_null(app$token)
    probe <- viewer_local_access_socket(app, "")
    expect_false(probe$closed)
    expect_identical(probe$values$pong, "pong:socket")
  }
})

test_that("a headless browser can use the authorized Viewer interactively", {
  skip_if_no_chrome()
  app <- viewer_local_access_process()
  browser <- chromote::ChromoteSession$new()
  withr::defer(browser$close())
  browser$Page$navigate(paste0(app$address, "?token=", app$token))
  ready <- browser$Runtime$evaluate(
    expression = paste0(
      "new Promise(resolve => { const deadline = Date.now() + 20000; ",
      "const check = () => { const pong = document.querySelector('#pong'); ",
      "if (pong && pong.textContent === 'pong:') return resolve(true); ",
      "if (Date.now() > deadline) return resolve(false); ",
      "setTimeout(check, 50); }; check(); })"
    ),
    awaitPromise = TRUE,
    returnByValue = TRUE
  )
  expect_true(ready$result$value)
  result <- browser$Runtime$evaluate(
    expression = paste0(
      "Shiny.setInputValue('ping', 'browser'); ",
      "new Promise(resolve => { const deadline = Date.now() + 10000; ",
      "const check = () => { ",
      "if (document.querySelector('#pong').textContent === 'pong:browser') ",
      "return resolve(true); if (Date.now() > deadline) return resolve(false); ",
      "setTimeout(check, 50); }; check(); })"
    ),
    awaitPromise = TRUE,
    returnByValue = TRUE
  )
  expect_true(result$result$value)
})

test_that("cerebroStart protects the real open-mode Viewer and its upload UI", {
  skip_if_no_chrome()
  app <- viewer_local_access_process(start = TRUE, cached = TRUE)
  expect_identical(app$initial$status_code, 200L)
  expect_match(app$token, "^[0-9a-f]{64}$")
  expect_match(app$log, "EARLY_CACHE_RUNNING:TRUE", fixed = TRUE)
  browser <- chromote::ChromoteSession$new()
  withr::defer(browser$close())
  browser$Page$navigate(paste0(app$address, "?token=", app$token))
  ready_expression <- paste0(
    "new Promise(resolve => { const deadline = Date.now() + 30000; ",
    "const check = () => { ",
    "if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && ",
    "Shiny.shinyapp.$socket.readyState === 1) { ",
    "const link = document.querySelector('[data-value=\"loadData\"]'); ",
    "if (link) link.click(); ",
    "if (document.querySelector('#input_file')) return resolve(true); } ",
    "if (Date.now() > deadline) return resolve(false); ",
    "setTimeout(check, 100); }; check(); })"
  )
  ready <- browser$Runtime$evaluate(
    expression = ready_expression,
    awaitPromise = TRUE,
    returnByValue = TRUE
  )
  expect_true(ready$result$value)
  retained <- browser$Runtime$evaluate(
    "new URL(location.href).searchParams.get('token')",
    returnByValue = TRUE
  )
  expect_identical(retained$result$value, app$token)
  loaded <- browser$Page$loadEventFired(wait_ = FALSE)
  browser$Page$reload()
  browser$wait_for(loaded)
  ready <- browser$Runtime$evaluate(
    expression = ready_expression,
    awaitPromise = TRUE,
    returnByValue = TRUE
  )
  expect_true(ready$result$value)
  # The bare URL still serves only a shell; no session is granted.
  expect_identical(viewer_local_access_get(app$address)$status_code, 200L)
  expect_true(viewer_local_access_socket(app, "")$closed)
})

test_that("a bare Run App URL explains how to open the authorized Viewer", {
  skip_if_no_chrome()
  app <- viewer_local_access_process(start = TRUE, cached = TRUE)
  browser <- chromote::ChromoteSession$new()
  withr::defer(browser$close())
  browser$Page$navigate(app$address)
  message <- browser$Runtime$evaluate(
    paste0(
      "new Promise(resolve => { const deadline = Date.now() + 6000; ",
      "const check = () => { const node = document.getElementById('cerebro-access-denied'); ",
      "if (node) return resolve(node.textContent); ",
      "if (Date.now() > deadline) return resolve(''); ",
      "setTimeout(check, 50); }; check(); })"
    ),
    awaitPromise = TRUE,
    returnByValue = TRUE
  )$result$value
  expect_match(message, "Access token required", fixed = TRUE)
  expect_match(message, "R console", fixed = TRUE)
  expect_false(grepl(app$token, message, fixed = TRUE))
})
