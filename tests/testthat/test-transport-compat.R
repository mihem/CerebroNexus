transport_compat_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("transport_compat.R"), envir = env)
  env
}

## A stand-in session: only the two methods the wrapper touches.
transport_compat_session <- function() {
  session <- new.env()
  session$sent <- list()
  session$sendBinaryMessage <- function(type, message) {
    stop("a raw binary frame reached the transport", call. = FALSE)
  }
  session$sendCustomMessage <- function(type, message) {
    session$sent[[length(session$sent) + 1L]] <- list(
      type = type,
      message = message
    )
    invisible(NULL)
  }
  session
}

test_that("auto mode follows the environment Shiny Server sets", {
  env <- transport_compat_env()
  withr::local_options(cerebro.sockjs_compat = NULL)

  withr::local_envvar(SHINY_SERVER_VERSION = NA)
  expect_false(env$viewerSockjsCompatEnabled(list()))
  withr::local_envvar(SHINY_SERVER_VERSION = "")
  expect_false(env$viewerSockjsCompatEnabled(list()))
  withr::local_envvar(SHINY_SERVER_VERSION = "1.5.22.1017")
  expect_true(env$viewerSockjsCompatEnabled(list()))
  expect_true(env$viewerSockjsCompatEnabled(list(sockjs_compat = "auto")))
})

test_that("an explicit setting wins over the environment", {
  env <- transport_compat_env()
  withr::local_options(cerebro.sockjs_compat = NULL)

  withr::local_envvar(SHINY_SERVER_VERSION = NA)
  expect_true(env$viewerSockjsCompatEnabled(list(sockjs_compat = TRUE)))
  withr::local_envvar(SHINY_SERVER_VERSION = "1.5.22.1017")
  expect_false(env$viewerSockjsCompatEnabled(list(sockjs_compat = FALSE)))

  ## The R option decides only when Cerebro.options says nothing.
  withr::local_options(cerebro.sockjs_compat = FALSE)
  expect_false(env$viewerSockjsCompatEnabled(list()))
  expect_true(env$viewerSockjsCompatEnabled(list(sockjs_compat = TRUE)))
})

test_that("an invalid setting is refused", {
  env <- transport_compat_env()
  for (setting in list("yes", NA, c(TRUE, FALSE), 1L)) {
    expect_error(
      env$viewerSockjsCompatEnabled(list(sockjs_compat = setting)),
      "must be TRUE, FALSE or"
    )
  }
})

test_that("a disabled wrapper returns the page and server untouched", {
  env <- transport_compat_env()
  ui <- shiny::tags$div(id = "page")
  server <- function(input, output, session) NULL

  result <- env$viewerApplyTransportCompat(
    ui,
    server,
    inst_test_path(),
    enabled = FALSE
  )
  expect_identical(result$ui, ui)
  expect_identical(result$server, server)
})

test_that("an enabled wrapper adds the browser handler to the page head", {
  env <- transport_compat_env()
  server <- function(input, output, session) NULL
  ## Shiny renders <head> content into the document head, not the body.
  head_of <- function(ui) {
    as.character(htmltools::renderTags(ui)$head)
  }
  body_of <- function(ui) {
    as.character(htmltools::renderTags(ui)$html)
  }

  page <- shiny::tags$div(id = "page")
  ui <- env$viewerApplyTransportCompat(page, server, inst_test_path(), TRUE)$ui
  expect_match(head_of(ui), "cerebro_sockjs_binary", fixed = TRUE)
  expect_match(body_of(ui), 'id="page"', fixed = TRUE)
  expect_no_match(body_of(ui), "cerebro_sockjs_binary", fixed = TRUE)

  ## A page built per request keeps its request argument.
  request_page <- function(request) shiny::tags$div(id = request$name)
  wrapped <- env$viewerApplyTransportCompat(
    request_page,
    server,
    inst_test_path(),
    TRUE
  )$ui
  expect_true(is.function(wrapped))
  ui <- wrapped(list(name = "per-request"))
  expect_match(head_of(ui), "cerebro_sockjs_binary", fixed = TRUE)
  expect_match(body_of(ui), 'id="per-request"', fixed = TRUE)
})

test_that("the wrapped server sends binary frames as base64 text", {
  env <- transport_compat_env()
  payload <- as.raw(0:255)
  inner <- function(input, output, session) {
    session$sendBinaryMessage("cell_view_binary", payload)
    "done"
  }
  wrapped <- env$viewerApplyTransportCompat(
    shiny::tags$div(),
    inner,
    inst_test_path(),
    TRUE
  )$server

  session <- transport_compat_session()
  expect_identical(wrapped(NULL, NULL, session), "done")
  expect_length(session$sent, 1L)
  expect_identical(session$sent[[1L]]$type, "cerebro_sockjs_binary")
  expect_identical(session$sent[[1L]]$message$type, "cell_view_binary")
  expect_identical(
    base64enc::base64decode(session$sent[[1L]]$message$data),
    payload
  )

  ## The frame is a type and bytes; anything else is a programming error.
  inner_bad <- function(input, output, session) {
    expect_error(session$sendBinaryMessage(c("a", "b"), payload))
    expect_error(session$sendBinaryMessage("a", "not raw"))
  }
  wrapped_bad <- env$viewerApplyTransportCompat(
    shiny::tags$div(),
    inner_bad,
    inst_test_path(),
    TRUE
  )$server
  wrapped_bad(NULL, NULL, transport_compat_session())
})

test_that("the wrapper puts a locked sendBinaryMessage binding back", {
  env <- transport_compat_env()
  wrapped <- env$viewerApplyTransportCompat(
    shiny::tags$div(),
    function(input, output, session) NULL,
    inst_test_path(),
    TRUE
  )$server

  session <- transport_compat_session()
  lockBinding("sendBinaryMessage", session)
  wrapped(NULL, NULL, session)
  expect_true(bindingIsLocked("sendBinaryMessage", session))
})

test_that("the R and browser halves agree on the message name", {
  js <- paste(
    readLines(viewer_test_path("transport_compat.js")),
    collapse = "\n"
  )
  r <- paste(readLines(viewer_test_path("transport_compat.R")), collapse = "\n")
  expect_match(
    js,
    "addCustomMessageHandler('cerebro_sockjs_binary'",
    fixed = TRUE
  )
  expect_match(r, '"cerebro_sockjs_binary"', fixed = TRUE)
})
