## In-place feedback (notify.R) replaces Shiny corner notifications.

builder_notify_test_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(builder_test_inst_path("builder", "notify.R"), envir = env)
  env
}

builder_notify_test_session <- function() {
  sent <- new.env(parent = emptyenv())
  sent$messages <- list()
  list(
    sent = sent,
    session = list(sendCustomMessage = function(type, message) {
      sent$messages[[length(sent$messages) + 1L]] <- list(
        type = type,
        message = message
      )
    })
  )
}

test_that("builder_notify sends a targeted message with severity defaults", {
  env <- builder_notify_test_env()
  mock <- builder_notify_test_session()

  env$builder_notify(
    "error",
    "The server folder does not exist.",
    target = list(field = "builder_project_server_folder"),
    session = mock$session
  )
  env$builder_notify(
    "success",
    "Project saved.",
    target = "status",
    session = mock$session
  )
  env$builder_notify("info", "Removed A.", session = mock$session)
  env$builder_notify(
    "info",
    "Reading sheets",
    duration = Inf,
    id = "sheets",
    session = mock$session
  )
  env$builder_notify_clear("sheets", session = mock$session)

  messages <- lapply(mock$sent$messages, `[[`, "message")
  expect_true(all(vapply(
    mock$sent$messages,
    function(item) identical(item$type, "builder_notify"),
    logical(1)
  )))
  expect_identical(messages[[1]]$type, "error")
  expect_identical(messages[[1]]$target$field, "builder_project_server_folder")
  # Errors and warnings stay until dismissed; info and success hide.
  expect_null(messages[[1]]$hide_after)
  expect_identical(messages[[2]]$hide_after, 4)
  expect_identical(messages[[3]]$hide_after, 6)
  expect_null(messages[[3]]$target)
  expect_null(messages[[4]]$hide_after)
  expect_identical(messages[[4]]$id, "sheets")
  expect_identical(messages[[5]]$clear, "sheets")
})

test_that("builder_notify escapes text and renders tags", {
  env <- builder_notify_test_env()
  expect_identical(
    env$builder_notify_html("Folder <a> & b"),
    "Folder &lt;a&gt; &amp; b"
  )
  html <- env$builder_notify_html(htmltools::tagList(
    "Removed A. ",
    htmltools::tags$a(id = "undo_remove_notice", "Undo")
  ))
  expect_match(html, 'id="undo_remove_notice"', fixed = TRUE)
})

test_that("the Builder no longer calls showNotification directly", {
  files <- list.files(
    builder_test_inst_path("builder"),
    pattern = "[.]R$",
    recursive = TRUE,
    full.names = TRUE
  )
  hits <- unlist(lapply(files, function(path) {
    lines <- readLines(path, warn = FALSE)
    found <- grep("(show|remove)Notification\\(", lines)
    if (length(found)) paste0(basename(path), ":", found) else character()
  }))
  expect_identical(hits, character())

  browser <- paste(
    readLines(
      builder_test_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    browser,
    'addCustomMessageHandler("builder_notify"',
    fixed = TRUE
  )
  # The save confirmation no longer builds its own corner toast panel.
  expect_false(grepl('.id = "shiny-notification-panel"', browser, fixed = TRUE))
})
