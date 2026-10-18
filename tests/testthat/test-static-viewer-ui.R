test_that("static UI retains dependencies, head content and singleton identity", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("static_ui.R"), envir = scope)
  dependency <- htmltools::htmlDependency(
    "viewer-test",
    "1.0",
    src = c(href = "assets"),
    script = "test.js"
  )
  once <- htmltools::singleton(htmltools::tags$head(htmltools::tags$script(
    "once()"
  )))
  ui <- htmltools::tags$body(
    lang = "en",
    once,
    htmltools::attachDependencies(
      htmltools::tags$section(
        htmltools::tags$head(htmltools::tags$style(".plot { color: red; }")),
        htmltools::tags$div(id = "value", "<escaped> & text")
      ),
      dependency
    ),
    once
  )
  cached <- scope$viewerStaticUI(ui)
  before <- htmltools::renderTags(ui)
  after <- htmltools::renderTags(cached)
  expect_identical(cached$attribs, ui$attribs)
  expect_identical(after$singletons, before$singletons)
  expect_identical(after$dependencies, before$dependencies)
  normalize <- function(x) gsub("[[:space:]]+", "", as.character(x))
  expect_identical(normalize(after$html), normalize(before$html))
  expect_identical(normalize(after$head), normalize(before$head))
})

test_that("static UI does not evaluate request-time tag functions at startup", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("static_ui.R"), envir = scope)
  calls <- 0L
  ui <- htmltools::tags$body(
    htmltools::tags$div("static"),
    htmltools::tagFunction(function() {
      calls <<- calls + 1L
      htmltools::tags$span(paste("request", calls))
    })
  )
  cached <- scope$viewerStaticUI(ui)
  expect_identical(calls, 0L)
  expect_match(htmltools::renderTags(cached)$html, "request 1", fixed = TRUE)
  previous <- calls
  htmltools::renderTags(cached)
  expect_gt(calls, previous)
})
