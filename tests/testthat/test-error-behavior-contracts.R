test_that("launchers do not replace the process-wide Shiny error handler", {
  handler <- function() "user handler"
  withr::local_options(list(
    shiny.error = handler,
    shiny.maxRequestSize = getOption("shiny.maxRequestSize")
  ))
  rlang::local_bindings(Cerebro.options = NULL, .env = .GlobalEnv)
  withr::local_envvar(c(
    CEREBRO_1M_DEMO_CRB = NA,
    CEREBRO_REN_DEMO_CRB = NA,
    CEREBRO_COSMX_DEMO_CRB = NA,
    CEREBRO_TCELL_DEMO_CRB = NA
  ))
  for (launcher in c("package", "demo")) {
    scope <- new.env(parent = environment(launchCerebro))
    scope$ui <- shiny::div("test")
    scope$server <- function(input, output, session) NULL
    scope$source <- function(file, local, ...) {
      ## The helpers app.R sources besides the UI and server, which are stubs.
      if (basename(file) %in% c("million_cell_demo.R", "transport_compat.R")) {
        sys.source(viewer_test_path(basename(file)), scope)
      }
      invisible(NULL)
    }
    if (launcher == "package") {
      launch <- launchCerebro
      environment(launch) <- scope
      app <- launch()
    } else {
      app <- source(
        file.path(viewer_app_test_path(), "app.R"),
        local = scope
      )$value
    }
    expect_s3_class(app, "shiny.appobj")
    expect_identical(getOption("shiny.error"), handler)
  }
})

test_that("server startup and dataset loading fail with the original error", {
  original <- structure(
    list(message = "module failure", call = NULL, detail = "keep me"),
    class = c("fixture_failure", "error", "condition")
  )
  scope <- new.env(parent = globalenv())
  scope$Cerebro.options <- list(cerebro_root = viewer_app_test_path())
  scope$viewerSource <- function(...) stop(original)
  eval(viewer_r_assignment(viewer_test_path("shiny_server.R"), "server"), scope)
  observed <- tryCatch(scope$server(list(), list(), list()), error = identity)
  expect_identical(observed, original)
  # Dataset-read failures and absence of a success acknowledgement are exercised
  # by test-dataset-loading-progress.R through the real server reactive.
})
