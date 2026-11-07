builder_open_app_env <- function() {
  env <- new.env(parent = environment())
  sys.source(builder_test_inst_path("builder", "ui", "inspect_stage.R"), env)
  sys.source(builder_test_inst_path("builder", "ui", "review_stage.R"), env)
  sys.source(builder_test_inst_path("builder", "ui", "build_status.R"), env)
  env
}

test_that("Review maps the host localhost to 127.0.0.1", {
  env <- builder_open_app_env()
  expect_identical(
    env$builder_review_options(host = "localhost")$host,
    "127.0.0.1"
  )
  expect_identical(
    env$builder_review_options(host = "LocalHost")$host,
    "127.0.0.1"
  )
  expect_identical(env$builder_review_options(host = "0.0.0.0")$host, "0.0.0.0")
})

test_that("Open App starts an App built with host localhost", {
  skip_if_not_installed("httpuv")
  env <- builder_open_app_env()
  app_dir <- withr::local_tempdir()
  saveRDS(
    list(
      .bundle_run_options = list(
        shiny_app_options = list(host = "localhost", port = 8155L)
      )
    ),
    file.path(app_dir, "cerebro_config.rds")
  )
  options <- env$builder_app_run_options(app_dir)
  expect_identical(options$host, "127.0.0.1")
  port <- env$builder_app_free_port(options$host, options$port)
  expect_true(is.integer(port) && port >= 1L)
})

test_that("probing a busy port prints nothing", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")
  port <- httpuv::randomPort()
  server <- httpuv::startServer("127.0.0.1", port, list())
  withr::defer(httpuv::stopServer(server))
  # httpuv prints from its own thread, so only a child process's stderr
  # shows the message.
  stderr <- withr::local_tempfile()
  free <- callr::r(
    function(path, port) {
      env <- new.env()
      sys.source(path, env)
      env$builder_port_is_free("127.0.0.1", port)
    },
    args = list(
      path = builder_test_inst_path("builder", "ui", "build_status.R"),
      port = port
    ),
    stderr = stderr
  )
  expect_false(free)
  expect_false(any(grepl("createTcpServer", readLines(stderr))))
})
