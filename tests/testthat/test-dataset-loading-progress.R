test_that("dataset loading reports progress and acknowledges the loaded dataset", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      sent <- list()
      session$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- list(type = type, message = message)
      }
      reads <- 0L
      dataset_load_requested <- reactiveVal(FALSE)
      context <- list(epoch = "session", dataset_key = "tiny", generation = 1L)
      ## The load boundary resolves the loader's label through the data set
      ## selector, so the harness has to provide its store.
      available_crb_files <- reactiveValues(
        files = c(tiny = "tiny.crb"),
        selected = "tiny",
        names = "tiny",
        labels = NULL
      )
      viewer_dataset_request <- reactive({
        list(target = input$path, dataset_context = context)
      })
      .crb_process_cache <- new.env()
      crb_prefetch_tasks <- list()
      get_or_load_crb <- function(...) {
        reads <<- reads + 1L
        if (identical(input$path, "broken.crb")) {
          stop("Unreadable dataset")
        }
        list(print = function() "tiny dataset", expression = matrix(1:6, 2))
      }
      viewerDatasetIdentity <- function() {
        data_set()
        list(
          cell_count = 3L,
          fingerprint = "tiny-values",
          order_fingerprint = "abc"
        )
      }
      viewer_server_load(
        viewer_server_statements("shiny_server.R", server = TRUE),
        scope,
        names = c("datasetLoadProgress", "viewer_dataset_snapshot", "data_set"),
        events = "cerebro_benchmark_dataset_load_request"
      )
    },
    {
      session$setInputs(path = "tiny.crb")
      expect_identical(scope$reads, 0L)
      expect_length(scope$sent, 0L)
      session$setInputs(cerebro_benchmark_dataset_load_request = "request-1")
      expect_identical(scope$reads, 1L)
      progress <- lapply(
        Filter(function(x) x$type == "cerebro_dataset_load", scope$sent),
        `[[`,
        "message"
      )
      expect_gt(length(progress), 1L)
      percentages <- vapply(progress, `[[`, numeric(1), "percent")
      expect_true(all(diff(percentages) >= 0))
      expect_true(all(percentages >= 0 & percentages <= 100))
      expect_lt(percentages[1], 100)
      expect_equal(tail(percentages, 1), 100)
      expect_true(all(vapply(
        progress,
        function(x) identical(x$label, "tiny") && nzchar(x$detail),
        logical(1)
      )))
      expect_false(any(vapply(head(progress, -1), `[[`, logical(1), "done")))
      expect_true(tail(progress, 1)[[1]]$done)
      expect_identical(
        tail(scope$sent, 1)[[1]],
        list(
          type = "cerebro_benchmark_dataset_ready",
          message = list(
            request = "request-1",
            cell_count = 3L,
            dataset_fingerprint = "tiny-values",
            order_fingerprint = "abc"
          )
        )
      )
      # A new acknowledgement uses the cached dataset, not a second disk read.
      session$setInputs(cerebro_benchmark_dataset_load_request = "request-2")
      expect_identical(scope$reads, 1L)
      expect_identical(tail(scope$sent, 1)[[1]]$message$request, "request-2")
      scope$sent <- list()
      session$setInputs(path = "broken.crb")
      ## A failed load is recoverable: the page reports a generic message and the
      ## selector can switch data sets, while the original error stays available
      ## on the snapshot instead of closing the session.
      expect_error(data_set(), "could not be loaded")
      expect_false(viewer_dataset_snapshot()$ok)
      expect_match(
        conditionMessage(viewer_dataset_snapshot()$error),
        "Unreadable dataset"
      )
      expect_false(any(vapply(
        scope$sent,
        function(x) isTRUE(x$message$done),
        logical(1)
      )))
      expect_false(any(vapply(
        scope$sent,
        function(x) x$type == "cerebro_benchmark_dataset_ready",
        logical(1)
      )))
    }
  )
})


test_that("loading labels match selector labels without changing dataset identity", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  files <- c(ds1 = "/private/one.crb", ds2 = "/private/two.crb")
  labels <- c(ds2 = "Tissue atlas", ds1 = "All nuclei")
  expect_identical(
    runtime$viewerDatasetLabel(files, "ds1", labels),
    "All nuclei"
  )
  expect_identical(
    runtime$viewerDatasetLabel(files, "ds2", labels),
    "Tissue atlas"
  )
  expect_identical(
    runtime$viewerDatasetLabel(files, "ds2", c("First", "Second")),
    "Second"
  )
  repeated <- c(ds1 = "Repeated", ds2 = "Repeated")
  expect_identical(
    runtime$viewerDatasetLabel(files, "ds2", repeated),
    "Repeated"
  )
  expect_identical(
    unname(runtime$viewerDatasetChoices(files, repeated)),
    c("ds1", "ds2")
  )
  expect_identical(runtime$viewerDatasetPath(files, "ds2"), "/private/two.crb")
  expect_identical(
    runtime$viewerDatasetLabel(c(Atlas = "/private/data.crb"), "Atlas"),
    "Atlas"
  )
  for (selected in list(NULL, NA_character_, "missing", "__upload_1__")) {
    expect_identical(
      runtime$viewerDatasetLabel(files, selected, labels),
      "Dataset"
    )
  }
  expect_identical(runtime$viewerDatasetLabel(NULL, "__single__"), "Dataset")
  expect_identical(
    runtime$viewerDatasetLabel(unname(files), files[[1]]),
    "Dataset"
  )
  expect_identical(
    runtime$viewerDatasetLabel(files, "ds1", c(" ", "Second")),
    "Dataset"
  )
})


test_that("loading transitions avoid flashes and survive consecutive switches", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_identical(
    system2(
      "node",
      c(
        shQuote(testthat::test_path(
          "fixtures",
          "dataset-loading-transition.js"
        )),
        shQuote(viewer_test_path("www", "viewer-shell.js"))
      )
    ),
    0L
  )
})

test_that("catalog Data info decides loading with the request and loads once", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      sys.source(viewer_test_path("core", "dataset_overview.R"), scope)
      session$sendCustomMessage <- function(type, message) NULL
      removeNotification <- function(...) NULL
      reads <- 0L
      Cerebro.options <- list(
        .dataset_catalog = list(
          a.crb = list(path = "a.crb", cells = 3),
          b.crb = list(path = "b.crb", cells = 4)
        )
      )
      .saved_overviews <- list()
      .cerebro_cell_view_aux_pending <- new.env()
      initial_tab <- NULL
      initial_page_applied <- reactiveVal(TRUE)
      dataset_load_requested <- reactiveVal(FALSE)
      viewer_dataset_request <- reactiveVal(NULL)
      viewer_dataset_epoch <- "session"
      viewer_dataset_generation <- 0
      available_crb_files <- reactiveValues(
        files = c(a = "a.crb", b = "b.crb"),
        selected = NULL,
        names = c("a", "b"),
        labels = NULL
      )
      .crb_process_cache <- new.env()
      crb_prefetch_tasks <- list()
      get_or_load_crb <- function(...) {
        reads <<- reads + 1L
        list(print = function() "", expression = NULL)
      }
      viewer_server_load(
        viewer_server_statements("shiny_server.R", server = TRUE),
        scope,
        names = c(
          "viewer_send_dataset_phase",
          "viewer_clear_cell_view_aux",
          "viewer_dataset_load_required",
          "viewer_request_dataset",
          "datasetLoadProgress",
          "viewer_dataset_snapshot"
        )
      )
      ## Mirrors the priority-1000 ready observer, which used to run before the
      ## loading decision and read the previous request's flag.
      observe(try(viewer_dataset_snapshot(), silent = TRUE), priority = 1000)
      observeEvent(input$pick, {
        viewer_request_dataset(
          input$pick,
          paste0(input$pick, ".crb"),
          input$pick,
          "configured"
        )
      })
    },
    {
      session$setInputs(sidebar = "loadData", pick = "a")
      expect_false(dataset_load_requested())
      expect_identical(scope$reads, 0L)
      session$setInputs(pick = "b")
      expect_identical(scope$reads, 0L)
      ## Opening an analysis page loads the current request exactly once.
      dataset_load_requested(TRUE)
      session$flushReact()
      expect_identical(scope$reads, 1L)
      session$setInputs(sidebar = "overview", pick = "a")
      expect_true(dataset_load_requested())
      expect_identical(scope$reads, 2L)
    }
  )
})

test_that("cell fingerprints are computed once per CRB file version", {
  scope <- new.env()
  viewer_server_load(
    viewer_server_statements("shiny_server.R"),
    scope,
    names = c(".viewer_cell_fingerprint_cache", ".viewerCellFingerprintCached")
  )
  path <- withr::local_tempfile(fileext = ".crb")
  writeLines("a", path)
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    list(fingerprint = calls)
  }
  cached <- scope$.viewerCellFingerprintCached
  expect_identical(cached(path, compute)$fingerprint, 1L)
  expect_identical(cached(path, compute)$fingerprint, 1L)
  writeLines("a changed file", path)
  expect_identical(cached(path, compute)$fingerprint, 2L)
  expect_identical(cached(NULL, compute)$fingerprint, 3L)
})
