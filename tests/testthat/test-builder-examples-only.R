## Examples-only Builder (issue #272): a demonstration that loads only the
## bundled examples, builds them into a private folder and offers the result
## as a download. The page offers no upload, server folder or Project, and the
## server refuses them too (inst/builder/access.R).

access_env <- new.env(parent = globalenv())
builder_repo_source("access.R", local = access_env)

test_that("the examples-only option is read strictly", {
  mode <- access_env$builder_examples_only_mode
  expect_false(mode(FALSE))
  expect_true(mode(TRUE))
  withr::with_options(
    list(CerebroNexus.builder.examples_only = NULL),
    expect_false(mode())
  )
  for (value in list(NA, "yes", 1, c(TRUE, FALSE), logical())) {
    expect_error(mode(value), "must be TRUE or FALSE", fixed = TRUE)
  }
  ## Without app.R (as here) nothing turns the mode on.
  expect_false(access_env$builder_examples_only())
})

test_that("the launcher checks examples_only before anything else", {
  expect_error(
    CerebroNexus::launchCerebroBuilder(examples_only = NA),
    "'examples_only' must be TRUE or FALSE.",
    fixed = TRUE
  )
  expect_error(
    CerebroNexus::launchCerebroBuilder(examples_only = "yes"),
    "'examples_only' must be TRUE or FALSE.",
    fixed = TRUE
  )
})

test_that("a release folder is zipped with the folder at the top", {
  root <- withr::local_tempdir()
  release <- file.path(root, "CerebroNexus_crb_abc123")
  dir.create(file.path(release, "sub"), recursive = TRUE)
  writeLines("one", file.path(release, "one.crb"))
  writeLines("two", file.path(release, "sub", "two.txt"))
  runtime <- new.env(parent = globalenv())
  runtime$`%||%` <- function(a, b) if (is.null(a)) b else a
  sys.source(
    builder_test_inst_path("builder", "ui", "build_status.R"),
    envir = runtime
  )
  zip_with <- function(use_package) {
    zipfile <- file.path(root, paste0("out-", use_package, ".zip"))
    runtime$builder_zip_directory(
      release,
      zipfile,
      .use_zip_package = use_package
    )
    sort(utils::unzip(zipfile, list = TRUE)$Name)
  }
  expected <- c(
    "CerebroNexus_crb_abc123/one.crb",
    "CerebroNexus_crb_abc123/sub/two.txt"
  )
  if (requireNamespace("zip", quietly = TRUE)) {
    expect_true(all(expected %in% zip_with(TRUE)))
  }
  if (nzchar(Sys.which("zip"))) {
    expect_true(all(expected %in% zip_with(FALSE)))
  }
  expect_error(
    runtime$builder_zip_directory(file.path(root, "missing"), tempfile()),
    "does not exist",
    fixed = TRUE
  )
})

test_that("the examples-only session limits are read strictly", {
  limits <- access_env$builder_examples_only_limits
  withr::with_options(
    list(
      CerebroNexus.builder.examples_only_max_sessions = NULL,
      CerebroNexus.builder.examples_only_idle_minutes = NULL
    ),
    expect_identical(limits(), list(max_sessions = 4, idle_seconds = 900))
  )
  expect_identical(
    limits(Inf, Inf),
    list(max_sessions = Inf, idle_seconds = Inf)
  )
  expect_identical(limits(2L, 1.5)$idle_seconds, 90)
  for (value in list(0, 1.5, -1, NA, "4", c(1, 2), numeric())) {
    expect_error(limits(value, 15), "max_sessions must be", fixed = TRUE)
  }
  for (value in list(0.5, 0, -5, NA, "15", c(1, 2), numeric())) {
    expect_error(limits(4, value), "idle_minutes must be", fixed = TRUE)
  }
})

test_that("examples-only seats are counted and each is given back once", {
  seats <- access_env$builder_examples_only_seats()
  enter <- access_env$builder_examples_only_enter
  first <- enter(seats, 2)
  second <- enter(seats, 2)
  expect_true(is.function(first) && is.function(second))
  expect_null(enter(seats, 2))
  expect_identical(seats$count, 2L)
  first()
  first()
  expect_identical(seats$count, 1L)
  third <- enter(seats, 2)
  expect_true(is.function(third))
  expect_null(enter(seats, 2))
  second()
  third()
  expect_identical(seats$count, 0L)
  expect_true(is.function(enter(seats, Inf)))
})

test_that("a closed examples-only session is told why before it closes", {
  sent <- list()
  flushed <- list()
  closed <- FALSE
  session <- list(
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    },
    onFlushed = function(fun, once = FALSE) {
      expect_true(once)
      flushed[[length(flushed) + 1L]] <<- fun
    },
    close = function() closed <<- TRUE
  )
  access_env$builder_examples_only_close(session, "Busy.")
  expect_false(closed)
  expect_identical(sent[[1L]]$type, "builder_examples_only_closed")
  expect_identical(sent[[1L]]$message$text, "Busy.")
  flushed[[1L]]()
  expect_true(closed)
  expect_match(
    access_env$builder_examples_only_idle_message(900),
    "after 15 minutes without activity",
    fixed = TRUE
  )
})

test_that("an idle examples-only session is closed unless it is busy", {
  skip_if_not_installed("shiny")
  clock <- new.env(parent = emptyenv())
  clock$now <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  clock$busy <- FALSE
  closes <- 0L
  watch_env <- new.env(parent = access_env)
  watch_env$builder_examples_only_close <- function(session, text) {
    closes <<- closes + 1L
    expect_match(text, "after 1 minutes", fixed = TRUE)
    session$close()
  }
  watch <- access_env$builder_examples_only_watch_idle
  environment(watch) <- watch_env
  at <- function(seconds) {
    clock$now <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + seconds
  }
  shiny::testServer(
    function(input, output, session) {
      watch(
        session,
        input,
        idle_seconds = 60,
        busy = function() clock$busy,
        now = function() clock$now
      )
    },
    {
      session$elapse(30000)
      at(59)
      session$elapse(30000)
      expect_false(session$isClosed())
      ## A click restarts the count.
      session$setInputs(builder_examples_only_activity = 1)
      at(110)
      session$elapse(30000)
      expect_false(session$isClosed())
      ## A running build or import keeps the session open.
      clock$busy <- TRUE
      at(600)
      session$elapse(30000)
      expect_false(session$isClosed())
      clock$busy <- FALSE
      at(659)
      session$elapse(30000)
      expect_false(session$isClosed())
      at(660)
      session$elapse(30000)
      expect_true(session$isClosed())
      expect_identical(closes, 1L)
    }
  )
  ## Inf turns the watch off.
  expect_null(access_env$builder_examples_only_watch_idle(NULL, NULL, Inf))
})

## Slow from here: these source the whole app. CI only.
skip_on_cran()

builder_examples_only_app <- function(env = parent.frame()) {
  withr::local_options(
    CerebroNexus.builder.examples_only = TRUE,
    CerebroNexus.builder.project_worker_prewarm = FALSE,
    .local_envir = env
  )
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"), .local_envir = env)
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this examples-only test.")
  }
  app_env
}

test_that("the examples-only page offers the examples and nothing to upload", {
  skip_if_not_installed("shiny")
  app_env <- builder_examples_only_app()
  expect_true(app_env$builder_examples_only())
  page <- htmltools::renderTags(app_env$ui(list(QUERY_STRING = "")))
  html <- paste(page$head, page$html)
  expect_match(
    html,
    'name="cerebro-builder-mode" content="examples-only"',
    fixed = TRUE
  )
  expect_match(html, "example-btn", fixed = TRUE)
  expect_match(html, "Examples-only demo", fixed = TRUE)
  for (absent in c(
    'id="dataset_files"',
    "builder-add-datasets",
    'id="open_builder_project"',
    'id="save_builder_project"',
    "Drop files here"
  )) {
    expect_false(grepl(absent, html, fixed = TRUE), info = absent)
  }
  expect_error(
    app_env$builder_server_path_roots(),
    "Server folders are turned off",
    fixed = TRUE
  )
})

test_that("examples-only result, marker and Project controls stay off the server", {
  skip_if_not_installed("shiny")
  app_env <- builder_examples_only_app()
  release <- withr::local_tempdir()
  app_dir <- file.path(release, "cerebro_app")
  dir.create(app_dir)
  value <- app_env$builder_result_success(
    published = TRUE,
    app_dir = app_dir,
    app_verified = TRUE,
    report_path = file.path(release, "build-report.json"),
    release = list(target = release)
  )
  demo <- as.character(app_env$builder_build_status_ui(
    value,
    examples_only = TRUE
  ))
  expect_match(demo, "builder_examples_only_download", fixed = TRUE)
  expect_match(demo, "Download Viewer app (.zip)", fixed = TRUE)
  for (absent in c(
    "open_app",
    "reveal_folder",
    "copy_path",
    "copy_report",
    release
  )) {
    expect_false(grepl(absent, demo, fixed = TRUE), info = absent)
  }
  full <- as.character(app_env$builder_build_status_ui(value))
  expect_match(full, "open_app", fixed = TRUE)
  expect_false(grepl("builder_examples_only_download", full, fixed = TRUE))

  choice <- as.character(app_env$builder_marker_source_choice_ui(
    "enhance",
    examples_only = TRUE
  ))
  expect_match(choice, "enhance-marker_genes_calculate", fixed = TRUE)
  expect_false(grepl("enhance-marker_genes_upload", choice, fixed = TRUE))
  expect_match(
    as.character(app_env$builder_marker_source_choice_ui("enhance")),
    "enhance-marker_genes_upload",
    fixed = TRUE
  )

  toolbar <- as.character(app_env$builder_project_toolbar_ui(
    examples_only = TRUE
  ))
  expect_false(grepl("open_builder_project", toolbar, fixed = TRUE))
  expect_false(grepl("save_builder_project", toolbar, fixed = TRUE))
})

test_that("the examples-only server refuses files, folders, Projects and Apps", {
  skip_if_not_installed("shiny")
  app_env <- builder_examples_only_app()
  opened <- new.env(parent = emptyenv())
  opened$app <- FALSE
  app_env$builder_open_final_app <- function(...) {
    opened$app <- TRUE
    TRUE
  }
  app_env$builder_reveal_release <- function(...) {
    opened$folder <- TRUE
    TRUE
  }
  upload <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(), upload)

  shiny::testServer(app_env$server, {
    ## Browsing server folders changes nothing.
    builder_server_browser(list(
      input_id = "builder_build_server_folder",
      type = "directory",
      pattern = NULL,
      folder = NULL
    ))
    session$setInputs(
      builder_server_browse = list(path = tempdir(), type = "directory")
    )
    expect_null(builder_server_browser()$folder)

    ## No file dialog opens; the page is told the picker is idle again.
    finished <- FALSE
    expect_false(builder_schedule_native_picker(
      "dataset_files",
      on_result = function(...) stop("No picker may open."),
      on_finally = function() finished <<- TRUE
    ))
    expect_true(finished)

    ## Only examples are loaded: a file never enters the import queue.
    before <- length(imports()$entries)
    expect_false(start_load("file", upload, "uploaded"))
    expect_identical(length(imports()$entries), before)
    expect_true(file.exists(upload))

    for (operation in c("create_project", "save_project", "open_project")) {
      expect_false(builder_operation_allowed(operation), info = operation)
    }

    ## No App starts and no folder opens on the server.
    result(app_env$builder_result_success(
      published = TRUE,
      app_dir = tempdir(),
      app_verified = TRUE,
      release = list(target = tempdir())
    ))
    session$setInputs(open_app = 1, reveal_folder = 1)
    expect_false(opened$app)
    expect_null(opened$folder)
  })
})

test_that("an examples-only build goes to a private folder that is downloaded and removed", {
  skip_if_not_installed("shiny")
  app_env <- builder_examples_only_app()
  kept <- new.env(parent = emptyenv())

  shiny::testServer(app_env$server, {
    parent <- builder_examples_only_output_parent()
    kept$parent <- parent
    expect_true(dir.exists(parent))
    expect_identical(builder_examples_only_output_parent(), parent)
    expect_true(startsWith(
      normalizePath(parent, winslash = "/"),
      normalizePath(tempdir(), winslash = "/")
    ))
    if (.Platform$OS.type != "windows") {
      expect_identical(
        format(file.info(parent)$mode),
        "700"
      )
    }

    ## Nothing to download before a build finished.
    expect_null(builder_examples_only_release_dir())

    release <- file.path(parent, "CerebroNexus_crb_0a1b2c")
    dir.create(release)
    writeLines("crb", file.path(release, "PBMC.crb"))
    result(app_env$builder_result_success(
      published = TRUE,
      built = file.path(release, "PBMC.crb"),
      release = list(target = release)
    ))
    expect_identical(
      builder_examples_only_release_dir(),
      normalizePath(release, winslash = "/")
    )
    if (requireNamespace("zip", quietly = TRUE) || nzchar(Sys.which("zip"))) {
      zipfile <- output$builder_examples_only_download
      expect_true(file.exists(zipfile))
      expect_true(
        "CerebroNexus_crb_0a1b2c/PBMC.crb" %in%
          utils::unzip(zipfile, list = TRUE)$Name
      )
    }

    ## A release outside this session's folder is never offered.
    outside <- withr::local_tempdir()
    result(app_env$builder_result_success(
      published = TRUE,
      built = file.path(outside, "x.crb"),
      release = list(target = outside)
    ))
    expect_null(builder_examples_only_release_dir())
  })
  ## The session ended: its build folder is gone.
  expect_false(dir.exists(kept$parent))
})

test_that("a new examples-only build removes the releases of the earlier ones", {
  skip_if_not_installed("shiny")
  app_env <- builder_examples_only_app()

  shiny::testServer(app_env$server, {
    parent <- builder_examples_only_output_parent()
    for (name in c("CerebroNexus_crb_aaaaaa", "CerebroNexus_crb_bbbbbb")) {
      dir.create(file.path(parent, name))
      writeLines("crb", file.path(parent, name, "PBMC.crb"))
    }
    dir.create(file.path(parent, ".release-control"))
    expect_identical(builder_examples_only_clear_releases(parent), 3L)
    expect_length(
      list.files(parent, all.files = TRUE, no.. = TRUE),
      0L
    )
    ## The folder itself stays for the next build, and an empty one is fine.
    expect_true(dir.exists(parent))
    expect_identical(builder_examples_only_clear_releases(parent), 0L)
  })
})

test_that("every file, table, image and picker request is refused on the server", {
  skip_if_not_installed("shiny")
  app_env <- builder_examples_only_app()
  ## Count the refusals: a handler that lost its guard no longer says so.
  refusals <- new.env(parent = emptyenv())
  refusals$n <- 0L
  app_env$builder_notify <- function(type = "info", message = "", ...) {
    if (identical(message, app_env$builder_examples_only_message)) {
      refusals$n <- refusals$n + 1L
    }
    invisible(TRUE)
  }
  upload <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(), upload)
  uploaded <- function(name) {
    data.frame(
      name = name,
      size = 1L,
      type = "",
      datapath = upload,
      stringsAsFactors = FALSE
    )
  }

  shiny::testServer(app_env$server, {
    before <- length(imports()$entries)
    refused <- function(...) {
      refusals$n <- 0L
      session$setInputs(...)
      refusals$n >= 1L
    }
    expect_true(
      refused(dataset_files = uploaded("x.rds")),
      info = "dataset_files"
    )
    ## Even a file Shiny let through is deleted, not kept.
    expect_false(file.exists(upload))
    saveRDS(list(), upload)
    expect_true(
      refused(
        builder_client_import_dispatch = list(
          client_id = "c1",
          name = "x.rds",
          size = 10
        )
      ),
      info = "builder_client_import_dispatch"
    )
    for (upload_input in c(
      "enhance-table_files",
      "enhance-marker_import_files",
      "enhance-tissue_image_file"
    )) {
      saveRDS(list(), upload)
      value <- stats::setNames(list(uploaded("x.csv")), upload_input)
      expect_true(do.call(refused, value), info = upload_input)
      expect_false(file.exists(upload), info = upload_input)
    }
    expect_true(
      refused(builder_choose_dataset_files = 1),
      info = "builder_choose_dataset_files"
    )
    expect_identical(length(imports()$entries), before)
  })
})

## Record why the server closes a session instead of sending the message.
builder_examples_only_capture_close <- function(app_env) {
  closed <- new.env(parent = emptyenv())
  closed$text <- NULL
  app_env$builder_examples_only_close <- function(session, text) {
    closed$text <- text
    session$close()
  }
  closed
}

test_that("a full examples-only server loads nothing for one more visitor", {
  skip_if_not_installed("shiny")
  withr::local_options(CerebroNexus.builder.examples_only_max_sessions = 1)
  app_env <- builder_examples_only_app()
  seats <- app_env$.builder_examples_only_seats
  closed <- builder_examples_only_capture_close(app_env)

  ## Another visitor holds the only seat.
  leave_other <- app_env$builder_examples_only_enter(seats, 1)
  shiny::testServer(app_env$server, {
    expect_true(session$isClosed())
    expect_false(exists("build_flow"))
  })
  expect_identical(closed$text, app_env$builder_examples_only_busy_message)
  expect_identical(seats$count, 1L)

  ## Once that visitor leaves, the next one gets the seat, and gives it back.
  leave_other()
  closed$text <- NULL
  shiny::testServer(app_env$server, {
    expect_false(session$isClosed())
    expect_true(exists("build_flow"))
    expect_identical(seats$count, 1L)
  })
  expect_null(closed$text)
  expect_identical(seats$count, 0L)
})

test_that("the examples-only server closes a session left idle, not one building", {
  skip_if_not_installed("shiny")
  app_env <- builder_examples_only_app()
  closed <- builder_examples_only_capture_close(app_env)
  clock <- new.env(parent = emptyenv())
  start <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  clock$now <- start
  watch <- app_env$builder_examples_only_watch_idle
  app_env$builder_examples_only_watch_idle <- function(...) {
    watch(..., now = function() clock$now)
  }
  at <- function(seconds) clock$now <- start + seconds

  shiny::testServer(app_env$server, {
    at(899)
    session$elapse(30000)
    expect_false(session$isClosed())
    ## A running build counts as activity (the default limit is 15 minutes).
    build_flow(list(stage = "building", plan = NULL))
    at(2000)
    session$elapse(30000)
    expect_false(session$isClosed())
    build_flow(list(stage = "idle", plan = NULL))
    at(2899)
    session$elapse(30000)
    expect_false(session$isClosed())
    at(2900)
    session$elapse(30000)
    expect_true(session$isClosed())
  })
  expect_match(closed$text, "after 15 minutes without activity", fixed = TRUE)
  expect_identical(app_env$.builder_examples_only_seats$count, 0L)
})

## In a real browser: no file control, a dropped file is ignored, and the
## three examples build into a download. Opt-in, like the other browser tests.
test_that("an examples-only Builder in a browser takes no file and downloads its build", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  skip_if_not_installed("shinytest2")
  skip_on_os("windows")
  builder_dir <- builder_profile_inst_path("builder")
  app_dir <- builder_browser_current_contract_app(
    builder_dir,
    .local_envir = environment()
  )
  app_file <- file.path(app_dir, "app.R")
  writeLines(
    c(
      "options(CerebroNexus.builder.examples_only = TRUE)",
      readLines(app_file, warn = FALSE)
    ),
    app_file
  )
  shinytest2::local_app_support(app_dir)
  files <- withr::local_tempdir()
  dropped <- file.path(files, "dropped.rds")
  stopifnot(file.copy(
    file.path(
      builder_dir,
      "fixtures",
      "complete_viewer",
      "complete_viewer_data.rds"
    ),
    dropped
  ))

  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "builder_examples_only",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_browser_wait_for_example_ready(app)

  expect_identical(
    app$get_js(paste0(
      "document.querySelectorAll('input[type=file], .builder-add-datasets, ",
      "#open_builder_project, #save_builder_project').length"
    )),
    0L
  )

  ## A file dropped on the page starts no upload.
  app$run_js(paste0(
    "(() => { const input = document.createElement('input');",
    "input.type = 'file'; input.id = 'test_drop_files';",
    "input.hidden = true; document.body.appendChild(input); })()"
  ))
  chrome <- app$get_chromote_session()
  root <- chrome$DOM$getDocument()$root$nodeId
  chrome$DOM$setFileInputFiles(
    files = list(normalizePath(dropped)),
    nodeId = chrome$DOM$querySelector(root, "#test_drop_files")$nodeId
  )
  app$run_js(paste0(
    "(() => { const transfer = new DataTransfer();",
    "for (const file of document.getElementById('test_drop_files').files) ",
    "transfer.items.add(file);",
    "document.body.dispatchEvent(new DragEvent('drop', ",
    "{dataTransfer: transfer, bubbles: true, cancelable: true}));",
    "document.getElementById('test_drop_files').remove(); })()"
  ))
  app$wait_for_idle(duration = 1000, timeout = 30000)
  expect_identical(
    app$get_js(
      "document.querySelectorAll('.ds--client-upload, .ds--import').length"
    ),
    0L
  )

  builder_with_browser_diagnostics(app, "browser-examples-only-build", {
    app$click(selector = ".example-btn[data-ex=complete_viewer_data]")
    app$wait_for_js(
      "document.querySelectorAll('.ds.ds--ready[data-ds]').length === 3",
      timeout = 180000
    )
    app$wait_for_idle(timeout = 30000)
    builder_browser_check_all_datasets(app, timeout = 60000)
    builder_browser_click_dynamic(app, "#continue_to_review")
    app$wait_for_js(
      "document.getElementById('confirm_review') !== null",
      timeout = 30000
    )
    builder_browser_click_dynamic(app, "#confirm_review")
    app$wait_for_js(
      paste0(
        "document.getElementById('build') !== null && ",
        "!document.getElementById('build').disabled"
      ),
      timeout = 60000
    )
    app$wait_for_idle(timeout = 30000)
    builder_browser_click_dynamic(app, "#build")
    app$wait_for_js(
      "document.querySelector('.result-card.is-success') !== null",
      timeout = 600000
    )
  })
  expect_identical(
    app$get_js(paste0(
      "['open_app', 'reveal_folder', 'copy_path', 'build_same_folder']",
      ".filter(id => document.getElementById(id) !== null).length"
    )),
    0L
  )
  zipfile <- app$get_download("builder_examples_only_download")
  members <- utils::unzip(zipfile, list = TRUE)$Name
  expect_true(any(grepl(
    "^CerebroNexus_viewer_[0-9a-f]{6}/cerebro_app/app[.]R$",
    members
  )))
  builder_expect_clean_browser_logs(app)
})
