## "Step k/N" and measured percentages for dataset loading and the build.

builder_progress_steps_app <- function(scope = parent.frame()) {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"), .local_envir = scope)
  sys.source("app.R", envir = app_env)
  app_env
}

builder_progress_steps_js <- function(names) {
  js <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  extract <- function(name) {
    start <- regexpr(paste0("function ", name, "("), js, fixed = TRUE)[[1L]]
    stopifnot(start > 0L)
    chars <- strsplit(substring(js, start), "", fixed = TRUE)[[1L]]
    depth <- 0L
    for (index in seq_along(chars)) {
      if (identical(chars[[index]], "{")) {
        depth <- depth + 1L
      } else if (identical(chars[[index]], "}")) {
        depth <- depth - 1L
        if (depth == 0L) {
          return(paste0(chars[seq_len(index)], collapse = ""))
        }
      }
    }
    stop("incomplete function ", name)
  }
  phases <- regmatches(
    js,
    regexpr("var buildOperationPhases = \\[[^]]*\\];", js)
  )
  c(phases, vapply(names, extract, character(1)))
}

test_that("each active import state is one counted step", {
  app_env <- builder_progress_steps_app()
  steps <- lapply(
    c("reading", "inspecting", "validating", "preparing"),
    app_env$builder_import_step
  )
  expect_identical(
    steps,
    lapply(1:4, function(index) list(index = index, count = 4L))
  )
  for (state in c("queued", "ready", "error", "cancelled", NA, "")) {
    expect_null(app_env$builder_import_step(state))
  }
  expect_null(app_env$builder_import_step(c("reading", "inspecting")))
})

test_that("a loading row shows its step and a bar only for a measured percent", {
  withr::local_package("shiny")
  app_env <- builder_progress_steps_app()
  queue <- app_env$builder_import_add(
    app_env$builder_import_queue(),
    app_env$builder_import_entry("ds1", "PBMC", list(kind = "file"))
  )
  queued <- app_env$builder_import_rail_row_model(queue$entries$ds1, 1L)
  expect_null(queued$step_index)
  queued_html <- as.character(app_env$builder_import_rail_row_ui(queued))
  expect_false(grepl("builder-import-step", queued_html, fixed = TRUE))
  expect_false(grepl("builder-import-meter", queued_html, fixed = TRUE))

  queue <- app_env$builder_import_transition(queue, "ds1", "reading", 1L)
  reading <- app_env$builder_import_rail_row_model(queue$entries$ds1, 1L)
  html <- as.character(app_env$builder_import_rail_row_ui(reading))
  expect_match(html, ">Step 1/4</span>", fixed = TRUE)
  expect_match(html, ">Reading dataset…</span>", fixed = TRUE)
  expect_match(
    html,
    'class="builder-import-meter" aria-hidden="true" hidden',
    fixed = TRUE
  )
  expect_false(grepl("progressbar", html, fixed = TRUE))

  measured <- app_env$builder_import_set_progress_bytes(
    queue,
    "ds1",
    1L,
    25,
    100
  )
  model <- app_env$builder_import_rail_row_model(measured$entries$ds1, 1L)
  html <- as.character(app_env$builder_import_rail_row_ui(model))
  expect_match(html, "Reading dataset… 25%", fixed = TRUE)
  expect_match(html, 'aria-valuenow="25"', fixed = TRUE)
  expect_match(html, "--builder-import-percent: 25%", fixed = TRUE)
  expect_false(grepl("meter\" aria-hidden=\"true\" hidden", html, fixed = TRUE))
  ## The step and the percent change the row, but not its structure: a tick
  ## patches the row in place.
  expect_false(identical(
    app_env$builder_import_rail_row_fingerprint(model),
    app_env$builder_import_rail_row_fingerprint(reading)
  ))
  expect_identical(
    app_env$builder_import_rail_row_structure(model),
    app_env$builder_import_rail_row_structure(reading)
  )

  queue <- app_env$builder_import_transition(queue, "ds1", "validating", 1L)
  validating <- app_env$builder_import_rail_row_model(queue$entries$ds1, 1L)
  expect_identical(
    validating[c("step_index", "step_count")],
    list(
      step_index = 3L,
      step_count = 4L
    )
  )
  expect_match(
    as.character(app_env$builder_import_rail_row_ui(validating)),
    ">Step 3/4</span>",
    fixed = TRUE
  )
})

test_that("read progress comes from the reader's file offset", {
  skip_on_os("windows")
  app_env <- builder_progress_steps_app()
  root <- withr::local_tempdir()
  data <- file.path(root, "data.rds")
  writeBin(as.raw(rep(1L, 1000L)), data)
  proc <- file.path(root, "proc")
  dir.create(file.path(proc, "4242", "fd"), recursive = TRUE)
  dir.create(file.path(proc, "4242", "fdinfo"))
  file.symlink(normalizePath(data), file.path(proc, "4242", "fd", "7"))
  file.symlink("/dev/null", file.path(proc, "4242", "fd", "1"))
  writeLines(
    c("pos:\t250", "flags:\t0100000"),
    file.path(proc, "4242", "fdinfo", "7")
  )
  writeLines("pos:\t9999", file.path(proc, "4242", "fdinfo", "1"))

  expect_identical(
    app_env$builder_import_read_offset(4242L, data, "rds", proc_root = proc),
    list(bytes_done = 250, bytes_total = 1000)
  )
  expect_identical(
    app_env$builder_import_read_offset(4242L, data, "qs2", proc_root = proc),
    list(bytes_done = 250, bytes_total = 1000)
  )
  ## A converted format is not read front to back: no percent.
  expect_null(app_env$builder_import_read_offset(
    4242L,
    data,
    "h5ad",
    proc_root = proc
  ))
  expect_null(app_env$builder_import_read_offset(
    4243L,
    data,
    "rds",
    proc_root = proc
  ))
  expect_null(app_env$builder_import_read_offset(
    NA,
    data,
    "rds",
    proc_root = proc
  ))
  other <- file.path(root, "other.rds")
  file.copy(data, other)
  expect_null(app_env$builder_import_read_offset(
    4242L,
    other,
    "rds",
    proc_root = proc
  ))

  ## The live process itself, where the platform reports offsets.
  skip_if_not(dir.exists(file.path("/proc", Sys.getpid(), "fdinfo")))
  connection <- file(data, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, "raw", 400L)
  live <- app_env$builder_import_read_offset(Sys.getpid(), data, "rds")
  expect_identical(live$bytes_total, 1000)
  expect_gte(live$bytes_done, 400)
})

test_that("the build percent counts only what a step counts", {
  app_env <- builder_progress_steps_app()
  measure <- app_env$builder_build_progress_measure
  expect_identical(
    measure(list(phase = "datasets", dataset_index = 2L, dataset_count = 3L)),
    list(percent = 33L, text = "1 of 3 datasets built")
  )
  expect_identical(
    measure(list(phase = "datasets", dataset_index = 1L, dataset_count = 4L)),
    list(percent = 0L, text = "0 of 4 datasets built")
  )
  ## One dataset without step counts: the share of it that is written is
  ## unknown.
  expect_identical(
    measure(list(phase = "datasets", dataset_index = 1L, dataset_count = 1L)),
    list(percent = NULL, text = NULL)
  )
  ## With step counts the finished steps of the current dataset are added.
  expect_identical(
    measure(list(
      phase = "datasets",
      dataset_index = 1L,
      dataset_count = 1L,
      steps_done = 3L,
      steps_total = 10L
    )),
    list(percent = 30L, text = "3 of 10 steps done")
  )
  expect_identical(
    measure(list(
      phase = "datasets",
      dataset_index = 2L,
      dataset_count = 2L,
      steps_done = 5L,
      steps_total = 10L
    )),
    list(
      percent = 75L,
      text = "1 of 2 datasets built · 5 of 10 steps done"
    )
  )
  expect_identical(
    measure(list(
      phase = "verify",
      step_label = "Checking files",
      step_index = 120L,
      step_count = 230L
    )),
    list(percent = 51L, text = "119 of 230 checked")
  )
  expect_identical(
    measure(list(
      phase = "verify",
      step_label = "Checking datasets",
      step_index = 0L,
      step_count = 2L
    ))$percent,
    0L
  )
  for (detail in list(
    list(phase = "verify", step_label = "Checking Viewer runtime"),
    list(phase = "viewer"),
    list(phase = "publish"),
    list(phase = "datasets", dataset_index = 1.5, dataset_count = 3L),
    NULL
  )) {
    expect_identical(measure(detail), list(percent = NULL, text = NULL))
  }
  message <- app_env$builder_build_progress_message(list(
    phase = "datasets",
    dataset_index = 3L,
    dataset_count = 4L,
    dataset_name = "PBMC",
    step_label = "Writing CRB"
  ))
  expect_identical(message$text, "Dataset 3/4 · PBMC · Writing CRB")
  expect_identical(message$percent, 50L)
  expect_identical(message$percent_text, "2 of 4 datasets built")
  expect_null(
    app_env$builder_build_progress_message(list(phase = "viewer"))$percent
  )
})

test_that("the Build card carries a step label and a hidden meter", {
  app_env <- builder_progress_steps_app()
  html <- as.character(app_env$builder_build_stage_status_body_ui(list(
    state = "building",
    message = "Building 2 datasets…"
  )))
  expect_match(html, 'class="builder-build-progress-step" hidden', fixed = TRUE)
  expect_match(
    html,
    '<progress class="builder-build-progress-meter" max="100" value="0"',
    fixed = TRUE
  )
  expect_match(
    html,
    'class="builder-build-progress-measure" hidden',
    fixed = TRUE
  )
  ## The overlay names the step in its own header.
  overlay <- as.character(app_env$builder_build_progress_detail_ui("overlay"))
  expect_false(grepl("builder-build-progress-step", overlay, fixed = TRUE))
  expect_match(overlay, "builder-build-progress-meter", fixed = TRUE)
})

test_that("the browser maps build phases to steps and clamps percents", {
  testthat::skip_on_cran()
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    enc2utf8(c(
      "'use strict';",
      builder_progress_steps_js(c("buildPhaseStep", "buildProgressPercent")),
      "console.log(JSON.stringify([",
      "  buildPhaseStep('preflight'), buildPhaseStep('datasets'),",
      "  buildPhaseStep('publish'), buildPhaseStep('result'),",
      "  buildProgressPercent(33), buildProgressPercent('51'),",
      "  buildProgressPercent(140), buildProgressPercent(null),",
      "  buildProgressPercent(undefined), buildProgressPercent('x')",
      "]));"
    )),
    runner,
    useBytes = TRUE
  )
  out <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_identical(
    jsonlite::fromJSON(out[[length(out)]], simplifyVector = FALSE),
    list(
      list(index = 1L, count = 7L),
      list(index = 4L, count = 7L),
      list(index = 7L, count = 7L),
      NULL,
      33L,
      51L,
      100L,
      NULL,
      NULL,
      NULL
    )
  )
})
