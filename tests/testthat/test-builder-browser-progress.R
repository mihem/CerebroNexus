library(shinytest2)

## Hold each import stage and each dataset build step on screen for a moment,
## so a real browser sees every step of a small fixture.
builder_browser_progress_hooks <- function(app_dir) {
  cat(
    "",
    "## Test fixture: keep each import step visible for a moment.",
    "local({",
    "  if (exists('.builder_import_progress_callback', envir = globalenv(),",
    "      inherits = FALSE)) {",
    "    make <- get('.builder_import_progress_callback', envir = globalenv())",
    "    assign('.builder_import_progress_callback', function(path, generation,",
    "        min_interval = 0.25) {",
    "      write <- make(path, generation, min_interval = min_interval)",
    "      last <- NULL",
    "      function(stage, bytes_done = NULL, bytes_total = NULL) {",
    "        written <- write(stage, bytes_done, bytes_total)",
    "        key <- paste(stage, !is.null(bytes_done))",
    "        if (isTRUE(written) && !identical(key, last)) {",
    "          last <<- key",
    "          Sys.sleep(1.2)",
    "        }",
    "        invisible(written)",
    "      }",
    "    }, envir = globalenv())",
    "  }",
    "})",
    sep = "\n",
    file = file.path(app_dir, "prerequisite.R"),
    append = TRUE
  )
  cat(
    "",
    "## Test fixture: keep each dataset build step visible for a moment.",
    "local({",
    "  if (exists('.builder_build_progress_callback', envir = globalenv(),",
    "      inherits = FALSE)) {",
    "    make <- get('.builder_build_progress_callback', envir = globalenv())",
    "    assign('.builder_build_progress_callback', function(path) {",
    "      report <- make(path)",
    "      function(phase, detail = NULL) {",
    "        value <- report(phase, detail)",
    "        if (identical(phase, 'datasets') && is.list(detail)) Sys.sleep(1)",
    "        invisible(value)",
    "      }",
    "    }, envir = globalenv())",
    "  }",
    "})",
    sep = "\n",
    file = file.path(app_dir, "build.R"),
    append = TRUE
  )
  invisible(app_dir)
}

## Every distinct state of the watched elements, recorded as the page changes.
builder_browser_progress_recorder <- function(app, name, script) {
  app$run_js(sprintf(
    paste0(
      "(() => { const seen = window['%1$s'] = []; const keys = new Set();",
      "const take = () => { for (const record of (%2$s)()) {",
      "const key = JSON.stringify(record);",
      "if (!keys.has(key)) { keys.add(key); seen.push(record); } } };",
      "new MutationObserver(take).observe(document.body, {subtree: true,",
      "childList: true, characterData: true, attributes: true}); take(); })()"
    ),
    name,
    script
  ))
}

test_that("loading rows and the Build card show the step and a measured percent", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  skip_on_os(c("windows", "mac"))
  builder_dir <- builder_profile_inst_path("builder")
  app_dir <- builder_browser_current_contract_app(
    builder_dir,
    .local_envir = environment()
  )
  builder_browser_progress_hooks(app_dir)
  local_app_support(app_dir)
  files <- withr::local_tempdir()
  fixture <- file.path(
    builder_dir,
    "fixtures",
    "complete_viewer",
    "complete_viewer_data.rds"
  )
  picked <- file.path(files, c("progress_one.rds", "progress_two.rds"))
  stopifnot(all(file.copy(fixture, picked)))
  build_parent <- file.path(files, "build")
  dir.create(build_parent)

  ## The desktop dialog answers with both files, or with the build folder.
  fake_bin <- withr::local_tempdir()
  picker_script <- c(
    "#!/bin/sh",
    "case \" $* \" in",
    sprintf(
      "  *--directory*|*--getexistingdirectory*) printf '%%s\\n' %s ;;",
      shQuote(normalizePath(build_parent))
    ),
    sprintf(
      "  *) printf '%%s\\n' %s ;;",
      paste(shQuote(normalizePath(picked)), collapse = " ")
    ),
    "esac"
  )
  pickers <- file.path(fake_bin, c("zenity", "kdialog"))
  for (picker in pickers) {
    writeLines(picker_script, picker)
  }
  Sys.chmod(pickers, mode = "0755")
  withr::local_envvar(
    PATH = paste(fake_bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    WSL_DISTRO_NAME = NA_character_,
    WSL_INTEROP = NA_character_
  )

  app <- AppDriver$new(
    app_dir,
    name = "builder_progress_steps",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_browser_wait_for_example_ready(app)

  builder_browser_progress_recorder(
    app,
    "__importSeen",
    paste0(
      "() => Array.from(document.querySelectorAll('.ds--import[data-import-id]'))",
      ".map(row => { const part = selector => row.querySelector(selector);",
      "const meter = part('.builder-import-meter');",
      "const status = part('.builder-import-status');",
      "return { step: part('.builder-import-step') ? ",
      "part('.builder-import-step').textContent : null,",
      "status: status ? status.textContent : null,",
      "role: status ? status.getAttribute('role') : null,",
      "now: status ? status.getAttribute('aria-valuenow') : null,",
      "meter: meter ? (meter.hidden ? 'hidden' : meter.style.getPropertyValue(",
      "'--builder-import-percent').trim()) : null }; })"
    )
  )
  builder_browser_click_dynamic(app, ".builder-add-datasets")
  builder_with_browser_diagnostics(app, "browser-progress-import", {
    app$wait_for_js(
      paste0(
        "['progress_one', 'progress_two'].every(name => ",
        "Array.from(document.querySelectorAll('.ds--ready[data-ds] .nm'))",
        ".some(node => node.textContent.trim() === name))"
      ),
      timeout = 120000
    )
  })
  seen <- app$get_js("window.__importSeen")
  field <- function(records, name) {
    vapply(
      records,
      function(record) if (is.null(record[[name]])) "" else record[[name]],
      character(1)
    )
  }
  steps <- field(seen, "step")
  statuses <- field(seen, "status")
  for (expected in list(
    c("Step 1/4", "Reading dataset…"),
    c("Step 2/4", "Checking cells, genes and metadata…"),
    c("Step 3/4", "Validating detected content…"),
    c("Step 4/4", "Preparing dataset settings…")
  )) {
    expect_true(
      any(steps == expected[[1L]] & startsWith(statuses, expected[[2L]])),
      info = paste(expected, collapse = " ")
    )
  }
  ## A step without a byte count shows no percent and no bar.
  plain <- Filter(
    function(record) identical(record$step, "Step 2/4"),
    seen
  )
  expect_true(all(vapply(
    plain,
    function(record) identical(record$meter, "hidden") && is.null(record$now),
    logical(1)
  )))
  ## The snapshot copy is counted: the status, its progressbar value and the
  ## bar agree.
  measured <- Filter(
    function(record) identical(record$step, "Step 4/4") && !is.null(record$now),
    seen
  )
  expect_gt(length(measured), 0L)
  for (record in measured) {
    expect_identical(record$role, "progressbar")
    expect_identical(
      record$status,
      paste0("Preparing dataset settings… ", record$now, "%")
    )
    expect_identical(record$meter, paste0(record$now, "%"))
  }

  builder_browser_dismiss_project_offer(app)
  builder_with_browser_diagnostics(app, "browser-progress-review", {
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
  })
  builder_browser_progress_recorder(
    app,
    "__buildSeen",
    paste0(
      "() => { const card = document.getElementById('build-stage-status');",
      "const detail = card && card.querySelector('[data-build-progress-detail]');",
      "if (!detail || detail.hidden) return [];",
      "const part = selector => detail.querySelector(selector);",
      "const measure = part('.builder-build-progress-measure');",
      "const meter = part('.builder-build-progress-meter');",
      "return [{ step: part('.builder-build-progress-step').hidden ? null : ",
      "part('.builder-build-progress-step').textContent,",
      "text: part('.builder-build-progress-text').textContent,",
      "live: part('.builder-build-progress-text').getAttribute('aria-live'),",
      "percent: measure.hidden ? null : part('.builder-build-progress-percent')",
      ".textContent,",
      "now: measure.hidden ? null : meter.getAttribute('aria-valuenow'),",
      "value: measure.hidden ? null : meter.value,",
      "valuetext: measure.hidden ? null : meter.getAttribute('aria-valuetext') }]; }"
    )
  )
  builder_browser_click_dynamic(app, "#build")
  builder_with_browser_diagnostics(app, "browser-progress-build", {
    app$wait_for_js(
      "document.querySelector('.result-card.is-success') !== null",
      timeout = 600000
    )
  })
  built <- app$get_js("window.__buildSeen")
  build_steps <- field(built, "step")
  expect_true(any(build_steps == "Step 4/7"))
  expect_true(all(grepl("^Step [1-7]/7$", build_steps[nzchar(build_steps)])))
  expect_true(all(vapply(
    built,
    function(record) identical(record$live, "polite"),
    logical(1)
  )))
  ## Two datasets: each percent is the datasets built plus the finished steps
  ## of the one being written, and the meter says so.
  counted <- Filter(
    function(record) {
      identical(record$step, "Step 4/7") && !is.null(record$percent)
    },
    built
  )
  expect_gt(length(counted), 0L)
  for (record in counted) {
    numbers <- as.numeric(regmatches(
      record$valuetext,
      gregexpr("[0-9]+", record$valuetext)
    )[[1L]])
    expect_length(numbers, 4L)
    expected <- floor(100 * (numbers[[1L]] + numbers[[3L]] / numbers[[4L]]) / 2)
    expect_identical(record$now, as.character(expected))
    expect_equal(record$value, expected)
    expect_identical(
      record$percent,
      paste0(expected, "% · ", record$valuetext)
    )
    expect_match(
      record$valuetext,
      "^[01] of 2 datasets built · [0-9]+ of [0-9]+ steps done$"
    )
  }
  ## The bar moves inside each dataset, not only between them.
  first <- Filter(
    function(record) startsWith(record$text, "Dataset 1/2"),
    counted
  )
  expect_gt(length(unique(vapply(first, `[[`, "", "now"))), 2L)
  second <- Filter(
    function(record) startsWith(record$text, "Dataset 2/2"),
    counted
  )
  expect_true(any(vapply(second, `[[`, "", "now") == "50"))
  expect_true(any(as.numeric(vapply(second, `[[`, "", "now")) > 50))
  expect_false(any(vapply(
    built,
    function(record) {
      identical(record$text, "Packaging Viewer") && !is.null(record$percent)
    },
    logical(1)
  )))
  builder_expect_clean_browser_logs(app)
})
