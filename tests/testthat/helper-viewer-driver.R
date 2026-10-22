## The bundled app lives at the installed package root, so shiny::loadSupport()
## warns about the package layout even though sourcing these files is intended.
local_app_support <- function(app_dir, envir = rlang::caller_env()) {
  withCallingHandlers(
    shinytest2::local_app_support(app_dir, envir = envir),
    warning = function(warning) {
      if (
        grepl(
          "Loading R/ subdirectory for Shiny application",
          conditionMessage(warning),
          fixed = TRUE
        )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

## Returns a function giving the file's shared AppDriver: created on first use,
## stopped when the calling file finishes. Booting dominates browser tests, so
## tests that navigate to the tab they assert on share it. A test that asserts
## pristine startup state, leaves a modal open or switches data sets keeps its
## own AppDriver.
viewer_shared_app <- function(
  name,
  app_dir = viewer_app_test_path(),
  envir = parent.frame(),
  width = 1619
) {
  driver <- NULL
  function() {
    if (is.null(driver)) {
      local_app_support(app_dir, envir = envir)
      driver <<- shinytest2::AppDriver$new(
        app_dir,
        name = name,
        height = 950,
        width = width,
        load_timeout = 120000
      )
      withr::defer(driver$stop(), envir = envir)
      driver$wait_for_idle(timeout = 20000)
    }
    ## An AppDriver booted by a test in between opens a newer Chrome tab and
    ## leaves this page hidden, where Chrome stops animation frames and pages
    ## that wait for them never render.
    driver$get_chromote_session()$Page$bringToFront()
    driver
  }
}

## Starts a new Shiny session in the app process and Chrome tab of `app`, so a
## test that needs fresh session state does not boot another app. AppDriver
## injects its page helper and records the session's test URL (which
## get_value() reads) once, at start; both are redone here the way
## AppDriver$new() does them.
viewer_new_session <- function(app, timeout = 60000) {
  private <- app$.__enclos_env__$private
  stopifnot(exists("shiny_test_url", envir = private, inherits = FALSE))
  session <- app$get_chromote_session()
  session$Page$navigate(app$get_url())
  tracer <- system.file(
    "internal",
    "js",
    "shiny-tracer.js",
    package = "shinytest2"
  )
  stopifnot(nzchar(tracer))
  session$Runtime$evaluate(paste(
    readLines(tracer, warn = FALSE),
    collapse = "\n"
  ))
  app$wait_for_js(
    "window.shinytest2 && window.shinytest2.ready === true",
    timeout = timeout
  )
  app$wait_for_idle(duration = 200, timeout = timeout)
  private$shiny_test_url <- app$get_js(
    "Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl:true})"
  )
  invisible(app)
}

viewer_drag_mouse <- function(app, x1, y1, x2, y2) {
  mouse <- app$get_chromote_session()$Input$dispatchMouseEvent
  mouse(
    type = "mousePressed",
    x = x1,
    y = y1,
    button = "left",
    buttons = 1,
    clickCount = 1
  )
  mouse(
    type = "mouseMoved",
    x = x2,
    y = y2,
    button = "left",
    buttons = 1
  )
  mouse(
    type = "mouseReleased",
    x = x2,
    y = y2,
    button = "left",
    buttons = 0,
    clickCount = 1
  )
}

viewer_set_selectize <- function(app, input_id, value) {
  input_json <- jsonlite::toJSON(input_id, auto_unbox = TRUE)
  values_json <- jsonlite::toJSON(as.character(value), auto_unbox = FALSE)
  ## Gene controls register their search on the first visit, after the page's
  ## data chain has run; allow for that under load.
  app$wait_for_js(
    sprintf(
      "typeof document.getElementById(%s)?.selectize?.settings.load === 'function'",
      input_json
    ),
    timeout = 15000
  )
  app$run_js(sprintf(
    paste0(
      "(() => {const s=document.getElementById(%s)?.selectize;",
      "if(!s)throw new Error('Selectize input is not ready');",
      "const v=%s;v.filter(Boolean).forEach(x=>s.addOption({",
      "[s.settings.valueField]:x,[s.settings.labelField]:x}));",
      "const out=s.settings.maxItems===1?(v[0]||''):v;s.setValue(out);",
      "Shiny.setInputValue(%s,out,{priority:'event'});})()"
    ),
    input_json,
    values_json,
    input_json
  ))
}

# Read published fixture content without duplicating file-reading boilerplate.
viewer_test_text <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

## Gene expression summaries update only while their gate is visible
## (expression_summary_viewport_request). Scroll on every poll: panels above
## the gate may still change height and push it back out of view.
viewer_show_expression_summary <- function(app, timeout = 60000) {
  app$wait_for_js(
    paste0(
      "(() => {document.getElementById('expression_summary_gate')",
      "?.scrollIntoView();",
      "return Shiny.shinyapp.$inputValues",
      ".expression_summary_viewport_request === true;})()"
    ),
    timeout = timeout
  )
}
