viewer_test_path <- function(...) {
  source_path <- testthat::test_path("..", "..", "inst", "viewer", ...)
  if (file.exists(source_path)) {
    return(source_path)
  }

  system.file("viewer", ..., package = "CerebroNexus")
}

viewer_app_test_path <- function() {
  source_path <- testthat::test_path("..", "..", "inst")
  if (file.exists(file.path(source_path, "app.R"))) {
    return(source_path)
  }

  system.file(package = "CerebroNexus")
}

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
  envir = parent.frame()
) {
  driver <- NULL
  function() {
    if (is.null(driver)) {
      local_app_support(app_dir, envir = envir)
      driver <<- shinytest2::AppDriver$new(
        app_dir,
        name = name,
        height = 950,
        width = 1619,
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
