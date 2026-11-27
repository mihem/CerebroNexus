## Runs a JavaScript file under Node and returns its output lines; a non-zero
## exit leaves a "status" attribute, as system2() does.
run_node <- function(script, ...) {
  node <- skip_if_no_node()
  system2(node, shQuote(c(script, ...)), stdout = TRUE, stderr = TRUE)
}

## Inline JavaScript lines, run from a temporary file.
run_node_script <- function(code) {
  skip_if_no_node()
  runner <- withr::local_tempfile(fileext = ".js")
  writeLines(code, runner)
  run_node(runner)
}

## A JavaScript statement that evaluates `path` in the current scope; the
## script must have required `fs`.
js_eval_file <- function(path) {
  sprintf("eval(fs.readFileSync(%s, 'utf8'));", encodeString(path, quote = '"'))
}

## Fails with the script's output when it exited non-zero. `pass` names a line
## the script prints once its assertions ran, so an early clean exit cannot
## pass.
expect_node_output <- function(output, pass = NULL) {
  log <- paste(output, collapse = "\n")
  expect_null(attr(output, "status"), info = log)
  if (!is.null(pass)) {
    expect_match(log, pass, fixed = TRUE)
  }
  invisible(output)
}

## A script from tests/testthat/fixtures/, given the viewer files it loads.
expect_node_fixture <- function(fixture, ..., pass = NULL) {
  expect_node_output(run_node(test_path("fixtures", fixture), ...), pass)
}

expect_node_script <- function(code, pass = NULL) {
  expect_node_output(run_node_script(code), pass)
}

viewer_node_test <- function(scenario) {
  expect_node_output(run_node(
    "--no-global-search-paths",
    test_path("fixtures", "viewer-transport.cjs"),
    viewer_test_path("www"),
    scenario
  ))
}

## The Linked views page and the single-view hosts every browser scenario
## starts from. Building it sources the Viewer UI (about 2.5 s) and gives the
## same HTML each time, so each test worker builds it once.
viewer_browser_page <- local({
  html <- NULL
  function() {
    if (is.null(html)) {
      scope <- viewer_ui_test_env()
      sys.source(viewer_test_path("coordinated_views", "UI.R"), scope)
      ids <- c("overview", "expression", "spatial", "trajectory", "trekker")
      html <<- as.character(htmltools::tagList(
        scope$tab_coordinated_views,
        lapply(ids, function(id) {
          scope$cerebroCellViewOutput(paste0(id, "_projection"))
        })
      ))
    }
    html
  }
})

viewer_browser_test <- function(scenario) {
  skip_if_no_chrome()
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$close(), add = TRUE)
  evaluate <- function(code) {
    result <- browser$Runtime$evaluate(
      expression = code,
      awaitPromise = TRUE,
      returnByValue = TRUE
    )
    if (!is.null(result$exceptionDetails)) {
      description <- result$exceptionDetails$exception$description
      if (is.null(description)) {
        description <- result$exceptionDetails$text
      }
      stop(description, call. = FALSE)
    }
    result$result$value
  }
  read_js <- function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }
  evaluate(sprintf(
    "document.body.innerHTML = %s",
    jsonlite::toJSON(viewer_browser_page(), auto_unbox = TRUE)
  ))
  css <- paste(
    readLines(viewer_test_path("www", "coordviews.css")),
    collapse = "\n"
  )
  evaluate(sprintf(
    "{const s=document.createElement('style');s.textContent=%s;document.head.append(s);}",
    jsonlite::toJSON(
      paste(
        css,
        ".coordviews-page{width:1000px}.cv-panes{height:500px}.cerebro-cell-view-host{display:none}"
      ),
      auto_unbox = TRUE
    )
  ))
  evaluate(read_js(system.file("www/shared/jquery.js", package = "shiny")))
  ## The Linked views page always has selectize: its pickers rely on it.
  evaluate(read_js(system.file(
    "www/shared/selectize/js/selectize.min.js",
    package = "shiny"
  )))
  evaluate(read_js(test_path("fixtures", "viewer-browser.js")))
  if (identical(scenario, "cloneCache")) {
    evaluate("delete window.CerebroPointRenderer")
  }
  for (file in c(
    "dataset-context.js",
    "background-model.js",
    "cell_views_state.js",
    "cell_views_wire.js",
    "cv-geom.js",
    "cell_views.js"
  )) {
    evaluate(read_js(viewer_test_path("www", file)))
  }
  evaluate(read_js(test_path("fixtures", "viewer-behavior.js")))
  result <- evaluate(sprintf(
    "viewerScenarios[%s]()",
    jsonlite::toJSON(scenario, auto_unbox = TRUE)
  ))
  expect_true(isTRUE(result), info = scenario)
}

## Place an encoded CRB image pixel through the same geometry used by the Viewer.
expect_viewer_image_pixel <- function(image, row, column, expected) {
  viewer <- new.env()
  sys.source(viewer_test_path("utility_functions.R"), viewer)
  preset <- viewer$spatialImagePreset(
    NULL, NULL, NULL, NULL, image$histology_alignment
  )
  pixels <- png::readPNG(jsonlite::base64_dec(sub(
    "^data:image/png;base64,", "", image$histology_image
  )))
  payload <- jsonlite::toJSON(list(
    preset = preset,
    bounds = as.list(image$histology_image_bounds),
    u = (column - 0.5) / ncol(pixels),
    v = (row - 0.5) / nrow(pixels)
  ), auto_unbox = TRUE, digits = NA)
  output <- run_node_script(c(
    "const fs = require('fs'); global.window = {};",
    js_eval_file(viewer_test_path("www", "background-model.js")),
    paste0("const image = ", payload, ";"),
    "const model = window.CerebroBackgroundModel;",
    "const state = model.renderState(image, model.presetState(image));",
    "const [tl, tr, bl] = model.corners(image.bounds, state);",
    paste0(
      "console.log(JSON.stringify(tl.map((x, i) => x + ",
      "image.u * (tr[i] - x) + image.v * (bl[i] - x))));"
    )
  ))
  expect_node_output(output)
  expect_equal(jsonlite::fromJSON(output), unname(expected))
}
