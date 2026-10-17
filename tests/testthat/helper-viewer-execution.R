viewer_node_test <- function(scenario) {
  skip_if(Sys.which("node") == "", "Node unavailable")
  output <- system2(
    "node",
    c(
      "--no-global-search-paths",
      shQuote(test_path("fixtures", "viewer-transport.cjs")),
      shQuote(viewer_test_path("www")),
      shQuote(scenario)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
}

viewer_browser_test <- function(scenario) {
  skip_on_cran()
  skip_if_not_installed("chromote")
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
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("coordinated_views", "UI.R"), scope)
  ids <- c("overview", "expression", "spatial", "trajectory", "trekker")
  html <- as.character(htmltools::tagList(
    scope$tab_coordinated_views,
    lapply(ids, function(id) {
      scope$cerebroCellViewOutput(paste0(id, "_projection"))
    })
  ))
  evaluate(sprintf(
    "document.body.innerHTML = %s",
    jsonlite::toJSON(html, auto_unbox = TRUE)
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
        ".coordviews-page{width:1000px}.cv-panes{height:500px}.cv-pane{height:460px}.cerebro-cell-view-host{display:none}"
      ),
      auto_unbox = TRUE
    )
  ))
  evaluate(read_js(system.file("www/shared/jquery.js", package = "shiny")))
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
