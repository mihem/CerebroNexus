repo_file <- function(...) {
  parts <- c(...)
  stripped <- if (length(parts) && identical(parts[[1L]], "inst")) {
    parts[-1L]
  } else {
    parts
  }
  if (length(stripped)) {
    installed <- system.file(
      do.call(file.path, as.list(stripped)),
      package = "cerebroAppLite"
    )
    if (nzchar(installed)) {
      return(installed)
    }
  }
  testthat::test_path("..", "..", ...)
}

js_source <- function(...) {
  paste(readLines(repo_file(...), warn = FALSE), collapse = "\n")
}

run_viewport_projection_node <- function(body) {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  viewport_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "viewport.js"
  )
  projection_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "projection_scatter.js"
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.Event = function (type) { this.type = type; };",
      "global.window = {",
      "  innerHeight: 900,",
      "  addEventListener: function () {},",
      "  dispatchEvent: function () {},",
      "  requestAnimationFrame: function () { return 1; }",
      "};",
      "global.document = {",
      "  addEventListener: function () {},",
      "  getElementsByClassName: function () { return []; },",
      "  getElementById: function () { return null; },",
      "  body: null, documentElement: null",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(viewport_path, quote = "\"")
      ),
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(projection_path, quote = "\"")
      ),
      body
    ),
    runner
  )
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("projection initializes when its bundle is emitted before viewport", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  viewport_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "viewport.js"
  )
  projection_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "projection_scatter.js"
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const handlers = {};",
      "global.Event = function (type) { this.type = type; };",
      "global.window = {",
      "  addEventListener: (name, fn) => { handlers[name] = fn; },",
      "  dispatchEvent: event => { if (handlers[event.type]) handlers[event.type](event); },",
      "  requestAnimationFrame: function () { return 1; }",
      "};",
      "global.document = {",
      "  addEventListener: function () {},",
      "  getElementsByClassName: function () { return []; },",
      "  getElementById: function () { return null; },",
      "  body: null, documentElement: null",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(projection_path, quote = "\"")
      ),
      "const before = Boolean(window.cerebroProjection);",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(viewport_path, quote = "\"")
      ),
      "console.log(JSON.stringify([before, window.cerebroProjection.__ready]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[false,true]")
})

test_that("projection adapter delegates sizing to the shared controller", {
  output <- run_viewport_projection_node(c(
    "const api = window.cerebroProjection;",
    "const plainParent = { classList: { contains: () => false } };",
    "const spinnerParent = { classList: { contains: name => name === 'shiny-spinner-output-container' } };",
    "const plain = { parentElement: plainParent };",
    "const spun = { parentElement: spinnerParent };",
    "console.log(JSON.stringify([",
    "  api._projectionTargetHeight(900, 120, 70, 18, 240),",
    "  api._projectionTargetHeight(520, 250, 80, 18, 240),",
    "  api._projectionSizingElement(plain) === plain,",
    "  api._projectionSizingElement(spun) === spinnerParent",
    "]));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[692,240,true,true]")
})

test_that("projection adapter measures the box and observes a dynamic legend", {
  output <- run_viewport_projection_node(c(
    "const box = { name: 'box', getBoundingClientRect: () => ({ bottom: 790 }) };",
    "const legend = { name: 'legend' };",
    "const wrapper = {",
    "  classList: { contains: name => name === 'shiny-spinner-output-container' },",
    "  getBoundingClientRect: () => ({ top: 120, bottom: 700, width: 800 })",
    "};",
    "const plot = {",
    "  id: 'overview_projection', parentElement: wrapper,",
    "  closest: selector => selector === '.box' ? box : null",
    "};",
    "document.getElementById = id => id === 'overview_projection_legend' ? legend : null;",
    "const adapter = window.cerebroProjection._viewportAdapter;",
    "const measured = adapter.measure(plot);",
    "const observed = adapter.observeTargets(plot).map(x => x.name);",
    "console.log(JSON.stringify([measured, observed]));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_equal(
    output,
    '[{"top":120,"contentBelow":90,"width":800},["box","legend"]]'
  )
})

test_that("projection reveal remains keyed to the stable visibility gate", {
  output <- run_viewport_projection_node(c(
    "const added = [];",
    "const gate = { classList: { add: value => added.push(value) } };",
    "let selector = null;",
    "const plot = { closest: value => { selector = value; return gate; } };",
    "const api = window.cerebroProjection;",
    "api._revealProjectionHost(plot);",
    "console.log(JSON.stringify([",
    "  api._shouldRevealProjection(false, 754, 754),",
    "  api._shouldRevealProjection(true, 754, 775),",
    "  api._shouldRevealProjection(true, 754, 754),",
    "  selector, added",
    "]));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_equal(
    output,
    paste0(
      '[false,false,true,".cerebro-viewport-gate",["is-sized"]]'
    )
  )
})

test_that("Plotly relayout completion remains part of viewport readiness", {
  projection <- js_source(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "projection_scatter.js"
  )

  expect_match(
    projection,
    "viewport.register(plot, projectionViewportAdapter)",
    fixed = TRUE
  )
  expect_match(projection, "state.relayoutPending = true", fixed = TRUE)
  expect_match(projection, "relayout.then(finish, finish)", fixed = TRUE)
  expect_match(projection, "!state.relayoutPending", fixed = TRUE)
  expect_match(
    projection,
    "plotlySizeMatches(plot, height, width)",
    fixed = TRUE
  )
  expect_false(grepl("projectionRevealed", projection, fixed = TRUE))
})

test_that("all projection tabs use one stable viewport gate", {
  ui_paths <- list(
    c("inst", "shiny", "v1.4", "overview", "UI_projection.R"),
    c("inst", "shiny", "v1.4", "gene_expression", "UI_projection.R"),
    c("inst", "shiny", "v1.4", "spatial", "UI_projection.R"),
    c("inst", "shiny", "v1.4", "trajectory", "projection.R")
  )
  sources <- vapply(
    ui_paths,
    function(parts) do.call(js_source, as.list(parts)),
    character(1)
  )

  expect_true(all(grepl("cerebro-viewport-gate", sources, fixed = TRUE)))
  expect_false(any(grepl("calc\\(100vh - [0-9]+px\\)", sources)))
})

test_that("the shared controller replaces the deleted generic fill script", {
  ui <- js_source("inst", "shiny", "v1.4", "shiny_UI.R")
  viewport <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  old_fill <- repo_file("inst", "shiny", "v1.4", "www", "fill_height.js")

  expect_match(ui, '"shiny/v1.4/www/viewport.js"', fixed = TRUE)
  expect_false(grepl("fill_height.js", ui, fixed = TRUE))
  expect_true(file.exists(viewport))
  expect_false(file.exists(old_fill))
})

test_that("IR owns one viewport host and each ordinary output only fills it", {
  visualizations <- js_source(
    "inst",
    "shiny",
    "v1.4",
    "immune_repertoire",
    "visualizations.R"
  )

  expect_match(visualizations, 'class = "cerebro-viewport-host"', fixed = TRUE)
  expect_match(visualizations, '"cerebro-viewport-fill"', fixed = TRUE)
  expect_match(visualizations, '"cerebro-viewport-natural"', fixed = TRUE)
  expect_match(
    visualizations,
    'uiOutput("ir_ui_pairedScatter_plot", class = "cerebro-viewport-fill")',
    fixed = TRUE
  )
  expect_false(grepl(
    'withSpinner(uiOutput("ir_ui_pairedScatter_plot"',
    visualizations,
    fixed = TRUE
  ))
})

test_that("viewport CSS reveals only a settled card and never tweens height", {
  css <- js_source("inst", "shiny", "v1.4", "www", "custom.css")

  expect_match(css, ".cerebro-viewport-host {", fixed = TRUE)
  expect_match(css, "height: 60vh;", fixed = TRUE)
  expect_match(css, "height: 60dvh;", fixed = TRUE)
  expect_match(
    css,
    "cerebro-viewport-gate.is-sized",
    fixed = TRUE
  )
  expect_match(css, "visibility: hidden", fixed = TRUE)
  expect_match(css, "visibility: visible", fixed = TRUE)
  expect_false(grepl("transition:[^;}]*height", css, perl = TRUE))
})

test_that("trajectory selectors remain inside Main parameters", {
  tab_source <- js_source(
    "inst",
    "shiny",
    "v1.4",
    "trajectory",
    "UI.R"
  )
  projection_source <- js_source(
    "inst",
    "shiny",
    "v1.4",
    "trajectory",
    "projection.R"
  )

  expect_false(grepl(
    'uiOutput("trajectory_select_method_and_name_UI")',
    tab_source,
    fixed = TRUE
  ))
  expect_match(
    projection_source,
    'uiOutput("trajectory_select_method_and_name_UI")',
    fixed = TRUE
  )
})

test_that("Spatial background remains registered to Plotly data axes", {
  source <- js_source(
    "inst",
    "shiny",
    "v1.4",
    "spatial",
    "js_spatial_background.js"
  )

  expect_match(source, "xaxis.l2p", fixed = TRUE)
  expect_match(source, "yaxis.l2p", fixed = TRUE)
  expect_match(source, "plotly_afterplot", fixed = TRUE)
  expect_match(source, "applySpatialBackground", fixed = TRUE)
})
