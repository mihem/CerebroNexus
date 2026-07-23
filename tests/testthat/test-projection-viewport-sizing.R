repo_file <- function(...) {
  parts <- c(...)
  stripped <- if (length(parts) && identical(parts[[1L]], "inst")) {
    parts[-1L]
  } else {
    parts
  }
  # 1) installed package (R CMD check) or load_all-shimmed source location
  if (length(stripped)) {
    p <- system.file(
      do.call(file.path, as.list(stripped)),
      package = "CerebroNexus"
    )
    if (nzchar(p)) {
      return(p)
    }
  }
  # 2) fall back to the source tree (devtools::test_dir run from the repo)
  testthat::test_path("..", "..", ...)
}

test_that("projection height is calculated from measured viewport geometry", {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
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
      "global.window = {};",
      "global.document = { addEventListener: function () {} };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const target = window.cerebroProjection._projectionTargetHeight;",
      "console.log(JSON.stringify([",
      "  target(900, 120, 70, 18, 240),",
      "  target(900, 170, 70, 18, 240),",
      "  target(520, 250, 80, 18, 240),",
      "  target(1000, 120, 70, 18, 240)",
      "]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[692,642,240,792]")
})

test_that("generic fill sizing skips elements outside layout", {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "fill_height.js"
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = { addEventListener: function () {}, requestAnimationFrame: function () {} };",
      "global.document = {",
      "  addEventListener: function () {},",
      "  getElementsByClassName: function () { return []; },",
      "  body: null",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const visible = window.cerebroFill._isVisible;",
      "console.log(JSON.stringify([",
      "  visible({ getClientRects: () => [] }),",
      "  visible({ getClientRects: () => [{ width: 10, height: 10 }] })",
      "]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[false,true]")
})

test_that("generic fill observes only its content ancestry", {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "fill_height.js"
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = { addEventListener: function () {}, requestAnimationFrame: function () {} };",
      "global.document = {",
      "  addEventListener: function () {},",
      "  getElementsByClassName: function () { return []; },",
      "  body: { name: 'body' },",
      "  documentElement: { name: 'html' }",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const content = { name: 'content', classList: { contains: x => x === 'content' }, parentElement: document.body };",
      "const parent = { name: 'parent', classList: { contains: () => false }, parentElement: content };",
      "const fill = { name: 'fill', classList: { contains: () => false }, parentElement: parent };",
      "console.log(JSON.stringify(window.cerebroFill._layoutTargets(fill).map(x => x.name)));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, '["fill","parent","content"]')

  source <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("observe(document.body)", source, fixed = TRUE))
})

test_that("content below is stable when the fill itself changes height", {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "fill_height.js"
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = {",
      "  addEventListener: function () {},",
      "  requestAnimationFrame: function () {},",
      "  getComputedStyle: node => ({ paddingBottom: node.paddingBottom || '0px' })",
      "};",
      "global.document = {",
      "  addEventListener: function () {},",
      "  getElementsByClassName: function () { return []; },",
      "  body: null,",
      "  documentElement: null",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const wrapper = { classList: { contains: x => x === 'content-wrapper' }, paddingBottom: '18px', parentElement: null };",
      "const content = { classList: { contains: x => x === 'content' }, scrollHeight: 760, scrollTop: 0, parentElement: wrapper, getBoundingClientRect: () => ({ top: 20 }) };",
      "let bottom = 700;",
      "const fill = { classList: { contains: () => false }, parentElement: content, getBoundingClientRect: () => ({ bottom }) };",
      "const below = window.cerebroFill._contentBelow;",
      "const first = below(fill);",
      "bottom = 800; content.scrollHeight = 860;",
      "const second = below(fill);",
      "console.log(JSON.stringify([first, second]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[98,98]")
})

test_that("generic fill reveals only after the height settles across two frames", {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "fill_height.js"
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = { addEventListener: function () {}, requestAnimationFrame: function () {} };",
      "global.document = {",
      "  addEventListener: function () {},",
      "  getElementsByClassName: function () { return []; },",
      "  body: null",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const should = window.cerebroFill._shouldReveal;",
      "console.log(JSON.stringify([",
      "  should(undefined, 754),", # first measurement -> wait
      "  should(775, 754),", # height changed (775->754) -> wait
      "  should(754, 754)", # two equal frames -> reveal
      "]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[false,false,true]")
})

test_that("generic fill never animates height, only opacity", {
  css <- paste(
    readLines(repo_file("inst", "shiny", "v1.4", "www", "custom.css")),
    collapse = "\n"
  )

  # The height-grow flash fix: the is-filled reveal fades opacity only; the
  # height is applied in one frame (settled before reveal), never tweened. Assert
  # the opacity-only declaration exists and the old height tween is gone.
  expect_match(css, "transition: opacity 0.12s ease;", fixed = TRUE)
  expect_false(grepl("height 0.2s ease", css, fixed = TRUE))
})

test_that("content wrapper prefers dynamic viewport units with a fallback", {
  css <- paste(
    readLines(repo_file("inst", "shiny", "v1.4", "www", "custom.css")),
    collapse = "\n"
  )

  expect_match(css, "height: 100vh;", fixed = TRUE)
  expect_match(css, "height: 100dvh;", fixed = TRUE)
})

test_that("generic fill wrappers do not clip widget controls", {
  css <- paste(
    readLines(repo_file("inst", "shiny", "v1.4", "www", "custom.css")),
    collapse = "\n"
  )
  fill_rule <- regmatches(
    css,
    regexpr("\\.cerebro-fill \\{[^}]+\\}", css, perl = TRUE)
  )

  expect_length(fill_rule, 1L)
  expect_false(grepl("overflow: hidden", fill_rule, fixed = TRUE))
})

test_that("all projection tabs delegate live height to the shared controller", {
  ui_paths <- c(
    repo_file("inst", "shiny", "v1.4", "overview", "UI_projection.R"),
    repo_file("inst", "shiny", "v1.4", "gene_expression", "UI_projection.R"),
    repo_file("inst", "shiny", "v1.4", "spatial", "UI_projection.R"),
    repo_file("inst", "shiny", "v1.4", "trajectory", "projection.R")
  )
  ui_source <- paste(unlist(lapply(ui_paths, readLines)), collapse = "\n")

  expect_false(grepl("calc\\(100vh - [0-9]+px\\)", ui_source))
  # Every projection output is wrapped in a gate div so the flash-suppression
  # rule in custom.css applies from first paint (see the flash tests below).
  expect_equal(
    lengths(regmatches(
      ui_source,
      gregexpr("cerebro-projection-gate", ui_source, fixed = TRUE)
    )),
    4L
  )
})

test_that("projection sizing isolates Plotly from surrounding box content", {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
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
      "global.window = {};",
      "global.document = { addEventListener: function () {} };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const sizingElement = window.cerebroProjection._projectionSizingElement;",
      "const plainParent = { classList: { contains: () => false } };",
      "const spinnerParent = { classList: { contains: (x) => x === 'shiny-spinner-output-container' } };",
      "const plainPlot = { parentElement: plainParent };",
      "const spinnerPlot = { parentElement: spinnerParent };",
      "console.log(sizingElement(plainPlot) === plainPlot);",
      "console.log(sizingElement(spinnerPlot) === spinnerParent);"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, c("true", "true"))
})

test_that("trajectory selectors live inside Main parameters", {
  tab_source <- paste(
    readLines(repo_file("inst", "shiny", "v1.4", "trajectory", "UI.R")),
    collapse = "\n"
  )
  projection_source <- paste(
    readLines(repo_file(
      "inst",
      "shiny",
      "v1.4",
      "trajectory",
      "projection.R"
    )),
    collapse = "\n"
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
  expect_match(
    projection_source,
    paste0(
      'tagList\\(\\s*',
      'uiOutput\\("trajectory_select_method_and_name_UI"\\),\\s*',
      'uiOutput\\("trajectory_projection_main_parameters_UI"\\)'
    ),
    perl = TRUE
  )
})

test_that("shared controller observes wrapped legends and resizes Plotly", {
  source <- paste(
    readLines(repo_file(
      "inst",
      "shiny",
      "v1.4",
      "www",
      "projection_scatter.js"
    )),
    collapse = "\n"
  )

  expect_match(source, "ResizeObserver", fixed = TRUE)
  expect_match(source, "requestAnimationFrame", fixed = TRUE)
  expect_match(source, "Plotly.relayout", fixed = TRUE)
  expect_match(source, "Plotly.Plots.resize", fixed = TRUE)
  expect_match(source, "scheduleProjectionResize", fixed = TRUE)
})

test_that("reveal waits for two equal measurements so the first frame is settled", {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
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
      "global.window = {};",
      "global.document = { addEventListener: function () {} };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const should = window.cerebroProjection._shouldRevealProjection;",
      "console.log(JSON.stringify([",
      # (fullLayoutPresent, height, settledHeight)
      "  should(false, 775, null),", # no data yet -> false
      "  should(true, 775, null),", # first measurement, nothing to match -> false
      "  should(true, 754, 775),", # measurement changed (775->754) -> false
      "  should(true, 754, 754)", # two equal measurements -> reveal
      "]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[false,false,false,true]")
})

test_that("reveal marks the gate wrapper sized, not the plot itself", {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
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
      "global.window = {};",
      "global.document = { addEventListener: function () {} };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const api = window.cerebroProjection;",
      "const gateClass = api._projectionGateClass;",
      "const sizedClass = api._projectionSizedClass;",
      "const added = [];",
      # A stub gate the plot resolves to via closest(); reveal must add is-sized
      # to THIS wrapper, never to the plot div (htmlwidgets would drop that).
      "const gate = { classList: { add: (c) => added.push(c) } };",
      "let closestArg = null;",
      "const plot = { closest: (sel) => { closestArg = sel; return gate; } };",
      "api._revealProjectionHost(plot);",
      "console.log(JSON.stringify([gateClass, sizedClass, closestArg, added]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(
    output,
    paste0(
      "[\"cerebro-projection-gate\",\"is-sized\",",
      "\".cerebro-projection-gate\",[\"is-sized\"]]"
    )
  )
})

test_that("CSS hides projection outputs until the resize path reveals them", {
  js_source <- paste(
    readLines(repo_file(
      "inst",
      "shiny",
      "v1.4",
      "www",
      "projection_scatter.js"
    )),
    collapse = "\n"
  )
  css_source <- paste(
    readLines(repo_file("inst", "shiny", "v1.4", "www", "custom.css")),
    collapse = "\n"
  )

  # The gate class name is a single source of truth in the JS and matches UI/CSS.
  expect_match(
    js_source,
    "PROJECTION_GATE_CLASS = 'cerebro-projection-gate'",
    fixed = TRUE
  )
  # CSS hides the gate itself from first paint (before any JS can run); the plot
  # inherits hidden. is-sized flips the gate back to visible. Hiding the gate
  # (not the plot div) is required: plotly writes an inline visibility on its own
  # div that would beat any stylesheet rule targeting that div.
  expect_match(
    css_source,
    paste0(
      "\\.cerebro-projection-gate[\\s]{0,4}\\{[\\s\\S]{0,40}?",
      "visibility:[\\s]{0,4}hidden"
    ),
    perl = TRUE
  )
  expect_match(
    css_source,
    paste0(
      "\\.cerebro-projection-gate\\.is-sized[\\s]{0,4}\\{[\\s\\S]{0,40}?",
      "visibility:[\\s]{0,4}visible"
    ),
    perl = TRUE
  )
  # The measured-resize path reveals AFTER writing the DOM height, and only once
  # Plotly has drawn data (a _fullLayout exists) AND the measured height has
  # stabilised (shouldRevealProjection), so the placeholder never shows and the
  # first visible frame is already the final size.
  expect_match(
    js_source,
    paste0(
      "state &&[\\s\\S]{0,60}?fullLayout &&[\\s\\S]{0,40}?gate &&[\\s\\S]{0,80}?",
      "!gate\\.classList\\.contains\\(PROJECTION_SIZED_CLASS\\)",
      "[\\s\\S]{0,220}?",
      "shouldRevealProjection\\(fullLayout, height, state\\.settledHeight\\)",
      "[\\s\\S]{0,140}?revealProjectionHost\\(elements\\.plot\\)"
    ),
    perl = TRUE
  )
  # When not yet stable, it records the height and forces a confirming resize.
  expect_match(
    js_source,
    paste0(
      "state\\.settledHeight = height;[\\s\\S]{0,80}?",
      "scheduleProjectionResize\\(plotId\\)"
    ),
    perl = TRUE
  )
  # Reveal state is keyed to the gate element's is-sized class (checked above),
  # NOT a plotId-keyed set — so a host that is removed and recreated (e.g. the IR
  # Clonal UMAP when faceting toggles) reveals again instead of staying hidden.
  expect_false(grepl("projectionRevealed", js_source, fixed = TRUE))
})

test_that("Spatial background remains registered to Plotly data axes", {
  source <- paste(
    readLines(repo_file(
      "inst",
      "shiny",
      "v1.4",
      "spatial",
      "js_spatial_background.js"
    )),
    collapse = "\n"
  )

  expect_match(source, "xaxis.l2p", fixed = TRUE)
  expect_match(source, "yaxis.l2p", fixed = TRUE)
  expect_match(source, "plotly_afterplot", fixed = TRUE)
  expect_match(source, "applySpatialBackground", fixed = TRUE)
})
