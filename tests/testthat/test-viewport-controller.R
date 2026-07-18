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

test_that("viewport controller owns the shared pure sizing contract", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = { addEventListener: function () {}, requestAnimationFrame: function () {} };",
      "global.document = { addEventListener: function () {}, body: null, documentElement: null };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const api = window.cerebroViewport;",
      "console.log(JSON.stringify([",
      "  api._targetHeight(900, 120, 70, 18, 240),",
      "  api._targetHeight(520, 250, 80, 18, 240),",
      "  api._shouldReveal(undefined, 754),",
      "  api._shouldReveal(775, 754),",
      "  api._shouldReveal(754, 754)",
      "]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[692,240,false,false,true]")
})

test_that("viewport rect measurement is stable when the host grows", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = { addEventListener: function () {}, requestAnimationFrame: function () {} };",
      "global.document = { addEventListener: function () {}, body: null, documentElement: null };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "let hostBottom = 700; let boxBottom = 780;",
      "const box = { getBoundingClientRect: () => ({ bottom: boxBottom }) };",
      "const host = {",
      "  closest: selector => selector === '.box' ? box : null,",
      "  getBoundingClientRect: () => ({ bottom: hostBottom })",
      "};",
      "const below = window.cerebroViewport._contentBelow;",
      "const first = below(host);",
      "hostBottom = 800; boxBottom = 880;",
      "console.log(JSON.stringify([first, below(host)]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[80,80]")
})

test_that("viewport observation stops at content and excludes body", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = { addEventListener: function () {}, requestAnimationFrame: function () {} };",
      "global.document = {",
      "  addEventListener: function () {},",
      "  body: { name: 'body' }, documentElement: { name: 'html' }",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const content = { name: 'content', classList: { contains: x => x === 'content' }, parentElement: document.body };",
      "const parent = { name: 'parent', classList: { contains: () => false }, parentElement: content };",
      "const host = { name: 'host', classList: { contains: () => false }, parentElement: parent };",
      "console.log(JSON.stringify(window.cerebroViewport._layoutTargets(host).map(x => x.name)));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, '["host","parent","content"]')
})

test_that("an unready adapter cannot prime or bypass the settle gate", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const frames = [];",
      "global.window = {",
      "  innerHeight: 900,",
      "  addEventListener: function () {},",
      "  requestAnimationFrame: fn => { frames.push(fn); return frames.length; }",
      "};",
      "global.document = { body: null, documentElement: null };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "let ready = false; let revealed = false;",
      "const host = { getClientRects: () => [{ width: 800, height: 600 }] };",
      "const adapter = {",
      "  measure: () => ({ top: 120, contentBelow: 70, width: 800 }),",
      "  apply: function () {},",
      "  ready: () => ready,",
      "  isRevealed: () => revealed,",
      "  reveal: () => { revealed = true; }",
      "};",
      "const state = window.cerebroViewport.register(host, adapter);",
      "frames.shift()();",
      "const beforeReady = state.settledHeight;",
      "ready = true; window.cerebroViewport.resize(host); frames.shift()();",
      "const afterFirstReadyFrame = [state.settledHeight, revealed];",
      "frames.shift()();",
      "console.log(JSON.stringify([beforeReady, afterFirstReadyFrame, revealed]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[null,[692,false],true]")
})

test_that("a viewport stage reveals its enclosing card, not an inner plot", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const frames = [];",
      "global.window = {",
      "  innerHeight: 900,",
      "  addEventListener: function () {},",
      "  requestAnimationFrame: fn => { frames.push(fn); return frames.length; }",
      "};",
      "global.document = {",
      "  addEventListener: function () {},",
      "  getElementsByClassName: function () { return []; },",
      "  body: null, documentElement: null",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const added = [];",
      "const gate = { classList: {",
      "  add: c => added.push(c),",
      "  remove: function () {},",
      "  contains: c => added.includes(c)",
      "} };",
      "const box = { getBoundingClientRect: () => ({ bottom: 780 }) };",
      "const host = {",
      "  style: {},",
      "  getClientRects: () => [{ width: 800, height: 600 }],",
      "  getBoundingClientRect: () => ({ top: 120, bottom: 700, width: 800 }),",
      "  querySelector: () => null,",
      "  closest: selector => selector === '.box' ? box : gate",
      "};",
      "const api = window.cerebroViewport;",
      "api.register(host, api._stageAdapter);",
      "frames.shift()();",
      "const first = [host.style.height, added.slice()];",
      "frames.shift()();",
      "console.log(JSON.stringify([first, host.style.height, added]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, '[["682px",[]],"682px",["is-sized"]]')
})

test_that("detached Shiny hosts are pruned before global resize", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const frames = [];",
      "global.window = {",
      "  innerHeight: 900,",
      "  addEventListener: function () {},",
      "  requestAnimationFrame: fn => { frames.push(fn); return frames.length; }",
      "};",
      "global.document = {",
      "  addEventListener: function () {},",
      "  getElementsByClassName: function () { return []; },",
      "  body: null, documentElement: null",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const host = { isConnected: true, getClientRects: () => [{}] };",
      "const adapter = {",
      "  measure: () => ({ top: 120, contentBelow: 70, width: 800 }),",
      "  apply: function () {}, isRevealed: () => true",
      "};",
      "window.cerebroViewport.register(host, adapter);",
      "frames.shift()();",
      "host.isConnected = false;",
      "window.cerebroViewport.resizeAll();",
      "console.log(frames.length);"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "0")
})

test_that("visual changes register only their nearest viewport stage", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "let scans = 0; let visualHandler = null;",
      "const gate = { classList: { add: function () {}, remove: function () {}, contains: () => false } };",
      "const host = {",
      "  isConnected: true, style: { removeProperty: function () {} },",
      "  classList: { contains: () => false, toggle: function () {} },",
      "  getClientRects: () => [{}], querySelector: () => null,",
      "  closest: selector => selector === '.cerebro-viewport-gate' ? gate : null",
      "};",
      "const child = { closest: selector => selector === '.cerebro-viewport-host' ? host : null };",
      "global.window = {",
      "  addEventListener: function () {}, requestAnimationFrame: function () { return 1; },",
      "  jQuery: function () { return { on: function (names, handler) { visualHandler = handler; } }; }",
      "};",
      "global.document = {",
      "  addEventListener: function () {}, body: null, documentElement: null,",
      "  getElementsByClassName: function () { scans++; return [host]; }",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "visualHandler({ target: child });",
      "console.log(scans);"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "1")
})

test_that("register prepares an adapter only once before its frame", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const frames = []; let prepares = 0;",
      "global.window = {",
      "  innerHeight: 900, addEventListener: function () {},",
      "  requestAnimationFrame: fn => { frames.push(fn); return frames.length; }",
      "};",
      "global.document = { addEventListener: function () {}, body: null, documentElement: null };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const host = { getClientRects: () => [{}] };",
      "const adapter = {",
      "  prepare: () => { prepares++; },",
      "  measure: () => ({ top: 120, contentBelow: 70, width: 800 }),",
      "  apply: function () {}, isRevealed: () => true",
      "};",
      "window.cerebroViewport.register(host, adapter);",
      "frames.shift()();",
      "console.log(prepares);"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "1")
})

test_that("an empty uiOutput becomes ready after receiving a Shiny value", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file("inst", "shiny", "v1.4", "www", "viewport.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const handlers = {};",
      "global.window = { addEventListener: function () {}, requestAnimationFrame: function () { return 1; } };",
      "global.document = {",
      "  addEventListener: (name, handler) => { handlers[name] = handler; },",
      "  getElementsByClassName: () => [], body: null, documentElement: null",
      "};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const output = {",
      "  childNodes: [],",
      "  classList: { contains: name => name === 'shiny-html-output' }",
      "};",
      "const pane = {",
      "  querySelector: () => null,",
      "  querySelectorAll: selector => selector === '.shiny-html-output' ? [output] : []",
      "};",
      "const gate = { classList: { add: function () {}, remove: function () {}, contains: () => false } };",
      "const host = {",
      "  style: { removeProperty: function () {} },",
      "  classList: { contains: () => false, toggle: function () {} },",
      "  querySelector: selector => selector === '.tab-pane.active' ? pane : null,",
      "  closest: selector => selector === '.cerebro-viewport-gate' ? gate : null",
      "};",
      "output.closest = selector => selector === '.cerebro-viewport-host' ? host : null;",
      "const adapter = window.cerebroViewport._stageAdapter;",
      "const state = { relayoutPending: false };",
      "const before = adapter.ready(host, 240, 300, state);",
      "handlers['shiny:value']({ target: output });",
      "const after = adapter.ready(host, 240, 300, state);",
      "console.log(JSON.stringify([before, after]));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_equal(output, "[false,true]")
})
