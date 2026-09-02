viewer_hla_text <- function(path) {
  paste(
    readLines(viewer_test_path(path), warn = FALSE),
    collapse = "\n"
  )
}

run_specialist_state_node <- function(body) {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const timers = []; const controls = {}; const applied = []; const handlers = {};",
      "const windowHandlers = {};",
      "global.window = {",
      "  setTimeout: (fn, delay) => { fn.delay = delay; timers.push(fn); return fn; },",
      "  clearTimeout: fn => { const at = timers.indexOf(fn); if (at >= 0) timers.splice(at, 1); },",
      "  addEventListener: (name, fn) => { windowHandlers[name] = fn; }",
      "};",
      "global.document = {",
      "  getElementById: id => id === 'shiny-tab-overview' ?",
      "    {classList:{contains:()=>true},querySelectorAll:()=>[]} : (controls[id] || null),",
      "  querySelector: () => null",
      "};",
      "window.jQuery = global.jQuery = element => ({",
      "  data: () => element.binding,",
      "  on: (event, handler) => { handlers[event] = handler; },",
      "  off: event => { delete handlers[event]; }",
      "});",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(
          viewer_test_path("www", "specialist-view-state.js"),
          quote = "\""
        )
      ),
      body
    ),
    runner
  )
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("HLA node selection maps cleanly between motif keys and cells", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("hla_tcr_motifs", "core", "hla_visual_helpers.R"),
    envir = runtime
  )
  segments <- data.frame(
    barcode = c("cell-1", "cell-2", "cell-3"),
    v_gene = c("TRBV1", "TRBV1", "TRBV2"),
    cdr3 = c("CASSA", "CASSA", "CASSB"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    runtime$hla_segment_node_keys(segments, by_v = FALSE),
    c("CASSA", "CASSA", "CASSB")
  )
  expect_identical(
    runtime$hla_segment_node_keys(segments, by_v = TRUE),
    c("TRBV1::CASSA", "TRBV1::CASSA", "TRBV2::CASSB")
  )
  expect_identical(
    runtime$hla_cells_for_node_keys(segments, "CASSA", by_v = FALSE),
    c("cell-1", "cell-2")
  )
  expect_identical(
    runtime$hla_node_keys_for_cells(segments, "cell-3", by_v = TRUE),
    "TRBV2::CASSB"
  )
})

test_that("HLA exposes the shared cohort controls and a network saved-view adapter", {
  ui <- viewer_hla_text("hla_tcr_motifs/UI.R")
  visual <- viewer_hla_text("hla_tcr_motifs/visualizations.R")
  server <- viewer_hla_text("hla_tcr_motifs/network_table.R")
  client <- viewer_hla_text("www/hla_motifs.js")
  adapter <- viewer_hla_text("www/specialist-view-state.js")
  config <- viewer_hla_text("coordinated_views/config.R")

  expect_match(ui, 'cerebroSelectionStatus(', fixed = TRUE)
  expect_match(ui, '"hla_motif_network"', fixed = TRUE)
  expect_match(visual, "node_key", fixed = TRUE)
  expect_match(visual, "handleNativeSelection", fixed = TRUE)
  expect_match(server, "hla_motif_selected_keys", fixed = TRUE)
  expect_match(server, "hla_motif_selection_command", fixed = TRUE)
  expect_match(server, "node_keys = I(keys)", fixed = TRUE)
  expect_match(server, "cells = I(cells)", fixed = TRUE)
  expect_match(client, "DOMtoCanvas", fixed = TRUE)
  expect_match(client, "canvasToDOM", fixed = TRUE)
  expect_match(client, "hla_motif_selected_keys", fixed = TRUE)
  expect_match(client, "captureState", fixed = TRUE)
  expect_match(client, "applyState", fixed = TRUE)
  expect_match(client, "downloadPNG", fixed = TRUE)
  expect_match(client, "cerebro:png-result", fixed = TRUE)
  expect_match(adapter, "hla_motif_network", fixed = TRUE)
  expect_match(adapter, "downloadPNG", fixed = TRUE)
  expect_match(config, "hla_motif_network", fixed = TRUE)
})

test_that("Projection adapter captures shared JSON and downloads its PNG", {
  output <- run_specialist_state_node(c(
    "const calls = [];",
    "window.cerebroSavedViewDataset = {cell_count:2,cell_fingerprint:'cells'};",
    "window.cerebroCellViews = {",
    "  captureState: id => ({cells:['cell-1'],geometry:null,view:{mode:'lasso'}}),",
    "  downloadPNG: id => { calls.push(id); return true; }",
    "};",
    "windowHandlers['cerebro:specialist-state']({detail:{",
    "  viewId:'overview_projection',datasetFingerprint:'cells'}});",
    "const adapter = window.cerebroSpecialistViews.get('overview_projection');",
    "const captured = adapter.capture();",
    "console.log(JSON.stringify({schema:captured.schema,page:captured.page.id,",
    "  cells:captured.selection.cells,downloaded:adapter.downloadPNG(),calls:calls}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      schema = "cerebronexus-specialist-view",
      page = "overview_projection",
      cells = list("cell-1"),
      downloaded = TRUE,
      calls = list("overview_projection")
    )
  )
})

test_that("HLA PNG adapter reports browser export success and failure", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(c(
    "const fs = require('fs');",
    "global.window = global;",
    "let fail = false; let clicks = 0;",
    "const canvas = {toDataURL: () => { if (fail) throw Error('blocked'); return 'data:image/png'; }};",
    "window.HTMLWidgets = {find: () => ({network:{canvas:{frame:{canvas:canvas}}}})};",
    "global.document = {readyState:'loading',addEventListener:()=>{},",
    "  createElement:()=>({click:()=>{ clicks += 1; }}),getElementById:()=>null};",
    sprintf(
      "eval(fs.readFileSync(%s, 'utf8'));",
      encodeString(viewer_test_path("www", "hla_motifs.js"), quote = "\"")
    ),
    "const success = window.cerebroHlaMotifs.downloadPNG();",
    "fail = true; const failure = window.cerebroHlaMotifs.downloadPNG();",
    "console.log(JSON.stringify({success:success,failure:failure,clicks:clicks}));"
  ), runner)
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(success = TRUE, failure = FALSE, clicks = 1L)
  )
})

test_that("successful specialist restoration does not replay stale state", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: (_id, config) => {",
    "  applied.push(config.selection.cells[0]); return {selectedCells:1};",
    "}};",
    "const adapter = window.cerebroSpecialistViews.get('overview_projection');",
    "adapter.apply({selection:{cells:['old']},controls:[]});",
    "adapter.apply({selection:{cells:['new']},controls:[]});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("new")
  )
})

test_that("specialist sharing waits for state from the current dataset", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {captureState: () => ({cells:['cell-1']})};",
    "const adapter = window.cerebroSpecialistViews.get('overview_projection');",
    "window.cerebroSavedViewDataset = {cell_fingerprint:'dataset-a'};",
    "windowHandlers['cerebro:specialist-state']({detail:{",
    "  viewId:'overview_projection',datasetFingerprint:'dataset-a'}});",
    "const before = adapter.ready();",
    "window.cerebroSavedViewDataset = {cell_fingerprint:'dataset-b'};",
    "console.log(JSON.stringify({before:before,after:adapter.ready()}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(before = TRUE, after = FALSE)
  )
})

test_that("specialist restoration follows dynamically bound controls", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {",
    "  if (controls.overview_dynamic) return;",
    "  controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "    receiveMessage: (_el, msg) => applied.push(msg.value)",
    "  }};",
    "  Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "    .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("restored")
  )
})

test_that("specialist restoration waits for late Shiny-bound controls", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "timers.filter(fn => fn.delay <= 750).forEach(fn => fn());",
    "controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("restored")
  )
})

test_that("specialist restoration keeps waiting for unresolved controls", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "console.log(JSON.stringify({applied:applied,handlers:Object.keys(handlers)}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(applied = list("restored"), handlers = list())
  )
})

test_that("specialist restoration retains only unresolved controls after the grace period", {
  output <- run_specialist_state_node(c(
    "controls.overview_static = {id:'overview_static',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "window.cerebroCellViews = {applyState: () => {}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[",
    "    {id:'overview_static',multiple:false,values:['static restored']},",
    "    {id:'overview_dynamic',multiple:false,values:['dynamic restored']}",
    "  ]",
    "});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_static}));",
    "controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "console.log(JSON.stringify({applied:applied,handlers:Object.keys(handlers)}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      applied = list("static restored", "dynamic restored"),
      handlers = list()
    )
  )
})

test_that("specialist restoration retries a replaced binding that initially throws", {
  output <- run_specialist_state_node(c(
    "controls.overview_dynamic = {id:'overview_dynamic',binding:{",
    "  receiveMessage: () => { throw new Error('replaced'); }",
    "}};",
    "window.cerebroCellViews = {applyState: () => {}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "controls.overview_dynamic.binding.receiveMessage =",
    "  (_el, msg) => applied.push(msg.value);",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_dynamic}));",
    "console.log(JSON.stringify({applied:applied,handlers:Object.keys(handlers)}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(applied = list("restored"), handlers = list())
  )
})

test_that("a new specialist restore cancels unresolved controls from the previous one", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {}};",
    "const adapter = window.cerebroSpecialistViews.get('overview_projection');",
    "adapter.apply({selection:{cells:['cell-1']},controls:[",
    "  {id:'overview_old',multiple:false,values:['old']}",
    "]});",
    "timers.sort((a, b) => a.delay - b.delay);",
    "while (timers.length) timers.shift()();",
    "controls.overview_new = {id:'overview_new',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "adapter.apply({selection:{cells:['cell-1']},controls:[",
    "  {id:'overview_new',multiple:false,values:['new']}",
    "]});",
    "controls.overview_old = {id:'overview_old',binding:{",
    "  receiveMessage: (_el, msg) => applied.push(msg.value)",
    "}};",
    "Object.keys(handlers).filter(name => name.indexOf('shiny:bound') === 0)",
    "  .forEach(name => handlers[name]({target:controls.overview_old}));",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("new")
  )
})
