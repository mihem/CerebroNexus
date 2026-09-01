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
      "const timers = []; const controls = {}; const applied = [];",
      "global.window = {setTimeout: fn => timers.push(fn)};",
      "global.document = {",
      "  getElementById: id => id === 'shiny-tab-overview' ?",
      "    {classList:{contains:()=>true}} : (controls[id] || null),",
      "  querySelector: () => null",
      "};",
      "window.jQuery = global.jQuery = element => ({",
      "  data: () => element.binding",
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
  expect_match(adapter, "hla_motif_network", fixed = TRUE)
  expect_match(config, "hla_motif_network", fixed = TRUE)
})

test_that("specialist restoration retries dynamically rendered controls", {
  output <- run_specialist_state_node(c(
    "window.cerebroCellViews = {applyState: () => {",
    "  controls.overview_dynamic = {binding:{receiveMessage: (_el, msg) => applied.push(msg.value)}};",
    "}};",
    "window.cerebroSpecialistViews.get('overview_projection').apply({",
    "  selection:{cells:['cell-1']},",
    "  controls:[{id:'overview_dynamic',multiple:false,values:['restored']}]",
    "});",
    "while (timers.length) timers.shift()();",
    "console.log(JSON.stringify(applied));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list("restored", "restored")
  )
})
