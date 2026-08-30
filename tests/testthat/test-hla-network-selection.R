viewer_hla_text <- function(path) {
  paste(
    readLines(viewer_test_path(path), warn = FALSE),
    collapse = "\n"
  )
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

test_that("HLA exposes shared cohort controls and network state hooks", {
  ui <- viewer_hla_text("hla_tcr_motifs/UI.R")
  visual <- viewer_hla_text("hla_tcr_motifs/visualizations.R")
  server <- viewer_hla_text("hla_tcr_motifs/network_table.R")
  client <- viewer_hla_text("www/hla_motifs.js")

  expect_match(ui, 'cerebroSelectionStatus(', fixed = TRUE)
  expect_match(ui, '"hla_motif_network"', fixed = TRUE)
  expect_match(visual, "node_key", fixed = TRUE)
  expect_match(visual, "handleNativeSelection", fixed = TRUE)
  expect_match(server, "hla_motif_selected_keys", fixed = TRUE)
  expect_match(server, "hla_motif_selection_command", fixed = TRUE)
  expect_match(client, "DOMtoCanvas", fixed = TRUE)
  expect_match(client, "canvasToDOM", fixed = TRUE)
  expect_match(client, "hla_motif_selected_keys", fixed = TRUE)
  expect_match(client, "captureState", fixed = TRUE)
  expect_match(client, "applyState", fixed = TRUE)
})
