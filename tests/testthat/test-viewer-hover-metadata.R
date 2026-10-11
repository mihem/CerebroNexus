test_that("hover preserves full metadata text and cell order without requiring QC metrics", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), env)
  env$getGroups <- function() "group"
  table <- data.frame(
    cell_barcode = c("c3", "c1", "c2"),
    nUMI = c(1234, NA, 7),
    nGene = c(200, 0, 3),
    group = c("B", "A", NA)
  )
  original <- glue::glue(
    "<b>Cell</b>: {table[[ 'cell_barcode' ]]}<br>",
    "<b>Transcripts</b>: {formatC(table[[ 'nUMI' ]], format = 'f', big.mark = ',', digits = 0)}<br>",
    "<b>Expressed genes</b>: {formatC(table[[ 'nGene' ]], format = 'f', big.mark = ',', digits = 0)}"
  )
  original <- glue::glue("{original}<br><b>group</b>: {table[['group']]}")
  expect_identical(env$buildHoverInfoForProjections(table), original)

  no_qc <- table[c("cell_barcode", "group")]
  expect_identical(
    as.character(env$buildHoverInfoForProjections(no_qc)),
    c(
      "<b>Cell</b>: c3<br><b>group</b>: B",
      "<b>Cell</b>: c1<br><b>group</b>: A",
      "<b>Cell</b>: c2<br><b>group</b>: NA"
    )
  )
  one_metric <- as.character(env$buildHoverInfoForProjections(table[c(
    "cell_barcode",
    "nGene",
    "group"
  )]))
  expect_true(all(grepl("<b>Expressed genes</b>:", one_metric, fixed = TRUE)))
  expect_false(any(grepl("Transcripts", one_metric, fixed = TRUE)))
  expect_length(env$buildHoverInfoForProjections(no_qc[FALSE, ]), 0L)
  rownames(no_qc) <- no_qc$cell_barcode
  no_qc$cell_barcode <- NULL
  expect_identical(
    env$buildHoverInfoForProjections(no_qc),
    env$buildHoverInfoForProjections(table[c("cell_barcode", "group")])
  )
})
