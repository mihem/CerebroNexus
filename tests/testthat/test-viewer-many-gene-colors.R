test_that("distinct gene panels retain valid colors beyond nine genes", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("gene_expression", "func_color_scale.R"), runtime)
  genes <- paste0("gene", seq_len(20))
  scales <- runtime$expressionPanelColorScales(
    genes,
    "different",
    "Cerebro orange"
  )
  expect_identical(names(scales), genes)
  expect_length(scales, length(genes))
  expect_identical(scales[[1L]], scales[[10L]])
  expect_true(all(vapply(
    scales,
    function(scale) {
      length(scale) == 5L &&
        all(vapply(
          scale,
          function(stop) {
            length(stop) == 2L && grepl("^#[0-9A-Fa-f]{6}$", stop[[2L]])
          },
          logical(1)
        ))
    },
    logical(1)
  )))
})
