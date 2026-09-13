# test-spatial-coexpression.R — unit tests for mapping 2-3 genes onto the RGB
# channels of a per-cell colour, for spatial co-expression visualisation.
#
# Each gene drives one channel; a cell's colour blends them, so spatial overlap
# (co-expression) reads as a mixed hue. Each channel is independently normalised
# to its own max, so a channel is only dark where that gene is low relative to
# its own range. A missing channel (NULL) contributes 0.

blend <- blend_genes_to_rgb

test_that("each channel scales to its own max independently", {
  # R gene peaks at 10, G gene peaks at 4; both cells at their own max -> full.
  out <- blend(r = c(0, 10), g = c(4, 0), b = NULL)
  expect_equal(out[1], "rgb(0,255,0)")
  expect_equal(out[2], "rgb(255,0,0)")
})

test_that("a zero channel stays zero across all cells", {
  out <- blend(r = c(1, 2, 3), g = NULL, b = NULL)
  expect_true(all(grepl("rgb\\([0-9]+,0,0\\)", out)))
})

test_that("mid-range values map to a proportional channel byte", {
  # single cell at half of max -> ~128
  out <- blend(r = c(0, 5, 10), g = NULL, b = NULL)
  expect_equal(out[2], "rgb(128,0,0)")
})

test_that("all three channels combine into one colour per cell", {
  out <- blend(r = c(10), g = c(10), b = c(10))
  expect_equal(out, "rgb(255,255,255)")
})

test_that("a flat (all-equal) channel maps to full intensity", {
  # max == min: treat as fully expressed rather than dividing by zero.
  out <- blend(r = c(5, 5, 5), g = NULL, b = NULL)
  expect_equal(out, c("rgb(255,0,0)", "rgb(255,0,0)", "rgb(255,0,0)"))
})

test_that("an all-zero channel maps to zero, not full", {
  out <- blend(r = c(0, 0, 0), g = NULL, b = NULL)
  expect_equal(out, c("rgb(0,0,0)", "rgb(0,0,0)", "rgb(0,0,0)"))
})

test_that("NA expression is treated as zero for that cell", {
  out <- blend(r = c(10, NA), g = NULL, b = NULL)
  expect_equal(out[2], "rgb(0,0,0)")
})

test_that("returns one colour string per cell", {
  out <- blend(r = c(1, 2, 3, 4), g = c(4, 3, 2, 1), b = NULL)
  expect_length(out, 4)
})

test_that("spatial RGB batches expression reads and reuses full ranges", {
  viewer_file <- viewer_test_path(
    "spatial",
    "obj_projection_data_to_plot.R"
  )
  lines <- readLines(viewer_file, warn = FALSE)
  rgb_start <- grep("^  ## Co-expression", lines)[[1L]]
  rgb_end <- grep("^  ## get colors", lines)[[1L]] - 1L
  rgb_lines <- lines[rgb_start:rgb_end]

  expression_read <- grep("viewerExpressionValues\\(", rgb_lines)[[1L]]
  channel_loop <- grep("for \\(channel in", rgb_lines)[[1L]]
  expect_lt(expression_read, channel_loop)
  expect_match(
    paste(lines, collapse = "\n"),
    "spatial_projection_full_ranges <- reactive",
    fixed = TRUE
  )
})
