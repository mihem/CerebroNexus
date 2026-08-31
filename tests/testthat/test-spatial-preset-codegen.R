# test-spatial-preset-codegen.R — unit tests for turning a hand-tuned overlay
# alignment into pasteable Cerebro.options preset code.
#
# After a user nudges the histology overlay into place in the Spatial tab, they
# need those numbers as `spatial_images_*` presets in app.R so the demo opens
# pre-aligned. This generator produces that snippet from the current control
# values for the current dataset label. It emits only the six supported options
# (offset_x/y, scale_x/y, flip_x/y) and only the non-identity ones, so a clean
# alignment yields a short snippet.

codegen <- format_spatial_preset_code

test_that("emits every option under the canonical image identity", {
  out <- codegen(
    dataset = "Spatial atlas",
    spatial_name = "Mouse brain (Visium)",
    image_label = "H&E",
    offset_x = 500,
    offset_y = -1000,
    scale_x = 1.55,
    scale_y = 1.55,
    flip_x = FALSE,
    flip_y = TRUE,
    rotation = 0,
    image_opacity = 0.8
  )
  expect_true(grepl(
    '"Spatial atlas" = list(',
    out,
    fixed = TRUE
  ))
  expect_true(grepl(
    '"Mouse brain (Visium)" = list(',
    out,
    fixed = TRUE
  ))
  expect_true(grepl(
    '"H&E" = list(',
    out,
    fixed = TRUE
  ))
  expect_true(grepl(
    'offset_x = 500',
    out,
    fixed = TRUE
  ))
  expect_true(grepl(
    'flip_y = TRUE',
    out,
    fixed = TRUE
  ))
  expect_match(out, "image_opacity = 0.8", fixed = TRUE)
})

test_that("preserves identity values in a complete image preset", {
  out <- codegen(
    dataset = "Atlas",
    spatial_name = "X",
    image_label = "None",
    offset_x = 0,
    offset_y = 0,
    scale_x = 1,
    scale_y = 1,
    flip_x = FALSE,
    flip_y = FALSE,
    rotation = 0
  )
  expect_match(out, "offset_x = 0", fixed = TRUE)
  expect_match(out, "scale_x = 1", fixed = TRUE)
  expect_match(out, "flip_x = FALSE", fixed = TRUE)
})

test_that("a fully-identity alignment remains pasteable", {
  out <- codegen(
    dataset = "Atlas",
    spatial_name = "X",
    image_label = "H&E",
    offset_x = 0,
    offset_y = 0,
    scale_x = 1,
    scale_y = 1,
    flip_x = FALSE,
    flip_y = FALSE,
    rotation = 0
  )
  expect_match(out, "spatial_image_settings = list(", fixed = TRUE)
  expect_match(out, '"H&E" = list(', fixed = TRUE)
})

test_that("emits an offset under the selected image", {
  out <- codegen(
    dataset = "Atlas",
    spatial_name = "X",
    image_label = "DAPI",
    offset_x = 42,
    offset_y = 0,
    scale_x = 1,
    scale_y = 1,
    flip_x = FALSE,
    flip_y = FALSE,
    rotation = 0
  )
  expect_true(grepl(
    'offset_x = 42',
    out,
    fixed = TRUE
  ))
  expect_match(out, '"DAPI" = list(', fixed = TRUE)
})

test_that("emits flip_x TRUE when horizontally flipped", {
  out <- codegen(
    dataset = "Atlas",
    spatial_name = "X",
    image_label = "H&E",
    offset_x = 0,
    offset_y = 0,
    scale_x = 1,
    scale_y = 1,
    flip_x = TRUE,
    flip_y = FALSE,
    rotation = 0
  )
  expect_true(grepl(
    'flip_x = TRUE',
    out,
    fixed = TRUE
  ))
})

test_that("quotes every identity containing special characters verbatim", {
  out <- codegen(
    dataset = "Atlas (review)",
    spatial_name = "Mouse ileum (MERFISH)",
    image_label = "Rose H&E",
    offset_x = -350,
    offset_y = 0,
    scale_x = 1,
    scale_y = 1,
    flip_x = FALSE,
    flip_y = FALSE,
    rotation = -90
  )
  expect_match(out, '"Atlas (review)" = list(', fixed = TRUE)
  expect_match(out, '"Mouse ileum (MERFISH)" = list(', fixed = TRUE)
  expect_match(out, '"Rose H&E" = list(', fixed = TRUE)
  expect_match(out, "rotation = -90", fixed = TRUE)
})
