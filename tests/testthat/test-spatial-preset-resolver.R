# Pure contracts for Viewer spatial background identity and settings.

spatial_options <- list(
  spatial_plot_rotation = list(
    Atlas = c(sliceA = 90, sliceB = -90),
    Other = c(sliceA = 180)
  ),
  spatial_images = list(
    Atlas = list(
      sliceA = list(
        `H&E` = "spatial-assets/Atlas/sliceA/he.png",
        DAPI = list(
          path = "spatial-assets/Atlas/sliceA/dapi.png",
          bounds = c(xmin = 0, xmax = 100, ymin = 5, ymax = 95)
        )
      ),
      sliceB = c(IF = "spatial-assets/Atlas/sliceB/if.png"),
      sliceC = list()
    ),
    Other = list(
      sliceA = c(Histology = "spatial-assets/Other/sliceA/other.png")
    )
  ),
  spatial_image_settings = list(
    Atlas = list(
      sliceA = list(
        `H&E` = list(
          offset_x = 11,
          scale_x = 1.25,
          flip_y = TRUE,
          image_opacity = 0.8
        ),
        DAPI = list(offset_x = 22, rotation = 90)
      ),
      sliceB = list(IF = list(offset_x = 33, flip_x = TRUE))
    ),
    Other = list(sliceA = list(Histology = list(offset_x = 99)))
  )
)

test_that("one resolver rotates spatial coordinates for every Viewer surface", {
  expect_identical(spatialPlotRotation(spatial_options, "Atlas", "sliceA"), 90)
  expect_identical(spatialPlotRotation(spatial_options, "Atlas", "sliceB"), -90)
  expect_identical(spatialPlotRotation(spatial_options, "Other", "sliceA"), 180)
  expect_identical(spatialPlotRotation(spatial_options, "Atlas", "missing"), 0)
  expect_identical(spatialPlotRotation(spatial_options, "missing", "sliceA"), 0)

  coordinates <- data.frame(x = c(0, 1), y = c(0, 2))
  expect_equal(
    rotateSpatialCoordinates(coordinates, 90),
    data.frame(x = c(0, -2), y = c(0, 1)),
    tolerance = 1e-12
  )
})

test_that("MERFISH plot and image rotations are configured independently", {
  app_env <- new.env(parent = globalenv())
  app_lines <- readLines(system.file("app.R", package = "CerebroNexus"))
  options_start <- grep("^Cerebro.options", app_lines)[[1L]]
  options_end <- grep('^  "projections_show_hover_info"', app_lines)[[1L]]
  expression <- app_lines[options_start:(options_end + 1L)]
  expression[[1L]] <- sub("<<-", "<-", expression[[1L]], fixed = TRUE)
  app_env$custom_welcome_message <- "test"
  eval(parse(text = expression), envir = app_env)
  options <- app_env$Cerebro.options

  expect_identical(
    options$spatial_plot_rotation[["Mouse ileum (MERFISH)"]][["fov"]],
    90
  )
  expect_identical(
    options$spatial_image_settings[["Mouse ileum (MERFISH)"]][["fov"]][[
      "Tissue background"
    ]]$rotation,
    90
  )
  expect_null(options$spatial_images[["Mouse ileum (MERFISH)"]])
})

test_that("configured images resolve one exact dataset and spatial leaf", {
  slice_a <- configured_spatial_images(spatial_options, "Atlas", "sliceA")

  expect_named(slice_a, c("H&E", "DAPI"))
  expect_identical(
    slice_a[["H&E"]],
    list(path = "spatial-assets/Atlas/sliceA/he.png", bounds = NULL)
  )
  expect_identical(
    slice_a$DAPI$bounds,
    c(xmin = 0, xmax = 100, ymin = 5, ymax = 95)
  )
  expect_named(
    configured_spatial_images(spatial_options, "Atlas", "sliceB"),
    "IF"
  )
  expect_identical(
    configured_spatial_images(spatial_options, "Atlas", "sliceC"),
    list()
  )
})

test_that("configured images fail closed without leaking neighbouring leaves", {
  expect_identical(
    configured_spatial_images(spatial_options, "Atlas", "missing"),
    list()
  )
  expect_identical(
    configured_spatial_images(spatial_options, "missing", "sliceA"),
    list()
  )
  expect_identical(configured_spatial_images(NULL, "Atlas", "sliceA"), list())
})

test_that("embedded images expose canonical labels and normalize singular legacy", {
  canonical <- list(
    histology_images = list(
      `H&E` = list(
        histology_image = "data:image/png;base64,HE",
        histology_image_bounds = c(xmin = 0, xmax = 10, ymin = 0, ymax = 20)
      ),
      DAPI = list(histology_image = "data:image/png;base64,DAPI")
    ),
    histology_image = "data:image/png;base64,STALE"
  )
  expect_named(embedded_spatial_images(canonical), c("H&E", "DAPI"))
  expect_identical(
    embedded_spatial_images(canonical)$DAPI$image,
    "data:image/png;base64,DAPI"
  )

  legacy <- list(
    histology_image = "data:image/png;base64,LEGACY",
    histology_image_bounds = c(xmin = 1, xmax = 2, ymin = 3, ymax = 4)
  )
  expect_named(embedded_spatial_images(legacy), "Tissue background")
  expect_identical(
    embedded_spatial_images(legacy)[["Tissue background"]]$bounds,
    legacy$histology_image_bounds
  )
  expect_identical(embedded_spatial_images(list()), list())
})

test_that("one preset resolver owns every image setting and default", {
  expect_identical(
    spatialImagePreset(spatial_options, "Atlas", "sliceA", "H&E"),
    list(
      offsetX = 11,
      offsetY = 0,
      scaleX = 1.25,
      scaleY = 1,
      flipX = FALSE,
      flipY = TRUE,
      rotation = 0,
      opacity = 0.8
    )
  )
  expect_identical(
    spatialImagePreset(spatial_options, "Atlas", "sliceA", "DAPI"),
    list(
      offsetX = 22,
      offsetY = 0,
      scaleX = 1,
      scaleY = 1,
      flipX = FALSE,
      flipY = FALSE,
      rotation = 90,
      opacity = 0.6
    )
  )
  expect_identical(
    spatialImagePreset(spatial_options, "Atlas", "sliceC", "IF"),
    spatialImagePreset(NULL, NULL, NULL, NULL)
  )
  expect_identical(
    spatialImagePreset(spatial_options, "Atlas", "sliceA", NULL),
    spatialImagePreset(NULL, NULL, NULL, NULL)
  )
})

test_that("background choices keep labels separate from source-tagged identity", {
  embedded <- embedded_spatial_images(list(
    histology_images = list(
      `H&E` = list(histology_image = "data:image/png;base64,HE")
    )
  ))
  external <- configured_spatial_images(spatial_options, "Atlas", "sliceA")
  choices <- spatial_background_choices(embedded, external)

  expect_identical(names(choices), c("No Background", "H&E", "H&E", "DAPI"))
  expect_identical(
    unname(choices),
    c(
      "none",
      spatial_background_key("embedded", "H&E"),
      spatial_background_key("external", "H&E"),
      spatial_background_key("external", "DAPI")
    )
  )
  expect_false(any(
    unname(choices) %in%
      c(
        "data:image/png;base64,HE",
        "spatial-assets/Atlas/sliceA/he.png"
      )
  ))
})

test_that("stale selections reset to the first current image or none", {
  slice_a_choices <- spatial_background_choices(
    list(),
    configured_spatial_images(spatial_options, "Atlas", "sliceA")
  )
  slice_b_choices <- spatial_background_choices(
    list(),
    configured_spatial_images(spatial_options, "Atlas", "sliceB")
  )
  slice_c_choices <- spatial_background_choices(list(), list())
  old <- spatial_background_key("external", "H&E")

  expect_identical(
    normalize_spatial_background_choice(old, slice_a_choices),
    old
  )
  expect_identical(
    normalize_spatial_background_choice(old, slice_b_choices),
    spatial_background_key("external", "IF")
  )
  expect_identical(
    normalize_spatial_background_choice(old, slice_c_choices),
    "none"
  )
})

test_that("selected identity resolves the matching descriptor and bounds", {
  embedded <- embedded_spatial_images(list(
    histology_images = list(
      DAPI = list(
        histology_image = "data:image/png;base64,DAPI",
        histology_image_bounds = c(xmin = 1, xmax = 9, ymin = 2, ymax = 8)
      )
    )
  ))
  external <- configured_spatial_images(spatial_options, "Atlas", "sliceA")

  expect_identical(
    resolve_spatial_background(
      spatial_background_key("embedded", "DAPI"),
      embedded,
      external
    )$bounds,
    c(xmin = 1, xmax = 9, ymin = 2, ymax = 8)
  )
  expect_identical(
    resolve_spatial_background(
      spatial_background_key("external", "DAPI"),
      embedded,
      external
    )$bounds,
    c(xmin = 0, xmax = 100, ymin = 5, ymax = 95)
  )
  expect_null(resolve_spatial_background("none", embedded, external))
})

test_that("background identity includes its full logical image location", {
  descriptor <- list(
    source = "embedded",
    label = "H&E",
    image = "data:image/png;base64,SAME"
  )

  expect_identical(
    spatial_background_identity("Atlas", "sliceA", descriptor),
    list(
      dataset = "Atlas",
      spatial_name = "sliceA",
      source = "embedded",
      label = "H&E"
    )
  )
  expect_false(identical(
    spatial_background_identity("Atlas", "sliceA", descriptor),
    spatial_background_identity("Other", "sliceB", descriptor)
  ))
  expect_null(spatial_background_identity("Atlas", "sliceA", NULL))
})

test_that("copy preset formatter emits exact canonical image settings", {
  code <- format_spatial_preset_code(
    dataset = "Atlas",
    spatial_name = "sliceA",
    image_label = "DAPI",
    offset_x = 2,
    offset_y = -3,
    scale_x = 1.25,
    scale_y = 0.75,
    flip_x = TRUE,
    flip_y = FALSE,
    rotation = 90
  )
  value <- eval(parse(text = paste0("list(", code, ")")), envir = baseenv())

  expect_named(value, "spatial_image_settings")
  expect_true(startsWith(code, "spatial_image_settings = list("))
  expect_identical(
    value$spatial_image_settings$Atlas$sliceA$DAPI,
    list(
      flip_x = TRUE,
      flip_y = FALSE,
      scale_x = 1.25,
      scale_y = 0.75,
      offset_x = 2,
      offset_y = -3,
      rotation = 90,
      image_opacity = 0.6
    )
  )
  expect_false(grepl("spatial_images_offset_x", code, fixed = TRUE))
})

test_that("copy preset formatter rejects a missing image target", {
  expect_null(format_spatial_preset_code(
    dataset = "Atlas",
    spatial_name = "sliceA",
    image_label = NULL,
    offset_x = 0,
    offset_y = 0,
    scale_x = 1,
    scale_y = 1,
    flip_x = FALSE,
    flip_y = FALSE,
    rotation = 0
  ))
})
