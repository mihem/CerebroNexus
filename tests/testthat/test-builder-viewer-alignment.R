## A background aligned in the Builder must appear in the generated Viewer
## exactly where the Builder preview showed it, for every rotation and flip.

builder_viewer_alignment_env <- function() {
  env <- new.env(parent = globalenv())
  env$`%||%` <- function(a, b) if (is.null(a)) b else a
  sys.source(builder_test_inst_path("builder", "extras.R"), envir = env)
  env
}

## Where the Builder preview puts image pixel (u, v) of a W x H image: flip
## and scale in the image frame, rotate counter-clockwise about the image
## centre, then move by (dx, dy). Data y points up, image rows point down.
builder_preview_point <- function(u, v, record, W, H) {
  b <- record$base_bounds
  fx <- if (isTRUE(record$flip_x)) -1 else 1
  fy <- if (isTRUE(record$flip_y)) -1 else 1
  lx <- fx * (u / W - 0.5) * (b$xmax - b$xmin) * record$scale
  ly <- -fy * (v / H - 0.5) * (b$ymax - b$ymin) * record$scale
  a <- record$rotation * pi / 180
  c(
    (b$xmin + b$xmax) / 2 + record$dx + cos(a) * lx - sin(a) * ly,
    (b$ymin + b$ymax) / 2 + record$dy + sin(a) * lx + cos(a) * ly
  )
}

## Where the Viewer (cell_views.js drawImage) puts the same pixel for the
## settings it receives: bounds rotated about the data origin, flips and
## scale after the rotation, offset added to the rotated centre.
viewer_point <- function(u, v, setting, bounds, W, H) {
  a <- setting$rotation * pi / 180
  rot <- function(x, y) c(cos(a) * x - sin(a) * y, sin(a) * x + cos(a) * y)
  p <- rot(
    bounds$xmin + u / W * (bounds$xmax - bounds$xmin),
    bounds$ymax - v / H * (bounds$ymax - bounds$ymin)
  )
  cr <- rot((bounds$xmin + bounds$xmax) / 2, (bounds$ymin + bounds$ymax) / 2)
  d <- c(
    setting$scale_x * if (isTRUE(setting$flip_x)) -1 else 1,
    setting$scale_y * if (isTRUE(setting$flip_y)) -1 else 1
  )
  c(setting$offset_x, setting$offset_y) + cr + d * (p - cr)
}

builder_viewer_alignment_cases <- function() {
  transforms <- list(
    identity = list(),
    translate = list(dx = 120, dy = -80),
    scale = list(scale = 1.5),
    flip_x = list(flip_x = TRUE),
    flip_y = list(flip_y = TRUE),
    rotate_90 = list(rotation = 90),
    rotate_180 = list(rotation = 180),
    rotate_270 = list(rotation = 270),
    rotate_28 = list(rotation = 28),
    rotate_90_flip_x = list(rotation = 90, flip_x = TRUE),
    rotate_45_flip_y = list(rotation = 45, flip_y = TRUE),
    rotate_30_both_flips = list(rotation = 30, flip_x = TRUE, flip_y = TRUE),
    combined = list(
      dx = 60,
      dy = 40,
      scale = 1.25,
      rotation = 45,
      flip_y = TRUE
    )
  )
  bounds <- list(
    arrow = list(xmin = 0, xmax = 800, ymin = 0, ymax = 500),
    far = list(xmin = 1000, xmax = 1600, ymin = 2000, ymax = 2400)
  )
  defaults <- list(
    dx = 0,
    dy = 0,
    scale = 1,
    rotation = 0,
    flip_x = FALSE,
    flip_y = FALSE,
    image_opacity = 1,
    point_opacity = 1,
    point_size = 5
  )
  cases <- list()
  for (b in names(bounds)) {
    for (t in names(transforms)) {
      record <- utils::modifyList(defaults, transforms[[t]])
      record$base_bounds <- bounds[[b]]
      cases[[paste(b, t)]] <- record
    }
  }
  cases
}

test_that("Viewer settings place every pixel where the Builder preview shows it", {
  env <- builder_viewer_alignment_env()
  W <- 800
  H <- 500
  pixels <- rbind(c(0, 0), c(800, 0), c(0, 500), c(25, 475), c(720, 250))
  for (name in names(builder_viewer_alignment_cases())) {
    record <- builder_viewer_alignment_cases()[[name]]
    setting <- env$builder_alignment_viewer_setting(record)
    for (k in seq_len(nrow(pixels))) {
      expect_equal(
        viewer_point(
          pixels[k, 1],
          pixels[k, 2],
          setting,
          record$base_bounds,
          W,
          H
        ),
        builder_preview_point(pixels[k, 1], pixels[k, 2], record, W, H),
        tolerance = 1e-9,
        info = paste(name, "pixel", k)
      )
    }
  }
})

test_that("the Viewer's drawImage renders the Builder preview's image corners", {
  env <- builder_viewer_alignment_env()
  cases <- lapply(names(builder_viewer_alignment_cases()), function(name) {
    record <- builder_viewer_alignment_cases()[[name]]
    list(
      name = name,
      bounds = record$base_bounds,
      builder = record[c(
        "dx",
        "dy",
        "scale",
        "rotation",
        "flip_x",
        "flip_y",
        "image_opacity"
      )],
      viewer = env$builder_alignment_viewer_setting(record),
      expected_tl = builder_preview_point(0, 0, record, 800, 500)
    )
  })
  path <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(cases, auto_unbox = TRUE, digits = NA), path)
  expect_node_fixture(
    "builder-viewer-alignment.js",
    viewer_test_path("www", "cell_views.js"),
    builder_test_inst_path("builder", "www", "builder-spatial-canvas.js"),
    path,
    pass = "PASS Builder alignment renders identically in the Viewer"
  )
})
