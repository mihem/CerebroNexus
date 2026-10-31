builder_spatial_canvas_node <- function() {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  runner <- testthat::test_path("builder-spatial-canvas-runtime-node.js")
  output <- tryCatch(
    system2(
      "node",
      c(
        shQuote(runner),
        shQuote(builder_test_inst_path(
          "builder",
          "www",
          "builder-spatial-canvas.js"
        ))
      ),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(error) NULL
  )
  if (is.null(output)) {
    skip("node cannot be launched from this R test process")
  }
  expect_identical(attr(output, "status") %||% 0L, 0L, info = output)
  jsonlite::fromJSON(tail(output, 1L), simplifyVector = FALSE)
}

test_that("Spatial Builder uses a persistent Canvas live preview", {
  root <- builder_test_inst_path("builder")
  ui <- paste(
    readLines(file.path(root, "ui", "enhance_stage.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(ui, '"Cells shown (%)"', fixed = TRUE)
  expect_match(ui, '`aria-label` = label', fixed = TRUE)
})

test_that("Canvas renderer owns bounded raw points and latest-only controls", {
  root <- builder_test_inst_path("builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "devicePixelRatio", fixed = TRUE)
  expect_match(js, "Math.min(window.devicePixelRatio || 1, 2)", fixed = TRUE)
  expect_match(js, "var left = screen({x: b.xmin, y: cy});", fixed = TRUE)
  expect_match(js, "var right = screen({x: b.xmax, y: cy});", fixed = TRUE)
  expect_match(js, "new ResizeObserver(schedule)", fixed = TRUE)
  expect_match(js, "pointermove", fixed = TRUE)
  expect_match(js, 'var label = "View area";', fixed = TRUE)
  expect_match(js, "var VIEWER_FRAME_PADDING = 10;", fixed = TRUE)
  expect_false(grepl("ctx.fillText(group", js, fixed = TRUE))
  expect_match(js, '"builder_spatial_roi_select"', fixed = TRUE)
  expect_match(js, "if (!groups.length)", fixed = TRUE)
  expect_match(js, "state.releaseGuardId = null;", fixed = TRUE)
  expect_match(js, '"builder_spatial_coordinate_draft"', fixed = TRUE)
  expect_match(js, "coordinateSequence", fixed = TRUE)
})

test_that("Canvas runtime persists point-safe View areas and deduplicates Ion events", {
  result <- builder_spatial_canvas_node()

  legacy_view <- function(bounds, width, height, padding) {
    view_width <- bounds$xmax - bounds$xmin
    view_height <- bounds$ymax - bounds$ymin
    plot_width <- width - padding * 2
    plot_height <- height - padding * 2
    target_aspect <- plot_width / plot_height
    centre_x <- (bounds$xmin + bounds$xmax) / 2
    centre_y <- (bounds$ymin + bounds$ymax) / 2
    if (view_width / view_height < target_aspect) {
      view_width <- view_height * target_aspect
    } else {
      view_height <- view_width / target_aspect
    }
    list(
      xmin = centre_x - view_width / 2,
      xmax = centre_x + view_width / 2,
      ymin = centre_y - view_height / 2,
      ymax = centre_y + view_height / 2
    )
  }
  point_safe_view <- function(bounds, width, height, padding) {
    fitted <- legacy_view(bounds, width, height, padding)
    plot_width <- width - padding * 2
    scale <- plot_width / (fitted$xmax - fitted$xmin)
    expansion <- padding / scale
    list(
      xmin = fitted$xmin - expansion,
      xmax = fitted$xmax + expansion,
      ymin = fitted$ymin - expansion,
      ymax = fitted$ymax + expansion
    )
  }
  as_numeric_bounds <- function(value) {
    lapply(value, as.numeric)
  }

  expect_equal(
    as_numeric_bounds(result$overlay$viewports$`__section__`),
    point_safe_view(
      list(xmin = -5, xmax = 15, ymin = -10, ymax = 30),
      500,
      400,
      18
    ),
    tolerance = 1e-12
  )
  expect_equal(
    as_numeric_bounds(result$resizedOverlay$viewports$`__section__`),
    as_numeric_bounds(result$overlay$viewports$`__section__`),
    tolerance = 1e-12
  )
  expect_equal(
    mean(unlist(as_numeric_bounds(result$separate$viewports$A)[c(
      "xmin",
      "xmax"
    )])),
    -45,
    tolerance = 1e-12
  )
  expect_equal(
    mean(unlist(as_numeric_bounds(result$separate$viewports$A)[c(
      "ymin",
      "ymax"
    )])),
    100,
    tolerance = 1e-12
  )
  expect_equal(
    mean(unlist(as_numeric_bounds(result$separate$imageFitViewports$A)[c(
      "xmin",
      "xmax"
    )])),
    -45,
    tolerance = 1e-12
  )
  expect_equal(
    mean(unlist(as_numeric_bounds(result$separate$imageFitViewports$A)[c(
      "ymin",
      "ymax"
    )])),
    100,
    tolerance = 1e-12
  )
  expect_equal(as.numeric(result$separateFirstPoint$x), 127, tolerance = 1e-12)
  expect_equal(as.numeric(result$separateFirstPoint$y), 200, tolerance = 1e-12)
  expect_length(result$overlayViewerFrames, 1L)
  expect_length(result$separateViewerFrames, 2L)
  overlay_frame <- result$overlayViewerFrames[[1L]]
  expect_length(result$overlayPoints, 2L)
  expect_true(all(vapply(
    result$overlayPoints,
    function(point) {
      x <- as.numeric(point$x)
      y <- as.numeric(point$y)
      x - as.numeric(overlay_frame$left) >= 16 &&
        as.numeric(overlay_frame$right) - x >= 16 &&
        y - as.numeric(overlay_frame$top) >= 16 &&
        as.numeric(overlay_frame$bottom) - y >= 16
    },
    logical(1)
  )))
  expect_true(all(vapply(
    result$separateViewerFrames,
    function(frame) {
      as.numeric(frame$left) > 0 &&
        as.numeric(frame$top) > 0 &&
        as.numeric(frame$right) < 500 &&
        as.numeric(frame$bottom) < 400
    },
    logical(1)
  )))
  expect_identical(result$nullControlsViewKey, "null-controls")
  expect_false(isTRUE(all.equal(
    result$overlay$viewports$`__section__`,
    result$overlay$imageFitViewports$`__section__`
  )))
  expect_identical(result$numberValue, "0.01")
  expect_length(result$commits, 1L)
  expect_identical(as.numeric(result$commits[[1L]]$value$controls$scale), 0.01)
  expect_identical(as.numeric(result$commits[[1L]]$value$resetToken), 0)
  expect_identical(as.numeric(result$commits[[1L]]$value$sequence), 1)
  expect_identical(result$tinyNumberValue, "0.001")
  expect_identical(result$tinySliderValue, "0.001")
  expect_identical(result$tinyStep, "0.001")
  expect_identical(result$authoritativeStep, "0.001")
  expect_length(result$tinyScaleCommits, 1L)
  expect_identical(
    as.numeric(result$tinyScaleCommits[[1L]]$value$controls$scale),
    0.001
  )
  expect_identical(
    as.numeric(result$tinyScaleCommits[[1L]]$value$sequence),
    2
  )
  expect_length(result$resetRaceCommits, 1L)
  expect_identical(
    as.numeric(result$resetRaceCommits[[1L]]$value$controls$scale),
    0.02
  )
  expect_identical(
    as.numeric(result$resetRaceCommits[[1L]]$value$generation),
    4
  )
  expect_identical(
    as.numeric(result$resetRaceCommits[[1L]]$value$resetToken),
    0
  )
  expect_identical(as.integer(result$imageLoads), 1L)
  expect_identical(as.integer(result$imageDrawsAfterClear), 1L)
  expect_identical(as.numeric(result$rotationAfterClear), -94.7)
  expect_identical(as.integer(result$retryImageLoads), 3L)
  expect_gte(as.integer(result$retryImageDraws), 1L)
  expect_identical(as.integer(result$retryImageErrors), 1L)
  expect_identical(result$retryImageErrorViewKey, "retry-image")
  expect_identical(as.integer(result$retryImageErrorGeneration), 6L)
  expect_identical(result$retryImageErrorSnapshot, "snapshot-a")
  expect_identical(as.integer(result$deferredOverlayLoads), 0L)
  expect_identical(as.integer(result$deferredSeparateLoads), 1L)
  expect_identical(as.integer(result$evictedImageReloads), 2L)
  expect_gte(as.integer(result$decodedCacheEvictions), 2L)
  expect_identical(as.integer(result$imageRenderCacheBuilds), 1L)
  expect_identical(as.integer(result$rasterBuildDraws), 1L)
  expect_identical(as.integer(result$dragCommitsBeforeRelease), 0L)
  expect_identical(as.integer(result$dragCommitsAfterRelease), 1L)
  expect_identical(as.integer(result$percentage20Points), 20L)
  expect_identical(as.integer(result$percentage50Points), 50L)
  expect_identical(as.integer(result$percentageSceneMessages), 0L)
  expect_length(result$percentageCommits, 1L)
  expect_identical(
    as.numeric(result$percentageCommits[[1L]]$value$controls$cell_percentage),
    50
  )
  expect_identical(as.integer(result$stopped), 2L)
  expect_identical(as.integer(result$pendingTimers), 0L)
})

test_that("paired number controls do not echo slider updates through Shiny", {
  root <- builder_test_inst_path("builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  sync_start <- regexpr(
    "sync_coordinate_control <- function",
    server,
    fixed = TRUE
  )[[1L]]
  sync_end <- regexpr(
    'sync_coordinate_control("img_scale", 0, 10)',
    server,
    fixed = TRUE
  )[[1L]]
  sync_block <- substr(server, sync_start, sync_end - 1L)

  expect_false(grepl(
    "shiny::observeEvent(\n      input[[slider_id]]",
    sync_block,
    fixed = TRUE
  ))
})

test_that("Canvas hover coalesces pointer work and reuses rendered screen points", {
  root <- builder_test_inst_path("builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "screenPoints: []", fixed = TRUE)
  expect_match(js, "state.screenPoints = new Array(p.x.length);", fixed = TRUE)
  expect_match(js, "state.screenPoints[index] = at;", fixed = TRUE)
  expect_match(
    js,
    "var cosine = Math.cos(angle), sine = Math.sin(angle);",
    fixed = TRUE
  )
  expect_match(js, "function setupCanvasHover(node)", fixed = TRUE)
  expect_match(js, 'node.addEventListener("pointermove"', fixed = TRUE)
  expect_match(js, "if (!state.hoverFrame)", fixed = TRUE)
  expect_match(js, "window.requestAnimationFrame(updateHover)", fixed = TRUE)
  expect_match(
    js,
    "window.cancelAnimationFrame(state.hoverFrame)",
    fixed = TRUE
  )
  expect_false(grepl(
    'document.addEventListener("pointermove"',
    js,
    fixed = TRUE
  ))
})

test_that("same-token scene refreshes preserve browser-local controls", {
  root <- builder_test_inst_path("builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "viewChanged || resetToken > state.resetToken", fixed = TRUE)
})

test_that("Canvas scene refreshes reuse image bytes already sent", {
  root <- builder_test_inst_path("builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server, "builder_spatial_canvas_cache_sources(", fixed = TRUE)
})

test_that("Reset image waits for server-authoritative controls", {
  root <- builder_test_inst_path("builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, 'closest("#enhance-reset_align")', fixed = TRUE)
  expect_match(
    js,
    'closest("#enhance-reset_coordinate_transform")',
    fixed = TRUE
  )
})

test_that("programmatic Image settings restore cannot commit partial values", {
  root <- builder_test_inst_path("builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  start <- regexpr(
    "commit_alignment_controls <- function()",
    server,
    fixed = TRUE
  )[[1L]]
  block <- substr(server, start, start + 2200L)
  expected_start <- regexpr(
    "expected <- shiny::isolate(expected_controls())",
    block,
    fixed = TRUE
  )[[1L]]
  block <- substr(block, expected_start, nchar(block))
  expect_match(
    block,
    paste0(
      "if \\(\\s*!is.null\\(expected\\)\\s*\\) \\{",
      "[\\s\\S]*?expected_controls\\(NULL\\)",
      "[\\s\\S]*?!isTRUE\\(all.equal\\("
    ),
    perl = TRUE
  )
  expect_match(
    server,
    "settled_parameters <- shiny::debounce(parameters, millis = 50)",
    fixed = TRUE
  )
  expect_match(
    gsub("[[:space:]]+", " ", server),
    paste0(
      "parameters <- shiny::reactive({ browser <- browser_control_state() ",
      "owner <- active_control_owner()"
    ),
    fixed = TRUE
  )
})

test_that("control commits carry a complete spatial owner and flush on switch", {
  root <- builder_test_inst_path("builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(js, "snapshotIdentity: scene.snapshotIdentity", fixed = TRUE)
  expect_match(js, "image: scene.activeImage", fixed = TRUE)
})

test_that("preview settlement cannot overwrite live control values", {
  root <- builder_test_inst_path("builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  start <- regexpr(
    "shiny::observeEvent(alignment_preview()",
    server,
    fixed = TRUE
  )[[1L]]
  block <- substr(server, start, start + 1600L)

  expect_match(
    block,
    "update_position_steps(draft(), preview$bounds)",
    fixed = TRUE
  )
  expect_false(grepl("update_controls(", block, fixed = TRUE))
})

test_that("image replacement clears stale decoded pixels", {
  root <- builder_test_inst_path("builder")
  js <- paste(
    readLines(
      file.path(root, "www", "builder-spatial-canvas.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    js,
    "if (state.imageKey !== key) state.image = null;",
    fixed = TRUE
  )
  expect_match(js, "state.imageKey = null;", fixed = TRUE)
})

test_that("Spatial preview worker receives only the ROI base transform", {
  root <- builder_test_inst_path("builder")
  server <- paste(
    readLines(file.path(root, "spatial_alignment_server.R"), warn = FALSE),
    collapse = "\n"
  )
  worker_session <- paste(
    readLines(file.path(root, "session.R"), warn = FALSE),
    collapse = "\n"
  )
  imports <- paste(
    readLines(file.path(root, "server", "imports.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("coordinate_preview_transforms", server, fixed = TRUE))
  expect_false(grepl(
    "coordinate_transforms = coordinate_draft",
    server,
    fixed = TRUE
  ))
  expect_match(
    worker_session,
    "base_coordinate_transform = base_coordinate_transform",
    fixed = TRUE
  )
  expect_match(imports, "nxt$base_coordinate_transform", fixed = TRUE)
})
