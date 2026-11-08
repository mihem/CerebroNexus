test_that("projection defaults are canonical and spatial defaults independent", {
  env <- viewer_runtime_env()
  options <- list(
    point_size = c(a = 3, b = 8),
    point_opacity = c(a = .4, b = .9),
    percentage_cells_to_show = c(a = 70, b = 100),
    expression_point_size = c(a = 10, b = 12),
    spatial_percentage_cells_to_show = c(a = 50, b = 80)
  )
  for (id in c("a", "b")) {
    for (page in c("projection", "expression", "linked", "trajectory")) {
      expect_identical(
        env$viewerScatterDefaults(options, id, page),
        env$viewerScatterDefaults(options, id)
      )
    }
  }
  expect_identical(
    env$viewerScatterDefaults(options, "a", "spatial"),
    list(point_size = 5, point_opacity = 1, percentage_cells_to_show = 50)
  )
  expect_equal(
    env$viewerScatterDefaults(list(point_size = c(a = 2)), "b")$point_size,
    5
  )
})

test_that("function coordinate settings validate dataset, FOV and numeric bounds", {
  catalogs <- list(a = list(section = list()), b = list(other = list()))
  points <- list(a = list(section = list(point_size = 4, point_opacity = .6)))
  rois <- list(
    a = list(
      section = list(
        region = list(rotation_degrees = 30, point_size = 2, point_opacity = .5)
      )
    )
  )
  expect_equal(.normalizeAppSpatialPointSettings(points, catalogs), points)
  expect_equal(
    .normalizeAppSpatialPointSettings(rois, catalogs, roi = TRUE),
    rois
  )
  expect_error(
    .normalizeAppSpatialPointSettings(list(unknown = points$a), catalogs),
    "unknown dataset"
  )
  expect_error(
    .normalizeAppSpatialPointSettings(list(b = points$a), catalogs),
    "unknown FOV"
  )
  points$a$section$point_opacity <- 2
  expect_error(
    .normalizeAppSpatialPointSettings(points, catalogs),
    "point_opacity"
  )
})

test_that("Builder freeze and full publication preserve projection and spatial settings", {
  generated_app_fixture_source_runtime()
  root <- Sys.getenv("CEREBRO_SCATTER_QA_ROOT", "")
  if (!nzchar(root)) {
    root <- withr::local_tempdir()
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  snapshots_dir <- file.path(root, "snapshots")
  dir.create(snapshots_dir)
  fixtures <- list(
    .generated_app_fixture_basic(),
    .generated_app_fixture_basic(),
    .generated_app_fixture_spatial()
  )
  prepared <- lapply(seq_along(fixtures), function(i) {
    item <- .generated_app_e2e_entry(
      paste0("scatter-", i),
      fixtures[[i]],
      snapshots_dir
    )
    item$entry$settings$overview_point_size <- c(2, 8, 3)[i]
    item$entry$settings$overview_point_opacity <- c(.4, .9, .7)[i]
    item$entry$settings$overview_percentage_cells_to_show <- c(70, 100, 60)[i]
    item$entry$settings$spatial_percentage_cells_to_show <- c(100, 100, 80)[i]
    if (i == 3) {
      item$entry$settings$spatial_point_appearance <- list(
        section_a = list(point_size = 6, point_opacity = .5),
        section_b = list(point_size = 4, point_opacity = .8)
      )
    }
    item
  })
  entries <- lapply(prepared, `[[`, "entry")
  snapshots <- stats::setNames(
    lapply(prepared, `[[`, "snapshot"),
    vapply(entries, `[[`, character(1), "id")
  )
  plan <- builder_freeze_plan(
    entries,
    file.path(root, "release"),
    make_app = TRUE,
    app_options = list(
      initial_dataset = entries[[1]]$id,
      initial_page = "data_info",
      launch_browser = FALSE
    ),
    app_auth = list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
  )
  expect_null(plan$error)
  coordinator <- builder_coordinator_prepare(plan, "scatter-roundtrip")
  result <- builder_execute_plan(plan, coordinator$stage, snapshots)
  result$build_id <- coordinator$build_id
  expect_identical(result$state, "success")
  published <- builder_coordinator_publish(coordinator, result)
  expect_true(published$published)
  config <- readRDS(file.path(published$app_dir, "cerebro_config.rds"))
  expect_equal(unname(config$point_size), c(2, 8, 3))
  expect_equal(unname(config$point_opacity), c(.4, .9, .7))
  expect_equal(unname(config$percentage_cells_to_show), c(70, 100, 60))
  expect_equal(unname(config$spatial_percentage_cells_to_show), c(100, 100, 80))
  expect_null(config$expression_point_size)
  expect_identical(
    config$viewer_content[[entries[[3]]$id]]$spatial_point_appearance,
    entries[[3]]$settings$spatial_point_appearance
  )
  function_dir <- file.path(root, "function-app")
  spatial_id <- entries[[3]]$id
  points <- stats::setNames(
    list(entries[[3]]$settings$spatial_point_appearance),
    spatial_id
  )
  createShinyApp(
    stats::setNames(unname(unlist(published$built)), names(config$point_size)),
    result_dir = function_dir,
    point_size = config$point_size,
    point_opacity = config$point_opacity,
    percentage_cells_to_show = config$percentage_cells_to_show,
    spatial_percentage_cells_to_show = config$spatial_percentage_cells_to_show,
    spatial_point_appearance = points,
    quiet = TRUE,
    verbose = FALSE,
    launch_browser = FALSE
  )
  from_function <- readRDS(file.path(function_dir, "cerebro_config.rds"))
  expect_identical(from_function$point_size, config$point_size)
  expect_null(from_function$expression_point_size)
  expect_identical(
    from_function$viewer_content[[spatial_id]]$spatial_point_appearance,
    config$viewer_content[[spatial_id]]$spatial_point_appearance
  )
  message("Scatter roundtrip app: ", published$app_dir)
})

test_that("Builder, function and Viewer agree on coordinate appearance bounds", {
  generated_app_fixture_source_runtime()
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_content_contract.R"), env)
  viewer_runtime_env(env)
  catalogs <- list(dataset = list(fov = list()))
  for (roi in c(FALSE, TRUE)) {
    for (size in c(.5, 1, 20, 21, Inf, NA_real_)) {
      leaf <- list(point_size = size, point_opacity = .6)
      if (roi) {
        leaf$rotation_degrees <- 30
      }
      setting <- if (roi) list(region = leaf) else leaf
      entries <- list(fov = setting)
      valid <- is.finite(size) && size >= 1 && size <= 20
      check <- if (roi) {
        .builder_app_spatial_roi_settings_valid
      } else {
        .builder_app_spatial_point_appearance_valid
      }
      expect_identical(check(entries), valid)
      config <- list(viewer_content = list(dataset = list()))
      key <- if (roi) "spatial_roi_settings" else "spatial_point_appearance"
      config$viewer_content$dataset[[key]] <- entries
      displayed <- if (roi) {
        env$spatialRoiSetting(config, "dataset", "fov", "region")
      } else {
        env$spatialPointAppearance(config, "dataset", "fov")
      }
      expect_identical(!is.null(displayed), valid)
      if (valid) {
        expect_equal(
          .normalizeAppSpatialPointSettings(
            list(dataset = entries),
            catalogs,
            roi
          ),
          list(dataset = entries)
        )
      } else {
        expect_error(
          .normalizeAppSpatialPointSettings(
            list(dataset = entries),
            catalogs,
            roi
          ),
          "point_size"
        )
      }
    }
    for (bad in list("4", TRUE, numeric(), c(2, 3), list(4))) {
      leaf <- list(point_size = bad, point_opacity = .6)
      if (roi) {
        leaf$rotation_degrees <- 0
      }
      expect_null(env$viewerSpatialPointSetting(leaf, roi))
    }
  }
})
