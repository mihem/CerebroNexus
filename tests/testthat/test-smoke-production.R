##----------------------------------------------------------------------------##
## End-to-end production smoke test, fully synthetic (no network, no data
## packages): build spatial Seurat objects -> convertSeuratToCerebro -> .crb ->
## createShinyApp, then assert the generated app bundle is complete and that
## multiple crbs each carry their own background image + alignment parameters.
##
## Guards the full public pipeline a user runs, and specifically the per-dataset
## isolation of spatial_images / offset / flip when more than one crb is bundled.
##----------------------------------------------------------------------------##

skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")

## Shared fixture: convert two synthetic spatial datasets and build one app that
## bundles both, each with its own background image and alignment defaults.
build_smoke_app <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())

  crb_a <- convert_synthetic_to_crb(
    make_synthetic_spatial_seurat(seed = 1, shift = 0),
    file.path(root, "ds_a"),
    "Synthetic A"
  )
  crb_b <- convert_synthetic_to_crb(
    make_synthetic_spatial_seurat(seed = 2, shift = 500),
    file.path(root, "ds_b"),
    "Synthetic B"
  )

  img_a <- write_dummy_png(file.path(root, "bg_a.png"), 4, 4)
  img_b <- write_dummy_png(file.path(root, "bg_b.png"), 8, 6)

  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = c("Dataset A" = crb_a, "Dataset B" = crb_b),
    result_dir = app_dir,
    launch_browser = FALSE,
    spatial_images = c("Dataset A" = img_a, "Dataset B" = img_b),
    spatial_images_offset_x = c("Dataset A" = 100, "Dataset B" = 250),
    spatial_images_offset_y = c("Dataset A" = -50, "Dataset B" = 75),
    spatial_images_flip_y = c("Dataset A" = TRUE, "Dataset B" = FALSE),
    verbose = FALSE
  )

  list(root = root, app_dir = app_dir, crb_a = crb_a, crb_b = crb_b)
}

test_that("convertSeuratToCerebro produces a .crb carrying spatial data", {
  root <- withr::local_tempdir()
  crb_path <- convert_synthetic_to_crb(
    make_synthetic_spatial_seurat(seed = 1),
    file.path(root, "ds"),
    "Synthetic A"
  )
  expect_true(file.exists(crb_path))

  crb <- readRDS(crb_path)
  expect_true(length(crb$availableSpatial()) > 0)
  sd <- crb$getSpatialData(crb$availableSpatial()[1])
  expect_true(all(c("coordinates", "expression") %in% names(sd)))
  expect_true(nrow(sd$coordinates) > 0)
})

test_that("createShinyApp bundles the app directory and config", {
  app <- build_smoke_app()

  expect_true(dir.exists(app$app_dir))
  expect_true(file.exists(file.path(app$app_dir, "app.R")))
  expect_true(file.exists(file.path(app$app_dir, "cerebro_config.rds")))

  ## Both crbs and both background images copied into the bundle.
  bundled <- list.files(
    file.path(app$app_dir, "data"),
    pattern = "\\.(crb|png)$"
  )
  expect_true(any(grepl("Synthetic_A\\.crb$", bundled)))
  expect_true(any(grepl("Synthetic_B\\.crb$", bundled)))
  expect_true(any(grepl("bg_a\\.png$", bundled)))
  expect_true(any(grepl("bg_b\\.png$", bundled)))
})

test_that("multi-crb config lists both datasets by name", {
  app <- build_smoke_app()
  cfg <- readRDS(file.path(app$app_dir, "cerebro_config.rds"))

  expect_identical(
    cfg[["cerebro_version"]],
    as.character(utils::packageVersion("cerebroAppLite"))
  )
  expect_setequal(
    names(cfg[["crb_file_to_load"]]),
    c("Dataset A", "Dataset B")
  )
  expect_match(cfg[["crb_file_to_load"]][["Dataset A"]], "Synthetic_A\\.crb$")
  expect_match(cfg[["crb_file_to_load"]][["Dataset B"]], "Synthetic_B\\.crb$")
})

test_that("each dataset keeps its own background image + alignment params", {
  app <- build_smoke_app()
  cfg <- readRDS(file.path(app$app_dir, "cerebro_config.rds"))

  ## Background image path is per-dataset, not shared.
  expect_match(cfg[["spatial_images"]][["Dataset A"]], "bg_a\\.png$")
  expect_match(cfg[["spatial_images"]][["Dataset B"]], "bg_b\\.png$")

  ## Offset / flip resolve independently per dataset name — the isolation that
  ## a single shared value would silently break.
  expect_equal(cfg[["spatial_images_offset_x"]][["Dataset A"]], 100)
  expect_equal(cfg[["spatial_images_offset_x"]][["Dataset B"]], 250)
  expect_equal(cfg[["spatial_images_offset_y"]][["Dataset A"]], -50)
  expect_equal(cfg[["spatial_images_offset_y"]][["Dataset B"]], 75)
  expect_true(cfg[["spatial_images_flip_y"]][["Dataset A"]])
  expect_false(cfg[["spatial_images_flip_y"]][["Dataset B"]])
})

## Real-data counterpart: bundle two genuine spatial .crb demos shipped in the
## package (no convert step — these are already .crb) so the app is exercised
## against real coordinate spaces and both background-image paths: Visium with
## an EXTERNAL H&E png, Xenium with an EMBEDDED histology image.
build_real_app <- function() {
  visium_crb <- system.file(
    "extdata/v1.4/demo_spatial_visium.crb",
    package = "cerebroAppLite"
  )
  xenium_crb <- system.file(
    "extdata/v1.4/demo_spatial_xenium.crb",
    package = "cerebroAppLite"
  )
  visium_png <- system.file(
    "extdata/v1.4/demo_spatial_visium_he.png",
    package = "cerebroAppLite"
  )
  if (!all(nzchar(c(visium_crb, xenium_crb, visium_png)))) {
    return(NULL)
  }

  root <- withr::local_tempdir(.local_envir = parent.frame())
  app_dir <- file.path(root, "app")
  createShinyApp(
    cerebro_data = c("Visium" = visium_crb, "Xenium" = xenium_crb),
    result_dir = app_dir,
    launch_browser = FALSE,
    ## Only Visium gets an external image; Xenium carries its own embedded one.
    spatial_images = c("Visium" = visium_png),
    spatial_images_offset_x = c("Visium" = 120),
    spatial_images_flip_y = c("Visium" = TRUE),
    verbose = FALSE
  )
  list(
    root = root,
    app_dir = app_dir,
    visium_crb = visium_crb,
    xenium_crb = xenium_crb
  )
}

test_that("createShinyApp bundles real spatial demos with mixed image paths", {
  app <- build_real_app()
  skip_if(is.null(app), "bundled real spatial demos not available")

  expect_true(dir.exists(app$app_dir))
  cfg <- readRDS(file.path(app$app_dir, "cerebro_config.rds"))

  ## Both real datasets listed by name.
  expect_setequal(names(cfg[["crb_file_to_load"]]), c("Visium", "Xenium"))

  ## The external image + its alignment apply to Visium only; Xenium relies on
  ## its embedded histology and must NOT inherit Visium's external image, so it
  ## has no entry in spatial_images at all.
  expect_match(cfg[["spatial_images"]][["Visium"]], "\\.png$")
  expect_false("Xenium" %in% names(cfg[["spatial_images"]]))
  expect_equal(cfg[["spatial_images_offset_x"]][["Visium"]], 120)
  expect_true(cfg[["spatial_images_flip_y"]][["Visium"]])

  ## Both real crbs copied into the bundle.
  bundled <- list.files(file.path(app$app_dir, "data"), pattern = "\\.crb$")
  expect_true(any(grepl("visium", bundled, ignore.case = TRUE)))
  expect_true(any(grepl("xenium", bundled, ignore.case = TRUE)))
})

test_that("the generated app remains self-contained at runtime", {
  app <- build_real_app()
  skip_if(is.null(app), "bundled real spatial demos not available")

  app_source <- paste(
    readLines(file.path(app$app_dir, "app.R"), warn = FALSE),
    collapse = "\n"
  )
  bundled_source <- paste(
    unlist(lapply(
      list.files(
        file.path(app$app_dir, "shiny"),
        pattern = "\\.[Rr]$",
        recursive = TRUE,
        full.names = TRUE
      ),
      readLines,
      warn = FALSE
    )),
    collapse = "\n"
  )

  ## createShinyApp() copies the complete UI/server implementation. The bundle
  ## must therefore boot without resolving the package that created it.
  expect_false(grepl(
    'requireNamespace("cerebroAppLite"',
    app_source,
    fixed = TRUE
  ))
  expect_false(grepl("cerebroAppLite::", bundled_source, fixed = TRUE))
  expect_false(grepl(
    "asNamespace(\"cerebroAppLite\"",
    bundled_source,
    fixed = TRUE
  ))
  expect_false(grepl(
    'packageVersion("cerebroAppLite")',
    bundled_source,
    fixed = TRUE
  ))
})

test_that("the generated real-data app boots with the Spatial tab", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_info <- build_real_app()
  skip_if(is.null(app_info), "bundled real spatial demos not available")
  shinytest2::local_app_support(app_info$app_dir)

  driver <- shinytest2::AppDriver$new(
    app_info$app_dir,
    name = "smoke_real_multicrb",
    load_timeout = 60000
  )
  withr::defer(driver$stop())
  driver$wait_for_idle(timeout = 30000)

  driver$set_inputs(crb_file_selector = app_info$visium_crb, wait_ = FALSE)
  driver$wait_for_idle(timeout = 30000)
  spatial_tab <- driver$get_js(
    "document.querySelector('a[href=\"#shiny-tab-spatial\"]') !== null;"
  )
  expect_true(isTRUE(spatial_tab))
})

test_that("the generated multi-crb app boots and switches datasets", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_info <- build_smoke_app()
  shinytest2::local_app_support(app_info$app_dir)

  driver <- shinytest2::AppDriver$new(
    app_info$app_dir,
    name = "smoke_multicrb",
    load_timeout = 60000
  )
  withr::defer(driver$stop())
  driver$wait_for_idle(timeout = 30000)

  ## Both datasets are offered in the loader's dataset selector.
  selector <- driver$get_value(input = "crb_file_selector")
  expect_true(!is.null(selector))

  ## Load the first dataset and confirm the Spatial tab appears (it is only
  ## inserted when the active dataset carries spatial data).
  driver$set_inputs(
    crb_file_selector = app_info$crb_a,
    wait_ = FALSE
  )
  driver$wait_for_idle(timeout = 30000)
  spatial_tab_a <- driver$get_js(
    "document.querySelector('a[href=\"#shiny-tab-spatial\"]') !== null;"
  )
  expect_true(isTRUE(spatial_tab_a))

  ## Switch to the second dataset; the Spatial tab must still be present, proving
  ## multi-crb switching keeps the spatial module wired for each dataset.
  driver$set_inputs(
    crb_file_selector = app_info$crb_b,
    wait_ = FALSE
  )
  driver$wait_for_idle(timeout = 30000)
  spatial_tab_b <- driver$get_js(
    "document.querySelector('a[href=\"#shiny-tab-spatial\"]') !== null;"
  )
  expect_true(isTRUE(spatial_tab_b))
})
