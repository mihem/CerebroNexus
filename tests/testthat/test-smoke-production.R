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

shared_fixture_env <- environment()
shared_smoke_app <- NULL
shared_real_app <- NULL
shared_real_app_initialized <- FALSE

## Shared fixture: convert two synthetic spatial datasets and build one app that
## bundles both, each with its own background image and alignment defaults.
build_smoke_app <- function(envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)

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
    spatial_images = list(
      "Dataset A" = list(fov = c(Histology = img_a)),
      "Dataset B" = list(fov = c(Histology = img_b))
    ),
    spatial_image_settings = list(
      "Dataset A" = list(
        fov = list(
          Histology = list(offset_x = 100, offset_y = -50, flip_y = TRUE)
        )
      ),
      "Dataset B" = list(
        fov = list(
          Histology = list(offset_x = 250, offset_y = 75, flip_y = FALSE)
        )
      )
    ),
    verbose = FALSE
  )

  list(root = root, app_dir = app_dir, crb_a = crb_a, crb_b = crb_b)
}

## Building this bundle is substantially more expensive than inspecting it.
## Every consumer below is read-only, so build it once for this file. The
## browser test still creates its own AppDriver and therefore its own session.
get_smoke_app <- function() {
  if (is.null(shared_smoke_app)) {
    shared_smoke_app <<- build_smoke_app(envir = shared_fixture_env)
  }
  shared_smoke_app
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
  app <- get_smoke_app()

  expect_true(dir.exists(app$app_dir))
  expect_true(file.exists(file.path(app$app_dir, "app.R")))
  expect_true(file.exists(file.path(app$app_dir, "cerebro_config.rds")))

  ## Raw CRBs and background images remain in their separate bundle directories.
  private_data <- list.files(file.path(app$app_dir, "private-data"))
  spatial_assets <- list.files(
    file.path(app$app_dir, "spatial-assets"),
    recursive = TRUE
  )
  expect_true(any(grepl("Synthetic_A\\.crb$", private_data)))
  expect_true(any(grepl("Synthetic_B\\.crb$", private_data)))
  expect_false(any(grepl("\\.png$", private_data)))
  expect_setequal(
    spatial_assets,
    c(
      sub(
        "^spatial-assets/",
        "",
        expected_spatial_image_target(
          "Dataset A",
          "fov",
          "Histology",
          "bg_a.png"
        )
      ),
      sub(
        "^spatial-assets/",
        "",
        expected_spatial_image_target(
          "Dataset B",
          "fov",
          "Histology",
          "bg_b.png"
        )
      )
    )
  )
})

test_that("multi-crb config lists both datasets by name", {
  app <- get_smoke_app()
  cfg <- readRDS(file.path(app$app_dir, "cerebro_config.rds"))

  expect_identical(
    cfg[["cerebro_version"]],
    as.character(utils::packageVersion("CerebroNexus"))
  )
  expect_setequal(
    names(cfg[["crb_file_to_load"]]),
    c("Dataset A", "Dataset B")
  )
  expect_match(cfg[["crb_file_to_load"]][["Dataset A"]], "Synthetic_A\\.crb$")
  expect_match(cfg[["crb_file_to_load"]][["Dataset B"]], "Synthetic_B\\.crb$")
  manifest <- cfg[[".bundle_backend_plan"]]
  expect_identical(manifest$schema_version, 1L)
  configured_crbs <- unique(unname(cfg[["crb_file_to_load"]]))
  expect_length(manifest$entries, length(configured_crbs))
  expect_identical(anyDuplicated(names(manifest$entries)), 0L)
  expect_setequal(
    names(manifest$entries),
    configured_crbs
  )
  expect_true(all(vapply(
    manifest$entries,
    function(entry) {
      identical(
        entry,
        list(type = "embedded", mode = "embedded", location = NULL)
      )
    },
    logical(1)
  )))
})

test_that("each dataset keeps its own background image + alignment params", {
  app <- get_smoke_app()
  cfg <- readRDS(file.path(app$app_dir, "cerebro_config.rds"))

  ## Background image path is per-dataset, not shared.
  expect_identical(
    cfg$spatial_images[["Dataset A"]]$fov$Histology,
    expected_spatial_image_target("Dataset A", "fov", "Histology", "bg_a.png")
  )
  expect_identical(
    cfg$spatial_images[["Dataset B"]]$fov$Histology,
    expected_spatial_image_target("Dataset B", "fov", "Histology", "bg_b.png")
  )

  ## Offset / flip resolve independently per dataset name — the isolation that
  ## a single shared value would silently break.
  settings <- cfg$spatial_image_settings
  expect_equal(settings[["Dataset A"]]$fov$Histology$offset_x, 100)
  expect_equal(settings[["Dataset B"]]$fov$Histology$offset_x, 250)
  expect_equal(settings[["Dataset A"]]$fov$Histology$offset_y, -50)
  expect_equal(settings[["Dataset B"]]$fov$Histology$offset_y, 75)
  expect_true(settings[["Dataset A"]]$fov$Histology$flip_y)
  expect_false(settings[["Dataset B"]]$fov$Histology$flip_y)
})

## Real-data counterpart: bundle two genuine spatial .crb demos shipped in the
## package (no convert step — these are already .crb) so the app is exercised
## against real coordinate spaces and external image manifests for both Visium
## and Xenium.
build_real_app <- function(envir = parent.frame()) {
  visium_crb <- system.file(
    "extdata/examples/demo_spatial_visium.crb",
    package = "CerebroNexus"
  )
  xenium_crb <- system.file(
    "extdata/examples/demo_spatial_xenium.crb",
    package = "CerebroNexus"
  )
  visium_png <- system.file(
    "extdata/examples/demo_spatial_visium_he.png",
    package = "CerebroNexus"
  )
  xenium_dir <- system.file(
    "extdata/examples/spatial/xenium",
    package = "CerebroNexus"
  )
  xenium_pngs <- file.path(
    xenium_dir,
    c("dapi.png", "pink_stain_90.png", "fluorescent_yellow_90.png")
  )
  if (
    !all(nzchar(c(visium_crb, xenium_crb, visium_png))) ||
      !all(file.exists(xenium_pngs))
  ) {
    return(NULL)
  }

  root <- withr::local_tempdir(.local_envir = envir)
  app_dir <- file.path(root, "app")
  visium_spatial <- readRDS(visium_crb)$availableSpatial()[[1L]]
  xenium_spatial <- readRDS(xenium_crb)$availableSpatial()
  createShinyApp(
    cerebro_data = c("Visium" = visium_crb, "Xenium" = xenium_crb),
    result_dir = app_dir,
    launch_browser = FALSE,
    initial_page = "spatial",
    spatial_images = list(
      Visium = stats::setNames(
        list(c(Histology = visium_png)),
        visium_spatial
      ),
      Xenium = stats::setNames(
        list(
          c(`Tissue background` = xenium_pngs[[1L]]),
          c(
            `Pink stain` = xenium_pngs[[2L]],
            `Fluorescent yellow` = xenium_pngs[[3L]]
          )
        ),
        xenium_spatial
      )
    ),
    spatial_image_settings = list(
      Visium = stats::setNames(
        list(list(Histology = list(offset_x = 120, flip_y = TRUE))),
        visium_spatial
      )
    ),
    verbose = FALSE
  )
  list(
    root = root,
    app_dir = app_dir,
    visium_crb = visium_crb,
    xenium_crb = xenium_crb
  )
}

## Keep the real-data bundle separate from the synthetic fixture. Its consumers
## are also read-only, and its browser test creates an independent AppDriver.
get_real_app <- function() {
  if (!shared_real_app_initialized) {
    shared_real_app <<- build_real_app(envir = shared_fixture_env)
    shared_real_app_initialized <<- TRUE
  }
  shared_real_app
}

test_that("createShinyApp bundles real spatial demos with mixed image paths", {
  app <- get_real_app()
  skip_if(is.null(app), "bundled real spatial demos not available")

  expect_true(dir.exists(app$app_dir))
  cfg <- readRDS(file.path(app$app_dir, "cerebro_config.rds"))
  visium_spatial <- names(cfg$spatial_images$Visium)[[1L]]

  ## Both real datasets listed by name.
  expect_setequal(names(cfg[["crb_file_to_load"]]), c("Visium", "Xenium"))

  ## Both datasets retain their own spatial/image nesting.
  expect_match(
    cfg$spatial_images$Visium[[visium_spatial]]$Histology,
    "\\.png$"
  )
  expect_named(cfg$spatial_images$Xenium, c("fov", "fov_colour"))
  expect_named(
    cfg$spatial_images$Xenium$fov_colour,
    c("Pink stain", "Fluorescent yellow")
  )
  expect_true(all(vapply(
    cfg$spatial_images$Xenium$fov_colour,
    function(path) file.exists(file.path(app$app_dir, path)),
    logical(1)
  )))
  expect_equal(
    cfg$spatial_image_settings$Visium[[visium_spatial]]$Histology$offset_x,
    120
  )
  expect_true(
    cfg$spatial_image_settings$Visium[[visium_spatial]]$Histology$flip_y
  )

  ## Both real crbs copied into the bundle.
  bundled <- list.files(
    file.path(app$app_dir, "private-data"),
    pattern = "\\.crb$"
  )
  expect_true(any(grepl("visium", bundled, ignore.case = TRUE)))
  expect_true(any(grepl("xenium", bundled, ignore.case = TRUE)))
})

test_that("the generated app remains self-contained at runtime", {
  app <- get_real_app()
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
    'requireNamespace("CerebroNexus"',
    app_source,
    fixed = TRUE
  ))
  expect_false(grepl("CerebroNexus::", bundled_source, fixed = TRUE))
  expect_false(grepl(
    "asNamespace(\"CerebroNexus\"",
    bundled_source,
    fixed = TRUE
  ))
  expect_false(grepl(
    'packageVersion("CerebroNexus")',
    bundled_source,
    fixed = TRUE
  ))
  ## system.file(package = "CerebroNexus") resolves to "" once the package is
  ## gone, silently breaking whatever resource it points at. The bundle must
  ## locate its own resources relative to cerebro_root, never via the package.
  expect_false(grepl(
    'package = "CerebroNexus"',
    bundled_source,
    fixed = TRUE
  ))
  expect_false(grepl("library(CerebroNexus", bundled_source, fixed = TRUE))
})

test_that("the generated real-data app boots with the Spatial tab", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_info <- get_real_app()
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
  spatial_active <- driver$get_js(
    paste0(
      "document.querySelector('a[href=\"#shiny-tab-spatial\"]')",
      ".parentElement.classList.contains('active') && ",
      "document.querySelector('.tab-pane.active').id === 'shiny-tab-spatial';"
    )
  )
  expect_true(isTRUE(spatial_active))
})

test_that("the generated multi-crb app boots and switches datasets", {
  skip_if_not_installed("shinytest2")
  skip_on_cran()

  app_info <- get_smoke_app()
  shinytest2::local_app_support(app_info$app_dir)

  driver <- shinytest2::AppDriver$new(
    app_info$app_dir,
    name = "smoke_multicrb",
    load_timeout = 60000
  )
  withr::defer(driver$stop())
  driver$wait_for_idle(timeout = 30000)

  ## Both datasets are offered in the loader's dataset selector.
  config <- readRDS(file.path(app_info$app_dir, "cerebro_config.rds"))
  configured_crbs <- config[["crb_file_to_load"]]
  configured_values <- unname(configured_crbs)
  ## Selectize keeps only the selected item in its hidden source <select>.
  ## Open its rendered dropdown and inspect the public option DOM instead.
  driver$wait_for_js(
    paste0(
      "(function() {",
      "var selector = document.querySelector('#crb_file_selector');",
      "if (!selector || !selector.selectize) return false;",
      "selector.selectize.open();",
      "return selector.parentElement.querySelectorAll(",
      "'.selectize-dropdown-content .option[data-value]'",
      ").length === ",
      length(configured_values),
      ";",
      "})()"
    ),
    timeout = 30000
  )
  selector <- driver$get_value(input = "crb_file_selector")
  expect_true(!is.null(selector))
  option_labels <- unlist(
    driver$get_js(
      paste0(
        "(function() {",
        "var selector = document.querySelector('#crb_file_selector');",
        "var options = selector.parentElement.querySelectorAll(",
        "'.selectize-dropdown-content .option[data-value]'",
        ");",
        "return Array.from(options).map(function(option) {",
        "return option.textContent.trim();",
        "});",
        "})()"
      )
    ),
    use.names = FALSE
  )
  option_values <- unlist(
    driver$get_js(
      paste0(
        "(function() {",
        "var selector = document.querySelector('#crb_file_selector');",
        "var options = selector.parentElement.querySelectorAll(",
        "'.selectize-dropdown-content .option[data-value]'",
        ");",
        "return Array.from(options).map(function(option) {",
        "return option.getAttribute('data-value');",
        "});",
        "})()"
      )
    ),
    use.names = FALSE
  )
  expect_identical(option_labels, names(configured_crbs))
  expect_identical(option_values, configured_values)
  expect_identical(anyDuplicated(option_values), 0L)

  ## Load the first dataset and confirm the Spatial tab appears (it is only
  ## inserted when the active dataset carries spatial data).
  driver$set_inputs(
    crb_file_selector = configured_values[[1L]],
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
    crb_file_selector = configured_values[[2L]],
    wait_ = FALSE
  )
  driver$wait_for_idle(timeout = 30000)
  expect_identical(
    driver$get_value(input = "crb_file_selector"),
    configured_values[[2L]]
  )
  spatial_tab_b <- driver$get_js(
    "document.querySelector('a[href=\"#shiny-tab-spatial\"]') !== null;"
  )
  expect_true(isTRUE(spatial_tab_b))
})

## The static self-contained test above proves the BUNDLE SOURCE never names the
## package. This one proves the harder half: the .crb data itself. A .crb is an
## R6 object whose class lives in R/ (not in the copied bundle), so if any of its
## methods reached into the CerebroNexus namespace, readRDS would carry a
## namespace reference and fail once the package is gone. Load it in a child
## process whose library path genuinely lacks CerebroNexus and use it.
test_that("a bundled dataset deserializes and works without CerebroNexus", {
  skip_if_not_installed("callr")
  skip_on_cran()
  skip_on_os("windows") # the hermetic library is built with symlinks

  app <- get_smoke_app()
  cfg <- readRDS(file.path(app$app_dir, "cerebro_config.rds"))
  first_crb <- file.path(app$app_dir, cfg[["crb_file_to_load"]][[1]])
  expect_true(file.exists(first_crb))

  ## Exclude the package so serialized fixtures cannot conceal a namespace
  ## dependency in a standalone bundle.
  hermetic_lib <- withr::local_tempdir()
  linked_any <- FALSE
  for (lib in .libPaths()) {
    for (pkg in list.dirs(lib, recursive = FALSE, full.names = FALSE)) {
      if (identical(pkg, "CerebroNexus")) {
        next
      }
      dest <- file.path(hermetic_lib, pkg)
      if (!file.exists(dest)) {
        ok <- tryCatch(
          file.symlink(file.path(lib, pkg), dest),
          error = function(e) FALSE
        )
        linked_any <- linked_any || isTRUE(ok)
      }
    }
  }
  skip_if_not(linked_any, "could not build a hermetic library via symlinks")

  result <- callr::r(
    function(app_dir) {
      ## Prove the package really is unreachable before we rely on the result.
      if (requireNamespace("CerebroNexus", quietly = TRUE)) {
        stop("CerebroNexus is reachable; the library is not hermetic")
      }
      setwd(app_dir)
      config <- readRDS("cerebro_config.rds")
      assign("Cerebro.options", config, envir = .GlobalEnv)
      runtime <- new.env(parent = globalenv())
      sys.source(
        file.path("viewer", "utility_functions.R"),
        envir = runtime
      )
      crb <- unname(config$crb_file_to_load[[1L]])
      obj <- runtime$get_or_load_crb(
        crb,
        config$.bundle_backend_plan,
        unname(config$crb_file_to_load)
      )
      list(
        classes = class(obj),
        version = as.character(obj$getVersion()),
        n_cells = length(obj$getCellNames()),
        n_genes = length(obj$getGeneNames()),
        n_projections = length(obj$availableProjections())
      )
    },
    args = list(app_dir = app$app_dir),
    libpath = hermetic_lib
  )

  expect_true("Cerebro" %in% result$classes)
  expect_true(nzchar(result$version))
  expect_gt(result$n_cells, 0)
  expect_gt(result$n_genes, 0)
  expect_gt(result$n_projections, 0)
})

## Regression guard for a class of bug we have hit repeatedly: bundle code that
## silently depends on CerebroNexus being installed. The static grep above and
## the deserialize test above each cover one half; this covers the runtime half
## for module code that loads package-authored helpers. The HLA module is the
## worst offender -- its pure core lives in R/ (not copied into a bundle) and it
## used to reach the installed namespace via core_shim. That resolved under
## R CMD check (package installed) but NOT in an exported bundle, so it passed
## every check except a user actually running the exported app.
##
## The only faithful test is the production condition: build the bundle, then
## load its module code in a process whose library path genuinely lacks
## CerebroNexus -- exactly what a user who never installed the package has.
## If the core files were not copied into the bundle, or core_shim reached for
## the namespace, or a core file dropped off its source list, the functions the
## module calls by bare name go unbound here and this fails loudly.
test_that("an exported bundle resolves the HLA core with no CerebroNexus installed", {
  skip_if_not_installed("callr")
  skip_on_cran()
  skip_on_os("windows") # the hermetic library is built with symlinks

  app <- get_smoke_app()
  shim <- file.path(app$app_dir, "viewer/hla_tcr_motifs/core_shim.R")
  skip_if_not(file.exists(shim), "HLA module not present in bundle")

  ## Exclude the package so it cannot conceal a namespace dependency in a
  ## bundle or serialized object.
  hermetic_lib <- withr::local_tempdir()
  linked_any <- FALSE
  for (lib in .libPaths()) {
    for (pkg in list.dirs(lib, recursive = FALSE, full.names = FALSE)) {
      if (identical(pkg, "CerebroNexus")) {
        next
      }
      dest <- file.path(hermetic_lib, pkg)
      if (!file.exists(dest)) {
        ok <- tryCatch(
          file.symlink(file.path(lib, pkg), dest),
          error = function(e) FALSE
        )
        linked_any <- linked_any || isTRUE(ok)
      }
    }
  }
  skip_if_not(linked_any, "could not build a hermetic library via symlinks")

  result <- callr::r(
    function(app_dir) {
      ## Prove the package really is unreachable before we trust the result.
      if (requireNamespace("CerebroNexus", quietly = TRUE)) {
        stop("CerebroNexus is reachable; the library is not hermetic")
      }
      ## Reproduce how the bundled app loads the HLA module: cerebro_root is the
      ## bundle root, and core_shim is sourced into the app-server scope.
      e <- new.env(parent = globalenv())
      e$Cerebro.options <- list(cerebro_root = app_dir)
      sys.source(
        file.path(app_dir, "viewer/hla_tcr_motifs/core_shim.R"),
        envir = e
      )
      ## One representative function per core file, so a whole file dropping off
      ## the shim's source list is caught, plus the exact call that used to 500.
      need <- c(
        "hla_normalize_typing", # hla_typing.R
        "hla_build_motif_graph", # hla_motif_core.R
        "hla_descriptive_feature_overlap", # hla_association_core.R
        "hla_distinct_colors", # hla_visual_helpers.R
        "hla_build_manifest" # hla_export.R
      )
      bound <- vapply(
        need,
        function(n) exists(n, envir = e, inherits = FALSE),
        logical(1)
      )
      empty <- get("hla_normalize_typing", envir = e)(
        list(),
        source_type = "unknown"
      )
      list(bound = bound, empty_ok = is.data.frame(empty))
    },
    args = list(app_dir = app$app_dir),
    libpath = hermetic_lib
  )

  expect_true(
    all(result$bound),
    info = paste(
      "bundled HLA core unresolved without the package:",
      paste(names(result$bound)[!result$bound], collapse = ", ")
    )
  )
  expect_true(result$empty_ok)
})
