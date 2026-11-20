run_demo <- function(options, scope = new.env(parent = globalenv())) {
  launcher <- testthat::test_path("..", "..", "run-demo.R")
  testthat::skip_if_not(file.exists(launcher), "repository-only launcher")
  scope$commandArgs <- function(trailingOnly) {
    if (trailingOnly) options else paste0("--file=", normalizePath(launcher))
  }
  sys.source(launcher, scope)
}

test_that("run-demo prepares the data sets --data names and opens the Viewer", {
  keys <- c("1m", "ren", "cosmx", "tcell")
  variables <- paste0("CEREBRO_", toupper(keys), "_DEMO_CRB")
  withr::local_envvar(stats::setNames(
    rep(NA, 5),
    c(variables, "CEREBRO_VISIUM_DEMOS")
  ))
  datasets <- stats::setNames(tempfile(paste0(keys, "-")), keys)
  file.create(datasets)
  withr::defer(unlink(datasets))
  root <- normalizePath(testthat::test_path("..", ".."))
  loaded <- NULL
  testthat::local_mocked_bindings(
    load_all = function(path, ...) loaded <<- path,
    .package = "pkgload"
  )
  ## The default port is busy: the launcher falls back to a random one.
  testthat::local_mocked_bindings(
    randomPort = function(...) {
      if (length(list(...))) stop("port in use") else 18765L
    },
    .package = "httpuv"
  )
  testthat::local_mocked_bindings(
    runApp = function(appDir, host, port, launch.browser) {
      launched <<- list(appDir, host, port, launch.browser)
    },
    .package = "shiny"
  )
  runs <- list(
    list(options = character(), expected = "1m"),
    list(options = "--data=cosmx", expected = "cosmx"),
    list(options = "--data=tcell,ren,1m", expected = c("1m", "ren", "tcell"))
  )
  fake <- function(key) {
    function() {
      prepared <<- c(prepared, key)
      datasets[[key]]
    }
  }
  for (run in runs) {
    prepared <- packs <- character()
    launched <- NULL
    scope <- new.env(parent = globalenv())
    scope$source <- function(file, local) {
      scope$prepareViewer1mBenchmarkData <- fake("1m")
      scope$prepareViewerRenDemoData <- fake("ren")
      scope$prepareViewerCosMxDemoData <- fake("cosmx")
      scope$prepareViewerTcellDemoData <- fake("tcell")
      scope$.viewer1mVersion <- scope$.viewerRenVersion <- 1L
      scope$.viewerCosMxVersion <- scope$.viewerTcellVersion <- 1L
      ## A Viewer Pack for each prepared data set and its compressed twin.
      scope$prepareDemoViewerPack <- function(crb) {
        packs <<- c(packs, basename(crb))
      }
      scope$prepareCompressedDemoVariant <- function(crb) {
        paste0(crb, "_compressed")
      }
    }
    run_demo(run$options, scope)
    label <- paste(c("run-demo", run$options), collapse = " ")
    expect_identical(loaded, root, label = label)
    expect_identical(prepared, run$expected, label = label)
    names <- basename(datasets[run$expected])
    expect_identical(
      packs,
      as.vector(rbind(names, paste0(names, "_compressed"))),
      label = label
    )
    expect_identical(
      unname(Sys.getenv(variables)),
      unname(ifelse(keys %in% run$expected, normalizePath(datasets), "")),
      label = label
    )
    expect_identical(
      launched,
      list(file.path(root, "inst"), "127.0.0.1", 18765L, TRUE),
      label = label
    )
  }
})

test_that("run-demo rejects unknown options and data sets", {
  for (options in list("--everything", "--data=mouse", "--data=")) {
    expect_error(
      run_demo(options),
      paste0(
        "Usage: Rscript run-demo.R [--data=1m,ren,cosmx,tcell,",
        "dlpfc,mouse_brain,breast,prostate] [--prepare-only]"
      ),
      fixed = TRUE
    )
  }
})

test_that("run-demo builds the Visium sets with the Builder beside the others", {
  withr::local_envvar(c(
    CEREBRO_1M_DEMO_CRB = NA,
    CEREBRO_REN_DEMO_CRB = NA,
    CEREBRO_COSMX_DEMO_CRB = NA,
    CEREBRO_TCELL_DEMO_CRB = NA,
    CEREBRO_VISIUM_DEMOS = NA
  ))
  testthat::local_mocked_bindings(
    load_all = function(path, ...) NULL,
    .package = "pkgload"
  )
  root <- withr::local_tempdir()
  crb <- file.path(root, "1m.crb")
  file.create(crb)
  runs <- list(
    list(options = "--data=1m,dlpfc,breast", launches = 1L),
    list(options = c("--data=prostate", "--prepare-only"), launches = 0L)
  )
  for (run in runs) {
    built <- character()
    launches <- 0L
    scope <- new.env(parent = globalenv())
    scope$source <- function(file, local) {
      scope$prepareViewer1mBenchmarkData <- function() crb
      scope$.viewer1mVersion <- 1L
      scope$prepareDemoViewerPack <- function(crb) NULL
      scope$prepareCompressedDemoVariant <- function(crb) crb
      scope$prepareBuilderVisiumDemoData <- function(id) {
        file.path(root, paste0("visium_", id, "_sce.rds"))
      }
      scope$prepareViewerVisiumDemoData <- function(id, repo_root) {
        expect_identical(
          repo_root,
          normalizePath(testthat::test_path("..", ".."))
        )
        built <<- c(built, id)
        demo <- file.path(root, paste0(id, ".rds"))
        file.create(demo)
        demo
      }
    }
    testthat::local_mocked_bindings(
      runApp = function(...) launches <<- launches + 1L,
      .package = "shiny"
    )
    label <- paste(c("run-demo", run$options), collapse = " ")
    expect_message(
      run_demo(run$options, scope),
      "built by the Builder from",
      label = label
    )
    selected <- strsplit(sub("^--data=", "", run$options[[1L]]), ",")[[1L]]
    visium <- intersect(selected, c("dlpfc", "breast", "prostate"))
    expect_identical(built, visium, label = label)
    expect_identical(
      strsplit(Sys.getenv("CEREBRO_VISIUM_DEMOS"), .Platform$path.sep)[[1L]],
      normalizePath(file.path(root, paste0(visium, ".rds")), winslash = "/"),
      label = label
    )
    expect_identical(
      nzchar(Sys.getenv("CEREBRO_1M_DEMO_CRB")),
      "1m" %in% selected,
      label = label
    )
    expect_identical(launches, run$launches, label = label)
  }
})

test_that("a Builder-built Visium App is kept as a Viewer data set", {
  helper <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_builder_visium_data.R"
  )
  skip_if_not(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, env)
  app <- withr::local_tempdir()
  dir.create(file.path(app, "private-data"))
  dir.create(file.path(app, "spatial-assets"))
  writeLines("crb", file.path(app, "private-data", "01-dlpfc-visium.crb"))
  writeLines("png", file.path(app, "spatial-assets", "u1.png"))
  bounds <- c(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  saveRDS(
    list(
      crb_file_to_load = c(visium = "private-data/01-dlpfc-visium.crb"),
      spatial_images = list(
        visium = list(
          A = list(
            hires = list(path = "spatial-assets/u1.png", bounds = bounds)
          )
        )
      ),
      spatial_image_settings = list(
        visium = list(
          A = list(hires = list(flip_y = TRUE))
        )
      ),
      viewer_content = list(
        visium = list(display = list(spatial_layout = "grid"))
      ),
      point_size = c(visium = 5),
      point_opacity = c(visium = 1)
    ),
    file.path(app, "cerebro_config.rds")
  )
  stage <- withr::local_tempdir()
  env$.viewerVisiumKeepApp(app, stage, "Human DLPFC")
  expect_setequal(
    list.files(stage, recursive = TRUE),
    c("01-dlpfc-visium.crb", "demo.rds", "pictures/u1.png")
  )
  demo <- readRDS(file.path(stage, "demo.rds"))
  expect_identical(demo$label, "Human DLPFC")
  expect_identical(demo$crb, "01-dlpfc-visium.crb")
  expect_identical(demo$spatial_images$A$hires$path, "pictures/u1.png")
  expect_identical(demo$spatial_images$A$hires$bounds, bounds)
  expect_true(demo$spatial_image_settings$A$hires$flip_y)
  expect_identical(demo$viewer_content$display$spatial_layout, "grid")
  expect_identical(demo$point_size, 5)
})

test_that("Visium demo sections name complete, sized downloads", {
  helper <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_builder_visium_data.R"
  )
  skip_if_not(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, env)
  datasets <- env$.builderVisiumDatasets()
  expect_setequal(
    names(datasets),
    c("dlpfc", "mouse_brain", "breast", "prostate")
  )
  for (id in names(datasets)) {
    sections <- datasets[[id]]$sections
    expect_gt(nrow(sections), 1L)
    expect_false(anyDuplicated(sections$section) > 0L, label = id)
    for (index in seq_len(nrow(sections))) {
      files <- env$.builderVisiumSectionFiles(
        id,
        sections[index, , drop = FALSE]
      )
      expect_true(
        all(vapply(
          files,
          function(file) {
            is.character(file$url) &&
              startsWith(file$url, "https://") &&
              is.numeric(file$size) &&
              file$size > 0
          },
          logical(1)
        )),
        label = paste(id, sections$section[[index]])
      )
      ## Every section brings a photograph: its own file or the 10x archive.
      expect_true(
        any(c("image", "spatial") %in% names(files)),
        label = paste(id, sections$section[[index]])
      )
    }
  }
})

test_that("demo preparation reuses valid packs and rebuilds missing or stale first frames", {
  helper <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_viewer_pack.R"
  )
  skip_if_not(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, env)
  result <- list(valid = FALSE)
  builds <- 0L
  testthat::local_mocked_bindings(
    validateViewerPack = function(file, full) {
      expect_true(full)
      result
    },
    buildViewerPack = function(file, viewer_binary, overwrite) {
      expect_identical(file, "fixture.crb")
      expect_identical(viewer_binary, "always")
      expect_true(overwrite)
      builds <<- builds + 1L
      "fixture.viewer"
    },
    .package = "CerebroNexus"
  )
  expect_message(
    env$prepareDemoViewerPack("fixture.crb"),
    "Preparing Viewer Pack"
  )
  expect_identical(builds, 1L)
  result <- list(
    valid = TRUE,
    path = "fixture.viewer",
    manifest = list(
      hla_chains = c("TRA", "TRB"),
      assets = data.frame(
        path = c("hla_tcr/TRA.first.v3.qs2", "hla_tcr/TRB.first.v3.qs2")
      )
    )
  )
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 1L)
  result$manifest$assets <- result$manifest$assets[1L, , drop = FALSE]
  expect_message(
    env$prepareDemoViewerPack("fixture.crb"),
    "Preparing Viewer Pack"
  )
  expect_identical(builds, 2L)
  result$manifest$hla_chains <- character()
  expect_identical(env$prepareDemoViewerPack("fixture.crb"), "fixture.viewer")
  expect_identical(builds, 2L)
})

test_that("a Visium section keeps its spots, picture extent and orientation", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("png")
  helper <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_builder_visium_data.R"
  )
  skip_if_not(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, env)
  root <- withr::local_tempdir()
  ## A 10x feature-barcode matrix of 3 genes x 3 barcodes.
  matrix <- file.path(root, "filtered_feature_bc_matrix.h5")
  rhdf5::h5createFile(matrix)
  rhdf5::h5createGroup(matrix, "matrix")
  rhdf5::h5createGroup(matrix, "matrix/features")
  rhdf5::h5write(c(1L, 2L, 5L, 3L), matrix, "matrix/data")
  rhdf5::h5write(c(0L, 2L, 1L, 0L), matrix, "matrix/indices")
  rhdf5::h5write(c(0L, 2L, 3L, 4L), matrix, "matrix/indptr")
  rhdf5::h5write(c(3L, 3L), matrix, "matrix/shape")
  rhdf5::h5write(c("AAA-1", "CCC-1", "GGG-1"), matrix, "matrix/barcodes")
  rhdf5::h5write(c("Gfap", "Snap25", "Gfap"), matrix, "matrix/features/name")
  rhdf5::h5closeAll()
  ## Space Ranger 1 positions: no header; GGG-1 is outside the tissue.
  positions <- file.path(root, "tissue_positions_list.csv")
  writeLines(
    c("AAA-1,1,0,0,100,40", "CCC-1,1,0,2,300,80", "GGG-1,0,1,1,500,90"),
    positions
  )
  scalefactors <- file.path(root, "scalefactors_json.json")
  writeLines('{"tissue_hires_scalef": 0.5}', scalefactors)
  image <- file.path(root, "tissue_hires_image.png")
  png::writePNG(array(0.5, c(60, 80, 3)), image)
  expect_identical(
    env$.builderVisiumPngSize(image),
    c(height = 60, width = 80)
  )
  ## Space Ranger 2 writes the same table with a header.
  headed <- file.path(root, "tissue_positions.csv")
  writeLines(
    c(
      "barcode,in_tissue,array_row,array_col,pxl_row_in_fullres,pxl_col_in_fullres",
      readLines(positions)
    ),
    headed
  )
  expect_identical(
    env$.builderVisiumReadPositions(headed),
    env$.builderVisiumReadPositions(positions)
  )
  section <- env$.builderVisiumSection(
    list(
      matrix = matrix,
      positions = positions,
      scalefactors = scalefactors,
      image = image
    ),
    "S1"
  )
  expect_identical(colnames(section$counts), c("S1_AAA-1", "S1_CCC-1"))
  expect_identical(rownames(section$counts), c("Gfap", "Snap25", "Gfap.1"))
  expect_equal(as.numeric(section$counts[, "S1_AAA-1"]), c(1, 0, 2))
  ## x is the pixel column, y the pixel row: the picture's orientation,
  ## which the Builder flips together with its picture.
  expect_equal(unname(section$coordinates[, "x"]), c(40, 80))
  expect_equal(unname(section$coordinates[, "y"]), c(100, 300))
  expect_equal(
    section$picture$histology_image_bounds,
    c(xmin = 0, xmax = 160, ymin = 0, ymax = 120)
  )
  expect_match(section$picture$histology_image, "^data:image/png;base64,")
})
