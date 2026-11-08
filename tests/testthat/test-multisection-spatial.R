##----------------------------------------------------------------------------##

builder_multisection_source_extras <- function(local = parent.frame()) {
  relative <- file.path(
    "viewer",
    "core",
    "spatial_coordinate_contract.R"
  )
  core <- builder_test_inst_path(relative)
  if (!file.exists(core)) {
    core <- system.file(relative, package = "CerebroNexus")
  }
  sys.source(core, envir = local)
  builder_repo_source("spatial.R", local = local)
  builder_repo_source("extras.R", local = local)
  builder_repo_source("build.R", local = local)
}
## Objects holding several tissue sections.
##
## Every shipped demo `.crb` has exactly one spatial entry, and every other
## assertion in the suite indexes `availableSpatial()[1]`. That blind spot let
## two real defects live: the builder wrote one histology image into every
## section, and the viewer could not reach any section but the first.
##
## Fully synthetic -- no network, no data packages.
##----------------------------------------------------------------------------##

## Building the object and exporting it dominate this file's run time, so every
## test shares one export; each test reads its own copy of the CRB.
multisection_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      obj <- make_synthetic_multisection_seurat()
      dir <- tempfile("multisection-")
      withr::defer(
        unlink(dir, recursive = TRUE),
        envir = testthat::teardown_env()
      )
      cache <<- list(
        obj = obj,
        crb_path = convert_synthetic_to_crb(obj, dir, "multisection")
      )
    }
    cache
  }
})

test_that("export keeps one spatial entry per tissue section", {
  skip_if_not_installed("Seurat")

  obj <- multisection_fixture()$obj
  expect_equal(
    names(obj@images),
    c("sectionA1", "sectionA2", "sectionB1")
  )

  crb <- readCerebro(multisection_fixture()$crb_path)

  expect_equal(
    crb$availableSpatial(),
    c("sectionA1", "sectionA2", "sectionB1")
  )

  ## Each entry must carry its OWN cells and its OWN patch of coordinate space.
  ## A section silently handed the whole object's coordinates is the documented
  ## failure mode of the metadata fallback in .getSpatialData, and it would show
  ## up here as overlapping ranges.
  ranges <- lapply(crb$availableSpatial(), function(nm) {
    co <- crb$getSpatialData(nm)$coordinates
    list(
      n = nrow(co),
      lo = min(co[, 1]),
      hi = max(co[, 1]),
      cells = rownames(co)
    )
  })
  names(ranges) <- crb$availableSpatial()

  expect_equal(
    vapply(ranges, function(r) r$n, numeric(1)),
    c(sectionA1 = 30, sectionA2 = 30, sectionB1 = 40)
  )
  ## Built 500 apart, so the sections cannot overlap unless something is wrong.
  expect_lt(ranges$sectionA1$hi, ranges$sectionA2$lo)
  expect_lt(ranges$sectionA2$hi, ranges$sectionB1$lo)
  ## No cell may appear in two sections.
  all_cells <- unlist(lapply(ranges, function(r) r$cells), use.names = FALSE)
  expect_equal(anyDuplicated(all_cells), 0L)
})

test_that("external histology remains scoped to its own section", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")

  local({
    builder_multisection_source_extras(environment())

    dir <- withr::local_tempdir()
    crb_path <- multisection_fixture()$crb_path

    ## One image and one extent per section, each derived from that section's
    ## own coordinates. Writing a single pair into every section -- the defect
    ## this guards -- would put one slide behind every other slide's cells.
    sections <- c("sectionA1", "sectionA2", "sectionB1")
    images <- list()
    for (i in seq_along(sections)) {
      image_path <- file.path(dir, paste0(sections[[i]], ".png"))
      arr <- array(stats::runif(6 * 6 * 3), dim = c(6, 6, 3))
      png::writePNG(arr, image_path)
      image <- builder_read_image(image_path)
      expect_null(image$error)
      images[[sections[i]]] <- builder_alignment_record(
        source = list(name = basename(image_path), type = image$mime),
        base_bounds = list(
          xmin = (i - 1) * 500,
          xmax = (i - 1) * 500 + 100,
          ymin = 0,
          ymax = 80
        ),
        section = list(id = sections[[i]], kind = "spatial"),
        source_path = image$source_path
      )
    }

    crb <- readCerebro(crb_path)
    applied <- .builder_apply_external_spatial_appearance(crb, images)
    expect_null(applied$error)
    expect_setequal(applied$applied, sections)

    external <- .builder_build_materialize_spatial_images(
      list(id = "dataset", name = "Dataset", images = images),
      dir
    )$images$dataset
    bounds <- lapply(sections, function(nm) {
      records <- external[[nm]]
      expect_length(records, 1L)
      records[[1L]]$bounds
    })
    names(bounds) <- sections

    ## Distinct extents, and each one actually frames its own section's cells.
    expect_equal(
      vapply(bounds, function(b) b[["xmin"]], numeric(1)),
      c(sectionA1 = 0, sectionA2 = 500, sectionB1 = 1000)
    )
    for (nm in sections) {
      sd <- crb$getSpatialData(nm)
      expect_true(
        all(
          sd$coordinates[, 1] >= bounds[[nm]][["xmin"]] &
            sd$coordinates[, 1] <= bounds[[nm]][["xmax"]]
        ),
        info = paste(nm, "cells must sit inside their own image extent")
      )
    }

    ## The four documented bound keys live in the external manifest.
    for (nm in sections) {
      expect_setequal(
        names(external[[nm]][[1L]]$bounds),
        c("xmin", "xmax", "ymin", "ymax")
      )
    }
  })
})

test_that("attaching to some sections leaves the others without an image", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")

  local({
    builder_multisection_source_extras(environment())

    crb_path <- multisection_fixture()$crb_path

    image_path <- withr::local_tempfile(fileext = ".png")
    png::writePNG(array(stats::runif(6 * 6 * 3), dim = c(6, 6, 3)), image_path)
    image <- builder_read_image(image_path)
    expect_null(image$error)
    record <- builder_alignment_record(
      source = list(name = basename(image_path), type = image$mime),
      base_bounds = list(xmin = 500, xmax = 600, ymin = 0, ymax = 80),
      section = list(id = "sectionA2", kind = "spatial"),
      source_path = image$source_path
    )
    crb <- readCerebro(crb_path)
    applied <- .builder_apply_external_spatial_appearance(
      crb,
      list(sectionA2 = record)
    )
    ## A partial attachment is legitimate -- the viewer simply offers no
    ## background for the bare sections -- but it must not bleed across.
    expect_equal(applied$applied, "sectionA2")
  })
})

test_that("attaching nothing, or to a section that is absent, is handled", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")

  local({
    builder_multisection_source_extras(environment())

    crb <- readCerebro(multisection_fixture()$crb_path)

    ## No images at all: a no-op, not an error -- most data sets have none.
    expect_equal(
      .builder_apply_external_spatial_appearance(crb, list())$applied,
      character()
    )

    ## An image for a section the object lacks is not applied to any other.
    image_path <- withr::local_tempfile(fileext = ".png")
    png::writePNG(array(stats::runif(6 * 6 * 3), dim = c(6, 6, 3)), image_path)
    image <- builder_read_image(image_path)
    record <- builder_alignment_record(
      source = list(name = basename(image_path), type = image$mime),
      base_bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
      section = list(id = "nosuchsection", kind = "spatial"),
      source_path = image$source_path
    )
    absent <- .builder_apply_external_spatial_appearance(
      crb,
      list(nosuchsection = record)
    )
    expect_equal(absent$applied, character())
    expect_equal(absent$changed, character())
  })
})
