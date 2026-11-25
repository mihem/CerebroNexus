## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

## Keeping only the cells inside regions drawn on a spatial section: the pure
## geometry, the stored settings, the plan identity and a real build.
builder_e2e_source_runtime(environment())
builder_repo_source("review.R", local = environment())

square <- function(xmin, xmax, ymin, ymax) {
  list(x = c(xmin, xmax, xmax, xmin), y = c(ymin, ymin, ymax, ymax))
}

## The bounding box of one tissue of the Trekker example, padded a little.
region_of_tissue <- function(object, tissue, section = "slice1") {
  coordinates <- builder_region_section_coordinates(object, section)
  cells <- colnames(object)[object$tissue_id == tissue]
  rows <- coordinates$cell_barcode %in% cells
  pad <- 50
  square(
    min(coordinates$x[rows]) - pad,
    max(coordinates$x[rows]) + pad,
    min(coordinates$y[rows]) - pad,
    max(coordinates$y[rows]) + pad
  )
}

## The cells whose slide coordinates fall inside a rectangle, found without the
## polygon code under test.
cells_in_box <- function(object, box, section = "slice1") {
  coordinates <- builder_region_section_coordinates(object, section)
  inside <- coordinates$x > min(box$x) &
    coordinates$x < max(box$x) &
    coordinates$y > min(box$y) &
    coordinates$y < max(box$y)
  coordinates$cell_barcode[inside]
}

trekker_object <- function() {
  skip_if_builder_examples_missing()
  builder_example_catalog()$trekker_spatial$make()$object
}

test_that("points are inside a polygon by the even-odd rule", {
  x <- c(1, 5, 9, 5, 2)
  y <- c(1, 5, 9, 1, 8)
  ## An L shape: the corner at (5, 5) is inside, (9, 9) is in the notch.
  l_shape <- list(
    x = c(0, 10, 10, 4, 4, 0),
    y = c(0, 0, 4, 4, 10, 10)
  )
  expect_identical(
    builder_region_points_inside(x, y, list(l_shape)),
    c(TRUE, FALSE, FALSE, TRUE, TRUE)
  )
  ## Several polygons keep the union of their cells.
  expect_identical(
    builder_region_points_inside(
      c(1, 11, 21),
      c(1, 1, 1),
      list(square(0, 2, 0, 2), square(10, 12, 0, 2))
    ),
    c(TRUE, TRUE, FALSE)
  )
  expect_identical(
    builder_region_points_inside(numeric(), numeric(), list()),
    logical()
  )
})

test_that("drawn polygons are validated before they are stored", {
  ok <- builder_region_polygons_normalize(
    list(list(x = list(0, 1, 1), y = list(0, 0, 1)))
  )
  expect_identical(ok, list(list(x = c(0, 1, 1), y = c(0, 0, 1))))
  expect_error(builder_region_polygons_normalize(list()), "at least one")
  expect_error(
    builder_region_polygons_normalize(list(list(x = c(0, 1), y = c(0, 1)))),
    "three vertices"
  )
  expect_error(
    builder_region_polygons_normalize(list(list(
      x = c(0, 1, NA),
      y = c(0, 1, 2)
    ))),
    "finite numbers"
  )
  expect_error(
    builder_region_polygons_normalize(rep(
      list(square(0, 1, 0, 1)),
      BUILDER_REGION_MAX_POLYGONS + 1L
    )),
    "at most"
  )
})

test_that("a stored region subset is complete or it is rejected", {
  cells <- builder_axis_identity(c("a", "b"))
  subset <- list(
    sections = list(slice1 = list(polygons = list(square(0, 1, 0, 1)))),
    resolved = list(
      source_md5 = strrep("A", 32L),
      cells = cells,
      kept = 2L,
      total = 5L,
      group_levels = list(tissue_id = "T1"),
      sections = list(slice1 = list(kept = 2L, total = 5L))
    )
  )
  normalized <- builder_region_subset_normalize(subset)
  expect_identical(normalized$resolved$source_md5, strrep("a", 32L))
  expect_identical(builder_region_subset_normalize(normalized), normalized)
  expect_identical(builder_region_subset_normalize(NULL), list())
  expect_identical(builder_region_subset_normalize(list()), list())

  broken <- subset
  broken$resolved$kept <- 3L
  expect_error(builder_region_subset_normalize(broken), "incomplete")
  broken <- subset
  broken$sections <- list()
  expect_error(builder_region_subset_normalize(broken), "non-empty")
  broken <- subset
  broken$resolved$source_md5 <- "not-a-hash"
  expect_error(builder_region_subset_normalize(broken), "incomplete")
  expect_error(
    builder_region_subset_normalize(list(sections = list())),
    "expected"
  )
})

test_that("a region keeps one tissue of a slide and reports what is left", {
  object <- trekker_object()
  sections <- list(
    slice1 = list(polygons = list(region_of_tissue(object, "T2")))
  )
  levels <- list(tissue_id = c("T1", "T2", "T3", "T4", "outlier"))
  resolved <- builder_region_subset_resolve(object, sections, levels)
  kept <- builder_region_subset_cells(object, sections)$kept

  box <- sections$slice1$polygons[[1L]]
  expected <- cells_in_box(object, box)
  expect_equal(resolved$total, ncol(object))
  expect_gte(length(expected), 1250L)
  expect_setequal(kept, expected)
  expect_true(all(colnames(object)[object$tissue_id == "T2"] %in% kept))
  expect_identical(resolved$kept, length(kept))
  ## The kept cells keep the object's order.
  expect_identical(kept, colnames(object)[colnames(object) %in% kept])
  expect_identical(resolved$cells, builder_axis_identity(kept))
  expect_true("T2" %in% resolved$group_levels$tissue_id)
  expect_lt(length(resolved$group_levels$tissue_id), 5L)
  expect_identical(
    resolved$sections$slice1,
    list(kept = length(kept), total = 5020L)
  )

  ## Several shapes keep the union; a shape without cells is refused.
  two <- list(
    slice1 = list(
      polygons = list(
        region_of_tissue(object, "T1"),
        region_of_tissue(object, "T4")
      )
    )
  )
  expect_identical(
    builder_region_subset_resolve(object, two)$kept,
    length(union(
      cells_in_box(object, two$slice1$polygons[[1L]]),
      cells_in_box(object, two$slice1$polygons[[2L]])
    ))
  )
  expect_error(
    builder_region_subset_resolve(
      object,
      list(slice1 = list(polygons = list(square(1e6, 2e6, 1e6, 2e6))))
    ),
    "contains no cells"
  )
  expect_error(
    builder_region_subset_resolve(
      object,
      list(missing = list(polygons = list(square(0, 1, 0, 1))))
    ),
    "no spatial coordinates"
  )
})

test_that("a region in one section leaves the cells of other sections alone", {
  object <- trekker_object()
  ## The same cells are also the points of the SPATIAL reduction, rotated
  ## from the slide; a cut of slice1 must not change what the reduction holds
  ## for the cells it keeps, and cells missing from slice1 would stay.
  sections <- list(
    slice1 = list(polygons = list(region_of_tissue(object, "T3")))
  )
  cut <- builder_region_subset_cells(object, sections)
  expect_gte(length(cut$kept), 1250L)
  applied <- builder_region_subset_apply(object, list(sections = sections))
  expect_identical(colnames(applied), cut$kept)
  expect_identical(
    SeuratObject::Cells(applied[["slice1"]]),
    cut$kept
  )
  expect_identical(
    rownames(SeuratObject::Embeddings(applied, "SPATIAL")),
    cut$kept
  )
  expect_identical(
    SeuratObject::LayerData(applied, layer = "counts"),
    SeuratObject::LayerData(object, layer = "counts")[, cut$kept]
  )
})

test_that("the cut refuses to run when it no longer matches the frozen identity", {
  object <- trekker_object()
  sections <- list(
    slice1 = list(polygons = list(region_of_tissue(object, "T1")))
  )
  kept <- builder_region_subset_cells(object, sections)$kept
  expect_no_error(builder_region_subset_apply(
    object,
    list(sections = sections),
    expected = builder_axis_identity(kept)
  ))
  expect_error(
    builder_region_subset_apply(
      object,
      list(sections = sections),
      expected = builder_axis_identity(colnames(object))
    ),
    "differ from the frozen plan"
  )
})

test_that("the Trekker payload follows the kept cells", {
  barcodes <- paste0("BC", 1:6)
  trekker <- list(
    meta = list(n_cells_full = 6, n_cells = 6, n_genes_obj = 10, unit = "um"),
    barcodes = barcodes,
    x = c(1, 2, 3, 4, 5, 6),
    y = c(6, 5, 4, 3, 2, 1),
    ux = c(11, 12, 13, 14, 15, 16),
    uy = c(16, 15, 14, 13, 12, 11),
    clusters = c(0L, 1L, 2L, 0L, 1L, 2L),
    celltype = c("a", "b", "c"),
    fields = list(
      f = list(v = c(10, 20, 30, 40, 50, 60), max = 60, label = "f")
    ),
    conf = list(prop_top = seq(0.1, 0.6, by = 0.1)),
    moran = list(list(rank = 1, gene = "G1", I = 0.5)),
    evidence = list(
      list(cell = 1L, bc = "BC2", img = "data:image/png;base64,AA=="),
      list(cell = 4L, bc = "BC5", img = "data:image/png;base64,AA=="),
      list(cell = 5L, bc = "BC6", img = "data:image/png;base64,AA==")
    )
  )
  cut <- builder_region_subset_trekker(trekker, c("BC2", "BC5", "BC6"))
  expect_identical(cut$barcodes, c("BC2", "BC5", "BC6"))
  expect_identical(cut$x, c(2, 5, 6))
  expect_identical(cut$uy, c(15, 12, 11))
  expect_identical(cut$clusters, c(1L, 1L, 2L))
  expect_identical(cut$fields$f$v, c(20, 50, 60))
  expect_identical(cut$conf$prop_top, c(0.2, 0.5, 0.6))
  expect_identical(cut$meta$n_cells, 3L)
  expect_identical(cut$meta$n_cells_full, 6)
  ## The evidence index is zero-based and follows the new positions.
  expect_identical(vapply(cut$evidence, `[[`, integer(1), "cell"), 0:2)
  expect_identical(vapply(cut$evidence, `[[`, "", "bc"), cut$barcodes)
  ## Whole-slide summaries are left as they were.
  expect_identical(cut$moran, trekker$moran)
  expect_identical(cut$celltype, trekker$celltype)

  kept_two <- builder_region_subset_trekker(trekker, "BC1")
  expect_length(kept_two$evidence, 0L)
})

test_that("the plan sees the identity of the cells that remain", {
  object <- trekker_object()
  record <- list(
    id = "trekker_region",
    label = "Trekker region",
    make = function() list(object = object, format = "Built-in example")
  )
  entry <- builder_e2e_entry(record)
  entry$snapshot <- list(object_md5 = strrep("b", 32L))
  full <- .builder_plan_artifact_identity(
    entry,
    entry$settings$included_groups,
    entry$settings$included_projections
  )

  sections <- list(
    slice1 = list(polygons = list(region_of_tissue(object, "T1")))
  )
  resolved <- builder_region_subset_resolve(object, sections, entry$levels)
  resolved$source_md5 <- strrep("b", 32L)
  entry$settings$spatial_region_subset <- builder_region_subset_normalize(list(
    sections = sections,
    resolved = resolved
  ))
  expect_null(.builder_plan_region_error(entry))
  region_entry <- .builder_plan_region_entry(entry)
  identity <- .builder_plan_artifact_identity(
    region_entry,
    entry$settings$included_groups,
    entry$settings$included_projections
  )
  expect_identical(identity$cells, resolved$cells)
  expect_equal(identity$cells$count, resolved$kept)
  expect_identical(identity$features, full$features)
  expect_identical(region_entry$profile$n_cells, resolved$kept)
  expect_false(identical(identity$cells, full$cells))
  for (group in names(identity$group_levels)) {
    expect_true(all(
      identity$group_levels[[group]] %in% entry$levels[[group]]
    ))
  }
  ## Review counts the cells that will be built.
  item <- .builder_review_item_from_entry(
    entry,
    builder_dataset_state(entry),
    1L,
    1L
  )
  expect_identical(item$cell_count, resolved$kept)
  ## An entry without regions is passed through unchanged.
  plain <- entry
  plain$settings$spatial_region_subset <- list()
  expect_identical(.builder_plan_region_entry(plain), plain)
  expect_null(.builder_plan_region_error(plain))

  ## A region drawn on another copy of the data, or beside trajectories, blocks
  ## the plan instead of building something out of step.
  stale <- entry
  stale$snapshot <- list(object_md5 = strrep("c", 32L))
  expect_identical(
    .builder_plan_region_error(stale)$error_code,
    "stale_region_subset"
  )
  with_trajectory <- entry
  with_trajectory$settings$included_trajectories <- list(monocle2 = "lineage")
  expect_identical(
    .builder_plan_region_error(with_trajectory)$error_code,
    "region_subset_with_trajectories"
  )
  ## A dataset that came back from a Project is already built.
  built <- stale
  built$load_state <- "artifact_ready"
  expect_null(.builder_plan_region_error(built))
})

test_that("a Trekker slide builds a CRB with only the kept tissue", {
  object <- trekker_object()
  record <- list(
    id = "trekker_region_build",
    label = "Trekker one tissue",
    make = function() list(object = object, format = "Built-in example")
  )
  entry <- builder_e2e_entry(record)
  root <- withr::local_tempdir()
  entry$snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  sections <- list(
    slice1 = list(polygons = list(region_of_tissue(object, "T2")))
  )
  resolved <- builder_region_subset_resolve(object, sections, entry$levels)
  resolved$source_md5 <- tolower(entry$snapshot$object_md5)
  entry$settings$spatial_region_subset <- builder_region_subset_normalize(list(
    sections = sections,
    resolved = resolved
  ))

  plan <- builder_freeze_plan(
    list(entry),
    file.path(root, "release"),
    make_app = FALSE
  )
  expect_null(plan$error)
  broken <- entry
  broken$settings$spatial_region_subset$resolved$kept <- 99L
  expect_identical(
    builder_freeze_plan(
      list(broken),
      file.path(root, "release-broken"),
      make_app = FALSE
    )$error_code,
    "invalid_spatial_region_subset"
  )
  item <- plan$items[[1L]]
  expect_identical(item$cell_count, resolved$kept)
  expect_identical(item$artifact_identity$cells, resolved$cells)
  expect_identical(
    item$spatial_region_subset$sections$slice1$polygons[[1L]],
    sections$slice1$polygons[[1L]]
  )

  handle <- builder_coordinator_prepare(plan, entry$id)
  result <- builder_execute_plan(
    plan,
    handle$stage,
    stats::setNames(list(entry$snapshot), entry$id)
  )
  expect_identical(result$state, "success", info = format(result$error))
  expect_identical(result$verifications[[1L]]$cell_count, resolved$kept)
  result$build_id <- handle$build_id
  published <- builder_coordinator_publish(handle, result)
  expect_true(isTRUE(published$published))

  crb <- readCerebro(published$built[[1L]])
  meta <- crb$getMetaData()
  kept <- builder_region_subset_cells(object, sections)$kept
  expect_identical(nrow(meta), resolved$kept)
  expect_setequal(as.character(meta$cell_barcode), kept)
  ## Levels that no remaining cell has are not listed any more.
  expect_setequal(
    levels(meta$tissue_id),
    unique(as.character(object$tissue_id[match(kept, colnames(object))]))
  )
  expect_lt(nlevels(meta$tissue_id), 5L)
  for (name in names(crb$spatial)) {
    expect_true(
      all(crb$getSpatialData(name)$coordinates$cell_barcode %in% kept),
      info = name
    )
  }
  expect_identical(ncol(crb$getExpressionMatrix()), resolved$kept)
})

test_that("a changed region is not rebuilt from the earlier plan", {
  object <- trekker_object()
  record <- list(
    id = "trekker_region_key",
    label = "Trekker key",
    make = function() list(object = object, format = "Built-in example")
  )
  entry <- builder_e2e_entry(record)
  root <- withr::local_tempdir()
  entry$snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  freeze <- function(tissue) {
    sections <- list(
      slice1 = list(polygons = list(region_of_tissue(object, tissue)))
    )
    resolved <- builder_region_subset_resolve(object, sections, entry$levels)
    resolved$source_md5 <- tolower(entry$snapshot$object_md5)
    entry$settings$spatial_region_subset <- builder_region_subset_normalize(
      list(sections = sections, resolved = resolved)
    )
    plan <- builder_freeze_plan(
      list(entry),
      file.path(root, "release"),
      make_app = FALSE
    )
    expect_null(plan$error)
    plan$items[[1L]]
  }
  first <- freeze("T1")
  second <- freeze("T3")
  expect_false(identical(
    builder_plan_reuse_key(first),
    builder_plan_reuse_key(second)
  ))
})

test_that("a region subset survives the Project file and stays valid state", {
  skip_if_not_installed("jsonlite")
  cells <- builder_axis_identity(c("a", "b", "c"))
  subset <- builder_region_subset_normalize(list(
    sections = list(
      slice1 = list(
        polygons = list(
          square(0, 1, 0, 1),
          square(5, 7.5, 5, 9.25)
        )
      )
    ),
    resolved = list(
      source_md5 = strrep("d", 32L),
      cells = cells,
      kept = 3L,
      total = 10L,
      ## A group with one level and a group no remaining cell has.
      group_levels = list(tissue_id = "T1", batch = character()),
      sections = list(slice1 = list(kept = 3L, total = 10L))
    )
  ))
  stored <- jsonlite::toJSON(
    subset,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  restored <- jsonlite::fromJSON(as.character(stored), simplifyVector = FALSE)
  expect_equal(builder_region_subset_normalize(restored), subset)
})

test_that("the canvas draws regions in raw coordinates and sends them", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  output <- tryCatch(
    system2(
      "node",
      c(
        shQuote(testthat::test_path("builder-spatial-region-runtime-node.js")),
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
  skip_if(is.null(output), "node cannot be launched from this R test process")
  expect_identical(attr(output, "status") %||% 0L, 0L, info = output)
  result <- jsonlite::fromJSON(tail(output, 1L), simplifyVector = FALSE)

  expect_match(result$statusBefore, "All cells are kept")
  expect_identical(result$drawingPressed, "true")
  expect_match(result$statusWhileDrawing, "4 points")
  expect_match(result$statusAfterClose, "1 shape drawn")

  ## The clicks were placed on the screen positions of four points drawn with
  ## a 30 degree rotation and a 1.5 scale; the shape is those points in raw
  ## coordinates.
  expect_identical(result$applied$action, "apply")
  expect_identical(result$applied$viewKey, "view-a")
  polygon <- result$applied$polygons[[1L]]
  expect_equal(
    unlist(polygon$x),
    vapply(result$rawPoints, `[[`, numeric(1), "x"),
    tolerance = 1e-6
  )
  expect_equal(
    unlist(polygon$y),
    vapply(result$rawPoints, `[[`, numeric(1), "y"),
    tolerance = 1e-6
  )

  expect_match(result$statusAfterUndo, "1 point")
  expect_identical(result$eventsAfterShortApply, 0L)

  expect_match(result$statusStored, "Keeping 1,250 of 5,020 cells")
  expect_true(result$clearEnabled)
  expect_identical(result$clearEvent$action, "clear")
  expect_identical(result$clearAllEvent$action, "clear_all")

  ## Only the overlay of one section has raw coordinates to draw on.
  expect_identical(result$drawingInSeparate, "false")
  expect_match(result$statusSeparate, "All ROIs")
  expect_true(result$drawDisabledInSeparate)
  expect_true(result$drawDisabledInRoi)
  expect_identical(result$separateEvents, 0L)
})

test_that("a cut Trekker payload is still a valid Viewer payload", {
  payload <- .builder_content_spatial_demo_payload()
  evidence_cells <- vapply(payload$evidence, function(entry) entry$cell, 1L)
  keep <- payload$barcodes[sort(unique(c(2L, evidence_cells + 1L)))]
  expect_lt(length(keep), length(payload$barcodes))

  cut <- builder_region_subset_trekker(payload, keep)
  result <- builder_profile_trekker_payload(
    cut,
    builder_content_spatial_trekker_context(cut)
  )
  expect_true(result$valid, info = paste(result$diagnostics, collapse = ", "))
  expect_identical(result$normalized$cell_count, length(keep))
  ## The slide-wide figure keeps counting the whole slide.
  expect_gt(cut$meta$n_cells_full, cut$meta$n_cells)
  expect_identical(cut$barcodes, payload$barcodes[payload$barcodes %in% keep])
  expect_identical(
    vapply(cut$evidence, `[[`, "", "bc"),
    cut$barcodes[vapply(cut$evidence, `[[`, 1L, "cell") + 1L]
  )
})

test_that("a kept region builds into a generated Viewer app", {
  object <- trekker_object()
  record <- list(
    id = "trekker_region_app",
    label = "Trekker region app",
    make = function() list(object = object, format = "Built-in example")
  )
  entry <- builder_e2e_entry(record)
  root <- withr::local_tempdir()
  entry$snapshot <- builder_snapshot_seurat(
    object,
    file.path(root, "snapshot"),
    available_bytes = 2^40
  )
  sections <- list(
    slice1 = list(polygons = list(region_of_tissue(object, "T4")))
  )
  resolved <- builder_region_subset_resolve(object, sections, entry$levels)
  resolved$source_md5 <- tolower(entry$snapshot$object_md5)
  entry$settings$spatial_region_subset <- builder_region_subset_normalize(list(
    sections = sections,
    resolved = resolved
  ))

  plan <- builder_freeze_plan(
    list(entry),
    file.path(root, "release"),
    make_app = TRUE,
    app_options = list(
      initial_dataset = entry$id,
      initial_page = "data_info",
      launch_browser = FALSE
    ),
    app_auth = list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
  )
  expect_null(plan$error)
  handle <- builder_coordinator_prepare(plan, "region-app")
  result <- builder_execute_plan(
    plan,
    handle$stage,
    stats::setNames(list(entry$snapshot), entry$id)
  )
  expect_identical(result$state, "success", info = format(result$error))
  result$build_id <- handle$build_id
  published <- builder_coordinator_publish(handle, result)
  expect_true(isTRUE(published$published))
  expect_true(dir.exists(published$app_dir))
  crb <- readCerebro(published$built[[1L]])
  expect_identical(length(crb$getCellNames()), resolved$kept)
})
