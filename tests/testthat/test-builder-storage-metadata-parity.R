builder_e2e_source_runtime(environment())

skip_backend_if_unavailable <- function(backend) {
  if (identical(backend, "h5")) {
    testthat::skip_if_not_installed("HDF5Array")
  }
  invisible(backend)
}

builder_storage_parity_record <- function(section, label, root) {
  source_path <- file.path(
    root,
    paste0(section, "-", gsub("[^A-Za-z0-9]", "-", label), ".png")
  )
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), source_path)
  inspected <- builder_read_image(source_path)
  record <- builder_alignment_record(
    source = list(
      name = paste0(label, ".png"),
      type = "image/png",
      size = inspected$bytes
    ),
    base_bounds = list(xmin = -1e6, xmax = 1e6, ymin = -1e6, ymax = 1e6),
    parameters = list(image_opacity = 0.8),
    section = list(id = section, kind = "spatial"),
    source_path = inspected$source_path
  )
  record$source_content_md5 <- inspected$source_content_md5
  record
}

builder_storage_parity_entry <- function(
  root,
  expression_backend,
  spatial_image_storage
) {
  sections <- paste0("section_", letters[seq_len(6L)])
  template <- builder_example_catalog()[["complete_viewer_data"]]$make()$object
  template@misc <- template@misc["spatial_coordinate_system"]
  source_image <- template@images[[1L]]
  template@images <- stats::setNames(rep(list(source_image), 6L), sections)
  cells <- SeuratObject::Cells(template)
  template$orig.ident <- factor(rep("synthetic", length(cells)))
  groups <- paste0("group_", seq_len(8L))
  for (index in seq_along(groups)) {
    template[[groups[[index]]]] <- factor(
      rep(c("A", "B"), length.out = length(cells))
    )
  }
  images <- stats::setNames(
    lapply(sections, function(section) {
      list(`H&E` = builder_storage_parity_record(section, "H&E", root))
    }),
    sections
  )
  images[[sections[[1L]]]]$DAPI <- builder_storage_parity_record(
    sections[[1L]],
    "DAPI",
    root
  )

  entries <- list()
  snapshots <- list()
  dir.create(file.path(root, "snapshots"), mode = "0700")
  for (index in seq_len(2L)) {
    object <- unserialize(serialize(template, NULL, version = 3L))
    id <- paste0("storage-parity-", index)
    label <- paste("Storage parity", index)
    record <- list(
      id = id,
      label = label,
      make = local({
        value <- object
        function() {
          list(
            object = unserialize(serialize(value, NULL, version = 3L)),
            format = "Synthetic fixture"
          )
        }
      })
    )
    entry <- builder_e2e_entry(record)
    entry$settings$metadata_policy <- builder_e2e_review_metadata_policy(
      entry$settings$recommendations$metadata,
      groups
    )
    entry$settings$groups <- groups
    entry$settings$included_groups <- groups
    entry$settings$default_group <- groups[[1L]]
    entry$settings$analyses <- character()
    entry$settings$tables <- list()
    entry$settings$images <- images
    entry$settings$expression_backend <- expression_backend
    entry$settings$spatial_image_storage <- spatial_image_storage
    snapshot <- builder_snapshot_seurat(
      object,
      file.path(root, "snapshots", id),
      available_bytes = 2^40
    )
    entry$snapshot <- snapshot
    entries[[index]] <- entry
    snapshots[[id]] <- snapshot
  }
  list(entries = entries, snapshots = snapshots)
}

build_storage_parity_fixture <- function(
  expression_backend,
  spatial_image_storage
) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  fixture <- builder_storage_parity_entry(
    root,
    expression_backend = expression_backend,
    spatial_image_storage = spatial_image_storage
  )
  plan <- builder_freeze_plan(
    fixture$entries,
    file.path(root, "release"),
    make_app = TRUE
  )
  expect_null(plan$error)
  stage <- file.path(root, "stage")
  dir.create(stage, mode = "0700")
  result <- builder_execute_plan(
    plan,
    stage,
    snapshots = fixture$snapshots
  )
  expect_identical(result$state, "success", info = result$error)
  config <- readRDS(file.path(result$app_dir, "cerebro_config.rds"))
  crb <- readCerebro(file.path(
    result$app_dir,
    "private-data",
    plan$items[[1L]]$filename
  ))
  image_paths <- unlist(
    lapply(config$spatial_images, function(dataset) {
      unlist(
        lapply(dataset, function(section) {
          vapply(
            section,
            function(descriptor) {
              if (is.list(descriptor)) descriptor$path else descriptor
            },
            character(1)
          )
        }),
        use.names = FALSE
      )
    }),
    use.names = FALSE
  )
  spatial <- .builder_build_field(crb, "spatial") %||% list()
  list(
    crb = crb,
    backend = .builder_build_field(crb, "expression_backend") %||%
      list(type = "embedded"),
    config = config,
    image_paths_exist = file.exists(file.path(result$app_dir, image_paths)),
    builder_images_embedded = any(vapply(
      spatial,
      function(section) {
        length(section$histology_images %||% list()) > 0L
      },
      logical(1)
    )),
    plan = plan
  )
}

for (backend in c("embedded", "h5")) {
  test_that(paste("external image storage is independent of", backend), {
    skip_backend_if_unavailable(backend)
    result <- build_storage_parity_fixture(
      expression_backend = backend,
      spatial_image_storage = "external"
    )
    expect_identical(result$backend$type, backend)
    expect_true(all(result$image_paths_exist))
    expect_false(result$builder_images_embedded)
    expect_identical(length(result$plan$items), 2L)
    expect_identical(
      result$plan$items[[1L]]$spatial_alignment$section_count,
      6L
    )
    expect_identical(result$plan$items[[1L]]$spatial_alignment$image_count, 7L)
    expect_identical(length(result$plan$items[[1L]]$included_groups), 8L)
  })
}

test_that("embedded spatial image storage is rejected", {
  skip_if_not_installed("png")
  root <- withr::local_tempdir()
  fixture <- builder_storage_parity_entry(
    root,
    expression_backend = "embedded",
    spatial_image_storage = "embedded"
  )
  preflight <- .builder_plan_preflight_entries(fixture$entries, make_app = TRUE)

  expect_identical(preflight$error_code, "invalid_spatial_image_storage")
  expect_match(preflight$error, "external storage", fixed = TRUE)
})
