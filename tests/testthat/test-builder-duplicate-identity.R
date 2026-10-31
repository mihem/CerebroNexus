builder_plan_contract_source_runtime(environment())
builder_repo_source("extras.R", local = environment())

builder_duplicate_project_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "prerequisite.R"),
    envir = runtime
  )
  runtime$builder_source_utf8(
    builder_profile_inst_path("builder", "core", "bundle_path_contract.R"),
    envir = runtime
  )
  runtime$builder_source_utf8(
    builder_profile_inst_path(
      "viewer",
      "core",
      "spatial_coordinate_transform.R"
    ),
    envir = runtime
  )
  for (file in c("io.R", "worker.R", "extras.R", "project.R")) {
    runtime$builder_source_utf8(
      builder_profile_inst_path("builder", file),
      envir = runtime
    )
  }
  runtime
}

test_that("stored sources may be added again while pending events stay unique", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "ui", "dataset_rail.R"),
    envir = runtime
  )
  root <- withr::local_tempdir()
  source <- file.path(root, "dataset.rds")
  writeBin(charToRaw("same dataset"), source)
  entries <- list(list(id = "ds1", path = source, example = NULL))

  repeated <- runtime$builder_source_reserve(
    entries,
    character(),
    "file",
    source
  )
  expect_true(repeated$ok)
  expect_identical(repeated$code, "source_in_store")
  project_runtime <- builder_duplicate_project_runtime()
  first_id <- project_runtime$builder_project_allocate_dataset_id(0L)
  second_id <- project_runtime$builder_project_allocate_dataset_id(
    first_id$sequence,
    first_id$id
  )
  expect_identical(c(first_id$id, second_id$id), c("ds1", "ds2"))

  double_click <- runtime$builder_source_reserve(
    entries,
    repeated$pending,
    "file",
    source
  )
  expect_false(double_click$ok)
  expect_identical(double_click$code, "source_pending")
})

test_that("different paths with the same MD5 remain separate import sources", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "ui", "dataset_rail.R"),
    envir = runtime
  )
  root <- withr::local_tempdir()
  first <- file.path(root, "first.rds")
  second <- file.path(root, "second.rds")
  writeBin(charToRaw("identical bytes"), first)
  expect_true(file.copy(first, second))
  expect_identical(unname(tools::md5sum(first)), unname(tools::md5sum(second)))

  reserved <- runtime$builder_source_reserve(
    list(list(id = "ds1", path = first, example = NULL)),
    character(),
    "file",
    second
  )
  expect_true(reserved$ok)
  expect_null(reserved$code)
  expect_false(identical(
    runtime$builder_source_key("file", first),
    runtime$builder_source_key("file", second)
  ))
})

test_that("duplicate labels preserve independent parameters and alignment", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    image_path <- withr::local_tempfile(fileext = ".png")
    write_dummy_png(image_path)
    inspected <- builder_read_image(image_path)
    make_entry <- function(id, dx, rotation, point_size) {
      entry <- builder_task6_entry()
      entry$id <- id
      entry$source_id <- id
      entry$output_id <- id
      entry$settings$name <- "Repeated"
      entry$settings$overview_point_size <- point_size
      entry$snapshot$source_fingerprint <- "same-source-fingerprint"
      record <- builder_alignment_record(
        source = list(name = "histology.png", type = "image/png"),
        base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
        parameters = list(dx = dx, rotation = rotation),
        section = list(id = "fov", kind = "spatial"),
        source_path = inspected$source_path
      )
      record$source_content_md5 <- inspected$source_content_md5
      record$outside <- 0L
      entry$dataset_profile$spatial <- list(sections = "fov")
      entry$settings$images <- list(fov = list(histology = record))
      entry$settings$spatial_image_storage <- "external"
      entry
    }
    entries <- list(
      make_entry("ds4", 2, 15, 4),
      make_entry("ds7", -3, 75, 9)
    )

    plan <- builder_freeze_plan(entries, tempdir(), make_app = TRUE)

    expect_null(plan$error)
    expect_identical(plan$dataset_order, c("ds4", "ds7"))
    expect_identical(
      vapply(plan$items, `[[`, character(1), "name"),
      rep("Repeated", 2L)
    )
    expect_identical(
      vapply(plan$items, `[[`, numeric(1), "overview_point_size"),
      c(4, 9)
    )
    expect_identical(plan$items[[1L]]$images$fov$histology$dx, 2)
    expect_identical(plan$items[[2L]]$images$fov$histology$dx, -3)
    expect_identical(plan$items[[1L]]$images$fov$histology$rotation, 15)
    expect_identical(plan$items[[2L]]$images$fov$histology$rotation, 75)
  })
})

test_that("BuildPlan rejects duplicate ids and filenames", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    first <- builder_task6_entry()
    second <- builder_task6_entry()
    second$id <- "dataset-b"
    second$source_id <- "dataset-b"
    second$output_id <- "dataset-b"

    duplicate_id <- second
    duplicate_id$id <- first$id
    expect_identical(
      builder_freeze_plan(list(first, duplicate_id), tempdir())$error_code,
      "duplicate_dataset_id"
    )

    builder_item_filename <- function(item, index, total) {
      if (index == 1L) "Same.crb" else "same.crb"
    }
    expect_identical(
      builder_freeze_plan(list(first, second), tempdir())$error_code,
      "duplicate_dataset_filename"
    )
  })
})

test_that("source path case follows the host filesystem contract", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "ui", "dataset_rail.R"),
    envir = runtime
  )
  upper <- runtime$builder_source_key("file", "C:/Data/Sample.RDS")
  lower <- runtime$builder_source_key("file", "c:/data/sample.rds")

  if (identical(.Platform$OS.type, "windows")) {
    expect_identical(upper, lower)
  } else {
    expect_false(identical(upper, lower))
  }
  expect_identical(
    names(formals(runtime$builder_source_key)),
    c("kind", "value")
  )
})
