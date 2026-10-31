## A Builder project is one file in a folder: what the user chose for each
## dataset and where its source is. These tests cover the file and the rules for
## opening it; the Shiny side (Save, Open, restoring the workspace) is exercised
## in test-builder-stage-server.R and the browser tests.

project_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  builder_repo_source("project.R", local = runtime)
  runtime
}

project_entry <- function(id = "ds1", path = NULL, origin = "local", ...) {
  entry <- builder_minimal_entry(id, "PBMC")
  entry$path <- path
  entry$filename <- if (is.null(path)) "pbmc.rds" else basename(path)
  entry$source_origin <- origin
  entry$format <- "seurat"
  entry$revision <- 2L
  entry$settings$assay <- "RNA"
  entry$settings$layer <- "data"
  modifyList(entry, list(...))
}

project_source_file <- function(name = "pbmc.rds") {
  path <- file.path(withr::local_tempdir(.local_envir = parent.frame()), name)
  writeLines("data", path)
  normalizePath(path, winslash = "/")
}

test_that("a project file keeps the choices and the source path of each dataset", {
  runtime <- project_runtime()
  source <- project_source_file()
  folder <- withr::local_tempdir()
  entry <- project_entry(path = source)
  entry$settings$groups <- c("cluster", "sample")
  entry$acknowledgements <- c("spatial_images_missing")
  manifest <- runtime$builder_project_snapshot(
    "My project",
    list(entry),
    checked_ids = "ds1"
  )
  path <- runtime$builder_project_write(folder, manifest)
  expect_identical(basename(path), "builder-project.rds")
  expect_identical(
    list.files(folder, all.files = TRUE, no.. = TRUE),
    "builder-project.rds"
  )

  saved <- runtime$builder_project_read(path)
  expect_identical(saved$project$name, "My project")
  expect_length(saved$datasets, 1L)
  record <- saved$datasets[[1L]]
  expect_identical(record$id, "ds1")
  expect_true(record$checked)
  expect_identical(record$source$kind, "file")
  expect_identical(record$source$origin, "local")
  expect_identical(record$source$path, source)
  expect_identical(record$source$bytes, as.double(file.size(source)))
  expect_identical(record$configuration$settings$groups, c("cluster", "sample"))
  expect_identical(
    record$configuration$acknowledgements,
    "spatial_images_missing"
  )
  expect_identical(record$configuration$revision, 2L)
})

test_that("an upload is recorded by name, an example by its id, neither by a path", {
  runtime <- project_runtime()
  upload <- project_entry(
    "ds1",
    path = project_source_file(),
    origin = "upload"
  )
  example <- project_entry(
    "ds2",
    path = NULL,
    origin = "example",
    example = "pbmc"
  )
  manifest <- runtime$builder_project_snapshot("p", list(upload, example))
  expect_null(manifest$datasets[[1L]]$source$path)
  expect_identical(manifest$datasets[[1L]]$source$origin, "upload")
  expect_identical(manifest$datasets[[2L]]$source$kind, "example")
  expect_identical(manifest$datasets[[2L]]$source$example, "pbmc")
})

test_that("writing replaces the file in one step and leaves no partial file", {
  runtime <- project_runtime()
  folder <- withr::local_tempdir()
  first <- runtime$builder_project_snapshot("first", list())
  second <- runtime$builder_project_snapshot("second", list())
  runtime$builder_project_write(folder, first)
  runtime$builder_project_write(folder, second)
  expect_identical(
    runtime$builder_project_read(runtime$builder_project_manifest_path(
      folder
    ))$project$name,
    "second"
  )
  expect_identical(
    list.files(folder, all.files = TRUE, no.. = TRUE),
    "builder-project.rds"
  )
})

test_that("a project is plain data: code never goes in or comes out", {
  runtime <- project_runtime()
  folder <- withr::local_tempdir()
  manifest <- runtime$builder_project_snapshot("p", list())
  manifest$project$callback <- function() 1
  expect_error(
    runtime$builder_project_write(folder, manifest),
    "cannot be saved"
  )

  manifest$project$callback <- NULL
  manifest$project$env <- new.env()
  expect_error(
    runtime$builder_project_write(folder, manifest),
    "cannot be saved"
  )

  tampered <- runtime$builder_project_snapshot("p", list())
  tampered$project$callback <- function() 1
  path <- file.path(folder, "builder-project.rds")
  saveRDS(tampered, path)
  expect_error(runtime$builder_project_read(path), "not a Builder project file")
})

test_that("a file that is not a project of this version is refused", {
  runtime <- project_runtime()
  folder <- withr::local_tempdir()
  path <- file.path(folder, "builder-project.rds")
  expect_error(runtime$builder_project_read(path), "not found")
  writeLines("not an rds file", path)
  expect_error(runtime$builder_project_read(path), "not a Builder project file")
  saveRDS(list(format = "something else"), path)
  expect_error(runtime$builder_project_read(path), "not a Builder project file")

  manifest <- runtime$builder_project_snapshot("p", list())
  manifest$schema_version <- 99L
  saveRDS(manifest, path)
  expect_error(runtime$builder_project_read(path), "different Builder version")

  manifest <- runtime$builder_project_snapshot("p", list(project_entry()))
  manifest$datasets[[1L]]$id <- "../escape"
  saveRDS(manifest, path)
  expect_error(runtime$builder_project_read(path), "damaged dataset record")

  manifest <- runtime$builder_project_snapshot(
    "p",
    list(project_entry("ds1"), project_entry("ds2"))
  )
  manifest$datasets[[2L]]$id <- "ds1"
  manifest$datasets[[2L]]$configuration$id <- "ds1"
  saveRDS(manifest, path)
  expect_error(runtime$builder_project_read(path), "damaged dataset record")
})

test_that("a folder can hold a project when it exists and can be written", {
  runtime <- project_runtime()
  folder <- withr::local_tempdir()
  expect_identical(runtime$builder_project_folder_state(folder)$kind, "new")
  runtime$builder_project_write(
    folder,
    runtime$builder_project_snapshot("p", list())
  )
  expect_identical(runtime$builder_project_folder_state(folder)$kind, "project")
  expect_error(
    runtime$builder_project_folder_state(file.path(folder, "missing")),
    "existing folder"
  )
  expect_error(runtime$builder_project_folder_state(NULL), "existing folder")
})

test_that("the restore plan says which datasets can be loaded again", {
  runtime <- project_runtime()
  present <- project_source_file()
  gone <- file.path(dirname(present), "gone.rds")
  manifest <- runtime$builder_project_snapshot(
    "p",
    list(
      project_entry("ds1", path = present),
      project_entry("ds2", path = gone),
      project_entry("ds3", path = present, origin = "upload"),
      project_entry("ds4", path = NULL, origin = "example", example = "pbmc"),
      project_entry("ds5", path = NULL, origin = "example", example = "gone")
    )
  )
  plan <- runtime$builder_project_restore_plan(manifest, examples = "pbmc")
  expect_identical(
    vapply(plan, `[[`, "", "status"),
    c(
      ds1 = "ready",
      ds2 = "missing",
      ds3 = "upload",
      ds4 = "ready",
      ds5 = "missing"
    )
  )
  expect_match(plan$ds2$reason, "gone.rds", fixed = TRUE)
  expect_match(plan$ds3$reason, "Add it again")

  summary <- runtime$builder_project_restore_summary(plan, failed = "x: broke")
  expect_identical(summary$upload, "PBMC")
  expect_identical(summary$missing, c("PBMC", "PBMC"))
  expect_identical(summary$failed, "x: broke")
  expect_true(all(
    vapply(plan, function(item) is.character(item$name), logical(1))
  ))
})

test_that("saved settings go back onto the freshly loaded dataset", {
  runtime <- project_runtime()
  saved_entry <- project_entry(path = project_source_file())
  saved_entry$settings$name <- "My PBMC"
  saved_entry$settings$groups <- c("cluster")
  saved_entry$acknowledgements <- "a"
  saved_entry$revision <- 5L
  manifest <- runtime$builder_project_snapshot("p", list(saved_entry))
  record <- manifest$datasets[[1L]]

  loaded <- project_entry(path = record$source$path)
  loaded$settings$name <- "PBMC"
  loaded$settings$groups <- character()
  loaded$settings$recommendations <- list(metadata = "fresh")
  loaded$revision <- 1L
  restored <- runtime$builder_project_finalize_loaded_entry(loaded, record)
  expect_identical(restored$settings$name, "My PBMC")
  expect_identical(restored$settings$groups, "cluster")
  expect_identical(restored$acknowledgements, "a")
  expect_identical(restored$revision, 5L)
  expect_identical(restored$settings$recommendations, list(metadata = "fresh"))

  expect_error(
    runtime$builder_project_finalize_loaded_entry(
      modifyList(loaded, list(id = "ds9")),
      record
    ),
    "does not match"
  )
})

test_that("saved settings that do not fit the source leave the defaults", {
  runtime <- project_runtime()
  saved_entry <- project_entry(path = project_source_file())
  manifest <- runtime$builder_project_snapshot("p", list(saved_entry))
  record <- manifest$datasets[[1L]]
  loaded <- project_entry(path = record$source$path)
  loaded$profile$assay_profiles <- list(
    ATAC = list(layers = c("counts"))
  )
  loaded$settings$assay <- "ATAC"
  loaded$settings$layer <- "counts"
  loaded$revision <- 1L
  restored <- runtime$builder_project_finalize_loaded_entry(loaded, record)
  expect_identical(restored$settings$assay, "ATAC")
  expect_identical(restored$revision, 1L)
})

test_that("a saved picture whose file is gone does not hide the one the source brought", {
  runtime <- project_runtime()
  picture <- project_source_file("tissue.png")
  gone <- file.path(dirname(picture), "gone.png")
  saved_entry <- project_entry(path = project_source_file())
  saved_entry$settings$images <- list(
    sec1 = list(
      kept = list(source_path = picture),
      dead = list(source_path = gone),
      embedded = list(source_path = gone, uri = "data:image/png;base64,AAAA")
    )
  )
  manifest <- runtime$builder_project_snapshot("p", list(saved_entry))
  record <- manifest$datasets[[1L]]
  loaded <- project_entry(path = record$source$path)
  loaded$settings$images <- list(
    sec1 = list(seeded = list(source_path = picture))
  )
  restored <- runtime$builder_project_finalize_loaded_entry(loaded, record)
  expect_setequal(
    names(restored$settings$images$sec1),
    c("seeded", "kept", "embedded")
  )
})

test_that("a saved picture from a gone session folder is rebound to the source's own", {
  runtime <- project_runtime()
  fresh_file <- project_source_file("fresh.png")
  gone <- file.path(dirname(fresh_file), "old-session", "tissue.png")
  record <- function(path, scale) {
    list(
      source_path = path,
      source_content_md5 = "abc",
      scale = scale,
      dx = 3,
      source = list(name = "hires")
    )
  }
  saved_entry <- project_entry(path = project_source_file())
  saved_entry$settings$images <- list(sec1 = list(hires = record(gone, 2)))
  manifest <- runtime$builder_project_snapshot("p", list(saved_entry))
  loaded <- project_entry(path = manifest$datasets[[1L]]$source$path)
  loaded$settings$images <- list(sec1 = list(hires = record(fresh_file, 1)))
  restored <- runtime$builder_project_finalize_loaded_entry(
    loaded,
    manifest$datasets[[1L]]
  )
  expect_identical(restored$settings$images$sec1$hires$scale, 2)
  expect_identical(restored$settings$images$sec1$hires$dx, 3)
  expect_identical(restored$settings$images$sec1$hires$source_path, fresh_file)
  # Opening a project that changed nothing leaves it saved, whatever the paths.
  expect_false(runtime$builder_project_live_dirty(list(restored), manifest))

  # A picture whose content changed is not the saved one.
  other <- loaded
  other$settings$images$sec1$hires$source_content_md5 <- "different"
  restored_other <- runtime$builder_project_finalize_loaded_entry(
    other,
    manifest$datasets[[1L]]
  )
  expect_identical(restored_other$settings$images$sec1$hires$scale, 1)
})

test_that("the live workspace is compared with the saved project", {
  runtime <- project_runtime()
  entry <- project_entry(path = project_source_file())
  manifest <- runtime$builder_project_snapshot("p", list(entry))
  expect_false(runtime$builder_project_live_dirty(list(entry), manifest))

  changed <- entry
  changed$settings$name <- "Another name"
  expect_true(runtime$builder_project_live_dirty(list(changed), manifest))
  expect_true(runtime$builder_project_live_dirty(list(), manifest))
  expect_true(runtime$builder_project_live_dirty(
    list(entry, project_entry("ds2")),
    manifest
  ))
  acknowledged <- entry
  acknowledged$acknowledgements <- "x"
  expect_true(runtime$builder_project_live_dirty(list(acknowledged), manifest))
})

test_that("a project does not copy the data", {
  runtime <- project_runtime()
  source <- project_source_file()
  folder <- withr::local_tempdir()
  runtime$builder_project_write(
    folder,
    runtime$builder_project_snapshot("p", list(project_entry(path = source)))
  )
  expect_true(file.size(file.path(folder, "builder-project.rds")) < 50000)
  expect_identical(
    list.files(folder, recursive = TRUE, all.files = TRUE),
    "builder-project.rds"
  )
})

test_that("Save writes the project file and Open brings the datasets back", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this project test.")
  }
  folder <- withr::local_tempdir()
  present <- project_source_file()
  gone <- file.path(dirname(present), "gone.rds")

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-project-open", snapshot_root = folder))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-project-open"))
    first <- project_entry("ds1", path = present)
    second <- project_entry("ds2", path = gone)
    second$settings$name <- "Moved dataset"
    first$snapshot <- list(source_fingerprint = "a")
    second$snapshot <- list(source_fingerprint = "b")
    use_state_only_fixture(list(first, second))
    live <- isolate(sets())
    dataset_check_marks(stats::setNames(
      vapply(live, app_env$builder_project_check_identity, character(1)),
      vapply(live, `[[`, "", "id")
    )[1L])

    create_builder_project_in_folder(app_env$builder_project_folder_state(
      folder
    ))
    expect_true(file.exists(file.path(folder, "builder-project.rds")))
    expect_identical(builder_project()$name, basename(folder))
    saved <- app_env$builder_project_read(file.path(
      folder,
      "builder-project.rds"
    ))
    expect_identical(
      vapply(saved$datasets, `[[`, "", "id"),
      c("ds1", "ds2")
    )

    start_builder_project_open(folder)
    expect_identical(builder_project()$name, basename(folder))
    expect_identical(names(builder_project_pending_entries()), "ds1")
    expect_identical(builder_project_skipped_ids(), "ds2")
    expect_identical(builder_project_restore_outcome()$missing, "Moved dataset")
    expect_length(isolate(sets()), 0L)

    # The review compares the marks with the identities of the live entries
    # as character vectors: a restored mark must match one for one.
    expect_type(dataset_check_marks(), "character")
    loaded <- app_env$builder_project_finalize_loaded_entry(
      first,
      builder_project_pending_entries()$ds1
    )
    builder_project_commit_restored_entry(loaded)
    expect_identical(
      unname(dataset_check_marks()),
      unname(vapply(
        list(loaded),
        app_env$builder_project_check_identity,
        character(1)
      ))
    )
    expect_identical(names(dataset_check_marks()), "ds1")
  })
})
