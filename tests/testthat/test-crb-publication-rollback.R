test_that("every failed CRB publication stage preserves original bytes", {
  for (failed_call in seq_len(4L)) {
    root <- tempfile("crb-rollback-")
    dir.create(root)
    file <- file.path(root, "data.crb")
    stage <- file.path(root, "stage.crb")
    spatial <- file.path(root, "spatial")
    spatial_stage <- file.path(root, "spatial-stage")
    dir.create(spatial)
    dir.create(spatial_stage)
    writeBin(as.raw(1:10), file)
    writeBin(as.raw(11:20), stage)
    writeLines("old", file.path(spatial, "content"))
    writeLines("new", file.path(spatial_stage, "content"))
    install <- CerebroNexus:::.installCerebroPayload
    calls <- 0L
    environment(install) <- list2env(
      list(file.rename = function(from, to) {
        calls <<- calls + 1L
        if (calls == failed_call) {
          return(FALSE)
        }
        base::file.rename(from, to)
      }),
      parent = environment(install)
    )
    expect_error(install(stage, file, spatial_stage, spatial), "Could not")
    expect_identical(readBin(file, "raw", 100), as.raw(1:10))
    expect_identical(readLines(file.path(spatial, "content")), "old")
    unlink(root, recursive = TRUE)
  }
})

test_that("every failed publication stage restores the spatial and immune sidecars", {
  ## Renames in order: CRB backup, spatial backup, spatial install, immune
  ## backup, immune install, CRB install.
  for (failed_call in seq_len(6L)) {
    root <- tempfile("crb-rollback-immune-")
    dir.create(root)
    file <- file.path(root, "data.crb")
    stage <- file.path(root, "stage.crb")
    spatial <- file.path(root, "spatial")
    spatial_stage <- file.path(root, "spatial-stage")
    immune <- file.path(root, "data.immune.qs2")
    immune_stage <- file.path(root, "immune-stage")
    dir.create(spatial)
    dir.create(spatial_stage)
    writeBin(as.raw(1:10), file)
    writeBin(as.raw(11:20), stage)
    writeBin(as.raw(21:30), immune)
    writeBin(as.raw(31:40), immune_stage)
    writeLines("old", file.path(spatial, "content"))
    writeLines("new", file.path(spatial_stage, "content"))
    install <- CerebroNexus:::.installCerebroPayload
    calls <- 0L
    environment(install) <- list2env(
      list(file.rename = function(from, to) {
        calls <<- calls + 1L
        if (calls == failed_call) {
          return(FALSE)
        }
        base::file.rename(from, to)
      }),
      parent = environment(install)
    )
    expect_error(
      install(
        stage,
        file,
        spatial_stage,
        spatial,
        immune_stage = immune_stage,
        immune = immune
      ),
      "Could not",
      info = paste("failed call", failed_call)
    )
    expect_identical(readBin(file, "raw", 100), as.raw(1:10))
    expect_identical(readBin(immune, "raw", 100), as.raw(21:30))
    expect_identical(readLines(file.path(spatial, "content")), "old")
    expect_length(
      list.files(root, all.files = TRUE, pattern = "-b-|-backup-"),
      0L
    )
    unlink(root, recursive = TRUE)
  }
})

test_that("an immune-only install replaces the sidecar and leaves no backup", {
  root <- withr::local_tempdir()
  file <- file.path(root, "data.crb")
  stage <- file.path(root, "stage.crb")
  immune <- file.path(root, "data.immune.qs2")
  immune_stage <- file.path(root, "immune-stage")
  writeBin(as.raw(1:10), file)
  writeBin(as.raw(11:20), stage)
  writeBin(as.raw(21:30), immune)
  writeBin(as.raw(31:40), immune_stage)

  CerebroNexus:::.installCerebroPayload(
    stage,
    file,
    immune_stage = immune_stage,
    immune = immune
  )

  expect_identical(readBin(file, "raw", 100), as.raw(11:20))
  expect_identical(readBin(immune, "raw", 100), as.raw(31:40))
  expect_false(file.exists(immune_stage))
  expect_length(
    list.files(root, all.files = TRUE, pattern = "-b-|-backup-"),
    0L
  )
})

test_that("public save can replace a long CRB name using a bounded backup name", {
  skip_on_os("windows")
  root <- tempfile("crb-long-name-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, paste0(strrep("x", 233), ".crb"))
  object <- Cerebro$new()
  saveCerebro(object, file, codec = "rds")
  expect_silent(saveCerebro(object, file, codec = "rds"))
  ## Not byte-identical to the first save: with R_KEEP_PKG_SOURCE=yes (as in the
  ## R-CMD-check job) every method carries a source reference whose environment
  ## label (env::N) changes between saves. The replaced file must still load.
  expect_s3_class(readCerebro(file), "Cerebro")
  expect_identical(
    list.files(root, all.files = TRUE, no.. = TRUE),
    basename(file)
  )
})
