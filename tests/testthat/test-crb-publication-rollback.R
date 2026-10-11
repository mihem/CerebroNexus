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

test_that("public save preserves an existing CRB when its backup name is too long", {
  skip_on_os("windows")
  root <- tempfile("crb-long-name-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, paste0(strrep("x", 233), ".crb"))
  object <- Cerebro$new()
  saveCerebro(object, file, codec = "rds")
  original <- readBin(file, "raw", file.info(file)$size)
  expect_error(
    suppressWarnings(saveCerebro(object, file, codec = "rds")),
    "Could not preserve the existing CRB"
  )
  expect_identical(readBin(file, "raw", length(original)), original)
})
