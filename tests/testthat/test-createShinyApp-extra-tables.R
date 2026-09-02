test_that("external table plan validates named regular files", {
  csv <- tempfile(fileext = ".csv")
  writeLines(c("cell,score", "cell-1,1"), csv)

  plan <- CerebroNexus:::.bundleExtraTables(list("QC table" = csv))

  expect_named(plan$files, "QC table")
  expect_equal(plan$files[[1L]]$extension, "csv")
  expect_error(
    CerebroNexus:::.bundleExtraTables(unname(c(csv))),
    "named collection"
  )
  expect_error(
    CerebroNexus:::.bundleExtraTables(list(first = csv, second = csv)),
    "only be included once"
  )
})

test_that("external tables become private lazy RDS assets", {
  csv <- tempfile(fileext = ".csv")
  writeLines(c("cell,score", "cell-1,1", "cell-2,2"), csv)
  stage <- tempfile("extra-table-stage-")
  dir.create(stage)

  bundle <- CerebroNexus:::.materializeExtraTables(
    CerebroNexus:::.bundleExtraTables(list("QC table" = csv)),
    stage
  )
  sheet <- bundle$files[[1L]]$sheets[[1L]]

  expect_equal(sheet$label, "QC table")
  expect_match(sheet$path, "^private-data/extra-tables/")
  expect_equal(
    readRDS(file.path(stage, sheet$path)),
    data.frame(cell = c("cell-1", "cell-2"), score = c(1L, 2L))
  )
})

test_that("external table RDS files use fast gzip compression", {
  target <- tempfile(fileext = ".rds")
  opened <- NULL
  open_gz <- function(file, open, compression) {
    opened <<- list(open = open, compression = compression)
    gzfile(file, open = open, compression = compression)
  }

  CerebroNexus:::.saveExtraTableRDS(
    data.frame(value = 1:3),
    target,
    open_gz = open_gz
  )

  expect_identical(opened, list(open = "wb", compression = 1L))
  expect_equal(readRDS(target), data.frame(value = 1:3))
  expect_true(is.function(CerebroNexus:::.bundleBuildOps()$save_extra_rds))
})

test_that("sheet renames are restricted to declared Excel files", {
  csv <- tempfile(fileext = ".csv")
  writeLines(c("cell,score", "cell-1,1"), csv)

  expect_error(
    CerebroNexus:::.bundleExtraTables(
      list("QC table" = csv),
      list("QC table" = list("Display" = "Sheet1"))
    ),
    "only map Excel"
  )
})
