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

test_that("Excel workbooks materialize every non-empty sheet privately", {
  skip_if_not_installed("readxl")
  workbook <- readxl::readxl_example("datasets.xlsx")
  stage <- tempfile("extra-workbook-stage-")
  dir.create(stage)

  bundle <- CerebroNexus:::.materializeExtraTables(
    CerebroNexus:::.bundleExtraTables(
      list("Reference workbook" = workbook),
      list("Reference workbook" = list("Cars" = "mtcars"))
    ),
    stage
  )
  sheets <- bundle$files[[1L]]$sheets

  expect_identical(
    vapply(sheets, `[[`, character(1), "label"),
    c("Cars", "chickwts", "quakes")
  )
  expect_true(all(vapply(
    sheets,
    function(sheet) grepl("^private-data/extra-tables/", sheet$path),
    logical(1)
  )))
  tables <- lapply(sheets, function(sheet) {
    readRDS(file.path(stage, sheet$path))
  })
  expect_true(all(vapply(tables, is.data.frame, logical(1))))
  expect_true(all(vapply(tables, nrow, integer(1)) > 0L))

  expect_error(
    CerebroNexus:::.materializeExtraTables(
      CerebroNexus:::.bundleExtraTables(
        list("Reference workbook" = workbook),
        list("Reference workbook" = list("Missing" = "absent"))
      ),
      tempfile("missing-workbook-sheet-")
    ),
    "Mapped source sheet `absent` was not found or is empty",
    fixed = TRUE
  )
})

test_that("generated apps select and safely render external sheets", {
  skip_if_not_installed("readxl")
  root <- withr::local_tempdir()
  crb <- file.path(root, "dataset.crb")
  workbook <- readxl::readxl_example("datasets.xlsx")
  unsafe_csv <- file.path(root, "unsafe.csv")
  app <- file.path(root, "app")
  saveRDS(Cerebro$new(), crb)
  writeLines(
    c("=formula,html", "=1+1,<script>alert(1)</script>"),
    unsafe_csv
  )

  createShinyApp(
    cerebro_data = c("Dataset" = crb),
    result_dir = app,
    extra_tables = list(
      "Reference workbook" = workbook,
      "Unsafe table" = unsafe_csv
    ),
    extra_tables_sheets = list(
      "Reference workbook" = list("Cars" = "mtcars")
    ),
    launch_browser = FALSE,
    verbose = FALSE
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  runtime <- new.env()
  runtime$Cerebro.options <- config
  runtime$Cerebro.options$cerebro_root <- app
  source(file.path(app, "viewer", "utility_functions.R"), local = runtime)
  groups <- runtime$extra_material_table_groups(config$extra_tables, NULL)

  expect_identical(
    runtime$extra_material_table_choices(groups),
    c(
      "Reference workbook" = "external-file:1",
      "Unsafe table" = "external-file:2"
    )
  )
  workbook_sheet <- config$extra_tables$files[[1L]]$sheets[[1L]]
  pending <- runtime$extra_material_table_selection(
    groups,
    file_key = "external-file:1",
    sheet_key = workbook_sheet$key,
    load = FALSE
  )
  expect_identical(pending$sheet$label, "Cars")
  expect_null(pending$sheet$table)
  expect_false(exists(
    workbook_sheet$key,
    envir = runtime$extra_material_external_table_cache,
    inherits = FALSE
  ))

  loaded_workbook <- runtime$extra_material_table_selection(
    groups,
    file_key = "external-file:1",
    sheet_key = workbook_sheet$key
  )
  expect_s3_class(loaded_workbook$sheet$table, "data.frame")
  expect_true(exists(
    workbook_sheet$key,
    envir = runtime$extra_material_external_table_cache,
    inherits = FALSE
  ))

  unsafe_sheet <- config$extra_tables$files[[2L]]$sheets[[1L]]
  loaded_unsafe <- runtime$extra_material_table_selection(
    groups,
    file_key = "external-file:2",
    sheet_key = unsafe_sheet$key
  )
  expect_identical(names(loaded_unsafe$sheet$table), c("'=formula", "html"))
  expect_identical(loaded_unsafe$sheet$table[[1L]], "'=1+1")
  expect_identical(
    loaded_unsafe$sheet$table[[2L]],
    "<script>alert(1)</script>"
  )

  widget <- runtime$prettifyTable(
    loaded_unsafe$sheet$table,
    filter = "none",
    dom = "Bfrtip",
    escape = TRUE,
    show_buttons = TRUE,
    download_file_name = "external_table"
  )
  expect_identical(attr(widget$x$options, "escapeIdx"), "true")
  expect_identical(widget$x$data, loaded_unsafe$sheet$table)
  download_buttons <- widget$x$options$buttons[[2L]]$buttons
  expect_identical(
    vapply(download_buttons, `[[`, character(1), "extend"),
    c("csv", "excel")
  )
  expect_true(all(vapply(
    download_buttons,
    function(button) identical(button$filename, "external_table"),
    logical(1)
  )))
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
