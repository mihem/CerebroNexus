test_that("table label normalization filters stale entries and private source metadata", {
  index <- list(
    stable = list(
      workbook_name = " Renamed workbook ",
      display_name = " Sheet ",
      file_name = "/private/source.xlsx"
    ),
    stale = list(display_name = "Gone")
  )
  expect_identical(
    viewerExtraMaterialTableIndex(index, "stable"),
    list(
      stable = list(workbook_name = "Renamed workbook", display_name = "Sheet")
    )
  )
  expect_identical(
    viewerExtraMaterialTableIndex(list(
      a = list(workbook_name = " ", display_name = NA_character_)
    )),
    list()
  )
})

test_that("Builder builds portable CRBs and an app with workbook and sheet renames", {
  root <- Sys.getenv("CEREBRO_EXTRA_LABEL_QA_ROOT", "")
  if (!nzchar(root)) {
    root <- withr::local_tempdir()
  }
  bundle <- builder_extra_label_roundtrip(root)
  runtime <- new.env(parent = globalenv())
  sys.source(file.path(bundle$app_dir, "viewer/utility_functions.R"), runtime)
  runtime$Cerebro.options <- bundle$config
  for (i in seq_along(bundle$crbs)) {
    data <- bundle$crbs[[i]]
    id <- bundle$plan$items[[i]]$id
    suffix <- c("a", "b")[[i]]
    index <- bundle$config$viewer_content[[id]]$extra_material_table_index
    expect_identical(
      index$stable_one$workbook_name,
      paste("Renamed markers", suffix)
    )
    expect_identical(
      index$stable_three$workbook_name,
      paste("Renamed DE", suffix)
    )
    expect_identical(index$stable_two$display_name, "Second renamed sheet")
    portable <- data$getTechnicalInfo()$extra_material_table_index
    expect_identical(portable[names(index)], index)
    expect_identical(
      portable$source_only$workbook_name,
      paste("Source", suffix)
    )
    expect_identical(data$getExtraMaterialCategories(), "tables")
    runtime$data_set <- local({
      value <- data
      function() value
    })
    runtime$viewer_current_dataset_key <- local({
      value <- id
      function() value
    })
    groups <- runtime$extra_material_table_groups()
    expect_identical(
      unname(vapply(groups, `[[`, character(1), "label")),
      c(
        paste("Source", suffix),
        paste("Renamed markers", suffix),
        paste("Renamed DE", suffix)
      )
    )
    expect_identical(groups[[2L]]$sheets[[1L]]$label, "Shared sheet name")
    expect_identical(groups[[3L]]$sheets[[1L]]$label, "Shared sheet name")
    expect_identical(
      groups[[2L]]$sheets[[1L]]$table$value,
      paste(suffix, "one")
    )
    expect_identical(
      groups[[3L]]$sheets[[1L]]$table$value,
      paste(suffix, "three")
    )
    runtime$Cerebro.options$viewer_content[[id]] <- NULL
    expect_identical(runtime$extra_material_table_groups(), groups)
  }
})
