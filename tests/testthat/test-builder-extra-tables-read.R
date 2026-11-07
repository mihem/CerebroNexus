builder_repo_source("extras.R", local = environment())

builder_test_read_table <- function(lines, filename, ...) {
  path <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
  writeLines(lines, path)
  builder_read_table_source(list(
    source_path = path,
    file_name = filename,
    sheet_name = tools::file_path_sans_ext(filename),
    ...
  ))
}

test_that("supplementary CSV cells keep their exact text", {
  got <- builder_test_read_table(
    c(
      "id,big,hex,gene,p_val",
      "007,123456789012345678,0x1A,NA,0.05",
      "010,123456789012345679,0x2B,CD3D,1e-4"
    ),
    "table.csv"
  )

  expect_null(got$error)
  table <- got$table
  expect_identical(table$id, c("007", "010"))
  expect_identical(table$big, c("123456789012345678", "123456789012345679"))
  expect_identical(table$hex, c("0x1A", "0x2B"))
  expect_identical(table$gene, c("NA", "CD3D"))
  expect_identical(table$p_val, c(0.05, 1e-4))
})

test_that("a stray quote in a TSV keeps every row", {
  got <- builder_test_read_table(
    c(
      "\"gene\"\tnote",
      "CD3D\t5\" probe",
      "MS4A1\tplain",
      "\"NKG7\"\t\"say \"\"hi\"\"\""
    ),
    "table.tsv"
  )

  expect_null(got$error)
  expect_identical(names(got$table), c("gene", "note"))
  expect_identical(got$table$gene, c("CD3D", "MS4A1", "NKG7"))
  expect_identical(got$table$note, c("5\" probe", "plain", "say \"hi\""))
})

test_that("the delimiter is detected for TXT and semicolon CSV files", {
  comma <- builder_test_read_table(
    c("gene,score", "CD3D,1", "MS4A1,2"),
    "comma.txt"
  )
  expect_identical(names(comma$table), c("gene", "score"))
  expect_identical(comma$table$score, c(1L, 2L))

  semicolon <- builder_test_read_table(
    c("gene;logFC", "CD3D;1,5", "MS4A1;-0,25"),
    "semicolon.csv"
  )
  expect_identical(names(semicolon$table), c("gene", "logFC"))
  expect_identical(semicolon$table$logFC, c(1.5, -0.25))
})

test_that("an unreadable table says why", {
  got <- builder_test_read_table(
    c("a,b", "1,2,3", "4,5"),
    "broken.csv"
  )
  expect_match(
    got$error,
    "broken.csv: broken could not be read (",
    fixed = TRUE
  )
  expect_false(grepl("could not be read.$", got$error))

  quote <- builder_test_read_table(
    c("a,b", "1,\"open", "2,3"),
    "quote.csv"
  )
  expect_match(quote$error, "quote is not closed", fixed = TRUE)
})

test_that("Excel column types are guessed from every row", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  ## Text appears only after more than 1000 empty rows.
  writexl::write_xlsx(
    list(
      Sheet1 = data.frame(
        id = c(rep(NA, 1100L), "007"),
        gene = c(rep("CD3D", 1100L), "NA"),
        score = c(seq_len(1100L) / 2, 0.25),
        day = as.Date("2024-01-01") + 0:1100
      )
    ),
    path
  )

  got <- builder_read_table_source(list(
    source_path = path,
    file_name = "numbers.xlsx",
    sheet_name = "Sheet1"
  ))

  expect_null(got$error)
  expect_identical(got$table$id[[1101L]], "007")
  expect_identical(got$table$gene[[1101L]], "NA")
  expect_identical(got$table$score[[1101L]], 0.25)
  expect_s3_class(got$table$day, "POSIXct")
})

test_that("a missing readxl names the package", {
  reader <- builder_read_table_source
  env <- new.env(parent = environment(reader))
  env$requireNamespace <- function(...) FALSE
  environment(reader) <- env
  path <- tempfile(fileext = ".xlsx")
  writeLines("x", path)

  got <- reader(list(
    source_path = path,
    file_name = "tables.xlsx",
    sheet_name = "Sheet1"
  ))

  expect_match(got$error, "install the readxl package", fixed = TRUE)
  inventory <- builder_table_inventory
  environment(inventory) <- env
  expect_match(
    inventory(path, "tables.xlsx")$error,
    "Install the readxl package",
    fixed = TRUE
  )
})

test_that("a table name with a trailing space keeps its format", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), path)
  inventory <- builder_table_inventory(path, "table.csv ")
  expect_null(inventory$error)
  expect_identical(names(inventory), "table")
  expect_identical(
    builder_table_inventory(path, "table")$error,
    paste0(
      "Supported table formats are CSV, TSV, TXT, XLS, XLSX and XLSM; ",
      "this file name has no extension."
    )
  )
})
