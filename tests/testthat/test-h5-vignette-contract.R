extract_vignette_chunk <- function(lines, label) {
  header_pattern <- paste0(
    "^```\\{r[[:space:]]+",
    label,
    "([,}])"
  )
  header <- grep(header_pattern, lines)
  if (length(header) != 1L) {
    stop("Expected exactly one '", label, "' chunk.", call. = FALSE)
  }

  remainder <- lines[seq.int(header + 1L, length(lines))]
  closing <- which(trimws(remainder) == "```")
  if (length(closing) == 0L || closing[[1L]] == 1L) {
    stop("Chunk '", label, "' is empty or unclosed.", call. = FALSE)
  }
  paste(remainder[seq_len(closing[[1L]] - 1L)], collapse = "\n")
}

test_that("legacy H5 guidance persists a portable backend descriptor", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("Matrix")

  vignette_path <- test_path(
    "..",
    "..",
    "vignettes",
    "create_expression_matrix_in_h5_format.Rmd"
  )
  skip_if_not(
    file.exists(vignette_path),
    "static source-tree vignette contract"
  )

  lines <- readLines(vignette_path, warn = FALSE)
  convert_code <- extract_vignette_chunk(lines, "legacy-convert")
  save_code <- extract_vignette_chunk(lines, "legacy-save")
  launch_code <- extract_vignette_chunk(lines, "legacy-launch")

  convert_expression <- expect_silent(parse(text = convert_code))
  save_expression <- expect_silent(parse(text = save_code))
  launch_expression <- expect_silent(parse(text = launch_code))

  test_root <- withr::local_tempdir()
  crb_input <- file.path(test_root, "input.crb")
  crb <- Cerebro$new()
  crb$expression <- Matrix::Matrix(
    matrix(
      seq_len(6L),
      nrow = 2L,
      dimnames = list(c("gene-a", "gene-b"), c("cell-1", "cell-2", "cell-3"))
    ),
    sparse = TRUE
  )
  expected_on_disk <- as.matrix(Matrix::t(crb$expression))
  saveRDS(crb, crb_input)
  evaluation <- rlang::env(
    system.file = function(...) crb_input,
    tempdir = function() test_root
  )

  expect_silent(eval(convert_expression, envir = evaluation))
  expect_true(file.exists(evaluation$h5_output))
  on_disk <- HDF5Array::TENxMatrix(
    evaluation$h5_output,
    group = "expression"
  )
  expect_identical(as.matrix(on_disk), expected_on_disk)

  expect_silent(eval(save_expression, envir = evaluation))
  expect_true(file.exists(evaluation$crb_output))

  saved <- .readCerebroPayload(evaluation$crb_output)
  backend <- saved$getExpressionBackend()
  expect_null(saved$expression)
  expect_identical(
    backend,
    list(type = "h5", location = basename(evaluation$h5_output))
  )
  expect_identical(
    dirname(evaluation$crb_output),
    dirname(evaluation$h5_output)
  )
  expect_true(file.exists(file.path(
    dirname(evaluation$crb_output),
    backend$location
  )))

  launch_capture <- rlang::env(arguments = NULL)
  evaluation$launchCerebro <- function(...) {
    launch_capture$arguments <- list(...)
    invisible(NULL)
  }
  expect_silent(eval(launch_expression, envir = evaluation))
  expect_identical(
    names(launch_capture$arguments),
    "crb_file_to_load"
  )
  expect_identical(
    launch_capture$arguments$crb_file_to_load,
    evaluation$crb_output
  )
  expect_false(any(grepl(
    "expression_matrix_h5 = h5_output",
    lines,
    fixed = TRUE
  )))
})
