source_file <- function(...) {
  testthat::test_path("..", "..", ...)
}

skip_if_not_source_tree <- function() {
  skip_if_not(
    file.exists(source_file(".Rbuildignore")),
    "static source-tree contract"
  )
}

test_that("development-only directories are excluded from package builds", {
  skip_if_not_source_tree()
  ignores <- readLines(source_file(".Rbuildignore"), warn = FALSE)
  expected <- c(
    "^\\.claude$",
    "^\\.loci$",
    "^\\.playwright-mcp$",
    "^\\.sisyphus$",
    "^\\.superpowers$"
  )

  expect_true(all(expected %in% ignores))
})

test_that("self-contained app vignette never purls interactive runApp calls", {
  skip_if_not_source_tree()
  lines <- readLines(
    source_file("vignettes", "create_a_self_contained_shiny_app.Rmd"),
    warn = FALSE
  )
  run_lines <- which(grepl("shiny::runApp(out_dir)", lines, fixed = TRUE))
  expect_length(run_lines, 2L)

  chunk_headers <- vapply(
    run_lines,
    function(line_number) {
      prior <- lines[seq_len(line_number)]
      prior[max(which(grepl("^```\\{r", prior)))]
    },
    character(1)
  )
  expect_true(all(grepl("purl=FALSE", chunk_headers, fixed = TRUE)))
})

test_that("summarisation has no unused unqualified ave call", {
  skip_if_not_source_tree()
  seurat_source <- paste(
    readLines(source_file("R", "seurat_utils.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("idx_first <- ave(", seurat_source, fixed = TRUE))
})

test_that("later remains declared because bundled runtime code uses it", {
  skip_if_not_source_tree()
  description <- read.dcf(source_file("DESCRIPTION"), fields = "Imports")[[1]]
  namespace <- readLines(source_file("NAMESPACE"), warn = FALSE)
  runtime_source <- paste(
    readLines(source_file("inst", "shiny", "v1.4", "utility_functions.R")),
    collapse = "\n"
  )

  expect_match(description, "later")
  expect_true("importFrom(later,later)" %in% namespace)
  expect_match(runtime_source, "later::later(", fixed = TRUE)
})
