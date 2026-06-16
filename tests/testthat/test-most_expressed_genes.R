# test-most_expressed_genes.R — Tests for most expressed genes module

shiny_root <- system.file("shiny/v1.4", package = "cerebroAppLite")
example_crb <- system.file(
  "extdata/v1.4/example.crb",
  package = "cerebroAppLite"
)

test_that("most_expressed_genes module files parse without errors", {
  mod_files <- c("UI.R", "server.R", "table.R", "select_group.R")
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "most_expressed_genes", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("most_expressed_genes UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "most_expressed_genes", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"mostExpressedGenes"', perl = TRUE)
})

test_that("example.crb most expressed genes class methods work", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  groups <- crb$getGroupsWithMostExpressedGenes()
  expect_true(is.character(groups))
  expect_true(length(groups) > 0)
  result <- crb$getMostExpressedGenes(groups[1])
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("gene" %in% colnames(result))
})

test_that("utility wrappers in inst/shiny/v1.4 parse without error", {
  util_file <- file.path(shiny_root, "utility_functions.R")
  skip_if_not(file.exists(util_file))
  expect_no_error(parse(file = util_file))
})
