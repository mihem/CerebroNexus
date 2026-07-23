# test-enriched_pathways.R — Tests for enriched pathways module

shiny_root <- system.file("shiny/v1.4", package = "CerebroNexus")
example_crb <- system.file(
  "extdata/v1.4/example.crb",
  package = "CerebroNexus"
)

test_that("enriched_pathways module files parse without errors", {
  mod_files <- c("UI.R", "server.R", "table.R", "select_content.R")
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "enriched_pathways", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("enriched_pathways UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "enriched_pathways", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"enrichedPathways"', perl = TRUE)
})

test_that("example.crb enriched_pathways class methods work", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  methods <- crb$getMethodsForEnrichedPathways()
  expect_true(is.character(methods))
  expect_true(length(methods) >= 2)
})

test_that("enriched_pathways seurat_enrichr returns real data", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  gse_groups <- crb$getGroupsWithEnrichedPathways("cerebro_seurat_enrichr")
  expect_true(is.character(gse_groups))
  expect_true(length(gse_groups) > 0)
  result <- crb$getEnrichedPathways("cerebro_seurat_enrichr", "seurat_clusters")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("getMethodsWithEnrichedPathways class method exists", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  has_with <- is.function(crb$getMethodsWithEnrichedPathways)
  has_for <- is.function(crb$getMethodsForEnrichedPathways)
  expect_true(has_with || has_for)
})
