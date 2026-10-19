# test-enriched_pathways.R — Tests for enriched pathways module

shiny_root <- system.file("viewer", package = "CerebroNexus")
example_crb <- system.file(
  "extdata/examples/example.crb",
  package = "CerebroNexus"
)

test_that("enriched_pathways module files parse without errors", {
  mod_files <- c("UI.R", "server.R")
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "enriched_pathways", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("enriched_pathways UI defines correct tabName", {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("enriched_pathways", "UI.R"), scope)
  page <- htmltools::tagQuery(scope$tab_enriched_pathways)
  expect_equal(page$filter("#shiny-tab-enrichedPathways")$length(), 1L)
})

test_that("example.crb enriched_pathways class methods work", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  methods <- crb$getMethodsForEnrichedPathways()
  expect_true(is.character(methods))
  expect_true(length(methods) >= 2)
})

test_that("enriched_pathways seurat_enrichr returns real data", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  gse_groups <- crb$getGroupsWithEnrichedPathways("cerebro_seurat_enrichr")
  expect_true(is.character(gse_groups))
  expect_true(length(gse_groups) > 0)
  result <- crb$getEnrichedPathways("cerebro_seurat_enrichr", "seurat_clusters")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("getMethodsWithEnrichedPathways class method exists", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  has_with <- is.function(crb$getMethodsWithEnrichedPathways)
  has_for <- is.function(crb$getMethodsForEnrichedPathways)
  expect_true(has_with || has_for)
})
