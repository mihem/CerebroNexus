# test-extra_material.R — Tests for extra material module

shiny_root <- system.file("shiny/v1.4", package = "cerebroAppLite")
example_crb <- system.file(
  "extdata/v1.4/example.crb",
  package = "cerebroAppLite"
)

fresh_extra_material_crb <- function(include_plots = FALSE) {
  crb <- Cerebro_v1.3$new()
  crb$addExtraTable(
    "example_table",
    data.frame(cell = c("cell_1", "cell_2"), score = c(1, 2))
  )
  if (include_plots) {
    crb$addExtraPlot(
      "example_plot",
      ggplot2::ggplot(
        data.frame(x = c(1, 2), y = c(2, 1)),
        ggplot2::aes(x = x, y = y)
      ) +
        ggplot2::geom_point()
    )
  }

  path <- tempfile(fileext = ".crb")
  saveRDS(crb, path)
  readRDS(path)
}

test_that("extra_material module files parse without errors", {
  mod_files <- c("UI.R", "server.R", "content.R", "select_content.R")
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "extra_material", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("extra_material UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "extra_material", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"extra_material"', perl = TRUE)
})

test_that("example.crb extra material returns valid content", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  categories <- crb$getExtraMaterialCategories()
  expect_true(is.character(categories))
  expect_true("tables" %in% categories)
})

test_that("extra material tables are accessible", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  tables <- crb$getNamesOfExtraTables()
  expect_true(is.character(tables))
  expect_true(length(tables) > 0)
})

test_that("checkForExtraTables returns TRUE for example.crb", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  expect_true(crb$checkForExtraTables())
})

test_that("getExtraTable returns a data.frame", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  tables <- crb$getNamesOfExtraTables()
  skip_if(length(tables) == 0, "No tables in example.crb")
  tbl <- crb$getExtraTable(tables[1])
  expect_s3_class(tbl, "data.frame")
})

test_that("fresh serialized crb reports when no extra plots are present", {
  crb <- fresh_extra_material_crb()
  expect_false(crb$checkForExtraPlots())
})

test_that("fresh serialized crb lists extra plots when present", {
  crb <- fresh_extra_material_crb(include_plots = TRUE)
  expect_true(crb$checkForExtraPlots())
  expect_equal(crb$getNamesOfExtraPlots(), "example_plot")
})

test_that("fresh serialized crb returns stored extra plots", {
  crb <- fresh_extra_material_crb(include_plots = TRUE)
  expect_s3_class(crb$getExtraPlot("example_plot"), "ggplot")
  expect_null(crb$getExtraPlot("nonexistent"))
})
