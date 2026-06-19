# test-immune_repertoire.R — Tests for immune repertoire module

shiny_root <- system.file("shiny/v1.4", package = "cerebroAppLite")
example_crb <- system.file(
  "extdata/v1.4/example.crb",
  package = "cerebroAppLite"
)
tcr_crb <- system.file(
  "extdata/v1.4/example_tcr.crb",
  package = "cerebroAppLite"
)

test_that("immune_repertoire module files parse without errors", {
  mod_files <- c(
    "UI.R",
    "server.R",
    "data.R",
    "settings.R",
    "tabs.R",
    "help.R",
    "visualizations.R"
  )
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "immune_repertoire", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("immune_repertoire UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "immune_repertoire", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"immune_repertoire"', perl = TRUE)
})

test_that("example.crb immune_repertoire slot is accessible", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  expect_true(is.null(crb$immune_repertoire) || is.list(crb$immune_repertoire))
})

test_that("example_tcr.crb loads and contains immune repertoire data", {
  skip_if_not(file.exists(tcr_crb))
  crb <- readRDS(tcr_crb)
  ir <- crb$getImmuneRepertoire()
  expect_true(is.list(ir))
  expect_true(length(ir) > 0)
  for (nm in names(ir)) {
    df <- ir[[nm]]
    expect_s3_class(df, "data.frame")
    expect_true(all(c("barcode", "CTgene", "chain") %in% colnames(df)))
    expect_true(nrow(df) > 0)
  }
})

test_that("ir_bindCache injects dataset identity into cache key", {
  # data_to_load$path in every cache key prevents stale plots when switching
  # datasets; cache = "session" prevents cross-user/session cache leakage.
  # Static source check — guards a production bug that behaviour tests can't
  # reach (needs a second dataset, which the single-dataset fixtures lack).
  srv <- file.path(shiny_root, "immune_repertoire", "server.R")
  skip_if_not(file.exists(srv))
  content <- paste(readLines(srv), collapse = "\n")
  expect_match(content, "data_to_load\\$path")
  expect_match(content, 'cache\\s*=\\s*"session"')
})

test_that("example_tcr.crb preserves original data fields", {
  skip_if_not(file.exists(tcr_crb))
  crb <- readRDS(tcr_crb)
  expect_true(!is.null(crb$getMetaData()))
  expect_true(nrow(crb$getMetaData()) == 501)
  expect_true(!is.null(crb$experiment))
})
