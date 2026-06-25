# test-immune_repertoire.R — Tests for immune repertoire module
#
# The example dataset (example.crb) carries real 10x immune repertoire data
# (sc5p_v2_hs_PBMC_10k, 5' GEX + TCR + BCR from the same experiment). The
# single donor is partitioned into three demo samples; sample labels do not
# represent distinct biological donors.

shiny_root <- system.file("shiny/v1.4", package = "cerebroAppLite")
example_crb <- system.file(
  "extdata/v1.4/example.crb",
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

test_that("example.crb contains real immune repertoire data", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  expect_true(is.list(ir))
  expect_true(length(ir) > 0)
  for (nm in names(ir)) {
    df <- ir[[nm]]
    expect_s3_class(df, "data.frame")
    expect_true(all(c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict") %in%
      colnames(df)))
    expect_true(nrow(df) > 0)
  }
})

test_that("example.crb IR barcodes align with cell metadata", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  md <- crb$getMetaData()
  ir_bc <- unlist(lapply(ir, function(df) df$barcode), use.names = FALSE)
  overlap <- length(intersect(ir_bc, md$cell_barcode))
  expect_true(overlap > 0)
  # every IR barcode should correspond to a real cell in the dataset
  expect_equal(overlap, length(unique(ir_bc)))
})

test_that("IR grouping variables are recoverable from cell metadata by barcode", {
  # The IR data.frames carry only standard scRepertoire columns. The module
  # joins ANY grouping variable (getGroups(): sample, condition, cell type, ...)
  # onto the IR rows by barcode at runtime. This verifies that join is possible
  # for the example data set, so the "Sample column" / "Group by" dropdowns are
  # populated regardless of which columns a producer embedded in the IR table.
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  md <- crb$getMetaData()
  groups <- crb$getGroups()
  expect_true(length(groups) >= 1)

  # replicate the module's join: map each IR barcode to a metadata row
  ir_bc <- unlist(lapply(ir, function(df) df$barcode), use.names = FALSE)
  idx <- match(ir_bc, md$cell_barcode)
  expect_true(all(!is.na(idx)))

  # at least one grouping variable yields >= 2 levels over the IR cells
  multilevel <- vapply(groups, function(g) {
    length(unique(md[[g]][idx])) >= 2
  }, logical(1))
  expect_true(any(multilevel))
})

test_that("example.crb IR contains both TCR and BCR clonotypes", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  all_ct <- paste(unlist(lapply(ir, function(df) df$CTgene)), collapse = ";")
  expect_true(grepl("TR[AB]", all_ct)) # TCR present
  expect_true(grepl("IG[HKL]", all_ct)) # BCR present
})

test_that("example.crb IR has TCR chains detectable from CTgene", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  all_ct <- paste(unlist(lapply(ir, function(df) df$CTgene)), collapse = ";")
  has_tcr <- grepl("TRA", all_ct) || grepl("TRB", all_ct)
  expect_true(has_tcr)
})

test_that("data.R joins metadata so grouping is not limited to IR columns", {
  # Guards the generic fix: grouping options must come from the data set's
  # metadata (getGroups + barcode join), not only from columns embedded in the
  # IR table. A regression to "shared IR columns only" would silently break
  # grouping for users whose IR tables carry just the standard scRepertoire
  # columns.
  data_file <- file.path(shiny_root, "immune_repertoire", "data.R")
  skip_if_not(file.exists(data_file))
  content <- paste(readLines(data_file), collapse = "\n")
  expect_match(content, "ir_data_annotated")
  expect_match(content, "cell_barcode")
  expect_match(content, "getGroups\\(\\)")
})

test_that("clonalScatter render guards against invalid sample selection", {
  # clonalScatter compares two named list elements via x.axis/y.axis and must
  # not also receive group.by (scRepertoire errors: "undefined columns
  # selected"). The render must also validate >= 2 distinct samples to avoid
  # "attempt to select less than one element" on a single-element list.
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")
  # locate the main clonalScatter render block
  block <- sub(
    ".*output\\$ir_plot_clonalScatter.*?(renderPlot.*?ir_bindCache).*",
    "\\1",
    content,
    perl = TRUE
  )
  # the main scatter render must guard sample count and distinctness
  expect_match(content, "Clonal scatter needs at least 2 samples")
  expect_match(content, "Select two different samples")
})

test_that("safeRenderPlot lets validate/req conditions pass through", {
  # validate()/need()/req() raise a "shiny.silent.error" which is also an
  # `error`. safeRenderPlot must NOT turn it into an error plot, otherwise
  # first-paint NULL inputs render "[IR ERROR] ..." instead of a grey
  # placeholder. The fix re-raises shiny.silent.error from within the error
  # handler (a sibling shiny.silent.error handler would re-catch it).
  srv <- file.path(shiny_root, "immune_repertoire", "server.R")
  skip_if_not(file.exists(srv))
  content <- paste(readLines(srv), collapse = "\n")
  expect_match(content, "shiny.silent.error")
  expect_match(content, "inherits\\(e, \"shiny.silent.error\"\\)")

  # functional check: silent conditions propagate, real errors are caught
  safeRenderPlot <- function(expr, plot_name = "unknown") {
    tryCatch(
      {
        expr
      },
      error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        "ERROR_PLOT"
      }
    )
  }
  silent <- tryCatch(
    safeRenderPlot(shiny::validate(shiny::need(FALSE, "x"))),
    shiny.silent.error = function(e) "PASSED",
    error = function(e) "CAUGHT"
  )
  expect_equal(silent, "PASSED")
  expect_equal(safeRenderPlot(stop("boom")), "ERROR_PLOT")
})

test_that("ir_bindCache injects dataset identity into cache key", {
  # data_to_load$path in every cache key prevents stale plots when switching
  # datasets; cache = "session" prevents cross-user/session cache leakage.
  srv <- file.path(shiny_root, "immune_repertoire", "server.R")
  skip_if_not(file.exists(srv))
  content <- paste(readLines(srv), collapse = "\n")
  expect_match(content, "data_to_load\\$path")
  expect_match(content, 'cache\\s*=\\s*"session"')
})

test_that("example.crb preserves core data fields", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  expect_true(!is.null(crb$getMetaData()))
  expect_true(nrow(crb$getMetaData()) > 0)
  expect_true(!is.null(crb$experiment))
})
