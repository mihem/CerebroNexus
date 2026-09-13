# test-trekker.R — Trekker single-cell spatial-mapping page.
#
# Covers the parts of the feature that are pure and don't need a browser: the
# Cerebro `trekker` slot round-trip (which also gates whether the tab
# appears), and the two pure helpers that drive the gene picker and the
# meta-field colouring. The pure helpers are sourced from the same inst/ file the
# app sources at runtime (see helper-trekker-helpers.R).

# ---- R6 slot: addTrekker / getTrekker round-trip -------------------------- ##

test_that("getTrekker defaults to NULL so the tab stays hidden for old .crb", {
  obj <- Cerebro$new()
  # An object that predates the feature carries no trekker slot; the tab is
  # inserted only when getTrekker() is non-NULL, so this is what keeps it hidden.
  expect_null(obj$getTrekker())
})

test_that("addTrekker stores the payload and getTrekker returns it verbatim", {
  obj <- Cerebro$new()
  payload <- list(
    barcodes = c("AAA", "CCC"),
    clusters = c(0L, 1L),
    moran = list(list(gene = "Plp1"), list(gene = "Mbp"))
  )
  obj$addTrekker(payload)
  # Non-NULL slot is exactly the condition that makes the Trekker tab appear.
  expect_false(is.null(obj$getTrekker()))
  expect_identical(obj$getTrekker(), payload)
})

test_that("addTrekker rejects a non-list", {
  obj <- Cerebro$new()
  expect_error(obj$addTrekker(42), "must be a list")
  expect_null(obj$getTrekker())
})

# ---- trekker_gene_suggest ------------------------------------------------- ##

test_that("trekker_gene_suggest keeps Moran + marker genes present in the matrix", {
  tk <- list(moran = list(list(gene = "Dgkb"), list(gene = "Ghost_gene")))
  gene_names <- c("Dgkb", "Plp1", "Mbp", "Some_other_gene")

  out <- trekker_gene_suggest(tk, gene_names)

  expect_true("Dgkb" %in% out) # a Moran gene that is measured
  expect_true("Plp1" %in% out) # a canonical marker that is measured
  expect_false("Ghost_gene" %in% out) # Moran gene absent from the matrix -> dropped
  expect_true(all(out %in% gene_names)) # never suggests an unmeasured gene
})

test_that("trekker_gene_suggest tolerates an empty Moran list", {
  tk <- list(moran = list())
  out <- trekker_gene_suggest(tk, c("Plp1", "Gad1", "not_a_marker"))
  expect_setequal(out, c("Plp1", "Gad1"))
})

# ---- trekker_numeric_meta_cols -------------------------------------------- ##

test_that("trekker_numeric_meta_cols keeps only numeric, non-constant columns", {
  meta <- data.frame(
    myelination = c(-0.3, 0.5, 1.8),
    percent_mt = c(1.0, 2.0, 3.0),
    constant = c(2, 2, 2),
    cell_type = c("ExN", "InN", "Oligo"),
    stringsAsFactors = FALSE
  )

  out <- trekker_numeric_meta_cols(meta)

  expect_setequal(out, c("myelination", "percent_mt"))
  expect_false("constant" %in% out) # zero-variance dropped (nothing to colour by)
  expect_false("cell_type" %in% out) # non-numeric dropped
})

test_that("trekker_numeric_meta_cols returns empty for NULL / empty input", {
  expect_identical(trekker_numeric_meta_cols(NULL), character(0))
  expect_identical(trekker_numeric_meta_cols(list()), character(0))
})

test_that("trekker_numeric_meta_cols ignores NA when judging constancy", {
  meta <- data.frame(
    all_na = c(NA_real_, NA_real_, NA_real_),
    one_value = c(5, NA, 5),
    varying = c(1, NA, 9)
  )
  out <- trekker_numeric_meta_cols(meta)
  expect_setequal(out, "varying")
})

test_that("Trekker payload stays lazy until its page is visible", {
  server <- paste(
    readLines(viewer_test_path("trekker", "server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    server,
    'req(identical(input[["sidebar"]], "trekker"))',
    fixed = TRUE
  )
  expect_false(grepl(
    'outputOptions(output, "trekker_main_parameters_ui"',
    server,
    fixed = TRUE
  ))
})
