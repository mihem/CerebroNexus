skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")

test_that("a flat data.frame in the slot is refused, with the fix in the text", {
  ## `is.list(a_data_frame)` is TRUE and `length()` is its column count, so the
  ## old guard waved this through and `names()` became the column names.
  object <- make_ir_seurat()
  object@misc$immune_repertoire <- do.call(rbind, make_ir_repertoire(object))

  expect_error(export_ir(object), regexp = "named list")
  expect_error(export_ir(object), regexp = "list\\(")
})

test_that("an unnamed list is refused: sample names are the app's labels", {
  object <- make_ir_seurat()
  object@misc$immune_repertoire <- unname(make_ir_repertoire(object))

  expect_error(export_ir(object), regexp = "name")
})

test_that("an element that is not a data.frame is named in the error", {
  object <- make_ir_seurat()
  repertoire <- make_ir_repertoire(object)
  repertoire$donorB <- "not a data frame"
  object@misc$immune_repertoire <- repertoire

  err <- tryCatch(
    export_ir(object),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("donorB", err, fixed = TRUE))
  expect_true(grepl("character", err, fixed = TRUE))
})

test_that("a missing barcode column is refused: it is the only join to a cell", {
  object <- make_ir_seurat()
  repertoire <- lapply(make_ir_repertoire(object), function(df) {
    df$barcode <- NULL
    df
  })
  object@misc$immune_repertoire <- repertoire

  expect_error(export_ir(object), regexp = "barcode")
})

test_that("missing clonotype columns stop before the app receives them", {
  object <- make_ir_seurat()
  for (column in c("CTgene", "CTnt", "CTaa", "CTstrict")) {
    repertoire <- lapply(make_ir_repertoire(object), function(df) {
      df[[column]] <- NULL
      df
    })
    object@misc$immune_repertoire <- repertoire
    expect_error(
      export_ir(object),
      regexp = column
    )
  }
})

test_that("barcodes that match no cell stop the export", {
  ## `combineTCR(samples = )` prefixes barcodes. If the cell names were not
  ## renamed to match, every receptor is orphaned -- that is not a degraded
  ## page but no page, and it breaks the contract the function documents.
  object <- make_ir_seurat()
  repertoire <- lapply(make_ir_repertoire(object), function(df) {
    df$barcode <- paste0("donorA_", df$barcode)
    df
  })
  object@misc$immune_repertoire <- repertoire

  err <- tryCatch(
    export_ir(object),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("shares no barcode", err, fixed = TRUE))
  expect_true(grepl("RenameCells", err, fixed = TRUE))
  ## and it shows both sides so the mismatch is visible
  expect_true(grepl("donorA_cell", err, fixed = TRUE))
})

test_that("a sample matching no cell is removed from the exported repertoire", {
  ## Cells being filtered after the repertoire was combined is ordinary, so a
  ## partial match must not fail. A whole sample matching nothing while its
  ## neighbours match is a naming problem worth saying out loud.
  object <- make_ir_seurat()
  repertoire <- make_ir_repertoire(object)
  repertoire$donorB$barcode <-
    paste0("mismatched_", repertoire$donorB$barcode)
  object@misc$immune_repertoire <- repertoire

  expect_warning(
    crb_path <- export_ir(object),
    regexp = "match no cell|removed",
    ignore.case = TRUE
  )
  exported <- readRDS(crb_path)$getImmuneRepertoire()
  expect_identical(names(exported), "donorA")
  expect_true(all(exported[["donorA"]]$barcode %in% colnames(object)))
})

test_that("required columns are one-dimensional atomic row vectors", {
  object <- make_ir_seurat()
  repertoire <- make_ir_repertoire(object)

  matrix_barcode <- repertoire
  matrix_barcode[["donorA"]]$barcode <- I(matrix(
    c(
      matrix_barcode[["donorA"]]$barcode,
      paste0("extra_", matrix_barcode[["donorA"]]$barcode)
    ),
    nrow = nrow(matrix_barcode[["donorA"]])
  ))
  object@misc$immune_repertoire <- matrix_barcode
  expect_error(
    export_ir(object),
    regexp = "barcode.*one-dimensional|one-dimensional.*barcode",
    ignore.case = TRUE
  )

  list_clone <- repertoire
  list_clone[["donorA"]]$CTgene <- I(as.list(
    list_clone[["donorA"]]$CTgene
  ))
  object@misc$immune_repertoire <- list_clone
  expect_error(
    export_ir(object),
    regexp = "CTgene.*atomic|atomic.*CTgene",
    ignore.case = TRUE
  )
})

test_that("a duplicated sample name is refused: the second entry is unreachable", {
  ## `x[["donorA"]]` returns the first match only, so concatenating a TCR list
  ## and a BCR list describing the same samples silently drops one of them.
  object <- make_ir_seurat()
  repertoire <- make_ir_repertoire(object)
  object@misc$immune_repertoire <- c(repertoire, repertoire)

  err <- tryCatch(
    export_ir(object),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("more than one entry named", err, fixed = TRUE))
  expect_true(grepl("row-bind", err, fixed = TRUE))
})

test_that("barcodes are unique within and across samples", {
  object <- make_ir_seurat()
  within_sample <- make_ir_repertoire(object)
  within_sample[["donorA"]] <- rbind(
    within_sample[["donorA"]],
    within_sample[["donorA"]][1, , drop = FALSE]
  )
  object@misc$immune_repertoire <- within_sample
  expect_error(
    export_ir(object),
    regexp = "more than one row for barcode"
  )

  across_samples <- make_ir_repertoire(object)
  across_samples[["donorB"]]$barcode[1] <-
    across_samples[["donorA"]]$barcode[1]
  object@misc$immune_repertoire <- across_samples
  expect_error(
    export_ir(object),
    regexp = "more than one sample"
  )
})

test_that("the shape the app consumes exports without complaint", {
  object <- make_ir_seurat()
  object@misc$immune_repertoire <- make_ir_repertoire(object)

  expect_no_error(expect_no_warning(export_ir(object)))
})

test_that("a valid unified repertoire outranks stale legacy slots", {
  object <- make_ir_seurat()
  unified <- make_ir_repertoire(object)
  object@misc$immune_repertoire <- unified
  object@misc$tcr_data <- list(s1 = data.frame(not_barcode = "stale"))
  object@misc$bcr_data <- "stale"

  crb_path <- export_ir(object)
  loaded <- readRDS(crb_path)$getImmuneRepertoire()

  expect_identical(loaded, unified)
})

## ---------------------------------------------------------------------------
## The legacy slots
## ---------------------------------------------------------------------------

test_that("the legacy tcr_data slot is held to the same shape", {
  ## `getImmuneRepertoire()` falls back to merging `bcr_data` and `tcr_data`,
  ## so a legacy slot that skipped validation would be a way around it.
  object <- make_ir_seurat()
  object@misc$tcr_data <- lapply(make_ir_repertoire(object), function(df) {
    df$barcode <- NULL
    df
  })

  expect_error(export_ir(object), regexp = "barcode")
})

test_that("the legacy bcr_data slot is held to the same shape", {
  object <- make_ir_seurat()
  object@misc$bcr_data <- lapply(make_ir_repertoire(object), function(df) {
    df$barcode <- NULL
    df
  })

  expect_error(export_ir(object), regexp = "barcode")
})

test_that("legacy BCR and TCR are unified per sample at export", {
  ## R6 methods are serialized into a .crb, so changing the class getter later
  ## does not repair an object already written. The export boundary has to put
  ## new files into the one-entry-per-sample shape that their serialized getter
  ## already prefers.
  object <- make_ir_seurat()
  combined <- make_ir_repertoire(object)
  tcr <- lapply(combined, function(df) {
    df[seq_len(floor(nrow(df) / 2)), , drop = FALSE]
  })
  bcr <- lapply(combined, function(df) {
    df <- df[seq.int(floor(nrow(df) / 2) + 1L, nrow(df)), , drop = FALSE]
    df$CTgene <- "IGHV1.IGHJ4.IGHM_IGKV3.IGKJ1.IGKC"
    df
  })
  object@misc$tcr_data <- tcr
  object@misc$bcr_data <- bcr

  crb_path <- export_ir(object)
  loaded <- readRDS(crb_path)
  repertoire <- loaded$getImmuneRepertoire()

  expect_setequal(names(repertoire), names(tcr))
  expect_false(anyDuplicated(names(repertoire)) > 0)
  expect_true(any(grepl("^TRAV", repertoire[["donorA"]]$CTgene)))
  expect_true(any(grepl("^IGHV", repertoire[["donorA"]]$CTgene)))
  ## Keep the legacy fields for callers of getTCR()/getBCR().
  expect_equal(loaded$getTCR(), tcr)
  expect_equal(loaded$getBCR(), bcr)
})

test_that("merged legacy slots reject one barcode assigned to two samples", {
  object <- make_ir_seurat()
  row <- make_ir_repertoire(object)[["donorA"]][1, , drop = FALSE]
  tcr <- list(donorA = row)
  bcr_row <- row
  bcr_row$CTgene <- "IGHV1.IGHJ4.IGHM_IGKV3.IGKJ1.IGKC"
  bcr <- list(donorB = bcr_row)
  object@misc$tcr_data <- tcr
  object@misc$bcr_data <- bcr

  expect_error(
    export_ir(object),
    regexp = "more than one sample|globally unique",
    ignore.case = TRUE
  )
})

## ---------------------------------------------------------------------------
## No false positives on shipped data
## ---------------------------------------------------------------------------

test_that("the bundled TCR/BCR demo passes the check", {
  crb_path <- system.file(
    "extdata/examples/demo_full_tcr_bcr.crb",
    package = "CerebroNexus"
  )
  if (!nzchar(crb_path)) {
    crb_path <- testthat::test_path(
      "../../inst/extdata/examples/demo_full_tcr_bcr.crb"
    )
  }
  skip_if_not(file.exists(crb_path), "demo_full_tcr_bcr.crb not available")

  repertoire <- readCerebro(crb_path)$getImmuneRepertoire()
  skip_if(length(repertoire) == 0, "demo carries no immune repertoire")

  expect_no_error(expect_no_warning(
    .validateImmuneRepertoire(
      repertoire,
      source_label = "`@misc$immune_repertoire`"
    )
  ))
})
