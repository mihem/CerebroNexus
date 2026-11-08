## Tests for immune receptor (AIRR) data in AnnData / MuData files:
##   obsm["airr"] (awkward array) -> readH5adAsSCE() -> metadata(sce)$immune_repertoire
##   -> convertH5adToCerebro() -> .crb -> getImmuneRepertoire()
##
## small_airr.h5mu / small_airr.h5ad / small_awkward.h5ad were written by the
## real anndata + awkward (see make_fixtures.py, `airr` mode); the tests need
## neither Python nor awkward.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")
skip_if_not_installed("scRepertoire")

fixture <- anndata_test_fixture
read_h5ad <- function(path, ...) {
  suppressWarnings(suppressMessages(readH5adAsSCE(path, ...)))
}
repertoire_of <- function(sce) sce@metadata$immune_repertoire
bind_repertoire <- function(repertoire) {
  all <- do.call(rbind, unname(repertoire))
  rownames(all) <- NULL
  all[order(all$barcode), ]
}
chains_of <- function(name, entry) {
  ctx <- .h5adContext(fixture(name))
  .h5adReadAwkwardChains(ctx, paste0("obsm/", entry))
}

test_that("every awkward layout decodes to the same chains anndata reads", {
  ## the 8 records of small_awkward.h5ad, as make_fixtures.py defines them
  record <- data.frame(
    locus = c("TRA", "TRB", "TRA", "TRB", "IGH", "IGK", "TRA", "TRB"),
    v_call = c("V1", "V2", NA, "V4µ", "V5", "V6", "V7", "V8"),
    count = c(5, NA, 3, 4, 9, 8, 7, 6),
    masked = c(0, NA, 20, 30, 40, 50, NA, 70),
    flag = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  expect_chains <- function(entry, rows, per_row) {
    decoded <- chains_of("small_awkward.h5ad", entry)
    expect_identical(decoded$n, 6L)
    expected <- record[rows, ]
    rownames(expected) <- NULL
    got <- decoded$chains
    expect_identical(got$row, rep(seq_along(per_row), per_row))
    got$row <- NULL
    expect_equal(got, expected, ignore_attr = TRUE)
  }
  ## ListOffsetArray, with empty lists
  expect_chains("offsets", 1:8, c(2, 0, 3, 0, 3, 0))
  ## ListArray: starts/stops out of order
  expect_chains("listarray", c(5, 6, 1, 2, 3, 4, 8), c(2, 2, 0, 2, 1, 0))
  ## IndexedArray content reversed
  expect_chains("indexed", 8:1, c(2, 1, 0, 3, 2, 0))
  ## RegularArray of size 2 over an indexed content
  expect_chains("regular", c(1:8, 1:4), rep(2, 6))
})

test_that("AIRR chains of an h5mu are read as a per-sample repertoire", {
  sce <- read_h5ad(fixture("small_airr.h5mu"))
  ## the expression modality is chosen, not the receptor one
  expect_identical(dim(sce), c(60L, 120L))
  repertoire <- repertoire_of(sce)
  ## keyed by the obs column `sample`
  expect_identical(names(repertoire), c("S1", "S2"))
  expect_identical(vapply(repertoire, nrow, integer(1)), c(S1 = 40L, S2 = 26L))
  all <- bind_repertoire(repertoire)
  expect_true(all(
    c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict") %in% names(all)
  ))
  ## matched by name to the expression cells; ghost0/ghost1 live only in airr
  expect_true(all(all$barcode %in% colnames(sce)))
  expect_false(any(grepl("ghost", all$barcode)))
  expect_identical(anyDuplicated(all$barcode), 0L)
  ## the repertoire passes the validation the exporter applies
  expect_silent(.validateImmuneRepertoire(
    repertoire,
    cell_barcodes = colnames(sce)
  ))
  ## a cell goes to the sample its obs row says
  sample_of <- as.character(SummarizedExperiment::colData(sce)$sample)
  names(sample_of) <- colnames(sce)
  in_s1 <- repertoire$S1$barcode
  expect_true(all(sample_of[in_s1] == "S1"))
  expect_true(all(sample_of[repertoire$S2$barcode] == "S2"))
})

test_that("chains become CTgene / CTnt / CTaa / CTstrict the way scRepertoire writes them", {
  all <- bind_repertoire(repertoire_of(read_h5ad(fixture("small_airr.h5mu"))))
  row <- function(cell) all[all$barcode == cell, , drop = FALSE]

  ## cell000: TRA + TRB (clone 0): V.J.C for alpha, V.D.J.C for beta
  expect_identical(
    row("cell000")$CTgene,
    "TRAV1.TRAJ1.TRAC_TRBV1.TRBD1.TRBJ1.TRBC1"
  )
  expect_identical(row("cell000")$CTaa, "CAVAAAF_CSSAAAF")
  expect_identical(
    row("cell000")$CTnt,
    "TGTGCCGCCGCCGCCGCCTTT_TGTGCCGCCGCCGCCGCCTTT"
  )
  expect_identical(
    row("cell000")$CTstrict,
    paste0(
      "TRAV1.TRAJ1.TRAC;TGTGCCGCCGCCGCCGCCTTT_",
      "TRBV1.TRBD1.TRBJ1.TRBC1;TGTGCCGCCGCCGCCGCCTTT"
    )
  )
  ## a missing D gene is "None", as in Cell Ranger contigs
  expect_match(row("cell001")$CTgene, "TRAV9.TRAJ9.TRAC")
  ## two alpha chains stay in one cell
  expect_identical(
    row("cell001")$CTaa,
    "CAVCCCF;CAVKKKF_CSSCCCF"
  )
  ## junction_aa is the CDR3 (conserved C ... F included)
  expect_true(all(grepl("^(C|NA)", all$CTaa)))

  ## cell007: the alpha chain has no CDR3, so only the beta chain is left
  expect_identical(row("cell007")$CTaa, "NA_CSSIIIF")
  ## cell009: the Multi-locus contig is dropped
  expect_identical(row("cell009")$CTaa, "NA_CSSCCCF")
  ## cell003: its only chain is not productive; cell005: no chains at all
  expect_equal(nrow(row("cell003")), 0L)
  expect_equal(nrow(row("cell005")), 0L)
  ## cell082 (monocyte): only a non-productive chain
  expect_equal(nrow(row("cell082")), 0L)

  ## B cells: heavy + light chain
  expect_identical(row("cell040")$IGH, "IGHV1.IGHD3.IGHJ4.IGHM")
  expect_match(row("cell040")$CTaa, "^CARAAAAF_")
  ## a cell with 2 TCR and 1 BCR chain is a T cell; with one chain of each, the
  ## type with more UMIs wins
  expect_match(row("cell044")$CTgene, "^TRAV3")
  expect_match(row("cell046")$CTgene, "^IGHV1")
})

test_that("a plain h5ad with obsm['airr'] gives one sample and no reduction", {
  sce <- read_h5ad(fixture("small_airr.h5ad"))
  repertoire <- repertoire_of(sce)
  expect_identical(names(repertoire), "Sample_1")
  expect_identical(nrow(repertoire$Sample_1), 66L)
  ## the awkward array is not mistaken for an embedding
  expect_false("airr" %in% SingleCellExperiment::reducedDimNames(sce))
  ## 19 distinct clones by CDR3 pair among the 66 cells
  expect_identical(length(unique(repertoire$Sample_1$CTaa)), 19L)
  ## same cells, same calls as the MuData version of the data
  from_mudata <- bind_repertoire(repertoire_of(read_h5ad(fixture(
    "small_airr.h5mu"
  ))))
  from_h5ad <- bind_repertoire(repertoire)
  expect_identical(from_h5ad$barcode, from_mudata$barcode)
  expect_identical(from_h5ad$CTstrict, from_mudata$CTstrict)
})

test_that("the airr and airr_sample options are honoured", {
  ## skip receptor data
  expect_null(repertoire_of(read_h5ad(
    fixture("small_airr.h5mu"),
    airr = FALSE
  )))
  expect_null(repertoire_of(read_h5ad(fixture("small_airr.h5ad"), airr = NULL)))
  ## another sample column
  by_donor <- repertoire_of(
    read_h5ad(fixture("small_airr.h5mu"), airr_sample = "donor")
  )
  expect_identical(names(by_donor), c("d0", "d1", "d2"))
  expect_identical(sum(vapply(by_donor, nrow, integer(1))), 66L)
  expect_error(
    read_h5ad(fixture("small_airr.h5mu"), airr_sample = "nope"),
    "airr_sample"
  )
  ## naming the source explicitly
  expect_identical(
    names(repertoire_of(read_h5ad(fixture("small_airr.h5mu"), airr = "airr"))),
    c("S1", "S2")
  )
  expect_identical(
    names(repertoire_of(read_h5ad(fixture("small_airr.h5ad"), airr = "airr"))),
    "Sample_1"
  )
  expect_error(
    read_h5ad(fixture("small_airr.h5mu"), airr = "nope"),
    "not found"
  )
  expect_error(
    read_h5ad(fixture("small_csr.h5ad"), airr = "airr"),
    "awkward-array"
  )
  expect_error(read_h5ad(fixture("small_airr.h5mu"), airr = 1), "airr")
})

test_that("files without receptor data are unaffected", {
  for (name in c("small_csr.h5ad", "small_csc.h5ad", "small_dense.h5ad")) {
    sce <- read_h5ad(fixture(name))
    expect_null(repertoire_of(sce))
  }
  ## a MuData file with no airr modality still picks its expression modality
  sce <- read_h5ad(fixture("small_mudata.h5mu"))
  expect_null(repertoire_of(sce))
  expect_identical(dim(sce), c(60L, 120L))
  ## naming the receptor modality as the one to read as expression still works
  expect_identical(
    dim(read_h5ad(fixture("small_airr.h5mu"), modality = "gex", airr = FALSE)),
    c(60L, 120L)
  )
})

test_that("convertH5adToCerebro carries the repertoire into the .crb", {
  skip_if_not_installed("qs2")
  out <- withr::local_tempdir()
  crb <- suppressWarnings(suppressMessages(
    convertH5adToCerebro(
      fixture("small_airr.h5mu"),
      out,
      groups = c("cell_type", "donor"),
      codec = "rds",
      verbose = FALSE
    )
  ))
  object <- readRDS(crb)
  stored <- object$getImmuneRepertoire()
  expect_identical(names(stored), c("S1", "S2"))
  source <- repertoire_of(read_h5ad(fixture("small_airr.h5mu")))
  for (sample in names(source)) {
    expect_identical(stored[[sample]]$barcode, source[[sample]]$barcode)
    expect_identical(stored[[sample]]$CTaa, source[[sample]]$CTaa)
    expect_identical(stored[[sample]]$CTstrict, source[[sample]]$CTstrict)
  }

  ## and a conversion with airr = FALSE has none
  out2 <- withr::local_tempdir()
  crb2 <- suppressWarnings(suppressMessages(
    convertH5adToCerebro(
      fixture("small_airr.h5mu"),
      out2,
      groups = "cell_type",
      airr = FALSE,
      codec = "rds",
      verbose = FALSE
    )
  ))
  expect_length(readRDS(crb2)$getImmuneRepertoire(), 0L)
})

## ---------------------------------------------------------------------------
## writing receptors and pseudotime back
## ---------------------------------------------------------------------------

test_that("exportToH5ad writes the immune repertoire as scirpy's obsm['airr'] and it reads back the same", {
  skip_if_not_installed("scRepertoire")
  first <- readCerebro(suppressWarnings(suppressMessages(convertH5adToCerebro(
    fixture("small_airr.h5ad"),
    withr::local_tempdir(),
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  ))))
  out <- file.path(withr::local_tempdir(), "airr_back.h5ad")
  suppressMessages(exportToH5ad(first, out))
  expect_true(
    rhdf5::h5read(out, "obsm/airr", read.attributes = TRUE) |>
      attr("encoding-type") |>
      identical("awkward-array")
  )

  again <- readCerebro(suppressWarnings(suppressMessages(convertH5adToCerebro(
    out,
    withr::local_tempdir(),
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  ))))
  a <- first$getImmuneRepertoire()
  b <- again$getImmuneRepertoire()
  a <- do.call(rbind, a)
  b <- do.call(rbind, b)
  a <- a[order(a$barcode), ]
  b <- b[order(b$barcode), ]
  expect_identical(b$barcode, a$barcode)
  expect_identical(b$CTaa, a$CTaa)
  expect_identical(b$CTgene, a$CTgene)
})

test_that("a Cerebro object without a repertoire writes no obsm['airr']", {
  crb <- suppressWarnings(suppressMessages(convertH5adToCerebro(
    fixture("small_csr.h5ad"),
    withr::local_tempdir(),
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  )))
  out <- file.path(withr::local_tempdir(), "none.h5ad")
  suppressMessages(exportToH5ad(crb, out))
  expect_false("airr" %in% rhdf5::h5ls(out)$name)
})
