## While another R process encodes the repertoire, the Clonal projection is
## drawn from the Viewer Pack's receptor index (ir_clonal_umap_packed(),
## immune_repertoire/data.R). Both must choose the same cells with the same
## clone sizes as the encoding does (ir_clonal_frame_cells(), encode.R).

ir_fallback_repertoire <- function() {
  tcr <- function(a, b) {
    paste0("TRAV", a, ".TRAJ1.TRAC_TRBV", b, ".TRBJ1.TRBC1")
  }
  bcr <- function(h) paste0("IGHV", h, ".IGHJ1.IGHM_IGKV1.IGKJ1.IGKC")
  tcr_genes <- c(
    tcr(1, 1),
    tcr(1, 1),
    tcr(1, 1),
    tcr(2, 2),
    tcr(2, 2),
    tcr(3, 3),
    tcr(1, 1),
    NA,
    "",
    tcr(4, 4),
    tcr(5, 5),
    tcr(1, 1)
  )
  tcr_frame <- data.frame(
    ## c03 twice (a cell's first row counts), and a barcode that is no cell
    barcode = c(
      "c01",
      "c02",
      "c03",
      "c03",
      "c04",
      "c05",
      "c06",
      "c07",
      "c08",
      "c09",
      "unknown",
      "c10"
    ),
    CTgene = tcr_genes,
    CTstrict = tcr_genes,
    CTaa = paste0("CA", seq_along(tcr_genes)),
    CTnt = paste0("TG", seq_along(tcr_genes)),
    stringsAsFactors = FALSE
  )
  bcr_genes <- c(bcr(1), bcr(1), bcr(2), bcr(1), NA, bcr(3), bcr(2))
  bcr_frame <- data.frame(
    barcode = c("c11", "c12", "c12", "c13", "c14", "c15", "c16"),
    CTgene = bcr_genes,
    CTstrict = bcr_genes,
    CTaa = paste0("CB", seq_along(bcr_genes)),
    CTnt = paste0("TB", seq_along(bcr_genes)),
    stringsAsFactors = FALSE
  )
  list(T1 = tcr_frame, B1 = bcr_frame)
}

test_that("the Pack's receptor index draws the Clonal map as the encoding does", {
  scope <- ir_scope(
    names = "ir_clonal_umap_packed",
    sources = "clone_contract.R"
  )
  repertoire <- ir_fallback_repertoire()
  cells <- sprintf("c%02d", 1:20)
  encoded <- scope$ir_encode_repertoire(repertoire, cells)
  scope$viewerPackCurrent <- function() NULL
  scope$viewerPackImmuneIndex <- function(pack, receptor) {
    CerebroNexus:::.viewerPackImmuneIndex(repertoire, cells, receptor)
  }
  bins <- scope$CEREBRO_CLONE_BINS
  for (receptor in c("TCR", "BCR")) {
    packed <- scope$ir_clonal_umap_packed(receptor, "CTgene", "encoding")
    for (percentage in c(100, 50)) {
      for (show_all in c(TRUE, FALSE)) {
        label <- paste(receptor, percentage, show_all)
        from_encoding <- scope$ir_clonal_frame_cells(
          encoded,
          receptor,
          "CTgene",
          length(cells),
          bins,
          show_all = show_all,
          percentage = percentage
        )
        from_pack <- scope$ir_clonal_frame_cells(
          packed,
          receptor,
          "CTgene",
          length(cells),
          bins,
          show_all = show_all,
          percentage = percentage
        )
        expect_false(is.null(from_encoding), label = label)
        expect_identical(from_pack, from_encoding, label = label)
      }
    }
  }
  ## another clone column waits for the encoding
  expect_error(
    scope$ir_clonal_umap_packed("TCR", "CTaa", "encoding"),
    "encoding"
  )
})
