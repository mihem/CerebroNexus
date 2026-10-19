#' @title
#' Read GMT file.
#'
#' @description
#' This functions reads a (tab-delimited) GMT file which contains the gene set
#' name in the first column, the gene set description in the second column, and
#' the gene names in the following columns.
#'
#' @param file Path to GMT file.
#'
#' @return
#' Returns an object in the same format as from the GSA.read.gmt function (GSA
#' package) with gene sets, gene set names, and gene set descriptions stored in
#' lists.
#'
#' @import dplyr
#'
.read_GMT_file <- function(
  file
) {
  ## read provided file
  ## The original parser used ';' as the delimiter — a character GMT files do
  ## not contain — so every row collapsed into a single column `X1` holding the
  ## whole line. readLines gives the same one-line-per-element shape. Drop blank
  ## lines first: a GMT file that ends in a trailing newline (as the bundled
  ## example_gene_set.gmt does) would otherwise produce a phantom final row with
  ## NA name/description/genes that pollutes downstream enrichment. readr's
  ## read_delim (the parser this replaced) skipped empty rows — match that.
  lines <- readLines(file)
  fields <- strsplit(lines[nzchar(trimws(lines))], split = '\t')

  ## gene set name, description, and genes in the following columns
  list(
    genesets = lapply(fields, function(x) x[3:length(x)]),
    geneset.names = vapply(fields, `[`, character(1), 1),
    geneset.description = vapply(fields, `[`, character(1), 2)
  )
}
