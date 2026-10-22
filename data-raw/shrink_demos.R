# Cuts the shipped example data sets to the demo gene panel in place, without
# rebuilding them from their (large) sources: cells, embeddings, QC and every
# analysis table are kept, only the expression rows (and the spatial expression)
# are subset by demo_gene_panel(). Every file keeps at most 100 genes; a file
# that already has no more is left untouched, so the script can be run again.
# Run from the package root:
#   Rscript data-raw/shrink_demos.R
devtools::load_all(".", quiet = TRUE)
source("data-raw/demo_gene_panel.R")

cap <- 100L

for (file in list.files(
  "inst/extdata/examples",
  "[.]crb$",
  full.names = TRUE
)) {
  object <- readCerebro(file)
  before <- nrow(object$expression)
  if (before <= cap) {
    message(basename(file), ": ", before, " genes, unchanged")
    next
  }
  save_demo_crb(demo_gene_panel(object, cap), file)
  message(
    basename(file),
    ": ",
    before,
    " -> ",
    nrow(readCerebro(file)$expression),
    " genes, ",
    file.size(file),
    " bytes"
  )
}
