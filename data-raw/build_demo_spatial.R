# Builds inst/extdata/examples/demo_spatial.crb, the small spatial example that
# ships with the package (tests, the App bundle), from real data: a random 500
# of the 2,000 cells of demo_spatial_xenium.crb, the 10x Genomics Xenium mouse
# brain section (Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP, see
# data-raw/DATASETS.md). Only the `fov` section is kept; the rotated `fov_colour`
# copy is dropped. Cells are subset with the same code as
# data-raw/subsample_demos.R, and saving applies the 100-gene demo panel.
#
# From the package root:
#   Rscript data-raw/build_demo_spatial.R
devtools::load_all(".", quiet = TRUE)
source("data-raw/demo_gene_panel.R")

## The subsetting helpers of subsample_demos.R, without running its loop over
## the shipped files.
local({
  exprs <- parse("data-raw/subsample_demos.R", keep.source = FALSE)
  for (expr in exprs) {
    is_function <- is.call(expr) &&
      identical(expr[[1L]], as.name("<-")) &&
      is.call(expr[[3L]]) &&
      identical(expr[[3L]][[1L]], as.name("function"))
    if (is_function) {
      eval(expr, envir = globalenv())
    }
  }
})

source_file <- "inst/extdata/examples/demo_spatial_xenium.crb"
output <- "inst/extdata/examples/demo_spatial.crb"
n_keep <- 500L
seed <- 20261110L

object <- readCerebro(source_file)
stopifnot("fov" %in% names(object$spatial))
object$spatial <- object$spatial["fov"]
object <- demo_subsample_cells(object, n_keep, seed)
object$experiment$experiment_name <- "Mouse brain (Xenium), 500 cells"
save_demo_crb(object, output)

check <- readCerebro(output)
stopifnot(
  ncol(check$expression) == n_keep,
  identical(names(check$spatial), "fov"),
  nrow(check$spatial$fov$coordinates) > 0L
)
message(
  "Wrote ",
  output,
  ": ",
  ncol(check$expression),
  " cells, ",
  nrow(check$expression),
  " genes, ",
  format(file.size(output), big.mark = ","),
  " bytes; md5 ",
  unname(tools::md5sum(output))
)
