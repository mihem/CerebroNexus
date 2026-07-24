# Populate the immune_repertoire_precomputed slot of a bundled demo .crb so the
# "Repertoire (precomputed)" tab has scRepertoire-computed tables to draw.
# Run on a machine with scRepertoire installed:
#   Rscript data-raw/add_precomputed_repertoire.R
# Re-run after changing computeRepertoireMetrics() or upgrading scRepertoire.
#
# A .crb serializes the R6 method table it was built with, so an older object
# lacks the new addImmuneRepertoirePrecomputed() method and its object env is
# locked. We therefore rebuild the object with the CURRENT class and copy the
# public data fields across (this also refreshes its method table).

suppressMessages(devtools::load_all(".", quiet = TRUE))
stopifnot(requireNamespace("scRepertoire", quietly = TRUE))

# Only the richest IR demo is updated; example.crb is left untouched so the
# test suite keeps reading a stable fixture.
targets <- "inst/extdata/v1.4/demo_full_tcr_bcr.crb"

# Carry over every public data field EXCEPT the precomputed slot (set via the
# method afterwards). Read the field names straight from the current generator
# so no field is ever missed.
data_fields <- setdiff(
  names(Cerebro_v1.3$public_fields),
  "immune_repertoire_precomputed"
)

for (path in targets) {
  if (!file.exists(path)) {
    message("skip (missing): ", path)
    next
  }
  old <- readRDS(path)
  ir <- tryCatch(old$getImmuneRepertoire(), error = function(e) NULL)
  if (is.null(ir) || length(ir) == 0) {
    message("skip (no IR): ", basename(path))
    next
  }
  tables <- computeRepertoireMetrics(ir, verbose = TRUE)
  if (length(tables) == 0) {
    message("skip (nothing computed): ", basename(path))
    next
  }

  fresh <- Cerebro_v1.3$new()
  for (f in data_fields) {
    val <- tryCatch(old[[f]], error = function(e) NULL)
    if (!is.null(val)) {
      tryCatch(fresh[[f]] <- val, error = function(e) {
        message("  could not copy field '", f, "': ", conditionMessage(e))
      })
    }
  }
  fresh$addImmuneRepertoirePrecomputed(tables)
  saveRDS(fresh, path, compress = "xz")
  message("updated ", basename(path))
}
