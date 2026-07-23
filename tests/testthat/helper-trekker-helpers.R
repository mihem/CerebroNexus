# Source the Trekker pure helpers (trekker_gene_suggest,
# trekker_numeric_meta_cols) from the same inst/ file the running app sources, so
# the unit tests exercise the real runtime code rather than a copy.
trekker_helpers_file <- file.path(
  "..",
  "..",
  "inst",
  "shiny",
  "v1.4",
  "trekker",
  "helpers.R"
)

if (!file.exists(trekker_helpers_file)) {
  trekker_helpers_file <- system.file(
    "shiny/v1.4/trekker/helpers.R",
    package = "CerebroNexus"
  )
}

sys.source(trekker_helpers_file, envir = environment())
