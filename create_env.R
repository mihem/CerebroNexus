# Run this script to regenerate default.nix after changing dependencies.
# You need rix installed: nix-shell -p rPackages.rix R --run "Rscript create_env.R"
# Then source this file.

library(rix)

# Fetch the latest date published by the osmzhlab Attic cache so that
# default.nix uses a nixpkgs snapshot with available binary substitutes.
reports_readme <- readLines(
  "https://raw.githubusercontent.com/mihem/attic/main/reports/README.md",
  warn = FALSE
)
available_dates <- regmatches(
  reports_readme,
  gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", reports_readme)
)
available_dates <- sort(unique(unlist(available_dates)))
latest_date <- tail(available_dates, 1)
if (length(latest_date) == 0 || is.na(latest_date)) {
  stop("Could not determine latest osmzhlab cache date")
}
cat("Using latest_date:", latest_date, "\n")


rix(
  date = latest_date,
  r_pkgs = c(
    # development and tests (DESCRIPTION Suggests)
    "devtools",
    "testthat",
    "pkgdown",
    "shinytest2",
    "chromote",
    "xml2",
    "callr",
    "withr",
    "png",
    "httr",
    # optional features, guarded by requireNamespace() (DESCRIPTION Suggests)
    "monocle",
    "ggdendro",
    "patchwork",
    "HDF5Array",
    "readxl",
    "Seurat",
    "SeuratObject",
    "shinymanager",
    "anndataR", # reference reader in a test (Suggests)
    "arrow", # SpatialData shapes (Suggests)
    "jpeg", # Builder tissue images (Suggests)
    "writexl", # Builder tables (Suggests)
    # runtime dependencies (DESCRIPTION Imports)
    "base64enc",
    "biomaRt",
    "colourpicker",
    "dplyr",
    "DT",
    "fs",
    "future.apply",
    "ggplot2",
    "glue",
    "GSVA",
    "htmltools",
    "htmlwidgets",
    "igraph",
    "jsonlite",
    "later",
    "Matrix",
    "mirai",
    "msigdbr",
    "pbapply",
    "plotly",
    "qvalue",
    "qs2",
    "R6",
    "rlang",
    "scales",
    "scRepertoire",
    "shiny",
    "shinycssloaders",
    "shinydashboard",
    "shinyjs",
    "shinyWidgets",
    "stringdist",
    "stringr",
    "tibble",
    "tidyr",
    "tidyselect"
  ),
  system_pkgs = c(
    "chromium", # headless browser for shinytest2
    "nodejs", # complete-script JavaScript tests
    "pandoc" # required for building vignettes
  ),
  ide = "none",
  project_path = ".",
  overwrite = TRUE
)

cat(
  "###################################################################################################\n"
)
cat(
  "###################################################################################################\n"
)
cat(
  "###################################################################################################\n"
)
cat("Updated default.nix successfully\n")
