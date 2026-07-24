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

# Use the BPCells revision built by the osmzhlab Attic cache, not GitHub main.
pins_json <- readLines(
  sprintf(
    "https://raw.githubusercontent.com/mihem/attic/main/reports/%s/pins.json",
    latest_date
  ),
  warn = FALSE
)
pin_value <- function(name) {
  line <- grep(sprintf('"%s"', name), pins_json, value = TRUE)
  sub(sprintf('.*"%s"[[:space:]]*:[[:space:]]*"([^"]+)".*', name), "\\1", line)
}
pins_date <- pin_value("r_nixpkgs_date")
if (!identical(pins_date, latest_date)) {
  stop("pins.json date does not match selected osmzhlab cache date")
}
bpcells_sha <- pin_value("bp_cells_rev")
if (length(bpcells_sha) != 1 || !grepl("^[0-9a-f]{40}$", bpcells_sha)) {
  stop("Could not determine BPCells revision from osmzhlab Attic pins.json")
}
cat("Using BPCells revision:", bpcells_sha, "\n")


rix(
  date = latest_date,
  r_pkgs = c(
    # package development
    "devtools",
    "testthat",
    "pkgdown",
    "shinytest2",
    "callr",
    "png",
    "withr",
    "formattable",
    "stringr",
    "shinyvalidate",
    "Seurat",

    "SeuratObject",
    # runtime deps (CRAN)
    "biomaRt",
    "colourpicker",
    "dplyr",
    "DT",
    "future.apply",
    "ggplot2",
    "glue",
    "GSVA",
    "HDF5Array",
    "htmlwidgets",
    "httr",
    "igraph",
    "later",
    "Matrix",
    "msigdbr",
    "pbapply",
    "plotly",
    "qvalue",
    "R6",
    "rlang",
    "scales",
    "shiny",
    "shinycssloaders",
    "shinydashboard",
    "shinyFiles",
    "shinyjs",
    "shinyWidgets",
    "tibble",
    "tidyr",
    "tidyselect",
    "stringdist",
    "visNetwork"
  ),
  system_pkgs = c(
    "chromium", # headless browser for shinytest2
    "pandoc" # required for building vignettes
  ),
  git_pkgs = list(
    # BPCells is not on CRAN, install from GitHub
    list(
      package_name = "BPCells",
      repo_url = "https://github.com/bnprks/BPCells/r",
      commit = bpcells_sha
    )
  ),
  ide = "none",
  project_path = ".",
  overwrite = TRUE
)

# --- BPCells block fix ---
nix_file <- "default.nix"
nix_lines <- readLines(nix_file)

bp_start <- grep("BPCells = \\(pkgs.rPackages.buildRPackage \\{", nix_lines)
bp_end <- grep("^\\s*\\}\\);", nix_lines)
bp_end <- bp_end[bp_end > bp_start][1]

if (length(bp_start) == 1 && !is.na(bp_end)) {
  # Extract repo URL, rev, sha256 from BPCells block
  url_line <- grep("url = ", nix_lines[bp_start:bp_end], fixed = TRUE) +
    bp_start -
    1
  rev_line <- grep("rev = ", nix_lines[bp_start:bp_end], fixed = TRUE) +
    bp_start -
    1
  sha_line <- grep("sha256 = ", nix_lines[bp_start:bp_end], fixed = TRUE) +
    bp_start -
    1

  url <- sub(".*url = \"(.*)\";.*", "\\1", nix_lines[url_line])
  url_src <- sub("/r$", "", url) # Remove trailing /r for BPCells-src
  rev <- sub(".*rev = \"(.*)\";.*", "\\1", nix_lines[rev_line])
  sha <- sub(".*sha256 = \"(.*)\";.*", "\\1", nix_lines[sha_line])

  # Create complete replacement block
  replacement_block <- c(
    "    BPCells-src = pkgs.fetchgit {",
    sprintf("      url = \"%s\";", url_src),
    sprintf("      rev = \"%s\";", rev),
    sprintf("      sha256 = \"%s\";", sha),
    "    };",
    "",
    "    BPCells = (pkgs.rPackages.buildRPackage {",
    "      name = \"BPCells\";",
    "      src = \"${BPCells-src}/r\";",
    "      postPatch = \"patchShebangs configure\";",
    "      nativeBuildInputs = [ pkgs.hdf5.dev ];"
  )

  propagatedBuildInputs <- grep(
    "propagatedBuildInputs = ",
    nix_lines[bp_start:bp_end],
    fixed = TRUE
  ) +
    bp_start -
    1

  # Replace BPCells block with corrected version
  nix_lines <- c(
    nix_lines[1:(bp_start - 1)],
    replacement_block,
    nix_lines[propagatedBuildInputs:bp_end],
    nix_lines[(bp_end + 1):length(nix_lines)]
  )

  writeLines(nix_lines, nix_file)
}

cat(
  "###################################################################################################\n"
)
cat(
  "###################################################################################################\n"
)
cat(
  "###################################################################################################\n"
)
cat("Updated nix files successfully:\n")
print(nix_lines)
