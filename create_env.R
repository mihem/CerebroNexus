# Run this script to regenerate default.nix after changing dependencies.
# You need rix installed: nix-shell -p rPackages.rix R --run "Rscript create_env.R"
# Then source this file.

library(rix)

# Auto-fetch latest BPCells commit SHA from GitHub
bpcells_sha <- jsonlite::fromJSON(
  "https://api.github.com/repos/bnprks/BPCells/commits/main"
)$sha

rix(
  r_ver = "bleeding-edge",
  r_pkgs = c(
    # package development
    "devtools",
    "testthat",
    "pkgdown",
    "shinytest2",
    "formattable",
    "stringr",
    "shinyvalidate",
    "Seurat",

    "SeuratObject",
    # runtime deps (CRAN)
    "ape",
    "biomaRt",
    "colourpicker",
    "dplyr",
    "DT",
    "future.apply",
    "ggplot2",
    "glue",
    "GSVA",
    "HDF5Array",
    "httr",
    "igraph",
    "Matrix",
    "msigdbr",
    "pbapply",
    "plotly",
    "qvalue",
    "R6",
    "readr",
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
    "viridis"
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
