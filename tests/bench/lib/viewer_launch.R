# Shared launch and readiness helpers for browser benchmarks of the Viewer.

# Measure the viewer as deployments run it. An installed package loads only its
# NAMESPACE imports; devtools::load_all() also attaches every DESCRIPTION
# Imports package (GSVA, scRepertoire, biomaRt, HDF5Array, ...), which
# multiplies the R heap and makes each full garbage collection take seconds,
# so pauses land on whichever step allocates next. VIEWER_BENCH_LOAD=load_all
# keeps the source-tree mode for profiling an uninstalled tree.
viewer_bench_load_mode <- function() {
  validate_viewer_load_mode(tolower(Sys.getenv(
    "VIEWER_BENCH_LOAD",
    unset = "installed"
  )))
}

validate_viewer_load_mode <- function(mode) {
  if (!mode %in% c("installed", "load_all")) {
    stop("VIEWER_BENCH_LOAD must be installed or load_all.", call. = FALSE)
  }
  invisible(mode)
}

# Install a candidate source tree (c(label = , root = )) into its own
# directory below library_root and return that library path.
install_candidate_package <- function(candidate, library_root) {
  name <- gsub("[^A-Za-z0-9_.-]", "_", candidate[["label"]])
  library <- file.path(library_root, name)
  dir.create(library, recursive = TRUE, showWarnings = FALSE)
  log <- file.path(library_root, paste0(name, "-install.log"))
  status <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "INSTALL", "--no-docs", "--no-multiarch", "--no-test-load",
      paste0("--library=", shQuote(library)),
      shQuote(candidate[["root"]])
    ),
    stdout = log,
    stderr = log
  )
  if (!identical(as.integer(status), 0L)) {
    stop(
      "Installing candidate '", candidate[["label"]], "' failed; see ", log,
      call. = FALSE
    )
  }
  normalizePath(library, winslash = "/")
}

# Install every candidate when the load mode needs it. `candidates` is a named
# list of c(label = , root = ); each gains a `library` element.
prepare_viewer_candidates <- function(candidates, load_mode) {
  if (!identical(load_mode, "installed")) {
    return(candidates)
  }
  library_root <- tempfile("viewer-bench-libraries-")
  dir.create(library_root)
  for (label in names(candidates)) {
    message("Installing candidate ", label, " into a temporary library...")
    candidates[[label]] <- c(
      candidates[[label]],
      library = install_candidate_package(candidates[[label]], library_root)
    )
  }
  candidates
}

# The app.R line that makes launchCerebro() available for one candidate.
viewer_load_line <- function(candidate, load_mode) {
  if (identical(load_mode, "load_all")) {
    sprintf(
      "devtools::load_all(%s, quiet = TRUE)",
      encodeString(candidate[["root"]], quote = '"')
    )
  } else {
    sprintf(
      "library(CerebroNexus, lib.loc = %s)",
      encodeString(candidate[["library"]], quote = '"')
    )
  }
}

# Data info renders its cell count inside the load_data_overview output; the
# element keeps the old load_data_number_of_cells id but is no output.
data_info_cells_js <- function() {
  paste0(
    "(() => {const cells=document.getElementById('load_data_number_of_cells');",
    "return cells ? cells.innerText : null;})()"
  )
}

# Wait until Data info shows its cell count; check it when expected_label is
# given (e.g. "1,000,000").
wait_for_data_info_cells <- function(app, expected_label = NULL,
                                     timeout = 900000) {
  app$wait_for_js(paste0(data_info_cells_js(), " !== null"), timeout = timeout)
  count <- app$get_js(data_info_cells_js())
  if (
    !is.null(expected_label) &&
      (!is.character(count) || !grepl(expected_label, count, fixed = TRUE))
  ) {
    stop("Data Info did not report ", expected_label, " cells.", call. = FALSE)
  }
  invisible(count)
}
