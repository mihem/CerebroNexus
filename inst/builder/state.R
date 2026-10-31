## Builder state loader. Files are evaluated in order in the caller's
## environment so the public and internal API remains identical.

.builder_state_source_utf8 <- get0(
  "builder_source_utf8",
  mode = "function",
  inherits = TRUE
)
if (!is.function(.builder_state_source_utf8)) {
  .builder_state_source_utf8 <- function(file, envir) {
    ofile <- normalizePath(file, winslash = "/", mustWork = TRUE)
    lines <- readLines(ofile, encoding = "UTF-8", warn = FALSE)
    expressions <- parse(text = lines, encoding = "UTF-8")
    for (index in seq_along(expressions)) {
      eval(expressions[[index]], envir = envir)
    }
    invisible(TRUE)
  }
}

.builder_state_explicit_dir <- get0(
  "dir",
  envir = environment(),
  inherits = FALSE
)
if (
  !is.character(.builder_state_explicit_dir) ||
    length(.builder_state_explicit_dir) != 1L ||
    is.na(.builder_state_explicit_dir)
) {
  .builder_state_explicit_dir <- ""
}
.builder_state_source_files <- unlist(lapply(
  sys.frames(),
  function(frame) get0("ofile", envir = frame, inherits = FALSE)
))
.builder_state_source_files <- .builder_state_source_files[
  is.character(.builder_state_source_files) &
    nzchar(.builder_state_source_files)
]
.builder_state_sibling_dir <- if (length(.builder_state_source_files)) {
  file.path(dirname(tail(.builder_state_source_files, 1L)), "state")
} else {
  ""
}
.builder_state_source_candidates <- c(
  .builder_state_sibling_dir,
  file.path(.builder_state_explicit_dir, "state"),
  file.path(getwd(), "state"),
  file.path(getwd(), "inst", "builder", "state"),
  system.file("builder", "state", package = "CerebroNexus")
)
.builder_state_source_candidates <- unique(
  .builder_state_source_candidates[nzchar(.builder_state_source_candidates)]
)
.builder_state_source_matches <- .builder_state_source_candidates[
  dir.exists(.builder_state_source_candidates)
]
if (!length(.builder_state_source_matches)) {
  stop("Builder state source location is unavailable.", call. = FALSE)
}
.builder_state_source_dir <- .builder_state_source_matches[[1L]]
for (.builder_state_source_name in c(
  "core.R",
  "content.R",
  "metadata.R",
  "dataset.R"
)) {
  .builder_state_source_utf8(
    file.path(.builder_state_source_dir, .builder_state_source_name),
    envir = environment()
  )
}
rm(
  .builder_state_explicit_dir,
  .builder_state_sibling_dir,
  .builder_state_source_files,
  .builder_state_source_dir,
  .builder_state_source_candidates,
  .builder_state_source_matches,
  .builder_state_source_name,
  .builder_state_source_utf8
)
