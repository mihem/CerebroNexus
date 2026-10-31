## Deterministic Builder plan loader.

.builder_plan_source_utf8 <- get0(
  "builder_source_utf8",
  mode = "function",
  inherits = TRUE
)
if (!is.function(.builder_plan_source_utf8)) {
  .builder_plan_source_utf8 <- function(file, envir) {
    ofile <- normalizePath(file, winslash = "/", mustWork = TRUE)
    lines <- readLines(ofile, encoding = "UTF-8", warn = FALSE)
    expressions <- parse(text = lines, encoding = "UTF-8")
    for (index in seq_along(expressions)) {
      eval(expressions[[index]], envir = envir)
    }
    invisible(TRUE)
  }
}

.builder_plan_source_files <- unlist(lapply(
  sys.frames(),
  function(frame) get0("ofile", envir = frame, inherits = FALSE)
))
.builder_plan_source_files <- .builder_plan_source_files[
  is.character(.builder_plan_source_files) &
    nzchar(.builder_plan_source_files)
]
.builder_plan_sibling_dir <- if (length(.builder_plan_source_files)) {
  file.path(
    dirname(tail(.builder_plan_source_files, 1L)),
    "plan"
  )
} else {
  ""
}
.builder_plan_source_candidates <- unique(c(
  .builder_plan_sibling_dir,
  file.path(getwd(), "plan"),
  file.path(getwd(), "inst", "builder", "plan"),
  system.file("builder", "plan", package = "CerebroNexus")
))
.builder_plan_source_candidates <- .builder_plan_source_candidates[
  nzchar(.builder_plan_source_candidates)
]
.builder_plan_source_matches <- .builder_plan_source_candidates[
  dir.exists(.builder_plan_source_candidates)
]
if (!length(.builder_plan_source_matches)) {
  stop("Builder plan source location is unavailable.", call. = FALSE)
}
.builder_plan_source_dir <- .builder_plan_source_matches[[1L]]
for (.builder_plan_source_name in c(
  "defaults.R",
  "assets.R",
  "preflight.R",
  "freeze.R"
)) {
  .builder_plan_source_utf8(
    file.path(.builder_plan_source_dir, .builder_plan_source_name),
    envir = environment()
  )
}
rm(
  .builder_plan_sibling_dir,
  .builder_plan_source_candidates,
  .builder_plan_source_dir,
  .builder_plan_source_files,
  .builder_plan_source_matches,
  .builder_plan_source_name,
  .builder_plan_source_utf8
)
