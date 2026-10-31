## Private generated-App assembly contract loader.

.builder_app_bundle_source_utf8 <- get0(
  "builder_source_utf8",
  mode = "function",
  inherits = TRUE
)
if (!is.function(.builder_app_bundle_source_utf8)) {
  .builder_app_bundle_source_utf8 <- function(file, envir) {
    ofile <- normalizePath(file, winslash = "/", mustWork = TRUE)
    lines <- readLines(ofile, encoding = "UTF-8", warn = FALSE)
    expressions <- parse(text = lines, encoding = "UTF-8")
    for (index in seq_along(expressions)) {
      eval(expressions[[index]], envir = envir)
    }
    invisible(TRUE)
  }
}

.builder_app_bundle_source_files <- unlist(lapply(
  sys.frames(),
  function(frame) get0("ofile", envir = frame, inherits = FALSE)
))
.builder_app_bundle_source_files <- .builder_app_bundle_source_files[
  is.character(.builder_app_bundle_source_files) &
    nzchar(.builder_app_bundle_source_files)
]
.builder_app_bundle_sibling_dir <- if (
  length(.builder_app_bundle_source_files)
) {
  file.path(
    dirname(tail(.builder_app_bundle_source_files, 1L)),
    "app_bundle"
  )
} else {
  ""
}
.builder_app_bundle_source_candidates <- unique(c(
  .builder_app_bundle_sibling_dir,
  file.path(getwd(), "app_bundle"),
  file.path(getwd(), "inst", "builder", "app_bundle"),
  system.file(
    "builder",
    "app_bundle",
    package = "CerebroNexus"
  )
))
.builder_app_bundle_source_candidates <-
  .builder_app_bundle_source_candidates[
    nzchar(.builder_app_bundle_source_candidates)
  ]
.builder_app_bundle_source_matches <-
  .builder_app_bundle_source_candidates[
    dir.exists(.builder_app_bundle_source_candidates)
  ]
if (!length(.builder_app_bundle_source_matches)) {
  stop("Builder App assembly source location is unavailable.", call. = FALSE)
}
.builder_app_bundle_source_dir <-
  .builder_app_bundle_source_matches[[1L]]
for (.builder_app_bundle_source_name in c(
  "auth.R",
  "contract.R",
  "backend.R",
  "request.R",
  "topology.R",
  "build.R"
)) {
  .builder_app_bundle_source_utf8(
    file.path(
      .builder_app_bundle_source_dir,
      .builder_app_bundle_source_name
    ),
    envir = environment()
  )
}
rm(
  .builder_app_bundle_sibling_dir,
  .builder_app_bundle_source_candidates,
  .builder_app_bundle_source_dir,
  .builder_app_bundle_source_files,
  .builder_app_bundle_source_matches,
  .builder_app_bundle_source_name,
  .builder_app_bundle_source_utf8
)
