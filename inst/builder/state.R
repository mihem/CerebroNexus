## Builder state loader. Files are evaluated in order in the caller's
## environment so the public and internal API remains identical.

.builder_state_source_group <- get0(
  "builder_source_group",
  mode = "function",
  inherits = TRUE
)
if (!is.function(.builder_state_source_group)) {
  ## Sourced without prerequisite.R (tests, the release coordinator): take the
  ## group loader from the prerequisite.R beside this file, defining nothing
  ## else in the caller's environment.
  .builder_state_source_group <- local({
    loaders <- unlist(lapply(
      sys.frames(),
      function(frame) get0("ofile", envir = frame, inherits = FALSE)
    ))
    loaders <- loaders[is.character(loaders) & nzchar(loaders)]
    candidates <- c(
      if (length(loaders)) {
        file.path(dirname(tail(loaders, 1L)), "prerequisite.R")
      },
      file.path(getwd(), "prerequisite.R"),
      file.path(getwd(), "inst", "builder", "prerequisite.R"),
      system.file("builder", "prerequisite.R", package = "CerebroNexus")
    )
    candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (!length(candidates)) {
      stop("Builder state source location is unavailable.", call. = FALSE)
    }
    runtime <- new.env(parent = globalenv())
    base::source(candidates[[1L]], local = runtime, encoding = "UTF-8")
    runtime$builder_source_group
  })
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
.builder_state_source_group(
  "state",
  c(
    "core.R",
    "content.R",
    "metadata.R",
    "dataset.R"
  ),
  envir = environment(),
  message = "Builder state source location is unavailable.",
  extra_dirs = file.path(.builder_state_explicit_dir, "state")
)
rm(.builder_state_explicit_dir, .builder_state_source_group)
