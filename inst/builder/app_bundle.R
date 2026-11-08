## Private generated-App assembly contract loader.

.builder_app_bundle_source_group <- get0(
  "builder_source_group",
  mode = "function",
  inherits = TRUE
)
if (!is.function(.builder_app_bundle_source_group)) {
  ## Sourced without prerequisite.R (tests, the release coordinator): take the
  ## group loader from the prerequisite.R beside this file, defining nothing
  ## else in the caller's environment.
  .builder_app_bundle_source_group <- local({
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
      stop(
        "Builder App assembly source location is unavailable.",
        call. = FALSE
      )
    }
    runtime <- new.env(parent = globalenv())
    base::source(candidates[[1L]], local = runtime, encoding = "UTF-8")
    runtime$builder_source_group
  })
}
.builder_app_bundle_source_group(
  "app_bundle",
  c(
    "auth.R",
    "contract.R",
    "backend.R",
    "request.R",
    "topology.R",
    "build.R"
  ),
  envir = environment(),
  message = "Builder App assembly source location is unavailable."
)
rm(.builder_app_bundle_source_group)
