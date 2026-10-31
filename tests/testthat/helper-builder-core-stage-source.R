builder_core_stage_source_files <- c(
  "ui/core_stage.R",
  file.path("ui", "core_stage", "assay.R"),
  file.path("ui", "core_stage", "groups.R"),
  file.path("ui", "core_stage", "projections.R"),
  file.path("ui", "core_stage", "trajectory.R"),
  file.path("ui", "core_stage", "specialized.R"),
  file.path("ui", "core_stage", "stage.R")
)

builder_core_stage_source_lines <- function() {
  unlist(
    lapply(
      builder_core_stage_source_files,
      function(file) {
        readLines(
          builder_profile_inst_path("builder", file),
          warn = FALSE
        )
      }
    ),
    use.names = FALSE
  )
}

builder_core_stage_source_text <- function() {
  paste(builder_core_stage_source_lines(), collapse = "\n")
}
