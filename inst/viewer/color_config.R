## Resolve palettes configured when a self-contained Viewer bundle was built.

resolve_configured_colors <- function(
  color_config,
  selected_path,
  configured_files
) {
  if (!is.list(color_config) || !length(color_config)) {
    return(list())
  }
  if (
    is.null(selected_path) ||
      length(selected_path) != 1L ||
      is.na(selected_path) ||
      !nzchar(selected_path)
  ) {
    return(list())
  }
  labels <- names(configured_files)
  if (is.null(configured_files) || is.null(labels)) {
    return(list())
  }
  match_at <- which(unname(configured_files) == selected_path)
  if (!length(match_at)) {
    return(list())
  }
  palette <- color_config[[labels[match_at[[1L]]]]]
  if (!is.list(palette) || !length(palette)) {
    return(list())
  }
  palette
}

apply_configured_colors <- function(defaults, configured) {
  if (!length(configured) || is.null(names(configured))) {
    return(defaults)
  }
  shared <- intersect(names(defaults), names(configured))
  if (!length(shared)) {
    return(defaults)
  }
  defaults[shared] <- as.character(configured[shared])
  defaults
}
