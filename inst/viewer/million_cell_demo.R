viewerAddMillionCellDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_1M_DEMO_CRB", unset = "")
) {
  if (!nzchar(path)) {
    return(options)
  }
  path <- normalizePath(path, mustWork = FALSE)
  sidecar <- sub("[.]crb$", ".bpcells", path)
  if (
    identical(sidecar, path) ||
      !file.exists(path) ||
      dir.exists(path) ||
      !dir.exists(sidecar) ||
      !length(list.files(sidecar, all.files = TRUE, no.. = TRUE))
  ) {
    stop(
      "CEREBRO_1M_DEMO_CRB must name a .crb with an adjacent non-empty .bpcells directory.",
      call. = FALSE
    )
  }

  label <- "10x E18 mouse brain (1M)"
  datasets <- options[["crb_file_to_load"]]
  old_names <- names(datasets)
  add_setting <- function(key, value) {
    current <- options[[key]]
    if (
      length(current) == 1L &&
        (is.null(names(current)) || !nzchar(names(current)))
    ) {
      current <- stats::setNames(rep(current, length(datasets)), old_names)
    }
    current[label] <- value
    options[[key]] <<- current
  }

  datasets[label] <- path
  options[["crb_file_to_load"]] <- datasets
  add_setting("point_size", 1)
  add_setting("point_opacity", 0.5)
  add_setting("percentage_cells_to_show", 10)
  options
}
