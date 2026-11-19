## A demo CRB keeps its expression matrix in the sibling <stem>.h5.
viewerDemoCheckSidecar <- function(path, variable) {
  sidecar <- sub("[.]crb$", ".h5", path)
  if (
    identical(sidecar, path) ||
      !file.exists(path) ||
      dir.exists(path) ||
      !file.exists(sidecar) ||
      dir.exists(sidecar)
  ) {
    stop(
      variable,
      " must name a .crb with an adjacent .h5 file.",
      call. = FALSE
    )
  }
  invisible(sidecar)
}

## Registers one large demo CRB under `label`, with the appearance every large
## demo shares. `path` empty means the demo is not configured.
viewerAddLargeDemo <- function(
  options,
  path,
  variable,
  label,
  point_size = 1,
  point_opacity = 0.5
) {
  if (!nzchar(path)) {
    return(options)
  }
  path <- normalizePath(path, mustWork = FALSE)
  viewerDemoCheckSidecar(path, variable)

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
  add_setting("point_size", point_size)
  add_setting("point_opacity", point_opacity)
  add_setting("percentage_cells_to_show", 100)

  ## run-demo.R writes a compressed twin of the H5 file as <stem>_compressed.crb
  ## next to the CRB; it opens from the same menu, labelled as such.
  twin <- sub("[.]crb$", "_compressed.crb", path)
  if (
    !identical(twin, path) &&
      file.exists(twin) &&
      file.exists(sub("[.]crb$", ".h5", twin))
  ) {
    options <- viewerAddLargeDemo(
      options,
      twin,
      variable,
      paste(label, "[compressed]"),
      point_size,
      point_opacity
    )
  }
  options
}

viewerAddMillionCellDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_1M_DEMO_CRB", unset = "")
) {
  viewerAddLargeDemo(
    options,
    path,
    "CEREBRO_1M_DEMO_CRB",
    "10x E18 mouse brain (1.3M)"
  )
}

viewerAddRenDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_REN_DEMO_CRB", unset = "")
) {
  viewerAddLargeDemo(
    options,
    path,
    "CEREBRO_REN_DEMO_CRB",
    "Ren et al. COVID-19 atlas (1.46M + TCR/BCR)"
  )
}

## Spatial scatter of single cells: small, fairly opaque points keep the
## tissue outline readable at around a million cells.
viewerAddCosMxDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_COSMX_DEMO_CRB", unset = "")
) {
  viewerAddLargeDemo(
    options,
    path,
    "CEREBRO_COSMX_DEMO_CRB",
    "CosMx SMI NSCLC (8 sections)",
    point_size = 1,
    point_opacity = 0.6
  )
}

viewerAddTcellDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_TCELL_DEMO_CRB", unset = "")
) {
  viewerAddLargeDemo(
    options,
    path,
    "CEREBRO_TCELL_DEMO_CRB",
    "Zheng et al. pan-cancer T cells (398K + TCR)"
  )
}
