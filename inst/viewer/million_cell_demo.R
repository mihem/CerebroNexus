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

## The four-stain microscopy composite is embedded in the CRB itself, so the
## menu entry only adds the dataset and makes the image, not a dense field of
## cell dots, the backdrop. (External images are served only from inside the
## app's own folders, so a cache-resident PNG would be silently refused.)
viewerAddXeniumPrimeDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_XENIUM_PRIME_DEMO_CRB", unset = "")
) {
  if (!nzchar(path)) {
    return(options)
  }
  label <- "Mouse brain (Xenium Prime 5K)"
  options <- viewerAddLargeDemo(
    options,
    path,
    "CEREBRO_XENIUM_PRIME_DEMO_CRB",
    label
  )
  options[["viewer_content"]][[label]][["spatial_point_appearance"]] <- list(
    coronal = list(point_size = 1, point_opacity = 0.1)
  )
  options
}
