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

## A fixed four-stain PNG with source-derived bounds, using the existing image
## renderer. Preparation stores both beside the CRB, outside the package.
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
  image <- file.path(
    dirname(normalizePath(path, mustWork = TRUE)),
    "morphology.png"
  )
  info <- file.path(dirname(image), "morphology.json")
  if (!file.exists(image) || !file.exists(info)) {
    stop(
      "The Prime 5K demo needs morphology.png and morphology.json beside its .crb. ",
      "Run Rscript run-demo.R --data=xenium-prime --prepare-only.",
      call. = FALSE
    )
  }
  bounds <- jsonlite::fromJSON(info)$bounds
  values <- unlist(bounds, use.names = FALSE)
  if (
    !identical(names(bounds), c("xmin", "xmax", "ymin", "ymax")) ||
      !is.numeric(values) ||
      length(values) != 4L ||
      any(!is.finite(values)) ||
      bounds$xmin >= bounds$xmax ||
      bounds$ymin >= bounds$ymax
  ) {
    stop("The Prime 5K microscopy bounds are invalid.", call. = FALSE)
  }
  options[["spatial_images"]][[label]] <- list(
    coronal = list(
      "Four-stain morphology" = list(path = image, bounds = bounds)
    )
  )
  ## The image, rather than a dense field of opaque cell dots, is the backdrop.
  options[["viewer_content"]][[label]][["spatial_point_appearance"]] <- list(
    coronal = list(point_size = 1, point_opacity = 0.1)
  )
  options
}
