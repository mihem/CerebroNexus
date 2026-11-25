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

## Adds the CRB at `path` to the data set menu under `label`, with its point
## appearance; settings given once for every data set become per data set.
viewerAddDemoDataset <- function(
  options,
  path,
  label,
  point_size,
  point_opacity
) {
  datasets <- options[["crb_file_to_load"]]
  old_names <- names(datasets)
  add_setting <- function(key, value) {
    current <- options[[key]]
    if (
      length(current) == 1L &&
        (is.null(names(current)) || !nzchar(names(current)))
    ) {
      current <- stats::setNames(rep(current, length(old_names)), old_names)
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
  options <- viewerAddDemoDataset(
    options,
    path,
    label,
    point_size,
    point_opacity
  )
  ## Section pictures prepared next to the CRB (<name>.pictures/pictures.rds,
  ## e.g. prepareViewerPicturesKukanja()).
  pictures <- file.path(sub("[.]crb$", ".pictures", path), "pictures.rds")
  if (file.exists(pictures)) {
    options <- viewerLinkDemoPictures(
      options,
      label,
      readRDS(pictures),
      dirname(pictures),
      file.path("demo-large", sub("[.]crb$", "", basename(path)))
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

## 59 coronal sections of one mouse brain, about 67,000 cells each.
viewerAddAllenDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_ALLEN_DEMO_CRB", unset = "")
) {
  viewerAddLargeDemo(
    options,
    path,
    "CEREBRO_ALLEN_DEMO_CRB",
    "Allen whole mouse brain MERFISH (59 sections, 3.9M)",
    point_size = 1,
    point_opacity = 0.6
  )
}

## Kukanja et al. 2024: mouse EAE spinal cord by in situ sequencing, about
## 11,000 cells per section, and human MS spinal cord by Xenium.
viewerAddEaeDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_EAE_DEMO_CRB", unset = "")
) {
  viewerAddLargeDemo(
    options,
    path,
    "CEREBRO_EAE_DEMO_CRB",
    "Kukanja et al. mouse EAE ISS (44 sections)",
    point_size = 1,
    point_opacity = 0.6
  )
}

viewerAddMsDemo <- function(
  options,
  path = Sys.getenv("CEREBRO_MS_DEMO_CRB", unset = "")
) {
  viewerAddLargeDemo(
    options,
    path,
    "CEREBRO_MS_DEMO_CRB",
    "Kukanja et al. human MS Xenium (6 sections)",
    point_size = 1,
    point_opacity = 0.6
  )
}

## The Viewer serves pictures only from its own spatial-assets folder: links
## (copies where a link cannot be made) the pictures of `images` (section ->
## label -> list(path, bounds), paths relative to `root`) under
## spatial-assets/<folder> and names them as the data set `label`'s images.
viewerLinkDemoPictures <- function(options, label, images, root, folder) {
  assets <- file.path("spatial-assets", folder)
  target <- file.path(options[["cerebro_root"]] %||% ".", assets)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  options[["spatial_images"]][[label]] <- lapply(images, function(section) {
    lapply(section, function(image) {
      source <- file.path(root, image$path)
      linked <- file.path(target, basename(source))
      if (
        !file.exists(linked) ||
          !identical(file.size(linked), file.size(source))
      ) {
        unlink(linked)
        if (!suppressWarnings(file.link(source, linked))) {
          file.copy(source, linked)
        }
      }
      image$path <- file.path(assets, basename(source))
      image
    })
  })
  options
}

## The multi-section Visium demos run-demo.R built with the Builder: each
## demo.rds (prepareViewerVisiumDemoData()) names its CRB, the picture of every
## section and the display settings the Builder chose, Spatial opening as the
## grid. `files` lists them, separated by the path separator.
##
## The Viewer serves pictures only from its own spatial-assets folder, so each
## demo's pictures are linked there (copied where a link cannot be made), under
## spatial-assets/demo-visium/<CRB name>.
viewerAddVisiumDemos <- function(
  options,
  files = Sys.getenv("CEREBRO_VISIUM_DEMOS", unset = "")
) {
  files <- strsplit(files, .Platform$path.sep, fixed = TRUE)[[1L]]
  for (file in files[nzchar(files)]) {
    root <- dirname(normalizePath(file, mustWork = TRUE))
    demo <- readRDS(file)
    label <- demo$label
    options <- viewerAddDemoDataset(
      options,
      file.path(root, demo$crb),
      label,
      demo$point_size %||% 4,
      demo$point_opacity %||% 1
    )
    options <- viewerLinkDemoPictures(
      options,
      label,
      demo$spatial_images,
      root,
      file.path("demo-visium", sub("[.]crb$", "", demo$crb))
    )
    options[["spatial_image_settings"]][[label]] <- demo$spatial_image_settings
    options[["viewer_content"]][[label]] <- demo$viewer_content
  }
  options
}

## The four-stain microscopy composite is embedded in the CRB itself, so the
## menu entry only adds the dataset and sizes the cell dots for 63,000 cells:
## small and nearly opaque, so the clusters read clearly while the tissue still
## shows between them. (External images are served only from inside the app's
## own folders, so a cache-resident PNG would be silently refused.)
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
    label,
    point_size = 2,
    point_opacity = 0.8
  )
  options[["viewer_content"]][[label]][["spatial_point_appearance"]] <- list(
    coronal = list(point_size = 2, point_opacity = 0.9)
  )
  options
}
