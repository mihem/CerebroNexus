## The authors' DAPI picture of every section of the Kukanja et al. 2024 demos
## (prepare_viewer_kukanja_data.R), placed under the cells.
##
## Each h5ad keeps one picture per tissue slide in uns/spatial/<slide>/images/
## hires and every cell's position on it. The Viewer draws the cells at the
## authors' aligned coordinates (obsm xy_loc, y mirrored), which turn and mirror
## each slide differently, so the picture of a section is resampled into those
## coordinates: the map from picture pixels to Viewer coordinates is fitted on
## the section's own cells (a rotation, a mirror, a scale and a shift), and the
## picture is cut to the section, since some slides hold two sections. The
## pictures go next to the CRB, in <CRB name>.pictures/, with pictures.rds
## naming each one and its bounds; the Viewer serves them as Spatial images
## (viewerAddLargeDemo()). The CRB and its stamp are not touched.
##
## The names start with .viewerPicturesKukanja so that the CRB's recipe
## (.demoRecipe(, "Kukanja")) does not count them.

## Where each data set's pictures and the cells' pixels come from: `image_by` is
## the obs column naming each cell's slide (the uns/spatial key), `pixels` the
## obsm matrix of positions, in hires pixels (`um_per_pixel` NULL) or in
## microns of a picture of `um_per_pixel` microns per full-resolution pixel.
.viewerPicturesKukanjaSets <- function() {
  list(
    eae = list(
      image_by = "sample_id",
      pixels = "spatial_hires",
      um_per_pixel = NULL
    ),
    ## Xenium morphology images: 0.2125 um per pixel.
    ms = list(
      image_by = "library_id",
      pixels = "spatial",
      um_per_pixel = 0.2125
    )
  )
}

.viewerPicturesKukanjaPaths <- function(set, cache_dir = NULL) {
  paths <- .viewerKukanjaPaths(set, cache_dir)
  dir <- sub("[.]crb$", ".pictures", paths$crb)
  list(
    crb = paths$crb,
    archive = paths$archive,
    dir = dir,
    manifest = file.path(dir, "pictures.rds"),
    stamp = file.path(dir, "pictures-version")
  )
}

## The cells' slide, section and positions: picture pixels and the Viewer's.
.viewerPicturesKukanjaCells <- function(h5ad, set) {
  spec <- .viewerPicturesKukanjaSets()[[set]]
  xy <- .viewerKukanjaObsm(h5ad, "xy_loc")
  list(
    slide = as.character(.viewerKukanjaObs(h5ad, spec$image_by)),
    section = as.character(.viewerKukanjaObs(h5ad, "sample_id")),
    pixels = .viewerKukanjaObsm(h5ad, spec$pixels),
    viewer = cbind(xy[, 1L], -xy[, 2L])
  )
}

## One section's picture in Viewer coordinates: a grey picture with alpha (no
## picture outside the slide), its bounds, or NULL when the fit fails.
.viewerPicturesKukanjaSection <- function(picture, pixels, viewer) {
  ## Viewer = [pixel_x, pixel_y, 1] %*% fit, by least squares.
  design <- cbind(pixels, 1)
  fit <- qr.solve(design, viewer)
  error <- max(abs(design %*% fit - viewer))
  extent <- max(apply(viewer, 2L, function(v) diff(range(v))))
  if (!all(is.finite(fit)) || error > 0.01 * extent) {
    return(NULL)
  }
  linear <- fit[1:2, , drop = FALSE]
  shift <- fit[3L, ]
  ## Viewer units per picture pixel; the cut keeps the picture's resolution.
  unit <- sqrt(abs(det(linear)))
  margin <- 0.04 * extent
  xr <- range(viewer[, 1L]) + c(-margin, margin)
  yr <- range(viewer[, 2L]) + c(-margin, margin)
  width <- min(2500L, max(1L, as.integer(ceiling(diff(xr) / unit))))
  height <- min(2500L, max(1L, as.integer(ceiling(diff(yr) / unit))))
  step_x <- diff(xr) / width
  step_y <- diff(yr) / height
  ## Row 1 of the cut is its top (the largest y), as the Viewer draws it.
  x <- xr[[1L]] + (seq_len(width) - 0.5) * step_x
  y <- yr[[2L]] - (seq_len(height) - 0.5) * step_y
  grid_x <- rep(x, each = height)
  grid_y <- rep(y, times = width)
  inverse <- solve(linear)
  source <- cbind(grid_x - shift[[1L]], grid_y - shift[[2L]]) %*% inverse
  column <- floor(source[, 1L]) + 1
  row <- floor(source[, 2L]) + 1
  ## rhdf5 reads the picture transposed: picture[column, row].
  inside <- column >= 1 &
    column <= nrow(picture) &
    row >= 1 &
    row <= ncol(picture)
  values <- numeric(length(grid_x))
  values[inside] <- picture[cbind(column[inside], row[inside])]
  ## DAPI is dark: stretch its bright end to the full range.
  shown <- values[inside & values > 0]
  limits <- if (length(shown)) {
    stats::quantile(shown, c(0.01, 0.995), names = FALSE)
  } else {
    c(0, 1)
  }
  grey <- pmin(1, pmax(0, (values - limits[[1L]]) / max(diff(limits), 1)))
  image <- array(0, dim = c(height, width, 2L))
  image[,, 1L] <- matrix(grey, nrow = height)
  image[,, 2L] <- matrix(as.numeric(inside), nrow = height)
  list(
    image = image,
    bounds = c(
      xmin = xr[[1L]],
      xmax = xr[[2L]],
      ymin = yr[[1L]],
      ymax = yr[[2L]]
    )
  )
}

## Writes every section's picture into `dir` and returns the Spatial image
## manifest: section -> list(DAPI = list(path, bounds)), paths relative to `dir`.
.viewerPicturesKukanjaWrite <- function(h5ad, set, dir) {
  spec <- .viewerPicturesKukanjaSets()[[set]]
  cells <- .viewerPicturesKukanjaCells(h5ad, set)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- list()
  for (section in unique(cells$section)) {
    rows <- which(cells$section == section)
    slide <- names(sort(table(cells$slide[rows]), decreasing = TRUE))[[1L]]
    base <- paste0("uns/spatial/", slide)
    picture <- tryCatch(
      rhdf5::h5read(h5ad, paste0(base, "/images/hires")),
      error = function(error) NULL
    )
    if (length(dim(picture)) != 2L) {
      message("No picture for section ", section, ".")
      next
    }
    storage.mode(picture) <- "double"
    pixels <- cells$pixels[rows, , drop = FALSE]
    if (!is.null(spec$um_per_pixel)) {
      scale <- as.numeric(rhdf5::h5read(
        h5ad,
        paste0(base, "/scalefactors/tissue_hires_scalef")
      ))
      pixels <- pixels / spec$um_per_pixel * scale
    }
    cut <- .viewerPicturesKukanjaSection(
      picture,
      pixels,
      cells$viewer[rows, , drop = FALSE]
    )
    if (is.null(cut)) {
      message("The picture of section ", section, " does not fit its cells.")
      next
    }
    file <- paste0(gsub("[^A-Za-z0-9_.-]", "_", section), ".png")
    png::writePNG(cut$image, file.path(dir, file))
    manifest[[section]] <- list(DAPI = list(path = file, bounds = cut$bounds))
  }
  manifest
}

.viewerPicturesKukanjaComplete <- function(paths) {
  file.exists(paths$manifest) &&
    .demoStampCurrent(paths$stamp, .viewerPicturesKukanjaRecipe)
}

## `set`: "eae" or "ms"; the data set's CRB must be prepared. `h5ad`: an
## unpacked h5ad to read instead of unpacking the cached archive.
prepareViewerPicturesKukanja <- function(
  set = c("eae", "ms"),
  cache_dir = NULL,
  h5ad = NULL
) {
  set <- match.arg(set)
  paths <- .viewerPicturesKukanjaPaths(set, cache_dir)
  if (.viewerPicturesKukanjaComplete(paths)) {
    return(paths$manifest)
  }
  .demoRequire(c("png", "rhdf5"), "Kukanja pictures")
  if (!file.exists(paths$crb)) {
    stop("Prepare the ", set, " data set before its pictures.", call. = FALSE)
  }
  stage <- tempfile("pictures-stage-", dirname(paths$dir))
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  if (is.null(h5ad)) {
    message("Unpacking ", basename(paths$archive), " for its pictures...")
    h5ad <- .viewerKukanjaExtract(paths$archive, file.path(stage, "work"))
  }
  message("Writing the section pictures...")
  manifest <- .viewerPicturesKukanjaWrite(h5ad, set, file.path(stage, "out"))
  saveRDS(manifest, file.path(stage, "out", "pictures.rds"))
  .demoStampWrite(
    file.path(stage, "out", "pictures-version"),
    .viewerPicturesKukanjaRecipe
  )
  unlink(file.path(stage, "work"), recursive = TRUE)
  .demoPublishStage(
    file.path(stage, "out"),
    paths$dir,
    dirname(paths$dir),
    "Kukanja pictures"
  )
  paths$manifest
}

.viewerPicturesKukanjaRecipe <- .demoRecipe(environment(), "PicturesKukanja")
