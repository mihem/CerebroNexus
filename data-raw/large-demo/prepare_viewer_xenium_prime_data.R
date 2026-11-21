## Optional 10x Xenium Prime 5K mouse brain demo. Uses the published cells,
## panel genes, graph clusters and UMAP, plus a four-stain composite PNG.
## No transcript points, segmentation outlines or interactive image channels.
## Source download_helpers.R first. See xenium-prime.ATTRIBUTION.md.

.viewerXeniumPrimeFiles <- function() {
  list(
    outs = list(
      url = paste0(
        "https://cf.10xgenomics.com/samples/xenium/3.0.0/",
        "Xenium_Prime_Mouse_Brain_Coronal_FF/",
        "Xenium_Prime_Mouse_Brain_Coronal_FF_outs.zip"
      ),
      size = 13315451681,
      sha256 = "d624a46a468596969b7021dc12b0fa09690b92f741e6d51642183da8e45a3729"
    )
  )
}

.viewerXeniumPrimePaths <- function(cache_dir = NULL) {
  root <- file.path(.demoLargeCacheDir(cache_dir), "xenium-prime")
  prepared <- .demoPreparedDir(root)
  list(
    root = root,
    archive = file.path(root, "source", "outs.zip"),
    crb = file.path(prepared, "cerebro_xenium_prime.crb"),
    sidecar = file.path(prepared, "cerebro_xenium_prime.h5"),
    image = file.path(prepared, "morphology.png"),
    image_info = file.path(prepared, "morphology.json"),
    stamp = file.path(prepared, "demo-version")
  )
}

.viewerXeniumPrimeExtract <- function(archive, work) {
  members <- c(
    matrix = "cell_feature_matrix.h5",
    cells = "cells.csv.gz",
    analysis = "analysis.tar.gz",
    stats::setNames(
      sprintf("morphology_focus/morphology_focus_%04d.ome.tif", 0:3),
      paste0("channel", 0:3)
    )
  )
  utils::unzip(archive, files = unname(members), exdir = work)
  files <- stats::setNames(file.path(work, members), names(members))
  if (!all(file.exists(files))) {
    stop("The Prime 5K archive lacks required demo files.", call. = FALSE)
  }
  files
}

.viewerXeniumPrimeAnalysis <- function(archive, work, cells) {
  members <- utils::untar(archive, list = TRUE)
  read_table <- function(suffix) {
    member <- members[endsWith(members, suffix)]
    if (length(member) != 1L || grepl("(^|/)\\.\\.(/|$)", member)) {
      stop("Missing or ambiguous Prime 5K analysis: ", suffix, call. = FALSE)
    }
    status <- utils::untar(archive, files = member, exdir = work)
    if (status != 0L) {
      stop("Could not extract the Prime 5K analysis.", call. = FALSE)
    }
    table <- utils::read.csv(file.path(work, member), check.names = FALSE)
    if (!"Barcode" %in% names(table) || anyDuplicated(table$Barcode)) {
      stop("Invalid Prime 5K analysis cell names.", call. = FALSE)
    }
    if (anyNA(table$Barcode) || any(!table$Barcode %in% cells)) {
      stop("Prime 5K analysis and matrix cells disagree.", call. = FALSE)
    }
    table
  }
  clusters <- read_table("clustering/gene_expression_graphclust/clusters.csv")
  projection <- read_table("umap/gene_expression_2_components/projection.csv")
  if (!"Cluster" %in% names(clusters) || ncol(projection) != 3L) {
    stop("Unexpected Prime 5K analysis columns.", call. = FALSE)
  }
  umap <- projection[, -match("Barcode", names(projection)), drop = FALSE]
  if (
    !all(vapply(umap, is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(umap)))
  ) {
    stop("The published Prime 5K UMAP must be finite.", call. = FALSE)
  }
  names(umap) <- c("Dim_1", "Dim_2")
  umap <- umap[match(cells, projection$Barcode), , drop = FALSE]
  rownames(umap) <- cells
  cluster <- clusters$Cluster[match(cells, clusters$Barcode)]
  ## XOA omits 11 cells from this data set's clustering/UMAP. Keep those cells
  ## in Spatial and expression; do not invent an embedding for them.
  cluster <- ifelse(is.na(cluster), "Unassigned", paste("Cluster", cluster))
  list(cluster = factor(cluster), umap = umap)
}

.viewerXeniumPrimeRead <- function(files, work) {
  read <- function(path) {
    rhdf5::h5read(files[["matrix"]], paste0("matrix/", path))
  }
  shape <- as.integer(read("shape"))
  counts <- methods::new(
    "dgCMatrix",
    Dim = shape,
    i = as.integer(read("indices")),
    p = as.integer(read("indptr")),
    x = as.numeric(read("data")),
    Dimnames = list(
      as.character(read("features/name")),
      as.character(read("barcodes"))
    )
  )
  keep <- as.character(read("features/feature_type")) == "Gene Expression"
  counts <- counts[keep, , drop = FALSE]
  if (!nrow(counts) || !ncol(counts) || anyDuplicated(colnames(counts))) {
    stop("The Prime 5K matrix needs genes and unique cells.", call. = FALSE)
  }
  rownames(counts) <- make.unique(rownames(counts))
  cells <- colnames(counts)
  table <- utils::read.csv(gzfile(files[["cells"]]), stringsAsFactors = FALSE)
  required <- c(
    "cell_id",
    "x_centroid",
    "y_centroid",
    "cell_area",
    "nucleus_area"
  )
  if (!all(required %in% names(table)) || anyDuplicated(table$cell_id)) {
    stop(
      "The Prime 5K cell table lacks unique cells or coordinates.",
      call. = FALSE
    )
  }
  index <- match(cells, table$cell_id)
  if (anyNA(index)) {
    stop("The Prime 5K cell table and matrix disagree.", call. = FALSE)
  }
  table <- table[index, , drop = FALSE]
  coordinates <- data.frame(
    x = table$x_centroid,
    y = -table$y_centroid,
    row.names = cells
  )
  if (any(!is.finite(as.matrix(coordinates)))) {
    stop("Prime 5K coordinates must be finite.", call. = FALSE)
  }
  analysis <- .viewerXeniumPrimeAnalysis(files[["analysis"]], work, cells)
  metadata <- data.frame(
    cell_barcode = cells,
    sample = factor("coronal"),
    cluster = analysis$cluster,
    nUMI = as.numeric(Matrix::colSums(counts)),
    nGene = as.numeric(Matrix::colSums(counts > 0)),
    cell_area = table$cell_area,
    nucleus_area = table$nucleus_area,
    row.names = cells
  )
  list(
    counts = counts,
    metadata = metadata,
    coordinates = coordinates,
    umap = analysis$umap
  )
}

## The four stains of the morphology image, in file order, and the colour each
## is drawn in. The names are checked against the file, so a differently
## ordered or relabelled image is refused instead of being coloured wrongly.
.viewerXeniumPrimeStains <- function() {
  data.frame(
    name = c("DAPI", "ATP1A1/CD45/E-Cadherin", "18S", "alphaSMA/Vimentin"),
    colour = c("#4c8dff", "#3fd14a", "#ff4fd8", "#ffd23f"),
    stringsAsFactors = FALSE
  )
}

## The pyramid level to decode: the smallest whose longer side is at least
## `max_px`, else the largest one there is. `sizes` is a matrix of (x, y) per
## level, finest first. Returns the 1-based level.
.viewerXeniumPrimeLevel <- function(sizes, max_px = 4096L) {
  longest <- apply(sizes, 1L, max)
  enough <- which(longest >= max_px)
  if (length(enough)) enough[[which.min(longest[enough])]] else 1L
}

## Display window of one stain: the 0.5th to 99.9th percentile of its positive
## intensities. Stretching to the full 16-bit range would leave a stain whose
## signal sits in the low thousands looking black.
.viewerXeniumPrimeWindow <- function(plane) {
  positive <- as.numeric(plane)
  positive <- positive[is.finite(positive) & positive > 0]
  if (!length(positive)) {
    return(c(0, 1))
  }
  window <- stats::quantile(
    positive,
    c(0.005, 0.999),
    names = FALSE,
    type = 7
  )
  if (window[[2L]] > window[[1L]]) window else c(0, max(window[[2L]], 1))
}

## The four stains of `planes` (rows x columns x 4) as one RGB image: each
## stain is stretched over its own window, drawn in its colour, and the four
## are added. Returns the image (rows x columns x 3, in 0..1) and the windows.
.viewerXeniumPrimeCompose <- function(
  planes,
  stains = .viewerXeniumPrimeStains()
) {
  if (length(dim(planes)) != 3L || dim(planes)[[3L]] != nrow(stains)) {
    stop("Expected one plane per stain.", call. = FALSE)
  }
  rgb <- array(0, dim = c(dim(planes)[1:2], 3L))
  windows <- vector("list", nrow(stains))
  for (stain in seq_len(nrow(stains))) {
    plane <- planes[,, stain]
    window <- .viewerXeniumPrimeWindow(plane)
    windows[[stain]] <- window
    signal <- pmin(
      pmax((plane - window[[1L]]) / (window[[2L]] - window[[1L]]), 0),
      1
    )
    weight <- grDevices::col2rgb(stains$colour[[stain]])[, 1L] / 255
    for (channel in 1:3) {
      rgb[,, channel] <- rgb[,, channel] + signal * weight[[channel]]
    }
  }
  list(rgb = pmin(rgb, 1), windows = windows)
}

## Reads the four-stain morphology image with Bio-Formats (RBioFormats, as the
## original Xenium demo does), writes the composite as a PNG and its calibration
## as JSON. The four morphology_focus_000*.ome.tif files describe one image and
## must stay together in one folder; Bio-Formats follows their cross-references.
##
## Only one pyramid level is decoded, never the full 34,154 x 23,912 px scan.
## The physical extent comes from the full-resolution size and the pixel size in
## the file, so it does not depend on the level chosen. Cells use
## (x_centroid, -y_centroid), so the picture's top row sits at y = 0.
.viewerXeniumPrimeMorphology <- function(
  ome,
  png_file,
  info_file,
  max_px = 4096L
) {
  .demoRequire(c("RBioFormats", "png", "jsonlite"), "Xenium Prime 5K")
  if (is.null(getOption("java.parameters"))) {
    options(java.parameters = "-Xmx4g")
  }
  stains <- .viewerXeniumPrimeStains()
  metadata <- RBioFormats::read.metadata(ome)
  sizes <- t(vapply(
    seq_along(metadata),
    function(level) {
      core <- RBioFormats::coreMetadata(metadata, series = level)
      c(core$sizeX, core$sizeY, core$sizeC)
    },
    numeric(3L)
  ))
  if (any(sizes[, 3L] != nrow(stains))) {
    stop(
      "The morphology image does not have the four expected stains.",
      call. = FALSE
    )
  }
  omexml <- RBioFormats::read.omexml(ome)
  found <- regmatches(
    omexml,
    gregexpr("<Channel [^>]*Name=\"[^\"]+\"", omexml)
  )[[1L]]
  names_found <- sub(".*Name=\"([^\"]+)\".*", "\\1", found)
  if (!identical(tolower(names_found), tolower(stains$name))) {
    stop(
      "Unexpected morphology stains: ",
      paste(names_found, collapse = ", "),
      call. = FALSE
    )
  }
  micron <- function(axis) {
    value <- as.numeric(sub(
      ".*PhysicalSize[XY]=\"([^\"]+)\".*",
      "\\1",
      regmatches(
        omexml,
        regexpr(sprintf("PhysicalSize%s=\"[^\"]+\"", axis), omexml)
      )
    ))
    unit <- sub(
      ".*Unit=\"([^\"]+)\".*",
      "\\1",
      regmatches(
        omexml,
        regexpr(sprintf("PhysicalSize%sUnit=\"[^\"]+\"", axis), omexml)
      )
    )
    if (!is.finite(value) || value <= 0 || !unit %in% c("\u00b5m", "um")) {
      stop(
        "The morphology pixel size must be a positive size in micrometres.",
        call. = FALSE
      )
    }
    value
  }
  pixel <- c(x = micron("X"), y = micron("Y"))
  level <- .viewerXeniumPrimeLevel(sizes[, 1:2, drop = FALSE], max_px)
  message(sprintf(
    "Decoding morphology level %d of %d (%d x %d px)...",
    level,
    nrow(sizes),
    sizes[level, 1L],
    sizes[level, 2L]
  ))
  image <- RBioFormats::read.image(ome, resolution = level, normalize = FALSE)
  planes <- as.array(image)
  ## Bio-Formats returns [x, y, stain]; the PNG wants [row = y, column = x].
  planes <- aperm(planes[,, seq_len(nrow(stains)), drop = FALSE], c(2L, 1L, 3L))
  composite <- .viewerXeniumPrimeCompose(planes, stains)
  png::writePNG(composite$rgb, png_file)
  full <- sizes[1L, 1:2]
  info <- list(
    sourceSize = unname(full),
    pixelSize = unname(pixel),
    displaySize = unname(sizes[level, 1:2]),
    bounds = list(
      xmin = 0,
      xmax = unname(full[[1L]] * pixel[["x"]]),
      ymin = unname(-full[[2L]] * pixel[["y"]]),
      ymax = 0
    ),
    channels = lapply(seq_len(nrow(stains)), function(stain) {
      list(
        name = stains$name[[stain]],
        color = stains$colour[[stain]],
        window = composite$windows[[stain]]
      )
    })
  )
  jsonlite::write_json(
    info,
    info_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA
  )
  invisible(info)
}

.viewerXeniumPrimeBuildObject <- function(paths, work, recipe) {
  files <- .viewerXeniumPrimeExtract(paths$archive, work)
  message("Reading the Prime 5K matrix and published analysis...")
  section <- .viewerXeniumPrimeRead(files, work)
  message("Preparing the four-stain morphology composite...")
  .viewerXeniumPrimeMorphology(
    files[["channel0"]],
    paths$image,
    paths$image_info
  )
  info <- jsonlite::fromJSON(paths$image_info)
  co <- section$coordinates
  b <- info$bounds
  if (any(co$x < b$xmin | co$x > b$xmax | co$y < b$ymin | co$y > b$ymax)) {
    stop("Prime 5K cells fall outside the microscopy extent.", call. = FALSE)
  }
  expression <- .demoLogNormalize(section$counts)
  message("Writing the Prime 5K H5 expression sidecar...")
  CerebroNexus:::.writeTENxExpression(
    methods::as(Matrix::t(expression), "CsparseMatrix"),
    paths$sidecar,
    group = "expression",
    level = 1L
  )
  object <- Cerebro$new()
  object$setVersion(utils::packageVersion("CerebroNexus"))
  object$addExperiment(
    "experiment_name",
    "10x Xenium Prime 5K mouse brain (coronal, FF)"
  )
  object$addExperiment("organism", "Mus musculus")
  object$addExperiment("date_of_export", as.character(Sys.Date()))
  object$addParameters("main_group", "cluster")
  object$addParameters(
    "source",
    "10x Genomics Xenium_Prime_Mouse_Brain_Coronal_FF, XOA 3.0.0"
  )
  object$setExpressionBackend("h5", basename(paths$sidecar))
  object$setMetaData(section$metadata)
  for (group in c("sample", "cluster")) {
    object$addGroup(group, levels(section$metadata[[group]]))
  }
  object$addProjection("umap", section$umap)
  ## The composite travels in the CRB (the embedded-image contract MERFISH
  ## uses): the Viewer serves external images only from inside the app, and
  ## serves an embedded one through a private per-session URL.
  png_uri <- paste0(
    "data:image/png;base64,",
    gsub(
      "\n",
      "",
      jsonlite::base64_enc(
        readBin(paths$image, "raw", file.info(paths$image)$size)
      ),
      fixed = TRUE
    )
  )
  object$addSpatialData(
    "coronal",
    list(
      coordinates = co,
      expression = Matrix::sparseMatrix(
        i = integer(),
        j = integer(),
        x = numeric(),
        dims = dim(expression),
        dimnames = list(rownames(expression), NULL)
      ),
      histology_images = list(
        "Four-stain morphology" = list(
          histology_image = png_uri,
          histology_image_bounds = unlist(b[c("xmin", "xmax", "ymin", "ymax")])
        )
      )
    )
  )
  object$addExtraTable(
    "data_provenance",
    data.frame(
      field = c(
        "source",
        "cells",
        "genes",
        "expression",
        "embedding",
        "microscopy",
        "coordinates"
      ),
      value = c(
        .viewerXeniumPrimeFiles()$outs$url,
        as.character(ncol(expression)),
        as.character(nrow(expression)),
        "Gene Expression features only; log1p of counts per 10,000 per cell",
        "Published 10x UMAP and graph clusters; omitted cells remain Unassigned with NA UMAP",
        "Four measured stains; fixed-colour, per-channel contrast-stretched PNG",
        "Micrometres; x = x_centroid, y = -y_centroid; native microscopy extent"
      )
    )
  )
  saveCerebro(object, paths$crb)
  .demoStampWrite(paths$stamp, recipe)
  paths$crb
}

.viewerXeniumPrimeComplete <- function(paths, recipe) {
  files <- unlist(
    paths[c("crb", "sidecar", "image", "image_info")],
    use.names = FALSE
  )
  all(file.exists(files) & !dir.exists(files)) &&
    .demoStampCurrent(paths$stamp, recipe)
}

prepareViewerXeniumPrimeDemoData <- function(cache_dir = NULL) {
  recipe <- .viewerXeniumPrimeRecipe
  paths <- .viewerXeniumPrimePaths(cache_dir)
  if (.viewerXeniumPrimeComplete(paths, recipe)) {
    return(paths$crb)
  }
  ## Checked before the 13 GB download, not after it.
  .demoRequire(
    c("Matrix", "rhdf5", "jsonlite", "RBioFormats", "png"),
    "Xenium Prime 5K"
  )
  message(
    "Preparing Prime 5K source (13.3 GB; only required members are unpacked)..."
  )
  .demoDownload(.viewerXeniumPrimeFiles()$outs, paths$archive)
  stage <- tempfile("prepared-stage-", paths$root)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  work <- file.path(stage, "work")
  dir.create(work)
  staged <- paths
  for (name in c("crb", "sidecar", "image", "image_info", "stamp")) {
    staged[[name]] <- file.path(stage, basename(paths[[name]]))
  }
  .viewerXeniumPrimeBuildObject(staged, work, recipe)
  unlink(work, recursive = TRUE)
  if (!.viewerXeniumPrimeComplete(staged, recipe)) {
    stop("Prime 5K preparation did not finish.", call. = FALSE)
  }
  .demoPublishStage(stage, dirname(paths$crb), paths$root, "Xenium Prime 5K")
  paths$crb
}

.viewerXeniumPrimeRecipe <- .demoRecipe(environment(), "XeniumPrime")
