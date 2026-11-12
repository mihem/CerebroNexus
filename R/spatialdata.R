## SpatialData (.zarr) --------------------------------------------------------------
##
## A SpatialData store keeps its cell-by-gene table as an AnnData Zarr group
## under tables/<name>, and the spatial side next to it: shapes (spots or cell
## outlines, as parquet), points, images and the transformations that place each
## in a coordinate system. The table is read with the AnnData reader; this file
## adds the coordinates (from obsm["spatial"], or from the shapes the table
## annotates) and the tissue pictures that sit in a section's coordinate system.

#' 3x3 affine matrix of a list of NGFF coordinate transformations, acting on
#' (x, y, 1), for the transformations that matter here (identity, scale,
#' translation, a 2-D affine, and sequences of those). NULL for anything else.
#' @param transformations The `coordinateTransformations` list.
#' @param axes Axis names of the element, in the order the transformation uses.
#' @keywords internal
#' @noRd
.sdAffine <- function(transformations, axes) {
  result <- diag(3)
  if (!length(transformations)) {
    return(result)
  }
  position <- function(name) match(name, axes)
  for (transformation in transformations) {
    type <- as.character(transformation$type)
    step <- diag(3)
    if (identical(type, "identity")) {
      next
    } else if (identical(type, "scale")) {
      scale <- as.numeric(unlist(transformation$scale))
      sx <- scale[position("x")]
      sy <- scale[position("y")]
      step[1L, 1L] <- sx
      step[2L, 2L] <- sy
    } else if (identical(type, "translation")) {
      shift <- as.numeric(unlist(transformation$translation))
      step[1L, 3L] <- shift[position("x")]
      step[2L, 3L] <- shift[position("y")]
    } else if (identical(type, "sequence")) {
      inner <- .sdAffine(transformation$transformations, axes)
      if (is.null(inner)) {
        return(NULL)
      }
      step <- inner
    } else {
      return(NULL)
    }
    if (anyNA(step)) {
      return(NULL)
    }
    ## transformations in a list apply in order: the next one acts after this one
    result <- step %*% result
  }
  result
}

#' Centre of one geometry in WKB (point, polygon or multipolygon; the area-
#' weighted centroid of polygons).
#' @keywords internal
#' @noRd
.sdWkbCentre <- function(wkb) {
  position <- 1L
  read_int <- function(endian) {
    value <- readBin(
      wkb[position:(position + 3L)],
      "integer",
      size = 4L,
      endian = endian
    )
    position <<- position + 4L
    value
  }
  read_doubles <- function(n, endian) {
    value <- readBin(
      wkb[position:(position + 8L * n - 1L)],
      "double",
      n = n,
      size = 8L,
      endian = endian
    )
    position <<- position + 8L * n
    value
  }
  read_geometry <- function() {
    endian <- if (wkb[[position]] == 1L) "little" else "big"
    position <<- position + 1L
    type <- read_int(endian) %% 1000L
    if (type == 1L) {
      xy <- read_doubles(2L, endian)
      return(list(list(area = 0, x = xy[[1L]], y = xy[[2L]])))
    }
    if (type == 3L) {
      rings <- read_int(endian)
      pieces <- list()
      for (ring in seq_len(rings)) {
        n <- read_int(endian)
        coordinates <- matrix(
          read_doubles(2L * n, endian),
          ncol = 2L,
          byrow = TRUE
        )
        if (ring > 1L) {
          next
        }
        x <- coordinates[, 1L]
        y <- coordinates[, 2L]
        j <- c(seq_len(n)[-1L], 1L)
        cross <- x * y[j] - x[j] * y
        area <- sum(cross) / 2
        pieces[[1L]] <- if (abs(area) > 0) {
          list(
            area = abs(area),
            x = sum((x + x[j]) * cross) / (6 * area),
            y = sum((y + y[j]) * cross) / (6 * area)
          )
        } else {
          list(area = 1e-12, x = mean(x), y = mean(y))
        }
      }
      return(pieces)
    }
    if (type %in% c(4L, 5L, 6L, 7L)) {
      count <- read_int(endian)
      return(do.call(c, lapply(seq_len(count), function(i) read_geometry())))
    }
    stop("Unsupported WKB geometry type ", type, ".", call. = FALSE)
  }
  pieces <- read_geometry()
  weights <- vapply(pieces, function(p) p$area, numeric(1))
  if (sum(weights) <= 0) {
    weights <- rep(1, length(pieces))
  }
  c(
    x = sum(vapply(pieces, function(p) p$x, numeric(1)) * weights) /
      sum(weights),
    y = sum(vapply(pieces, function(p) p$y, numeric(1)) * weights) /
      sum(weights)
  )
}

#' The x and y of every geometry when all of them are little-endian 2-d WKB
#' points (21 bytes: byte order, type 1, two doubles), read in one pass; NULL
#' for anything else (polygons and the like go through .sdWkbCentre()).
#' @keywords internal
#' @noRd
.sdPointCentres <- function(geometry) {
  if (!length(geometry) || !all(lengths(geometry) == 21L)) {
    return(NULL)
  }
  bytes <- matrix(unlist(geometry, use.names = FALSE), nrow = 21L)
  header <- c(1L, 1L, 0L, 0L, 0L)
  if (!all(bytes[1:5, , drop = FALSE] == as.raw(header))) {
    return(NULL)
  }
  rbind(
    readBin(
      as.vector(bytes[6:13, ]),
      "double",
      n = ncol(bytes),
      endian = "little"
    ),
    readBin(
      as.vector(bytes[14:21, ]),
      "double",
      n = ncol(bytes),
      endian = "little"
    )
  )
}

#' Coordinates of a shapes element: a data frame of x, y and the element's index
#' column (when it has one).
#' @keywords internal
#' @noRd
.sdShapeCentres <- function(directory) {
  file <- file.path(directory, "shapes.parquet")
  if (!file.exists(file)) {
    stop("Shapes element without shapes.parquet: ", directory, call. = FALSE)
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "The 'arrow' package is needed to read the shapes of a SpatialData store.",
      call. = FALSE
    )
  }
  table <- as.data.frame(arrow::read_parquet(file))
  centres <- .sdPointCentres(table$geometry)
  if (is.null(centres)) {
    centres <- vapply(table$geometry, .sdWkbCentre, numeric(2))
  }
  result <- data.frame(x = centres[1L, ], y = centres[2L, ])
  extra <- setdiff(names(table), c("geometry", "radius"))
  for (name in extra) {
    result[[name]] <- table[[name]]
  }
  result
}

#' @keywords internal
#' @noRd
.sdTables <- function(path) {
  tables <- file.path(path, "tables")
  if (!dir.exists(tables)) {
    return(character())
  }
  entries <- list.dirs(tables, recursive = FALSE, full.names = FALSE)
  entries[
    file.exists(file.path(tables, entries, "zarr.json")) |
      file.exists(file.path(tables, entries, ".zgroup"))
  ]
}

#' Pictures that sit in a section's coordinate system, as the payload the
#' converters embed: library -> label -> list(histology_image,
#' histology_image_bounds, image_label). Bounds are in the section's shape
#' coordinates, which is the space of the cell coordinates.
#' @keywords internal
#' @noRd
.sdImages <- function(
  path,
  regions,
  shape_meta,
  shape_region,
  which = "auto",
  verbose = FALSE
) {
  images_dir <- file.path(path, "images")
  if (isFALSE(which) || !dir.exists(images_dir)) {
    return(list())
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    message(
      "[INFO] The 'png' package is needed to embed tissue images; they were skipped."
    )
    return(list())
  }
  result <- list()
  for (name in list.dirs(images_dir, recursive = FALSE, full.names = FALSE)) {
    directory <- file.path(images_dir, name)
    node <- tryCatch(.zarrNode(directory), error = function(e) NULL)
    ome <- node$attributes$ome %||% node$attributes
    scale <- ome$multiscales[[1L]]
    if (is.null(scale)) {
      next
    }
    axes <- vapply(scale$axes, function(a) as.character(a$name), character(1))
    ## the coordinate system the picture is placed in
    outer <- scale$coordinateTransformations
    target <- if (length(outer)) {
      as.character(outer[[1L]]$output$name %||% "")
    } else {
      ""
    }
    region <- if (target %in% names(shape_region)) {
      shape_region[[target]]
    } else {
      NA_character_
    }
    if (is.na(region) || !region %in% regions) {
      next
    }
    labels <- vapply(
      ome$omero$channels %||% list(),
      function(c) as.character(c$label),
      character(1)
    )
    level <- scale$datasets[[1L]]
    level_path <- file.path(directory, as.character(level$path))
    array_node <- tryCatch(.zarrNode(level_path), error = function(e) NULL)
    shape <- as.integer(unlist(array_node$meta$shape))
    if (
      is.null(array_node) ||
        length(shape) != 3L ||
        shape[[match("c", axes)]] != 3L
    ) {
      next
    }
    pixels <- .zarrReadArray(level_path, array_node)
    ## dim (x, y, c) -> (y, x, c), the picture as rows then columns
    order <- match(c("y", "x", "c"), rev(axes))
    pixels <- aperm(pixels, order)
    storage.mode(pixels) <- "double"
    top <- suppressWarnings(max(pixels, na.rm = TRUE))
    if (is.finite(top) && top > 1) {
      pixels <- pixels / 255
    }
    pixels[pixels < 0] <- 0
    pixels[pixels > 1] <- 1
    height <- dim(pixels)[[1L]]
    width <- dim(pixels)[[2L]]

    to_system <- .sdAffine(level$coordinateTransformations, axes)
    placed <- .sdAffine(outer, axes)
    shape_to_system <- shape_meta[[target]]
    if (is.null(to_system) || is.null(placed) || is.null(shape_to_system)) {
      next
    }
    corners <- rbind(c(0, 0, 1), c(width, height, 1))
    mapped <- t(solve(shape_to_system) %*% placed %*% to_system %*% t(corners))
    bounds <- c(
      xmin = min(mapped[, 1L]),
      xmax = max(mapped[, 1L]),
      ymin = min(mapped[, 2L]),
      ymax = max(mapped[, 2L])
    )
    label <- if (grepl("hires", name, ignore.case = TRUE)) {
      "hires"
    } else if (grepl("lowres", name, ignore.case = TRUE)) {
      "lowres"
    } else {
      name
    }
    result[[region]][[label]] <- list(
      histology_image = .h5adImageDataUri(pixels),
      histology_image_bounds = bounds,
      image_label = label
    )
    if (verbose) {
      message("[INFO] Picture `", name, "` placed for `", region, "`.")
    }
  }
  if (identical(which, "auto")) {
    result <- lapply(result, function(libraries) {
      libraries[intersect(
        c("hires", "lowres", names(libraries)),
        names(libraries)
      )[1L]]
    })
  } else if (is.character(which)) {
    result <- lapply(result, function(libraries) {
      libraries[intersect(which, names(libraries))]
    })
    result <- Filter(length, result)
  }
  result
}

#' A zip archive, by its first bytes, for a store zipped under another name.
#' @keywords internal
#' @noRd
.sdIsZip <- function(path) {
  head <- tryCatch(readBin(path, "raw", n = 4L), error = function(e) raw())
  length(head) == 4L &&
    (identical(head, as.raw(c(0x50, 0x4b, 0x03, 0x04))) ||
      identical(head, as.raw(c(0x50, 0x4b, 0x05, 0x06))))
}

#' Unpack a .zip holding a SpatialData store into a temporary directory.
#' @return list(directory = the temporary directory, store = the .zarr inside).
#' @keywords internal
#' @noRd
.sdUnzip <- function(zip) {
  entries <- utils::unzip(zip, list = TRUE)$Name
  if (
    !length(entries) || any(grepl("(^|/)\\.\\.(/|$)|^/|^[A-Za-z]:", entries))
  ) {
    stop("`", zip, "` is not a safe zip of a SpatialData store.", call. = FALSE)
  }
  directory <- tempfile("spatialdata-")
  dir.create(directory)
  utils::unzip(zip, exdir = directory)
  markers <- list.files(
    directory,
    pattern = "^zarr\\.json$|^\\.zgroup$",
    recursive = TRUE,
    all.files = TRUE
  )
  roots <- unique(dirname(markers[
    basename(markers) %in% c("zarr.json", ".zgroup")
  ]))
  stores <- roots[vapply(
    roots,
    function(r) dir.exists(file.path(directory, r, "tables")),
    logical(1)
  )]
  if (!length(stores)) {
    unlink(directory, recursive = TRUE, force = TRUE)
    stop(
      "`",
      zip,
      "` holds no SpatialData store (no tables/ group).",
      call. = FALSE
    )
  }
  store <- stores[order(nchar(stores))][[1L]]
  list(
    directory = directory,
    store = normalizePath(file.path(directory, store), winslash = "/")
  )
}

#' Read a SpatialData store as a SingleCellExperiment
#'
#' @description
#' Reads the cell-by-gene table of a SpatialData \code{.zarr} store natively
#' (no Python), with the spatial coordinates of its cells and the tissue
#' pictures that sit in each section's coordinate system, so the result goes to
#' \code{\link{convertSCEToCerebro}} like any other SingleCellExperiment.
#'
#' @details
#' The table is an AnnData and is read as described in
#' \code{\link{readH5adAsSCE}}. Coordinates come from \code{obsm["spatial"]}
#' when the table has them (Visium); otherwise from the shapes element the table
#' annotates (MERFISH, Xenium and other cell-based technologies: the centre of
#' each shape, matched by the table's instance key). Coordinates are in the
#' units of the shapes element. The section of a cell is the table's region
#' column, copied to \code{colData$library_id}.
#'
#' Pictures with three channels are attached to the section whose coordinate
#' system they are placed in, as \code{metadata(sce)$spatial_images}, with their
#' extent worked out from the transformations. Points, labels, other shapes and
#' single-channel images are not read.
#'
#' Zarr v2 and v3 stores are read (including sharded arrays) with zstd, gzip,
#' Blosc (lz4, zstd or zlib) or no compression; any other codec stops with its
#' name. Blosc chunks are decoded in R, lz4 (the numcodecs default) at about
#' 4 MB/s of output: re-save a large store with zstd, or use an \code{.h5ad}
#' file.
#'
#' @param path A SpatialData \code{.zarr} directory, or a \code{.zip} of one (as the
#'   SpatialData sample datasets are distributed).
#' @param table Name of the table under \code{tables/}; by default \code{"table"}
#'   or the only one.
#' @param spatial_images \code{"auto"} (hires where there is one, otherwise
#'   lowres), a vector of image labels, or \code{FALSE} for none.
#' @param verbose Print what was read.
#' @return A SingleCellExperiment.
#' @seealso \code{\link{convertSpatialDataToCerebro}}, \code{\link{readH5adAsSCE}}
#' @examplesIf nzchar(system.file(package = "SingleCellExperiment")) && nzchar(system.file(package = "arrow"))
#' \donttest{
#' ## A small SpatialData store (50 spots), zipped
#' sce <- readSpatialDataAsSCE(
#'   system.file("extdata/examples/small_spatialdata.zip", package = "CerebroNexus")
#' )
#' sce
#' }
#' @export
readSpatialDataAsSCE <- function(
  path,
  table = NULL,
  spatial_images = "auto",
  verbose = FALSE
) {
  if (
    is.character(path) &&
      length(path) == 1L &&
      file.exists(path) &&
      !dir.exists(path) &&
      (identical(tolower(tools::file_ext(path)), "zip") || .sdIsZip(path))
  ) {
    unpacked <- .sdUnzip(path)
    on.exit(
      unlink(unpacked$directory, recursive = TRUE, force = TRUE),
      add = TRUE
    )
    path <- unpacked$store
  }
  if (!.zarrIsStore(path)) {
    stop("`path` must be a SpatialData .zarr directory.", call. = FALSE)
  }
  tables <- .sdTables(path)
  if (!length(tables)) {
    stop(
      "`",
      path,
      "` has no tables/ group; is it a SpatialData store?",
      call. = FALSE
    )
  }
  if (is.null(table)) {
    table <- if ("table" %in% tables) {
      "table"
    } else if (length(tables) == 1L) {
      tables
    } else {
      NULL
    }
    if (is.null(table)) {
      stop(
        "The store has several tables (",
        paste(tables, collapse = ", "),
        "); choose one with `table =`.",
        call. = FALSE
      )
    }
  }
  if (!table %in% tables) {
    stop(
      "Table `",
      table,
      "` not found; the store has: ",
      paste(tables, collapse = ", "),
      call. = FALSE
    )
  }
  root <- paste0("tables/", table)
  sce <- readH5adAsSCE(path, .root = root, verbose = verbose)
  ctx <- .zarrContext(path, root)
  attrs <- .zarrAttrs(ctx, "")
  spatial_attrs <- attrs$spatialdata_attrs %||% list()
  region_key <- attrs$region_key %||% spatial_attrs$region_key
  instance_key <- attrs$instance_key %||% spatial_attrs$instance_key
  regions <- as.character(unlist(attrs$region %||% spatial_attrs$region))
  cell_data <- SummarizedExperiment::colData(sce)

  cell_region <- if (
    !is.null(region_key) && region_key %in% colnames(cell_data)
  ) {
    as.character(cell_data[[region_key]])
  } else {
    rep(if (length(regions)) regions[[1L]] else NA_character_, ncol(sce))
  }
  if (!length(regions)) {
    regions <- unique(cell_region)
  }

  ## shapes elements of the regions: where they sit in their coordinate system
  shape_meta <- list()
  shape_region <- list()
  for (region in regions) {
    directory <- file.path(path, "shapes", region)
    node <- tryCatch(.zarrNode(directory), error = function(e) NULL)
    if (is.null(node)) {
      next
    }
    transformations <- node$attributes$coordinateTransformations
    system <- as.character(transformations[[1L]]$output$name %||% "")
    axes <- as.character(unlist(node$attributes$axes %||% c("x", "y")))
    affine <- .sdAffine(transformations, axes)
    if (nzchar(system) && !is.null(affine)) {
      shape_meta[[system]] <- affine
      shape_region[[system]] <- region
    }
  }

  ## coordinates
  coordinates <- if (
    "spatial" %in% SingleCellExperiment::reducedDimNames(sce)
  ) {
    SingleCellExperiment::reducedDim(sce, "spatial")
  } else {
    NULL
  }
  if (is.null(coordinates)) {
    matrix_xy <- matrix(NA_real_, nrow = ncol(sce), ncol = 2L)
    for (region in unique(cell_region[!is.na(cell_region)])) {
      directory <- file.path(path, "shapes", region)
      if (!dir.exists(directory)) {
        next
      }
      centres <- .sdShapeCentres(directory)
      rows <- which(cell_region == region)
      key <- if (
        !is.null(instance_key) && instance_key %in% colnames(cell_data)
      ) {
        cell_data[[instance_key]][rows]
      } else {
        seq_along(rows) - 1L
      }
      index <- if (!is.null(instance_key) && instance_key %in% names(centres)) {
        match(as.character(key), as.character(centres[[instance_key]]))
      } else {
        match(as.integer(key), seq_len(nrow(centres)) - 1L)
      }
      matrix_xy[rows, ] <- as.matrix(centres[index, c("x", "y")])
    }
    if (all(is.na(matrix_xy))) {
      coordinates <- NULL
      message(
        "[INFO] No spatial coordinates found for the cells of `",
        table,
        "`."
      )
    } else {
      dimnames(matrix_xy) <- list(colnames(sce), c("x", "y"))
      coordinates <- matrix_xy
    }
  }
  if (!is.null(coordinates)) {
    SingleCellExperiment::reducedDim(sce, "spatial") <- coordinates
  }

  if (
    length(unique(cell_region[!is.na(cell_region)])) > 1L ||
      length(regions) > 1L
  ) {
    SummarizedExperiment::colData(sce)$library_id <- cell_region
  }
  images <- .sdImages(
    path,
    regions,
    shape_meta,
    shape_region,
    spatial_images,
    verbose
  )
  if (length(images)) {
    sce@metadata$spatial_images <- images
  }
  sce
}

#' Convert a SpatialData store to a Cerebro object
#'
#' Reads the store with \code{\link{readSpatialDataAsSCE}} and exports it with
#' \code{\link{convertSCEToCerebro}}: one spatial section per region (with its
#' tissue picture, where the store has one), expression, cell data and any
#' embeddings, markers and colours the table carries.
#'
#' @param path A SpatialData \code{.zarr} directory.
#' @param result_dir Directory the \code{.crb} is written to.
#' @param table,spatial_images See \code{\link{readSpatialDataAsSCE}}.
#' @param assay Which matrix to export; default \code{"X"}.
#' @param ... Further arguments of \code{\link{convertSCEToCerebro}} (for example
#'   \code{groups}, \code{organism}, \code{expression_matrix_mode}).
#' @return The path of the \code{.crb}, invisibly.
#' @seealso \code{\link{readSpatialDataAsSCE}}
#' @examplesIf nzchar(system.file(package = "SingleCellExperiment")) && nzchar(system.file(package = "arrow"))
#' \donttest{
#' dir <- tempfile("cerebro-")
#' dir.create(dir)
#' convertSpatialDataToCerebro(
#'   system.file("extdata/examples/small_spatialdata.zip", package = "CerebroNexus"),
#'   result_dir = dir,
#'   groups = "cell_type",
#'   verbose = FALSE
#' )
#' list.files(dir)
#' unlink(dir, recursive = TRUE)
#' }
#' @export
convertSpatialDataToCerebro <- function(
  path,
  result_dir,
  table = NULL,
  spatial_images = "auto",
  assay = "X",
  ...
) {
  sce <- readSpatialDataAsSCE(
    path,
    table = table,
    spatial_images = spatial_images
  )
  arguments <- list(...)
  if (is.null(arguments$nUMI) || is.null(arguments$nGene)) {
    columns <- names(SummarizedExperiment::colData(sce))
    arguments$nUMI <- arguments$nUMI %||%
      .h5adQcColumn(
        columns,
        c("total_counts", "n_counts", "nCount_RNA", "nUMI"),
        "nUMI"
      )
    arguments$nGene <- arguments$nGene %||%
      .h5adQcColumn(
        columns,
        c("n_genes_by_counts", "n_genes", "nFeature_RNA", "nGene"),
        "nGene"
      )
  }
  do.call(
    convertSCEToCerebro,
    c(
      list(
        sce_file = sce,
        result_dir = result_dir,
        assay = assay,
        .source_name = basename(normalizePath(path, winslash = "/"))
      ),
      arguments
    )
  )
}
