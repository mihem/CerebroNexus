#' Convert a SingleCellExperiment into a Seurat object
#'
#' Gives a SingleCellExperiment (read from an `.rds`/`.qs2` file, or an AnnData
#' file opened with [readH5adAsSCE()]) the shape the Seurat-based tools expect,
#' so it can go through the Builder like any other dataset. Cells, genes, cell
#' metadata and every reduced dimension are carried over; so are the results kept
#' in `metadata(object)` (marker genes, immune repertoire, trajectories and
#' category colours), which the export reads from `@misc`.
#'
#' The expression matrix is placed by what it holds: a `counts` assay becomes
#' the `counts` layer and `logcounts` the `data` layer. Without those, the first
#' assay (an AnnData `X`) is used as `counts` when it holds whole numbers and as
#' `data` otherwise.
#'
#' Spatial coordinates (`reducedDim(object, "spatial")`) become one Seurat
#' spatial image (a FOV of cell centroids) per tissue section, named after the
#' section, so the Builder's spatial pages treat them like any other spatial
#' data set. Sections are told apart by the `library_id` column. Without one the
#' coordinates are kept as a single section. When the coordinates are the only
#' embedding, they are also kept as the projection \code{spatial_xy}, so there
#' is one to open the data set with.
#'
#' The tissue pictures in `metadata(object)$spatial_images` are kept per section
#' in `@misc$cerebro_source_images` (not in `cerebro_spatial_images`, which the
#' export would embed in the `.crb`). The Builder takes them from there and
#' attaches them to their sections as separate image files, placed at the extent
#' their scale factor gives.
#'
#' @param object A SingleCellExperiment.
#' @param main_assay Name of the Seurat assay to create.
#' @param extra_features Alternative experiments (\code{altExp}, for example
#'   antibody counts \code{"ADT"}) whose features are added to the assay as
#'   \code{<experiment>:<feature>}: their names, or \code{TRUE} for all.
#' @param verbose Print what was converted.
#' @return A Seurat object.
#' @export
convertSCEToSeurat <- function(
  object,
  main_assay = "RNA",
  extra_features = NULL,
  verbose = FALSE
) {
  if (!requireNamespace("SeuratObject", quietly = TRUE)) {
    stop(
      "The 'SeuratObject' package is needed for this conversion.",
      call. = FALSE
    )
  }
  if (!methods::is(object, "SingleCellExperiment")) {
    stop("`object` must be a SingleCellExperiment.", call. = FALSE)
  }
  assay_names <- SummarizedExperiment::assayNames(object)
  if (!length(assay_names)) {
    stop("The SingleCellExperiment has no assays.", call. = FALSE)
  }

  as_sparse <- function(x) {
    if (!methods::is(x, "dgCMatrix")) {
      x <- methods::as(
        methods::as(methods::as(x, "dMatrix"), "generalMatrix"),
        "CsparseMatrix"
      )
    }
    x
  }
  n_genes <- nrow(object)
  extra_names <- if (isTRUE(extra_features)) {
    SingleCellExperiment::altExpNames(object)
  } else {
    intersect(
      as.character(extra_features),
      SingleCellExperiment::altExpNames(object)
    )
  }
  fetch <- function(name) {
    m <- as_sparse(SummarizedExperiment::assay(object, name))
    dimnames(m) <- list(rownames(object), colnames(object))
    if (length(extra_names)) {
      m <- rbind(m, .sceAltExpRows(object, name, extra_names))
    }
    m
  }
  feature_names <- rownames(object)
  if (length(extra_names)) {
    feature_names <- c(
      feature_names,
      unlist(lapply(extra_names, function(name) {
        paste0(name, ":", rownames(SingleCellExperiment::altExp(object, name)))
      }))
    )
  }

  counts <- NULL
  data <- NULL
  if ("counts" %in% assay_names) {
    counts <- fetch("counts")
  }
  if ("logcounts" %in% assay_names) {
    data <- fetch("logcounts")
  }
  if (is.null(counts) && is.null(data)) {
    m <- fetch(assay_names[[1L]])
    sample <- if (length(m@x) > 100000L) m@x[seq_len(100000L)] else m@x
    if (length(sample) && all(sample == round(sample))) {
      counts <- m
    } else {
      data <- m
    }
  }
  if (anyNA(feature_names) || anyDuplicated(feature_names)) {
    stop(
      "Gene names must be present and unique to convert to Seurat.",
      call. = FALSE
    )
  }

  assay_object <- if (is.null(counts)) {
    SeuratObject::CreateAssay5Object(data = data)
  } else if (is.null(data)) {
    SeuratObject::CreateAssay5Object(counts = counts)
  } else {
    SeuratObject::CreateAssay5Object(counts = counts, data = data)
  }

  cell_data <- as.data.frame(SummarizedExperiment::colData(object))
  keep <- vapply(
    cell_data,
    function(column) is.atomic(column) || is.factor(column),
    logical(1)
  )
  cell_data <- cell_data[, keep, drop = FALSE]
  rownames(cell_data) <- colnames(object)

  result <- suppressWarnings(SeuratObject::CreateSeuratObject(
    counts = assay_object,
    assay = main_assay,
    meta.data = cell_data
  ))

  ## Seurat's constructors write "_" in a gene name as "-", but accept it once
  ## the assay exists. Put the file's own names back so a gene keeps the name it
  ## has in the source.
  seurat_features <- rownames(result)
  if (!identical(seurat_features, feature_names)) {
    restored <- result[[main_assay]]
    rownames(restored) <- feature_names
    result[[main_assay]] <- restored
  }

  ## The Builder and the export need a per-cell count and gene count. Seurat
  ## derives them from the counts layer, which a normalised-only file lacks, so
  ## use what the file recorded (scanpy: total_counts / n_genes_by_counts), or
  ## sum the matrix that is there.
  reference <- if (!is.null(counts)) {
    counts[seq_len(n_genes), , drop = FALSE]
  } else {
    data[seq_len(n_genes), , drop = FALSE]
  }
  fill <- function(target, candidates, from_matrix) {
    present <- result@meta.data[[target]]
    if (!is.null(present) && any(present > 0, na.rm = TRUE)) {
      return(invisible(NULL))
    }
    found <- intersect(candidates, colnames(cell_data))
    found <- found[vapply(
      found,
      function(n) is.numeric(cell_data[[n]]),
      logical(1)
    )]
    values <- if (length(found)) cell_data[[found[[1L]]]] else from_matrix()
    result[[target]] <<- as.numeric(values)
  }
  fill(
    paste0("nCount_", main_assay),
    c("total_counts", "n_counts", "nCount_RNA", "nUMI"),
    function() Matrix::colSums(reference)
  )
  fill(
    paste0("nFeature_", main_assay),
    c("n_genes_by_counts", "n_genes", "nFeature_RNA", "nGene"),
    function() Matrix::colSums(reference > 0)
  )

  used_keys <- character()
  for (name in SingleCellExperiment::reducedDimNames(object)) {
    embedding <- SingleCellExperiment::reducedDim(object, name)
    if (
      !is.matrix(embedding) || !is.numeric(embedding) || ncol(embedding) < 1L
    ) {
      next
    }
    key <- tolower(gsub("[^A-Za-z0-9]", "", name))
    if (!grepl("^[a-z]", key)) {
      key <- paste0("dim", key)
    }
    base <- key
    suffix <- 1L
    while (key %in% used_keys) {
      suffix <- suffix + 1L
      key <- paste0(base, suffix)
    }
    used_keys <- c(used_keys, key)
    storage.mode(embedding) <- "double"
    dimnames(embedding) <- list(
      colnames(object),
      paste0(key, "_", seq_len(ncol(embedding)))
    )
    result[[name]] <- SeuratObject::CreateDimReducObject(
      embeddings = embedding,
      key = paste0(key, "_"),
      assay = main_assay
    )
  }

  spatial <- .spatialSections(object)
  if (!is.null(spatial)) {
    ## the sections replace the plain coordinate reduction
    result@reductions[[spatial$reduction]] <- NULL
    ## a data set whose only embedding is its coordinates (SpatialData, MERFISH)
    ## still needs a projection to open with: keep them as "spatial_xy"
    if (!length(result@reductions)) {
      xy <- spatial$coordinates[, 1:2, drop = FALSE]
      dimnames(xy) <- list(colnames(object), c("spatialxy_1", "spatialxy_2"))
      storage.mode(xy) <- "double"
      result[["spatial_xy"]] <- SeuratObject::CreateDimReducObject(
        embeddings = xy,
        key = "spatialxy_",
        assay = main_assay
      )
    }
    sections <- list()
    for (library in names(spatial$cells)) {
      cells <- spatial$cells[[library]]
      coordinates <- data.frame(
        x = spatial$coordinates[cells, 1L],
        y = spatial$coordinates[cells, 2L],
        cell = cells
      )
      fov <- SeuratObject::CreateFOV(
        coordinates,
        type = "centroids",
        assay = main_assay,
        key = paste0("section", length(sections) + 1L, "_")
      )
      result[[library]] <- fov
      sections[[library]] <- spatial$images[[library]]
    }
    sections <- Filter(length, sections)
    if (length(sections)) {
      result@misc$cerebro_source_images <- sections
    }
  }

  carried <- as.list(object@metadata)
  carried$spatial_images <- NULL
  carried$spatial_sections <- NULL
  if (is.list(carried$trajectories)) {
    carried$trajectories <- .finiteTrajectories(carried$trajectories)
  }
  carried <- carried[nzchar(names(carried) %||% character(length(carried)))]
  result@misc <- utils::modifyList(result@misc, carried)

  if (verbose) {
    message(
      "[INFO] Converted ",
      ncol(object),
      " cells x ",
      nrow(object),
      " genes; layers: ",
      paste(
        c(if (!is.null(counts)) "counts", if (!is.null(data)) "data"),
        collapse = ", "
      ),
      "; reductions: ",
      paste(names(result@reductions), collapse = ", ")
    )
  }
  result
}

## The Builder checks every trajectory strictly; scVelo leaves pseudotime
## undefined for some cells, which the Viewer leaves out when it draws. Keep the
## cells with finite coordinates and pseudotime, and drop a trajectory that has
## none.
.finiteTrajectories <- function(trajectories) {
  trim <- function(payload) {
    meta <- payload[["meta"]]
    needed <- c("DR_1", "DR_2", "pseudotime")
    if (is.data.frame(meta) && all(needed %in% colnames(meta))) {
      keep <- stats::complete.cases(meta[, needed, drop = FALSE]) &
        is.finite(meta[["DR_1"]]) &
        is.finite(meta[["DR_2"]]) &
        is.finite(meta[["pseudotime"]])
      payload[["meta"]] <- meta[keep, , drop = FALSE]
    }
    payload
  }
  result <- lapply(trajectories, function(method) {
    kept <- lapply(method, trim)
    kept[vapply(
      kept,
      function(p) {
        !is.data.frame(p[["meta"]]) || nrow(p[["meta"]]) > 0L
      },
      logical(1)
    )]
  })
  result[lengths(result) > 0L]
}

## Tissue sections of an object: which cells belong to which library, their
## coordinates and the picture of each. NULL when there are no spatial
## coordinates, or when the sections cannot be told apart.
.spatialSections <- function(object) {
  reductions <- SingleCellExperiment::reducedDimNames(object)
  reduction <- reductions[tolower(reductions) == "spatial"][1L]
  if (is.na(reduction)) {
    return(NULL)
  }
  coordinates <- SingleCellExperiment::reducedDim(object, reduction)
  if (!is.matrix(coordinates) || ncol(coordinates) < 2L) {
    return(NULL)
  }
  rownames(coordinates) <- colnames(object)
  images <- object@metadata$spatial_images
  if (!is.list(images)) {
    images <- list()
  }
  cell_data <- SummarizedExperiment::colData(object)
  library_of <- if ("library_id" %in% colnames(cell_data)) {
    as.character(cell_data[["library_id"]])
  } else {
    NULL
  }
  if (
    !is.null(library_of) &&
      !anyNA(library_of) &&
      length(unique(library_of)) > 1L
  ) {
    cells <- split(
      colnames(object),
      factor(library_of, levels = unique(library_of))
    )
  } else if (length(images) > 1L) {
    message(
      "[INFO] The tissue images of ",
      length(images),
      " libraries cannot be assigned ",
      "to cells without a `library_id` column; the images were left out."
    )
    cells <- list(spatial = colnames(object))
    images <- list()
  } else {
    name <- if (length(images) == 1L) names(images) else "spatial"
    cells <- stats::setNames(list(colnames(object)), name)
  }
  if (any(!nzchar(names(cells)))) {
    return(NULL)
  }
  list(
    cells = cells,
    coordinates = coordinates,
    images = images,
    reduction = reduction
  )
}
