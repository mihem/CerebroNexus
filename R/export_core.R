## Format-independent export core -------------------------------------------
##
## Every exportFromXxx() adapter reduces its native object to plain R data (a
## cell meta data.frame, a `misc` list, cell barcodes, a genes x cells matrix,
## named embeddings) and hands it to these helpers. The helpers know nothing
## about Seurat, SingleCellExperiment or AnnData, so a new input format only
## needs a thin adapter and inherits the external expression backends, the
## transactional publish and the Viewer Pack from here.

## The parts of one export, in order, as named to an `.on_progress` callback
## (the Builder counts them for its Build progress). A part that does not
## apply, such as a Viewer Pack for a small dataset, is not reported.
.exportProgressParts <- c(
  "reading expression",
  "writing expression",
  "adding metadata and results",
  "adding spatial data",
  "saving the CRB file",
  "writing the Viewer Pack"
)

.exportReportPart <- function(on_progress, part) {
  if (is.function(on_progress)) {
    on_progress(match.arg(part, .exportProgressParts))
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.exportCheckOutputArgs <- function(
  file,
  expression_matrix_mode,
  use_delayed_array,
  verbose
) {
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(file)
  ) {
    stop("`file` must be one non-empty output path.", call. = FALSE)
  }
  if (dir.exists(file)) {
    stop("`file` points to a directory, not a .crb path: ", file, call. = FALSE)
  }
  if (!identical(expression_matrix_mode, "embedded")) {
    if (!identical(tolower(tools::file_ext(file)), "crb")) {
      stop(
        "External expression backends require `file` to end in .crb so the ",
        "Cerebro object and its sidecar have distinct portable names. Received: ",
        file,
        call. = FALSE
      )
    }
    .validatePortableExportBasename(file)
  }
  if (
    expression_matrix_mode == "h5" &&
      !requireNamespace("HDF5Array", quietly = TRUE)
  ) {
    stop(
      "expression_matrix_mode = \"h5\" requires the HDF5Array package. ",
      "Install it via BiocManager::install(\"HDF5Array\") and re-run, or ",
      "switch to expression_matrix_mode = \"embedded\".",
      call. = FALSE
    )
  }
  if (expression_matrix_mode != "embedded" && use_delayed_array) {
    if (verbose) {
      message(
        "expression_matrix_mode = \"",
        expression_matrix_mode,
        "\" supersedes use_delayed_array; the RleArray conversion is skipped."
      )
    }
  }
}

#' @keywords internal
#' @noRd
.exportInit <- function(
  experiment_name,
  organism,
  misc,
  verbose
) {
  .logStep(
    if (verbose) {
      'Initializing Cerebro object...'
    } else {
      'Start collecting data...'
    }
  )

  ## create new Cerebro object
  export <- Cerebro$new()

  ## add experiment name
  export$addExperiment('experiment_name', experiment_name)
  # Preserve optional overview prose exactly when supplied, never manufacture it.
  for (field in c("description", "source", "sample_column")) {
    value <- misc$experiment[[field]]
    if (
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(value))
    ) {
      export$addExperiment(field, value)
    }
  }

  ## add organism
  export$addExperiment('organism', organism)

  ## add cerebroApp version
  export$setVersion(package_version(.cerebroRuntimeVersion()))

  export
}

#' Write a matrix as a TENxMatrix-format group. A `dgCMatrix` already holds the
#' three arrays of that format, so they are written as they are, into the
#' datasets HDF5Array creates for its own writer; `writeTENxMatrix()` would cut
#' the matrix into blocks first, which takes several times as long as the
#' writing. Any other matrix goes through `writeTENxMatrix()`.
#' @keywords internal
#' @noRd
.writeTENxExpression <- function(
  x,
  path,
  group,
  level = NULL,
  slice = 2^24
) {
  if (!inherits(x, "dgCMatrix")) {
    HDF5Array::writeTENxMatrix(x, path, group = group, level = level)
    return(invisible(path))
  }
  ## Creates the group, its shape and names, and the empty, extensible `data`,
  ## `indices` and `indptr` datasets (rhdf5 comes with HDF5Array).
  HDF5Array::TENxRealizationSink(
    dim(x),
    dimnames(x),
    "double",
    filepath = path,
    group = group,
    level = level
  )
  write <- function(name, values, n) {
    dataset <- paste0(group, "/", name)
    rhdf5::h5set_extent(path, dataset, n)
    for (start in seq(1, n, by = slice)) {
      end <- min(start + slice - 1, n)
      rhdf5::h5write(
        values(start, end),
        path,
        dataset,
        start = start,
        count = end - start + 1
      )
    }
  }
  n <- length(x@x)
  if (n > 0) {
    write("data", function(start, end) x@x[start:end], n)
    write("indices", function(start, end) x@i[start:end], n)
  }
  write(
    "indptr",
    function(start, end) as.double(x@p[start:end]),
    length(x@p)
  )
  invisible(path)
}

#' Persist the genes x cells matrix (embedded / h5). Returns the
#' matrix as it stands afterwards, because the spatial block reuses it.
#' @keywords internal
#' @noRd
.exportWriteExpression <- function(
  export,
  expression_data,
  expression_matrix_mode,
  use_delayed_array,
  export_stage_dir,
  final_file,
  verbose,
  h5_compression_level = NULL
) {
  if (expression_matrix_mode == "embedded") {
    expression_data <- .delayedExportExpression(
      expression_data,
      use_delayed_array,
      verbose
    )
    .logStep('Adding expression data (embedded)...')
    export$setExpression(expression_data)
  } else if (expression_matrix_mode == "h5") {
    ## Write the expression matrix to a TENxMatrix-format sparse HDF5 file
    ## sitting next to the target .crb. The on-disk orientation is cells x
    ## genes — TENx CSC stores columns contiguously, so the per-gene reads
    ## that Cerebro does at runtime become single-column lookups. Cerebro's
    ## internal layout is genes x cells, so the runtime attach lazily
    ## transposes the TENxMatrix seed back via DelayedArray::t() (free).
    crb_dir <- export_stage_dir
    h5_filename <- .exportSidecarName(final_file, expression_matrix_mode)
    h5_abs <- file.path(crb_dir, h5_filename)

    if (!inherits(expression_data, "dgCMatrix")) {
      if (inherits(expression_data, "matrix")) {
        expression_data <- methods::as(expression_data, "CsparseMatrix")
      }
    }

    if (file.exists(h5_abs)) {
      file.remove(h5_abs)
    }
    ## Hashing the matrix pays only for a caller that keeps sidecars to
    ## reuse (the Builder sets the option, if only to an empty list).
    content_digest <- if (!is.null(getOption("cerebro.expression_sidecars"))) {
      .expressionContentDigest(expression_data, h5_compression_level)
    }
    ## An earlier sidecar written from this very matrix is taken as it is.
    h5_md5 <- if (!is.null(content_digest)) {
      .takeExpressionSidecar(
        getOption("cerebro.expression_sidecars"),
        content_digest,
        h5_abs
      )
    }

    if (is.null(h5_md5)) {
      ## transpose genes x cells -> cells x genes for storage
      m_disk <- if (
        inherits(expression_data, c("RleMatrix", "DelayedMatrix"))
      ) {
        DelayedArray::t(expression_data)
      } else {
        methods::as(Matrix::t(expression_data), "CsparseMatrix")
      }

      if (verbose) {
        .logStep("Writing expression matrix to TENx HDF5 file: ", h5_abs)
      }

      ## A level of 0 stores the data without compression (the shuffle and
      ## deflate filters are still applied): on 1.3-1.5M-cell sources single
      ## genes read ~30% faster than at HDF5Array's default level 6, at roughly
      ## 7x the file size. NULL leaves HDF5Array's default.
      .writeTENxExpression(
        m_disk,
        h5_abs,
        group = "expression",
        level = h5_compression_level
      )
      if (!is.null(content_digest)) {
        .tagExpressionSidecar(h5_abs, content_digest)
      }
      h5_md5 <- unname(tools::md5sum(h5_abs))
    } else if (verbose) {
      .logStep("Reusing the expression matrix of an earlier export: ", h5_abs)
    }

    ## self$expression stays NULL — saveRDS therefore does not embed the
    ## matrix inside the .crb. The runtime attach reads the sibling back
    ## as a lazy TENxMatrix seed (no in-memory dgCMatrix materialisation).
    export$setExpressionBackend(
      type = "h5",
      location = h5_filename,
      md5 = h5_md5
    )
  }

  expression_data
}

## Reusing an expression sidecar --------------------------------------------
## An HDF5 sidecar holds the matrix and nothing that depends on how a dataset
## is configured, so exporting the same data again after a change of groups or
## metadata would write the same bytes. The sidecar carries a digest of the
## matrix it was written from (an attribute of its "expression" group); an
## export that is offered earlier sidecars (the option
## `cerebro.expression_sidecars`: a list of `list(path, md5)`) copies one when
## the file still has the MD5 it was recorded with and its digest is that of
## the matrix to write. Without that option (every plain `exportFromSeurat()`)
## the matrix is not hashed and the sidecar is written untagged. The Builder
## sets it in every process that exports, an empty list when there is nothing
## to offer yet, so that its sidecars carry the digest for the next time.

#' Bump when the way a sidecar is written changes its bytes, so that a sidecar
#' of the older way is not taken.
#' @keywords internal
#' @noRd
.expressionSidecarWriter <- 1L

#' Digest of a `dgCMatrix` and of how it is written; `NULL` for other classes.
#' @keywords internal
#' @noRd
.expressionContentDigest <- function(x, level = NULL) {
  if (!inherits(x, "dgCMatrix")) {
    return(NULL)
  }
  rlang::hash(list(
    writer = .expressionSidecarWriter,
    level = if (is.null(level)) NA_integer_ else as.integer(level),
    dim = dim(x),
    dimnames = dimnames(x),
    p = x@p,
    i = x@i,
    x = x@x
  ))
}

#' @keywords internal
#' @noRd
.tagExpressionSidecar <- function(path, digest) {
  tryCatch(
    {
      file <- rhdf5::H5Fopen(path)
      on.exit(rhdf5::H5Fclose(file), add = TRUE)
      group <- rhdf5::H5Gopen(file, "expression")
      on.exit(rhdf5::H5Gclose(group), add = TRUE, after = FALSE)
      rhdf5::h5writeAttribute(digest, group, "content_digest")
      TRUE
    },
    error = function(error) FALSE
  )
}

.expressionSidecarHasDigest <- function(path, digest) {
  tryCatch(
    identical(
      as.character(rhdf5::h5readAttributes(path, "expression")$content_digest),
      digest
    ),
    error = function(error) FALSE
  )
}

#' Copies the first candidate sidecar that still has its recorded MD5 and was
#' written from the matrix with `digest` to `target`. Returns the MD5 of the
#' copy, or `NULL` (leaving no `target`) when none qualifies.
#' @keywords internal
#' @noRd
.takeExpressionSidecar <- function(candidates, digest, target) {
  for (candidate in candidates) {
    if (!is.list(candidate)) {
      next
    }
    path <- candidate$path
    md5 <- candidate$md5
    if (
      !is.character(path) ||
        length(path) != 1L ||
        is.na(path) ||
        !file.exists(path) ||
        dir.exists(path) ||
        !is.character(md5) ||
        length(md5) != 1L ||
        !grepl("^[[:xdigit:]]{32}$", md5)
    ) {
      next
    }
    ## The digest is an attribute: reading it costs nothing, copying a sidecar
    ## of a matrix that has changed (a filter, say) costs gigabytes.
    if (!.expressionSidecarHasDigest(path, digest)) {
      next
    }
    copied <- isTRUE(file.copy(path, target, overwrite = TRUE))
    taken <- copied &&
      tryCatch(
        identical(tolower(unname(tools::md5sum(target))), tolower(md5)) &&
          .expressionSidecarHasDigest(target, digest),
        error = function(error) FALSE
      )
    if (isTRUE(taken)) {
      return(tolower(md5))
    }
    unlink(target)
  }
  NULL
}

#' Trees, most expressed genes, mean expression, immune repertoire, HLA,
#' markers, pathways, trajectories, extra material and Trekker, all read from
#' a plain `misc` list.
#' @keywords internal
#' @noRd
.exportAddMiscResults <- function(
  export,
  misc,
  groups,
  cells,
  verbose,
  where = "@metadata",
  supported_note = NULL
) {
  .addExportGroupTables(
    misc$trees,
    paste0(where, '$trees'),
    'Extracting trees...',
    export$addTree,
    verbose
  )
  .addExportGroupTables(
    misc$most_expressed_genes,
    paste0(where, '$most_expressed_genes'),
    'Extracting tables of most expressed genes...',
    export$addMostExpressedGenes,
    verbose,
    groups
  )
  .addExportGroupTables(
    misc$mean_expression,
    paste0(where, '$mean_expression'),
    'Extracting tables of mean expression...',
    export$addMeanExpression,
    verbose,
    groups
  )

  .addExportImmune(export, misc, cells, where, verbose)

  ## marker_genes is a nested list: list(method = list(group = data.frame))
  .addExportMethodTables(
    misc$marker_genes,
    paste0(where, '$marker_genes'),
    'Extracting marker genes table...',
    export$addMarkerGenes,
    verbose
  )
  .addExportMethodTables(
    misc$enriched_pathways,
    paste0(where, '$enriched_pathways'),
    'Extracting pathway enrichment results...',
    export$addEnrichedPathways,
    verbose,
    groups
  )
  .addExportTrajectories(
    export,
    misc$trajectories,
    verbose,
    supported_note = supported_note
  )
  .addExportExtraMaterial(export, misc$extra_material, verbose)

  ##--------------------------------------------------------------------------##
  ## Trekker single-cell spatial mapping
  ##--------------------------------------------------------------------------##
  if (!is.null(misc$trekker)) {
    export$addTrekker(misc$trekker)
  }

  ## Default colours for groups, e.g. scanpy's uns[<column>_colors]: a list named
  ## by grouping variable, each a named vector (level -> "#RRGGBB"). The Viewer
  ## uses them as the starting colours; only exported groups are kept.
  if (is.list(misc$group_colors) && length(misc$group_colors)) {
    kept <- Filter(
      function(colors) {
        is.character(colors) &&
          !is.null(names(colors)) &&
          length(colors) > 0L &&
          !anyNA(colors) &&
          all(grepl("^#[0-9A-Fa-f]{6}$", colors))
      },
      misc$group_colors[intersect(names(misc$group_colors), groups)]
    )
    if (length(kept)) {
      export$addTechnicalInfo("group_colors", kept)
    }
  }

  invisible(export)
}

#' @keywords internal
#' @noRd
.exportFinalize <- function(
  export,
  final_file,
  export_stage_dir,
  expression_matrix_mode,
  codec,
  viewer_options,
  immune_options = list(mode = "auto", threshold = 100000L),
  on_progress = NULL
) {
  ##--------------------------------------------------------------------------##
  ## show overview of Cerebro object
  ##--------------------------------------------------------------------------##
  .logStep('Overview of Cerebro object:\n')

  ## print object
  export$print()

  ##--------------------------------------------------------------------------##
  ## save Cerebro object to disk
  ##--------------------------------------------------------------------------##

  ## log message
  .logStep('Saving Cerebro object to: ', final_file)
  .exportReportPart(on_progress, "saving the CRB file")

  ## Replace the sidecar and .crb with best-effort rollback on ordinary errors.
  payload <- .publishCerebroExport(
    export = export,
    final_file = final_file,
    stage_dir = export_stage_dir,
    expression_matrix_mode = expression_matrix_mode,
    codec = codec,
    immune_sidecar = immune_options$mode,
    immune_sidecar_threshold = immune_options$threshold
  )
  if (
    .viewerPackEnabled(
      viewer_options$mode,
      viewer_options$threshold,
      length(.viewerPackCells(export))
    )
  ) {
    .exportReportPart(on_progress, "writing the Viewer Pack")
    .buildViewerPack(
      final_file,
      .viewerPackOptions("always", viewer_options$threshold),
      overwrite = TRUE,
      payload = if (.recognizedCerebroObject(payload)) payload
    )
  }

  ## log message
  ## ... writing to file was successful
  if (file.exists(final_file)) {
    .logStep('Done!')
    ## ... target file doesn't exist
  } else {
    stop(
      '[',
      format(Sys.time(), '%H:%M:%S'),
      '] Something went wrong while saving the file.',
      call. = FALSE
    )
  }
}

#' The spatial record of one embedding of point coordinates: its first two
#' columns paired with the expression of the same cells. Stops when the cells
#' and coordinates are not safely paired.
#' @keywords internal
#' @noRd
.exportEmbeddingSpatialData <- function(
  embedding,
  entry_name,
  source_name,
  expression_data,
  assay,
  requested_layer,
  layer
) {
  cells <- intersect(rownames(embedding), colnames(expression_data))
  coordinates <- data.frame(
    .cell = cells,
    .image = entry_name,
    .coordinate_source = paste0("reduction.", source_name),
    x = as.numeric(embedding[cells, 1L]),
    y = as.numeric(embedding[cells, 2L]),
    row.names = cells,
    stringsAsFactors = FALSE
  )
  if (
    !length(cells) ||
      anyDuplicated(rownames(embedding)) ||
      any(!is.finite(coordinates$x)) ||
      any(!is.finite(coordinates$y))
  ) {
    stop("The spatial reduction has no safe paired coordinates.")
  }
  list(
    coordinates = coordinates,
    expression = expression_data[, cells, drop = FALSE],
    assay = assay,
    requested_layer = requested_layer,
    layer = layer,
    image = entry_name,
    coordinate_source = paste0("reduction.", source_name)
  )
}

#' Spatial tab from point coordinates held in embeddings (MERFISH, Slide-seq,
#' AnnData obsm["spatial"]): each embedding with at least two numeric columns
#' becomes a spatial entry, paired with the expression of the same cells. With
#' `split` (a vector named by cell) the cells are divided into one entry per
#' level, which is how several tissue sections kept in one coordinate frame are
#' shown as separate sections. Same record the Seurat route builds for a spatial
#' reduction. `images` holds tissue images per library (see
#' .h5adReadSpatialImages): the single library's images go to an unsplit entry,
#' and with `split` each section takes the images of the library of that name.
#' @keywords internal
#' @noRd
.exportAddSpatialFromEmbeddings <- function(
  export,
  embeddings,
  expression_data,
  assay,
  layer,
  split = NULL,
  images = NULL,
  verbose = FALSE
) {
  add_entry <- function(
    entry_name,
    source_name,
    embedding,
    entry_images = NULL
  ) {
    spatial_data <- tryCatch(
      {
        spatial_data <- .exportEmbeddingSpatialData(
          embedding,
          entry_name,
          source_name,
          expression_data,
          assay,
          requested_layer = layer,
          layer = layer
        )
        projection <- .spx_export_projection_coordinates(
          spatial_data$coordinates
        )
        if (!is.null(projection)) {
          spatial_data$coordinates <- projection
        }
        spatial_data
      },
      error = function(e) {
        ## never drop spatial data silently
        warning(
          "Could not extract spatial data for `",
          entry_name,
          "`: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
    if (is.null(spatial_data)) {
      return(invisible(NULL))
    }
    if (length(entry_images)) {
      spatial_data$histology_images <- entry_images
    }
    export$addSpatialData(entry_name, spatial_data)
    if (verbose) {
      message(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Added spatial data: ',
        entry_name,
        ' (',
        nrow(spatial_data$coordinates),
        ' cells)'
      )
    }
  }

  for (image_name in names(embeddings)) {
    embedding <- embeddings[[image_name]]
    if (is.null(split)) {
      ## one library's images belong to the one entry; several libraries need
      ## `split` to tell which section each belongs to
      if (length(images) > 1L) {
        message(
          "[INFO] The file has tissue images for ",
          length(images),
          " libraries; they are not attached to the single `",
          image_name,
          "` entry. Split the sections (spatial_split_by = \"library_id\") ",
          "to show each on its own image."
        )
      }
      add_entry(
        image_name,
        image_name,
        embedding,
        if (length(images) == 1L) images[[1L]]
      )
      next
    }
    section <- as.character(split[rownames(embedding)])
    levels_present <- unique(section[!is.na(section)])
    ## numeric-looking sections (e.g. Bregma -29, -24, ...) sort numerically
    numeric_levels <- suppressWarnings(as.numeric(levels_present))
    if (!anyNA(numeric_levels)) {
      levels_present <- levels_present[order(numeric_levels)]
    } else {
      levels_present <- sort(levels_present)
    }
    for (level in levels_present) {
      add_entry(
        paste0(image_name, " [", level, "]"),
        image_name,
        embedding[!is.na(section) & section == level, , drop = FALSE],
        images[[level]]
      )
    }
  }
  invisible(export)
}
