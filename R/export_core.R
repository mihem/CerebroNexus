## Format-independent export core -------------------------------------------
##
## Every exportFromXxx() adapter reduces its native object to plain R data (a
## cell meta data.frame, a `misc` list, cell barcodes, a genes x cells matrix,
## named embeddings) and hands it to these helpers. The helpers know nothing
## about Seurat, SingleCellExperiment or AnnData, so a new input format only
## needs a thin adapter and inherits the external expression backends, the
## transactional publish and the Viewer Pack from here.

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

    ## transpose genes x cells -> cells x genes for storage
    m_disk <- if (inherits(expression_data, c("RleMatrix", "DelayedMatrix"))) {
      DelayedArray::t(expression_data)
    } else {
      methods::as(Matrix::t(expression_data), "CsparseMatrix")
    }

    if (verbose) {
      .logStep("Writing expression matrix to TENx HDF5 file: ", h5_abs)
    }

    if (file.exists(h5_abs)) {
      file.remove(h5_abs)
    }
    ## A level of 0 stores the data without compression (the shuffle and
    ## deflate filters are still applied): on 1.3-1.5M-cell sources single genes
    ## read ~30% faster than at HDF5Array's default level 6, at roughly 7x the
    ## file size. NULL leaves HDF5Array's default.
    HDF5Array::writeTENxMatrix(
      m_disk,
      h5_abs,
      group = "expression",
      level = h5_compression_level
    )

    ## self$expression stays NULL — saveRDS therefore does not embed the
    ## matrix inside the .crb. The runtime attach reads the sibling back
    ## as a lazy TENxMatrix seed (no in-memory dgCMatrix materialisation).
    export$setExpressionBackend(
      type = "h5",
      location = h5_filename,
      md5 = unname(tools::md5sum(h5_abs))
    )
  }

  expression_data
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
  immune_options = list(mode = "auto", threshold = 100000L)
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

  ## Replace the sidecar and .crb with best-effort rollback on ordinary errors.
  .publishCerebroExport(
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
    buildViewerPack(
      final_file,
      viewer_binary = "always",
      viewer_binary_threshold = viewer_options$threshold,
      overwrite = TRUE
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
