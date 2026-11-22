##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
## ROI membership, sample scope and pivots for a section, by position in its
## coordinates: `roi_by_position` (the ROI of each row), `scoped` (the rows of
## the selected sample) and `pivots`. Ranges, hulls and the frame all need it
## with the same inputs, so share one result. The ROIs come from the section's
## metadata rows (spatial_section_scope()), not from matching cell names.
spatial_projection_roi_context_memo <- viewerMemoOne()
spatial_projection_roi_context <- function(spatial_name, rotation_angle) {
  sample_value <- spatial_projection_selected_sample()
  spatial_projection_roi_context_memo(
    list(data_set(), spatial_name, sample_value, rotation_angle),
    function() {
      coordinates <- viewerSpatialCoordinates(spatial_name)
      metadata <- viewerProjectionFirstFrameMetadata()
      spatial_roi_section_context(
        coordinates,
        spatial_section_scope(
          spatial_name,
          sample_value,
          metadata,
          function() rownames(coordinates) %||% character()
        ),
        metadata,
        sample_value,
        rotation_angle
      )
    }
  )
}
## The ROI of each drawn cell (spatial_projection_coordinates()).
spatial_projection_drawn_roi <- function(roi_context) {
  rows <- spatial_projection_coordinate_rows()
  roi <- roi_context$roi_by_position
  as.character(if (is.null(rows)) roi else roi[rows])
}

spatial_projection_full_ranges <- reactive({
  ## Follow the resolved parameters: the FOV control may not have bound yet.
  parameters <- spatial_projection_parameters_plot()
  spatial_name <- parameters[["projection"]]
  req(spatial_name %in% availableSpatial())

  dataset <- viewer_current_dataset_key()
  rotation_angle <- spatialPlotRotation(
    Cerebro.options,
    dataset,
    spatial_name
  )
  full_coords <- viewerSpatialCoordinates(spatial_name)
  if (parameters[["roi_mode"]] %in% c("single", "separate")) {
    roi_context <- spatial_projection_roi_context(spatial_name, rotation_angle)
    scoped <- roi_context$scoped
    roi <- roi_context$roi_by_position
    selected_roi <- spatial_roi_value(parameters[["roi_selection"]])
    if (nzchar(selected_roi)) {
      scoped <- scoped[!is.na(roi[scoped]) & roi[scoped] == selected_roi]
    }
    full_coords <- rotateSpatialCoordinatesByRoi(
      full_coords[scoped, , drop = FALSE],
      as.character(roi[scoped]),
      spatialRoiSettings(Cerebro.options, dataset, spatial_name),
      rotation_angle,
      pivots = roi_context$pivots
    )
  } else {
    full_coords <- rotateSpatialCoordinates(full_coords, rotation_angle)
  }
  x_full <- range(full_coords[[1]], na.rm = TRUE)
  y_full <- range(full_coords[[2]], na.rm = TRUE)
  if (!all(is.finite(c(x_full, y_full)))) {
    return(list(x_range = NULL, y_range = NULL))
  }

  list(
    x_range = x_full + c(-1, 1) * diff(x_full) * 0.02,
    y_range = y_full + c(-1, 1) * diff(y_full) * 0.02
  )
})

spatial_projection_group_hulls <- reactive({
  ## A grid draws no region outlines: they belong to one section's plane.
  if (
    !isTRUE(spatial_projection_region_outlines()) ||
      !identical(input[["spatial_projection_plot_type"]], "ImageDimPlot") ||
      length(spatial_projection_controls()$grid_sections)
  ) {
    return(list())
  }
  color_variable <- input[["spatial_projection_point_color"]]
  metadata <- spatial_projection_metadata()
  req(color_variable, color_variable %in% colnames(metadata))
  color_input <- metadata[[color_variable]]
  if (is.numeric(color_input)) {
    return(list())
  }
  coordinates <- spatial_projection_coordinates()
  dataset <- viewer_current_dataset_key()
  parameters <- spatial_projection_parameters_plot()
  spatial_name <- parameters[["projection"]]
  rotation_angle <- spatialPlotRotation(
    Cerebro.options,
    dataset,
    spatial_name
  )
  if (parameters[["roi_mode"]] %in% c("single", "separate")) {
    roi_context <- spatial_projection_roi_context(spatial_name, rotation_angle)
    coordinates <- rotateSpatialCoordinatesByRoi(
      coordinates,
      spatial_projection_drawn_roi(roi_context),
      spatialRoiSettings(Cerebro.options, dataset, spatial_name),
      rotation_angle,
      pivots = roi_context$pivots
    )
  } else {
    coordinates <- rotateSpatialCoordinates(coordinates, rotation_angle)
  }
  if (ncol(coordinates) != 2L) {
    return(list())
  }
  compute_group_hulls(
    coordinates[[1]],
    coordinates[[2]],
    as.character(color_input)
  )
})

## The data of a grid frame (func_spatial_helpers.R): the cells of every section
## of the sample, coloured as one frame so that all panels share one colour
## scale, and the slice, extent and image of each section's panel. It is the
## same shape as a single-section frame without a geometry resource.
## The expression of each RGB channel's gene as the metadata columns
## coexpr_r/g/b (NA for an empty channel), for the given cells.
spatial_projection_coexpression_columns <- function(
  metadata,
  plot_parameters,
  cells
) {
  ## Use a list, not c(): an empty channel is NULL, and c() would DROP it and
  ## shift the remaining names, misaligning genes to channels.
  coexpr_genes <- list(
    coexpr_r = plot_parameters$coexpr_r,
    coexpr_g = plot_parameters$coexpr_g,
    coexpr_b = plot_parameters$coexpr_b
  )
  requested_genes <- unique(unlist(coexpr_genes, use.names = FALSE))
  requested_genes <- requested_genes[
    !is.na(requested_genes) &
      nzchar(requested_genes) &
      requested_genes %in% getGeneNames()
  ]
  expression_values <- if (length(requested_genes)) {
    viewerExpressionValues(data_set(), c(cells), requested_genes)
  } else {
    list()
  }
  for (channel in names(coexpr_genes)) {
    gene <- coexpr_genes[[channel]]
    metadata[[channel]] <- NA_real_
    if (!is.null(gene) && gene %in% names(expression_values)) {
      metadata[[channel]] <- expression_values[[gene]]
    }
  }
  metadata
}

spatial_projection_grid_data <- function(
  dataset_context,
  plot_parameters,
  cells_to_extract
) {
  dataset <- dataset_context$dataset_key
  options <- if (exists("Cerebro.options")) Cerebro.options else NULL
  layout <- spatial_grid_coordinates(
    cells_to_extract,
    spatial_grid_section_xy,
    spatial_grid_section_index,
    function(section) spatialPlotRotation(options, dataset, section)
  )
  cells <- c(cells_to_extract)
  columns <- spatial_projection_metadata_columns(plot_parameters)
  full_metadata <- viewerProjectionFirstFrameMetadata()
  req(all(columns %in% colnames(full_metadata)))
  metadata <- spatial_grid_metadata(full_metadata, cells, columns)
  if (
    identical(plot_parameters$plot_type, "ImageFeaturePlot") &&
      !is.null(plot_parameters$feature_to_display)
  ) {
    gene <- plot_parameters$feature_to_display
    expression <- if (gene %in% getGeneNames()) {
      viewerExpressionRow(data_set(), cells, gene)
    }
    req(!is.null(expression))
    metadata[[gene]] <- expression
  }
  if (identical(plot_parameters$plot_type, "Co-expression (RGB)")) {
    metadata <- spatial_projection_coexpression_columns(
      metadata,
      plot_parameters,
      cells
    )
  }
  color_variable <- plot_parameters[["color_variable"]]
  metadata <- spatial_with_no_gene_column(metadata, color_variable)
  numeric_color <- is.numeric(metadata[[color_variable]])
  ## One assignment over every section: a group has the same colour in each.
  color_assignments <- if (numeric_color) {
    NULL
  } else if (identical(color_variable, spatial_no_gene_variable)) {
    stats::setNames(spatial_no_gene_color, spatial_no_gene_label)
  } else {
    assignColorsToGroups(metadata, color_variable)
  }
  plot_parameters[["grid"]] <- layout[setdiff(names(layout), "coordinates")]
  ## The embedding panel draws the same cells, in frame order.
  projection_name <- plot_parameters[["grid_projection"]] %||% ""
  if (nzchar(projection_name)) {
    embedding <- getProjection(projection_name)
    req(is.data.frame(embedding) || is.matrix(embedding))
    plot_parameters[["grid"]][["projection"]] <- list(
      label = projection_name,
      x = as.numeric(embedding[cells, 1L]),
      y = as.numeric(embedding[cells, 2L])
    )
  }
  plot_parameters[["x_range"]] <- spatial_grid_range(layout$coordinates$x)
  plot_parameters[["y_range"]] <- spatial_grid_range(layout$coordinates$y)
  list(
    dataset_context = dataset_context,
    cells_df = metadata,
    coordinates = layout$coordinates,
    reset_axes = FALSE,
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    group_hulls = list(),
    geometry_resource = NULL,
    categorical_resource = NULL,
    resource_first = FALSE,
    cell_count = length(cells),
    cell_indices = cells
  )
}

spatial_projection_data_to_plot_raw <- reactive({
  dataset_context <- viewer_loaded_dataset_context()
  req(
    spatial_projection_parameters_plot(),
    reactive_colors()
  )
  plot_parameters <- spatial_projection_parameters_plot()
  cells_to_extract <- spatial_projection_cells_to_show()
  if (identical(plot_parameters[["layout"]], "grid")) {
    return(spatial_projection_grid_data(
      dataset_context,
      plot_parameters,
      cells_to_extract
    ))
  }

  ## Resolve the canonical geometry contract before materializing the displayed
  ## metadata and coordinates. The common full, unrotated ImageDimPlot frame can
  ## then stream both arrays from the Viewer Pack in the browser.
  current_name <- dataset_context$dataset_key
  rotation_angle <- spatialPlotRotation(
    Cerebro.options,
    current_name,
    plot_parameters[["projection"]]
  )
  geometry_resource <- viewerSpatialGeometryAsset(
    plot_parameters[["projection"]],
    cells_to_extract,
    rotation_angle
  )
  failed_resource <- input[[
    "spatial_projection_projection_resource_failed"
  ]]
  if (
    !is.null(geometry_resource) &&
      length(failed_resource) == 1L &&
      !is.na(failed_resource) &&
      identical(failed_resource, geometry_resource$url)
  ) {
    geometry_resource <- NULL
  }
  full_metadata <- viewerProjectionFirstFrameMetadata()
  color_variable <- plot_parameters[["color_variable"]]
  full_color <- if (color_variable %in% colnames(full_metadata)) {
    full_metadata[[color_variable]]
  } else {
    NULL
  }
  full_canonical_frame <- identical(
    as.integer(cells_to_extract),
    seq_len(nrow(full_metadata))
  )
  background_descriptor <- plot_parameters[["background_descriptor"]]
  background_bounds <- if (is.list(background_descriptor)) {
    background_descriptor[["bounds"]]
  } else {
    NULL
  }
  background_geometry_ready <- is.null(background_descriptor) ||
    ((is.atomic(background_bounds) || is.list(background_bounds)) &&
      all(c("xmin", "xmax", "ymin", "ymax") %in% names(background_bounds)) &&
      all(is.finite(as.numeric(unlist(
        background_bounds[c("xmin", "xmax", "ymin", "ymax")]
      )))))
  roi_settings <- spatialRoiSettings(
    Cerebro.options,
    current_name,
    plot_parameters[["projection"]]
  )
  roi_rotation_needed <- plot_parameters[["roi_mode"]] %in%
    c("single", "separate") &&
    any(vapply(
      roi_settings,
      function(setting) {
        degrees <- suppressWarnings(as.numeric(setting[["rotation_degrees"]]))
        length(degrees) == 1L && is.finite(degrees) && degrees != 0
      },
      logical(1)
    ))
  ## A section of a data set of several: the geometry asset exists only for
  ## exactly the section's cells in the Pack's order, unrotated. The browser
  ## draws it from the Pack too: it maps the canonical codes of the colour
  ## column through the frame's canonical rows, which travel as a range
  ## (cell_views.js, hydrateSingleProjectionResource()). Its colours are those
  ## of the whole column only for a factor or a group with assigned colours (a
  ## character column is coloured by the levels the frame has).
  section_frame <- !isTRUE(full_canonical_frame) &&
    is.list(geometry_resource) &&
    !is.null(cerebroRowRange(as.integer(cells_to_extract) - 1L)) &&
    (is.factor(full_color) || color_variable %in% names(reactive_colors()))
  resource_first_candidate <-
    identical(plot_parameters[["plot_type"]], "ImageDimPlot") &&
    !nzchar(plot_parameters[["split_by"]] %||% "") &&
    is.list(geometry_resource) &&
    (isTRUE(full_canonical_frame) || isTRUE(section_frame)) &&
    !is.numeric(full_color) &&
    isTRUE(background_geometry_ready) &&
    !roi_rotation_needed &&
    !identical(color_variable, spatial_no_gene_variable)
  ## Only the resource-first path uses these; the others assign colours from
  ## the plotted frame below.
  color_assignments <- if (!resource_first_candidate) {
    NULL
  } else if (length(full_color)) {
    assignColorsToGroups(full_metadata, color_variable)
  } else {
    character()
  }
  categorical_resource <- if (resource_first_candidate) {
    viewerMetadataCodesAsset(color_variable, names(color_assignments))
  } else {
    NULL
  }
  codes_cells <- geometry_resource$cells
  if (isTRUE(section_frame) && is.list(categorical_resource)) {
    ## The section's legend lists the levels its cells have, as the frame of
    ## its values would.
    section_codes <- spatial_section_codes_resource(
      categorical_resource,
      full_color[as.integer(cells_to_extract)],
      color_assignments
    )
    categorical_resource <- section_codes$resource
    color_assignments <- section_codes$color_assignments
    codes_cells <- nrow(full_metadata)
  }
  resource_first <- resource_first_candidate &&
    is.list(categorical_resource) &&
    identical(
      as.integer(categorical_resource$cells),
      as.integer(codes_cells)
    )
  metadata <- if (resource_first) NULL else spatial_projection_metadata()
  metadata <- spatial_with_no_gene_column(
    metadata,
    plot_parameters[["color_variable"]]
  )

  ## Handle ImageFeaturePlot (add gene expression data)
  if (
    plot_parameters$plot_type == 'ImageFeaturePlot' &&
      !is.null(plot_parameters$feature_to_display)
  ) {
    gene <- plot_parameters$feature_to_display
    if (gene %in% getGeneNames()) {
      expr_values <- viewerExpressionRow(
        data_set(),
        cells_to_extract,
        gene
      )
      if (!is.null(expr_values)) {
        metadata[[gene]] <- expr_values
      }
    }
  }

  ## Co-expression: pull each channel's gene expression into metadata columns
  ## keyed by a stable channel name, so the renderer can blend them onto RGB.
  if (plot_parameters$plot_type == "Co-expression (RGB)") {
    metadata <- spatial_projection_coexpression_columns(
      metadata,
      plot_parameters,
      cells_to_extract
    )
  }

  ## get colors for groups (if applicable)
  ## Filtered and feature frames retain their existing per-frame palette.
  if (
    !resource_first &&
      identical(plot_parameters[["color_variable"]], spatial_no_gene_variable)
  ) {
    color_assignments <- stats::setNames(
      spatial_no_gene_color,
      spatial_no_gene_label
    )
  } else if (!resource_first) {
    color_assignments <- if (
      plot_parameters[['color_variable']] %in%
        colnames(metadata) &&
        is.numeric(metadata[[plot_parameters[['color_variable']]]])
    ) {
      NULL
    } else {
      assignColorsToGroups(
        metadata,
        plot_parameters[['color_variable']]
      )
    }
  }
  ## Apply rotation to the displayed (subset) coordinates.
  coordinates <- if (resource_first) {
    NULL
  } else if (plot_parameters[["roi_mode"]] %in% c("separate", "single")) {
    full_coordinates <- viewerSpatialCoordinates(plot_parameters[[
      "projection"
    ]])
    roi_context <- spatial_projection_roi_context(
      plot_parameters[["projection"]],
      rotation_angle
    )
    roi_settings <- spatialRoiSettings(
      Cerebro.options,
      current_name,
      plot_parameters[["projection"]]
    )
    displayed_coordinates <- spatial_projection_coordinates()
    displayed_roi <- spatial_projection_drawn_roi(roi_context)
    scoped <- roi_context$scoped
    scoped_coordinates <- full_coordinates[scoped, , drop = FALSE]
    scoped_roi <- as.character(roi_context$roi_by_position[scoped])
    if (identical(plot_parameters[["roi_mode"]], "separate")) {
      transformed_full <- rotateSpatialCoordinatesByRoi(
        scoped_coordinates,
        scoped_roi,
        roi_settings,
        rotation_angle,
        pivots = roi_context$pivots
      )
      plot_parameters[["roi_extents"]] <- spatial_roi_extents(
        transformed_full,
        scoped_roi
      )
    }
    rotateSpatialCoordinatesByRoi(
      displayed_coordinates,
      displayed_roi,
      roi_settings,
      rotation_angle,
      pivots = roi_context$pivots
    )
  } else {
    rotateSpatialCoordinates(
      spatial_projection_coordinates(),
      rotation_angle
    )
  }
  ## Pin the axes to the FULL cell extent, not the currently displayed subset.
  ## Otherwise, changing "Show % of cells" rescales the axes to whatever subset
  ## is plotted and the plot visibly jitters. We compute the range over ALL cells
  ## (in the same rotated frame) and pass it as an explicit x/y range. A small
  ## margin keeps edge points off the frame.
  if (
    !resource_first &&
      (is.null(plot_parameters[["x_range"]]) ||
        length(plot_parameters[["x_range"]]) < 2 ||
        is.null(plot_parameters[["y_range"]]) ||
        length(plot_parameters[["y_range"]]) < 2)
  ) {
    full_ranges <- spatial_projection_full_ranges()
    plot_parameters[["x_range"]] <- full_ranges[["x_range"]]
    plot_parameters[["y_range"]] <- full_ranges[["y_range"]]
  }

  ## With an explicit full-extent range we must NOT let the JS autorange (which
  ## would refit to the subset). reset_axes is meant to snap back to the full
  ## view on a dataset switch — that is exactly the full-extent range we set, so
  ## keep the fixed range.
  reset_axes <- isolate(spatial_projection_parameters_other[['reset_axes']])
  if (
    length(plot_parameters[["x_range"]]) >= 2 &&
      length(plot_parameters[["y_range"]]) >= 2
  ) {
    reset_axes <- FALSE
  }

  ## return collect data. Region outlines are an overlay patched onto the drawn
  ## frame (event_projection_update_plot.R), and control values are added by
  ## the sender: neither makes a new frame.
  to_return <- list(
    dataset_context = dataset_context,
    cells_df = metadata,
    coordinates = coordinates,
    reset_axes = reset_axes,
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    group_hulls = isolate(spatial_projection_group_hulls()),
    geometry_resource = geometry_resource,
    categorical_resource = categorical_resource,
    resource_first = resource_first,
    cell_count = length(cells_to_extract),
    cell_indices = cells_to_extract
  )

  return(to_return)
})

spatial_projection_data_to_plot <- debounceAfterFirst(
  spatial_projection_data_to_plot_raw,
  150
)
