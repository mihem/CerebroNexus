##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
## A change in colour/point appearance must not draw a new random sample.
spatial_projection_sampling <- reactiveVal(NULL)
observe(
  {
    controls <- spatial_projection_controls()
    req(controls)
    sampling <- controls[c("projection", "percentage_cells_to_show")]
    viewerPublishChanged(spatial_projection_sampling, sampling)
  },
  priority = 950
)

spatial_projection_cell_index <- reactive({
  controls <- spatial_projection_sampling()
  req(controls)
  spatial_name <- controls$projection
  req(spatial_name %in% availableSpatial())
  ## The Viewer Pack's index of the section (or the rows a thin CRB stores)
  ## needs no cell name.
  index <- spatial_section_index(spatial_name)
  if (!is.null(index)) {
    return(index)
  }
  coordinates <- viewerSpatialCoordinates(spatial_name)
  req(coordinates)
  spatial_section_rows(spatial_name, coordinates)
})

## Group filter and ROI controls bind after the first frame, usually keeping
## every cell; with a Viewer Pack the browser applies the group filters to the
## drawn section.
spatial_projection_cells <- viewerProjectionCells(
  "spatial_projection",
  "spatial",
  client = function() viewerPackCanonical(viewerPackCurrent()),
  rows = function(filters) {
    controls <- spatial_projection_sampling()
    req(controls, controls$percentage_cells_to_show)
    index <- spatial_projection_cell_index()
    frame_metadata <- viewerProjectionFirstFrameMetadata()
    for (scope in list(
      list(
        value = spatial_projection_selected_sample(),
        fields = spatial_sample_fields
      ),
      list(
        value = spatial_roi_value(input[["spatial_projection_roi"]] %||% ""),
        fields = spatial_roi_fields
      )
    )) {
      selected <- scope$value %||% ""
      if (!.runtimeIsString(selected)) {
        next
      }
      field_index <- match(
        tolower(scope$fields),
        tolower(colnames(frame_metadata)),
        nomatch = 0L
      )
      field_index <- field_index[field_index > 0L]
      if (!length(field_index)) {
        next
      }
      field <- colnames(frame_metadata)[[field_index[[1L]]]]
      values <- as.character(frame_metadata[[field]][index])
      if (!selected %in% values) {
        next
      }
      index <- index[!is.na(values) & values == selected]
    }
    if (length(filters)) {
      metadata <- viewerProjectionSubsetRows(
        viewerProjectionFirstFrameMetadata(),
        index,
        names(filters)
      )
      index <- index[cerebroGroupFilterMask(metadata, filters)]
    }
    percentage <- controls$percentage_cells_to_show
    if (length(index) && percentage < 100) {
      index <- sample(index, ceiling(length(index) * percentage / 100))
    }
    index
  },
  priority = 900,
  wait = TRUE
)
spatial_projection_group_filters <- spatial_projection_cells$group_filters
spatial_projection_client_filters <- spatial_projection_cells$client_filters
spatial_projection_cells_to_show <- spatial_projection_cells$cells_to_show
