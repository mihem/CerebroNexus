##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
## A change in colour/point appearance must not draw a new random sample.
spatial_projection_sampling <- reactiveVal(NULL)
observe(
  {
    controls <- spatial_projection_controls()
    req(controls)
    sampling <- controls[c(
      "projection",
      "grid_sections",
      "percentage_cells_to_show"
    )]
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

## The canonical rows of a grid section, as for the single section above.
spatial_grid_section_index <- function(spatial_name) {
  index <- spatial_section_index(spatial_name)
  if (!is.null(index)) {
    return(index)
  }
  coordinates <- viewerSpatialCoordinates(spatial_name)
  req(coordinates)
  spatial_section_rows(spatial_name, coordinates)
}

## While one section is shown and the session is quiet, read what a switch to
## the grid needs of every other section (its coordinates, its rows, its
## image's descriptor), one section per event-loop turn, so the switch builds
## its frame from memory. Each section is read once per data set.
observe({
  req(identical(input[["sidebar"]], "spatial"))
  controls <- spatial_projection_controls()
  req(controls, !length(controls$grid_sections))
  sections <- availableSpatial()
  req(length(sections) > 1L, exists("viewerIdleRun", mode = "function"))
  dataset <- viewer_current_dataset_key()
  options <- if (exists("Cerebro.options")) Cerebro.options else NULL
  tasks <- lapply(sections, function(section) {
    function() {
      shiny::withReactiveDomain(
        session,
        shiny::isolate({
          viewerSpatialCoordinates(section)
          spatial_grid_section_index(section)
          spatial_grid_section_background(
            options,
            dataset,
            section,
            tryCatch(getSpatialData(section), error = function(error) NULL)
          )
        })
      )
    }
  })
  names(tasks) <- paste0(
    "spatial-grid:",
    viewerDatasetContextToken(viewer_loaded_dataset_context()),
    ":",
    sections
  )
  isolate(viewerIdleRun(
    tasks,
    function() {
      !session$isClosed() &&
        identical(shiny::isolate(input[["sidebar"]]), "spatial")
    },
    delay = 1.5
  ))
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
    frame_metadata <- viewerProjectionFirstFrameMetadata()
    ## The rows of `index` in `sample` and `roi` ("" for all), kept by the
    ## group filters.
    scope_rows <- function(index, sample, roi) {
      for (scope in list(
        list(
          value = sample,
          fields = spatial_sample_fields
        ),
        list(
          value = spatial_roi_value(roi),
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
          frame_metadata,
          index,
          names(filters)
        )
        index <- index[cerebroGroupFilterMask(metadata, filters)]
      }
      index
    }
    percentage <- controls$percentage_cells_to_show
    ## A grid draws every cell of every section, one section after the
    ## other: the Sample and ROI controls belong to the single section.
    if (length(controls$grid_sections)) {
      return(spatial_grid_cells(
        controls$grid_sections,
        spatial_grid_section_index,
        function(index) scope_rows(index, "", ""),
        percentage
      ))
    }
    index <- scope_rows(
      spatial_projection_cell_index(),
      spatial_projection_selected_sample(),
      input[["spatial_projection_roi"]] %||% ""
    )
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
