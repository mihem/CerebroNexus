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
    if (!identical(isolate(spatial_projection_sampling()), sampling)) {
      spatial_projection_sampling(sampling)
    }
  },
  priority = 950
)

spatial_projection_cell_index <- reactive({
  controls <- spatial_projection_sampling()
  req(controls)
  spatial_name <- controls$projection
  req(spatial_name %in% availableSpatial())
  coordinates <- viewerSpatialCoordinates(spatial_name)
  req(coordinates)
  spatial_cells <- rownames(coordinates)
  pack <- viewerPackCurrent()
  pack_cell_count <- suppressWarnings(as.integer(pack$cell_count))
  valid_pack_count <- length(pack_cell_count) == 1L &&
    !is.na(pack_cell_count)
  index <- viewerPackSpatialIndex(pack, spatial_name)
  valid_index <- length(index) == nrow(coordinates) &&
    !anyNA(index) &&
    isTRUE(all(index >= 1L))
  if (!isTRUE(valid_index)) {
    index <- NULL
  }
  pack_order_matches <-
    is.null(index) &&
    !is.null(pack) &&
    is.list(pack) &&
    valid_pack_count &&
    length(spatial_cells) == pack_cell_count &&
    identical(
      viewerPackCellOrderFingerprint(spatial_cells),
      as.character(pack$manifest$cell_order_fingerprint)
    )
  if (isTRUE(pack_order_matches)) {
    index <- seq_len(pack_cell_count)
  }
  if (is.null(index)) {
    index <- match(spatial_cells, getMetaData()[["cell_barcode"]])
  }
  if (length(index) != nrow(coordinates) || anyNA(index)) {
    stop("Spatial cells do not match the canonical dataset order.")
  }
  as.integer(index)
})

spatial_projection_cells_to_show_current <- reactive({
  controls <- spatial_projection_sampling()
  req(controls, controls$percentage_cells_to_show)
  groups <- getGroups()
  filters <- stats::setNames(
    lapply(groups, function(group) {
      input[[paste0("spatial_projection_group_filter_", group)]]
    }),
    groups
  )
  filters <- filters[!vapply(filters, is.null, logical(1))]
  index <- spatial_projection_cell_index()
  frame_metadata <- viewerProjectionFirstFrameMetadata()
  for (scope in list(
    list(
      value = spatial_projection_selected_sample(),
      fields = c("sample", "sample_id", "orig.ident")
    ),
    list(
      value = spatial_roi_value(input[["spatial_projection_roi"]] %||% ""),
      fields = c("sample_roi", "roi", "roi_id", "region_of_interest")
    )
  )) {
    selected <- scope$value %||% ""
    if (
      !is.character(selected) ||
        length(selected) != 1L ||
        is.na(selected) ||
        !nzchar(selected)
    ) {
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
})

## Group filter and ROI controls bind after the first frame, usually keeping
## every cell. Publish the cells on change only, so binding does not rebuild
## the million-cell frame downstream.
spatial_projection_cells_state <- reactiveVal(NULL)
observe(
  {
    ## Off the Spatial page, keep the last cells; returning re-evaluates.
    if (
      "sidebar" %in%
        isolate(names(input)) &&
        !identical(input[["sidebar"]], "spatial")
    ) {
      return()
    }
    value <- tryCatch(
      spatial_projection_cells_to_show_current(),
      error = function(error) NULL
    )
    if (!identical(isolate(spatial_projection_cells_state()), value)) {
      spatial_projection_cells_state(value)
    }
  },
  priority = 900
)
spatial_projection_cells_to_show <- reactive({
  value <- spatial_projection_cells_state()
  req(!is.null(value))
  value
})
