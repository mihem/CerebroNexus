##----------------------------------------------------------------------------##
## Tab: Trajectory — projection plot.
##
## Rendered by the shared cell-view Canvas engine. The trajectory path is sent
## as coordinate-space line segments alongside the cell payload.

trajectory_projection_lines <- function(trajectory_edges) {
  lapply(seq_len(nrow(trajectory_edges)), function(i) {
    list(
      type = "line",
      line = list(color = cerebro_plotly_theme()$title, width = 1),
      xref = "x",
      yref = "y",
      x0 = trajectory_edges$source_dim_1[i],
      y0 = trajectory_edges$source_dim_2[i],
      x1 = trajectory_edges$target_dim_1[i],
      y1 = trajectory_edges$target_dim_2[i]
    )
  })
}

##----------------------------------------------------------------------------##
## Reactive that prepares the cells + trajectory-line data for the current
## parameters (filtering, subsetting, hover, colours). One source of truth so
## the coordinates sent to the plot match those used for selection and hover.
##----------------------------------------------------------------------------##
## shiny::debounce() evaluates this eagerly; only the visible page may pay for it.
trajectory_projection_active <- function() identical(input[["sidebar"]], "trajectory")
trajectory_projection_prepared_raw <- reactive({
  req(trajectory_projection_active())
  dataset_context <- viewer_loaded_dataset_context()
  req(
    trajectory_selection_ok(),
    input[["trajectory_point_color"]]
  )

  trajectory_data <- trajectory_data_reactive()

  groups <- getGroups()
  group_filters <- stats::setNames(
    lapply(groups, function(group) {
      input[[paste0("trajectory_projection_group_filter_", group)]]
    }),
    groups
  )
  group_filters <- group_filters[
    !vapply(group_filters, is.null, logical(1))
  ]
  color_variable <- input[["trajectory_point_color"]]
  cells_df <- trajectory_cells_reactive(c(
    color_variable,
    names(group_filters)
  ))
  keep <- cerebroGroupFilterMask(cells_df, group_filters)
  if (!all(keep)) {
    cells_df <- cells_df[keep, , drop = FALSE]
  }

  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(
    cells_df,
    trajectory_projection_appearance$percentage_cells_to_show
  )

  ## Send an explicit empty payload so clearing every filter cannot leave the
  ## previous Canvas frame visible.
  if (nrow(cells_df) == 0L) {
    return(list(
      dataset_context = dataset_context,
      feedback = cerebroFeedbackControls(
        "trajectory_projection",
        input,
        list(
          trajectory_point_size = isolate(trajectory_projection_appearance$point_size),
          trajectory_point_opacity = isolate(trajectory_projection_appearance$point_opacity),
          trajectory_percentage_cells_to_show = trajectory_projection_appearance$percentage_cells_to_show
        )
      ),
      cells_df = cells_df,
      trajectory_lines = list(),
      hover = isTRUE(preferences[["show_hover_info_in_projections"]]),
      color_variable = input[["trajectory_point_color"]],
      point_size = isolate(trajectory_projection_appearance$point_size),
      point_opacity = isolate(trajectory_projection_appearance$point_opacity),
      draw_border = trajectory_projection_appearance$draw_border,
      keep_square = trajectory_projection_appearance$keep_square
    ))
  }

  ## Categorical payloads are split into one trace per group below, so shuffling
  ## rows within a trace cannot change paint order. Continuous colours stay
  ## shuffled so the original overlap behaviour is preserved.
  if (is.numeric(cells_df[[color_variable]])) {
    cells_df <- cells_df[sample.int(nrow(cells_df)), , drop = FALSE]
  }

  ## trajectory path as line-segment shapes (warm near-black, the theme title
  ## colour), drawn under the points as the structural backbone
  trajectory_lines <- trajectory_projection_lines(trajectory_data[["edges"]])

  list(
    dataset_context = dataset_context,
    feedback = cerebroFeedbackControls(
      "trajectory_projection",
      input,
      list(
        trajectory_point_size = isolate(trajectory_projection_appearance$point_size),
        trajectory_point_opacity = isolate(trajectory_projection_appearance$point_opacity),
        trajectory_percentage_cells_to_show = trajectory_projection_appearance$percentage_cells_to_show
      )
    ),
    cells_df = cells_df,
    trajectory_lines = trajectory_lines,
    hover = isTRUE(preferences[["show_hover_info_in_projections"]]),
    color_variable = color_variable,
    point_size = isolate(trajectory_projection_appearance$point_size),
    point_opacity = isolate(trajectory_projection_appearance$point_opacity),
    draw_border = trajectory_projection_appearance$draw_border,
    keep_square = trajectory_projection_appearance$keep_square
  )
})

## Debounce the prepared reactive so dragging a slider (point size / opacity /
## "% of cells") coalesces its rapid-fire input events into a single redraw
## after the drag settles, instead of rebuilding the Canvas payload on every
## intermediate value. Mirrors the debounce the other projection tabs already
## apply to their parameter/data reactives.
trajectory_projection_prepared <- debounceAfterFirst(
  trajectory_projection_prepared_raw,
  200,
  active = trajectory_projection_active
)

##----------------------------------------------------------------------------##
## Axis-reset state, mirroring the overview / gene-expression projections.
##
## Resetting axes on every render would re-run autorange on any parameter change
## (colour variable, point size, group filter) and the axes would visibly snap —
## on top of the reveal/resize settle this reads as a jump. So reset only when
## the *trajectory itself* changes (method/name); every other re-render holds the
## current range. Reset back to FALSE after each push so a subsequent parameter
## change does not autorange.
##----------------------------------------------------------------------------##
trajectory_projection_parameters_other <- reactiveValues(
  reset_axes = TRUE
)

observeEvent(
  {
    input[["trajectory_selected_method"]]
    input[["trajectory_selected_name"]]
  },
  {
    trajectory_projection_parameters_other[["reset_axes"]] <- TRUE
  }
)

##----------------------------------------------------------------------------##
## Observer that pushes the prepared data to the shared JS renderer.
##----------------------------------------------------------------------------##
trajectory_projection_sent <- reactiveVal(FALSE)

## The default million-cell frame is already stored in the Viewer Pack in the
## exact trajectory row order. Keep this gate deliberately narrow: any filter,
## sampling, non-state colour, unsupported transport, or failed resource uses
## the established R construction path below.
trajectory_static_first_frame <- reactive({
  req(trajectory_selection_ok())
  color_variable <- input[["trajectory_point_color"]]
  groups <- getGroups()
  if (
    !isTRUE(input[["coordviews_wire_supported"]]) ||
      !(identical(color_variable, "state") || isTRUE(color_variable %in% groups)) ||
      !isTRUE(all.equal(
        as.numeric(trajectory_projection_appearance$percentage_cells_to_show),
        100
      ))
  ) {
    return(NULL)
  }
  filters <- lapply(groups, function(group) {
    input[[paste0("trajectory_projection_group_filter_", group)]]
  })
  if (any(!vapply(filters, is.null, logical(1)))) {
    return(NULL)
  }
  method <- input[["trajectory_selected_method"]]
  name <- input[["trajectory_selected_name"]]
  resource <- viewerTrajectoryFrameAsset(method, name)
  if (is.null(resource)) {
    return(NULL)
  }
  failed <- input[["trajectory_projection_projection_resource_failed"]]
  resource_urls <- c(
    resource$projection$url,
    resource$state$url,
    resource$subset$url
  )
  if (
    length(failed) == 1L &&
      !is.na(failed) &&
      failed %in% resource_urls
  ) {
    return(NULL)
  }
  if (identical(color_variable, "state")) {
    levels <- as.character(resource$state$levels)
  } else {
    ## A metadata group (e.g. the default main group) is coloured from its
    ## canonical Viewer Pack codes, mapped to the trajectory rows by the frame's
    ## canonical subset in the browser. Other geometries have no such mapping.
    if (!identical(as.character(resource$geometry_kind), "canonical_projection")) {
      return(NULL)
    }
    group_resource <- viewerMetadataCodesAsset(
      color_variable,
      getGroupLevels(color_variable)
    )
    if (is.null(group_resource)) {
      return(NULL)
    }
    resource$group <- group_resource
    levels <- as.character(group_resource$levels)
  }
  colors <- stats::setNames(cerebro_group_colors(length(levels)), levels)
  if (color_variable %in% groups) {
    configured <- reactive_group_colors(color_variable)
    if (all(levels %in% names(configured))) {
      colors <- configured[levels]
    }
  }
  trajectory_edges <- viewerPackTrajectoryEdges(
    viewerPackCurrent(),
    method,
    name
  )
  if (is.null(trajectory_edges)) {
    trajectory_edges <- trajectory_data_reactive()[["edges"]]
  }
  list(
    resource = resource,
    color_variable = color_variable,
    levels = levels,
    colors = colors,
    hover = isTRUE(preferences[["show_hover_info_in_projections"]]),
    trajectory_lines = trajectory_projection_lines(trajectory_edges)
  )
})

observeEvent(
  viewerDatasetIdentity()$fingerprint,
  {
    appearance <- current_scatter_defaults()
    trajectory_projection_appearance$point_size <- appearance$point_size
    trajectory_projection_appearance$point_opacity <- appearance$point_opacity
    trajectory_projection_appearance$percentage_cells_to_show <-
      appearance$percentage_cells_to_show
    display <- current_page_display_defaults("trajectory")
    trajectory_projection_appearance$group_labels <- display$group_labels
    trajectory_projection_appearance$draw_border <- display$draw_border
    trajectory_projection_appearance$keep_square <- display$keep_square
    trajectory_projection_sent(FALSE)
  },
  ignoreInit = TRUE
)

## Point size and opacity are renderer patches, as on Spatial: changing them
## must not resample or rebuild the frame. Other pages' edits reach this view
## through the shared scatter defaults and the next render.
observeEvent(
  list(
    trajectory_projection_appearance$point_size,
    trajectory_projection_appearance$point_opacity
  ),
  {
    req(trajectory_projection_active(), isTRUE(trajectory_projection_sent()))
    identity <- viewerDatasetIdentity()
    req(identity$fingerprint)
    session$sendCustomMessage(
      "cell_view_appearance",
      list(
        id = "trajectory_projection",
        dataset_context = viewer_loaded_dataset_context(),
        dataset_fingerprint = identity$fingerprint,
        values = list(
          point_size = trajectory_projection_appearance$point_size,
          point_opacity = trajectory_projection_appearance$point_opacity
        )
      )
    )
  },
  ignoreInit = TRUE
)

observe({
  ## Shared point defaults change from other pages; do not rebuild and send
  ## this page's frame in the background.
  req(trajectory_projection_active())
  render_request <- input[["trajectory_projection_render_request"]]
  req(
    is.list(render_request),
    viewerDatasetContextEqual(
      render_request$dataset_context,
      viewer_loaded_dataset_context()
    )
  )
  current_context <- viewer_loaded_dataset_context()

  ## resolve current reset_axes, then clear it so only a trajectory switch (not
  ## a colour / point-size tweak) triggers the next autorange.
  reset_axes_now <- isolate(
    trajectory_projection_parameters_other[["reset_axes"]]
  )
  trajectory_projection_parameters_other[["reset_axes"]] <- FALSE

  static_frame <- trajectory_static_first_frame()
  if (!is.null(static_frame)) {
    resource <- static_frame$resource
    point_line <- if (trajectory_projection_appearance$draw_border) {
      list(color = cerebro_plotly_theme()$axis, width = 1)
    } else {
      list()
    }
    meta <- list(
      color_type = "categorical",
      color_variable = static_frame$color_variable,
      traces = as.list(static_frame$levels),
      appearance = list(
        group_labels = isTRUE(trajectory_projection_appearance$group_labels),
        draw_border = isTRUE(trajectory_projection_appearance$draw_border),
        keep_square = isTRUE(trajectory_projection_appearance$keep_square)
      ),
      space_label = input[["trajectory_selected_name"]]
    )
    primary <- list(
      color = as.list(unname(static_frame$colors)),
      point_size = isolate(trajectory_projection_appearance$point_size),
      point_opacity = isolate(trajectory_projection_appearance$point_opacity),
      point_line = point_line,
      x_range = list(),
      y_range = list(),
      reset_axes = reset_axes_now,
      trajectory_frame_resource = resource,
      deferred_selection_lengths = resource$cells
    )
    deferred_aux <- function() {
      hover_cells <- trajectory_cells_reactive(
        c("nUMI", "nGene", getGroups()),
        barcodes = TRUE
      )
      if (nrow(hover_cells) != resource$cells) {
        stop("Trajectory Viewer Pack frame no longer matches trajectory rows.")
      }
      hover_columns <- list()
      if (static_frame$hover) {
        state <- as.character(hover_cells[["state"]])
        state[is.na(state)] <- "NA"
        state_levels <- unique(state)
        hover_columns <- c(
          cerebroProjectionHoverColumns(hover_cells),
          list(
            list(
              label = "State",
              levels = state_levels,
              values = match(state, state_levels) - 1L
            ),
            list(
              label = "Pseudotime",
              format = "fixed",
              digits = 2L,
              values = unname(as.numeric(hover_cells[["pseudotime"]]))
            )
          )
        )
      }
      cerebroCellViewDeferredAux(
        selection_rows = seq_len(resource$cells),
        cell_barcodes = as.character(hover_cells[["cell_barcode"]]),
        hover_columns = hover_columns,
        hover = static_frame$hover
      )
    }
    cerebroCellViewRender(
      "trajectory_projection",
      meta,
      primary,
      list(hoverinfo = "skip"),
      extra = list(shapes = static_frame$trajectory_lines),
      deferred_aux = deferred_aux,
      dataset_context = current_context,
      feedback = cerebroFeedbackControls(
        "trajectory_projection",
        input,
        list(
          trajectory_point_size = isolate(trajectory_projection_appearance$point_size),
          trajectory_point_opacity = isolate(trajectory_projection_appearance$point_opacity),
          trajectory_percentage_cells_to_show = trajectory_projection_appearance$percentage_cells_to_show
        )
      )
    )
    if (!isolate(trajectory_projection_sent())) {
      session$onFlushed(
        function() trajectory_projection_sent(TRUE),
        once = TRUE
      )
    }
    return()
  }

  prepared <- trajectory_projection_prepared()
  req(
    prepared,
    viewerDatasetContextEqual(prepared$dataset_context, current_context)
  )

  cells_df <- prepared[["cells_df"]]
  color_variable <- prepared[["color_variable"]]
  if (
    identical(color_variable, "state") &&
      is.numeric(cells_df[[color_variable]])
  ) {
    cells_df[[color_variable]] <- factor(cells_df[[color_variable]])
  }
  ## The projection coordinates come directly from the compact trajectory meta.
  coordinates <- list(cells_df[["DR_1"]], cells_df[["DR_2"]])
  color_input <- cells_df[[color_variable]]

  point_line <- if (prepared[["draw_border"]]) {
    list(color = cerebro_plotly_theme()$axis, width = 1)
  } else {
    list()
  }

  color_assignments <- if (nrow(cells_df) == 0L) {
    character(0)
  } else {
    NULL
  }
  if (nrow(cells_df) > 0L && !is.numeric(color_input)) {
    color_assignments <- assignColorsToGroups(cells_df, color_variable)
    ## Fall back to the default colourset if the variable is not pre-assigned.
    if (is.null(color_assignments)) {
      levels_here <- unique(as.character(color_input))
      color_assignments <- stats::setNames(
        cerebro_group_colors(length(levels_here)),
        levels_here
      )
    }
  }

  payload <- cerebroCellViewScatterPayload(
    coordinates = coordinates,
    color = color_input,
    color_variable = color_variable,
    selection_keys = seq_len(nrow(cells_df)),
    ## Point size and opacity are patched in place; send their current values
    ## rather than those cached with the prepared frame.
    point_size = isolate(trajectory_projection_appearance$point_size),
    point_opacity = isolate(trajectory_projection_appearance$point_opacity),
    group_labels = isolate(trajectory_projection_appearance$group_labels),
    keep_square = prepared[["keep_square"]],
    point_line = point_line,
    reset_axes = reset_axes_now,
    color_assignments = color_assignments,
    hover_columns = list(),
    hover = FALSE,
    space_label = input[["trajectory_selected_name"]]
  )
  selection_rows <- payload$data$selection_key
  deferred_aux <- function() {
    hover_cells <- trajectory_cells_reactive(
      c("nUMI", "nGene", getGroups()),
      barcodes = TRUE
    )
    hover_cells <- hover_cells[
      match(cells_df[["cell_index"]], hover_cells[["cell_index"]]),
      ,
      drop = FALSE
    ]
    cell_barcodes <- as.character(hover_cells[["cell_barcode"]])
    hover_columns <- list()
    if (prepared[["hover"]]) {
      state <- as.character(hover_cells[["state"]])
      state[is.na(state)] <- "NA"
      state_levels <- unique(state)
      hover_columns <- c(
        cerebroProjectionHoverColumns(hover_cells),
        list(
          list(
            label = "State",
            levels = state_levels,
            values = match(state, state_levels) - 1L
          ),
          list(
            label = "Pseudotime",
            format = "fixed",
            digits = 2L,
            values = unname(as.numeric(hover_cells[["pseudotime"]]))
          )
        )
      )
    }
    cerebroCellViewDeferredAux(
      selection_rows = selection_rows,
      cell_barcodes = cell_barcodes,
      hover_columns = hover_columns,
      hover = prepared[["hover"]]
    )
  }
  cerebroCellViewRender(
    "trajectory_projection",
    payload[["meta"]],
    payload[["data"]],
    payload[["hover"]],
    extra = list(shapes = prepared[["trajectory_lines"]]),
    deferred_aux = deferred_aux,
    dataset_context = prepared[["dataset_context"]],
    feedback = utils::modifyList(prepared[["feedback"]], list(
      trajectory_point_size = as.character(
        isolate(trajectory_projection_appearance$point_size)
      ),
      trajectory_point_opacity = as.character(
        isolate(trajectory_projection_appearance$point_opacity)
      )
    ))
  )
  if (!isolate(trajectory_projection_sent())) {
    session$onFlushed(
      function() trajectory_projection_sent(TRUE),
      once = TRUE
    )
  }
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(
  input,
  "trajectory_projection_info",
  trajectory_projection_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_info <- list(
  title = "Trajectory",
  text = p(
    "Cells are coloured by the selected metadata. The black line shows the trajectory; pseudotime and state describe each cell's position along it."
  )
)

##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (from the persistent selection).
##----------------------------------------------------------------------------##
trajectory_projection_selected_cells <- reactive({
  req(trajectory_selection_ok())

  ## The selection is held persistently on the JS side (shared
  ## cell_views.js) and pushed here as {x, y, ids} under
  ## <plot_id>_persistent_selection, so it survives plot-parameter changes.
  ## The identifier matches how the selected-cells table keys cells
  ## (paste0 of the two projection coordinates with '-').
  sel <- input[["trajectory_projection_persistent_selection"]]
  if (
    !is.null(sel) &&
      (!is.list(sel) ||
        !viewerDatasetContextEqual(
          sel[["dataset_context"]],
          viewer_loaded_dataset_context()
        ))
  ) {
    return(NULL)
  }
  selection <- viewerPersistentSelection(sel, viewer_loaded_dataset_context())
  if (is.null(selection)) {
    return(NULL)
  }

  ## Drop cells whose group is currently hidden via the legend, so the count and
  ## the selected-cells panels reflect only visible groups (shared helper in
  ## utility_functions.R). Coordinates come from the trajectory's DR_1 / DR_2,
  ## keyed the same way as the selection and the selected-cells table.
  hidden_request <- input[["trajectory_projection_hidden_groups"]]
  hidden_groups <- if (
    is.list(hidden_request) &&
      viewerDatasetContextEqual(
        hidden_request[["dataset_context"]],
        viewer_loaded_dataset_context()
      )
  ) {
    as.character(hidden_request[["groups"]] %||% character())
  } else {
    character()
  }
  if (length(hidden_groups) > 0) {
    color_variable <- input[["trajectory_point_color"]]
    metadata <- trajectory_cells_reactive(
      color_variable,
      barcodes = TRUE
    )
    metadata[["selection_key"]] <- as.character(metadata[["cell_barcode"]])
    stable_selection <- "selection_key" %in%
      colnames(metadata) &&
      "selection_key" %in% colnames(selection) &&
      any(
        !is.na(selection[["selection_key"]]) &
          nzchar(as.character(selection[["selection_key"]]))
      )
    if (!stable_selection) {
      metadata[["identifier"]] <- paste0(
        metadata[["DR_1"]],
        "-",
        metadata[["DR_2"]]
      )
    }
    selection <- filterSelectionByHiddenGroups(
      selection,
      metadata,
      color_variable,
      hidden_groups
    )
    if (is.null(selection) || nrow(selection) == 0) {
      return(NULL)
    }
  }

  selection
})

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##

output[["trajectory_number_of_selected_cells"]] <- renderUI({
  selection <- trajectory_projection_selected_cells()
  cerebroSelectionSummary(
    selection,
    input[["trajectory_selected_name"]],
    input[["trajectory_point_color"]],
    ## Match Gene's empty-selection path: only the validated count is needed.
    metadata = if (is.null(selection)) {
      viewerProjectionFirstFrameMetadata()
    } else {
      getMetaData()
    }
  )
})

output[["trajectory_projection_composition"]] <- renderUI({
  cerebroSelectionSummary(
    trajectory_projection_selected_cells(),
    input[["trajectory_selected_name"]],
    input[["trajectory_point_color"]],
    composition = TRUE
  )
})
