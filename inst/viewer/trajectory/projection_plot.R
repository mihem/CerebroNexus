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

## Slider feedback sent with every frame. Point size and opacity are isolated
## (they are renderer patches); "% of cells" stays a dependency of the caller.
trajectory_projection_feedback <- function() {
  cerebroFeedbackControls(
    "trajectory_projection",
    input,
    list(
      trajectory_point_size = isolate(
        trajectory_projection_appearance$point_size
      ),
      trajectory_point_opacity = isolate(
        trajectory_projection_appearance$point_opacity
      ),
      trajectory_percentage_cells_to_show = trajectory_projection_appearance$percentage_cells_to_show
    )
  )
}

## The feedback a frame is sent with, with the current point size and opacity.
trajectory_projection_full_feedback <- function(feedback) {
  utils::modifyList(
    feedback,
    list(
      trajectory_point_size = as.character(
        isolate(trajectory_projection_appearance$point_size)
      ),
      trajectory_point_opacity = as.character(
        isolate(trajectory_projection_appearance$point_opacity)
      )
    )
  )
}

trajectory_projection_point_line <- function(draw_border) {
  if (draw_border) {
    list(color = cerebro_plotly_theme()$axis, width = 1)
  } else {
    list()
  }
}

## Hover columns shared by the static and the R-built frame: the projection's
## standard columns plus the trajectory state and pseudotime.
trajectory_projection_hover_columns <- function(hover_cells) {
  state <- as.character(hover_cells[["state"]])
  state[is.na(state)] <- "NA"
  state_levels <- unique(state)
  c(
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

##----------------------------------------------------------------------------##
## Reactive that prepares the cells + trajectory-line data for the current
## parameters (filtering, subsetting, hover, colours). One source of truth so
## the coordinates sent to the plot match those used for selection and hover.
##----------------------------------------------------------------------------##
## shiny::debounce() evaluates this eagerly; only the visible page may pay for it.
trajectory_projection_active <- function() {
  identical(input[["sidebar"]], "trajectory")
}
## Group filters that leave cells out. A filter that selects every level (what
## the drawer's controls bind with) is no filter, so mounting the drawer does
## not rebuild or resend the frame.
trajectory_active_group_filters <- function() {
  groups <- getGroups()
  filters <- stats::setNames(
    lapply(groups, function(group) {
      input[[paste0("trajectory_projection_group_filter_", group)]]
    }),
    groups
  )
  filters <- filters[!vapply(filters, is.null, logical(1))]
  filters <- filters[!vapply(
    names(filters),
    function(group) viewerGroupFilterStale(filters[[group]], group),
    logical(1)
  )]
  everything <- vapply(
    names(filters),
    function(group) {
      levels <- as.character(getGroupLevels(group))
      length(levels) > 0L &&
        setequal(as.character(filters[[group]]), levels)
    },
    logical(1)
  )
  filters[!everything]
}

## With a Viewer Pack the frame carries every cell and the browser hides a
## deterministic share of them, as on Overview, so the "% of cells" slider does
## not resample and resend the trajectory cells from R.
trajectory_client_percentage <- reactive({
  viewerPackCanonical(viewerPackCurrent())
})
## The share of cells R samples for the frame.
trajectory_server_percentage <- function() {
  if (isTRUE(trajectory_client_percentage())) {
    100
  } else {
    trajectory_projection_appearance$percentage_cells_to_show
  }
}
## Frame fields that hand "% of cells" to the browser.
trajectory_client_percentage_fields <- function() {
  if (!isTRUE(trajectory_client_percentage())) {
    return(list())
  }
  list(
    client_percentage = TRUE,
    percentage_cells_to_show = isolate(
      trajectory_projection_appearance$percentage_cells_to_show
    )
  )
}

## What a frame is built for. The debounced frame is only sent once it matches
## the current request; an earlier one would show the previous state.
trajectory_projection_request <- function() {
  list(
    color_variable = input[["trajectory_point_color"]],
    filters = trajectory_active_group_filters(),
    percentage = trajectory_server_percentage()
  )
}

trajectory_projection_cells_memo <- viewerMemoOne()
trajectory_projection_prepared_raw <- reactive({
  req(trajectory_projection_active())
  dataset_context <- viewer_loaded_dataset_context()
  req(
    trajectory_selection_ok(),
    input[["trajectory_point_color"]]
  )

  trajectory_data <- trajectory_data_reactive()

  request <- trajectory_projection_request()
  group_filters <- request$filters
  color_variable <- input[["trajectory_point_color"]]
  ## Sampling and shuffling are random. The same cells and request (evaluated
  ## again because a control bound with its current value) reuse the drawn
  ## cells, so the frame is identical and is not sent again. The cells are a
  ## function of the data set, the trajectory and the columns read, so those
  ## key them rather than the cells themselves, which would be read and
  ## compared in full on every evaluation.
  built <- trajectory_projection_cells_memo(
    list(
      dataset_context,
      trajectory_data,
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]],
      request,
      color_variable
    ),
    function() {
      cells_df <- trajectory_cells_reactive(c(
        color_variable,
        names(group_filters)
      ))
      keep <- cerebroGroupFilterMask(cells_df, group_filters)
      if (!all(keep)) {
        cells_df <- cells_df[keep, , drop = FALSE]
      }

      ## randomly remove cells (if necessary)
      cells_df <- randomlySubsetCells(cells_df, request$percentage)

      ## An empty frame is still sent, without trajectory lines, so clearing
      ## every filter cannot leave the previous Canvas frame visible.
      trajectory_lines <- list()
      if (nrow(cells_df) > 0L) {
        ## Categorical payloads are split into one trace per group below, so
        ## shuffling rows within a trace cannot change paint order. Continuous
        ## colours stay shuffled so the original overlap behaviour is preserved.
        if (is.numeric(cells_df[[color_variable]])) {
          cells_df <- cells_df[sample.int(nrow(cells_df)), , drop = FALSE]
        }

        ## trajectory path as line-segment shapes (warm near-black, the theme
        ## title colour), drawn under the points as the structural backbone
        trajectory_lines <- trajectory_projection_lines(trajectory_data[[
          "edges"
        ]])
      }
      list(cells_df = cells_df, trajectory_lines = trajectory_lines)
    }
  )
  cells_df <- built$cells_df
  trajectory_lines <- built$trajectory_lines

  list(
    request = request,
    dataset_context = dataset_context,
    feedback = trajectory_projection_feedback(),
    cells_df = cells_df,
    trajectory_lines = trajectory_lines,
    hover = isTRUE(preferences[["show_hover_info_in_projections"]]),
    color_variable = color_variable,
    point_size = isolate(trajectory_projection_appearance$point_size),
    point_opacity = isolate(trajectory_projection_appearance$point_opacity),
    draw_border = isolate(trajectory_projection_appearance$draw_border),
    keep_square = isolate(trajectory_projection_appearance$keep_square)
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
## The render request and frame last sent. The observer also re-runs when a
## control it reads binds with the value it already used (the drawer's group
## filters); the same frame for the same request is not sent again.
## The key is recorded once the frame is sent, so a frame that failed is sent
## again on the next run.
trajectory_projection_last <- new.env(parent = emptyenv())
trajectory_projection_frame_key <- function(render_request, frame) {
  list(render_request = render_request, frame = frame)
}

## The default million-cell frame is already stored in the Viewer Pack in the
## exact trajectory row order. Keep this gate deliberately narrow: any filter,
## sampling, non-state colour or failed resource uses
## the established R construction path below.
trajectory_static_first_frame <- reactive({
  static_frame <- trajectory_static_frame()
  if (
    length(trajectory_active_group_filters()) &&
      is.null(trajectory_client_filters())
  ) {
    return(NULL)
  }
  static_frame
})
## The group filters for the browser to apply to the Viewer Pack frame, which
## holds every trajectory cell and maps them to the Pack's codes (NULL: R
## builds and filters the frame).
trajectory_client_filters <- viewerClientFilters(
  "trajectory_projection",
  active = function() trajectory_projection_active(),
  client = function() !is.null(trajectory_static_frame()),
  filters = function() trajectory_active_group_filters()
)
## The Viewer Pack frame for every trajectory cell, filters aside.
trajectory_static_frame <- reactive({
  req(trajectory_selection_ok())
  color_variable <- input[["trajectory_point_color"]]
  groups <- getGroups()
  if (
    !(identical(color_variable, "state") ||
      isTRUE(color_variable %in% groups)) ||
      !isTRUE(all.equal(as.numeric(trajectory_server_percentage()), 100))
  ) {
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
    if (
      !identical(as.character(resource$geometry_kind), "canonical_projection")
    ) {
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

  ## "% of cells" reaches R first as the shared scatter edit (which may or may
  ## not draw a frame) and then as the slider's own debounced value. Re-run on
  ## the latter, so the drawn frame acknowledges it.
  input[["trajectory_percentage_cells_to_show"]]
  static_frame <- trajectory_static_first_frame()
  if (!is.null(static_frame)) {
    feedback <- trajectory_projection_feedback()
    key <- trajectory_projection_frame_key(render_request, static_frame)
    if (identical(key, trajectory_projection_last$key)) {
      ## "% of cells" applied by the browser: acknowledge it on the drawn frame.
      if (
        !is.null(trajectory_projection_last$token) &&
          !identical(feedback, trajectory_projection_last$feedback)
      ) {
        cerebroSendViewFeedback(
          "trajectory_projection",
          trajectory_projection_last$token,
          current_context,
          feedback
        )
        trajectory_projection_last$feedback <- feedback
      }
      return()
    }
    resource <- static_frame$resource
    meta <- list(
      color_type = "categorical",
      color_variable = static_frame$color_variable,
      traces = as.list(static_frame$levels),
      appearance = isolate(list(
        group_labels = isTRUE(trajectory_projection_appearance$group_labels),
        draw_border = isTRUE(trajectory_projection_appearance$draw_border),
        keep_square = isTRUE(trajectory_projection_appearance$keep_square)
      )),
      space_label = input[["trajectory_selected_name"]]
    )
    primary <- list(
      color = as.list(unname(static_frame$colors)),
      point_size = isolate(trajectory_projection_appearance$point_size),
      point_opacity = isolate(trajectory_projection_appearance$point_opacity),
      point_line = trajectory_projection_point_line(
        isolate(trajectory_projection_appearance$draw_border)
      ),
      x_range = list(),
      y_range = list(),
      reset_axes = reset_axes_now,
      trajectory_frame_resource = resource,
      deferred_selection_lengths = resource$cells
    )
    primary <- c(primary, trajectory_client_percentage_fields())
    deferred_aux <- function() {
      ## Without hover only the rows are needed, not a million-row table of
      ## every group.
      hover_cells <- trajectory_cells_reactive(
        if (static_frame$hover) c("nUMI", "nGene", getGroups())
      )
      if (nrow(hover_cells) != resource$cells) {
        stop("Trajectory Viewer Pack frame no longer matches trajectory rows.")
      }
      ## Named by canonical row when the cell-name file covers them.
      names_resource <- viewerCellNamesResourceFor(hover_cells[["cell_index"]])
      cerebroCellViewDeferredAux(
        selection_rows = seq_len(resource$cells),
        cell_barcodes = if (is.null(names_resource)) {
          viewerCellBarcodesAt(hover_cells[["cell_index"]])
        },
        hover_columns = if (static_frame$hover) {
          trajectory_projection_hover_columns(hover_cells)
        } else {
          list()
        },
        hover = static_frame$hover,
        names_resource = names_resource,
        cell_rows = hover_cells[["cell_index"]]
      )
    }
    trajectory_projection_parameters_other[["reset_axes"]] <- FALSE
    trajectory_projection_last$token <- cerebroCellViewRender(
      "trajectory_projection",
      meta,
      primary,
      list(hoverinfo = "skip"),
      extra = list(shapes = static_frame$trajectory_lines),
      deferred_aux = deferred_aux,
      dataset_context = current_context,
      feedback = feedback
    )
    trajectory_projection_last$key <- key
    trajectory_projection_last$feedback <- feedback
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
  ## Wait for the debounced frame of this request; it re-runs this observer.
  if (!identical(prepared$request, trajectory_projection_request())) {
    return()
  }
  ## Control values are not part of the frame: when only they changed (the
  ## drawer's group filters binding), the drawn frame acknowledges them.
  feedback <- trajectory_projection_full_feedback(
    trajectory_projection_feedback()
  )
  key <- trajectory_projection_frame_key(
    render_request,
    prepared[setdiff(names(prepared), "feedback")]
  )
  if (identical(key, trajectory_projection_last$key)) {
    if (
      !is.null(trajectory_projection_last$token) &&
        !identical(feedback, trajectory_projection_last$feedback)
    ) {
      cerebroSendViewFeedback(
        "trajectory_projection",
        trajectory_projection_last$token,
        current_context,
        feedback
      )
      trajectory_projection_last$feedback <- feedback
    }
    return()
  }

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
    point_line = trajectory_projection_point_line(prepared[["draw_border"]]),
    reset_axes = reset_axes_now,
    color_assignments = color_assignments,
    hover_columns = list(),
    hover = FALSE,
    space_label = input[["trajectory_selected_name"]]
  )
  payload$data <- c(payload$data, trajectory_client_percentage_fields())
  selection_rows <- payload$data$selection_key
  deferred_aux <- function() {
    hover_cells <- trajectory_cells_reactive(
      if (prepared[["hover"]]) c("nUMI", "nGene", getGroups())
    )
    hover_cells <- hover_cells[
      match(cells_df[["cell_index"]], hover_cells[["cell_index"]]),
      ,
      drop = FALSE
    ]
    ## Named by canonical row when the cell-name file covers them.
    names_resource <- viewerCellNamesResourceFor(hover_cells[["cell_index"]])
    cerebroCellViewDeferredAux(
      selection_rows = selection_rows,
      cell_barcodes = if (is.null(names_resource)) {
        viewerCellBarcodesAt(hover_cells[["cell_index"]])
      },
      hover_columns = if (prepared[["hover"]]) {
        trajectory_projection_hover_columns(hover_cells)
      } else {
        list()
      },
      hover = prepared[["hover"]],
      names_resource = names_resource,
      cell_rows = hover_cells[["cell_index"]]
    )
  }
  trajectory_projection_parameters_other[["reset_axes"]] <- FALSE
  trajectory_projection_last$token <- cerebroCellViewRender(
    "trajectory_projection",
    payload[["meta"]],
    payload[["data"]],
    payload[["hover"]],
    extra = list(shapes = prepared[["trajectory_lines"]]),
    deferred_aux = deferred_aux,
    dataset_context = prepared[["dataset_context"]],
    feedback = feedback
  )
  trajectory_projection_last$key <- key
  trajectory_projection_last$feedback <- feedback
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
  ## the selected-cells panels reflect only visible groups. Coordinates come
  ## from the trajectory's DR_1 / DR_2, keyed the same way as the selection and
  ## the selected-cells table.
  viewerVisibleSelection(
    selection,
    input[["trajectory_projection_hidden_groups"]],
    viewer_loaded_dataset_context(),
    input[["trajectory_point_color"]],
    function() {
      trajectory_cells_reactive(
        input[["trajectory_point_color"]],
        barcodes = TRUE
      )
    },
    coordinates = c("DR_1", "DR_2")
  )
})

##----------------------------------------------------------------------------##
## Number and composition of the selected cells.
##----------------------------------------------------------------------------##
viewerSelectionSummaryOutputs(
  "trajectory_projection",
  "trajectory_number_of_selected_cells",
  function() trajectory_projection_selected_cells(),
  source = function() input[["trajectory_selected_name"]],
  color = function() input[["trajectory_point_color"]]
)
