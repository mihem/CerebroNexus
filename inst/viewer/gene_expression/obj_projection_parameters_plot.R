##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
## The projection the selector starts with: the one Linked views shares, else
## the first projection, else the first trajectory.
expression_projection_default_projection <- function() {
  available_projections <- availableProjections()
  shared <- isolate(input[["coordviews_shared_base"]])
  selected_projection <- as.character(shared$projection %||% "")
  if (selected_projection %in% available_projections) {
    return(selected_projection)
  }
  if (length(available_projections)) {
    return(available_projections[[1L]])
  }
  available_trajectories <- available_trajectories()
  if (length(available_trajectories)) available_trajectories[[1L]] else NULL
}

expressionPlotOrderLabel <- function(highest_on_top) {
  if (isTRUE(highest_on_top)) "Highest expression on top" else "Input order"
}

## The display mode the selector (UI_projection_controls.R) opens on.
expression_projection_default_display_mode <- "combined"

## The sidebar's "Highest on top" (load_data/preferences.R) when it is bound,
## else the author's page default.
expression_projection_default_plotting_order <- function() {
  chosen <- isolate(input[["expression_on_top_checkbox"]])
  if (!is.null(chosen)) {
    return(isTRUE(chosen))
  }
  identical(
    current_page_display_defaults("gene")$plotting_order,
    "Highest expression on top"
  )
}

## Server code reads these controls through viewerControlValue(): the first
## frame is drawn with the values below before the controls exist.
expression_projection_projection <- viewerControlValue(
  input,
  "expression_projection_to_display",
  expression_projection_default_projection
)
expression_projection_plotting_order <- viewerControlValue(
  input,
  "expression_projection_plotting_order",
  expression_projection_default_plotting_order
)
expression_projection_display_mode <- viewerControlValue(
  input,
  "expression_projection_genes_in_separate_panels",
  function() expression_projection_default_display_mode
)

cerebroObserveLocalAppearance("expression_projection", input, session)
## The projection or trajectory drawn: the selector's value when it is one of
## them, else the one Linked views shares, else the first one.
expression_projection_selected_projection <- reactive({
  available_projections <- availableProjections()
  available_trajectory_names <- available_trajectories()
  selected_projection <- expression_projection_projection()
  if (
    is.null(selected_projection) ||
      !selected_projection %in%
        c(
          available_projections,
          available_trajectory_names
        )
  ) {
    selected_projection <- expression_projection_default_projection()
  }
  req(
    selected_projection,
    selected_projection %in%
      available_projections ||
      selected_projection %in% available_trajectory_names
  )
  selected_projection
})

expression_projection_parameters_plot <- reactive({
  selected_projection <- expression_projection_selected_projection()
  req(
    !is.null(isolate(expression_projection_plotting_order())),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  if (selected_projection %in% availableProjections()) {
    is_trajectory <- FALSE
    range_data <- viewerProjectionFirstFrameCoordinates(selected_projection)
    n_dimensions <- ncol(range_data)
  } else {
    is_trajectory <- TRUE
    # currently, only trajectories with 2 dimensions are supported
    n_dimensions <- 2
    selection <- strsplit(selected_projection, split = " // ")[[1]]
    range_data <- getTrajectory(selection[[1]], selection[[2]])[["meta"]]
  }
  XYranges <- getXYranges(range_data)
  display <- current_page_display_defaults("gene")
  parameters <- list(
    projection = selected_projection,
    ## Not a dependency: the order is a browser patch (event_projection_update_plot.R).
    plot_order = expressionPlotOrderLabel(
      isolate(expression_projection_plotting_order())
    ),
    n_dimensions = n_dimensions,
    is_trajectory = is_trajectory,
    point_size = isolate(current_scatter_defaults()$point_size),
    point_opacity = isolate(current_scatter_defaults()$point_opacity),
    draw_border = isTRUE(
      isolate(input[["expression_projection_point_border"]]) %||%
        display$draw_border
    ),
    keep_square = isTRUE(
      isolate(input[["expression_projection_keep_square"]]) %||%
        display$keep_square
    ),
    x_range = c(XYranges$x$min, XYranges$x$max),
    y_range = c(XYranges$y$min, XYranges$y$max),
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  return(parameters)
})

##
expression_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

## Reset the axes when the user picks another projection. The first value
## (the selector's default) is not a change: a stale TRUE would keep the next
## gene selection from recolouring the frame in place.
expression_projection_previous_projection <- NULL
observeEvent(expression_projection_projection(), {
  projection <- expression_projection_projection()
  if (
    !is.null(expression_projection_previous_projection) &&
      !identical(projection, expression_projection_previous_projection)
  ) {
    expression_projection_parameters_other[['reset_axes']] <- TRUE
  }
  expression_projection_previous_projection <<- projection
})
