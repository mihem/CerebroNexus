##----------------------------------------------------------------------------##
## Tab: Trajectory — projection plot.
##
## Rendered through the SHARED projection-scatter system (the empty-skeleton +
## JS-observer model spatial/overview/gene_expression use), so the trajectory
## projection gets the same custom top legend, persistent x|y selection,
## group labels and modebar-off look. The trajectory path itself is drawn as a
## layout `shapes` overlay (black line segments), passed as the tab-specific
## `extra.shapes`. Colouring is categorical (state / metadata) or continuous
## (pseudotime / numeric).
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Empty plotly skeleton; the shared JS observer fills it via Plotly.react.
##----------------------------------------------------------------------------##
output[["trajectory_projection"]] <- plotly::renderPlotly({
  plotly::plot_ly(
    type = "scattergl",
    mode = "markers",
    source = "trajectory_projection"
  ) %>%
    plotly::layout(
      xaxis = list(
        autorange = TRUE,
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(
        autorange = TRUE,
        mirror = TRUE,
        showline = TRUE,
        zeroline = FALSE
      )
    )
})

##----------------------------------------------------------------------------##
## Reactive that prepares the cells + trajectory-line data for the current
## parameters (filtering, subsetting, hover, colours). One source of truth so
## the coordinates sent to the plot match those used for selection and hover.
##----------------------------------------------------------------------------##
trajectory_projection_prepared <- reactive({
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_color"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )

  trajectory_data <- trajectory_data_reactive()

  ## build data frame with data
  cells_df <- mergeTrajectoryWithMetaData(trajectory_data) %>%
    dplyr::filter(!is.na(pseudotime))

  ## available group filters
  group_filters <- names(input)[grepl(
    names(input),
    pattern = "trajectory_projection_group_filter_"
  )]

  ## remove cells based on group filters
  keep_cells <- rep(TRUE, nrow(cells_df))
  for (i in group_filters) {
    group <- strsplit(i, split = "trajectory_projection_group_filter_")[[1]][2]
    if (group %in% colnames(cells_df)) {
      keep_cells <- keep_cells & (cells_df[[group]] %in% input[[i]])
    }
  }
  cells_df <- cells_df[keep_cells, ]

  ## randomly remove cells (if necessary)
  cells_df <- randomlySubsetCells(
    cells_df,
    input[["trajectory_percentage_cells_to_show"]]
  )

  ## Empty-state guard: no cells after filtering.
  if (nrow(cells_df) == 0) {
    return(NULL)
  }

  ## put rows in random order (so no group is drawn systematically on top)
  cells_df <- cells_df[sample(seq_len(nrow(cells_df))), ]

  ## trajectory path as line-segment shapes (black), drawn under the points
  trajectory_edges <- trajectory_data[["edges"]]
  trajectory_lines <- lapply(seq_len(nrow(trajectory_edges)), function(i) {
    list(
      type = "line",
      line = list(color = "black", width = 1),
      xref = "x",
      yref = "y",
      x0 = trajectory_edges$source_dim_1[i],
      y0 = trajectory_edges$source_dim_2[i],
      x1 = trajectory_edges$target_dim_1[i],
      y1 = trajectory_edges$target_dim_2[i]
    )
  })

  ## hover info: cell + metadata + state + pseudotime
  hover_info <- buildHoverInfoForProjections(cells_df)
  hover_info <- glue::glue(
    "{hover_info}<br>",
    "<b>State</b>: {cells_df$state}<br>",
    "<b>Pseudotime</b>: {formatC(cells_df$pseudotime, format = 'f', digits = 2)}"
  )

  list(
    cells_df = cells_df,
    trajectory_lines = trajectory_lines,
    hover_info = as.character(hover_info),
    color_variable = input[["trajectory_point_color"]],
    point_size = input[["trajectory_point_size"]],
    point_opacity = input[["trajectory_point_opacity"]]
  )
})

##----------------------------------------------------------------------------##
## Observer that pushes the prepared data to the shared JS renderer.
##----------------------------------------------------------------------------##
observeEvent(trajectory_projection_prepared(), {
  prepared <- trajectory_projection_prepared()
  req(prepared)

  cells_df <- prepared[["cells_df"]]
  color_variable <- prepared[["color_variable"]]
  ## The projection coordinates are the DR_1 / DR_2 columns contributed by the
  ## trajectory meta (mergeTrajectoryWithMetaData appends them after the cell
  ## metadata, so they are NOT columns 1/2).
  coordinates <- list(cells_df[["DR_1"]], cells_df[["DR_2"]])
  color_input <- cells_df[[color_variable]]

  container_dimensions <- shinyjs::js$trajectoryGetContainerDimensions()
  container_info <- list(
    width = container_dimensions[["width"]],
    height = container_dimensions[["height"]]
  )

  point_line <- list(color = "rgb(196,196,196)", width = 1)

  ## continuous colouring (pseudotime / numeric metadata)
  if (is.numeric(color_input)) {
    output_meta <- list(
      color_type = "continuous",
      traces = color_variable,
      color_variable = color_variable
    )
    output_data <- list(
      x = coordinates[[1]],
      y = coordinates[[2]],
      color = color_input,
      point_size = prepared[["point_size"]],
      point_opacity = prepared[["point_opacity"]],
      point_line = point_line,
      x_range = list(),
      y_range = list(),
      reset_axes = TRUE
    )
    output_hover <- list(hoverinfo = "text", text = prepared[["hover_info"]])
    shinyjs::js$trajectoryUpdatePlot2DContinuous(
      output_meta,
      output_data,
      output_hover,
      list(),
      container_info,
      prepared[["trajectory_lines"]]
    )

    ## categorical colouring (state / character/factor metadata)
  } else {
    color_assignments <- assignColorsToGroups(cells_df, color_variable)
    ## Fall back to the default colourset if the variable is not pre-assigned.
    if (is.null(color_assignments)) {
      levels_here <- unique(as.character(color_input))
      color_assignments <- stats::setNames(
        default_colorset[seq_along(levels_here)],
        levels_here
      )
    }

    output_meta <- list(
      color_type = "categorical",
      traces = list(),
      color_variable = color_variable
    )
    output_data <- list(
      x = list(),
      y = list(),
      z = list(),
      color = list(),
      point_size = prepared[["point_size"]],
      point_opacity = prepared[["point_opacity"]],
      point_line = point_line,
      x_range = list(),
      y_range = list(),
      reset_axes = TRUE
    )
    output_hover <- list(hoverinfo = "text", text = list())

    cells_by_group <- split(seq_along(color_input), as.character(color_input))
    i <- 1
    for (j in names(color_assignments)) {
      cells_to_extract <- cells_by_group[[j]]
      if (is.null(cells_to_extract)) {
        next
      }
      output_meta[["traces"]][[i]] <- j
      output_data[["x"]][[i]] <- coordinates[[1]][cells_to_extract]
      output_data[["y"]][[i]] <- coordinates[[2]][cells_to_extract]
      output_data[["color"]][[i]] <- unname(color_assignments[[j]])
      output_hover[["text"]][[i]] <- prepared[["hover_info"]][cells_to_extract]
      i <- i + 1
    }

    ## group-centre labels
    coords_df <- data.frame(
      x = coordinates[[1]],
      y = coordinates[[2]]
    )
    group_centers_df <- centerOfGroups(coords_df, cells_df, 2, color_variable)
    output_group_centers <- list(
      group = group_centers_df[["group"]],
      x = group_centers_df[["x_median"]],
      y = group_centers_df[["y_median"]]
    )

    shinyjs::js$trajectoryUpdatePlot2DCategorical(
      output_meta,
      output_data,
      output_hover,
      output_group_centers,
      container_info,
      prepared[["trajectory_lines"]]
    )
  }
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_projection_info"]], {
  showModal(
    modalDialog(
      trajectory_projection_info[["text"]],
      title = trajectory_projection_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_projection_info <- list(
  title = "Trajectory",
  text = p(
    "This plot shows cells projected into trajectory space, colored by the specified meta info, e.g. sample or cluster. The path of the trajectory is shown as a black line. Specific to this analysis, every cell has a 'pseudotime' and a transcriptional 'state' which corresponds to its position along the trajectory path."
  )
)

##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (from the persistent selection).
##----------------------------------------------------------------------------##
trajectory_projection_selected_cells <- reactive({
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )

  ## The selection is held persistently on the JS side (shared
  ## projection_scatter.js) and pushed here as {x, y} under
  ## <plot_id>_persistent_selection, so it survives plot-parameter changes.
  ## The identifier matches how the selected-cells table keys cells
  ## (paste0 of the two projection coordinates with '-').
  sel <- input[["trajectory_projection_persistent_selection"]]
  if (is.null(sel) || is.null(sel[["x"]]) || length(sel[["x"]]) == 0) {
    return(NULL)
  }
  data.frame(
    x = as.numeric(sel[["x"]]),
    y = as.numeric(sel[["y"]]),
    identifier = paste0(as.numeric(sel[["x"]]), '-', as.numeric(sel[["y"]])),
    stringsAsFactors = FALSE
  )
})

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##

output[["trajectory_number_of_selected_cells"]] <- renderText({
  if (is.null(trajectory_projection_selected_cells())) {
    number_of_selected_cells <- 0
  } else {
    number_of_selected_cells <- formatC(
      nrow(trajectory_projection_selected_cells()),
      format = "f",
      big.mark = ",",
      digits = 0
    )
  }
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})

##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##
