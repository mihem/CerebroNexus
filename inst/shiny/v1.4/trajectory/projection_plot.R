output[["trajectory_projection"]] <- plotly::renderPlotly({
  ## don't do anything before these inputs are selected
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_color"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )

  withProgress(message = "Generating trajectory projection...", value = 0, {
    ## collect trajectory data
    trajectory_data <- trajectory_data_reactive()

    ## build data frame with data
    cells_df <- cbind(trajectory_data[["meta"]], getMetaData()) %>%
      dplyr::filter(!is.na(pseudotime))

    incProgress(0.2, detail = "Filtering cells...")

    ## available group filters
    group_filters <- names(input)[grepl(
      names(input),
      pattern = 'trajectory_projection_group_filter_'
    )]

    ## remove cells based on group filters
    keep_cells <- rep(TRUE, nrow(cells_df))
    for (i in group_filters) {
      group <- strsplit(i, split = 'trajectory_projection_group_filter_')[[1]][
        2
      ]
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

    ## put rows in random order
    cells_df <- cells_df[sample(1:nrow(cells_df)), ]

    incProgress(0.4, detail = "Preparing trajectory lines...")

    ## convert edges of trajectory into list format to plot with plotly
    trajectory_edges <- trajectory_data[["edges"]]

    trajectory_lines <- lapply(seq_len(nrow(trajectory_edges)), function(i) {
      list(
        type = "line",
        line = list(color = "black"),
        xref = "x",
        yref = "y",
        x0 = trajectory_edges$source_dim_1[i],
        y0 = trajectory_edges$source_dim_2[i],
        x1 = trajectory_edges$target_dim_1[i],
        y1 = trajectory_edges$target_dim_2[i]
      )
    })

    incProgress(0.6, detail = "Building plot...")

    ## prepare hover info
    hover_info <- buildHoverInfoForProjections(cells_df)

    ## add expression levels to hover info
    hover_info <- glue::glue(
      "{hover_info}
    <b>State</b>: {cells_df$state}
    <b>Pseudotime</b>: {formatC(cells_df$pseudotime, format = 'f', digits = 2)}"
    )

    ##
    if (
      is.factor(cells_df[[input[["trajectory_point_color"]]]]) ||
        is.character(cells_df[[input[["trajectory_point_color"]]]])
    ) {
      ## get colors for groups
      colors_for_groups <- assignColorsToGroups(
        cells_df,
        input[["trajectory_point_color"]]
      )

      ## Native scattergl when WebGL is enabled; SVG scatter otherwise.
      ## Replaces the former plotly::toWebGL() post-processing so the trace
      ## type is decided up-front, avoiding the trace-rewrite pass.
      scatter_type <- if (isTRUE(preferences$use_webgl)) {
        "scattergl"
      } else {
        "scatter"
      }

      plot <- plotly::plot_ly(
        cells_df,
        x = ~DR_1,
        y = ~DR_2,
        color = ~ cells_df[[input[["trajectory_point_color"]]]],
        colors = colors_for_groups,
        type = scatter_type,
        mode = "markers",
        marker = list(
          opacity = input[["trajectory_point_opacity"]],
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["trajectory_point_size"]]
        ),
        hoverinfo = "text",
        text = ~hover_info,
        source = "trajectory_projection"
      )

      ##
    } else {
      scatter_type <- if (isTRUE(preferences$use_webgl)) {
        "scattergl"
      } else {
        "scatter"
      }

      plot <- plotly::plot_ly(
        data = cells_df,
        x = ~DR_1,
        y = ~DR_2,
        type = scatter_type,
        mode = "markers",
        marker = list(
          colorbar = list(
            title = colnames(cells_df)[which(
              colnames(cells_df) == input[["trajectory_point_color"]]
            )]
          ),
          color = ~ cells_df[[input[["trajectory_point_color"]]]],
          opacity = input[["trajectory_point_opacity"]],
          colorscale = "Blues",
          reversescale = FALSE,
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["trajectory_point_size"]]
        ),
        hoverinfo = "text",
        text = ~hover_info,
        source = "trajectory_projection"
      )
    }

    ## add layout to plot
    plot <- plot %>%
      plotly::layout(
        shapes = trajectory_lines,
        xaxis = list(
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = range(cells_df$DR_1) * 1.1
        ),
        yaxis = list(
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE,
          range = range(cells_df$DR_2) * 1.1
        ),
        hoverlabel = list(
          font = list(
            size = 11
          ),
          align = 'left'
        )
      )

    ## scatter trace type already picked based on preferences$use_webgl at
    ## construction time, no post-processing needed.
    plot
  })
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
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
trajectory_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_color"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )

  ## check selection
  ## ... selection has not been made or there is no cell in it
  if (
    is.null(plotly::event_data(
      "plotly_selected",
      source = "trajectory_projection"
    )) ||
      length(plotly::event_data(
        "plotly_selected",
        source = "trajectory_projection"
      )) ==
        0
  ) {
    return(NULL)
    ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    plotly::event_data("plotly_selected", source = "trajectory_projection") %>%
      dplyr::mutate(identifier = paste0(x, '-', y)) %>%
      return()
  }
})

##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##

output[["trajectory_number_of_selected_cells"]] <- renderText({
  ## check selection
  ## ... selection has not been made or there is no cell in it
  if (is.null(trajectory_projection_selected_cells())) {
    ## manually set counter to 0
    number_of_selected_cells <- 0

    ## ... selection has been made and at least 1 cell is in it
  } else {
    ## get number of selected cells
    number_of_selected_cells <- formatC(
      nrow(trajectory_projection_selected_cells()),
      format = "f",
      big.mark = ",",
      digits = 0
    )
  }

  ## prepare string to show
  paste0("<b>Number of selected cells</b>: ", number_of_selected_cells)
})

##----------------------------------------------------------------------------##
## Export projection plot to PDF when pressing the "export to PDF" button.
##----------------------------------------------------------------------------##
