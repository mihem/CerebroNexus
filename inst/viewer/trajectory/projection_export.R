observeEvent(input[["trajectory_projection_export"]], {
  ##
  req(
    trajectory_selection_ok(),
    input[["trajectory_point_color"]],
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )

  ## open dialog to select where plot should be saved and how the file should
  ## be named
  shinyFiles::shinyFileSave(
    input,
    id = "trajectory_projection_export",
    roots = available_storage_volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  ## retrieve info from dialog
  save_file_input <- shinyFiles::parseSavePath(
    available_storage_volumes,
    input[["trajectory_projection_export"]]
  )

  ## only proceed if a path has been provided
  if (nrow(save_file_input) > 0) {
    ## extract specified file path
    save_file_path <- as.character(save_file_input$datapath[1])

    ## ggplot2 functions are necessary to create the plot
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error!",
        text = "The 'ggplot2' package is required to export trajectory plots.",
        type = "error"
      )
      return()
    }

    trajectory_data <- getTrajectory(
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]]
    )

    ## build data frame with data
    cells_df <- mergeTrajectoryWithMetaData(trajectory_data) %>%
      dplyr::filter(!is.na(pseudotime))

    ## randomly remove cells (if necessary)
    cells_df <- randomlySubsetCells(
      cells_df,
      input[["trajectory_percentage_cells_to_show"]]
    )

    ## put rows in random order
    cells_df <- cells_df[sample(seq_len(nrow(cells_df))), ]

    ## start building the plot
    plot <- ggplot() +
      geom_point(
        data = cells_df,
        aes(
          x = .data[[colnames(cells_df)[1]]],
          y = .data[[colnames(cells_df)[2]]],
          fill = .data[[input[["trajectory_point_color"]]]]
        ),
        shape = 21,
        size = input[["trajectory_point_size"]] / 3,
        stroke = 0.2,
        color = "#c4c4c4",
        alpha = input[["trajectory_point_opacity"]]
      ) +
      geom_segment(
        data = trajectory_data[["edges"]],
        aes(
          source_dim_1,
          source_dim_2,
          xend = target_dim_1,
          yend = target_dim_2
        ),
        size = 0.75,
        linetype = "solid",
        na.rm = TRUE
      ) +
      cerebro_export_theme()

    ## depending on type of cell coloring, add different color scale
    ## ... categorical
    if (
      is.factor(cells_df[[input[["trajectory_point_color"]]]]) ||
        is.character(cells_df[[input[["trajectory_point_color"]]]])
    ) {
      ## get colors for groups
      colors_for_groups <- assignColorsToGroups(
        cells_df,
        input[["trajectory_point_color"]]
      )

      ## add color assignments
      plot <- plot + scale_fill_manual(values = colors_for_groups)

      ## ... not categorical (probably numerical)
    } else {
      ## add continuous color scale
      plot <- plot +
        scale_fill_distiller(
          palette = "Blues",
          direction = 1,
          guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
        )
    }

    ## save plot
    pdf(NULL)
    ggsave(save_file_path, plot, height = 8, width = 11)

    ## check if file was succesfully saved
    ## ... successful
    if (file.exists(save_file_path)) {
      ## give positive message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Success!",
        text = paste0("Plot saved successfully as: ", save_file_path),
        type = "success"
      )

      ## ... failed
    } else {
      ## give negative message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Sorry, it seems something went wrong...",
        type = "error"
      )
    }
  }
})
