##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## States by group.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_states_by_group_UI"]] <- renderUI({
  req(input[["trajectory_states_section_visible"]])
  req(trajectory_projection_sent())
  req(trajectory_selection_ok())

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("States by group"),
        cerebroInfoButton("states_by_group_info")
      ),
      tagList(
        fluidRow(
          column(
            width = 3,
            shinyWidgets::radioGroupButtons(
              inputId = "states_by_group_plot_type",
              label = NULL,
              choices = c("Bar chart", "Sankey plot"),
              status = "primary",
              justified = TRUE,
              width = "100%",
              size = "sm"
            )
          ),
          column(
            width = 9,
            style = "padding: 5px;",
            shinyWidgets::materialSwitch(
              inputId = "states_by_group_show_as_percent",
              label = "Show composition as percent [%]:",
              status = "primary",
              inline = TRUE
            ),
            shinyWidgets::materialSwitch(
              inputId = "states_by_group_show_table",
              label = "Show table:",
              status = "primary",
              inline = TRUE
            )
          )
        ),
        tagList(
          selectInput(
            "states_by_group_select_other_group",
            label = "Group to compare to:",
            choices = c(getGroups(), getCellCycle())
          ),
          uiOutput("states_by_group_table_UI")
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## UI element that either shows just the plot or also a table.
##----------------------------------------------------------------------------##

output[["states_by_group_table_UI"]] <- renderUI({
  tagList(
    plotly::plotlyOutput("states_by_group_plot"),
    {
      if (
        !is.null(input[["states_by_group_show_table"]]) &&
          input[["states_by_group_show_table"]] == TRUE
      ) {
        DT::dataTableOutput("states_by_group_table")
      }
    }
  )
})

##----------------------------------------------------------------------------##
## Plot.
##----------------------------------------------------------------------------##

output[["states_by_group_plot"]] <- plotly::renderPlotly({
  ## wait for this input
  req(
    trajectory_selection_ok(),
    input[["states_by_group_select_other_group"]],
    input[["states_by_group_plot_type"]]
  )

  ##
  grouping_variable <- input[["states_by_group_select_other_group"]]
  cells_df <- trajectory_cells_reactive(grouping_variable)

  ## check which plot type is chosen
  ## ... bar chart
  if (input[["states_by_group_plot_type"]] == "Bar chart") {
    ##
    composition_df <- cells_df %>%
      calculateTableAB(
        "state",
        grouping_variable,
        mode = "long",
        percent = input[["states_by_group_show_as_percent"]]
      )

    ## get colors for groups
    colors_for_groups <- assignColorsToGroups(composition_df, grouping_variable)

    ## generate plot
    plotlyBarChart(
      table = composition_df,
      first_grouping_variable = "state",
      second_grouping_variable = grouping_variable,
      colors = colors_for_groups,
      percent = input[["states_by_group_show_as_percent"]]
    )

    ## ... Sankey plot
  } else if (input[["states_by_group_plot_type"]] == "Sankey plot") {
    ##
    composition_df <- cells_df %>%
      calculateTableAB(
        "state",
        grouping_variable,
        mode = "long",
        percent = FALSE
      )

    ## get color code for all group levels (from both groups)
    colors_for_groups <- c(
      assignColorsToGroups(composition_df, "state"),
      assignColorsToGroups(composition_df, grouping_variable)
    )

    ## generate plot
    plotlySankeyPlot(
      table = composition_df,
      first_grouping_variable = "state",
      second_grouping_variable = grouping_variable,
      colors_for_groups = colors_for_groups
    )
  }
})

##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##

output[["states_by_group_table"]] <- DT::renderDataTable({
  ## wait for these inputs
  req(
    trajectory_selection_ok(),
    input[["states_by_group_select_other_group"]]
  )

  ##
  grouping_variable <- input[["states_by_group_select_other_group"]]
  cells_df <- trajectory_cells_reactive(grouping_variable)

  ## generate table
  composition_df <- cells_df %>%
    calculateTableAB(
      "state",
      grouping_variable,
      mode = "wide",
      percent = input[["states_by_group_show_as_percent"]]
    )

  ## get indices of columns that should be formatted as percent
  if (input[["states_by_group_show_as_percent"]] == TRUE) {
    columns_percentage <- c(3:ncol(composition_df))
  } else {
    columns_percentage <- NULL
  }

  composition_df %>%
    dplyr::rename("# of cells" = total_cell_count) %>%
    prettifyTable(
      filter = "none",
      dom = "Brtlip",
      show_buttons = FALSE,
      number_formatting = TRUE,
      color_highlighting = FALSE,
      hide_long_columns = TRUE,
      columns_percentage = columns_percentage
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(input, "states_by_group_info", states_by_group_info)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

states_by_group_info <- list(
  title = "Composition of states by another group",
  text = p(
    "Compare trajectory states with the selected group using bars or a Sankey plot. Show the table for exact values. Percentages apply to bars and the table; Sankey links show cell counts."
  )
)
