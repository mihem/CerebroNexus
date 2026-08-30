##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##
output[["overview_projection_UI"]] <- renderUI({
  tagList(
    cerebroVizPageHeader(
      "Projection",
      "overview_projection_info",
      "Explore cells in dimensional-reduction space and colour them by metadata."
    ),
    fluidRow(
      class = "cerebro-viz-row cerebro-viz-top-layout",
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-toolbar-col",
        div(
          class = "cerebro-viz-toolbar",
          div(
            class = "cerebro-viz-primary",
            uiOutput("overview_projection_main_parameters_UI")
          ),
          cerebroSettingsButton(
            "overview_projection_more_button",
            "overview_projection_more"
          ),
          cerebroSettingsDrawer(
            "overview_projection_more",
            cerebroSettingsSection(
              "Appearance",
              tagList(
                uiOutput("overview_projection_additional_parameters_UI"),
                uiOutput("overview_projection_show_group_label_UI"),
                uiOutput("overview_projection_point_border_UI"),
                uiOutput("overview_projection_scales_UI")
              ),
              cerebroInfoButton(
                "overview_projection_additional_parameters_info"
              )
            ),
            cerebroSettingsSection(
              "Data",
              uiOutput("overview_projection_data_parameters_UI")
            ),
            cerebroSettingsSection(
              "Group filters",
              uiOutput("overview_projection_group_filters_UI"),
              cerebroInfoButton("overview_projection_group_filters_info")
            )
          )
        )
      ),
      ## plot
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-col",
        cerebroSelectionStatus(
          "overview_projection",
          "overview_number_of_selected_cells"
        ),
        cerebroCellViewOutput("overview_projection")
      )
    )
  )
})
