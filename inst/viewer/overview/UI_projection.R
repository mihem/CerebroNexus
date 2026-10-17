##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##
overviewProjectionUI <- function() {
  tagList(
    cerebroVizPageHeader(
      "Projection",
      "overview_projection_info",
      "Explore cells in dimensional-reduction space and colour them by registered groups."
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
          cerebroToolbarActions(
            cerebroSettingsButton(
              "overview_projection_more_button",
              "overview_projection_more"
            ),
            cerebroShareButton("overview_projection")
          ),
          cerebroSettingsDrawer(
            "overview_projection_more",
            cerebroSettingsSection(
              "Appearance",
              tagList(
                uiOutput("overview_projection_additional_parameters_UI"),
                uiOutput("overview_projection_show_group_label_UI"),
                uiOutput("overview_projection_point_border_UI"),
                uiOutput("overview_projection_keep_square_UI")
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
          ),
          cerebroSelectionStatus(
            "overview_projection",
            "overview_number_of_selected_cells",
            portable = FALSE
          )
        )
      ),
      ## plot
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-col",
        cerebroCellViewOutput("overview_projection")
      )
    )
  )
}
