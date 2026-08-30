##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##
output[["spatial_projection_UI"]] <- renderUI({
  tagList(
    cerebroVizPageHeader(
      "Spatial",
      "spatial_projection_info",
      "Explore spatial organization, expression, and tissue context."
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
            uiOutput("spatial_projection_main_parameters_UI")
          ),
          cerebroSettingsButton(
            "spatial_projection_more_button",
            "spatial_projection_more"
          ),
          cerebroSettingsDrawer(
            "spatial_projection_more",
            cerebroSettingsSection(
              "Appearance",
              tagList(
                uiOutput("spatial_projection_scatter_parameters_UI"),
                uiOutput("spatial_projection_show_group_label_UI"),
                uiOutput("spatial_projection_point_border_UI"),
                uiOutput("spatial_projection_scales_UI")
              ),
              cerebroInfoButton(
                "spatial_projection_additional_parameters_info"
              )
            ),
            cerebroSettingsSection(
              "Background image",
              tagList(
                uiOutput("spatial_projection_background_select_UI"),
                uiOutput("spatial_projection_background_parameters_UI")
              )
            ),
            cerebroSettingsSection(
              "Group filters",
              uiOutput("spatial_projection_group_filters_UI"),
              cerebroInfoButton("spatial_projection_group_filters_info")
            )
          )
        )
      ),
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-col",
        cerebroSelectionStatus(
          "spatial_projection",
          "spatial_number_of_selected_cells"
        ),
        ## Spatial autocorrelation (Moran's I) of the displayed gene,
        ## placed between the legend and the scatter.
        conditionalPanel(
          condition = "input.spatial_projection_plot_type == 'ImageFeaturePlot'",
          tags$div(
            style = paste0(
              "font-size: 12px; color: #555; margin: 0 0 4px 2px; ",
              "display: flex; align-items: center; gap: 4px;"
            ),
            tags$strong("Moran's I:"),
            textOutput("spatial_projection_morans_i", inline = TRUE),
            actionLink(
              "spatial_projection_morans_i_info",
              label = NULL,
              icon = icon("circle-info"),
              title = "What is Moran's I?",
              style = "color: #999;"
            )
          )
        ),
        cerebroCellViewOutput("spatial_projection")
      )
    )
  )
})
