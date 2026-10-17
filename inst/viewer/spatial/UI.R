##----------------------------------------------------------------------------##
## Tab: Spatial
##----------------------------------------------------------------------------##
## Drawing and image placement live in the app-wide cell-view engine. Spatial
## keeps only its page-specific scroll indicator.
js_code_spatial_page <- cerebro_read_file(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/spatial/js_page_helpers.js"
  )
)

tab_spatial <- tabItem(
  tabName = "spatial",
  ## necessary to ensure alignment of table headers and content
  shinyjs::inlineCSS(
    "
    #spatial_details_selected_cells_table .table th {
      text-align: center;
    }
    #spatial_details_selected_cells_table .dt-middle {
      vertical-align: middle;
    }

    "
  ),
  shinyjs::extendShinyjs(
    text = js_code_spatial_page,
    functions = c("showScrollDownIndicator", "hideScrollDownIndicator")
  ),
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
          uiOutput("spatial_projection_main_parameters_UI"),
          div(
            class = "cerebro-spatial-background-row",
            uiOutput("spatial_projection_background_selector_UI"),
            cerebroToolbarActions(
              cerebroSettingsButton(
                "spatial_projection_more_button",
                "spatial_projection_more"
              ),
              cerebroShareButton("spatial_projection")
            )
          )
        ),
        cerebroSettingsDrawer(
          "spatial_projection_more",
          cerebroSettingsSection(
            "Appearance",
            tagList(
              uiOutput("spatial_projection_scatter_parameters_UI"),
              uiOutput("spatial_projection_show_group_label_UI"),
              uiOutput("spatial_projection_keep_square_UI"),
              uiOutput("spatial_projection_point_border_UI"),
              uiOutput("spatial_projection_show_region_outline_UI")
            ),
            cerebroInfoButton(
              "spatial_projection_additional_parameters_info"
            )
          ),
          cerebroSettingsSection(
            "Data",
            uiOutput("spatial_projection_data_parameters_UI")
          ),
          cerebroSettingsSection(
            "Background image",
            div(
              class = "spatial-image-controls",
              uiOutput("spatial_projection_background_parameters_UI")
            )
          ),
          cerebroSettingsSection(
            "Group filters",
            uiOutput("spatial_projection_group_filters_UI"),
            cerebroInfoButton("spatial_projection_group_filters_info")
          )
        ),
        cerebroSelectionStatus(
          "spatial_projection",
          "spatial_number_of_selected_cells",
          portable = FALSE
        )
      )
    ),
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-col",
      div(
        class = "spatial-viz-surface",
        conditionalPanel(
          condition = "input.spatial_projection_plot_type == 'ImageFeaturePlot'",
          tags$div(
            class = "spatial-moran-overlay",
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
  ),
  uiOutput("spatial_selected_cells_plot_UI"),
  uiOutput("spatial_selected_cells_table_UI")
)
