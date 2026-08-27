##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##
output[["overview_projection_UI"]] <- renderUI({
  tagList(
    cerebroVizPageHeader(
      "Projection",
      "overview_projection_main_parameters_info",
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
        shiny::tagAppendAttributes(
          cerebroBox(
            title = tagList(
              boxTitle("Dimensional reduction"),
              cerebroInfoButton("overview_projection_info")
              #shinyFiles::shinySaveButton(
              # "overview_projection_export",
              #label = "export to PDF",
              #title = "Export dimensional reduction to PDF file.",
              #filetype = "pdf",
              #viewtype = "icon",
              #class = "btn-xs",
              #style = "margin-right: 3px"
              #)
            ),
            tagList(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  "overview_projection",
                  width = "auto",
                  height = "60vh"
                ),
                type = 8,
                hide.ui = FALSE
              ),
              tags$br(),
              fluidRow(
                column(
                  width = 8,
                  htmlOutput("overview_number_of_selected_cells")
                ),
                column(
                  width = 4,
                  tags$div(
                    class = "cerebro-selection-actions",
                    shinyjs::hidden(
                      actionButton(
                        inputId = "overview_projection_zoom_to_selection",
                        label = "Zoom to selection",
                        icon = icon("magnifying-glass-plus"),
                        class = "btn-xs btn-default"
                      )
                    ),
                    shinyjs::hidden(
                      actionButton(
                        inputId = "overview_projection_clear_selection",
                        label = "Clear selection",
                        icon = icon("eraser"),
                        class = "btn-xs btn-default btn-breathing"
                      )
                    )
                  )
                )
              ),
            )
          ),
          class = "cerebro-projection-gate"
        )
      )
    )
  )
})
