##----------------------------------------------------------------------------##
## UI element with layout for user input and plot.
##----------------------------------------------------------------------------##
output[["expression_projection_UI"]] <- renderUI({
  fluidRow(
    class = "cerebro-viz-row",
    column(
      width = 3,
      offset = 0,
      class = "cerebro-param-col",
      tagList(
        cerebroBox(
          title = tagList(
            "Main parameters",
            cerebroInfoButton("expression_projection_main_parameters_info")
          ),
          tagList(
            shinyWidgets::radioGroupButtons(
              inputId = "expression_analysis_mode",
              label = NULL,
              choices = c("Gene(s)"),
              status = "primary",
              justified = TRUE,
              width = "100%"
            ),
            uiOutput("expression_projection_input_type_UI"),
            uiOutput("expression_projection_select_projection_UI")
          )
        ),
        cerebroBox(
          title = tagList(
            "Additional parameters",
            cerebroInfoButton(
              "expression_projection_additional_parameters_info"
            )
          ),
          uiOutput("expression_projection_additional_parameters_UI"),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Group filters",
            cerebroInfoButton("expression_projection_group_filters_info")
          ),
          uiOutput("expression_projection_group_filters_UI"),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Color scale",
            cerebroInfoButton("expression_projection_color_scale_info")
          ),
          tagList(
            uiOutput("expression_projection_color_scale_UI"),
            uiOutput("expression_projection_color_range_UI"),
          ),
          collapsed = TRUE
        )
      )
    ),
    column(
      width = 9,
      offset = 0,
      class = "cerebro-viz-col",
      shiny::tagAppendAttributes(
        cerebroBox(
          title = tagList(
            boxTitle("Dimensional reduction"),
            tagList(
              cerebroInfoButton("expression_projection_info"),
              ## shinyFiles::shinySaveButton(
              ##   "expression_projection_export",
              ##   label = "export to PDF",
              ##   title = "Export dimensional reduction to PDF file.",
              ##   filetype = "pdf",
              ##   viewtype = "icon",
              ##   class = "btn-xs",
              ##   style = "margin-right: 3px"
              ## ),
              shinyWidgets::dropdownButton(
                tags$div(
                  tags$style(
                    HTML("div.awesome-checkbox {margin-top: 10px;}")
                  ),
                  style = "color: black !important;",
                  tagList(
                    uiOutput("expression_projection_point_border_UI"),
                    uiOutput(
                      "expression_projection_genes_in_separate_panels_UI"
                    ),
                    uiOutput("expression_projection_scales_UI")
                  )
                ),
                circle = FALSE,
                icon = icon("cog"),
                inline = TRUE,
                size = "xs"
              )
            )
          ),
          tagList(
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(
                "expression_projection",
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
                htmlOutput("expression_number_of_selected_cells")
              ),
              column(
                width = 4,
                tags$div(
                  class = "cerebro-selection-actions",
                  shinyjs::hidden(
                    actionButton(
                      inputId = "expression_projection_zoom_to_selection",
                      label = "Zoom to selection",
                      icon = icon("magnifying-glass-plus"),
                      class = "btn-xs btn-default"
                    )
                  ),
                  shinyjs::hidden(
                    actionButton(
                      inputId = "expression_projection_clear_selection",
                      label = "Clear selection",
                      icon = icon("eraser"),
                      class = "btn-xs btn-default btn-breathing"
                    )
                  )
                )
              )
            ),
            tags$br(),
            htmlOutput("expression_genes_displayed")
          )
        ),
        class = "cerebro-viewport-gate"
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_main_parameters_info"]], {
  showModal(
    modalDialog(
      expression_projection_main_parameters_info$text,
      title = expression_projection_main_parameters_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_projection_main_parameters_info <- list(
  title = "Main parameters for gene (set) expression",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Gene(s)</b> In the case of 'Gene(s)', you can select one or multiple genes from the input field below. If you select multiple genes, the mean expression across the selected genes will be calculated for each cell. A list of which genes are present or missing in the data set can be found below the projection.</li>
      <li><b>Projection:</b> Select here which projection you want to see in the scatter plot on the right.</li>
    </ul>
    "
  )
)
