##----------------------------------------------------------------------------##
## UI element with layout for user input and plot.
##----------------------------------------------------------------------------##
output[["expression_projection_UI"]] <- renderUI({
  tagList(
    cerebroVizPageHeader(
      "Gene expression",
      "expression_projection_info",
      "Explore gene expression across cells in projection or trajectory space."
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
            div(
              style = "display:none;",
              `aria-hidden` = "true",
              shinyWidgets::radioGroupButtons(
                inputId = "expression_analysis_mode",
                label = NULL,
                choices = c("Gene(s)"),
                selected = "Gene(s)"
              )
            ),
            uiOutput("expression_projection_input_type_UI"),
            uiOutput("expression_projection_select_projection_UI")
          ),
          cerebroSettingsButton(
            "expression_projection_more_button",
            "expression_projection_more"
          ),
          cerebroSettingsDrawer(
            "expression_projection_more",
            cerebroSettingsSection(
              "Appearance",
              tagList(
                uiOutput("expression_projection_additional_parameters_UI"),
                uiOutput("expression_projection_point_border_UI"),
                uiOutput("expression_projection_genes_in_separate_panels_UI")
              ),
              cerebroInfoButton(
                "expression_projection_additional_parameters_info"
              )
            ),
            cerebroSettingsSection(
              "Colour scale",
              tagList(
                uiOutput("expression_projection_color_scale_UI"),
                uiOutput("expression_projection_color_range_UI")
              ),
              cerebroInfoButton("expression_projection_color_scale_info")
            ),
            cerebroSettingsSection(
              "Axes",
              uiOutput("expression_projection_scales_UI")
            ),
            cerebroSettingsSection(
              "Group filters",
              uiOutput("expression_projection_group_filters_UI"),
              cerebroInfoButton("expression_projection_group_filters_info")
            )
          )
        )
      ),
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-col",
        cerebroSelectionStatus(
          "expression_projection",
          "expression_number_of_selected_cells"
        ),
        cerebroCellViewOutput("expression_projection"),
        tags$br(),
        htmlOutput("expression_genes_displayed")
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
      <li><b>Gene(s)</b> Select one or multiple genes. If multiple genes are selected, the mean expression across those genes is calculated for each cell.</li>
      <li><b>Projection:</b> Select the projection or trajectory to display.</li>
    </ul>
    "
  )
)
