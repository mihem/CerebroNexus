##----------------------------------------------------------------------------##
## Layout of the UI elements.
##----------------------------------------------------------------------------##
output[["spatial_projection_UI"]] <- renderUI({
  fluidRow(
    class = "cerebro-viz-row",
    ## selections and parameters
    column(
      width = 3,
      offset = 0,
      class = "cerebro-param-col",
      style = "padding: 0px;",
      tags$div(
        id = "spatial_main_parameters_wrapper",
        cerebroBox(
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "spatial_projection_main_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("spatial_projection_main_parameters_UI")
        )
      ),
      tags$div(
        id = "spatial_additional_parameters_wrapper",
        cerebroBox(
          title = tagList(
            "Additional parameters",
            actionButton(
              inputId = "spatial_projection_additional_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("spatial_projection_additional_parameters_UI"),
          collapsed = TRUE
        )
      ),
      cerebroBox(
        title = tagList(
          "Group filters",
          actionButton(
            inputId = "spatial_projection_group_filters_info",
            label = "info",
            icon = NULL,
            class = "btn-xs",
            title = "Show additional information for this panel.",
            style = "margin-left: 5px"
          )
        ),
        uiOutput("spatial_projection_group_filters_UI"),
        collapsed = TRUE
      )
    ),
    ## plot
    column(
      width = 9,
      offset = 0,
      class = "cerebro-viz-col",
      style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          boxTitle("Dimensional reduction"),
          actionButton(
            inputId = "spatial_projection_info",
            label = "info",
            title = "Show additional information for this panel.",
            icon = NULL,
            class = "btn-xs",
            style = "margin-right: 3px"
          ),
          #shinyFiles::shinySaveButton(
          # "spatial_projection_export",
          #label = "export to PDF",
          #title = "Export dimensional reduction to PDF file.",
          #filetype = "pdf",
          #viewtype = "icon",
          #class = "btn-xs",
          #style = "margin-right: 3px"
          #),
          shinyWidgets::dropdownButton(
            tags$div(
              style = "color: black !important;",
              uiOutput("spatial_projection_show_group_label_UI"),
              uiOutput("spatial_projection_point_border_UI"),
              uiOutput("spatial_projection_scales_UI")
            ),
            circle = FALSE,
            icon = icon("cog"),
            inline = TRUE,
            size = "xs"
          )
        ),
        tagList(
          ## Spatial autocorrelation (Moran's I) of the displayed gene, placed
          ## right below the colour legend and above the scatter so it reads as
          ## a property of the currently displayed gene. Only meaningful for a
          ## single continuous feature, so shown only in ImageFeaturePlot mode.
          ## The JS legend (#spatial_projection_legend) is inserted above this
          ## row, so the DOM order ends up legend -> Moran's I -> plot. The
          ## score itself is computed in out_morans_i.R.
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
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              "spatial_projection",
              width = "auto",
              ## Leave room below the plot for the selected-cells count and the
              ## Clear-selection button; the previous 200px offset was too small
              ## so the plot overflowed onto the button on shorter screens.
              height = "calc(100vh - 300px)"
            ),
            ## Match the Main/Gene-expression tabs: default hide.ui = TRUE. The
            ## projection renders an empty plotly shell that is then filled by a
            ## plotlyProxy; with hide.ui = FALSE the spinner stayed visible on
            ## top of the already-drawn plot and never cleared.
            type = 8
          ),
          tags$br(),
          fluidRow(
            column(width = 8, htmlOutput("spatial_number_of_selected_cells")),
            column(
              width = 4,
              style = "text-align: right;",
              shinyjs::hidden(
                actionButton(
                  inputId = "spatial_projection_clear_selection",
                  label = "Clear selection",
                  icon = icon("eraser"),
                  class = "btn-xs btn-default btn-breathing",
                  style = "margin-top: 5px;"
                )
              )
            )
          )
        )
      )
    )
  )
})
