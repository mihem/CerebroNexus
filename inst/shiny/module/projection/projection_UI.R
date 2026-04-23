projection_UI <- function(id) {
  ns <- NS(id)

  js_file <- paste0(Cerebro.options[["cerebro_root"]], "/shiny/module/projection/js_projection_shared.js")
  if (file.exists(js_file)) {
    js_code <- paste(readLines(js_file), collapse = "\n")
  } else {
    js_code <- ""
    warning("JS file not found: ", js_file)
  }

  # Remove shinyjs::useShinyjs() if it's already in the main UI to avoid conflicts
  # shinyjs::useShinyjs()

  tagList(
    shinyjs::extendShinyjs(
      text = js_code,
      functions = c("updateProjectionPlot", "projectionClearSelection", "showScrollDownIndicator", "hideScrollDownIndicator")
    ),
    fluidRow(
      ## selections and parameters
      column(width = 3, offset = 0, style = "padding: 0px;",
        cerebroBox(
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = ns("main_parameters_info"),
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px")),
          uiOutput(ns("main_parameters_UI"))
        ),
        cerebroBox(
          title = tagList(
            "Additional parameters",
            actionButton(
              inputId = ns("additional_parameters_info"),
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px")),
          uiOutput(ns("additional_parameters_UI")),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Group filters",
            actionButton(
              inputId = ns("group_filters_info"),
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px")),
          uiOutput(ns("group_filters_UI")),
          collapsed = TRUE
        )
      ),
      ## plot
      column(width = 9, offset = 0, style = "padding: 0px;",
        cerebroBox(title =
          tagList(
            boxTitle("Dimensional reduction"),
            actionButton(
              inputId = ns("projection_info"),
              label = "info",
              title = "Show additional information for this panel.",
              icon = NULL,
              class = "btn-xs",
              style = "margin-right: 3px"
            ),
            shinyWidgets::dropdownButton(
              tags$div(
                style = "color: black !important;",
                uiOutput(ns("show_group_label_UI")),
                uiOutput(ns("point_border_UI")),
                uiOutput(ns("scales_UI"))
              ),
              circle = FALSE,
              icon = icon("cog"),
              inline = TRUE,
              size = "xs"
            )
          ),
          tagList(
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(
                ns("projection"),
                width = "auto",
                height = "calc(100vh - 200px)"
              ),
              type = 8,
              hide.ui = FALSE
            ),
            tags$br(),
            fluidRow(
              column(width = 8,
                htmlOutput(ns("number_of_selected_cells"))
              ),
              column(width = 4, style = "text-align: right;",
                shinyjs::hidden(
                  actionButton(
                    inputId = ns("clear_selection"),
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
    ),
    ## Plot of selected cells - full width outside main fluidRow
    uiOutput(ns("selected_cells_plot_UI")),
    ## Table of selected cells - full width outside main fluidRow
    uiOutput(ns("selected_cells_table_UI"))
  )
}
