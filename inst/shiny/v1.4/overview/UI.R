##----------------------------------------------------------------------------##
## Tab: Overview
##----------------------------------------------------------------------------##
## Prepend the shared plotly layout factory and the shared projection-scatter
## renderer, then overview's thin wrappers — all in ONE extendShinyjs() text so
## they share a global scope (same pattern as spatial/UI.R).
## Shared projection engine (projection_layouts.js + projection_scatter.js) is
## loaded once app-wide as static scripts (see shiny_UI.R) and exposes window
## globals; here we inline only this tab's thin wrappers.
js_code_overview_projection <- cerebro_read_file(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/overview/js_projection_update_plot.js"
  )
)

tab_overview <- tabItem(
  tabName = "overview",
  ## necessary to ensure alignment of table headers and content
  shinyjs::inlineCSS(
    "
    #overview_details_selected_cells_table .table th {
      text-align: center;
    }
    #overview_details_selected_cells_table .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  shinyjs::extendShinyjs(
    text = js_code_overview_projection,
    functions = c(
      "updatePlot2DContinuous",
      "updatePlot3DContinuous",
      "updatePlot2DCategorical",
      "updatePlot3DCategorical",
      "overviewClearSelection",
      "overviewZoomToSelection"
    )
  ),
  uiOutput("overview_projection_UI"),
  uiOutput("overview_selected_cells_plot_UI"),
  uiOutput("overview_selected_cells_table_UI")
)
