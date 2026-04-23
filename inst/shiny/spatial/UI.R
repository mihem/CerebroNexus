##----------------------------------------------------------------------------##
## Tab: Spatial
##----------------------------------------------------------------------------##
js_code_spatial_projection <- readr::read_file(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/spatial/js_projection_update_plot.js")
)

tab_spatial <- tabItem(
  tabName = "spatial",
  ## necessary to ensure alignment of table headers and content
  shinyjs::inlineCSS("
    #spatial_details_selected_cells_table .table th {
      text-align: center;
    }
    #spatial_details_selected_cells_table .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  shinyjs::extendShinyjs(
    text = js_code_spatial_projection,
    functions = c(
      "updatePlot2DContinuousSpatial",
      "updatePlot3DContinuousSpatial",
      "updatePlot2DCategoricalSpatial",
      "updatePlot3DCategoricalSpatial",
      "getContainerDimensions"
    )
  ),
  uiOutput("spatial_projection_UI"),
  uiOutput("spatial_selected_cells_plot_UI"),
  uiOutput("spatial_selected_cells_table_UI")
)
