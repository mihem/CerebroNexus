##----------------------------------------------------------------------------##
## Tab: Spatial
##----------------------------------------------------------------------------##
## Prepend the shared plotly layout factory, then the shared projection-scatter
## renderer, then the spatial background layer, then spatial's thin wrappers —
## all concatenated into the SAME extendShinyjs() text so they share one global
## scope. The shared renderer (projection_scatter.js) is what every projection
## tab now delegates to; spatial's js_projection_update_plot.js only adds the
## plot-id-tagged wrappers + spatial-only page chrome.
## Shared projection engine (projection_layouts.js + projection_scatter.js) is
## loaded once app-wide as static scripts (see shiny_UI.R) and exposes window
## globals. Spatial's own two scripts stay concatenated into one extendShinyjs()
## text so its background-overlay layer and plot wrappers share one scope.
js_code_spatial_projection <- paste(
  ## Background-overlay layer, split out of js_projection_update_plot.js but
  ## concatenated back into the SAME extendShinyjs() text, so all functions
  ## still share one global scope (see the header in js_spatial_background.js).
  cerebro_read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/spatial/js_spatial_background.js"
    )
  ),
  cerebro_read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/viewer/spatial/js_projection_update_plot.js"
    )
  ),
  sep = "\n"
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
    text = js_code_spatial_projection,
    functions = c(
      "updatePlot2DContinuousSpatial",
      "updatePlot3DContinuousSpatial",
      "updatePlot2DCategoricalSpatial",
      "updatePlot3DCategoricalSpatial",
      "updateSpatialBackgroundAppearance",
      "getContainerDimensions",
      "spatialClearSelection",
      "spatialZoomToSelection",
      "showScrollDownIndicator",
      "hideScrollDownIndicator"
    )
  ),
  uiOutput("spatial_projection_UI"),
  uiOutput("spatial_selected_cells_plot_UI"),
  uiOutput("spatial_selected_cells_table_UI")
)
