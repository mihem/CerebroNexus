##----------------------------------------------------------------------------##
## Tab: Spatial
##----------------------------------------------------------------------------##
## Prepend the shared plotly layout factory; see overview/UI.R for context.
js_code_spatial_projection <- paste(
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/www/projection_layouts.js"
    )
  ),
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/spatial/js_projection_update_plot.js"
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

    /* ---- Custom draggable legend for the spatial projection --------------- */
    /* The JS builds these elements but ships no styles (the upstream CSS was
       never committed), so define them here. Both the categorical legend
       (#spatial_projection_legend) and the continuous legend
       (#spatial_projection_continuous_legend) share the card / header / drag
       styling. */
    #spatial_projection_legend,
    #spatial_projection_continuous_legend {
      position: absolute;
      top: 12px;
      right: 12px;
      z-index: 20;
      background: rgba(255, 255, 255, 0.92);
      border: 1px solid #d9d9d9;
      border-radius: 6px;
      padding: 6px 10px 8px 10px;
      box-shadow: 0 1px 4px rgba(0, 0, 0, 0.12);
      font-family: inherit;
      max-height: 70%;
      overflow-y: auto;
      user-select: none;
    }
    #spatial_projection_legend .legend-header,
    #spatial_projection_continuous_legend .legend-header {
      display: flex;
      align-items: center;
      gap: 6px;
      margin-bottom: 6px;
      cursor: grab;
    }
    #spatial_projection_legend .legend-title-text,
    #spatial_projection_continuous_legend .legend-title-text {
      font-weight: 600;
      font-size: 13px;
      color: #333;
    }
    /* six-dot drag handle */
    #spatial_projection_legend .legend-drag-handle,
    #spatial_projection_continuous_legend .legend-drag-handle {
      display: flex;
      flex-direction: column;
      gap: 2px;
    }
    #spatial_projection_legend .legend-drag-handle-dots,
    #spatial_projection_continuous_legend .legend-drag-handle-dots {
      display: flex;
      gap: 2px;
    }
    #spatial_projection_legend .legend-drag-handle-dot,
    #spatial_projection_continuous_legend .legend-drag-handle-dot {
      width: 3px;
      height: 3px;
      border-radius: 50%;
      background: #b0b0b0;
    }
    /* one legend row: swatch + label */
    #spatial_projection_legend .custom-legend-item {
      display: flex;
      align-items: center;
      gap: 7px;
      cursor: pointer;
      border-radius: 3px;
    }
    #spatial_projection_legend .custom-legend-item:hover {
      background: rgba(0, 0, 0, 0.05);
    }
    /* colour swatch rendered as a filled circle */
    #spatial_projection_legend .legend-color-box {
      display: inline-block;
      flex: 0 0 auto;
      border-radius: 50%;
      border: 1px solid rgba(0, 0, 0, 0.15);
    }
    #spatial_projection_legend .legend-text {
      color: #333;
      white-space: nowrap;
    }
    /* clicked-to-hide state */
    #spatial_projection_legend .legend-item-hidden {
      opacity: 0.35;
    }
    #spatial_projection_legend .legend-item-hidden .legend-text {
      text-decoration: line-through;
    }
    /* first-time drag hint */
    #spatial_projection_legend .legend-drag-tip,
    #spatial_projection_continuous_legend .legend-drag-tip {
      font-size: 11px;
      color: #888;
      margin-top: 4px;
      font-style: italic;
    }
    /* continuous colour legend: gradient bar + min/max labels */
    #spatial_projection_continuous_legend .continuous-legend-content {
      display: flex;
      align-items: stretch;
      gap: 8px;
      height: 120px;
    }
    #spatial_projection_continuous_legend .continuous-legend-gradient {
      width: 14px;
      flex: 0 0 auto;
      border-radius: 3px;
      border: 1px solid rgba(0, 0, 0, 0.15);
    }
    #spatial_projection_continuous_legend .continuous-legend-labels {
      display: flex;
      flex-direction: column;
      justify-content: space-between;
      font-size: 11px;
      color: #333;
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
      "getContainerDimensions",
      "spatialClearSelection",
      "showScrollDownIndicator",
      "hideScrollDownIndicator"
    )
  ),
  uiOutput("spatial_projection_UI"),
  uiOutput("spatial_selected_cells_plot_UI"),
  uiOutput("spatial_selected_cells_table_UI")
)
