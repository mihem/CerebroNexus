##----------------------------------------------------------------------------##
## Tab: Spatial
##----------------------------------------------------------------------------##
## Prepend the shared plotly layout factory, then the shared projection-scatter
## renderer, then the spatial background layer, then spatial's thin wrappers —
## all concatenated into the SAME extendShinyjs() text so they share one global
## scope. The shared renderer (projection_scatter.js) is what every projection
## tab now delegates to; spatial's js_projection_update_plot.js only adds the
## plot-id-tagged wrappers + spatial-only page chrome.
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
      "/shiny/v1.4/www/projection_scatter.js"
    )
  ),
  ## Background-overlay layer, split out of js_projection_update_plot.js but
  ## concatenated back into the SAME extendShinyjs() text, so all functions
  ## still share one global scope (see the header in js_spatial_background.js).
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/spatial/js_spatial_background.js"
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

    /* The projection legend styles now live in www/custom.css under the shared
       .cerebro-projection-legend class (used by every projection tab). */

    /* ---- Additional-parameters panel: internal scroll ------------------- */
    /* The background-image controls are tall; on shorter screens adjusting
       Rotate/Move used to push the plot out of view. Cap the panel body and
       let it scroll internally instead of scrolling the whole page, so the
       plot stays put. The scrollbar is hidden and a soft top/bottom fade hints
       that more content is scrollable. */
    #spatial_additional_parameters_wrapper .box-body {
      max-height: calc(100vh - 320px);
      overflow-y: auto;
      /* hide scrollbar: Firefox + legacy, WebKit handled below */
      scrollbar-width: none;
      -ms-overflow-style: none;
      /* soft fade at top and bottom edges */
      -webkit-mask-image: linear-gradient(
        to bottom,
        transparent 0,
        #000 14px,
        #000 calc(100% - 14px),
        transparent 100%
      );
      mask-image: linear-gradient(
        to bottom,
        transparent 0,
        #000 14px,
        #000 calc(100% - 14px),
        transparent 100%
      );
    }
    #spatial_additional_parameters_wrapper .box-body::-webkit-scrollbar {
      width: 0;
      height: 0;
    }
    /* 'more below' pill: a small hint that the panel scrolls. Shown only while
       the body is scrollable and not yet at the bottom (toggled from JS), and it
       fades out as the user reaches the end. */
    #spatial_additional_parameters_wrapper {
      position: relative;
    }
    #spatial_additional_scroll_hint {
      position: absolute;
      left: 50%;
      bottom: 8px;
      transform: translateX(-50%);
      z-index: 5;
      display: none;
      align-items: center;
      justify-content: center;
      width: 26px;
      height: 26px;
      border-radius: 50%;
      background: rgba(51, 122, 183, 0.9);
      color: #fff;
      font-size: 15px;
      line-height: 1;
      box-shadow: 0 1px 4px rgba(0, 0, 0, 0.25);
      pointer-events: none;
      opacity: 0;
      transition: opacity 0.25s ease;
      animation: spatial-scroll-bob 1.4s ease-in-out infinite;
    }
    #spatial_additional_scroll_hint.is-visible {
      display: flex;
      opacity: 1;
    }
    @keyframes spatial-scroll-bob {
      0%, 100% { transform: translateX(-50%) translateY(0); }
      50% { transform: translateX(-50%) translateY(3px); }
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
      "showScrollDownIndicator",
      "hideScrollDownIndicator"
    )
  ),
  uiOutput("spatial_projection_UI"),
  uiOutput("spatial_selected_cells_plot_UI"),
  uiOutput("spatial_selected_cells_table_UI")
)
