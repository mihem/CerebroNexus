// =============================================================================
// Trajectory projection: thin wrappers over the shared projection-scatter
// renderer (inst/shiny/www/projection_scatter.js). Trajectory was previously a
// pure-R renderPlotly; it now uses the shared empty-skeleton + JS-observer model
// so it gets the custom top legend, persistent x|y selection, group labels and
// modebar-off look. The trajectory PATH is a layout `shapes` overlay (black line
// segments) passed as the tab-specific `extra.shapes`.
//
// The R observer calls:
//   trajectoryUpdatePlot2DContinuous(meta, data, hover, group_centers, container, shapes)
//   trajectoryUpdatePlot2DCategorical(meta, data, hover, group_centers, container, shapes)
//   trajectoryGetContainerDimensions()
// shinyjs delivers positional args as ONE array `params`.
// =============================================================================

const TRAJECTORY_PLOT_ID = 'trajectory_projection';

if (window.cerebroProjection) {
  window.cerebroProjection.registerPlot(TRAJECTORY_PLOT_ID);
}

shinyjs.trajectoryUpdatePlot2DContinuous = function (params) {
  const [meta, data, hover, group_centers, container, shapes] = params;
  meta.plot_id = TRAJECTORY_PLOT_ID;
  window.cerebroProjection.render2DContinuous(meta, data, hover, group_centers, container, {
    shapes: shapes || [],
  });
};

shinyjs.trajectoryUpdatePlot2DCategorical = function (params) {
  const [meta, data, hover, group_centers, container, shapes] = params;
  meta.plot_id = TRAJECTORY_PLOT_ID;
  window.cerebroProjection.render2DCategorical(meta, data, hover, group_centers, container, {
    shapes: shapes || [],
  });
};

shinyjs.trajectoryGetContainerDimensions = function () {
  return window.cerebroProjection.getContainerDimensions(TRAJECTORY_PLOT_ID);
};
