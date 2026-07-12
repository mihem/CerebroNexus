// =============================================================================
// Overview (Main) projection: thin wrappers over the shared projection-scatter
// renderer (inst/shiny/www/projection_scatter.js). The rendering logic (custom
// top legend, persistent x|y selection, group labels, modebar-off, container
// sizing) lives once in the shared module; here we only keep the js$ function
// names overview's UI.R registers and inject this tab's plot id.
//
// The R dispatcher calls:
//   updatePlot2DContinuous(meta, data, hover)
//   updatePlot3DContinuous(meta, data, hover)
//   updatePlot2DCategorical(meta, data, hover, group_centers)
//   updatePlot3DCategorical(meta, data, hover, group_centers)
// shinyjs delivers those positional args as ONE array `params`.
// =============================================================================

const OVERVIEW_PLOT_ID = 'overview_projection';

if (window.cerebroProjection) {
  window.cerebroProjection.registerPlot(OVERVIEW_PLOT_ID);
}

shinyjs.updatePlot2DContinuous = function (params) {
  const [meta, data, hover] = params;
  meta.plot_id = OVERVIEW_PLOT_ID;
  window.cerebroProjection.render2DContinuous(meta, data, hover, null, null, {});
};

shinyjs.updatePlot3DContinuous = function (params) {
  const [meta, data, hover] = params;
  meta.plot_id = OVERVIEW_PLOT_ID;
  window.cerebroProjection.render3DContinuous(meta, data, hover, null, null, {});
};

shinyjs.updatePlot2DCategorical = function (params) {
  const [meta, data, hover, group_centers] = params;
  meta.plot_id = OVERVIEW_PLOT_ID;
  window.cerebroProjection.render2DCategorical(meta, data, hover, group_centers, null, {});
};

shinyjs.updatePlot3DCategorical = function (params) {
  const [meta, data, hover, group_centers] = params;
  meta.plot_id = OVERVIEW_PLOT_ID;
  window.cerebroProjection.render3DCategorical(meta, data, hover, group_centers, null, {});
};
