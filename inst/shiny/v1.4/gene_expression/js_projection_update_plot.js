// =============================================================================
// Gene (set) expression projection.
//
// Single-panel 2D and 3D delegate to the shared projection-scatter renderer
// (inst/shiny/www/projection_scatter.js) so they get the custom continuous
// legend, persistent x|y selection, modebar-off and container sizing — the same
// system spatial/overview use. Colouring is continuous (expression level) with
// a user-selected colour scale + explicit colour RANGE, both passed through as
// tab-specific data params; a trajectory-line overlay rides in `extra.shapes`.
//
// The MULTI-PANEL mode is genuinely tab-specific (an independent grid of
// colorbars, one sub-plot per gene) and has no analogue in the shared single-
// scatter model, so it stays here unchanged.
//
// The R dispatcher calls:
//   expressionProjectionUpdatePlot2D(data, hover, color, trajectory)
//   expressionProjectionUpdatePlot2DMultiPanel(data, hover, color, trajectory)
//   expressionProjectionUpdatePlot3D(data, hover, color)
// shinyjs delivers those positional args as ONE array `params`.
// =============================================================================

const EXPRESSION_PLOT_ID = 'expression_projection';

if (window.cerebroProjection) {
  window.cerebroProjection.registerPlot(EXPRESSION_PLOT_ID);
}

// Build the shared-renderer meta/data from gene_expression's (data, hover,
// color, trajectory) quadruple. Expression is always continuous, coloured by
// the chosen named scale with reversescale (matching the previous behaviour),
// over the user's colour range.
function expressionBuildParams(data, color) {
  const meta = {
    plot_id: EXPRESSION_PLOT_ID,
    color_type: 'continuous',
    color_variable: 'Expression',
  };
  const sharedData = Object.assign({}, data, {
    colorscale: color.scale,
    color_range: color.range,
    reversescale: true,
  });
  return { meta: meta, data: sharedData };
}

shinyjs.expressionProjectionUpdatePlot2D = function (params) {
  const [data, hover, color, trajectory] = params;
  const { meta, data: sharedData } = expressionBuildParams(data, color);
  window.cerebroProjection.render2DContinuous(meta, sharedData, hover, null, null, {
    shapes: trajectory || [],
  });
};

shinyjs.expressionProjectionUpdatePlot3D = function (params) {
  const [data, hover, color] = params;
  const { meta, data: sharedData } = expressionBuildParams(data, color);
  window.cerebroProjection.render3DContinuous(meta, sharedData, hover, null, null, {});
};

// =============================================================================
// Multi-panel mode (tab-specific): a grid of independent scatter sub-plots, one
// per gene, sharing a single colorbar. Kept out of the shared module because it
// is not one scatter — no other projection tab has it.
// =============================================================================

const expression_projection_layout_2D_multi_panel = {
  uirevision: 'true',
  hovermode: 'closest',
  margin: { l: 50, r: 50, b: 50, t: 50, pad: 4 },
  hoverlabel: { font: { size: 11 }, bgcolor: 'lightgrey', align: 'left' },
  shapes: []
};

const expression_projection_multi_default_params = {
  data: {
    x: [], y: [], z: [], color: [], size: '', opacity: '', line: {},
    x_range: [], y_range: [], reset_axes: false
  },
  hover: { hoverinfo: '', text: [] },
  color: { scale: '', range: [0, 1] },
  trajectory: []
};

shinyjs.expressionProjectionUpdatePlot2DMultiPanel = function (params) {
  params = shinyjs.getParams(params, expression_projection_multi_default_params);
  if (Array.isArray(params.data.color)) {
    return null;
  }
  // Multi-panel draws its own native plotly colorbar, so hide the shared custom
  // legend bar that a prior single-panel render may have left above the plot.
  if (window.cerebroProjection) {
    window.cerebroProjection.hideLegend(EXPRESSION_PLOT_ID);
  }
  const layout_here = Object.assign({}, expression_projection_layout_2D_multi_panel);
  layout_here.shapes = params.trajectory;
  const number_of_genes = Object.keys(params.data.color).length;
  let n_rows = 1;
  let n_cols = 1;
  if (number_of_genes == 2) {
    n_rows = 1; n_cols = 2;
  } else if (number_of_genes <= 4) {
    n_rows = 2; n_cols = 2;
  } else if (number_of_genes <= 6) {
    n_rows = 2; n_cols = 3;
  } else if (number_of_genes <= 9) {
    n_rows = 3; n_cols = 3;
  }
  layout_here.grid = { rows: n_rows, columns: n_cols, pattern: 'independent' };
  layout_here.annotations = [];
  const data = [];
  Object.keys(params.data.color).forEach(function (gene, index) {
    const x_axis = index === 0 ? 'xaxis' : `xaxis${index + 1}`;
    const y_axis = index === 0 ? 'yaxis' : `yaxis${index + 1}`;
    const x_anchor = `x${index + 1}`;
    const y_anchor = `y${index + 1}`;
    data.push({
      x: params.data.x,
      y: params.data.y,
      xaxis: x_anchor,
      yaxis: y_anchor,
      mode: 'markers',
      type: 'scattergl',
      marker: {
        size: params.data.point_size,
        opacity: params.data.point_opacity,
        line: params.data.point_line,
        color: params.data.color[gene],
        colorscale: params.color.scale,
        reversescale: true,
        cauto: false,
        cmin: params.color.range[0],
        cmax: params.color.range[1]
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text,
      showlegend: false
    });
    if (index === 0) {
      data[index].marker.colorbar = {
        title: {
          text: 'Expression',
          ticks: 'outside',
          outlinewidth: 1,
          outlinecolor: 'black'
        }
      };
    }
    layout_here[x_axis] = {
      title: gene,
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
      anchor: x_anchor
    };
    layout_here[y_axis] = {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
      anchor: y_anchor
    };
    if (params.data.reset_axes) {
      layout_here[x_axis]['autorange'] = true;
      layout_here[y_axis]['autorange'] = true;
    } else {
      layout_here[x_axis]['autorange'] = false;
      layout_here[x_axis]['range'] = params.data.x_range;
      layout_here[y_axis]['autorange'] = false;
      layout_here[y_axis]['range'] = params.data.y_range;
    }
  });
  Plotly.react('expression_projection', data, layout_here, {
    displayModeBar: false,
    displaylogo: false
  });
};
