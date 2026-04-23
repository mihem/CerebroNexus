// layout for 2D projections
const spatial_projection_layout_2D = {
  uirevision: 'true',
  hovermode: 'closest',
  dragmode: 'select', // DEBUG: Added dragmode for selection to work
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4,
  },
  legend: {
    itemsizing: 'constant',
  },
  xaxis: {
    autorange: true,
    mirror: true,
    showline: true,
    zeroline: false,
    range: [],
    gridcolor: '#E2E8F0',
    linecolor: '#CBD5E0',
    tickfont: {
      color: '#718096',
      family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
    },
    titlefont: {
      color: '#2D3748',
      family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
    },
  },
  yaxis: {
    autorange: true,
    mirror: true,
    showline: true,
    zeroline: false,
    range: [],
    gridcolor: '#E2E8F0',
    linecolor: '#CBD5E0',
    tickfont: {
      color: '#718096',
      family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
    },
    titlefont: {
      color: '#2D3748',
      family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
    },
  },
  hoverlabel: {
    font: {
      size: 12,
      color: '#2D3748',
      family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
    },
    bgcolor: 'rgba(255, 255, 255, 0.95)',
    bordercolor: '#E2E8F0',
    align: 'left',
  },
  plot_bgcolor: 'rgba(255, 255, 255, 0)',
  paper_bgcolor: 'rgba(255, 255, 255, 0)',
};

// Inject CSS for spatial projection
(function () {
  const style = document.createElement('style');
  style.innerHTML = `
    /* Custom Legend Styles */
    #spatial_projection_legend {
      position: absolute;
      top: 10px;
      right: 10px;
      background: rgba(255, 255, 255, 0.95);
      border: 1px solid #E2E8F0;
      border-radius: 8px;
      padding: 12px;
      max-height: 300px;
      overflow-y: auto;
      z-index: 1000;
      box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      cursor: move;
    }
    .custom-legend-item {
      display: flex;
      align-items: center;
      margin-bottom: 6px;
      cursor: pointer;
      user-select: none;
      padding: 4px 6px;
      border-radius: 4px;
      transition: background-color 0.2s ease;
    }
    .custom-legend-item:hover {
      background-color: rgba(91, 124, 153, 0.08);
    }
    .custom-legend-item:last-child {
      margin-bottom: 0;
    }
    .legend-color-box {
      width: 16px;
      height: 16px;
      margin-right: 10px;
      border-radius: 4px;
      flex-shrink: 0;
      box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
    }
    .legend-text {
      font-size: 13px;
      color: #2D3748;
      font-weight: 500;
    }
    .legend-item-hidden .legend-text {
      text-decoration: line-through;
      color: #A0AEC0;
    }
    .legend-item-hidden .legend-color-box {
      opacity: 0.4;
    }

    /* Continuous Legend Styles */
    .continuous-legend {
      position: absolute;
      top: 10px;
      right: 10px;
      background: rgba(255, 255, 255, 0.95);
      border: 1px solid #E2E8F0;
      border-radius: 8px;
      padding: 12px;
      z-index: 1000;
      box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      cursor: move;
      min-width: 80px;
    }
    .continuous-legend-title {
      font-size: 13px;
      color: #2D3748;
      font-weight: 500;
      margin-bottom: 8px;
      text-align: center;
    }
    .continuous-legend-gradient {
      width: 20px;
      height: 150px;
      margin: 0 auto;
      border-radius: 4px;
      box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
    }
    .continuous-legend-labels {
      display: flex;
      flex-direction: column;
      justify-content: space-between;
      height: 150px;
      margin-left: 8px;
    }
    .continuous-legend-label {
      font-size: 11px;
      color: #718096;
      font-weight: 400;
    }
    .continuous-legend-content {
      display: flex;
      align-items: center;
    }
    .detached-modebar {
      position: absolute !important;
      top: 0px !important;
      right: 0px !important;
      z-index: 1001 !important;
    }
    .detached-modebar .modebar-btn {
      background: transparent;
      border: none;
      border-radius: 4px;
      box-shadow: none;
      transition: all 0.2s ease;
    }
    .detached-modebar .modebar-btn:hover {
      background: rgba(91, 124, 153, 0.1);
      border: none;
      transform: translateY(-1px);
      box-shadow: none;
    }
    .detached-modebar .modebar-btn svg {
      fill: #5B7C99;
    }
    .detached-modebar .modebar-btn:hover svg {
      fill: #3D5A73;
    }
    .detached-modebar .modebar-group {
      display: flex !important;
      flex-direction: row !important;
      align-items: center !important;
      gap: 4px !important;
    }
    .detached-modebar .modebar {
      display: flex !important;
      flex-direction: row !important;
      align-items: center !important;
      gap: 8px !important;
    }
    #spatial_projection_background {
      position: absolute;
      left: 0;
      top: 0;
      width: 100%;
      height: 100%;
      z-index: 0;
      background-repeat: no-repeat;
      background-position: center center;
      background-size: 100% 100%;
      pointer-events: none;
      transform-origin: center center;
    }
  `;
  document.head.appendChild(style);
})();

shinyjs.detachModebar = function () {
  const plotContainer = document.getElementById('spatial_projection');
  if (!plotContainer) return;

  const parent = plotContainer.parentElement;
  if (getComputedStyle(parent).position === 'static') {
    parent.style.position = 'relative';
  }

  // Find the modebar inside the plot container
  const modebar = plotContainer.querySelector('.modebar-container') || plotContainer.querySelector('.modebar');

  if (modebar) {
    // Remove stale detached modebars
    const staleModebars = parent.querySelectorAll('.detached-modebar');
    staleModebars.forEach((el) => el.remove());

    parent.appendChild(modebar);
    modebar.classList.add('detached-modebar');
  }
};

shinyjs.applySpatialBackground = function () {
  const plotContainer = document.getElementById('spatial_projection');
  const bg = document.getElementById('spatial_projection_background');
  if (!plotContainer || !bg) return;

  const backgroundImage = bg.dataset.backgroundImage;

  if (backgroundImage) {
    bg.style.display = 'block';
    bg.style.backgroundImage = `url("${backgroundImage}")`;

    const flipX = bg.dataset.flipX === 'true';
    const flipY = bg.dataset.flipY === 'true';
    const scaleX = parseFloat(bg.dataset.scaleX) || 1;
    const scaleY = parseFloat(bg.dataset.scaleY) || 1;
    const opacity = parseFloat(bg.dataset.opacity);

    const finalScaleX = (flipX ? -1 : 1) * scaleX;
    const finalScaleY = (flipY ? -1 : 1) * scaleY;
    bg.style.transform = `scale(${finalScaleX}, ${finalScaleY})`;
    bg.style.opacity = isNaN(opacity) ? 1 : opacity;

    const size = plotContainer._fullLayout && plotContainer._fullLayout._size ? plotContainer._fullLayout._size : null;
    if (size) {
      bg.style.left = size.l + 'px';
      bg.style.top = size.t + 'px';
      bg.style.width = size.w + 'px';
      bg.style.height = size.h + 'px';
    } else {
      const parent = plotContainer.parentElement;
      bg.style.left = '0px';
      bg.style.top = '0px';
      bg.style.width = parent.clientWidth + 'px';
      bg.style.height = parent.clientHeight + 'px';
    }
  } else {
    bg.style.display = 'none';
    bg.style.backgroundImage = '';
    bg.style.transform = '';
    bg.style.opacity = '';
  }
};

shinyjs.syncSpatialBackground = function (backgroundImage, flipX, flipY, scaleX, scaleY, opacity) {
  const plotContainer = document.getElementById('spatial_projection');
  if (!plotContainer) return;
  let parent = plotContainer.parentElement;
  let wrapper = parent && parent.id === 'spatial_projection_wrapper' ? parent : null;
  if (!wrapper) {
    wrapper = document.createElement('div');
    wrapper.id = 'spatial_projection_wrapper';
    wrapper.style.position = 'relative';
    wrapper.style.width = '100%';
    wrapper.style.height = '100%';
    wrapper.style.overflow = 'hidden';
    parent.insertBefore(wrapper, plotContainer);
    wrapper.appendChild(plotContainer);
  }
  parent = wrapper;
  let bg = document.getElementById('spatial_projection_background');
  if (!bg) {
    bg = document.createElement('div');
    bg.id = 'spatial_projection_background';
    bg.style.transition = 'transform 0.5s cubic-bezier(0.4, 0, 0.2, 1), opacity 0.3s ease';
    parent.insertBefore(bg, plotContainer);
  }

  if (backgroundImage !== undefined) bg.dataset.backgroundImage = backgroundImage || '';
  if (flipX !== undefined) bg.dataset.flipX = String(flipX);
  if (flipY !== undefined) bg.dataset.flipY = String(flipY);
  if (scaleX !== undefined) bg.dataset.scaleX = String(scaleX || 1);
  if (scaleY !== undefined) bg.dataset.scaleY = String(scaleY || 1);
  if (opacity !== undefined) bg.dataset.opacity = String(opacity === null ? 1 : opacity);

  shinyjs.applySpatialBackground();

  plotContainer.style.position = 'relative';
  plotContainer.style.zIndex = '1';

  if (!plotContainer.dataset.bgListenerAttached && typeof plotContainer.on === 'function') {
    plotContainer.on('plotly_afterplot', shinyjs.applySpatialBackground);
    plotContainer.dataset.bgListenerAttached = 'true';
  }
};

// Custom Legend Helper Functions
shinyjs.makeDraggable = function (el) {
  let isDragging = false;
  let hasMoved = false;
  let startX, startY, initialLeft, initialTop;

  el.onmousedown = function (e) {
    // Only left mouse button
    if (e.button !== 0) return;

    isDragging = true;
    hasMoved = false;
    startX = e.clientX;
    startY = e.clientY;

    // Get current position
    const rect = el.getBoundingClientRect();
    const parentRect = el.parentElement.getBoundingClientRect();

    // Convert to relative position (left/top)
    initialLeft = rect.left - parentRect.left;
    initialTop = rect.top - parentRect.top;

    // Switch to left/top positioning if not already
    el.style.right = 'auto';
    el.style.bottom = 'auto';
    el.style.left = initialLeft + 'px';
    el.style.top = initialTop + 'px';

    el.style.cursor = 'grabbing';

    document.addEventListener('mousemove', onMouseMove);
    document.addEventListener('mouseup', onMouseUp);

    // Prevent default text selection
    e.preventDefault();
  };

  function onMouseMove(e) {
    if (!isDragging) return;

    const dx = e.clientX - startX;
    const dy = e.clientY - startY;

    if (dx !== 0 || dy !== 0) {
      hasMoved = true;
      el.dataset.isDragging = 'true';
    }

    el.style.left = initialLeft + dx + 'px';
    el.style.top = initialTop + dy + 'px';
  }

  function onMouseUp(e) {
    isDragging = false;
    el.style.cursor = 'move';
    document.removeEventListener('mousemove', onMouseMove);
    document.removeEventListener('mouseup', onMouseUp);

    if (hasMoved) {
      // Keep the flag for a short moment to block click events on children
      setTimeout(() => {
        el.dataset.isDragging = 'false';
      }, 50);
    } else {
      el.dataset.isDragging = 'false';
    }
  }
};

shinyjs.createCustomLegend = function (traces, colors) {
  const plotContainer = document.getElementById('spatial_projection');
  if (!plotContainer) return;

  // Ensure parent has relative positioning
  const parent = plotContainer.parentElement;
  if (getComputedStyle(parent).position === 'static') {
    parent.style.position = 'relative';
  }

  // Find or create legend container
  let legendContainer = document.getElementById('spatial_projection_legend');
  if (!legendContainer) {
    legendContainer = document.createElement('div');
    legendContainer.id = 'spatial_projection_legend';
    parent.appendChild(legendContainer);
  }

  // Enable dragging
  shinyjs.makeDraggable(legendContainer);

  // Reset content
  legendContainer.innerHTML = '';
  legendContainer.style.display = 'block';

  // Create legend items
  traces.forEach((traceName, index) => {
    const item = document.createElement('div');
    item.className = 'custom-legend-item';

    const colorBox = document.createElement('span');
    colorBox.className = 'legend-color-box';
    colorBox.style.backgroundColor = colors[index];

    const text = document.createElement('span');
    text.className = 'legend-text';
    text.innerText = traceName;

    item.appendChild(colorBox);
    item.appendChild(text);

    // Toggle visibility on click
    item.onclick = function () {
      if (legendContainer.dataset.isDragging === 'true') return;

      const plot = document.getElementById('spatial_projection');
      // Check current visibility status (default is visible/true)
      // We assume trace index corresponds to legend index
      let isVisible = true;
      if (plot.data && plot.data[index]) {
        isVisible = plot.data[index].visible !== false && plot.data[index].visible !== 'legendonly';
      }

      const newVisible = isVisible ? false : true;
      Plotly.restyle('spatial_projection', { visible: newVisible }, [index]);

      item.classList.toggle('legend-item-hidden', isVisible);
    };

    legendContainer.appendChild(item);
  });
};

shinyjs.removeCustomLegend = function () {
  const legendContainer = document.getElementById('spatial_projection_legend');
  if (legendContainer) {
    legendContainer.style.display = 'none';
  }
};

shinyjs.createContinuousLegend = function (title, colorMin, colorMax, colorscale) {
  const plotContainer = document.getElementById('spatial_projection');
  if (!plotContainer) return;

  const parent = plotContainer.parentElement;
  if (getComputedStyle(parent).position === 'static') {
    parent.style.position = 'relative';
  }

  let legendContainer = document.getElementById('spatial_projection_continuous_legend');
  if (!legendContainer) {
    legendContainer = document.createElement('div');
    legendContainer.id = 'spatial_projection_continuous_legend';
    parent.appendChild(legendContainer);
  }

  shinyjs.makeDraggable(legendContainer);
  legendContainer.innerHTML = '';
  legendContainer.style.display = 'block';
  legendContainer.className = 'continuous-legend';

  const titleEl = document.createElement('div');
  titleEl.className = 'continuous-legend-title';
  titleEl.innerText = title;
  legendContainer.appendChild(titleEl);

  const contentEl = document.createElement('div');
  contentEl.className = 'continuous-legend-content';

  const gradientEl = document.createElement('div');
  gradientEl.className = 'continuous-legend-gradient';

  const gradientColors = colorscale.map((item) => item[1]).join(', ');
  gradientEl.style.background = `linear-gradient(to top, ${gradientColors})`;

  const labelsEl = document.createElement('div');
  labelsEl.className = 'continuous-legend-labels';

  const minLabel = document.createElement('div');
  minLabel.className = 'continuous-legend-label';
  minLabel.innerText = colorMin.toFixed(2);

  const maxLabel = document.createElement('div');
  maxLabel.className = 'continuous-legend-label';
  maxLabel.innerText = colorMax.toFixed(2);

  labelsEl.appendChild(maxLabel);
  labelsEl.appendChild(minLabel);

  contentEl.appendChild(gradientEl);
  contentEl.appendChild(labelsEl);
  legendContainer.appendChild(contentEl);
};

shinyjs.removeContinuousLegend = function () {
  const legendContainer = document.getElementById('spatial_projection_continuous_legend');
  if (legendContainer) {
    legendContainer.style.display = 'none';
  }
};

// layout for 3D projections
const spatial_projection_layout_3D = {
  uirevision: 'true',
  hovermode: 'closest',
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4,
  },
  legend: {
    itemsizing: 'constant',
  },
  scene: {
    xaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
      gridcolor: '#E2E8F0',
      linecolor: '#CBD5E0',
      tickfont: {
        color: '#718096',
        family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      },
      titlefont: {
        color: '#2D3748',
        family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      },
    },
    yaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
      gridcolor: '#E2E8F0',
      linecolor: '#CBD5E0',
      tickfont: {
        color: '#718096',
        family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      },
      titlefont: {
        color: '#2D3748',
        family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      },
    },
    zaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      gridcolor: '#E2E8F0',
      linecolor: '#CBD5E0',
      tickfont: {
        color: '#718096',
        family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      },
      titlefont: {
        color: '#2D3748',
        family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      },
    },
  },
  hoverlabel: {
    font: {
      size: 12,
      color: '#2D3748',
      family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
    },
    bgcolor: 'rgba(255, 255, 255, 0.95)',
    bordercolor: '#E2E8F0',
    align: 'left',
  },
  plot_bgcolor: 'rgba(255, 255, 255, 0)',
  paper_bgcolor: 'rgba(255, 255, 255, 0)',
};

// structure of input data
const spatial_projection_default_params = {
  meta: {
    color_type: '',
    traces: [],
    color_variable: '',
  },
  data: {
    x: [],
    y: [],
    z: [],
    color: [],
    size: '',
    opacity: '',
    line: {},
    x_range: [],
    y_range: [],
    reset_axes: false,
  },
  hover: {
    hoverinfo: '',
    text: [],
  },
  group_centers: {
    group: [],
    x: [],
    y: [],
    z: [],
  },
  container: {
    width: null,
    height: null,
  },
};

// update 2D projection with continuous coloring
shinyjs.updatePlot2DContinuousSpatial = function (params) {
  console.log('[DEBUG] updatePlot2DContinuousSpatial called');
  params = shinyjs.getParams(params, spatial_projection_default_params);
  console.log('[DEBUG] Params received:', {
    meta: params.meta,
    dataPointsCount: params.data.x ? params.data.x.length : 0,
    colorVariable: params.meta.color_variable,
  });

  shinyjs.removeCustomLegend();
  shinyjs.removeContinuousLegend();
  const data = [];
  const colorArray = params.data.color;
  const colorMin = Math.min(...colorArray);
  const colorMax = Math.max(...colorArray);
  const colorscale = [
    [0, '#E8F4F8'],
    [0.2, '#D1E8ED'],
    [0.4, '#A8D0DC'],
    [0.6, '#7FB8CB'],
    [0.8, '#5B9FB8'],
    [1, '#3D7A9E'],
  ];
  data.push({
    x: params.data.x,
    y: params.data.y,
    mode: 'markers',
    type: 'scattergl',
    marker: {
      size: params.data.point_size,
      opacity: params.data.point_opacity,
      line: params.data.point_line,
      color: params.data.color,
      cmin: colorMin,
      cmax: colorMax,
      colorscale: colorscale,
      showscale: false,
    },
    hoverinfo: params.hover.hoverinfo,
  });
  shinyjs.createContinuousLegend(params.meta.color_variable, colorMin, colorMax, colorscale);
  const layout_here = Object.assign(spatial_projection_layout_2D);
  if (params.data.reset_axes) {
    layout_here.xaxis['autorange'] = true;
    layout_here.yaxis['autorange'] = true;
  } else {
    layout_here.xaxis['autorange'] = false;
    layout_here.xaxis['range'] = params.data.x_range;
    layout_here.yaxis['autorange'] = false;
    layout_here.yaxis['range'] = params.data.y_range;
  }
  if (params.container && params.container.width && params.container.height) {
    layout_here.width = params.container.width;
    layout_here.height = params.container.height;
  } else {
    const plotContainer = document.getElementById('spatial_projection');
    if (plotContainer && plotContainer.parentElement) {
      layout_here.width = plotContainer.parentElement.clientWidth;
      layout_here.height = plotContainer.parentElement.clientHeight;
    }
  }

  console.log('[DEBUG] Layout dragmode:', layout_here.dragmode);
  console.log('[DEBUG] Number of data points:', params.data.x ? params.data.x.length : 0);

  Plotly.react('spatial_projection', data, layout_here).then(() => {
    console.log('[DEBUG] Plotly.react (continuous) completed successfully');

    // DEBUG: Check if plotly selection events work
    const plotContainer = document.getElementById('spatial_projection');
    if (plotContainer) {
      console.log('[DEBUG] Plot container found after react');
      console.log('[DEBUG] Current dragmode in _fullLayout:', plotContainer._fullLayout ? plotContainer._fullLayout.dragmode : 'N/A');

      // Re-attach selection debug listeners
      if (typeof shinyjs.setupSelectionDebug === 'function') {
        shinyjs.setupSelectionDebug();
      }
    }

    shinyjs.syncSpatialBackground(
      params.meta.background_image,
      params.meta.background_flip_x,
      params.meta.background_flip_y,
      params.meta.background_scale_x,
      params.meta.background_scale_y,
      params.meta.background_opacity
    );
    shinyjs.detachModebar();
  });
};

// update 3D projection with continuous coloring
shinyjs.updatePlot3DContinuousSpatial = function (params) {
  params = shinyjs.getParams(params, spatial_projection_default_params);
  shinyjs.removeCustomLegend();
  shinyjs.removeContinuousLegend();
  const data = [];
  const colorArray = params.data.color;
  const colorMin = Math.min(...colorArray);
  const colorMax = Math.max(...colorArray);
  const colorscale = [
    [0, '#E8F4F8'],
    [0.2, '#D1E8ED'],
    [0.4, '#A8D0DC'],
    [0.6, '#7FB8CB'],
    [0.8, '#5B9FB8'],
    [1, '#3D7A9E'],
  ];
  data.push({
    x: params.data.x,
    y: params.data.y,
    z: params.data.z,
    mode: 'markers',
    type: 'scatter3d',
    marker: {
      size: params.data.point_size,
      opacity: params.data.point_opacity,
      line: params.data.point_line,
      color: params.data.color,
      cmin: colorMin,
      cmax: colorMax,
      colorscale: colorscale,
      reversescale: true,
      showscale: false,
    },
    showlegend: false,
  });
  shinyjs.createContinuousLegend(params.meta.color_variable, colorMin, colorMax, colorscale);
  const layout_here = Object.assign(spatial_projection_layout_3D);
  if (params.container && params.container.width && params.container.height) {
    layout_here.width = params.container.width;
    layout_here.height = params.container.height;
  } else {
    const plotContainer = document.getElementById('spatial_projection');
    if (plotContainer && plotContainer.parentElement) {
      layout_here.width = plotContainer.parentElement.clientWidth;
      layout_here.height = plotContainer.parentElement.clientHeight;
    }
  }
  Plotly.react('spatial_projection', data, layout_here).then(() => {
    shinyjs.syncSpatialBackground(null, false, false, 1, 1, 1);
    shinyjs.detachModebar();
  });
};

shinyjs.getContainerDimensions = function () {
  const plotContainer = document.getElementById('spatial_projection');
  if (plotContainer) {
    const parentContainer = plotContainer.parentElement;
    return {
      width: parentContainer.clientWidth,
      height: parentContainer.clientHeight,
    };
  }
  return { width: 0, height: 0 };
};

// update 2D projection with categorical coloring
shinyjs.updatePlot2DCategoricalSpatial = function (params) {
  console.log('[DEBUG] updatePlot2DCategoricalSpatial called');
  params = shinyjs.getParams(params, spatial_projection_default_params);
  console.log('[DEBUG] Params received:', {
    meta: params.meta,
    dataPointsCount: params.data.x ? params.data.x.length : 0,
    tracesCount: params.meta.traces ? params.meta.traces.length : 0,
  });

  shinyjs.removeContinuousLegend();
  shinyjs.createCustomLegend(params.meta.traces, params.data.color);
  const data = [];
  for (let i = 0; i < params.data.x.length; i++) {
    data.push({
      x: params.data.x[i],
      y: params.data.y[i],
      name: params.meta.traces[i],
      mode: 'markers',
      type: 'scattergl',
      marker: {
        size: params.data.point_size,
        opacity: params.data.point_opacity,
        line: params.data.point_line,
        color: params.data.color[i],
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text[i],
      hoverlabel: {
        bgcolor: 'rgba(255, 255, 255, 0.95)',
        bordercolor: '#E2E8F0',
        font: {
          color: '#2D3748',
          size: 12,
          family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
        },
      },
      showlegend: false,
    });
  }
  if (params.group_centers.group.length >= 1) {
    data.push({
      x: params.group_centers.x,
      y: params.group_centers.y,
      text: params.group_centers.group,
      type: 'scatter',
      mode: 'text',
      name: 'Labels',
      textposition: 'middle center',
      textfont: {
        color: '#000000',
        size: 16,
      },
      hoverinfo: 'skip',
      inherit: false,
      showlegend: false,
    });
  }
  const layout_here = JSON.parse(JSON.stringify(spatial_projection_layout_2D));

  if (params.data.reset_axes) {
    layout_here.xaxis.autorange = true;
    delete layout_here.xaxis.range;
    layout_here.yaxis.autorange = true;
    delete layout_here.yaxis.range;
  } else {
    layout_here.xaxis.autorange = false;
    layout_here.xaxis.range = [...params.data.x_range];
    layout_here.yaxis.autorange = false;
    layout_here.yaxis.range = [...params.data.y_range];
  }
  if (params.container && params.container.width && params.container.height) {
    layout_here.width = params.container.width;
    layout_here.height = params.container.height;
  } else {
    const plotContainer = document.getElementById('spatial_projection');
    if (plotContainer && plotContainer.parentElement) {
      layout_here.width = plotContainer.parentElement.clientWidth;
      layout_here.height = plotContainer.parentElement.clientHeight;
    }
  }

  console.log('[DEBUG] Layout dragmode:', layout_here.dragmode);
  console.log('[DEBUG] Number of traces to render:', data.length);

  Plotly.react('spatial_projection', data, layout_here).then(() => {
    console.log('[DEBUG] Plotly.react completed successfully');

    // DEBUG: Check if plotly selection events work
    const plotContainer = document.getElementById('spatial_projection');
    if (plotContainer) {
      console.log('[DEBUG] Plot container found after react');
      console.log('[DEBUG] Current dragmode in _fullLayout:', plotContainer._fullLayout ? plotContainer._fullLayout.dragmode : 'N/A');

      // Re-attach selection debug listeners
      if (typeof shinyjs.setupSelectionDebug === 'function') {
        shinyjs.setupSelectionDebug();
      }
    }

    shinyjs.syncSpatialBackground(
      params.meta.background_image,
      params.meta.background_flip_x,
      params.meta.background_flip_y,
      params.meta.background_scale_x,
      params.meta.background_scale_y,
      params.meta.background_opacity
    );
    shinyjs.detachModebar();
  });
};

// update 3D projection with categorical coloring
shinyjs.updatePlot3DCategoricalSpatial = function (params) {
  params = shinyjs.getParams(params, spatial_projection_default_params);
  shinyjs.removeContinuousLegend();
  shinyjs.createCustomLegend(params.meta.traces, params.data.color);
  const data = [];
  for (let i = 0; i < params.data.x.length; i++) {
    data.push({
      x: params.data.x[i],
      y: params.data.y[i],
      z: params.data.z[i],
      name: params.meta.traces[i],
      mode: 'markers',
      type: 'scatter3d',
      marker: {
        size: params.data.point_size,
        opacity: params.data.point_opacity,
        line: params.data.point_line,
        color: params.data.color[i],
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text[i],
      hoverlabel: {
        bgcolor: 'rgba(255, 255, 255, 0.95)',
        bordercolor: '#E2E8F0',
        font: {
          color: '#2D3748',
          size: 12,
          family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
        },
      },
      showlegend: false,
    });
  }
  if (params.group_centers.group.length >= 1) {
    data.push({
      x: params.group_centers.x,
      y: params.group_centers.y,
      z: params.group_centers.z,
      text: params.group_centers.group,
      type: 'scatter3d',
      mode: 'text',
      name: 'Labels',
      textposition: 'middle center',
      textfont: {
        color: '#000000',
        size: 16,
      },
      hoverinfo: 'skip',
      inherit: false,
      showlegend: false,
    });
  }
  const layout_here = JSON.parse(JSON.stringify(spatial_projection_layout_3D));
  if (params.container && params.container.width && params.container.height) {
    layout_here.width = params.container.width;
    layout_here.height = params.container.height;
  } else {
    const plotContainer = document.getElementById('spatial_projection');
    if (plotContainer && plotContainer.parentElement) {
      layout_here.width = plotContainer.parentElement.clientWidth;
      layout_here.height = plotContainer.parentElement.clientHeight;
    }
  }
  Plotly.react('spatial_projection', data, layout_here).then(() => {
    shinyjs.syncSpatialBackground(null, false, false, 1, 1, 1);
    shinyjs.detachModebar();
  });
};

// =============================================================================
// DEBUG: Selection Event Monitoring
// =============================================================================

// Debug helper to monitor plotly selection events
shinyjs.setupSelectionDebug = function () {
  const plotContainer = document.getElementById('spatial_projection');
  if (!plotContainer) {
    console.warn('[DEBUG] spatial_projection element not found');
    return;
  }

  console.log('[DEBUG] Setting up selection event listeners on spatial_projection');

  // Monitor plotly_selected event
  plotContainer.on('plotly_selected', function (eventData) {
    console.log('=== [DEBUG] plotly_selected event fired ===');
    console.log('  eventData:', eventData);
    if (eventData) {
      console.log('  eventData.points length:', eventData.points ? eventData.points.length : 'undefined');
      if (eventData.points && eventData.points.length > 0) {
        console.log('  First point:', eventData.points[0]);
        console.log('  First 5 points x,y:', eventData.points.slice(0, 5).map((p) => `(${p.x}, ${p.y})`));
      }
    } else {
      console.log('  eventData is null/undefined - selection cleared?');
    }

    // Check if Shiny is available
    if (typeof Shiny !== 'undefined') {
      console.log('  Shiny object available: true');
      console.log('  Checking if event will be sent to Shiny...');
    } else {
      console.warn('  Shiny object NOT available!');
    }
  });

  // Monitor plotly_selecting event (during selection)
  plotContainer.on('plotly_selecting', function (eventData) {
    // Only log occasionally to avoid spam
    if (Math.random() < 0.1) {
      console.log('[DEBUG] plotly_selecting - points being selected:', eventData ? (eventData.points ? eventData.points.length : 0) : 0);
    }
  });

  // Monitor plotly_deselect event
  plotContainer.on('plotly_deselect', function () {
    console.log('[DEBUG] plotly_deselect event fired - selection cleared');
  });

  console.log('[DEBUG] Selection event listeners attached successfully');
};

// Debug function to check plot configuration
shinyjs.debugPlotConfig = function () {
  const plotContainer = document.getElementById('spatial_projection');
  if (!plotContainer) {
    console.warn('[DEBUG] spatial_projection element not found');
    return;
  }

  console.log('=== [DEBUG] Plot Configuration ===');
  console.log('  plotContainer.id:', plotContainer.id);
  console.log('  plotContainer.data:', plotContainer.data);
  console.log('  plotContainer._fullLayout:', plotContainer._fullLayout);

  if (plotContainer.data && plotContainer.data.length > 0) {
    console.log('  Number of traces:', plotContainer.data.length);
    plotContainer.data.forEach((trace, i) => {
      console.log(`  Trace ${i}:`, {
        type: trace.type,
        mode: trace.mode,
        name: trace.name,
        pointsCount: trace.x ? trace.x.length : 0,
      });
    });
  }

  // Check layout for dragmode
  if (plotContainer._fullLayout) {
    console.log('  dragmode:', plotContainer._fullLayout.dragmode);
    console.log('  hovermode:', plotContainer._fullLayout.hovermode);
  }
};

// Auto-setup debug when document is ready
$(document).ready(function () {
  console.log('[DEBUG] Document ready - will setup selection debug after short delay');

  // Wait for plot to be initialized
  setTimeout(function () {
    shinyjs.setupSelectionDebug();
  }, 2000);

  // Also setup on any plot update
  const observer = new MutationObserver(function (mutations) {
    const plotContainer = document.getElementById('spatial_projection');
    if (plotContainer && !plotContainer.dataset.debugListenerAttached) {
      console.log('[DEBUG] Plot detected via MutationObserver, setting up debug listeners');
      shinyjs.setupSelectionDebug();
      plotContainer.dataset.debugListenerAttached = 'true';
    }
  });

  observer.observe(document.body, { childList: true, subtree: true });
});
