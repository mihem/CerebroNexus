// Shared JS for projection module
// Following the style of js_projection_update_plot.js

// layout uirevision for axis persistence
var projection_uirevision = 'true';

// layout for 2D projections
const projection_layout_2D = {
  // uirevision will be set dynamically
  hovermode: 'closest',
  dragmode: 'select',
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

// layout for 3D projections
const projection_layout_3D = {
  // uirevision will be set dynamically
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

// Inject CSS for projection module
(function () {
  const style = document.createElement('style');
  style.innerHTML = `
    /* Custom Legend Styles */
    .projection-legend {
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
      max-height: 80vh;
      overflow-y: auto;
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
    .projection-continuous-legend {
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

    /* Legend Header with Drag Handle */
    .legend-header {
      display: flex;
      align-items: center;
      margin-bottom: 8px;
      padding-bottom: 6px;
      border-bottom: 1px solid #E2E8F0;
      cursor: grab;
    }
    .legend-header:active {
      cursor: grabbing;
    }
    .legend-drag-handle {
      display: flex;
      flex-direction: column;
      gap: 2px;
      margin-right: 8px;
      opacity: 0.4;
      transition: opacity 0.2s ease;
    }
    .projection-legend:hover .legend-drag-handle,
    .projection-continuous-legend:hover .legend-drag-handle {
      opacity: 0.7;
    }
    .legend-drag-handle-dots {
      display: flex;
      gap: 2px;
    }
    .legend-drag-handle-dot {
      width: 3px;
      height: 3px;
      background-color: #718096;
      border-radius: 50%;
    }
    .legend-title-text {
      font-size: 12px;
      color: #718096;
      font-weight: 500;
      flex-grow: 1;
    }

    /* Drag Tip Tooltip */
    .legend-drag-tip {
      position: absolute;
      top: -8px;
      left: 50%;
      transform: translateX(-50%) translateY(-100%);
      background: #2D3748;
      color: white;
      padding: 6px 10px;
      border-radius: 6px;
      font-size: 11px;
      white-space: nowrap;
      box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.15);
      z-index: 1001;
      animation: legendTipFadeIn 0.3s ease;
    }
    .legend-drag-tip::after {
      content: '';
      position: absolute;
      bottom: -6px;
      left: 50%;
      transform: translateX(-50%);
      border-width: 6px 6px 0 6px;
      border-style: solid;
      border-color: #2D3748 transparent transparent transparent;
    }
    @keyframes legendTipFadeIn {
      from { opacity: 0; transform: translateX(-50%) translateY(-90%); }
      to { opacity: 1; transform: translateX(-50%) translateY(-100%); }
    }
    @keyframes legendTipFadeOut {
      from { opacity: 1; transform: translateX(-50%) translateY(-100%); }
      to { opacity: 0; transform: translateX(-50%) translateY(-90%); }
    }

    /* Scroll Down Arrow Indicator */
    .scroll-down-indicator {
      position: fixed;
      bottom: 80px;
      left: 50%;
      transform: translateX(-50%);
      z-index: 9999;
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 8px;
      cursor: pointer;
      transition: opacity 0.4s ease;
    }
    .scroll-down-indicator.hiding {
      opacity: 0;
      pointer-events: none;
    }
    .scroll-down-arrow {
      width: 50px;
      height: 50px;
      display: flex;
      align-items: center;
      justify-content: center;
      background: rgba(49, 130, 206, 0.15);
      border: 2px solid rgba(49, 130, 206, 0.4);
      border-radius: 50%;
      animation: scrollArrowBreathe 2s ease-in-out infinite;
    }
    .scroll-down-arrow svg {
      width: 28px;
      height: 28px;
      fill: none;
      stroke: #3182ce;
      stroke-width: 2.5;
      stroke-linecap: round;
      stroke-linejoin: round;
      animation: scrollArrowBounce 2s ease-in-out infinite;
    }
    .scroll-down-text {
      font-size: 13px;
      color: #3182ce;
      font-weight: 500;
      background: rgba(255, 255, 255, 0.9);
      padding: 4px 12px;
      border-radius: 12px;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    }
    @keyframes scrollArrowBreathe {
      0%, 100% {
        transform: scale(1);
        background: rgba(49, 130, 206, 0.15);
        border-color: rgba(49, 130, 206, 0.4);
      }
      50% {
        transform: scale(1.1);
        background: rgba(49, 130, 206, 0.25);
        border-color: rgba(49, 130, 206, 0.6);
      }
    }
    @keyframes scrollArrowBounce {
      0%, 100% { transform: translateY(0); }
      50% { transform: translateY(5px); }
    }
  `;
  document.head.appendChild(style);
})();

// Scroll down indicator functions
shinyjs.showScrollDownIndicator = function (message) {
  // Remove existing indicator if any
  shinyjs.hideScrollDownIndicator();

  const indicator = document.createElement('div');
  indicator.id = 'scroll-down-indicator';
  indicator.className = 'scroll-down-indicator';
  indicator.innerHTML = `
    <div class="scroll-down-arrow">
      <svg viewBox="0 0 24 24">
        <polyline points="6 9 12 15 18 9"></polyline>
      </svg>
    </div>
    <div class="scroll-down-text">${message || 'Charts generated below'}</div>
  `;

  document.body.appendChild(indicator);

  // Click indicator to scroll down and hide
  indicator.onclick = function () {
    window.scrollBy({ top: 300, behavior: 'smooth' });
    shinyjs.hideScrollDownIndicator();
  };

  // Hide on scroll
  let scrollTimeout;
  const onScroll = function () {
    clearTimeout(scrollTimeout);
    scrollTimeout = setTimeout(function () {
      shinyjs.hideScrollDownIndicator();
      window.removeEventListener('scroll', onScroll);
    }, 100);
  };
  window.addEventListener('scroll', onScroll);

  // Hide on any click outside the indicator
  const onClickOutside = function (e) {
    if (!indicator.contains(e.target)) {
      shinyjs.hideScrollDownIndicator();
      document.removeEventListener('click', onClickOutside);
    }
  };
  // Delay adding click listener to avoid immediate trigger
  setTimeout(function () {
    document.addEventListener('click', onClickOutside);
  }, 100);

  // Store cleanup functions
  indicator.dataset.cleanup = 'true';
  indicator._onScroll = onScroll;
  indicator._onClickOutside = onClickOutside;
};

shinyjs.hideScrollDownIndicator = function () {
  const indicator = document.getElementById('scroll-down-indicator');
  if (indicator) {
    // Clean up event listeners
    if (indicator._onScroll) {
      window.removeEventListener('scroll', indicator._onScroll);
    }
    if (indicator._onClickOutside) {
      document.removeEventListener('click', indicator._onClickOutside);
    }
    // Fade out animation
    indicator.classList.add('hiding');
    setTimeout(function () {
      if (indicator.parentElement) {
        indicator.remove();
      }
    }, 400);
  }
};

// Detach modebar from plot
shinyjs.detachProjectionModebar = function (plotId) {
  const plotContainer = document.getElementById(plotId);
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

// Custom Legend Helper Functions - make legend draggable
shinyjs.makeProjectionDraggable = function (el) {
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

      // Record that user has dragged a legend (first time)
      if (!localStorage.getItem('cerebro_legend_dragged')) {
        localStorage.setItem('cerebro_legend_dragged', 'true');
        // Remove tip if it exists
        const tip = el.querySelector('.legend-drag-tip');
        if (tip) {
          tip.style.animation = 'legendTipFadeOut 0.2s ease forwards';
          setTimeout(() => tip.remove(), 200);
        }
      }
    }

    el.style.left = initialLeft + dx + 'px';
    el.style.top = initialTop + dy + 'px';
  }

  function onMouseUp(e) {
    isDragging = false;
    el.style.cursor = 'grab';
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

// Helper: Create drag handle element
function createLegendDragHandle() {
  const handle = document.createElement('div');
  handle.className = 'legend-drag-handle';
  // Create 3 rows of 2 dots each
  for (let i = 0; i < 3; i++) {
    const row = document.createElement('div');
    row.className = 'legend-drag-handle-dots';
    for (let j = 0; j < 2; j++) {
      const dot = document.createElement('div');
      dot.className = 'legend-drag-handle-dot';
      row.appendChild(dot);
    }
    handle.appendChild(row);
  }
  return handle;
}

// Helper: Create legend header with drag handle
function createLegendHeader(titleText) {
  const header = document.createElement('div');
  header.className = 'legend-header';

  const handle = createLegendDragHandle();
  header.appendChild(handle);

  if (titleText) {
    const title = document.createElement('div');
    title.className = 'legend-title-text';
    title.innerText = titleText;
    header.appendChild(title);
  }

  return header;
}

// Helper: Show first-time drag tip
function showLegendDragTip(legendContainer) {
  // Check if user has already dragged before
  if (localStorage.getItem('cerebro_legend_dragged')) {
    return;
  }

  // Create tip element
  const tip = document.createElement('div');
  tip.className = 'legend-drag-tip';
  tip.innerHTML = '💡 Drag to reposition';
  legendContainer.appendChild(tip);

  // Auto-hide after 4 seconds
  setTimeout(() => {
    if (tip.parentElement) {
      tip.style.animation = 'legendTipFadeOut 0.3s ease forwards';
      setTimeout(() => tip.remove(), 300);
    }
  }, 4000);
}

// Create categorical custom legend
shinyjs.createProjectionCustomLegend = function (plotId, traces, colors) {
  const plotContainer = document.getElementById(plotId);
  if (!plotContainer) return;

  // Ensure parent has relative positioning
  const parent = plotContainer.parentElement;
  if (getComputedStyle(parent).position === 'static') {
    parent.style.position = 'relative';
  }

  const legendId = plotId + '_legend';

  // Find or create legend container
  let legendContainer = document.getElementById(legendId);
  if (!legendContainer) {
    legendContainer = document.createElement('div');
    legendContainer.id = legendId;
    legendContainer.className = 'projection-legend';
    parent.appendChild(legendContainer);
  }

  // Enable dragging
  shinyjs.makeProjectionDraggable(legendContainer);

  // Reset content
  legendContainer.innerHTML = '';
  legendContainer.style.display = 'block';
  legendContainer.style.cursor = 'grab';

  // Add header with drag handle
  const header = createLegendHeader('Legend');
  legendContainer.appendChild(header);

  // Show first-time tip
  showLegendDragTip(legendContainer);

  // Calculate scaling based on number of traces
  const count = traces.length;
  let fontSize = 13;
  let itemMargin = 6;
  let itemPadding = 4;
  let itemPaddingX = 6;
  let boxSize = 16;

  if (count > 10) {
    if (count <= 20) {
      fontSize = 12;
      itemMargin = 4;
      itemPadding = 3;
      boxSize = 14;
    } else if (count <= 30) {
      fontSize = 11;
      itemMargin = 3;
      itemPadding = 2;
      boxSize = 12;
    } else if (count <= 50) {
      fontSize = 10;
      itemMargin = 2;
      itemPadding = 1;
      boxSize = 10;
    } else {
      fontSize = 9;
      itemMargin = 1;
      itemPadding = 0;
      boxSize = 8;
    }
  }

  // Create legend items
  traces.forEach((traceName, index) => {
    const item = document.createElement('div');
    item.className = 'custom-legend-item';
    item.style.marginBottom = itemMargin + 'px';
    item.style.padding = itemPadding + 'px ' + itemPaddingX + 'px';

    const colorBox = document.createElement('span');
    colorBox.className = 'legend-color-box';
    colorBox.style.backgroundColor = colors[index];
    colorBox.style.width = boxSize + 'px';
    colorBox.style.height = boxSize + 'px';

    const text = document.createElement('span');
    text.className = 'legend-text';
    text.innerText = traceName;
    text.style.fontSize = fontSize + 'px';

    item.appendChild(colorBox);
    item.appendChild(text);

    // Toggle visibility on click
    item.onclick = function () {
      if (legendContainer.dataset.isDragging === 'true') return;

      const plot = document.getElementById(plotId);
      // Check current visibility status (default is visible/true)
      let isVisible = true;
      if (plot.data && plot.data[index]) {
        isVisible = plot.data[index].visible !== false && plot.data[index].visible !== 'legendonly';
      }

      const newVisible = isVisible ? false : true;
      Plotly.restyle(plotId, { visible: newVisible }, [index]);

      item.classList.toggle('legend-item-hidden', isVisible);
    };

    legendContainer.appendChild(item);
  });
};

// Remove categorical custom legend
shinyjs.removeProjectionCustomLegend = function (plotId) {
  const legendId = plotId + '_legend';
  const legendContainer = document.getElementById(legendId);
  if (legendContainer) {
    legendContainer.style.display = 'none';
  }
};

// Create continuous legend
shinyjs.createProjectionContinuousLegend = function (plotId, title, colorMin, colorMax, colorscale) {
  const plotContainer = document.getElementById(plotId);
  if (!plotContainer) return;

  const parent = plotContainer.parentElement;
  if (getComputedStyle(parent).position === 'static') {
    parent.style.position = 'relative';
  }

  const legendId = plotId + '_continuous_legend';

  let legendContainer = document.getElementById(legendId);
  if (!legendContainer) {
    legendContainer = document.createElement('div');
    legendContainer.id = legendId;
    legendContainer.className = 'projection-continuous-legend';
    parent.appendChild(legendContainer);
  }

  shinyjs.makeProjectionDraggable(legendContainer);
  legendContainer.innerHTML = '';
  legendContainer.style.display = 'block';
  legendContainer.style.cursor = 'grab';

  // Add header with drag handle and title
  const header = createLegendHeader(title);
  legendContainer.appendChild(header);

  // Show first-time tip
  showLegendDragTip(legendContainer);

  const contentEl = document.createElement('div');
  contentEl.className = 'continuous-legend-content';

  const gradientEl = document.createElement('div');
  gradientEl.className = 'continuous-legend-gradient';

  // colorscale is array of [pos, color]
  const gradientColors = colorscale.map((item) => item[1]).join(', ');
  gradientEl.style.background = `linear-gradient(to top, ${gradientColors})`;

  const labelsEl = document.createElement('div');
  labelsEl.className = 'continuous-legend-labels';

  const maxLabel = document.createElement('div');
  maxLabel.className = 'continuous-legend-label';
  maxLabel.innerText = colorMax.toFixed(2);

  const minLabel = document.createElement('div');
  minLabel.className = 'continuous-legend-label';
  minLabel.innerText = colorMin.toFixed(2);

  labelsEl.appendChild(maxLabel);
  labelsEl.appendChild(minLabel);

  contentEl.appendChild(gradientEl);
  contentEl.appendChild(labelsEl);
  legendContainer.appendChild(contentEl);
};

// Remove continuous legend
shinyjs.removeProjectionContinuousLegend = function (plotId) {
  const legendId = plotId + '_continuous_legend';
  const legendContainer = document.getElementById(legendId);
  if (legendContainer) {
    legendContainer.style.display = 'none';
  }
};

// structure of input data
const projection_default_params = {
  meta: {
    color_type: '',
    traces: [],
    color_variable: '',
    background_image: null,
    image_bounds: {},
    background_flip_x: false,
    background_flip_y: false,
    background_scale_x: 1,
    background_scale_y: 1,
    background_opacity: 1,
  },
  data: {
    x: [],
    y: [],
    z: [],
    color: [],
    point_size: 5,
    point_opacity: 1,
    point_line: {},
    x_range: [],
    y_range: [],
    reset_axes: false,
    n_dimensions: 2,
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
    color: [],
  },
  plot_id: null,
};

// update 2D projection with continuous coloring
shinyjs.updateProjectionPlot2DContinuous = function (params) {
  params = shinyjs.getParams(params, projection_default_params);

  const plotId = params.plot_id;
  if (!plotId) {
    console.error('[projection] plotId is required');
    return;
  }

  shinyjs.removeProjectionCustomLegend(plotId);
  shinyjs.removeProjectionContinuousLegend(plotId);

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

  const data = [];
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
      showscale: false, // Hide default colorbar
    },
    hoverinfo: params.hover.hoverinfo,
    text: params.hover.text,
    showlegend: false,
  });

  shinyjs.createProjectionContinuousLegend(plotId, params.meta.color_variable, colorMin, colorMax, colorscale);

  const layout_here = JSON.parse(JSON.stringify(projection_layout_2D));

  // if (params.data.reset_axes) {
  //   projection_uirevision = Date.now().toString();
  //   layout_here.xaxis['autorange'] = true;
  //   layout_here.yaxis['autorange'] = true;
  // } else {
  //   layout_here.xaxis['autorange'] = false;
  //   layout_here.xaxis['range'] = params.data.x_range;
  //   layout_here.xaxis['constrain'] = 'range';
  //   layout_here.yaxis['autorange'] = false;
  //   layout_here.yaxis['range'] = params.data.y_range;
  //   layout_here.yaxis['constrain'] = 'range';
  //   layout_here.yaxis['scaleanchor'] = 'x';
  //   layout_here.yaxis['scaleratio'] = 1;
  // }
  layout_here.uirevision = projection_uirevision;

  // Maximize plot area
  const plotContainer = document.getElementById(plotId);
  if (plotContainer && plotContainer.parentElement) {
    layout_here.width = plotContainer.parentElement.clientWidth;
    layout_here.height = plotContainer.parentElement.clientHeight;
  }

  Plotly.react(plotId, data, layout_here).then(() => {
    shinyjs.detachProjectionModebar(plotId);
    syncProjectionBackground(
      plotId,
      params.meta.background_image,
      params.meta.background_flip_x,
      params.meta.background_flip_y,
      params.meta.background_scale_x,
      params.meta.background_scale_y,
      params.meta.background_opacity
    );
  });
};

// update 3D projection with continuous coloring
shinyjs.updateProjectionPlot3DContinuous = function (params) {
  params = shinyjs.getParams(params, projection_default_params);

  const plotId = params.plot_id;
  if (!plotId) {
    console.error('[projection] plotId is required');
    return;
  }

  shinyjs.removeProjectionCustomLegend(plotId);
  shinyjs.removeProjectionContinuousLegend(plotId);

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

  const data = [];
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
      showscale: false, // Hide default colorbar
    },
    hoverinfo: params.hover.hoverinfo,
    text: params.hover.text,
    showlegend: false,
  });

  shinyjs.createProjectionContinuousLegend(plotId, params.meta.color_variable, colorMin, colorMax, colorscale);

  const layout_here = JSON.parse(JSON.stringify(projection_layout_3D));

  if (params.data.reset_axes) {
    projection_uirevision = Date.now().toString();
  }
  layout_here.uirevision = projection_uirevision;

  // Maximize plot area
  const plotContainer = document.getElementById(plotId);
  if (plotContainer && plotContainer.parentElement) {
    layout_here.width = plotContainer.parentElement.clientWidth;
    layout_here.height = plotContainer.parentElement.clientHeight;
  }

  Plotly.react(plotId, data, layout_here).then(() => {
    shinyjs.detachProjectionModebar(plotId);
  });
};

// update 2D projection with categorical coloring
shinyjs.updateProjectionPlot2DCategorical = function (params) {
  params = shinyjs.getParams(params, projection_default_params);

  const plotId = params.plot_id;
  if (!plotId) {
    console.error('[projection] plotId is required');
    return;
  }

  shinyjs.removeProjectionContinuousLegend(plotId);
  shinyjs.createProjectionCustomLegend(plotId, params.meta.traces, params.data.color);

  // Optimization: map directly to data array
  const data = params.data.x.map((_, i) => ({
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
      bgcolor: params.data.color[i],
      bordercolor: '#E2E8F0',
      font: {
        color: '#2D3748',
        size: 12,
        family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      },
    },
    showlegend: false, // Hide default legend
  }));

  if (params.group_centers.group.length >= 1) {
    data.push({
      x: params.group_centers.x,
      y: params.group_centers.y,
      text: params.group_centers.group,
      type: 'scattergl',
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

  const layout_here = JSON.parse(JSON.stringify(projection_layout_2D));

  // if (params.data.reset_axes) {
  //   projection_uirevision = Date.now().toString();
  //   layout_here.xaxis['autorange'] = true;
  //   layout_here.yaxis['autorange'] = true;
  //   delete layout_here.xaxis.range;
  //   delete layout_here.yaxis.range;
  // } else {
  //   layout_here.xaxis['autorange'] = false;
  //   layout_here.xaxis['range'] = [...params.data.x_range];
  //   layout_here.xaxis['constrain'] = 'range';
  //   layout_here.yaxis['autorange'] = false;
  //   layout_here.yaxis['range'] = [...params.data.y_range];
  //   layout_here.yaxis['constrain'] = 'range';
  //   layout_here.yaxis['scaleanchor'] = 'x';
  //   layout_here.yaxis['scaleratio'] = 1;
  // }
  layout_here.uirevision = projection_uirevision;

  // Maximize plot area
  const plotContainer = document.getElementById(plotId);
  if (plotContainer && plotContainer.parentElement) {
    layout_here.width = plotContainer.parentElement.clientWidth;
    layout_here.height = plotContainer.parentElement.clientHeight;
  }

  Plotly.react(plotId, data, layout_here).then(() => {
    shinyjs.detachProjectionModebar(plotId);
    syncProjectionBackground(
      plotId,
      params.meta.background_image,
      params.meta.background_flip_x,
      params.meta.background_flip_y,
      params.meta.background_scale_x,
      params.meta.background_scale_y,
      params.meta.background_opacity
    );
  });
};

// update 3D projection with categorical coloring
shinyjs.updateProjectionPlot3DCategorical = function (params) {
  params = shinyjs.getParams(params, projection_default_params);

  const plotId = params.plot_id;
  if (!plotId) {
    console.error('[projection] plotId is required');
    return;
  }

  shinyjs.removeProjectionContinuousLegend(plotId);
  shinyjs.createProjectionCustomLegend(plotId, params.meta.traces, params.data.color);

  const data = params.data.x.map((_, i) => ({
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
      bgcolor: params.data.color[i],
      bordercolor: '#E2E8F0',
      font: {
        color: '#2D3748',
        size: 12,
        family: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif',
      },
    },
    showlegend: false, // Hide default legend
  }));

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

  const layout_here = JSON.parse(JSON.stringify(projection_layout_3D));

  if (params.data.reset_axes) {
    projection_uirevision = Date.now().toString();
  }
  layout_here.uirevision = projection_uirevision;

  // Maximize plot area
  const plotContainer = document.getElementById(plotId);
  if (plotContainer && plotContainer.parentElement) {
    layout_here.width = plotContainer.parentElement.clientWidth;
    layout_here.height = plotContainer.parentElement.clientHeight;
  }

  Plotly.react(plotId, data, layout_here).then(() => {
    shinyjs.detachProjectionModebar(plotId);
  });
};

// Unified update function that routes to the correct function based on color type and dimensions
shinyjs.updateProjectionPlot = function (params) {
  params = shinyjs.getParams(params, projection_default_params);

  const colorType = params.meta.color_type;
  const nDimensions = params.data.n_dimensions || 2;

  if (colorType === 'continuous') {
    if (nDimensions === 3) {
      shinyjs.updateProjectionPlot3DContinuous(params);
    } else {
      shinyjs.updateProjectionPlot2DContinuous(params);
    }
  } else {
    // categorical
    if (nDimensions === 3) {
      shinyjs.updateProjectionPlot3DCategorical(params);
    } else {
      shinyjs.updateProjectionPlot2DCategorical(params);
    }
  }
};

// Clear selection on projection plot
shinyjs.projectionClearSelection = function (plotId) {
  const plotContainer = document.getElementById(plotId);
  if (plotContainer && plotContainer.data) {
    // Use Plotly.update to reset both data selection and layout in one call
    // Setting selectedpoints to null for all traces restores full opacity
    const numTraces = plotContainer.data.length;
    const restyleUpdate = {};
    for (let i = 0; i < numTraces; i++) {
      restyleUpdate.selectedpoints = restyleUpdate.selectedpoints || [];
      restyleUpdate.selectedpoints.push(null);
    }

    // Combine restyle and relayout in one update call
    Plotly.update(
      plotId,
      { selectedpoints: null }, // Reset selected points for all traces
      { selections: [], dragmode: 'select' } // Clear selection box, keep select mode
    ).then(function () {
      // Emit deselect event after update completes
      plotContainer.emit('plotly_deselect');
    });
  }
};

// Background image support functions
function applyProjectionBackground(plotId) {
  const plotContainer = document.getElementById(plotId);
  const bgId = plotId + '_background';
  const bg = document.getElementById(bgId);

  if (!plotContainer || !bg) {
    return;
  }

  const backgroundImage = bg.dataset.backgroundImage;

  if (backgroundImage) {
    bg.style.display = 'block';
    bg.style.backgroundImage = `url("${backgroundImage}")`;
    bg.style.backgroundSize = '100% 100%';
    bg.style.backgroundRepeat = 'no-repeat';
    bg.style.backgroundPosition = 'center center';
    bg.style.position = 'absolute';
    bg.style.pointerEvents = 'none';
    bg.style.transformOrigin = 'center center';

    // Parse flip values - handle both boolean and string
    const flipX = bg.dataset.flipX === 'true' || bg.dataset.flipX === true;
    const flipY = bg.dataset.flipY === 'true' || bg.dataset.flipY === true;
    const scaleX = parseFloat(bg.dataset.scaleX) || 1;
    const scaleY = parseFloat(bg.dataset.scaleY) || 1;
    const opacity = parseFloat(bg.dataset.opacity);

    const finalScaleX = (flipX ? -1 : 1) * scaleX;
    const finalScaleY = (flipY ? -1 : 1) * scaleY;
    bg.style.transform = `scale(${finalScaleX}, ${finalScaleY})`;
    bg.style.opacity = isNaN(opacity) ? 1 : opacity;
    bg.style.transition = 'transform 0.5s cubic-bezier(0.4, 0, 0.2, 1), opacity 0.3s ease';

    // Get plot area size from Plotly layout
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
}

function syncProjectionBackground(plotId, backgroundImage, flipX, flipY, scaleX, scaleY, opacity) {
  const plotContainer = document.getElementById(plotId);
  if (!plotContainer) {
    return;
  }

  let parent = plotContainer.parentElement;
  const wrapperId = plotId + '_wrapper';
  let wrapper = parent && parent.id === wrapperId ? parent : null;

  if (!wrapper) {
    wrapper = document.createElement('div');
    wrapper.id = wrapperId;
    wrapper.style.position = 'relative';
    wrapper.style.width = '100%';
    wrapper.style.height = '100%';
    wrapper.style.overflow = 'hidden';
    parent.insertBefore(wrapper, plotContainer);
    wrapper.appendChild(plotContainer);
  }
  parent = wrapper;

  const bgId = plotId + '_background';
  let bg = document.getElementById(bgId);
  if (!bg) {
    bg = document.createElement('div');
    bg.id = bgId;
    bg.style.transition = 'transform 0.5s cubic-bezier(0.4, 0, 0.2, 1), opacity 0.3s ease';
    parent.insertBefore(bg, plotContainer);
  }

  // Store parameters in dataset - ensure boolean values are stored correctly
  if (backgroundImage !== undefined) bg.dataset.backgroundImage = backgroundImage || '';
  if (flipX !== undefined) bg.dataset.flipX = String(flipX === true || flipX === 'true');
  if (flipY !== undefined) bg.dataset.flipY = String(flipY === true || flipY === 'true');
  if (scaleX !== undefined) bg.dataset.scaleX = String(scaleX || 1);
  if (scaleY !== undefined) bg.dataset.scaleY = String(scaleY || 1);
  if (opacity !== undefined) bg.dataset.opacity = String(opacity === null || opacity === undefined ? 1 : opacity);

  applyProjectionBackground(plotId);

  plotContainer.style.position = 'relative';
  plotContainer.style.zIndex = '1';

  // Attach listener for plot updates
  if (!plotContainer.dataset.bgListenerAttached && typeof plotContainer.on === 'function') {
    plotContainer.on('plotly_afterplot', function () {
      applyProjectionBackground(plotId);
    });
    plotContainer.dataset.bgListenerAttached = 'true';
  }
}
