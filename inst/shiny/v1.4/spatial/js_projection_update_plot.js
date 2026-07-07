// layout for 2D projections — built from the shared factory, see
// inst/shiny/www/projection_layouts.js
const spatial_projection_layout_2D = window.cerebroProjectionLayout.make2D({
  uirevision: 'true',
});

// Inject CSS for spatial projection
// CSS for plot widgets (legends, modebar, drag tip, scroll-down,
// spatial bg) is now in inst/shiny/www/custom.css. The runtime <style>
// injection that previously lived here has been removed; the same classes
// are styled there using Fluent CSS variables.

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

shinyjs.detachModebar = function () {
  // The Plotly modebar is disabled at the source (displayModeBar: false in the
  // Plotly.react config), so there are no toolbar buttons to relocate. Plotly
  // still emits an empty .modebar-container div, and earlier renders may have
  // left a detached copy in the parent; remove both so no stray element lingers
  // in the top-left of the plot.
  const plotContainer = document.getElementById('spatial_projection');
  if (!plotContainer) return;

  const parent = plotContainer.parentElement;
  parent
    .querySelectorAll('.detached-modebar')
    .forEach((el) => el.remove());
  plotContainer
    .querySelectorAll('.modebar-container, .modebar')
    .forEach((el) => el.remove());
};

// Map the background image's data-space extent (image_bounds) onto the plot in
// PIXELS, so the image sits in the cells' fixed coordinate system instead of the
// axes bending to fit the image. Returns a rect relative to the wrapper, or null
// when bounds/axes are unavailable (caller then falls back to filling the area).
//
// This is the core of the decoupling: the scatter plot's axes are owned by the
// cell coordinates alone; the image is a passenger placed via l2p (data→pixel).
// An image larger than the spot bbox simply overflows the plot area and is
// clipped by the wrapper's overflow:hidden — the points never move.
function spatialBgRectFromBounds(plotContainer) {
  const fl = plotContainer._fullLayout;
  if (!fl || !fl._size || !fl.xaxis || !fl.yaxis) return null;
  if (typeof fl.xaxis.l2p !== 'function' || typeof fl.yaxis.l2p !== 'function') {
    return null;
  }
  const bg = document.getElementById('spatial_projection_background');
  const xmin = parseFloat(bg.dataset.boundsXmin);
  const xmax = parseFloat(bg.dataset.boundsXmax);
  const ymin = parseFloat(bg.dataset.boundsYmin);
  const ymax = parseFloat(bg.dataset.boundsYmax);
  if (![xmin, xmax, ymin, ymax].every(isFinite) || xmax <= xmin || ymax <= ymin) {
    return null;
  }
  const s = fl._size;
  // l2p returns pixels relative to the plot-area origin (s.l, s.t).
  const px = (v) => s.l + fl.xaxis.l2p(v);
  const py = (v) => s.t + fl.yaxis.l2p(v);
  const left = px(xmin);
  const right = px(xmax);
  // y axis points up, so ymax is the top edge of the image in screen space.
  const top = py(ymax);
  const bottom = py(ymin);
  return { left, top, width: right - left, height: bottom - top };
}

shinyjs.applySpatialBackground = function () {
  const plotContainer = document.getElementById('spatial_projection');
  const bg = document.getElementById('spatial_projection_background');
  if (!plotContainer || !bg) return;

  const backgroundImage = bg.dataset.backgroundImage;
  const parent = bg.parentElement;

  // Get or create the label element
  let label = document.getElementById('spatial_background_label');

  if (backgroundImage) {
    bg.style.display = 'block';
    bg.style.position = 'absolute';
    bg.style.pointerEvents = 'none';
    bg.style.zIndex = '0';

    const flipX = bg.dataset.flipX === 'true';
    const flipY = bg.dataset.flipY === 'true';
    const scaleX = parseFloat(bg.dataset.scaleX) || 1;
    const scaleY = parseFloat(bg.dataset.scaleY) || 1;
    const opacity = parseFloat(bg.dataset.opacity);
    const imgW = parseInt(bg.dataset.imgWidth) || 0;
    const imgH = parseInt(bg.dataset.imgHeight) || 0;

    // flip is a mirror on top of the placement; scale is an extra user nudge.
    const finalScaleX = (flipX ? -1 : 1) * scaleX;
    const finalScaleY = (flipY ? -1 : 1) * scaleY;

    const size =
      plotContainer._fullLayout && plotContainer._fullLayout._size
        ? plotContainer._fullLayout._size
        : null;
    // Primary path: place the image by mapping its data-space bounds to pixels,
    // so it aligns to the cells in their own (unchanged) coordinate system.
    const rect = size ? spatialBgRectFromBounds(plotContainer) : null;

    if (rect) {
      // Position the div at the mapped rect. flip/scale are applied as a CSS
      // transform about the rect centre so they mirror in place without moving
      // the image off its mapped location.
      bg.style.left = rect.left + 'px';
      bg.style.top = rect.top + 'px';
      bg.style.width = rect.width + 'px';
      bg.style.height = rect.height + 'px';
      bg.style.transformOrigin = '50% 50%';
      bg.style.transform =
        finalScaleX === 1 && finalScaleY === 1
          ? ''
          : `scale(${finalScaleX}, ${finalScaleY})`;
      if (imgW > 0 && imgH > 0) {
        // native-resolution <img>, stretched to the mapped rect
        if (!bg._imgEl) {
          const img = document.createElement('img');
          img.style.width = '100%';
          img.style.height = '100%';
          img.style.display = 'block';
          img.draggable = false;
          bg.appendChild(img);
          bg._imgEl = img;
        }
        bg._imgEl.src = backgroundImage;
        bg.style.backgroundImage = '';
      } else {
        // SVG / dimensionless raster → CSS background stretched to the rect
        if (bg._imgEl) bg._imgEl.src = '';
        bg.style.backgroundImage = `url("${backgroundImage}")`;
        bg.style.backgroundSize = '100% 100%';
        bg.style.backgroundRepeat = 'no-repeat';
      }
      bg.style.opacity = isNaN(opacity) ? 1 : opacity;

      if (!label) {
        label = document.createElement('div');
        label.id = 'spatial_background_label';
        label.innerText = 'Towards brain';
        parent.insertBefore(label, bg.nextSibling);
      }
      // Label sits at the top-centre of the plot drawing area (not the image),
      // so it stays put regardless of how far the image overflows.
      label.style.display = 'block';
      label.style.left = size.l + size.w / 2 + 'px';
      label.style.top = size.t + 8 + 'px';
      label.style.transform = 'translateX(-50%)';
    } else {
      // Fallback: no bounds or no Plotly geometry yet — fill the wrapper so the
      // image is at least visible; it will snap to the mapped rect on the next
      // afterplot once geometry is available.
      const parentW = parent.clientWidth;
      const parentH = parent.clientHeight;
      if (bg._imgEl) bg._imgEl.src = '';
      bg.style.backgroundImage = `url("${backgroundImage}")`;
      bg.style.backgroundSize = '100% 100%';
      bg.style.backgroundRepeat = 'no-repeat';
      bg.style.left = '0px';
      bg.style.top = '0px';
      bg.style.width = parentW + 'px';
      bg.style.height = parentH + 'px';
      bg.style.transformOrigin = '50% 50%';
      bg.style.transform = `scale(${finalScaleX}, ${finalScaleY})`;
      bg.style.opacity = isNaN(opacity) ? 1 : opacity;

      if (!label) {
        label = document.createElement('div');
        label.id = 'spatial_background_label';
        label.innerText = 'Towards brain';
        parent.insertBefore(label, bg.nextSibling);
      }
      label.style.display = 'block';
      label.style.left = '50%';
      label.style.top = '8px';
      label.style.transform = 'translateX(-50%)';
    }
  } else {
    bg.style.display = 'none';
    bg.style.backgroundImage = '';
    bg.style.transform = '';
    bg.style.opacity = '';
    if (bg._imgEl) bg._imgEl.src = '';

    // Hide label when no background image
    if (label) {
      label.style.display = 'none';
    }
  }
};

shinyjs.syncSpatialBackground = function (backgroundImage, flipX, flipY, scaleX, scaleY, opacity, imageBounds) {
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
  if (imageBounds !== undefined && imageBounds) {
    bg.dataset.imgWidth = String(imageBounds.img_width || 0);
    bg.dataset.imgHeight = String(imageBounds.img_height || 0);
    // Data-space extent of the image, used to place it in the cells' coordinate
    // system (see spatialBgRectFromBounds). Absent/empty → cleared so the rect
    // helper returns null and the fill-the-area fallback runs.
    if (
      imageBounds.xmin !== undefined &&
      imageBounds.xmax !== undefined &&
      imageBounds.ymin !== undefined &&
      imageBounds.ymax !== undefined
    ) {
      bg.dataset.boundsXmin = String(imageBounds.xmin);
      bg.dataset.boundsXmax = String(imageBounds.xmax);
      bg.dataset.boundsYmin = String(imageBounds.ymin);
      bg.dataset.boundsYmax = String(imageBounds.ymax);
    } else {
      delete bg.dataset.boundsXmin;
      delete bg.dataset.boundsXmax;
      delete bg.dataset.boundsYmin;
      delete bg.dataset.boundsYmax;
    }
  }

  shinyjs.applySpatialBackground();

  plotContainer.style.position = 'relative';
  plotContainer.style.zIndex = '1';

  if (!plotContainer.dataset.bgListenerAttached && typeof plotContainer.on === 'function') {
    plotContainer.on('plotly_afterplot', shinyjs.applySpatialBackground);
    plotContainer.dataset.bgListenerAttached = 'true';
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
  let itemPadding = 4; // top/bottom padding
  let itemPaddingX = 6; // left/right padding
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

    // Apply dynamic styles
    item.style.marginBottom = itemMargin + 'px';
    item.style.padding = itemPadding + 'px ' + itemPaddingX + 'px';

    const colorBox = document.createElement('span');
    colorBox.className = 'legend-color-box';
    colorBox.style.backgroundColor = colors[index];
    // Apply dynamic box size
    colorBox.style.width = boxSize + 'px';
    colorBox.style.height = boxSize + 'px';

    const text = document.createElement('span');
    text.className = 'legend-text';
    text.innerText = traceName;
    // Apply dynamic font size
    text.style.fontSize = fontSize + 'px';

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
const spatial_projection_layout_3D = window.cerebroProjectionLayout.make3D({
  uirevision: 'true',
});

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
  params = shinyjs.getParams(params, spatial_projection_default_params);

  // Preserve an existing selection (dimming + outline) across the re-render.
  const selectedKeys = harvestSpatialSelection();
  const selectionOutline = harvestSelectionOutline();

  shinyjs.removeCustomLegend();
  shinyjs.removeContinuousLegend();
  const data = [];
  const colorArray = params.data.color;
  const colorMin = Math.min(...colorArray);
  const colorMax = Math.max(...colorArray);
  // Fluent blue ramp (matches CSS --theme-primary #0f6cbd family)
  const colorscale = [
    [0,   '#f7fbff'],
    [0.2, '#dbeaf6'],
    [0.4, '#a4c8e1'],
    [0.6, '#5e9bc7'],
    [0.8, '#2c7ab3'],
    [1,   '#0c4b85'],
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
  // Keep the prior selection: selected cells stay solid, the rest dim.
  applySpatialSelection(data, selectedKeys);
  shinyjs.createContinuousLegend(params.meta.color_variable, colorMin, colorMax, colorscale);

  // Use deep clone to avoid mutating global layout
  const layout_here = JSON.parse(JSON.stringify(spatial_projection_layout_2D));

  // Carry the dashed selection outline across the re-render.
  if (selectionOutline) layout_here.selections = selectionOutline;

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

  Plotly.react('spatial_projection', data, layout_here, {
    displayModeBar: false,
    displaylogo: false
  }).then(() => {
    // Re-attach selection debug listeners
    if (typeof shinyjs.setupSelectionDebug === 'function') {
      shinyjs.setupSelectionDebug();
    }

    shinyjs.syncSpatialBackground(
      params.meta.background_image,
      params.meta.background_flip_x,
      params.meta.background_flip_y,
      params.meta.background_scale_x,
      params.meta.background_scale_y,
      params.meta.background_opacity,
      params.meta.image_bounds
    );
    shinyjs.detachModebar();
  });
};

// update 3D projection with continuous coloring
shinyjs.updatePlot3DContinuousSpatial = function (params) {
  params = shinyjs.getParams(params, spatial_projection_default_params);
  const selectedKeys = harvestSpatialSelection();
  shinyjs.removeCustomLegend();
  shinyjs.removeContinuousLegend();
  const data = [];
  const colorArray = params.data.color;
  const colorMin = Math.min(...colorArray);
  const colorMax = Math.max(...colorArray);
  // Fluent blue ramp (matches CSS --theme-primary #0f6cbd family)
  const colorscale = [
    [0,   '#f7fbff'],
    [0.2, '#dbeaf6'],
    [0.4, '#a4c8e1'],
    [0.6, '#5e9bc7'],
    [0.8, '#2c7ab3'],
    [1,   '#0c4b85'],
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
  applySpatialSelection(data, selectedKeys);
  shinyjs.createContinuousLegend(params.meta.color_variable, colorMin, colorMax, colorscale);

  // Use deep clone
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
  Plotly.react('spatial_projection', data, layout_here, {
    displayModeBar: false,
    displaylogo: false
  }).then(() => {
    shinyjs.syncSpatialBackground(null, false, false, 1, 1, 1, null);
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

// Persistent selection, decoupled from every plot parameter. Selection is keyed
// on cell position (x-y), so it survives changes to the colouring variable,
// point size, opacity, "show % of cells", background, etc. It is populated only
// by an actual box/lasso selection (plotly_selected) and cleared only by
// plotly_deselect or the Clear-selection action (button / Esc / Delete). Every
// re-render re-applies it, so no plot parameter can drop or corrupt it.
//
// This is also the single source of truth for the R side: whenever it changes
// we push the selected coordinates to a Shiny input, so the selected-cells
// table/count and the Clear button follow the persistent selection instead of
// Plotly's volatile plotly_selected event (which a re-render wipes).
let spatialSelectionKeys = null;

// Push the current selection to Shiny as {x: [...], y: [...]} (or null). R
// rebuilds its "x-y" identifiers from these, matching how the table keys cells.
function syncSpatialSelectionToShiny() {
  if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
  let payload = null;
  if (spatialSelectionKeys && spatialSelectionKeys.size) {
    const x = [];
    const y = [];
    spatialSelectionKeys.forEach((k) => {
      const sep = k.indexOf('|');
      x.push(parseFloat(k.slice(0, sep)));
      y.push(parseFloat(k.slice(sep + 1)));
    });
    payload = { x: x, y: y };
  }
  // No {priority:'event'}: this is persistent selection STATE that must remain
  // readable across later reactive invalidations (e.g. a colour change
  // re-running the selected-cells reactive). An event-priority input resets to
  // null after one flush, which would drop the selection on the next re-render.
  Shiny.setInputValue('spatial_persistent_selection', payload);
}

// Record a fresh selection from a plotly_selected event.
function setSpatialSelectionFromEvent(eventData) {
  if (!eventData || !eventData.points || !eventData.points.length) {
    spatialSelectionKeys = null;
  } else {
    const keys = new Set();
    eventData.points.forEach((p) => {
      keys.add(p.x + '|' + p.y);
    });
    spatialSelectionKeys = keys;
  }
  syncSpatialSelectionToShiny();
}

// Return the persistent selection (or null when nothing is selected).
function harvestSpatialSelection() {
  return spatialSelectionKeys && spatialSelectionKeys.size
    ? spatialSelectionKeys
    : null;
}

// Re-apply a harvested selection to freshly built traces: for each trace, mark
// the points whose x-y is in the set as selectedpoints so Plotly keeps them at
// full opacity and dims the rest. No-op when there is no active selection.
function applySpatialSelection(traces, selectedKeys) {
  if (!selectedKeys || selectedKeys.size === 0) return;
  traces.forEach((trace) => {
    if (!trace.x || trace.mode === 'text') return;
    const picked = [];
    for (let i = 0; i < trace.x.length; i++) {
      if (selectedKeys.has(trace.x[i] + '|' + trace.y[i])) picked.push(i);
    }
    if (picked.length) trace.selectedpoints = picked;
  });
}

// The dashed selection outline lives in layout.selections and is a layout-level
// state that Plotly.react drops when it swaps in a fresh layout. Grab it from
// the live plot so it can be carried into the new layout, but only while a
// selection is active (cleared selection must not resurrect the outline).
function harvestSelectionOutline() {
  if (!spatialSelectionKeys || !spatialSelectionKeys.size) return null;
  const pc = document.getElementById('spatial_projection');
  if (pc && pc.layout && pc.layout.selections && pc.layout.selections.length) {
    return pc.layout.selections;
  }
  return null;
}

// update 2D projection with categorical coloring
shinyjs.updatePlot2DCategoricalSpatial = function (params) {
  params = shinyjs.getParams(params, spatial_projection_default_params);

  // Preserve an existing selection (dimming + outline) across the re-render.
  const selectedKeys = harvestSpatialSelection();
  const selectionOutline = harvestSelectionOutline();

  shinyjs.removeContinuousLegend();
  shinyjs.createCustomLegend(params.meta.traces, params.data.color);

  // Optimization: Use map instead of loop push
  const data = params.data.x.map((xVal, i) => ({
    x: xVal,
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
  }));

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

  // Keep the prior selection: selected cells stay solid, the rest dim.
  applySpatialSelection(data, selectedKeys);

  // Use deep clone
  const layout_here = JSON.parse(JSON.stringify(spatial_projection_layout_2D));

  // Carry the dashed selection outline across the re-render.
  if (selectionOutline) layout_here.selections = selectionOutline;

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

  Plotly.react('spatial_projection', data, layout_here, {
    displayModeBar: false,
    displaylogo: false
  }).then(() => {
    // Re-attach selection debug listeners
    if (typeof shinyjs.setupSelectionDebug === 'function') {
      shinyjs.setupSelectionDebug();
    }

    shinyjs.syncSpatialBackground(
      params.meta.background_image,
      params.meta.background_flip_x,
      params.meta.background_flip_y,
      params.meta.background_scale_x,
      params.meta.background_scale_y,
      params.meta.background_opacity,
      params.meta.image_bounds
    );
    shinyjs.detachModebar();
  });
};

// update 3D projection with categorical coloring
shinyjs.updatePlot3DCategoricalSpatial = function (params) {
  params = shinyjs.getParams(params, spatial_projection_default_params);
  const selectedKeys = harvestSpatialSelection();
  shinyjs.removeContinuousLegend();
  shinyjs.createCustomLegend(params.meta.traces, params.data.color);

  // Optimization: Use map
  const data = params.data.x.map((xVal, i) => ({
    x: xVal,
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

  // Keep the prior selection: selected cells stay solid, the rest dim.
  applySpatialSelection(data, selectedKeys);

  // Use deep clone
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
  Plotly.react('spatial_projection', data, layout_here, {
    displayModeBar: false,
    displaylogo: false
  }).then(() => {
    shinyjs.syncSpatialBackground(null, false, false, 1, 1, 1, null);
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
    return;
  }

  // plotContainer.on only exists once Plotly has turned the div into a graph
  // object. The MutationObserver below can fire before that, so guard the call
  // to avoid "plotContainer.on is not a function" errors during mounting.
  if (typeof plotContainer.on !== 'function') {
    return;
  }

  // Capture a real box/lasso selection (points present). We deliberately do NOT
  // clear the selection on plotly_deselect: Plotly fires deselect during every
  // re-render (colour/point-size change swaps the trace set), which would wipe a
  // selection the user never touched. Per the desired UX, the selection is
  // cleared ONLY by the Clear-selection action (button / Esc / Delete); an empty
  // selection event (no points) also clears it.
  plotContainer.on('plotly_selected', function (eventData) {
    if (eventData && eventData.points && eventData.points.length) {
      setSpatialSelectionFromEvent(eventData);
    }
  });
};

// Debug function to check plot configuration
shinyjs.debugPlotConfig = function () {
  const plotContainer = document.getElementById('spatial_projection');
  if (!plotContainer) {
    return;
  }
  // Debug info suppressed for production
};

// Auto-setup debug when document is ready
$(document).ready(function () {
  // Wait for plot to be initialized
  setTimeout(function () {
    shinyjs.setupSelectionDebug();
  }, 2000);

  // Also setup on any plot update
  const observer = new MutationObserver(function (mutations) {
    const plotContainer = document.getElementById('spatial_projection');
    if (plotContainer && !plotContainer.dataset.debugListenerAttached) {
      shinyjs.setupSelectionDebug();
      plotContainer.dataset.debugListenerAttached = 'true';
    }
  });

  observer.observe(document.body, { childList: true, subtree: true });

  // Keyboard shortcut: Delete / Backspace / Escape clears the current spatial
  // selection, mirroring the Clear-selection button. Only acts when the Spatial
  // tab is visible and a selection exists (its button is shown), and never while
  // the user is typing in an input/textarea/select.
  document.addEventListener('keydown', function (e) {
    if (e.key !== 'Delete' && e.key !== 'Backspace' && e.key !== 'Escape') {
      return;
    }
    const tab = document.getElementById('shiny-tab-spatial');
    if (!tab || tab.offsetParent === null) return; // tab not active/visible
    const tag = (e.target.tagName || '').toLowerCase();
    if (
      tag === 'input' ||
      tag === 'textarea' ||
      tag === 'select' ||
      e.target.isContentEditable
    ) {
      return;
    }
    const btn = document.getElementById('spatial_projection_clear_selection');
    // btn is wrapped in shinyjs::hidden() until a selection exists; offsetParent
    // is null while hidden, so this is a no-op when there is nothing to clear.
    if (btn && btn.offsetParent !== null) {
      e.preventDefault();
      btn.click();
    }
  });
});

// Clear selection on the spatial projection plot
shinyjs.spatialClearSelection = function () {
  // Forget the persistent selection so subsequent re-renders don't re-apply it,
  // and tell R so the count/table/button reset too.
  spatialSelectionKeys = null;
  syncSpatialSelectionToShiny();
  const plotContainer = document.getElementById('spatial_projection');
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
      'spatial_projection',
      { selectedpoints: null }, // Reset selected points for all traces
      { selections: [], dragmode: 'select' } // Clear selection box, keep select mode
    ).then(function () {
      // Emit deselect event after update completes
      plotContainer.emit('plotly_deselect');
    });
  }
};
