// =============================================================================
// Shared projection-scatter renderer.
//
// This is spatial's mature projection JS (custom top legend, persistent x|y
// selection, group labels, group hulls, modebar-off, container sizing),
// GENERALISED so every projection tab (spatial / overview / gene_expression /
// trajectory / immune_repertoire) renders through the SAME code.
//
// The one real generalisation is PLOT-ID NAMESPACING. spatial hardcoded
// 'spatial_projection' as the DOM id, the Plotly.react target, the event
// source and the Shiny input key. Here every render call carries its own plot
// id in `params.meta.plot_id`; the legend-container id, the persistent-
// selection input key (`<plot_id>_persistent_selection`) and the Plotly target
// all derive from it. Per-plot state (the selection key set) lives in a Map
// keyed by plot id, so two projection plots never share a selection.
//
// Tab-specific features (histology background, convex-hull outlines,
// co-expression RGB colouring, multi-panel, trajectory-line shapes, IR grey
// "Other cells" background) are OPTIONAL params — a tab that doesn't use one
// simply omits it / passes an empty list.
//
// This file is prepended into each tab's extendShinyjs(text=) via
// paste(read_file(projection_scatter.js), read_file(tab_js)) exactly like the
// existing projection_layouts.js, so all functions share one global scope. It
// is idempotent: the IIFE assigns to window, safe to run once per tab.
// =============================================================================

(function initializeProjection() {
  if (window.cerebroProjection && window.cerebroProjection.__ready) {
    return;
  }
  if (!window.cerebroViewport) {
    window.addEventListener(
      'cerebro:viewport-ready',
      initializeProjection,
      { once: true }
    );
    return;
  }

  // --- per-plot state ---------------------------------------------------------
  // Persistent selection, decoupled from every plot parameter. Keyed on cell
  // position (x|y), so it survives changes to the colouring variable, point
  // size, opacity, "show % of cells", background, etc. Held per plot id, so
  // each projection plot has its own independent selection.
  const selectionByPlot = new Map(); // plotId -> Set<"x|y"> (or absent)

  // Groups hidden via the legend, per plot. Pushed to Shiny so the selected-
  // cells count/panels can exclude cells in hidden groups. Kept separate from
  // the Plotly trace visibility so it survives re-renders that rebuild traces.
  const hiddenGroupsByPlot = new Map(); // plotId -> Set<groupName>
  function setHiddenGroup(plotId, groupName, hidden) {
    let set = hiddenGroupsByPlot.get(plotId);
    if (!set) {
      set = new Set();
      hiddenGroupsByPlot.set(plotId, set);
    }
    if (hidden) {
      set.add(groupName);
    } else {
      set.delete(groupName);
    }
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue(plotId + '_hidden_groups', Array.from(set));
    }
  }

  // The colouring variable the hidden set was recorded against. When it changes,
  // the group labels are a different set, so the old hidden state is meaningless
  // (and would wrongly hide same-named groups under the new variable); drop it.
  const hiddenGroupsVariableByPlot = new Map(); // plotId -> color_variable
  function clearHiddenGroups(plotId) {
    const set = hiddenGroupsByPlot.get(plotId);
    if (set && set.size) {
      set.clear();
      if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue(plotId + '_hidden_groups', []);
      }
    }
  }
  function syncHiddenGroupsForVariable(plotId, colorVariable) {
    if (hiddenGroupsVariableByPlot.get(plotId) !== colorVariable) {
      clearHiddenGroups(plotId);
      hiddenGroupsVariableByPlot.set(plotId, colorVariable);
    }
  }
  function isGroupHidden(plotId, groupName) {
    const set = hiddenGroupsByPlot.get(plotId);
    return !!(set && set.has(groupName));
  }

  // --- viewport sizing -------------------------------------------------------
  // The shared controller owns frame scheduling, scoped observation, target-
  // height maths and the two-equal-measurement reveal gate. This adapter keeps
  // only Plotly-specific DOM measurement and relayout work.
  const viewport = window.cerebroViewport;

  // withSpinner() adds one wrapper whose bounds are exactly the output bounds.
  // Without a spinner, Plotly itself is the sizing element; its parent is the
  // whole box body and may also contain legends, footers and other controls.
  // Treating that parent as the plot double-counts its children and makes the
  // box taller than the viewport (Trajectory is intentionally spinner-free).
  function projectionSizingElement(plot) {
    const parent = plot && plot.parentElement;
    if (
      parent &&
      parent.classList &&
      parent.classList.contains('shiny-spinner-output-container')
    ) {
      return parent;
    }
    return plot;
  }

  function projectionElements(plot) {
    if (typeof plot === 'string') plot = document.getElementById(plot);
    if (!plot) return null;
    const wrapper = projectionSizingElement(plot);
    const box = typeof plot.closest === 'function' ? plot.closest('.box') : null;
    if (!box || !wrapper.getBoundingClientRect || !box.getBoundingClientRect) {
      return null;
    }
    return {
      plot: plot,
      wrapper: wrapper,
      box: box,
      legend: document.getElementById(plot.id + '_legend'),
    };
  }

  // CSS visibility on the stable gate must remain the first-paint mechanism:
  // Plotly writes its own inline visibility and htmlwidgets may rebuild the plot
  // node. Reading the CURRENT gate class also makes a rebuilt Clonal UMAP host
  // go through reveal again instead of inheriting stale plot-id state.
  const PROJECTION_GATE_CLASS = 'cerebro-viewport-gate';
  const PROJECTION_SIZED_CLASS = 'is-sized';
  function shouldRevealProjection(fullLayoutPresent, height, settledHeight) {
    return Boolean(fullLayoutPresent) &&
      viewport._shouldReveal(settledHeight, height);
  }
  function projectionGate(plot) {
    return plot && typeof plot.closest === 'function'
      ? plot.closest('.' + PROJECTION_GATE_CLASS)
      : null;
  }
  function revealProjectionHost(plot) {
    const gate = projectionGate(plot);
    if (gate && gate.classList) gate.classList.add(PROJECTION_SIZED_CLASS);
  }

  function hideProjectionHost(plot) {
    const gate = projectionGate(plot);
    if (gate && gate.classList) gate.classList.remove(PROJECTION_SIZED_CLASS);
  }

  function plotlySizeMatches(plot, height, width) {
    const layout = plot && plot._fullLayout;
    return Boolean(
      layout &&
      Math.abs(layout.height - height) <= 1 &&
      Math.abs(layout.width - width) <= 1
    );
  }

  const projectionViewportAdapter = {
    gap: 18,
    minimum: 240,
    hide: hideProjectionHost,
    measure: function (plot) {
      const elements = projectionElements(plot);
      if (!elements) return null;
      const wrapperRect = elements.wrapper.getBoundingClientRect();
      const boxRect = elements.box.getBoundingClientRect();
      return {
        top: wrapperRect.top,
        contentBelow: Math.max(0, boxRect.bottom - wrapperRect.bottom),
        width: wrapperRect.width,
      };
    },
    matches: plotlySizeMatches,
    apply: function (plot, height, width, state, done) {
      const elements = projectionElements(plot);
      if (!elements) return;
      elements.wrapper.style.height = height + 'px';
      plot.style.height = height + 'px';

      const fullLayout = plot._fullLayout;
      if (
        typeof Plotly !== 'undefined' &&
        fullLayout &&
        typeof Plotly.relayout === 'function' &&
        !plotlySizeMatches(plot, height, width)
      ) {
        // scattergl redraw is asynchronous. The controller must not reveal the
        // gate until this promise settles and the live Plotly layout matches.
        if (state.relayoutPending) return;
        state.relayoutPending = true;
        const finish = function () {
          state.relayoutPending = false;
          done();
        };
        const relayout = Plotly.relayout(plot, {
          width: width,
          height: height,
          'transition.duration': 0,
        });
        if (relayout && typeof relayout.then === 'function') {
          relayout.then(finish, finish);
        } else {
          finish();
        }
      } else if (
        typeof Plotly !== 'undefined' &&
        Plotly.Plots &&
        Plotly.Plots.resize
      ) {
        Plotly.Plots.resize(plot);
      }
    },
    ready: function (plot, height, width, state) {
      return Boolean(
        plot._fullLayout &&
        !state.relayoutPending &&
        plotlySizeMatches(plot, height, width)
      );
    },
    isRevealed: function (plot) {
      const gate = projectionGate(plot);
      return Boolean(
        gate && gate.classList.contains(PROJECTION_SIZED_CLASS)
      );
    },
    reveal: revealProjectionHost,
    observeTargets: function (plot) {
      const elements = projectionElements(plot);
      return elements ? [elements.box, elements.legend].filter(Boolean) : [];
    },
  };

  function scheduleProjectionResize(plotId) {
    const plot = document.getElementById(plotId);
    if (plot && viewport) viewport.register(plot, projectionViewportAdapter);
  }

  function getSelection(plotId) {
    const s = selectionByPlot.get(plotId);
    return s && s.size ? s : null;
  }

  // Push the current selection to Shiny as {x:[...], y:[...]} (or null) under
  // `<plot_id>_persistent_selection`. R rebuilds its "x-y" identifiers from
  // these, matching how the selected-cells table keys cells. No
  // {priority:'event'}: this is persistent selection STATE that must remain
  // readable across later reactive invalidations (an event-priority input
  // resets to null after one flush, dropping the selection on the next
  // re-render).
  function syncSelectionToShiny(plotId) {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    const keys = selectionByPlot.get(plotId);
    let payload = null;
    if (keys && keys.size) {
      const x = [];
      const y = [];
      keys.forEach((k) => {
        const sep = k.indexOf('|');
        x.push(parseFloat(k.slice(0, sep)));
        y.push(parseFloat(k.slice(sep + 1)));
      });
      payload = { x: x, y: y };
    }
    Shiny.setInputValue(plotId + '_persistent_selection', payload);
  }

  // Record a fresh selection from a plotly_selected event.
  function setSelectionFromEvent(plotId, eventData) {
    if (!eventData || !eventData.points || !eventData.points.length) {
      selectionByPlot.delete(plotId);
    } else {
      const keys = new Set();
      eventData.points.forEach((p) => {
        keys.add(p.x + '|' + p.y);
      });
      selectionByPlot.set(plotId, keys);
    }
    syncSelectionToShiny(plotId);
  }

  // Re-apply a harvested selection to freshly built traces: mark the points
  // whose x|y is in the set as selectedpoints so Plotly keeps them at full
  // opacity and dims the rest. No-op when nothing is selected.
  function applySelection(traces, selectedKeys) {
    if (!selectedKeys || selectedKeys.size === 0) return;
    traces.forEach((trace) => {
      if (!trace.x || trace.mode === 'text' || trace.mode === 'lines') return;
      const picked = [];
      for (let i = 0; i < trace.x.length; i++) {
        if (selectedKeys.has(trace.x[i] + '|' + trace.y[i])) picked.push(i);
      }
      if (picked.length) trace.selectedpoints = picked;
    });
  }

  // The dashed selection outline lives in layout.selections, a layout-level
  // state that Plotly.react drops when it swaps in a fresh layout. Grab it from
  // the live plot so it can be carried over, but only while a selection is
  // active (a cleared selection must not resurrect the outline).
  function harvestSelectionOutline(plotId) {
    if (!getSelection(plotId)) return null;
    const pc = document.getElementById(plotId);
    if (pc && pc.layout && pc.layout.selections && pc.layout.selections.length) {
      return pc.layout.selections;
    }
    return null;
  }

  // --- zoom to selection ------------------------------------------------------
  // Extra span added to the framed box so the zoom never butts the selection
  // right up against the plot edge — a little breathing room all around. This is
  // the TOTAL fractional growth (split evenly, so each side gets half). Applied
  // after the aspect-ratio match, so it scales both axes equally and the 1:1
  // data-per-pixel is preserved.
  const ZOOM_FRAME_PADDING = 0.08;

  // Expand a data-space selection box to the plot area's pixel aspect ratio so
  // that after zooming one data unit spans the same number of pixels on x and y
  // — the selected region is framed, never stretched. We only ever GROW the
  // shorter axis (and centre the box within it), so the whole selection stays
  // visible with letterbox whitespace on the longer side, then add a uniform
  // margin on all sides. A zero-area (single point / degenerate) selection is
  // padded to a tiny non-zero span first, so the ratio maths never divides by
  // zero and Plotly gets a valid range.
  function computeEqualAspectRange(xMin, xMax, yMin, yMax, pxW, pxH) {
    let dataW = xMax - xMin;
    let dataH = yMax - yMin;
    let cx = (xMin + xMax) / 2;
    let cy = (yMin + yMax) / 2;
    // Degenerate box: give it a small span around its centre.
    if (!(dataW > 0)) dataW = Math.max(Math.abs(cx), 1) * 1e-3 || 1e-3;
    if (!(dataH > 0)) dataH = Math.max(Math.abs(cy), 1) * 1e-3 || 1e-3;
    // Guard against a zero/invalid pixel box (pre-layout): fall back to square.
    const pxRatio = pxW > 0 && pxH > 0 ? pxW / pxH : 1;
    // Target: dataW/dataH === pxW/pxH. Grow whichever axis is too short.
    if (dataW / dataH < pxRatio) {
      dataW = dataH * pxRatio; // widen x
    } else {
      dataH = dataW / pxRatio; // heighten y
    }
    // Uniform margin on all sides (scales both axes equally -> ratio unchanged).
    const scale = 1 + ZOOM_FRAME_PADDING;
    dataW *= scale;
    dataH *= scale;
    return {
      xRange: [cx - dataW / 2, cx + dataW / 2],
      yRange: [cy - dataH / 2, cy + dataH / 2],
    };
  }

  // Data-space bounding box of the currently selected points (or null).
  function selectionBounds(plotId) {
    const keys = getSelection(plotId);
    if (!keys) return null;
    let xMin = Infinity, xMax = -Infinity, yMin = Infinity, yMax = -Infinity;
    keys.forEach((k) => {
      const sep = k.indexOf('|');
      const x = parseFloat(k.slice(0, sep));
      const y = parseFloat(k.slice(sep + 1));
      if (Number.isFinite(x)) {
        if (x < xMin) xMin = x;
        if (x > xMax) xMax = x;
      }
      if (Number.isFinite(y)) {
        if (y < yMin) yMin = y;
        if (y > yMax) yMax = y;
      }
    });
    if (xMin === Infinity || yMin === Infinity) return null;
    return { xMin, xMax, yMin, yMax };
  }

  // Which plots are currently zoomed into their selection. Drives the toggle
  // (zoom in vs. reset) and the button style/label reported to Shiny.
  const zoomedPlots = new Set();
  // Plotly's native selection rectangle stashed on zoom-in (per plot), so reset
  // can restore it after it was cleared to hide the editable drag handles.
  const zoomSavedSelections = new Map();

  // Pure decision for the zoom toggle. Not zoomed -> zoom in and LOCK the plot
  // (dragmode false: no box-select while zoomed, so the user must reset before
  // selecting again). Zoomed -> reset to the full view and UNLOCK (dragmode
  // 'select'). Returned `zoomed` is the state to report to Shiny for the button.
  function nextZoomAction(isZoomed) {
    return isZoomed
      ? { zoomIn: false, zoomed: false, dragmode: 'select' }
      : { zoomIn: true, zoomed: true, dragmode: false };
  }

  // Report the zoom state to Shiny so the button can switch style/label. Named
  // <plot_id>_zoom_state; true while zoomed into the selection.
  function reportZoomState(plotId, zoomed) {
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue(plotId + '_zoom_state', zoomed);
    }
  }

  // Toggle zoom-to-selection. Frames the selection at the true data aspect ratio
  // (no stretch) and locks selection while zoomed; toggling again resets to the
  // full autorange view and unlocks. The persistent selection and data are
  // untouched, so selected points stay highlighted throughout.
  // A dashed rectangle marking the selection bounds. While zoomed the plot is
  // locked (dragmode:false), which hides Plotly's own selection outline, so we
  // draw our own layout shape instead — it is independent of dragmode, so the
  // user still sees exactly which region was zoomed. Tagged via `name` so it can
  // be stripped on reset without touching tab-specific shapes (trajectory path).
  const ZOOM_MARKER_NAME = 'cerebro-zoom-marker';
  function zoomMarkerShape(bounds) {
    return {
      name: ZOOM_MARKER_NAME,
      type: 'rect',
      xref: 'x',
      yref: 'y',
      x0: bounds.xMin,
      x1: bounds.xMax,
      y0: bounds.yMin,
      y1: bounds.yMax,
      line: { color: '#2c7ab3', width: 1.5, dash: 'dash' },
      fillcolor: 'rgba(44, 122, 179, 0.015)',
      layer: 'above',
    };
  }
  // Current shapes minus any previous zoom marker (keeps tab shapes intact).
  function shapesWithoutZoomMarker(plot) {
    const shapes = (plot._fullLayout && plot._fullLayout.shapes) || [];
    return shapes.filter((s) => s && s.name !== ZOOM_MARKER_NAME);
  }

  function toggleZoom(plotId) {
    const plot = document.getElementById(plotId);
    if (!plot || !plot._fullLayout || typeof Plotly === 'undefined') return;
    const action = nextZoomAction(zoomedPlots.has(plotId));

    if (action.zoomIn) {
      const bounds = selectionBounds(plotId);
      if (!bounds) return; // nothing selected -> nothing to zoom into
      const xa = plot._fullLayout.xaxis;
      const ya = plot._fullLayout.yaxis;
      // Plotly stores each axis's pixel length on _length after layout.
      const pxW = xa && xa._length ? xa._length : plot._fullLayout.width;
      const pxH = ya && ya._length ? ya._length : plot._fullLayout.height;
      const r = computeEqualAspectRange(
        bounds.xMin,
        bounds.xMax,
        bounds.yMin,
        bounds.yMax,
        pxW,
        pxH
      );
      const shapes = shapesWithoutZoomMarker(plot).concat([
        zoomMarkerShape(bounds),
      ]);
      // Stash Plotly's native selection rectangle so it can be restored on
      // reset, then drop it: while zoomed we show our own static marker, so the
      // native editable outline (with its drag handles) would be a redundant,
      // misleading "you can still drag this" affordance. Clearing it also hides
      // those drag handles.
      zoomSavedSelections.set(plotId, harvestSelectionOutline(plotId));
      Plotly.relayout(plot, {
        'xaxis.autorange': false,
        'yaxis.autorange': false,
        'xaxis.range': r.xRange,
        'yaxis.range': r.yRange,
        dragmode: action.dragmode,
        shapes: shapes,
        selections: [],
      });
      zoomedPlots.add(plotId);
    } else {
      // Restore the native selection outline harvested on zoom-in, so returning
      // to the full view leaves the box exactly as it was before zooming.
      const savedSelections = zoomSavedSelections.get(plotId) || [];
      zoomSavedSelections.delete(plotId);
      Plotly.relayout(plot, {
        'xaxis.autorange': true,
        'yaxis.autorange': true,
        dragmode: action.dragmode,
        shapes: shapesWithoutZoomMarker(plot),
        selections: savedSelections,
      });
      zoomedPlots.delete(plotId);
    }
    reportZoomState(plotId, action.zoomed);
  }

  // --- legend -----------------------------------------------------------------
  // Min/max over an array WITHOUT the spread operator. `Math.min(...arr)`
  // overflows V8's argument/stack limit (~1e5) on full Xenium/MERFISH slides.
  function finiteExtent(arr) {
    let min = Infinity;
    let max = -Infinity;
    for (let i = 0; i < arr.length; i++) {
      const v = arr[i];
      if (typeof v === 'number' && Number.isFinite(v)) {
        if (v < min) min = v;
        if (v > max) max = v;
      }
    }
    if (min === Infinity) return { min: 0, max: 0 };
    return { min, max };
  }

  // Resolve where the legend bar should live and (re)insert it there. The
  // plotly html-widget wraps the plot div in a fixed-height, overflow:hidden
  // wrapper. A legend inside that wrapper competes with the plot for the fixed
  // height and gets clipped when it wraps. Instead the legend is inserted as a
  // SIBLING of the wrapper (one level up, in the auto-height spinner/box-body
  // container), so a taller legend grows the container, never the plot.
  function ensureLegendContainer(plotId) {
    const plotContainer = document.getElementById(plotId);
    if (!plotContainer) return null;
    const widgetWrapper = plotContainer.parentElement || plotContainer;
    const host = widgetWrapper.parentElement || widgetWrapper;
    const legendId = plotId + '_legend';

    let legendContainer = document.getElementById(legendId);
    if (!legendContainer) {
      legendContainer = document.createElement('div');
      legendContainer.id = legendId;
      // Generic class so ONE CSS block styles every projection's legend.
      legendContainer.className = 'cerebro-projection-legend';
    }
    if (
      legendContainer.parentElement !== host ||
      legendContainer.nextElementSibling !== widgetWrapper
    ) {
      host.insertBefore(legendContainer, widgetWrapper);
    }
    return legendContainer;
  }

  function createCustomLegend(plotId, traces, colors) {
    const legendContainer = ensureLegendContainer(plotId);
    if (!legendContainer) return;

    legendContainer.innerHTML = '';
    legendContainer.style.display = 'flex';
    legendContainer.classList.remove('is-continuous');

    const count = traces.length;
    let fontSize = 13;
    let itemMargin = 6;
    let itemPadding = 4;
    let itemPaddingX = 6;
    let boxSize = 16;

    if (count > 10) {
      if (count <= 20) {
        fontSize = 12; itemMargin = 4; itemPadding = 3; boxSize = 14;
      } else if (count <= 30) {
        fontSize = 11; itemMargin = 3; itemPadding = 2; boxSize = 12;
      } else if (count <= 50) {
        fontSize = 10; itemMargin = 2; itemPadding = 1; boxSize = 10;
      } else {
        fontSize = 9; itemMargin = 1; itemPadding = 0; boxSize = 8;
      }
    }

    traces.forEach((traceName, index) => {
      const item = document.createElement('div');
      item.className = 'custom-legend-item';
      // Reflect a group hidden on a previous render (its trace is rebuilt as
      // 'legendonly') so the fresh legend item reads as disabled rather than
      // enabled-while-Shiny-still-excludes-it.
      if (isGroupHidden(plotId, traceName)) {
        item.classList.add('legend-item-hidden');
      }
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

      // Toggle visibility on click. Trace index corresponds to legend index.
      item.onclick = function () {
        if (legendContainer.dataset.isDragging === 'true') return;
        const plot = document.getElementById(plotId);
        let isVisible = true;
        if (plot.data && plot.data[index]) {
          isVisible =
            plot.data[index].visible !== false &&
            plot.data[index].visible !== 'legendonly';
        }
        const newVisible = isVisible ? false : true;
        Plotly.restyle(plotId, { visible: newVisible }, [index]);
        item.classList.toggle('legend-item-hidden', isVisible);
        // Report the now-hidden groups to Shiny so the selected-cells count and
        // panels can drop cells in hidden groups. traceName is the group label.
        setHiddenGroup(plotId, traceName, !newVisible);
      };

      legendContainer.appendChild(item);
    });
    scheduleProjectionResize(plotId);
  }

  function removeCustomLegend(plotId) {
    const legendContainer = document.getElementById(plotId + '_legend');
    if (legendContainer) legendContainer.style.display = 'none';
    scheduleProjectionResize(plotId);
  }

  function createContinuousLegend(plotId, title, colorMin, colorMax, colorscale) {
    const legendContainer = ensureLegendContainer(plotId);
    if (!legendContainer) return;

    legendContainer.innerHTML = '';
    legendContainer.style.display = 'flex';
    legendContainer.classList.add('is-continuous');

    const header = document.createElement('div');
    header.className = 'legend-header';
    const titleEl = document.createElement('span');
    titleEl.className = 'legend-title-text';
    titleEl.innerText = title;
    header.appendChild(titleEl);
    legendContainer.appendChild(header);

    const contentEl = document.createElement('div');
    contentEl.className = 'continuous-legend-content';

    const gradientEl = document.createElement('div');
    gradientEl.className = 'continuous-legend-gradient';
    const gradientColors = colorscale.map((item) => item[1]).join(', ');
    gradientEl.style.background = `linear-gradient(to right, ${gradientColors})`;

    const minLabel = document.createElement('span');
    minLabel.className = 'continuous-legend-label';
    minLabel.innerText = colorMin.toFixed(2);

    const maxLabel = document.createElement('span');
    maxLabel.className = 'continuous-legend-label';
    maxLabel.innerText = colorMax.toFixed(2);

    contentEl.appendChild(minLabel);
    contentEl.appendChild(gradientEl);
    contentEl.appendChild(maxLabel);
    legendContainer.appendChild(contentEl);
    scheduleProjectionResize(plotId);
  }

  function removeContinuousLegend(plotId) {
    const legendContainer = document.getElementById(plotId + '_legend');
    if (legendContainer) {
      legendContainer.classList.remove('is-continuous');
      legendContainer.style.display = 'none';
    }
    scheduleProjectionResize(plotId);
  }

  // Sequential blue ramp for continuous colouring, in the --c-blue family so it
  // reads as the same design system as the signal/accent tokens.
  const CONTINUOUS_COLORSCALE = [
    [0,   '#f7fbff'],
    [0.2, '#dbeaf6'],
    [0.4, '#a4c8e1'],
    [0.6, '#5e9bc7'],
    [0.8, '#2c7ab3'],
    [1,   '#0c4b85'],
  ];

  // gene_expression lets the user pick a Plotly NAMED colorscale (a string like
  // 'YlGnBu'). Plotly's marker accepts the string directly, but the custom
  // continuous legend needs a [[stop,color],...] array to build its gradient.
  // Map each named scale we expose to a representative gradient so the legend
  // matches the scatter. reversescale is applied by the caller when needed.
  const NAMED_COLORSCALES = {
    YlGnBu: [[0,'#ffffd9'],[0.25,'#c7e9b4'],[0.5,'#41b6c4'],[0.75,'#225ea8'],[1,'#081d58']],
    YlOrRd: [[0,'#ffffcc'],[0.25,'#fed976'],[0.5,'#fd8d3c'],[0.75,'#e31a1c'],[1,'#800026']],
    Blues:  [[0,'#f7fbff'],[0.25,'#c6dbef'],[0.5,'#6baed6'],[0.75,'#2171b5'],[1,'#08306b']],
    Greens: [[0,'#f7fcf5'],[0.25,'#c7e9c0'],[0.5,'#74c476'],[0.75,'#238b45'],[1,'#00441b']],
    Reds:   [[0,'#fff5f0'],[0.25,'#fcbba1'],[0.5,'#fb6a4a'],[0.75,'#cb181d'],[1,'#67000d']],
    RdBu:   [[0,'#67001f'],[0.25,'#f4a582'],[0.5,'#f7f7f7'],[0.75,'#92c5de'],[1,'#053061']],
    Viridis:[[0,'#440154'],[0.25,'#3b528b'],[0.5,'#21918c'],[0.75,'#5ec962'],[1,'#fde725']],
  };

  // Resolve any colorscale spec (array, named string, or undefined) to a
  // [[stop,color],...] array for the legend gradient. `reverse` flips it to
  // match a reversescale marker.
  function resolveColorscaleArray(scale, reverse) {
    let arr;
    if (Array.isArray(scale)) {
      arr = scale;
    } else if (typeof scale === 'string' && NAMED_COLORSCALES[scale]) {
      arr = NAMED_COLORSCALES[scale];
    } else {
      arr = CONTINUOUS_COLORSCALE;
    }
    if (reverse) {
      arr = arr.map((item, i, a) => [item[0], a[a.length - 1 - i][1]]);
    }
    return arr;
  }

  // Hover label styling, sourced from the shared theme (projection_layouts.js)
  // so scatter hovers match the app --chart-* tokens instead of drifting with
  // their own palette. Falls back to the previous inline values if the layout
  // factory has not loaded yet (defensive; it is always prepended in practice).
  const _THEME = (window.cerebroProjectionLayout &&
    window.cerebroProjectionLayout.theme) || {};
  const HOVERLABEL = {
    bgcolor: _THEME.hoverBg || 'rgba(255, 255, 255, 0.95)',
    bordercolor: _THEME.grid || '#ececec',
    font: {
      color: _THEME.title || '#1c1c1e',
      size: 12,
      family: _THEME.font ||
        '"Segoe UI Variable", "Segoe UI", -apple-system, BlinkMacSystemFont, "Helvetica Neue", Arial, sans-serif',
    },
  };

  // --- layout helpers ---------------------------------------------------------
  function baseLayout2D() {
    return JSON.parse(
      JSON.stringify(window.cerebroProjectionLayout.make2D({ uirevision: 'true' }))
    );
  }
  function baseLayout3D() {
    return JSON.parse(
      JSON.stringify(window.cerebroProjectionLayout.make3D({ uirevision: 'true' }))
    );
  }

  function applyContainerSize(layout, plotId, container) {
    if (container && container.width && container.height) {
      layout.width = container.width;
      layout.height = container.height;
    } else {
      const plotContainer = document.getElementById(plotId);
      if (plotContainer && plotContainer.parentElement) {
        layout.width = plotContainer.parentElement.clientWidth;
        layout.height = plotContainer.parentElement.clientHeight;
      }
    }
  }

  function apply2DAxes(layout, data) {
    if (data.reset_axes) {
      layout.xaxis.autorange = true;
      delete layout.xaxis.range;
      layout.yaxis.autorange = true;
      delete layout.yaxis.range;
    } else {
      // Preserve the current view. Only pin an explicit range when the server
      // sent a real [min, max]; an empty/absent range with autorange:false is
      // not a valid viewport (Plotly falls back / resets on redraw), so leave
      // the axis untouched and let the layout's uirevision keep the user's
      // pan/zoom. Trajectory sends empty ranges to mean "keep the current view".
      if (Array.isArray(data.x_range) && data.x_range.length === 2) {
        layout.xaxis.autorange = false;
        layout.xaxis.range = [...data.x_range];
      }
      if (Array.isArray(data.y_range) && data.y_range.length === 2) {
        layout.yaxis.autorange = false;
        layout.yaxis.range = [...data.y_range];
      }
    }
  }

  // Custom modebar buttons: "Zoom to selection" and "Clear selection". These
  // used to be Shiny actionButtons under the plot, but they belong with the
  // other plot tools in the top-right modebar. Each resolves the plot id from
  // the graph div Plotly hands the click handler (gd.id), so one definition
  // works for every tab. With nothing selected both are safe no-ops.
  const zoomToSelectionButton = {
    name: 'Zoom to selection',
    title: 'Zoom to selection',
    // magnifying glass with a "+"
    icon: {
      width: 512,
      height: 512,
      path:
        'M416 208c0 45.9-14.9 88.3-40 122.7L502.6 457.4c12.5 12.5 12.5 32.8 0 ' +
        '45.3s-32.8 12.5-45.3 0L330.7 376c-34.4 25.2-76.8 40-122.7 40C93.1 416 ' +
        '0 322.9 0 208S93.1 0 208 0S416 93.1 416 208zM184 296c0 13.3 10.7 24 24 ' +
        '24s24-10.7 24-24V232h64c13.3 0 24-10.7 24-24s-10.7-24-24-24H232V120c0-13.3-' +
        '10.7-24-24-24s-24 10.7-24 24v64H120c-13.3 0-24 10.7-24 24s10.7 24 24 24h64v64z',
    },
    click: function (gd) {
      if (gd && gd.id) toggleZoom(gd.id);
    },
  };
  const clearSelectionButton = {
    name: 'Clear selection',
    title: 'Clear selection',
    // eraser / X-in-square
    icon: {
      width: 512,
      height: 512,
      path:
        'M290.7 57.4L57.4 290.7c-25 25-25 65.5 0 90.5l80 80c12 12 28.3 18.7 ' +
        '45.3 18.7H288h9.4H512c17.7 0 32-14.3 32-32s-14.3-32-32-32H387.9L518.6 ' +
        '363.3c25-25 25-65.5 0-90.5L381.3 57.4c-25-25-65.5-25-90.5 0zM162.7 ' +
        '416l-80-80L216 202.7 349.3 336 269.3 416H162.7z',
    },
    click: function (gd) {
      if (gd && gd.id) clearSelection(gd.id);
    },
  };

  // A curated modebar: keep the tools that are genuinely useful — box-select +
  // lasso feed the shared plotly_selected selection, stepwise zoom-in (+) /
  // zoom-out (-), pan, reset and PNG download — plus our zoom-to / clear
  // selection — and drop the clutter (the drag-rectangle zoom, autoscale,
  // hover-mode toggles, spikelines). 3D renders ignore the 2D button names and
  // keep their own tools.
  const REACT_CONFIG = {
    displaylogo: false,
    displayModeBar: true,
    modeBarButtonsToAdd: [zoomToSelectionButton, clearSelectionButton],
    modeBarButtonsToRemove: [
      'zoom2d',
      'autoScale2d',
      'hoverClosestCartesian',
      'hoverCompareCartesian',
      'toggleSpikelines',
    ],
  };

  // Optional histology background sync (spatial only). syncProjectionBackground
  // is defined in js_spatial_background.js and only prepended for the spatial
  // tab; guard so the other tabs (which never pass a background) don't error.
  function syncBackground(plotId, meta) {
    if (typeof shinyjs.syncSpatialBackground !== 'function') return;
    // Only the spatial render manages the histology background. Every tab shares
    // the same global shinyjs.syncSpatialBackground, so without this guard a
    // non-spatial render (Overview / Gene expression / Trajectory / IR — which
    // carry no is_spatial flag) would clear the image currently shown on the
    // spatial tab. Spatial itself still clears explicitly below (3D / no image).
    if (!meta || !meta.is_spatial) return;
    if (meta.background_image) {
      shinyjs.syncSpatialBackground(
        meta.background_image,
        meta.background_flip_x,
        meta.background_flip_y,
        meta.background_scale_x,
        meta.background_scale_y,
        meta.background_opacity,
        meta.image_bounds,
        meta.background_offset_x,
        meta.background_offset_y
      );
    } else {
      shinyjs.syncSpatialBackground(null, false, false, 1, 1, 1, null, 0, 0);
    }
  }

  // Plotly.react can leave a stale "detached" modebar in the parent on some
  // re-renders. Strip only those strays; KEEP the live modebar inside the plot
  // (the curated tools from REACT_CONFIG — lasso/select/zoom/pan/reset/download).
  function detachModebar(plotId) {
    const plotContainer = document.getElementById(plotId);
    if (!plotContainer) return;
    const parent = plotContainer.parentElement;
    if (parent) {
      parent.querySelectorAll('.detached-modebar').forEach((el) => el.remove());
    }
  }

  // --- re-render loading feedback ---------------------------------------------
  // A parameter change (colour, point size, "% of cells", …) sends the render
  // over a Shiny round trip; on a large data set the plot can sit frozen for a
  // beat with nothing signalling that work is happening. Dim the plotly div to
  // a low opacity WHILE a render is in flight and restore it once Plotly.react
  // resolves, so a slow render fades slightly instead of looking stuck.
  //
  // Two rules make this safe:
  //  * The dim is a pure CSS OPACITY class on the .js-plotly-plot div, kept
  //    strictly separate from the first-paint gate (which uses `visibility`);
  //    the two properties never interfere. It is NEVER applied to the gate.
  //  * The dim is ARMED but not applied for 100ms; a render that resolves inside
  //    that window clears the timer and never dims at all, so the common fast
  //    render has zero flash. Only a genuinely slow render actually fades.
  // Deliberately NOT a Plotly transition: scattergl ignores transition on
  // restyle/react, so the only reliable fade is this container-level CSS one.
  const LOADING_CLASS = 'cerebro-plot-loading';
  const LOADING_ARM_DELAY = 100; // ms before a slow render visibly dims
  const loadingTimers = new Map(); // plotId -> timeout handle

  function plotlyDivFor(plotId) {
    const el = document.getElementById(plotId);
    // The htmlwidget marks the graph div itself with .js-plotly-plot; the CSS
    // opacity rule targets that class, so dim the element only once it is one.
    return el && el.classList && el.classList.contains('js-plotly-plot')
      ? el
      : null;
  }

  function beginRenderFeedback(plotId) {
    // Re-entrant: a render firing while a previous dim is armed/applied reuses
    // the same cycle. Clear any pending arm, then arm a fresh one.
    const prev = loadingTimers.get(plotId);
    if (prev) window.clearTimeout(prev);
    const timer = window.setTimeout(function () {
      const div = plotlyDivFor(plotId);
      if (div) div.classList.add(LOADING_CLASS);
      loadingTimers.delete(plotId);
    }, LOADING_ARM_DELAY);
    loadingTimers.set(plotId, timer);
  }

  function endRenderFeedback(plotId) {
    const timer = loadingTimers.get(plotId);
    if (timer) {
      window.clearTimeout(timer);
      loadingTimers.delete(plotId);
    }
    const div = plotlyDivFor(plotId);
    if (div) div.classList.remove(LOADING_CLASS);
  }

  // Our own plotly_selected handlers, one per plot id, so we can detach ONLY
  // ours on re-render without touching plotly-binding's handler on the same div.
  const selectionHandlers = new Map(); // plotId -> function

  // Attach the plotly_selected listener. This runs in EVERY render's .then(),
  // because Plotly.react rebuilds the graph div's event emitter and drops
  // listeners bound to the previous one — a one-time DOM-flag guard would keep
  // the flag set while the actual listener was gone, so selection would work on
  // the first render and silently stop after any re-colour/re-size. So we
  // detach our prior handler (if the emitter still holds it) and re-bind fresh
  // each time. We deliberately do NOT clear on plotly_deselect (Plotly fires
  // deselect during every re-render); selection is cleared only by an empty
  // selection event or the Clear action.
  function setupSelection(plotId) {
    const plotContainer = document.getElementById(plotId);
    if (!plotContainer || typeof plotContainer.on !== 'function') return;
    const prev = selectionHandlers.get(plotId);
    if (prev && typeof plotContainer.removeListener === 'function') {
      plotContainer.removeListener('plotly_selected', prev);
    }
    const handler = function (eventData) {
      if (eventData && eventData.points && eventData.points.length) {
        setSelectionFromEvent(plotId, eventData);
      }
    };
    selectionHandlers.set(plotId, handler);
    plotContainer.on('plotly_selected', handler);
  }

  // --- the four render entry points -------------------------------------------
  // Each takes the SAME (meta, data, hover, group_centers, container, extra)
  // positional shape the R dispatchers already produce. `extra` carries
  // tab-specific optional payloads (group hulls, trajectory shapes,
  // coexpression swatch colours). meta.plot_id selects the target.

  function render2DContinuous(meta, data, hover, group_centers, container, extra) {
    const plotId = meta.plot_id;
    if (!projectionTargetReady(plotId)) return;
    beginRenderFeedback(plotId);
    extra = extra || {};
    const selectedKeys = getSelection(plotId);
    const selectionOutline = harvestSelectionOutline(plotId);
    removeCustomLegend(plotId);
    removeContinuousLegend(plotId);

    const traces = [];
    const isCoexpr = meta.color_type === 'coexpression';
    if (isCoexpr) {
      traces.push({
        x: data.x,
        y: data.y,
        mode: 'markers',
        type: 'scattergl',
        marker: {
          size: data.point_size,
          opacity: data.point_opacity,
          line: data.point_line,
          color: data.color,
        },
        hoverinfo: hover.hoverinfo,
        hoverlabel: HOVERLABEL,
      });
      applySelection(traces, selectedKeys);
      createCustomLegend(plotId, meta.traces, extra.coexpr_colors || meta.coexpr_colors);
    } else {
      const { min: colorMin, max: colorMax } = finiteExtent(data.color);
      // gene_expression passes an explicit colour range + scale; honour it.
      const cmin = data.color_range ? data.color_range[0] : colorMin;
      const cmax = data.color_range ? data.color_range[1] : colorMax;
      const colorscale = data.colorscale || CONTINUOUS_COLORSCALE;
      const marker = {
        size: data.point_size,
        opacity: data.point_opacity,
        line: data.point_line,
        color: data.color,
        cmin: cmin,
        cmax: cmax,
        colorscale: colorscale,
        showscale: false,
      };
      if (data.reversescale) marker.reversescale = true;
      traces.push({
        x: data.x,
        y: data.y,
        mode: 'markers',
        type: 'scattergl',
        marker: marker,
        hoverinfo: hover.hoverinfo,
        text: hover.text,
        hoverlabel: HOVERLABEL,
      });
      applySelection(traces, selectedKeys);
      createContinuousLegend(
        plotId,
        meta.color_variable,
        cmin,
        cmax,
        resolveColorscaleArray(colorscale, data.reversescale)
      );
    }

    const layout = baseLayout2D();
    if (selectionOutline) layout.selections = selectionOutline;
    if (extra.shapes) layout.shapes = extra.shapes;
    apply2DAxes(layout, data);
    applyContainerSize(layout, plotId, container);

    Plotly.react(plotId, traces, layout, REACT_CONFIG)
      .then(() => {
        endRenderFeedback(plotId);
        setupSelection(plotId);
        syncBackground(plotId, meta);
        detachModebar(plotId);
        scheduleProjectionResize(plotId);
      })
      // Never leave the plot stuck dimmed if a render rejects: clear the
      // loading state even on failure (the .then() above would be skipped).
      .catch(() => endRenderFeedback(plotId));
  }

  function render3DContinuous(meta, data, hover, group_centers, container, extra) {
    const plotId = meta.plot_id;
    if (!projectionTargetReady(plotId)) return;
    beginRenderFeedback(plotId);
    extra = extra || {};
    const selectedKeys = getSelection(plotId);
    removeCustomLegend(plotId);
    removeContinuousLegend(plotId);

    const { min: colorMin, max: colorMax } = finiteExtent(data.color);
    const cmin = data.color_range ? data.color_range[0] : colorMin;
    const cmax = data.color_range ? data.color_range[1] : colorMax;
    const colorscale = data.colorscale || CONTINUOUS_COLORSCALE;
    const marker = {
      size: data.point_size,
      opacity: data.point_opacity,
      line: data.point_line,
      color: data.color,
      cmin: cmin,
      cmax: cmax,
      colorscale: colorscale,
      showscale: false,
    };
    if (data.reversescale) marker.reversescale = true;
    const traces = [{
      x: data.x,
      y: data.y,
      z: data.z,
      mode: 'markers',
      type: 'scatter3d',
      marker: marker,
      hoverinfo: hover.hoverinfo,
      text: hover.text,
      hoverlabel: HOVERLABEL,
      showlegend: false,
    }];
    applySelection(traces, selectedKeys);
    createContinuousLegend(
      plotId,
      meta.color_variable,
      cmin,
      cmax,
      resolveColorscaleArray(colorscale, data.reversescale)
    );

    const layout = baseLayout3D();
    applyContainerSize(layout, plotId, container);
    Plotly.react(plotId, traces, layout, REACT_CONFIG)
      .then(() => {
        endRenderFeedback(plotId);
        setupSelection(plotId);
        syncBackground(plotId, meta);
        detachModebar(plotId);
        scheduleProjectionResize(plotId);
      })
      // Never leave the plot stuck dimmed if a render rejects: clear the
      // loading state even on failure (the .then() above would be skipped).
      .catch(() => endRenderFeedback(plotId));
  }

  // The target plotly div must already be in the DOM before Plotly.react runs.
  // Every tab creates it via an empty bootstrap renderPlotly, but a tab whose
  // host lives behind a renderUI branch (immune_repertoire's Clonal UMAP is only
  // emitted when non-faceted) can fire an update before/without the div. Bail
  // quietly in that case instead of throwing; the next update redraws once the
  // div exists. Harmless for the always-present hosts of the other tabs.
  function projectionTargetReady(plotId) {
    return !!(plotId && document.getElementById(plotId));
  }

  function render2DCategorical(meta, data, hover, group_centers, container, extra) {
    const plotId = meta.plot_id;
    if (!projectionTargetReady(plotId)) return;
    beginRenderFeedback(plotId);
    extra = extra || {};
    const selectedKeys = getSelection(plotId);
    const selectionOutline = harvestSelectionOutline(plotId);
    removeContinuousLegend(plotId);
    // A group hidden via the legend must survive trace rebuilds (size/opacity/
    // colour changes) AND be dropped when the colouring variable itself changes,
    // so client trace visibility and the <plot>_hidden_groups sent to Shiny stay
    // in agreement. Reconcile against the current variable, then reapply below.
    syncHiddenGroupsForVariable(plotId, meta.color_variable);

    // Legend mode. Existing tabs (spatial/overview/gene_expr/trajectory) never
    // set meta.legend_position, so they keep the custom top-bar legend. A tab
    // that DOES set it (immune_repertoire, whose users pick a position) gets
    // plotly's native legend for right/bottom/left, and 'none' hides both — the
    // custom bar only covers the 'top' choice. Native-legend traces set
    // showlegend so plotly draws them; the custom bar keeps showlegend false.
    const legendPosition = meta.legend_position || 'custom';
    const useCustomLegend = legendPosition === 'custom' || legendPosition === 'top';
    const useNativeLegend =
      legendPosition === 'right' ||
      legendPosition === 'bottom' ||
      legendPosition === 'left';
    if (useCustomLegend) {
      createCustomLegend(plotId, meta.traces, data.color);
    } else {
      removeCustomLegend(plotId);
    }

    // hover.hoverinfo may be a single value (all traces share it — the existing
    // tabs) or a per-trace array (immune_repertoire: the grey "Other cells"
    // background skips hover, the coloured levels show it). Resolve per trace.
    const perTraceHoverinfo = Array.isArray(hover.hoverinfo);

    const traces = data.x.map((xVal, i) => ({
      x: xVal,
      y: data.y[i],
      name: meta.traces[i],
      // Reapply a legend-hidden group across this rebuild so the trace stays
      // 'legendonly' instead of springing back to visible while Shiny still
      // treats it as hidden.
      visible: isGroupHidden(plotId, meta.traces[i]) ? 'legendonly' : true,
      mode: 'markers',
      type: 'scattergl',
      marker: {
        size: data.point_size,
        opacity: data.point_opacity,
        line: data.point_line,
        color: data.color[i],
      },
      hoverinfo: perTraceHoverinfo ? hover.hoverinfo[i] : hover.hoverinfo,
      text: hover.text[i],
      hoverlabel: HOVERLABEL,
      showlegend: useNativeLegend,
    }));

    // Optional per-group convex-hull outlines (spatial). Drawn under labels.
    const hulls = extra.group_hulls;
    if (hulls && hulls.group && hulls.group.length >= 1) {
      hulls.group.forEach((g, i) => {
        traces.push({
          x: hulls.x[i],
          y: hulls.y[i],
          type: 'scatter',
          mode: 'lines',
          name: g + ' region',
          line: { color: hulls.color[i], width: 2 },
          fill: 'toself',
          fillcolor: 'rgba(0,0,0,0)',
          opacity: 0.7,
          hoverinfo: 'skip',
          inherit: false,
          showlegend: false,
        });
      });
    }

    if (group_centers && group_centers.group && group_centers.group.length >= 1) {
      traces.push({
        x: group_centers.x,
        y: group_centers.y,
        text: group_centers.group,
        type: 'scatter',
        mode: 'text',
        name: 'Labels',
        textposition: 'middle center',
        textfont: { color: '#000000', size: 16 },
        hoverinfo: 'skip',
        inherit: false,
        showlegend: false,
      });
    }

    applySelection(traces, selectedKeys);

    const layout = baseLayout2D();
    if (selectionOutline) layout.selections = selectionOutline;
    if (extra.shapes) layout.shapes = extra.shapes;
    apply2DAxes(layout, data);
    applyContainerSize(layout, plotId, container);
    applyNativeLegend(layout, legendPosition, meta.legend_font_size);

    Plotly.react(plotId, traces, layout, REACT_CONFIG)
      .then(() => {
        endRenderFeedback(plotId);
        setupSelection(plotId);
        syncBackground(plotId, meta);
        detachModebar(plotId);
        scheduleProjectionResize(plotId);
      })
      // Never leave the plot stuck dimmed if a render rejects: clear the
      // loading state even on failure (the .then() above would be skipped).
      .catch(() => endRenderFeedback(plotId));
  }

  // Position plotly's native legend for the categorical tabs that opt in via
  // meta.legend_position (immune_repertoire). The base layout is tuned for the
  // custom top bar (no native legend), so the default 'custom'/'top'/'none'
  // paths leave showlegend off and let the custom bar (or nothing) handle it.
  function applyNativeLegend(layout, legendPosition, fontSize) {
    const font = { size: fontSize > 0 ? fontSize : 12 };
    if (legendPosition === 'right') {
      layout.showlegend = true;
      layout.legend = { itemsizing: 'constant', font: font };
    } else if (legendPosition === 'left') {
      layout.showlegend = true;
      layout.legend = { itemsizing: 'constant', font: font, x: -0.2 };
    } else if (legendPosition === 'bottom') {
      layout.showlegend = true;
      layout.legend = {
        itemsizing: 'constant',
        font: font,
        orientation: 'h',
        x: 0,
        y: -0.15,
      };
    } else {
      // 'custom' / 'top' (custom bar handles it) or 'none' (hidden): no native
      // legend. Explicitly off so a re-render from a side position clears it.
      layout.showlegend = false;
    }
  }

  function render3DCategorical(meta, data, hover, group_centers, container, extra) {
    const plotId = meta.plot_id;
    if (!projectionTargetReady(plotId)) return;
    beginRenderFeedback(plotId);
    extra = extra || {};
    const selectedKeys = getSelection(plotId);
    removeContinuousLegend(plotId);
    createCustomLegend(plotId, meta.traces, data.color);

    const traces = data.x.map((xVal, i) => ({
      x: xVal,
      y: data.y[i],
      z: data.z[i],
      name: meta.traces[i],
      mode: 'markers',
      type: 'scatter3d',
      marker: {
        size: data.point_size,
        opacity: data.point_opacity,
        line: data.point_line,
        color: data.color[i],
      },
      hoverinfo: hover.hoverinfo,
      text: hover.text[i],
      hoverlabel: HOVERLABEL,
      showlegend: false,
    }));

    if (group_centers && group_centers.group && group_centers.group.length >= 1) {
      traces.push({
        x: group_centers.x,
        y: group_centers.y,
        z: group_centers.z,
        text: group_centers.group,
        type: 'scatter3d',
        mode: 'text',
        name: 'Labels',
        textposition: 'middle center',
        textfont: { color: '#000000', size: 16 },
        hoverinfo: 'skip',
        inherit: false,
        showlegend: false,
      });
    }

    applySelection(traces, selectedKeys);

    const layout = baseLayout3D();
    applyContainerSize(layout, plotId, container);
    Plotly.react(plotId, traces, layout, REACT_CONFIG)
      .then(() => {
        endRenderFeedback(plotId);
        setupSelection(plotId);
        syncBackground(plotId, meta);
        detachModebar(plotId);
        scheduleProjectionResize(plotId);
      })
      // Never leave the plot stuck dimmed if a render rejects: clear the
      // loading state even on failure (the .then() above would be skipped).
      .catch(() => endRenderFeedback(plotId));
  }

  // Clear the selection for one plot (button / Esc / Delete). Also drop any
  // zoom-into-selection state: with no selection there is nothing to stay zoomed
  // on, so return to the full autorange view, unlock, and reset the button.
  function clearSelection(plotId) {
    selectionByPlot.delete(plotId);
    syncSelectionToShiny(plotId);
    const wasZoomed = zoomedPlots.delete(plotId);
    zoomSavedSelections.delete(plotId);
    if (wasZoomed) reportZoomState(plotId, false);
    const plotContainer = document.getElementById(plotId);
    if (plotContainer && plotContainer.data) {
      const relayout = { selections: [], dragmode: 'select' };
      if (wasZoomed) {
        relayout['xaxis.autorange'] = true;
        relayout['yaxis.autorange'] = true;
        relayout.shapes = shapesWithoutZoomMarker(plotContainer);
      }
      Plotly.update(plotId, { selectedpoints: null }, relayout).then(function () {
        plotContainer.emit('plotly_deselect');
      });
    }
  }

  function getContainerDimensions(plotId) {
    const plotContainer = document.getElementById(plotId);
    if (plotContainer && plotContainer.parentElement) {
      return {
        width: plotContainer.parentElement.clientWidth,
        height: plotContainer.parentElement.clientHeight,
      };
    }
    return { width: 0, height: 0 };
  }

  // Global Delete/Backspace/Escape -> clear the visible projection's selection.
  // Any registered plot whose Clear button is currently visible is cleared.
  const registeredPlots = new Set();
  let keyHandlerBound = false;
  function bindKeyHandler() {
    if (keyHandlerBound) return;
    keyHandlerBound = true;
    document.addEventListener('keydown', function (e) {
      if (e.key !== 'Delete' && e.key !== 'Backspace' && e.key !== 'Escape') return;
      const tag = (e.target.tagName || '').toLowerCase();
      if (
        tag === 'input' || tag === 'textarea' || tag === 'select' ||
        e.target.isContentEditable
      ) {
        return;
      }
      registeredPlots.forEach((plotId) => {
        const btn = document.getElementById(plotId + '_clear_selection');
        if (btn && btn.offsetParent !== null) {
          e.preventDefault();
          btn.click();
        }
      });
    });
  }

  // Public API. Each tab's thin shinyjs wrappers delegate here; nothing else
  // needs to know about plot ids beyond passing meta.plot_id.
  window.cerebroProjection = {
    __ready: true,
    render2DContinuous: render2DContinuous,
    render3DContinuous: render3DContinuous,
    render2DCategorical: render2DCategorical,
    render3DCategorical: render3DCategorical,
    clearSelection: clearSelection,
    zoomToSelection: toggleZoom,
    // Reattach the shared plotly_selected handler. gene_expression multi-panel
    // renders directly via Plotly.react (not the shared 2D path), so it must
    // call this after each react to restore box/lasso selection.
    setupSelection: setupSelection,
    getContainerDimensions: getContainerDimensions,
    // Hide both legend variants for a plot. Used by tab-specific render modes
    // that draw their own legend (gene_expression multi-panel uses a native
    // plotly colorbar), so the shared custom legend bar doesn't linger above.
    hideLegend: function (plotId) {
      removeCustomLegend(plotId);
      removeContinuousLegend(plotId);
    },
    registerPlot: function (plotId) {
      registeredPlots.add(plotId);
      bindKeyHandler();
      scheduleProjectionResize(plotId);
    },
    _finiteExtent: finiteExtent,
    _projectionTargetHeight: viewport._targetHeight,
    _projectionSizingElement: projectionSizingElement,
    _viewportAdapter: projectionViewportAdapter,
    _revealProjectionHost: revealProjectionHost,
    _shouldRevealProjection: shouldRevealProjection,
    _projectionGateClass: PROJECTION_GATE_CLASS,
    _projectionSizedClass: PROJECTION_SIZED_CLASS,
    _computeEqualAspectRange: computeEqualAspectRange,
    _nextZoomAction: nextZoomAction,
    _zoomMarkerShape: zoomMarkerShape,
    _shapesWithoutZoomMarker: shapesWithoutZoomMarker,
  };

  bindKeyHandler();
})();
