/* HLA motif-network selection, navigation, export, and saved-view state. */
(function () {
  'use strict';

  var mode = 'lasso';
  var overlay = null;
  var overlayNet = null;
  var nodeTooltip = null;
  var drawNet = null;
  var drag = null;
  var committedCanvas = null;
  var selectedKeys = [];
  var selectedCells = [];
  var selectionFocused = false;
  var pendingState = null;
  var networkContextKey = '';
  var syncingSelection = false;
  var shinyBound = false;
  var associationObserver = null;
  var associationRequested = false;

  function contextApi() {
    return window.CerebroDatasetContext || null;
  }
  function currentContext() {
    var api = contextApi();
    return api ? api.readyContext() : null;
  }
  function currentContextKey() {
    var api = contextApi();
    return api ? api.readyKey() : '';
  }
  function fresh(message) {
    var api = contextApi();
    return !!(api && api.accepts && api.accepts(message));
  }

  function requestAssociations() {
    if (associationRequested || !window.Shiny || !Shiny.setInputValue) return;
    associationRequested = true;
    Shiny.setInputValue('hla_associations_render_request', Date.now(), {
      priority: 'event'
    });
  }

  function observeAssociations() {
    var host = document.getElementById('hla_associations_mount');
    if (!host || host._renderObserved) return;
    host._renderObserved = true;
    if (!window.IntersectionObserver) {
      requestAssociations();
      return;
    }
    associationObserver = new IntersectionObserver(function (entries) {
      if (entries.some(function (entry) { return entry.isIntersecting; })) {
        requestAssociations();
        associationObserver.disconnect();
      }
    });
    associationObserver.observe(host);
  }

  var reducedMotion = window.matchMedia
    ? window.matchMedia('(prefers-reduced-motion: reduce)')
    : { matches: false };

  function withMotion(options, duration) {
    options.animation = duration && !reducedMotion.matches
      ? { duration: duration }
      : false;
    return options;
  }

  function net() {
    if (!window.HTMLWidgets || !window.HTMLWidgets.find) return null;
    var widget = window.HTMLWidgets.find('#hla_plot_motifNetwork');
    return widget && widget.network ? widget.network : null;
  }
  function nodeData(network) {
    var data = network && network.body && network.body.data;
    return data && data.nodes ? data.nodes : null;
  }
  function nodeKeysFromIds(network, ids) {
    var data = nodeData(network);
    if (!data) return [];
    return ids.map(function (id) {
      var node = data.get(id);
      return node && String(node.node_key || '');
    }).filter(Boolean);
  }
  function idsFromNodeKeys(network, keys) {
    var data = nodeData(network);
    if (!data) return [];
    var wanted = new Set((keys || []).map(String));
    return data.get().filter(function (node) {
      return wanted.has(String(node.node_key || ''));
    }).map(function (node) { return node.id; });
  }
  function point(event) {
    var rect = overlay.getBoundingClientRect();
    return [event.clientX - rect.left, event.clientY - rect.top];
  }
  function pointInPolygon(x, y, polygon) {
    if (window.CBGeom && window.CBGeom.inPoly) {
      return window.CBGeom.inPoly(x, y, polygon);
    }
    var inside = false;
    for (var i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
      var xi = polygon[i][0], yi = polygon[i][1];
      var xj = polygon[j][0], yj = polygon[j][1];
      if (((yi > y) !== (yj > y)) &&
          x < (xj - xi) * (y - yi) / (yj - yi) + xi) inside = !inside;
    }
    return inside;
  }
  function boxPolygon(a, b) {
    return [[a[0], a[1]], [b[0], a[1]], [b[0], b[1]], [a[0], b[1]]];
  }
  function resizeOverlay() {
    if (!overlay) return;
    var dpr = window.devicePixelRatio || 1;
    var width = overlay.parentElement.clientWidth;
    var height = overlay.parentElement.clientHeight;
    if (overlay.width !== Math.round(width * dpr) ||
        overlay.height !== Math.round(height * dpr)) {
      overlay.width = Math.round(width * dpr);
      overlay.height = Math.round(height * dpr);
      overlay.style.width = width + 'px';
      overlay.style.height = height + 'px';
    }
    overlay.getContext('2d').setTransform(dpr, 0, 0, dpr, 0, 0);
  }
  function screenOutline(network) {
    if (drag && drag.points.length) {
      return mode === 'box'
        ? boxPolygon(drag.points[0], drag.points[drag.points.length - 1])
        : drag.points;
    }
    if (!committedCanvas) return null;
    return committedCanvas.map(function (value) {
      var p = network.canvasToDOM({ x: value[0], y: value[1] });
      return [p.x, p.y];
    });
  }
  function drawOverlay() {
    var network = net();
    if (!overlay || !network) return;
    resizeOverlay();
    var ctx = overlay.getContext('2d');
    ctx.clearRect(0, 0, overlay.clientWidth, overlay.clientHeight);
    var polygon = screenOutline(network);
    if (!polygon || polygon.length < 2) return;
    ctx.beginPath();
    ctx.moveTo(polygon[0][0], polygon[0][1]);
    polygon.slice(1).forEach(function (p) { ctx.lineTo(p[0], p[1]); });
    ctx.closePath();
    ctx.fillStyle = 'rgba(249, 115, 22, .08)';
    ctx.strokeStyle = '#f97316';
    ctx.lineWidth = 2;
    ctx.setLineDash([6, 4]);
    ctx.fill(); ctx.stroke(); ctx.setLineDash([]);
  }
  function hideNodeTooltip() {
    if (nodeTooltip) nodeTooltip.hidden = true;
  }
  function showNodeTooltip(event) {
    var network = net();
    if (!network || !nodeTooltip) return;
    var p = point(event);
    var id = network.getNodeAt({ x: p[0], y: p[1] });
    var data = nodeData(network);
    var node = data && id != null ? data.get(id) : null;
    var detail = node && node.title;
    if (!detail) { hideNodeTooltip(); return; }
    nodeTooltip.innerHTML = detail;
    nodeTooltip.hidden = false;
    var frame = network.canvas.frame;
    var left = Math.min(p[0] + 12, frame.clientWidth - nodeTooltip.offsetWidth - 8);
    var top = p[1] + 12;
    if (top + nodeTooltip.offsetHeight > frame.clientHeight - 8) {
      top = p[1] - nodeTooltip.offsetHeight - 12;
    }
    nodeTooltip.style.left = Math.max(8, left) + 'px';
    nodeTooltip.style.top = Math.max(8, top) + 'px';
  }
  function sendSelectedKeys(keys) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('hla_motif_selected_keys', {
        node_keys: keys,
        dataset_context: currentContext()
      }, { priority: 'event' });
    }
  }
  function chooseNodes(polygon) {
    var network = net();
    var data = nodeData(network);
    if (!network || !data) return;
    var ids = data.getIds();
    var positions = network.getPositions(ids);
    var chosen = ids.filter(function (id) {
      var position = positions[id];
      if (!position) return false;
      var p = network.canvasToDOM(position);
      return pointInPolygon(p.x, p.y, polygon);
    });
    syncingSelection = true;
    network.selectNodes(chosen, false);
    syncingSelection = false;
    selectedKeys = nodeKeysFromIds(network, chosen);
    sendSelectedKeys(selectedKeys);
  }
  function pointerDown(event) {
    if (event.button !== 0) return;
    event.preventDefault();
    hideNodeTooltip();
    var start = point(event);
    drag = { pointer: event.pointerId, points: [start] };
    overlay.setPointerCapture(event.pointerId);
    drawOverlay();
  }
  function pointerMove(event) {
    if (!drag) { showNodeTooltip(event); return; }
    if (drag.pointer !== event.pointerId) return;
    var next = point(event);
    if (mode === 'box') drag.points = [drag.points[0], next];
    else {
      var last = drag.points[drag.points.length - 1];
      if (Math.hypot(next[0] - last[0], next[1] - last[1]) > 3) {
        drag.points.push(next);
      }
    }
    drawOverlay();
  }
  function pointerUp(event) {
    if (!drag || drag.pointer !== event.pointerId) return;
    var network = net();
    var polygon = mode === 'box'
      ? boxPolygon(drag.points[0], point(event))
      : drag.points.slice();
    drag = null;
    if (polygon.length < 3 ||
        (polygon.length === 4 && Math.hypot(
          polygon[0][0] - polygon[2][0],
          polygon[0][1] - polygon[2][1]
        ) < 5)) {
      var clickPoint = point(event);
      var clicked = network && network.getNodeAt({ x: clickPoint[0], y: clickPoint[1] });
      committedCanvas = null;
      syncingSelection = true;
      if (network) network.selectNodes(clicked == null ? [] : [clicked], false);
      syncingSelection = false;
      selectedKeys = clicked == null ? [] : nodeKeysFromIds(network, [clicked]);
      sendSelectedKeys(selectedKeys);
    } else {
      committedCanvas = polygon.map(function (value) {
        var p = network.DOMtoCanvas({ x: value[0], y: value[1] });
        return [p.x, p.y];
      });
      chooseNodes(polygon);
    }
    drawOverlay();
  }
  function ensureOverlay(network) {
    if (!network || !network.canvas || !network.canvas.frame) return;
    var frame = network.canvas.frame;
    if (overlayNet === network && overlay && document.contains(overlay)) {
      resizeOverlay(); return;
    }
    overlayNet = network;
    overlay = document.createElement('canvas');
    overlay.className = 'hla-selection-overlay';
    nodeTooltip = document.createElement('div');
    nodeTooltip.className = 'hla-node-tooltip';
    nodeTooltip.setAttribute('role', 'tooltip');
    nodeTooltip.hidden = true;
    frame.style.position = 'relative';
    frame.appendChild(overlay);
    frame.appendChild(nodeTooltip);
    overlay.addEventListener('pointerdown', pointerDown);
    overlay.addEventListener('pointermove', pointerMove);
    overlay.addEventListener('pointerup', pointerUp);
    overlay.addEventListener('pointercancel', pointerUp);
    overlay.addEventListener('pointerleave', hideNodeTooltip);
    setMode(mode);
    resizeOverlay();
  }
  function syncModeButtons() {
    document.querySelectorAll('#hla-modebar [data-act]').forEach(function (button) {
      button.classList.toggle(
        'is-on',
        button.dataset.act === mode ||
          (button.dataset.act === 'zsel' && selectionFocused)
      );
      if (['zsel', 'clear'].indexOf(button.dataset.act) >= 0) {
        button.disabled = !selectedKeys.length;
        button.classList.toggle('hla-mb-btn--off', button.disabled);
      }
    });
    var focus = document.getElementById('hla_motif_network_focus_selection');
    if (focus) {
      focus.classList.toggle('is-on', selectionFocused);
      focus.setAttribute('aria-pressed', selectionFocused ? 'true' : 'false');
    }
  }
  function setMode(next) {
    mode = ['box', 'lasso', 'pan'].indexOf(next) >= 0 ? next : 'lasso';
    var network = net();
    if (overlay) overlay.style.pointerEvents = mode === 'pan' ? 'none' : 'auto';
    if (network) network.setOptions({ interaction: { dragView: mode === 'pan' } });
    syncModeButtons();
  }
  function zoomBy(factor) {
    var network = net();
    if (!network) return;
    var floor = typeof network.hlaMinScale === 'number' ? network.hlaMinScale : 0.02;
    network.moveTo(withMotion({
      scale: Math.max(floor, Math.min(6, network.getScale() * factor)),
    }, 200));
  }
  function resetView() {
    var network = net();
    selectionFocused = false;
    syncModeButtons();
    if (network) network.fit(withMotion({}, 300));
  }
  function zoomSelection() {
    var network = net();
    var ids = idsFromNodeKeys(network, selectedKeys);
    if (!network || !ids.length) return;
    if (selectionFocused) {
      resetView();
      return;
    }
    var boxes = ids.map(function (id) { return network.getBoundingBox(id); });
    var left = Math.min.apply(null, boxes.map(function (box) { return box.left; }));
    var right = Math.max.apply(null, boxes.map(function (box) { return box.right; }));
    var top = Math.min.apply(null, boxes.map(function (box) { return box.top; }));
    var bottom = Math.max.apply(null, boxes.map(function (box) { return box.bottom; }));
    var frame = network.canvas.frame;
    var scale = Math.min(
      Math.max(1, frame.clientWidth - 80) / Math.max(1, right - left),
      Math.max(1, frame.clientHeight - 80) / Math.max(1, bottom - top)
    );
    scale = Math.max(network.hlaMinScale || 0.02, scale);
    selectionFocused = true;
    syncModeButtons();
    network.moveTo(withMotion({
      position: { x: (left + right) / 2, y: (top + bottom) / 2 },
      scale: Math.min(3, scale),
    }, 300));
  }
  function clearSelection(notify) {
    var network = net();
    if (selectionFocused) resetView();
    committedCanvas = null;
    selectedKeys = [];
    selectedCells = [];
    selectionFocused = false;
    if (network) network.unselectAll();
    syncModeButtons();
    drawOverlay();
    if (notify) sendSelectedKeys([]);
  }
  function requestClearSelection() {
    var button = document.getElementById('hla_motif_network_clear_selection');
    if (button) button.click();
    else clearSelection(true);
  }
  function downloadPNG() {
    var network = net();
    if (!network || !network.canvas || !network.canvas.frame) return false;
    try {
      var link = document.createElement('a');
      link.href = network.canvas.frame.canvas.toDataURL('image/png');
      link.download = 'hla_motif_network.png';
      link.click();
      return true;
    } catch (ignore) {
      return false;
    }
  }
  function bounds(network) {
    var frame = network.canvas.frame;
    var a = network.DOMtoCanvas({ x: 0, y: 0 });
    var b = network.DOMtoCanvas({ x: frame.clientWidth, y: frame.clientHeight });
    return {
      x0: Math.min(a.x, b.x), x1: Math.max(a.x, b.x),
      y0: Math.min(a.y, b.y), y1: Math.max(a.y, b.y)
    };
  }
  function captureState() {
    var network = net();
    if (!network || !networkContextKey || networkContextKey !== currentContextKey()) {
      return null;
    }
    return {
      cells: selectedCells.slice(),
      geometry: committedCanvas && committedCanvas.length > 2 ? {
        mode: mode === 'box' ? 'box' : 'lasso', panel: 0,
        polygon: committedCanvas.map(function (p) { return [p[0], p[1]]; })
      } : null,
      view: {
        viewport: bounds(network),
        rotation: { x: 0, y: 0 },
        mode: mode,
        zoomed: network.getScale() > (network.hlaMinScale || 0) + 1e-6,
        hidden_groups: []
      }
    };
  }
  function applyPendingState() {
    var network = net();
    if (!network || !pendingState) return;
    if (!pendingState.contextKey || pendingState.contextKey !== currentContextKey()) {
      pendingState = null;
      return;
    }
    if (networkContextKey !== currentContextKey()) return;
    var saved = pendingState.saved;
    pendingState = null;
    var view = saved.view || {};
    var viewport = view.viewport;
    setMode(view.mode || 'lasso');
    committedCanvas = saved.selection && saved.selection.geometry
      ? saved.selection.geometry.polygon.map(function (p) {
        return [Number(p[0]), Number(p[1])];
      })
      : null;
    if (viewport && viewport.x1 > viewport.x0 && viewport.y1 > viewport.y0) {
      var frame = network.canvas.frame;
      var scale = Math.min(
        frame.clientWidth / (viewport.x1 - viewport.x0),
        frame.clientHeight / (viewport.y1 - viewport.y0)
      );
      network.moveTo(withMotion({
        position: {
          x: (viewport.x0 + viewport.x1) / 2,
          y: (viewport.y0 + viewport.y1) / 2
        },
        scale: Math.max(0.02, Math.min(6, scale)),
      }, 0));
    }
    drawOverlay();
  }
  function applyState(_id, saved) {
    var datasetContext = currentContext();
    var contextKey = currentContextKey();
    if (!datasetContext || !contextKey) return { selectedCells: 0 };
    pendingState = { saved: saved, contextKey: contextKey };
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('hla_motif_restore_cells', {
        nonce: Date.now().toString(36),
        cells: saved && saved.selection ? saved.selection.cells || [] : [],
        dataset_context: datasetContext
      }, { priority: 'event' });
    }
    applyPendingState();
    return {
      selectedCells: saved && saved.selection ? saved.selection.cells.length : 0
    };
  }
  function reportState() {
    var fingerprint = (window.cerebroSavedViewDataset || {}).cell_fingerprint || '';
    if (!fingerprint || !currentContextKey() || networkContextKey !== currentContextKey()) return;
    window.dispatchEvent(new CustomEvent('cerebro:specialist-state', {
      detail: {
        viewId: 'hla_motif_network',
        dataset_context: contextApi().current(),
        selectedCells: selectedCells.length,
        datasetFingerprint: fingerprint
      }
    }));
  }
  function receiveSelection(result) {
    if (!fresh(result)) return;
    var network = net();
    networkContextKey = currentContextKey();
    selectionFocused = false;
    selectedKeys = result && Array.isArray(result.node_keys)
      ? result.node_keys.map(String) : [];
    selectedCells = result && Array.isArray(result.cells)
      ? result.cells.map(String) : [];
    var guide = document.getElementById('hla_motif_network_selection_guide');
    var active = document.getElementById('hla_motif_network_selection_active');
    if (guide) guide.classList.toggle('cerebro-selection-status-hidden', !!selectedCells.length);
    if (active) active.classList.toggle('cerebro-selection-status-hidden', !selectedCells.length);
    if (network) {
      syncingSelection = true;
      network.selectNodes(idsFromNodeKeys(network, selectedKeys), false);
      syncingSelection = false;
    }
    syncModeButtons();
    applyPendingState();
    reportState();
  }
  function connectShiny() {
    if (shinyBound || !window.Shiny || !Shiny.addCustomMessageHandler) return;
    shinyBound = true;
    observeAssociations();
    Shiny.addCustomMessageHandler('hla_motif_selection_state', receiveSelection);
    Shiny.addCustomMessageHandler('hla_motif_selection_command', function (request) {
      if (!request || !fresh(request)) return;
      if (request.action === 'clear') clearSelection(false);
      else if (request.action === 'focus') zoomSelection();
    });
  }
  function build() {
    var bar = document.getElementById('hla-modebar');
    if (!bar || bar._built) return;
    bar._built = true;
    bar.querySelectorAll('.hla-mb-btn').forEach(function (button) {
      button.addEventListener('click', function () {
        var action = button.dataset.act;
        if (['box', 'lasso', 'pan'].indexOf(action) >= 0) setMode(action);
        else if (action === 'zoomin') zoomBy(1.3);
        else if (action === 'zoomout') zoomBy(1 / 1.3);
        else if (action === 'zsel') zoomSelection();
        else if (action === 'reset') resetView();
        else if (action === 'clear') requestClearSelection();
        else if (action === 'download') {
          window.dispatchEvent(new CustomEvent('cerebro:png-result', {
            detail: { ok: downloadPNG() }
          }));
        }
      });
    });
    syncModeButtons();
  }
  function tryBuild(attempts) {
    build(); connectShiny();
    if ((!document.getElementById('hla-modebar') || !net()) && attempts > 0) {
      window.setTimeout(function () { tryBuild(attempts - 1); }, 500);
    }
  }

  window.cerebroHlaMotifs = {
    captureState: captureState,
    applyState: applyState,
    downloadPNG: function () {
      return downloadPNG();
    },
    handleNativeSelection: function (ids) {
      if (syncingSelection) return;
      committedCanvas = null;
      selectedKeys = nodeKeysFromIds(net(), ids || []);
      sendSelectedKeys(selectedKeys);
      drawOverlay();
    },
    onDraw: function () {
      var network = net();
      var changed = drawNet !== network;
      drawNet = network;
      ensureOverlay(network);
      if (changed && network && selectedKeys.length) {
        syncingSelection = true;
        network.selectNodes(idsFromNodeKeys(network, selectedKeys), false);
        syncingSelection = false;
      }
      applyPendingState();
      drawOverlay();
      if (changed && network) reportState();
    }
  };

  window.addEventListener('cerebro:dataset-context', function (event) {
    var detail = event && event.detail || {};
    if (!detail.changed && detail.phase === 'ready') return;
    pendingState = null;
    networkContextKey = '';
    // The old viewport is being retired; do not animate it during a switch.
    selectionFocused = false;
    clearSelection(false);
    hideNodeTooltip();
    if (detail.phase !== 'pending') return;
    if (detail.changed || detail.selectionPending) {
      // The upload host stays mounted across datasets. Retire its old file.
      var upload = document.getElementById('hla_upload');
      if (upload) {
        var jq = window.jQuery;
        var uploader = jq && jq(upload).data('currentUploader');
        if (uploader && typeof uploader.abort === 'function') uploader.abort();
        if (jq) jq(upload).removeData('currentUploader').removeAttr('data-restore');
        upload.value = '';
        var group = upload.closest('.form-group');
        var filename = group && group.querySelector('input[type="text"]');
        var progress = group && group.querySelector('.progress');
        if (filename) filename.value = '';
        if (progress) progress.style.visibility = 'hidden';
        if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('hla_upload', null);
      }
      var provenance = document.getElementById('hla_upload_source_type');
      if (provenance && provenance.selectize) provenance.selectize.setValue('unknown');
    }
    ['hla_motif_network_composition', 'hla_selected_count'].forEach(function (id) {
      var root = document.getElementById(id);
      if (!root) return;
      if (window.Shiny && Shiny.unbindAll) Shiny.unbindAll(root);
      root.replaceChildren();
    });
  });

  if (window.jQuery) {
    window.jQuery(document).on('shiny:connected', function () { tryBuild(30); });
  } else {
    document.addEventListener('shiny:connected', function () {
      tryBuild(30);
    }, { once: true });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function () { tryBuild(30); });
  } else {
    tryBuild(30);
  }
})();
