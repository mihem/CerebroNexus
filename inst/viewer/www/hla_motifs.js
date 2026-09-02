/* HLA motif-network selection, navigation, export, and saved-view state. */
(function () {
  'use strict';

  var TR = 'matrix(1 0 0 -1 0 850)';
  var ICONS = {
    box: { vb: '0 0 875 1000', d: 'M125 125h625v625H125zM210 210v455h455V210z' },
    lasso: { vb: '0 0 875 1000', d: 'M120 235l215-120 300 70 120 250-145 290-330 45-170-220zM205 270l-55 260 145 185 280-38 125-245-92-190-260-61z' },
    pan: { vb: '0 0 875 1000', d: 'M375 70h125v210l80-80 88 88-230 230-230-230 88-88 79 80zM375 780V570l-79 80-88-88 230-230 230 230-88 88-80-80v210z' },
    zoomin: { vb: '0 0 875 1000', d: 'm1 787l0-875 875 0 0 875-875 0z m687-500l-187 0 0-187-125 0 0 187-188 0 0 125 188 0 0 187 125 0 0-187 187 0 0-125z' },
    zoomout: { vb: '0 0 875 1000', d: 'm0 788l0-876 875 0 0 876-875 0z m688-500l-500 0 0 125 500 0 0-125z' },
    reset: { vb: '0 0 928.6 1000', d: 'm786 296v-267q0-15-11-26t-25-10h-214v214h-143v-214h-214q-15 0-25 10t-11 26v267l321 264 321-264z m124 39l-34-41q-12-12-26-3l-386 322-386-322q-13-8-25 3l-35 41q-9 13 3 25l401 334q42 30 85 0l401-334q12-12 2-25z' },
    download: { vb: '0 0 1000 1000', d: 'm500 450c-83 0-150-67-150-150s67-150 150-150 150 67 150 150-67 150-150 150z m400 150h-120q-29 0-39 29l-31 93q-9 28-40 28h-340q-31 0-39-28l-31-94q-10-28-40-28h-120q-100 0-100-100v-450q0-100 100-100h800q100 0 100 100v450q0 100-100 100z' }
  };
  var BUTTONS = [
    { act: 'box', title: 'Box select', icon: ICONS.box },
    { act: 'lasso', title: 'Lasso select', icon: ICONS.lasso },
    { act: 'pan', title: 'Pan', icon: ICONS.pan },
    { act: 'zoomin', title: 'Zoom in', icon: ICONS.zoomin },
    { act: 'zoomout', title: 'Zoom out', icon: ICONS.zoomout },
    { act: 'reset', title: 'Reset view', icon: ICONS.reset },
    { act: 'download', title: 'Download plot as a png', icon: ICONS.download }
  ];
  var mode = 'lasso';
  var overlay = null;
  var overlayNet = null;
  var drawNet = null;
  var drag = null;
  var committedCanvas = null;
  var selectedKeys = [];
  var selectedCells = [];
  var pendingState = null;
  var syncingSelection = false;
  var shinyBound = false;

  function svgIcon(icon) {
    return '<svg viewBox="' + icon.vb + '" width="15" height="15">' +
      '<path d="' + icon.d + '" transform="' + TR +
      '" fill="currentColor"></path></svg>';
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
  function sendSelectedKeys(keys) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('hla_motif_selected_keys', keys, { priority: 'event' });
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
    window.hlaShowNodeDetails(chosen.length ? chosen[0] : null);
    sendSelectedKeys(selectedKeys);
  }
  function pointerDown(event) {
    if (event.button !== 0) return;
    event.preventDefault();
    var start = point(event);
    drag = { pointer: event.pointerId, points: [start] };
    overlay.setPointerCapture(event.pointerId);
    drawOverlay();
  }
  function pointerMove(event) {
    if (!drag || drag.pointer !== event.pointerId) return;
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
      window.hlaShowNodeDetails(clicked);
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
    frame.style.position = 'relative';
    frame.appendChild(overlay);
    overlay.addEventListener('pointerdown', pointerDown);
    overlay.addEventListener('pointermove', pointerMove);
    overlay.addEventListener('pointerup', pointerUp);
    overlay.addEventListener('pointercancel', pointerUp);
    setMode(mode);
    resizeOverlay();
  }
  function syncModeButtons() {
    document.querySelectorAll('#hla-modebar [data-act]').forEach(function (button) {
      button.classList.toggle('is-on', button.dataset.act === mode);
    });
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
    network.moveTo({
      scale: Math.max(floor, Math.min(6, network.getScale() * factor)),
      animation: { duration: 200 }
    });
  }
  function resetView() {
    var network = net();
    if (network) network.fit({ animation: { duration: 300 } });
  }
  function zoomSelection() {
    var network = net();
    var ids = idsFromNodeKeys(network, selectedKeys);
    if (!network || !ids.length) return;
    network.fit({ nodes: ids, animation: { duration: 300 } });
  }
  function clearSelection(notify) {
    var network = net();
    committedCanvas = null;
    selectedKeys = [];
    selectedCells = [];
    if (network) network.unselectAll();
    window.hlaShowNodeDetails(null);
    drawOverlay();
    if (notify) sendSelectedKeys([]);
  }
  function downloadPNG() {
    var network = net();
    if (!network || !network.canvas || !network.canvas.frame) return;
    var link = document.createElement('a');
    link.href = network.canvas.frame.canvas.toDataURL('image/png');
    link.download = 'hla_motif_network.png';
    link.click();
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
    if (!network) return null;
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
    var saved = pendingState;
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
      network.moveTo({
        position: {
          x: (viewport.x0 + viewport.x1) / 2,
          y: (viewport.y0 + viewport.y1) / 2
        },
        scale: Math.max(0.02, Math.min(6, scale)),
        animation: false
      });
    }
    drawOverlay();
  }
  function applyState(_id, saved) {
    pendingState = saved;
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('hla_motif_restore_cells', {
        nonce: Date.now().toString(36),
        cells: saved && saved.selection ? saved.selection.cells || [] : []
      }, { priority: 'event' });
    }
    applyPendingState();
    return {
      selectedCells: saved && saved.selection ? saved.selection.cells.length : 0
    };
  }
  function receiveSelection(result) {
    var network = net();
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
    window.dispatchEvent(new CustomEvent('cerebro:specialist-state', {
      detail: { viewId: 'hla_motif_network', selectedCells: selectedCells.length }
    }));
  }
  function connectShiny() {
    if (shinyBound || !window.Shiny || !Shiny.addCustomMessageHandler) return;
    shinyBound = true;
    Shiny.addCustomMessageHandler('hla_motif_selection_state', receiveSelection);
    Shiny.addCustomMessageHandler('hla_motif_selection_command', function (request) {
      if (!request) return;
      if (request.action === 'clear') clearSelection(false);
      if (request.action === 'zoom') zoomSelection();
    });
    Shiny.addCustomMessageHandler('hla-refresh-node-details', function (_message) {
      var network = net();
      var ids = network ? network.getSelectedNodes() : [];
      window.hlaShowNodeDetails(ids.length ? ids[0] : null);
    });
  }
  function build() {
    var bar = document.getElementById('hla-modebar');
    if (!bar || bar._built) return;
    bar._built = true;
    bar.innerHTML = BUTTONS.map(function (button) {
      return '<button type="button" class="hla-mb-btn" data-act="' + button.act +
        '" title="' + button.title + '" aria-label="' + button.title + '">' +
        svgIcon(button.icon) + '</button>';
    }).join('');
    bar.querySelectorAll('.hla-mb-btn').forEach(function (button) {
      button.addEventListener('click', function () {
        var action = button.dataset.act;
        if (['box', 'lasso', 'pan'].indexOf(action) >= 0) setMode(action);
        else if (action === 'zoomin') zoomBy(1.3);
        else if (action === 'zoomout') zoomBy(1 / 1.3);
        else if (action === 'reset') resetView();
        else if (action === 'download') downloadPNG();
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

  window.hlaShowNodeDetails = function (id) {
    var output = document.getElementById('hla-node-details');
    var network = net();
    if (!output || !network || id == null) {
      if (output) { output.innerHTML = ''; output.style.display = 'none'; }
      return;
    }
    var data = nodeData(network);
    var node = data ? data.get(id) : null;
    var detail = node && (node.detail || node.title);
    if (!detail) {
      output.innerHTML = ''; output.style.display = 'none'; return;
    }
    output.innerHTML = detail;
    output.style.display = 'block';
  };
  window.cerebroHlaMotifs = {
    captureState: captureState,
    applyState: applyState,
    downloadPNG: function () {
      if (!net()) return false;
      downloadPNG();
      return true;
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
    }
  };

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
