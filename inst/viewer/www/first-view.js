/* A preview of the default Projection, drawn while the session starts.

   A `?page=projection` link used to show its plot only after the session had
   loaded the data set and exchanged the page's controls: 1.5 to 2.7 s, of which
   fetching and drawing the cells is a fraction. The server publishes what the
   first frame shows (viewer/core/shared_resources.R, `cerebro-first-view/`):
   the Viewer Pack files with the coordinates and the group codes, the group
   colours and the point size. This script reads that description as soon as
   the document exists, fetches the two files, opens the Projection page and
   draws the cells where the plot will be. The plot itself replaces the preview
   when it has painted its first frame.

   The preview is a picture: it does not zoom, select or answer the pointer.
   Without a description (a first visit, a login, a data set without a Viewer
   Pack) nothing here runs and the page opens as before. */
(function () {
  'use strict';

  var VIEW_ID = 'overview_projection';
  var TAB = 'overview';
  // project() in cell_views.js keeps this margin around the cells.
  var PAD = 10;
  // Where the plot's canvas ends up inside its host, and the room it leaves
  // below itself in the window. The plot is laid out by code that measures
  // its own legend and header, and the preview moves onto the real canvas
  // when that exists. `top` is only the guess for a description without the
  // names of the groups: with them the legend is laid out here as the plot
  // will lay it out, and the canvas begins PLOT_HEAD below it (the margin
  // and padding of the pane, and its title).
  var INSET = { left: 9, top: 83, right: 9, below: 32 };
  var PLOT_HEAD = 40;
  // The row of the page's selectors above the plot, which the session fills.
  var SELECTORS_HEIGHT = 60;
  var MIN_HEIGHT = 240;
  var GIVE_UP_MS = 30000;
  var MISSING = '#e6e7ea';

  var params;
  try {
    params = new URLSearchParams(window.location.search);
  } catch (error) {
    return;
  }
  if (params.get('page') !== 'projection' || !window.fetch ||
      !window.TextEncoder || !window.Promise || !window.requestAnimationFrame) {
    return;
  }

  // viewerFirstViewKey() in viewer/core/shared_resources.R.
  function firstViewKey() {
    var dataset = params.get('dataset');
    var address = window.location.pathname + '\n' + (dataset == null ? '' : dataset);
    var bytes = new TextEncoder().encode(address), key = '';
    for (var i = 0; i < bytes.length; i++) {
      key += (bytes[i] < 16 ? '0' : '') + bytes[i].toString(16);
    }
    return key;
  }

  function validDescription(view) {
    // A Viewer Pack file under its shared prefix, or one of the two files the
    // server wrote for a data set without a Pack.
    var file = /^(cerebro-(projection|metadata)-[a-z0-9]+-[0-9]+\/[A-Za-z0-9._-]+|cerebro-first-view\/[0-9a-f]+\.[0-9a-f]+\.(xy|codes)\.bin)$/;
    function range(value) {
      return value == null || (Array.isArray(value) && value.length === 2 &&
        isFinite(value[0]) && isFinite(value[1]) && value[0] !== value[1]);
    }
    if (!view || view.version !== 1 || view.id !== VIEW_ID ||
        !Number.isSafeInteger(view.cells) || view.cells < 1 ||
        !Array.isArray(view.colors) || !view.colors.length ||
        !Array.isArray(view.code_map) ||
        !(view.point_size > 0) || !(view.point_opacity > 0) ||
        !view.projection || !view.categories ||
        !file.test(String(view.projection.url)) ||
        !file.test(String(view.categories.url)) ||
        view.projection.bytes !== view.cells * 8 ||
        view.categories.bytes !== view.cells ||
        !range(view.x_range) || !range(view.y_range) ||
        (view.x_range == null) !== (view.y_range == null)) return null;
    for (var i = 0; i < view.colors.length; i++) {
      if (!/^#[0-9A-Fa-f]{6}$/.test(view.colors[i])) return null;
    }
    // Optional: a description kept by an earlier release has no names.
    if (!Array.isArray(view.labels) || view.labels.length !== view.colors.length) {
      view.labels = null;
    }
    return view;
  }

  function fetchBuffer(resource) {
    return window.fetch(resource.url, { credentials: 'same-origin', cache: 'force-cache' })
      .then(function (response) {
        if (!response.ok) throw new Error('first view: ' + response.status);
        return response.arrayBuffer();
      }).then(function (buffer) {
        if (buffer.byteLength !== resource.bytes) throw new Error('first view: size');
        return buffer;
      });
  }

  var description = window.fetch(
    'cerebro-first-view/' + firstViewKey() + '.json',
    { credentials: 'same-origin', cache: 'no-cache' }
  ).then(function (response) {
    return response.ok ? response.json() : null;
  }).then(validDescription).catch(function () { return null; });

  // The cells in unit coordinates and their colours, once both files are here.
  var cells = description.then(function (view) {
    if (!view) return null;
    return Promise.all([
      fetchBuffer(view.projection), fetchBuffer(view.categories)
    ]).then(function (buffers) {
      return prepare(view, new Float32Array(buffers[0]), new Uint8Array(buffers[1]));
    });
  }).catch(function () { return null; });

  function packed(hex, opacity) {
    var value = parseInt(hex.slice(1), 16);
    var alpha = Math.max(0, Math.min(255, Math.round(255 * opacity)));
    // ImageData is RGBA in memory: ABGR as a little-endian word.
    return ((alpha << 24) | ((value & 255) << 16) | (value & 0xff00) | (value >> 16)) >>> 0;
  }

  function prepare(view, xy, codes) {
    var n = view.cells;
    var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity, i, x, y;
    if (view.x_range) {
      // The frame names its axes (a data set without a Viewer Pack).
      x0 = Math.min(view.x_range[0], view.x_range[1]);
      x1 = Math.max(view.x_range[0], view.x_range[1]);
      y0 = Math.min(view.y_range[0], view.y_range[1]);
      y1 = Math.max(view.y_range[0], view.y_range[1]);
    } else {
      for (i = 0; i < n; i++) {
        x = xy[2 * i]; y = xy[2 * i + 1];
        if (x !== x || y !== y) continue;
        if (x < x0) x0 = x; if (x > x1) x1 = x;
        if (y < y0) y0 = y; if (y > y1) y1 = y;
      }
    }
    if (!(x1 >= x0) || !(y1 >= y0)) return null;
    // unitOf() in cell_views.js: each axis fills its own unit interval.
    var kx = 1 / ((x1 - x0) || 1), ky = 1 / ((y1 - y0) || 1);
    var nx = new Float32Array(n), ny = new Float32Array(n);
    for (i = 0; i < n; i++) {
      nx[i] = (xy[2 * i] - x0) * kx;
      ny[i] = (xy[2 * i + 1] - y0) * ky;
    }
    // A code is a position in code_map, which gives the level (or -1).
    var colorOfCode = new Uint32Array(256), missing = packed(MISSING, view.point_opacity);
    for (i = 0; i < 256; i++) {
      var level = i < view.code_map.length ? view.code_map[i] : -1;
      colorOfCode[i] = level >= 0 && level < view.colors.length
        ? packed(view.colors[level], view.point_opacity) : missing;
    }
    // The legend of the plot: each group with the number of its cells.
    var legend = null;
    if (view.labels) {
      var counts = new Float64Array(view.colors.length);
      for (i = 0; i < n; i++) {
        var group = codes[i] < view.code_map.length ? view.code_map[codes[i]] : -1;
        if (group >= 0 && group < counts.length) counts[group]++;
      }
      legend = view.labels.map(function (label, index) {
        return { label: String(label), color: view.colors[index], count: counts[index] };
      });
    }
    return { n: n, nx: nx, ny: ny, codes: codes, colorOfCode: colorOfCode,
      pointSize: view.point_size, legend: legend };
  }

  // Each cell as the plot draws it, a disc of the point size, without the
  // plot's antialiasing: a million of them take a few tens of milliseconds.
  function paint(canvas, data, width, height) {
    var ratio = Math.max(1, Math.min(3, window.devicePixelRatio || 1));
    var W = Math.max(1, Math.round(width * ratio)), H = Math.max(1, Math.round(height * ratio));
    canvas.width = W; canvas.height = H;
    var context = canvas.getContext('2d');
    var image = context.createImageData(W, H);
    var pixels = new Uint32Array(image.data.buffer);
    var diameter = data.pointSize * ratio;
    var side = Math.max(1, Math.round(diameter));
    var half = side / 2, radius = diameter / 2 + 0.25;
    // The pixels of one disc, as offsets from its top left corner.
    var disc = [];
    for (var dy = 0; dy < side; dy++) {
      for (var dx = 0; dx < side; dx++) {
        var cx = dx + 0.5 - half, cy = dy + 0.5 - half;
        if (side < 3 || cx * cx + cy * cy <= radius * radius) disc.push(dy * W + dx);
      }
    }
    var count = disc.length;
    var ox = PAD * ratio, oy = PAD * ratio;
    var SX = Math.max(1, W - 2 * PAD * ratio), SY = Math.max(1, H - 2 * PAD * ratio);
    var nx = data.nx, ny = data.ny, codes = data.codes, colors = data.colorOfCode;
    for (var i = 0, n = data.n; i < n; i++) {
      var x = nx[i], y = ny[i];
      if (x !== x || y !== y) continue;
      var left = Math.round(ox + x * SX - half);
      var top = Math.round(oy + SY - y * SY - half);
      if (left < 0 || top < 0 || left + side > W || top + side > H) continue;
      var color = colors[codes[i]], at = top * W + left;
      for (var k = 0; k < count; k++) pixels[at + disc[k]] = color;
    }
    context.putImageData(image, 0, 0);
  }

  var root = document.documentElement;
  var preview = null;      // { canvas, host, box, timer, frame }
  var finished = false;

  function installStyle() {
    var style = document.createElement('style');
    style.textContent =
      // The page's own "loading" cards would sit on the preview they announce.
      'html.cerebro-first-view .cerebro-page-loader,' +
      'html.cerebro-first-view .cerebro-dataset-loading { display: none !important; }' +
      '.cerebro-first-view-canvas { position: absolute; z-index: 3; pointer-events: none;' +
      ' transform-origin: 0 0; }' +
      // Out of the flow, so that the plot's own legend is not pushed down
      // while both exist.
      '.cerebro-first-view-legend { position: absolute; z-index: 3; left: 0; right: 0;' +
      ' top: 0; pointer-events: none; }' +
      // The selectors arrive with the session: their row is kept free, so the
      // plot does not move down when they do.
      'html.cerebro-first-view #shiny-tab-' + TAB + ' .cerebro-viz-primary' +
      ' { min-height: ' + SELECTORS_HEIGHT + 'px; }';
    document.head.appendChild(style);
  }

  // shinydashboard shows the tab marked as its start tab when the document is
  // ready, before Shiny reads which outputs are hidden. Marking the page's tab
  // here, earlier still, opens the page as if it had been configured as the
  // start page: no visit to Data info, and no second round with the server.
  function startOnPage() {
    var links = document.querySelectorAll('.main-sidebar a[data-toggle="tab"]');
    var target = null, i;
    for (i = 0; i < links.length; i++) {
      if (links[i].getAttribute('data-value') === TAB) target = links[i];
    }
    if (!target) return false;
    for (i = 0; i < links.length; i++) links[i].removeAttribute('data-start-selected');
    target.setAttribute('data-start-selected', '1');
    return true;
  }

  // The legend as renderLegend() in cell_views.js builds it, with the classes
  // its styles are written for, so that it wraps as the plot's legend will.
  function legendElement(items) {
    var wrap = document.createElement('div');
    wrap.className = 'cerebro-cell-view-legend cerebro-first-view-legend';
    wrap.setAttribute('aria-hidden', 'true');
    var list = document.createElement('div');
    list.className = 'cv-legend';
    items.forEach(function (entry) {
      var item = document.createElement('div');
      item.className = 'cv-lg';
      var dot = document.createElement('span');
      dot.className = 'cv-dot';
      dot.style.background = entry.color;
      var count = document.createElement('span');
      count.className = 'cv-lg-ct';
      count.textContent = '(' + entry.count + ')';
      item.appendChild(dot);
      item.appendChild(document.createTextNode(entry.label + ' '));
      item.appendChild(count);
      list.appendChild(item);
    });
    wrap.appendChild(list);
    return wrap;
  }

  // Where the canvas begins below the top of the host: under the legend when
  // the description names the groups. The legend gives way to the plot's own
  // as soon as that has its entries; the place it measured stays.
  function canvasTop() {
    var legend = preview.legend;
    if (!legend) return preview.top == null ? INSET.top : preview.top;
    var real = preview.host.querySelector('.cerebro-cell-view-surface .cv-legend');
    if (real && real.children.length) {
      legend.parentNode.removeChild(legend);
      preview.legend = null;
    } else if (legend.offsetHeight) {
      preview.top = legend.offsetHeight + PLOT_HEAD;
    }
    return preview.top == null ? INSET.top : preview.top;
  }

  // The canvas box inside the host, in CSS pixels, or null while the page is
  // not laid out.
  function box(host) {
    var rect = host.getBoundingClientRect();
    if (!(rect.width > INSET.left + INSET.right + 50)) return null;
    var top = canvasTop();
    return {
      left: INSET.left,
      top: top,
      width: Math.round(rect.width - INSET.left - INSET.right),
      height: Math.max(MIN_HEIGHT,
        Math.round(window.innerHeight - rect.top - top - INSET.below))
    };
  }

  function place(data) {
    if (!preview || finished) return;
    var next = box(preview.host);
    var current = preview.box;
    if (next && (!current || next.width !== current.width ||
        next.height !== current.height || next.top !== current.top)) {
      var canvas = preview.canvas;
      canvas.style.left = next.left + 'px';
      canvas.style.top = next.top + 'px';
      canvas.style.width = next.width + 'px';
      canvas.style.height = next.height + 'px';
      // Reserve the plot's place, so the page does not jump when it arrives.
      preview.host.style.minHeight = (next.top + next.height + 17) + 'px';
      paint(canvas, data, next.width, next.height);
      preview.box = next;
      if (!current) shown();
    }
    preview.frame = window.requestAnimationFrame(function () { place(data); });
  }

  function shown() {
    root.classList.add('cerebro-first-view');
    window.CerebroFirstView = { shown: true };
    // startup.js lifts the cover for a page that already shows its plot.
    window.dispatchEvent(new CustomEvent('cerebro:first-view', { detail: { id: VIEW_ID } }));
  }

  function mount(data) {
    var host = document.getElementById(VIEW_ID + '_cell_view_host');
    if (!host || finished) return;
    var canvas = document.createElement('canvas');
    canvas.className = 'cerebro-first-view-canvas';
    canvas.setAttribute('aria-hidden', 'true');
    host.appendChild(canvas);
    preview = { canvas: canvas, host: host, box: null, frame: 0, legend: null, top: null };
    if (data.legend) {
      preview.legend = legendElement(data.legend);
      host.appendChild(preview.legend);
    }
    preview.timer = window.setTimeout(function () { finish(null); }, GIVE_UP_MS);
    place(data);
  }

  // Removes the preview. With the plot's canvas, the preview first moves onto
  // it: both show the same cells in their own box, so the two coincide when
  // the boxes do.
  function finish(target) {
    if (finished) return;
    finished = true;
    window.removeEventListener('cerebro:cell-view-ready', onPlotReady);
    window.removeEventListener('cerebro:dataset-context', onContext);
    if (!preview) return;
    var canvas = preview.canvas, host = preview.host;
    window.cancelAnimationFrame(preview.frame);
    window.clearTimeout(preview.timer);
    if (preview.legend && preview.legend.parentNode) {
      preview.legend.parentNode.removeChild(preview.legend);
    }
    preview.legend = null;
    function remove() {
      if (canvas.parentNode) canvas.parentNode.removeChild(canvas);
      host.style.minHeight = '';
      root.classList.remove('cerebro-first-view');
    }
    var from = canvas.getBoundingClientRect();
    var to = target && target.getBoundingClientRect();
    if (!to || !from.width || !to.width || !preview.box) return remove();
    var moved = Math.abs(to.left - from.left) > 1 || Math.abs(to.top - from.top) > 1 ||
      Math.abs(to.width - from.width) > 1 || Math.abs(to.height - from.height) > 1;
    var reduced = window.matchMedia &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    if (!moved || reduced) return remove();
    canvas.style.transition = 'transform 120ms ease-out';
    canvas.style.transform = 'translate(' + (to.left - from.left) + 'px,' +
      (to.top - from.top) + 'px) scale(' + (to.width / from.width) + ',' +
      (to.height / from.height) + ')';
    window.setTimeout(remove, 140);
  }

  function onPlotReady(event) {
    if (!event.detail || event.detail.id !== VIEW_ID) return;
    var host = document.getElementById(VIEW_ID + '_cell_view_host');
    var surface = host && host.querySelector('.cerebro-cell-view-surface');
    finish(surface && surface.querySelector('canvas'));
  }
  function onContext(event) {
    if (event.detail && event.detail.phase === 'error') finish(null);
  }
  window.addEventListener('cerebro:cell-view-ready', onPlotReady);
  window.addEventListener('cerebro:dataset-context', onContext);

  function whenParsed(run) {
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', run, { once: true });
    } else {
      run();
    }
  }

  whenParsed(function () {
    if (!startOnPage()) {
      finish(null);
      return;
    }
    cells.then(function (data) {
      if (!data || finished) return finish(null);
      installStyle();
      mount(data);
    });
  });
})();
