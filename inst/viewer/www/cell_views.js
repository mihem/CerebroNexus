/* ==========================================================================
   Shared cell-view Canvas engine.

   Every modality is just a named 2-D layout of the SAME cells:
     umap    = expression embedding
     spatial = physical coordinates
     clone   = (clone expansion-rank, within-clone index)
   The engine renders one specialist view or N linked panels, each showing one
   "space", over a shared selection set keyed on CELL INDEX. Because selection
   is by cell index,
   brushing in any panel highlights the same cells in every other panel —
   across modalities — with no special-casing. This is the coordination fabric
   the paper claims, generalised so the immune repertoire is a first-class
   space (which a generic viewer cannot express).

   One full R->JS handoff per dataset (Shiny message "coordviews_data"); later
   palette edits travel as a small "coordviews_colors" patch. All interaction
   (brush, highlight, readout) is client-side and instant.

   All ids are `cv-`-prefixed; all styles scoped under `.coordviews-page`.
   ========================================================================== */
(function () {
  'use strict';

  // Shared by Spatial and Linked views; helpers never mutate saved presets.
  var backgrounds = window.CerebroBackgroundModel;
  var spatialImages = backgrounds.images, currentImage = backgrounds.currentImage;
  var backgroundStateKey = backgrounds.stateKey, imgKey = backgrounds.imageKey;
  var presetState = backgrounds.presetState, imageRenderState = backgrounds.renderState;

  var D = null;                 // the data bundle
  var linkedBundle = null;      // cached while a dedicated page owns the canvas
  // Specialist payloads and requests are scoped to one exact server-session
  // dataset load. A page id alone is not an identity: every data set has a
  // `spatial_projection`, and a late response from the previous one must never
  // be allowed to recreate that cache entry after a switch.
  var singleViews = {};
  var singleRequests = new Set();
  var singleResourceDescriptors = new Map();
  var singleDefaultResourceNames = new Map();
  var singleCategoryResourceDescriptors = new Map();
  var singleActive = null;      // plot id currently using the shared surface
  var singleIndexCells = null;  // cells array behind the cached barcode index
  var singleIndexMap = null;
  // Last barcode (core aux) message per view. A render whose cells match a
  // delivered one names it in data.aux_reuse, so it is not downloaded again.
  var singleAuxCache = {};
  var singleSpaceIds = [];      // one space, or one per gene in multi-panel mode
  var singleSpaceModes = {};    // space id -> categorical/continuous colour mode
  var singleColorCache = new Map();
  var SINGLE_COLOR_CACHE_LIMIT = 4;
  var surfaceHome = null;       // original Linked Views panel/legend locations
  var linkedState = null;       // Linked workspace state while a single page owns the surface
  var pendingColorPatch = null; // palette received before its dataset bundle
  var progressiveRequestedKey = null;
  var assetCache = Object.create(null);
  var pendingAssets = new Set();
  var pendingCloneDetails = new Set();
  var pendingCloneSupplement = null;
  var deferredLinkedSupplements = [];
  var transportMetrics = {};
  var linkedRequestTiming = {};
  var singleTiming = Object.create(null);
  var specialistPayloadMeter = null;
  window.__cerebroResetSpecialistBench = function () {
    specialistPayloadMeter = {
      primary: { count: 0, bytes: 0, byId: {} },
      aux: { count: 0, bytes: 0, byId: {} }
    };
    return true;
  };
  window.__cerebroSpecialistBenchSnapshot = function () {
    return specialistPayloadMeter
      ? JSON.parse(JSON.stringify(specialistPayloadMeter))
      : null;
  };
  function recordSpecialistPayload(kind, id, bytes) {
    if (!specialistPayloadMeter || !specialistPayloadMeter[kind]) return;
    var metric = specialistPayloadMeter[kind];
    var key = String(id || 'unknown');
    metric.count += 1;
    metric.bytes += Number(bytes) || 0;
    metric.byId[key] = metric.byId[key] || { count: 0, bytes: 0 };
    metric.byId[key].count += 1;
    metric.byId[key].bytes += Number(bytes) || 0;
  }
  var panels = [];              // [{key, canvas, ctx, spaceId, W, H, sx, sy, lasso, drag, moved}]
  var gpuAcceleration = true;   // session preference, independent of data/page
  var sel = null;               // Set of selected cell indices (null = none)
  var selectionZoomed = false;
  var selectionSource = null;   // label of the lens that created the active cohort
  var selectionSourceSpace = null; // space id of the lens that created it
  var pick = null;              // hovered/clicked cell index
  // Panel + cell of a PINNED tooltip: the one a click left in place, carrying the
  // Details and Close buttons. {null, null} when no tooltip is pinned.
  var pinnedTip = { panel: null, cell: null };
  // Cell under the cursor, wherever it is. The whole point of linked views is that
  // a cell is the SAME cell in every panel, so pointing at it in one has to mark it
  // in the others -- that is how the eye carries a position in the embedding over
  // to a position in tissue. Null when the cursor is not on a cell.
  var hoverCell = null;
  var hoverPanel = null;
  var cellTooltipsOn = true;
  var linkedHoverOn = true;
  var linkedBackgroundsOn = true;
  // The gene a reply is still wanted for. Set when one is asked for and cleared by
  // the reply, so a late answer for a gene the user has moved on from is dropped
  // rather than drawn under the current gene's name.
  var geneWanted = null;
  // Alignment is a property of a (section, background image) PAIR, not of the
  // workspace. One shared imgState meant the numbers followed the user from one
  // slide to another, which is how a calibration ended up describing an image it
  // was never made for. Keyed `section|imageId`; cleared with the data set, since
  // the ids belong to the object that produced them.
  var imgStates = {};
  // Which background each section was last showing, so returning to a section
  // returns the view of it the user left, not merely its alignment. Keyed by
  // section name; cleared with the data set.
  var imgChoice = {};
  // The data set the current state belongs to. Compared against the incoming
  // bundle's identity to tell a new data set from a re-sent one.
  var dataShown = null;
  var wireToken = 0;
  // Guards the async decode: a fast switch could have an earlier image finish
  // loading after a later one and paint itself over the current choice.
  var imgToken = 0;
  // The visible spatial sections are independent spaces. Selection, colour and
  // filters are global; image choice/alignment stay on their own section. One of
  // them is active so the single alignment bar has an unambiguous target.
  var selectedSpatial = [];
  var activeSpatialId = null;
  var backgroundScopePulse = false;
  var spatialTemplate = null;
  // Panel key currently promoted as the primary lens, or null for equal overview.
  // Context lenses stay present and coordinated while the primary grows.
  var focusPanel = null;
  var selectMode = 'lasso';     // drag-select mode: 'lasso' (freeform) or 'box'
  // Trekker controls brought into Linked views (only when D.trekker is present):
  var dissolvePct = 0;          // % of least-confident nuclei to dissolve
  var dissolveThresh = null;    // conf value below which cells are hidden
  var evidenceOn = false;       // ring nuclei that carry positioning evidence
  var nicheRadius = 250;        // µm radius for the picked-nucleus niche readout
  var nicheSet = null;          // cell indices inside the picked nucleus's niche
  var colorBy = null;           // group name
  var ps = 5;                   // point diameter in CSS pixels
  var pointOpacity = 1;         // base draw opacity (no-selection view)
  var bordersOn = false;        // outline cell points in the active view
  var keepPlotsSquare = false;  // fluid rectangles use the workspace by default
  // A live user edit overrides Builder's per-spatial-section starting values.
  var pointSizeEdited = false;
  var pointOpacityEdited = false;
  var hidden = new Set();       // hidden level indices for the active group
  // Every selected embedding is an independent panel. Colour/filter/selection
  // state remains global, while viewport and 3-D rotation live on the panel.
  var selectedProjections = [];
  var pctShow = 100;            // % of cells to render
  var sharedScatter = null;
  var scatterUserEdit = null;
  var scatterControlFields = {
    'cv-ps': 'point_size', 'cv-opacity': 'point_opacity',
    'cv-pct': 'percentage_cells_to_show',
    'ir_d_point_size': 'point_size', 'ir_d_alpha': 'point_opacity',
    'ir_d_percentage_cells_to_show': 'percentage_cells_to_show'
  };
  ['overview_projection', 'expression_projection', 'trajectory'].forEach(function (prefix) {
    ['point_size', 'point_opacity', 'percentage_cells_to_show'].forEach(function (field) {
      scatterControlFields[prefix + '_' + field] = field;
    });
  });
  function sharedScatterValues() {
    return sharedScatter && sameDatasetContext(sharedScatter.dataset_context,
      currentDatasetContext()) ? sharedScatter.values : null;
  }
  function syncSharedScatter() {
    var values = sharedScatterValues();
    if (!values) return;
    if (linkedBundle) {
      linkedBundle.default_point_size = values.point_size;
      linkedBundle.default_point_opacity = values.point_opacity;
      linkedBundle.default_percentage_cells_to_show = values.percentage_cells_to_show;
    }
    if (linkedState && linkedState.view) Object.assign(linkedState.view.display, {
      point_size: values.point_size, point_opacity: values.point_opacity,
      percentage_cells: values.percentage_cells_to_show
    });
    // Spatial has its own section/ROI settings. Never apply projection edits.
    if (singleActive === 'spatial_projection') return;
    ps = values.point_size; pointOpacity = values.point_opacity;
    if (!singleActive || singleClientPercentage()) {
      pctShow = values.percentage_cells_to_show;
      rebuildPctMask();
    }
  }
  // Single views whose frame carries every cell and leaves "% of cells" to
  // the browser (Overview with a Viewer Pack).
  function singleClientPercentage() {
    var view = singleActive && singleView(singleActive);
    return !!(view && view.data && view.data.client_percentage);
  }
  function publishScatterEdit(field, value) {
    if (!field || !currentDatasetContext()) return;
    var known = sharedScatterValues();
    var values = Object.assign({}, known || {});
    values[field] = value;
    if (known) sharedScatter = {dataset_context: currentDatasetContext(), values: values};
    Shiny.setInputValue('viewer_scatter_edit', {
      dataset_context: currentDatasetContext(), values: values,
      nonce: Date.now()
    }, {priority: 'event'});
  }
  var pctMask = null;           // Uint8Array subsample mask, or null (all shown)
  var groupFilter = {};         // groupName -> Set(allowed level idx); absent = all
  var spaceById = {};
  var resizeObserver = null;    // fires when the tab becomes visible / resizes
  var resizeTimer = null;
  var focusResizeTimer = null;
  var focusAnimating = false;
  // Each spatial instance owns `_imgEl`, `_imgReady`, `_imgState` and its image
  // identity. The alignment bar edits only activeSpatialId.

  function datasetContextApi() {
    return window.CerebroDatasetContext || null;
  }
  function currentDatasetContext() {
    var api = datasetContextApi();
    return api && api.current ? api.current() : null;
  }
  function currentDatasetContextKey() {
    var api = datasetContextApi();
    return api && api.key ? api.key(currentDatasetContext()) : '';
  }
  function datasetMessageFresh(message) {
    var api = datasetContextApi();
    return !!(api && api.accepts && api.accepts(message));
  }
  function sameDatasetContext(left, right) {
    var api = datasetContextApi();
    return !!(api && api.same && api.same(left, right));
  }
  function singleCacheKey(id, context) {
    var api = datasetContextApi();
    var contextKey = api && api.key
      ? api.key(context || currentDatasetContext()) : '';
    return id && contextKey ? contextKey + '\u0000' + id : '';
  }
  function singleView(id, context) {
    var cacheKey = singleCacheKey(id, context);
    return cacheKey ? singleViews[cacheKey] : null;
  }
  function singleRequestKey(id, context) {
    return singleCacheKey(id, context);
  }
  function canonicalIdentity(value) {
    if (value == null) return '';
    if (typeof value !== 'object') return JSON.stringify(value);
    if (Array.isArray(value)) {
      return '[' + value.map(canonicalIdentity).join(',') + ']';
    }
    return '{' + Object.keys(value).sort().map(function (name) {
      return JSON.stringify(name) + ':' + canonicalIdentity(value[name]);
    }).join(',') + '}';
  }

  // Categorical fallback palette (mirrors the app).
  var PAL = ['#636EFA', '#EF553B', '#00CC96', '#AB63FA', '#FFA15A', '#19D3F3',
    '#FF6692', '#B6E880', '#FF97FF', '#FECB52', '#2f6fd6', '#f97316',
    '#16a34a', '#9a5cd0', '#e05780', '#38b2ac', '#d97706', '#7bb0e8'];

  function updateCanvasDescription(p) {
    if (!p || !p.canvas || !p.description) return;
    var space = p.spaceId && spaceById[p.spaceId];
    var label = String((space && space.label) || 'Cell');
    var pointCount = D && Number(D.n) || 0;
    var selectedCount = sel ? sel.size : 0;
    var description = label + ' view with ' + pointCount + ' points; ' +
      selectedCount + ' selected.';
    p.canvas.setAttribute('role', 'img');
    p.canvas.setAttribute('aria-label', label + ' cell view');
    if (p.description.textContent !== description) {
      p.description.textContent = description;
    }
  }

  function prepareCanvasSemantics(p) {
    // Cloned panels inherit the original canvas' attributes. A description
    // belongs to this canvas, never to the panel it was cloned from.
    var descriptionId = p.canvas.id.replace(/^cv-cv-/, 'cv-description-');
    var description = document.getElementById(descriptionId);
    if (description && description.parentNode !== p.canvas.parentNode) description = null;
    if (!description) {
      description = document.createElement('span');
      description.id = descriptionId;
      description.className = 'sr-only';
      p.canvas.parentNode.insertBefore(description, p.canvas.nextSibling);
    }
    p.canvas.setAttribute('aria-describedby', descriptionId);
    p.description = description;
    if (p.mini) p.mini.setAttribute('aria-hidden', 'true');
    updateCanvasDescription(p);
  }

  function cssEscape(value) {
    if (window.CSS && typeof window.CSS.escape === 'function') {
      return window.CSS.escape(String(value));
    }
    return String(value).replace(/[^a-zA-Z0-9_-]/g, '\\$&');
  }

  // RGB co-expression: cells whose max channel is <= RGB_MIN form a light-grey
  // substrate; the rest blend FROM that grey toward their full-brightness hue by
  // intensity (max/255), so weak co-expression stays faint (≈grey). Cells above
  // threshold in all three channels are black so triple co-expression remains
  // visible on the white canvas. One threshold shared by colour and layering.
  var RGB_MIN = 28;
  var RGB_GREY_RGB = [217, 219, 222];
  var RGB_GREY = 'rgb(' + RGB_GREY_RGB.join(',') + ')';

  // Viridis anchors for continuous (single-gene) colouring.
  var VIR = [[68, 1, 84], [72, 40, 120], [62, 73, 137], [49, 104, 142],
    [38, 130, 142], [31, 158, 137], [53, 183, 121], [110, 206, 88],
    [181, 222, 43], [253, 231, 37]];
  function viridis(t) {
    t = Math.max(0, Math.min(1, t));
    var s = t * (VIR.length - 1), i = Math.floor(s), f = s - i;
    var a = VIR[i], b = VIR[Math.min(i + 1, VIR.length - 1)];
    return [Math.round(a[0] + (b[0] - a[0]) * f),
      Math.round(a[1] + (b[1] - a[1]) * f),
      Math.round(a[2] + (b[2] - a[2]) * f)];
  }
  // Continuous colouring builds a CSS colour per cell per draw; on a large data
  // set that is hundreds of thousands of string allocations per frame, on the
  // lasso-drag path. The ramp only has 256 distinct steps, so build them once.
  var VIR_CSS = (function () {
    var out = new Array(256);
    for (var k = 0; k < 256; k++) {
      var c = viridis(k / 255);
      out[k] = 'rgb(' + c[0] + ',' + c[1] + ',' + c[2] + ')';
    }
    return out;
  })();
  function viridisCss(t) {
    var k = Math.round(Math.max(0, Math.min(1, t)) * 255);
    return VIR_CSS[k];
  }
  var SINGLE_SCALES = {
    YlGnBu: ['#ffffd9', '#c7e9b4', '#41b6c4', '#225ea8', '#081d58'],
    YlOrRd: ['#ffffcc', '#fed976', '#fd8d3c', '#e31a1c', '#800026'],
    Blues: ['#f7fbff', '#c6dbef', '#6baed6', '#2171b5', '#08306b'],
    Greens: ['#f7fcf5', '#c7e9c0', '#74c476', '#238b45', '#00441b'],
    Reds: ['#fff5f0', '#fcbba1', '#fb6a4a', '#cb181d', '#67000d'],
    RdBu: ['#67001f', '#d6604d', '#f7f7f7', '#4393c3', '#053061'],
    Viridis: ['#440154', '#3b528b', '#21918c', '#5ec962', '#fde725']
  };
  function rgbChannels(color) {
    var value = cssColor(color, '#888888');
    if (value[0] === '#') {
      if (value.length === 4) value = '#' + value[1] + value[1] + value[2] +
        value[2] + value[3] + value[3];
      return [parseInt(value.slice(1, 3), 16), parseInt(value.slice(3, 5), 16),
        parseInt(value.slice(5, 7), 16)];
    }
    var match = value.match(/[\d.]+/g);
    return match ? match.slice(0, 3).map(Number) : [136, 136, 136];
  }
  function singlePalette(scale, reverse) {
    var raw = typeof scale === 'string' ? SINGLE_SCALES[scale] : scale;
    if (!Array.isArray(raw) || !raw.length) return null;
    var stops = Array.isArray(raw[0]) ? raw.map(function (item) {
      return [Number(item[0]), item[1]];
    }) : raw.map(function (color, index) {
      return [index / Math.max(1, raw.length - 1), color];
    });
    if (reverse) stops = stops.map(function (item) {
      return [1 - item[0], item[1]];
    }).sort(function (a, b) { return a[0] - b[0]; });
    var out = new Array(256), right = 1;
    for (var i = 0; i < 256; i++) {
      var t = i / 255;
      while (right < stops.length - 1 && t > stops[right][0]) right++;
      var left = stops[Math.max(0, right - 1)], next = stops[right];
      var span = next[0] - left[0], local = span > 0 ? (t - left[0]) / span : 0;
      var a = rgbChannels(left[1]), b = rgbChannels(next[1]);
      out[i] = 'rgb(' + [0, 1, 2].map(function (channel) {
        return Math.round(a[channel] + (b[channel] - a[channel]) * local);
      }).join(',') + ')';
    }
    return out;
  }

  function $(id) { return document.getElementById(id); }
  // Level names, column names and clonotype labels all come from the data set,
  // so everything interpolated into innerHTML goes through here.
  function esc(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;')
      .replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  }
  // Colours reach a `style` attribute rather than a text node, and they are as
  // data-driven as the labels are: the group colours are seeded from the object
  // and only then made editable in Colour management. Escaping is the wrong tool
  // here -- `red;position:fixed;inset:0` never leaves the attribute, so nothing
  // needs quoting for it to append declarations of its own -- so a value that is
  // not a colour is replaced by one that is.
  //
  // The browser's own parser is the authority, but it has to be the RIGHT
  // parser. `CSS.supports('color', x)` answers "is this a legal value for the
  // CSS color property", which is a wider question: the CSS-wide keywords
  // (inherit, initial, unset, revert) and var(--x) all pass it and none of them
  // is a colour a canvas can paint. The strictest consumer here is the canvas,
  // so ask the canvas.
  //
  // Assigning an unparsable value to fillStyle is a no-op -- it keeps whatever
  // was there -- which is also the failure being defended against. Two different
  // sentinels turn that silence into an answer: a refused value reads back as
  // whichever sentinel preceded it, so the two reads disagree. One sentinel
  // could not tell a refusal apart from a colour that happens to equal it.
  var _colCtx = (function () {
    try {
      var el = document.createElement('canvas');
      el.width = 1; el.height = 1;
      return el.getContext('2d');
    } catch (e) { return null; }
  })();
  function cssColor(c, fallback) {
    var s = String(c == null ? '' : c).trim();
    var fb = fallback || '#888888';
    if (!_colCtx) {
      return /^#([0-9a-f]{3}|[0-9a-f]{4}|[0-9a-f]{6}|[0-9a-f]{8})$/i.test(s)
        ? s : fb;
    }
    _colCtx.fillStyle = '#000000'; _colCtx.fillStyle = s;
    var a = _colCtx.fillStyle;
    _colCtx.fillStyle = '#ffffff'; _colCtx.fillStyle = s;
    return a === _colCtx.fillStyle ? a : fb;
  }
  // Validate every colour once, as the bundle lands, so no consumer has to
  // remember to. The canvas is why this is done here rather than at each sink:
  // it takes a colour per point per frame, which is no place for a parser call.
  function sanitiseColors(bundle) {
    ['groups', 'cat_extra'].forEach(function (k) {
      var m = bundle[k]; if (!m) return;
      Object.keys(m).forEach(function (g) {
        var cols = m[g] && m[g].colors; if (!cols) return;
        for (var i = 0; i < cols.length; i++) cols[i] = cssColor(cols[i]);
      });
    });
  }
  function fmt(n) { return (n == null || isNaN(n)) ? '—' : n.toLocaleString('en-US'); }
  function cellLabel(index) {
    return Array.isArray(D && D.cells) && D.cells[index] != null
      ? String(D.cells[index]) : 'Cell #' + (Number(index) + 1).toLocaleString('en-US');
  }
  // Human-facing label for a group key. Metadata columns keep their own names;
  // the server-synthesised expansion group gets a readable label.
  function groupLabel(g) { return g === 'clone_expansion' ? 'Clone Expansion' : g; }

  // ---- colouring sources ---------------------------------------------------
  // Shared colouring sources used by Linked and specialist cell views:
  //   D.groups    registered grouping variables — colour AND group filters
  //   D.cat_extra specialist categorical modes — colour only (no filter)
  //   D.fields    specialist continuous modes, including Trekker fields
  // Everything that reads a categorical colouring goes through catOf(), so the
  // two categorical sources never have to be special-cased at the call site.
  function catOf(name) {
    if (!D || !name) return null;
    var group = (D.groups && D.groups[name]) ||
      (D.cat_extra && D.cat_extra[name]);
    // Deferred descriptors expose labels before their per-cell codes arrive.
    // Readouts and selection must not treat these descriptors as usable data.
    return group && group.values && group.levels ? group : null;
  }
  // The categorical variable the composition readout summarises by: cell_type
  // when present, else the active categorical colouring, else the first one
  // available. colorBy may be a gene/RGB/field pseudo-mode that is not
  // categorical at all, so it is used only when catOf() resolves it.
  function compGroupName() {
    if (!D) return null;
    if (catOf('cell_type')) return 'cell_type';
    if (catOf(colorBy)) return colorBy;
    var k = D.groups ? Object.keys(D.groups).filter(catOf) : [];
    if (k.length) return k[0];
    var e = D.cat_extra ? Object.keys(D.cat_extra).filter(catOf) : [];
    return e.length ? e[0] : null;
  }
  // True value behind a field's quantised code (fields travel 0..scale to keep
  // the bundle small; min/max carry the real range).
  function fieldValue(fld, i) {
    if (fld.constantValue != null) return fld.constantValue;
    if (fld.raw && fld.raw[i] != null && !isNaN(fld.raw[i])) return fld.raw[i];
    var q = fld.v[i];
    if (q == null || isNaN(q)) return null;
    return fld.min + (q / (fld.scale || 255)) * (fld.max - fld.min);
  }
  // Compact display of a continuous value: integers plain, small values with
  // enough decimals to be meaningful (percent.mt 3.7, a score 0.042).
  function fmtVal(v) {
    if (v == null) return '—';
    var a = Math.abs(v);
    if (a >= 1000) return Math.round(v).toLocaleString('en-US');
    if (a >= 10) return v.toFixed(1);
    if (a >= 1) return v.toFixed(2);
    return v.toFixed(3);
  }

  // ---- occupancy of the unit box ------------------------------------------
  // Which parts of the box actually hold points, on a coarse lattice, plus a
  // summed-area table over it. The SAT answers "does this view rectangle contain
  // any point?" in four lookups no matter how much of the box the view spans —
  // which is what makes the check affordable on the per-mousemove pan path.
  var OCC = 64;
  function occIdx(v) {
    var i = Math.floor(v * OCC);
    return i < 0 ? 0 : (i > OCC - 1 ? OCC - 1 : i);
  }
  function occSAT(occ) {
    var W = OCC + 1, s = new Int32Array(W * W);
    for (var y = 0; y < OCC; y++) {
      for (var x = 0; x < OCC; x++) {
        s[(y + 1) * W + x + 1] = occ[y * OCC + x] +
          s[y * W + x + 1] + s[(y + 1) * W + x] - s[y * W + x];
      }
    }
    return s;
  }
  // Number of occupied cells in the inclusive lattice rectangle (clipped).
  function occCount(u, gx0, gy0, gx1, gy1) {
    var W = OCC + 1, s = u.sat;
    gx0 = Math.max(0, gx0); gy0 = Math.max(0, gy0);
    gx1 = Math.min(OCC - 1, gx1); gy1 = Math.min(OCC - 1, gy1);
    if (gx1 < gx0 || gy1 < gy0) return 0;
    return s[(gy1 + 1) * W + gx1 + 1] - s[gy0 * W + gx1 + 1] -
      s[(gy1 + 1) * W + gx0] + s[gy0 * W + gx0];
  }
  // Only cells lying ENTIRELY within the view count. A cell the view merely
  // clips can hold its points on the outside of that edge, which is exactly how
  // a "there is data here" answer ends up in front of a blank canvas. Requiring
  // full containment can only err the safe way — toward reporting no data — and
  // the lattice is fine enough (1/64) that even the tightest zoom still contains
  // whole cells.
  function viewHasData(u, cx, cy, span) {
    var h = span / 2;
    return occCount(u,
      Math.ceil((cx - h) * OCC), Math.ceil((cy - h) * OCC),
      Math.floor((cx + h) * OCC) - 1, Math.floor((cy + h) * OCC) - 1) > 0;
  }

  // ---- per-space unit normalisation ---------------------------------------
  // Every 2-D layout fills its fluid canvas by normalising x/y independently.
  // Spatial views recover one physical screen scale when project() letterboxes
  // their unit box; only rotatable 3-D embeddings keep one shared axis scale here.
  function deferredUnitOf(space) {
    var xr = space.xRange, yr = space.yRange;
    if (space._fitToData || space.z || !Array.isArray(xr) || xr.length !== 2 ||
        !Array.isArray(yr) || yr.length !== 2) return null;
    var x0 = Math.min(Number(xr[0]), Number(xr[1]));
    var x1 = Math.max(Number(xr[0]), Number(xr[1]));
    var y0 = Math.min(Number(yr[0]), Number(yr[1]));
    var y1 = Math.max(Number(yr[0]), Number(yr[1]));
    if (![x0, x1, y0, y1].every(isFinite) || x0 === x1 || y0 === y1) return null;
    var n = space.x.length, dw = x1 - x0, dh = y1 - y0;
    return {
      nx: new Float32Array(n), ny: new Float32Array(n), nz: null,
      ok: new Uint8Array(n), x0: x0, y0: y0, k: 1 / dw, ky: 1 / dh,
      ox: 0, oy: 0, aspect: dw / dh,
      bx: { x0: 0, x1: 1, y0: 0, y1: 1 },
      occ: new Uint8Array(OCC * OCC), sat: null, cmx: 0.5, cmy: 0.5,
      _pending: { x: space.x, y: space.y }
    };
  }
  function unitOf(space) {
    var xs = space.x, ys = space.y, zs = space.z || null, n = xs.length;
    var xr = space.xRange, yr = space.yRange;
    // A linked spatial lens must use the current section's occupied extent as
    // its initial camera. Builder/editor viewports can still travel with the
    // data as coordinate metadata, but a viewport saved in a differently
    // shaped canvas must not shrink the tissue inside this panel. Dedicated
    // page adapters keep their explicit ranges because those are plot ranges,
    // not linked-workspace camera state.
    var fixedX = !space._fitToData && Array.isArray(xr) && xr.length === 2 &&
      isFinite(Number(xr[0])) && isFinite(Number(xr[1])) &&
      Number(xr[0]) !== Number(xr[1]);
    var fixedY = !space._fitToData && Array.isArray(yr) && yr.length === 2 &&
      isFinite(Number(yr[0])) && isFinite(Number(yr[1])) &&
      Number(yr[0]) !== Number(yr[1]);
    var x0 = fixedX ? Math.min(Number(xr[0]), Number(xr[1])) : Infinity;
    var x1 = fixedX ? Math.max(Number(xr[0]), Number(xr[1])) : -Infinity;
    var y0 = fixedY ? Math.min(Number(yr[0]), Number(yr[1])) : Infinity;
    var y1 = fixedY ? Math.max(Number(yr[0]), Number(yr[1])) : -Infinity;
    for (var i = 0; i < n; i++) {
      var xv = xs[i], yv = ys[i];
      if (xv == null || yv == null || isNaN(xv) || isNaN(yv)) continue;
      if (!fixedX) { if (xv < x0) x0 = xv; if (xv > x1) x1 = xv; }
      if (!fixedY) { if (yv < y0) y0 = yv; if (yv > y1) y1 = yv; }
    }
    var dw = (x1 - x0) || 1, dh = (y1 - y0) || 1;
    var kx, ky, ox, oy;
    if (!zs) {
      kx = 1 / dw; ky = 1 / dh; ox = 0; oy = 0;
    } else {
      var k = 1 / Math.max(dw, dh);
      kx = k; ky = k; ox = (1 - dw * k) / 2; oy = (1 - dh * k) / 2;
    }
    var nx = new Float32Array(n), ny = new Float32Array(n), ok = new Uint8Array(n);
    // ---- third dimension, when the embedding has one ----------------------
    // Normalised on the SAME scale as x/y (an embedding's axes are comparable,
    // so stretching one would misrepresent the shape), then the whole cloud is
    // scaled to fit inside the unit box's inscribed SPHERE. That is what makes
    // rotation safe: a cube's corners stick out of the box when you turn it, a
    // sphere never does, so no angle can throw points off the panel.
    var nz = null;
    if (zs) {
      var z0 = Infinity, z1 = -Infinity;
      for (i = 0; i < n; i++) {
        var zv = zs[i];
        if (zv == null || isNaN(zv)) continue;
        if (zv < z0) z0 = zv; if (zv > z1) z1 = zv;
      }
      if (!isFinite(z0)) { z0 = 0; z1 = 1; }
      nz = new Float32Array(n);
      var zmid = (z0 + z1) / 2, kz = kx;
      var rmax = 0;
      for (i = 0; i < n; i++) {
        var ax = xs[i], ay = ys[i], az = zs[i];
        if (ax == null || isNaN(ax) || ay == null || isNaN(ay)) continue;
        var ddx = (ax - x0) * kx + ox - 0.5;
        var ddy = (ay - y0) * ky + oy - 0.5;
        var ddz = (az == null || isNaN(az)) ? 0 : (az - zmid) * kz;
        var rr = ddx * ddx + ddy * ddy + ddz * ddz;
        if (rr > rmax) rmax = rr;
      }
      rmax = Math.sqrt(rmax) || 1;
      var shrink = 0.5 / rmax;                     // fit the inscribed sphere
      for (i = 0; i < n; i++) {
        var bx2 = xs[i], by2 = ys[i], bz2 = zs[i];
        if (bx2 == null || isNaN(bx2) || by2 == null || isNaN(by2)) continue;
        nz[i] = ((bz2 == null || isNaN(bz2)) ? 0 : (bz2 - zmid) * kz) * shrink;
      }
      // x/y shrink to match, applied in the main loop below
      space._shrink3 = shrink;
    }
    // Occupancy of a coarse lattice over the unit box, plus the centre of mass —
    // both used by clampView() to keep a panned view on actual data. A bounding
    // box cannot do that job: a UMAP fills its box very unevenly, so a view held
    // at a corner of the BOX can still show nothing at all.
    var occ = new Uint8Array(OCC * OCC), cmx = 0, cmy = 0, cnt = 0;
    var sh = nz ? space._shrink3 : 1;
    for (var j = 0; j < n; j++) {
      var a = xs[j], b = ys[j];
      if (a == null || isNaN(a) || b == null || isNaN(b)) { ok[j] = 0; continue; }
      // 3-D clouds shrink about the centre so x/y/z share one scale and the
      // whole thing sits inside the sphere that rotation is safe within.
      nx[j] = ((a - x0) * kx + ox - 0.5) * sh + 0.5;
      ny[j] = ((b - y0) * ky + oy - 0.5) * sh + 0.5;
      ok[j] = 1;
      cmx += nx[j]; cmy += ny[j]; cnt++;
      occ[occIdx(ny[j]) * OCC + occIdx(nx[j])] = 1;
    }
    // x0/y0/k/ox/oy let us map ARBITRARY data coords (e.g. image bounds) to the
    // same unit box the points use, so a background image aligns to the cells.
    // `ky` is retained separately because spatial data is normalised per axis,
    // then letterboxed inside its canvas without changing the panel geometry.
    //
    // `bx` is where the DATA actually lies inside the unit box. A 3-D cloud can
    // occupy less than [0,1] after its rotation-safe spherical fit, and clampView()
    // needs that real extent to keep a panned view on the data.
    return { nx: nx, ny: ny, nz: nz, ok: ok,
      x0: x0, y0: y0, k: kx, ky: ky, ox: ox, oy: oy,
      aspect: dw / dh,
      bx: nz
        ? { x0: 0, x1: 1, y0: 0, y1: 1 }   // rotatable: the sphere fills the box
        : { x0: ox, x1: ox + dw * kx, y0: oy, y1: oy + dh * ky },
      occ: occ, sat: occSAT(occ),
      cmx: cnt ? cmx / cnt : 0.5, cmy: cnt ? cmy / cnt : 0.5 };
  }

  // Trekker's endpoints use unrelated coordinate systems. Normalise each one
  // once, then interpolate in their shared unit box; normalising every mixed
  // raw frame would refit the cloud and visually cancel the transition.
  function transitionUnit(from, to, t) {
    var n = from.nx.length, nx = new Float32Array(n), ny = new Float32Array(n);
    var ok = new Uint8Array(n), occ = new Uint8Array(OCC * OCC);
    var cmx = 0, cmy = 0, cnt = 0;
    for (var i = 0; i < n; i++) {
      if (!from.ok[i] || !to.ok[i]) continue;
      nx[i] = from.nx[i] + (to.nx[i] - from.nx[i]) * t;
      ny[i] = from.ny[i] + (to.ny[i] - from.ny[i]) * t;
      ok[i] = 1; cmx += nx[i]; cmy += ny[i]; cnt++;
      occ[occIdx(ny[i]) * OCC + occIdx(nx[i])] = 1;
    }
    return {
      nx: nx, ny: ny, nz: null, ok: ok,
      aspect: from.aspect + (to.aspect - from.aspect) * t,
      bx: { x0: 0, x1: 1, y0: 0, y1: 1 },
      occ: occ, sat: occSAT(occ),
      cmx: cnt ? cmx / cnt : 0.5, cmy: cnt ? cmy / cnt : 0.5
    };
  }

  // Map a unit-box coord (nx, ny in [0,1]) to this panel's screen pixels,
  // applying the panel's optional zoom view (a centred sub-rectangle of the unit
  // box blown up to fill the panel). view=null is the identity — the default,
  // unchanged path. Used by the point loop, image bounds, and axis ticks alike,
  // so a zoomed panel stays internally consistent.
  function unitToScreen(p, nx, ny) {
    var v = p.view, zx, zy;
    if (v) { zx = (nx - v.cx) / v.span + 0.5; zy = (ny - v.cy) / v.span + 0.5; }
    else { zx = nx; zy = ny; }
    return [p._sox + zx * p._SX, p._soy + p._SY - zy * p._SY];
  }
  // Map a data-space (dx, dy) to this panel's screen pixels, using the same
  // transform the points use. Requires project(p) to have run.
  function dataToScreen(p, dx, dy) {
    var u = spaceById[p.spaceId] && spaceById[p.spaceId]._unit;
    if (!u || p._SX == null || p._SY == null) return null;
    var nx = (dx - u.x0) * u.k + u.ox;
    var ny = (dy - u.y0) * u.ky + u.oy;
    return unitToScreen(p, nx, ny);
  }

  // Per-cell RGB channels (0..255) + their max — the one source for both the
  // colour blend and the expressing test.
  function rgbAt(i) {
    var d = D.rgb, r = d && d.r ? d.r[i] : 0, g = d && d.g ? d.g[i] : 0,
      b = d && d.b ? d.b[i] : 0;
    return { r: r, g: g, b: b, m: Math.max(r, g, b) };
  }
  function rgbExpressing(i) { return rgbAt(i).m > RGB_MIN; }
  function rgbGpuColor(r, g, b) {
    r = r || 0; g = g || 0; b = b || 0;
    var m = Math.max(r, g, b), rr, gg, bb;
    if (m <= RGB_MIN) {
      rr = RGB_GREY_RGB[0]; gg = RGB_GREY_RGB[1]; bb = RGB_GREY_RGB[2];
    } else if (r > RGB_MIN && g > RGB_MIN && b > RGB_MIN) {
      rr = gg = bb = 0;
    } else {
      var t = m / 255;
      rr = Math.round(RGB_GREY_RGB[0] + (r / m * 255 - RGB_GREY_RGB[0]) * t);
      gg = Math.round(RGB_GREY_RGB[1] + (g / m * 255 - RGB_GREY_RGB[1]) * t);
      bb = Math.round(RGB_GREY_RGB[2] + (b / m * 255 - RGB_GREY_RGB[2]) * t);
    }
    return rr | (gg << 8) | (bb << 16) | (m > RGB_MIN ? 0x1000000 : 0);
  }

  // ---- colour of a cell under the active mode -----------------------------
  // Painting order for a CONTINUOUS colouring: ascending value, so the cells
  // carrying the signal land on top. Index order is arbitrary with respect to
  // expression, so without this a focus of high expression is at the mercy of
  // whichever low-expressing neighbours happen to come after it in the array --
  // they paint over it, and a real signal reads as absent. Categorical
  // colourings have no such order and keep the natural one.
  //
  // Cached per (data set, colouring, gene): the sort is over every cell and the
  // answer only changes when one of those does.
  var _ordD = null, _ordKey = null, _ordVal = null;
  function panelColorMode(p) {
    return p && p.colorBy ? p.colorBy : colorBy;
  }
  function quantisedOrder(vals, span) {
    var counts = new Uint32Array(span + 2), n = vals.length, i, bucket;
    for (i = 0; i < n; i++) {
      bucket = vals[i] == null || isNaN(vals[i])
        ? 0 : Math.max(0, Math.min(span, vals[i] | 0)) + 1;
      counts[bucket]++;
    }
    var offsets = new Uint32Array(counts.length), total = 0;
    for (i = 0; i < counts.length; i++) {
      offsets[i] = total;
      total += counts[i];
    }
    var order = new Uint32Array(n);
    for (i = 0; i < n; i++) {
      bucket = vals[i] == null || isNaN(vals[i])
        ? 0 : Math.max(0, Math.min(span, vals[i] | 0)) + 1;
      order[offsets[bucket]++] = i;
    }
    return order;
  }
  function paintOrder(p) {
    if (!D) return null;
    var mode = panelColorMode(p);
    var key = mode + '|' + (D.gene ? D.gene.gene : '');
    if (_ordD === D && _ordKey === key) return _ordVal;
    var vals = null, span = 255;
    if (mode === GENE_MODE && D.gene) {
      vals = D.gene.v;
    } else {
      var f = fieldForMode(mode);
      if (f && f.paintOrder !== false) { vals = f.v; span = f.scale || 255; }
    }
    // Expression fields keep full-precision values: match Seurat's ascending
    // order, with missing values first and stable ties. Quantised-only sources
    // retain their counting-sort path.
    var ord;
    if (vals && f && f.raw) {
      var raw = f.raw;
      ord = Array.from({ length: raw.length }, function (_, i) { return i; });
      ord.sort(function (a, b) {
        var av = raw[a], bv = raw[b];
        var am = av == null || isNaN(av), bm = bv == null || isNaN(bv);
        return am !== bm ? (am ? -1 : 1) : am ? a - b : (av - bv || a - b);
      });
    } else {
      ord = vals ? quantisedOrder(vals, span) : null;
    }
    _ordD = D; _ordKey = key; _ordVal = ord;
    return ord;
  }

  // ---- colour range --------------------------------------------------------
  // Fraction trimmed from EACH tail of a continuous colouring before it is
  // mapped onto viridis. Mapping the full min-max instead hands the top of the
  // scale to whichever cell happens to be the most extreme, and presses every
  // other one into the bottom few percent of the colour map, where differences
  // that matter are differences nobody can see. The trimmed values are not
  // hidden -- they saturate at the ends, and the colourbar says so.
  var colorClip = 0.01;
  var _clipD = null, _clipKey = null, _clipVal = null;
  function clipRange(p) {
    if (!D) return null;
    var mode = panelColorMode(p);
    var vals = null, span = 255, field = null;
    if (mode === GENE_MODE && D.gene) { vals = D.gene.v; span = 255; }
    else {
      field = fieldForMode(mode);
      if (field) { vals = field.v; span = field.scale || 255; }
    }
    if (!vals) return null;
    if (field && field.constantColor) return { lo: 0, hi: span };
    if (field && field.unclipped) return { lo: 0, hi: span };
    var key = mode + '|' + (D.gene ? D.gene.gene : '') + '|' + colorClip;
    if (_clipD === D && _clipKey === key) return _clipVal;
    var r;
    if (colorClip <= 0) {
      r = { lo: 0, hi: span };
    } else {
      // Histogram rather than a sort: the values arrive quantised into a known
      // number of steps, so the quantile is a running count over the bins and
      // costs one pass instead of n log n on every change.
      var bins = new Int32Array(span + 1), k = 0, i;
      for (i = 0; i < vals.length; i++) {
        var v = vals[i];
        if (v == null || isNaN(v)) continue;
        bins[Math.max(0, Math.min(span, v | 0))]++; k++;
      }
      if (!k) return (_clipD = D, _clipKey = key, _clipVal = { lo: 0, hi: span });
      var want = colorClip * k, acc = 0, lo = 0, hi = span;
      for (i = 0; i <= span; i++) { acc += bins[i]; if (acc >= want) { lo = i; break; } }
      acc = 0;
      for (i = span; i >= 0; i--) { acc += bins[i]; if (acc >= want) { hi = i; break; } }
      if (hi <= lo) { lo = 0; hi = span; }   // degenerate (constant) -- show it all
      r = { lo: lo, hi: hi };
    }
    _clipD = D; _clipKey = key; _clipVal = r;
    return r;
  }
  // Quantised value -> 0..1 across the active colour range.
  function clipT(v, span, range) {
    var r = range || _clipVal;
    if (!r) return v / span;
    if (r.hi <= r.lo) return 0;
    return Math.max(0, Math.min(1, (v - r.lo) / (r.hi - r.lo)));
  }

  function colorOf(i, p) {
    var mode = panelColorMode(p);
    if (mode === GENE_MODE) {
      if (!D.gene) return '#dcdcdc';
      return viridisCss(clipT(D.gene.v[i], 255, clipRange(p)));
    }
    if (mode === RGB_MODE) {
      var e = rgbAt(i);
      if (e.m <= RGB_MIN) return RGB_GREY;            // non-expressing → grey
      if (e.r > RGB_MIN && e.g > RGB_MIN && e.b > RGB_MIN) return '#000000';
      // Blend grey -> full-brightness hue by intensity.
      var t = e.m / 255, ch = [e.r, e.g, e.b], o = [0, 0, 0];
      for (var k = 0; k < 3; k++) {
        o[k] = Math.round(RGB_GREY_RGB[k] + (ch[k] / e.m * 255 - RGB_GREY_RGB[k]) * t);
      }
      return 'rgb(' + o[0] + ',' + o[1] + ',' + o[2] + ')';
    }
    var fld = fieldForMode(mode);
    if (fld) {
      if (fld.constantColor) return fld.constantColor;
      if (fld.colors) return cssColor(fld.colors[i], '#e6e7ea');
      var fv = fld.v[i];
      if (fv == null || isNaN(fv)) return '#e6e7ea';   // unpositioned / NA → faint
      var t = clipT(fv, fld.scale || 255, clipRange(p));
      return fld.palette ? fld.palette[Math.round(t * 255)] : viridisCss(t);
    }
    var g = catOf(mode);
    if (!g) return '#7b8794';   // nothing categorical to colour by → one colour
    var lv = g.values[i];
    if (lv < 0 || lv == null) return '#cccccc';
    return (g.colors && g.colors[lv]) || PAL[lv % PAL.length];
  }
  function visible(i, p) {
    var mode = panelColorMode(p);
    // Continuous modes never hide points (no categorical legend to toggle).
    if (mode === GENE_MODE || mode === RGB_MODE || fieldForMode(mode)) return true;
    var g = catOf(mode);
    if (!g) return true;
    return !hidden.has(g.values[i]);
  }
  // A cell is "active" when it survives the group filters AND the % subsample.
  // Inactive cells are drawn in no panel and are not selectable, matching the
  // Overview page's group-filter + "show % of cells" behaviour.
  function panelPctMask(p) {
    var space = p && spaceById[p.spaceId];
    if (singleActive || !isSpatialSpace(space)) return pctMask;
    var percentage = Number(space.builder_percentage_cells);
    if (!isFinite(percentage) || percentage >= 100) return null;
    if (!space._pctMask || space._pctMask.length !== D.n) {
      space._pctMask = percentageMask(D.n, percentage);
    }
    return space._pctMask;
  }
  function activeCell(i, p) {
    var mask = panelPctMask(p);
    if (mask && !mask[i]) return false;
    // Trekker: dissolve the least-confidently-positioned nuclei.
    if (dissolveThresh != null && D.trekker && D.trekker.conf) {
      var cf = D.trekker.conf[i];
      if (cf != null && cf < dissolveThresh) return false;
    }
    for (var gname in groupFilter) {
      if (!Object.prototype.hasOwnProperty.call(groupFilter, gname)) continue;
      var allowed = groupFilter[gname];
      if (!allowed) continue;
      var g = D.groups[gname];
      if (g && !allowed.has(g.values[i])) return false;
    }
    return true;
  }
  // Threshold for the dissolve slider: the conf value at the dissolvePct-quantile
  // of positioned cells; cells below it are hidden.
  function rebuildDissolve() {
    dissolveThresh = null;
    if (!D || !D.trekker || !D.trekker.conf || dissolvePct <= 0) return;
    var vals = D.trekker._sortedConf;
    if (!vals) {
      vals = [];
      for (var i = 0; i < D.n; i++) {
        var c = D.trekker.conf[i];
        if (c != null && !isNaN(c)) vals.push(c);
      }
      vals.sort(function (a, b) { return a - b; });
      D.trekker._sortedConf = vals;
    }
    if (!vals.length) return;
    var k = Math.floor(vals.length * dissolvePct / 100);
    dissolveThresh = vals[Math.min(k, vals.length - 1)];
  }
  // Legend-visible AND filter-active — the single gate every render/hit-test uses.
  function shown(i, p) { return visible(i, p) && activeCell(i, p); }
  // Stable per-cell value in [0, 1): the same subset persists across
  // redraws/toggles, so lowering "% of cells" never reshuffles the view.
  function cellHash(i) { return (Math.imul(i + 1, 2654435761) >>> 0) / 4294967296; }
  function percentageMask(n, percentage) {
    var mask = new Uint8Array(n), thr = percentage / 100;
    for (var i = 0; i < n; i++) mask[i] = cellHash(i) < thr ? 1 : 0;
    return mask;
  }
  function rebuildPctMask() {
    pctMask = (!D || pctShow >= 100) ? null : percentageMask(D.n, pctShow);
  }
  // After a filter / subsample change: drop now-inactive cells from the current
  // selection, then redraw. Keeps the coordinated selection consistent.
  function applyActiveChange() {
    if (sel) {
      var s = new Set();
      sel.forEach(function (i) { if (panels.some(function (p) { return p.spaceId && shown(i, p); })) s.add(i); });
      setSelection(s.size ? s : null);
    } else {
      drawAll();
    }
  }

  // ---- panel geometry + projection ----------------------------------------
  function project(p, forceCpu) {
    var sp = spaceById[p.spaceId];
    if (!sp) {
      p.sx = null; p.sy = null; p.gpuTransformOnly = false;
      return;
    }
    if (!sp._unit) sp._unit = deferredUnitOf(sp) || unitOf(sp);
    var u = sp._unit, n = D.n;
    // Axis'd spaces (the clone panel) reserve room on the left for the y-label
    // and along the bottom for the x-label. The remaining rectangle is filled:
    // linked panels deliberately use the available screen width instead of
    // centring a square canvas with unused space on both sides.
    var axed = !!sp._axisSpec;
    var padL = axed ? 36 : 10, padB = axed ? 24 : 10, padT = 10, padR = 10;
    var SX = Math.max(1, p.W - padL - padR);
    var SY = Math.max(1, p.H - padT - padB);
    var ox = padL, oy = padT;
    if (isSpatialSpace(sp) && !u.nz) {
      var aspect = Number(u.aspect), panelAspect = SX / SY;
      if (isFinite(aspect) && aspect > 0) {
        if (panelAspect > aspect) {
          var fittedWidth = SY * aspect;
          ox += (SX - fittedWidth) / 2;
          SX = fittedWidth;
        } else {
          var fittedHeight = SX / aspect;
          oy += (SY - fittedHeight) / 2;
          SY = fittedHeight;
        }
      }
    }
    p._SX = SX; p._SY = SY; p._S = Math.min(SX, SY);
    p._sox = ox; p._soy = oy;                // for dataToScreen (image bounds)
    if (!forceCpu && gpuCandidate(p)) {
      p.ok = u.ok;
      p.gpuTransformOnly = true;
      return;
    }
    if (u._pending) {
      sp._unit = unitOf(sp);
      u = sp._unit;
    }
    p.ok = u.ok;
    p.gpuTransformOnly = false;
    if (!p.sx || p.sx.length !== n) {
      p.sx = new Float32Array(n); p.sy = new Float32Array(n);
    }
    // Inline the unitToScreen transform (avoids per-cell allocation on big sets).
    var v = p.view;
    // ---- 3-D: rotate about the cloud's centre, then flatten ---------------
    // Orthographic, not perspective: a scatter is read by comparing positions,
    // and perspective makes the same distance mean different things depending on
    // where in the frame it falls. Depth is kept per point (-0.5..0.5) so the
    // draw can shade by it — with no depth sort, that shading is what carries
    // which end of the cloud is nearer.
    var rot = u.nz ? (p.rot || null) : null;
    if (!u.nz) { p.depth = null; p.rot = null; }
    if (u.nz) {
      if (!p.depth || p.depth.length !== n) p.depth = new Float32Array(n);
      if (!p._rx || p._rx.length !== n) {
        p._rx = new Float32Array(n); p._ry = new Float32Array(n);
      }
    }
    var dmin = Infinity, dmax = -Infinity;
    var cy1 = 1, sy1 = 0, cx1 = 1, sx1 = 0;
    if (rot) {
      cy1 = Math.cos(rot.ry); sy1 = Math.sin(rot.ry);
      cx1 = Math.cos(rot.rx); sx1 = Math.sin(rot.rx);
    }
    for (var i = 0; i < n; i++) {
      var ux = u.nx[i], uy = u.ny[i];
      if (u.nz) {
        var dx = ux - 0.5, dy = uy - 0.5, dz = u.nz[i];
        if (rot) {
          var x1 = dx * cy1 + dz * sy1;          // yaw about the vertical axis
          var z1 = dz * cy1 - dx * sy1;
          var y1 = dy * cx1 - z1 * sx1;          // pitch about the horizontal
          var z2 = z1 * cx1 + dy * sx1;
          ux = x1 + 0.5; uy = y1 + 0.5; p.depth[i] = z2;
        } else {
          p.depth[i] = dz;
        }
        var dv = p.depth[i];
        if (dv < dmin) dmin = dv;
        if (dv > dmax) dmax = dv;
        p._rx[i] = ux; p._ry[i] = uy;   // post-rotation unit coords (minimap)
      }
      var zx, zy;
      if (v) { zx = (ux - v.cx) / v.span + 0.5; zy = (uy - v.cy) / v.span + 0.5; }
      else { zx = ux; zy = uy; }
      p.sx[i] = ox + zx * SX;
      p.sy[i] = oy + SY - zy * SY;   // y up
    }
    // Depth is scaled to the range actually present, not to the theoretical
    // ±0.5 of the unit sphere. At most angles the cloud occupies a fraction of
    // that, and mapping against the theoretical range left the near/far size
    // difference at about 1.1x — measurable, invisible.
    if (u.nz) {
      if (isFinite(dmin) && dmax > dmin) {
        p._dmin = dmin; p._dspan = dmax - dmin;
      } else {
        // Every cell at the same depth (a constant z, or a single cell). Map
        // them to the MIDDLE of the size range: anchoring at 0 would shrink the
        // whole cloud to the "furthest" size for no reason.
        p._dmin = -0.5; p._dspan = 1;
      }
    }
  }

  function pointScreenX(p, i) {
    if (!p.gpuTransformOnly) return p.sx && p.sx[i];
    var u = spaceById[p.spaceId]._unit, x = u.nx[i], view = p.view;
    if (view) x = (x - view.cx) / view.span + 0.5;
    return p._sox + x * p._SX;
  }

  function pointScreenY(p, i) {
    if (!p.gpuTransformOnly) return p.sy && p.sy[i];
    var u = spaceById[p.spaceId]._unit, y = u.ny[i], view = p.view;
    if (view) y = (y - view.cy) / view.span + 0.5;
    return p._soy + p._SY - y * p._SY;
  }

  function ensureScreenProjection(p) {
    if (p.gpuTransformOnly) project(p, true);
  }

  // A committed brush is part of the data conversation, not a decoration at a
  // particular canvas pixel. Keep it in unit coordinates so a Focus reflow (or
  // any other resize) sends the outline through the exact same projection as
  // the cells it selected. `p.lasso` remains screen-space while the pointer is
  // moving, because hit testing happens against the already-projected cells.
  function lassoToUnit(p, lasso) {
    if (!lasso || !p._SX || !p._SY) return null;
    return CBGeom.screenToUnit(p.view, {
      x: p._sox, y: p._soy, width: p._SX, height: p._SY
    }, lasso);
  }
  function lassoToScreen(p) {
    var lasso = p.lassoData || p.lasso;
    if (!lasso || !p._SX || !p._SY) return lasso;
    if (!p.lassoData) return lasso;
    return CBGeom.unitToScreen(p.view, {
      x: p._sox, y: p._soy, width: p._SX, height: p._SY
    }, lasso);
  }

  // The rotation project() applies, as a function. Anything that has to land
  // in the same place as the cells — the group labels, the axis tripod — turns
  // through here. project() inlines the identical arithmetic on its per-cell
  // path, where a call per point would cost more than the duplication does; the
  // two MUST stay in step.
  function rotUnit(rot, dx, dy, dz) {
    if (!rot) return [dx, dy, dz];
    var cy = Math.cos(rot.ry), sy = Math.sin(rot.ry);
    var cx = Math.cos(rot.rx), sx = Math.sin(rot.rx);
    var x1 = dx * cy + dz * sy;
    var z1 = dz * cy - dx * sy;
    return [x1, dy * cx - z1 * sx, z1 * cx + dy * sx];
  }

  // Is this panel showing an embedding that can be turned?
  function panelIs3D(p) {
    var sp = p && spaceById[p.spaceId];
    return !!(sp && sp.z);
  }
  function projectionId(name) { return 'projection::' + name; }
  function isProjectionSpace(sp) { return !!(sp && sp._projectionName); }
  // Is there a panel one can actually select on?
  function anyFlatPanel() {
    return panels.some(function (p) { return p.spaceId && !panelIs3D(p); });
  }

  // The cursor is the only warning of what a drag is about to do. A rotatable
  // panel always drags-to-turn, whatever the toolbar says, so it always shows
  // the grab cursor rather than the crosshair that promises a selection.
  function syncCursors() {
    panels.forEach(function (p) {
      if (!p.canvas) return;
      p.canvas.classList.toggle('cv-pannable',
        panelIs3D(p) || selectMode === 'pan' || selectMode === 'orbit');
    });
  }

  // Match the per-panel toolbar to what the panel can actually do. Called from
  // layoutPanels (spaces assigned) AND from setProjection (a panel keeps its
  // space but that space changes dimensionality) — the second is not reachable
  // from the first, which is how a flat projection once kept the rotate tool.
  function syncOrbitButtons() {
    panels.forEach(function (p) {
      var is3d = panelIs3D(p);
      var ob = p.pane && p.pane.querySelector('.cv-orbit-btn');
      if (ob) ob.style.display = is3d ? '' : 'none';
      // Box and lasso come off a rotatable panel: selection there is keyed on
      // screen position, which stops being a faithful key once the cloud has
      // depth (see wireBrush). The buttons go with the behaviour — offering a
      // tool that silently does something else is worse than not offering it.
      ['box', 'lasso'].forEach(function (act) {
        var b = p.pane &&
          p.pane.querySelector('.cv-tbtn[data-act="' + act + '"]');
        if (b) b.style.display = is3d ? 'none' : '';
      });
    });
    // Never leave the toolbar in a mode none of the panels offer.
    var flat = anyFlatPanel();
    if (selectMode === 'orbit' && !panels.some(panelIs3D)) {
      selectMode = 'lasso'; syncModeButtons();
    } else if (!flat && (selectMode === 'box' || selectMode === 'lasso')) {
      selectMode = 'orbit'; syncModeButtons();
    }
    syncCursors();
  }
  // Size one panel to the rectangle assigned by resizeAll. Below 420px the
  // header cannot hold title + toolbar, so the toolbar floats vertically.
  function resizePanel(p, width, height) {
    var dpr = window.devicePixelRatio || 1;
    p.W = width; p.H = height;
    resizePanelLayers(p, width, height, dpr);
    if (p.pane) {
      p.pane.classList.toggle('cv-narrow', width < 420);
      p.pane.classList.toggle('cv-square-plot', keepPlotsSquare);
    }
    project(p);
  }

  function resizeCanvasLayer(canvas, context, width, height, dpr) {
    if (!canvas) return;
    canvas.width = width * dpr;
    canvas.height = height * dpr;
    canvas.style.width = width + 'px';
    canvas.style.height = height + 'px';
    if (context) context.setTransform(dpr, 0, 0, dpr, 0, 0);
  }

  function resizePanelLayers(p, width, height, dpr) {
    resizeCanvasLayer(p.canvas, p.ctx, width, height, dpr);
    resizeCanvasLayer(p.underlayCanvas, p.underlayCtx, width, height, dpr);
    resizeCanvasLayer(p.interactionCanvas, p.interactionCtx, width, height, dpr);
    if (p.gpu) p.gpu.resize(width, height, dpr);
  }

  function syncPanelOverlay(p) {
    var gpuVisible = p.gpuCanvas && p.gpuCanvas.style.display !== 'none';
    p.canvas.classList.toggle('cv-layer-overlay', !!p.underlayCanvas || gpuVisible);
  }

  function setGpuLayerVisible(p, visible) {
    if (!p.gpuCanvas) return;
    p.gpuCanvas.style.display = visible ? 'block' : 'none';
    syncPanelOverlay(p);
  }

  function clearPanelLayers(p) {
    p.ctx.clearRect(0, 0, p.W, p.H);
    if (p.underlayCtx) p.underlayCtx.clearRect(0, 0, p.W, p.H);
    if (p.interactionCtx) p.interactionCtx.clearRect(0, 0, p.W, p.H);
  }

  function exportPanelLayers(context, panel, x, y, width, height) {
    [panel.underlayCanvas,
      panel.gpuCanvas && panel.gpuCanvas.style.display !== 'none'
        ? panel.gpuCanvas : null,
      panel.canvas,
      panel.interactionCanvas].forEach(function (canvas) {
      if (canvas) context.drawImage(canvas, x, y, width, height);
    });
  }

  // Draw the histology image behind the points of the spatial panel. The image's
  // data-space bounds are mapped to screen with the SAME transform as the cells,
  // so it aligns; opacity/offset/scale/flip/rotate then adjust it on top.
  function rotateDataPoint(x, y, degrees) {
    if (!degrees) return [x, y];
    var theta = degrees * Math.PI / 180;
    return [x * Math.cos(theta) - y * Math.sin(theta),
      x * Math.sin(theta) + y * Math.cos(theta)];
  }
  function drawImage(p, context) {
    if (!singleActive && !linkedBackgroundsOn) return;
    var sp = spaceById[p.spaceId];
    var cimg = currentImage(sp);
    var state = sp && sp._imgState;
    if (!sp || !cimg || !sp._imgEl || !sp._imgReady || !state || !state.show) return;
    var b = cimg.bounds;
    if (!b) return;
    // Embedded images already carry their saved calibration in the pixels.
    state = imageRenderState(cimg, state);
    // Rotate the image in DATA space about the origin, exactly as R rotates the
    // cell coordinates (rotateSpatialCoordinates). Rotating on screen about the
    // image centre instead moves the image away from the cells it belongs to.
    var tlData = rotateDataPoint(b.xmin, b.ymax, state.rotate);
    var trData = rotateDataPoint(b.xmax, b.ymax, state.rotate);
    var blData = rotateDataPoint(b.xmin, b.ymin, state.rotate);
    var centerData = rotateDataPoint(
      (Number(b.xmin) + Number(b.xmax)) / 2,
      (Number(b.ymin) + Number(b.ymax)) / 2,
      state.rotate
    );
    var tl = dataToScreen(p, tlData[0], tlData[1]);
    var tr = dataToScreen(p, trData[0], trData[1]);
    var bl = dataToScreen(p, blData[0], blData[1]);
    var center = dataToScreen(p, centerData[0], centerData[1]);
    if (!tl || !tr || !bl || !center) return;
    // Convert the DATA-unit offset to screen pixels by mapping two points through
    // the same projection the cells use (handles scale + the y-axis inversion).
    var o1 = dataToScreen(p, centerData[0] + state.offsetX,
      centerData[1] + state.offsetY);
    var offSX = o1 ? (o1[0] - center[0]) : 0;
    var offSY = o1 ? (o1[1] - center[1]) : 0;
    var c = context || p.ctx;
    c.save();
    c.globalAlpha = state.opacity;
    c.translate(center[0] + offSX, center[1] + offSY);
    c.scale(state.scaleX * (state.flipX ? -1 : 1),
      state.scaleY * (state.flipY ? -1 : 1));
    c.transform(tr[0] - tl[0], tr[1] - tl[1],
      bl[0] - tl[0], bl[1] - tl[1],
      tl[0] - center[0], tl[1] - center[1]);
    c.drawImage(sp._imgEl, 0, 0, 1, 1);
    c.restore();
    c.globalAlpha = 1;
  }

  function drawTrajectory(p, context) {
    var sp = spaceById[p.spaceId];
    if (!sp || !sp.trajectory || !sp.edges) return;
    var c = context || p.ctx;
    c.save();
    c.globalAlpha = 0.72;
    c.strokeStyle = '#1f2937';
    c.lineWidth = 1.5;
    c.lineCap = 'round';
    c.beginPath();
    var drawEdge = function (x0, y0, x1, y1) {
      var source = dataToScreen(p, Number(x0), Number(y0));
      var target = dataToScreen(p, Number(x1), Number(y1));
      if (!source || !target) return;
      c.moveTo(source[0], source[1]); c.lineTo(target[0], target[1]);
    };
    if (Array.isArray(sp.edges)) {
      sp.edges.forEach(function (edge) {
        if (edge && edge.length >= 4) drawEdge(edge[0], edge[1], edge[2], edge[3]);
      });
    } else if (sp.edges.from && sp.edges.to) {
      var edgeCount = Math.min(sp.edges.from.length, sp.edges.to.length);
      for (var edgeIndex = 0; edgeIndex < edgeCount; edgeIndex++) {
        // R graph endpoints are one-based. Resolve them against the already
        // aligned point coordinates in the browser instead of transferring four
        // repeated floating-point coordinates per edge.
        var from = Number(sp.edges.from[edgeIndex]) - 1;
        var to = Number(sp.edges.to[edgeIndex]) - 1;
        if (from < 0 || to < 0 || from >= sp.x.length || to >= sp.x.length) continue;
        drawEdge(sp.x[from], sp.y[from], sp.x[to], sp.y[to]);
      }
    } else {
      var n = sp.edges.x0 && sp.edges.x0.length || 0;
      for (var i = 0; i < n; i++) {
        drawEdge(sp.edges.x0[i], sp.edges.y0[i], sp.edges.x1[i], sp.edges.y1[i]);
      }
    }
    c.stroke();
    c.restore();
  }

  function drawHulls(p, context) {
    var sp = spaceById[p.spaceId];
    if (!sp || !sp.hulls || !sp.hulls.length) return;
    var c = context || p.ctx;
    sp.hulls.forEach(function (hull) {
      if (!hull || hull.x.length < 3 || hull.x.length !== hull.y.length) return;
      c.save(); c.beginPath();
      hull.x.forEach(function (x, index) {
        var point = dataToScreen(p, Number(x), Number(hull.y[index]));
        if (!point) return;
        if (index) c.lineTo(point[0], point[1]);
        else c.moveTo(point[0], point[1]);
      });
      c.closePath();
      c.fillStyle = cssColor(hull.color, '#7b8794'); c.globalAlpha = 0.08; c.fill();
      c.globalAlpha = 0.7; c.lineWidth = 1.5; c.strokeStyle = c.fillStyle; c.stroke();
      c.restore();
    });
  }

  // Draw a labelled L-shaped frame for an abstract space (the clone panel).
  // Geometric spaces (umap, spatial) carry no _axisSpec and stay axis-free, so
  // their coordinates read as "layout, not measurement" — the usual convention.
  function drawAxes(p) {
    var sp = spaceById[p.spaceId], spec = sp && sp._axisSpec;
    if (!spec || p._SX == null || p._SY == null) return;
    var u = sp._unit; if (!u) return;
    var c = p.ctx, SX = p._SX, SY = p._SY;
    var x0 = p._sox, yt = p._soy, x1 = p._sox + SX, yb = p._soy + SY;
    var dataToY = function (dy) {
      var ny = (dy - u.y0) * u.ky + u.oy;
      var zy = p.view ? ((ny - p.view.cy) / p.view.span + 0.5) : ny;
      return p._soy + SY - zy * SY;
    };
    c.save();
    c.font = "10px -apple-system, 'Segoe UI', Roboto, sans-serif";
    // expansion-tier bands: faint separator + tier label per band
    if (spec.bands) {
      spec.bands.forEach(function (bd, bi) {
        if (bi > 0) {
          c.strokeStyle = '#eef0f3'; c.lineWidth = 1;
          var ys = dataToY(bd.lo);
          c.beginPath(); c.moveTo(x0, ys); c.lineTo(x1, ys); c.stroke();
        }
        // white chip behind the label so it stays legible over dense points
        var ly = dataToY(bd.mid), lw = c.measureText(bd.label).width;
        c.fillStyle = 'rgba(255,255,255,0.82)';
        c.fillRect(x0 + 3, ly - 7, lw + 6, 14);
        c.fillStyle = '#6b7280'; c.textAlign = 'left'; c.textBaseline = 'middle';
        c.fillText(bd.label, x0 + 6, ly);
      });
    }
    // L-shaped frame
    c.strokeStyle = '#d5dae1'; c.lineWidth = 1;
    c.beginPath(); c.moveTo(x0, yt); c.lineTo(x0, yb); c.lineTo(x1, yb); c.stroke();
    // x-axis label under the bottom edge
    c.fillStyle = '#6b7280'; c.textAlign = 'center'; c.textBaseline = 'top';
    c.fillText(spec.xlab, (x0 + x1) / 2, yb + 9);
    // y-axis label rotated up the left edge
    c.save();
    c.translate(x0 - 14, (yt + yb) / 2); c.rotate(-Math.PI / 2);
    c.textAlign = 'center'; c.textBaseline = 'bottom';
    c.fillText(spec.ylab, 0, 0);
    c.restore();
    c.restore();
  }

  // Paint one cell dot at the given alpha, coloured by the active mode.
  // ---- group labels --------------------------------------------------------
  // Each level's median position, drawn on the panel so a cluster map does not
  // need to be read only by cross-referencing the legend.
  //
  // Cached per (space, colouring) in SPACE units, so panning and zooming reuse
  // it — a median over n cells must never run on the drag-redraw path. The
  // positions deliberately ignore the group filters: a label that jumps every
  // time a filter changes is worse than one that stays where the group is.
  var labelsOn = false;
  var _lblCache = { d: null };
  function middleValue(values) {
    if (!values.length) return null;
    var target = values.length >> 1, left = 0, right = values.length - 1;
    while (left < right) {
      var pivot = values[(left + right) >> 1], i = left, j = right;
      while (i <= j) {
        while (values[i] < pivot) i++;
        while (values[j] > pivot) j--;
        if (i <= j) {
          var value = values[i]; values[i] = values[j]; values[j] = value;
          i++; j--;
        }
      }
      if (target <= j) right = j;
      else if (target >= i) left = i;
      else break;
    }
    return values[target];
  }
  function groupLabelsFor(p) {
    var mode = panelColorMode(p), g = catOf(mode); if (!g) return null;
    var sp = spaceById[p.spaceId], u = sp && sp._unit;
    if (!u) return null;
    var key = p.spaceId + '|' + mode;
    if (_lblCache.d !== D) _lblCache = { d: D };
    // The cached medians belong to ONE unit normalisation; switching projection,
    // spatial sample or clonal layout rebuilds `_unit`, which must invalidate
    // them (identity check, so no extra bookkeeping at those call sites).
    var hit = _lblCache[key];
    if (hit && hit.u === u) return hit.out;
    var nlev = g.levels.length, li;
    // Exact per-level medians retain two boxed coordinate arrays per cell. On a
    // million-cell atlas that allocation dominates first paint even though a
    // label only needs one stable anchor. Approximate the same component-wise
    // median on a fine fixed histogram: robust to outliers, deterministic, and
    // bounded by levels x 256 instead of cells x coordinates.
    if (D.n >= 200000) {
      var LABEL_BINS = 256;
      var histX = new Uint32Array(nlev * LABEL_BINS);
      var histY = new Uint32Array(nlev * LABEL_BINS);
      var histZ = u.nz ? new Uint32Array(nlev * LABEL_BINS) : null;
      var count = new Uint32Array(nlev);
      for (var at = 0; at < D.n; at++) {
        if (!u.ok[at]) continue;
        var level = g.values[at];
        if (level == null || level < 0 || level >= nlev) continue;
        var offset = level * LABEL_BINS;
        var bx = Math.max(0, Math.min(LABEL_BINS - 1,
          Math.floor(u.nx[at] * LABEL_BINS)));
        var by = Math.max(0, Math.min(LABEL_BINS - 1,
          Math.floor(u.ny[at] * LABEL_BINS)));
        histX[offset + bx]++;
        histY[offset + by]++;
        if (histZ) {
          var bz = Math.max(0, Math.min(LABEL_BINS - 1,
            Math.floor((u.nz[at] + 0.5) * LABEL_BINS)));
          histZ[offset + bz]++;
        }
        count[level]++;
      }
      var histogramMiddle = function (histogram, offset, total) {
        var target = Math.floor(total / 2) + 1, cumulative = 0;
        for (var bin = 0; bin < LABEL_BINS; bin++) {
          cumulative += histogram[offset + bin];
          if (cumulative >= target) return (bin + 0.5) / LABEL_BINS;
        }
        return 0.5;
      };
      var centres = [];
      for (li = 0; li < nlev; li++) {
        if (!count[li]) continue;
        offset = li * LABEL_BINS;
        centres.push({
          li: li,
          nx: histogramMiddle(histX, offset, count[li]),
          ny: histogramMiddle(histY, offset, count[li]),
          nz: histZ
            ? histogramMiddle(histZ, offset, count[li]) - 0.5 : 0,
          text: String(g.levels[li])
        });
      }
      _lblCache[key] = { u: u, out: centres };
      return centres;
    }
    var xs = [], ys = [], zs = [];
    for (li = 0; li < nlev; li++) { xs.push([]); ys.push([]); zs.push([]); }
    for (var i = 0; i < D.n; i++) {
      if (!u.ok[i]) continue;
      var lv = g.values[i];
      if (lv == null || lv < 0 || lv >= nlev) continue;
      xs[lv].push(u.nx[i]); ys[lv].push(u.ny[i]);
      if (u.nz) zs[lv].push(u.nz[i]);
    }
    var out = [];
    for (li = 0; li < nlev; li++) {
      var mx = middleValue(xs[li]);
      if (mx == null) continue;
      // The THIRD component is cached too, so a rotation only has to turn one
      // point per level instead of re-deriving a median over every cell. Cached
      // unrotated, since the cache must survive the rotation changing.
      out.push({ li: li, nx: mx, ny: middleValue(ys[li]),
        nz: u.nz ? (middleValue(zs[li]) || 0) : 0, text: String(g.levels[li]) });
    }
    _lblCache[key] = { u: u, out: out };
    return out;
  }
  // ---- 3-D axis tripod -----------------------------------------------------
  // Three arms from the cloud's centre, named after the projection's own
  // columns. Without them a turned cloud has no frame of reference at all: the
  // shape moves, nothing says which way it went, and two similar angles are
  // indistinguishable. Drawn faintly and over the cells — under them it would
  // vanish into a dense cloud, which is exactly when orientation is hardest.
  var AXIS_LEN = 0.42;   // just inside the sphere the cloud is fitted to
  function drawAxes3D(p) {
    var sp = spaceById[p.spaceId], u = sp && sp._unit;
    if (!u || !u.nz || p.W < 260) return;
    var c = p.ctx, SX = p._SX, SY = p._SY, ox = p._sox, oy = p._soy,
      v = p.view;
    if (!SX || !SY) return;
    var names = sp.axes || ['dim 1', 'dim 2', 'dim 3'];
    var arms = [[AXIS_LEN, 0, 0], [0, AXIS_LEN, 0], [0, 0, AXIS_LEN]];
    var toScreen = function (dx, dy) {
      var zx = v ? (dx + 0.5 - v.cx) / v.span + 0.5 : dx + 0.5;
      var zy = v ? (dy + 0.5 - v.cy) / v.span + 0.5 : dy + 0.5;
      return [ox + zx * SX, oy + SY - zy * SY];
    };
    var o0 = toScreen(0, 0);
    c.save();
    c.font = '600 10px system-ui, -apple-system, "Segoe UI", sans-serif';
    c.textAlign = 'center'; c.textBaseline = 'middle';
    for (var k = 0; k < 3; k++) {
      var r = rotUnit(p.rot, arms[k][0], arms[k][1], arms[k][2]);
      var e = toScreen(r[0], r[1]);
      // An arm pointing away from the viewer is drawn fainter, so the tripod
      // itself shows which way the cloud is facing.
      var near = 0.5 + r[2] / (AXIS_LEN * 2);
      if (near < 0) near = 0; else if (near > 1) near = 1;
      c.globalAlpha = 0.35 + 0.5 * near;
      c.strokeStyle = '#7c8595'; c.lineWidth = 1.5;
      c.beginPath(); c.moveTo(o0[0], o0[1]); c.lineTo(e[0], e[1]); c.stroke();
      c.fillStyle = '#7c8595';
      c.beginPath(); c.arc(e[0], e[1], 2.2, 0, 6.2832); c.fill();
      // An arm pointing straight at the viewer collapses to a dot; its label
      // would then sit on the origin, on top of the other two. Below a few
      // pixels of projected length there is no direction left to name, so the
      // name is dropped and the dot alone marks it.
      var len = Math.hypot(e[0] - o0[0], e[1] - o0[1]);
      if (len < 14) continue;
      var t = String(names[k] || ('dim ' + (k + 1)));
      if (t.length > 12) t = t.slice(0, 11) + '…';
      var w = c.measureText(t).width;
      // push the label a little further out, so it clears the arm's own dot
      var ux2 = (e[0] - o0[0]) / len, uy2 = (e[1] - o0[1]) / len;
      var lx = e[0] + ux2 * 9, ly = e[1] + uy2 * 9;
      c.globalAlpha = 0.6 + 0.4 * near;
      c.fillStyle = 'rgba(255,255,255,.86)';
      c.fillRect(lx - w / 2 - 3, ly - 7, w + 6, 14);
      c.fillStyle = '#4a5261';
      c.fillText(t, lx, ly);
    }
    c.restore();
  }

  function drawGroupLabels(p) {
    // Below ~260px cluster names crowd the data.
    if (!labelsOn || p.W < 260) return;
    var L = groupLabelsFor(p); if (!L || !L.length) return;
    var c = p.ctx, v = p.view, SX = p._SX, SY = p._SY,
      ox = p._sox, oy = p._soy;
    if (!SX || !SY) return;
    c.save();
    c.globalAlpha = 1;
    var authorDisplay = D.display_defaults || {};
    var labelSize = Math.max(8, Math.min(24, Number(authorDisplay.label_size) || 11));
    var labelFamily = ['Arial', 'Georgia'].indexOf(authorDisplay.font_family) >= 0
      ? authorDisplay.font_family : 'system-ui, -apple-system, "Segoe UI", sans-serif';
    c.font = '600 ' + labelSize + 'px ' + labelFamily;
    c.textAlign = 'center'; c.textBaseline = 'middle';
    var u3 = spaceById[p.spaceId] && spaceById[p.spaceId]._unit;
    var rot = (u3 && u3.nz) ? p.rot : null;
    L.forEach(function (o) {
      if (hidden.has(o.li)) return;
      var ux = o.nx, uy = o.ny;
      if (rot) {
        // Turn the cached centre with the cloud. Without this the labels sat
        // still while the cells moved under them — pinned to where each group
        // used to be, which is worse than no label at all.
        var rp = rotUnit(rot, ux - 0.5, uy - 0.5, o.nz);
        ux = rp[0] + 0.5; uy = rp[1] + 0.5;
      }
      var zx = v ? (ux - v.cx) / v.span + 0.5 : ux;
      var zy = v ? (uy - v.cy) / v.span + 0.5 : uy;
      var x = ox + zx * SX, y = oy + SY - zy * SY;
      if (x < ox || x > ox + SX || y < oy || y > oy + SY) return;
      var t = o.text.length > 18 ? o.text.slice(0, 17) + '…' : o.text;
      var w = c.measureText(t).width;
      // Keep larger author-selected labels inside the canvas at cluster edges.
      x = Math.max(w / 2 + 6, Math.min(p.W - w / 2 - 6, x));
      y = Math.max((labelSize + 5) / 2 + 2,
        Math.min(p.H - (labelSize + 5) / 2 - 2, y));
      c.fillStyle = '#1c1c1e';
      c.fillText(t, x, y);
    });
    c.restore();
  }

  // ---- minimap -------------------------------------------------------------
  // Once a panel is zoomed or panned, the view alone no longer says where it
  // sits in the whole space. A coarse thumbnail with a frame around the visible
  // part answers that, and nothing more — it is read-only, and deliberately
  // rough (a fixed sample budget, one flat colour) because "roughly where" is
  // the entire question.
  //
  // The dots are rendered ONCE per space normalisation into an offscreen canvas
  // and then blitted. Panning and zooming cannot change them — only the frame
  // moves — so redrawing them per frame would be pure waste on the drag path.
  var MINI = 84, MINI_PAD = 5, MINI_DOTS = 2600;
  function buildMiniBg(p, u) {
    var off = document.createElement('canvas');
    var dpr = window.devicePixelRatio || 1;
    off.width = MINI * dpr; off.height = MINI * dpr;
    var c = off.getContext('2d');
    c.setTransform(dpr, 0, 0, dpr, 0, 0);
    var S = MINI - MINI_PAD * 2;
    var step = Math.max(1, Math.floor(D.n / MINI_DOTS));
    // A rotated cloud must be thumbnailed at its CURRENT angle, or the frame
    // would sit over a shape that is no longer on screen.
    var mx = (u.nz && p._rx) ? p._rx : u.nx;
    var my = (u.nz && p._ry) ? p._ry : u.ny;
    c.fillStyle = '#9aa3b0';
    for (var i = 0; i < D.n; i += step) {
      if (!u.ok[i]) continue;
      c.fillRect(MINI_PAD + mx[i] * S - 0.6,
        MINI_PAD + S - my[i] * S - 0.6, 1.2, 1.2);
    }
    p.miniUnit = u;
    return off;
  }
  function drawMinimap(p) {
    if (!p.mini || !p.mctx) return;
    var sp = spaceById[p.spaceId], u = sp && sp._unit;
    var on = !!(p.view && D && p.ok && u);
    p.mini.classList.toggle('is-on', on);
    if (!on) return;
    if (!p.miniBg || p.miniUnit !== u) p.miniBg = buildMiniBg(p, u);
    var c = p.mctx, S = MINI - MINI_PAD * 2, v = p.view;
    c.clearRect(0, 0, MINI, MINI);
    c.drawImage(p.miniBg, 0, 0, MINI, MINI);
    // The frame, in the same padded unit box as the dots. It can extend past the
    // edge when the view runs off the data; the canvas clips it, which reads
    // correctly — part of what is on screen is outside the space.
    var bounds = CBGeom.viewBounds(v);
    var x = MINI_PAD + bounds.x0 * S;
    var y = MINI_PAD + (1 - bounds.y1) * S;
    var w = (bounds.x1 - bounds.x0) * S;
    c.fillStyle = 'rgba(249,115,22,.14)';
    c.fillRect(x, y, w, w);
    c.strokeStyle = '#f97316'; c.lineWidth = 1.25;
    c.strokeRect(x + 0.5, y + 0.5, w - 1, w - 1);
  }

  // Radius of one point. In a rotatable space it also carries depth: nearer
  // points are drawn larger. Size rather than opacity, because the batched draw
  // path fills one path per COLOUR — a path can hold arcs of different radii,
  // but not of different alphas, so shading by opacity would cost the batching
  // exactly where it matters most.
  function pointSizeOf(p) {
    var sp = spaceById[p.spaceId];
    if ((!pointSizeEdited || !singleActive) && sp && sp.builder_point_size != null) {
      return Math.max(1, Math.min(20, Number(sp.builder_point_size)));
    }
    return ps;
  }
  function pointOpacityOf(p) {
    var sp = spaceById[p.spaceId];
    if ((!pointOpacityEdited || !singleActive) && sp && sp.builder_point_opacity != null) {
      return Math.max(0, Math.min(1, Number(sp.builder_point_opacity)));
    }
    return pointOpacity;
  }
  function radiusOf(p, i) {
    var space = spaceById[p.spaceId];
    var pointSizes = space && space.pointSizes;
    var pointSize = p._renderPointSize == null
      ? pointSizeOf(p) : p._renderPointSize;
    if (pointSizes && Number(pointSizes[i]) > 0) pointSize = Number(pointSizes[i]);
    var radius = pointSize / 2;
    if (!p.depth) return radius;
    var t = (p.depth[i] - p._dmin) / p._dspan;   // 0 = furthest, 1 = nearest
    if (t < 0) t = 0; else if (t > 1) t = 1;
    return radius * (0.5 + 0.85 * t);
  }

  function paintCell(p, i, alpha, border) {
    var c = p.ctx;
    c.globalAlpha = alpha;
    c.fillStyle = colorOf(i, p);
    c.beginPath(); c.arc(p.sx[i], p.sy[i], radiusOf(p, i), 0, 6.2832); c.fill();
    if (border) {
      c.strokeStyle = border.color; c.lineWidth = border.width; c.stroke();
    }
  }

  var allShownData = null, allShownMask = null;
  function shownState(p) {
    var filtered = panelPctMask(p) || dissolveThresh != null || hidden.size;
    if (!filtered) {
      for (var name in groupFilter) {
        if (groupFilter[name]) { filtered = true; break; }
      }
    }
    if (!filtered) {
      if (allShownData !== D) {
        allShownData = D;
        allShownMask = new Uint8Array(D.n);
        allShownMask.fill(1);
      }
      return { mask: allShownMask, count: D.n };
    }
    var mask = new Uint8Array(D.n), count = 0;
    for (var i = 0; i < D.n; i++) {
      if (shown(i, p)) { mask[i] = 1; count++; }
    }
    return { mask: mask, count: count };
  }

  var GPU_MIN_CELLS = 20000;
  var gpuColorCache = new Map();
  function gpuColor(value) {
    if (gpuColorCache.has(value)) return gpuColorCache.get(value);
    var original = value;
    value = cssColor(value, '#888888');
    if (gpuColorCache.has(value)) {
      var cached = gpuColorCache.get(value);
      gpuColorCache.set(original, cached);
      return cached;
    }
    var rgba;
    if (_colCtx) {
      _colCtx.clearRect(0, 0, 1, 1);
      _colCtx.globalAlpha = 1;
      _colCtx.fillStyle = value;
      _colCtx.fillRect(0, 0, 1, 1);
      rgba = Array.prototype.slice.call(_colCtx.getImageData(0, 0, 1, 1).data);
    } else {
      rgba = rgbChannels(value).concat(255);
    }
    gpuColorCache.set(value, rgba);
    gpuColorCache.set(original, rgba);
    return rgba;
  }

  function gpuFilterKey() {
    var groups = Object.keys(groupFilter).sort().map(function (name) {
      var values = groupFilter[name];
      return name + ':' + (values ? Array.from(values).sort().join(',') : '');
    }).join('|');
    return Array.from(hidden).sort().join(',') + '|' + groups;
  }

  function gpuDataState(p) {
    var mode = panelColorMode(p), field = fieldForMode(mode), group = catOf(mode);
    return {
      dataset: D.dataset_fingerprint || D.cell_fingerprint || D.dataset_id || D,
      unit: spaceById[p.spaceId] && spaceById[p.spaceId]._unit,
      mode: mode,
      gene: D.gene && D.gene.v,
      rgbR: D.rgb && D.rgb.r,
      rgbG: D.rgb && D.rgb.g,
      rgbB: D.rgb && D.rgb.b,
      fieldValues: field && field.v,
      fieldColors: field && field.colors && field.colors.join(','),
      fieldPalette: field && field.palette && field.palette.join(','),
      fieldConstantColor: field && field.constantColor,
      groupValues: group && group.values,
      groupColors: group && group.colors && group.colors.join(','),
      pctMask: panelPctMask(p),
      dissolve: dissolveThresh,
      filter: gpuFilterKey(),
      selection: sel,
      niche: nicheSet,
      opacity: pointOpacityOf(p),
      clip: colorClip
    };
  }

  function sameGpuDataState(a, b) {
    if (!a || !b) return false;
    return Object.keys(a).every(function (key) { return a[key] === b[key]; });
  }

  function gpuCandidate(p) {
    if (!gpuAcceleration || !p.gpu || !D || D.n < GPU_MIN_CELLS) return false;
    var space = spaceById[p.spaceId], unit = space && space._unit;
    // Images, hulls and trajectories live on the CPU underlay; axes, labels and
    // interaction marks live on the CPU overlay. Only features that change the
    // point primitive itself still require the Canvas2D path.
    return !!(space && unit && !unit.nz && !space.pointSizes);
  }
  function gpuEligible(p) {
    return gpuCandidate(p) && p.gpu.isReady();
  }

  function hideGpu(p) {
    if (!p.gpuCanvas) return;
    setGpuLayerVisible(p, false);
    try { if (p.gpu) p.gpu.clear(); } catch (error) { /* Canvas fallback */ }
  }

  function disableGpu(p) {
    hideGpu(p);
    p.gpu = null;
    p.gpuTransformOnly = false;
    p.gpuData = null;
    p.gpuDataState = null;
    if (D && p.spaceId) project(p, true);
  }

  function replaceGpuWithWebGl(p, renderer) {
    if (!p.gpuCanvas || p.gpu !== renderer ||
        !window.CerebroPointRenderer ||
        !window.CerebroPointRenderer.createWebGl) return false;
    var stats = renderer.stats ? renderer.stats() : {};
    if (stats.backend === 'webgl2') return false;
    var previous = p.gpuCanvas;
    var canvas = document.createElement('canvas');
    canvas.className = 'cv-gpu-layer';
    canvas.style.display = 'none';
    canvas.setAttribute('aria-hidden', 'true');
    if (!previous.parentNode) return false;
    previous.parentNode.replaceChild(canvas, previous);
    try {
      p.gpuCanvas = canvas;
      p.gpu = window.CerebroPointRenderer.createWebGl(canvas);
      canvas._cerebroPointRenderer = p.gpu;
      p.gpu.resize(p.W || canvas.clientWidth || 1,
        p.H || canvas.clientHeight || 1, window.devicePixelRatio || 1);
      return true;
    } catch (error) {
      p.gpuCanvas = canvas;
      p.gpu = null;
      return false;
    }
  }

  function watchGpu(p, renderer) {
    renderer.failed.then(function () {
      if (p.gpu !== renderer) return;
      if (replaceGpuWithWebGl(p, renderer)) {
        watchGpu(p, p.gpu);
        if (D && p.spaceId) draw(p);
        return;
      }
      disableGpu(p);
      if (D && p.spaceId) draw(p);
    });
    renderer.ready.then(function () {
      if (p.gpu !== renderer) return;
      if (D && p.spaceId) draw(p);
    }).catch(function () {
      if (p.gpu !== renderer) return;
      if (replaceGpuWithWebGl(p, renderer)) {
        watchGpu(p, p.gpu);
        if (D && p.spaceId) draw(p);
        return;
      }
      disableGpu(p);
      if (D && p.spaceId) draw(p);
    });
  }

  function attachGpu(p) {
    if (!gpuAcceleration || p.gpu) return;
    if (!window.CerebroPointRenderer || !window.CerebroPointRenderer.create) return;
    if (p.gpuCanvas) p.gpuCanvas.remove();
    var canvas = document.createElement('canvas');
    canvas.className = 'cv-gpu-layer';
    canvas.style.display = 'none';
    canvas.setAttribute('aria-hidden', 'true');
    p.canvas.parentNode.insertBefore(canvas, p.canvas.nextSibling);
    try {
      p.gpuCanvas = canvas;
      p.gpu = window.CerebroPointRenderer.create(canvas);
      canvas._cerebroPointRenderer = p.gpu;
      if (p.W && p.H) p.gpu.resize(p.W, p.H, window.devicePixelRatio || 1);
      var renderer = p.gpu;
      watchGpu(p, renderer);
    } catch (error) {
      canvas.remove();
      p.gpuCanvas = null;
    }
  }

  function setGpuAcceleration(enabled) {
    enabled = enabled !== false;
    if (gpuAcceleration === enabled) { reportRendererStatus(); return; }
    gpuAcceleration = enabled;
    panels.forEach(function (p) {
      // Keep a working renderer available for a later toggle. Redrawing the
      // same panel preserves the camera, filters, brush and cell identities.
      if (enabled) attachGpu(p); else hideGpu(p);
      p.cloneBase = null;
      if (D && p.spaceId) project(p, !enabled);
    });
    if (D) drawAll();
    reportRendererStatus();
  }

  // The active backends are appended to the GPU acceleration tooltip so the
  // switch explains what is currently drawing without a separate badge.
  function reportRendererStatus() {
    var checkbox = document.getElementById('webgl_checkbox');
    var row = checkbox && checkbox.closest('.cerebro-preference');
    if (!row) return;
    var backends = [];
    panels.forEach(function (p) {
      if (!p.spaceId || !p.canvas.offsetParent ||
          getComputedStyle(p.canvas).visibility === 'hidden') return;
      var stats = p.gpu && p.gpuCanvas && p.gpuCanvas.style.display !== 'none'
        && p.gpu.stats ? p.gpu.stats() : null;
      var backend = stats && stats.backend === 'webgpu' ? 'WebGPU'
        : stats && stats.backend === 'webgl2' ? 'WebGL2' : 'Canvas';
      if (backends.indexOf(backend) < 0) backends.push(backend);
    });
    // Plotly-backed pages still honour the same server-side preference.
    if (!backends.length) {
      document.querySelectorAll('.tab-pane.active .js-plotly-plot').forEach(function (plot) {
        if (!plot.offsetParent || !plot._fullData) return;
        plot._fullData.forEach(function (trace) {
          if (trace.visible === false || trace.visible === 'legendonly') return;
          var backend = /gl$|3d$|^(surface|mesh3d|volume|isosurface|splom|parcoords)$/.test(trace.type)
            ? 'WebGL' : 'SVG';
          if (backends.indexOf(backend) < 0) backends.push(backend);
        });
      });
    }
    var title = (row.dataset.help || '') + (backends.length
      ? ' Current plots: ' + backends.join(', ') + '.' : '');
    if (row.title !== title) row.title = title;
    var help = document.getElementById('webgl_checkbox_help');
    if (help && help.textContent !== title) help.textContent = title;
  }

  function attachUnderlay(p) {
    if (p.underlayCanvas) return;
    var canvas = document.createElement('canvas');
    canvas.className = 'cv-underlay-layer';
    canvas.setAttribute('aria-hidden', 'true');
    p.canvas.parentNode.insertBefore(canvas, p.canvas.nextSibling);
    p.underlayCanvas = canvas;
    p.underlayCtx = canvas.getContext('2d');
    syncPanelOverlay(p);
    if (p.W && p.H) {
      var dpr = window.devicePixelRatio || 1;
      resizeCanvasLayer(canvas, p.underlayCtx, p.W, p.H, dpr);
    }
  }

  // Hover changes much more often than the graph beneath it. Keep the moving
  // ring on its own transparent layer so following the cursor never repaints a
  // trajectory's complete edge set and point cloud.
  function attachInteractionLayer(p) {
    if (p.interactionCanvas) return;
    var canvas = document.createElement('canvas');
    canvas.className = 'cv-interaction-layer';
    canvas.setAttribute('aria-hidden', 'true');
    p.canvas.parentNode.insertBefore(canvas, p.canvas.nextSibling);
    p.interactionCanvas = canvas;
    p.interactionCtx = canvas.getContext('2d');
    if (p.W && p.H) {
      resizeCanvasLayer(canvas, p.interactionCtx, p.W, p.H,
        window.devicePixelRatio || 1);
    }
  }

  function needsUnderlay(p) {
    var space = spaceById[p.spaceId];
    // Linked and Trekker scenes can carry images without background_scope.
    // Keep every image below both GPU points and Canvas points, including
    // images whose pixels are still loading.
    return !!(space && (space.background_scope || space.image ||
      (space.images && space.images.length) || space.trajectory ||
      (space.hulls && space.hulls.length)));
  }

  function updateGpuSelection(p, state, hiSet) {
    var previous = p.gpuData, oldState = p.gpuDataState;
    if (!previous || !oldState || !previous.opaque || !previous.vertexIndex ||
        panelColorMode(p) === RGB_MODE || Object.keys(state).some(function (key) {
          return key !== 'selection' && key !== 'niche' && state[key] !== oldState[key];
        })) return null;
    // Selection only changes alpha and foreground layers. Keep the sorted
    // positions and RGB bytes; the vertex map preserves filtered/invalid cells.
    // Translucent palettes and RGB retain the general colour preparation path.
    var colors = previous.colors.slice(), layers = new Uint32Array(previous.layers.length);
    var alpha = Math.round(255 * (hiSet ? 0.05 : pointOpacityOf(p)));
    for (var i = 0; i < previous.count; i++) colors[i * 4 + 3] = alpha;
    var foreground = false;
    if (hiSet) hiSet.forEach(function (cell) {
      var vertex = previous.vertexIndex[cell] - 1;
      if (!(vertex >= 0)) return;
      colors[vertex * 4 + 3] = Math.round(255 * 0.95);
      layers[vertex] = 1; foreground = true;
    });
    p.gpuDataState = state;
    p.gpuData = {positions: previous.positions, colors: colors, layers: layers,
      count: previous.count, foreground: foreground, opaque: true,
      vertexIndex: previous.vertexIndex};
    return p.gpuData;
  }

  function buildGpuData(p, shownMask, shownCount) {
    var state = gpuDataState(p);
    if (sameGpuDataState(state, p.gpuDataState)) return p.gpuData;
    var rgb = panelColorMode(p) === RGB_MODE;
    var hiSet = (sel && sel.size) ? sel : nicheSet;
    var selectionData = updateGpuSelection(p, state, hiSet);
    if (selectionData) return selectionData;
    var unit = state.unit, order = paintOrder(p), n = D.n;
    var shared = window.CerebroSharedDatasetState;
    var sharedName = null;
    var currentSpace = spaceById[p.spaceId];
    if (shared && shownCount === n && !order && currentSpace) {
      Object.keys(shared.projections || {}).some(function (name) {
        var projection = shared.projections[name];
        if (projection.x !== currentSpace.x || projection.y !== currentSpace.y) return false;
        sharedName = name; return true;
      });
    }
    var projectionCache = sharedName && shared.projections[sharedName];
    var positions = sharedName && shared.gpuPositions[sharedName];
    var sharedPositionsReady = !!(positions &&
      positions.length === shownCount * 2 && projectionCache &&
      Number.isInteger(projectionCache.gpuPositionCount));
    if (!positions || positions.length !== shownCount * 2) {
      positions = new Float32Array(shownCount * 2);
      if (sharedName) shared.gpuPositions[sharedName] = positions;
      sharedPositionsReady = false;
    }
    var colors = new Uint8Array(shownCount * 4);
    var layers = new Uint32Array(shownCount);
    var categoricalGroup = rgb ? null : catOf(panelColorMode(p));
    var categoricalRgba = categoricalGroup
      ? categoricalGroup.levels.map(function (_level, index) {
        return gpuColor((categoricalGroup.colors && categoricalGroup.colors[index]) ||
          PAL[index % PAL.length]);
      }) : null;
    var missingCategoricalRgba = categoricalGroup ? gpuColor('#cccccc') : null;
    var vertexIndex = rgb ? null : new Int32Array(n), opaque = true;
    var foreground = false, out = 0;
    var pending = unit._pending, cmx = 0, cmy = 0, validCount = 0;
    var singleGroup = !order && !hiSet && !rgb && shownCount === n &&
      state.groupValues && state.groupColors &&
      state.groupColors.indexOf(',') < 0;
    var flatColor = singleGroup ? state.groupColors :
      (!order && !hiSet && !rgb && shownCount === n &&
        state.fieldConstantColor);
    if (flatColor) {
      if (sharedPositionsReady && !pending) {
        out = projectionCache.gpuPositionCount;
        if (vertexIndex) {
          var vertex = 0;
          for (var i = 0; i < n; i++) if (unit.ok[i]) vertexIndex[i] = ++vertex;
        }
      } else {
        for (var i = 0; i < n; i++) {
          if (pending) {
            var x = pending.x[i], y = pending.y[i];
            if (x != null && y != null && isFinite(x) && isFinite(y)) {
              unit.nx[i] = (x - unit.x0) * unit.k;
              unit.ny[i] = (y - unit.y0) * unit.ky;
              unit.ok[i] = 1;
              cmx += unit.nx[i]; cmy += unit.ny[i]; validCount++;
              unit.occ[occIdx(unit.ny[i]) * OCC + occIdx(unit.nx[i])] = 1;
            }
          }
          if (!unit.ok[i]) continue;
          if (vertexIndex) vertexIndex[i] = out + 1;
          positions[out * 2] = unit.nx[i];
          positions[out * 2 + 1] = unit.ny[i];
          out++;
        }
      }
      var flatRgba = gpuColor(flatColor);
      opaque = flatRgba[3] === 255;
      var flatAlpha = Math.round(flatRgba[3] * pointOpacityOf(p));
      var packedRgba = (flatRgba[0] | (flatRgba[1] << 8) |
        (flatRgba[2] << 16) | (flatAlpha << 24)) >>> 0;
      new Uint32Array(colors.buffer).fill(packedRgba);
    } else {
      for (var at = 0; at < n; at++) {
        i = order ? order[at] : at;
        if (pending) {
          x = pending.x[i]; y = pending.y[i];
          if (x != null && y != null && isFinite(x) && isFinite(y)) {
            unit.nx[i] = (x - unit.x0) * unit.k;
            unit.ny[i] = (y - unit.y0) * unit.ky;
            unit.ok[i] = 1;
            cmx += unit.nx[i]; cmy += unit.ny[i]; validCount++;
            unit.occ[occIdx(unit.ny[i]) * OCC + occIdx(unit.nx[i])] = 1;
          }
        }
        if (!unit.ok[i] || !shownMask[i]) continue;
        if (vertexIndex) vertexIndex[i] = out + 1;
        positions[out * 2] = unit.nx[i];
        positions[out * 2 + 1] = unit.ny[i];
        var packedRgb = rgb
          ? rgbGpuColor(D.rgb.r[i], D.rgb.g[i], D.rgb.b[i]) : 0;
        var fg = rgb ? !!(packedRgb & 0x1000000) : !!(hiSet && hiSet.has(i));
        var alpha = hiSet ? (fg ? 0.95 : 0.05)
          : rgb ? (fg ? 1 : 0.5 * pointOpacityOf(p)) : pointOpacityOf(p);
        if (rgb) {
          colors[out * 4] = packedRgb & 255;
          colors[out * 4 + 1] = (packedRgb >>> 8) & 255;
          colors[out * 4 + 2] = (packedRgb >>> 16) & 255;
          colors[out * 4 + 3] = Math.round(255 * alpha);
        } else {
          var category = categoricalGroup && categoricalGroup.values[i];
          var rgba = categoricalGroup
            ? (category == null || category < 0 || category >= categoricalRgba.length
              ? missingCategoricalRgba : categoricalRgba[category])
            : gpuColor(colorOf(i, p));
          if (rgba[3] !== 255) opaque = false;
          colors[out * 4] = rgba[0];
          colors[out * 4 + 1] = rgba[1];
          colors[out * 4 + 2] = rgba[2];
          colors[out * 4 + 3] = Math.round(rgba[3] * alpha);
        }
        layers[out] = fg ? 1 : 0;
        foreground = foreground || fg;
        out++;
      }
    }
    if (pending) {
      unit.sat = occSAT(unit.occ);
      unit.cmx = validCount ? cmx / validCount : 0.5;
      unit.cmy = validCount ? cmy / validCount : 0.5;
      delete unit._pending;
    }
    if (projectionCache && !unit._pending) {
      projectionCache.unit = unit;
      projectionCache.gpuPositionCount = out;
    }
    p.gpuDataState = state;
    p.gpuData = {
      positions: positions,
      colors: colors,
      layers: layers,
      count: out,
      opaque: !rgb && opaque,
      vertexIndex: vertexIndex,
      foreground: foreground
    };
    return p.gpuData;
  }

  function drawGpuPoints(p, shownMask, shownCount, border) {
    if (!gpuEligible(p)) { hideGpu(p); return false; }
    var data, ok;
    try {
      data = buildGpuData(p, shownMask, shownCount);
      p.gpu.setData(data);
      ok = p.gpu.draw({
        view: p.view || { cx: 0.5, cy: 0.5, span: 1 },
        rect: { x: p._sox, y: p._soy, width: p._SX, height: p._SY },
        pointSize: p._renderPointSize,
        border: border ? { color: gpuColor(border.color), width: border.width } : null
      });
    } catch (error) {
      var failedRenderer = p.gpu;
      if (replaceGpuWithWebGl(p, failedRenderer)) {
        watchGpu(p, p.gpu);
        return drawGpuPoints(p, shownMask, shownCount, border);
      }
      disableGpu(p);
      return false;
    }
    if (!ok) { disableGpu(p); return false; }
    setGpuLayerVisible(p, true);
    p.gpuCanvas.dataset.pointCount = String(data.count);
    return true;
  }

  function requestSingleAux() {
    var id = singleActive, view = id && singleView(id);
    var token = CBViewState.specialistLifecycle.requestAux(view);
    if (token == null) return;
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        if (singleView(id) !== view || view.data.wire_token !== token || singleActive !== id) {
          if (view._auxPending === token) view._auxPending = null;
          return;
        }
        var cached = singleAuxCache[id], reuse = view.data.aux_reuse;
        if (reuse != null && cached && Number(cached.aux_origin_token) === Number(reuse) &&
            sameDatasetContext(cached.dataset_context, view.dataset_context)) {
          applySingleAux(Object.assign({}, cached, {
            wire_token: token, render_token: view.aux_render_token
          }));
          if (view._auxToken === token) return;
        }
        Shiny.setInputValue('cell_view_aux_request', {id:id, wire_token:token, dataset_context:view.dataset_context}, {priority:'event'});
      });
    });
  }

  function cloneBaseState(p) {
    var space = spaceById[p.spaceId];
    // The clonal chart uses batched Canvas paths to preserve their overlap
    // semantics. Its unchanged base must not be repainted for hover elsewhere.
    if (!D || D.n < GPU_MIN_CELLS || !space || space !== spaceById.clone ||
        !space._axisSpec || !space._unit || space._unit.nz ||
        space.trajectory || (space.hulls && space.hulls.length) || isSpatialSpace(space)) {
      p.cloneBase = null;
      return null;
    }
    var state = gpuDataState(p), border = space.pointBorder;
    if (!border && bordersOn) border = {color:'rgba(90,90,90,.42)',width:0.7};
    state.axes = space._axisSpec;
    state.width = p.canvas.width; state.height = p.canvas.height;
    state.logicalWidth = p.W; state.logicalHeight = p.H;
    state.pixelRatio = window.devicePixelRatio || 1;
    state.pointSize = p._renderPointSize;
    state.borderColor = border && border.color; state.borderWidth = border && border.width;
    state.viewX = p.view && p.view.cx; state.viewY = p.view && p.view.cy;
    state.viewSpan = p.view && p.view.span;
    return state;
  }

  function restoreCloneBase(p, state) {
    var cached = p.cloneBase;
    if (!state || !cached || !sameGpuDataState(state, cached.state)) return false;
    var c = p.ctx;
    c.save(); c.setTransform(1, 0, 0, 1, 0, 0);
    c.globalAlpha = 1; c.globalCompositeOperation = 'copy';
    c.drawImage(cached.canvas, 0, 0); c.restore();
    c.globalAlpha = cached.alpha; c.fillStyle = cached.fill;
    c.strokeStyle = cached.stroke; c.lineWidth = cached.lineWidth;
    return true;
  }

  function rememberCloneBase(p, state) {
    var previous = p.cloneBase;
    var canvas = previous ? previous.canvas : document.createElement('canvas');
    if (canvas.width !== p.canvas.width) canvas.width = p.canvas.width;
    if (canvas.height !== p.canvas.height) canvas.height = p.canvas.height;
    var copy = canvas.getContext('2d');
    copy.setTransform(1, 0, 0, 1, 0, 0); copy.globalAlpha = 1;
    copy.globalCompositeOperation = 'copy'; copy.drawImage(p.canvas, 0, 0);
    p.cloneBase = {state:state,canvas:canvas,alpha:p.ctx.globalAlpha,
      fill:p.ctx.fillStyle,stroke:p.ctx.strokeStyle,lineWidth:p.ctx.lineWidth};
  }

  function draw(p, shownMask, shownCount) {
    var c = p.ctx; clearPanelLayers(p);
    if (needsUnderlay(p)) attachUnderlay(p);
    var underlay = p.underlayCtx || c;
    updateCanvasDescription(p);
    if (!p.ok) { hideGpu(p); return; }
    p._renderPointSize = pointSizeOf(p);
    var panelPointOpacity = pointOpacityOf(p);
    var cloneState = cloneBaseState(p), cachedBase = restoreCloneBase(p, cloneState);
    if (!cachedBase) {
      drawImage(p, underlay);
      drawHulls(p, underlay);
      drawTrajectory(p, underlay);
      drawAxes(p);
    }
    // One two-layer pass: background on layer 0, foreground on layer 1 (on top).
    // The foreground is the "meaningful" set — expressing cells in RGB mode,
    // otherwise the selected cells. Alpha: with a selection, selected stay solid
    // and the rest fade; with none, RGB dims its grey substrate, else the opacity
    // slider governs. Both the layering and the paint go through one code path.
    // Highlight set = the lasso selection, or (Trekker) the picked nucleus's
    // niche. Cells in it stay solid; everything else fades.
    var n = D.n, i, rgb = panelColorMode(p) === RGB_MODE;
    var space = spaceById[p.spaceId], border = space && space.pointBorder;
    if (!border && bordersOn) {
      border = { color: 'rgba(90,90,90,.42)', width: 0.7 };
    }
    var hiSet = (sel && sel.size) ? sel : nicheSet;
    // shown(i) (= visible + activeCell) is stable across this draw but is tested
    // 2n times below (two layers) plus once in the evidence pass; precompute it
    // once. Uint8 mask, indexed instead of recomputed — halves the per-cell work
    // on the hot lasso-drag redraw path.
    if (!shownMask || shownMask.length !== n) {
      var shown = shownState(p);
      shownMask = shown.mask;
      shownCount = shown.count;
    }
    // Within one layer the alpha is CONSTANT (it depends only on fg/hiSet, which
    // is what defines the layer), so a layer can be drawn as one path per colour
    // instead of one path per cell. On a large data set that turns ~n canvas
    // operations per frame into ~(number of distinct colours), which is what
    // makes a 100k-cell lasso drag usable at all.
    //
    // It is not free: inside a single path, overlapping same-colour dots fill
    // ONCE, so dense regions lose the alpha build-up that per-cell fills give.
    // That build-up reads as density, so the per-cell path is kept for the data
    // sets where it is visible and affordable, and batching only kicks in past
    // the size where the frame cost dominates.
    var BATCH_MIN = 20000;
    // Ascending-value order for a continuous colouring; null (= natural order)
    // otherwise. In the batched path this also fixes the ORDER OF THE BUCKETS:
    // they are created as their first member is met, and object keys keep
    // insertion order, so filling them low-to-high paints them low-to-high.
    var pointsDrawn = cachedBase || drawGpuPoints(p, shownMask, shownCount, border);
    if (!pointsDrawn && p.gpuTransformOnly) return;
    if (!pointsDrawn) ensureScreenProjection(p);
    var ord = pointsDrawn ? null : paintOrder(p);
    // Colour buckets reorder interleaved cells. Preserve input order for
    // unsorted expression fields, including the Canvas fallback on large data.
    var orderedField = fieldForMode(panelColorMode(p));
    var preserveInputOrder = orderedField && orderedField.paintOrder === false;
    for (var layer = 0; !pointsDrawn && layer < 2; layer++) {
      var alpha = hiSet ? (layer === 1 ? 0.95 : 0.05)
        : rgb ? (layer === 1 ? 1 : 0.5 * panelPointOpacity) : panelPointOpacity;
      if (n >= BATCH_MIN && !preserveInputOrder) {
        var buckets = null;
        for (var oi = 0; oi < n; oi++) {
          i = ord ? ord[oi] : oi;
          if (!p.ok[i] || !shownMask[i]) continue;
          if ((layer === 0) === (rgb ? rgbExpressing(i) : !!(hiSet && hiSet.has(i)))) continue;
          var col = colorOf(i, p);
          if (!buckets) buckets = {};
          (buckets[col] || (buckets[col] = [])).push(i);
        }
        if (buckets) {
          c.globalAlpha = alpha;
          for (var col2 in buckets) {
            var idx = buckets[col2];
            c.fillStyle = col2;
            c.beginPath();
            for (var b = 0; b < idx.length; b++) {
              var j = idx[b], rj = radiusOf(p, j);
              c.moveTo(p.sx[j] + rj, p.sy[j]);
              c.arc(p.sx[j], p.sy[j], rj, 0, 6.2832);
            }
            c.fill();
            if (border) {
              c.strokeStyle = border.color; c.lineWidth = border.width; c.stroke();
            }
          }
        }
        continue;
      }
      for (var oj = 0; oj < n; oj++) {
        i = ord ? ord[oj] : oj;
        if (!p.ok[i] || !shownMask[i]) continue;
        var fg = rgb ? rgbExpressing(i) : !!(hiSet && hiSet.has(i));
        if ((layer === 0) === fg) continue;   // bg on layer 0, fg on layer 1
        paintCell(p, i, alpha, border);
      }
    }
    if (!pointsDrawn && cloneState) rememberCloneBase(p, cloneState);
    // Trekker: ring nuclei that carry positioning evidence.
    if (evidenceOn && D.trekker && D.trekker.evidence) {
      c.globalAlpha = 1; c.strokeStyle = '#1f2937'; c.lineWidth = 1.4;
      for (i = 0; i < n; i++) {
        if (!p.ok[i] || !shownMask[i] || D.trekker.evidence[i] !== 1) continue;
        c.beginPath(); c.arc(pointScreenX(p, i), pointScreenY(p, i),
          p._renderPointSize + 2.5, 0, 6.2832); c.stroke();
      }
    }
    drawAxes3D(p);
    drawGroupLabels(p);
    // picked cell ring
    if (pick != null && p.ok[pick]) {
      c.globalAlpha = 1; c.strokeStyle = '#f97316'; c.lineWidth = 2.2;
      c.beginPath(); c.arc(pointScreenX(p, pick), pointScreenY(p, pick),
        p._renderPointSize + 4, 0, 6.2832); c.stroke();
    }
    // Trekker: dashed niche-radius circle around the picked nucleus (physical
    // panel). Radius µm → screen px via the same data→unit→screen scale.
    if (pick != null && !sel && D.trekker && panelIsTrekker(p) && p.ok[pick]) {
      var trekker = trekkerSpace();
      var nu = trekker && trekker._unit;
      if (nu && p._SX != null && p._SY != null) {
        var viewScale = p.view ? 1 / p.view.span : 1;
        var radiusX = nicheRadius * nu.k * p._SX * viewScale;
        var radiusY = nicheRadius * nu.k * p._SY * viewScale;
        c.globalAlpha = 1;
        c.fillStyle = 'rgba(249,115,22,.10)';
        c.beginPath();
        c.ellipse(pointScreenX(p, pick), pointScreenY(p, pick),
          radiusX, radiusY, 0, 0, 6.2832);
        c.fill();
        c.strokeStyle = '#f97316'; c.lineWidth = 2.5; c.setLineDash([7, 4]);
        c.beginPath();
        c.ellipse(pointScreenX(p, pick), pointScreenY(p, pick),
          radiusX, radiusY, 0, 0, 6.2832);
        c.stroke();
        c.setLineDash([]);
      }
    }
    // Match the specialist Projection canvas: keep the light fill and solid
    // outline after selection so the active region remains immediately legible.
    var outline = lassoToScreen(p);
    if (outline && outline.length > 1) {
      c.globalAlpha = 1; c.strokeStyle = '#ff7013'; c.lineWidth = 1.6;
      c.beginPath(); c.moveTo(outline[0][0], outline[0][1]);
      for (var q = 1; q < outline.length; q++) c.lineTo(outline[q][0], outline[q][1]);
      c.closePath();
      c.fillStyle = 'rgba(255,112,19,.07)'; c.fill(); c.stroke();
    }
    c.globalAlpha = 1;
    // Its own canvas — drawn last so it also settles after a view change.
    drawMinimap(p);
    // A pinned tooltip points at a cell, so it has to move when the cell does —
    // a pan, a zoom, a rotation. Left where it was, it would be labelling
    // whatever the view slid underneath it.
    repositionPinned(p);
    p.canvas.dataset.pointCount = String(D.n);
    drawHoverOverlay(p);
  }

  function drawHoverOverlay(p) {
    var c = p.interactionCtx;
    if (!c) return;
    c.clearRect(0, 0, p.W, p.H);
    if (hoverCell == null || hoverCell === pick || !p.ok || !p.ok[hoverCell]) return;
    if (!singleActive && !linkedHoverOn && p !== hoverPanel) return;
    // Thinner and cooler than the pick ring so hover and selection never read
    // as the same state.
    c.globalAlpha = 1; c.strokeStyle = '#0f172a'; c.lineWidth = 1.6;
    c.beginPath();
    c.arc(pointScreenX(p, hoverCell), pointScreenY(p, hoverCell),
      p._renderPointSize + 3.5, 0, 6.2832);
    c.stroke();
    c.strokeStyle = 'rgba(255,255,255,0.85)'; c.lineWidth = 1;
    c.beginPath();
    c.arc(pointScreenX(p, hoverCell), pointScreenY(p, hoverCell),
      p._renderPointSize + 5, 0, 6.2832);
    c.stroke();
  }

  function drawHoverAll() {
    panels.forEach(function (p) {
      if (p.spaceId) drawHoverOverlay(p);
    });
  }
  // Live "showing N / M cells" readout — the single feedback that a filter or
  // subsample took effect, regardless of what the panels are coloured by.
  function renderShownCount(n) {
    var el = $('cv-shown'); if (!el || !D) return;
    // Hidden entirely when nothing is filtered out; only surfaces to explain a
    // reduced view (group filter, subsample, or legend-hide).
    if (n >= D.n) { el.style.display = 'none'; return; }
    el.style.display = '';
    el.textContent = 'showing ' + fmt(n) + ' of ' + fmt(D.n) + ' cells';
  }
  var drawAllDeferred = false;
  function drawAll() {
    if (drawAllDeferred) return;
    var first = null;
    panels.forEach(function (p) {
      if (!p.spaceId) { p.cloneBase = null; return; }
      var state = shownState(p);
      if (!first) first = state;
      draw(p, state.mask, state.count);
    });
    if (first) renderShownCount(first.count);
    updateResetButtons();
  }
  // Drop any committed lasso outlines when an interaction deliberately changes
  // what the region means (for example a new selection or selection zoom).
  // Returns true if anything was cleared.
  // Seed the dataset-level point size once, then leave user changes alone.
  var psSeeded = false;
  function autoPointSize(side) {
    if (!D) return;
    psSeeded = true;
    var configured = D.default_point_size == null
      ? NaN : Number(D.default_point_size);
    var v;
    if (isFinite(configured)) {
      v = Math.max(0, Math.min(20, configured));
    } else {
      var base = 6.5 - Math.log10(Math.max(1, D.n));
      var scale = Math.max(0.75, Math.min(1.35, (side || 520) / 520));
      v = Math.max(0.8, Math.min(7, base * scale));
    }
    ps = Math.round(v * 5) / 5;                    // slider step is 0.2
    setLinkedSliderValue('cv-ps', ps);
  }

  // ---- the shared More settings overlay ------------------------------------
  // The app-wide settings_drawer.js owns mounting, focus and overlay state.
  // This renderer only requests closure when its data context resets.
  function closeMore() {
    var drawer = $('cv-more');
    if (!drawer || !drawer.classList.contains('is-open')) return;
    var close = drawer.querySelector('[data-cerebro-drawer-close]');
    if (close) close.click();
  }

  function clearLassos() {
    var any = false;
    panels.forEach(function (p) {
      if (p.lasso || p.lassoData) { p.lasso = null; p.lassoData = null; any = true; }
    });
    return any;
  }

  // Linked views uses the same Shiny slider widget as Projection. Programmatic
  // state restores update its existing ionRangeSlider instance instead of
  // maintaining a second, hand-positioned value bubble.
  function setLinkedSliderValue(id, value, step) {
    var el = $(id); if (!el) return;
    el.value = String(value);
    var slider = window.jQuery(el).data('ionRangeSlider');
    if (!slider) return;
    var options = { from: Number(value) };
    if (step != null) options.step = step;
    el._cvSyncing = true;
    try { slider.update(options); }
    finally { el._cvSyncing = false; }
  }
  function syncPointControls() {
    setLinkedSliderValue('cv-ps', ps, 1);
    setLinkedSliderValue('cv-opacity', pointOpacity, 0.1);
    setLinkedSliderValue('cv-pct', pctShow, 10);
  }
  function setLinkedSliderDisabled(id, disabled) {
    var el = $(id); if (!el) return;
    el.disabled = disabled;
    var slider = window.jQuery(el).data('ionRangeSlider');
    if (!slider) return;
    el._cvSyncing = true;
    try { slider.update({ disable: disabled }); }
    finally { el._cvSyncing = false; }
  }

  function syncImgRangeValue(range, raw) {
    if (!range || raw === '') return false;
    var min = parseFloat(range.min), max = parseFloat(range.max);
    var value = Number(raw);
    if (!isFinite(value)) return false;
    if (isFinite(min)) value = Math.max(min, value);
    if (isFinite(max)) value = Math.min(max, value);
    range.value = String(value);
    value = Number(range.value);
    var number = $(range.id + '-number');
    if (number) {
      number.min = range.min; number.max = range.max; number.step = range.step;
      number.value = String(value);
    }
    var frac = (max > min) ? (value - min) / (max - min) : 0;
    range.style.setProperty('--cv-range-fill', (frac * 100) + '%');
    return true;
  }

  function syncAllImgRangeValues() {
    Array.prototype.forEach.call(
      document.querySelectorAll('.cv-img-range input[type=range]'),
      function (range) { syncImgRangeValue(range, range.value); }
    );
  }

  // ---- geometry helpers ----------------------------------------------------
  // inPoly lives in the shared CBGeom module (www/cv-geom.js). `nearest` stays
  // here: its visibility predicate (p.ok + shown) and fixed hit radius are this
  // engine's, not shared.
  function nearest(p, mx, my) {
    var sp = spaceById[p.spaceId], unit = sp && sp._unit;
    // A flat view can hit-test the same normalized geometry used by the GPU.
    // Keep the full scan for 3-D and incomplete geometry. Visibility is checked
    // at query time, so filters, hidden groups and selections stay current.
    if (D.n >= GPU_MIN_CELLS && unit && !unit.nz && !unit._pending &&
        unit.nx && unit.ny && unit.ok && p._SX > 0 && p._SY > 0) {
      return CBGeom.nearestInGrid(CBGeom.pointGrid(unit, D.n), p.view, {
        x: p._sox, y: p._soy, width: p._SX, height: p._SY
      }, mx, my, function (i) { return shown(i, p); });
    }
    ensureScreenProjection(p);
    var best = -1, bd = 200, n = D.n, i;
    for (i = 0; i < n; i++) {
      if (!p.ok[i] || !shown(i, p)) continue;
      var dx = p.sx[i] - mx, dy = p.sy[i] - my, d = dx * dx + dy * dy;
      if (d < bd) { bd = d; best = i; }
    }
    return best;
  }

  // ---- selection ----------------------------------------------------------
  function selectionSourceLabel(source) {
    if (!source) return '';
    if (typeof source === 'string') return source;
    var sp = source.spaceId && spaceById[source.spaceId];
    return (sp && sp.label) || source.spaceId || '';
  }
  function setSelection(s, source) {
    sel = (s && s.size) ? s : null;
    if (!sel) {
      selectionSource = null;
      selectionSourceSpace = null;
    } else if (source) {
      selectionSource = selectionSourceLabel(source);
      if (typeof source === 'string') selectionSourceSpace = null;
      else selectionSourceSpace = source.spaceId || null;
    }
    rebuildNiche();   // a lasso selection supersedes the niche highlight
    updateSelActions();
    updateZselButtons();
    renderSelbar(); renderReadout(); reportSelection(); drawAll();
  }
  // Subtle reveal/collapse for the selection bar + the Zoom/Clear group, so they
  // fade/slide in and out instead of popping. Elements start with `cv-collapse`
  // (+ display:none). To show: drop display (back into flow), let two frames
  // commit the collapsed state, then remove the class to transition in. To hide:
  // re-add the class to transition out, then set display:none once it settles.
  function revealEl(el, show) {
    if (!el) return;
    if (el.parentElement && el.parentElement.classList.contains('cv-status-slot')) {
      el.style.display = '';
      el.classList.toggle('cv-collapse', !show);
      return;
    }
    if (show) {
      if (el._hideT) { clearTimeout(el._hideT); el._hideT = null; }
      el.style.display = '';
      requestAnimationFrame(function () {
        requestAnimationFrame(function () { el.classList.remove('cv-collapse'); });
      });
    } else {
      if (getComputedStyle(el).display === 'none') return;
      el.classList.add('cv-collapse');
      el._hideT = setTimeout(function () {
        el.style.display = 'none'; el._hideT = null;
      }, 240);
    }
  }
  function renderWorkspaceGuide() {
    var guide = $('cv-workspace-guide');
    var text = $('cv-workspace-guide-text');
    var overview = $('cv-workspace-overview');
    if (!guide) return;
    var active = !!((sel && sel.size) || (pick != null && nicheSet));
    if (focusPanel) {
      var focused = null;
      panels.forEach(function (p) { if (p.key === focusPanel) focused = p; });
      var sp = focused && spaceById[focused.spaceId];
      if (text) {
        text.textContent = 'Focused view: ' + ((sp && sp.label) || 'linked lens') +
          ' · other views remain visible and linked.';
      }
      if (overview) overview.style.display = '';
    } else {
      if (text) {
        text.textContent = 'Drag in any view to create an active cohort. ' +
          'Use Focus to enlarge one lens while keeping the others linked.';
      }
      if (overview) overview.style.display = 'none';
    }
    revealEl(guide, !!D && !active);
  }
  // Clearing is a global cohort action, so expose one compact control only
  // while a selection or Trekker neighbourhood exists.
  function updateSelActions() {
    var hasSel = !!(sel && sel.size);
    var hasNiche = !hasSel && pick != null && !!nicheSet;
    revealEl($('cv-selactions'), hasSel || hasNiche);
  }
  // Zoom one requested panel to the bounding box of its selected cells.
  function setSelectionZoomed(on) {
    selectionZoomed = !!on;
    document.querySelectorAll(
      '[data-cell-view-action="focus"], #cv-zsel'
    ).forEach(function (button) {
      button.classList.toggle('is-on', selectionZoomed);
      button.setAttribute('aria-pressed', selectionZoomed ? 'true' : 'false');
    });
  }
  function zoomToSelection(p) {
    if (!canZoomPanel(p)) return false;
    var u = spaceById[p.spaceId]._unit;
    var nx0 = Infinity, nx1 = -Infinity, ny0 = Infinity, ny1 = -Infinity;
    sel.forEach(function (i) {
      if (!u.ok[i]) return;
      var nx = u.nx[i], ny = u.ny[i];
      if (nx < nx0) nx0 = nx; if (nx > nx1) nx1 = nx;
      if (ny < ny0) ny0 = ny; if (ny > ny1) ny1 = ny;
    });
    p.view = clampView(p, CBGeom.fitView(
      nx0, nx1, ny0, ny1, 1.25, 0.02
    ));
    setSelectionZoomed(true);
    project(p);
    drawAll();
    return true;
  }
  // Promote one lens without leaving the linked workspace. The focused pane
  // grows; every other pane remains visible as context and keeps its viewport.
  function setFocusPanel(key) {
    var host = panels[0] && panels[0].pane && panels[0].pane.parentElement;
    // A second click during the motion starts from the visible layout rather
    // than inheriting a half-finished transform from the first transition.
    clearTimeout(focusResizeTimer);
    if (host) host.classList.remove('cv-focus-transitioning');
    panels.forEach(function (p) {
      if (!p.pane) return;
      p.pane.style.transition = '';
      p.pane.style.transform = '';
      p.pane.style.transformOrigin = '';
      p.pane.style.opacity = '';
    });
    var before = {};
    panels.forEach(function (p) {
      if (p.pane && p.spaceId) before[p.key] = p.pane.getBoundingClientRect();
    });
    focusAnimating = true;
    if (host) host.classList.add('cv-focus-transitioning');
    focusPanel = (focusPanel === key) ? null : key;
    panels.forEach(function (p) {
      if (!p.pane) return;
      var primary = !!focusPanel && p.key === focusPanel;
      var context = !!focusPanel && p.key !== focusPanel && !!p.spaceId;
      p.pane.classList.toggle('cv-focus-primary', primary);
      p.pane.classList.toggle('cv-focus-context', context);
      p.pane.style.order = primary ? '0' : (context ? '1' : '');
      var btn = p.pane.querySelector('.cv-focus-btn');
      if (btn) {
        var on = primary;
        btn.classList.toggle('is-on', on);
        btn.setAttribute('aria-pressed', on ? 'true' : 'false');
        var tip = on ? 'Exit focus and return to overview' : 'Make this the focus';
        btn.setAttribute('data-tip', tip);
        btn.setAttribute('aria-label', tip);
        var label = btn.querySelector('.cv-focus-label');
        if (label) label.textContent = on ? 'Exit focus' : 'Focus';
      }
      var role = p.pane.querySelector('.cv-role-badge');
      if (role) {
        role.classList.remove('is-focus', 'is-context');
        if (focusPanel && p.spaceId) {
          role.style.display = '';
          role.textContent = primary ? 'FOCUS' : 'CONTEXT';
          role.classList.add(primary ? 'is-focus' : 'is-context');
        } else {
          role.style.display = 'none';
          role.textContent = '';
        }
      }
    });
    if (host) host.classList.toggle('cv-has-focus', !!focusPanel);
    updateFocusButtons();
    renderWorkspaceGuide();
    resizeAll();

    // FLIP the grid reflow: the CSS grid reaches its final geometry immediately,
    // then every pane animates from its old rectangle into that geometry. This is
    // symmetric for enter/exit and avoids ResizeObserver repeatedly redrawing a
    // canvas while its CSS box is still moving.
    panels.forEach(function (p) {
      if (!p.pane || !p.spaceId || !before[p.key]) return;
      var from = before[p.key], to = p.pane.getBoundingClientRect();
      if (!(to.width > 0 && to.height > 0)) return;
      var dx = from.left - to.left, dy = from.top - to.top;
      var sx = from.width / to.width, sy = from.height / to.height;
      p.pane.style.transition = 'none';
      p.pane.style.transformOrigin = 'top left';
      p.pane.style.transform = 'translate(' + dx + 'px,' + dy + 'px) scale(' + sx + ',' + sy + ')';
      p.pane.style.opacity = focusPanel && p.key !== focusPanel ? '.88' : '.96';
    });
    if (host) void host.offsetWidth; // commit the inverse transforms before release
    requestAnimationFrame(function () {
      panels.forEach(function (p) {
        if (!p.pane || !p.spaceId) return;
        p.pane.style.transition = '';
        p.pane.style.transform = '';
        p.pane.style.opacity = '';
      });
    });
    focusResizeTimer = setTimeout(function () {
      _layoutKey = null;
      focusAnimating = false;
      if (host) host.classList.remove('cv-focus-transitioning');
      panels.forEach(function (p) {
        if (!p.pane) return;
        p.pane.style.transformOrigin = '';
      });
      resizeAll();
    }, 460);
  }
  // The per-panel "zoom to selection" is only an action while there IS one, and
  // only on a panel that lays cells out in a plane -- a rotated cloud has no
  // rectangle to zoom to that survives the next turn.
  // Focus is meaningful whenever another linked lens exists: it changes visual
  // hierarchy even when all panels already fit on one row.
  function updateFocusButtons() {
    var vis = panels.filter(function (p) { return p.spaceId; });
    panels.forEach(function (p) {
      if (!p.pane) return;
      var btn = p.pane.querySelector('.cv-focus-btn');
      if (!btn) return;
      var useful = vis.length > 1 || focusPanel === p.key;
      btn.style.display = (p.spaceId && useful) ? '' : 'none';
    });
  }
  function updateZselButtons() {
    panels.forEach(function (p) {
      if (!p.pane) return;
      var btn = p.pane.querySelector('.cv-zsel-btn');
      if (btn) btn.style.display = canZoomPanel(p) ? '' : 'none';
      var clear = p.pane.querySelector('.cv-clear-btn');
      if (clear) clear.style.display = sel && sel.size ? '' : 'none';
    });
  }
  function updateResetButtons() {
    panels.forEach(function (p) {
      if (!p.pane) return;
      var button = p.pane.querySelector('.cv-tbtn[data-act="reset"]');
      if (button) button.disabled = !(p.view || p.rot);
    });
  }
  // Only where the button would do something: a selection, a flat panel (a
  // rotated cloud has no rectangle to zoom to that survives the next turn), and
  // at least one selected cell that HAS a position in this space. On a
  // multi-section spatial data set the selection can be entirely in another
  // section, and the button then looked available and did nothing.
  function canZoomPanel(p) {
    if (!p || !sel || !sel.size || !p.spaceId || panelIs3D(p)) return false;
    var sp = spaceById[p.spaceId], u = sp && sp._unit;
    if (!u || !u.ok) return false;
    var any = false;
    sel.forEach(function (i) { if (u.ok[i]) any = true; });
    return any;
  }
  function resetZoom(only) {
    var any = false;
    panels.forEach(function (p) {
      if (only && p !== only) return;
      if (p.view) { p.view = null; project(p); any = true; }
    });
    setSelectionZoomed(false);
    if (any) drawAll();
  }
  function toggleSelectionFocus(id) {
    if (id && singleActive !== id) return;
    if (selectionZoomed) {
      resetZoom();
      return;
    }
    panels.forEach(function (panel) {
      if (canZoomPanel(panel)) zoomToSelection(panel);
    });
  }
  // Sync the active drag-mode highlight (box vs lasso) across every toolbar.
  function syncModeButtons() {
    ['box', 'lasso', 'pan', 'orbit'].forEach(function (m) {
      var btns = document.querySelectorAll('.cv-tbtn[data-act="' + m + '"]');
      Array.prototype.forEach.call(btns, function (b) {
        b.classList.toggle('is-on', selectMode === m);
      });
    });
    syncCursors();
  }
  // Step-zoom a panel about its current view centre. factor<1 zooms in, >1 out;
  // zooming back past the full extent clears the zoom.
  // ---- keeping a view on the data -----------------------------------------
  // Panning and zooming are otherwise unbounded, and a view dragged off the data
  // is a dead end: the canvas goes blank, the minimap's frame slides out of
  // frame, and nothing on screen distinguishes "you went too far" from "this
  // broke". The rule is that a view must always show SOMETHING.
  //
  // Two steps, because the first alone is not enough. Holding the centre inside
  // the data's bounding box still allows a blank view — a UMAP occupies its box
  // very unevenly, and panning to one edge of the demo left the canvas entirely
  // empty even with the box respected. So a view that ends up seeing no points
  // is walked back toward the data's centre of mass and stopped at the first
  // position that sees any.
  //
  // Dragging further past that point keeps resolving to the same place, so it
  // reads as hitting a wall rather than being yanked around — while a view that
  // legitimately crosses a gap between clusters is untouched, because it still
  // has the clusters on either side in frame.
  //
  // Every path that moves a view goes through here rather than clamping for
  // itself: pan, wheel/button zoom and zoom-to-selection can all push a view
  // out, and a rule enforced in one of three places is a rule with holes.
  function clampView(p, v) {
    if (!v) return v;
    var sp = spaceById[p.spaceId], u = sp && sp._unit;
    if (!u || !u.bx || !u.sat) return v;
    var b = u.bx;
    var cx = Math.min(Math.max(v.cx, b.x0), b.x1);
    var cy = Math.min(Math.max(v.cy, b.y0), b.y1);
    // The occupancy lattice is built from the UNROTATED cloud, so it says
    // nothing useful once a 3-D space has been turned; the bounding-box clamp
    // above still applies, and the sphere fit keeps the cloud inside it.
    if (u.nz && p.rot) return { cx: cx, cy: cy, span: v.span };
    if (!viewHasData(u, cx, cy, v.span)) {
      var STEPS = 24, found = false;
      for (var s = 1; s <= STEPS && !found; s++) {
        var t = s / STEPS;
        var qx = cx + (u.cmx - cx) * t, qy = cy + (u.cmy - cy) * t;
        if (viewHasData(u, qx, qy, v.span)) { cx = qx; cy = qy; found = true; }
      }
      // Nothing along that line saw anything (a ring-shaped space whose centre
      // of mass falls in the hole) — sit on the centre of mass and accept it.
      if (!found) { cx = u.cmx; cy = u.cmy; }
    }
    return { cx: cx, cy: cy, span: v.span };
  }

  // Call after REPLACING a space's coordinates (a different projection, spatial
  // section, or clonal layout). Everything a panel remembers about where it was
  // looking refers to coordinates that no longer exist:
  //   * p.view is a fraction of the space's own box — the box is still there,
  //     but different points are in it now, so the same fraction lands
  //     somewhere arbitrary. Zoomed in, that is usually blank canvas.
  //   * p.lasso is in screen pixels, so it would outline whatever happens to
  //     sit beneath it now.
  // Note that clampView() cannot save this: it runs on pan and zoom, and this
  // is neither — which is how a view can still end up on empty space despite it.
  // Everything a panel remembers about how it is currently looking at its space.
  // Kept in one place because it is dropped from two directions -- the space's
  // coordinates changing under the panel, and the panel being handed a different
  // space on a data-set switch -- and a field cleared in only one of them
  // survives into a data set it was never computed for.
  function clearPanelView(p) {
    p.view = null; p.lasso = null; p.lassoData = null;
    p.rot = null; p.depth = null; p.miniBg = null;
  }
  function resetSpaceViews(spaceId) {
    panels.forEach(function (p) {
      if (p.spaceId !== spaceId) return;
      clearPanelView(p);
    });
  }

  // Zoom about a screen point, keeping whatever is under it fixed — the gesture
  // every map and every plotly plot uses. `factor` < 1 zooms in. Passing the
  // panel centre gives the plain in/out of the toolbar buttons.
  function zoomAt(p, mx, my, factor) {
    if (!p._SX || !p._SY) return;
    setSelectionZoomed(false);
    var zx = (mx - p._sox) / p._SX;
    var zy = (p._soy + p._SY - my) / p._SY;
    var next = CBGeom.zoomView(p.view, factor, [zx, zy], 0.04);
    if (!next) {
      if (p.view) { p.view = null; project(p); }
    } else {
      // Clamped, so zooming toward a point near the edge slides the anchor a
      // little rather than carrying the view off the data.
      p.view = clampView(p, next);
      project(p);
    }
    drawAll();
  }
  function zoomStep(p, factor) {
    if (!p._SX || !p._SY) return;
    zoomAt(p, p._sox + p._SX / 2, p._soy + p._SY / 2, factor);
  }
  function downloadCanvasPNG(canvas, filename) {
    var link = null;
    try {
      link = document.createElement('a');
      link.href = canvas.toDataURL('image/png');
      link.download = filename;
      document.body.appendChild(link);
      link.click();
      return true;
    } catch (e) {
      return false;
    } finally {
      if (link && link.parentNode) link.parentNode.removeChild(link);
    }
  }

  function downloadPanelPNG(p, filenamePrefix) {
    var nm = (spaceById[p.spaceId] && spaceById[p.spaceId].label) ||
      p.spaceId || 'panel';
    return downloadPanelsPNG(
      [p],
      (filenamePrefix || 'linked-views') + '-' + nm.replace(/[^\w.-]+/g, '_') + '.png'
    );
  }

  function pngFilename(prefix) {
    return prefix + '-' + new Date().toISOString()
      .replace(/[-:]/g, '').replace('T', '-').slice(0, 15) + '.png';
  }

  function visibleRect(element) {
    if (!element) return null;
    var style = window.getComputedStyle(element);
    var rect = element.getBoundingClientRect();
    return style.display !== 'none' && style.visibility !== 'hidden' &&
      rect.width > 0 && rect.height > 0 ? rect : null;
  }

  function drawExportKey(context, element, originX, originY) {
    var rootRect = element.getBoundingClientRect();
    element.querySelectorAll('.cv-lg, .cv-rgb-note').forEach(function (item) {
      var rect = item.getBoundingClientRect();
      var dot = item.querySelector('.cv-dot, .cv-evidence-dot');
      var itemStyle = window.getComputedStyle(item);
      var x = originX + rect.left - rootRect.left;
      var y = originY + rect.top - rootRect.top + rect.height / 2;
      context.save();
      var opacity = Number(itemStyle.opacity);
      context.globalAlpha = isFinite(opacity) ? opacity : 1;
      if (dot) {
        var dotStyle = window.getComputedStyle(dot);
        context.beginPath();
        context.fillStyle = dotStyle.backgroundColor;
        context.arc(x + 5, y, 5, 0, Math.PI * 2);
        context.fill();
        if (parseFloat(dotStyle.borderWidth)) {
          context.strokeStyle = dotStyle.borderColor;
          context.stroke();
        }
        x += 16;
      }
      context.fillStyle = itemStyle.color;
      context.font = itemStyle.fontWeight + ' ' + itemStyle.fontSize + ' ' +
        itemStyle.fontFamily;
      context.textBaseline = 'middle';
      context.fillText(item.textContent.trim(), x, y);
      context.restore();
    });
  }

  function fillExportGradient(context, x, y, width, height, palette) {
    var gradient = context.createLinearGradient(x, 0, x + width, 0);
    for (var index = 0; index <= 4; index++) {
      gradient.addColorStop(
        index / 4,
        palette ? palette[Math.min(index * 64, 255)] : viridisCss(index / 4)
      );
    }
    context.fillStyle = gradient;
    context.fillRect(x, y, width, height);
  }

  function drawExportColorbar(context, element, originX, originY) {
    var rootRect = element.getBoundingClientRect();
    var gradientElement = $('cv-grad');
    var gradientRect = visibleRect(gradientElement);
    if (gradientRect) {
      var field = fieldOf();
      fillExportGradient(
        context,
        originX + gradientRect.left - rootRect.left,
        originY + gradientRect.top - rootRect.top,
        gradientRect.width,
        gradientRect.height,
        field && field.palette
      );
    }
    ['cv-cb0', 'cv-cb1', 'cv-cbar-note'].forEach(function (id) {
      var label = $(id);
      var rect = visibleRect(label);
      if (!rect || !label.textContent.trim()) return;
      var style = window.getComputedStyle(label);
      context.fillStyle = style.color;
      context.font = style.fontWeight + ' ' + style.fontSize + ' ' + style.fontFamily;
      context.textBaseline = 'middle';
      context.fillText(
        label.textContent.trim(),
        originX + rect.left - rootRect.left,
        originY + rect.top - rootRect.top + rect.height / 2
      );
    });
  }

  function downloadPanelsPNG(candidates, filename) {
    var visible = candidates.filter(function (panel) {
      if (!panel.spaceId || !panel.pane || !panel.canvas) return false;
      return !!visibleRect(panel.pane);
    });
    if (!visible.length) return false;

    var bounds = visible.reduce(function (result, panel) {
      var rect = panel.pane.getBoundingClientRect();
      result.left = Math.min(result.left, rect.left);
      result.top = Math.min(result.top, rect.top);
      result.right = Math.max(result.right, rect.right);
      result.bottom = Math.max(result.bottom, rect.bottom);
      return result;
    }, { left: Infinity, top: Infinity, right: -Infinity, bottom: -Infinity });
    var padding = 8;
    var panelWidth = bounds.right - bounds.left;
    var panelHeight = bounds.bottom - bounds.top;
    var legend = $('cv-legend');
    var colorbar = $('cv-cbar');
    var legendRect = visibleRect(legend);
    var colorbarRect = visibleRect(colorbar);
    var keyHeight = (legendRect ? legendRect.height + padding : 0) +
      (colorbarRect ? colorbarRect.height + padding : 0);
    var keyWidth = Math.max(
      legendRect ? legendRect.width : 0,
      colorbarRect ? colorbarRect.width : 0
    );
    var width = Math.ceil(Math.max(panelWidth, keyWidth) + padding * 2);
    var height = Math.ceil(panelHeight + keyHeight + padding * 2);
    if (width < 1 || height < 1) return false;

    // Keep exports within conservative cross-browser canvas limits. Large
    // workspaces are proportionally downscaled rather than partially cropped.
    var scale = Math.min(window.devicePixelRatio || 1, 2);
    var maxEdge = 8192;
    var maxPixels = 32 * 1024 * 1024;
    scale = Math.min(
      scale,
      maxEdge / width,
      maxEdge / height,
      Math.sqrt(maxPixels / (width * height))
    );
    if (!isFinite(scale) || scale <= 0) return false;

    var output = document.createElement('canvas');
    output.width = Math.max(1, Math.floor(width * scale));
    output.height = Math.max(1, Math.floor(height * scale));
    var context = output.getContext('2d');
    if (!context) return false;
    context.setTransform(scale, 0, 0, scale, 0, 0);
    context.fillStyle = '#ffffff';
    context.fillRect(0, 0, width, height);

    visible.forEach(function (panel) {
      var paneRect = panel.pane.getBoundingClientRect();
      var canvasRect = panel.canvas.getBoundingClientRect();
      var x = paneRect.left - bounds.left + padding;
      var y = paneRect.top - bounds.top + padding;
      context.fillStyle = '#ffffff';
      context.fillRect(x, y, paneRect.width, paneRect.height);
      context.strokeStyle = '#d7dce3';
      context.lineWidth = 1;
      context.strokeRect(x + 0.5, y + 0.5, paneRect.width - 1, paneRect.height - 1);

      var title = panel.pane.querySelector('.cv-ptitle');
      context.fillStyle = '#202733';
      context.font = '600 13px sans-serif';
      context.textBaseline = 'middle';
      context.fillText(
        title && title.textContent ? title.textContent.trim() : 'Linked view',
        x + 12,
        y + 20
      );
      var panelScale = visibleRect(panel.pane.querySelector('.cv-panel-scale'));
      if (panelScale) {
        var panelField = fieldForMode(panelColorMode(panel));
        fillExportGradient(
          context,
          panelScale.left - bounds.left + padding,
          panelScale.top - bounds.top + padding,
          panelScale.width,
          panelScale.height,
          panelField && panelField.palette
        );
      }

      var canvasX = canvasRect.left - bounds.left + padding;
      var canvasY = canvasRect.top - bounds.top + padding;
      context.fillStyle = '#ffffff';
      context.fillRect(canvasX, canvasY, canvasRect.width, canvasRect.height);
      exportPanelLayers(
        context, panel, canvasX, canvasY, canvasRect.width, canvasRect.height
      );

      if (panel.mini) {
        var miniStyle = window.getComputedStyle(panel.mini);
        var miniRect = panel.mini.getBoundingClientRect();
        if (miniStyle.display !== 'none' && miniStyle.visibility !== 'hidden' &&
            Number(miniStyle.opacity) > 0 && miniRect.width > 0 && miniRect.height > 0) {
          context.drawImage(
            panel.mini,
            miniRect.left - bounds.left + padding,
            miniRect.top - bounds.top + padding,
            miniRect.width,
            miniRect.height
          );
        }
      }
    });

    var keyY = panelHeight + padding * 2;
    if (legendRect) {
      drawExportKey(context, legend, padding, keyY);
      keyY += legendRect.height + padding;
    }
    if (colorbarRect) {
      drawExportColorbar(context, colorbar, padding, keyY);
    }

    return downloadCanvasPNG(output, filename);
  }

  function downloadVisiblePanelsPNG(filename) {
    return downloadPanelsPNG(panels, filename);
  }

  function downloadWorkspacePNG() {
    return !singleActive && downloadVisiblePanelsPNG(
      pngFilename('linked-views')
    );
  }
  function reportSelection(eventKind) {
    eventKind = eventKind || 'interaction';
    var hasSelection = !!(sel && sel.size);
    var specialistReport = singleActive
      ? window.CBViewState.specialistSelectionReport(
        sel,
        D && D.cells,
        D && D.n
      )
      : null;
    var pendingStableSelection = !!(specialistReport && specialistReport.pending);
    var arr = null;
    if (specialistReport) {
      arr = specialistReport.ids;
    } else if (hasSelection && Array.isArray(D && D.cells)) {
      arr = [];
      sel.forEach(function (i) { arr.push(D.cells[i]); });
    }
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      if (singleActive) {
        // Stable IDs identify the selection on their own. Send coordinates
        // only as the fallback key when some selected cell has no ID.
        var sp = spaceById[singleSpaceIds[0]], x = [], y = [];
        var needCoordinates = arr && arr.some(function (id) {
          return id == null || id === '';
        });
        if (needCoordinates && sp) sel.forEach(function (i) {
          x.push(sp.x[i]); y.push(sp.y[i]);
        });
        // Large specialist views paint before their stable cell IDs arrive.
        // Publish null during that window so every selected-cell result UI is
        // gated off instead of retaining stale/empty Plot and Table shells.
        // onSingleAuxBinary() reports only the then-current selection as soon
        // as the deferred IDs have been attached.
        var activeView = singleView(singleActive);
        Shiny.setInputValue(singleActive + '_persistent_selection', {
          x: pendingStableSelection || !arr ? [] : x,
          y: pendingStableSelection || !arr ? [] : y,
          ids: pendingStableSelection || !arr ? [] : arr,
          dataset_context: activeView && activeView.dataset_context
        });
        var renderKey = activeView && activeView.data && activeView.data.render_key;
        if (eventKind === 'primary' && renderKey != null) {
          Shiny.setInputValue(singleActive + '_rendered_state', {
            render_key: String(renderKey),
            dataset_context: activeView.dataset_context
          }, { priority: 'event' });
          Shiny.setInputValue(
            singleActive + '_rendered_key',
            String(renderKey),
            { priority: 'event' }
          );
        }
      } else {
        Shiny.setInputValue('coordviews_selection_indices', {indices:hasSelection ? Array.from(sel) : [], dataset_context:D && D.dataset_context});
      }
    }
    if (singleActive) {
      var guide = $(singleActive + '_selection_guide');
      var active = $(singleActive + '_selection_active');
      if (guide) guide.classList.toggle('cerebro-selection-status-hidden', hasSelection);
      if (active) active.classList.toggle('cerebro-selection-status-hidden', !hasSelection);
    }
    var datasetFingerprint = singleActive && D
      ? String(D.dataset_fingerprint || D.cell_fingerprint || '')
      : '';
    if (singleActive &&
        !/^md5-cell-set-v1:[0-9a-f]{32}$/.test(datasetFingerprint)) return;
    var specialistTiming = null;
    if (singleActive) {
      var timing = singleTiming[singleActive] || {};
      var readyAt = performance.now();
      specialistTiming = CBViewState.specialistLifecycle.ready(
        singleTiming, singleActive, readyAt
      );
    }
    var renderedPointCount = 0;
    if (singleActive) {
      var renderedPanel = panels.filter(function (panel) {
        return panel && panel.spaceId && panel.canvas;
      })[0];
      renderedPointCount = Number(
        renderedPanel && renderedPanel.canvas.dataset.pointCount
      ) || 0;
    }
    window.dispatchEvent(new CustomEvent(
      singleActive ? 'cerebro:specialist-state' : 'cerebro:linkedviews-selection',
      { detail: singleActive
        ? {
          viewId: singleActive,
          dataset_context: activeView && activeView.dataset_context,
          selectedCells: specialistReport
            ? specialistReport.selectedCells
            : (hasSelection ? sel.size : 0),
          selectionPending: pendingStableSelection,
          datasetFingerprint: datasetFingerprint,
          eventKind: eventKind,
          renderKey: renderKey == null ? null : String(renderKey),
          generation: Number(timing.generation) || 0,
          benchmarkGeneration:
            Number(window.__cerebroPageBenchGeneration) || 0,
          renderRequestSent: !!timing.renderRequestSent,
          renderedPointCount: renderedPointCount,
          timing: specialistTiming
        }
        : { selectedCells: hasSelection ? sel.size : 0, dataset_context: D && D.dataset_context } }
    ));
  }

  function attachSingleDatasetIdentity(identity) {
    var fingerprint = identity && identity.cell_fingerprint;
    if (!fingerprint) return;
    var staleCache = Object.keys(singleViews).some(function (id) {
      var shown = singleViews[id] && singleViews[id].datasetIdentity;
      if (!shown || !shown.cell_fingerprint) return false;
      return String(shown.cell_fingerprint) !== String(fingerprint) ||
        Number(shown.cell_count) !== Number(identity.cell_count) ||
        String(shown.cell_order_fingerprint || '') !==
          String(identity.cell_order_fingerprint || '');
    });
    var staleActive = singleActive && D && D.dataset_fingerprint &&
      String(D.dataset_fingerprint) !== String(fingerprint);
    if (staleCache || staleActive) {
      resetSingleViews();
      return;
    }
    if (!singleActive || !D || D.dataset_fingerprint) return;
    D.dataset_fingerprint = String(fingerprint);
    reportSelection();
  }

  function reportSingleHiddenGroups() {
    if (!singleActive || typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var group = catOf(singleSpaceModes[singleSpaceIds[0]]);
    var names = group ? Array.from(hidden).map(function (index) {
      return group.levels[index];
    }).filter(function (name) { return name != null; }) : [];
    var view = singleView(singleActive);
    Shiny.setInputValue(singleActive + '_hidden_groups', {
      groups: names,
      dataset_context: view && view.dataset_context
    });
  }

  // ---- readouts (composition + top clonotypes) — fully client-side --------
  function compactSpaceLabel(space, fallback) {
    if (space && space._baseSpaceId) {
      space = spaceById[space._baseSpaceId] || space;
    }
    var label = String((space && space.label) || fallback || '');
    if (space && space.trajectory) return 'Trajectory';
    if (space && (space.id === 'clone' || space._role === 'clone')) return 'TCR';
    if ((space && space._projectionName) || /\(expression(?:,[^)]+)?\)$/i.test(label)) {
      return label.replace(/\s*\(expression(?:,[^)]+)?\)\s*$/i, '').toUpperCase();
    }
    return label;
  }

  function renderSelbar() {
    var bar = $('cv-selbar');
    if (!bar) return;
    if (sel && sel.size) {
      var kicker = $('cv-sel-kicker');
      if (kicker) {
        kicker.hidden = true;
        kicker.textContent = '';
      }
      $('cv-seltext').innerHTML = '<b>' + fmt(sel.size) + '</b> / ' +
        fmt(D.n) + ' selected';

      // Give the geometry a compact biological identity. This is descriptive:
      // the dominant registered category and its observed share, not an inferred
      // interpretation of the selection.
      var profile = $('cv-selprofile'), compName = compGroupName();
      var g = catOf(compName), top = null, counts = {};
      if (g) {
        sel.forEach(function (i) {
          var lv = g.values[i];
          if (g.levels[+lv] != null) counts[lv] = (counts[lv] || 0) + 1;
        });
        Object.keys(counts).forEach(function (lv) {
          if (!top || counts[lv] > top[1]) top = [lv, counts[lv]];
        });
      }
      if (profile) {
        profile.textContent = top
          ? (g.levels[+top[0]] + ' · ' + Math.round(top[1] / sel.size * 100) + '%')
          : '';
        profile.hidden = !top;
      }

      var sourceSpace = selectionSourceSpace && spaceById[selectionSourceSpace];
      var sourceBaseId = (sourceSpace && sourceSpace._baseSpaceId) ||
        selectionSourceSpace;
      var sourceBaseSpace = sourceBaseId && spaceById[sourceBaseId];
      var origin = $('cv-selorigin');
      if (origin) {
        var sourceLabel = (sourceBaseSpace && sourceBaseSpace.label) || selectionSource;
        origin.textContent = selectionSource
          ? ('From ' + compactSpaceLabel(sourceSpace, selectionSource)) : '';
        origin.hidden = !selectionSource;
        if (selectionSource) {
          origin.title = 'Selected in ' + sourceLabel;
          origin.setAttribute('aria-label', 'Selected in ' + sourceLabel);
        } else {
          origin.removeAttribute('title');
          origin.removeAttribute('aria-label');
        }
      }

      // A cohort remains the same set even when one lens cannot place every
      // member. Report that coverage rather than silently making it look smaller.
      var mapped = [], seenSpaces = {};
      panels.forEach(function (p) {
        if (!p.spaceId || !p.ok) return;
        var space = spaceById[p.spaceId];
        var baseId = (space && space._baseSpaceId) || p.spaceId;
        if (baseId === sourceBaseId || seenSpaces[baseId]) return;
        seenSpaces[baseId] = true;
        var n = 0; sel.forEach(function (i) { if (p.ok[i]) n++; });
        var baseSpace = spaceById[baseId] || space;
        mapped.push({
          space: baseSpace,
          label: baseSpace ? baseSpace.label : baseId,
          count: n
        });
      });
      var coverage = $('cv-selcoverage');
      if (coverage) {
        coverage.textContent = '';
        coverage.hidden = !mapped.length;
        mapped.forEach(function (item) {
          var child = document.createElement('span');
          var description = item.label + ': ' + fmt(item.count) + ' of ' +
            fmt(sel.size) + ' selected cells mapped';
          child.className = 'cv-coverage-item';
          child.textContent = compactSpaceLabel(item.space, item.label) + ' ' +
            fmt(item.count);
          child.title = description;
          child.setAttribute('aria-label', description);
          coverage.appendChild(child);
        });
      }
    } else if (pick != null && nicheSet) {
      // A picked Trekker nucleus is the single-cell counterpart of an active
      // cohort. Keep it in the same shared-state surface so its identity,
      // neighbourhood, and Clear action remain visible across every lens.
      var pickKicker = $('cv-sel-kicker');
      if (pickKicker) {
        pickKicker.hidden = false;
        pickKicker.textContent = 'Active cell';
      }
      $('cv-seltext').innerHTML = 'Picked nucleus <b>' + esc(cellLabel(pick)) + '</b>';

      var pickProfile = $('cv-selprofile'), pickGroup = catOf(compGroupName());
      if (pickProfile) {
        pickProfile.hidden = false;
        var pickLevel = pickGroup && pickGroup.values[pick];
        pickProfile.textContent = pickGroup && pickGroup.levels[pickLevel] != null
          ? pickGroup.levels[pickLevel] : 'Single nucleus';
      }
      var pickOrigin = $('cv-selorigin');
      if (pickOrigin) {
        pickOrigin.hidden = false;
        pickOrigin.removeAttribute('title');
        pickOrigin.removeAttribute('aria-label');
        pickOrigin.textContent = 'Single-cell neighbourhood';
      }
      var pickCoverage = $('cv-selcoverage');
      if (pickCoverage) {
        pickCoverage.hidden = false;
        pickCoverage.removeAttribute('title');
        pickCoverage.removeAttribute('aria-label');
        pickCoverage.textContent = Math.max(0, nicheSet.size - 1) +
          ' neighbours within ' + nicheRadius + ' µm';
      }
    }
    revealEl(bar, !!((sel && sel.size) || (pick != null && nicheSet)));
    renderWorkspaceGuide();
  }
  // Trekker: cell-type composition of the picked nucleus's physical neighbours
  // within the niche radius (µm). Uses the physical (spatial) space coords. Shown
  // in place of the empty readout when a single nucleus is picked.
  // Compute the niche: the picked nucleus + every cell within `nicheRadius` µm of
  // it in the Trekker physical space. Doubles as the highlight set — cells in
  // it stay solid, everything else fades. Null unless a single nucleus is picked
  // (and no lasso selection is active). The distance loop is CBGeom.nicheAround
  // inclusive=<=, skipNaN=true (unpositioned cells align to NaN here).
  function trekkerSpace() {
    return window.CBViewState.spaceByRole(spaceById, 'trekker');
  }
  function panelIsTrekker(panel) {
    var space = panel && spaceById[panel.spaceId];
    return !!(space && (space.id === 'trekker' || space._role === 'trekker'));
  }
  function rebuildNiche() {
    nicheSet = null;
    if (pick == null || (sel && sel.size) || !D.trekker) return;
    var sp = trekkerSpace();
    if (!sp || !sp.x) return;
    var px = sp.x[pick], py = sp.y[pick];
    if (px == null || isNaN(px)) return;   // picked cell not positioned
    nicheSet = CBGeom.nicheAround(
      sp.x, sp.y, D.n, pick, px, py,
      nicheRadius * nicheRadius, true, true
    );
  }
  // Composition bar-chart HTML for a {levelIdx: count} map under categorical
  // group g, sorted descending, bars scaled to the top count. Shared by the
  // selection readout and the Trekker niche readout (identical markup).
  function compBars(comp, g) {
    var rows = Object.keys(comp).map(function (k) { return [parseInt(k, 10), comp[k]]; })
      .sort(function (a, b) { return b[1] - a[1]; });
    var mx = rows.length ? rows[0][1] : 1;
    return rows.map(function (r) {
      var col = cssColor((g.colors && g.colors[r[0]]) || PAL[r[0] % PAL.length]);
      return '<div class="cv-bar"><span class="cv-bar-nm" style="color:' + col + '">' +
        esc(g.levels[r[0]]) + '</span><span class="cv-bar-tr"><span class="cv-bar-fl" style="width:' +
        (r[1] / mx * 100).toFixed(1) + '%;background:' + col + '"></span></span>' +
        '<span class="cv-bar-ct">' + r[1] + '</span></div>';
    }).join('');
  }
  function renderNiche(host) {
    if (!nicheSet) return false;
    var compGroup = compGroupName();
    var g = catOf(compGroup); if (!g) return false;
    var comp = {}, tot = 0;
    nicheSet.forEach(function (i) {
      if (i === pick) return;   // the composition is of the neighbours
      var lv = g.values[i]; comp[lv] = (comp[lv] || 0) + 1; tot++;
    });
    var head = '<div class="cv-readcol cv-rise"><h4 class="cv-read-h">Niche of picked nucleus ' +
      '<span class="cv-read-sub">' + fmt(tot) + ' neighbours within ' + nicheRadius +
      ' µm · by ' + esc(groupLabel(compGroup)) + '</span></h4>';
    if (!tot) {
      host.innerHTML = head + '<div class="cv-empty-sm">No other nuclei within this ' +
        'radius — increase the niche radius.</div></div>';
      return true;
    }
    host.innerHTML = head + '<div class="cv-bars">' + compBars(comp, g) + '</div></div>';
    return true;
  }
  function median(values) {
    if (!values.length) return null;
    values.sort(function (a, b) { return a - b; });
    var m = Math.floor(values.length / 2);
    return values.length % 2 ? values[m] : (values[m - 1] + values[m]) / 2;
  }
  function fieldSummaryHtml() {
    var fld = fieldOf();
    if (!fld || fld.source !== 'trekker') return '';
    var desc = fld.desc
      ? '<div class="cv-field-desc">' + esc(fld.desc) + '</div>' : '';
    var g = catOf(compGroupName()), selected = !!(sel && sel.size), rows = [];
    if (g) {
      var all = {}, chosen = {}, i;
      for (i = 0; i < D.n; i++) {
        if (!visible(i)) continue;
        var value = fieldValue(fld, i); if (value == null) continue;
        var level = g.values[i];
        (all[level] || (all[level] = [])).push(value);
        if (selected && sel.has(i)) (chosen[level] || (chosen[level] = [])).push(value);
      }
      Object.keys(all).forEach(function (level) {
        rows.push({
          label: g.levels[+level],
          all: median(all[level]),
          selected: selected ? median(chosen[level] || []) : null
        });
      });
    } else if (fld.by_type && fld.by_type.length) {
      rows = fld.by_type.map(function (row) {
        return { label: row.type, all: +row.median, selected: null };
      });
    }
    rows.sort(function (a, b) { return (b.all || 0) - (a.all || 0); });
    var max = 0;
    rows.forEach(function (row) {
      max = Math.max(max, row.all || 0, row.selected || 0);
    });
    var table = rows.length ? '<div class="cv-field-table-title">Median by cell type' +
      (selected ? ' <span>Selected vs all visible cells</span>' : '') + '</div>' +
      '<div class="cv-field-table">' + rows.map(function (row) {
        var allWidth = max ? row.all / max * 100 : 0;
        var selectedWidth = max && row.selected != null ? row.selected / max * 100 : 0;
        return '<div class="cv-field-row"><div class="cv-field-type">' + esc(row.label) + '</div>' +
          '<div class="cv-field-bars"><div class="cv-field-track"><span style="width:' +
          allWidth.toFixed(1) + '%"></span></div>' +
          (selected ? '<div class="cv-field-track is-selected"><span style="width:' +
            selectedWidth.toFixed(1) + '%"></span></div>' : '') + '</div>' +
          '<div class="cv-field-values"><span>' + fmtVal(row.all) + '</span>' +
          (selected ? '<b>' + (row.selected == null ? '—' : fmtVal(row.selected)) + '</b>' : '') +
          '</div></div>';
      }).join('') + '</div>' : '';
    return '<div class="cv-readcol cv-rise cv-field-summary"><h4 class="cv-read-h">' +
      esc(fld.label || 'Trekker field') +
      ' <span class="cv-read-sub">physical field</span></h4>' + desc + table + '</div>';
  }
  // The niche radius only means something once a single nucleus is picked (and no
  // lasso selection is active), so its slider is disabled — with a hint tooltip —
  // until then.
  function updateNicheEnabled() {
    var on = (pick != null && !(sel && sel.size));
    setLinkedSliderDisabled('cv-niche', !on);
    var wrap = $('cv-niche-wrap');
    if (wrap) {
      wrap.classList.toggle('cv-disabled', !on);
      wrap.title = on ? '' : 'Click a nucleus in a panel first to set its niche';
    }
  }
  function requestCloneDetails(ids) {
    if (!D || !D.clone || !D.clone.details_deferred ||
        typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var missing = [];
    ids.forEach(function (raw) {
      var id = Number(raw);
      if (!Number.isInteger(id) || id < 0 || id >= D.clone.n_clones ||
          D.clone.label[id] != null || pendingCloneDetails.has(id)) return;
      pendingCloneDetails.add(id);
      missing.push(id);
    });
    if (!missing.length) return;
    Shiny.setInputValue('coordviews_clone_details_request', {
      dataset_context: D.dataset_context,
      dataset_fingerprint: configFingerprint(),
      ids: missing,
      nonce: Date.now()
    }, { priority: 'event' });
  }
  function renderReadout() {
    var host = $('cv-readout'); if (!host) return;
    host.style.display = '';
    updateNicheEnabled();
    if (!sel || !sel.size) {
      if (renderNiche(host)) return;   // Trekker: picked-nucleus niche composition
      var fieldOnly = fieldSummaryHtml();
      if (fieldOnly) { host.innerHTML = fieldOnly; return; }
      host.innerHTML = '';
      host.style.display = 'none';
      return;
    }
    var idxs = []; sel.forEach(function (i) { idxs.push(i); });

    // Composition by cell_type if present, else the active categorical colouring,
    // else the first available one. A data set with NO categorical column at all
    // (only numeric meta) has no composition to show — the clonotype readout and
    // the selected-cell panels below still work, so we simply omit this column
    // instead of throwing on g.values.
    var compGroup = compGroupName();
    var g = catOf(compGroup);
    var compHtml = '';
    if (g) {
      var comp = {};
      idxs.forEach(function (i) { var lv = g.values[i]; comp[lv] = (comp[lv] || 0) + 1; });
      // Same rise as the server-rendered plot/table below, first in the stagger.
      compHtml = '<div class="cv-readcol cv-rise"><h4 class="cv-read-h">Composition ' +
        '<span class="cv-read-sub">by ' + esc(groupLabel(compGroup)) + '</span></h4>' +
        '<div class="cv-bars">' + compBars(comp, g) + '</div></div>';
    }

    // top clonotypes among the selection (the immune axis in the loop)
    var cloneHtml = '';
    if (D.clone) {
      var cc = {};
      idxs.forEach(function (i) { var ci = D.clone.id[i]; if (ci >= 0) cc[ci] = (cc[ci] || 0) + 1; });
      var crows = Object.keys(cc).map(function (k) { return [parseInt(k, 10), cc[k]]; })
        .sort(function (a, b) { return b[1] - a[1]; }).slice(0, 8);
      requestCloneDetails(crows.map(function (row) { return row[0]; }));
      var withRcp = idxs.filter(function (i) { return D.clone.id[i] >= 0; }).length;
      if (crows.length) {
        cloneHtml = '<div class="cv-readcol cv-rise" style="--cv-rise-delay:60ms"><h4 class="cv-read-h">Top clonotypes in selection' +
          ' <span class="cv-read-sub">' + fmt(withRcp) + ' of ' + fmt(sel.size) +
          ' carry a receptor</span></h4><table class="cv-ctable"><thead><tr>' +
          '<th>#</th><th>Clonotype</th><th class="num">in sel.</th>' +
          '<th class="num">clone size</th><th class="num">% of sel.</th></tr></thead><tbody>' +
          crows.map(function (r, k) {
            var lab = D.clone.label[r[0]] || '(clone ' + r[0] + ')';
            if (lab.length > 42) lab = lab.slice(0, 40) + '…';
            // A clone is called on the V(D)J genes, and one such clone can cover
            // several CDR3s. The label names its dominant sequence; saying so is
            // the difference between a handle and a false identification, since
            // clicking the row selects every cell of the clone, not the ones
            // carrying that sequence.
            var nseq = (D.clone.n_cdr3 && D.clone.n_cdr3[r[0]]) || 1;
            var extra = nseq > 1
              ? ' <span class="cv-cdr3-more" title="' + esc(String(nseq)) +
                ' distinct CDR3 sequences in this clone">+' + (nseq - 1) + '</span>'
              : '';
            return '<tr class="cv-crow" data-clone="' + r[0] + '"><td class="num">' + (k + 1) +
              '</td><td class="cv-cdr3">' + esc(lab) + extra + '</td><td class="num">' + r[1] +
              '</td><td class="num">' + D.clone.size[r[0]] + '</td><td class="num">' +
              (r[1] / sel.size * 100).toFixed(1) + '%</td></tr>';
          }).join('') + '</tbody></table>' +
          '<div class="cv-hint">Click a clonotype row to select all its cells across every panel.</div></div>';
      } else {
        cloneHtml = '<div class="cv-readcol cv-rise" style="--cv-rise-delay:60ms"><h4 class="cv-read-h">Clonotypes</h4>' +
          '<div class="cv-empty-sm">No receptor-bearing cells in this selection.</div></div>';
      }
    }

    host.innerHTML = fieldSummaryHtml() + compHtml + cloneHtml;

    // wire clonotype-row -> select its cells (repertoire selection into the loop)
    Array.prototype.forEach.call(host.querySelectorAll('.cv-crow'), function (tr) {
      tr.onclick = function () {
        var cid = parseInt(tr.getAttribute('data-clone'), 10);
        var s = new Set();
        for (var i = 0; i < D.n; i++) if (D.clone.id[i] === cid) s.add(i);
        clearLassos();   // this selection didn't come from a lasso
        setSelection(s, 'Clonotype table');
      };
    });
  }

  // ---- legend (categorical) / colourbar (gene) ----------------------------
  function renderColorbar(show, maxVal, label, minVal) {
    var C = $('cv-cbar'); if (!C) return;
    C.classList.remove('cv-shared-range');
    C.style.display = show ? 'flex' : 'none';
    var g = $('cv-grad');
    if (g) g.style.display = '';
    if (!show) return;
    if (g) {
      var field = fieldOf();
      var stops = [];
      for (var i = 0; i <= 10; i++) {
        var color = field && field.palette
          ? field.palette[Math.round(i / 10 * 255)] : viridisCss(i / 10);
        stops.push(color + ' ' + (i * 10) + '%');
      }
      g.style.background = 'linear-gradient(90deg,' + stops.join(',') + ')';
    }
    // The bar has to show the range the COLOURS actually span. Printing the
    // data's min and max while the colours stop short of them would misreport
    // every cell at the ends: they saturate, and the reader would take the
    // brightest ones for the maximum. The chevrons say where that happens.
    var lo = (minVal == null ? 0 : +minVal), hi = (maxVal == null ? null : +maxVal);
    var r = clipRange(), clipped = false;
    if (r && hi != null) {
      var span = (colorBy === GENE_MODE) ? 255 : ((fieldOf() || {}).scale || 255);
      if (r.lo > 0 || r.hi < span) {
        var lo0 = lo, hi0 = hi;
        lo = lo0 + (hi0 - lo0) * (r.lo / span);
        hi = lo0 + (hi0 - lo0) * (r.hi / span);
        clipped = true;
      }
    }
    var cb0 = $('cv-cb0');
    if (cb0) cb0.textContent = (clipped ? '≤' : '') + lo.toFixed(1);
    var cb1 = $('cv-cb1');
    if (cb1) cb1.textContent = hi == null ? '' : (clipped ? '≥' : '') + hi.toFixed(1);
    var note = $('cv-cbar-note'); if (note) note.textContent = label || 'expression';
  }
  function renderPanelScales(show) {
    panels.forEach(function (panel) {
      if (!panel.pane) return;
      var old = panel.pane.querySelector('.cv-panel-scale');
      if (old) old.remove();
      if (!show || !panel.spaceId) return;
      var field = fieldForMode(panelColorMode(panel));
      if (!field || !field.palette) return;
      var stops = [0, 64, 128, 192, 255].map(function (at, index) {
        return field.palette[at] + ' ' + (index * 25) + '%';
      });
      var scale = document.createElement('span');
      scale.className = 'cv-panel-scale';
      scale.style.background = 'linear-gradient(90deg,' + stops.join(',') + ')';
      scale.title = field.rgbChannel
        ? field.label + ': 0 to ' + field.max + ' (channel maximum)'
        : field.label + ' colour scale; numeric range shared across panels';
      if (field.rgbChannel) {
        scale.setAttribute('aria-label', scale.title);
      }
      var title = panel.pane.querySelector('.cv-ptitle');
      if (title) title.insertAdjacentElement('afterend', scale);
    });
  }
  function renderSharedExpressionRange(field) {
    var C = $('cv-cbar'); if (!C || !field) return;
    C.style.display = 'flex';
    C.classList.add('cv-shared-range');
    var g = $('cv-grad'); if (g) g.style.display = 'none';
    var lo = $('cv-cb0');
    if (lo) lo.textContent = 'Shared expression range:';
    var hi = $('cv-cb1');
    if (hi) hi.textContent = Number(field.min).toFixed(1) + '–' +
      Number(field.max).toFixed(1);
    var note = $('cv-cbar-note'); if (note) note.textContent = '';
  }
  function renderLegend() {
    var L = $('cv-legend'); if (!L) return;
    var singlePayload = singleActive && singleView(singleActive);
    var hideSingleLegend = !!(singlePayload && singlePayload.meta &&
      singlePayload.meta.legend_position === 'none');
    L.style.display = hideSingleLegend ? 'none' : '';
    var singleFontSize = Number(singlePayload && singlePayload.meta &&
      singlePayload.meta.legend_font_size);
    L.style.fontSize = isFinite(singleFontSize) && singleFontSize > 0
      ? singleFontSize + 'px' : '';
    L.innerHTML = '';
    if (singlePayload && singlePayload.meta && singlePayload.meta.show_color_scale === false) {
      renderColorbar(false);
      renderPanelScales(false);
      return;
    }
    var distinctPanels = !!(singlePayload && singlePayload.data &&
      singlePayload.data.panel_colorscales &&
      Object.keys(singlePayload.data.panel_colorscales).length > 1);
    renderPanelScales(distinctPanels);
    function appendEvidenceLegend() {
      if (!evidenceOn) return;
      var e = document.createElement('div');
      e.className = 'cv-lg cv-rgb-lg';
      e.innerHTML = '<span class="cv-evidence-dot"></span>Positioning evidence';
      L.appendChild(e);
    }
    // Continuous modes: colourbar for a single gene; nothing for RGB.
    if (colorBy === GENE_MODE) {
      renderColorbar(!!D.gene, D.gene ? D.gene.max : null,
        D.gene ? (D.gene.gene + ' (log-normalised)') : 'select a gene');
      appendEvidenceLegend();
      return;
    }
    if (colorBy === GENE_PANELS_MODE) {
      var firstPanelField = panels.length ? fieldOf(panels[0]) : null;
      if (firstPanelField) renderSharedExpressionRange(firstPanelField);
      renderPanelScales($('cv-gene-panel-color') &&
        $('cv-gene-panel-color').value === 'distinct');
      return;
    }
    var fld = fieldOf();
    if (fld && fld.rgbChannel) {
      renderColorbar(false);
      renderPanelScales(true);
      panels.forEach(function (panel) {
        var field = fieldOf(panel);
        if (!panel.spaceId || !field || !field.rgbChannel) return;
        var range = document.createElement('span');
        range.className = 'cv-lg';
        range.textContent = field.label + ': 0–' + Number(field.max).toPrecision(3);
        L.appendChild(range);
      });
      return;
    }
    if (distinctPanels && fld) {
      renderSharedExpressionRange(fld);
      return;
    }
    if (fld && fld.colors) {
      renderColorbar(false);
      (fld.channelLabels || []).forEach(function (label, index) {
        var d = document.createElement('div'); d.className = 'cv-lg cv-rgb-lg';
        d.innerHTML = '<span class="cv-dot" style="background:' +
          cssColor((fld.channelColors || [])[index]) + '"></span>' + esc(label);
        L.appendChild(d);
      });
      return;
    }
    if (fld) {   // Trekker physical / meta field → viridis colourbar (min..max)
      renderColorbar(true, fld.max, fld.label, fld.min);
      appendEvidenceLegend();
      return;
    }
    if (colorBy === RGB_MODE) {
      // RGB needs both the channel assignments and their blends: without the
      // latter, yellow/cyan/magenta/black points have no visible explanation.
      renderColorbar(false);
      var chans = [['R', '#e11d1d'], ['G', '#12a150'], ['B', '#2563eb']];
      var genes = (D.rgb && D.rgb.genes) || ['', '', ''];
      chans.forEach(function (ch, k) {
        var gene = genes[k] || '';
        var d = document.createElement('div');
        d.className = 'cv-lg cv-rgb-lg' + (gene ? '' : ' off');
        d.innerHTML = '<span class="cv-dot" style="background:' + ch[1] + '"></span>' +
          '<b style="color:' + ch[1] + '">' + ch[0] + '</b> ' +
          (gene ? esc(gene) : '<span class="cv-rgb-none">not set</span>');
        L.appendChild(d);
      });
      [
        ['Low expression', RGB_GREY, ''],
        ['R + G', '#ffff00', ''],
        ['R + B', '#ff00ff', ''],
        ['G + B', '#00ffff', ''],
        ['R + G + B', '#000000', '']
      ].forEach(function (blend) {
        var d = document.createElement('div');
        d.className = 'cv-lg cv-rgb-lg';
        d.innerHTML = '<span class="cv-dot' + blend[2] + '" style="background:' +
          blend[1] + '"></span>' + blend[0];
        L.appendChild(d);
      });
      var note = document.createElement('div');
      note.className = 'cv-rgb-note';
      note.textContent = 'Each channel is scaled independently to its gene’s expression range.';
      L.appendChild(note);
      appendEvidenceLegend();
      return;
    }
    renderColorbar(false);
    var g = catOf(colorBy);
    if (!g) { appendEvidenceLegend(); return; }
    var counts = {};
    for (var i = 0; i < D.n; i++) counts[g.values[i]] = (counts[g.values[i]] || 0) + 1;
    g.levels.forEach(function (nm, li) {
      var col = cssColor((g.colors && g.colors[li]) || PAL[li % PAL.length]);
      var d = document.createElement('div');
      d.className = 'cv-lg' + (hidden.has(li) ? ' off' : '');
      d.innerHTML = '<span class="cv-dot" style="background:' + col + '"></span>' +
        esc(nm) + ' <span class="cv-lg-ct">(' + (counts[li] || 0) + ')</span>';
      d.onclick = function () {
        if (hidden.has(li)) hidden.delete(li); else hidden.add(li);
        reportSingleHiddenGroups(); renderLegend(); drawAll();
      };
      L.appendChild(d);
    });
    appendEvidenceLegend();
  }

  // ---- hover tooltip -------------------------------------------------------
  // Three depths, and each is reached deliberately from the one before:
  //
  //   hover   what the cell IS under the current colouring -- a glance, so it
  //           stays short. It follows the cursor and cannot be interacted with.
  //   pinned  after a click: the same, plus every registered grouping variable,
  //           and the two actions. It stops following the cursor, which is what
  //           makes those buttons reachable at all.
  //   card    everything, via Details.
  //
  // The hover used to carry the pinned set, which is a paragraph to read past
  // while moving the mouse and, on a data set with several grouping variables,
  // a tooltip taller than what it is pointing at.
  var HOVER_MAX_GROUPS = 6;
  function singleNestedValue(space, values, i) {
    if (!space || !space._hoverOffsets) return values && values[i];
    var offsets = space._hoverOffsets, lo = 0, hi = offsets.length - 1;
    while (lo + 1 < hi) {
      var mid = (lo + hi) >> 1;
      if (i < offsets[mid]) hi = mid; else lo = mid;
    }
    var group = values && values[lo];
    return group && group[i - offsets[lo]];
  }
  function singleHoverEnabledAt(space, i) {
    var modes = space && space._hoverModes;
    if (space && space._hoverMask && !space._hoverMask[i]) return false;
    if (!space || !space._hoverOffsets || !Array.isArray(modes)) {
      return modes !== 'skip';
    }
    var offsets = space._hoverOffsets, lo = 0, hi = offsets.length - 1;
    while (lo + 1 < hi) {
      var mid = (lo + hi) >> 1;
      if (i < offsets[mid]) hi = mid; else lo = mid;
    }
    return modes[lo] !== 'skip';
  }
  function singleHoverColumnValue(column, i, space) {
    var value = singleNestedValue(space, column.values, i);
    if (column.levels != null && value != null) {
      // JSON auto-unboxing can represent a one-level dictionary as a string.
      var levels = Array.isArray(column.levels) ? column.levels : [column.levels];
      value = levels[Number(value)];
    }
    if (value == null || (typeof value === 'number' && !isFinite(value))) {
      return 'NA';
    }
    if (column.format === 'integer') {
      var number = Number(value);
      return isFinite(number) ? Math.round(number).toLocaleString('en-US') : 'NA';
    }
    if (column.format === 'fixed') {
      var fixed = Number(value);
      return isFinite(fixed) ? fixed.toFixed(Number(column.digits) || 0) : 'NA';
    }
    return String(value);
  }
  function hoverHtml(i, pinned, panel) {
    var actions = pinned ? '<div class="cv-tip-act">' +
      '<button type="button" class="cv-tip-btn cv-tip-details">Details</button>' +
      '<button type="button" class="cv-tip-btn cv-tip-close" aria-label="Close">Close</button></div>' : '';
    if (singleActive && singleActive !== 'hla_motif_network') {
      var view = singleView(singleActive), mode = panelColorMode(panel);
      var lines = [];
      var cell = Array.isArray(D && D.cells) && D.cells[i];
      lines.push(cell ? 'Cell: ' + cell : 'Cell identifier available in Details');
      // The dictionary already present on the client for this view IS the
      // tooltip's content: pinning adds the Details/Close actions, it must not
      // decide whether the columns are shown. Nothing is requested here.
      var space = panel && spaceById[panel.spaceId] || spaceById[singleSpaceIds[0]];
      var awaiting = !!(view && view.hover && view.hover.pending);
      var columns = space && space._hoverColumns;
      if (!awaiting && Array.isArray(columns) && columns.length) {
        columns.slice(0, HOVER_MAX_GROUPS).forEach(function (column) {
          var line = column.label + ': ' + singleHoverColumnValue(column, i, space);
          if (lines.indexOf(line) < 0) lines.push(line);
        });
        return '<div class="cv-tip-row">' + esc(lines.join('\n')).replace(/\n/g, '<br>') + '</div>' + actions;
      }
      // Without a dictionary, summarise what the client already knows: the
      // active level and the continuous variable in play.
      var category = catOf(mode), field = fieldOf(panel);
      if (category && category.values) {
        var level = category.levels[category.values[i]];
        lines.push(((view && view.meta && view.meta.color_variable) || groupLabel(mode)) +
          ': ' + (level == null ? 'NA' : level));
      }
      if (field) {
        var number = fieldValue(field, i);
        if (number != null) lines.push(field.label + ': ' + fmtVal(number));
      }
      return '<div class="cv-tip-row">' + esc(lines.join('\n')).replace(/\n/g, '<br>') + '</div>' + actions;
    }
    if (singleActive) {
      var singleSpace = spaceById[singleSpaceIds[0]];
      var activeSingleView = singleView(singleActive);
      if (activeSingleView && activeSingleView.hover && activeSingleView.hover.pending) {
        var basic = ['Cell: ' + cellLabel(i)];
        var group = catOf(colorBy), value = fieldOf();
        if (group) {
          var level = group.levels[group.values[i]];
          basic.push(activeSingleView.meta.color_variable + ': ' + (level == null ? 'NA' : level));
        }
        if (value && value.v) {
          var numeric = fieldValue(value, i);
          basic.push(value.label + ': ' + (numeric == null ? 'NA' : fmtVal(numeric)));
        }
        basic.push('Additional metadata loading…');
        return '<div class="cv-tip-row">' +
          esc(basic.join('\n')).replace(/\n/g, '<br>') + '</div>';
      }
        var columns = singleSpace && singleSpace._hoverColumns;
        if (Array.isArray(columns) && columns.length) {
          var lines = ['Cell: ' + cellLabel(i)];
          columns.forEach(function (column) {
            var value = singleHoverColumnValue(column, i, singleSpace);
            if (value !== '') lines.push(column.label + ': ' + value);
          });
        return '<div class="cv-tip-row">' +
          esc(lines.join('\n')).replace(/\n/g, '<br>') + '</div>';
      }
      var raw = singleSpace && singleNestedValue(singleSpace, singleSpace._hover, i);
      var text = String(raw || cellLabel(i))
        .replace(/<br\s*\/?>/gi, '\n').replace(/<[^>]*>/g, '');
      return '<div class="cv-tip-row">' + esc(text).replace(/\n/g, '<br>') + '</div>';
    }
    var rows = [];
    var g = catOf(panelColorMode(panel));
    // headline = the active categorical level, else the barcode
    var head = g ? g.levels[g.values[i]] : cellLabel(i);
    var h = '<div class="cv-tip-row"><b>' + esc(head) + '</b></div>';
    if (g) rows.push(['Cell', cellLabel(i)]);
    // the continuous variable in play, at its true value
    var fld = fieldOf(panel);
    if (fld) rows.push([fld.label, fmtVal(fieldValue(fld, i))]);
    if (colorBy === GENE_MODE && D.gene) {
      rows.push([D.gene.gene, fmtVal(D.gene.v[i] / 255 * D.gene.max)]);
    }
    if (pinned) {
      // every grouping variable, as the Projection tab does (capped so a data
      // set with many registered groups cannot outgrow the panel)
      var gn = D.groups ? Object.keys(D.groups) : [];
      gn.slice(0, HOVER_MAX_GROUPS).forEach(function (k) {
        if (k === colorBy) return;
        var gg = D.groups[k];
        // Pinned summaries show available groups without loading deferred columns.
        // The Details action already requests the complete metadata row on demand.
        if (!gg || gg.values == null) return;
        rows.push([groupLabel(k), gg.levels[gg.values[i]]]);
      });
    }
    rows.forEach(function (r) {
      if (r[1] == null || r[1] === '') return;
      h += '<div class="cv-tip-row"><span class="cv-tip-k">' + esc(r[0]) +
        ':</span> ' + esc(r[1]) + '</div>';
    });
    if (pinned) {
      // The way to the full record, offered rather than imposed. Clicking a cell
      // used to throw a large card over the middle of the workspace -- a lot of
      // screen for a question the user may not have asked, landing on top of the
      // panels at the moment a Trekker user was clicking nuclei to read their
      // niche. Close dismisses this tooltip and nothing else: the pick it came
      // from stays, because the niche readout is what the click was for.
      h += actions;
    }
    return h;
  }
  // ---- single-cell detail card ---------------------------------------------
  // Clicking a cell promotes the hover tooltip into a card parked in the middle
  // of the panel grid: the same facts plus everything the tooltip has to cut
  // (the full CDR3, every meta column, the cell's coordinates in each space),
  // held still so it can be read and copied.
  //
  // The card opens with what the client already has and asks the server for the
  // complete meta row in parallel — the bundle carries categorical levels and
  // quantised numerics, not the original values, and a cell's meta row is
  // exactly the kind of thing worth being exact about.
  var cardCell = null;          // cell index the card is showing, or null
  var cardMeta = null;          // { cell, rows } from the server, when it lands
  var cardRequest = null, cardRequestSerial = 0;

  function requestCardMetadata() {
    if (!cardOpen() || !D || (cardRequest && cardRequest.sent)) return;
    var cell = Array.isArray(D.cells) && D.cells[cardCell];
    if (cell == null) { requestSingleAux(); return; }
    var request = cardRequest = {
      request_id: ++cardRequestSerial, cell: String(cell), client_index: cardCell,
      view_id: singleActive || 'linked', dataset_context: D.dataset_context, sent: true
    };
    Shiny.setInputValue('coordviews_cell_detail', request, { priority: 'event' });
  }

  function receiveCardMetadata(message) {
    if (!D || !cardOpen() || !cardRequest || !message || !datasetMessageFresh(message) ||
        !sameDatasetContext(message.dataset_context, D.dataset_context) ||
        message.request_id !== cardRequest.request_id || message.view_id !== (singleActive || 'linked') ||
        message.cell !== cardRequest.cell || Number(message.client_index) !== cardCell) return;
    cardMeta = { index: cardCell, cell: message.cell, rows: message.rows || [], error: message.error };
    renderCard();
  }

  function cardOpen() { return cardCell != null; }

  function cardCoordRows() {
    var rows = [];
    orderedSpaces().forEach(function (id) {
      var sp = spaceById[id];
      if (!sp) return;
      var x = sp.x[cardCell], y = sp.y[cardCell];
      rows.push([sp.label, (x == null || isNaN(x)) ? 'not positioned'
        : (fmtVal(x) + ',  ' + fmtVal(y))]);
    });
    return rows;
  }

  // What Trekker knows about THIS nucleus's position. Empty for a data set that
  // carries none, so the card gains a section only where there is one to gain.
  //
  // The physical fields are read out of the same list the colour picker offers,
  // which is what keeps this honest: whatever Trekker exported -- bead noise,
  // spatial-barcode counts, purity -- appears here without this code being told
  // its name, and cannot disagree with what colouring by it would show.
  function trekkerCellRows(i) {
    var rows = [];
    if (!D || !D.trekker) return rows;
    var tk = D.trekker;
    // Trekker's own physical fields FIRST, and they already include
    // position_confidence -- it is one of the colourings. Adding a row from
    // D.trekker.conf as well printed that number twice under two names. `conf`
    // is still what the dissolve slider indexes; it is just not a separate fact.
    Object.keys(D.fields || {}).forEach(function (k) {
      if (k.indexOf('meta:') === 0) return;   // a meta column, not Trekker's own
      var f = D.fields[k];
      var v = fieldValue(f, i);
      if (v == null) return;
      rows.push([f.label || k, fmtVal(v)]);
    });
    // ... then the rest of what was recorded about the placement, named and
    // formatted as the dedicated Trekker page names it, so a reader moving
    // between the two is reading the same quantities.
    var pct = function (v) {
      return (v == null || isNaN(v)) ? null : Math.round(v * 100) + '%';
    };
    if (tk.conf_noise) {
      var noise = pct(tk.conf_noise[i]);
      if (noise != null) rows.push(['bead noise', noise]);
    }
    if (tk.conf_sb && tk.conf_sb[i] != null && !isNaN(tk.conf_sb[i])) {
      rows.push(['spatial barcodes', fmt(tk.conf_sb[i])]);
    }
    if (tk.evidence) {
      rows.push(['positioning evidence',
        tk.evidence[i] === 1 ? 'recorded' : 'none']);
    }
    return rows;
  }

  function kvHtml(rows) {
    if (!rows.length) return '';
    return '<div class="cv-card-kv">' + rows.map(function (r) {
      return '<div class="cv-card-k">' + esc(r[0]) + '</div>' +
        '<div class="cv-card-v">' + esc(r[1]) + '</div>';
    }).join('') + '</div>';
  }

  function renderCard() {
    if (cardCell == null || !D) return;
    var i = cardCell;
    var g = catOf(colorBy);
    var t = $('cv-card-title'), b = $('cv-card-bc'), body = $('cv-card-body');
    var it = $('cv-tk-cell-title'), ib = $('cv-tk-cell-bc');
    var inspectorBody = $('cv-tk-cell-body');
    var barcode = cardMeta && cardMeta.index === i ? cardMeta.cell : cellLabel(i);
    if (t) t.textContent = g ? g.levels[g.values[i]] : 'Cell';
    if (b) b.textContent = barcode;
    if (it) it.textContent = g ? g.levels[g.values[i]] : 'Cell';
    if (ib) ib.textContent = barcode;
    if (!body && !inspectorBody) return;
    var html = '';
    var inspectorSections = [];
    var inspectorClass = {
      position: 'cv-tk-cell-block--position',
      positioning: 'cv-tk-cell-block--positioning',
      evidence: 'cv-tk-cell-block--evidence',
      metadata: 'cv-tk-cell-block--metadata',
      clonotype: 'cv-tk-cell-block--clonotype'
    };
    var addSection = function (key, label, content) {
      html += '<div class="cv-card-sec">' + esc(label) + '</div>' + content;
      inspectorSections.push(
        '<section class="cv-tk-cell-block ' + inspectorClass[key] + '">' +
        '<div class="cv-card-sec">' + esc(label) + '</div>' + content +
        '</section>'
      );
    };
    // clonotype, in full — the tooltip can only ever show a prefix of this
    if (D.clone && D.clone.id[i] >= 0) {
      var ci = D.clone.id[i];
      requestCloneDetails([ci]);
      addSection('clonotype', 'Clonotype',
        '<div class="cv-card-seq">' + esc(D.clone.label[ci] || 'Loading…') + '</div>' +
        '<div class="cv-card-sub">' + fmt(D.clone.size[ci]) +
        ' cells in this clone</div>');
    }
    addSection('position', 'Position', kvHtml(cardCoordRows()));
    // Trekker: how much to trust where this nucleus was put. A physical position
    // is an inference, not a measurement, and the card is where a reader decides
    // whether to believe the one they just clicked -- otherwise the only way to
    // ask was to colour the whole map by confidence and squint at one dot.
    // Everything here is already in the bundle: the per-cell confidence, the
    // evidence flag, and Trekker's own physical fields, which are offered as
    // colourings and therefore carry a value for every cell.
    var tkRows = trekkerCellRows(i);
    if (tkRows.length) {
      addSection('positioning', 'Positioning', kvHtml(tkRows));
    }
    var evidenceImg = D.trekker && D.trekker.evidence_img &&
      D.trekker.evidence_img[i];
    if (evidenceImg && /^data:image\//.test(evidenceImg)) {
      addSection('evidence', 'Positioning evidence',
        '<button type="button" class="cv-evidence-thumb" ' +
        'data-cell="' + esc(cellLabel(i)) + '" aria-label="Enlarge positioning evidence">' +
        '<img src="' + esc(evidenceImg) + '" alt="Positioning evidence for ' +
        esc(cellLabel(i)) + '"></button>' +
        '<div class="cv-card-sub">Why this nucleus was placed here. Click to enlarge.</div>');
    } else if (D.trekker && D.trekker.evidence_deferred &&
        D.trekker.evidence && D.trekker.evidence[i] === 1) {
      requestDeferredAsset('trekker-evidence:' + i);
      addSection('evidence', 'Positioning evidence',
        '<div class="cv-card-skel"><span></span><span></span></div>');
    }
    // the full meta row, or a placeholder until the server answers
    var metaHtml = '';
    if (cardMeta && cardMeta.index === i && cardMeta.rows) {
      metaHtml = cardMeta.error ? '<div class="cv-card-sub">' + esc(cardMeta.error) + '</div>' :
        kvHtml(cardMeta.rows.map(function (r) { return [r.k, r.v]; }));
    } else {
      metaHtml = '<div class="cv-card-skel"><span></span><span></span><span></span></div>';
    }
    addSection('metadata', 'Meta data', metaHtml);
    if (body) body.innerHTML = html;
    if (inspectorBody) inspectorBody.innerHTML = inspectorSections.join('');
    var empty = $('cv-tk-cell-empty'), content = $('cv-tk-cell-content');
    if (empty) empty.style.display = 'none';
    if (content) content.style.display = '';
    // The meta row arriving makes the card taller, so where it was centred is no
    // longer the centre. Re-centre (left/top are transitioned, so it glides).
    if (cardOpen() && $('cv-card').classList.contains('is-open')) centreCard();
  }

  // Centre the card on the VISIBLE part of the panel grid, and keep it inside
  // the grid's bounds. Centring on the whole grid is wrong whenever the grid is
  // taller than the window — the card then opens below the fold, which on a
  // small screen looks exactly like nothing happened. Clamped as well, so a
  // short grid cannot push it out the other side.
  function centreCard() {
    var card = $('cv-card'), host = document.querySelector('.cv-panes');
    if (!card || !host) return;
    var hr = host.getBoundingClientRect();
    var vw = window.innerWidth, vh = window.innerHeight;
    var l = Math.max(hr.left, 0), rgt = Math.min(hr.right, vw);
    var t = Math.max(hr.top, 0), b = Math.min(hr.bottom, vh);
    // no overlap with the viewport at all → fall back to the grid's own centre
    if (rgt <= l || b <= t) { l = hr.left; rgt = hr.right; t = hr.top; b = hr.bottom; }
    var m = 10;
    // Cap the height to the band that is actually visible BEFORE measuring. The
    // stylesheet can only cap against the grid and the viewport as wholes, which
    // says nothing about how much of the grid is on screen — and when the grid
    // starts near the bottom of the window, there is no position that fits a
    // card sized against either. Capping first means a fit always exists; the
    // body scrolls instead.
    card.style.maxHeight = Math.max(200, Math.min(560, (b - t) - 2 * m)) + 'px';
    var cw = card.offsetWidth, ch = card.offsetHeight;
    // Clamped against BOTH boxes: inside the grid it belongs to, and on screen.
    // The grid alone is not enough — it can extend well past the bottom of the
    // window, and a card merely "inside the grid" can still be off screen.
    var clamp = function (v, lo, hi) {
      return hi < lo ? lo : Math.max(lo, Math.min(v, hi));
    };
    var x = clamp((l + rgt) / 2 - hr.left - cw / 2,
      Math.max(m, m - hr.left),
      Math.min(hr.width - cw - m, vw - cw - m - hr.left));
    var y = clamp((t + b) / 2 - hr.top - ch / 2,
      Math.max(m, m - hr.top),
      Math.min(hr.height - ch - m, vh - ch - m - hr.top));
    card.style.left = Math.round(x) + 'px';
    card.style.top = Math.round(y) + 'px';
  }

  // Fly in FROM the clicked point: the card is placed and measured where it will
  // land, then offset back onto the point and released, so it travels under its
  // own transform. Position and scale only — no layout is touched mid-flight.
  function cardFlyFrom(p, i) {
    var card = $('cv-card'), host = document.querySelector('.cv-panes');
    if (!card || !host) return;
    card.classList.add('is-open');
    card.classList.remove('is-in');
    centreCard();                       // land it before measuring the landing
    var cr = card.getBoundingClientRect();
    var from = null;
    if (p && p.ok && p.canvas) {
      var canv = p.canvas.getBoundingClientRect();
      from = {
        x: canv.left + pointScreenX(p, i),
        y: canv.top + pointScreenY(p, i)
      };
    }
    // The start state must be written with transitions OFF. Left on, the browser
    // coalesces "jump to the point" and "go back to centre" into a single
    // no-op change and the card simply appears, already landed.
    card.style.transition = 'none';
    if (from) {
      var dx = from.x - (cr.left + cr.width / 2);
      var dy = from.y - (cr.top + cr.height / 2);
      card.style.transform = 'translate(' + dx + 'px,' + dy + 'px) scale(.28)';
    } else {
      card.style.transform = 'scale(.9)';
    }
    void card.offsetWidth;        // commit the start state
    card.style.transition = '';   // hand the transition back to the stylesheet
    card.style.transform = '';
    card.classList.add('is-in');
  }

  function openCard(p, i) {
    if (D && D.trekker) {
      openTrekkerInspector(i);
      return;
    }
    cardCell = i;
    cardRequest = null;
    if (!cardMeta || cardMeta.index !== i) cardMeta = null;
    renderCard();
    // The tooltip has just been promoted into the card — leaving it up would
    // show the same cell twice, once truncated. It returns on the next hover.
    unpinTip();
    cardFlyFrom(p, i);
    // ask for the exact meta row; the card is already on screen either way
    requestCardMetadata();
  }

  function openTrekkerInspector(i) {
    cardCell = i;
    cardRequest = null;
    if (!cardMeta || cardMeta.index !== i) cardMeta = null;
    renderCard();
    openTrekkerInsights('cell');
    requestCardMetadata();
  }

  function closeCard() {
    var card = $('cv-card');
    cardCell = null;
    cardRequest = null;
    var empty = $('cv-tk-cell-empty'), content = $('cv-tk-cell-content');
    if (empty) empty.style.display = '';
    if (content) content.style.display = 'none';
    if (!card || !card.classList.contains('is-open')) return;
    card.classList.remove('is-in');
    // let the fade finish before it leaves the flow
    setTimeout(function () {
      if (cardCell == null) { card.classList.remove('is-open'); card.style.transform = ''; }
    }, 220);
  }

  // Place a panel's tooltip beside cell `i`: above-right, flipping to the other
  // side of either axis when that would overflow, then clamped into the canvas
  // on both. Flipping alone is not enough: near a corner the flipped position
  // can overflow the OTHER edge, which is how a tooltip ends up half outside the
  // panel with its labels cut off — leaving the values on screen with nothing to
  // say what they are.
  function placeTip(p, tip, i) {
    var tw = tip.offsetWidth, th = tip.offsetHeight, m = 4;
    var px = pointScreenX(p, i), py = pointScreenY(p, i);
    var tx = px + 14, ty = py - th - 10;
    if (tx + tw > p.W - m) tx = px - tw - 14;   // flip left
    if (ty < m) ty = py + 14;                   // flip below
    tx = Math.max(m, Math.min(tx, p.W - tw - m));
    ty = Math.max(m, Math.min(ty, p.H - th - m));
    tip.style.left = tx + 'px'; tip.style.top = ty + 'px';
  }

  // Pin a panel's tooltip to a cell: it stops following the cursor, gains the
  // grouping variables and the two actions, and stays until closed. This is what
  // makes the buttons usable -- a tooltip that tracks the pointer moves out from
  // under any attempt to click it.
  function pinTip(p, i) {
    var tip = $(p.tipId); if (!tip) return;
    pinnedTip = { panel: p, cell: i };
    tip.classList.add('cv-tip-pinned');
    tip.innerHTML = hoverHtml(i, true, p);
    var detailHandled = false;
    var runDetails = function (e) {
      e.preventDefault(); e.stopPropagation();
      if (detailHandled) return;
      detailHandled = true;
      openCard(p, i);
    };
    var details = tip.querySelector('.cv-tip-details');
    if (details) {
      details.addEventListener('pointerdown', runDetails);
      details.addEventListener('click', runDetails);
    }
    var closeHandled = false;
    var runClose = function (e) {
      e.preventDefault(); e.stopPropagation();
      if (closeHandled) return;
      closeHandled = true;
      unpinTip();
    };
    var close = tip.querySelector('.cv-tip-close');
    if (close) {
      close.addEventListener('pointerdown', runClose);
      close.addEventListener('click', runClose);
    }
    tip.style.opacity = 1;
    placeTip(p, tip, i);
  }
  function unpinTip() {
    var p = pinnedTip.panel;
    pinnedTip = { panel: null, cell: null };
    if (!p) return;
    var tip = $(p.tipId); if (!tip) return;
    tip.classList.remove('cv-tip-pinned');
    tip.style.opacity = 0;
  }
  // Keep a pinned tooltip on its cell through pans, zooms and rotations, and
  // take it away when that cell leaves the panel — a tooltip clamped to an edge,
  // pointing at nothing, is worse than none.
  function repositionPinned(p) {
    if (pinnedTip.panel !== p) return;
    var i = pinnedTip.cell, tip = $(p.tipId);
    if (!tip) return;
    var x = pointScreenX(p, i), y = pointScreenY(p, i);
    if (x == null || isNaN(x) || x < 0 || y < 0 || x > p.W || y > p.H) {
      tip.style.opacity = 0;
      return;
    }
    tip.style.opacity = 1;
    placeTip(p, tip, i);
  }

  // Mark the hovered cell everywhere. The rings have their own lightweight
  // canvas, so a change never redraws clouds or trajectory underlays.
  function setHoverCell(i) {
    if (hoverCell === i) return;
    hoverCell = i;
    drawHoverAll();
  }

  function interactionIsStatic() {
    return singleActive ? singleInteractionIsStatic() : false;
  }

  function syncLinkedHoverDefault(dataChanged) {
    // This is a user preference, not a dataset display default.
    var control = $('cv-hover');
    if (control) control.checked = linkedHoverOn;
  }

  function setLinkedHover(enabled) {
    linkedHoverOn = !!enabled;
    syncLinkedHoverDefault(false);
    setHoverCell(null);
    panels.forEach(refreshPanelHover);
  }

  function setCellTooltips(enabled) {
    cellTooltipsOn = !!enabled;
    var control = $('hover_info_in_projections_checkbox');
    if (control) control.checked = cellTooltipsOn;
    panels.forEach(function (panel) {
      var tip = $(panel.tipId);
      if (!cellTooltipsOn && pinnedTip.panel !== panel && tip) {
        tip.style.opacity = 0;
      }
    });
    setHoverCell(null);
    panels.forEach(refreshPanelHover);
  }

  function syncLinkedBackgroundsDefault(dataChanged) {
    if (dataChanged) linkedBackgroundsOn = true;
    var control = $('cv-backgrounds');
    if (control) control.checked = linkedBackgroundsOn;
  }

  function setLinkedBackgrounds(enabled) {
    linkedBackgroundsOn = !!enabled;
    syncLinkedBackgroundsDefault(false);
    if (!singleActive) {
      panels.forEach(function (panel) { panel.miniBg = null; });
      renderImagePicker();
      drawAll();
      viewOperation('linked', linkedBackgroundsOn
        ? 'Background images shown' : 'Background images hidden');
    }
  }

  function refreshPanelHover(p) {
    var tip = $(p.tipId);
    if (!p.hoverPointer) {
      if (tip && pinnedTip.panel !== p) tip.style.opacity = 0;
      return;
    }
    if (interactionIsStatic()) return;
    // Point tooltips, including HLA motif nodes, share the global preference.
    // Cross-view highlighting remains independent of tooltip text.
    var showText = cellTooltipsOn;
    var crossHighlight = !singleActive && linkedHoverOn;
    if (!showText && !crossHighlight) {
      if (tip && pinnedTip.panel !== p) tip.style.opacity = 0;
      setHoverCell(null); return;
    }
    var space = singleActive && spaceById[p.spaceId];
    if (singleActive === 'hla_motif_network' && space && space._hoverEnabled === false) {
      tip.style.opacity = 0; setHoverCell(null); return;
    }
    var r = p.canvas.getBoundingClientRect();
    var mx = p.hoverPointer.x - r.left, my = p.hoverPointer.y - r.top;
    // A pinned tooltip owns this panel's tooltip element until it is closed —
    // but the cross-panel mark still follows the cursor.
    var own = pinnedTip.panel !== p;
    if (p.drag || p.panning || p.orbiting) {
      if (own) tip.style.opacity = 0;
      setHoverCell(null);
      return;
    }
    var i = nearest(p, mx, my);
    if (i < 0 || (singleActive === 'hla_motif_network' && space && !singleHoverEnabledAt(space, i))) {
      if (own) tip.style.opacity = 0;
      setHoverCell(null);
      return;
    }
    // These views paint with indices before stable IDs are needed. A real point
    // hover must request them, and requestAux() deduplicates while one is in
    // flight, so sweeping the pointer across the cloud asks once.
    requestSingleAux();
    if (hoverPanel !== p) { hoverPanel = p; drawHoverAll(); }
    setHoverCell(i);
    if (!own) return;
    if (!showText) { tip.style.opacity = 0; return; }
    tip.innerHTML = hoverHtml(i, false, p); tip.style.opacity = 1;
    placeTip(p, tip, i);
  }

  function wireHover(p) {
    var tip = $(p.tipId);
    p.canvas.addEventListener('mousemove', function (e) {
      p.hoverPointer = { x: e.clientX, y: e.clientY };
      refreshPanelHover(p);
    });
    p.canvas.addEventListener('mouseleave', function () {
      p.hoverPointer = null;
      hoverPanel = null;
      setHoverCell(null);
      if (pinnedTip.panel === p) return;
      tip.style.opacity = 0;
    });
  }

  // ---- brush + pick --------------------------------------------------------
  function pickCell(p, index) {
    requestSingleAux();
    pick = index >= 0 && index !== pick ? index : null;
    closeCard(); unpinTip();
    if (pick != null) pinTip(p, pick);
    // Picking still drives Trekker's niche readout. Full metadata is opened
    // deliberately through Details, as on the other cell views.
    rebuildNiche(); updateSelActions(); renderSelbar(); drawAll();
    if (!sel) renderReadout();
  }

  function clearTransientHover() {
    hoverPanel = null;
    setHoverCell(null);
    panels.forEach(function (panel) {
      panel.hoverPointer = null;
      var tip = $(panel.tipId);
      if (tip && pinnedTip.panel !== panel) tip.style.opacity = 0;
    });
  }

  function wireBrush(p) {
    var brushTarget = p.pane || p.canvas;
    var pos = function (e) {
      var r = p.canvas.getBoundingClientRect();
      return [e.clientX - r.left, e.clientY - r.top];
    };
    // A tap pins a cell without consuming the touch gesture used to scroll.
    // Ignore the browser's subsequent compatibility mouse events for that tap.
    p.canvas.addEventListener('pointerdown', function (e) {
      if (e.pointerType !== 'touch' || e.isPrimary === false || interactionIsStatic()) return;
      p.touchStart = {x:e.clientX,y:e.clientY,id:e.pointerId};
      clearTransientHover();
    });
    p.canvas.addEventListener('pointercancel', function () { p.touchStart = null; });
    p.canvas.addEventListener('pointerup', function (e) {
      var start = p.touchStart; p.touchStart = null;
      if (!start || start.id !== e.pointerId) return;
      p.ignoreMouseUntil = Date.now() + 700;
      if (Math.hypot(e.clientX - start.x, e.clientY - start.y) > 6) return;
      var point = pos(e); pickCell(p, nearest(p, point[0], point[1]));
    });
    brushTarget.addEventListener('mousedown', function (e) {
      if (Date.now() < (p.ignoreMouseUntil || 0)) return;
      if (interactionIsStatic()) return;
      if (e.target.closest && e.target.closest('button, select, input, a, .cv-tip')) return;
      clearTransientHover();
      requestSingleAux();
      if (isSpatialSpace(spaceById[p.spaceId])) activateSpatial(p.spaceId);
      // A rotatable panel NAVIGATES; it does not select. Selection here is done
      // on screen coordinates, and once the cloud has depth those stop being a
      // faithful key to it: cells at different depths overlap on screen, so a
      // lasso would take in cells the user cannot see it taking in, and the set
      // it produced would change with the viewing angle. That is a selection
      // nobody can check. Selecting happens on a flat panel and highlights here.
      //
      // Hiding the box/lasso buttons is NOT enough on its own: selectMode is
      // shared by every panel, so a lasso chosen on a flat panel would still
      // arm a drag on this one. The mode is therefore overridden at the event,
      // and a drag that would have selected orbits instead — the gesture stays
      // useful rather than dying silently.
      var dragKind = CBGeom.dragKind(
        selectMode,
        panelIs3D(p),
        e.button,
        e.shiftKey
      );
      if (dragKind === 'orbit') {
        e.preventDefault();
        p.orbiting = true; p.orbitFrom = pos(e);
        p.moved = false;
        p.orbitBase = p.rot ? { rx: p.rot.rx, ry: p.rot.ry } : { rx: 0, ry: 0 };
        p.canvas.classList.add('cv-grabbing');
        return;
      }
      // Pan: the toolbar's hand mode, or middle-drag / shift-drag from any mode
      // (the shortcut plotly users reach for without switching tools). Orbit
      // mode lands here too when the panel is FLAT — there is nothing to turn,
      // and panning is the nearest useful reading of the same drag.
      if (dragKind === 'pan') {
        e.preventDefault();
        p.panning = true; p.panFrom = pos(e);
        p.moved = false;
        p.panView = p.view
          ? { cx: p.view.cx, cy: p.view.cy, span: p.view.span }
          : { cx: 0.5, cy: 0.5, span: 1 };
        p.canvas.classList.add('cv-grabbing');
        return;
      }
      // a fresh brush supersedes any committed lasso (this panel's is replaced,
      // the other panel's is dropped)
      panels.forEach(function (o) {
        if (o !== p) { o.lasso = null; o.lassoData = null; }
      });
      p.drag = true; p.moved = false; p.start = pos(e); p.lasso = [p.start]; p.lassoData = null;
    });
    window.addEventListener('mousemove', function (e) {
      if (p.orbiting) {
        var oq = pos(e), RAD = 0.009;   // radians per pixel dragged
        if (Math.hypot(oq[0] - p.orbitFrom[0], oq[1] - p.orbitFrom[1]) > 4) p.moved = true;
        p.rot = {
          ry: p.orbitBase.ry + (oq[0] - p.orbitFrom[0]) * RAD,
          // Pitch stops short of ±90°: past vertical the cloud reads as
          // upside-down and the drag direction appears to invert.
          rx: Math.max(-1.4, Math.min(1.4,
            p.orbitBase.rx + (oq[1] - p.orbitFrom[1]) * RAD))
        };
        project(p); draw(p);
        return;
      }
      if (p.panning) {
        var pq = pos(e), SX = p._SX || 1, SY = p._SY || 1, v = p.panView;
        if (Math.hypot(pq[0] - p.panFrom[0], pq[1] - p.panFrom[1]) > 4) p.moved = true;
        // screen delta -> view units; y is inverted (canvas y grows downward).
        // Clamped, so dragging on past the edge simply stops instead of sailing
        // off into blank canvas.
        p.view = clampView(p, CBGeom.panView(
          v,
          (pq[0] - p.panFrom[0]) / SX,
          (pq[1] - p.panFrom[1]) / SY
        ));
        project(p); draw(p);
        return;
      }
      if (!p.drag) return;
      var q = pos(e);
      if (selectMode === 'box') {
        // rectangle from the drag origin to the cursor; inPoly treats it as a
        // 4-point polygon, so mouseup selection is unchanged.
        var a = p.start;
        if (Math.abs(q[0] - a[0]) + Math.abs(q[1] - a[1]) > 3) {
          p.lasso = [[a[0], a[1]], [q[0], a[1]], [q[0], q[1]], [a[0], q[1]]];
          p.moved = true; draw(p);
        }
      } else {
        var last = p.lasso[p.lasso.length - 1];
        if (Math.abs(q[0] - last[0]) + Math.abs(q[1] - last[1]) > 3) {
          p.lasso.push(q); p.moved = true; draw(p);
        }
      }
    });
    // Zooming is a toolbar action only. Wheel-zoom made the panels hostile to
    // scroll past: a page scroll that happened to cross a panel silently rescaled
    // it instead, and on a trackpad the two gestures are the same one. The wheel
    // is therefore left to the page.
    window.addEventListener('mouseup', function (e) {
      if (p.orbiting) {
        p.orbiting = false;
        p.canvas.classList.remove('cv-grabbing');
        p.miniBg = null;    // the thumbnail is of the old angle
        drawAll();
        if (!p.moved) { var orbitPoint = pos(e); pickCell(p, nearest(p, orbitPoint[0], orbitPoint[1])); }
        return;
      }
      if (p.panning) {
        p.panning = false;
        p.canvas.classList.remove('cv-grabbing');
        drawAll();
        if (!p.moved) { var panPoint = pos(e); pickCell(p, nearest(p, panPoint[0], panPoint[1])); }
        return;
      }
      if (!p.drag) return;
      p.drag = false;
      if (!D || !p.ok) { p.lasso = null; p.lassoData = null; return; }  // not projected yet
      var keep = false;
      if (p.moved && p.lasso && p.lasso.length > 2) {
        ensureScreenProjection(p);
        var s = new Set();
        for (var i = 0; i < D.n; i++) {
          if (p.ok[i] && shown(i, p) && CBGeom.inPoly(p.sx[i], p.sy[i], p.lasso)) s.add(i);
        }
        // A real lasso selection supersedes any prior single-cell pick — clear it
        // BEFORE setSelection so its drawAll() drops the stale orange pick ring
        // (that ring is not gated on `!sel`). An empty lasso keeps the pick.
        // a lasso is a different question from "tell me about this one cell"
        if (s.size) {
          pick = null; unpinTip(); closeCard(); setSelection(s, p);
          p.lassoData = lassoToUnit(p, p.lasso); keep = true;
        } else { setSelection(null); }
      } else {
        var m = pos(e), k = nearest(p, m[0], m[1]);
        // A click PICKS -- it no longer opens the detail card. On a Trekker data
        // set picking a nucleus is how its niche is read, and having a card
        // covering the panels every time made that unusable. The card is opened
        // deliberately, from the tooltip's Details button.
        //
        // Clicking the picked cell again drops the pick, so the click that made
        // it is also the click that undoes it.
        pickCell(p, k);
      }
      if (!keep) { p.lasso = null; p.lassoData = null; }
      draw(p);
    });
  }

  // ---- build panels from DOM ----------------------------------------------
  function panelKey(index) {
    return index < 26 ? String.fromCharCode(65 + index) : ('P' + (index + 1));
  }
  function ensurePanelSlots(count) {
    var host = document.querySelector('.coordviews-page .cv-panes');
    if (!host) return;
    var panes = host.querySelectorAll(':scope > .cv-pane');
    var source = panes[0];
    if (!source) return;
    for (var index = panes.length; index < count; index++) {
      var key = panelKey(index), low = key.toLowerCase();
      var clone = source.cloneNode(true);
      clone.className = 'cv-pane cv-hidden';
      clone.querySelectorAll('.cv-gpu-layer').forEach(function (canvas) {
        canvas.remove();
      });
      clone.querySelectorAll('[id]').forEach(function (el) {
        el.id = el.id.replace(/-a$/, '-' + low);
      });
      clone.querySelectorAll('[data-panel]').forEach(function (el) {
        el.dataset.panel = key;
      });
      var title = clone.querySelector('.cv-ptitle'); if (title) title.textContent = '—';
      var insertBefore = $('cv-card-pos');
      host.insertBefore(clone, insertBefore || null);
    }
  }

  // The canvas elements persist across dataset switches, so panel objects and
  // their event listeners are created EXACTLY ONCE. Re-wiring on every onData
  // would stack duplicate listeners whose stale closures fire on unprojected
  // panels. Subsequent onData calls reuse these objects; project() resets their
  // per-dataset arrays.
  function buildPanels() {
    var paneEls = document.querySelectorAll('.coordviews-page .cv-panes > .cv-pane');
    var dpr = window.devicePixelRatio || 1;
    for (var index = panels.length; index < paneEls.length; index++) {
      var key = panelKey(index), low = key.toLowerCase();
      var cv = $('cv-cv-' + low); if (!cv) continue;
      var mini = $('cv-mini-' + low);
      var p = { key: key, canvas: cv, ctx: cv.getContext('2d'), tipId: 'cv-tip-' + low,
        // the canvas sits in .cv-canvas-wrap now, so the pane is two levels up
        pane: cv.closest('.cv-pane'), spaceId: null, W: 0, H: 0,
        sx: null, sy: null, ok: null, lasso: null, lassoData: null, drag: false, moved: false,
        view: null, mini: mini, mctx: null, miniBg: null, miniUnit: null,
        underlayCanvas: null, underlayCtx: null,
        interactionCanvas: null, interactionCtx: null,
        gpu: null, gpuCanvas: null, gpuData: null, gpuDataState: null,
        gpuTransformOnly: false };
      attachGpu(p);
      attachInteractionLayer(p);
      prepareCanvasSemantics(p);
      // The minimap is a FIXED size, so its backing store is set once here
      // rather than on every re-fit.
      if (mini) {
        mini.width = MINI * dpr; mini.height = MINI * dpr;
        p.mctx = mini.getContext('2d');
        p.mctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      }
      panels.push(p);
      wireHover(p); wireBrush(p);
      if (p.pane) {
        p.pane.addEventListener('mousedown', function () {
          var pane = this, found = null;
          panels.forEach(function (candidate) {
            if (candidate.pane === pane) found = candidate;
          });
          if (found && isSpatialSpace(spaceById[found.spaceId])) {
            activateSpatial(found.spaceId);
          }
        });
        p.pane.addEventListener('click', function (e) {
          if (!e.target.closest('.cv-ptitle')) return;
          if (!canFocusPanel()) return;
          var pane = this, found = null;
          panels.forEach(function (candidate) {
            if (candidate.pane === pane) found = candidate;
          });
          if (found && found.spaceId) setFocusPanel(found.key);
        });
        p.pane.addEventListener('dblclick', function (e) {
          if (!e.target.closest('.cv-canvas-wrap')) return;
          if (!canFocusPanel()) return;
          var pane = this, found = null;
          panels.forEach(function (candidate) {
            if (candidate.pane === pane) found = candidate;
          });
          if (!found || !found.spaceId) return;
          e.preventDefault();
          setFocusPanel(found.key);
        });
      }
      if (resizeObserver && p.pane) resizeObserver.observe(p.pane);
    }
    // Re-project + redraw whenever the panes gain/lose size — critically, when
    // the tab flips from display:none to visible (0 -> real width).
    if (!resizeObserver) {
      resizeObserver = new ResizeObserver(function () {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(function () {
          if (!D || focusAnimating) return;
          resizeAll();
          reportWorkspaceReady();
        }, 30);
      });
      panels.forEach(function (p) {
        if (p.pane) resizeObserver.observe(p.pane);
      });
    }
    // Only flow chrome changes the available panel height. More is an overlay,
    // and observing it would make an innocuous settings click re-fit the grid.
    // The legend wraps as categories are added or the workspace narrows, so its
    // live height must participate in the same fit as the cohort status row.
    if (!resizeObserver._cvChromeObserved) {
      ['cv-workspace-guide', 'cv-selbar', 'cv-legend', 'cv-cbar'].forEach(function (id) {
        var el = $(id);
        if (el) resizeObserver.observe(el);
      });
      resizeObserver._cvChromeObserved = true;
    }
    // The pane container gains its final width only after a hidden workspace is
    // restored. Observe it directly so the grid recomputes at that width.
    if (!resizeObserver._cvPanesObserved) {
      var panesHost = document.querySelector('.coordviews-page .cv-panes');
      if (panesHost) resizeObserver.observe(panesHost);
      resizeObserver._cvPanesObserved = true;
    }
  }

  function canFocusPanel() {
    return !singleActive;
  }

  // Coordinate-source / QC / positioning / Moran's I detail for a Trekker data
  // set. The three depth views share one default-collapsed region beneath the
  // linked grid, rather than three old-page boxes or a hidden modal.
  var trekkerInsightCurrent = 'cell';
  var trekkerInsightTimer = null;
  var trekkerInsightFrame = null;

  function updateTrekkerInsightTabs(name) {
    var names = ['cell', 'qc', 'moran'];
    if (names.indexOf(name) < 0) name = 'cell';
    names.forEach(function (candidate) {
      var tab = $('cv-tk-tab-' + candidate);
      var active = candidate === name;
      if (tab) {
        tab.classList.toggle('is-active', active);
        tab.setAttribute('aria-selected', active ? 'true' : 'false');
      }
    });
    return name;
  }

  function trekkerScrollHost(region) {
    var node = region && region.parentElement;
    while (node && node !== document.body && node !== document.documentElement) {
      var style = window.getComputedStyle(node);
      if (/(auto|scroll|overlay)/.test(style.overflowY) &&
        node.scrollHeight > node.clientHeight) return node;
      node = node.parentElement;
    }
    return document.scrollingElement || document.documentElement;
  }

  function scrollTrekkerHostBy(host, delta) {
    if (!host || Math.abs(delta) <= .5) return;
    if (host === document.scrollingElement || host === document.documentElement ||
      host === document.body) {
      window.scrollBy(0, delta);
    } else {
      host.scrollTop += delta;
    }
  }

  // The insight card sits at the end of the document. If a tall panel is simply
  // replaced by a short one, the browser has to clamp scrollY to the new document
  // height and the whole card jumps down the viewport. Keep an invisible tail only
  // as large as the missing viewport space; the card itself can still shrink, but
  // its heading remains where the reader left it.
  function setTrekkerAnchorSpace(stageHeight) {
    var region = $('cv-tk-insights');
    var stage = $('cv-tk-panel-stage');
    if (!region || !stage || !stageHeight) return;
    var host = trekkerScrollHost(region);
    var viewportHeight = host === document.scrollingElement ||
      host === document.documentElement || host === document.body
      ? window.innerHeight : host.clientHeight;
    var chrome = Math.max(0, region.offsetHeight - stage.offsetHeight);
    var space = Math.max(0, viewportHeight - chrome - stageHeight - 18);
    region.style.setProperty('--cv-tk-anchor-space', Math.round(space) + 'px');
  }

  function settleTrekkerInsight(name) {
    var stage = $('cv-tk-panel-stage');
    ['cell', 'qc', 'moran'].forEach(function (candidate) {
      var panel = $('cv-tk-panel-' + candidate);
      if (!panel) return;
      var active = candidate === name;
      panel.style.display = active ? '' : 'none';
      panel.style.position = '';
      panel.style.visibility = '';
      panel.style.width = '';
      panel.classList.toggle('is-active', active);
      panel.classList.remove('is-entering', 'is-leaving');
    });
    if (stage) {
      stage.classList.remove('is-switching');
      stage.style.height = '';
      setTrekkerAnchorSpace(stage.getBoundingClientRect().height);
    }
    trekkerInsightCurrent = name;
  }

  function animateTrekkerInsight(name) {
    var stage = $('cv-tk-panel-stage');
    var region = $('cv-tk-insights');
    var oldPanel = $('cv-tk-panel-' + trekkerInsightCurrent);
    var nextPanel = $('cv-tk-panel-' + name);
    if (!stage || !region || !oldPanel || !nextPanel) {
      settleTrekkerInsight(name);
      return;
    }

    clearTimeout(trekkerInsightTimer);
    if (trekkerInsightFrame) cancelAnimationFrame(trekkerInsightFrame);
    if (stage.classList.contains('is-switching')) {
      settleTrekkerInsight(trekkerInsightCurrent);
      oldPanel = $('cv-tk-panel-' + trekkerInsightCurrent);
    }

    var anchorTop = region.getBoundingClientRect().top;
    var scrollHost = trekkerScrollHost(region);
    var startHeight = stage.getBoundingClientRect().height;
    var minHeight = parseFloat(window.getComputedStyle(stage).minHeight) || 0;
    nextPanel.style.display = '';
    nextPanel.style.position = 'absolute';
    nextPanel.style.visibility = 'hidden';
    nextPanel.style.width = '100%';
    var targetHeight = Math.max(minHeight, nextPanel.scrollHeight);
    nextPanel.style.visibility = '';

    stage.style.height = Math.max(minHeight, startHeight) + 'px';
    stage.classList.add('is-switching');
    oldPanel.classList.add('is-leaving');
    nextPanel.classList.add('is-entering');
    nextPanel.classList.add('is-active');
    setTrekkerAnchorSpace(targetHeight);
    void stage.offsetHeight;

    var motionReduced = window.matchMedia &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    if (motionReduced) {
      settleTrekkerInsight(name);
      var reducedDelta = region.getBoundingClientRect().top - anchorTop;
      scrollTrekkerHostBy(scrollHost, reducedDelta);
      return;
    }

    var started = performance.now();
    var holdAnchor = function () {
      var delta = region.getBoundingClientRect().top - anchorTop;
      scrollTrekkerHostBy(scrollHost, delta);
      if (performance.now() - started < 320) {
        trekkerInsightFrame = requestAnimationFrame(holdAnchor);
      }
    };
    trekkerInsightFrame = requestAnimationFrame(function () {
      stage.style.height = targetHeight + 'px';
      nextPanel.classList.remove('is-entering');
      oldPanel.classList.remove('is-active');
      holdAnchor();
    });
    trekkerInsightTimer = setTimeout(function () {
      settleTrekkerInsight(name);
      var delta = region.getBoundingClientRect().top - anchorTop;
      scrollTrekkerHostBy(scrollHost, delta);
    }, 330);
  }

  function selectTrekkerInsight(name) {
    name = updateTrekkerInsightTabs(name);
    var body = $('cv-tk-insights-body');
    var stage = $('cv-tk-panel-stage');
    var hidden = !body || window.getComputedStyle(body).display === 'none';
    if (hidden || !stage || name === trekkerInsightCurrent) {
      settleTrekkerInsight(name);
      return;
    }
    animateTrekkerInsight(name);
  }

  function setTrekkerInsightsOpen(on) {
    var toggle = $('cv-tk-insights-toggle');
    var body = $('cv-tk-insights-body');
    if (!toggle || !body) return;
    toggle.setAttribute('aria-expanded', on ? 'true' : 'false');
    toggle.classList.toggle('is-open', on);
    body.style.display = on ? '' : 'none';
    if (on) {
      requestAnimationFrame(function () {
        var stage = $('cv-tk-panel-stage');
        if (stage) setTrekkerAnchorSpace(stage.getBoundingClientRect().height);
      });
    } else {
      var region = $('cv-tk-insights');
      if (region) region.style.setProperty('--cv-tk-anchor-space', '0px');
    }
  }

  function trekkerStatsGrid(q) {
    var pass2p = q.pct_2plus < 20;
    return [
      ['Total nuclei', fmt(q.total_nuclei), 'single-nuclei library', ''],
      ['In Trekker library', q.pct_in_lib.toFixed(2) + '%', fmt(q.in_lib) + ' nuclei · ref >95%', q.pct_in_lib > 95 ? 'ok' : 'warn'],
      ['Valid spatial barcodes', q.pct_valid_sb.toFixed(2) + '%', 'ref >95%', q.pct_valid_sb > 95 ? 'ok' : 'warn'],
      ['At least 1 location', q.pct_positioned.toFixed(2) + '%', fmt(q.positioned) + ' nuclei · ref >60%', q.pct_positioned > 60 ? 'ok' : 'warn'],
      ['Confidently positioned', q.pct_conf.toFixed(2) + '%', fmt(q.conf) + ' nuclei · ref >40%', q.pct_conf > 40 ? 'ok' : 'warn'],
      ['2+ locations', q.pct_2plus.toFixed(2) + '%', 'ref <20% ' + (pass2p ? '' : '← over'), pass2p ? 'ok' : 'warn']
    ].map(function (r) {
      return '<div class="tk-stat ' + r[3] + '"><div class="tk-k">' + r[0] + '</div><div class="tk-v">' +
        r[1] + '</div><div class="tk-m">' + r[2] + '</div></div>';
    }).join('');
  }

  function trekkerPositionTable(q) {
    var total = q.total_nuclei;
    return [
      ['0 (unpositioned)', q.n_0, 'Excluded · coordinate is the <code>0,0</code> sentinel'],
      ['1', q.n_1, '<b>Imported</b> (incl. salvaged)'],
      ['2', q.n_2, 'Excluded'], ['3', q.n_3, 'Excluded'], ['≥4', q.n_4p, 'Excluded']
    ].map(function (r) {
      return '<tr><td>' + r[0] + '</td><td class="num">' + fmt(r[1]) + '</td><td class="num">' +
        (r[1] / total * 100).toFixed(2) + '%</td><td class="tk-muted">' + r[2] + '</td></tr>';
    }).join('');
  }

  function trekkerSalvageFlag(q) {
    var salvaged = q.salv_2 + q.salv_3;
    return '<b>Confidently positioned ≠ exactly one location.</b> The ' +
      fmt(q.n_1) + ' imported nuclei = native single-location ' + fmt(q.o_1) +
      ' + vendor-salvaged from 2 (' + esc(q.salv_2) + ') + from 3 (' + esc(q.salv_3) +
      '), i.e. <b>' + esc(salvaged) +
      ' (' + (salvaged / q.n_1 * 100).toFixed(2) + '%)</b> are upstream-salvaged multi-location nuclei. ' +
      'The label must be <code>vendor_confidently_positioned</code>.';
  }

  function trekkerProvenance(q) {
    return [
      ['Platform / assay', esc(q.assay)], ['Sample ID', esc(q.sample_id)], ['Tile ID', esc(q.tile_id)],
      ['Pipeline version', '<span class="tk-muted">missing (metric absent)</span>'],
      ['Coordinate source', 'Location CSV (canonical)'],
      ['Coordinate unit', 'µm <span class="tk-muted">(per manual; not declared in file)</span>'],
      ['DBSCAN eps', esc(q.eps)], ['minPts', esc(q.min_sb)],
      ['Histology image', '<span class="tk-muted">none (not provided in bundle)</span>'],
      ['Moran\'s I source', '<span class="tk-badge tk-badge-soft" style="font-size:10px">Upstream</span>']
    ].map(function (r) { return '<dt>' + r[0] + '</dt><dd>' + r[1] + '</dd>'; }).join('');
  }

  function trekkerRangeFlag(q) {
    return '<b>The vendor\'s own demo crosses the vendor\'s own reference line.</b> ' +
      'The 2+ location rate ' + esc(q.pct_2plus) + '% &gt; the manual\'s suggested &lt;20%. The app should only show ' +
      '"below vendor reference range" and must not adjudicate sample usability for the user.';
  }

  function trekkerMoranRows(moran, linkable) {
    return moran.map(function (r) {
      var link = linkable
        ? '<td><a href="#" class="tk-link" data-g="' + esc(r.gene) + '">Show in plot →</a></td>'
        : '';
      return '<tr><td class="num tk-muted">' + esc(r.rank) + '</td>' +
        '<td style="font-weight:600">' + esc(r.gene) + '</td><td class="num">' + r.I.toFixed(4) + '</td>' +
        link + '</tr>';
    }).join('');
  }

  function fillTrekkerInsights() {
    if (!D || !D.trekker) return;
    var q = D.trekker.qc || {};
    // Each box is filled independently. The builders read a couple of dozen QC
    // fields and throw on one that is absent, so a .crb carrying a partial QC
    // record used to lose the WHOLE detail view to whichever box failed first --
    // including the Moran table, which does not depend on any of them.
    var fill = function (id, build) {
      var el = $(id); if (!el) return;
      try { el.innerHTML = build(q); } catch (err) { el.innerHTML = ''; }
    };
    fill('cv-tk-stats', trekkerStatsGrid);
    fill('cv-tk-postbl', trekkerPositionTable);
    fill('cv-tk-salvflag', trekkerSalvageFlag);
    fill('cv-tk-prov', trekkerProvenance);
    fill('cv-tk-rangeflag', trekkerRangeFlag);
    // Linkable: the table names the genes whose expression is spatially
    // structured, and this workspace can colour by a gene, so each row is one
    // click from the map that makes the number mean something. It was built
    // unlinked back when there was no gene mode here to send it to.
    $('cv-tk-morantbl').innerHTML = D.trekker.moran
      ? trekkerMoranRows(D.trekker.moran, true) : '';
    Array.prototype.forEach.call(
      $('cv-tk-morantbl').querySelectorAll('a[data-g]'),
      function (a) {
        a.onclick = function (e) {
          e.preventDefault();
          colourByGene(a.getAttribute('data-g'));
        };
      }
    );
  }

  function openTrekkerInsights(tab, reveal) {
    var region = $('cv-tk-insights');
    if (!region || !D || !D.trekker) return;
    fillTrekkerInsights();
    selectTrekkerInsight(tab || 'cell');
    setTrekkerInsightsOpen(true);
    if (reveal && region.scrollIntoView) {
      region.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
    }
  }

  // Mark a gene as asked-for and clear what is on screen for the previous one.
  function requestGene(gene) {
    geneWanted = gene;
    D.gene = null;
    clipRange();
    if (colorBy === GENE_MODE) {
      renderColorbar(true, null, 'loading ' + esc(gene) + '…');
    }
  }

  // Colour every panel by `gene`, from anywhere that names one. The gene picker
  // is a server-side selectize, so the option has to be added before it can be
  // selected -- it holds only the page of names the server last sent, and the
  // one being asked for is usually not in it. Setting the value through the
  // widget rather than the input keeps the control showing what is on screen.
  function colourByGene(gene) {
    if (!gene) return;
    var sel = $('cv-pick-color');
    if (sel) {
      if (sel.selectize) sel.selectize.setValue(GENE_MODE, true);
      else sel.value = GENE_MODE;
    }
    // Drop the previous gene BEFORE switching: the picker is about to say
    // `gene`, and until the reply arrives the old vector would be drawn under
    // that name. Cells fall back to the neutral "no gene" colour meanwhile.
    requestGene(gene);
    setColorBy(GENE_MODE);
    var el = $('coordviews_gene');
    if (el && el.selectize) {
      el.selectize.addOption({ value: gene, label: gene });
      el.selectize.setValue(gene, false);   // false: do fire change → Shiny
    } else if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('coordviews_gene', gene);
    }
    var trekkerControls = window.CBViewState.trekkerGeneControls(
      singleActive,
      gene
    );
    if (trekkerControls) {
      var mode = $('trekker_mode');
      var picker = $('trekker_gene_pick');
      if (mode && mode.selectize) {
        mode.selectize.setValue(trekkerControls.trekker_mode, false);
      }
      if (picker && picker.selectize) {
        picker.selectize.addOption({ value: gene, label: gene });
        picker.selectize.setValue(trekkerControls.trekker_gene_pick, false);
      } else if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue('trekker_mode', trekkerControls.trekker_mode);
        Shiny.setInputValue(
          'trekker_gene_pick',
          trekkerControls.trekker_gene_pick
        );
      }
    }
  }
  // Controls that belong to a specific right-panel space (the clonal-layout switch,
  // the histology-image bar) are shown only while that space is the one on screen —
  // they used to key off "the dataset HAS this space", which is wrong now that the
  // right panel can switch away from it.
  function updateSpaceScopedControls() {
    if (!D || !D.spaces) return;
    // Every space now has its own always-on panel, so these controls show whenever
    // the data set HAS the space they act on: the clonal-layout switch when a
    // clone panel exists, the histology-image bar when a spatial panel carries an
    // image. revealEl fades them in/out (see .cv-collapse CSS).
    var hasClone = !!spaceById['clone'];
    // The alignment bar adjusts the image ON SCREEN, so it follows the choice
    // rather than the data set: with "None" chosen there is nothing to align.
    var sp = activeSpatial();
    var hasImg = !!(sp && currentImage(sp));
    revealEl($('cv-clone-layout-ctl'), hasClone);
    revealEl($('coordviews_image_ui'), hasImg);
  }

  // ---- clonal-panel layout (client-side, instant) -------------------------
  // The clone space has abstract axes, so it is re-laid-out here between
  // representations without a server round-trip, using only D.clone (per-cell
  // clone id + per-clone size). No-receptor cells are left NA (not plotted):
  // they have no clonal identity, so they belong in the UMAP/Spatial panels.
  var CLONE_MODE = 'stack';
  // The bins arrive with the data (clone_contract.R owns them, and the Immune
  // repertoire tab reads the same file). They were hard-coded here as four
  // tiers topping out at "Large (>20)" while that tab used five -- so a
  // 500-cell clone was Hyperexpanded on one page and Large on the other, from
  // the same object. The fallback is only for a bundle built before the bins
  // travelled; it is the old four-tier scheme and is deliberately not extended.
  var TIER_UB = [1, 5, 20];
  var TIER_LAB = ['Single (1)', 'Small (2–5)', 'Medium (6–20)', 'Large (>20)'];
  function syncCloneTiers() {
    var c = D && D.clone;
    if (!c || !c.tiers || !c.tiers.length || !c.tier_labels) return;
    TIER_UB = c.tiers.slice();
    TIER_LAB = c.tier_labels.slice();
  }
  function tierOf(sz) {
    for (var k = 0; k < TIER_UB.length; k++) if (sz <= TIER_UB[k]) return k;
    return TIER_LAB.length - 1;
  }

  function applyCloneLayout(mode) {
    var sp = spaceById['clone'];
    if (!sp || !D || !D.clone) return;
    CLONE_MODE = mode;
    var n = D.n, id = D.clone.id, size = D.clone.size, i, ci;
    var x = new Float32Array(n), y = new Float32Array(n);
    for (i = 0; i < n; i++) { x[i] = NaN; y[i] = NaN; }

    if (mode === 'bands') {
      // Horizontal bands by expansion tier; band height fixed, band width grows
      // with the number of cells in that tier (so the eye reads tier size).
      var present = {};
      for (i = 0; i < n; i++) { ci = id[i]; if (ci >= 0) present[tierOf(size[ci])] = 1; }
      var tiers = Object.keys(present).map(Number).sort(function (a, b) { return a - b; });
      var band = {}; tiers.forEach(function (t, bi) { band[t] = bi; });
      var count = {};
      for (i = 0; i < n; i++) {
        ci = id[i]; if (ci < 0) continue;
        var b = band[tierOf(size[ci])];
        count[b] = (count[b] || 0) + 1;
        x[i] = count[b] - 1;
        // stable per-cell jitter fills the band strip (0.1 .. 0.9 within band b)
        y[i] = b + 0.1 + cellHash(i) * 0.8;
      }
      sp._axisSpec = {
        xlab: 'cells in tier  (width ∝ count) →',
        ylab: 'clonal expansion tier ↑',
        bands: tiers.map(function (t, bi) {
          return { label: TIER_LAB[t], lo: bi, mid: bi + 0.5 };
        })
      };
    } else { // 'stack' — one column per clonotype, cells stacked within it
      var seen = {};
      for (i = 0; i < n; i++) {
        ci = id[i]; if (ci < 0) continue;
        seen[ci] = (seen[ci] || 0) + 1;
        x[i] = ci;               // clone id is already the size rank (0 = largest)
        y[i] = seen[ci] - 1;
      }
      sp._axisSpec = {
        xlab: 'clonotypes, ranked  (largest at left)',
        ylab: 'cells stacked in clone  (expansion ↑)'
      };
    }
    sp.x = x; sp.y = y; sp.stretch = true; sp._unit = null;
  }

  function setSegOn(mode) {
    var host = $('cv-clone-layout'); if (!host) return;
    Array.prototype.forEach.call(host.querySelectorAll('.cv-seg-btn'), function (b) {
      b.classList.toggle('is-on', b.getAttribute('data-mode') === mode);
    });
  }

  // Colour picker: categorical groups + two special continuous modes. Selecting
  // a gene mode reveals its picker(s); the server answers with the value vector.
  var GENE_MODE = '__gene__', GENE_PANELS_MODE = '__gene_panels__';
  var RGB_MODE = '__rgb__', FIELD_PREFIX = '__field__';
  var genePanelBaseCount = 1;
  // Trekker continuous field currently selected in "Colour by", or null.
  // Memoised on (D, colorBy): fieldOf() is called per cell inside colorOf()/
  // visible() on the draw hot path, but its result only changes when the dataset
  // or the colour mode changes — so the string work runs once, not ~3n/draw.
  var _fldCacheD = null, _fldCacheKey = null, _fldCacheVal = null;
  function fieldOf(p) {
    var mode = panelColorMode(p);
    if (_fldCacheD === D && _fldCacheKey === mode) return _fldCacheVal;
    var v = null;
    if (D && D.fields && mode && mode.indexOf(FIELD_PREFIX) === 0) {
      v = D.fields[mode.slice(FIELD_PREFIX.length)] || null;
    }
    _fldCacheD = D; _fldCacheKey = mode; _fldCacheVal = v;
    return v;
  }
  function fieldForMode(mode) {
    return D && D.fields && typeof mode === 'string' &&
      mode.indexOf(FIELD_PREFIX) === 0
      ? (D.fields[mode.slice(FIELD_PREFIX.length)] || null) : null;
  }

  // Spatial autocorrelation belongs to the spatial lens that gives it meaning.
  // Keep the score on each spatial card, and only while a continuous quantity
  // (one gene or one numeric field) is actually being drawn.
  function continuousValues() {
    if (colorBy === GENE_MODE && D && D.gene) {
      return { label: D.gene.gene, values: D.gene.v };
    }
    var fld = fieldOf();
    return fld
      ? { label: fld.label || 'Continuous value', values: fld.raw || fld.v }
      : null;
  }
  function spatialMoran(space, values) {
    var valid = [], i;
    for (i = 0; i < D.n; i++) {
      var x = space.x[i], y = space.y[i], v = values[i];
      if (x == null || y == null || v == null || isNaN(x) || isNaN(y) || isNaN(v)) continue;
      valid.push(i);
    }
    if (valid.length < 7) return null;
    // Stable, evenly-spaced sampling avoids a random-looking score after every
    // redraw while bounding the six-neighbour search on large slides.
    var cap = 1000, idx = valid;
    if (valid.length > cap) {
      idx = new Array(cap);
      for (i = 0; i < cap; i++) idx[i] = valid[Math.floor(i * valid.length / cap)];
    }
    var n = idx.length, z = new Float64Array(n), mean = 0;
    for (i = 0; i < n; i++) mean += +values[idx[i]];
    mean /= n;
    var denom = 0;
    for (i = 0; i < n; i++) { z[i] = +values[idx[i]] - mean; denom += z[i] * z[i]; }
    if (!denom) return 0;
    var adj = new Array(n);
    for (i = 0; i < n; i++) adj[i] = new Set();
    for (i = 0; i < n; i++) {
      var nearest = [], ii = idx[i], xi = +space.x[ii], yi = +space.y[ii];
      for (var j = 0; j < n; j++) {
        if (j === i) continue;
        var jj = idx[j], dx = xi - +space.x[jj], dy = yi - +space.y[jj];
        var d = dx * dx + dy * dy;
        var at = nearest.length;
        while (at > 0 && nearest[at - 1].d > d) at--;
        nearest.splice(at, 0, { j: j, d: d });
        if (nearest.length > 6) nearest.pop();
      }
      nearest.forEach(function (q) { adj[i].add(q.j); adj[q.j].add(i); });
    }
    var num = 0, weight = 0;
    for (i = 0; i < n; i++) {
      var degree = adj[i].size;
      if (!degree) continue;
      weight += 1;
      var neighbourSum = 0;
      adj[i].forEach(function (j) { neighbourSum += z[j]; });
      num += z[i] * neighbourSum / degree;
    }
    return weight ? (n / weight) * num / denom : null;
  }
  function updateMoranBadges() {
    var continuous = continuousValues();
    panels.forEach(function (p) {
      var badge = $('cv-moran-' + p.key.toLowerCase());
      if (!badge) return;
      var sp = spaceById[p.spaceId];
      if (!isSpatialSpace(sp)) {
        badge.style.display = 'none'; badge.textContent = '';
        delete badge.dataset.value; delete badge.dataset.field;
        return;
      }
      if (!continuous) {
        badge.style.display = 'none'; badge.textContent = '';
        delete badge.dataset.value; delete badge.dataset.field;
        return;
      }
      var score = spatialMoran(sp, continuous.values);
      if (score == null || !isFinite(score)) {
        badge.style.display = 'none'; return;
      }
      var value = Math.max(-1, Math.min(1, score));
      badge.dataset.value = value.toFixed(6);
      badge.dataset.field = continuous.label;
      badge.textContent = "Moran's I " + value.toFixed(3);
      badge.title = continuous.label +
        " · each cell's six nearest spatial neighbours · click for details";
      badge.style.display = '';
    });
  }
  function openMoranDialog(badge) {
    if (!badge || !badge.dataset.value) return;
    var dlg = $('cv-moran-modal'); if (!dlg || !dlg.showModal) return;
    var title = $('cv-moran-modal-title'), value = $('cv-moran-modal-value');
    var pane = badge.closest('.cv-pane'), panel = null;
    panels.forEach(function (candidate) { if (candidate.pane === pane) panel = candidate; });
    var sp = panel && spaceById[panel.spaceId];
    if (title) title.textContent = (badge.dataset.field || 'Continuous value') +
      ' in ' + ((sp && sp.label) || 'spatial data');
    if (value) value.textContent = "Moran's I " + Number(badge.dataset.value).toFixed(3);
    dlg.showModal();
  }
  // Registered groups are the only metadata options. Specialist continuous
  // fields and gene-expression modes remain available as non-metadata modes.
  function destroyPicker(select) {
    if (!select.selectize) return;
    select.selectize.destroy();
    // Selectize leaves an unnamespaced validation listener on reused selects.
    // It retains the old dropdown and its full option catalogue.
    if (window.jQuery) window.jQuery(select).off('invalid');
    delete select.dataset.cerebroDropdownReady;
    delete select.dataset.cerebroMultiSelectReady;
  }

  function fillColorPicker() {
    var sel = $('cv-pick-color'); if (!sel) return;
    var optsOf = function (keys, prefix, labelOf) {
      return keys.map(function (k) {
        return '<option value="' + esc(prefix + k) + '">' +
          esc(labelOf(k)) + '</option>';
      }).join('');
    };
    var grp = function (label, body) {
      return body ? '<optgroup label="' + esc(label) + '">' + body + '</optgroup>' : '';
    };
    var html = grp('Grouping variables',
      optsOf(Object.keys(D.groups || {}), '', groupLabel));
    html += grp('Other categorical',
      optsOf(Object.keys(D.cat_extra || {}), '', function (k) { return k; }) +
      // Columns with too many levels to colour by are listed but disabled. They
      // exist in the meta data and appear on the Projection tab, so omitting
      // them silently left the two tabs disagreeing with no way to see why.
      Object.keys(D.cat_skipped || {}).map(function (k) {
        return '<option disabled>' + esc(k) + ' — ' +
          fmt(D.cat_skipped[k]) + ' levels, too many to colour</option>';
      }).join(''));
    html += grp('Continuous',
      optsOf(Object.keys(D.fields || {}), FIELD_PREFIX, function (k) {
        return D.fields[k].label || k;
      }));
    html += grp('Expression',
      '<option value="' + GENE_MODE + '">Mean gene expression</option>' +
      '<option value="' + GENE_PANELS_MODE + '">Per-gene expression panels</option>' +
      '<option value="' + RGB_MODE + '">RGB co-expression</option>');
    destroyPicker(sel);
    sel.innerHTML = html;
    if (window.jQuery && window.jQuery.fn && window.jQuery.fn.selectize) {
      window.jQuery(sel).selectize({
        persist: false,
        onChange: function (value) { setColorBy(value); }
      });
      sel.selectize.setValue(colorBy, true);
    } else {
      if (colorBy) sel.value = colorBy;
      sel.onchange = function () { setColorBy(sel.value); };
    }
  }
  // The colour-range control belongs to a continuous colouring and nothing else:
  // a categorical one has no range to trim, and RGB blends three of them.
  function updateClipControl() {
    var el = $('cv-clip-ctl'); if (!el) return;
    var on = (colorBy === GENE_MODE) || !!fieldOf();
    el.style.display = on ? '' : 'none';
  }
  function deferredColor(mode) {
    var name = mode;
    var kind = null;
    var value = null;
    if (mode && mode.indexOf(FIELD_PREFIX) === 0) {
      name = mode.slice(FIELD_PREFIX.length);
      kind = 'fields'; value = D.fields && D.fields[name];
    } else if (D.groups && D.groups[name]) {
      kind = 'groups'; value = D.groups[name];
    } else if (D.cat_extra && D.cat_extra[name]) {
      kind = 'cat_extra'; value = D.cat_extra[name];
    }
    return value && value.deferred ? { kind: kind, name: name, value: value } : null;
  }
  function setColorBy(mode) {
    var deferred = deferredColor(mode);
    if (deferred) {
      viewOperation('linked', 'Loading colour field: ' + deferred.name + '…');
      if (!deferred.value.requested && typeof Shiny !== 'undefined') {
        deferred.value.requested = true;
        Shiny.setInputValue('coordviews_attribute_request', {
      dataset_context: D.dataset_context,
          dataset_id: D.dataset_id,
          dataset_fingerprint: configFingerprint(),
          kind: deferred.kind,
          name: deferred.name,
          nonce: Date.now()
        }, { priority: 'event' });
      }
      return;
    }
    var previousMode = colorBy;
    colorBy = mode; hidden = new Set();
    var geneCtl = $('cv-gene-ctl'), rgbCtl = $('cv-rgb-ctl');
    var panelColorCtl = $('cv-gene-panel-color-ctl');
    var geneMode = mode === GENE_MODE || mode === GENE_PANELS_MODE;
    if (geneCtl) geneCtl.style.display = geneMode ? '' : 'none';
    if (rgbCtl) rgbCtl.style.display = (mode === RGB_MODE) ? '' : 'none';
    if (panelColorCtl) panelColorCtl.style.display =
      (mode === GENE_PANELS_MODE) ? '' : 'none';
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue(
        'coordviews_gene_controls_active',
        mode === RGB_MODE ? 'rgb' : (geneMode ? 'gene' : ''),
        { priority: 'event' }
      );
      Shiny.setInputValue('coordviews_expression_mode',
        mode === GENE_PANELS_MODE ? 'panels' : 'mean', { priority: 'event' });
    }
    updateClipControl();
    clipRange();                 // seed the cache the colours read from
    if (previousMode === GENE_PANELS_MODE || mode === GENE_PANELS_MODE) {
      var order = orderedSpaces();
      ensurePanelSlots(Math.min(12, order.length)); buildPanels(); layoutPanels();
    }
    renderLegend(); drawAll(); renderReadout(); updateMoranBadges();
    if (!singleActive && mode !== GENE_MODE) viewOperation('linked', 'Colour mode changed');
  }

  // ---- projection multi-picker --------------------------------------------
  // Every projection's coordinates are in the bundle. A selected projection is
  // represented by a lightweight client-side space and therefore gets its own
  // canvas, viewport and optional 3-D rotation while sharing all cell state.
  function projDims(nm) {
    var pj = D.projections && D.projections[nm];
    return (pj && pj.ndim) || 2;
  }
  // What a panel is actually showing. Only the first three dimensions are
  // rendered, which needs saying whenever there are more: a PCA is routinely
  // exported with 50 components, and calling that "50-D" would claim a view
  // nothing here provides.
  function projDimLabel(nd) {
    if (nd <= 2) return '';
    return nd === 3 ? '3-D' : '3-D of ' + nd;
  }
  function projOptionLabel(nm) {
    var t = projDimLabel(projDims(nm));
    return t ? nm + ' (' + t + ')' : nm;
  }
  function projSpaceLabel(nm) {
    var t = projDimLabel(projDims(nm));
    return nm + ' (expression' + (t ? ', ' + t : '') + ')';
  }
  function rebuildProjectionInstances(previousSpaces) {
    var keep = {};
    selectedProjections.forEach(function (name) { keep[projectionId(name)] = true; });
    Object.keys(spaceById).forEach(function (id) {
      if (id.indexOf('projection::') === 0 && !keep[id]) delete spaceById[id];
    });
    selectedProjections.forEach(function (name) {
      var pj = D.projections && D.projections[name];
      if (!pj) return;
      var id = projectionId(name), old = spaceById[id];
      if (old) return;
      var previous = previousSpaces && previousSpaces[id];
      var reusable = previous && previous.x === pj.x && previous.y === pj.y &&
        previous.z === (pj.z || undefined);
      var sp = spaceById[id] = {
        id: id,
        label: projSpaceLabel(name),
        x: pj.x,
        y: pj.y,
        _projectionName: name,
        _unit: reusable ? previous._unit : null
      };
      if (Array.isArray(pj.xRange)) sp.xRange = pj.xRange;
      if (Array.isArray(pj.yRange)) sp.yRange = pj.yRange;
      if (pj.z) { sp.z = pj.z; sp.axes = pj.axes; }
    });
  }
  function fillProjPicker() {
    var selEl = $('cv-pick-proj'); if (!selEl) return;
    var names = D.projections ? Object.keys(D.projections) : [];
    var ctl = $('cv-proj-ctl');
    destroyPicker(selEl);
    if (ctl) ctl.style.display = names.length ? '' : 'none';
    if (!names.length) return;
    selEl.innerHTML = names.map(function (nm) {
      return '<option value="' + esc(nm) + '">' +
        esc(projOptionLabel(nm)) + '</option>';
    }).join('');
    Array.prototype.forEach.call(selEl.options, function (option) {
      option.selected = selectedProjections.indexOf(option.value) >= 0;
    });
    var changed = function (values) {
      values = values == null ? [] : (Array.isArray(values) ? values : [values]);
      setSelectedProjections(values);
    };
    if (window.jQuery && window.jQuery.fn && window.jQuery.fn.selectize) {
      var $sel = window.jQuery(selEl);
      $sel.selectize({
        plugins: ['remove_button'],
        persist: false,
        closeAfterSelect: false,
        onChange: changed
      });
      selEl.selectize.setValue(selectedProjections, true);
    } else {
      selEl.onchange = function () {
        changed(Array.prototype.filter.call(selEl.options, function (o) {
          return o.selected;
        }).map(function (o) { return o.value; }));
      };
    }
  }
  function setSelectedProjections(names) {
    var available = D && D.projections ? Object.keys(D.projections) : [];
    names = names.filter(function (name, index) {
      return available.indexOf(name) >= 0 && names.indexOf(name) === index;
    });
    selectedProjections = names;
    rebuildProjectionInstances();
    ensurePanelSlots(orderedSpaces().length);
    buildPanels();
    layoutPanels();
    updateSpaceScopedControls();
    renderMeta();
    resizeAll();
  }

  // Preload a space's histology image + seed the transform state from its preset.
  // Reused by onData and by the Spatial-sample switch below.
  // ---- background images ---------------------------------------------------
  // A section can be shown against several backgrounds (its own embedded
  // histology, whatever the deployment configured), each with its own identity
  // and its own calibration. These read whichever is currently chosen.
  // The picker's value for "no background", kept out of the id space a builder
  // can produce.
  var IMG_NONE = window.CerebroBackgroundModel.NONE;
  function isSpatialSpace(sp) {
    return !!(sp && (sp._spatialSample || sp.background_scope));
  }
  function backgroundSpaces() {
    var out = selectedSpatial.map(function (name) {
      return spaceById[spatialId(name)];
    }).filter(Boolean);
    Object.keys(spaceById).forEach(function (id) {
      var sp = spaceById[id];
      if (isSpatialSpace(sp) && out.indexOf(sp) < 0) out.push(sp);
    });
    return out;
  }
  function activeSpatial() {
    if (activeSpatialId && spaceById[activeSpatialId]) return spaceById[activeSpatialId];
    for (var id in spaceById) if (isSpatialSpace(spaceById[id])) return spaceById[id];
    return null;
  }
  // Put away what the user has done to the image currently on screen, so coming
  // back to it returns their work rather than the preset. Losing it on every
  // switch would make comparing two backgrounds mean re-aligning each time.
  function stashImgState(sp) {
    sp = sp || activeSpatial();
    if (sp) {
      // Which background this section was showing, so returning to it returns
      // the view that was left rather than resetting to its first image.
      var stateKey = backgroundStateKey(sp);
      if (stateKey) imgChoice[stateKey] = sp._customImageId || null;
    }
    var k = imgKey(sp, currentImage(sp));
    if (!k || !sp || !sp._imgState) return;
    var copy = {};
    for (var f in sp._imgState) copy[f] = sp._imgState[f];
    imgStates[k] = copy;
  }
  function viewOperation(id, text) {
    window.dispatchEvent(new CustomEvent('cerebro:view-operation', {
      detail: { id: id, text: text, dataset_context: currentDatasetContext() }
    }));
  }
  function loadSpaceImage(space) {
    if (!space) return;
    space._imgEl = null; space._imgReady = false;
    // A newer request invalidates whatever is still decoding, including when
    // the newer request is "none".
    space._imgToken = ++imgToken;
    // A new bundle is a reason to lay out whatever the window is doing. The
    // guard below exists to stop the layout retriggering ITSELF, not to skip a
    // relayout the data asked for.
    _layoutKey = null;
    var img = currentImage(space);
    if (!img) {
      if (singleActive === 'spatial_projection') viewOperation(singleActive, 'No background image');
      return;
    }
    var feedbackId = singleActive || 'linked';
    // Select the image's own transform before deferred pixels arrive. Otherwise
    // the editor briefly shows (and can overwrite) the previous image's values.
    var k = imgKey(space, img);
    space._imgState = (k && imgStates[k]) ? imgStates[k] : presetState(img);
    viewOperation(feedbackId, feedbackId === 'linked' && !linkedBackgroundsOn
      ? 'Background images hidden' : 'Loading background: ' + (space.label || space.id) + '…');
    if (!img.uri && img.asset_key) {
      requestDeferredAsset(img.asset_key);
      return;
    }
    if (!img.uri) {
      viewOperation(feedbackId, 'Background unavailable: ' + (space.label || space.id));
      return;
    }
    // Only the newest request may paint. Without the token a large image chosen
    // first can finish decoding after a small one chosen second and replace it.
    var mine = space._imgToken;
    var imageDatasetContext = currentDatasetContext();
    var imageIdentity = canonicalIdentity(img.id || img.uri);
    var stillCurrent = function () {
      var activeImage = currentImage(space);
      return mine === space._imgToken &&
        sameDatasetContext(imageDatasetContext, currentDatasetContext()) &&
        datasetContextApi().phase() === 'ready' &&
        canonicalIdentity(activeImage && (activeImage.id || activeImage.uri)) === imageIdentity;
    };
    var im = new Image();
    im.onload = function () {
      if (!stillCurrent()) return;
      space._imgReady = true; drawAll();
      viewOperation(feedbackId, feedbackId === 'linked' && !linkedBackgroundsOn
        ? 'Background images hidden' : 'Background loaded: ' + (space.label || space.id));
    };
    im.onerror = function () {
      if (!stillCurrent()) return;
      space._imgReady = false;
      viewOperation(feedbackId, 'Background unavailable: ' + (space.label || space.id));
      if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue('coordviews_spatial_image_load_error', {
          space: space.label || space.id || 'Spatial', image: img.label || img.id || 'background image',
          dataset_context: imageDatasetContext, nonce: Date.now()
        }, {priority:'event'});
      }
    };
    im.src = img.uri;
    space._imgEl = im;
  }

  function deferredAssetCacheKey(key) {
    return currentDatasetContextKey() + '\u0000' + String(configFingerprint() || '') + '\u0000' + String(key || '');
  }

  function requestDeferredAsset(key) {
    if (!D || !key || typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var cacheKey = deferredAssetCacheKey(key);
    if (assetCache[cacheKey]) {
      applyDeferredAsset(key, assetCache[cacheKey]);
      return;
    }
    if (pendingAssets.has(cacheKey)) return;
    pendingAssets.add(cacheKey);
    Shiny.setInputValue('coordviews_asset_request', {
      dataset_context: D.dataset_context,
      dataset_id: D.dataset_id,
      dataset_fingerprint: configFingerprint(),
      key: key,
      nonce: Date.now()
    }, { priority: 'event' });
  }

  function applyDeferredAsset(key, uri) {
    if (!D || !key || !uri) return;
    var visit = function (images) {
      (images || []).forEach(function (image) {
        if (image && image.asset_key === key) {
          image.uri = uri;
          image.deferred = false;
        }
      });
    };
    (D.spaces || []).forEach(function (space) {
      visit(space.images);
      (space.samples || []).forEach(function (sample) { visit(sample.images); });
    });
    if (key.indexOf('trekker-evidence:') === 0 && D.trekker) {
      var index = Number(key.slice('trekker-evidence:'.length));
      if (Number.isInteger(index) && index >= 0 && index < D.n) {
        if (!D.trekker.evidence_img) D.trekker.evidence_img = [];
        D.trekker.evidence_img[index] = uri;
        if (cardOpen() && cardCell === index) renderCard();
      }
      return;
    }
    Object.keys(spaceById).forEach(function (id) {
      var space = spaceById[id];
      var image = currentImage(space);
      if (image && image.asset_key === key) loadSpaceImage(space);
    });
  }

  // The line above the panels names the spaces on screen, so it has to be
  // rebuilt when one of them changes -- switching spatial section left it naming
  // the section the user had just moved away from.
  function renderMeta() {
    var meta = $('cv-meta');
    if (!meta || !D) return;
    var ids = orderedSpaces();
    var spaceLabels = ids.map(function (id) {
      return esc((spaceById[id] && spaceById[id].label) || id);
    }).join(' · ');
    meta.innerHTML = fmt(D.n) + ' cells · ' + ids.length +
      ' linked spaces (' + spaceLabels + ')' +
      (D.clone ? ' · ' + fmt(D.clone.n_receptor) + ' receptor-bearing cells, ' +
        fmt(D.clone.n_clones) + ' clonotypes' : '');
  }

  function setTrekkerSettingsVisible(visible) {
    var controls = $('cv-trekker-ctl');
    if (!controls) return;
    controls.style.display = visible ? '' : 'none';
    var section = controls.closest('.cerebro-settings-section');
    if (section) section.style.display = visible ? '' : 'none';
  }

  // Only sections carrying images are editable. Scope adjustments by stable
  // section/image identities; never modify the bundle's saved presets.
  function renderImagePicker() {
    var ctl = $('cv-img-pick-ctl'), select = $('cv-bg-image-select');
    var tabs = $('cv-bg-space-tabs');
    if (!ctl || !select) return;
    var allSpaces = backgroundSpaces().filter(function (sp) { return spatialImages(sp).length > 0; });
    var active = activeSpatial();
    if (!active || allSpaces.indexOf(active) < 0) active = allSpaces[0] || null;
    if (active) activeSpatialId = active.id;
    var section = ctl.closest('.cerebro-settings-section');
    if (section) section.style.display = active ? '' : 'none';
    ctl.style.display = active ? '' : 'none';
    if (tabs) {
      var scope = $('cv-bg-space-select');
      if (!scope) {
        tabs.innerHTML = '<label for="cv-bg-space-select">Adjust background</label>' +
          '<select id="cv-bg-space-select" class="form-control"></select>';
        scope = $('cv-bg-space-select');
      }
      var scopeOptions = allSpaces.map(function (sp) {
        return { value: sp.id, text: (sp.label || sp._sampleName || sp.id).replace(/ \(spatial\)$/, '') };
      });
      if (!scope.selectize && window.jQuery && window.jQuery.fn && window.jQuery.fn.selectize) {
        window.jQuery(scope).selectize({ persist: false, onChange: function (id) {
          if (id) { backgroundScopePulse = true; activateSpatial(id); }
        } });
      }
      if (scope.selectize) {
        scope.selectize.clear(true);
        scope.selectize.clearOptions(true);
        scope.selectize.addOption(scopeOptions);
        scope.selectize.setValue(active ? active.id : '', true);
      } else {
        scope.innerHTML = scopeOptions.map(function (option) {
          return '<option value="' + esc(option.value) + '">' + esc(option.text) + '</option>';
        }).join('');
        scope.value = active ? active.id : '';
        scope.onchange = function () { activateSpatial(scope.value); };
      }
      if (backgroundScopePulse) {
        var settings = ctl.querySelector('.cv-bg-settings');
        if (settings) {
          settings.classList.remove('is-scope-changing');
          void settings.offsetWidth;
          settings.classList.add('is-scope-changing');
        }
        backgroundScopePulse = false;
      }
    }
    var note = $('cv-bg-session-note');
    if (note) note.textContent = !singleActive && !linkedBackgroundsOn
      ? 'Background images are hidden. Enable Background images to preview changes.'
      : 'Changes apply only to this session.';
    var images = active ? spatialImages(active) : [];
    var options = images.map(function (im) {
      return { value: im.id, text: im.label || im.id };
    }).concat([{ value: IMG_NONE, text: 'None' }]);
    var value = active && active._customImageId
      ? active._customImageId
      : (images[0] ? images[0].id : IMG_NONE);
    if (!select.selectize && window.jQuery && window.jQuery.fn && window.jQuery.fn.selectize) {
      window.jQuery(select).selectize({
        persist: false,
        onChange: function (id) { if (id) setSpatialImage(id, activeSpatial()); }
      });
    }
    if (select.selectize) {
      // Selectize retains the selected option in clearOptions(). Clear the
      // value first so an image from the previous ROI cannot remain selectable.
      select.selectize.clear(true);
      select.selectize.clearOptions(true);
      select.selectize.addOption(options);
      select.selectize.setValue(value, true);
      if (images.length) select.selectize.enable(); else select.selectize.disable();
    } else {
      select.innerHTML = images.map(function (im) {
        return '<option value="' + esc(im.id) + '">' + esc(im.label || im.id) + '</option>';
      }).concat(['<option value="' + IMG_NONE + '">None</option>']).join('');
      select.disabled = !images.length;
      select.value = value;
      select.onchange = function () { setSpatialImage(select.value, active); };
    }
    seedImgControls();
  }

  // Switch background. The cells have not moved -- only what is behind them --
  // so the viewport is left exactly as it is; re-fitting it here would throw
  // away the zoom the user was comparing at.
  function setSpatialImage(id, sp) {
    sp = sp || activeSpatial();
    if (!sp || !id) return;
    if (id !== IMG_NONE && !spatialImages(sp).some(function (image) { return image.id === id; })) return;
    stashImgState(sp);
    sp._customImageId = id;
    imgChoice[backgroundStateKey(sp)] = id;
    loadSpaceImage(sp);
    if (sp.id === activeSpatialId) {
      seedImgControls();
    }
    renderImagePicker();
    updateSpaceScopedControls();
    drawAll();
  }

  function activateSpatial(id) {
    if (!id || !isSpatialSpace(spaceById[id])) {
      var scopes = backgroundSpaces();
      id = scopes.length ? scopes[0].id : null;
    }
    activeSpatialId = id;
    panels.forEach(function (p) {
      if (p.pane) p.pane.classList.toggle(
        'cv-active-spatial', !!id && p.spaceId === id
      );
    });
    renderImagePicker();
    seedImgControls();
    updateSpaceScopedControls();
  }

  // Back to the alignment the data set shipped with. Alignment is fiddly and
  // easy to lose, and the preset is the only reference point in the bar; without
  // this the way back was reloading the page.
  // Write a state object into the bar's controls, including the ranges: the Move
  // sliders are in DATA units, ranged to the section's coordinate span, so
  // carrying the previous section's range over can put the target's own offset
  // outside it. The slider then clamps and the alignment quietly is not the one
  // that was asked for.
  function seedImgControls(st) {
    var sp = activeSpatial();
    if (!sp) return;
    var cur = currentImage(sp);
    var v = st || sp._imgState || presetState(cur);
    var set = function (id, x) { syncImgRangeValue($(id), x); };
    var tick = function (id, on) { var el = $(id); if (el) el.checked = !!on; };
    var span = (cur && cur.coord_span) || null;
    if (span && span.length >= 2) {
      // Ranged on the section's coordinate span, but never tighter than the
      // value being written: a range that excludes its own value clamps it, and
      // the clamped number is what the next read of the bar puts into the state.
      var rerange = function (id, ext, want) {
        var el = $(id); if (!el) return;
        var lim = Math.max(Math.abs(ext) * 1.2, Math.abs(want || 0) * 1.1);
        el.min = String(-lim); el.max = String(lim);
        el.step = 'any';
      };
      rerange('cv-img-offx', span[0], v.offsetX);
      rerange('cv-img-offy', span[1], v.offsetY);
    }
    var sLo = Math.min(0.3, v.scaleX * 0.9, v.scaleY * 0.9);
    var sHi = Math.max(3, v.scaleX * 1.1, v.scaleY * 1.1);
    ['cv-img-scalex', 'cv-img-scaley'].forEach(function (id) {
      var el = $(id); if (!el) return;
      el.min = String(sLo); el.max = String(sHi);
      el.step = 'any';
    });
    var opacity = $('cv-img-opacity'); if (opacity) opacity.step = 'any';
    var rotation = $('cv-img-rotate'); if (rotation) rotation.step = 'any';
    set('cv-img-opacity', v.opacity);
    set('cv-img-offx', v.offsetX);
    set('cv-img-offy', v.offsetY);
    set('cv-img-scalex', v.scaleX);
    set('cv-img-scaley', v.scaleY);
    tick('cv-img-lock', v.lock != null ? v.lock : (v.scaleX === v.scaleY));
    set('cv-img-rotate', v.rotate || 0);
    tick('cv-img-flipx', v.flipX);
    tick('cv-img-flipy', v.flipY);
    tick('cv-img-show', v.show !== false);
    var activeLabel = $('cv-img-active-label');
    if (activeLabel) activeLabel.textContent = sp._sampleName || '';
  }

  // Back to the alignment THIS (section, image) shipped with -- and only this
  // one. Alignment is fiddly and easy to lose, and the preset is the sole
  // reference point in the bar; without this the way back was reloading.
  function resetImgToPreset() {
    var sp = activeSpatial();
    if (!sp) return;
    var cur = currentImage(sp);
    sp._imgState = presetState(cur);
    var k = imgKey(sp, cur);
    if (k) delete imgStates[k];   // forget the adjustments, for this pair only
    seedImgControls(sp._imgState);
    drawAll();
  }

  function spatialId(name) { return 'spatial::' + name; }
  function spatialSamples() {
    if (!spatialTemplate) return [];
    if (spatialTemplate.samples && spatialTemplate.samples.length) {
      return spatialTemplate.samples;
    }
    return [{
      name: spatialTemplate.name || spatialTemplate.label.replace(/ \(spatial\)$/, ''),
      label: spatialTemplate.label,
      is_roi_overview: spatialTemplate.is_roi_overview,
      x: spatialTemplate.x,
      y: spatialTemplate.y,
      x_range: spatialTemplate.x_range,
      y_range: spatialTemplate.y_range,
      builder_point_opacity: spatialTemplate.builder_point_opacity,
      builder_point_size: spatialTemplate.builder_point_size,
      builder_percentage_cells: spatialTemplate.builder_percentage_cells,
      image: spatialTemplate.image,
      images: spatialTemplate.images || []
    }];
  }
  function rebuildSpatialInstances() {
    var keep = {};
    selectedSpatial.forEach(function (name) { keep[spatialId(name)] = true; });
    Object.keys(spaceById).forEach(function (id) {
      if (id.indexOf('spatial::') === 0 && !keep[id]) delete spaceById[id];
    });
    spatialSamples().forEach(function (sample) {
      if (selectedSpatial.indexOf(sample.name) < 0) return;
      var id = spatialId(sample.name), old = spaceById[id];
      if (old) return;
      var images = sample.images || (sample.image ? [sample.image] : []);
      var sampleStateKey = 'fov:' + id;
      var custom = Object.prototype.hasOwnProperty.call(imgChoice, sampleStateKey)
        ? imgChoice[sampleStateKey]
        : ((images[0] && images[0].id) || IMG_NONE);
      var sp = spaceById[id] = {
        id: id,
        label: sample.label || (sample.name + ' (spatial)'),
        x: sample.x,
        y: sample.y,
        xRange: sample.x_range || null,
        yRange: sample.y_range || null,
        builder_point_opacity: sample.builder_point_opacity,
        builder_point_size: sample.builder_point_size,
        builder_percentage_cells: sample.builder_percentage_cells,
        image: sample.image || null,
        images: images,
        _sampleName: sample.name,
        _spatialSample: true,
        _fitToData: true,
        _customImageId: custom,
        _unit: null
      };
      loadSpaceImage(sp);
    });
    if (!activeSpatialId || !spaceById[activeSpatialId]) {
      activeSpatialId = selectedSpatial.length ? spatialId(selectedSpatial[0]) : null;
    }
  }

  // Spatial multi-picker. All samples travel in the bundle; selecting one only
  // creates a lightweight client-side space that references its arrays/images.
  function fillSpatialPicker() {
    var selEl = $('cv-pick-spatial'), ctl = $('cv-spatial-ctl');
    if (!selEl || !ctl) return;
    destroyPicker(selEl);
    var samples = spatialSamples();
    if (!samples.length) {
      ctl.style.display = 'none'; selEl.innerHTML = ''; return;
    }
    ctl.style.display = samples.length > 1 ? '' : 'none';
    selEl.innerHTML = samples.map(function (s) {
      var label = (s.label || s.name).replace(/ \(spatial\)$/, '');
      return '<option value="' + esc(s.name) + '">' + esc(label) + '</option>';
    }).join('');
    Array.prototype.forEach.call(selEl.options, function (option) {
      option.selected = selectedSpatial.indexOf(option.value) >= 0;
    });
    var changed = function (values) {
      values = values == null ? [] : (Array.isArray(values) ? values : [values]);
      setSelectedSpatial(values);
    };
    var nativeChanged = function () {
      changed(Array.prototype.filter.call(selEl.options, function (o) {
        return o.selected;
      }).map(function (o) { return o.value; }));
    };
    if (window.jQuery && window.jQuery.fn && window.jQuery.fn.selectize) {
      var $sel = window.jQuery(selEl);
      $sel.selectize({
        plugins: ['remove_button'],
        persist: false,
        closeAfterSelect: false,
        onChange: changed
      });
      selEl.selectize.setValue(selectedSpatial, true);
    } else {
      selEl.onchange = nativeChanged;
    }
  }
  function setSelectedSpatial(names) {
    var available = spatialSamples().map(function (s) { return s.name; });
    names = names.filter(function (name, index) {
      return available.indexOf(name) >= 0 && names.indexOf(name) === index;
    });
    selectedSpatial.forEach(function (name) {
      var sp = spaceById[spatialId(name)]; if (sp) stashImgState(sp);
    });
    selectedSpatial = names;
    rebuildSpatialInstances();
    ensurePanelSlots(orderedSpaces().length);
    buildPanels();
    layoutPanels();
    renderImagePicker();
    activateSpatial(activeSpatialId);
    renderMeta();
    resizeAll();
  }

  // ---- group filters (client-side cell subsetting) ------------------------
  // One collapsible chip per categorical group; unchecking a level drops those
  // cells from every panel and the selection. Reuses the bundle's group values
  // (no server round-trip), the same set the Overview page filters on.
  function renderGroupFilters() {
    var host = $('cv-filters-row'); if (!host) return;
    host.innerHTML = '';
    Object.keys(D.groups).forEach(function (gname) {
      if (gname.indexOf('__single_') === 0) return;
      var g = D.groups[gname];
      if (!g || !g.levels || !g.levels.length) return;
      var allowed = groupFilter[gname];
      var nsel = allowed ? allowed.size : g.levels.length;
      var items = g.levels.map(function (nm, li) {
        var on = !allowed || allowed.has(li);
        var col = (g.colors && g.colors[li]) || PAL[li % PAL.length];
        return '<label class="cv-filt-item"><input type="checkbox" data-lv="' + li +
          '"' + (on ? ' checked' : '') + '><span class="cv-dot" style="background:' +
          cssColor(col) + '"></span>' + esc(nm) + '</label>';
      }).join('');
      var wrap = document.createElement('div');
      wrap.className = 'cv-filt';
      wrap.setAttribute('data-group', gname);
      wrap.innerHTML = '<button type="button" class="cv-filt-btn" aria-expanded="false">' +
        esc(groupLabel(gname)) +
        ' <span class="cv-filt-ct">' + nsel + '/' + g.levels.length +
        '</span></button><div class="cv-filt-menu" style="display:none">' +
        '<div class="cv-filt-acts"><button type="button" data-act="all">All</button>' +
        '<button type="button" data-act="none">None</button></div>' + items + '</div>';
      host.appendChild(wrap);
    });
  }
  function readFilter(wrap) {
    var gname = wrap.getAttribute('data-group');
    var boxes = wrap.querySelectorAll('.cv-filt-menu input[type=checkbox]');
    var chosen = new Set(), total = boxes.length, k = 0;
    Array.prototype.forEach.call(boxes, function (b) {
      if (b.checked) { chosen.add(parseInt(b.getAttribute('data-lv'), 10)); k++; }
    });
    if (k >= total) delete groupFilter[gname];   // all levels on = no filter
    else groupFilter[gname] = chosen;
    applyActiveChange();
  }

  // Read the client-owned histology-image controls into the active section's
  // image state. Missing
  // controls (before render / no image) leave the current value unchanged.
  function syncImgControls(changed) {
    var sp = activeSpatial();
    if (!sp || !currentImage(sp)) return;
    if (!sp._imgState) sp._imgState = presetState(currentImage(sp));
    var imgState = sp._imgState;
    var num = function (id, cur) { var el = $(id); return el ? parseFloat(el.value) : cur; };
    var chk = function (id, cur) { var el = $(id); return el ? el.checked : cur; };
    imgState.show = chk('cv-img-show', imgState.show);
    imgState.opacity = num('cv-img-opacity', imgState.opacity);
    imgState.offsetX = num('cv-img-offx', imgState.offsetX);   // data units
    imgState.offsetY = num('cv-img-offy', imgState.offsetY);   // data units
    // Two axes, read separately. One slider driving both meant that touching
    // ANY control in this bar rewrote the other axis from it, so a preset whose
    // calibration was genuinely non-uniform was squared up by a nudge to the
    // opacity. `changed` is the control the user just moved; with the aspect
    // locked the other follows it, and nothing else in the bar disturbs either.
    var lock = chk('cv-img-lock', true);
    imgState.lock = lock;
    var sxEl = $('cv-img-scalex'), syEl = $('cv-img-scaley');
    var sxv = num('cv-img-scalex', imgState.scaleX);
    var syv = num('cv-img-scaley', imgState.scaleY);
    if (lock && changed === 'cv-img-scalex') {
      syv = sxv; syncImgRangeValue(syEl, sxv);
    } else if (lock && changed === 'cv-img-scaley') {
      sxv = syv; syncImgRangeValue(sxEl, syv);
    } else if (lock && changed === 'cv-img-lock') {
      syv = sxv; syncImgRangeValue(syEl, sxv);
    }
    imgState.scaleX = sxv; imgState.scaleY = syv;
    imgState.rotate = num('cv-img-rotate', imgState.rotate);
    imgState.flipX = chk('cv-img-flipx', imgState.flipX);
    imgState.flipY = chk('cv-img-flipy', imgState.flipY);
    // Write through to the per-image store on every adjustment, so it is always
    // the authority. Recording only on a switch left the work in flight: a
    // bundle re-sent while the user was still adjusting reloaded from the preset
    // and undid it.
    stashImgState(sp);
  }

  // Spaces in panel order: expression, trajectory, physical, immune, then any
  // future modality spaces.
  function baseOrderedSpaces(projections, spatialSections) {
    var configured = arguments.length > 0;
    projections = projections || selectedProjections;
    spatialSections = spatialSections || selectedSpatial;
    var out = [];
    projections.forEach(function (name) {
      var id = projectionId(name);
      if ((configured && D.projections && D.projections[name]) || spaceById[id]) {
        out.push(id);
      }
    });
    D.spaces.forEach(function (space) {
      if (space.trajectory && spaceById[space.id]) out.push(space.id);
    });
    spatialSections.forEach(function (name) {
      var id = spatialId(name); if (configured || spaceById[id]) out.push(id);
    });
    ['trekker', 'clone'].forEach(function (id) {
      if (spaceById[id]) out.push(id);
    });
    D.spaces.forEach(function (s) {
      if (s.id !== 'expression' && s.id !== 'spatial' && out.indexOf(s.id) < 0) {
        out.push(s.id);
      }
    });
    return out;
  }
  function orderedSpaces() {
    if (singleActive && singleSpaceIds.length) return singleSpaceIds.slice();
    var out = baseOrderedSpaces();
    if (colorBy !== GENE_PANELS_MODE || !D.genePanels || !D.genePanels.length) {
      return out;
    }
    genePanelBaseCount = Math.max(1, out.length);
    var maxGenes = Math.max(1, Math.floor(12 / Math.max(1, out.length)));
    var genes = D.genePanels.slice(0, maxGenes), expanded = [];
    var distinct = $('cv-gene-panel-color') &&
      $('cv-gene-panel-color').value === 'distinct';
    var scales = ['Reds', 'Blues', 'Greens', 'YlOrRd'];
    genes.forEach(function (gene, geneIndex) {
      var fieldName = '__linked_gene_' + geneIndex;
      D.fields[fieldName] = {
        label: gene.gene, v: gene.v, min: 0, max: D.genePanelMax || 1,
        scale: 255, unclipped: true,
        palette: singlePalette(distinct ? scales[geneIndex % scales.length] : 'YlOrRd')
      };
      out.forEach(function (spaceId) {
        var source = spaceById[spaceId];
        var virtualId = window.CBViewState.genePanelSpaceId(geneIndex, spaceId);
        if (!source) return;
        spaceById[virtualId] = Object.assign({}, source, {
          id: virtualId,
          _baseSpaceId: spaceId,
          label: gene.gene + ' · ' + source.label,
          _panelColorMode: FIELD_PREFIX + fieldName
        });
        expanded.push(virtualId);
      });
    });
    return expanded;
  }
  // Assign each present space to a panel; hide the unused slots; surface the
  // Trekker info button on whichever panel holds the Trekker space.
  // Fade a pane in from transparent (the .cv-pane opacity transition does the
  // rest) when it appears on a data-set switch, so new panels don't just pop.
  function fadeInPane(el) {
    el.style.opacity = '0';
    requestAnimationFrame(function () {
      requestAnimationFrame(function () { el.style.opacity = ''; });
    });
  }
  function layoutPanels() {
    var order = orderedSpaces();
    // A new data set is a new set of spaces; the previous focus may no longer
    // exist in the new layout.
    focusPanel = null;
    panels.forEach(function (p) {
      if (p.pane) {
        p.pane.classList.remove('cv-focus-primary', 'cv-focus-context');
        p.pane.style.order = '';
      }
      var fb = p.pane && p.pane.querySelector('.cv-focus-btn');
      if (fb) {
        fb.classList.remove('is-on');
        fb.setAttribute('aria-pressed', 'false');
        fb.setAttribute('data-tip', 'Make this the focus');
        fb.setAttribute('aria-label', 'Make this the focus');
        var fl = fb.querySelector('.cv-focus-label');
        if (fl) fl.textContent = 'Focus';
      }
      var rb = p.pane && p.pane.querySelector('.cv-role-badge');
      if (rb) {
        rb.style.display = 'none';
        rb.textContent = '';
        rb.classList.remove('is-focus', 'is-context');
      }
    });
    var host0 = panels[0] && panels[0].pane && panels[0].pane.parentElement;
    if (host0) host0.classList.remove('cv-has-focus');
    panels.forEach(function (p, i) {
      if (i < order.length) {
        var reappearing = p.pane && p.pane.classList.contains('cv-hidden');
        p.spaceId = order[i];
        var shownSpace = spaceById[p.spaceId];
        p.colorBy = singleActive ? singleSpaceModes[p.spaceId] || null
          : (shownSpace && shownSpace._panelColorMode) || null;
        clearPanelView(p);
        if (p.pane) {
          p.pane.classList.remove('cv-hidden');
          if (reappearing && !singleActive) fadeInPane(p.pane);
        }
        var title = $('cv-title-' + p.key.toLowerCase());
        shownSpace = spaceById[p.spaceId];
        if (title) title.textContent = shownSpace ? shownSpace.label : p.spaceId;
      } else {
        p.spaceId = null;
        p.colorBy = null;
        if (p.pane) p.pane.classList.add('cv-hidden');
      }
      updateCanvasDescription(p);
    });
    updateMoranBadges();
    // Trekker info button lives on the Trekker panel only.
    var showTk = !!(D.trekker && D.trekker.qc);
    panels.forEach(function (p) {
      var btn = $('cv-tk-info-' + p.key.toLowerCase());
      if (btn) btn.style.display = (showTk && panelIsTrekker(p)) ? '' : 'none';
    });
    updateFocusButtons();
    syncOrbitButtons();
    // A panel may now point at a different section while its box dimensions are
    // unchanged. Force the next resize pass to project the new coordinates.
    _layoutKey = null;
  }
  // Size visible panels from the available width. Each canvas remains at least
  // 300px wide when possible and extra spaces flow onto later rows.
  var MIN_SIDE = 300;
  var VIEWPORT_GUTTER = 7;
  // Prefer a balanced packing whenever it keeps at least this much of the
  // largest available panel size. This prevents sparse final rows without
  // forcing plots into a materially smaller layout.
  var BALANCED_SIZE_TOLERANCE = 0.85;
  // Context is a persistent orientation aid, not the place for close reading.
  // Let it become slightly smaller while a primary lens is present so a common
  // 1280px workspace can keep both roles in the same visual field.
  var FOCUS_CONTEXT_SIDE = 260;
  // The fitted size is computed from the pane header's height, and the header
  // grows when the pane is narrow enough for its toolbar to wrap. That is a
  // positive feedback loop: a smaller plot makes a taller header makes a
  // smaller plot. Reproduced when the histology bar appeared -- 26 shrinking
  // steps over three seconds, from 240px down to
  // the floor, while every INPUT (available width, the panels' top, the legend's
  // height, the window) stayed exactly the same. It reads as the panels
  // shivering.
  //
  // So: one corrective pass per genuine change of those inputs, and no more.
  // The first pass sizes from the header as it was; the second takes account of
  // a wrap that pass caused; anything after that is the loop feeding itself.
  var _layoutKey = null, _layoutPass = 0;
  //
  // The width and height are QUANTISED to 24px. A classic (space-taking)
  // scrollbar is the other way this feeds back on itself: sizing the grid to
  // just fill the viewport makes the page overflow by a hair, the scrollbar
  // appears and takes ~15px of width, the grid is re-fitted smaller, the
  // overflow goes away, the scrollbar leaves, and it starts again -- a loop with
  // no end, and the one that looks like shaking rather than settling. It cannot
  // be reproduced in a headless browser, whose scrollbars are overlays and take
  // no width at all. Ignoring sub-24px changes costs nothing on a grid of
  // hundreds of pixels and leaves no way for that cycle to continue.
  function layoutInputs(host) {
    var q = function (v) { return Math.round(v / 24); };
    return [
      q(host ? host.clientWidth : 0),
      q(host ? host.getBoundingClientRect().top : 0),
      ($('cv-legend') || {}).offsetHeight || 0,
      ($('cv-cbar') && $('cv-cbar').style.display !== 'none')
        ? $('cv-cbar').offsetHeight : 0,
      q(window.innerHeight),
      panels.filter(function (p) { return p.spaceId; }).length,
      focusPanel || '',
      keepPlotsSquare ? 'square' : 'fluid'
    ].join('|');
  }

  // Find a readable, balanced overview for any panel count. First keep layouts
  // close to the largest feasible panel size, then prefer complete rows, fewer
  // rows, and no one-panel orphan row. This remains independent of panel order:
  // adding a linked space changes only the packing, never visual geometry.
  function bestOverviewGrid(panelCount, availW, availH, chromeX, chromeY, gap) {
    var candidates = [];
    for (var cols = 1; cols <= panelCount; cols++) {
      var rows = Math.ceil(panelCount / cols);
      var widthSide = Math.floor(
        (availW - (cols - 1) * gap) / cols - chromeX
      );
      var heightSide = Math.floor(
        (availH - (rows - 1) * gap) / rows - chromeY
      );
      var side = Math.min(widthSide, heightSide);
      var lastRow = panelCount - (rows - 1) * cols;
      candidates.push({
        cols: cols,
        rows: rows,
        side: side,
        orphan: rows > 1 && cols > 1 && lastRow === 1,
        fill: panelCount / (rows * cols)
      });
    }
    var feasible = candidates.filter(function (candidate) {
      return candidate.side >= MIN_SIDE;
    });
    var pool = feasible.length ? feasible : candidates;
    var maxSide = pool.reduce(function (largest, candidate) {
      return Math.max(largest, candidate.side);
    }, 0);
    var balanced = feasible.length
      ? pool.filter(function (candidate) {
        return candidate.side >= maxSide * BALANCED_SIZE_TOLERANCE;
      })
      : pool.filter(function (candidate) {
        return candidate.side === maxSide;
      });
    balanced.sort(function (a, b) {
      if (a.orphan !== b.orphan) return a.orphan ? 1 : -1;
      if (a.fill !== b.fill) return b.fill - a.fill;
      if (a.rows !== b.rows) return a.rows - b.rows;
      return b.side - a.side;
    });
    return balanced[0];
  }

  function resizeAll() {
    if (!D || !panels.length) return false;
    var panes = panels[0].pane && panels[0].pane.parentElement;
    if (!panes) return false;
    var vis = panels.filter(function (p) { return p.spaceId; });
    var k = vis.length;
    if (!k) return false;
    var availW = panes.clientWidth;
    if (availW < 20) return false;           // tab still hidden; observer re-runs
    var key = layoutInputs(panes);
    if (key === _layoutKey) {
      if (_layoutPass >= 2) return false;
      _layoutPass++;
    } else {
      _layoutKey = key;
      _layoutPass = 1;
    }
    // CSS owns the flex layout. Read its actual gap so the width calculation
    // cannot drift from the browser and push the last pane onto a new row.
    var gap = parseFloat(getComputedStyle(panes).columnGap) || 0;
    // Each pane is border-box with 8px padding + 1px border, so its content is
    // 18px narrower than its column track. The canvas takes that full width;
    // height is still fitted to the viewport by the existing overview logic.
    var chromeX = 18;
    var firstCanvas = vis[0] && vis[0].canvas;
    var firstPane = vis[0] && vis[0].pane;
    var overhead = firstPane && firstCanvas
      ? Math.max(0, firstPane.offsetHeight - firstCanvas.clientHeight) : 58;
    var scrollHost = panes.closest('.content-wrapper');
    var visibleBottom = scrollHost
      ? scrollHost.getBoundingClientRect().bottom : window.innerHeight;
    var bottomPad = scrollHost
      ? (parseFloat(getComputedStyle(scrollHost).paddingBottom) || 0) : 0;
    var availH = Math.floor(
      visibleBottom - panes.getBoundingClientRect().top - bottomPad -
      2 * VIEWPORT_GUTTER
    );
    var usableW = availW;

    // Overview is a genuine two-dimensional fit: try every possible column
    // count and choose the one that maximises a balanced cell. If there are so
    // many panels that the result would be unreadable, retain the 300px floor
    // and let the workspace scroll instead of reducing plots to thumbnails.
    var overview = !focusPanel
      ? bestOverviewGrid(k, usableW, Math.max(1, availH), chromeX, overhead, gap)
      : null;
    var columnFloor = focusPanel ? FOCUS_CONTEXT_SIDE : MIN_SIDE;
    var availableCols = Math.max(1,
      Math.floor((usableW + gap) / (columnFloor + chromeX + gap)));
    var cols = focusPanel
      ? Math.max(1, Math.min(k + 1, availableCols))
      : Math.max(1, Math.min(overview ? overview.cols : 1, availableCols));
    var single = cols === 1;
    var colW = (usableW - (cols - 1) * gap) / cols;
    var side = focusPanel
      ? Math.max(FOCUS_CONTEXT_SIDE, Math.floor(colW - chromeX))
      : Math.max(MIN_SIDE, overview ? overview.side : 0);
    if (single && side + chromeX > usableW) {
      side = Math.max(150, usableW - chromeX);
    }
    // A lone/focused card should use the available HEIGHT as well as width.
    // Without this cap a wide monitor produced a 1200px square that continued
    // far below the viewport. Multi-card grids deliberately keep the 300px
    // width floor and may scroll vertically; a focused lens is still capped
    // unless that would make it smaller than its persistent context lenses.
    var focusSide = side;
    if (focusPanel && cols > 1) {
      focusSide = Math.floor((side + chromeX) * 2 + gap - chromeX);
    }
    if (single || focusPanel) {
      var focusObj = null;
      vis.forEach(function (p) { if (p.key === focusPanel) focusObj = p; });
      firstPane = (focusObj || vis[0]) && (focusObj || vis[0]).pane;
      var canvas = (focusObj || vis[0]) && (focusObj || vis[0]).canvas;
      overhead = firstPane && canvas
        ? Math.max(0, firstPane.offsetHeight - canvas.clientHeight) : 58;
      scrollHost = panes.closest('.content-wrapper');
      visibleBottom = scrollHost
        ? scrollHost.getBoundingClientRect().bottom : window.innerHeight;
      bottomPad = scrollHost
        ? (parseFloat(getComputedStyle(scrollHost).paddingBottom) || 0) : 0;
      availH = Math.floor(
        visibleBottom - panes.getBoundingClientRect().top - bottomPad - 8
      );
      if (availH > 0) {
        if (focusPanel) {
          focusSide = Math.min(focusSide, Math.max(MIN_SIDE, availH - overhead));
          // Focus hierarchy wins over one-viewport packing. When the controls
          // and cohort bar leave little height, squeezing the primary below its
          // context reverses the meaning of the layout; let the page scroll.
          focusSide = Math.max(
            focusSide,
            Math.min(Math.floor((side + chromeX) * 2 + gap - chromeX),
              Math.round(side * 1.4))
          );
        } else {
          side = Math.min(side, Math.max(MIN_SIDE, availH - overhead));
        }
      }
    }
    // Explicit tracks consume all available width. In the default fluid mode,
    // canvases also consume their row's share of available height. Square mode
    // constrains both dimensions to the shorter side.
    var rows = Math.ceil(k / cols);
    var panelHeight = Math.max(
      MIN_SIDE,
      Math.floor((availH - (rows - 1) * gap) / rows - overhead)
    );
    var panelWidth = Math.max(150, Math.floor(colW - chromeX));
    var primaryWidth = cols > 1
      ? Math.max(panelWidth, Math.floor(colW * 2 + gap - chromeX))
      : panelWidth;
    vis.forEach(function (p) {
      var primary = !!focusPanel && p.key === focusPanel;
      var width = primary ? primaryWidth : panelWidth;
      var height = primary ? focusSide : (focusPanel ? side : panelHeight);
      var paneWidth = primary ? primaryWidth + chromeX : colW;
      if (keepPlotsSquare) {
        var squareSide = Math.min(width, height);
        width = squareSide;
        height = squareSide;
      }
      if (p.pane) {
        p.pane.style.flexBasis = paneWidth + 'px';
        p.pane.style.width = paneWidth + 'px';
        p.pane.style.height = '';
      }
      resizePanel(p, width, height);
    });
    if (!psSeeded) autoPointSize(side);
    // Visibility of focus affordances depends on how many linked lenses remain.
    updateFocusButtons();
    drawAll();
    if ((sel && sel.size) || (pick != null && nicheSet)) renderSelbar();
    // Layout advice uses canvas CSS dimensions, never the spread of the cells.
    var hintView = singleActive && singleView(singleActive);
    var hintId = singleActive || 'linked';
    var hintPanels = vis.filter(function (p) {
      var space = spaceById[p.spaceId];
      var name = hintView ? hintView.meta && hintView.meta.space_label : space && space._projectionName;
      return space && !space.z && (hintId === 'trajectory_projection' ||
          /umap/i.test(String(name || '')));
    });
    window.dispatchEvent(new CustomEvent('cerebro:plot-layout', { detail: {
      id: hintId, dataset_context: currentDatasetContext(), square: keepPlotsSquare,
      eligible: hintPanels.length > 0,
      wide: hintPanels.some(function (p) { return p.W > 0 && p.H > 0 && p.W / p.H > 1.6; })
    } }));
    return true;
  }

  // ---- receive the bundle --------------------------------------------------
  // Blank the workspace when the current data set has nothing to link. Every
  // element that could still be showing the PREVIOUS data set is cleared —
  // canvases, legend, readout, selection bar — including the server-side
  // selection that drives the selected-cell plot and table, which would
  // otherwise describe cells from a data set no longer on screen.
  function showUnavailable(msg) {
    closeCard(); cardMeta = null;
    unpinTip();
    D = null; spaceById = {}; sel = null; selectionSource = null;
    selectionSourceSpace = null;
    pick = null; nicheSet = null;
    hoverCell = null; focusPanel = null;
    hidden = new Set(); groupFilter = {};
    panels.forEach(function (p) {
      p.spaceId = null; p.sx = null; p.sy = null; p.ok = null;
      p.gpuTransformOnly = false;
      p.lasso = null; p.lassoData = null; p.view = null;
      p.miniBg = null; p.miniUnit = null;
      if (p.mini) p.mini.classList.remove('is-on');
      if (p.ctx) p.ctx.clearRect(0, 0, p.W, p.H);
      if (p.interactionCtx) p.interactionCtx.clearRect(0, 0, p.W, p.H);
      hideGpu(p);
      if (p.pane) p.pane.classList.add('cv-hidden');
    });
    var meta = $('cv-meta');
    if (meta) {
      meta.textContent = msg ||
        'This data set has no dimensional reduction to link its modalities on.';
    }
    var L = $('cv-legend'); if (L) L.innerHTML = '';
    var C = $('cv-cbar'); if (C) C.style.display = 'none';
    var R = $('cv-readout');
    if (R) {
      R.innerHTML = '';
      R.style.display = 'none';
    }
    ['cv-workspace-guide', 'cv-selbar', 'cv-selactions', 'cv-shown', 'cv-trekker-ctl',
      'cv-tk-insights', 'cv-clone-layout-ctl'].forEach(function (id) {
      var el = $(id); if (el) el.style.display = 'none';
    });
    setTrekkerSettingsVisible(false);
    setTrekkerInsightsOpen(false);
    closeMore();
    reportSelection();
  }

  function applyColorPatch(patch) {
    if (!D || !patch || !datasetMessageFresh(patch) ||
        !sameDatasetContext(patch.dataset_context, D.dataset_context)) return false;
    ['groups', 'cat_extra'].forEach(function (kind) {
      var update = patch[kind] || {};
      var target = D[kind] || {};
      Object.keys(update).forEach(function (name) {
        if (target[name] && Array.isArray(update[name])) {
          target[name].colors = update[name];
        }
      });
    });
    sanitiseColors(D);
    renderLegend(); drawAll();
    return true;
  }

  // ---- single-view adapter ------------------------------------------------
  // Dedicated pages keep their existing R data preparation and controls, but
  // mount this workspace's panel surface and renderer. Only one dashboard tab
  // is visible at a time, so one physical surface can serve every page without
  // duplicating canvases, listeners or viewport state machines.
  var rebuildingBase = false;
  function singleHost(id) {
    return document.getElementById(id + '_cell_view_host');
  }
  function visibleSingleId() {
    // Toolbar buttons share the data attribute, while an empty active host still
    // has a client rect before the shared canvas is mounted into it.
    var hosts = document.querySelectorAll('.cerebro-cell-view-host[data-cell-view-id]');
    for (var i = 0; i < hosts.length; i++) {
      if (hosts[i].getClientRects().length) return hosts[i].dataset.cellViewId;
    }
    return null;
  }
  function rememberSurfaceHome() {
    if (surfaceHome) return;
    var legend = $('cv-legend'), cbar = $('cv-cbar');
    var insights = $('cv-tk-insights');
    var panes = document.querySelector('#coordviews .cv-panes') ||
      document.querySelector('.coordviews-page .cv-panes');
    if (!panes) return;
    surfaceHome = {
      panes: panes, panesParent: panes.parentNode, panesNext: panes.nextSibling,
      legend: legend, legendParent: legend && legend.parentNode,
      legendNext: legend && legend.nextSibling,
      cbar: cbar, cbarParent: cbar && cbar.parentNode,
      cbarNext: cbar && cbar.nextSibling,
      insights: insights, insightsParent: insights && insights.parentNode,
      insightsNext: insights && insights.nextSibling
    };
  }
  function restoreNode(node, parent, next) {
    if (!node || !parent || node.parentNode === parent) return;
    parent.insertBefore(node, next && next.parentNode === parent ? next : null);
  }
  function restoreLinkedSurface() {
    if (!surfaceHome) return;
    // A specialist network may have disabled this shared canvas in static mode.
    // Linked views always restores its zoom and selection controls.
    panels.forEach(function (panel) {
      if (panel.canvas) panel.canvas.style.pointerEvents = '';
      if (panel.pane) panel.pane.classList.remove('cv-pane-static');
    });
    restoreNode(surfaceHome.panes, surfaceHome.panesParent, surfaceHome.panesNext);
    restoreNode(surfaceHome.cbar, surfaceHome.cbarParent, surfaceHome.cbarNext);
    restoreNode(surfaceHome.legend, surfaceHome.legendParent, surfaceHome.legendNext);
    restoreNode(
      surfaceHome.insights,
      surfaceHome.insightsParent,
      surfaceHome.insightsNext
    );
  }
  function singleInteractionConfig(view) {
    var config = view && view.extra && view.extra.interaction;
    var threshold = Number(config && config.static_above);
    return isFinite(threshold) && threshold >= 0
      ? { threshold: threshold } : null;
  }
  function singleInteractionRecommendedStatic(view) {
    var config = singleInteractionConfig(view);
    var count = Number(view && view.data && view.data.n);
    if (!isFinite(count)) count = singlePayloadCells(view).length;
    return !!config && count > config.threshold;
  }
  function singleInteractionIsStatic() {
    var view = singleActive && singleView(singleActive);
    return singleInteractionRecommendedStatic(view) &&
      view.interactionEnabled !== true;
  }
  function updateSingleInteractionMode() {
    var view = singleActive && singleView(singleActive);
    var host = singleActive && singleHost(singleActive);
    if (!view || !host) return;
    var recommended = singleInteractionRecommendedStatic(view);
    var isStatic = recommended && view.interactionEnabled !== true;
    var count = Number(view.data && view.data.n);
    if (!isFinite(count)) count = singlePayloadCells(view).length;
    host.classList.toggle('cv-static-performance', isStatic);
    var notice = host.querySelector('.cv-performance-mode');
    if (notice) {
      notice.hidden = !recommended;
      var message = notice.querySelector('.cv-performance-mode-text');
      if (message) {
        message.textContent = isStatic
          ? 'Performance mode: ' + count.toLocaleString() +
            ' nodes are shown as a static network.'
          : 'Interaction enabled for this ' + count.toLocaleString() +
            '-node network.';
      }
      var button = notice.querySelector('button');
      if (button) {
        button.textContent = isStatic ? 'Enable interaction' : 'Use static mode';
        button.setAttribute('aria-pressed', isStatic ? 'false' : 'true');
      }
    }
    panels.forEach(function (panel) {
      if (panel.canvas) panel.canvas.style.pointerEvents = isStatic ? 'none' : '';
      if (panel.pane) panel.pane.classList.toggle('cv-pane-static', isStatic);
    });
    if (isStatic) {
      panels.forEach(function (panel) {
        panel.drag = false; panel.panning = false; panel.orbiting = false;
        var tip = $(panel.tipId); if (tip) tip.style.opacity = 0;
      });
      setHoverCell(null);
    }
  }
  function toggleSingleInteraction() {
    var view = singleActive && singleView(singleActive);
    if (!view || !singleInteractionRecommendedStatic(view)) return;
    view.interactionEnabled = singleInteractionIsStatic();
    updateSingleInteractionMode();
  }
  function resetSingleViews() {
    restoreLinkedSurface();
    singleAuxCache = {};
    Object.keys(singleViews).forEach(function (cacheKey) {
      var view = singleViews[cacheKey];
      if (view && view._auxTimer) clearTimeout(view._auxTimer);
      if (view) { view._auxTimer = null; view._auxPending = null; }
    });
    singleViews = {}; singleActive = null;
    singleRequests.clear();
    singleSpaceIds = []; singleSpaceModes = {};
    singleIndexCells = null; singleIndexMap = null;
    linkedState = null;
    singleTiming = Object.create(null);
    singleResourceDescriptors.clear();
    singleDefaultResourceNames.clear();
    singleCategoryResourceDescriptors.clear();
  }
  function markSingleHosts(phase, context) {
    var hosts = document.querySelectorAll('[data-cell-view-id]');
    Array.prototype.forEach.call(hosts, function (host) {
      host.dataset.renderState = phase || 'idle';
      if (context) {
        host.dataset.datasetEpoch = String(context.epoch);
        host.dataset.datasetKey = String(context.dataset_key);
        host.dataset.renderGeneration = String(context.generation);
      } else {
        delete host.dataset.datasetEpoch;
        delete host.dataset.datasetKey;
        delete host.dataset.renderGeneration;
      }
      if (phase !== 'ready') {
        delete host.dataset.backgroundIdentity;
        delete host.dataset.pointCount;
      }
    });
  }
  function invalidateDatasetViewState(detail) {
    setLinkedPresentationReady(false);
    resetSingleViews();
    clearStaticResourceCaches();
    // Deferred image keys include the exact generation; retired entries cannot
    // be reused, even when the same dataset is selected again.
    assetCache = Object.create(null);
    pendingAssets.clear();
    _fldCacheD = null; _fldCacheVal = null;
    Object.keys(spaceById || {}).forEach(function (spaceId) {
      var space = spaceById[spaceId];
      if (!space) return;
      space._imgToken = ++imgToken;
      space._imgEl = null;
      space._imgReady = false;
    });
    imgToken++;
    imgStates = {};
    imgChoice = {};
    geneWanted = null;
    pendingColorPatch = null;
    progressiveRequestedKey = null;
    deferredLinkedSupplements = [];
    linkedBundle = null;
    sel = null;
    selectionSource = null;
    selectionSourceSpace = null;
    pick = null;
    hoverCell = null;
    D = null;
    dataShown = null;
    spaceById = {};
    selectedSpatial = [];
    activeSpatialId = null;
    spatialTemplate = null;
    markSingleHosts(
      detail && detail.phase === 'ready'
        ? 'waiting'
        : detail && detail.phase || 'pending',
      detail && detail.dataset_context || currentDatasetContext()
    );
    showUnavailable(
      detail && detail.phase === 'error'
        ? 'This data set could not be loaded.'
        : 'Loading data set…'
    );
  }
  function mountSingleSurface(id) {
    rememberSurfaceHome();
    var host = singleHost(id), surface = host && host.querySelector('.cerebro-cell-view-surface');
    if (!surface || !surfaceHome) return false;
    surface.innerHTML = '';
    var legend = document.createElement('div');
    legend.className = 'cerebro-cell-view-legend';
    var performanceMode = document.createElement('div');
    performanceMode.className = 'cv-performance-mode';
    performanceMode.hidden = true;
    var performanceText = document.createElement('span');
    performanceText.className = 'cv-performance-mode-text';
    var performanceButton = document.createElement('button');
    performanceButton.type = 'button';
    performanceButton.className = 'btn btn-xs btn-default';
    performanceButton.addEventListener('click', toggleSingleInteraction);
    performanceMode.appendChild(performanceText);
    performanceMode.appendChild(performanceButton);
    surface.appendChild(performanceMode);
    surface.appendChild(legend); surface.appendChild(surfaceHome.panes);
    if (surfaceHome.legend) legend.appendChild(surfaceHome.legend);
    if (surfaceHome.cbar) legend.appendChild(surfaceHome.cbar);
    var insightsHost = document.getElementById(id.replace(/_projection$/, '') +
      '_shared_insights');
    if (insightsHost && surfaceHome.insights) {
      insightsHost.appendChild(surfaceHome.insights);
    }
    return true;
  }
  function singleIndex() {
    var cells = D && D.cells;
    if (cells === singleIndexCells && singleIndexMap) return singleIndexMap;
    var out = new Map();
    if (Array.isArray(cells)) {
      cells.forEach(function (cell, i) { out.set(String(cell), i); });
    }
    singleIndexCells = cells; singleIndexMap = out;
    return singleIndexMap;
  }
  function emptyVector(value) {
    return new Array(D && (D.n || (D.cells || []).length) || 0).fill(value);
  }
  function singlePayloadCells(payload) {
    var keys = payload && payload.data && payload.data.selection_key;
    if (!Array.isArray(keys)) return [];
    if (!keys.length || !Array.isArray(keys[0])) return keys.map(String);
    return keys.reduce(function (cells, group) {
      return cells.concat((group || []).map(String));
    }, []);
  }
  function singlePayloadBundle(id, payload) {
    var cells = singlePayloadCells(payload);
    var n = Number(payload.data && payload.data.n) || cells.length;
    var identity = payload.datasetIdentity || {};
    return {
      dataset_id: payload.dataset_context.dataset_key,
      dataset_context: payload.dataset_context,
      dataset_generation: payload.dataset_context.generation,
      dataset_fingerprint: identity.cell_fingerprint || '',
      cells: cells,
      n: n,
      groups: {},
      cat_extra: {},
      fields: {},
      projections: {},
      spaces: []
    };
  }
  function sharedBase() {
    var identity = window.cerebroSavedViewDataset || {};
    window.CerebroSharedDatasetState = window.CBViewState.sharedBase(
      window.CerebroSharedDatasetState,
      String(identity.cell_fingerprint || ''),
      Number(identity.cell_count) || 0,
      String(identity.cell_order_fingerprint || '')
    );
    return window.CerebroSharedDatasetState;
  }
  function cacheSharedProjection(name, space, n) {
    var base = sharedBase();
    if (!name || !space || !base.datasetFingerprint || !base.orderFingerprint ||
        base.cellCount !== n ||
        !space.x || space.x.length !== n || !space.y || space.y.length !== n) return;
    var previous = base.projections[name];
    var sameCoordinates = !!(previous && previous.x === space.x &&
      previous.y === space.y && (previous.z || null) === (space.z || null));
    if (previous && !sameCoordinates) {
      delete base.gpuPositions[name];
    }
    var cachedUnit = sameCoordinates ? previous.unit : null;
    if (cachedUnit && !space._unit) space._unit = cachedUnit;
    base.projections[name] = {
      x: space.x,
      y: space.y,
      z: space.z || null,
      unit: cachedUnit,
      gpuPositionCount: sameCoordinates ? previous.gpuPositionCount : null
    };
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('coordviews_shared_base', {
        dataset_context: currentDatasetContext(),
        dataset_fingerprint: base.datasetFingerprint,
        cell_count: base.cellCount,
        canonical_order_id: base.orderFingerprint,
        projection: name
      }, { priority: 'event' });
    }
  }
  function reuseSharedProjection(bundle) {
    var name = bundle && bundle.shared_projection;
    var base = name && sharedBase();
    var cached = window.CBViewState.sharedProjection(
      base, name, bundle && bundle.n
    );
    var projection = bundle && bundle.projections && bundle.projections[name];
    if (!cached || !projection) return bundle;
    projection.x = cached.x; projection.y = cached.y;
    if (cached.z) projection.z = cached.z;
    return bundle;
  }
  function reuseSharedSingleProjection(message) {
    var name = message && message.shared_projection;
    if (!name) return message;
    var data = message.data || {};
    var cached = window.CBViewState.sharedProjection(
      sharedBase(), name, Number(data.n) || 0
    );
    if (!cached) return null;
    data.x = cached.x; data.y = cached.y;
    if (cached.z) data.z = cached.z;
    message.data = data;
    return message;
  }
  function resourceCacheKey(resource, suffix) {
    return String(resource.url) + ':' + String(resource.checksum || '') +
      (suffix ? ':' + suffix : '');
  }
  function fetchCachedResource(resource, cache, label, suffix, decode) {
    var key = resourceCacheKey(resource, suffix);
    var pending = cache.get(key);
    var cacheHit = !!pending;
    if (!pending) {
      var downloadStarted = performance.now();
      pending = window.fetch(String(resource.url), {
        credentials: 'same-origin', cache: 'force-cache'
      }).then(function (response) {
        if (!response.ok) throw new Error(label + ' request failed');
        return response.arrayBuffer();
      }).then(function (buffer) {
        var downloadedAt = performance.now();
        return Object.assign(decode(buffer), {
          downloadMs: downloadedAt - downloadStarted,
          decodeMs: performance.now() - downloadedAt
        });
      });
      cache.set(key, pending);
      pending.catch(function () {
        // A failed old request must not evict a new generation's same-URL fetch.
        if (cache.get(key) === pending) cache.delete(key);
      });
    }
    var started = performance.now();
    // The decoded entry is shared by every caller; timings are per call.
    return pending.then(function (entry) {
      return Object.assign({}, entry, {
        fetchMs: performance.now() - started, cacheHit: cacheHit
      });
    });
  }
  var projectionResourceCache = new Map();
  function fetchProjectionResource(resource, expectedCells) {
    var descriptor = window.CBViewState.resourceDescriptor.validate(resource, {
      cells: expectedCells, dimensionSet: [2, 3], requireUrl: true
    });
    if (!descriptor) {
      return Promise.reject(new Error('Projection resource shape mismatch'));
    }
    var n = descriptor.cells;
    var dimensions = descriptor.dimensions;
    return fetchCachedResource(
      resource, projectionResourceCache, 'Projection resource', '',
      function (buffer) {
        if (Number(resource.bytes) && buffer.byteLength !== Number(resource.bytes)) {
          throw new Error('Projection resource byte count mismatch');
        }
        var interleaved = new Float32Array(buffer);
        if (interleaved.length !== n * dimensions) {
          throw new Error('Projection resource length mismatch');
        }
        var x = new Float32Array(n), y = new Float32Array(n);
        var z = dimensions === 3 ? new Float32Array(n) : null;
        var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
        for (var i = 0; i < n; i++) {
          var offset = i * dimensions;
          x[i] = interleaved[offset];
          y[i] = interleaved[offset + 1];
          if (isFinite(x[i]) && isFinite(y[i])) {
            if (x[i] < x0) x0 = x[i];
            if (x[i] > x1) x1 = x[i];
            if (y[i] < y0) y0 = y[i];
            if (y[i] > y1) y1 = y[i];
          }
          if (z) z[i] = interleaved[offset + 2];
        }
        return {coordinates: {
          x: x, y: y, z: z,
          xRange: !z && isFinite(x0) && x1 > x0 ? [x0, x1] : null,
          yRange: !z && isFinite(y0) && y1 > y0 ? [y0, y1] : null,
          bytes: buffer.byteLength
        }};
      }
    );
  }
  var projectionSubsetResourceCache = new Map();
  function canonicalResourceIdentityMatches(resource, identity) {
    return window.CBViewState.canonicalMapping.resourceIdentityMatches(
      resource, identity
    );
  }
  function canonicalDatasetIdentityMatches(resource, identity) {
    return window.CBViewState.canonicalMapping.datasetIdentityMatches(
      resource, identity
    );
  }
  function canonicalProjectionIdentityMatches(resource, message) {
    return window.CBViewState.canonicalMapping.projectionIdentityMatches(
      resource, message && message.dataset_identity
    );
  }
  function fetchValidatedProjectionResource(resource, message, expectedCells) {
    if (resource && resource.protocol === 'canonical-projection-v1' &&
        !canonicalProjectionIdentityMatches(resource, message)) {
      return Promise.reject(new Error('Canonical projection identity mismatch'));
    }
    return fetchProjectionResource(resource, expectedCells);
  }
  function fetchValidatedSingleGeometryResource(
    resource, message, expectedCells
  ) {
    if (resource && resource.protocol === 'spatial-geometry-v1') {
      if (!window.CBViewState.resourceDescriptor.validate(resource, {
        protocol: 'spatial-geometry-v1', dtype: 'float32', dimensions: 2,
        cells: expectedCells, requireUrl: true,
        identity: message && message.dataset_identity
      })) {
        return Promise.reject(new Error('Spatial geometry identity mismatch'));
      }
      return fetchProjectionResource(resource, expectedCells);
    }
    return fetchValidatedProjectionResource(resource, message, expectedCells);
  }
  function registerSingleGeometryResource(message) {
    var resource = message && message.resource;
    var identity = message && message.dataset_identity;
    if (!message || !resource) return;
    var isSpatial = message.id === 'spatial_projection' &&
      resource.spatial_name &&
      window.CBViewState.resourceDescriptor.validate(resource, {
        protocol: 'spatial-geometry-v1', dtype: 'float32', dimensions: 2,
        minCells: 1, identity: identity
      });
    var isOverview = message.id === 'overview_projection' &&
      resource.projection_name &&
      window.CBViewState.resourceDescriptor.validate(resource, {
        protocol: 'canonical-projection-v1', dtype: 'float32',
        minCells: 1, identity: identity
      });
    if (!isSpatial && !isOverview) return;
    singleResourceDescriptors.set(
      message.id + ':' + String(
        isSpatial ? resource.spatial_name : resource.projection_name
      ),
      message
    );
  }
  function cacheSingleColor(key, values) {
    if (!key || !ArrayBuffer.isView(values)) return values;
    singleColorCache.delete(key);
    singleColorCache.set(key, values);
    while (singleColorCache.size > SINGLE_COLOR_CACHE_LIMIT) {
      singleColorCache.delete(singleColorCache.keys().next().value);
    }
    return values;
  }
  function hydrateColorPacket(packet, key) {
    if (!packet) return null;
    var n = Number(packet.length);
    var cached = key && singleColorCache.get(key);
    if (cached && cached.length === n) {
      return cacheSingleColor(key, cached);
    }
    if (!Number.isInteger(n) || n < 0) return null;
    var source = packet.color;
    if (packet.protocol === 'dense-f64-v1') {
      if (!ArrayBuffer.isView(source) && !Array.isArray(source)) return null;
      if (source.length !== n) return null;
      var dense = source instanceof Float64Array
        ? source : Float64Array.from(source, function(value) {
          return value === null ? NaN : Number(value);
        });
      return cacheSingleColor(key, dense);
    }
    if (packet.protocol !== 'sparse-f64-v1') return null;
    var index = packet.index;
    if ((!ArrayBuffer.isView(index) && !Array.isArray(index)) ||
        (!ArrayBuffer.isView(source) && !Array.isArray(source)) ||
        index.length !== source.length) return null;
    var values = new Float64Array(n);
    for (var i = 0; i < index.length; i++) {
      var at = Number(index[i]);
      if (at >= 0 && at < n) {
        values[at] = source[i] === null ? NaN : Number(source[i]);
      }
    }
    return cacheSingleColor(key, values);
  }
  function hydrateSparseSingleColor(data) {
    var sparse = data && data.sparse_color;
    var key = String(data.color_cache_key || '');
    var values = hydrateColorPacket(sparse, key);
    if (values) data.color = values;
  }
  function hydrateColorPacketMap(data, packetField, keyField, targetField) {
    var packets = data && data[packetField];
    if (!packets || typeof packets !== 'object') return;
    var keys = data[keyField] || {}, target = {};
    Object.keys(packets).forEach(function (name) {
      var values = hydrateColorPacket(packets[name], String(keys[name] || ''));
      if (values) target[name] = values;
    });
    if (Object.keys(target).length) {
      data[targetField] = target;
    }
  }
  function registerSingleCategoryResource(message) {
    var resource = message && message.resource;
    var identity = message && message.dataset_identity;
    if (!message || !resource || !resource.metadata_name ||
        resource.protocol !== 'canonical-metadata-codes-v1' ||
        !window.CBViewState.resourceDescriptor.validate(resource, {
          dtypes: ['uint8', 'uint16'], minCells: 1, identity: identity
        })) return;
    singleCategoryResourceDescriptors.set(
      message.id + ':' + String(resource.metadata_name), message
    );
  }
  function singleGeometryRequestEligible(id) {
    var percentage = document.getElementById(
      id + '_percentage_cells_to_show'
    );
    // A specialist page's dynamic controls do not exist yet on its first
    // click. The server catalog supplies a default resource name only when the
    // configured initial frame is the full canonical cell set, so that case is
    // safe to prefetch before the controls bind.
    if (!percentage) {
      return singleDefaultResourceNames.has(id);
    }
    if (Number(percentage.value) !== 100) return false;
    var filters = document.querySelectorAll(
      '[id^="' + id + '_group_filter_"]'
    );
    for (var i = 0; i < filters.length; i++) {
      var boxes = filters[i].querySelectorAll('input[type="checkbox"]');
      for (var j = 0; j < boxes.length; j++) {
        if (!boxes[j].checked) return false;
      }
    }
    return true;
  }
  function prefetchRegisteredSingleResource(id) {
    if ((id !== 'spatial_projection' && id !== 'overview_projection') ||
        !singleGeometryRequestEligible(id)) {
      return;
    }
    var selector = document.getElementById(id + '_to_display');
    var name = selector && selector.value;
    if (!name) name = singleDefaultResourceNames.get(id);
    var message = name && singleResourceDescriptors.get(id + ':' + name);
    if (!message || !canonicalDatasetIdentityMatches(
      message.resource, window.cerebroSavedViewDataset
    )) return;
    fetchValidatedSingleGeometryResource(
      message.resource,
      {dataset_identity: window.cerebroSavedViewDataset},
      message.resource.cells
    ).catch(function () {
      Shiny.setInputValue(
        id + '_projection_resource_failed',
        String(message.resource.url || ''),
        {priority: 'event'}
      );
    });
  }
  function prefetchRegisteredSingleCategory(id) {
    if (id !== 'overview_projection') return;
    var selector = document.getElementById(id + '_point_color');
    var name = selector && selector.value;
    var message = name && singleCategoryResourceDescriptors.get(id + ':' + name);
    if (!message || !canonicalDatasetIdentityMatches(
      message.resource, window.cerebroSavedViewDataset
    )) return;
    fetchValidatedMetadataCodesResource(
      message.resource, Number(message.resource.cells), message.dataset_identity
    ).catch(function () {});
  }
  function fetchProjectionSubsetResource(
    resource, expectedCanonicalCells, expectedCells
  ) {
    var mapping = window.CBViewState.canonicalMapping.subsetContract(
      resource, expectedCanonicalCells, expectedCells
    );
    if (!mapping) {
      return Promise.reject(new Error('Projection subset contract mismatch'));
    }
    var kind = mapping.kind;
    var canonicalCells = mapping.canonicalCells;
    if (kind === 'identity') {
      return Promise.resolve({
        kind: kind, indices: null, bytes: 0, fetchMs: 0,
        downloadMs: 0, decodeMs: 0, cacheHit: true
      });
    }
    var expectedCount = mapping.expectedCount;
    return fetchCachedResource(
      resource, projectionSubsetResourceCache, 'Projection subset', '',
      function (buffer) {
        if (buffer.byteLength !== expectedCount * 4) {
          throw new Error('Projection subset byte count mismatch');
        }
        var indices = new Uint32Array(buffer);
        if (!window.CBViewState.canonicalMapping.indicesMatch(
          mapping, indices
        )) {
          throw new Error('Projection subset index mismatch');
        }
        return {kind: kind, indices: indices, bytes: buffer.byteLength};
      }
    );
  }
  function materializeProjectionSubset(coordinates, subset, expectedCells) {
    var started = performance.now();
    var canonicalCells = coordinates.x.length;
    if (subset.kind === 'identity') {
      if (canonicalCells !== expectedCells) {
        throw new Error('Projection identity materialization mismatch');
      }
      return {
        coordinates: coordinates,
        materializeMs: performance.now() - started
      };
    }
    var x = new Float32Array(expectedCells);
    var y = new Float32Array(expectedCells);
    var z = coordinates.z ? new Float32Array(expectedCells) : null;
    var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
    var out = 0;
    function copy(canonicalIndex) {
      if (out >= expectedCells) {
        throw new Error('Projection subset output overflow');
      }
      var xv = coordinates.x[canonicalIndex];
      var yv = coordinates.y[canonicalIndex];
      x[out] = xv; y[out] = yv;
      if (z) z[out] = coordinates.z[canonicalIndex];
      if (isFinite(xv) && isFinite(yv)) {
        if (xv < x0) x0 = xv;
        if (xv > x1) x1 = xv;
        if (yv < y0) y0 = yv;
        if (yv > y1) y1 = yv;
      }
      out++;
    }
    window.CBViewState.canonicalMapping.forEachIndex(
      {
        kind: subset.kind,
        canonicalCells: canonicalCells,
        cells: expectedCells
      },
      subset.indices,
      copy
    );
    if (out !== expectedCells) {
      throw new Error('Projection subset output length mismatch');
    }
    return {
      coordinates: {
        x: x, y: y, z: z,
        xRange: !z && isFinite(x0) && x1 > x0 ? [x0, x1] : null,
        yRange: !z && isFinite(y0) && y1 > y0 ? [y0, y1] : null,
        bytes: coordinates.bytes
      },
      materializeMs: performance.now() - started
    };
  }
  function canonicalProjectionCoordinates(resource, message) {
    if (!canonicalProjectionIdentityMatches(resource, message)) {
      return Promise.reject(new Error('Canonical projection identity mismatch'));
    }
    var cached = window.CBViewState.sharedProjection(
      sharedBase(), resource.projection_name, Number(resource.cells)
    );
    if (cached) {
      return Promise.resolve({
        coordinates: {
          x: cached.x, y: cached.y, z: cached.z,
          bytes: Number(resource.bytes) || 0
        },
        fetchMs: 0, downloadMs: 0, decodeMs: 0, cacheHit: true
      });
    }
    return fetchProjectionResource(resource, resource.cells).then(function (result) {
      cacheSharedProjection(
        resource.projection_name,
        result.coordinates,
        Number(resource.cells)
      );
      return result;
    });
  }
  function trajectoryFrameResources(resource, message, expectedCells) {
    var identity = message && message.dataset_identity || {};
    var geometryKind = String(resource && resource.geometry_kind || '');
    var projection = resource && resource.projection;
    var subset = resource && resource.subset;
    var state = resource && resource.state;
    // Optional colouring by a metadata group: canonical codes for every cell,
    // mapped to the trajectory rows through the canonical subset below.
    var group = resource && resource.group || null;
    if (group && (geometryKind !== 'canonical_projection' || !group.url ||
        group.protocol !== 'canonical-metadata-codes-v1' ||
        Number(group.cells) !== Number(resource.canonical_cells))) {
      throw new Error('Trajectory group resource mismatch');
    }
    if (!window.CBViewState.resourceDescriptor.validate(resource, {
          protocol: 'trajectory-frame-v1', cells: expectedCells,
          identity: identity
        }) ||
        Number(resource.canonical_cells) !== Number(identity.cell_count) ||
        !projection || !projection.url || !state || !state.url) {
      throw new Error('Trajectory frame resource mismatch');
    }
    if (geometryKind === 'canonical_projection') {
      if (!subset || !canonicalProjectionIdentityMatches(projection, message)) {
        throw new Error('Trajectory canonical frame mismatch');
      }
    } else if (geometryKind === 'trajectory_asset' ||
               geometryKind === 'trajectory') {
      if (subset || !window.CBViewState.resourceDescriptor.validate(
        projection, {
          cells: expectedCells, dtype: 'float32', requireUrl: true
        }
      )) {
        throw new Error('Trajectory geometry frame mismatch');
      }
    } else {
      throw new Error('Trajectory geometry kind mismatch');
    }
    return {projection: projection, subset: subset, state: state, group: group};
  }
  function materializeCanonicalCodes(values, subset, expectedCells) {
    if (subset.kind === 'identity') {
      if (values.length !== expectedCells) {
        throw new Error('Canonical code identity mismatch');
      }
      return values;
    }
    var out = new values.constructor(expectedCells), next = 0;
    window.CBViewState.canonicalMapping.forEachIndex(
      {kind: subset.kind, canonicalCells: values.length, cells: expectedCells},
      subset.indices,
      function (canonicalIndex) {
        if (next >= expectedCells) throw new Error('Canonical code overflow');
        out[next++] = values[canonicalIndex];
      }
    );
    if (next !== expectedCells) throw new Error('Canonical code count mismatch');
    return out;
  }
  function hydrateSingleProjectionResource(message) {
    var data = message && message.data || {};
    var frameResource = data.trajectory_frame_resource;
    var frame = null;
    try {
      frame = frameResource
        ? trajectoryFrameResources(frameResource, message, data.n) : null;
    } catch (error) {
      return Promise.reject(error);
    }
    var resource = frame ? frame.projection : data.projection_resource;
    var subsetResource = frame ? frame.subset : data.projection_subset_resource;
    var canonicalGroup = frame && frame.group;
    var groupResource = frame
      ? (canonicalGroup || frame.state) : data.categorical_resource;
    if ((!resource || !resource.url) &&
        (!groupResource || !groupResource.url)) {
      return Promise.resolve(message);
    }
    var coordinatesPromise = resource && resource.url
      ? (subsetResource
        ? canonicalProjectionCoordinates(resource, message)
        : fetchValidatedSingleGeometryResource(resource, message, data.n))
      : Promise.resolve(null);
    var subsetPromise = subsetResource
      ? fetchProjectionSubsetResource(
        subsetResource, resource && resource.cells, data.n
      ) : Promise.resolve(null);
    var groupsPromise = groupResource && groupResource.url
      ? (groupResource.protocol === 'canonical-metadata-codes-v1'
        ? fetchValidatedMetadataCodesResource(
          groupResource, canonicalGroup ? groupResource.cells : data.n,
          message && message.dataset_identity
        )
        : fetchCategoricalResource(groupResource, data.n))
      : Promise.resolve(null);
    return Promise.all([
      coordinatesPromise, subsetPromise, groupsPromise
    ]).then(function (results) {
      var result = results[0];
      var subsetResult = results[1];
      var groupResult = results[2];
      var coordinates = result && result.coordinates;
      var materialized = null;
      if (coordinates && subsetResult) {
        materialized = materializeProjectionSubset(
          coordinates, subsetResult, Number(data.n)
        );
        coordinates = materialized.coordinates;
      }
      if (coordinates) {
        data.x = coordinates.x;
        data.y = coordinates.y;
        if (coordinates.z) data.z = coordinates.z;
        if (coordinates.xRange) data.x_range = coordinates.xRange;
        if (coordinates.yRange) data.y_range = coordinates.yRange;
      }
      if (groupResult) {
        data.canonical_group = canonicalGroup
          ? materializeCanonicalCodes(
            groupResult.values, subsetResult, Number(data.n)
          )
          : groupResult.values;
      }
      delete data.projection_resource;
      delete data.projection_subset_resource;
      delete data.categorical_resource;
      delete data.trajectory_frame_resource;
      message.data = data;
      var timing = CBViewState.specialistLifecycle.update(
        singleTiming, message.id, {
          projectionFetchMs: result ? result.fetchMs : 0,
          projectionBytes: coordinates ? coordinates.bytes : 0,
          projectionDownloadMs: result ? result.downloadMs : 0,
          projectionDecodeMs: result ? result.decodeMs : 0,
          projectionCacheHit: result ? !!result.cacheHit : false,
          projectionSubsetFetchMs: subsetResult ? subsetResult.fetchMs : 0,
          projectionSubsetBytes: subsetResult ? subsetResult.bytes : 0,
          projectionSubsetDownloadMs: subsetResult ? subsetResult.downloadMs : 0,
          projectionSubsetDecodeMs: subsetResult ? subsetResult.decodeMs : 0,
          projectionSubsetCacheHit: subsetResult ? !!subsetResult.cacheHit : false,
          projectionSubsetMaterializeMs: materialized
            ? materialized.materializeMs : 0,
          categoricalFetchMs: groupResult ? groupResult.fetchMs : 0,
          categoricalBytes: groupResult ? groupResult.bytes : 0
        }
      );
      if (subsetResult) {
        timing.geometryReused = true;
        timing.geometryReuseProtocol = 'canonical-projection-v1';
        timing.geometryReuseSubset = subsetResult.kind;
      }
      return message;
    });
  }
  function linkedBundleResourceIdentity(bundle) {
    return {
      cell_count: bundle && bundle.n,
      cell_fingerprint: bundle && bundle.dataset_fingerprint,
      cell_order_fingerprint: bundle && bundle.canonical_order_id,
      pack_dataset_fingerprint: bundle && bundle.pack_dataset_fingerprint
    };
  }
  function hydrateLinkedProjectionResource(bundle) {
    var name = bundle && bundle.default_projection;
    var projection = name && bundle.projections && bundle.projections[name];
    var resource = projection && projection.projection_resource;
    if (!resource || !resource.url) {
      return Promise.resolve({
        bundle: bundle,
        projectionFetchMs: 0,
        projectionBytes: 0,
        projectionDownloadMs: 0,
        projectionDecodeMs: 0
      });
    }
    return fetchValidatedProjectionResource(
      resource,
      { dataset_identity: linkedBundleResourceIdentity(bundle) },
      bundle.n
    ).then(function (result) {
      var coordinates = result.coordinates;
      projection.x = coordinates.x;
      projection.y = coordinates.y;
      if (coordinates.z) projection.z = coordinates.z;
      if (coordinates.xRange) projection.xRange = coordinates.xRange;
      if (coordinates.yRange) projection.yRange = coordinates.yRange;
      delete projection.projection_resource;
      return {
        bundle: bundle,
        projectionFetchMs: result.fetchMs,
        projectionBytes: coordinates.bytes,
        projectionDownloadMs: result.downloadMs,
        projectionDecodeMs: result.decodeMs
      };
    });
  }
  var metadataCodesResourceCache = new Map();
  var categoricalResourceCache = new Map();
  function clearStaticResourceCaches() {
    projectionResourceCache.clear();
    projectionSubsetResourceCache.clear();
    metadataCodesResourceCache.clear();
    categoricalResourceCache.clear();
  }
  function fetchCategoricalResource(resource, expectedCells) {
    var descriptor = window.CBViewState.resourceDescriptor.validate(resource, {
      cells: expectedCells, dtypes: ['uint8', 'uint16', 'uint32'],
      requireUrl: true
    });
    var n = descriptor && descriptor.cells;
    var dtype = descriptor && descriptor.dtype;
    var bytesPerCode = window.CBViewState.resourceDescriptor.codeWidth(descriptor);
    var levels = resource.levels;
    if (!descriptor || !bytesPerCode || !levels) {
      return Promise.reject(new Error('Categorical resource mismatch'));
    }
    return fetchCachedResource(
      resource, categoricalResourceCache, 'Categorical resource', '',
      function (buffer) {
        if (buffer.byteLength !== n * bytesPerCode ||
            (Number(resource.bytes) && buffer.byteLength !== Number(resource.bytes))) {
          throw new Error('Categorical resource byte count mismatch');
        }
        var values = dtype === 'uint8' ? new Uint8Array(buffer) :
          (dtype === 'uint16' ? new Uint16Array(buffer) : new Uint32Array(buffer));
        for (var i = 0; i < n; i++) {
          if (values[i] >= levels.length) {
            throw new Error('Categorical code is outside its dictionary');
          }
        }
        return { values: values, bytes: buffer.byteLength };
      }
    );
  }
  function fetchMetadataCodesResource(resource, expectedCells) {
    var descriptor = window.CBViewState.resourceDescriptor.validate(resource, {
      cells: expectedCells, dtypes: ['uint8', 'uint16'], requireUrl: true
    });
    var n = descriptor && descriptor.cells;
    var dtype = descriptor && descriptor.dtype;
    var bytesPerCode = window.CBViewState.resourceDescriptor.codeWidth(descriptor);
    var codeMap = resource.code_map;
    if (!descriptor || !bytesPerCode || !codeMap || !codeMap.length) {
      return Promise.reject(new Error('Metadata codes resource mismatch'));
    }
    return fetchCachedResource(
      resource, metadataCodesResourceCache, 'Metadata codes',
      Array.prototype.join.call(codeMap, ','),
      function (buffer) {
        if (buffer.byteLength !== n * bytesPerCode ||
            (Number(resource.bytes) && buffer.byteLength !== Number(resource.bytes))) {
          throw new Error('Metadata codes byte count mismatch');
        }
        var source = dtype === 'uint8'
          ? new Uint8Array(buffer) : new Uint16Array(buffer);
        var maxCode = codeMap.length - 2;
        var values = maxCode <= 127
          ? new Int8Array(n)
          : (maxCode <= 32767 ? new Int16Array(n) : new Int32Array(n));
        for (var i = 0; i < n; i++) {
          var code = source[i];
          if (code >= codeMap.length) {
            throw new Error('Metadata code is outside its dictionary');
          }
          values[i] = Number(codeMap[code]);
        }
        return { values: values, bytes: buffer.byteLength };
      }
    );
  }
  function fetchValidatedMetadataCodesResource(resource, expectedCells, identity) {
    if (resource && resource.protocol === 'canonical-metadata-codes-v1' &&
        !canonicalResourceIdentityMatches(resource, identity)) {
      return Promise.reject(new Error('Canonical metadata identity mismatch'));
    }
    return fetchMetadataCodesResource(resource, expectedCells);
  }
  function hydrateLinkedMetadataResource(bundle) {
    var name = bundle && bundle.default_group;
    var group = name && ((bundle.groups && bundle.groups[name]) ||
      (bundle.cat_extra && bundle.cat_extra[name]));
    var resource = group && group.values_resource;
    if (!resource || !resource.url) {
      return Promise.resolve({ bundle: bundle, metadataFetchMs: 0,
        metadataBytes: 0 });
    }
    return fetchValidatedMetadataCodesResource(
      resource, bundle.n, linkedBundleResourceIdentity(bundle)
    ).then(function (result) {
      group.values = result.values;
      delete group.values_resource;
      return { bundle: bundle, metadataFetchMs: result.fetchMs,
        metadataBytes: result.bytes };
    });
  }
  function hydrateLinkedPrimaryResources(bundle) {
    return Promise.all([
      hydrateLinkedProjectionResource(bundle),
      hydrateLinkedMetadataResource(bundle)
    ]).then(function (results) {
      return Object.assign({}, results[0], results[1]);
    });
  }
  function canonicalGroupedValues(grouped, groups, fallback) {
    return window.CBViewState.canonicalGroupedValues(grouped, groups, fallback);
  }
  function canonicalAuxValues(values, groups, fallback) {
    return window.CBViewState.canonicalAuxValues(values, groups, fallback);
  }
  function alignSingleCoordinates(data, nested) {
    var n = D && D.n || 0;
    var sourceHoverColumns = data.hover && Array.isArray(data.hover.columns)
      ? data.hover.columns : [];
    if (!nested) {
      var directX = data.data.x || [], directY = data.data.y || [];
      var directZ = data.data.z || [];
      var directHover = !(data.hover && data.hover.hoverinfo === 'skip');
      var direct = !data.data.selection_key && ArrayBuffer.isView(directX) &&
        ArrayBuffer.isView(directY) && directX.length === n && directY.length === n;
      if (direct && !directHover && !sourceHoverColumns.length) {
        return {
          x: directX,
          y: directY,
          z: ArrayBuffer.isView(directZ) && directZ.length === n ? directZ : null,
          groups: null,
          levels: [],
          colors: [],
          hover: [],
          hoverEnabled: new Uint8Array(0),
          hoverColumns: []
        };
      }
    }
    var traces = nested && Array.isArray(data.meta.traces) ? data.meta.traces : [];
    var levels = traces.map(String);
    var colors = traces.map(function (name, group) {
      var colour = data.data.color && data.data.color[group];
      return String(Array.isArray(colour) ? colour[0] : colour || '#7b8794');
    });
    if (nested) {
      var directGroups = data.data.canonical_group;
      var directCanonicalX = data.data.x, directCanonicalY = data.data.y;
      var directCanonicalZ = data.data.z;
      if (ArrayBuffer.isView(directGroups) && directGroups.length === n &&
        ArrayBuffer.isView(directCanonicalX) && directCanonicalX.length === n &&
        ArrayBuffer.isView(directCanonicalY) && directCanonicalY.length === n) {
        return {
          x: directCanonicalX,
          y: directCanonicalY,
          z: ArrayBuffer.isView(directCanonicalZ) &&
            directCanonicalZ.length === n ? directCanonicalZ : null,
          groups: directGroups,
          levels: levels,
          colors: colors,
          hover: [],
          hoverEnabled: new Uint8Array(0),
          hoverColumns: []
        };
      }
    }
    var x = new Float64Array(n), y = new Float64Array(n);
    var z = data.data.z ? new Float64Array(n) : null;
    // Missing spatial coordinates belong to cells outside this section.
    // Preserve them as missing: Number(null) would invent a point at zero.
    x.fill(NaN); y.fill(NaN);
    if (z) z.fill(NaN);
    var groups = nested ? new Int32Array(n) : null;
    if (groups) groups.fill(-1);
    var hover = new Array(n);
    var hoverEnabled = new Uint8Array(n);
    var hoverColumns = sourceHoverColumns.map(function (column) {
      return {
        label: String(column.label || ''),
        format: column.format || null,
        digits: column.digits,
        levels: Array.isArray(column.levels) ? column.levels.map(String) : null,
        values: emptyVector(null)
      };
    });
    if (nested) {
      var hoverModes = data.hover && data.hover.hoverinfo;
      var canonicalGroups = data.data.canonical_group;
      if (ArrayBuffer.isView(canonicalGroups) && canonicalGroups.length === n) {
        var canonicalX = canonicalGroupedValues(data.data.x, canonicalGroups, NaN);
        var canonicalY = canonicalGroupedValues(data.data.y, canonicalGroups, NaN);
        var canonicalZ = z
          ? canonicalGroupedValues(data.data.z, canonicalGroups, NaN) : null;
        for (var canonicalIndex = 0; canonicalIndex < n; canonicalIndex++) {
          x[canonicalIndex] = Number(canonicalX[canonicalIndex]);
          y[canonicalIndex] = Number(canonicalY[canonicalIndex]);
          if (z) z[canonicalIndex] = Number(canonicalZ[canonicalIndex]);
          groups[canonicalIndex] = Number(canonicalGroups[canonicalIndex]);
        }
        return {
          x: x, y: y, z: z, groups: groups, levels: levels, colors: colors,
          hover: [], hoverEnabled: hoverEnabled, hoverColumns: []
        };
      }
      var offset = 0;
      traces.forEach(function (name, group) {
        var gx = data.data.x[group] || [], gy = data.data.y[group] || [];
        var gz = data.data.z && data.data.z[group] || [];
        var gh = data.hover && data.hover.text && data.hover.text[group] || [];
        var hoverMode = Array.isArray(hoverModes) ? hoverModes[group] : hoverModes;
        var limit = Math.min(gx.length, n - offset);
        for (var j = 0; j < limit; j++) {
          var at = offset + j;
          x[at] = gx[j] == null ? NaN : Number(gx[j]);
          y[at] = gy[j] == null ? NaN : Number(gy[j]);
          if (z && gz[j] != null) z[at] = Number(gz[j]);
          groups[at] = group; hover[at] = gh[j] || '';
          hoverEnabled[at] = hoverMode !== 'skip';
          sourceHoverColumns.forEach(function (column, columnIndex) {
            var values = column.values && column.values[group];
            hoverColumns[columnIndex].values[at] = values ? values[j] : null;
          });
        }
        offset += limit;
      });
    } else {
      var hx = data.data.x || [], hy = data.data.y || [], hz = data.data.z || [];
      var hh = data.hover && Array.isArray(data.hover.text) ? data.hover.text : [];
      var enabled = !(data.hover && data.hover.hoverinfo === 'skip');
      for (var k = 0; k < Math.min(hx.length, n); k++) {
        x[k] = hx[k] == null ? NaN : Number(hx[k]);
        y[k] = hy[k] == null ? NaN : Number(hy[k]);
        if (z && hz[k] != null) z[k] = Number(hz[k]);
        hover[k] = hh[k] || '';
        hoverEnabled[k] = enabled;
        sourceHoverColumns.forEach(function (column, columnIndex) {
          hoverColumns[columnIndex].values[k] = column.values
            ? column.values[k] : null;
        });
      }
    }
    return { x: x, y: y, z: z, groups: groups,
      levels: levels, colors: colors, hover: hover, hoverEnabled: hoverEnabled,
      hoverColumns: hoverColumns };
  }
  function quantisedField(label, raw, keys, data, panelScale, aligned) {
    var index = aligned ? null : singleIndex(), min = Infinity, max = -Infinity;
    var direct = !keys || !keys.length;
    if (data && data.zero_color && (!raw || !raw.length)) {
      var zeroPalette = singlePalette(panelScale || data.colorscale,
        panelScale ? false : data.reversescale);
      return {
        label: label,
        v: new Uint8Array(0),
        raw: null,
        constantValue: 0,
        min: 0,
        max: 1,
        scale: 1000,
        paintOrder: false,
        constantColor: zeroPalette ? zeroPalette[0] : viridisCss(0),
        unclipped: true,
        palette: zeroPalette
      };
    }
    var values = direct && ArrayBuffer.isView(raw) ? raw : emptyVector(null);
    var complete = direct && values.length === (D && D.n || 0);
    for (var i = 0; i < raw.length; i++) {
      var at = direct || aligned ? i : index.get(String(keys[i]));
      var value = Number(raw[i]);
      if (at == null || !isFinite(value)) { complete = false; continue; }
      values[at] = value; if (value < min) min = value; if (value > max) max = value;
    }
    if (!isFinite(min)) { min = 0; max = 1; }
    var range = data && data.color_range;
    if (Array.isArray(range) && range.length === 2 &&
      isFinite(Number(range[0])) && isFinite(Number(range[1])) &&
      Number(range[0]) !== Number(range[1])) {
      min = Math.min(Number(range[0]), Number(range[1]));
      max = Math.max(Number(range[0]), Number(range[1]));
    }
    if (max <= min) max = min + 1;
    var q = complete ? new Uint16Array(values.length) : new Array(values.length);
    for (i = 0; i < values.length; i++) {
      value = values[i];
      q[i] = value == null || !isFinite(value) ? null : Math.round(
        Math.max(0, Math.min(1, (value - min) / (max - min))) * 1000
      );
    }
    return { label: label, v: q, raw: values, min: min, max: max, scale: 1000,
      paintOrder: !!data && data.paint_order === 'highest',
      unclipped: true,
      palette: singlePalette(panelScale || (data && data.colorscale),
        panelScale ? false : data && data.reversescale) };
  }
  function directColorField(meta, raw, keys) {
    var index = singleIndex(), colors = emptyVector('#e6e7ea');
    var direct = !keys || !keys.length;
    for (var i = 0; i < raw.length; i++) {
      var at = direct ? i : index.get(String(keys[i]));
      if (at != null) colors[at] = cssColor(raw[i], '#e6e7ea');
    }
    return {
      label: meta.color_variable || 'Co-expression', colors: colors,
      channelLabels: Array.isArray(meta.traces) ? meta.traces : [],
      channelColors: Array.isArray(meta.coexpr_colors) ? meta.coexpr_colors : []
    };
  }
  function singleRgbData(data, aligned) {
    var index = aligned ? null : singleIndex(), keys = Array.isArray(data.selection_key)
      ? data.selection_key : [], raw = data.rgb || {}, out = {maxima: {}};
    var direct = !keys.length || aligned;
    ['r', 'g', 'b'].forEach(function (channel) {
      var source = raw[channel] || [], maximum = 0;
      var values = new Float32Array(D && D.n || 0);
      for (var i = 0; i < source.length; i++) {
        var at = direct ? i : index.get(String(keys[i])), value = Number(source[i]);
        if (at == null || !isFinite(value)) continue;
        values[at] = value; if (value > maximum) maximum = value;
      }
      var scaled = new Uint8Array(values.length);
      if (data.rgb_scaled) {
        scaled.set(values);
      } else if (maximum > 0) {
        for (i = 0; i < values.length; i++) {
          scaled[i] = Math.round(values[i] / maximum * 255);
        }
      }
      out[channel] = scaled;
      out.maxima[channel] = maximum;
    });
    var genes = data.rgb_genes || {};
    out.genes = ['r', 'g', 'b'].map(function (channel) {
      return genes[channel] || '';
    });
    return out;
  }
  function singleEdges(extra) {
    var columns = extra && extra.edges;
    if (columns && columns.from && columns.to) {
      return { from: columns.from, to: columns.to };
    }
    if (columns && columns.x0 && columns.y0 && columns.x1 && columns.y1) {
      return columns;
    }
    var shapes = extra && Array.isArray(extra.shapes) ? extra.shapes : [];
    return shapes.filter(function (shape) {
      return shape && shape.x0 != null && shape.y0 != null &&
        shape.x1 != null && shape.y1 != null;
    }).map(function (shape) {
      return [Number(shape.x0), Number(shape.y0), Number(shape.x1), Number(shape.y1)];
    });
  }
  function singleCellBoundaries(extra) {
    var raw = extra && extra.cell_boundaries;
    if (!raw || !Array.isArray(raw.cell_barcode) ||
      !Array.isArray(raw.part) || !Array.isArray(raw.x) ||
      !Array.isArray(raw.y)) return [];
    var n = Math.min(raw.cell_barcode.length, raw.part.length,
      raw.x.length, raw.y.length);
    var boundaries = [], current = null, currentKey = null;
    for (var i = 0; i < n; i++) {
      var x = Number(raw.x[i]), y = Number(raw.y[i]);
      if (!isFinite(x) || !isFinite(y)) continue;
      var cell = String(raw.cell_barcode[i]);
      var key = cell + '\u0000' + String(raw.part[i]);
      if (key !== currentKey) {
        current = { cell: cell, points: [] };
        boundaries.push(current);
        currentKey = key;
      }
      current.points.push([x, y]);
    }
    return boundaries.filter(function (boundary) {
      return boundary.points.length >= 3;
    });
  }
  function singleMolecules(extra) {
    var raw = extra && extra.molecule_points;
    if (!raw || !Array.isArray(raw.x) || !Array.isArray(raw.y)) return [];
    var n = Math.min(raw.x.length, raw.y.length), points = [];
    for (var i = 0; i < n; i++) {
      var x = Number(raw.x[i]), y = Number(raw.y[i]);
      if (isFinite(x) && isFinite(y)) points.push([x, y]);
    }
    return points;
  }
  function singleHulls(extra) {
    var raw = extra && extra.group_hulls;
    if (!raw || !Array.isArray(raw.x) || !Array.isArray(raw.y)) return [];
    return raw.x.map(function (x, index) {
      return {
        x: x || [], y: raw.y[index] || [],
        color: raw.color && raw.color[index] || '#7b8794'
      };
    });
  }
  function alignFlatValues(keys, values, fallback) {
    var index = singleIndex(), out = emptyVector(fallback);
    (keys || []).forEach(function (key, i) {
      var at = index.get(String(key));
      if (at != null) out[at] = values && values[i] != null ? values[i] : fallback;
    });
    return out;
  }
  function buildSpecialistPanels(id, payload) {
    var meta = payload.meta || {}, data = payload.data || {},
      extra = payload.extra || {};
    if (!Array.isArray(data.panels) || !data.panels.length) return null;
    var keys = Array.isArray(data.selection_key) ? data.selection_key : [];
    var cellBoundaries = singleCellBoundaries(extra);
    var spaces = [], modes = {}, mode;
    if (meta.color_type === 'categorical') {
      var levels = Array.isArray(meta.traces) ? meta.traces.map(String) : [];
      var rawGroups = alignFlatValues(keys, data.group || [], null);
      var groups = rawGroups.map(function (value) {
        var at = levels.indexOf(String(value)); return at < 0 ? -1 : at;
      });
      var groupName = '__single_group__' + id;
      var group = {
        values: groups,
        levels: levels,
        colors: Array.isArray(meta.group_colors) ? meta.group_colors : []
      };
      (meta.shared_filters ? D.groups : D.cat_extra)[groupName] = group;
      mode = groupName;
    } else {
      var fieldName = 'single:' + id + ':value';
      D.fields[fieldName] = quantisedField(
        meta.color_variable || 'Value',
        (Array.isArray(data.color) || ArrayBuffer.isView(data.color))
          ? data.color : [],
        keys,
        data
      );
      mode = FIELD_PREFIX + fieldName;
    }
    data.panels.forEach(function (panel, panelIndex) {
      var panelKeys = Array.isArray(panel.selection_key)
        ? panel.selection_key : keys;
      var t = Math.max(0, Math.min(1, Number(panel.transition) || 0));
      var xs = panel.x || [];
      var ys = panel.y || [];
      if (Array.isArray(panel.from_x) && Array.isArray(panel.to_x)) {
        xs = panel.from_x.map(function (value, i) {
          return Number(value) + (Number(panel.to_x[i]) - Number(value)) * t;
        });
        ys = panel.from_y.map(function (value, i) {
          return Number(value) + (Number(panel.to_y[i]) - Number(value)) * t;
        });
      }
      var spaceId = 'single::' + id + '::' + (panel.id || panelIndex);
      var space = {
        id: spaceId,
        _role: panel.id || null,
        label: panel.label || panel.id || id,
        x: alignFlatValues(panelKeys, xs, null),
        y: alignFlatValues(panelKeys, ys, null),
        _hover: alignFlatValues(panelKeys, panel.hover || [], ''),
        _hoverEnabled: true,
        _hoverMask: emptyVector(true)
      };
      if (cellBoundaries.length) {
        var panelCells = new Set(panelKeys.map(String));
        space.cellBoundaries = cellBoundaries.filter(function (boundary) {
          return panelCells.has(boundary.cell);
        });
      }
      if (Array.isArray(panel.from_x) && Array.isArray(panel.to_x)) {
        var fromX = alignFlatValues(panelKeys, panel.from_x, null);
        var fromY = alignFlatValues(panelKeys, panel.from_y, null);
        var toX = alignFlatValues(panelKeys, panel.to_x, null);
        var toY = alignFlatValues(panelKeys, panel.to_y, null);
        space._transition = {
          fromX: fromX, fromY: fromY, toX: toX, toY: toY,
          fromUnit: unitOf({ x: fromX, y: fromY }),
          toUnit: unitOf({ x: toX, y: toY })
        };
        space._unit = transitionUnit(
          space._transition.fromUnit, space._transition.toUnit, t
        );
      }
      if (panel.projection) space._projectionName = panel.id || 'projection';
      if (panel.spatial) {
        space.background_scope = panel.label || 'Trekker';
        space._sampleName = panel.label || 'Trekker';
        space._preserveAspect = !!panel.preserve_aspect;
      }
      space.builder_point_size = panel.builder_point_size;
      space.builder_point_opacity = panel.builder_point_opacity;
      space.xRange = panel.x_range;
      space.yRange = panel.y_range;
      var panelImage = panel.background_image || meta.background_image;
      space._backgroundIdentity = canonicalIdentity(panel.image_identity || meta.background_identity);
      var panelBounds = panel.image_bounds || meta.image_bounds;
      if (panelImage && panelBounds) {
        var opacity = Number(meta.background_opacity);
        space.images = [{
          id: panel.image_id || (panel.image_identity
            ? JSON.stringify(panel.image_identity)
            : meta.background_identity
            ? JSON.stringify(meta.background_identity) : 'trekker-background'),
          label: panel.image_label || meta.image_label || 'Tissue background',
          uri: panelImage,
          bounds: panelBounds,
          preset: panel.image_preset || {
            opacity: isFinite(opacity) ? opacity : 1,
            offsetX: Number(meta.background_offset_x) || 0,
            offsetY: Number(meta.background_offset_y) || 0,
            scaleX: Number(meta.background_scale_x) || 1,
            scaleY: Number(meta.background_scale_y) || 1,
            flipX: !!meta.background_flip_x,
            flipY: !!meta.background_flip_y,
            rotation: Number(meta.background_rotation) || 0
          }
        }];
        space._customImageId = space.images[0].id;
      }
      spaceById[spaceId] = space;
      spaces.push(spaceId);
      modes[spaceId] = mode;
    });
    return { spaces: spaces, modes: modes };
  }
  function buildSingleSpaces(id, payload) {
    var meta = payload.meta || {}, data = payload.data || {}, extra = payload.extra || {};
    hydrateSparseSingleColor(data);
    hydrateColorPacketMap(
      data, 'packed_colors', 'packed_color_cache_keys', 'color');
    hydrateColorPacketMap(
      data, 'packed_rgb', 'packed_rgb_cache_keys', 'rgb');
    var specialistPanels = buildSpecialistPanels(id, payload);
    if (specialistPanels) return specialistPanels;
    var categorical = meta.color_type === 'categorical';
    var aligned = alignSingleCoordinates({ meta: meta, data: data, hover: payload.hover }, categorical);
    var baseId = 'single::' + id, spaces = [], modes = {};
    var edges = singleEdges(extra);
    var hulls = singleHulls(extra);
    var pointSizes = (Array.isArray(data.point_sizes) ||
      ArrayBuffer.isView(data.point_sizes))
      ? alignFlatValues(
        Array.isArray(data.selection_key) ? data.selection_key : [],
        data.point_sizes,
        null
      ) : null;
    var cellBoundaries = singleCellBoundaries(extra);
    var molecules = singleMolecules(extra);
    var hasZ = aligned.z !== null;
    var hasHover = aligned.hoverEnabled.some(Boolean);
    var makeSpace = function (spaceId, label) {
      var space = { id: spaceId, label: label || id, x: aligned.x, y: aligned.y };
      space._projectionName = spaceId;
      space._hover = aligned.hover;
      space._hoverColumns = aligned.hoverColumns;
      space._hoverEnabled = hasHover;
      space._hoverMask = aligned.hoverEnabled;
      if (hasZ) space.z = aligned.z;
      if (!hasZ) {
        space.xRange = data.x_range;
        space.yRange = data.y_range;
      }
      var borderWidth = Number(data.point_line && data.point_line.width);
      if (borderWidth > 0) {
        space.pointBorder = {
          color: cssColor(data.point_line.color, 'rgba(90,90,90,.35)'),
          width: Math.max(0.5, borderWidth * 0.7)
        };
      }
      if (Array.isArray(meta.axes)) space.axes = meta.axes.slice(0, 3);
      if ((Array.isArray(edges) && edges.length) ||
          (!Array.isArray(edges) && (
            (edges.x0 && edges.x0.length) || (edges.from && edges.from.length)
          ))) {
        space.trajectory = true; space.edges = edges;
      }
      if (pointSizes) space.pointSizes = pointSizes;
      if (hulls.length) space.hulls = hulls;
      if (cellBoundaries.length) space.cellBoundaries = cellBoundaries;
      if (molecules.length) space.molecules = molecules;
      if (meta.is_spatial) {
        space.background_scope = id;
        space._sampleName = meta.image_label || 'Spatial';
        var bounds = meta.image_bounds || null;
        if (meta.background_image && bounds) {
          var imageId = meta.background_identity
            ? JSON.stringify(meta.background_identity) : 'single-background';
          var opacity = Number(meta.background_opacity);
          space.images = [{
            id: imageId, label: meta.image_label || 'Tissue background',
            uri: meta.background_image, bounds: bounds,
            preset: {
              opacity: isFinite(opacity) ? opacity : 1,
              offsetX: Number(meta.background_offset_x) || 0,
              offsetY: Number(meta.background_offset_y) || 0,
              scaleX: Number(meta.background_scale_x) || 1,
              scaleY: Number(meta.background_scale_y) || 1,
              flipX: !!meta.background_flip_x,
              flipY: !!meta.background_flip_y,
              rotation: Number(meta.background_rotation) || 0
            }
          }];
          space._customImageId = imageId;
        }
      }
      spaceById[spaceId] = space; spaces.push(spaceId); return space;
    };
    if (categorical) {
      var groupName = '__single_group__' + id;
      D.cat_extra[groupName] = {
        values: aligned.groups,
        levels: aligned.levels,
        colors: aligned.colors
      };
      makeSpace(baseId, meta.space_label || id);
      modes[baseId] = groupName;
    } else if (meta.color_type === 'rgb') {
      D.rgb = singleRgbData(data);
      if (meta.rgb_layout === 'separate' && D.rgb.genes.some(Boolean)) {
        ['r', 'g', 'b'].forEach(function (channel, index) {
          var gene = D.rgb.genes[index];
          if (!gene) return;
          var label = channel.toUpperCase() + ' · ' + gene;
          var maximum = D.rgb.maxima[channel];
          var field = quantisedField(label, data.rgb[channel] || [], data.selection_key || [],
            {color_range: [0, maximum], paint_order: data.paint_order});
          // Use the same 8-bit intensities and low-expression threshold as
          // the composite, with only this channel enabled. Keep raw values
          // for hover and selection summaries.
          field.v = D.rgb[channel]; field.scale = 255;
          field.min = 0; field.max = maximum; field.rgbChannel = channel;
          field.palette = Array.from({length: 256}, function (_, value) {
            var color = rgbGpuColor(channel === 'r' ? value : 0,
              channel === 'g' ? value : 0, channel === 'b' ? value : 0);
            return 'rgb(' + (color & 255) + ',' + ((color >> 8) & 255) + ',' + ((color >> 16) & 255) + ')';
          });
          var fieldName = 'single:' + id + ':rgb:' + channel;
          var spaceId = baseId + ':rgb:' + channel;
          D.fields[fieldName] = field;
          makeSpace(spaceId, label); modes[spaceId] = FIELD_PREFIX + fieldName;
        });
      } else {
        makeSpace(baseId, meta.space_label || id);
        modes[baseId] = RGB_MODE;
      }
    } else if (meta.color_type === 'coexpression') {
      var directName = 'single:' + id + ':rgb';
      D.fields[directName] = directColorField(
        meta, Array.isArray(data.color) ? data.color : [],
        Array.isArray(data.selection_key) ? data.selection_key : []
      );
      makeSpace(baseId, meta.space_label || id);
      modes[baseId] = FIELD_PREFIX + directName;
    } else {
      var keys = Array.isArray(data.selection_key) ? data.selection_key : [];
      var multiple = data.color && !Array.isArray(data.color) &&
        !ArrayBuffer.isView(data.color) && typeof data.color === 'object';
      var entries = multiple ? Object.keys(data.color).map(function (name) {
        return [name, data.color[name]];
      }) : [[meta.color_variable || 'Value', data.color || []]];
      entries.forEach(function (entry, panel) {
        var fieldName = 'single:' + id + ':' + panel;
        var panelScale = data.panel_colorscales && data.panel_colorscales[entry[0]];
        D.fields[fieldName] = quantisedField(
          entry[0], entry[1] || [], keys, data, panelScale);
        var spaceId = entries.length === 1 ? baseId : baseId + ':' + panel;
        makeSpace(spaceId, entry[0]); modes[spaceId] = FIELD_PREFIX + fieldName;
      });
    }
    return { spaces: spaces, modes: modes };
  }
  function stashSingleState() {
    var view = singleActive && singleView(singleActive);
    if (!view) return;
    view.selection = selectedCellIds();
    var group = catOf(singleSpaceModes[singleSpaceIds[0]]);
    view.hiddenGroups = group ? Array.from(hidden).map(function (index) {
      return group.levels[index];
    }).filter(function (name) { return name != null; }) : [];
    view.mode = selectMode;
    view.lenses = panels.filter(function (p) { return p.spaceId; }).map(function (p) {
      return { spaceId: p.spaceId,
        view: p.view && Object.assign({}, p.view), rot: p.rot && Object.assign({}, p.rot),
        lassoData: p.lassoData && p.lassoData.map(function (q) { return q.slice(); }) };
    });
  }
  function singleGpuReady(panel) {
    var renderer = panel && panel.gpu;
    if (!renderer || !renderer.ready) return Promise.resolve();
    return Promise.resolve(renderer.ready).catch(function () {}).then(function () {
      if (panel.gpu && panel.gpu !== renderer) return singleGpuReady(panel);
    });
  }
  function reportSinglePainted(id, payload, paintedData) {
    var paintedMeta = payload.meta;
    var paintedKey = payload.data && payload.data.render_key;
    var paintedToken = payload.render_token;
    var activePanels = panels.filter(function (panel) { return panel.spaceId; });
    var paintedCount = activePanels[0] && activePanels[0].canvas && Number(activePanels[0].canvas.dataset.pointCount);
    Promise.all(activePanels.map(singleGpuReady)).then(function () {
      if (singleActive !== id || D !== paintedData || payload.meta !== paintedMeta ||
          visibleSingleId() !== id) {
        return false;
      }
      drawAll();
      return Promise.all(activePanels.map(function (panel) {
        return panel.gpu && typeof panel.gpu.idle === 'function'
          ? panel.gpu.idle().catch(function () {})
          : Promise.resolve();
      }));
    }).then(function (settled) {
      if (settled === false) return;
      requestAnimationFrame(function () {
        requestAnimationFrame(function () {
          if (singleActive !== id || D !== paintedData || payload.meta !== paintedMeta ||
              visibleSingleId() !== id) return;
          reportSelection('primary');
          window.dispatchEvent(new CustomEvent('cerebro:cell-view-ready', {
            detail: {
              id: id,
              painted: true,
              renderKey: paintedKey,
              pointCount: paintedCount,
              dataset_context: payload.dataset_context,
              renderToken: paintedToken
            }
          }));
        });
      });
    });
  }
  function sameSingleLayout(previous, meta, data) {
    if (!previous || !previous.data || !previous.meta || data.reset_axes) return false;
    if (previous.meta.space_label !== meta.space_label || !!previous.data.z !== !!data.z) return false;
    if (previous.meta.rgb_layout !== meta.rgb_layout) return false;
    if (meta.rgb_layout === 'separate' &&
        JSON.stringify(previous.data.rgb_genes) !== JSON.stringify(data.rgb_genes)) return false;
    // Flat continuous payloads also describe multiple panels through a named
    // colour map. Reusing a single mean-expression panel for that map leaves
    // its space ID pointing at geometry that no longer exists.
    function colourPanels(value) {
      // Incoming packets have not been hydrated by buildSingleSpaces yet.
      // Their gene names already identify the layout, without decoding values.
      var color = value.packed_colors || value.color;
      return color && typeof color === 'object' && !Array.isArray(color) &&
        !ArrayBuffer.isView(color) ? Object.keys(color) : [];
    }
    if (JSON.stringify(colourPanels(previous.data)) !==
        JSON.stringify(colourPanels(data))) return false;
    function layout(value) {
      return (value.panels || []).map(function (panel) {
        return [panel.id, panel.label, !!panel.spatial, !!panel.projection, !!panel.z];
      });
    }
    return JSON.stringify(layout(previous.data)) === JSON.stringify(layout(data));
  }

  function sameSingleImages(left, right) {
    left = left || []; right = right || [];
    return left.length === right.length && left.every(function (image, index) {
      var other = right[index];
      return image.id === other.id && image.uri === other.uri && image.asset_key === other.asset_key &&
        JSON.stringify(image.bounds) === JSON.stringify(other.bounds) &&
        JSON.stringify(image.preset) === JSON.stringify(other.preset);
    });
  }

  function activateSingle(id, resetAxes, preserveTargetState, eventKind, reuseSurface, refreshFilters) {
    var activationStarted = performance.now();
    var timing = CBViewState.specialistLifecycle.activate(
      singleTiming, id, activationStarted
    );
    var payload = singleView(id);
    if (!payload || rebuildingBase ||
        !sameDatasetContext(payload.dataset_context, currentDatasetContext())) return false;
    if (!singleActive && linkedBundle && D && !linkedState) {
      linkedState = exportRuntimeWorkspace();
    }
    if (CBViewState.shouldStashSingleState(
      singleActive,
      id,
      preserveTargetState
    )) {
      stashSingleState();
    }
    if (resetAxes) payload.lenses = [];
    var previousSpaces = reuseSurface ? spaceById : null;
    var previousFocus = reuseSurface ? focusPanel : null;
    if (!reuseSurface) restoreLinkedSurface();
    singleActive = id; singleSpaceIds = []; singleSpaceModes = {};
    if (!reuseSurface) setSelectionZoomed(false);
    if (!reuseSurface && !mountSingleSurface(id)) { singleActive = null; return false; }
    var base = singlePayloadBundle(id, payload);
    D = Object.assign({}, base, {
      fields: Object.assign({}, base.fields),
      cat_extra: Object.assign({}, base.cat_extra)
    });
    spaceById = {}; spatialTemplate = null; _clipD = null;
    var buildStarted = performance.now();
    var built = buildSingleSpaces(id, payload);
    timing.buildSpacesMs = performance.now() - buildStarted;
    if (built.spaces.length && payload.meta && payload.meta.space_label) {
      cacheSharedProjection(
        String(payload.meta.space_label),
        spaceById[built.spaces[0]],
        D.n
      );
    }
    payload.data.reset_axes = false;
    singleSpaceIds = built.spaces; singleSpaceModes = built.modes;
    sanitiseColors(D);
    singleSpaceIds.forEach(function (spaceId) {
      var space = spaceById[spaceId], old = previousSpaces && previousSpaces[spaceId];
      if (!isSpatialSpace(space)) return;
      // Cell sampling/recolouring does not invalidate a decoded background.
      if (old && sameSingleImages(old.images, space.images)) {
        ['_imgEl', '_imgReady', '_imgState', '_imgToken'].forEach(function (key) {
          space[key] = old[key];
        });
        // A pending decode callback belongs to the old space; reload only then.
        if (space._imgReady || !currentImage(space)) return;
      }
      loadSpaceImage(space);
    });
    if (payload.backgroundState) updateSingleBackground({
      id: id,
      dataset_context: payload.dataset_context,
      background_identity: payload.background_identity_value,
      values: payload.backgroundState
    });
    Object.keys(payload.backgroundEdits || {}).forEach(function (key) {
      var edit = payload.backgroundEdits[key];
      updateSingleBackground({ id: id, dataset_context: payload.dataset_context,
        background_identity: edit.identity, values: edit.values });
    });
    colorBy = singleSpaceModes[singleSpaceIds[0]] || colorBy;
    hidden = new Set(); groupFilter = {}; sel = null; selectionSource = null;
    selectionSourceSpace = null;
    pick = null; hoverCell = null; focusPanel = previousFocus;
    var appearance = payload.meta.appearance || payload.appearanceOverrides
      ? Object.assign({}, payload.meta.appearance || {}, payload.appearanceOverrides || {}) : null;
    if (appearance) {
      labelsOn = appearance.group_labels === true;
      bordersOn = !!appearance.draw_border;
      keepPlotsSquare = !!appearance.keep_square;
    } else {
      var labelsControl = $('cv-labels');
      var bordersControl = $('cv-borders');
      var squareControl = $('cv-square-plots');
      if (labelsControl) labelsOn = !!labelsControl.checked;
      if (bordersControl) bordersOn = !!bordersControl.checked;
      if (squareControl) keepPlotsSquare = !!squareControl.checked;
    }
    var shared = payload.data && payload.data.client_percentage
      ? sharedScatterValues() : null;
    var incomingPercentage = Number(shared
      ? shared.percentage_cells_to_show
      : payload.data && payload.data.percentage_cells_to_show);
    pctShow = isFinite(incomingPercentage)
      ? Math.max(10, Math.min(100, incomingPercentage)) : 100;
    pctMask = null; rebuildPctMask();
    dissolvePct = 0; dissolveThresh = null; evidenceOn = false; nicheSet = null;
    unpinTip(); closeCard(); cardMeta = null;
    var group = catOf(singleSpaceModes[singleSpaceIds[0]]);
    if (group && Array.isArray(payload.hiddenGroups)) {
      payload.hiddenGroups.forEach(function (name) {
        var index = group.levels.indexOf(String(name)); if (index >= 0) hidden.add(index);
      });
    }
    var incomingPointSize = Number(payload.data && payload.data.point_size);
    if (isFinite(incomingPointSize) && incomingPointSize >= 1) {
      ps = Math.min(20, incomingPointSize);
    }
    var incomingOpacity = Number(payload.data && payload.data.point_opacity);
    if (isFinite(incomingOpacity)) pointOpacity = Math.max(0, Math.min(1, incomingOpacity));
    var panelPointSize = Array.isArray(payload.data && payload.data.panels) &&
      payload.data.panels.some(function (panel) {
        return panel && panel.builder_point_size != null;
      });
    var panelPointOpacity = Array.isArray(payload.data && payload.data.panels) &&
      payload.data.panels.some(function (panel) {
        return panel && panel.builder_point_opacity != null;
      });
    psSeeded = true;
    pointSizeEdited = !panelPointSize;
    pointOpacityEdited = !panelPointOpacity;
    if (payload.pointAppearance) {
      if (Number.isFinite(payload.pointAppearance.point_size)) {
        ps = payload.pointAppearance.point_size; pointSizeEdited = true;
      }
      if (Number.isFinite(payload.pointAppearance.point_opacity)) {
        pointOpacity = payload.pointAppearance.point_opacity; pointOpacityEdited = true;
      }
    }
    syncSharedScatter();
    if (reuseSurface) {
      panels.forEach(function (panel) {
        if (!panel.spaceId) return;
        panel.colorBy = singleSpaceModes[panel.spaceId] || null;
        panel.miniBg = null; panel.miniUnit = null;
        updateCanvasDescription(panel);
        project(panel);
      });
    } else {
      ensurePanelSlots(singleSpaceIds.length); buildPanels(); layoutPanels();
    }
    updateSingleInteractionMode();
    if (!reuseSurface || refreshFilters) {
      renderGroupFilters();
      updateSpaceScopedControls();
    }
    setTrekkerSettingsVisible(!!D.trekker);
    var trekkerInsights = $('cv-tk-insights');
    if (trekkerInsights) trekkerInsights.style.display = D.trekker ? '' : 'none';
    if (D.trekker) fillTrekkerInsights();
    selectMode = payload.mode || 'lasso'; syncModeButtons(); syncCursors();
    var saved = payload.lenses || [];
    panels.forEach(function (p, index) {
      if (!p.spaceId) return;
      var lens = window.CBViewState.lensForSpace(saved, p.spaceId, index);
      if (!lens) return;
      p.view = lens.view || null; p.rot = lens.rot || null;
      p.lassoData = lens.lassoData || null; project(p);
    });
    if (Array.isArray(payload.selection) && payload.selection.length) {
      var cells = singleIndex(), restored = new Set();
      payload.selection.forEach(function (cell) {
        var at = cells.get(String(cell)); if (at != null) restored.add(at);
      });
      sel = restored.size ? restored : null;
    }
    var drawStarted = performance.now();
    timing.preDrawMs = drawStarted - activationStarted;
    renderLegend();
    var drawn = resizeAll();
    renderSelbar();
    if (!drawn) drawAll();
    timing.firstDrawMs = performance.now() - drawStarted;
    timing.activationMs = performance.now() - activationStarted;
    reportSingleHiddenGroups(); reportSelection(eventKind || 'interaction');
    // Gene's selection core is requested by click or brush. Sending all cell
    // IDs here competes with the user's first gene operation on the R thread.
    if (id !== 'expression_projection' && id !== 'trajectory_projection') requestSingleAux();
    // Canvas drawing is synchronous, but the browser does not composite it
    // until a later frame. Keep the page loader in place until that first
    // painted frame is actually visible; otherwise users see a blank gap
    // between the loader and the specialist view.
    var paintedSingleData = D;
    var paintedSingleKey = payload.data && payload.data.render_key;
    var paintedSingleToken = payload.render_token;
    var paintedPanel = panels.filter(function (panel) { return panel.spaceId; })[0];
    var paintedSingleCount = paintedPanel && paintedPanel.canvas && Number(paintedPanel.canvas.dataset.pointCount);
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        if (singleActive !== id || D !== paintedSingleData ||
            visibleSingleId() !== id) return;
        window.dispatchEvent(new CustomEvent('cerebro:cell-view-ready', {
          detail: { id: id, painted: true,
            renderKey: paintedSingleKey,
            renderToken: paintedSingleToken,
            pointCount: paintedSingleCount,
            dataset_context: payload.dataset_context }
        }));
      });
    });
    return true;
  }
  function activateLinked() {
    if (!linkedBundle) return;
    var state = linkedState;
    stashSingleState(); restoreLinkedSurface();
    singleActive = null; singleSpaceIds = []; singleSpaceModes = {};
    rebuildingBase = true;
    try {
      onData(linkedBundle);
    } finally {
      rebuildingBase = false;
    }
    if (state) restoreWorkspace(state);
    syncSharedScatter();
    syncPointControls();
    drawAll();
    linkedState = null;
    var pending = deferredLinkedSupplements;
    deferredLinkedSupplements = [];
    pending.forEach(function (item) {
      if (item.buffer) onBinarySupplement(item.buffer);
      else applyHydratedSupplement(item.extra, item.metric);
    });
  }

  function registerSingle(id, context) {
    context = context || currentDatasetContext();
    var cacheKey = singleCacheKey(id, context);
    if (!cacheKey || !sameDatasetContext(context, currentDatasetContext())) return null;
    if (!singleViews[cacheKey]) {
      singleViews[cacheKey] = { id: id, dataset_context: context };
    }
    return singleViews[cacheKey];
  }

  function renderSingle(message) {
    if (!message || !message.id || !datasetMessageFresh(message)) return false;
    var api = datasetContextApi();
    var context = api.fromMessage(message);
    var id = message.id;
    var cacheKey = singleCacheKey(id, context);
    var renderToken = Number(message.render_token);
    if (!Number.isSafeInteger(renderToken) || renderToken < 1) {
      return false;
    }
    var previous = registerSingle(id, context); if (!previous) return false;
    if (previous.render_token != null && renderToken <= previous.render_token) {
      return false;
    }
    singleRequests.delete(singleRequestKey(id, context));
    var meta = message.meta || {};
    var data = message.data || {};
    var hover = message.hover || {};
    var extra = message.extra || {};
    meta = meta || {}; data = data || {};
    var host = singleHost(id), keys = data.selection_key || [];
    var pointCount = data.n != null ? Number(data.n) : (Array.isArray(keys[0])
      ? keys.reduce(function (total, values) { return total + values.length; }, 0)
      : keys.length);
    var backgroundIdentityValue = message.background_identity != null
      ? message.background_identity : meta.background_identity;
    var backgroundIdentity = canonicalIdentity(backgroundIdentityValue);
    if (previous.background_identity !== backgroundIdentity) {
      previous.backgroundState = null;
    }
    if (host) {
      host.dataset.datasetEpoch = String(context.epoch);
      host.dataset.datasetKey = String(context.dataset_key);
      host.dataset.renderGeneration = String(context.generation);
      host.dataset.renderState = 'received';
      host.dataset.pointCount = String(isFinite(pointCount) ? pointCount : 0);
      if (backgroundIdentity) host.dataset.backgroundIdentity = backgroundIdentity;
      else delete host.dataset.backgroundIdentity;
    }
    var changedGroup = previous.meta && previous.meta.color_variable !== meta.color_variable;
    var reuseSurface = singleActive === id && visibleSingleId() === id &&
      ['overview_projection', 'expression_projection', 'spatial_projection'].indexOf(id) >= 0 &&
      previous.meta.color_type === meta.color_type &&
      sameSingleLayout(previous, meta, data);
    if (!sameSingleLayout(previous, meta, data)) previous.pointAppearance = null;
    singleViews[cacheKey] = Object.assign(previous, {
      id: id, meta: meta || {}, data: data || {}, hover: hover || {},
      extra: extra || {}, dataset_context: context,
      render_token: renderToken, aux_render_token: renderToken,
      datasetIdentity: message.dataset_identity || {}, transportProfile: message.transport_profile || {},
      background_identity: backgroundIdentity,
      background_identity_value: backgroundIdentityValue
    });
    if (changedGroup) singleViews[cacheKey].hiddenGroups = [];
    if (data.reset_axes) singleViews[cacheKey].lenses = [];
    var pending = singleViews[cacheKey].pendingSavedState;
    if (pending) {
      singleViews[cacheKey].pendingSavedState = null;
      applySingleState(id, pending);
    } else if (visibleSingleId() === id) {
      activateSingle(id, !!data.reset_axes, false, 'primary', reuseSurface, changedGroup);
    }
    return true;
  }
  function updateSingleExpression(id, previousMeta, previousData, message) {
    var view = singleView(id), data = view.data, meta = view.meta;
    var pendingIds = D && Array.isArray(D.cells) && D.cells.length === 0 &&
      data.selection_key == null && previousData.selection_key == null &&
      Number.isSafeInteger(data.wire_token) &&
      data.wire_token === previousData.wire_token && Number(data.n) === D.n;
    // The R recolor protocol guarantees unchanged geometry. Keep the shortcut
    // narrow: an active, flat Gene canvas with one panel. An unchanged
    // progressive frame also has canonical positions before its IDs arrive.
    // Other views, layouts and geometry changes retain the full activation path.
    if (id !== 'expression_projection' || singleActive !== id || !D ||
        rebuildingBase || singleSpaceIds.length !== 1 ||
        singleSpaceIds[0] !== 'single::' + id || data.z || data.panels ||
        (!pendingIds && (!Array.isArray(D.cells) || D.cells.length !== D.n ||
          !Array.isArray(data.selection_key) || data.selection_key.length !== D.n)) ||
        data.x !== previousData.x || data.y !== previousData.y ||
        data.selection_key !== previousData.selection_key ||
        !['continuous', 'rgb'].includes(previousMeta.color_type) ||
        !['continuous', 'rgb'].includes(meta.color_type)) return false;
    var allowed = ['color', 'rgb', 'rgb_scaled', 'rgb_genes', 'colorscale',
      'panel_colorscales', 'color_range', 'reversescale', 'paint_order',
      'render_key', 'sparse_color', 'color_cache_key', 'packed_rgb', 'packed_rgb_cache_keys', 'zero_color'];
    if (Object.keys(message.data || {}).some(function (key) {
      return allowed.indexOf(key) < 0;
    }) || Object.keys(message.meta || {}).some(function (key) {
      return ['color_type', 'color_variable', 'render_token'].indexOf(key) < 0 &&
        JSON.stringify(meta[key]) !== JSON.stringify(previousMeta[key]);
    })) return false;
    var spaceId = singleSpaceIds[0], space = spaceById[spaceId];
    var activePanels = panels.filter(function (p) { return p.spaceId; });
    if (!space || space.z || activePanels.length !== 1 ||
        activePanels[0].spaceId !== spaceId) return false;
    var rgb = meta.color_type === 'rgb';
    if (rgb) {
      if (!data.rgb || ['r', 'g', 'b'].some(function (channel) {
        return !data.rgb[channel] || data.rgb[channel].length !== D.n;
      })) return false;
    } else if (!(Array.isArray(data.color) || ArrayBuffer.isView(data.color)) ||
        data.color.length !== D.n) return false;
    // Legacy payloads may contain duplicate stringified IDs. Their Map-based
    // alignment must keep its existing semantics rather than assuming an order.
    if (!pendingIds && singleIndex().size !== D.n) return false;
    var fieldName = 'single:' + id + ':0', mode;
    if (rgb) {
      // Flat single-view cells and coordinates were built in selection_key
      // order. An unchanged geometry payload needs no per-cell Map lookup.
      D.rgb = singleRgbData(data, true);
      delete D.fields[fieldName];
      mode = RGB_MODE;
    } else {
      var label = meta.color_variable || 'Value';
      D.fields[fieldName] = quantisedField(label, data.color,
        data.selection_key, data, data.panel_colorscales && data.panel_colorscales[label], true);
      delete D.rgb;
      mode = FIELD_PREFIX + fieldName;
    }
    space.label = rgb ? (meta.space_label || id) : (meta.color_variable || 'Value');
    colorBy = mode; singleSpaceModes[spaceId] = mode;
    // These caches used to be invalidated incidentally by replacing D.
    // A new expression can change paint order even when its field name is equal.
    _ordD = null; _clipD = null;
    _fldCacheD = null; _fldCacheVal = null;
    activePanels.forEach(function (p) {
      p.colorBy = mode; p.miniBg = null;
      var title = $('cv-title-' + p.key.toLowerCase());
      if (title) title.textContent = space.label;

    });
    pick = null; unpinTip(); closeCard(); cardMeta = null;
    renderLegend(); renderSelbar();
    // A mode/label change can alter header and colourbar height. Refit the
    // existing panels without rebuilding their geometry or restoring state.
    _layoutKey = null; resizeAll(); drawAll();
    // A second RGB packet can land while the pointer is stationary. Re-pick
    // against the retained geometry and refresh values after the colour change.
    activePanels.forEach(refreshPanelHover);
    reportSinglePainted(id, view, D);

    return true;
  }

  function recolorSingle(message) {
    if (!message || !message.id || !datasetMessageFresh(message)) return false;
    var view = singleView(message.id, message.dataset_context);
    if (!view || !view.data || view.data.x == null || view.data.y == null) {
      return false;
    }
    if (!sameDatasetContext(view.dataset_context, message.dataset_context) ||
        Number(message.render_token) <= Number(view.render_token)) return false;
    view.render_token = Number(message.render_token);
    var colorFields = [
      'color', 'rgb', 'rgb_scaled', 'rgb_genes', 'colorscale', 'panel_colorscales',
      'color_range', 'reversescale', 'paint_order', 'render_key', 'sparse_color',
      'color_cache_key', 'packed_rgb', 'packed_rgb_cache_keys', 'zero_color'
    ];
    var previousMeta = view.meta || {}, previousData = view.data;
    var data = Object.assign({}, view.data);
    colorFields.forEach(function (name) { delete data[name]; });
    view.meta = Object.assign({}, view.meta || {}, message.meta || {});
    view.data = Object.assign(data, message.data || {});
    hydrateSparseSingleColor(view.data);
    hydrateColorPacketMap(view.data, 'packed_rgb', 'packed_rgb_cache_keys', 'rgb');
    view.hiddenGroups = [];
    if (visibleSingleId() === message.id &&
        !updateSingleExpression(message.id, previousMeta, previousData, message)) {
      activateSingle(message.id, false, false, 'primary');
    }
    return true;
  }

  function clearSingleSelection(id) {
    var view = registerSingle(id); if (!view) return;
    view.selection = []; view.lenses = (view.lenses || []).map(function (lens) {
      return Object.assign({}, lens, { lassoData: null });
    });
    if (singleActive === id) {
      if (selectionZoomed) resetZoom();
      pick = null; unpinTip(); closeCard(); clearLassos(); setSelection(null);
    }
  }

  function captureSingleState(id) {
    var view = singleView(id);
    if (!view || !view.data) return null;
    if (singleActive === id) stashSingleState();
    var lens = view.lenses && view.lenses[0] || {};
    var rotation = lens.rot || {};
    return {
      cells: (view.selection || []).slice(),
      geometry: lens.lassoData && lens.lassoData.length > 2 ? {
        mode: view.mode === 'box' ? 'box' : 'lasso', panel: 0,
        polygon: lens.lassoData.map(function (point) { return point.slice(); })
      } : null,
      view: {
        viewport: lens.view ? Object.assign({}, lens.view) : null,
        rotation: { x: Number(rotation.rx) || 0, y: Number(rotation.ry) || 0 },
        mode: view.mode || 'lasso', zoomed: !!lens.view,
        hidden_groups: (view.hiddenGroups || []).slice()
      }
    };
  }

  function numericExtent(value) {
    var min = Infinity, max = -Infinity, count = 0;
    function visit(entry) {
      if (Array.isArray(entry) || ArrayBuffer.isView(entry)) {
        Array.prototype.forEach.call(entry, visit);
        return;
      }
      var number = Number(entry);
      if (!isFinite(number)) return;
      min = Math.min(min, number); max = Math.max(max, number); count++;
    }
    visit(value);
    return count ? Object.freeze({ min: min, max: max, count: count }) : null;
  }

  function singleSummary(id) {
    var context = currentDatasetContext();
    var view = singleView(id, context);
    var ready = datasetContextApi().phase() === 'ready' && !!(view && view.data &&
      sameDatasetContext(view.dataset_context, context));
    var keys = ready && view.data.selection_key || [];
    var pointCount = ready && view.data.n != null
      ? Number(view.data.n)
      : (ready && Array.isArray(keys[0])
        ? keys.reduce(function (total, values) { return total + values.length; }, 0)
        : (ready ? keys.length : 0));
    return Object.freeze({
      ready: ready,
      viewId: id,
      dataset_context: context,
      datasetKey: context && context.dataset_key || null,
      generation: context && context.generation || null,
      epoch: context && context.epoch || null,
      pointCount: isFinite(pointCount) ? pointCount : 0,
      pointSize: ready ? (singleActive === id ? ps : view.data.point_size) : null,
      pointOpacity: ready ? (singleActive === id ? pointOpacity : view.data.point_opacity) : null,
      xExtent: ready ? numericExtent(view.data.x) : null,
      yExtent: ready ? numericExtent(view.data.y) : null,
      backgroundIdentity: ready ? view.background_identity || null : null,
      backgroundIdentities: ready ? (view.data.panels || []).map(function (panel) {
        return canonicalIdentity(panel.image_identity);
      }).filter(Boolean) : [],
      wireToken: ready && view.data.wire_token != null
        ? view.data.wire_token : null
    });
  }

  function downloadSinglePNG(id) {
    if (singleActive !== id) return false;
    return downloadVisiblePanelsPNG(pngFilename(id));
  }

  function applySingleState(id, saved) {
    var target = registerSingle(id); if (!target) return { selectedCells: 0 };
    if (!target.data) {
      target.pendingSavedState = saved;
      return { selectedCells: saved && saved.selection ? saved.selection.cells.length : 0 };
    }
    var selection = saved && saved.selection || {};
    var view = saved && saved.view || {};
    var viewport = view.zoomed === false ? null : view.viewport;
    if (viewport && viewport.cx == null && viewport.x0 != null) {
      viewport = {
        cx: (Number(viewport.x0) + Number(viewport.x1)) / 2,
        cy: (Number(viewport.y0) + Number(viewport.y1)) / 2,
        span: Math.max(Number(viewport.x1) - Number(viewport.x0),
          Number(viewport.y1) - Number(viewport.y0))
      };
    }
    target.selection = (selection.cells || []).map(String);
    target.hiddenGroups = (view.hidden_groups || []).map(String);
    target.mode = view.mode || 'lasso';
    target.lenses = [{
      view: viewport ? Object.assign({}, viewport) : null,
      rot: view.rotation ? {
        rx: Number(view.rotation.x) || 0, ry: Number(view.rotation.y) || 0
      } : null,
      lassoData: selection.geometry && selection.geometry.polygon
        ? selection.geometry.polygon.map(function (point) {
          return [Number(point[0]), Number(point[1])];
        }) : null
    }];
    if (visibleSingleId() === id) activateSingle(id, false, true);
    return { selectedCells: target.selection.length };
  }

  function updateSingleBackground(message) {
    if (!message || !message.id || !datasetMessageFresh(message)) return false;
    var id = message.id;
    var view = singleView(id); if (!view) return false;
    var incomingIdentity = canonicalIdentity(message.background_identity);
    var panelMatch = (view.data && view.data.panels || []).some(function (panel) {
      return canonicalIdentity(panel.image_identity) === incomingIdentity;
    });
    if (!incomingIdentity || (incomingIdentity !== view.background_identity && !panelMatch)) return false;
    var values = message.values || {};
    var names = {
      opacity: 'opacity', offsetX: 'offsetX', offsetY: 'offsetY',
      scaleX: 'scaleX', scaleY: 'scaleY', flipX: 'flipX',
      flipY: 'flipY', rotate: 'rotate'
    };
    view.backgroundEdits = view.backgroundEdits || {};
    var oldEdit = view.backgroundEdits[incomingIdentity];
    view.backgroundEdits[incomingIdentity] = { identity: message.background_identity,
      values: Object.assign({}, oldEdit && oldEdit.values || {}, values) };
    if (!panelMatch) view.backgroundState = Object.assign({}, view.backgroundState || {}, values);
    if (singleActive !== id) return true;
    singleSpaceIds.forEach(function (spaceId) {
      var space = spaceById[spaceId]; if (!space || !space._imgState) return;
      if (panelMatch && space._backgroundIdentity !== incomingIdentity) return;
      Object.keys(names).forEach(function (name) {
        if (values[name] != null) space._imgState[names[name]] = values[name];
      });
      stashImgState(space);
    });
    drawAll();
    return true;
  }

  function onData(bundle) {
    if (!datasetMessageFresh(bundle)) return false;
    wireToken++;
    applyData(bundle);
    return true;
  }

  function requestWireFallback(datasetId, fingerprint) {
    Shiny.setInputValue('coordviews_wire_fallback', {
      dataset_id: datasetId || '',
      dataset_fingerprint: fingerprint || '',
      dataset_context: currentDatasetContext(),
      nonce: Date.now()
    }, { priority: 'event' });
  }

  function onBinaryData(buffer) {
    var token = ++wireToken;
    var started = performance.now();
    var decoded;
    var decodedAt;
    try {
      if (!window.CBViewWire || !window.CBViewWire.supported) {
        throw new Error('Linked views binary transport is unavailable');
      }
      decoded = reuseSharedProjection(window.CBViewWire.unpack(buffer));
      decodedAt = performance.now();
    } catch (error) {
      if (token !== wireToken) return;
      requestWireFallback('', '');
      return;
    }
    hydrateLinkedPrimaryResources(decoded).then(function (hydrated) {
      if (token !== wireToken) return;
      var applyStarted = performance.now();
      applyData(hydrated.bundle);
      var applyMs = performance.now() - applyStarted;
      var rendererStats = panels.length && panels[0].gpu && panels[0].gpu.stats
        ? panels[0].gpu.stats() : {};
      transportMetrics.primary = CBViewState.telemetry.transport(
        decoded.transport_profile,
        {
        bytes: buffer.byteLength,
        clickToRequestMs: linkedRequestTiming.clickToRequestMs,
        requestToBinaryMs: isFinite(linkedRequestTiming.requestAtMs)
          ? decodedAt - linkedRequestTiming.requestAtMs : null,
        decodeMs: decodedAt - started,
        projectionFetchMs: hydrated.projectionFetchMs,
        projectionBytes: hydrated.projectionBytes,
        projectionDownloadMs: hydrated.projectionDownloadMs,
        projectionDecodeMs: hydrated.projectionDecodeMs,
        metadataFetchMs: hydrated.metadataFetchMs,
        metadataBytes: hydrated.metadataBytes,
        applyMs: applyMs,
        rendererInitializationMs: Number(rendererStats.initializationMs),
        decodeToDrawMs: performance.now() - decodedAt
        },
        true,
        Date.now()
      );
    }).catch(function () {
      if (token !== wireToken) return;
      requestWireFallback(decoded && decoded.dataset_id,
        decoded && decoded.dataset_fingerprint);
    });
  }

  function hydrateLinkedGeometry(extra) {
    (extra.spaces || []).forEach(function (space) {
      var ref = space.geometry_ref;
      if (!ref) return;
      var base = D.projections && D.projections[ref.projection];
      if (!base || !base.x || !base.y || base.x.length !== D.n || base.y.length !== D.n ||
          !ref.exclude || typeof ref.exclude.length !== 'number') throw new Error('Invalid linked geometry reference');
      var last = -1;
      for (var j = 0; j < ref.exclude.length; j++) {
        var index = ref.exclude[j];
        if (!Number.isInteger(index) || index <= last || index >= D.n) throw new Error('Invalid linked geometry subset');
        last = index;
      }
      space.x = ref.exclude.length ? base.x.slice() : base.x;
      space.y = ref.exclude.length ? base.y.slice() : base.y;
      for (var k = 0; k < ref.exclude.length; k++) {
        space.x[ref.exclude[k]] = NaN; space.y[ref.exclude[k]] = NaN;
      }
      var original = spaceById[projectionId(ref.projection)];
      if (!ref.exclude.length && original) space._unit = original._unit;
    });
    return extra;
  }

  function hydrateSparseLinked(extra) {
    var cloneIndex = extra.clone && extra.clone.index;
    var hydrateGroups = function (groups) {
      Object.keys(groups || {}).forEach(function (name) {
        var group = groups[name];
        var index = group && group.clone_index ? cloneIndex : group && group.index;
        if (!group || !index || !group.values || group.default == null) return;
        group.values = window.CBViewState.expandSparse(
          index, group.values, D.n, Number(group.default)
        );
        delete group.index; delete group.clone_index; delete group.default;
      });
    };
    hydrateGroups(extra.groups);
    hydrateGroups(extra.cat_extra);
    if (extra.clone && cloneIndex && extra.clone.code) {
      extra.clone.id = window.CBViewState.expandSparse(
        cloneIndex, extra.clone.code, D.n, -1
      );
      delete extra.clone.index; delete extra.clone.code;
    }
    return extra;
  }

  function applyCloneSupplement(extra) {
    var cloneSpace = (extra.spaces || []).filter(function (space) {
      return space && space.id === 'clone';
    })[0];
    var oldOrder = orderedSpaces();
    var appendOnly = !singleActive && colorBy !== GENE_PANELS_MODE &&
      !spaceById.clone && cloneSpace && oldOrder.every(function (id) {
        var space = spaceById[id];
        return id === 'trekker' || isProjectionSpace(space) ||
          !!(space && space.trajectory) || isSpatialSpace(space);
      });
    if (!appendOnly) return false;
    ensurePanelSlots(oldOrder.length + 1);
    buildPanels();
    var panel = panels[oldOrder.length];
    if (!panel) return false;

    D.groups = Object.assign({}, D.groups, extra.groups);
    D.cat_extra = Object.assign({}, D.cat_extra, extra.cat_extra);
    D.cat_skipped = extra.cat_skipped || D.cat_skipped;
    D.fields = Object.assign({}, D.fields, extra.fields);
    D.spaces = (D.spaces || []).concat(extra.spaces || []);
    D.clone = extra.clone;
    linkedBundle = D;
    sanitiseColors(extra);

    cloneSpace._unit = null;
    spaceById.clone = cloneSpace;
    syncCloneTiers();
    setSegOn('stack');
    applyCloneLayout('stack');

    panel.spaceId = 'clone';
    panel.colorBy = null;
    clearPanelView(panel);
    if (panel.pane) {
      var reappearing = panel.pane.classList.contains('cv-hidden');
      panel.pane.classList.remove('cv-hidden', 'cv-focus-primary', 'cv-focus-context');
      panel.pane.style.order = '';
      if (focusPanel) {
        panel.pane.classList.add('cv-focus-context');
        panel.pane.style.order = '1';
        var role = panel.pane.querySelector('.cv-role-badge');
        if (role) {
          role.style.display = '';
          role.textContent = 'CONTEXT';
          role.classList.remove('is-focus');
          role.classList.add('is-context');
        }
      }
      if (reappearing) fadeInPane(panel.pane);
    }
    var title = $('cv-title-' + panel.key.toLowerCase());
    if (title) title.textContent = cloneSpace.label || 'Clonal expansion';
    var moran = $('cv-moran-' + panel.key.toLowerCase());
    if (moran) { moran.style.display = 'none'; moran.textContent = ''; }
    var trekker = $('cv-tk-info-' + panel.key.toLowerCase());
    if (trekker) trekker.style.display = 'none';

    updateFocusButtons();
    updateZselButtons();
    syncOrbitButtons();
    updateSpaceScopedControls();
    renderWorkspaceGuide();
    renderMeta();
    fillColorPicker();
    renderGroupFilters();
    renderLegend();
    renderReadout();
    _layoutKey = null;
    resizeAll();
    return true;
  }

  function linkedSupplementFresh(extra) {
    return !!(extra && linkedBundle && datasetMessageFresh(extra) &&
      sameDatasetContext(extra.dataset_context, linkedBundle.dataset_context) &&
      extra.dataset_id === linkedBundle.dataset_id &&
      extra.dataset_fingerprint === configFingerprint() &&
      extra.progressive_token === linkedBundle.progressive_token);
  }

  function applyHydratedSupplement(extra, metric) {
    if (!linkedSupplementFresh(extra)) return;
    // A clone supplement can arrive here in a later animation frame, after
    // navigation has mounted another page's data in the shared renderer.
    if (D !== linkedBundle) {
      deferredLinkedSupplements.push({ extra: extra, metric: metric });
      return;
    }
    var drawStarted = performance.now();
    try {
      var structural = !!(extra.clone || extra.trekker ||
        (extra.spaces && extra.spaces.length) ||
        (extra.projections && Object.keys(extra.projections).length));
      var cloneOnly = !!(extra.clone && !extra.trekker &&
        extra.spaces && extra.spaces.length === 1 &&
        extra.spaces[0].id === 'clone' &&
        (!extra.projections || !Object.keys(extra.projections).length));
      if (cloneOnly && applyCloneSupplement(extra)) {
        if (extra.progressive_complete !== false) D.progressive = false;
        reportWorkspaceReady();
      } else if (structural) {
        var saved = exportRuntimeWorkspace();
        var hadSpatial = !!spatialTemplate;
        var merged = Object.assign({}, D, {
          groups: Object.assign({}, D.groups, extra.groups),
          cat_extra: Object.assign({}, D.cat_extra, extra.cat_extra),
          cat_skipped: extra.cat_skipped || D.cat_skipped,
          fields: Object.assign({}, D.fields, extra.fields),
          projections: Object.assign({}, D.projections, extra.projections),
          spaces: (D.spaces || []).concat(extra.spaces || []),
          clone: extra.clone || D.clone,
          trekker: extra.trekker || D.trekker,
          progressive: extra.progressive_complete === false
            ? D.progressive : false
        });
        drawAllDeferred = true;
        try {
          applyData(merged);
          // The primary frame has no Spatial choice to restore. Keep the
          // default arriving sections, but preserve an explicit empty choice
          // when Spatial was already available before this supplement.
          if (!hadSpatial && spatialTemplate && saved.view) {
            saved.view.spatial_sections = selectedSpatial.slice();
          }
          restoreWorkspace(saved);
        } finally {
          drawAllDeferred = false;
        }
        drawAll();
        reportWorkspaceReady();
      } else {
        D.groups = Object.assign({}, D.groups, extra.groups);
        D.cat_extra = Object.assign({}, D.cat_extra, extra.cat_extra);
        D.cat_skipped = extra.cat_skipped || D.cat_skipped;
        D.fields = Object.assign({}, D.fields, extra.fields);
        if (extra.progressive_complete !== false) D.progressive = false;
        linkedBundle = D;
        sanitiseColors(D);
        fillColorPicker();
        renderGroupFilters();
        renderLegend();
        reportWorkspaceReady();
      }
      transportMetrics[extra.progressive_complete === false
        ? 'clone' : 'supplement'] = CBViewState.telemetry.snapshot(metric, {
        decodeToDrawMs: performance.now() - drawStarted
      });
      if (!D.progressive && pendingCloneSupplement) {
        var pending = pendingCloneSupplement;
        pendingCloneSupplement = null;
        window.requestAnimationFrame(function () {
          window.setTimeout(function () {
            applyHydratedSupplement(pending.extra, pending.metric);
          }, 0);
        });
      }
    } catch (error) {
      requestWireFallback(
        D && D.dataset_id,
        D && configFingerprint()
      );
    }
  }

  function onBinarySupplement(buffer) {
    var started = performance.now();
    try {
      var extra = window.CBViewWire.unpack(buffer);
      var decodedAt = performance.now();
      if (!extra) throw new Error('Linked views supplement is empty');
      if (!linkedSupplementFresh(extra)) return;
      // Hydration and drawing use D. Keep the wire payload until the Linked
      // bundle is mounted again, instead of dropping it on a specialist tab.
      if (D !== linkedBundle) {
        deferredLinkedSupplements.push({ buffer: buffer });
        return;
      }
      var finish = function () {
        if (!linkedSupplementFresh(extra)) return;
        if (D !== linkedBundle) {
          deferredLinkedSupplements.push({ buffer: buffer });
          return;
        }
        extra = hydrateSparseLinked(hydrateLinkedGeometry(extra));
        var metric = CBViewState.telemetry.transport(
          extra.transport_profile,
          {bytes: buffer.byteLength, decodeMs: decodedAt - started},
          false,
          Date.now()
        );
        if (extra.progressive_complete === false && D.progressive) {
          pendingCloneSupplement = { extra: extra, metric: metric };
          return;
        }
        applyHydratedSupplement(extra, metric);
      };
      // Projections the Viewer Pack serves arrive as descriptors; fetch them
      // (shared HTTP cache) before merging. Any failure takes the full bundle.
      var resourced = Object.keys(extra.projections || {}).filter(function (name) {
        var projection = extra.projections[name];
        return projection && projection.projection_resource &&
          projection.projection_resource.url;
      });
      if (!resourced.length) {
        finish();
        return;
      }
      var bundle = linkedBundle;
      Promise.all(resourced.map(function (name) {
        var projection = extra.projections[name];
        return fetchValidatedProjectionResource(
          projection.projection_resource,
          { dataset_identity: linkedBundleResourceIdentity(bundle) },
          bundle.n
        ).then(function (result) {
          var coordinates = result.coordinates;
          projection.x = coordinates.x;
          projection.y = coordinates.y;
          if (coordinates.z) projection.z = coordinates.z;
          if (coordinates.xRange) projection.xRange = coordinates.xRange;
          if (coordinates.yRange) projection.yRange = coordinates.yRange;
          delete projection.projection_resource;
        });
      })).then(function () {
        try {
          finish();
        } catch (error) {
          requestWireFallback(D && D.dataset_id, D && configFingerprint());
        }
      }, function () {
        if (linkedSupplementFresh(extra)) {
          requestWireFallback(bundle.dataset_id, configFingerprint());
        }
      });
    } catch (error) {
      requestWireFallback(D && D.dataset_id, D && configFingerprint());
    }
  }

  function onBinaryAttribute(buffer) {
    try {
      var message = window.CBViewWire.unpack(buffer);
      if (!datasetMessageFresh(message) || !D || !message || message.dataset_id !== D.dataset_id ||
          message.dataset_fingerprint !== configFingerprint()) return;
      var target = D[message.kind];
      if (!target || !target[message.name] || !message.value) return;
      target[message.name] = message.value;
      var mode = message.kind === 'fields'
        ? FIELD_PREFIX + message.name : message.name;
      setColorBy(mode);
    } catch (error) {
      requestWireFallback(D && D.dataset_id, D && configFingerprint());
    }
  }

  function onSingleBinary(buffer) {
    var receivedAt = performance.now();
    try {
      var decoded = window.CBViewWire.unpack(buffer);
      var decodedAt = performance.now();
      if (!decoded || !decoded.id || !datasetMessageFresh(decoded)) return;
      recordSpecialistPayload('primary', decoded.id, buffer.byteLength);
      var profile = decoded.transport_profile || {};
      var transportTiming = CBViewState.telemetry.transport(
        profile,
        {
          bytes: buffer.byteLength,
          binaryReceivedAtMs: receivedAt,
          decodeMs: decodedAt - receivedAt,
          requestToBinaryMs: isFinite(Number(profile.request_at_ms))
            ? Date.now() - Number(profile.request_at_ms) : null
        },
        false,
        Date.now()
      );
      var timing = CBViewState.specialistLifecycle.update(
        singleTiming, decoded.id, transportTiming
      );
      var requestedSharedProjection = !!decoded.shared_projection;
      var message = reuseSharedSingleProjection(decoded);
      timing.geometryReused = requestedSharedProjection && !!message;
      if (!message) {
        singleRequests.delete(singleRequestKey(decoded.id, decoded.dataset_context));
        Shiny.setInputValue('coordviews_shared_base', {}, { priority: 'event' });
        Shiny.setInputValue(decoded.id + '_render_request', {nonce:Date.now(), dataset_context:currentDatasetContext()}, {
          priority: 'event'
        });
        return;
      }
      if (!datasetMessageFresh(message)) return;
      hydrateSingleProjectionResource(message).then(function (hydrated) {
        if (!datasetMessageFresh(hydrated)) return;
        if (hydrated.dataset_identity) {
          window.cerebroSavedViewDataset = hydrated.dataset_identity;
        }
        renderSingle(hydrated);
      }).catch(function () {
        if (!datasetMessageFresh(message)) return;
        var failed = message.data && (
          message.data.projection_resource ||
          (message.data.trajectory_frame_resource &&
            message.data.trajectory_frame_resource.projection)
        );
        if (failed && failed.url && typeof Shiny !== 'undefined') {
          Shiny.setInputValue(
            message.id + '_projection_resource_failed',
            String(failed.url),
            { priority: 'event' }
          );
        }
      });
    } catch (error) {
      return;
    }
  }

  function onSingleAuxBinary(buffer) {
    var message;
    try {
      message = window.CBViewWire.unpack(buffer);
    } catch (error) {
      return;
    }
    if (message && message.id) {
      recordSpecialistPayload('aux', message.id, buffer.byteLength);
    }
    applySingleAux(message);
  }

  function applySingleAux(message) {
    try {
      if (!datasetMessageFresh(message)) return;
      var view = message && singleView(message.id, message.dataset_context);
      if (!view || !sameDatasetContext(view.dataset_context, message.dataset_context) ||
          Number(view.aux_render_token) !== Number(message.render_token)) return;
      if (!CBViewState.specialistLifecycle.acceptAux(view, message)) return;
      var hasCells = message.selection_key != null;
      if (hasCells) view.data.selection_key = message.selection_key;
      if (hasCells && !(message.hover && message.hover.pending)) {
        singleAuxCache[message.id] = Object.assign({}, message, {
          aux_origin_token: message.aux_origin_token != null
            ? message.aux_origin_token : message.wire_token
        });
      }
      view.hover = message.hover || {};
      if (singleActive !== message.id) return;
      var canonicalGroups = view.data.canonical_group;
      if (hasCells) {
        var cells = singlePayloadCells(view);
        if (ArrayBuffer.isView(canonicalGroups) && canonicalGroups.length === D.n) {
          cells = canonicalAuxValues(view.data.selection_key, canonicalGroups, '');
        }
        if (cells.length !== D.n) return;
        D.cells = cells;
        singleIndexCells = null; singleIndexMap = null;
        reportSelection('aux');
      }
      var nested = view.meta && view.meta.color_type === 'categorical' &&
        !ArrayBuffer.isView(canonicalGroups);
      var offsets = null;
      if (nested) {
        var groups = Array.isArray(view.data.x) ? view.data.x : [];
        offsets = new Uint32Array(groups.length + 1);
        groups.forEach(function (group, index) {
          offsets[index + 1] = offsets[index] + (group ? group.length : 0);
        });
      }
      var hover = view.hover || {};
      var modes = hover.hoverinfo;
      var enabled = Array.isArray(modes)
        ? modes.some(function (mode) { return mode !== 'skip'; })
        : modes !== 'skip';
      singleSpaceIds.forEach(function (spaceId) {
        var space = spaceById[spaceId];
        if (!space) return;
        space._hover = ArrayBuffer.isView(canonicalGroups)
          ? (canonicalAuxValues(hover.text, canonicalGroups, '') || [])
          : (Array.isArray(hover.text) ? hover.text : []);
        space._hoverColumns = Array.isArray(hover.columns)
          ? hover.columns.map(function (column) {
            if (!ArrayBuffer.isView(canonicalGroups)) return column;
            return Object.assign({}, column, {
              values: canonicalAuxValues(column.values, canonicalGroups, null)
            });
          }) : [];
        space._hoverModes = modes;
        space._hoverOffsets = offsets;
        space._hoverEnabled = enabled;
        space._hoverMask = null;
      });
      if (singleSpaceIds.length && view.meta && view.meta.space_label) {
        cacheSharedProjection(
          String(view.meta.space_label),
          spaceById[singleSpaceIds[0]],
          D.n
        );
      }
      panels.forEach(function (panel) {
        var tip = $(panel.tipId);
        var cell = pinnedTip.panel === panel ? pinnedTip.cell : hoverCell;
        if (tip && cell != null && Number(tip.style.opacity) > 0) {
          if (pinnedTip.panel === panel) pinTip(panel, cell);
          else tip.innerHTML = hoverHtml(cell, false, panel);
        }
      });
      if (cardOpen()) requestCardMetadata();
    } catch (error) {
      return;
    }
  }

  function applyData(bundle) {
    if (!datasetMessageFresh(bundle)) return;
    // A data set the builders cannot turn into a bundle (no embedding, or a
    // build error) arrives as {error: "..."}. Blank the workspace and SAY so —
    // returning early would leave the PREVIOUS data set's panels on screen,
    // silently attributing one data set's cells to another.
    if (!bundle || bundle.error || !bundle.spaces || !bundle.spaces.length) {
      linkedBundle = null;
      resetSingleViews();
      showUnavailable(bundle && bundle.error);
      reportWorkspaceReady();
      return;
    }
    if (singleActive) {
      stashSingleState(); restoreLinkedSurface();
      singleActive = null; singleSpaceIds = []; singleSpaceModes = {};
    }
    linkedBundle = bundle;
    D = bundle;
    Object.keys(D.projections || {}).forEach(function (name) {
      var projection = D.projections[name];
      if (projection && projection.x && projection.y) {
        cacheSharedProjection(name, projection, D.n);
      }
    });
    sanitiseColors(D);
    syncCloneTiers();
    _clipD = null;   // ranges belong to the data set that produced them
    // Image ids belong to the object that produced them, so alignment stored
    // under them is dropped when the DATA SET changes -- not when a bundle
    // arrives. A bundle can be re-sent when returning to the tab; clearing on
    // every push meant a user's alignment work survived only until they looked
    // away.
    var datasetIdentity = datasetContextApi().key(D.dataset_context);
    var dataChanged = datasetIdentity !== dataShown;
    syncLinkedHoverDefault(dataChanged);
    syncLinkedBackgroundsDefault(dataChanged);
    var previousSelected = selectedSpatial.slice();
    var hadSpatial = !!spatialTemplate;
    var previousProjections = selectedProjections.slice();
    var previousActiveName = activeSpatial() && activeSpatial()._sampleName;
    if (dataChanged) {
      resetSingleViews();
      imgStates = {};
      imgChoice = {};
      pendingCloneDetails.clear();
      pendingCloneSupplement = null;
      deferredLinkedSupplements = [];
    }
    dataShown = datasetIdentity;
    imgToken++;
    closeCard(); cardMeta = null;   // the card described the previous data set
    var previousSpaces = spaceById;
    spaceById = {};
    spatialTemplate = null;
    D.spaces.forEach(function (s) {
      var referenced = s.geometry_ref && D.projections[s.geometry_ref.projection];
      if (!referenced || s.x !== referenced.x || s.y !== referenced.y) s._unit = null;
      if (s.id === 'spatial') spatialTemplate = s;
      else if (s.id === 'expression') { /* rebuilt from D.projections below */ }
      else {
        spaceById[s.id] = s;
        if (s.background_scope) s._sampleName = s.background_scope;
      }
    });
    var projectionNames = D.projections ? Object.keys(D.projections) : [];
    var configuredInitialProjections = Array.isArray(D.initial_projections)
      ? D.initial_projections
      : (typeof D.initial_projections === 'string' ? [D.initial_projections] : []);
    configuredInitialProjections = configuredInitialProjections.filter(function (name, index) {
      return projectionNames.indexOf(name) >= 0 &&
        configuredInitialProjections.indexOf(name) === index;
    });
    selectedProjections = dataChanged
      ? configuredInitialProjections
      : previousProjections.filter(function (name) {
      return projectionNames.indexOf(name) >= 0;
    });
    if (!selectedProjections.length && projectionNames.length) {
      selectedProjections = [D.default_projection &&
        projectionNames.indexOf(D.default_projection) >= 0
        ? D.default_projection : projectionNames[0]];
    }
    rebuildProjectionInstances(previousSpaces);
    var initialSamples = spatialSamples();
    var availableSamples = initialSamples.map(function (sample) { return sample.name; });
    selectedSpatial = dataChanged ? [] : previousSelected.filter(function (name) {
      return availableSamples.indexOf(name) >= 0;
    });
    // Show individual FOV/ROI scenes by default, without their duplicate
    // All ROIs overviews. Use scene metadata, never dataset names or labels.
    // Later bundles preserve manual choices, including overviews or none.
    if ((dataChanged || !hadSpatial) && initialSamples.length) {
      selectedSpatial = initialSamples.filter(function (sample) {
        return sample.is_roi_overview !== true;
      }).map(function (sample) { return sample.name; });
    }
    var activeName = !dataChanged && selectedSpatial.indexOf(previousActiveName) >= 0
      ? previousActiveName : selectedSpatial[0];
    activeSpatialId = activeName ? spatialId(activeName) : null;
    rebuildSpatialInstances();
    Object.keys(spaceById).forEach(function (id) {
      var direct = spaceById[id];
      if (!direct || !direct.background_scope) return;
      var images = spatialImages(direct);
      var scopeKey = backgroundStateKey(direct);
      direct._customImageId = Object.prototype.hasOwnProperty.call(imgChoice, scopeKey)
        ? imgChoice[scopeKey] : ((images[0] && images[0].id) || IMG_NONE);
      loadSpaceImage(direct);
    });
    colorBy = D.default_group ||
      (D.groups ? Object.keys(D.groups)[0] : null) || null;
    unpinTip();
    hidden = new Set(); sel = null; selectionSource = null;
    selectionSourceSpace = null;
    pick = null; hoverCell = null;
    focusPanel = null;
    // Reset point appearance only for a genuinely different data set. A bundle
    // can be re-sent after returning to the tab; that refresh must not discard
    // the user's shared override.
    if (dataChanged) {
      var authorDisplay = D.display_defaults || {};
      labelsOn = authorDisplay.group_labels === true;
      bordersOn = authorDisplay.draw_border === true;
      keepPlotsSquare = authorDisplay.keep_square === true;
      colorClip = [0, 0.01, 0.02, 0.05].indexOf(authorDisplay.color_clip) >= 0
        ? authorDisplay.color_clip : 0.01;
      var clipControl = $('cv-clip');
      if (clipControl) clipControl.value = String(colorClip);
      groupFilter = {};
      var labelsControl = $('cv-labels');
      if (labelsControl) labelsControl.checked = labelsOn;
      var bordersControl = $('cv-borders');
      if (bordersControl) bordersControl.checked = bordersOn;
      var squareControl = $('cv-square-plots');
      if (squareControl) squareControl.checked = keepPlotsSquare;
      var configuredOpacity = Number(D.default_point_opacity);
      pointOpacity = D.default_point_opacity != null && isFinite(configuredOpacity)
        ? Math.max(0, Math.min(1, configuredOpacity)) : 0.8;
      var configuredPointSize = Number(D.default_point_size);
      ps = D.default_point_size != null && isFinite(configuredPointSize)
        ? Math.max(1, Math.min(20, configuredPointSize)) : 5;
      pointSizeEdited = false;
      pointOpacityEdited = false;
      psSeeded = true;
      setLinkedSliderValue('cv-ps', ps, 0.2);
      setLinkedSliderValue('cv-opacity', pointOpacity, 0.05);

      var configuredPercentage = Number(D.default_percentage_cells_to_show);
      pctShow = D.default_percentage_cells_to_show != null &&
        isFinite(configuredPercentage)
        ? Math.max(10, Math.min(100, configuredPercentage)) : 100;
      rebuildPctMask();
      setLinkedSliderValue('cv-pct', pctShow, 10);
    }
    syncSharedScatter();
    syncPointControls();
    closeMore();   // a new data set starts with advanced settings closed
    // Trekker controls reset
    dissolvePct = 0; dissolveThresh = null; evidenceOn = false; nicheRadius = 250;
    nicheSet = null;
    setLinkedSliderValue('cv-dissolve', dissolvePct, 5);
    var evEl = $('cv-evidence'); if (evEl) evEl.checked = false;
    setLinkedSliderValue('cv-niche', nicheRadius, 25);

    ensurePanelSlots(orderedSpaces().length);
    renderMeta();

    buildPanels();
    // Give every present space its own panel and hide the unused slots (this also
    // places the Trekker info button on the Trekker panel).
    layoutPanels();
    syncModeButtons();
    updateSelActions();   // hide Clear until a selection exists
    updateZselButtons();
    // Immune axis present → reveal the clonal-layout switch and lay out the
    // clone space (client-owned) before the first projection runs.
    var hasClone = !!spaceById['clone'];
    if (hasClone) { setSegOn('stack'); applyCloneLayout('stack'); }
    // Show the space-scoped controls that match the spaces now on screen.
    updateSpaceScopedControls();
    // Trekker controls (dissolve + evidence) appear only when the bundle has them
    var hasTk = !!(D.trekker && (D.trekker.conf || D.trekker.evidence));
    setTrekkerSettingsVisible(hasTk);
    var tkInsights = $('cv-tk-insights');
    if (tkInsights) tkInsights.style.display = D.trekker ? '' : 'none';
    setTrekkerInsightsOpen(false);
    selectTrekkerInsight('cell');
    if (D.trekker) fillTrekkerInsights();
    // positioning-evidence markers default ON when the data set carries them
    evidenceOn = !!(D.trekker && D.trekker.evidence);
    var evChk = $('cv-evidence'); if (evChk) evChk.checked = evidenceOn;
    fillColorPicker();
    fillProjPicker();
    fillSpatialPicker();
    renderGroupFilters();
    activateSpatial(activeSpatialId);
    // hide the gene/RGB pickers on a fresh dataset (starts in a categorical mode)
    var geneCtl = $('cv-gene-ctl'), rgbCtl = $('cv-rgb-ctl');
    if (geneCtl) geneCtl.style.display = 'none';
    if (rgbCtl) rgbCtl.style.display = 'none';
    updateClipControl();
    panels.forEach(function (p) {
      var t = $('cv-title-' + p.key.toLowerCase());
      if (t) { var sp = spaceById[p.spaceId]; t.textContent = sp ? sp.label : p.spaceId; }
    });
    renderLegend();
    resizeAll();
    setTimeout(syncPointControls, 0);
    renderSelbar(); renderReadout();
    // Clear the server-side selection too: sel was reset to null above, but the
    // server input still holds the PREVIOUS dataset's barcodes until we push. The
    // "selected cells" plot/table (server-rendered) key off it, so without this
    // they'd show stale/wrong cells after a dataset switch.
    reportSelection();
    if (pendingColorPatch && applyColorPatch(pendingColorPatch)) {
      pendingColorPatch = null;
    }
    reportWorkspaceReady();
    if (!rebuildingBase) {
      var singleId = visibleSingleId();
      if (singleId && singleView(singleId)) activateSingle(singleId);
    }
  }

  // ---- portable local workspace ------------------------------------------
  // The JSON contains identities and interaction state only; no expression,
  // coordinates, images, receptor sequences or other source data leave D.
  function configFingerprint() {
    if (!D) return '';
    if (typeof D.dataset_fingerprint === 'string' && D.dataset_fingerprint) {
      return D.dataset_fingerprint;
    }
    if (typeof D.cell_fingerprint === 'string' && D.cell_fingerprint) {
      return D.cell_fingerprint;
    }
    if (!Array.isArray(D.cells)) return '';
    var a = 2166136261, b = 2246822519;
    D.cells.forEach(function (cell) {
      var text = String(cell) + '\u0000';
      for (var i = 0; i < text.length; i++) {
        a = Math.imul(a ^ text.charCodeAt(i), 16777619) >>> 0;
        b = Math.imul(b ^ text.charCodeAt(i), 3266489917) >>> 0;
      }
    });
    return D.cells.length + ':' + a.toString(16) + ':' + b.toString(16);
  }

  function selectedCellIds() {
    var out = [];
    if (sel && Array.isArray(D && D.cells)) {
      sel.forEach(function (index) { out.push(D.cells[index]); });
    }
    return out;
  }

  function selectedCellIndices() {
    return sel ? Array.from(sel) : [];
  }

  function namedFilters() {
    var out = {};
    Object.keys(groupFilter).forEach(function (name) {
      var group = D.groups && D.groups[name];
      if (!group) return;
      out[name] = Array.from(groupFilter[name]).map(function (index) {
        return group.levels[index];
      });
    });
    return out;
  }

  function savedHiddenLevels() {
    var group = catOf(colorBy);
    if (!group || !hidden.size) return [];
    return [{
      group: colorBy,
      levels: Array.from(hidden).sort(function (a, b) { return a - b; })
        .map(function (index) { return group.levels[index]; })
    }];
  }

  function savedLenses() {
    return panels.filter(function (panel) { return !!panel.spaceId; }).map(function (panel) {
      var view = panel.view || { cx: 0.5, cy: 0.5, span: 1 };
      return {
        space: panel.spaceId,
        viewport: { cx: view.cx, cy: view.cy, span: view.span },
        rotation: panel.rot ? { rx: panel.rot.rx, ry: panel.rot.ry } : null
      };
    });
  }

  function savedGeometry() {
    var source = null;
    panels.some(function (panel) {
      if (!panel.lassoData || !panel.spaceId) return false;
      source = {
        space: panel.spaceId,
        mode: selectMode === 'box' ? 'box' : 'lasso',
        polygon: panel.lassoData
      };
      return true;
    });
    return source;
  }

  function savedBackgrounds() {
    return selectedSpatial.map(function (name) {
      var space = spaceById[spatialId(name)];
      var images = spatialImages(space);
      if (!space || !images.length) return null;
      stashImgState(space);
      var selectedImage = space._customImageId || images[0].id;
      var image = currentImage(space) || images.filter(function (item) {
        return item.id === selectedImage;
      })[0] || images[0];
      var key = imgKey(space, image);
      var state = space._imgState || (key && imgStates[key]) || presetState(image);
      return {
        section: name,
        mode: selectedImage === IMG_NONE ? 'none' : 'image',
        image_id: selectedImage === IMG_NONE ? null : selectedImage,
        opacity: state.opacity,
        alignment: {
          show: state.show !== false,
          offset_x: state.offsetX,
          offset_y: state.offsetY,
          scale_x: state.scaleX,
          scale_y: state.scaleY,
          rotation: state.rotate,
          flip_x: !!state.flipX,
          flip_y: !!state.flipY,
          lock_aspect: state.lock !== false
        }
      };
    }).filter(Boolean);
  }

  function selectedGene(id) {
    var input = $(id);
    return input && typeof input.value === 'string' ? input.value : '';
  }
  function selectedGenes(id) {
    var input = $(id);
    if (!input) return [];
    if (input.selectize) return input.selectize.items.slice();
    return Array.prototype.map.call(input.selectedOptions || [], function (option) {
      return option.value;
    });
  }

  function exportRuntimeWorkspace() {
    var config = exportWorkspace();
    // Internal navigation/progressive redraws must retain author defaults.
    // Keep this out of the portable JSON schema; imported views still apply
    // their explicitly saved display settings.
    Object.defineProperty(config, '_pointOverrides', {
      value: { size: pointSizeEdited, opacity: pointOpacityEdited }
    });
    return config;
  }
  function workspacePointOverrides(config) {
    return config._pointOverrides || { size: true, opacity: true };
  }
  function exportWorkspace() {
    if (!D) throw new Error('Load a data set before saving a linked view.');
    var active = activeSpatial();
    var focusSpace = null;
    panels.forEach(function (panel) {
      if (panel.key === focusPanel) focusSpace = panel.spaceId;
    });
    return {
      schema: 'cerebronexus-linked-view',
      version: 1,
      created_at: new Date().toISOString().replace(/\.\d{3}Z$/, 'Z'),
      dataset: {
        cell_count: D.n,
        cell_fingerprint: configFingerprint()
      },
      selection: {
        cells: selectedCellIds(),
        indices: selectedCellIndices(),
        source: selectionSource || null,
        geometry: savedGeometry()
      },
      view: {
        projections: selectedProjections.slice(),
        spatial_sections: selectedSpatial.slice(),
        active_spatial: active && active._sampleName || null,
        colour: {
          mode: colorBy,
          genes: (colorBy === GENE_MODE || colorBy === GENE_PANELS_MODE)
            ? selectedGenes('coordviews_gene') : [],
          gene: colorBy === GENE_MODE
            ? ((D.gene && D.gene.gene) || selectedGene('coordviews_gene')) : null,
          rgb_genes: colorBy === RGB_MODE
            ? ['r', 'g', 'b'].map(function (channel) {
              return selectedGene('coordviews_gene_' + channel);
            }) : [],
          clip: colorClip
        },
        filters: namedFilters(),
        hidden_levels: savedHiddenLevels(),
        display: {
          point_size: ps,
          point_opacity: pointOpacity,
          percentage_cells: pctShow,
          group_labels: labelsOn,
          cell_borders: bordersOn,
          background_images: linkedBackgroundsOn,
          selection_mode: selectMode === 'box' ? 'box' : 'lasso',
          clone_layout: CLONE_MODE,
          keep_square: keepPlotsSquare
        },
        focus_space: focusSpace,
        lenses: savedLenses(),
        spatial_backgrounds: savedBackgrounds(),
        trekker: {
          dissolve_percentage: dissolvePct,
          evidence: evidenceOn,
          niche_radius: nicheRadius
        }
      }
    };
  }

  function configError(message) {
    throw new Error(message);
  }

  function finiteNumber(value, fallback, min, max) {
    value = Number(value);
    if (!isFinite(value)) return fallback;
    return Math.max(min, Math.min(max, value));
  }

  function validateWorkspace(config) {
    if (!D) configError('Load a data set before opening a linked view.');
    if (!config || !config.dataset ||
        config.dataset.cell_fingerprint !== configFingerprint()) {
      configError('This linked view belongs to a different cell population.');
    }
    var view = config.view;
    var availableProjections = D.projections ? Object.keys(D.projections) : [];
    if (view.projections.some(function (name) {
      return availableProjections.indexOf(name) < 0;
    })) configError('The file uses a projection that is unavailable here.');

    var samples = spatialSamples();
    var availableSpatial = samples.map(function (sample) { return sample.name; });
    if (view.spatial_sections.some(function (name) {
      return availableSpatial.indexOf(name) < 0;
    })) configError('The file uses a Spatial section that is unavailable here.');

    var mode = view.colour.mode;
    if (!(catOf(mode) || fieldForMode(mode) || mode === GENE_MODE ||
      mode === GENE_PANELS_MODE || mode === RGB_MODE)) {
      configError('The file uses a colour mode that is unavailable here.');
    }
    Object.keys(view.filters).forEach(function (name) {
      if (!D.groups || !D.groups[name]) configError('The file contains an invalid filter.');
    });

    var baseSpaces = baseOrderedSpaces(view.projections, view.spatial_sections);
    var spaces = new Set(baseSpaces);
    if (mode === GENE_PANELS_MODE) {
      var maxGenes = Math.max(1, Math.floor(12 / Math.max(1, baseSpaces.length)));
      view.colour.genes.slice(0, maxGenes).forEach(function (_gene, geneIndex) {
        baseSpaces.forEach(function (spaceId) {
          spaces.add(window.CBViewState.genePanelSpaceId(geneIndex, spaceId));
        });
      });
    }
    if (view.lenses.some(function (lens) { return !spaces.has(lens.space); })) {
      configError('The file contains an invalid view lens.');
    }

    var indices = Array.isArray(config.selection.indices)
      ? config.selection.indices.map(Number) : null;
    if (indices && indices.some(function (index) {
      return !Number.isInteger(index) || index < 0 || index >= D.n;
    })) configError('The file selects cells that are unavailable here.');
    var cells = !indices && config.selection.cells.length ? singleIndex() : null;
    if (!indices && config.selection.cells.some(function (cell) {
      return !cells.has(String(cell));
    })) configError('The file selects cells that are unavailable here.');
    return { cells: cells, indices: indices };
  }

  function restoreWorkspace(config) {
    var validated = validateWorkspace(config);
    var view = config.view || {}, selection = config.selection || {};
    var restoredSelection = new Set(validated.indices || []);
    if (!validated.indices) selection.cells.forEach(function (cell) {
      restoredSelection.add(validated.cells.get(String(cell)));
    });

    setSelectedProjections(view.projections);
    setSelectedSpatial(view.spatial_sections);
    (view.spatial_backgrounds || []).forEach(function (saved) {
      var space = spaceById[spatialId(saved.section)];
      if (!space) return;
      var images = spatialImages(space);
      space._customImageId = saved.mode === 'none'
        ? IMG_NONE
        : (saved.image_id || ((images[0] && images[0].id) || IMG_NONE));
      imgChoice[backgroundStateKey(space)] = space._customImageId;
      var alignment = saved.alignment;
      if (alignment) {
        var image = currentImage(space), key = imgKey(space, image);
        if (key) imgStates[key] = {
          show: alignment.show !== false,
          opacity: finiteNumber(saved.opacity, .6, 0, 1),
          offsetX: finiteNumber(alignment.offset_x, 0, -1e9, 1e9),
          offsetY: finiteNumber(alignment.offset_y, 0, -1e9, 1e9),
          scaleX: finiteNumber(alignment.scale_x, 1, .000001, 1e6),
          scaleY: finiteNumber(alignment.scale_y, 1, .000001, 1e6),
          rotate: finiteNumber(alignment.rotation, 0, -1e6, 1e6),
          flipX: !!alignment.flip_x,
          flipY: !!alignment.flip_y,
          lock: alignment.lock_aspect !== false
        };
      }
      loadSpaceImage(space);
    });
    if (view.active_spatial) {
      activeSpatialId = spatialId(view.active_spatial);
      activateSpatial(activeSpatialId);
    }

    var colourState = view.colour, colour = colourState.mode;
    colorClip = Number(colourState.clip);
    var clipEl = $('cv-clip'); if (clipEl) clipEl.value = String(colorClip);
    if ((colour === GENE_MODE || colour === GENE_PANELS_MODE) &&
      Array.isArray(colourState.genes) && colourState.genes.length) {
      setColorBy(colour);
      var genePicker = $('coordviews_gene');
      if (genePicker && genePicker.selectize) {
        colourState.genes.forEach(function (gene) {
          genePicker.selectize.addOption({ value: gene, label: gene });
        });
        genePicker.selectize.setValue(colourState.genes, false);
      }
    } else if (colour === GENE_MODE && colourState.gene) {
      colourByGene(colourState.gene);
    } else {
      setColorBy(colour);
      var colourEl = $('cv-pick-color');
      if (colourEl) {
        if (colourEl.selectize) colourEl.selectize.setValue(colour, true);
        else colourEl.value = colour;
      }
    }
    if (colour === RGB_MODE) {
      ['r', 'g', 'b'].forEach(function (channel, index) {
        var gene = colourState.rgb_genes[index], el = $('coordviews_gene_' + channel);
        if (el && el.selectize) {
          if (gene) el.selectize.addOption({ value: gene, label: gene });
          el.selectize.setValue(gene, false);
        } else if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue('coordviews_gene_' + channel, gene);
        }
      });
    }

    groupFilter = {};
    Object.keys(view.filters || {}).forEach(function (name) {
      var group = D.groups && D.groups[name], levels = view.filters[name];
      var allowed = new Set();
      levels.forEach(function (level) {
        var index = group.levels.indexOf(level);
        allowed.add(index);
      });
      if (allowed.size < group.levels.length) groupFilter[name] = allowed;
    });
    hidden = new Set();
    var activeGroup = catOf(colorBy);
    var hiddenState = Array.isArray(view.hidden_levels)
      ? view.hidden_levels.filter(function (item) { return item.group === colorBy; })[0]
      : null;
    if (activeGroup && hiddenState) {
      hiddenState.levels.forEach(function (level) {
        var index = activeGroup.levels.indexOf(level);
        if (index >= 0) hidden.add(index);
      });
    }

    var display = view.display || {};
    ps = finiteNumber(display.point_size, ps, 0, 20);
    pointOpacity = finiteNumber(display.point_opacity, pointOpacity, 0, 1);
    pctShow = finiteNumber(display.percentage_cells, pctShow, 5, 100);
    psSeeded = true;
    var overrides = workspacePointOverrides(config);
    pointSizeEdited = overrides.size;
    pointOpacityEdited = overrides.opacity;
    labelsOn = display.group_labels === true;
    bordersOn = !!display.cell_borders;
    linkedBackgroundsOn = display.background_images !== false;
    syncLinkedBackgroundsDefault(false);
    keepPlotsSquare = !!display.keep_square;
    selectMode = display.selection_mode;
    applyCloneLayout(display.clone_layout);
    setSegOn(display.clone_layout);
    dissolvePct = finiteNumber(view.trekker && view.trekker.dissolve_percentage, dissolvePct, 0, 100);
    evidenceOn = !!(view.trekker && view.trekker.evidence);
    nicheRadius = finiteNumber(view.trekker && view.trekker.niche_radius, nicheRadius, 1, 100000);
    rebuildPctMask(); rebuildDissolve();

    [['cv-ps', ps], ['cv-opacity', pointOpacity], ['cv-pct', pctShow],
      ['cv-dissolve', dissolvePct], ['cv-niche', nicheRadius]].forEach(function (item) {
      setLinkedSliderValue(item[0], item[1]);
    });
    var labelsEl = $('cv-labels'); if (labelsEl) labelsEl.checked = labelsOn;
    var bordersEl = $('cv-borders'); if (bordersEl) bordersEl.checked = bordersOn;
    var squareEl = $('cv-square-plots'); if (squareEl) squareEl.checked = keepPlotsSquare;
    var evidenceEl = $('cv-evidence'); if (evidenceEl) evidenceEl.checked = evidenceOn;

    view.lenses.forEach(function (saved) {
      panels.forEach(function (panel) {
        if (panel.spaceId !== saved.space) return;
        var viewport = saved.viewport;
        panel.view = viewport.cx === 0.5 && viewport.cy === 0.5 &&
          viewport.span === 1 ? null : clampView(panel, viewport);
        panel.rot = saved.rotation && isFinite(saved.rotation.rx) &&
          isFinite(saved.rotation.ry)
          ? { rx: Number(saved.rotation.rx), ry: Number(saved.rotation.ry) } : null;
        project(panel);
      });
    });

    clearLassos();
    var geometry = selection.geometry;
    if (geometry && Array.isArray(geometry.polygon)) {
      panels.some(function (panel) {
        if (panel.spaceId !== geometry.space) return false;
        panel.lassoData = geometry.polygon.map(function (point) {
          return [Number(point[0]), Number(point[1])];
        });
        return true;
      });
    }

    focusPanel = null;
    if (view.focus_space) {
      panels.some(function (panel) {
        if (panel.spaceId !== view.focus_space) return false;
        setFocusPanel(panel.key);
        return true;
      });
    }
    renderGroupFilters(); renderLegend(); syncModeButtons();
    _layoutKey = null; resizeAll();
    setSelection(
      restoredSelection,
      geometry && geometry.space
        ? { spaceId: geometry.space }
        : (selection.source || 'Saved view')
    );
    return { selected_cells: restoredSelection.size };
  }

  function workspaceSummary() {
    var ownsWorkspace = !singleActive && D && D === linkedBundle && datasetMessageFresh(D);
    var primaryPanel = ownsWorkspace && panels.filter(function (panel) {
      return panel.spaceId && Number(panel.canvas.dataset.pointCount) === D.n;
    })[0];
    var rendered = !!(D && configFingerprint() && primaryPanel);
    var complete = !!(rendered && !D.progressive);
    var gpuUploadMs = 0;
    var gpuPositionUploads = 0;
    panels.forEach(function (panel) {
      var stats = panel.gpu && panel.gpu.stats ? panel.gpu.stats() : null;
      if (stats) {
        gpuUploadMs += Number(stats.uploadSubmitMs) || 0;
        gpuPositionUploads += Number(stats.positionUploads) || 0;
      }
    });
    return {
      primaryReady: rendered,
      ready: complete,
      complete: complete,
      datasetFingerprint: D ? configFingerprint() : null,
      renderedPointCount: primaryPanel && D ? Number(D.n) : 0,
      selectedCells: sel ? sel.size : 0,
      selectedCellBarcodes: selectedCellIds(),
      colourMode: colorBy,
      projections: selectedProjections.slice(),
      spatialSections: selectedSpatial.slice(),
      activeSpatial: D && activeSpatial() ? activeSpatial()._sampleName || null : null,
      transport: Object.assign({
        gpuUploadMs: gpuUploadMs,
        gpuPositionUploads: gpuPositionUploads
      }, transportMetrics)
    };
  }

  function applyWorkspace(config, colourData, selectionIndices) {
    if (Array.isArray(selectionIndices)) {
      config.selection.indices = selectionIndices;
    }
    if (D && colourData && colourData.mode === GENE_MODE) {
      D.gene = {
        gene: colourData.gene,
        v: colourData.v,
        max: colourData.max
      };
    } else if (D && colourData && colourData.mode === RGB_MODE) {
      D.rgb = {
        r: colourData.r,
        g: colourData.g,
        b: colourData.b,
        genes: colourData.genes
      };
    } else if (D && colourData && colourData.mode === GENE_PANELS_MODE) {
      D.genePanelMax = Number(colourData.max) || 1;
      D.genePanels = colourData.genes.map(function (gene, index) {
        return { gene: gene, v: colourData.values[index] || [] };
      });
    }
    var result = restoreWorkspace(config);
    return { selectedCells: result.selected_cells };
  }

  function requestedModalities() {
    var available = D && Array.isArray(D.available_modalities)
      ? D.available_modalities.filter(function (name) {
        return typeof name === 'string' && name.length;
      }) : [];
    return available.length ? available : ['all'];
  }

  var linkedReadyPaintToken = 0;

  function setLinkedPresentationReady(ready) {
    var root = document.querySelector('.linked-views-page');
    if (root) root.classList.toggle('cv-workspace-pending', !ready);
  }

  function linkedWorkspacePainted() {
    var meta = $('cv-meta');
    if (!meta || meta.offsetParent === null || !D) return false;
    return panels.some(function (panel) {
      if (!panel.spaceId ||
          Number(panel.canvas.dataset.pointCount) !== D.n) return false;
      var rect = panel.canvas.getBoundingClientRect();
      return rect.width > 0 && rect.height > 0 &&
        panel.canvas.width > 0 && panel.canvas.height > 0;
    });
  }

  function dispatchWorkspaceReady(summary, painted, eventKind) {
    window.dispatchEvent(new CustomEvent('cerebro:linkedviews-ready', {
      detail: {
        page: 'coordinated_views',
        dataset_context: D && D.dataset_context,
        primaryReady: summary.primaryReady,
        ready: summary.ready,
        painted: !!painted,
        complete: summary.complete,
        selectedCells: summary.selectedCells,
        datasetFingerprint: summary.datasetFingerprint,
        renderedPointCount: summary.renderedPointCount,
        projections: summary.projections.slice(),
        spatialSections: summary.spatialSections.slice(),
        benchmarkGeneration:
          Number(window.__cerebroPageBenchGeneration) || 0,
        eventKind: eventKind || 'primary'
      }
    }));
    if (summary.primaryReady && D.progressive && Shiny.setInputValue) {
      var key = String(D.dataset_id || '') + '\u0000' + configFingerprint() +
        '\u0000' + String(D.progressive_token || '');
      if (progressiveRequestedKey !== key) {
        progressiveRequestedKey = key;
        Shiny.setInputValue('coordviews_primary_ready', {
          dataset_context: D.dataset_context,
          dataset_id: D.dataset_id,
          dataset_fingerprint: configFingerprint(),
          progressive_token: D.progressive_token,
          modalities: requestedModalities(),
          nonce: Date.now()
        }, { priority: 'event' });
      }
    }
  }

  function linkedVisualReady() {
    if (!workspaceSummary().ready) {
      return Promise.resolve({ ready: false, reason: 'logical-not-ready' });
    }
    var visible = panels.filter(function (panel) {
      return panel.spaceId && panel.canvas &&
        Number(panel.canvas.dataset.pointCount) === D.n;
    });
    if (!visible.length || (D.clone && !visible.some(function (panel) {
      return panel.spaceId === 'clone';
    }))) {
      return Promise.resolve({ ready: false, reason: 'panel-not-ready' });
    }
    var idle = visible.map(function (panel) {
      return panel.gpu && panel.gpu.idle ? panel.gpu.idle() : Promise.resolve();
    });
    return Promise.all(idle).then(function () {
      return new Promise(function (resolve) {
        var previous = null;
        var stable = 0;
        var frames = 0;
        function sample() {
          requestAnimationFrame(function () {
            frames++;
            var signature = JSON.stringify(visible.map(function (panel) {
              var rect = panel.pane.getBoundingClientRect();
              return [
                panel.spaceId,
                rect.left,
                rect.top,
                rect.width,
                rect.height,
                getComputedStyle(panel.pane).opacity,
                panel.canvas.width,
                panel.canvas.height,
                panel.canvas.dataset.pointCount
              ];
            }));
            stable = signature === previous ? stable + 1 : 0;
            previous = signature;
            var gpuReady = visible.every(function (panel) {
              return !panel.gpu || !panel.gpu.isReady || panel.gpu.isReady();
            });
            if (stable >= 1 && gpuReady) {
              resolve({
                ready: true,
                frames: frames,
                panels: visible.length,
                clone: !D.clone || visible.some(function (panel) {
                  return panel.spaceId === 'clone';
                }),
                at: performance.now()
              });
            } else if (frames >= 300) {
              resolve({ ready: false, reason: 'layout-not-stable', frames: frames });
            } else {
              sample();
            }
          });
        }
        sample();
      });
    });
  }

  function reportWorkspaceReady(eventKind) {
    var summary = workspaceSummary();
    // Primary UMAP delivery starts the supplement request, but is not yet the
    // linked layout. Keep its pixels concealed without interrupting rendering.
    if (!summary.complete) setLinkedPresentationReady(false);
    var token = ++linkedReadyPaintToken;
    if (!summary.primaryReady) {
      dispatchWorkspaceReady(summary, false, eventKind);
      return;
    }
    var paintedData = D;
    var paintedFingerprint = summary.datasetFingerprint;
    // Two animation frames guarantee that the synchronous Canvas/WebGL draw
    // has crossed a browser paint boundary before the shell fades its loader.
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        if (token !== linkedReadyPaintToken || D !== paintedData) return;
        var settled = workspaceSummary();
        if (settled.datasetFingerprint !== paintedFingerprint) return;
        setLinkedPresentationReady(settled.complete);
        dispatchWorkspaceReady(settled, linkedWorkspacePainted(), eventKind);
      });
    });
  }

  window.cerebroLinkedViewsState = Object.freeze({
    primaryReady: function () { return workspaceSummary().primaryReady; },
    ready: function () { return workspaceSummary().ready; },
    visualReady: linkedVisualReady,
    capture: exportWorkspace,
    apply: applyWorkspace,
    summary: workspaceSummary,
    downloadPNG: downloadWorkspacePNG
  });

  window.cerebroCellViews = Object.freeze({
    summary: singleSummary,
    captureState: captureSingleState,
    applyState: applySingleState,
    downloadPNG: downloadSinglePNG,
    attachDatasetIdentity: attachSingleDatasetIdentity
  });

  // ---- boot ----------------------------------------------------------------
  // All controls are client-owned (cv- ids), so no shiny:inputchanged is needed.
  // The only server touchpoints: receive the bundle, and signal readiness once
  // the session is actually connected (setInputValue is not callable before).
  var booted = false;
  function boot() {
    if (booted) return true;
    if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) return false;
    booted = true;
    var tooltipControl = $('hover_info_in_projections_checkbox');
    if (tooltipControl) cellTooltipsOn = tooltipControl.checked;
    Shiny.addCustomMessageHandler('cerebro_cell_tooltips', function (message) {
      if (message) setCellTooltips(!!message.enabled);
    });
    document.addEventListener('change', function (event) {
      if (event.target.id === 'hover_info_in_projections_checkbox') setCellTooltips(event.target.checked);
    });
    Shiny.addCustomMessageHandler('cerebro_scatter_defaults', function (message) {
      if (!message || !sameDatasetContext(message.dataset_context, currentDatasetContext())) return;
      sharedScatter = message;
      Object.keys(scatterControlFields).forEach(function (id) {
        setLinkedSliderValue(id, message.values[scatterControlFields[id]]);
      });
      syncSharedScatter();
      if (D && (!singleActive || singleClientPercentage())) { applyActiveChange(); }
      else if (D && singleActive !== 'spatial_projection') { drawAll(); }
    });
    // Input bindings also emit on mount and on server updates. Accept ordinary
    // slider changes only after pointer/keyboard interaction with that control.
    function markScatterInteraction(event) {
      var container = event.target && event.target.closest &&
        event.target.closest('.shiny-input-container');
      var control = container && container.querySelector('input');
      if (control && scatterControlFields[control.id]) scatterUserEdit = {
        id: control.id, context: currentDatasetContext(), time: Date.now()
      };
    }
    document.addEventListener('pointerdown', markScatterInteraction, true);
    document.addEventListener('keydown', markScatterInteraction, true);
    window.jQuery(document).on('shiny:inputchanged.cerebroScatter', function (event) {
      var edit = scatterUserEdit, field = scatterControlFields[event.name];
      if (!edit || !field || event.name.indexOf('cv-') === 0 || edit.id !== event.name ||
          Date.now() - edit.time > 30000 || !sameDatasetContext(edit.context, currentDatasetContext()) ||
          (event.el && event.el._cvSyncing)) return;
      var values = sharedScatterValues();
      if (values && Number(event.value) === values[field]) return;
      publishScatterEdit(field, Number(event.value));
    });
    // Newly rendered hidden drawers receive the latest values without causing
    // a second user edit or replacing their DOM on every slider movement.
    window.jQuery(document).on('shiny:bound.cerebroScatter', function (event) {
      var values = sharedScatterValues(), field = scatterControlFields[event.target.id];
      if (values && field) setLinkedSliderValue(event.target.id, values[field]);
    });
    Shiny.addCustomMessageHandler('cerebro_gpu_acceleration', function (message) {
      if (message) setGpuAcceleration(message.enabled);
    });
    document.addEventListener('change', function (event) {
      if (event.target && event.target.id === 'webgl_checkbox') {
        setGpuAcceleration(event.target.checked);
      }
    });
    var gpuPreference = document.getElementById('webgl_checkbox');
    if (gpuPreference) setGpuAcceleration(gpuPreference.checked);
    Shiny.addCustomMessageHandler('coordviews_data', onData);
    Shiny.addCustomMessageHandler('coordviews_binary', onBinaryData);
    Shiny.addCustomMessageHandler('coordviews_attribute', onBinaryAttribute);
    Shiny.addCustomMessageHandler('coordviews_supplement', onBinarySupplement);
    Shiny.addCustomMessageHandler('coordviews_clone_details', function (message) {
      if (!D || !D.clone || !message || !datasetMessageFresh(message) ||
          message.dataset_fingerprint !== configFingerprint() ||
          !Array.isArray(message.ids) || !Array.isArray(message.labels)) return;
      if (!Array.isArray(D.clone.label)) D.clone.label = [];
      if (!Array.isArray(D.clone.n_cdr3)) D.clone.n_cdr3 = [];
      message.ids.forEach(function (raw, index) {
        var id = Number(raw);
        if (!Number.isInteger(id) || id < 0 || id >= D.clone.n_clones) return;
        D.clone.label[id] = message.labels[index];
        D.clone.n_cdr3[id] = Number(message.n_cdr3[index]) || 1;
        pendingCloneDetails.delete(id);
      });
      renderReadout();
      if (cardOpen()) renderCard();
    });
    Shiny.addCustomMessageHandler('coordviews_asset', function (message) {
      if (!D || !message || !datasetMessageFresh(message) ||
          message.dataset_fingerprint !== configFingerprint() ||
          !message.key || !message.uri) return;
      var cacheKey = deferredAssetCacheKey(message.key);
      pendingAssets.delete(cacheKey);
      assetCache[cacheKey] = message.uri;
      applyDeferredAsset(message.key, message.uri);
    });
    Shiny.addCustomMessageHandler('cell_view_binary', onSingleBinary);
    Shiny.addCustomMessageHandler('cell_view_recolor', recolorSingle);
    Shiny.addCustomMessageHandler('cell_view_recolor_binary', function(buffer) {
      recolorSingle(window.CBViewWire.unpack(buffer));
    });
    Shiny.addCustomMessageHandler('cell_view_aux_binary', onSingleAuxBinary);
    Shiny.addCustomMessageHandler(
      'cell_view_resource_catalog',
      function (message) {
        if (!datasetMessageFresh(message) || !Array.isArray(message.resources)) return;
        if (typeof message.default_resource_name === 'string' &&
            message.default_resource_name) {
          singleDefaultResourceNames.set(
            message.id, message.default_resource_name
          );
        } else {
          singleDefaultResourceNames.delete(message.id);
        }
        message.resources.forEach(function (resource) {
          registerSingleGeometryResource({
            id: message.id,
            resource: resource,
            dataset_identity: message.dataset_identity
          });
        });
        (message.category_resources || []).forEach(function (resource) {
          registerSingleCategoryResource({
            id: message.id,
            resource: resource,
            dataset_identity: message.dataset_identity
          });
        });
        // The first click can precede this catalog because both the dynamic
        // controls and their resource descriptors arrive in the same Shiny
        // flush. Retry as soon as the catalog is registered so the immutable
        // geometry download overlaps the remaining server preparation.
        if (singleRequests.has(singleRequestKey(message.id, message.dataset_context))) {
          prefetchRegisteredSingleResource(message.id);
          prefetchRegisteredSingleCategory(message.id);
        }
      }
    );
    Shiny.addCustomMessageHandler('coordviews_colors', function (patch) {
      if (!datasetMessageFresh(patch)) return;
      if (!applyColorPatch(patch)) pendingColorPatch = patch;
    });
    Shiny.addCustomMessageHandler('cell_view_render', function (message) {
      if (!message || !message.id || !datasetMessageFresh(message)) return;
      message = reuseSharedSingleProjection(message);
      if (!message) return;
      renderSingle(message);
    });
    Shiny.addCustomMessageHandler('cell_view_background', function (message) {
      updateSingleBackground(message);
    });
    Shiny.addCustomMessageHandler('cell_view_appearance', function (message) {
      if (!message || !message.id || !message.values || !datasetMessageFresh(message)) return;
      var view = singleView(message.id, message.dataset_context);
      var identity = view && view.datasetIdentity || {};
      if (!view || String(identity.cell_fingerprint || '') !==
          String(message.dataset_fingerprint || '')) return;
      view.meta = view.meta || {};
      view.meta.appearance = Object.assign(
        {},
        view.meta.appearance || {},
        message.values
      );
      view.appearanceOverrides = Object.assign({}, view.appearanceOverrides || {}, message.values);
      ['point_size', 'point_opacity'].forEach(function (key) {
        if (Number.isFinite(message.values[key])) {
          view.data[key] = message.values[key];
          view.pointAppearance = view.pointAppearance || {};
          view.pointAppearance[key] = message.values[key];
        }
      });
      if (singleActive !== message.id) return;
      if (Number.isFinite(message.values.point_size)) {
        ps = Math.max(1, Math.min(20, message.values.point_size)); pointSizeEdited = true;
      }
      if (Number.isFinite(message.values.point_opacity)) {
        pointOpacity = Math.max(0, Math.min(1, message.values.point_opacity)); pointOpacityEdited = true;
      }
      if (Object.prototype.hasOwnProperty.call(message.values, 'group_labels')) {
        labelsOn = message.values.group_labels === true;
      }
      if (Object.prototype.hasOwnProperty.call(message.values, 'draw_border')) {
        bordersOn = !!message.values.draw_border;
      }
      if (Object.prototype.hasOwnProperty.call(message.values, 'keep_square')) {
        keepPlotsSquare = !!message.values.keep_square;
      }
      if (Object.prototype.hasOwnProperty.call(message.values, 'keep_square')) resizeAll();
      drawAll();
      window.dispatchEvent(new CustomEvent('cerebro:view-appearance', { detail: {
        id: message.id, values: message.values, dataset_context: message.dataset_context
      } }));
    });

    // Single-gene expression vector (0-255) for the current gene.
    // A reply is only for the gene still being asked about. Two things used to
    // go wrong here. Between asking and answering the picker already showed the
    // new gene while the points and the colourbar still showed the OLD one --
    // the workspace naming one gene and drawing another. And a reply with
    // ok = FALSE was dropped on the floor, so a gene the server could not
    // provide left the previous one's colours on screen under its name, with
    // nothing to say the request had failed.
    Shiny.addCustomMessageHandler('coordviews_geneval', function (m) {
      if (!D || !m || !datasetMessageFresh(m) ||
          !sameDatasetContext(m.dataset_context, D.dataset_context)) return;
      if (m.ok && geneWanted != null && m.gene !== geneWanted) return; // stale reply
      geneWanted = null;
      if (!m.ok) {
        // Nothing to draw. Say so rather than keep showing the last gene.
        window.CBViewState.clearExpression(D, 'gene');
        clipRange();
        if (colorBy === GENE_MODE) {
          renderColorbar(true, null, esc(m.gene) + ' — not available');
          drawAll(); updateMoranBadges();
        }
        return;
      }
      D.gene = { gene: m.gene, v: m.v, max: m.max };
      // A new gene is a new distribution, so the trimmed range has to be
      // recomputed before anything reads a colour from it.
      clipRange();
      if (colorBy === GENE_MODE) { renderLegend(); drawAll(); updateMoranBadges(); }
    });
    Shiny.addCustomMessageHandler('coordviews_genepanels', function (m) {
      if (!D || !m || !datasetMessageFresh(m) ||
          !sameDatasetContext(m.dataset_context, D.dataset_context)) return;
      if (!m.ok) {
        window.CBViewState.clearExpression(D, 'panels');
        if (colorBy === GENE_PANELS_MODE) {
          buildPanels(); layoutPanels(); renderLegend(); resizeAll(); drawAll();
        }
        return;
      }
      var genes = Array.isArray(m.genes) ? m.genes : [m.genes];
      var values = m.values || [];
      if (genes.length === 1 && (!values.length || typeof values[0] === 'number')) {
        values = [values];
      }
      D.genePanelMax = Number(m.max) || 1;
      D.genePanels = genes.map(function (gene, index) {
        return { gene: gene, v: values[index] || [] };
      });
      if (colorBy !== GENE_PANELS_MODE) return;
      var limit = Math.max(1, Math.floor(12 / genePanelBaseCount));
      var picker = $('coordviews_gene');
      if (picker && picker.selectize && picker.selectize.items.length > limit) {
        picker.selectize.setValue(picker.selectize.items.slice(0, limit));
        return;
      }
      var order = orderedSpaces();
      ensurePanelSlots(Math.min(12, order.length));
      buildPanels();
      var current = panels.filter(function (panel) { return !!panel.spaceId; });
      var sameOrder = current.length === order.length && current.every(
        function (panel, index) { return panel.spaceId === order[index]; }
      );
      if (!sameOrder) {
        layoutPanels();
      } else {
        current.forEach(function (panel) {
          var space = spaceById[panel.spaceId];
          panel.colorBy = space && space._panelColorMode || null;
          var title = $('cv-title-' + panel.key.toLowerCase());
          if (title) title.textContent = space ? space.label : panel.spaceId;
        });
      }
      renderLegend(); resizeAll(); drawAll();
    });
    // The exact meta row behind the open detail card. Ignored if the card has
    // since moved on to another cell (or closed) — a slow reply must not
    // repaint a card that is now describing something else.
    Shiny.addCustomMessageHandler('coordviews_cell_meta', receiveCardMetadata);
    // Three 0-255 channels for RGB co-expression.
    Shiny.addCustomMessageHandler('coordviews_rgbval', function (m) {
      if (!D || !m || !datasetMessageFresh(m) ||
          !sameDatasetContext(m.dataset_context, D.dataset_context)) return;
      if (!m.ok) {
        window.CBViewState.clearExpression(D, 'rgb');
        if (colorBy === RGB_MODE) { renderLegend(); drawAll(); }
        return;
      }
      D.rgb = { r: m.r, g: m.g, b: m.b, genes: m.genes };
      if (colorBy === RGB_MODE) { renderLegend(); drawAll(); }
    });
    var genePanelColor = $('cv-gene-panel-color');
    if (genePanelColor) genePanelColor.onchange = function () {
      if (colorBy !== GENE_PANELS_MODE) return;
      layoutPanels(); renderLegend(); drawAll();
    };
    // The first panel is persistent across data sets. Initialise its GPU
    // renderer while the app is becoming interactive so the first Linked views
    // click does not also pay for context creation and shader compilation.
    ensurePanelSlots(1);
    buildPanels();

    // Report whether the workspace is on screen -- both ways, and not just the
    // first time. Building the bundle walks every cell of the loaded object and
    // this is one tab of eighteen, so a session that never opens it should pay
    // nothing; while hidden, both full data and palette patches are held back.
    //
    // The condition is "the workspace has a layout box", not the sidebar's
    // active-tab input: that reports shinydashboard's idea of which tab is
    // selected, which a programmatic tab switch leaves untouched even though the
    // panels are plainly visible. Size is true however the tab was opened.
    var lastVis = null;
    if (window.jQuery) {
      window.jQuery(document).on(
        'shiny:inputchanged.cerebroDatasetSwitch',
        function (event) {
          if (event.name !== 'crb_file_selector') return;
          resetSingleViews();
          lastVis = null;
        }
      );
    }
    window.addEventListener('cerebro:dataset-context', function (event) {
      var detail = event && event.detail || {};
      if (!detail.changed && detail.phase === 'ready' && !detail.phaseChanged) return;
      if (detail.changed || detail.phase !== 'ready') invalidateDatasetViewState(detail);
      else markSingleHosts('waiting', detail.dataset_context);
      lastVis = null;
      reportVisibility();
    });
    function requestSingleView(id) {
      var context = currentDatasetContext();
      if (!context || !datasetContextApi() || datasetContextApi().phase() !== 'ready') return false;
      var requestKey = singleRequestKey(id, context);
      if (!id || singleView(id) || singleRequests.has(requestKey) ||
          !Shiny.setInputValue) return false;
      singleRequests.add(requestKey);
      var requestedAt = performance.now();
      var timing = CBViewState.specialistLifecycle.begin(
        singleTiming, id, true
      );
      timing.requestAtMs = requestedAt;
      timing.clickToRequestMs = isFinite(window.__cerebroPageBenchClickStart)
        ? requestedAt - window.__cerebroPageBenchClickStart : null;
      prefetchRegisteredSingleResource(id);
      prefetchRegisteredSingleCategory(id);
      Shiny.setInputValue(id + '_render_request', {nonce:Date.now(), dataset_context:context}, {
        priority: 'event'
      });
      return true;
    }
    function tabSingleViewId(link) {
      var href = link && link.getAttribute('href');
      if (!href || href.slice(0, 11) !== '#shiny-tab-') return null;
      var pane = document.getElementById(href.slice(1));
      var host = pane && pane.querySelector(
        '.cerebro-cell-view-host[data-cell-view-id]'
      );
      return host && host.dataset.cellViewId || null;
    }
    function reportVisibility() {
      reportRendererStatus();
      var el = $('cv-meta');
      var linkedVis = !!(el && el.offsetParent !== null);
      var singleId = visibleSingleId();
      var vis = linkedVis;
      var key = linkedVis ? 'linked' : (singleId || 'hidden');
      var host = singleId && singleHost(singleId);
      var surface = host && host.querySelector('.cerebro-cell-view-surface');
      var mounted = !singleId || !!(surfaceHome && surface &&
        surface.contains(surfaceHome.panes));
      if (key === lastVis && mounted) return;
      if (key !== lastVis) { clearTransientHover(); unpinTip(); closeCard(); }
      lastVis = key;
      requestSingleView(singleId);
      if (singleId && singleView(singleId)) {
        var cachedAt = performance.now();
        var cachedTiming = CBViewState.specialistLifecycle.begin(
          singleTiming, singleId, false
        );
        cachedTiming.activationRequestAtMs = cachedAt;
        cachedTiming.clickToRequestMs = isFinite(window.__cerebroPageBenchClickStart)
          ? cachedAt - window.__cerebroPageBenchClickStart : null;
        activateSingle(singleId, false, false, 'cached');
      } else if (linkedVis && linkedBundle && (singleActive || D !== linkedBundle)) {
        // A non-canvas tab clears singleActive but leaves the specialist data
        // mounted in D. Restore the Linked bundle on return through those tabs
        // too, before its projection controls operate on the shared renderer.
        activateLinked();
      } else if (linkedVis && !linkedBundle) {
        stashSingleState();
        showUnavailable('Loading linked views…');
        restoreLinkedSurface();
        singleActive = null; singleSpaceIds = []; singleSpaceModes = {};
        setLinkedPresentationReady(false);
      } else if (singleActive) {
        stashSingleState();
        restoreLinkedSurface();
        singleActive = null; singleSpaceIds = []; singleSpaceModes = {};
        if (linkedVis) showUnavailable('Loading linked views…');
      }
      if (linkedVis && !singleActive && workspaceSummary().primaryReady) {
        reportWorkspaceReady('cached');
      }
      if (Shiny.setInputValue) {
        if (vis && !isFinite(linkedRequestTiming.requestAtMs)) {
          linkedRequestTiming.requestAtMs = performance.now();
          linkedRequestTiming.clickToRequestMs =
            isFinite(window.__cerebroPageBenchClickStart)
              ? linkedRequestTiming.requestAtMs -
                window.__cerebroPageBenchClickStart : null;
        }
        Shiny.setInputValue('coordviews_visible', vis);
      }
    }
    setInterval(reportVisibility, 250);
    reportVisibility();
    // The poll is the backstop; a tab switch is a click, so report on the way
    // out too. Without this the server can still believe the workspace is on
    // screen for up to one interval after the user has left it.
    document.addEventListener('click', function (event) {
      var target = event.target && event.target.closest
        ? event.target.closest('a[href="#shiny-tab-coordinated_views"]') : null;
      var tabLink = event.target && event.target.closest
        ? event.target.closest('a[href^="#shiny-tab-"]') : null;
      if (target && Shiny.setInputValue) {
        linkedRequestTiming.requestAtMs = performance.now();
        linkedRequestTiming.clickToRequestMs =
          isFinite(window.__cerebroPageBenchClickStart)
            ? linkedRequestTiming.requestAtMs -
              window.__cerebroPageBenchClickStart : null;
        Shiny.setInputValue('coordviews_visible', true, { priority: 'event' });
      }
      setTimeout(function () {
        requestSingleView(tabSingleViewId(tabLink));
        reportVisibility();
      }, 0);
    }, true);
    // Re-advertise client capabilities after either a resumed connection or a
    // fresh server session. Keep an already-confirmed context usable here: a
    // resumed session need not replay its ready message, while a fresh session
    // will immediately publish pending/ready with its new epoch and invalidate
    // this state through the authoritative context gate.
    var onConnected = function () {
      progressiveRequestedKey = null;
      Shiny.setInputValue(
        'coordviews_wire_supported',
        !!(window.CBViewWire && window.CBViewWire.supported),
        { priority: 'event' }
      );
      lastVis = null;
      reportVisibility();
    };
    var jq = window.jQuery;
    if (jq) { jq(document).on('shiny:connected', onConnected); }
    else { document.addEventListener('shiny:connected', onConnected); }

    // The alignment controls are server-rendered after the client bundle. When
    // Shiny replaces that small fragment, populate its section tabs again; the
    // observer watches only the host itself, so painting tab buttons cannot
    // trigger a render loop.
    var imageHost = $('coordviews_image_ui');
    if (imageHost && window.MutationObserver) {
      new MutationObserver(function () {
        renderImagePicker();
        syncAllImgRangeValues();
      })
        .observe(imageHost, { childList: true });
    }

    // clear button + point-size slider live in the top bar (client-owned)
    document.addEventListener('click', function (e) {
      var t = e.target;
      if (t && t.closest && t.closest('#cv-more-btn, #trekker_more_button')) {
        setTimeout(syncPointControls, 0);
      }
      if (t && t.closest && t.closest('#cv-tk-insights-toggle')) {
        var tkToggle = $('cv-tk-insights-toggle');
        setTrekkerInsightsOpen(
          !tkToggle || tkToggle.getAttribute('aria-expanded') !== 'true'
        );
        return;
      }
      var tkTab = t && t.closest && t.closest('.cv-tk-tab[data-tk-tab]');
      if (tkTab) {
        selectTrekkerInsight(tkTab.getAttribute('data-tk-tab'));
        return;
      }
      var evidenceThumb = t && t.closest && t.closest('.cv-evidence-thumb');
      if (evidenceThumb) {
        var image = evidenceThumb.querySelector('img');
        var dlg = $('cv-evidence-modal'), modalImage = $('cv-evidence-modal-img');
        var modalCell = $('cv-evidence-modal-cell');
        if (dlg && modalImage && image && dlg.showModal) {
          modalImage.src = image.src;
          if (modalCell) modalCell.textContent = evidenceThumb.dataset.cell || '';
          dlg.showModal();
        }
        return;
      }
      var moranBadge = t && t.closest && t.closest('.cv-moran-badge');
      if (moranBadge) { openMoranDialog(moranBadge); return; }
      if (t && t.closest && t.closest('#cv-workspace-overview')) {
        if (focusPanel) setFocusPanel(focusPanel);
        return;
      }
      // The pinned tooltip's two actions. Checked before anything else, because
      // the tooltip sits over a panel and the handlers below would otherwise
      // read the click as one on the workspace underneath.
      if (t && t.id === 'cv-img-reset') { resetImgToPreset(); return; }
      if (t && t.closest && t.closest('.cv-tip-details')) {
        if (pinnedTip.panel && pinnedTip.cell != null) {
          openCard(pinnedTip.panel, pinnedTip.cell);
        }
        return;
      }
      if (t && t.closest && t.closest('.cv-tip-close')) {
        // Dismisses the tooltip ONLY. The pick stays -- its ring, and on a
        // Trekker data set the niche readout it drives, are the reason the cell
        // was clicked; putting the tooltip away is not a reason to give them up.
        // Clicking the cell again is what drops the pick.
        unpinTip();
        return;
      }
      var singleAction = t && t.closest &&
        t.closest('[data-cell-view-action][data-cell-view-id]');
      if (singleAction) {
        e.preventDefault();
        var singleId = singleAction.getAttribute('data-cell-view-id');
        var singleAct = singleAction.getAttribute('data-cell-view-action');
        if (singleAct === 'clear') clearSingleSelection(singleId);
        if (singleAct === 'focus') toggleSelectionFocus(singleId);
        return;
      }
      // per-panel modebar: select mode (box/lasso), zoom in/out, reset, PNG
      var tb = t && t.closest && t.closest('.cv-tbtn');
      if (tb) {
        var act = tb.getAttribute('data-act'), key = tb.getAttribute('data-panel');
        if (interactionIsStatic() && act !== 'png') return;
        var pp = null;
        panels.forEach(function (p) { if (p.key === key) pp = p; });
        if (act === 'box' || act === 'lasso' || act === 'pan' || act === 'orbit') {
          selectMode = act; syncModeButtons(); return;
        }
        if (act === 'trekker-info') { openTrekkerInsights('qc', true); return; }
        if (act === 'clear') {
          pick = null; unpinTip(); closeCard(); clearLassos(); setSelection(null);
          return;
        }
        if (pp) {
          if (act === 'png') {
            window.dispatchEvent(new CustomEvent('cerebro:png-result', {
              detail: { ok: downloadPanelPNG(pp) }
            }));
          }
          else if (act === 'zin') { zoomStep(pp, 0.8); }
          else if (act === 'zout') { zoomStep(pp, 1.25); }
          else if (act === 'focus') { setFocusPanel(pp.key); }
          else if (act === 'zsel') { zoomToSelection(pp); }
          else if (act === 'reset') {
            // rotation is part of "where you are looking" too
            if (pp.rot) { pp.rot = null; pp.miniBg = null; project(pp); }
            if (pp.view) { pp.view = null; project(pp); }
            setSelectionZoomed(false);
            drawAll();
          }
        }
        return;
      }
      if (t && t.closest && t.closest('#cv-card-x')) {
        closeCard();
        return;
      }
      if (t && t.closest && t.closest('#cv-clear')) {
        if (selectionZoomed) resetZoom();
        pick = null; unpinTip(); closeCard(); clearLassos(); setSelection(null);
        return;
      }
      if (t && t.closest && t.closest('#cv-zsel')) {
        toggleSelectionFocus();
        return;
      }
      // clonal-layout segmented toggle: recompute the clone space + reproject
      var seg = t && t.closest && t.closest('#cv-clone-layout .cv-seg-btn');
      if (seg) {
        var mode = seg.getAttribute('data-mode');
        if (mode === CLONE_MODE) return;
        setSegOn(mode); applyCloneLayout(mode);
        resetSpaceViews('clone');   // the layout moved every cell in this space
        panels.forEach(function (p) { if (p.spaceId === 'clone') project(p); });
        drawAll();
        return;
      }
    });
    function updateTrekkerTransition(e) {
      var id = e.target && e.target.id;
      if (id !== 'trekker_morph' || singleActive !== 'trekker_projection') return;
      var transition = Math.max(0, Math.min(1, Number(e.target.value) || 0));
      singleSpaceIds.forEach(function (spaceId) {
        var space = spaceById[spaceId], source = space && space._transition;
        if (!source) return;
        space.x = source.fromX.map(function (value, index) {
          return value == null || source.toX[index] == null ? null
            : value + (source.toX[index] - value) * transition;
        });
        space.y = source.fromY.map(function (value, index) {
          return value == null || source.toY[index] == null ? null
            : value + (source.toY[index] - value) * transition;
        });
        space._unit = transitionUnit(source.fromUnit, source.toUnit, transition);
        panels.forEach(function (panel) {
          if (panel.spaceId === spaceId) project(panel);
        });
      });
      drawAll();
    }
    function updateLinkedSlider(target) {
      if (!target || target._cvSyncing) return;
      var id = target.id, value = Number(target.value);
      if (!isFinite(value)) return;
      if (scatterControlFields[id]) publishScatterEdit(scatterControlFields[id], value);
      if (id === 'cv-ps') {
        psSeeded = true;
        pointSizeEdited = true;
        ps = value;
        drawAll();
      } else if (id === 'cv-opacity') {
        pointOpacityEdited = true;
        pointOpacity = value;
        drawAll();
      } else if (id === 'cv-pct') {
        pctShow = value;
        rebuildPctMask(); applyActiveChange();
      } else if (id === 'cv-dissolve') {
        dissolvePct = value;
        rebuildDissolve(); applyActiveChange();
      } else if (id === 'cv-niche') {
        nicheRadius = value;
        rebuildNiche();
        renderSelbar();
        drawAll();
        if (!sel) renderReadout();
      }
    }
    function updateLinkedToggle(target) {
      var id = target && target.id;
      if (id === 'cv-hover') {
        setLinkedHover(target.checked);
        return true;
      }
      if (id === 'cv-backgrounds') {
        setLinkedBackgrounds(target.checked);
        return true;
      }
      if (id === 'cv-labels') {
        var nextLabels = !!target.checked;
        if (labelsOn === nextLabels) return true;
        labelsOn = nextLabels;
        drawAll();
        return true;
      }
      if (id === 'cv-borders') {
        var nextBorders = !!target.checked;
        if (bordersOn === nextBorders) return true;
        bordersOn = nextBorders;
        drawAll();
        return true;
      }
      if (id === 'cv-square-plots') {
        var nextSquare = !!target.checked;
        if (keepPlotsSquare === nextSquare) return true;
        keepPlotsSquare = nextSquare;
        _layoutKey = null;
        resizeAll();
        return true;
      }
      return false;
    }
    window.jQuery(document)
      .off(
        'input.cvTrekkerTransition change.cvTrekkerTransition',
        '#trekker_morph'
      )
      .on(
        'input.cvTrekkerTransition change.cvTrekkerTransition',
        '#trekker_morph',
        function () {
          updateTrekkerTransition({
            target: { id: this.id, value: this.value }
          });
        }
      );
    window.jQuery(document)
      .off(
        'input.cvLinkedSettings',
        '#cv-ps, #cv-opacity, #cv-pct, #cv-dissolve, #cv-niche'
      )
      .on(
        'input.cvLinkedSettings',
        '#cv-ps, #cv-opacity, #cv-pct, #cv-dissolve, #cv-niche',
        function () { updateLinkedSlider(this); }
      );
    document.addEventListener('input', function (e) {
      var id = e.target && e.target.id;
      if (updateLinkedToggle(e.target)) return;
      if (id && id.slice(-7) === '-number' && id.indexOf('cv-img-') === 0) {
        var range = $(id.slice(0, -7));
        if (syncImgRangeValue(range, e.target.value)) {
          syncImgControls(range.id); drawAll();
        }
        return;
      }
      if (id && id.indexOf('cv-img-') === 0) {
        if (e.target.type === 'range') syncImgRangeValue(e.target, e.target.value);
        syncImgControls(id); drawAll();
      }
    });
    document.addEventListener('change', function (e) {
      var id = e.target && e.target.id;
      if (id && id.slice(-7) === '-number' && id.indexOf('cv-img-') === 0) {
        var range = $(id.slice(0, -7));
        if (!syncImgRangeValue(range, e.target.value)) {
          syncImgRangeValue(range, range && range.value);
        }
        if (range) { syncImgControls(range.id); drawAll(); }
        return;
      }
      if (id && id.indexOf('cv-img-') === 0) { syncImgControls(id); drawAll(); return; }
      // The gene picker is read by the server directly, so without this the
      // client would not know a request was in flight and would keep drawing the
      // previous gene under the new gene's name until the reply landed.
      if (id === 'coordviews_gene') {
        var g = e.target.value;
        if (g) { requestGene(g); drawAll(); }
        return;
      }
      if (id === 'cv-clip') {
        colorClip = parseFloat(e.target.value) || 0;
        clipRange();               // recompute before anything reads the colours
        renderLegend(); drawAll();
        return;
      }
      if (id === 'cv-evidence') { evidenceOn = e.target.checked; drawAll(); return; }
      if (updateLinkedToggle(e.target)) return;
      // a group-filter level checkbox toggled
      var fwrap = e.target && e.target.closest &&
        e.target.closest('#cv-filters-row .cv-filt');
      if (fwrap && e.target.matches && e.target.matches('.cv-filt-menu input[type=checkbox]')) {
        readFilter(fwrap);
      }
    });
    // Escape closes the detail card. The shared drawer listener handles the
    // drawer and its group-filter menus.
    document.addEventListener('keydown', function (e) {
      if (e.key !== 'Escape') return;
      if (cardOpen()) closeCard();
      else if (pinnedTip.panel) unpinTip();
    });
    window.addEventListener('resize', function () {
      if (D) resizeAll();
      // the grid just changed shape; an open card has to be re-centred on it
      if (cardOpen()) centreCard();
    });
    return true;
  }

  function bootWhenReady() {
    if (!boot()) window.setTimeout(bootWhenReady, 50);
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', bootWhenReady);
  } else { bootWhenReady(); }
})();
