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

  var D = null;                 // the data bundle
  var linkedBundle = null;      // cached while a dedicated page owns the canvas
  var singleViews = {};         // latest specialist payload per plot id
  var singleRequests = new Set(); // visible hosts awaiting their first payload
  var singleActive = null;      // plot id currently using the shared surface
  var singleIndexCells = null;  // cells array behind the cached barcode index
  var singleIndexMap = null;
  var singleSpaceIds = [];      // one space, or one per gene in multi-panel mode
  var singleSpaceModes = {};    // space id -> categorical/continuous colour mode
  var surfaceHome = null;       // original Linked Views panel/legend locations
  var linkedState = null;       // Linked workspace state while a single page owns the surface
  var pendingColorPatch = null; // palette received before its dataset bundle
  var panels = [];              // [{key, canvas, ctx, spaceId, W, H, sx, sy, lasso, drag, moved}]
  var sel = null;               // Set of selected cell indices (null = none)
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
  // Guards the async decode: a fast switch could have an earlier image finish
  // loading after a later one and paint itself over the current choice.
  var imgToken = 0;
  // The visible spatial sections are independent spaces. Selection, colour and
  // filters are global; image choice/alignment stay on their own section. One of
  // them is active so the single alignment bar has an unambiguous target.
  var selectedSpatial = [];
  var activeSpatialId = null;
  // A background choice belongs to one spatial section. Auto/None/Custom must
  // not leak from donor A into donor C merely because they are shown together.
  var backgroundModes = {};
  var backgroundScopePulse = false;
  var spatialTemplate = null;
  // Panel key currently promoted as the primary lens, or null for equal overview.
  // Context lenses stay present and coordinated while the primary grows.
  var focusPanel = null;
  var zoomed = false;           // did the cohort Zoom action change a panel view?
  var zoomedSpace = null;       // space controlled by that action
  var selectMode = 'lasso';     // drag-select mode: 'lasso' (freeform) or 'box'
  // Trekker controls brought into Linked views (only when D.trekker is present):
  var dissolvePct = 0;          // % of least-confident nuclei to dissolve
  var dissolveThresh = null;    // conf value below which cells are hidden
  var evidenceOn = false;       // ring nuclei that carry positioning evidence
  var nicheRadius = 250;        // µm radius for the picked-nucleus niche readout
  var nicheSet = null;          // cell indices inside the picked nucleus's niche
  var colorBy = null;           // group name
  var ps = 3.0;                 // point size
  var pointOpacity = 0.8;       // base draw opacity (no-selection view)
  var keepPlotsSquare = false;  // fluid rectangles use the workspace by default
  // Builder alignment appearance belongs to one physical space. It seeds that
  // space until the user deliberately moves the shared workspace control; from
  // that point the user's value is the global linked-view override.
  var pointSizeEdited = false;
  var pointOpacityEdited = false;
  var hidden = new Set();       // hidden level indices for the active group
  // Every selected embedding is an independent panel. Colour/filter/selection
  // state remains global, while viewport and 3-D rotation live on the panel.
  var selectedProjections = [];
  var pctShow = 100;            // % of cells to render
  var pctMask = null;           // Uint8Array subsample mask, or null (all shown)
  var groupFilter = {};         // groupName -> Set(allowed level idx); absent = all
  var spaceById = {};
  var resizeObserver = null;    // fires when the tab becomes visible / resizes
  var resizeTimer = null;
  var focusResizeTimer = null;
  var focusAnimating = false;
  // Each spatial instance owns `_imgEl`, `_imgReady`, `_imgState` and its image
  // identity. The alignment bar edits only activeSpatialId.

  // Categorical fallback palette (mirrors the app).
  var PAL = ['#636EFA', '#EF553B', '#00CC96', '#AB63FA', '#FFA15A', '#19D3F3',
    '#FF6692', '#B6E880', '#FF97FF', '#FECB52', '#2f6fd6', '#f97316',
    '#16a34a', '#9a5cd0', '#e05780', '#38b2ac', '#d97706', '#7bb0e8'];

  function cssEscape(value) {
    if (window.CSS && typeof window.CSS.escape === 'function') {
      return window.CSS.escape(String(value));
    }
    return String(value).replace(/[^a-zA-Z0-9_-]/g, '\\$&');
  }

  // RGB co-expression: cells whose max channel is <= RGB_MIN form a light-grey
  // substrate; the rest blend FROM that grey toward their full-brightness hue by
  // intensity (max/255), so weak co-expression stays faint (≈grey) and only
  // strong expression is vivid — additive RGB otherwise renders low signal as
  // near-black on white. One grey source, shared by the colour and the layering.
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
  // and only then made editable in Color management. Escaping is the wrong tool
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
  // Human-facing label for a group key. Metadata columns keep their own names;
  // the server-synthesised expansion group gets a readable label.
  function groupLabel(g) { return g === 'clone_expansion' ? 'Clone Expansion' : g; }

  // ---- colouring sources ---------------------------------------------------
  // Three of them, mirroring how the Projection tab splits its two boxes:
  //   D.groups    registered grouping variables — colour AND group filters
  //   D.cat_extra other categorical meta columns — colour only (no filter)
  //   D.fields    numeric meta columns + Trekker's physical fields — continuous
  // Everything that reads a categorical colouring goes through catOf(), so the
  // two categorical sources never have to be special-cased at the call site.
  function catOf(name) {
    if (!D || !name) return null;
    return (D.groups && D.groups[name]) ||
      (D.cat_extra && D.cat_extra[name]) || null;
  }
  // The categorical variable the composition readout summarises by: cell_type
  // when present, else the active categorical colouring, else the first one
  // available. colorBy may be a gene/RGB/field pseudo-mode that is not
  // categorical at all, so it is used only when catOf() resolves it.
  function compGroupName() {
    if (!D) return null;
    if (D.groups && D.groups['cell_type']) return 'cell_type';
    if (catOf(colorBy)) return colorBy;
    var k = D.groups ? Object.keys(D.groups) : [];
    if (k.length) return k[0];
    var e = D.cat_extra ? Object.keys(D.cat_extra) : [];
    return e.length ? e[0] : null;
  }
  // True value behind a field's quantised code (fields travel 0..scale to keep
  // the bundle small; min/max carry the real range).
  function fieldValue(fld, i) {
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
  // Real geometry (umap, spatial) preserves aspect ratio. An abstract space
  // with independent axes (the clone panel) sets `stretch` so X and Y each
  // fill the box on their own — otherwise a wide clone-rank axis squashes the
  // expansion axis into a needle.
  function unitOf(space) {
    var xs = space.x, ys = space.y, zs = space.z || null, n = xs.length;
    var xr = space.xRange, yr = space.yRange;
    var fixedX = Array.isArray(xr) && xr.length === 2 &&
      isFinite(Number(xr[0])) && isFinite(Number(xr[1])) &&
      Number(xr[0]) !== Number(xr[1]);
    var fixedY = Array.isArray(yr) && yr.length === 2 &&
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
    if (space.stretch) {
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
    // (Aspect-preserving spaces only — kx === ky there; the image never uses a
    // stretched space.)
    //
    // `bx` is where the DATA actually lies inside the unit box. Aspect-preserving
    // spaces letterbox the shorter axis, so that is not [0,1] on both axes, and
    // clampView() needs the real extent to keep a panned view on the data.
    return { nx: nx, ny: ny, nz: nz, ok: ok,
      x0: x0, y0: y0, k: kx, ky: ky, ox: ox, oy: oy,
      bx: nz
        ? { x0: 0, x1: 1, y0: 0, y1: 1 }   // rotatable: the sphere fills the box
        : { x0: ox, x1: ox + dw * kx, y0: oy, y1: oy + dh * ky },
      occ: occ, sat: occSAT(occ),
      cmx: cnt ? cmx / cnt : 0.5, cmy: cnt ? cmy / cnt : 0.5 };
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
    var ny = (dy - u.y0) * u.k + u.oy;
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
  function paintOrder(p) {
    if (!D) return null;
    var mode = panelColorMode(p);
    var key = mode + '|' + (D.gene ? D.gene.gene : '');
    if (_ordD === D && _ordKey === key) return _ordVal;
    var vals = null;
    if (mode === GENE_MODE && D.gene) {
      vals = D.gene.v;
    } else {
      var f = fieldForMode(mode);
      if (f) vals = f.v;
    }
    var ord = null;
    if (vals) {
      ord = new Array(D.n);
      for (var i = 0; i < D.n; i++) ord[i] = i;
      // Missing values sort first: an unpositioned or NA cell has no signal to
      // show, so it must never end up covering one that has.
      ord.sort(function (a, b) {
        var va = vals[a], vb = vals[b];
        if (va == null || isNaN(va)) va = -Infinity;
        if (vb == null || isNaN(vb)) vb = -Infinity;
        return va - vb;
      });
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
      // Blend grey -> full-brightness hue by intensity; never near-black.
      var t = e.m / 255, ch = [e.r, e.g, e.b], o = [0, 0, 0];
      for (var k = 0; k < 3; k++) {
        o[k] = Math.round(RGB_GREY_RGB[k] + (ch[k] / e.m * 255 - RGB_GREY_RGB[k]) * t);
      }
      return 'rgb(' + o[0] + ',' + o[1] + ',' + o[2] + ')';
    }
    var fld = fieldForMode(mode);
    if (fld) {
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
  function activeCell(i) {
    if (pctMask && !pctMask[i]) return false;
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
  function shown(i, p) { return visible(i, p) && activeCell(i); }
  // Stable per-cell subsample: the same subset persists across redraws/toggles
  // (deterministic hash), so lowering "% of cells" never reshuffles the view.
  function rebuildPctMask() {
    if (!D || pctShow >= 100) { pctMask = null; return; }
    var n = D.n, thr = pctShow / 100;
    pctMask = new Uint8Array(n);
    for (var i = 0; i < n; i++) {
      var h = (Math.imul(i + 1, 2654435761) >>> 0) / 4294967296;
      pctMask[i] = h < thr ? 1 : 0;
    }
  }
  // After a filter / subsample change: drop now-inactive cells from the current
  // selection, then redraw. Keeps the coordinated selection consistent.
  function applyActiveChange() {
    if (sel) {
      var s = new Set();
      sel.forEach(function (i) { if (activeCell(i)) s.add(i); });
      setSelection(s.size ? s : null);
    } else {
      drawAll();
    }
  }

  // ---- panel geometry + projection ----------------------------------------
  function project(p) {
    var sp = spaceById[p.spaceId];
    if (!sp) { p.sx = null; p.sy = null; return; }
    if (!sp._unit) sp._unit = unitOf(sp);
    var u = sp._unit, n = D.n;
    // Axis'd spaces (the clone panel) reserve room on the left for the y-label
    // and along the bottom for the x-label. The remaining rectangle is filled:
    // linked panels deliberately use the available screen width instead of
    // centring a square canvas with unused space on both sides.
    var axed = !!sp._axisSpec;
    var padL = axed ? 42 : 16, padB = axed ? 30 : 16, padT = 16, padR = 16;
    var SX = Math.max(1, p.W - padL - padR);
    var SY = Math.max(1, p.H - padT - padB);
    var ox = padL, oy = padT;
    p._SX = SX; p._SY = SY; p._S = Math.min(SX, SY);
    p._sox = ox; p._soy = oy;                // for dataToScreen (image bounds)
    if (!p.sx || p.sx.length !== n) {
      p.sx = new Float32Array(n); p.sy = new Float32Array(n);
    }
    p.ok = u.ok;
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
  function isProjectionPanel(p) {
    return !!(p && isProjectionSpace(spaceById[p.spaceId]));
  }

  // Is there a panel one can actually select on?
  function anyFlatPanel() {
    return panels.some(function (p) { return p.spaceId && !panelIs3D(p); });
  }
  // Whether the data set offers a 2-D embedding at all -- not whether one is on
  // screen. Decides whether "you cannot select here" can be followed by advice.
  function anyFlatProjection() {
    var P = D && D.projections; if (!P) return false;
    return Object.keys(P).some(function (k) { return (P[k].ndim || 2) < 3; });
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
    p.canvas.width = width * dpr; p.canvas.height = height * dpr;
    p.canvas.style.width = width + 'px';
    p.canvas.style.height = height + 'px';
    p.ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    if (p.pane) p.pane.classList.toggle('cv-narrow', width < 420);
    project(p);
  }

  // Draw the histology image behind the points of the spatial panel. The image's
  // data-space bounds are mapped to screen with the SAME transform as the cells,
  // so it aligns; opacity/offset/scale/flip/rotate then adjust it on top.
  function imageRenderState(img, state) {
    var pr = (img && img.preset) || {};
    if (!pr.geometryBaked) return state;
    var baseScaleX = Number(pr.scaleX) || 1;
    var baseScaleY = Number(pr.scaleY) || baseScaleX;
    return {
      show: state.show,
      opacity: state.opacity,
      offsetX: state.offsetX - (Number(pr.offsetX) || 0),
      offsetY: state.offsetY - (Number(pr.offsetY) || 0),
      scaleX: state.scaleX / baseScaleX,
      scaleY: state.scaleY / baseScaleY,
      flipX: !!state.flipX !== !!pr.flipX,
      flipY: !!state.flipY !== !!pr.flipY,
      rotate: state.rotate - (Number(pr.rotation) || 0)
    };
  }
  function drawImage(p) {
    var sp = spaceById[p.spaceId];
    var cimg = currentImage(sp);
    var state = sp && sp._imgState;
    if (!sp || !cimg || !sp._imgEl || !sp._imgReady || !state || !state.show) return;
    var b = cimg.bounds;
    if (!b) return;
    state = imageRenderState(cimg, state);
    var tl = dataToScreen(p, b.xmin, b.ymax);   // data ymax = top (y-up)
    var br = dataToScreen(p, b.xmax, b.ymin);
    if (!tl || !br) return;
    var x = Math.min(tl[0], br[0]), y = Math.min(tl[1], br[1]);
    var w = Math.abs(br[0] - tl[0]), h = Math.abs(br[1] - tl[1]);
    if (!(w > 0) || !(h > 0)) return;
    // Convert the DATA-unit offset to screen pixels by mapping two points through
    // the same projection the cells use (handles scale + the y-axis inversion).
    var o0 = dataToScreen(p, b.xmin, b.ymin);
    var o1 = dataToScreen(p, b.xmin + state.offsetX, b.ymin + state.offsetY);
    var offSX = (o0 && o1) ? (o1[0] - o0[0]) : 0;
    var offSY = (o0 && o1) ? (o1[1] - o0[1]) : 0;
    var c = p.ctx;
    c.save();
    c.globalAlpha = state.opacity;
    c.translate(x + w / 2 + offSX, y + h / 2 + offSY);
    if (state.rotate) c.rotate(-state.rotate * Math.PI / 180);
    c.scale(state.scaleX * (state.flipX ? -1 : 1),
      state.scaleY * (state.flipY ? -1 : 1));
    c.drawImage(sp._imgEl, -w / 2, -h / 2, w, h);
    c.restore();
    c.globalAlpha = 1;
  }

  function drawTrajectory(p) {
    var sp = spaceById[p.spaceId];
    if (!sp || !sp.trajectory || !sp.edges || !sp.edges.length) return;
    var c = p.ctx;
    c.save();
    c.globalAlpha = 0.72;
    c.strokeStyle = '#1f2937';
    c.lineWidth = 1.5;
    c.lineCap = 'round';
    c.beginPath();
    sp.edges.forEach(function (edge) {
      if (!edge || edge.length < 4) return;
      var source = dataToScreen(p, Number(edge[0]), Number(edge[1]));
      var target = dataToScreen(p, Number(edge[2]), Number(edge[3]));
      if (!source || !target) return;
      c.moveTo(source[0], source[1]);
      c.lineTo(target[0], target[1]);
    });
    c.stroke();
    c.restore();
  }

  function drawHulls(p) {
    var sp = spaceById[p.spaceId];
    if (!sp || !sp.hulls || !sp.hulls.length) return;
    var c = p.ctx;
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
  var labelsOn = true;
  var _lblCache = { d: null };
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
    var nlev = g.levels.length, xs = [], ys = [], zs = [], li;
    for (li = 0; li < nlev; li++) { xs.push([]); ys.push([]); zs.push([]); }
    for (var i = 0; i < D.n; i++) {
      if (!u.ok[i]) continue;
      var lv = g.values[i];
      if (lv == null || lv < 0 || lv >= nlev) continue;
      xs[lv].push(u.nx[i]); ys[lv].push(u.ny[i]);
      if (u.nz) zs[lv].push(u.nz[i]);
    }
    var med = function (a) {
      if (!a.length) return null;
      a.sort(function (x, y) { return x - y; });
      return a[a.length >> 1];
    };
    var out = [];
    for (li = 0; li < nlev; li++) {
      var mx = med(xs[li]);
      if (mx == null) continue;
      // The THIRD component is cached too, so a rotation only has to turn one
      // point per level instead of re-deriving a median over every cell. Cached
      // unrotated, since the cache must survive the rotation changing.
      out.push({ li: li, nx: mx, ny: med(ys[li]),
        nz: u.nz ? (med(zs[li]) || 0) : 0, text: String(g.levels[li]) });
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
    // Below ~260px a label chip covers a meaningful share of the panel.
    if (!labelsOn || p.W < 260) return;
    var L = groupLabelsFor(p); if (!L || !L.length) return;
    var c = p.ctx, v = p.view, SX = p._SX, SY = p._SY,
      ox = p._sox, oy = p._soy;
    if (!SX || !SY) return;
    c.save();
    c.globalAlpha = 1;
    c.font = '600 11px system-ui, -apple-system, "Segoe UI", sans-serif';
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
      c.fillStyle = 'rgba(255,255,255,.80)';
      c.fillRect(x - w / 2 - 4, y - 8, w + 8, 16);
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
    var on = !!(p.view && D && p.sx && u);
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
    var sp = p && spaceById[p.spaceId];
    var configured = sp && sp.builder_point_size != null
      ? Number(sp.builder_point_size) : NaN;
    if (!pointSizeEdited && isFinite(configured) &&
      configured >= 0 && configured <= 20) {
      return configured;
    }
    return ps;
  }
  function pointOpacityOf(p) {
    var sp = p && spaceById[p.spaceId];
    var configured = sp && sp.builder_point_opacity != null
      ? Number(sp.builder_point_opacity) : NaN;
    if (!pointOpacityEdited && isFinite(configured) &&
      configured >= 0 && configured <= 1) {
      return configured;
    }
    return pointOpacity;
  }
  function radiusOf(p, i) {
    var pointSize = p._renderPointSize == null
      ? pointSizeOf(p) : p._renderPointSize;
    if (!p.depth) return pointSize;
    var t = (p.depth[i] - p._dmin) / p._dspan;   // 0 = furthest, 1 = nearest
    if (t < 0) t = 0; else if (t > 1) t = 1;
    return pointSize * (0.5 + 0.85 * t);
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

  function shownState(p) {
    var mask = new Uint8Array(D.n), count = 0;
    for (var i = 0; i < D.n; i++) {
      if (shown(i, p)) { mask[i] = 1; count++; }
    }
    return { mask: mask, count: count };
  }

  function draw(p, shownMask) {
    var c = p.ctx; c.clearRect(0, 0, p.W, p.H);
    if (!p.sx) return;
    p._renderPointSize = pointSizeOf(p);
    var panelPointOpacity = pointOpacityOf(p);
    drawImage(p);
    drawHulls(p);
    drawTrajectory(p);
    drawAxes(p);
    // One two-layer pass: background on layer 0, foreground on layer 1 (on top).
    // The foreground is the "meaningful" set — expressing cells in RGB mode,
    // otherwise the selected cells. Alpha: with a selection, selected stay solid
    // and the rest fade; with none, RGB dims its grey substrate, else the opacity
    // slider governs. Both the layering and the paint go through one code path.
    // Highlight set = the lasso selection, or (Trekker) the picked nucleus's
    // niche. Cells in it stay solid; everything else fades.
    var n = D.n, i, rgb = panelColorMode(p) === RGB_MODE;
    var space = spaceById[p.spaceId], border = space && space.pointBorder;
    var hiSet = (sel && sel.size) ? sel : nicheSet;
    // shown(i) (= visible + activeCell) is stable across this draw but is tested
    // 2n times below (two layers) plus once in the evidence pass; precompute it
    // once. Uint8 mask, indexed instead of recomputed — halves the per-cell work
    // on the hot lasso-drag redraw path.
    if (!shownMask || shownMask.length !== n) shownMask = shownState(p).mask;
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
    var ord = paintOrder(p);
    for (var layer = 0; layer < 2; layer++) {
      var alpha = hiSet ? (layer === 1 ? 0.95 : 0.05)
        : rgb ? (layer === 1 ? 1 : 0.5 * panelPointOpacity) : panelPointOpacity;
      if (n >= BATCH_MIN) {
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
    // Trekker: ring nuclei that carry positioning evidence.
    if (evidenceOn && D.trekker && D.trekker.evidence) {
      c.globalAlpha = 1; c.strokeStyle = '#1f2937'; c.lineWidth = 1.4;
      for (i = 0; i < n; i++) {
        if (!p.ok[i] || !shownMask[i] || D.trekker.evidence[i] !== 1) continue;
        c.beginPath(); c.arc(p.sx[i], p.sy[i], p._renderPointSize + 2.5, 0, 6.2832); c.stroke();
      }
    }
    drawAxes3D(p);
    drawGroupLabels(p);
    // Hovered cell, marked in EVERY panel including the one being pointed at.
    // Thinner and cooler than the pick ring so the two never read as the same
    // state: this one follows the cursor and is gone the moment it leaves.
    if (hoverCell != null && hoverCell !== pick && p.ok[hoverCell]) {
      c.globalAlpha = 1; c.strokeStyle = '#0f172a'; c.lineWidth = 1.6;
      c.beginPath();
      c.arc(p.sx[hoverCell], p.sy[hoverCell], p._renderPointSize + 3.5, 0, 6.2832);
      c.stroke();
      c.strokeStyle = 'rgba(255,255,255,0.85)'; c.lineWidth = 1;
      c.beginPath();
      c.arc(p.sx[hoverCell], p.sy[hoverCell], p._renderPointSize + 5, 0, 6.2832);
      c.stroke();
    }
    // picked cell ring
    if (pick != null && p.ok[pick]) {
      c.globalAlpha = 1; c.strokeStyle = '#f97316'; c.lineWidth = 2.2;
      c.beginPath(); c.arc(p.sx[pick], p.sy[pick], p._renderPointSize + 4, 0, 6.2832); c.stroke();
    }
    // Trekker: dashed niche-radius circle around the picked nucleus (physical
    // panel). Radius µm → screen px via the same data→unit→screen scale.
    if (pick != null && !sel && D.trekker && p.spaceId === 'trekker' && p.ok[pick]) {
      var nu = spaceById['trekker'] && spaceById['trekker']._unit;
      if (nu && p._SX != null && p._SY != null) {
        var viewScale = p.view ? 1 / p.view.span : 1;
        var radiusX = nicheRadius * nu.k * p._SX * viewScale;
        var radiusY = nicheRadius * nu.k * p._SY * viewScale;
        c.globalAlpha = 1;
        c.fillStyle = 'rgba(249,115,22,.10)';
        c.beginPath();
        c.ellipse(p.sx[pick], p.sy[pick], radiusX, radiusY, 0, 0, 6.2832);
        c.fill();
        c.strokeStyle = '#f97316'; c.lineWidth = 2.5; c.setLineDash([7, 4]);
        c.beginPath();
        c.ellipse(p.sx[pick], p.sy[pick], radiusX, radiusY, 0, 0, 6.2832);
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
  function drawAll() {
    var first = null;
    panels.forEach(function (p) {
      if (!p.spaceId) return;
      var state = shownState(p);
      if (!first) first = state;
      draw(p, state.mask);
    });
    if (first) renderShownCount(first.count);
  }
  // Drop any committed lasso outlines when an interaction deliberately changes
  // what the region means (for example a new selection or selection zoom).
  // Returns true if anything was cleared.
  // Default point radius from the cell count and the panel size: a 200k-cell
  // panel needs smaller dots than a 2k one, and nobody should have to find the
  // slider to get a readable first paint. Same idea as the Projection tab's
  // dynamicPointSize(), fitted to this canvas's radius scale.
  //
  // It seeds the value ONCE per data set and then leaves it alone. Recomputing
  // on every resize would mean the dots visibly change size when the bar's
  // second row opens or a bar appears — the panels shrink slightly, and a point
  // size that twitches at every unrelated layout change reads as a glitch.
  var psSeeded = false;
  function autoPointSize(side) {
    if (!D) return;
    psSeeded = true;
    var configured = D.default_point_size == null
      ? NaN : Number(D.default_point_size);
    var v;
    if (isFinite(configured)) {
      // The Builder's Overview point size now belongs to the unified workspace.
      // Builder and Linked views share the full public 0..20 range.
      v = Math.max(0, Math.min(20, configured));
    } else {
      var base = 6.5 - Math.log10(Math.max(1, D.n));
      var scale = Math.max(0.75, Math.min(1.35, (side || 520) / 520));
      v = Math.max(0.8, Math.min(7, base * scale));
    }
    ps = Math.round(v * 5) / 5;                    // slider step is 0.2
    var el = $('cv-ps'); if (el) el.value = String(ps);
    var lbl = $('cv-ps-val'); if (lbl) lbl.textContent = ps.toFixed(1);
    positionRangeVal('cv-ps', 'cv-ps-val');
  }

  // ---- the More settings overlay -------------------------------------------
  // Open/closed is one class on the viewport drawer plus aria-expanded on the
  // trigger. Unlike the former second bar row, it never claims layout height:
  // the visualisation grid stays still while advanced point/image controls are
  // adjusted above it.
  function isMoreOpen() {
    var mp = $('cv-more');
    return !!(mp && mp.classList.contains('is-open'));
  }
  // Shut every open group-filter menu and clear the "open" look on its chip.
  function closeFilterMenus() {
    Array.prototype.forEach.call(
      document.querySelectorAll('.coordviews-page .cv-filt'),
      function (wrap) {
        var m = wrap.querySelector('.cv-filt-menu');
        if (m) m.style.display = 'none';
        var b = wrap.querySelector('.cv-filt-btn');
        if (b) b.classList.remove('is-open');
      }
    );
  }

  var moreMountTimer = null;

  function syncMoreMode() {
    var mp = $('cv-more');
    if (!mp) return;
    mp.setAttribute(
      'aria-modal',
      window.matchMedia('(max-width: 900px)').matches ? 'true' : 'false'
    );
  }

  function setMoreOpen(open, restoreFocus) {
    var mp = $('cv-more'), btn = $('cv-more-btn');
    if (!mp) return;
    if (open) {
      document.dispatchEvent(new CustomEvent('cerebro:overlay-opening', {
        detail: { owner: 'more' }
      }));
    }
    syncMoreMode();
    // Mount before opening, unmount after closing. While folded the overlay is
    // display:none; once mounted it is fixed to the viewport, never creating a
    // new flex line or changing the available panel height.
    clearTimeout(moreMountTimer);
    if (open) {
      // A transformed app shell becomes the containing block of fixed children.
      // Move this exact node (never clone/rebuild it) to body so "fixed" means
      // the real viewport and every input value/event binding survives intact.
      if (mp.parentNode !== document.body) document.body.appendChild(mp);
      mp.classList.add('is-mounted');
      void mp.offsetWidth;              // commit the display change first
    } else {
      moreMountTimer = setTimeout(function () {
        if (!mp.classList.contains('is-open')) mp.classList.remove('is-mounted');
      }, 260);
    }
    mp.classList.toggle('is-open', open);
    mp.setAttribute('aria-hidden', open ? 'false' : 'true');
    if (btn) btn.setAttribute('aria-expanded', open ? 'true' : 'false');
    if (open) {
      window.requestAnimationFrame(function () {
        var close = $('cv-more-close');
        if (close && isMoreOpen()) close.focus();
      });
    } else if (restoreFocus !== false && btn && mp.contains(document.activeElement)) {
      btn.focus();
    }
    // A level menu left open inside a folded row would still be "open" when the
    // row comes back — and the click that reopens the row would then read as the
    // click that closes the menu. Fold them away with their row.
    if (!open) closeFilterMenus();
    // This is deliberately not resizeAll(): More is outside normal flow, so a
    // settings visit must not remeasure or resize any visualisation panel.
  }

  window.addEventListener('resize', syncMoreMode);
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape' && isMoreOpen()) {
      e.preventDefault();
      e.stopImmediatePropagation();
      setMoreOpen(false);
      return;
    }
    if (e.key !== 'Tab' || !isMoreOpen() ||
        !window.matchMedia('(max-width: 900px)').matches) return;
    var mp = $('cv-more');
    var focusable = Array.prototype.filter.call(
      mp.querySelectorAll(
        'button:not([disabled]), a[href], input:not([disabled]), ' +
        'select:not([disabled]), [tabindex]:not([tabindex="-1"])'
      ),
      function (el) { return el.getClientRects().length > 0; }
    );
    if (!focusable.length) return;
    var first = focusable[0], last = focusable[focusable.length - 1];
    if (e.shiftKey && document.activeElement === first) {
      e.preventDefault();
      last.focus();
    } else if (!e.shiftKey && document.activeElement === last) {
      e.preventDefault();
      first.focus();
    }
  }, true);
  document.addEventListener('cerebro:overlay-opening', function (e) {
    if (e.detail && e.detail.owner !== 'more' && isMoreOpen()) {
      setMoreOpen(false, false);
    }
  });

  function clearLassos() {
    var any = false;
    panels.forEach(function (p) {
      if (p.lasso || p.lassoData) { p.lasso = null; p.lassoData = null; any = true; }
    });
    return any;
  }

  // Centre a slider's value bubble over its thumb. Uses the slider's fixed 150px
  // width as a fallback so it positions correctly even while its panel is hidden
  // (offsetWidth 0), and re-runs on every input.
  function positionRangeVal(sliderId, valId) {
    var s = $(sliderId), v = $(valId);
    if (!s || !v) return;
    var min = parseFloat(s.min), max = parseFloat(s.max), val = parseFloat(s.value);
    var frac = (max > min) ? (val - min) / (max - min) : 0;
    var w = s.offsetWidth || 150, thumb = 25;
    v.style.left = (frac * (w - thumb) + thumb / 2) + 'px';
    s.style.setProperty('--cv-range-fill', (frac * 100) + '%');
  }
  function positionAllRangeVals() {
    positionRangeVal('cv-ps', 'cv-ps-val');
    positionRangeVal('cv-opacity', 'cv-op-val');
    positionRangeVal('cv-pct', 'cv-pct-val');
    positionRangeVal('cv-dissolve', 'cv-dissolve-val');
    positionRangeVal('cv-niche', 'cv-niche-val');
  }

  function positionImgRangeValue(slider) {
    if (!slider) return;
    var wrap = slider.closest('.cv-img-range');
    var value = wrap && wrap.querySelector('.cv-img-range-value');
    if (!value) return;
    var min = parseFloat(slider.min), max = parseFloat(slider.max), val = parseFloat(slider.value);
    var frac = (max > min) ? (val - min) / (max - min) : 0;
    var w = slider.offsetWidth || 148, thumb = 16;
    value.textContent = String(Math.round(val * 100) / 100);
    value.style.left = (frac * (w - thumb) + thumb / 2) + 'px';
    slider.style.setProperty('--cv-range-fill', (frac * 100) + '%');
  }

  function positionAllImgRangeValues() {
    Array.prototype.forEach.call(
      document.querySelectorAll('.cv-img-range input[type=range]'),
      positionImgRangeValue
    );
  }

  // ---- geometry helpers ----------------------------------------------------
  // inPoly lives in the shared CBGeom module (www/cv-geom.js). `nearest` stays
  // here: its visibility predicate (p.ok + shown) and fixed hit radius are this
  // engine's, not shared.
  function nearest(p, mx, my) {
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
    // A zoom is tied to a specific selection, so any selection change (new brush
    // or clear) returns to the full view and resets the toggle.
    if (zoomed) {
      resetZoom(panelForSpace(zoomedSpace));
      zoomed = false; zoomedSpace = null;
    }
    updateZoomBtn();
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
  // The Zoom / Clear buttons live together and appear only with a selection.
  function updateSelActions() {
    var hasSel = !!(sel && sel.size);
    // A Trekker niche pick also gets the (animated) Clear button — but not the
    // "Zoom to selection" button, which is meaningless for a single-cell pick.
    var hasNiche = !hasSel && pick != null && !!nicheSet;
    var show = hasSel || hasNiche;
    revealEl($('cv-selactions'), show);
    var zb = $('cv-zoom');
    // Offered only when at least one visible flat panel contains selected cells.
    // The action follows the panel that created the cohort, whatever its modality.
    var canZoom = hasSel && panels.some(canZoomPanel);
    if (zb) zb.style.display = canZoom ? '' : 'none';
  }
  // Zoom one requested panel to the bounding box of its selected cells.
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
      if (!btn) return;
      btn.style.display = canZoomPanel(p) ? '' : 'none';
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
  function panelForSpace(spaceId) {
    if (!spaceId) return null;
    return panels.find(function (panel) { return panel.spaceId === spaceId; }) || null;
  }
  function selectionZoomPanel() {
    var source = panelForSpace(selectionSourceSpace);
    if (canZoomPanel(source)) return source;
    return panels.find(function (panel) {
      return isProjectionPanel(panel) && canZoomPanel(panel);
    }) || panels.find(canZoomPanel) || null;
  }
  function resetZoom(only) {
    var any = false;
    panels.forEach(function (p) {
      if (only && p !== only) return;
      if (p.view) { p.view = null; project(p); any = true; }
    });
    if (any) drawAll();
  }
  // The Zoom button is a toggle: zoom in to the selection, or zoom back out. Its
  // label + active style reflect the current state.
  function updateZoomBtn() {
    var b = $('cv-zoom'); if (!b) return;
    b.textContent = zoomed ? 'Zoom back' : 'Zoom to selection';
    b.classList.toggle('is-zoomed', zoomed);
  }
  function toggleZoom() {
    var target = selectionZoomPanel();
    if (zoomed) {
      resetZoom(panelForSpace(zoomedSpace) || target);
      zoomed = false; zoomedSpace = null;
    } else {
      zoomed = !!target && zoomToSelection(target);
      zoomedSpace = zoomed ? target.spaceId : null;
    }
    updateZoomBtn();
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
    if (spaceId === zoomedSpace && zoomed) {
      zoomed = false; zoomedSpace = null; updateZoomBtn();
    }
  }

  // Zoom about a screen point, keeping whatever is under it fixed — the gesture
  // every map and every plotly plot uses. `factor` < 1 zooms in. Passing the
  // panel centre gives the plain in/out of the toolbar buttons.
  function zoomAt(p, mx, my, factor) {
    if (!p._SX || !p._SY) return;
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
    // Keep the cohort "Zoom back" toggle honest when its panel returns to full.
    if (p.spaceId === zoomedSpace && !p.view && zoomed) {
      zoomed = false; zoomedSpace = null; updateZoomBtn();
    }
    drawAll();
  }
  function zoomStep(p, factor) {
    if (!p._SX || !p._SY) return;
    zoomAt(p, p._sox + p._SX / 2, p._soy + p._SY / 2, factor);
  }
  function downloadPanelPNG(p) {
    try {
      // Composite onto white first — the canvas itself is transparent, so a raw
      // export would have no background.
      var src = p.canvas, tmp = document.createElement('canvas');
      tmp.width = src.width; tmp.height = src.height;
      var c = tmp.getContext('2d');
      c.fillStyle = '#ffffff'; c.fillRect(0, 0, tmp.width, tmp.height);
      c.drawImage(src, 0, 0);
      var nm = (spaceById[p.spaceId] && spaceById[p.spaceId].label) || p.spaceId || 'panel';
      var a = document.createElement('a');
      a.href = tmp.toDataURL('image/png');
      a.download = 'linked-views-' + nm.replace(/[^\w.-]+/g, '_') + '.png';
      document.body.appendChild(a); a.click(); document.body.removeChild(a);
    } catch (e) { /* toDataURL can throw if the canvas is tainted; ignore */ }
  }
  function reportSelection() {
    var arr = null;
    if (sel && sel.size) { arr = []; sel.forEach(function (i) { arr.push(D.cells[i]); }); }
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      if (singleActive) {
        var sp = spaceById[singleSpaceIds[0]], x = [], y = [];
        if (arr && sp) sel.forEach(function (i) {
          x.push(sp.x[i]); y.push(sp.y[i]);
        });
        Shiny.setInputValue(singleActive + '_persistent_selection', arr
          ? { x: x, y: y, ids: arr } : null);
      } else {
        Shiny.setInputValue('coordviews_selection', arr);
      }
    }
    if (singleActive) {
      var guide = $(singleActive + '_selection_guide');
      var active = $(singleActive + '_selection_active');
      if (guide) guide.classList.toggle('cerebro-selection-status-hidden', !!arr);
      if (active) active.classList.toggle('cerebro-selection-status-hidden', !arr);
    }
    window.dispatchEvent(new CustomEvent(
      singleActive ? 'cerebro:specialist-state' : 'cerebro:linkedviews-selection',
      { detail: singleActive
        ? { viewId: singleActive, selectedCells: arr ? arr.length : 0 }
        : { selectedCells: arr ? arr.length : 0 } }
    ));
  }

  function reportSingleHiddenGroups() {
    if (!singleActive || typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var group = catOf(singleSpaceModes[singleSpaceIds[0]]);
    var names = group ? Array.from(hidden).map(function (index) {
      return group.levels[index];
    }).filter(function (name) { return name != null; }) : [];
    Shiny.setInputValue(singleActive + '_hidden_groups', names);
  }

  function reportSingleZoom() {
    if (!singleActive) return;
    var panel = panels.find(function (candidate) { return !!candidate.spaceId; });
    var on = !!(panel && panel.view);
    var button = $(singleActive + '_zoom_to_selection');
    if (!button) return;
    button.classList.toggle('is-zoomed', on);
    button.setAttribute('aria-pressed', on ? 'true' : 'false');
    var label = button.querySelector('span');
    if (label) label.textContent = on ? 'Reset zoom' : 'Zoom to selection';
    var icon = button.querySelector('i');
    if (icon) {
      icon.classList.toggle('fa-magnifying-glass-plus', !on);
      icon.classList.toggle('fa-magnifying-glass-minus', on);
    }
  }

  // ---- readouts (composition + top clonotypes) — fully client-side --------
  function renderSelbar() {
    var bar = $('cv-selbar');
    if (!bar) return;
    if (sel && sel.size) {
      $('cv-sel-kicker').textContent = 'Active cohort';
      $('cv-seltext').innerHTML = 'Selected <b>' + fmt(sel.size) + '</b> / ' +
        fmt(D.n) + ' cells &mdash; coordinated across all panels';

      // Give the geometry a compact biological identity. This is descriptive:
      // the dominant registered category and its observed share, not an inferred
      // interpretation of the selection.
      var profile = $('cv-selprofile'), compName = compGroupName();
      var g = catOf(compName), top = null, counts = {};
      if (g) {
        sel.forEach(function (i) {
          var lv = g.values[i]; counts[lv] = (counts[lv] || 0) + 1;
        });
        Object.keys(counts).forEach(function (lv) {
          if (!top || counts[lv] > top[1]) top = [lv, counts[lv]];
        });
      }
      if (profile) {
        profile.textContent = top
          ? (g.levels[+top[0]] + ' · ' + Math.round(top[1] / sel.size * 100) + '%')
          : 'Linked cell set';
      }

      var origin = $('cv-selorigin');
      if (origin) origin.textContent = selectionSource
        ? ('Selected in ' + selectionSource) : 'Shared selection';

      // A cohort remains the same set even when one lens cannot place every
      // member. Report that coverage rather than silently making it look smaller.
      var mapped = [];
      panels.forEach(function (p) {
        if (!p.spaceId || !p.ok) return;
        var n = 0; sel.forEach(function (i) { if (p.ok[i]) n++; });
        var space = spaceById[p.spaceId];
        mapped.push({ label: space ? space.label : p.spaceId, count: n });
      });
      var coverage = $('cv-selcoverage');
      if (coverage) {
        var mappedCounts = mapped.map(function (item) { return item.count; });
        var lo = mappedCounts.length ? Math.min.apply(Math, mappedCounts) : sel.size;
        coverage.textContent = lo === sel.size
          ? (mapped.length + ' linked views · complete mapping')
          : mapped.map(function (item) {
            return item.label + ' ' + item.count + '/' + sel.size;
          }).join(' · ');
      }
    } else if (pick != null && nicheSet) {
      // A picked Trekker nucleus is the single-cell counterpart of an active
      // cohort. Keep it in the same shared-state surface so its identity,
      // neighbourhood, and Clear action remain visible across every lens.
      $('cv-sel-kicker').textContent = 'Active cell';
      $('cv-seltext').innerHTML = 'Picked nucleus <b>' + esc(D.cells[pick]) + '</b>';

      var pickProfile = $('cv-selprofile'), pickGroup = catOf(compGroupName());
      if (pickProfile) {
        var pickLevel = pickGroup && pickGroup.values[pick];
        pickProfile.textContent = pickGroup && pickGroup.levels[pickLevel] != null
          ? pickGroup.levels[pickLevel] : 'Single nucleus';
      }
      var pickOrigin = $('cv-selorigin');
      if (pickOrigin) pickOrigin.textContent = 'Single-cell neighbourhood';
      var pickCoverage = $('cv-selcoverage');
      if (pickCoverage) {
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
  // (shared with trekker.js). This engine keys on spaceById['trekker'] and passes
  // inclusive=<=, skipNaN=true (unpositioned cells align to NaN here).
  function rebuildNiche() {
    nicheSet = null;
    if (pick == null || (sel && sel.size) || !D.trekker) return;
    var sp = spaceById['trekker'];
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
    var nk = $('cv-niche'); if (!nk) return;
    var on = (pick != null && !(sel && sel.size));
    nk.disabled = !on;
    var wrap = $('cv-niche-wrap');
    if (wrap) {
      wrap.classList.toggle('cv-disabled', !on);
      wrap.title = on ? '' : 'Click a nucleus in a panel first to set its niche';
    }
  }
  function renderReadout() {
    var host = $('cv-readout'); if (!host) return;
    updateNicheEnabled();
    if (!sel || !sel.size) {
      if (renderNiche(host)) return;   // Trekker: picked-nucleus niche composition
      var fieldOnly = fieldSummaryHtml();
      if (fieldOnly) { host.innerHTML = fieldOnly; return; }
      host.innerHTML = '<div class="cv-empty">' + (anyFlatPanel()
        ? ('Lasso-drag in any ' + (panels.some(panelIs3D) ? '2-D ' : '') +
          'panel to select cells. The same cells highlight ' +
          'in every panel, and their composition and top clonotypes appear here.' +
          (D.trekker ? ' <b>Or click a single nucleus</b> to see its niche — ' +
            'the cell-type composition within the radius (µm).' : ''))
        // Everything on screen is rotatable, so there is nowhere here to draw a
        // selection that means anything. Say so rather than leave the instruction
        // above pointing at a gesture that will silently rotate. Both halves have
        // to agree about what the data set holds: "the only embedding is 3-D"
        // followed by "pick a 2-D one" contradicts itself, and the half that is
        // wrong is the one the user will act on. Pointing at the Projection tab
        // is no answer either -- its 3-D scatter is no more lassoable than these
        // panels are.
        : ((anyFlatProjection()
          ? 'Every panel is showing a 3-D embedding, so they are for turning ' +
            'and looking.'
          : 'This data set\'s only embedding is 3-D, so these panels are for ' +
            'turning and looking.') +
          ' A lasso on a rotated cloud would take in cells ' +
          'hidden behind the ones you can see, so ' + (anyFlatProjection()
            ? 'pick a <b>2-D</b> projection above to select.'
            : 'selecting needs a 2-D embedding, which this data set does not ' +
              'carry.'))) + '</div>';
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
    C.style.display = show ? 'flex' : 'none';
    if (!show) return;
    var g = $('cv-grad');
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
  function renderLegend() {
    var L = $('cv-legend'); if (!L) return;
    var singlePayload = singleActive && singleViews[singleActive];
    var hideSingleLegend = !!(singlePayload && singlePayload.meta &&
      singlePayload.meta.legend_position === 'none');
    L.style.display = hideSingleLegend ? 'none' : '';
    var singleFontSize = Number(singlePayload && singlePayload.meta &&
      singlePayload.meta.legend_font_size);
    L.style.fontSize = isFinite(singleFontSize) && singleFontSize > 0
      ? singleFontSize + 'px' : '';
    L.innerHTML = '';
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
    var fld = fieldOf();
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
      // Legend for the three additive channels: a colour swatch + the gene that
      // drives it (or "—" if empty). Updates when a channel gene changes.
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
  function hoverHtml(i, pinned) {
    if (singleActive) {
      var singleSpace = spaceById[singleSpaceIds[0]];
      var raw = singleSpace && singleSpace._hover && singleSpace._hover[i];
      var text = String(raw || D.cells[i] || '')
        .replace(/<br\s*\/?>/gi, '\n').replace(/<[^>]*>/g, '');
      return '<div class="cv-tip-row">' + esc(text).replace(/\n/g, '<br>') + '</div>';
    }
    var rows = [];
    var g = catOf(colorBy);
    // headline = the active categorical level, else the barcode
    var head = g ? g.levels[g.values[i]] : D.cells[i];
    var h = '<div class="cv-tip-row"><b>' + esc(head) + '</b></div>';
    // the continuous variable in play, at its true value
    var fld = fieldOf();
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
      h += '<div class="cv-tip-act">' +
        '<button type="button" class="cv-tip-btn cv-tip-details">Details</button>' +
        '<button type="button" class="cv-tip-btn cv-tip-close" ' +
        'aria-label="Close">Close</button></div>';
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
    if (t) t.textContent = g ? g.levels[g.values[i]] : 'Cell';
    if (b) b.textContent = D.cells[i];
    if (it) it.textContent = g ? g.levels[g.values[i]] : 'Cell';
    if (ib) ib.textContent = D.cells[i];
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
      addSection('clonotype', 'Clonotype',
        '<div class="cv-card-seq">' + esc(D.clone.label[ci] || '—') + '</div>' +
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
        'data-cell="' + esc(D.cells[i]) + '" aria-label="Enlarge positioning evidence">' +
        '<img src="' + esc(evidenceImg) + '" alt="Positioning evidence for ' +
        esc(D.cells[i]) + '"></button>' +
        '<div class="cv-card-sub">Why this nucleus was placed here. Click to enlarge.</div>');
    }
    // the full meta row, or a placeholder until the server answers
    var metaHtml = '';
    if (cardMeta && cardMeta.cell === D.cells[i] && cardMeta.rows) {
      metaHtml = kvHtml(cardMeta.rows.map(function (r) { return [r.k, r.v]; }));
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
    if (p && p.sx && p.canvas) {
      var canv = p.canvas.getBoundingClientRect();
      from = { x: canv.left + p.sx[i], y: canv.top + p.sy[i] };
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
    if (!cardMeta || cardMeta.cell !== D.cells[i]) cardMeta = null;
    renderCard();
    // The tooltip has just been promoted into the card — leaving it up would
    // show the same cell twice, once truncated. It returns on the next hover.
    var tip = p && $(p.tipId);
    if (tip) tip.style.opacity = 0;
    cardFlyFrom(p, i);
    // ask for the exact meta row; the card is already on screen either way
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('coordviews_cell_detail', D.cells[i],
        { priority: 'event' });
    }
  }

  function openTrekkerInspector(i) {
    cardCell = i;
    if (!cardMeta || cardMeta.cell !== D.cells[i]) cardMeta = null;
    renderCard();
    openTrekkerInsights('cell');
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('coordviews_cell_detail', D.cells[i],
        { priority: 'event' });
    }
  }

  function closeCard() {
    var card = $('cv-card');
    cardCell = null;
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
    var tx = p.sx[i] + 14, ty = p.sy[i] - th - 10;
    if (tx + tw > p.W - m) tx = p.sx[i] - tw - 14;   // flip left
    if (ty < m) ty = p.sy[i] + 14;                   // flip below
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
    tip.innerHTML = hoverHtml(i, true);
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
    var x = p.sx[i], y = p.sy[i];
    if (x == null || isNaN(x) || x < 0 || y < 0 || x > p.W || y > p.H) {
      tip.style.opacity = 0;
      return;
    }
    tip.style.opacity = 1;
    placeTip(p, tip, i);
  }

  // Mark the hovered cell everywhere. Redraws only when the cell CHANGES: a
  // mousemove fires far more often than the answer to "which cell is nearest"
  // changes, and each redraw is the whole cloud in every panel.
  function setHoverCell(i) {
    if (hoverCell === i) return;
    hoverCell = i;
    drawAll();
  }

  function wireHover(p) {
    var tip = $(p.tipId);
    p.canvas.addEventListener('mousemove', function (e) {
      var space = singleActive && spaceById[p.spaceId];
      if (space && space._hoverEnabled === false) {
        tip.style.opacity = 0; setHoverCell(null); return;
      }
      var r = p.canvas.getBoundingClientRect();
      var mx = e.clientX - r.left, my = e.clientY - r.top;
      // A pinned tooltip owns this panel's tooltip element until it is closed —
      // but the cross-panel mark still follows the cursor.
      var own = pinnedTip.panel !== p;
      if (p.drag || p.panning) {
        if (own) tip.style.opacity = 0;
        setHoverCell(null);
        return;
      }
      var i = nearest(p, mx, my);
      if (i < 0 || (space && space._hoverMask && !space._hoverMask[i])) {
        if (own) tip.style.opacity = 0;
        setHoverCell(null);
        return;
      }
      setHoverCell(i);
      if (!own) return;
      tip.innerHTML = hoverHtml(i, false); tip.style.opacity = 1;
      placeTip(p, tip, i);
    });
    p.canvas.addEventListener('mouseleave', function () {
      setHoverCell(null);
      if (pinnedTip.panel === p) return;
      tip.style.opacity = 0;
    });
  }

  // ---- brush + pick --------------------------------------------------------
  function wireBrush(p) {
    var pos = function (e) {
      var r = p.canvas.getBoundingClientRect();
      return [e.clientX - r.left, e.clientY - r.top];
    };
    p.canvas.addEventListener('mousedown', function (e) {
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
    p.canvas.addEventListener('mousemove', function (e) {
      if (p.orbiting) {
        var oq = pos(e), RAD = 0.009;   // radians per pixel dragged
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
        return;
      }
      if (p.panning) {
        p.panning = false;
        p.canvas.classList.remove('cv-grabbing');
        drawAll();
        return;
      }
      if (!p.drag) return;
      p.drag = false;
      if (!D || !p.ok || !p.sx) { p.lasso = null; p.lassoData = null; return; }  // not projected yet
      var keep = false;
      if (p.moved && p.lasso && p.lasso.length > 2) {
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
        var again = (k >= 0 && k === pick);
        pick = (k >= 0 && !again) ? k : null;
        // Any open card described a cell the user has now moved on from.
        if (cardOpen()) closeCard();
        unpinTip();
        if (pick != null) {
          pinTip(p, pick);
          if (D.trekker) openTrekkerInspector(pick);
        }
        rebuildNiche();              // Trekker: cells within the picked niche
        updateSelActions();          // niche pick → show the (animated) Clear button
        renderSelbar();               // expose the shared Active cell state
        drawAll();
        if (!sel) renderReadout();   // Trekker niche readout for the picked cell
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
        view: null, mini: mini, mctx: null, miniBg: null, miniUnit: null };
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
        resizeTimer = setTimeout(function () { if (D && !focusAnimating) resizeAll(); }, 30);
      });
      panels.forEach(function (p) {
        if (p.pane) resizeObserver.observe(p.pane);
      });
    }
    // Only flow chrome changes the available panel height. More is an overlay,
    // and observing it would make an innocuous settings click re-fit the grid.
    if (!resizeObserver._cvChromeObserved) {
      ['cv-workspace-guide', 'cv-selbar'].forEach(function (id) {
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

  function fillTrekkerInsights() {
    var CT = window.CerebroTrekker;
    if (!CT || !D || !D.trekker) return;
    var q = D.trekker.qc || {};
    // Each box is filled independently. The builders read a couple of dozen QC
    // fields and throw on one that is absent, so a .crb carrying a partial QC
    // record used to lose the WHOLE detail view to whichever box failed first --
    // including the Moran table, which does not depend on any of them.
    var fill = function (id, build) {
      var el = $(id); if (!el) return;
      try { el.innerHTML = build(q); } catch (err) { el.innerHTML = ''; }
    };
    fill('cv-tk-stats', CT.buildStatsGrid);
    fill('cv-tk-postbl', CT.buildPositionTable);
    fill('cv-tk-salvflag', CT.buildSalvFlag);
    fill('cv-tk-prov', CT.buildProvenanceDl);
    fill('cv-tk-rangeflag', CT.buildRangeFlag);
    // Linkable: the table names the genes whose expression is spatially
    // structured, and this workspace can colour by a gene, so each row is one
    // click from the map that makes the number mean something. It was built
    // unlinked back when there was no gene mode here to send it to.
    $('cv-tk-morantbl').innerHTML = D.trekker.moran
      ? CT.buildMoranRows(D.trekker.moran, true) : '';
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
        var h = (Math.imul(i + 1, 2654435761) >>> 0) / 4294967296;
        y[i] = b + 0.1 + h * 0.8;
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
  var GENE_MODE = '__gene__', RGB_MODE = '__rgb__', FIELD_PREFIX = '__field__';
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
  // The "Colour by" list mirrors the Projection tab's, which offers EVERY meta
  // column: registered groups first, then the other categorical columns, then
  // every continuous field (numeric meta columns — including the QC ones people
  // actually colour by — plus Trekker's physical fields), then the gene modes.
  // Grouped into <optgroup>s because the list is now long enough to need them.
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
      '<option value="' + GENE_MODE + '">Gene expression</option>' +
      '<option value="' + RGB_MODE + '">Co-expression (RGB)</option>');
    if (sel.selectize) sel.selectize.destroy();
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
  function setColorBy(mode) {
    colorBy = mode; hidden = new Set();
    var geneCtl = $('cv-gene-ctl'), rgbCtl = $('cv-rgb-ctl');
    if (geneCtl) geneCtl.style.display = (mode === GENE_MODE) ? '' : 'none';
    if (rgbCtl) rgbCtl.style.display = (mode === RGB_MODE) ? '' : 'none';
    updateClipControl();
    clipRange();                 // seed the cache the colours read from
    renderLegend(); drawAll(); renderReadout(); updateMoranBadges();
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
  function rebuildProjectionInstances() {
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
      var sp = spaceById[id] = {
        id: id,
        label: projSpaceLabel(name),
        x: pj.x,
        y: pj.y,
        _projectionName: name,
        _unit: null
      };
      if (pj.z) { sp.z = pj.z; sp.axes = pj.axes; }
    });
  }
  function fillProjPicker() {
    var selEl = $('cv-pick-proj'); if (!selEl) return;
    var names = D.projections ? Object.keys(D.projections) : [];
    var ctl = $('cv-proj-ctl');
    if (selEl.selectize) selEl.selectize.destroy();
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
  var IMG_NONE = '__none__';
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
  function spatialImages(sp) {
    if (!sp) return [];
    if (sp.images && sp.images.length) return sp.images;
    // Multi-section bundles keep the large image payload on each sample rather
    // than duplicating the opening sample's base64 data at the space level.
    // Before the picker has changed section, `_sampleName` is unset and the
    // opening sample is the active one.
    var samples = sp.samples || [];
    var name = sp._sampleName || (samples[0] && samples[0].name);
    var sample = samples.filter(function (s) { return s.name === name; })[0];
    if (sample && sample.images && sample.images.length) return sample.images;
    if (sample && sample.image && sample.image.uri) return [sample.image];
    // `image` is a reference to the default, without the pixels -- it names an
    // entry of `images`. Only a bundle predating that carries a usable one.
    return (sp.image && sp.image.uri) ? [sp.image] : [];
  }
  function currentImage(sp) {
    var list = spatialImages(sp);
    if (!list.length) return null;
    var mode = backgroundModeFor(sp);
    if (mode === 'none') return null;
    if (mode === 'auto') return list[0];
    var wanted = sp && sp._customImageId;
    if (wanted === IMG_NONE) return null;
    for (var i = 0; i < list.length; i++) {
      if (list[i].id === wanted) return list[i];
    }
    return list[0];
  }
  function spatialName(sp) {
    return sp && (sp._sampleName ||
      (sp.samples && sp.samples[0] && sp.samples[0].name) || sp.id);
  }
  // Visible labels are not state identities: a FOV may itself be named
  // "Trekker", so modality plus stable space id namespaces its background.
  function backgroundStateKey(sp) {
    if (!sp) return null;
    if (sp._spatialSample) return 'fov:' + (sp.id || spatialName(sp));
    return 'space:' + (sp.id || 'unknown') + ':' + spatialName(sp);
  }
  function backgroundModeFor(sp) {
    var key = backgroundStateKey(sp);
    return (key && backgroundModes[key]) || 'auto';
  }
  // The key a calibration is stored under. Both halves matter: the same image
  // against a different section is a different alignment problem.
  function imgKey(sp, img) {
    if (!sp || !img) return null;
    // The section name has to resolve the same way BEFORE the first switch as
    // after it. `_sampleName` is only set when the picker is used, so falling
    // back to the space id gave the opening section one key and the same
    // section a different one on return -- and the alignment stored under the
    // first was never found again.
    return backgroundStateKey(sp) + '|' + (img.id || 'image');
  }
  function presetState(img) {
    var pr = (img && img.preset) || {};
    return {
      show: true,
      opacity: (pr.opacity != null ? pr.opacity : 0.6),
      offsetX: (pr.offsetX != null ? pr.offsetX : 0),
      offsetY: (pr.offsetY != null ? pr.offsetY : 0),
      scaleX: (pr.scaleX != null ? pr.scaleX : 1),
      scaleY: (pr.scaleY != null ? pr.scaleY : 1),
      flipX: !!pr.flipX, flipY: !!pr.flipY,
      rotate: (pr.rotation != null ? pr.rotation : 0),
      // Part of the state, not of the controls: unlocking while X and Y happen
      // to be equal is a decision, and reading the lock back off the checkbox
      // silently re-made it as "locked" on the next visit.
      lock: (pr.scaleX == null || pr.scaleY == null || pr.scaleX === pr.scaleY)
    };
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
    if (!img || !img.uri) return;
    var imagePreset = img.preset || {};
    if (space._builderDefaultPointOpacity === undefined) {
      space._builderDefaultPointOpacity = space.builder_point_opacity;
    }
    if (space._builderDefaultPointSize === undefined) {
      space._builderDefaultPointSize = space.builder_point_size;
    }
    space.builder_point_opacity = imagePreset.pointOpacity != null
      ? imagePreset.pointOpacity : space._builderDefaultPointOpacity;
    space.builder_point_size = imagePreset.pointSize != null
      ? imagePreset.pointSize : space._builderDefaultPointSize;
    var k = imgKey(space, img);
    space._imgState = (k && imgStates[k]) ? imgStates[k] : presetState(img);
    // Only the newest request may paint. Without the token a large image chosen
    // first can finish decoding after a small one chosen second and replace it.
    var mine = space._imgToken;
    var im = new Image();
    im.onload = function () {
      if (mine !== space._imgToken) return;
      space._imgReady = true; drawAll();
    };
    im.src = img.uri;
    space._imgEl = im;
  }

  function seedPointAppearanceFromImage(space) {
    var img = currentImage(space);
    var preset = (img && img.preset) || {};
    if (!pointOpacityEdited && preset.pointOpacity != null &&
      isFinite(Number(preset.pointOpacity))) {
      pointOpacity = Number(preset.pointOpacity);
      var opacity = $('cv-opacity');
      if (opacity) { opacity.step = 'any'; opacity.value = String(pointOpacity); }
      var opacityLabel = $('cv-op-val');
      if (opacityLabel) opacityLabel.textContent = pointOpacity.toFixed(2);
    }
    if (!pointSizeEdited && preset.pointSize != null &&
      isFinite(Number(preset.pointSize))) {
      ps = Number(preset.pointSize);
      psSeeded = true;
      var size = $('cv-ps');
      if (size) { size.step = 'any'; size.value = String(ps); }
      var sizeLabel = $('cv-ps-val');
      if (sizeLabel) sizeLabel.textContent = ps.toFixed(1);
    }
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

  // The background picker is scoped to the selected spatial section. Its tabs
  // stay at the top of the Background image section so the mode below and the
  // Image alignment bar both read as settings for that chosen section.
  function renderImagePicker() {
    var ctl = $('cv-img-pick-ctl'), pop = $('cv-bg-popover'), tabs = $('cv-bg-space-tabs');
    if (!ctl || !pop) return;
    var allSpaces = backgroundSpaces();
    var active = activeSpatial();
    if (!active || allSpaces.indexOf(active) < 0) active = allSpaces[0] || null;
    ctl.style.display = active ? '' : 'none';
    if (tabs) {
      tabs.innerHTML = allSpaces.map(function (sp) {
        return '<button type="button" class="cv-bg-space-tab' + (sp === active ? ' is-on' : '') +
          '" data-cv-bg-tab="' + esc(sp.id) + '">' +
          esc(sp._sampleName || sp.label || sp.id) + '</button>';
      }).join('');
      Array.prototype.forEach.call(tabs.querySelectorAll('[data-cv-bg-tab]'), function (tab) {
        tab.onclick = function () {
          backgroundScopePulse = true;
          activateSpatial(tab.getAttribute('data-cv-bg-tab'));
          renderImagePicker();
        };
      });
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
    var buttons = ctl.querySelectorAll('[data-cv-bg-mode]');
    Array.prototype.forEach.call(buttons, function (button) {
      button.classList.toggle('is-on', button.dataset.cvBgMode === backgroundModeFor(active));
    });
    pop.innerHTML = active ? [active].map(function (sp) {
      var list = spatialImages(sp), count = list.length;
      var heading = '<div class="cv-bg-row-heading"><span class="cv-bg-row-label">' +
        esc(sp._sampleName) + '</span><span class="cv-bg-count">' + count +
        (count === 1 ? ' image' : ' images') + '</span></div>';
      var body;
      if (!count) {
        body = '<div class="cv-bg-unavailable">No image available</div>';
      } else if (count === 1) {
        body = '<label class="cv-bg-single"><span class="cv-bg-thumb"></span>' +
          '<span class="cv-bg-single-name">' + esc(list[0].label || list[0].id) +
          '</span><input type="checkbox" data-cv-bg-space="' + esc(sp.id) + '"' +
          (sp._customImageId === IMG_NONE ? '' : ' checked') + '><span class="cv-bg-switch"></span></label>';
      } else {
        var opts = ['<option value="' + IMG_NONE + '">None</option>'].concat(
          list.map(function (im) { return '<option value="' + esc(im.id) + '">' +
            esc(im.label || im.id) + '</option>'; })
        );
        body = '<select data-cv-bg-space="' + esc(sp.id) + '">' + opts.join('') + '</select>';
      }
      return '<div class="cv-bg-row">' + heading + body + '</div>';
    }).join('') : '';
    (active ? [active] : []).forEach(function (sp) {
      var input = pop.querySelector('[data-cv-bg-space="' + cssEscape(sp.id) + '"]');
      if (!input) return;
      if (input.tagName === 'SELECT') input.value = sp._customImageId || spatialImages(sp)[0].id;
      input.onchange = function () {
        var id = input.type === 'checkbox'
          ? (input.checked ? spatialImages(sp)[0].id : IMG_NONE)
          : input.value;
        setSpatialImage(id, sp);
      };
    });
  }

  // Switch background. The cells have not moved -- only what is behind them --
  // so the viewport is left exactly as it is; re-fitting it here would throw
  // away the zoom the user was comparing at.
  function setSpatialImage(id, sp) {
    sp = sp || activeSpatial();
    if (!sp || !id) return;
    stashImgState(sp);
    sp._customImageId = id;
    imgChoice[backgroundStateKey(sp)] = id;
    backgroundModes[backgroundStateKey(sp)] = 'custom';
    loadSpaceImage(sp);
    if (sp.id === activeSpatialId) {
      seedImgControls();
      seedPointAppearanceFromImage(sp);
    }
    renderImagePicker();
    updateSpaceScopedControls();
    drawAll();
  }

  function setBackgroundMode(mode, sp) {
    if (['auto', 'none', 'custom'].indexOf(mode) < 0) return;
    sp = sp || activeSpatial();
    if (!sp) return;
    stashImgState(sp);
    backgroundModes[backgroundStateKey(sp)] = mode;
    loadSpaceImage(sp);
    renderImagePicker();
    updateSpaceScopedControls();
    seedImgControls();
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
    seedPointAppearanceFromImage(activeSpatial());
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
    var set = function (id, x) { var el = $(id); if (el) el.value = String(x); };
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
      name: spatialTemplate.label.replace(/ \(spatial\)$/, ''),
      label: spatialTemplate.label,
      x: spatialTemplate.x,
      y: spatialTemplate.y,
      image: spatialTemplate.image,
      images: spatialTemplate.images || [],
      builder_point_opacity: spatialTemplate.builder_point_opacity,
      builder_point_size: spatialTemplate.builder_point_size
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
        image: sample.image || null,
        images: images,
        builder_point_opacity: sample.builder_point_opacity,
        builder_point_size: sample.builder_point_size,
        _builderDefaultPointOpacity: sample.builder_point_opacity,
        _builderDefaultPointSize: sample.builder_point_size,
        _sampleName: sample.name,
        _spatialSample: true,
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
    if (selEl.selectize) selEl.selectize.destroy();
    var samples = spatialSamples();
    if (!samples.length) {
      ctl.style.display = 'none'; selEl.innerHTML = ''; return;
    }
    ctl.style.display = samples.length > 1 ? '' : 'none';
    selEl.innerHTML = samples.map(function (s) {
      return '<option value="' + esc(s.name) + '">' + esc(s.name) + '</option>';
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
      wrap.innerHTML = '<button type="button" class="cv-filt-btn">' +
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
    var ct = wrap.querySelector('.cv-filt-ct');
    if (ct) ct.textContent = (groupFilter[gname] ? chosen.size : total) + '/' + total;
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
      syv = sxv; if (syEl) syEl.value = String(sxv);
    } else if (lock && changed === 'cv-img-scaley') {
      sxv = syv; if (sxEl) sxEl.value = String(syv);
    } else if (lock && changed === 'cv-img-lock') {
      syv = sxv; if (syEl) syEl.value = String(sxv);
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
  function orderedSpaces() {
    if (singleActive && singleSpaceIds.length) return singleSpaceIds.slice();
    var out = [];
    selectedProjections.forEach(function (name) {
      var id = projectionId(name); if (spaceById[id]) out.push(id);
    });
    D.spaces.forEach(function (space) {
      if (space.trajectory && spaceById[space.id]) out.push(space.id);
    });
    selectedSpatial.forEach(function (name) {
      var id = spatialId(name); if (spaceById[id]) out.push(id);
    });
    ['trekker', 'clone'].forEach(function (id) {
      if (spaceById[id]) out.push(id);
    });
    D.spaces.forEach(function (s) {
      if (s.id !== 'umap' && s.id !== 'spatial' && out.indexOf(s.id) < 0) {
        out.push(s.id);
      }
    });
    return out;
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
    // A new data set is a new set of spaces; whichever panel had the grid may
    // not even hold the same one now.
    focusPanel = null;
    panels.forEach(function (p) {
      if (p.pane) {
        p.pane.classList.remove('cv-focus-primary', 'cv-focus-context');
        p.pane.style.order = '';
        p.pane.style.gridColumn = '';
        p.pane.style.gridRow = '';
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
        p.colorBy = singleActive ? singleSpaceModes[p.spaceId] || null : null;
        clearPanelView(p);
        if (p.pane) {
          p.pane.classList.remove('cv-hidden');
          if (reappearing) fadeInPane(p.pane);
        }
        var title = $('cv-title-' + p.key.toLowerCase());
        var shownSpace = spaceById[p.spaceId];
        if (title) title.textContent = shownSpace ? shownSpace.label : p.spaceId;
      } else {
        p.spaceId = null;
        p.colorBy = null;
        if (p.pane) p.pane.classList.add('cv-hidden');
      }
    });
    updateMoranBadges();
    // Trekker info button lives on the Trekker panel only.
    var showTk = !!(D.trekker && D.trekker.qc);
    panels.forEach(function (p) {
      var btn = $('cv-tk-info-' + p.key.toLowerCase());
      if (btn) btn.style.display = (showTk && p.spaceId === 'trekker') ? '' : 'none';
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

  // Find the rows/columns that produce the largest complete square when BOTH
  // dimensions are finite. This is deliberately independent of panel order:
  // adding a linked space changes only the packing, never the visual geometry.
  function bestOverviewGrid(panelCount, availW, availH, chromeX, chromeY, gap) {
    var best = { cols: 1, rows: panelCount, side: 0 };
    for (var cols = 1; cols <= panelCount; cols++) {
      var rows = Math.ceil(panelCount / cols);
      var widthSide = Math.floor(
        (availW - (cols - 1) * gap) / cols - chromeX
      );
      var heightSide = Math.floor(
        (availH - (rows - 1) * gap) / rows - chromeY
      );
      var candidate = Math.min(widthSide, heightSide);
      if (candidate > best.side ||
          (candidate === best.side && rows < best.rows)) {
        best = { cols: cols, rows: rows, side: candidate };
      }
    }
    return best;
  }

  function resizeAll() {
    if (!D || !panels.length) return;
    var panes = panels[0].pane && panels[0].pane.parentElement;
    if (!panes) return;
    var vis = panels.filter(function (p) { return p.spaceId; });
    var k = vis.length;
    if (!k) return;
    var availW = panes.clientWidth;
    if (availW < 20) return;                 // tab still hidden; observer re-runs
    var key = layoutInputs(panes);
    if (key === _layoutKey) {
      if (_layoutPass >= 2) return;
      _layoutPass++;
    } else {
      _layoutKey = key;
      _layoutPass = 1;
    }
    var gap = 14;
    // Each pane is border-box with 12px padding + 1px border, so its content is
    // 26px narrower than its column track. The canvas takes that full width;
    // height is still fitted to the viewport by the existing overview logic.
    var chromeX = 26;
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
    var overviewFitsFloor = overview && overview.side >= MIN_SIDE;
    var cols = focusPanel
      ? Math.max(1, Math.min(k + 1, availableCols))
      : (overviewFitsFloor
        ? overview.cols : Math.max(1, Math.min(k, availableCols)));
    var single = cols === 1;
    var colW = (usableW - (cols - 1) * gap) / cols;
    var side = focusPanel
      ? Math.max(FOCUS_CONTEXT_SIDE, Math.floor(colW - chromeX))
      : (overviewFitsFloor
        ? overview.side : Math.max(MIN_SIDE, Math.floor(colW - chromeX)));
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
    panes.classList.remove('cv-n2', 'cv-n3', 'cv-n4', 'cv-single');
    panes.classList.add(single ? 'cv-single' : ('cv-n' + k));
    var col = []; for (var c = 0; c < cols; c++) col.push(colW + 'px');
    panes.style.gridTemplateColumns = col.join(' ');
    panes.style.gridTemplateRows = '';
    var panelWidth = Math.max(150, Math.floor(colW - chromeX));
    var rows = Math.ceil(k / cols);
    var panelHeight = Math.max(
      MIN_SIDE,
      Math.floor((availH - (rows - 1) * gap) / rows - overhead)
    );
    var primaryWidth = cols > 1
      ? Math.max(panelWidth, Math.floor(colW * 2 + gap - chromeX))
      : panelWidth;
    vis.forEach(function (p) {
      var primary = !!focusPanel && p.key === focusPanel;
      if (p.pane) {
        p.pane.style.gridColumn = primary && cols > 1 ? 'span 2' : '';
        // With three or more tracks the large lens occupies a genuine 2×2 bento
        // cell; the smaller lenses flow into the open tracks around it.
        p.pane.style.gridRow = primary && cols > 2 ? 'span 2' : '';
      }
      var width = primary ? primaryWidth : panelWidth;
      var height = primary ? focusSide : (focusPanel ? side : panelHeight);
      if (keepPlotsSquare) {
        var squareSide = Math.min(width, height);
        width = squareSide;
        height = squareSide;
      }
      resizePanel(p, width, height);
    });
    if (!psSeeded) autoPointSize(side);
    // Visibility of focus affordances depends on how many linked lenses remain.
    updateFocusButtons();
    drawAll();
    if ((sel && sel.size) || (pick != null && nicheSet)) renderSelbar();
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
    zoomed = false; zoomedSpace = null; hidden = new Set(); groupFilter = {};
    panels.forEach(function (p) {
      p.spaceId = null; p.sx = null; p.sy = null; p.ok = null;
      p.lasso = null; p.lassoData = null; p.view = null;
      p.miniBg = null; p.miniUnit = null;
      if (p.mini) p.mini.classList.remove('is-on');
      if (p.ctx) p.ctx.clearRect(0, 0, p.W, p.H);
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
      R.innerHTML = '<div class="cv-empty">Nothing to show for this data set.</div>';
    }
    ['cv-workspace-guide', 'cv-selbar', 'cv-selactions', 'cv-shown', 'cv-trekker-ctl',
      'cv-tk-insights', 'cv-clone-layout-ctl'].forEach(function (id) {
      var el = $(id); if (el) el.style.display = 'none';
    });
    setTrekkerInsightsOpen(false);
    setMoreOpen(false);
    reportSelection();
  }

  function applyColorPatch(patch) {
    if (!D || !patch || patch.dataset_id !== D.dataset_id) return false;
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
    var hosts = document.querySelectorAll('[data-cell-view-id]');
    for (var i = 0; i < hosts.length; i++) {
      if (hosts[i].offsetParent !== null) return hosts[i].dataset.cellViewId;
    }
    return null;
  }
  function rememberSurfaceHome() {
    if (surfaceHome) return;
    var legend = $('cv-legend'), cbar = $('cv-cbar');
    var panes = document.querySelector('#coordviews .cv-panes') ||
      document.querySelector('.coordviews-page .cv-panes');
    if (!panes) return;
    surfaceHome = {
      panes: panes, panesParent: panes.parentNode, panesNext: panes.nextSibling,
      legend: legend, legendParent: legend && legend.parentNode,
      legendNext: legend && legend.nextSibling,
      cbar: cbar, cbarParent: cbar && cbar.parentNode,
      cbarNext: cbar && cbar.nextSibling
    };
  }
  function restoreNode(node, parent, next) {
    if (!node || !parent || node.parentNode === parent) return;
    parent.insertBefore(node, next && next.parentNode === parent ? next : null);
  }
  function restoreLinkedSurface() {
    if (!surfaceHome) return;
    restoreNode(surfaceHome.panes, surfaceHome.panesParent, surfaceHome.panesNext);
    restoreNode(surfaceHome.cbar, surfaceHome.cbarParent, surfaceHome.cbarNext);
    restoreNode(surfaceHome.legend, surfaceHome.legendParent, surfaceHome.legendNext);
  }
  function resetSingleViews() {
    restoreLinkedSurface();
    singleViews = {}; singleActive = null;
    singleRequests.clear();
    singleSpaceIds = []; singleSpaceModes = {};
    singleIndexCells = null; singleIndexMap = null;
    linkedState = null;
  }
  function mountSingleSurface(id) {
    rememberSurfaceHome();
    var host = singleHost(id), surface = host && host.querySelector('.cerebro-cell-view-surface');
    if (!surface || !surfaceHome) return false;
    surface.innerHTML = '';
    var legend = document.createElement('div');
    legend.className = 'cerebro-cell-view-legend';
    surface.appendChild(legend); surface.appendChild(surfaceHome.panes);
    if (surfaceHome.legend) legend.appendChild(surfaceHome.legend);
    if (surfaceHome.cbar) legend.appendChild(surfaceHome.cbar);
    return true;
  }
  function singleIndex() {
    var cells = linkedBundle && linkedBundle.cells;
    if (cells === singleIndexCells && singleIndexMap) return singleIndexMap;
    var out = new Map();
    if (Array.isArray(cells)) {
      cells.forEach(function (cell, i) { out.set(String(cell), i); });
    }
    singleIndexCells = cells; singleIndexMap = out;
    return singleIndexMap;
  }
  function emptyVector(value) {
    return new Array(linkedBundle.n || (linkedBundle.cells || []).length).fill(value);
  }
  function alignSingleCoordinates(data, nested) {
    var index = singleIndex();
    var x = emptyVector(null), y = emptyVector(null), z = emptyVector(null);
    var groups = emptyVector(-1), levels = [], colors = [];
    var hover = emptyVector('');
    var hoverEnabled = emptyVector(true);
    if (nested) {
      var traces = Array.isArray(data.meta.traces) ? data.meta.traces : [];
      var hoverModes = data.hover && data.hover.hoverinfo;
      traces.forEach(function (name, group) {
        levels.push(String(name));
        var colour = data.data.color && data.data.color[group];
        colors.push(String(Array.isArray(colour) ? colour[0] : colour || '#7b8794'));
        var gx = data.data.x[group] || [], gy = data.data.y[group] || [];
        var gz = data.data.z && data.data.z[group] || [];
        var gk = data.data.selection_key && data.data.selection_key[group] || [];
        var gh = data.hover && data.hover.text && data.hover.text[group] || [];
        var hoverMode = Array.isArray(hoverModes) ? hoverModes[group] : hoverModes;
        for (var j = 0; j < gx.length; j++) {
          var at = index.get(String(gk[j])); if (at == null) continue;
          x[at] = Number(gx[j]); y[at] = Number(gy[j]);
          if (gz[j] != null) z[at] = Number(gz[j]);
          groups[at] = group; hover[at] = gh[j] || '';
          hoverEnabled[at] = hoverMode !== 'skip';
        }
      });
    } else {
      var keys = Array.isArray(data.data.selection_key)
        ? data.data.selection_key : [];
      var hx = data.data.x || [], hy = data.data.y || [], hz = data.data.z || [];
      var hh = data.hover && Array.isArray(data.hover.text) ? data.hover.text : [];
      var enabled = !(data.hover && data.hover.hoverinfo === 'skip');
      for (var k = 0; k < hx.length; k++) {
        var pos = index.get(String(keys[k])); if (pos == null) continue;
        x[pos] = Number(hx[k]); y[pos] = Number(hy[k]);
        if (hz[k] != null) z[pos] = Number(hz[k]);
        hover[pos] = hh[k] || '';
        hoverEnabled[pos] = enabled;
      }
    }
    return { x: x, y: y, z: z, groups: groups,
      levels: levels, colors: colors, hover: hover, hoverEnabled: hoverEnabled };
  }
  function quantisedField(label, raw, keys, data) {
    var index = singleIndex(), values = emptyVector(null), min = Infinity, max = -Infinity;
    for (var i = 0; i < raw.length; i++) {
      var at = index.get(String(keys[i]));
      var value = Number(raw[i]);
      if (at == null || !isFinite(value)) continue;
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
    var q = values.map(function (value) {
      if (value == null) return null;
      return Math.round(Math.max(0, Math.min(1,
        (value - min) / (max - min))) * 1000);
    });
    return { label: label, v: q, raw: values, min: min, max: max, scale: 1000,
      unclipped: true,
      palette: singlePalette(data && data.colorscale, data && data.reversescale) };
  }
  function directColorField(meta, raw, keys) {
    var index = singleIndex(), colors = emptyVector('#e6e7ea');
    for (var i = 0; i < raw.length; i++) {
      var at = index.get(String(keys[i]));
      if (at != null) colors[at] = cssColor(raw[i], '#e6e7ea');
    }
    return {
      label: meta.color_variable || 'Co-expression', colors: colors,
      channelLabels: Array.isArray(meta.traces) ? meta.traces : [],
      channelColors: Array.isArray(meta.coexpr_colors) ? meta.coexpr_colors : []
    };
  }
  function singleEdges(extra) {
    var shapes = extra && Array.isArray(extra.shapes) ? extra.shapes : [];
    return shapes.filter(function (shape) {
      return shape && shape.x0 != null && shape.y0 != null &&
        shape.x1 != null && shape.y1 != null;
    }).map(function (shape) {
      return [Number(shape.x0), Number(shape.y0), Number(shape.x1), Number(shape.y1)];
    });
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
  function buildSingleSpaces(id, payload) {
    var meta = payload.meta || {}, data = payload.data || {}, extra = payload.extra || {};
    var categorical = meta.color_type === 'categorical';
    var aligned = alignSingleCoordinates({ meta: meta, data: data, hover: payload.hover }, categorical);
    var baseId = 'single::' + id, spaces = [], modes = {};
    var edges = singleEdges(extra);
    var hulls = singleHulls(extra);
    var hasZ = aligned.z.some(function (value) { return value != null; });
    var hasHover = aligned.hoverEnabled.some(Boolean);
    var makeSpace = function (spaceId, label) {
      var space = { id: spaceId, label: label || id, x: aligned.x, y: aligned.y };
      space._projectionName = spaceId;
      space._hover = aligned.hover;
      space._hoverEnabled = hasHover;
      space._hoverMask = aligned.hoverEnabled;
      if (hasZ) space.z = aligned.z;
      if (!hasZ && !data.reset_axes) {
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
      if (edges.length) { space.trajectory = true; space.edges = edges; }
      if (hulls.length) space.hulls = hulls;
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
              opacity: isFinite(opacity) ? opacity : 0.6,
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
        values: aligned.groups, levels: aligned.levels, colors: aligned.colors
      };
      makeSpace(baseId, meta.space_label || id);
      modes[baseId] = groupName;
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
      var multiple = data.color && !Array.isArray(data.color) && typeof data.color === 'object';
      var entries = multiple ? Object.keys(data.color).map(function (name) {
        return [name, data.color[name]];
      }) : [[meta.color_variable || 'Value', data.color || []]];
      entries.forEach(function (entry, panel) {
        var fieldName = 'single:' + id + ':' + panel;
        D.fields[fieldName] = quantisedField(entry[0], entry[1] || [], keys, data);
        var spaceId = entries.length === 1 ? baseId : baseId + ':' + panel;
        makeSpace(spaceId, entry[0]); modes[spaceId] = FIELD_PREFIX + fieldName;
      });
    }
    return { spaces: spaces, modes: modes };
  }
  function stashSingleState() {
    if (!singleActive || !singleViews[singleActive]) return;
    var view = singleViews[singleActive];
    view.selection = selectedCellIds();
    var group = catOf(singleSpaceModes[singleSpaceIds[0]]);
    view.hiddenGroups = group ? Array.from(hidden).map(function (index) {
      return group.levels[index];
    }).filter(function (name) { return name != null; }) : [];
    view.mode = selectMode;
    view.lenses = panels.filter(function (p) { return p.spaceId; }).map(function (p) {
      return { view: p.view && Object.assign({}, p.view), rot: p.rot && Object.assign({}, p.rot),
        lassoData: p.lassoData && p.lassoData.map(function (q) { return q.slice(); }) };
    });
  }
  function activateSingle(id, resetAxes) {
    var payload = singleViews[id];
    if (!payload || !linkedBundle || rebuildingBase) return false;
    if (!singleActive && !linkedState) linkedState = exportWorkspace();
    stashSingleState();
    if (resetAxes) payload.lenses = [];
    restoreLinkedSurface();
    singleActive = id; singleSpaceIds = []; singleSpaceModes = {};
    if (!mountSingleSurface(id)) { singleActive = null; return false; }
    D = Object.assign({}, linkedBundle, {
      fields: Object.assign({}, linkedBundle.fields || {}),
      cat_extra: Object.assign({}, linkedBundle.cat_extra || {})
    });
    spaceById = {}; spatialTemplate = null; _clipD = null;
    var built = buildSingleSpaces(id, payload);
    payload.data.reset_axes = false;
    singleSpaceIds = built.spaces; singleSpaceModes = built.modes;
    sanitiseColors(D);
    singleSpaceIds.forEach(function (spaceId) {
      var space = spaceById[spaceId]; if (isSpatialSpace(space)) loadSpaceImage(space);
    });
    if (payload.backgroundState) updateSingleBackground(id, payload.backgroundState);
    colorBy = singleSpaceModes[singleSpaceIds[0]] || colorBy;
    hidden = new Set(); groupFilter = {}; sel = null; selectionSource = null;
    selectionSourceSpace = null;
    pick = null; hoverCell = null; focusPanel = null;
    zoomed = false; zoomedSpace = null;
    pctShow = 100; pctMask = null; keepPlotsSquare = false;
    dissolvePct = 0; dissolveThresh = null; evidenceOn = false; nicheSet = null;
    unpinTip(); closeCard(); cardMeta = null;
    var group = catOf(singleSpaceModes[singleSpaceIds[0]]);
    if (group && Array.isArray(payload.hiddenGroups)) {
      payload.hiddenGroups.forEach(function (name) {
        var index = group.levels.indexOf(String(name)); if (index >= 0) hidden.add(index);
      });
    }
    ps = Math.max(0.5, Number(payload.data && payload.data.point_size) || ps * 2) / 2;
    pointOpacity = Math.max(0.05,
      Math.min(1, Number(payload.data && payload.data.point_opacity) || pointOpacity));
    psSeeded = pointSizeEdited = pointOpacityEdited = true;
    ensurePanelSlots(singleSpaceIds.length); buildPanels(); layoutPanels();
    selectMode = payload.mode || 'lasso'; syncModeButtons(); syncCursors();
    var saved = payload.lenses || [];
    panels.forEach(function (p, index) {
      if (!p.spaceId || !saved[index]) return;
      p.view = saved[index].view || null; p.rot = saved[index].rot || null;
      p.lassoData = saved[index].lassoData || null; project(p);
    });
    if (Array.isArray(payload.selection) && payload.selection.length) {
      var cells = singleIndex(), restored = new Set();
      payload.selection.forEach(function (cell) {
        var at = cells.get(String(cell)); if (at != null) restored.add(at);
      });
      sel = restored.size ? restored : null;
    }
    renderLegend(); resizeAll(); renderSelbar(); drawAll();
    reportSingleHiddenGroups(); reportSelection(); reportSingleZoom();
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
    linkedState = null;
  }

  function registerSingle(id) {
    if (!id) return null;
    if (!singleViews[id]) singleViews[id] = { id: id };
    return singleViews[id];
  }

  function renderSingle(id, meta, data, hover, extra) {
    var previous = registerSingle(id); if (!previous) return;
    singleRequests.delete(id);
    meta = meta || {}; data = data || {};
    var changedGroup = previous.meta && previous.meta.color_variable !== meta.color_variable;
    singleViews[id] = Object.assign(previous, {
      id: id, meta: meta || {}, data: data || {}, hover: hover || {},
      extra: extra || {}
    });
    if (changedGroup) singleViews[id].hiddenGroups = [];
    if (data.reset_axes) singleViews[id].lenses = [];
    var pending = singleViews[id].pendingSavedState;
    if (pending) {
      singleViews[id].pendingSavedState = null;
      applySingleState(id, pending);
    } else if (visibleSingleId() === id) {
      activateSingle(id, !!data.reset_axes);
    }
  }

  function clearSingleSelection(id) {
    var view = registerSingle(id); if (!view) return;
    view.selection = []; view.lenses = (view.lenses || []).map(function (lens) {
      return Object.assign({}, lens, { lassoData: null });
    });
    if (singleActive === id) {
      pick = null; unpinTip(); closeCard(); clearLassos(); setSelection(null);
      reportSingleZoom();
    }
  }

  function toggleSingleZoom(id) {
    if (singleActive !== id) return;
    var panel = panels.find(function (candidate) { return !!candidate.spaceId; });
    if (!panel || panelIs3D(panel)) return;
    if (panel.view) {
      panel.view = null; project(panel); drawAll();
    } else {
      zoomToSelection(panel);
    }
    zoomed = !!panel.view;
    zoomedSpace = zoomed ? panel.spaceId : null;
    updateZoomBtn(); reportSingleZoom();
  }

  function captureSingleState(id) {
    var view = singleViews[id];
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
    if (visibleSingleId() === id) activateSingle(id);
    return { selectedCells: target.selection.length };
  }

  function updateSingleBackground(id, values) {
    var view = singleViews[id]; if (!view) return;
    values = values || {};
    var names = {
      opacity: 'opacity', offsetX: 'offsetX', offsetY: 'offsetY',
      scaleX: 'scaleX', scaleY: 'scaleY', flipX: 'flipX',
      flipY: 'flipY', rotate: 'rotate'
    };
    view.backgroundState = Object.assign({}, view.backgroundState || {}, values);
    if (singleActive !== id) return;
    singleSpaceIds.forEach(function (spaceId) {
      var space = spaceById[spaceId]; if (!space || !space._imgState) return;
      Object.keys(names).forEach(function (name) {
        if (values[name] != null) space._imgState[names[name]] = values[name];
      });
      stashImgState(space);
    });
    drawAll();
  }

  function onData(bundle) {
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
    sanitiseColors(D);
    syncCloneTiers();
    _clipD = null;   // ranges belong to the data set that produced them
    // Image ids belong to the object that produced them, so alignment stored
    // under them is dropped when the DATA SET changes -- not when a bundle
    // arrives. A bundle can be re-sent when returning to the tab; clearing on
    // every push meant a user's alignment work survived only until they looked
    // away.
    var dataChanged = D.dataset_id !== dataShown;
    var previousSelected = selectedSpatial.slice();
    var previousProjections = selectedProjections.slice();
    var previousActiveName = activeSpatial() && activeSpatial()._sampleName;
    if (dataChanged) {
      resetSingleViews();
      imgStates = {};
      imgChoice = {};
    }
    dataShown = D.dataset_id;
    imgToken++;
    closeCard(); cardMeta = null;   // the card described the previous data set
    spaceById = {};
    spatialTemplate = null;
    D.spaces.forEach(function (s) {
      s._unit = null;
      if (s.id === 'spatial') spatialTemplate = s;
      else if (s.id === 'umap') { /* rebuilt from D.projections below */ }
      else {
        spaceById[s.id] = s;
        if (s.background_scope) s._sampleName = s.background_scope;
      }
    });
    var projectionNames = D.projections ? Object.keys(D.projections) : [];
    selectedProjections = dataChanged ? [] : previousProjections.filter(function (name) {
      return projectionNames.indexOf(name) >= 0;
    });
    if (!selectedProjections.length && projectionNames.length) {
      selectedProjections = [D.default_projection &&
        projectionNames.indexOf(D.default_projection) >= 0
        ? D.default_projection : projectionNames[0]];
    }
    rebuildProjectionInstances();
    var initialSamples = spatialSamples();
    var availableSamples = initialSamples.map(function (sample) { return sample.name; });
    selectedSpatial = dataChanged ? [] : previousSelected.filter(function (name) {
      return availableSamples.indexOf(name) >= 0;
    });
    if (!selectedSpatial.length && initialSamples.length) {
      selectedSpatial = [initialSamples[0].name];
    }
    var activeName = !dataChanged && selectedSpatial.indexOf(previousActiveName) >= 0
      ? previousActiveName : selectedSpatial[0];
    activeSpatialId = activeName ? spatialId(activeName) : null;
    if (dataChanged) backgroundModes = {};
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
      groupFilter = {};
      var configuredOpacity = Number(D.default_point_opacity);
      pointOpacity = D.default_point_opacity != null && isFinite(configuredOpacity)
        ? Math.max(0, Math.min(1, configuredOpacity)) : 0.8;
      pointSizeEdited = false;
      pointOpacityEdited = false;
      psSeeded = false;
      var opEl = $('cv-opacity'); if (opEl) opEl.value = String(pointOpacity);
      var opLbl = $('cv-op-val');
      if (opLbl) opLbl.textContent = pointOpacity.toFixed(2);

      var configuredPercentage = Number(D.default_percentage_cells_to_show);
      pctShow = D.default_percentage_cells_to_show != null &&
        isFinite(configuredPercentage)
        ? Math.max(10, Math.min(100, configuredPercentage)) : 100;
      rebuildPctMask();
      var pctEl = $('cv-pct'); if (pctEl) pctEl.value = String(pctShow);
      var pctLbl = $('cv-pct-val'); if (pctLbl) pctLbl.textContent = String(pctShow);
    }
    setMoreOpen(false);   // a new data set starts with advanced settings closed
    // Trekker controls reset
    dissolvePct = 0; dissolveThresh = null; evidenceOn = false; nicheRadius = 250;
    nicheSet = null;
    var dsEl = $('cv-dissolve'); if (dsEl) dsEl.value = '0';
    var dsLbl = $('cv-dissolve-val'); if (dsLbl) dsLbl.textContent = '0';
    var evEl = $('cv-evidence'); if (evEl) evEl.checked = false;
    var nkEl = $('cv-niche'); if (nkEl) nkEl.value = '250';
    var nkLbl = $('cv-niche-val'); if (nkLbl) nkLbl.textContent = '250';

    ensurePanelSlots(orderedSpaces().length);
    renderMeta();

    buildPanels();
    // Give every present space its own panel and hide the unused slots (this also
    // places the Trekker info button on the Trekker panel).
    layoutPanels();
    zoomed = false; zoomedSpace = null; updateZoomBtn(); syncModeButtons();
    updateSelActions();   // hide the Zoom / Clear group until a selection exists
    updateZselButtons();
    // Immune axis present → reveal the clonal-layout switch and lay out the
    // clone space (client-owned) before the first projection runs.
    var hasClone = !!spaceById['clone'];
    if (hasClone) { setSegOn('stack'); applyCloneLayout('stack'); }
    // Show the space-scoped controls that match the spaces now on screen.
    updateSpaceScopedControls();
    // Trekker controls (dissolve + evidence) appear only when the bundle has them
    var hasTk = !!(D.trekker && (D.trekker.conf || D.trekker.evidence));
    var tkc = $('cv-trekker-ctl');
    if (tkc) tkc.style.display = hasTk ? '' : 'none';
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
    renderSelbar(); renderReadout();
    // Clear the server-side selection too: sel was reset to null above, but the
    // server input still holds the PREVIOUS dataset's barcodes until we push. The
    // "selected cells" plot/table (server-rendered) key off it, so without this
    // they'd show stale/wrong cells after a dataset switch.
    reportSelection();
    positionAllRangeVals();
    if (pendingColorPatch && applyColorPatch(pendingColorPatch)) {
      pendingColorPatch = null;
    }
    reportWorkspaceReady();
    if (!rebuildingBase) {
      var singleId = visibleSingleId();
      if (singleId && singleViews[singleId]) activateSingle(singleId);
    }
  }

  // ---- portable local workspace ------------------------------------------
  // The JSON contains identities and interaction state only; no expression,
  // coordinates, images, receptor sequences or other source data leave D.
  function configFingerprint() {
    if (!D || !Array.isArray(D.cells)) return '';
    if (typeof D.dataset_fingerprint === 'string' && D.dataset_fingerprint) {
      return D.dataset_fingerprint;
    }
    if (typeof D.cell_fingerprint === 'string' && D.cell_fingerprint) {
      return D.cell_fingerprint;
    }
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
    if (sel) sel.forEach(function (index) { out.push(D.cells[index]); });
    return out;
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
      var mode = backgroundModeFor(space);
      var selectedImage = mode === 'custom' ? space._customImageId : null;
      var image = currentImage(space) || images.filter(function (item) {
        return item.id === selectedImage;
      })[0] || images[0];
      var key = imgKey(space, image);
      var state = space._imgState || (key && imgStates[key]) || presetState(image);
      return {
        section: name,
        mode: mode === 'custom' ? 'image' : mode,
        image_id: mode === 'custom' ? selectedImage : null,
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
        source: selectionSource || null,
        geometry: savedGeometry()
      },
      view: {
        projections: selectedProjections.slice(),
        spatial_sections: selectedSpatial.slice(),
        active_spatial: active && active._sampleName || null,
        colour: {
          mode: colorBy,
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
    if (!(catOf(mode) || fieldForMode(mode) || mode === GENE_MODE || mode === RGB_MODE)) {
      configError('The file uses a colour mode that is unavailable here.');
    }
    Object.keys(view.filters).forEach(function (name) {
      if (!D.groups || !D.groups[name]) configError('The file contains an invalid filter.');
    });

    var spaces = new Set((D.spaces || []).map(function (space) { return space.id; }));
    view.projections.forEach(function (name) { spaces.add(projectionId(name)); });
    view.spatial_sections.forEach(function (name) { spaces.add(spatialId(name)); });
    if (view.lenses.some(function (lens) { return !spaces.has(lens.space); })) {
      configError('The file contains an invalid view lens.');
    }

    var cells = singleIndex();
    if (config.selection.cells.some(function (cell) { return !cells.has(String(cell)); })) {
      configError('The file selects cells that are unavailable here.');
    }
    return { cells: cells };
  }

  function restoreWorkspace(config) {
    var validated = validateWorkspace(config);
    var view = config.view || {}, selection = config.selection || {};
    var restoredSelection = new Set();
    selection.cells.forEach(function (cell) {
      restoredSelection.add(validated.cells.get(String(cell)));
    });

    setSelectedProjections(view.projections);
    setSelectedSpatial(view.spatial_sections);
    (view.spatial_backgrounds || []).forEach(function (saved) {
      var space = spaceById[spatialId(saved.section)];
      if (!space) return;
      var images = spatialImages(space);
      backgroundModes[backgroundStateKey(space)] = saved.mode === 'image' ? 'custom' : saved.mode;
      space._customImageId = saved.image_id || ((images[0] && images[0].id) || IMG_NONE);
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
    if (colour === GENE_MODE && colourState.gene) {
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
    psSeeded = pointSizeEdited = pointOpacityEdited = true;
    labelsOn = display.group_labels !== false;
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
      var el = $(item[0]); if (el) el.value = String(item[1]);
    });
    var psLabel = $('cv-ps-val'); if (psLabel) psLabel.textContent = ps.toFixed(1);
    var opacityLabel = $('cv-op-val'); if (opacityLabel) opacityLabel.textContent = pointOpacity.toFixed(2);
    var pctLabel = $('cv-pct-val'); if (pctLabel) pctLabel.textContent = String(pctShow);
    var dissolveLabel = $('cv-dissolve-val'); if (dissolveLabel) dissolveLabel.textContent = String(dissolvePct);
    var nicheLabel = $('cv-niche-val'); if (nicheLabel) nicheLabel.textContent = String(nicheRadius);
    var labelsEl = $('cv-labels'); if (labelsEl) labelsEl.checked = labelsOn;
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
    _layoutKey = null; resizeAll(); positionAllRangeVals();
    setSelection(
      restoredSelection,
      geometry && geometry.space
        ? { spaceId: geometry.space }
        : (selection.source || 'Saved view')
    );
    return { selected_cells: restoredSelection.size };
  }

  function workspaceSummary() {
    return {
      ready: !!(D && configFingerprint()),
      datasetFingerprint: D ? configFingerprint() : null,
      selectedCells: sel ? sel.size : 0,
      selectedCellBarcodes: selectedCellIds(),
      colourMode: colorBy,
      projections: selectedProjections.slice(),
      spatialSections: selectedSpatial.slice(),
      activeSpatial: D && activeSpatial() ? activeSpatial()._sampleName || null : null
    };
  }

  function applyWorkspace(config, colourData) {
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
    }
    var result = restoreWorkspace(config);
    return { selectedCells: result.selected_cells };
  }

  function reportWorkspaceReady() {
    var summary = workspaceSummary();
    window.dispatchEvent(new CustomEvent('cerebro:linkedviews-ready', {
      detail: {
        ready: summary.ready,
        selectedCells: summary.selectedCells
      }
    }));
  }

  window.cerebroLinkedViewsState = Object.freeze({
    ready: function () { return workspaceSummary().ready; },
    capture: exportWorkspace,
    apply: applyWorkspace,
    summary: workspaceSummary
  });

  window.cerebroCellViews = Object.freeze({
    captureState: captureSingleState,
    applyState: applySingleState
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
    Shiny.addCustomMessageHandler('coordviews_data', onData);
    Shiny.addCustomMessageHandler('coordviews_colors', function (patch) {
      if (!applyColorPatch(patch)) pendingColorPatch = patch;
    });
    Shiny.addCustomMessageHandler('cell_view_render', function (message) {
      if (!message || !message.id) return;
      renderSingle(
        message.id,
        message.meta,
        message.data,
        message.hover,
        message.extra
      );
    });
    Shiny.addCustomMessageHandler('cell_view_background', function (message) {
      if (!message || !message.id) return;
      updateSingleBackground(message.id, message.values);
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
      if (!D || !m) return;
      if (geneWanted != null && m.gene !== geneWanted) return;   // stale reply
      geneWanted = null;
      if (!m.ok) {
        // Nothing to draw. Say so rather than keep showing the last gene.
        D.gene = null;
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
    // The exact meta row behind the open detail card. Ignored if the card has
    // since moved on to another cell (or closed) — a slow reply must not
    // repaint a card that is now describing something else.
    Shiny.addCustomMessageHandler('coordviews_cell_meta', function (m) {
      if (!m || !m.cell) return;
      cardMeta = { cell: m.cell, rows: m.rows || [] };
      if (cardOpen() && D && D.cells[cardCell] === m.cell) renderCard();
    });
    // Three 0-255 channels for RGB co-expression.
    Shiny.addCustomMessageHandler('coordviews_rgbval', function (m) {
      if (!D || !m || !m.ok) return;
      D.rgb = { r: m.r, g: m.g, b: m.b, genes: m.genes };
      if (colorBy === RGB_MODE) { renderLegend(); drawAll(); }
    });

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
    function reportVisibility() {
      var el = $('cv-meta');
      var linkedVis = !!(el && el.offsetParent !== null);
      var singleId = visibleSingleId();
      var vis = linkedVis || !!singleId;
      var key = linkedVis ? 'linked' : (singleId || 'hidden');
      var host = singleId && singleHost(singleId);
      var surface = host && host.querySelector('.cerebro-cell-view-surface');
      var mounted = !singleId || !!(surfaceHome && surface &&
        surface.contains(surfaceHome.panes));
      if (key === lastVis && mounted) return;
      lastVis = key;
      if (singleId && !singleViews[singleId] && !singleRequests.has(singleId)) {
        singleRequests.add(singleId);
        Shiny.setInputValue(singleId + '_render_request', Date.now(), {
          priority: 'event'
        });
      }
      if (singleId && linkedBundle && singleViews[singleId]) {
        activateSingle(singleId);
      } else if (linkedVis && singleActive) {
        activateLinked();
      }
      if (Shiny.setInputValue) {
        Shiny.setInputValue('coordviews_visible', vis);
      }
    }
    setInterval(reportVisibility, 250);
    reportVisibility();
    // The poll is the backstop; a tab switch is a click, so report on the way
    // out too. Without this the server can still believe the workspace is on
    // screen for up to one interval after the user has left it.
    document.addEventListener('click', function () {
      setTimeout(reportVisibility, 0);
    }, true);
    // A reconnect gives a fresh server session that knows nothing, so the state
    // has to be sent again rather than suppressed as unchanged.
    var onConnected = function () { lastVis = null; reportVisibility(); };
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
        positionAllImgRangeValues();
      })
        .observe(imageHost, { childList: true });
    }

    // clear button + point-size slider live in the top bar (client-owned)
    document.addEventListener('click', function (e) {
      var t = e.target;
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
      var bgMode = t && t.closest && t.closest('[data-cv-bg-mode]');
      if (bgMode) {
        var mode = bgMode.getAttribute('data-cv-bg-mode');
        var active = activeSpatial();
        var pop = $('cv-bg-popover');
        if (mode === 'custom' && backgroundModeFor(active) === 'custom') {
          if (pop) pop.classList.toggle('is-open');
        } else {
          setBackgroundMode(mode, active);
          if (pop) pop.classList.toggle('is-open', mode === 'custom');
        }
        if (mode === 'custom' && pop && pop.classList.contains('is-open')) {
          var ctl = $('cv-img-pick-ctl'), buttonBox = bgMode.getBoundingClientRect();
          var ctlBox = ctl && ctl.getBoundingClientRect();
          if (ctl && ctlBox) {
            var left = buttonBox.left - ctlBox.left;
            left = Math.max(0, Math.min(ctl.clientWidth - pop.offsetWidth, left));
            pop.style.left = left + 'px';
            pop.style.top = (buttonBox.bottom - ctlBox.top + 8) + 'px';
            pop.style.right = 'auto';
          }
        }
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
        else if (singleAct === 'zoom') toggleSingleZoom(singleId);
        return;
      }
      // per-panel modebar: select mode (box/lasso), zoom in/out, reset, PNG
      var tb = t && t.closest && t.closest('.cv-tbtn');
      if (tb) {
        var act = tb.getAttribute('data-act'), key = tb.getAttribute('data-panel');
        var pp = null;
        panels.forEach(function (p) { if (p.key === key) pp = p; });
        if (act === 'box' || act === 'lasso' || act === 'pan' || act === 'orbit') {
          selectMode = act; syncModeButtons(); return;
        }
        if (act === 'trekker-info') { openTrekkerInsights('qc', true); return; }
        if (pp) {
          if (act === 'png') { downloadPanelPNG(pp); }
          else if (act === 'zin') { zoomStep(pp, 0.8); }
          else if (act === 'zout') { zoomStep(pp, 1.25); }
          else if (act === 'focus') { setFocusPanel(pp.key); }
          else if (act === 'zsel') { zoomToSelection(pp); }
          else if (act === 'reset') {
            // rotation is part of "where you are looking" too
            if (pp.rot) { pp.rot = null; pp.miniBg = null; project(pp); }
            if (pp.view) { pp.view = null; project(pp); }
            if (pp.spaceId === zoomedSpace) {
              zoomed = false; zoomedSpace = null; updateZoomBtn();
            }
            drawAll();
          }
        }
        return;
      }
      if (t && t.closest && t.closest('#cv-card-x')) {
        pick = null; unpinTip(); closeCard(); drawAll();
        if (!sel) { rebuildNiche(); renderReadout(); }
        return;
      }
      if (t && t.id === 'cv-zoom') { toggleZoom(); return; }
      if (t && t.id === 'cv-clear') {
        pick = null; unpinTip(); closeCard(); clearLassos(); setSelection(null);
        return;
      }
      if (t && t.closest && t.closest('#cv-more-close')) {
        setMoreOpen(false);
        return;
      }
      // "More" drawer toggle
      var moreBtn = t && t.closest && t.closest('#cv-more-btn');
      if (moreBtn) {
        setMoreOpen(!isMoreOpen());
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
      // group-filter chip: open/close its level menu. Only ever one at a time —
      // several open at once overlap each other and say nothing more than one.
      var fbtn = t && t.closest && t.closest('.cv-filt-btn');
      if (fbtn) {
        var wrap0 = fbtn.parentElement;
        var menu = wrap0.querySelector('.cv-filt-menu');
        var willOpen = !!menu && menu.style.display === 'none';
        closeFilterMenus();
        if (menu) menu.style.display = willOpen ? '' : 'none';
        fbtn.classList.toggle('is-open', willOpen);
        if (willOpen) {
          window.requestAnimationFrame(function () {
            // More scrolls internally. Keep the entire popover inside that
            // scrollport so it is visible and hit-testable near either edge.
            menu.scrollIntoView({ block: 'nearest', inline: 'nearest' });
          });
        }
        return;
      }
      // group-filter All / None
      var act = t && t.closest && t.closest('.cv-filt-acts button');
      if (act) {
        var wrap = act.closest('.cv-filt'), on = act.getAttribute('data-act') === 'all';
        Array.prototype.forEach.call(
          wrap.querySelectorAll('.cv-filt-menu input[type=checkbox]'),
          function (b) { b.checked = on; }
        );
        readFilter(wrap);
        return;
      }
    });
    document.addEventListener('input', function (e) {
      var id = e.target && e.target.id;
      if (id === 'cv-ps') {
        psSeeded = true;            // a chosen size is never overwritten
        pointSizeEdited = true;
        ps = +e.target.value;
        var lbl = $('cv-ps-val'); if (lbl) lbl.textContent = (+e.target.value).toFixed(1);
        positionRangeVal('cv-ps', 'cv-ps-val');
        drawAll();
      } else if (id === 'cv-opacity') {
        pointOpacityEdited = true;
        pointOpacity = +e.target.value;
        var ol = $('cv-op-val'); if (ol) ol.textContent = pointOpacity.toFixed(2);
        positionRangeVal('cv-opacity', 'cv-op-val');
        drawAll();
      } else if (id === 'cv-square-plots') {
        keepPlotsSquare = !!e.target.checked;
        _layoutKey = null;
        resizeAll();
      } else if (id === 'cv-pct') {
        pctShow = +e.target.value;
        var pl = $('cv-pct-val'); if (pl) pl.textContent = pctShow;
        positionRangeVal('cv-pct', 'cv-pct-val');
        rebuildPctMask(); applyActiveChange();
      } else if (id === 'cv-dissolve') {
        dissolvePct = +e.target.value;
        var dl = $('cv-dissolve-val'); if (dl) dl.textContent = dissolvePct;
        positionRangeVal('cv-dissolve', 'cv-dissolve-val');
        rebuildDissolve(); applyActiveChange();
      } else if (id === 'cv-niche') {
        nicheRadius = +e.target.value;
        var nl = $('cv-niche-val'); if (nl) nl.textContent = nicheRadius;
        positionRangeVal('cv-niche', 'cv-niche-val');
        rebuildNiche();               // resize the highlighted niche + circle
        renderSelbar();                // keep Active cell radius/count in sync
        drawAll();
        if (!sel) renderReadout();     // recompute the niche of the picked cell
      } else if (id && id.indexOf('cv-img-') === 0) {
        positionImgRangeValue(e.target); syncImgControls(id); drawAll();
      }
    });
    document.addEventListener('change', function (e) {
      var id = e.target && e.target.id;
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
      if (id === 'cv-labels') { labelsOn = e.target.checked; drawAll(); return; }
      // a group-filter level checkbox toggled
      var fwrap = e.target && e.target.closest && e.target.closest('.cv-filt');
      if (fwrap && e.target.matches && e.target.matches('.cv-filt-menu input[type=checkbox]')) {
        readFilter(fwrap);
      }
    });
    // A click anywhere outside a filter widget dismisses its menu — a popover
    // that only closes via the control that opened it is a trap. Registered
    // separately from the main click handler because that one returns early on
    // most branches, and this has to run for all of them. Clicks INSIDE the
    // widget are left alone so several levels can be ticked in one visit.
    document.addEventListener('click', function (e) {
      var t = e.target;
      var pop = $('cv-bg-popover');
      if (pop && !(t && t.closest && t.closest('.cv-bg-ctl'))) {
        pop.classList.remove('is-open');
      }
      if (t && t.closest && t.closest('.cv-filt')) return;
      closeFilterMenus();
    });
    // Escape closes the detail card — it behaves like a dialog, so it should
    // dismiss like one — and any open filter menu, for the same reason.
    document.addEventListener('keydown', function (e) {
      if (e.key !== 'Escape') return;
      var bgPop = $('cv-bg-popover');
      if (bgPop) bgPop.classList.remove('is-open');
      closeFilterMenus();
      if (!cardOpen()) return;
      pick = null; unpinTip(); closeCard(); drawAll();
      if (!sel) {
        rebuildNiche();
        updateSelActions();
        renderSelbar();
        renderReadout();
      }
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
