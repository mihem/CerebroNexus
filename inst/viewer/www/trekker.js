/*----------------------------------------------------------------------------*
 * Trekker page — client.
 *
 * The controls are standard Shiny inputs (rendered in trekker/server.R). This
 * script listens to `shiny:inputchanged` and updates the canvases client-side,
 * so dragging a slider is instant and never round-trips. The one exception is
 * gene expression, which the server holds: picking a gene returns a quantised
 * 0-255 vector aligned to these nuclei (`trekker_geneval`). Data (the `trekker`
 * slot) arrives once via the `trekker_data` message; positioning-evidence images
 * are base64 data: URIs embedded in that slot.
 *
 * A single Trekker tab exists per app, so fixed `tk-` element ids / global state
 * are safe.
 *----------------------------------------------------------------------------*/
(function () {
  "use strict";

  var D = null, N = 0;
  var view = "pair", src = "csv", mode = "celltype", gene = null, tool = "box";
  var ps = 2.2, pointOpacity = 0.85, nr = 250, showEv = true, morphT = 0;
  var sel = null, pick = null, hover = null;
  var nicheSet = null;   // cells within the picked nucleus's niche (physical)
  var confThresh = -1; // >=0 when the confidence dissolve filter is active
  var hidden = new Set();
  var gfCT = null, gfCL = null; // group filters: allowed cell types / clusters (null = all)
  var U_SP = null, U_UM = null;
  var P = null;
  var tissueImage = null;
  var dataGeneration = 0;

  var fmt = function (n) {
    return n == null || isNaN(n) ? "—" : Number(n).toLocaleString("en-US");
  };
  // Escape text taken from the loaded .crb (cell-type / cluster labels, gene and
  // meta-column names, sample/tile ids, barcodes) before it goes into innerHTML,
  // so a crafted .crb cannot inject markup. Numeric values are formatted apart.
  var esc = function (s) {
    return String(s == null ? "" : s).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;", "'": "&#39;" }[c];
    });
  };
  var PAL = ["#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A", "#19D3F3",
    "#FF6692", "#B6E880", "#FF97FF", "#FECB52", "#2f6fd6", "#f97316", "#16a34a",
    "#9a5cd0", "#e05780", "#38b2ac", "#d97706", "#7bb0e8"];
  var LEGACY_CT_COL = { ExN: "#636EFA", InN: "#EF553B", Oligo: "#00CC96", Astro: "#AB63FA",
    Micro: "#f97316", OPC: "#19D3F3", DG: "#FF6692", Neuron: "#9a9aa0" };
  var CT_COL = {};
  var VIR = [[68, 1, 84], [72, 40, 120], [62, 73, 137], [49, 104, 142],
    [38, 130, 142], [31, 158, 137], [53, 183, 121], [110, 206, 88],
    [181, 222, 43], [253, 231, 37]];
  var viridis = function (t) {
    t = Math.max(0, Math.min(1, t));
    var s = t * (VIR.length - 1), i = Math.floor(s), fr = s - i,
      a = VIR[i], b = VIR[Math.min(i + 1, VIR.length - 1)];
    return [a[0] + (b[0] - a[0]) * fr, a[1] + (b[1] - a[1]) * fr,
      a[2] + (b[2] - a[2]) * fr].map(Math.round);
  };

  var $ = function (id) { return document.getElementById(id); };
  var SRC = null, CT = null, EV = null;

  function rebuildSources() {
    // Coordinates are always the vendor's canonical Location CSV. (The
    // axis-transposed @images and y-mirrored SPATIAL orientations are documented
    // in the vignette rather than offered as an in-app switch.)
    SRC = { csv: { x: D.x, y: D.y, t: "Location CSV" } };
    CT = D.clusters.map(function (c) { return D.celltype[c]; });
    CT_COL = {};
    Array.from(new Set(CT)).forEach(function (cellType, index) {
      CT_COL[cellType] = (D.celltype_colors && D.celltype_colors[cellType]) ||
        LEGACY_CT_COL[cellType] || PAL[index % PAL.length];
    });
    EV = new Map(D.evidence.map(function (e) { return [e.cell, e]; }));
  }

  function unit(xs, ys) {
    var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity, i;
    for (i = 0; i < N; i++) {
      if (xs[i] < x0) x0 = xs[i]; if (xs[i] > x1) x1 = xs[i];
      if (ys[i] < y0) y0 = ys[i]; if (ys[i] > y1) y1 = ys[i];
    }
    var dw = x1 - x0 || 1, dh = y1 - y0 || 1, k = 1 / Math.max(dw, dh);
    var ox = (1 - dw * k) / 2, oy = (1 - dh * k) / 2;
    var nx = new Float32Array(N), ny = new Float32Array(N);
    for (i = 0; i < N; i++) { nx[i] = (xs[i] - x0) * k + ox; ny[i] = (ys[i] - y0) * k + oy; }
    return { nx: nx, ny: ny, x0: x0, y0: y0, k: k, ox: ox, oy: oy };
  }
  function rebuildSpatialUnit() { U_SP = unit(SRC[src].x, SRC[src].y); }

  function Pane(cvId, tipId, kind) {
    var cv = $(cvId);
    return { cv: cv, ctx: cv.getContext("2d"), tip: $(tipId), kind: kind,
      W: 0, H: 0, bx: null, by: null, sx: null, sy: null,
      k: 1, tx: 0, ty: 0, // pan/zoom viewport transform: screen = base*k + t
      lasso: null, rect: null, drag: false, moved: false, panLast: null };
  }

  function coordsFor(kind) {
    if (view === "morph") {
      var nx = new Float32Array(N), ny = new Float32Array(N), t = morphT, i;
      for (i = 0; i < N; i++) {
        nx[i] = U_UM.nx[i] + (U_SP.nx[i] - U_UM.nx[i]) * t;
        ny[i] = U_UM.ny[i] + (U_SP.ny[i] - U_UM.ny[i]) * t;
      }
      return { nx: nx, ny: ny };
    }
    return kind === "sp" ? U_SP : U_UM;
  }
  // Base (untransformed) screen coords; recomputed only on data / view / resize.
  function computeBase(p) {
    var c = coordsFor(p.kind), nx = c.nx, ny = c.ny, pad = 14,
      S = Math.min(p.W, p.H) - 2 * pad, ox = (p.W - S) / 2, oy = (p.H - S) / 2, i;
    p.bx = new Float32Array(N); p.by = new Float32Array(N);
    for (i = 0; i < N; i++) { p.bx[i] = ox + nx[i] * S; p.by[i] = oy + S - ny[i] * S; }
    applyTransform(p);
  }
  // Apply the pane's pan/zoom to its base coords. Cheap; called on pan/zoom.
  function applyTransform(p) {
    if (!p.bx) return;
    p.sx = new Float32Array(N); p.sy = new Float32Array(N);
    for (var i = 0; i < N; i++) {
      p.sx[i] = p.bx[i] * p.k + p.tx;
      p.sy[i] = p.by[i] * p.k + p.ty;
    }
  }
  function project(p) { computeBase(p); }
  function resetView(p) { p.k = 1; p.tx = 0; p.ty = 0; applyTransform(p); }
  function paneList() {
    if (view === "pair") return [P.sp, P.um];
    if (view === "um") return [P.um];
    return [P.sp]; // "sp" (spatial only) or "morph" (single interpolating pane)
  }
  function resize() {
    if (!D || !P) return;
    var dpr = window.devicePixelRatio || 1;
    paneList().forEach(function (p) {
      var w = p.cv.parentElement.clientWidth;
      if (!w) return;
      p.W = w;
      p.H = view !== "pair" ? Math.max(420, Math.min(660, Math.round(w * 0.62)))
        : Math.max(300, Math.min(520, Math.round(w * 0.92)));
      p.cv.width = p.W * dpr; p.cv.height = p.H * dpr; p.cv.style.height = p.H + "px";
      p.ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      project(p);
    });
    drawAll();
  }
  // A "continuous" mode is a gene (server-served) or any per-nucleus field
  // carried in the slot (the cross-space metrics, and later analysis outputs).
  // This is the substrate: adding a field to the slot makes it a colouring here
  // with no code change.
  function isCont() {
    return mode === "gene" || mode === "meta" ||
      (D && D.fields && Object.prototype.hasOwnProperty.call(D.fields, mode));
  }
  function curField() {
    if (mode === "gene") return D.genes[gene];
    if (mode === "meta") return D.servedMeta;
    return D.fields ? D.fields[mode] : null;
  }
  function curLabel() {
    if (mode === "gene") return gene;
    var f = curField();
    return f ? f.label : mode;
  }
  // Decode a field's displayed value at nucleus i from its 0-255 quantisation,
  // honouring a min (min-max scaled fields; 0 for unit-scaled ones).
  function fieldValue(f, i) {
    var lo = f.min != null ? f.min : 0;
    return lo + (f.v[i] / 255) * (f.max - lo);
  }
  // Per-nucleus positioning-confidence values (0-255), if the field is present.
  function cfVals() {
    return D && D.fields && D.fields.position_confidence
      ? D.fields.position_confidence.v : null;
  }
  // "Dissolve least-confident (pct%)": set the confidence value below which
  // nuclei fade out, from the pct-th percentile of the confidence distribution.
  function setConfPct(pct) {
    var arr = cfVals();
    if (!arr || pct <= 0) { confThresh = -1; return; }
    var s = Array.prototype.slice.call(arr).sort(function (a, b) { return a - b; });
    confThresh = s[Math.floor(pct / 100 * (s.length - 1))];
  }
  function cellTypeColor(value) {
    return (D && D.builder_colors && D.builder_colors[value]) ||
      CT_COL[value] || "#9a9aa0";
  }
  function clusterColor(value) {
    return (D && D.builder_colors && D.builder_colors[String(value)]) ||
      PAL[value % PAL.length];
  }
  function baseColor(i) {
    if (isCont()) {
      var g = curField();
      if (!g) return "#d0d0d3";
      var c = viridis(g.v[i] / 255);
      return "rgb(" + c[0] + "," + c[1] + "," + c[2] + ")";
    }
    if (mode === "celltype") {
      return cellTypeColor(CT[i]);
    }
    return clusterColor(D.clusters[i]);
  }
  function visible(i) {
    // Group filters (always applied): the pickerInputs in the "Group filters"
    // box send the selected levels; a nucleus outside the selection is hidden.
    if (gfCT && !gfCT.has(CT[i])) return false;
    if (gfCL && !gfCL.has(String(D.clusters[i]))) return false;
    // Legend click-toggle (only in the categorical mode it shows).
    if (mode === "celltype" && hidden.has(CT[i])) return false;
    if (mode === "cluster" && hidden.has(D.clusters[i])) return false;
    return true;
  }
  // Turn a pickerInput value (array / scalar / null) into a Set of string levels;
  // null (nothing selected) yields an empty set that hides everything.
  function toFilterSet(v) {
    if (v == null) return new Set();
    return new Set([].concat(v).map(String));
  }
  // After a group-filter change, drop a now-hidden pick and prune the selection.
  function afterFilterChange() {
    if (pick != null && !visible(pick)) { pick = null; renderInspector(); }
    if (sel) {
      var s2 = new Set();
      sel.forEach(function (i) { if (visible(i)) s2.add(i); });
      sel = s2.size ? s2 : null; renderSel();
    }
    rebuildNiche(); renderLegend(); drawAll();
  }
  // The picked nucleus + everything within `nr` µm of it in physical space. Also
  // the highlight set: cells in it stay solid, everything else fades. Null unless
  // a single nucleus is picked (a lasso selection supersedes it). The distance
  // loop is CBGeom.nicheAround (shared with cell_views.js). This engine keys on
  // D.x/D.y and passes inclusive=<, skipNaN=false.
  function rebuildNiche() {
    nicheSet = null;
    if (pick == null || sel || !D || !D.x) return;
    var px = D.x[pick], py = D.y[pick];
    nicheSet = CBGeom.nicheAround(D.x, D.y, N, pick, px, py, nr * nr, false, false);
  }
  function drawTissueImage(p) {
    if (
      !tissueImage || !tissueImage.complete || p.kind !== "sp" ||
      view === "morph" || !D.histology_image_bounds
    ) return;
    var b = D.histology_image_bounds, u = U_SP;
    var values = [b.xmin, b.xmax, b.ymin, b.ymax].map(Number);
    if (!values.every(Number.isFinite) || values[1] <= values[0] || values[3] <= values[2]) return;
    var pad = 14, S = Math.min(p.W, p.H) - 2 * pad;
    var ox = (p.W - S) / 2, oy = (p.H - S) / 2;
    var screen = function (x, y) {
      var nx = (x - u.x0) * u.k + u.ox;
      var ny = (y - u.y0) * u.k + u.oy;
      return [(ox + nx * S) * p.k + p.tx, (oy + S - ny * S) * p.k + p.ty];
    };
    var topLeft = screen(values[0], values[3]);
    var bottomRight = screen(values[1], values[2]);
    var alignment = D.histology_alignment || {};
    p.ctx.globalAlpha = Number.isFinite(+alignment.image_opacity)
      ? Math.max(0, Math.min(1, +alignment.image_opacity)) : 0.75;
    p.ctx.drawImage(
      tissueImage,
      topLeft[0],
      topLeft[1],
      bottomRight[0] - topLeft[0],
      bottomRight[1] - topLeft[1]
    );
    p.ctx.globalAlpha = 1;
  }
  function draw(p) {
    if (!p.sx || !p.W) return; // not projected yet (e.g. tab still hidden)
    var c = p.ctx; c.clearRect(0, 0, p.W, p.H);
    drawTissueImage(p);
    var order = null, j, i, pass;
    var cf = confThresh >= 0 ? cfVals() : null; // confidence dissolve filter
    if (isCont() && curField()) {
      var gv = curField().v;
      order = Array.from({ length: N }, function (_, k) { return k; })
        .sort(function (a, b) { return gv[a] - gv[b]; });
    }
    // Highlight set = lasso selection, or (on pick) the niche. In it → solid;
    // outside → faded.
    var hi = sel || nicheSet;
    for (pass = 0; pass < 2; pass++) {
      for (j = 0; j < N; j++) {
        i = order ? order[j] : j;
        if (!visible(i)) continue;
        var inHi = !hi || hi.has(i);
        if (pass === 0 ? inHi : !inHi) continue;
        var a = hi ? (inHi ? Math.max(pointOpacity, 0.9) : 0.06) : pointOpacity;
        if (cf && cf[i] < confThresh) a *= 0.05; // dissolve low-confidence nuclei
        c.globalAlpha = a;
        c.fillStyle = baseColor(i);
        c.beginPath(); c.arc(p.sx[i], p.sy[i], ps, 0, 6.2832); c.fill();
      }
    }
    if (showEv) {
      c.globalAlpha = 0.95; c.lineWidth = 1.4; c.strokeStyle = "#1c1c1e";
      EV.forEach(function (e, i) {
        if (!visible(i)) return;
        c.beginPath(); c.arc(p.sx[i], p.sy[i], ps + 3.2, 0, 6.2832); c.stroke();
      });
    }
    if (pick != null) {
      c.globalAlpha = 1; c.strokeStyle = "#f97316"; c.lineWidth = 2.2;
      c.beginPath(); c.arc(p.sx[pick], p.sy[pick], ps + 5, 0, 6.2832); c.stroke();
      if (p.kind === "sp" && view !== "morph") {
        var S = Math.min(p.W, p.H) - 28, xs = SRC[src].x, ys = SRC[src].y;
        var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity, k;
        for (var q = 0; q < N; q++) {
          if (xs[q] < x0) x0 = xs[q]; if (xs[q] > x1) x1 = xs[q];
          if (ys[q] < y0) y0 = ys[q]; if (ys[q] > y1) y1 = ys[q];
        }
        k = S / Math.max(x1 - x0, y1 - y0);
        var rpx = nr * k * p.k;
        c.fillStyle = "rgba(37,99,235,0.10)";
        c.beginPath(); c.arc(p.sx[pick], p.sy[pick], rpx, 0, 6.2832); c.fill();
        c.strokeStyle = "#1d4ed8"; c.lineWidth = 2.5; c.setLineDash([7, 4]);
        c.beginPath(); c.arc(p.sx[pick], p.sy[pick], rpx, 0, 6.2832); c.stroke();
        c.setLineDash([]);
      }
    }
    if (hover != null && hover >= 0 && hover !== pick && visible(hover)) {
      // Hovering a nucleus in either pane rings it in BOTH, so you can read off
      // where a transcriptomic neighbour sits in tissue without clicking first.
      c.globalAlpha = 1; c.strokeStyle = "#2f6fd6"; c.lineWidth = 1.8;
      c.beginPath(); c.arc(p.sx[hover], p.sy[hover], ps + 4, 0, 6.2832); c.stroke();
    }
    if (p.lasso && p.lasso.length > 1) {
      c.globalAlpha = 1; c.strokeStyle = "#2f6fd6"; c.lineWidth = 1.5;
      c.fillStyle = "rgba(47,111,214,.08)";
      c.beginPath(); c.moveTo(p.lasso[0][0], p.lasso[0][1]);
      p.lasso.slice(1).forEach(function (pt) { c.lineTo(pt[0], pt[1]); });
      c.closePath(); c.fill(); c.stroke();
    }
    if (p.rect) {
      c.globalAlpha = 1; c.strokeStyle = "#2f6fd6"; c.lineWidth = 1.5;
      c.fillStyle = "rgba(47,111,214,.08)";
      c.beginPath();
      c.rect(Math.min(p.rect[0], p.rect[2]), Math.min(p.rect[1], p.rect[3]),
        Math.abs(p.rect[2] - p.rect[0]), Math.abs(p.rect[3] - p.rect[1]));
      c.fill(); c.stroke();
    }
    c.globalAlpha = 1;
  }
  function drawAll() { if (P) paneList().forEach(draw); }

  // inPoly lives in the shared CBGeom module (www/cv-geom.js). `nearest` stays
  // here: its visibility predicate and point-size-tracking hit radius are this
  // engine's, not shared.
  function nearest(p, mx, my) {
    // Hit radius tracks the Point-size slider (with a floor) so a big dot has a
    // correspondingly big clickable area rather than a fixed ~13px target.
    var hr = Math.max(ps + 8, 10), best = -1, bd = hr * hr, i;
    for (i = 0; i < N; i++) {
      if (!visible(i)) continue;
      var dx = p.sx[i] - mx, dy = p.sy[i] - my, d = dx * dx + dy * dy;
      if (d < bd) { bd = d; best = i; }
    }
    return best;
  }
  // Zoom a pane around a screen point (mx,my) by factor f, keeping it fixed.
  // The scale is clamped to the same 0.2–20 bounds as zoom-to-selection, so
  // repeated wheel/button zooming can't drive the viewport somewhere unrecoverable.
  function zoomPaneAt(p, mx, my, f) {
    var nk = Math.max(0.2, Math.min(20, p.k * f));
    var af = nk / p.k; // actual factor after clamping (keeps the focal point fixed)
    p.tx = mx - (mx - p.tx) * af;
    p.ty = my - (my - p.ty) * af;
    p.k = nk;
    applyTransform(p); draw(p);
  }
  function clickInspect(p, e) {
    var r = p.cv.getBoundingClientRect();
    var k = nearest(p, e.clientX - r.left, e.clientY - r.top);
    if (k >= 0) { pick = k; rebuildNiche(); renderInspector(); drawAll(); }
  }
  function wire(p) {
    var pos = function (e) {
      var r = p.cv.getBoundingClientRect();
      return [e.clientX - r.left, e.clientY - r.top];
    };
    p.cv.addEventListener("mousedown", function (e) {
      if (hover != null) { hover = null; drawAll(); }
      p.drag = true; p.moved = false;
      var m = pos(e);
      if (tool === "pan") { p.panLast = m; }
      else if (tool === "box") { p.rect = [m[0], m[1], m[0], m[1]]; }
      else { p.lasso = [m]; }
    });
    p.cv.addEventListener("mousemove", function (e) {
      var m = pos(e), mx = m[0], my = m[1];
      if (p.drag) {
        p.tip.style.opacity = 0;
        if (tool === "pan") {
          p.tx += mx - p.panLast[0]; p.ty += my - p.panLast[1];
          p.panLast = m; p.moved = true; applyTransform(p); draw(p);
        } else if (tool === "box") {
          p.rect[2] = mx; p.rect[3] = my;
          // Only count it as a drag past a small threshold, so a click that
          // jitters a pixel or two still opens the Cell inspector instead of
          // becoming a degenerate box-select that selects nothing.
          if (Math.hypot(mx - p.rect[0], my - p.rect[1]) > 4) p.moved = true;
          draw(p);
        } else {
          var last = p.lasso[p.lasso.length - 1];
          if (Math.hypot(mx - last[0], my - last[1]) > 3) {
            p.lasso.push([mx, my]); p.moved = true; draw(p);
          }
        }
        return;
      }
      var i = nearest(p, mx, my);
      // Cross-pane hover sync: only redraw when the nearest nucleus changes, so
      // this stays cheap (both panes redraw at most once per nucleus crossing).
      if (i !== hover) { hover = i; drawAll(); }
      if (i < 0) { p.tip.style.opacity = 0; return; }
      var h = "<b>" + esc(CT[i]) + "</b> · cluster " + esc(D.clusters[i]);
      h += p.kind === "sp"
        ? "<br>x " + SRC[src].x[i].toFixed(0) + " · y " + SRC[src].y[i].toFixed(0) + " µm"
        : "<br>UMAP " + D.ux[i].toFixed(1) + " , " + D.uy[i].toFixed(1);
      if (isCont() && curField()) {
        var g = curField();
        h += "<br>" + esc(curLabel()) + " <b>" + fieldValue(g, i).toFixed(2) + "</b>";
      }
      if (EV.has(i)) h += "<br><b>Has positioning evidence</b> · click to view";
      p.tip.innerHTML = h; p.tip.style.opacity = 1;
      var tx = p.sx[i] + 12, ty = p.sy[i] - 8;
      if (tx + p.tip.offsetWidth > p.W) tx = p.sx[i] - p.tip.offsetWidth - 12;
      p.tip.style.left = tx + "px"; p.tip.style.top = ty + "px";
    });
    p.cv.addEventListener("mouseleave", function () {
      p.tip.style.opacity = 0;
      if (hover != null) { hover = null; drawAll(); }
    });
    p.cv.addEventListener("wheel", function (e) {
      e.preventDefault();
      var m = pos(e);
      zoomPaneAt(p, m[0], m[1], e.deltaY < 0 ? 1.15 : 1 / 1.15);
    }, { passive: false });
    window.addEventListener("mouseup", function (e) {
      if (!p.drag) return;
      p.drag = false;
      if (tool === "pan") {
        p.panLast = null;
      } else if (tool === "box") {
        if (p.moved && p.rect) {
          var x0 = Math.min(p.rect[0], p.rect[2]), x1 = Math.max(p.rect[0], p.rect[2]);
          var y0 = Math.min(p.rect[1], p.rect[3]), y1 = Math.max(p.rect[1], p.rect[3]);
          var s = new Set(), i;
          for (i = 0; i < N; i++) {
            if (visible(i) && p.sx[i] >= x0 && p.sx[i] <= x1 && p.sy[i] >= y0 && p.sy[i] <= y1) s.add(i);
          }
          sel = s.size ? s : null; pick = null; renderSel();
        } else { clickInspect(p, e); }
        p.rect = null;
      } else {
        if (p.moved && p.lasso.length > 2) {
          var s2 = new Set(), j;
          for (j = 0; j < N; j++) {
            if (visible(j) && CBGeom.inPoly(p.sx[j], p.sy[j], p.lasso)) s2.add(j);
          }
          sel = s2.size ? s2 : null; pick = null; renderSel();
        } else { clickInspect(p, e); }
        p.lasso = null;
      }
      rebuildNiche();   // a lasso/box selection clears any niche highlight
      drawAll();
    });
  }

  function nVisible() {
    var v = 0; for (var i = 0; i < N; i++) if (visible(i)) v++; return v;
  }
  function renderSel() {
    var bar = $("tk-selbar");
    if (!sel) { bar.style.display = "none"; return; }
    bar.style.display = "flex";
    var cnt = {}; sel.forEach(function (i) { cnt[CT[i]] = (cnt[CT[i]] || 0) + 1; });
    var top = Object.entries(cnt).sort(function (a, b) { return b[1] - a[1]; })
      .slice(0, 4).map(function (e) { return esc(e[0]) + " " + e[1]; }).join(" · ");
    // Denominator is the currently-visible pool (equals N when no filter is set),
    // so a selection reads against what's actually shown rather than the whole set.
    $("tk-seltext").innerHTML = "Selected <b>" + sel.size + "</b> / " + fmt(nVisible()) +
      " nuclei &nbsp;—&nbsp; " + top +
      "&nbsp;&nbsp;<span class=\"tk-muted\">(both panes highlight in sync)</span>";
  }

  function renderInspector() {
    var el = $("tk-inspbody");
    if (pick == null) {
      el.innerHTML = "<div class=\"tk-empty\">Click a nucleus to see its identity, " +
        "physical neighbourhood, and positioning evidence.</div>";
      return;
    }
    var i = pick, cnt = {}, n = 0, j;
    for (j = 0; j < N; j++) {
      if (j === i) continue;
      var dx = D.x[i] - D.x[j], dy = D.y[i] - D.y[j];
      if (dx * dx + dy * dy < nr * nr) { cnt[CT[j]] = (cnt[CT[j]] || 0) + 1; n++; }
    }
    var rows = Object.entries(cnt).sort(function (a, b) { return b[1] - a[1]; });
    var mx = rows.length ? rows[0][1] : 1;
    var bars = rows.length ? rows.map(function (e) {
      var k = e[0], v = e[1];
      return "<div class=\"tk-bar\"><span class=\"tk-nm\" style=\"color:" + cellTypeColor(k) +
        "\">" + esc(k) + "</span><span class=\"tk-tr\"><span class=\"tk-fl\" style=\"width:" +
        (v / mx * 100) + "%;background:" + cellTypeColor(k) + "\"></span></span>" +
        "<span class=\"tk-ct\">" + v + "</span></div>";
    }).join("") : "<div class=\"tk-hint\">No other nuclei within this radius — increase the niche radius.</div>";

    var ev = EV.get(i);
    var evHtml = ev
      ? "<img class=\"tk-evimg\" src=\"" + esc(ev.img) + "\" alt=\"positioning evidence\" " +
        "onclick=\"tkZoom(this.src)\" style=\"cursor:zoom-in\">" +
        "<div class=\"tk-hint\"><code>*</code> = adopted centroid · the field of grey dots are " +
        "nUMI=1 noise beads. <b>Why is this nucleus here — the evidence is here.</b></div>"
      : "<div class=\"tk-empty\">This nucleus has no official positioning-evidence image.<br>" +
        "<span class=\"tk-muted\">The vendor ships 50 per class; only ringed nuclei have one.</span></div>";

    var cf = curField();
    var g = isCont() && cf
      ? "<dt>" + esc(curLabel()) + "</dt><dd>" + fieldValue(cf, i).toFixed(2) + "</dd>"
      : "";
    var cs = D.conf;
    var confRows = cs
      ? "<dt>Position conf.</dt><dd>" + (cs.prop_top[i] * 100).toFixed(1) + "%</dd>" +
        "<dt>Bead noise</dt><dd>" + (cs.prop_noise[i] * 100).toFixed(0) + "%</dd>" +
        "<dt>Spatial barcodes</dt><dd>" + fmt(cs.sb_total[i]) + "</dd>"
      : "";
    el.innerHTML = "<div class=\"tk-insp\"><div>" +
      "<h4 class=\"tk-sub-h\">Identity</h4>" +
      "<dl class=\"tk-kv\"><dt>Cell type</dt><dd style=\"color:" + cellTypeColor(CT[i]) + "\">" + esc(CT[i]) + "</dd>" +
      "<dt>Cluster</dt><dd>" + esc(D.clusters[i]) + "</dd>" +
      "<dt>x</dt><dd>" + D.x[i].toFixed(0) + " µm</dd>" +
      "<dt>y</dt><dd>" + D.y[i].toFixed(0) + " µm</dd>" +
      "<dt>UMAP</dt><dd>" + D.ux[i].toFixed(1) + ", " + D.uy[i].toFixed(1) + "</dd>" + g + confRows + "</dl>" +
      "<div class=\"tk-hint\" style=\"word-break:break-all\">" + (ev ? esc(ev.bc) : "") + "</div></div>" +
      "<div><h4 class=\"tk-sub-h\">Physical neighbourhood " +
      "<span class=\"tk-muted\">r = " + nr + " µm · n = " + n + " · full tissue</span></h4>" +
      "<div class=\"tk-bars\">" + bars + "</div>" +
      "<div class=\"tk-hint\"><b>Real cell counts, not a deconvolution estimate.</b> Counted over " +
      "the whole tissue (group filters change the view, not a nucleus's real neighbours). " +
      "Visium cannot do this — a spot is internally mixed.</div></div>" +
      "<div><h4 class=\"tk-sub-h\">Positioning evidence</h4>" + evHtml + "</div></div>";
    drawAll();
  }

  // Per-cell-type summary for a field that carries one (spatial purity): the
  // at-a-glance "who forms domains, who disperses" readout, plus the field's
  // honest description.
  function renderFieldSummary() {
    var el = $("tk-fieldsummary"); if (!el) return;
    var f = mode === "gene" ? null : curField();
    if (!f) { el.innerHTML = ""; return; }
    var html = f.desc ? "<div class=\"tk-hint\" style=\"margin-top:8px\">" + esc(f.desc) + "</div>" : "";
    if (f.by_type && f.by_type.length) {
      var mx = Math.max.apply(null, f.by_type.map(function (b) { return b.median; })) || 1;
      var rows = f.by_type.slice().sort(function (a, b) { return b.median - a.median; })
        .map(function (b) {
          return "<div class=\"tk-bar\"><span class=\"tk-nm\" style=\"color:" + cellTypeColor(b.type) +
            "\">" + esc(b.type) + "</span><span class=\"tk-tr\"><span class=\"tk-fl\" style=\"width:" +
            (b.median / mx * 100) + "%;background:" + cellTypeColor(b.type) + "\"></span></span>" +
            "<span class=\"tk-ct\">" + b.median.toFixed(2) + "</span></div>";
        }).join("");
      html += "<div class=\"tk-sub-h\" style=\"margin-top:10px\">Median by cell type — who forms domains, who disperses</div>" +
        "<div class=\"tk-bars\">" + rows + "</div>";
    }
    el.innerHTML = html;
  }

  function renderLegend() {
    renderFieldSummary();
    var L = $("tk-legend"), C = $("tk-cbar");
    if (isCont()) {
      L.style.display = "none"; C.style.display = "flex";
      var g = curField();
      $("tk-cb0").textContent = g && g.min != null ? g.min.toFixed(1) : "0";
      $("tk-cb1").textContent = g ? g.max.toFixed(1) : "—";
      var nEl = $("tk-cbar-note");
      if (nEl) {
        nEl.textContent = mode === "gene"
          ? "SCT normalized"
          : (mode === "meta" ? "" : "0–1");
      }
      var st = [];
      for (var i = 0; i <= 10; i++) {
        var c = viridis(i / 10);
        st.push("rgb(" + c[0] + "," + c[1] + "," + c[2] + ") " + (i * 10) + "%");
      }
      $("tk-grad").style.background = "linear-gradient(90deg," + st.join(",") + ")";
      return;
    }
    C.style.display = "none"; L.style.display = "flex"; L.innerHTML = "";
    var keys = mode === "celltype"
      ? Array.from(new Set(CT)).sort(function (a, b) {
        return CT.filter(function (x) { return x === b; }).length -
          CT.filter(function (x) { return x === a; }).length;
      })
      : Array.from(new Set(D.clusters)).sort(function (a, b) { return a - b; });
    var cnt = {};
    (mode === "celltype" ? CT : D.clusters).forEach(function (k) { cnt[k] = (cnt[k] || 0) + 1; });
    keys.forEach(function (k) {
      var col = mode === "celltype" ? cellTypeColor(k) : clusterColor(k);
      var d = document.createElement("div");
      d.className = "tk-lg" + (hidden.has(k) ? " off" : "");
      d.innerHTML = "<span class=\"tk-dot\" style=\"background:" + col + "\"></span>" + esc(k) +
        " <span class=\"tk-muted\">(" + cnt[k] + ")</span>";
      d.onclick = function () {
        hidden.has(k) ? hidden.delete(k) : hidden.add(k); renderLegend(); drawAll();
      };
      L.appendChild(d);
    });
  }

  function applyGene(g) {
    gene = g; renderLegend(); drawAll(); if (pick != null) renderInspector();
  }
  function applyServed() {
    renderLegend(); drawAll(); if (pick != null) renderInspector();
  }

  /* ---- toolbar (a canvas-native modebar matching the app's plotly one) ---- */
  // plotly's own icon paths, so the toolbar reads as the same control as the
  // modebar on every other tab. The panes are a bespoke canvas (for the instant
  // morph / dissolve / linked selection), so pan+zoom+select are implemented here.
  var TB_ICONS = {
    pan: { vb: "0 0 1000 1000", tr: "matrix(1 0 0 -1 0 850)", d: "m1000 350l-187 188 0-125-250 0 0 250 125 0-188 187-187-187 125 0 0-250-250 0 0 125-188-188 186-187 0 125 252 0 0-250-125 0 187-188 188 188-125 0 0 250 250 0 0-126 187 188z" },
    box: { vb: "0 0 1000 1000", tr: "matrix(1 0 0 -1 0 850)", d: "m0 850l0-143 143 0 0 143-143 0z m286 0l0-143 143 0 0 143-143 0z m285 0l0-143 143 0 0 143-143 0z m286 0l0-143 143 0 0 143-143 0z m-857-286l0-143 143 0 0 143-143 0z m857 0l0-143 143 0 0 143-143 0z m-857-285l0-143 143 0 0 143-143 0z m857 0l0-143 143 0 0 143-143 0z m-857-286l0-143 143 0 0 143-143 0z m286 0l0-143 143 0 0 143-143 0z m285 0l0-143 143 0 0 143-143 0z m286 0l0-143 143 0 0 143-143 0z" },
    lasso: { vb: "0 0 1031 1000", tr: "matrix(1 0 0 -1 0 850)", d: "m1018 538c-36 207-290 336-568 286-277-48-473-256-436-463 10-57 36-108 76-151-13-66 11-137 68-183 34-28 75-41 114-42l-55-70 0 0c-2-1-3-2-4-3-10-14-8-34 5-45 14-11 34-8 45 4 1 1 2 3 2 5l0 0 113 140c16 11 31 24 45 40 4 3 6 7 8 11 48-3 100 0 151 9 278 48 473 255 436 462z m-624-379c-80 14-149 48-197 96 42 42 109 47 156 9 33-26 47-66 41-105z m-187-74c-19 16-33 37-39 60 50-32 109-55 174-68-42-25-95-24-135 8z m360 75c-34-7-69-9-102-8 8 62-16 128-68 170-73 59-175 54-244-5-9 20-16 40-20 61-28 159 121 317 333 354s407-60 434-217c28-159-121-318-333-355z" },
    zoomin: { vb: "0 0 875 1000", tr: "matrix(1 0 0 -1 0 850)", d: "m1 787l0-875 875 0 0 875-875 0z m687-500l-187 0 0-187-125 0 0 187-188 0 0 125 188 0 0 187 125 0 0-187 187 0 0-125z" },
    zoomout: { vb: "0 0 875 1000", tr: "matrix(1 0 0 -1 0 850)", d: "m0 788l0-876 875 0 0 876-875 0z m688-500l-500 0 0 125 500 0 0-125z" },
    reset: { vb: "0 0 928.6 1000", tr: "matrix(1 0 0 -1 0 850)", d: "m786 296v-267q0-15-11-26t-25-10h-214v214h-143v-214h-214q-15 0-25 10t-11 26v267q0 1 0 2t0 2l321 264 321-264q1-1 1-4z m124 39l-34-41q-5-5-12-6h-2q-7 0-12 3l-386 322-386-322q-7-4-13-4-7 2-12 7l-35 41q-4 5-3 13t6 12l401 334q18 15 42 15t43-15l136-114v109q0 8 5 13t13 5h107q8 0 13-5t5-13v-227l122-102q5-5 6-12t-4-13z" },
    zoomsel: { vb: "0 0 512 512", tr: null, d: "M416 208c0 45.9-14.9 88.3-40 122.7L502.6 457.4c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L330.7 376c-34.4 25.2-76.8 40-122.7 40C93.1 416 0 322.9 0 208S93.1 0 208 0S416 93.1 416 208zM184 296c0 13.3 10.7 24 24 24s24-10.7 24-24V232h64c13.3 0 24-10.7 24-24s-10.7-24-24-24H232V120c0-13.3-10.7-24-24-24s-24 10.7-24 24v64H120c-13.3 0-24 10.7-24 24s10.7 24 24 24h64v64z" },
    clear: { vb: "0 0 512 512", tr: null, d: "M290.7 57.4L57.4 290.7c-25 25-25 65.5 0 90.5l80 80c12 12 28.3 18.7 45.3 18.7H288h9.4H512c17.7 0 32-14.3 32-32s-14.3-32-32-32H387.9L518.6 363.3c25-25 25-65.5 0-90.5L381.3 57.4c-25-25-65.5-25-90.5 0zM162.7 416l-80-80L216 202.7 349.3 336 269.3 416H162.7z" },
    download: { vb: "0 0 1000 1000", tr: "matrix(1 0 0 -1 0 850)", d: "m500 450c-83 0-150-67-150-150 0-83 67-150 150-150 83 0 150 67 150 150 0 83-67 150-150 150z m400 150h-120c-16 0-34 13-39 29l-31 93c-6 15-23 28-40 28h-340c-16 0-34-13-39-28l-31-94c-6-15-23-28-40-28h-120c-55 0-100-45-100-100v-450c0-55 45-100 100-100h800c55 0 100 45 100 100v450c0 55-45 100-100 100z m-400-550c-138 0-250 112-250 250 0 138 112 250 250 250 138 0 250-112 250-250 0-138-112-250-250-250z m365 380c-19 0-35 16-35 35 0 19 16 35 35 35 19 0 35-16 35-35 0-19-16-35-35-35z" }
  };
  var TB_BTNS = [
    { id: "pan", title: "Pan" }, { id: "box", title: "Box Select" },
    { id: "lasso", title: "Lasso Select" }, { sep: true },
    { id: "zoomin", title: "Zoom in" }, { id: "zoomout", title: "Zoom out" },
    { id: "reset", title: "Reset axes" }, { id: "zoomsel", title: "Zoom to selection" },
    { sep: true }, { id: "clear", title: "Clear selection" },
    { id: "download", title: "Download plot as a png" }
  ];
  function svgIcon(ic) {
    return "<svg viewBox=\"" + ic.vb + "\" height=\"1em\" width=\"1em\">" +
      "<path d=\"" + ic.d + "\"" + (ic.tr ? " transform=\"" + ic.tr + "\"" : "") + "></path></svg>";
  }
  function buildToolbar() {
    var bar = $("tk-modebar"); if (!bar || bar._built) return;
    bar._built = true;
    bar.innerHTML = TB_BTNS.map(function (b) {
      return b.sep ? "<span class=\"tk-mb-sep\"></span>"
        : "<a class=\"tk-mb-btn\" data-act=\"" + b.id + "\" title=\"" + b.title + "\" role=\"button\">" +
          svgIcon(TB_ICONS[b.id]) + "</a>";
    }).join("");
    bar.querySelectorAll(".tk-mb-btn").forEach(function (a) {
      a.onclick = function () { toolbarAction(a.getAttribute("data-act")); };
    });
    updateToolbarActive();
  }
  function toolbarAction(act) {
    if (act === "pan" || act === "box" || act === "lasso") setTool(act);
    else if (act === "zoomin") zoomAll(1.3);
    else if (act === "zoomout") zoomAll(1 / 1.3);
    else if (act === "reset") resetAll();
    else if (act === "zoomsel") zoomToSel();
    else if (act === "clear") { sel = null; rebuildNiche(); renderSel(); drawAll(); }
    else if (act === "download") downloadPNG();
  }
  function updateToolbarActive() {
    var bar = $("tk-modebar"); if (!bar) return;
    bar.querySelectorAll(".tk-mb-btn").forEach(function (a) {
      var act = a.getAttribute("data-act");
      a.classList.toggle("active", (act === "pan" || act === "box" || act === "lasso") && act === tool);
    });
  }
  function setTool(t) {
    tool = t; updateToolbarActive();
    var cur = t === "pan" ? "grab" : "crosshair";
    if (P) paneList().forEach(function (p) { p.cv.style.cursor = cur; });
  }
  function zoomAll(f) {
    if (P) paneList().forEach(function (p) { if (p.W) zoomPaneAt(p, p.W / 2, p.H / 2, f); });
  }
  function resetAll() {
    if (P) paneList().forEach(function (p) { resetView(p); });
    drawAll();
  }
  // Fit the current selection's bounding box in each visible pane.
  function zoomToSel() {
    if (!sel || !sel.size || !P) return;
    paneList().forEach(function (p) {
      if (!p.bx) return;
      var x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
      sel.forEach(function (i) {
        var bx = p.bx[i], by = p.by[i];
        if (bx < x0) x0 = bx; if (bx > x1) x1 = bx;
        if (by < y0) y0 = by; if (by > y1) y1 = by;
      });
      var pad = 34, bw = (x1 - x0) || 1, bh = (y1 - y0) || 1;
      var k = Math.max(0.2, Math.min(20, Math.min((p.W - 2 * pad) / bw, (p.H - 2 * pad) / bh)));
      p.k = k;
      p.tx = p.W / 2 - ((x0 + x1) / 2) * k;
      p.ty = p.H / 2 - ((y0 + y1) / 2) * k;
      applyTransform(p);
    });
    drawAll();
  }
  // Export the visible panes as one white-background PNG.
  function downloadPNG() {
    var panes = paneList().filter(function (p) { return p.W; });
    if (!panes.length) return;
    var gap = 12, dpr = window.devicePixelRatio || 1;
    var totalW = panes.reduce(function (a, p) { return a + p.W; }, 0) + gap * (panes.length - 1);
    var maxH = Math.max.apply(null, panes.map(function (p) { return p.H; }));
    var out = document.createElement("canvas");
    out.width = totalW * dpr; out.height = maxH * dpr;
    var octx = out.getContext("2d");
    octx.setTransform(dpr, 0, 0, dpr, 0, 0);
    octx.fillStyle = "#ffffff"; octx.fillRect(0, 0, totalW, maxH);
    var x = 0;
    panes.forEach(function (p) {
      octx.drawImage(p.cv, 0, 0, p.cv.width, p.cv.height, x, 0, p.W, p.H);
      x += p.W + gap;
    });
    var a = document.createElement("a");
    a.href = out.toDataURL("image/png");
    a.download = "trekker_" + ((D && D.qc && D.qc.sample_id) || "plot").replace(/[^\w.-]+/g, "_") + ".png";
    a.click();
  }

  // Bring the scatter panes into view. The Moran table sits far below the plot
  // it recolours; scrollIntoView on the column does NOT move AdminLTE's
  // .content-wrapper scroller, so scroll that container to the plot explicitly.
  function scrollToPlot() {
    var plot = $("tk-panes"); if (!plot) return;
    var box = plot.closest(".box") || plot;
    var scroller = box.closest(".content-wrapper") ||
      document.querySelector(".content-wrapper") || document.scrollingElement;
    if (!scroller) return;
    var top = scroller.scrollTop +
      (box.getBoundingClientRect().top - scroller.getBoundingClientRect().top) - 12;
    scroller.scrollTo({ top: top, behavior: "smooth" });
  }

  /* ---- header / QC / evidence / moran (static per dataset) --------------- */
  // Shared with cell_views.js: Linked views shows the same coordinate-source /
  // QC / positioning / Moran's I detail for a Trekker data set, in a modal
  // instead of a permanent page section. These builders return HTML strings so
  // both callers can drop them into their own DOM ids.
  var CerebroTrekker = window.CerebroTrekker = window.CerebroTrekker || {};

  CerebroTrekker.buildStatsGrid = function (q) {
    var pass2p = q.pct_2plus < 20;
    return [
      ["Total nuclei", fmt(q.total_nuclei), "single-nuclei library", ""],
      ["In Trekker library", q.pct_in_lib.toFixed(2) + "%", fmt(q.in_lib) + " nuclei · ref >95%", q.pct_in_lib > 95 ? "ok" : "warn"],
      ["Valid spatial barcodes", q.pct_valid_sb.toFixed(2) + "%", "ref >95%", q.pct_valid_sb > 95 ? "ok" : "warn"],
      ["At least 1 location", q.pct_positioned.toFixed(2) + "%", fmt(q.positioned) + " nuclei · ref >60%", q.pct_positioned > 60 ? "ok" : "warn"],
      ["Confidently positioned", q.pct_conf.toFixed(2) + "%", fmt(q.conf) + " nuclei · ref >40%", q.pct_conf > 40 ? "ok" : "warn"],
      ["2+ locations", q.pct_2plus.toFixed(2) + "%", "ref <20% " + (pass2p ? "" : "← over"), pass2p ? "ok" : "warn"]
    ].map(function (r) {
      return "<div class=\"tk-stat " + r[3] + "\"><div class=\"tk-k\">" + r[0] + "</div><div class=\"tk-v\">" +
        r[1] + "</div><div class=\"tk-m\">" + r[2] + "</div></div>";
    }).join("");
  };

  CerebroTrekker.buildPositionTable = function (q) {
    var tot = q.total_nuclei;
    return [
      ["0 (unpositioned)", q.n_0, "Excluded · coordinate is the <code>0,0</code> sentinel"],
      ["1", q.n_1, "<b>Imported</b> (incl. salvaged)"],
      ["2", q.n_2, "Excluded"], ["3", q.n_3, "Excluded"], ["≥4", q.n_4p, "Excluded"]
    ].map(function (r) {
      return "<tr><td>" + r[0] + "</td><td class=\"num\">" + fmt(r[1]) + "</td><td class=\"num\">" +
        (r[1] / tot * 100).toFixed(2) + "%</td><td class=\"tk-muted\">" + r[2] + "</td></tr>";
    }).join("");
  };

  CerebroTrekker.buildSalvFlag = function (q) {
    var salv = q.salv_2 + q.salv_3;
    return "<b>Confidently positioned ≠ exactly one location.</b> The " +
      fmt(q.n_1) + " imported nuclei = native single-location " + fmt(q.o_1) +
      " + vendor-salvaged from 2 (" + q.salv_2 + ") + from 3 (" + q.salv_3 + "), i.e. <b>" + salv +
      " (" + (salv / q.n_1 * 100).toFixed(2) + "%)</b> are upstream-salvaged multi-location nuclei. " +
      "The label must be <code>vendor_confidently_positioned</code>.";
  };

  CerebroTrekker.buildProvenanceDl = function (q) {
    return [
      ["Platform / assay", esc(q.assay)], ["Sample ID", esc(q.sample_id)], ["Tile ID", esc(q.tile_id)],
      ["Pipeline version", "<span class=\"tk-muted\">missing (metric absent)</span>"],
      ["Coordinate source", "Location CSV (canonical)"],
      ["Coordinate unit", "µm <span class=\"tk-muted\">(per manual; not declared in file)</span>"],
      ["DBSCAN eps", esc(q.eps)], ["minPts", esc(q.min_sb)],
      ["Histology image", "<span class=\"tk-muted\">none (not provided in bundle)</span>"],
      ["Moran's I source", "<span class=\"tk-badge tk-badge-soft\" style=\"font-size:10px\">Upstream</span>"]
    ].map(function (r) { return "<dt>" + r[0] + "</dt><dd>" + r[1] + "</dd>"; }).join("");
  };

  CerebroTrekker.buildRangeFlag = function (q) {
    return "<b>The vendor's own demo crosses the vendor's own reference line.</b> " +
      "The 2+ location rate " + q.pct_2plus + "% > the manual's suggested <20%. The app should only show " +
      "\"below vendor reference range\" and must not adjudicate sample usability for the user.";
  };

  // `linkable` = true adds a 4th "Show in plot" column (Trekker page, where a
  // click can switch the gene-colour mode). Linked views has no such mode to
  // switch into, so its modal passes false and gets a plain 3-column table.
  CerebroTrekker.buildMoranRows = function (moran, linkable) {
    return moran.map(function (r) {
      var link = linkable
        ? "<td><a href=\"#\" class=\"tk-link\" data-g=\"" + esc(r.gene) + "\">Show in plot →</a></td>"
        : "";
      return "<tr><td class=\"num tk-muted\">" + r.rank + "</td>" +
        "<td style=\"font-weight:600\">" + esc(r.gene) + "</td><td class=\"num\">" + r.I.toFixed(4) + "</td>" +
        link + "</tr>";
    }).join("");
  };

  function renderStatic() {
    var q = D.qc, m = D.meta;
    $("tk-b-assay").textContent = q.assay || "Trekker";
    $("tk-subline").innerHTML = "<code>" + esc(q.sample_id) + "</code> · " + fmt(m.n_cells) +
      " nuclei (down-sampled from " + fmt(m.n_cells_full) + " confidently positioned) · " +
      fmt(m.n_genes_obj) + " genes (whole transcriptome, not a panel) · coordinate unit " + esc(m.unit);

    $("tk-stats").innerHTML = CerebroTrekker.buildStatsGrid(q);
    $("tk-postbl").innerHTML = CerebroTrekker.buildPositionTable(q);
    $("tk-salvflag").innerHTML = CerebroTrekker.buildSalvFlag(q);
    $("tk-prov").innerHTML = CerebroTrekker.buildProvenanceDl(q);
    $("tk-rangeflag").innerHTML = CerebroTrekker.buildRangeFlag(q);
    $("tk-morantbl").innerHTML = CerebroTrekker.buildMoranRows(D.moran, true);
    $("tk-morantbl").querySelectorAll("a").forEach(function (a) {
      a.onclick = function (e) {
        e.preventDefault();
        if (window.Shiny) Shiny.setInputValue("trekker_moran_gene", a.dataset.g, { priority: "event" });
        scrollToPlot();
      };
    });
  }

  /* ---- init on data arrival ---------------------------------------------- */
  function initFromData(data) {
    var generation = ++dataGeneration;
    D = data;
    if (!D.genes) D.genes = {};
    N = D.x.length;
    ps = 2.2;
    var alignment = D.histology_alignment || {};
    if (Number.isFinite(+alignment.point_size) && +alignment.point_size > 0) {
      ps = +alignment.point_size;
    }
    if (Number.isFinite(+alignment.point_opacity)) {
      pointOpacity = Math.max(0, Math.min(1, +alignment.point_opacity));
    } else {
      pointOpacity = 0.85;
    }
    tissueImage = null;
    if (typeof D.histology_image === "string" && D.histology_image.indexOf("data:image/") === 0) {
      var image = new Image();
      image.onload = function () {
        if (generation !== dataGeneration) return;
        tissueImage = image;
        drawAll();
      };
      image.onerror = function () {
        if (generation !== dataGeneration) return;
        tissueImage = null;
        drawAll();
      };
      image.src = D.histology_image;
    }
    rebuildSources();
    U_UM = unit(D.ux, D.uy);
    rebuildSpatialUnit();
    if (!P) P = { sp: Pane("tk-cv-sp", "tk-tip-sp", "sp"), um: Pane("tk-cv-um", "tk-tip-um", "um") };
    if (!P.sp._wired) { Object.values(P).forEach(function (p) { wire(p); p._wired = true; }); }
    var sc = $("tk-selclear"); if (sc && !sc._wired) { sc.onclick = function () { sel = null; rebuildNiche(); renderSel(); drawAll(); }; sc._wired = true; }
    sel = null; pick = null; hover = null; hidden.clear(); nicheSet = null;
    gfCT = null; gfCL = null;
    cancelTransitionPlay();
    Object.values(P).forEach(function (p) { p.k = 1; p.tx = 0; p.ty = 0; });
    buildToolbar();
    setTool(tool);
    hookMorphSlider();
    setTimeout(hookMorphSlider, 300);
    setTimeout(hookMorphSlider, 1200);
    renderStatic();
    renderLegend();
    renderInspector();
    resize();
  }

  // Shiny debounces slider input (~250ms) and doesn't use ion.rangeSlider's own
  // onChange, so by default the Transition only redraws once you stop dragging.
  // The morph is purely client-side, so hook ion's onChange (which fires
  // continuously during the drag) to animate the canvas in real time. Idempotent
  // per ion instance; re-applied on data load / when entering the Transition view
  // (a re-rendered slider is a fresh instance without the flag).
  function hookMorphSlider() {
    if (!window.jQuery) return;
    var irs = window.jQuery("#trekker_morph").data("ionRangeSlider");
    if (!irs || irs._tkHooked) return;
    irs._tkHooked = true;
    irs.update({
      onChange: function (data) {
        // A real drag fires this (programmatic irs.update does not) — so it also
        // interrupts the auto-demo below.
        cancelTransitionPlay();
        morphT = +data.from;
        if (view === "morph" && P) { project(P.sp); draw(P.sp); }
        updateSpUnitLabel();
      }
    });
  }

  // Auto-demo: on entering the Transition view, sweep morph 0 → 1 → 0 once so the
  // user sees the blend play out without having to drag. The slider handle moves
  // with it (irs.update, which doesn't re-fire onChange). Any real drag cancels it.
  var morphRaf = null;
  function setMorphFrame(v, irs) {
    morphT = v;
    if (P) { project(P.sp); draw(P.sp); }
    updateSpUnitLabel();
    if (irs) irs.update({ from: Math.round(v * 100) / 100 });
  }
  function cancelTransitionPlay() {
    if (morphRaf) { cancelAnimationFrame(morphRaf); morphRaf = null; }
  }
  function playTransition() {
    if (view !== "morph" || !P) return;
    cancelTransitionPlay();
    var irs = window.jQuery ? window.jQuery("#trekker_morph").data("ionRangeSlider") : null;
    var dur = 2800, t0 = performance.now();
    function frame(now) {
      var e = Math.min(1, (now - t0) / dur);
      var tri = e < 0.5 ? e * 2 : 2 - e * 2; // 0 → 1 → 0
      var eased = tri < 0.5 ? 2 * tri * tri : 1 - Math.pow(-2 * tri + 2, 2) / 2;
      setMorphFrame(eased, irs);
      if (e < 1) { morphRaf = requestAnimationFrame(frame); }
      else { morphRaf = null; setMorphFrame(0, irs); } // settle back at the UMAP end
    }
    morphRaf = requestAnimationFrame(frame);
  }

  // In the Transition view the single pane interpolates between UMAP (morph = 0)
  // and spatial (morph = 1), so the corner label must follow the slider rather
  // than stay fixed on the spatial coordinate source.
  function updateSpUnitLabel() {
    var el = $("tk-u-sp"); if (!el) return;
    if (view !== "morph") { el.textContent = "µm · Location CSV"; return; }
    el.textContent = morphT <= 0.001 ? "UMAP (transcriptome)"
      : morphT >= 0.999 ? "µm · Location CSV (spatial)"
        : "UMAP → Spatial · " + Math.round(morphT * 100) + "%";
  }

  /* ---- control changes: driven by the Shiny inputs ----------------------- */
  function onInput(name, value) {
    if (!D) return;
    switch (name) {
      case "trekker_view":
        view = value;
        if (view === "morph") hookMorphSlider();
        // pair = both panes; sp / morph = spatial pane only; um = UMAP pane only.
        $("tk-panes").classList.toggle("single", view !== "pair");
        $("tk-pane-sp").style.display = view === "um" ? "none" : "";
        $("tk-pane-um").style.display = (view === "pair" || view === "um") ? "" : "none";
        var t = document.querySelector("#tk-pane-sp .tk-pane-h span:first-child");
        if (t) t.textContent = view === "morph" ? "UMAP → Spatial" : "Spatial";
        updateSpUnitLabel();
        resize();
        if (view === "morph") setTimeout(playTransition, 150);
        else cancelTransitionPlay();
        break;
      case "trekker_group_filter_celltype":
        gfCT = toFilterSet(value); afterFilterChange();
        break;
      case "trekker_group_filter_cluster":
        gfCL = toFilterSet(value); afterFilterChange();
        break;
      case "trekker_mode":
        mode = value; hidden.clear(); renderLegend(); drawAll();
        if (pick != null) renderInspector();
        break;
      case "trekker_ps": ps = +value; drawAll(); break;
      case "trekker_nr":
        nr = +value; rebuildNiche(); drawAll();
        if (pick != null) renderInspector();
        break;
      case "trekker_morph":
        morphT = +value; if (P) { project(P.sp); draw(P.sp); }
        updateSpUnitLabel();
        break;
      case "trekker_evtoggle": showEv = !!value; drawAll(); break;
      case "trekker_conf": setConfPct(+value); drawAll(); break;
      default: break;
    }
  }

  /* ---- Shiny wiring ------------------------------------------------------ */
  window.tkZoom = function (s) { var z = $("tk-zoom"); $("tk-zoomimg").src = s; z.showModal(); };

  function boot() {
    if (!window.Shiny) return;
    Shiny.addCustomMessageHandler("trekker_data", function (d) { initFromData(d); });
    Shiny.addCustomMessageHandler("trekker_geneval", function (mmsg) {
      if (!D || !mmsg.ok) return;
      D.genes[mmsg.gene] = { v: mmsg.v, max: mmsg.max };
      if (mode === "gene") applyGene(mmsg.gene);
    });
    Shiny.addCustomMessageHandler("trekker_served", function (mmsg) {
      if (!D || !mmsg.ok) return;
      D.servedMeta = {
        v: mmsg.v, min: mmsg.min, max: mmsg.max,
        label: mmsg.label, desc: mmsg.desc
      };
      if (mode === "meta") applyServed();
    });
    var jq = window.jQuery;
    if (jq) {
      jq(document).on("shiny:inputchanged", function (e) { onInput(e.name, e.value); });
      var ping = function () { Shiny.setInputValue("trekker_ready", Date.now(), { priority: "event" }); };
      jq(document).on("shiny:connected", ping);
    }
    var ro = new ResizeObserver(function () { resize(); });
    var panes = $("tk-panes"); if (panes) ro.observe(panes);
    window.addEventListener("resize", resize);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", boot);
  } else {
    boot();
  }
})();
