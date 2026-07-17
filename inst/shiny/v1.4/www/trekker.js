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
  var view = "pair", src = "csv", mode = "celltype", gene = null;
  var ps = 2.2, nr = 250, showEv = true, morphT = 0;
  var sel = null, pick = null;
  var confThresh = -1; // >=0 when the confidence dissolve filter is active
  var hidden = new Set();
  var U_SP = null, U_UM = null;
  var P = null;

  var fmt = function (n) {
    return n == null || isNaN(n) ? "—" : Number(n).toLocaleString("en-US");
  };
  var PAL = ["#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A", "#19D3F3",
    "#FF6692", "#B6E880", "#FF97FF", "#FECB52", "#2f6fd6", "#f97316", "#16a34a",
    "#9a5cd0", "#e05780", "#38b2ac", "#d97706", "#7bb0e8"];
  var CT_COL = { ExN: "#636EFA", InN: "#EF553B", Oligo: "#00CC96", Astro: "#AB63FA",
    Micro: "#f97316", OPC: "#19D3F3", DG: "#FF6692", Neuron: "#9a9aa0" };
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
    SRC = {
      csv: { x: D.x, y: D.y, t: "Location CSV", hint:
        "Vendor canonical. The only trustworthy source — plain text, no " +
        "Seurat-version coupling, and the coordinates the vendor Report itself plots." },
      img: { x: D.y, y: D.x, t: "@images$slice1", hint:
        "What the generic <code>.getSpatialData()</code> reads, and it raises no " +
        "error. Relative to canonical it is <b>axis-transposed</b>." },
      red: { x: D.x, y: D.y.map(function (v) { return -v; }), t: "SPATIAL reduction", hint:
        "Relative to canonical this is <b>y-axis mirrored</b>. To use it you must " +
        "record <code>transform: y_negate</code> explicitly." }
    };
    CT = D.clusters.map(function (c) { return D.celltype[c]; });
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
    return { nx: nx, ny: ny };
  }
  function rebuildSpatialUnit() { U_SP = unit(SRC[src].x, SRC[src].y); }

  function Pane(cvId, tipId, kind) {
    var cv = $(cvId);
    return { cv: cv, ctx: cv.getContext("2d"), tip: $(tipId), kind: kind,
      W: 0, H: 0, sx: null, sy: null, lasso: null, drag: false, moved: false };
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
  function project(p) {
    var c = coordsFor(p.kind), nx = c.nx, ny = c.ny, pad = 14,
      S = Math.min(p.W, p.H) - 2 * pad, ox = (p.W - S) / 2, oy = (p.H - S) / 2, i;
    p.sx = new Float32Array(N); p.sy = new Float32Array(N);
    for (i = 0; i < N; i++) { p.sx[i] = ox + nx[i] * S; p.sy[i] = oy + S - ny[i] * S; }
  }
  function paneList() { return view === "morph" ? [P.sp] : [P.sp, P.um]; }
  function resize() {
    if (!D || !P) return;
    var dpr = window.devicePixelRatio || 1;
    paneList().forEach(function (p) {
      var w = p.cv.parentElement.clientWidth;
      if (!w) return;
      p.W = w;
      p.H = view === "morph" ? Math.max(420, Math.min(660, Math.round(w * 0.62)))
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
  function baseColor(i) {
    if (isCont()) {
      var g = curField();
      if (!g) return "#d0d0d3";
      var c = viridis(g.v[i] / 255);
      return "rgb(" + c[0] + "," + c[1] + "," + c[2] + ")";
    }
    if (mode === "celltype") return CT_COL[CT[i]] || "#9a9aa0";
    return PAL[D.clusters[i] % PAL.length];
  }
  function visible(i) {
    if (mode === "celltype") return !hidden.has(CT[i]);
    if (mode === "cluster") return !hidden.has(D.clusters[i]);
    return true;
  }
  function draw(p) {
    if (!p.sx || !p.W) return; // not projected yet (e.g. tab still hidden)
    var c = p.ctx; c.clearRect(0, 0, p.W, p.H);
    var order = null, j, i, pass;
    var cf = confThresh >= 0 ? cfVals() : null; // confidence dissolve filter
    if (isCont() && curField()) {
      var gv = curField().v;
      order = Array.from({ length: N }, function (_, k) { return k; })
        .sort(function (a, b) { return gv[a] - gv[b]; });
    }
    for (pass = 0; pass < 2; pass++) {
      for (j = 0; j < N; j++) {
        i = order ? order[j] : j;
        if (!visible(i)) continue;
        var inSel = !sel || sel.has(i);
        if (pass === 0 ? inSel : !inSel) continue;
        var a = sel ? (inSel ? 0.95 : 0.06) : 0.85;
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
        c.strokeStyle = "rgba(249,115,22,.55)"; c.lineWidth = 1.2; c.setLineDash([4, 3]);
        c.beginPath(); c.arc(p.sx[pick], p.sy[pick], nr * k, 0, 6.2832); c.stroke();
        c.setLineDash([]);
      }
    }
    if (p.lasso && p.lasso.length > 1) {
      c.globalAlpha = 1; c.strokeStyle = "#2f6fd6"; c.lineWidth = 1.5;
      c.fillStyle = "rgba(47,111,214,.08)";
      c.beginPath(); c.moveTo(p.lasso[0][0], p.lasso[0][1]);
      p.lasso.slice(1).forEach(function (pt) { c.lineTo(pt[0], pt[1]); });
      c.closePath(); c.fill(); c.stroke();
    }
    c.globalAlpha = 1;
  }
  function drawAll() { if (P) paneList().forEach(draw); }

  function inPoly(x, y, poly) {
    var c = false, i, j;
    for (i = 0, j = poly.length - 1; i < poly.length; j = i++) {
      var xi = poly[i][0], yi = poly[i][1], xj = poly[j][0], yj = poly[j][1];
      if (((yi > y) !== (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi)) c = !c;
    }
    return c;
  }
  function nearest(p, mx, my) {
    var best = -1, bd = 169, i;
    for (i = 0; i < N; i++) {
      if (!visible(i)) continue;
      var dx = p.sx[i] - mx, dy = p.sy[i] - my, d = dx * dx + dy * dy;
      if (d < bd) { bd = d; best = i; }
    }
    return best;
  }
  function wire(p) {
    var pos = function (e) {
      var r = p.cv.getBoundingClientRect();
      return [e.clientX - r.left, e.clientY - r.top];
    };
    p.cv.addEventListener("mousedown", function (e) {
      p.drag = true; p.moved = false; p.lasso = [pos(e)];
    });
    p.cv.addEventListener("mousemove", function (e) {
      var m = pos(e), mx = m[0], my = m[1];
      if (p.drag) {
        var last = p.lasso[p.lasso.length - 1];
        if (Math.hypot(mx - last[0], my - last[1]) > 3) {
          p.lasso.push([mx, my]); p.moved = true; draw(p);
        }
        p.tip.style.opacity = 0; return;
      }
      var i = nearest(p, mx, my);
      if (i < 0) { p.tip.style.opacity = 0; return; }
      var h = "<b>" + CT[i] + "</b> · cluster " + D.clusters[i];
      h += p.kind === "sp"
        ? "<br>x " + SRC[src].x[i].toFixed(0) + " · y " + SRC[src].y[i].toFixed(0) + " µm"
        : "<br>UMAP " + D.ux[i].toFixed(1) + " , " + D.uy[i].toFixed(1);
      if (isCont() && curField()) {
        var g = curField();
        h += "<br>" + curLabel() + " <b>" + fieldValue(g, i).toFixed(2) + "</b>";
      }
      if (EV.has(i)) h += "<br><b>Has positioning evidence</b> · click to view";
      p.tip.innerHTML = h; p.tip.style.opacity = 1;
      var tx = p.sx[i] + 12, ty = p.sy[i] - 8;
      if (tx + p.tip.offsetWidth > p.W) tx = p.sx[i] - p.tip.offsetWidth - 12;
      p.tip.style.left = tx + "px"; p.tip.style.top = ty + "px";
    });
    p.cv.addEventListener("mouseleave", function () { p.tip.style.opacity = 0; });
    window.addEventListener("mouseup", function (e) {
      if (!p.drag) return;
      p.drag = false;
      if (p.moved && p.lasso.length > 2) {
        var s = new Set(), i;
        for (i = 0; i < N; i++) {
          if (visible(i) && inPoly(p.sx[i], p.sy[i], p.lasso)) s.add(i);
        }
        sel = s.size ? s : null; pick = null; renderSel();
      } else {
        var m = pos(e), k = nearest(p, m[0], m[1]);
        if (k >= 0) { pick = k; renderInspector(); }
      }
      p.lasso = null; drawAll();
    });
  }

  function renderSel() {
    var bar = $("tk-selbar");
    if (!sel) { bar.style.display = "none"; return; }
    bar.style.display = "flex";
    var cnt = {}; sel.forEach(function (i) { cnt[CT[i]] = (cnt[CT[i]] || 0) + 1; });
    var top = Object.entries(cnt).sort(function (a, b) { return b[1] - a[1]; })
      .slice(0, 4).map(function (e) { return e[0] + " " + e[1]; }).join(" · ");
    $("tk-seltext").innerHTML = "Selected <b>" + sel.size + "</b> / " + fmt(N) +
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
      return "<div class=\"tk-bar\"><span class=\"tk-nm\" style=\"color:" + (CT_COL[k] || "#666") +
        "\">" + k + "</span><span class=\"tk-tr\"><span class=\"tk-fl\" style=\"width:" +
        (v / mx * 100) + "%;background:" + (CT_COL[k] || "#999") + "\"></span></span>" +
        "<span class=\"tk-ct\">" + v + "</span></div>";
    }).join("") : "<div class=\"tk-hint\">No other nuclei within this radius — increase the niche radius.</div>";

    var ev = EV.get(i);
    var evHtml = ev
      ? "<img class=\"tk-evimg\" src=\"" + ev.img + "\" alt=\"positioning evidence\" " +
        "onclick=\"tkZoom(this.src)\" style=\"cursor:zoom-in\">" +
        "<div class=\"tk-hint\"><code>*</code> = adopted centroid · the field of grey dots are " +
        "nUMI=1 noise beads. <b>Why is this nucleus here — the evidence is here.</b></div>"
      : "<div class=\"tk-empty\">This nucleus has no official positioning-evidence image.<br>" +
        "<span class=\"tk-muted\">The vendor ships 50 per class; only ringed nuclei have one.</span></div>";

    var cf = curField();
    var g = isCont() && cf
      ? "<dt>" + curLabel() + "</dt><dd>" + fieldValue(cf, i).toFixed(2) + "</dd>"
      : "";
    var cs = D.conf;
    var confRows = cs
      ? "<dt>Position conf.</dt><dd>" + (cs.prop_top[i] * 100).toFixed(1) + "%</dd>" +
        "<dt>Bead noise</dt><dd>" + (cs.prop_noise[i] * 100).toFixed(0) + "%</dd>" +
        "<dt>Spatial barcodes</dt><dd>" + fmt(cs.sb_total[i]) + "</dd>"
      : "";
    el.innerHTML = "<div class=\"tk-insp\"><div>" +
      "<h4 class=\"tk-sub-h\">Identity</h4>" +
      "<dl class=\"tk-kv\"><dt>Cell type</dt><dd style=\"color:" + CT_COL[CT[i]] + "\">" + CT[i] + "</dd>" +
      "<dt>Cluster</dt><dd>" + D.clusters[i] + "</dd>" +
      "<dt>x</dt><dd>" + D.x[i].toFixed(0) + " µm</dd>" +
      "<dt>y</dt><dd>" + D.y[i].toFixed(0) + " µm</dd>" +
      "<dt>UMAP</dt><dd>" + D.ux[i].toFixed(1) + ", " + D.uy[i].toFixed(1) + "</dd>" + g + confRows + "</dl>" +
      "<div class=\"tk-hint\" style=\"word-break:break-all\">" + (ev ? ev.bc : "") + "</div></div>" +
      "<div><h4 class=\"tk-sub-h\">Physical neighbourhood " +
      "<span class=\"tk-muted\">r = " + nr + " µm · n = " + n + "</span></h4>" +
      "<div class=\"tk-bars\">" + bars + "</div>" +
      "<div class=\"tk-hint\"><b>Real cell counts, not a deconvolution estimate.</b> Visium cannot do " +
      "this — a spot is internally mixed.</div></div>" +
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
    var html = f.desc ? "<div class=\"tk-hint\" style=\"margin-top:8px\">" + f.desc + "</div>" : "";
    if (f.by_type && f.by_type.length) {
      var mx = Math.max.apply(null, f.by_type.map(function (b) { return b.median; })) || 1;
      var rows = f.by_type.slice().sort(function (a, b) { return b.median - a.median; })
        .map(function (b) {
          return "<div class=\"tk-bar\"><span class=\"tk-nm\" style=\"color:" + (CT_COL[b.type] || "#666") +
            "\">" + b.type + "</span><span class=\"tk-tr\"><span class=\"tk-fl\" style=\"width:" +
            (b.median / mx * 100) + "%;background:" + (CT_COL[b.type] || "#999") + "\"></span></span>" +
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
      var col = mode === "celltype" ? (CT_COL[k] || "#999") : PAL[k % PAL.length];
      var d = document.createElement("div");
      d.className = "tk-lg" + (hidden.has(k) ? " off" : "");
      d.innerHTML = "<span class=\"tk-dot\" style=\"background:" + col + "\"></span>" + k +
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

  /* ---- header / QC / evidence / moran (static per dataset) --------------- */
  function renderStatic() {
    var q = D.qc, m = D.meta;
    $("tk-b-assay").textContent = q.assay || "Trekker";
    $("tk-b-tile").textContent = "Tile " + (q.tile_id || "—");
    $("tk-subline").innerHTML = "<code>" + q.sample_id + "</code> · " + fmt(m.n_cells) +
      " nuclei (down-sampled from " + fmt(m.n_cells_full) + " confidently positioned) · " +
      fmt(m.n_genes_obj) + " genes (whole transcriptome, not a panel) · coordinate unit " + m.unit;
    var vn = $("tk-vnote"); if (vn) vn.textContent = fmt(m.n_cells) + " nuclei · two coordinate systems";

    var pass2p = q.pct_2plus < 20;
    $("tk-stats").innerHTML = [
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

    var tot = q.total_nuclei;
    $("tk-postbl").innerHTML = [
      ["0 (unpositioned)", q.n_0, "Excluded · coordinate is the <code>0,0</code> sentinel"],
      ["1", q.n_1, "<b>Imported</b> (incl. salvaged)"],
      ["2", q.n_2, "Excluded"], ["3", q.n_3, "Excluded"], ["≥4", q.n_4p, "Excluded"]
    ].map(function (r) {
      return "<tr><td>" + r[0] + "</td><td class=\"num\">" + fmt(r[1]) + "</td><td class=\"num\">" +
        (r[1] / tot * 100).toFixed(2) + "%</td><td class=\"tk-muted\">" + r[2] + "</td></tr>";
    }).join("");

    var salv = q.salv_2 + q.salv_3;
    $("tk-salvflag").innerHTML = "<b>Confidently positioned ≠ exactly one location.</b> The " +
      fmt(q.n_1) + " imported nuclei = native single-location " + fmt(q.o_1) +
      " + vendor-salvaged from 2 (" + q.salv_2 + ") + from 3 (" + q.salv_3 + "), i.e. <b>" + salv +
      " (" + (salv / q.n_1 * 100).toFixed(2) + "%)</b> are upstream-salvaged multi-location nuclei. " +
      "The label must be <code>vendor_confidently_positioned</code>.";

    $("tk-prov").innerHTML = [
      ["Platform / assay", q.assay], ["Sample ID", q.sample_id], ["Tile ID", q.tile_id],
      ["Pipeline version", "<span class=\"tk-muted\">missing (metric absent)</span>"],
      ["Coordinate source", "Location CSV (canonical)"],
      ["Coordinate unit", "µm <span class=\"tk-muted\">(per manual; not declared in file)</span>"],
      ["DBSCAN eps", q.eps], ["minPts", q.min_sb],
      ["Histology image", "<span class=\"tk-muted\">none (not provided in bundle)</span>"],
      ["Moran's I source", "<span class=\"tk-badge tk-badge-soft\" style=\"font-size:10px\">Upstream</span>"]
    ].map(function (r) { return "<dt>" + r[0] + "</dt><dd>" + r[1] + "</dd>"; }).join("");

    $("tk-rangeflag").innerHTML = "<b>The vendor's own demo crosses the vendor's own reference line.</b> " +
      "The 2+ location rate " + q.pct_2plus + "% > the manual's suggested <20%. The app should only show " +
      "\"below vendor reference range\" and must not adjudicate sample usability for the user.";

    var EXLAB = { 0: "0 locations (unpositioned) — excluded",
      2: "2 locations (ambiguous) — excluded", 3: "3 locations (ambiguous) — excluded" };
    $("tk-exgrid").innerHTML = (D.qc_examples || []).map(function (e) {
      if (!e.img) return "";
      return "<figure><img src=\"" + e.img + "\" loading=\"lazy\" onclick=\"tkZoom(this.src)\">" +
        "<figcaption><b>" + (EXLAB[e.class] || e.class) + "</b><br>" +
        "<span class=\"tk-muted\">" + e.n + " examples available</span></figcaption></figure>";
    }).join("");

    $("tk-morantbl").innerHTML = D.moran.map(function (r) {
      return "<tr><td class=\"num tk-muted\">" + r.rank + "</td>" +
        "<td style=\"font-weight:600\">" + r.gene + "</td><td class=\"num\">" + r.I.toFixed(4) + "</td>" +
        "<td><a href=\"#\" class=\"tk-link\" data-g=\"" + r.gene + "\">Show in plot →</a></td></tr>";
    }).join("");
    $("tk-morantbl").querySelectorAll("a").forEach(function (a) {
      a.onclick = function (e) {
        e.preventDefault();
        if (window.Shiny) Shiny.setInputValue("trekker_moran_gene", a.dataset.g, { priority: "event" });
        var vc = document.querySelector(".cerebro-viz-col");
        if (vc) vc.scrollIntoView({ behavior: "smooth", block: "start" });
      };
    });
    var sh = $("tk-srchint"); if (sh) sh.innerHTML = SRC.csv.hint;
  }

  /* ---- init on data arrival ---------------------------------------------- */
  function initFromData(data) {
    D = data;
    if (!D.genes) D.genes = {};
    N = D.x.length;
    rebuildSources();
    U_UM = unit(D.ux, D.uy);
    rebuildSpatialUnit();
    if (!P) P = { sp: Pane("tk-cv-sp", "tk-tip-sp", "sp"), um: Pane("tk-cv-um", "tk-tip-um", "um") };
    if (!P.sp._wired) { Object.values(P).forEach(function (p) { wire(p); p._wired = true; }); }
    var sc = $("tk-selclear"); if (sc && !sc._wired) { sc.onclick = function () { sel = null; renderSel(); drawAll(); }; sc._wired = true; }
    sel = null; pick = null; hidden.clear();
    renderStatic();
    renderLegend();
    renderInspector();
    resize();
  }

  /* ---- control changes: driven by the Shiny inputs ----------------------- */
  function onInput(name, value) {
    if (!D) return;
    switch (name) {
      case "trekker_view":
        view = value;
        $("tk-panes").classList.toggle("single", view === "morph");
        $("tk-pane-um").style.display = view === "morph" ? "none" : "";
        var t = document.querySelector("#tk-pane-sp .tk-pane-h span:first-child");
        if (t) t.textContent = view === "morph" ? "UMAP → Spatial" : "Spatial";
        resize();
        break;
      case "trekker_mode":
        mode = value; hidden.clear(); renderLegend(); drawAll();
        if (pick != null) renderInspector();
        break;
      case "trekker_src":
        src = value;
        var sh = $("tk-srchint"); if (sh) sh.innerHTML = SRC[src].hint;
        $("tk-u-sp").textContent = "µm · " + SRC[src].t;
        rebuildSpatialUnit(); resize();
        break;
      case "trekker_ps": ps = +value; drawAll(); break;
      case "trekker_nr":
        nr = +value; if (pick != null) renderInspector(); else drawAll();
        break;
      case "trekker_morph":
        morphT = +value; if (P) { project(P.sp); draw(P.sp); }
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
