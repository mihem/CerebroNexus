/*----------------------------------------------------------------------------*
 * HLA & TCR Motifs — a modebar for the visNetwork motif network.
 *
 * visNetwork's own green navigation buttons clash with the app's plotly modebar,
 * so they are turned off (hla_tcr_motifs/visualizations.R) and replaced here by a
 * toolbar with the same plotly icons and top-right placement, wired to the
 * vis.Network API. Drag-to-pan stays on; zoom is button-only (scroll-to-zoom is
 * off in visualizations.R, so the graph cannot shrink past the opening fit). The
 * buttons do zoom in / out, reset (fit), and PNG export.
 *
 * The network re-renders whenever a parameter changes (a new vis.Network
 * instance), so each button looks the instance up fresh via HTMLWidgets rather
 * than caching it.
 *----------------------------------------------------------------------------*/
(function () {
  "use strict";

  // plotly's own icon paths, so the toolbar reads as the same control as the
  // modebar on the plotly tabs.
  var TR = "matrix(1 0 0 -1 0 850)";
  var ICONS = {
    zoomin: { vb: "0 0 875 1000", d: "m1 787l0-875 875 0 0 875-875 0z m687-500l-187 0 0-187-125 0 0 187-188 0 0 125 188 0 0 187 125 0 0-187 187 0 0-125z" },
    zoomout: { vb: "0 0 875 1000", d: "m0 788l0-876 875 0 0 876-875 0z m688-500l-500 0 0 125 500 0 0-125z" },
    reset: { vb: "0 0 928.6 1000", d: "m786 296v-267q0-15-11-26t-25-10h-214v214h-143v-214h-214q-15 0-25 10t-11 26v267q0 1 0 2t0 2l321 264 321-264q1-1 1-4z m124 39l-34-41q-5-5-12-6h-2q-7 0-12 3l-386 322-386-322q-7-4-13-4-7 2-12 7l-35 41q-4 5-3 13t6 12l401 334q18 15 42 15t43-15l136-114v109q0 8 5 13t13 5h107q8 0 13-5t5-13v-227l122-102q5-5 6-12t-4-13z" },
    download: { vb: "0 0 1000 1000", d: "m500 450c-83 0-150-67-150-150 0-83 67-150 150-150 83 0 150 67 150 150 0 83-67 150-150 150z m400 150h-120c-16 0-34 13-39 29l-31 93c-6 15-23 28-40 28h-340c-16 0-34-13-39-28l-31-94c-6-15-23-28-40-28h-120c-55 0-100-45-100-100v-450c0-55 45-100 100-100h800c55 0 100 45 100 100v450c0 55-45 100-100 100z m-400-550c-138 0-250 112-250 250 0 138 112 250 250 250 138 0 250-112 250-250 0-138-112-250-250-250z m365 380c-19 0-35 16-35 35 0 19 16 35 35 35 19 0 35-16 35-35 0-19-16-35-35-35z" }
  };
  function svgIcon(ic) {
    return "<svg viewBox=\"" + ic.vb + "\" width=\"15\" height=\"15\">" +
      "<path d=\"" + ic.d + "\" transform=\"" + TR + "\" fill=\"currentColor\"></path></svg>";
  }

  function net() {
    if (!window.HTMLWidgets || !window.HTMLWidgets.find) return null;
    var w = window.HTMLWidgets.find("#hla_plot_motifNetwork");
    return w && w.network ? w.network : null;
  }
  function zoomBy(f) {
    var n = net();
    if (!n || typeof n.getScale !== "function") return;
    // The initial fit is the floor (set by visEvents in visualizations.R): the
    // zoom-out button can only return to it, never below.
    var lo = (typeof n.hlaMinScale === "number") ? n.hlaMinScale : 0.02;
    n.moveTo({
      scale: Math.max(lo, Math.min(6, n.getScale() * f)),
      animation: { duration: 200 }
    });
  }
  function resetView() {
    var n = net();
    if (n && typeof n.fit === "function") n.fit({ animation: { duration: 300 } });
  }
  function downloadPNG() {
    var n = net();
    if (!n || !n.canvas || !n.canvas.frame) return;
    var a = document.createElement("a");
    a.href = n.canvas.frame.canvas.toDataURL("image/png");
    a.download = "hla_motif_network.png";
    a.click();
  }

  var BTNS = [
    { act: "zoomin", title: "Zoom in", ic: ICONS.zoomin },
    { act: "zoomout", title: "Zoom out", ic: ICONS.zoomout },
    { act: "reset", title: "Reset view", ic: ICONS.reset },
    { act: "download", title: "Download plot as a png", ic: ICONS.download }
  ];
  function build() {
    var bar = document.getElementById("hla-modebar");
    if (!bar || bar._built) return;
    bar._built = true;
    bar.innerHTML = BTNS.map(function (b) {
      return "<a class=\"hla-mb-btn\" data-act=\"" + b.act + "\" title=\"" + b.title +
        "\" role=\"button\">" + svgIcon(b.ic) + "</a>";
    }).join("");
    bar.querySelectorAll(".hla-mb-btn").forEach(function (a) {
      a.onclick = function () {
        var k = a.getAttribute("data-act");
        if (k === "zoomin") zoomBy(1.3);
        else if (k === "zoomout") zoomBy(1 / 1.3);
        else if (k === "reset") resetView();
        else if (k === "download") downloadPNG();
      };
    });
  }
  // The #hla-modebar container is static in tabItems but the tab is inserted
  // lazily, so poll a few times until it exists, then build once.
  function tryBuild(n) {
    build();
    var bar = document.getElementById("hla-modebar");
    if ((!bar || !bar._built) && n > 0) {
      setTimeout(function () { tryBuild(n - 1); }, 500);
    }
  }

  var jq = window.jQuery;
  if (jq) {
    jq(document).on("shiny:connected", function () { tryBuild(30); });
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () { tryBuild(30); });
  } else {
    tryBuild(30);
  }
})();
