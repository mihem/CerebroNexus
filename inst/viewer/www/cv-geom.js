/*----------------------------------------------------------------------------*
 * Shared 2-D geometry kernels for the cell_views.js canvas engine.
 *
 * Specialist and linked panels render the same cells and use these pure
 * geometry primitives through one engine.
 *
 * Pure functions over plain (typed) arrays — no engine state, no DOM. Loaded
 * before the engine (see shiny_UI.R) so `window.CBGeom` is ready when its IIFE
 * runs.
 *----------------------------------------------------------------------------*/
(function () {
  var G = {};

  // Ray-casting point-in-polygon. poly = [[x, y], ...]. Was byte-identical in
  // all cell-view modes.
  G.inPoly = function (x, y, poly) {
    var c = false, n = poly.length, i, j;
    for (i = 0, j = n - 1; i < n; j = i++) {
      var xi = poly[i][0], yi = poly[i][1], xj = poly[j][0], yj = poly[j][1];
      if (((yi > y) !== (yj > y)) &&
        (x < (xj - xi) * (y - yi) / (yj - yi) + xi)) c = !c;
    }
    return c;
  };

  // The niche: the `centre` cell + every cell within radius r (r2 = r*r) of
  // (px, py) in DATA space. Returns a Set of indices. The two engines differ
  // Flags stay explicit at the call site for the few modality-specific rules.
  //   inclusive — whether cells exactly on the radius are included
  //   skipNaN   — skip null/NaN coords, i.e. unpositioned cells
  G.nicheAround = function (xs, ys, n, centre, px, py, r2, inclusive, skipNaN) {
    var s = new Set();
    s.add(centre);
    for (var i = 0; i < n; i++) {
      var xi = xs[i], yi = ys[i];
      if (skipNaN && (xi == null || isNaN(xi))) continue;
      var dx = xi - px, dy = yi - py, d = dx * dx + dy * dy;
      if (inclusive ? d <= r2 : d < r2) s.add(i);
    }
    return s;
  };

  // One viewport model for Linked Views and the specialist canvases. A null
  // view means the full unit square; zoomed views use centre + square span.
  G.viewBounds = function (view) {
    if (view && view.x0 != null) {
      return { x0: view.x0, x1: view.x1, y0: view.y0, y1: view.y1 };
    }
    var v = view || { cx: 0.5, cy: 0.5, span: 1 };
    return {
      x0: v.cx - v.span / 2,
      x1: v.cx + v.span / 2,
      y0: v.cy - v.span / 2,
      y1: v.cy + v.span / 2
    };
  };

  G.zoomView = function (view, factor, anchor, minSpan) {
    var v = view || { cx: 0.5, cy: 0.5, span: 1 };
    var span = v.span * factor;
    if (span >= 1) return null;
    span = Math.max(minSpan == null ? 0.04 : minSpan, span);
    var a = anchor || [0.5, 0.5];
    var ux = v.cx + (a[0] - 0.5) * v.span;
    var uy = v.cy + (a[1] - 0.5) * v.span;
    return {
      cx: ux - (a[0] - 0.5) * span,
      cy: uy - (a[1] - 0.5) * span,
      span: span
    };
  };

  G.panView = function (view, dx, dy) {
    var v = view || { cx: 0.5, cy: 0.5, span: 1 };
    return {
      cx: v.cx - dx * v.span,
      cy: v.cy + dy * v.span,
      span: v.span
    };
  };

  G.fitView = function (x0, x1, y0, y1, padding, minSpan) {
    var span = Math.max(x1 - x0, y1 - y0) * padding;
    span = Math.max(minSpan, span);
    return {
      cx: (x0 + x1) / 2,
      cy: (y0 + y1) / 2,
      span: span
    };
  };

  G.screenToUnit = function (view, frame, polygon) {
    var b = G.viewBounds(view);
    return polygon.map(function (point) {
      return [
        b.x0 + (point[0] - frame.x) / frame.width * (b.x1 - b.x0),
        b.y0 + (frame.y + frame.height - point[1]) / frame.height *
          (b.y1 - b.y0)
      ];
    });
  };

  G.unitToScreen = function (view, frame, polygon) {
    var b = G.viewBounds(view);
    return polygon.map(function (point) {
      return [
        frame.x + (point[0] - b.x0) / (b.x1 - b.x0) * frame.width,
        frame.y + frame.height -
          (point[1] - b.y0) / (b.y1 - b.y0) * frame.height
      ];
    });
  };

  // Linked Views semantics: 3-D selection gestures orbit; middle/shift drag
  // and explicit Pan/Orbit modes pan a flat view; only the remainder selects.
  G.dragKind = function (mode, is3D, button, shiftKey) {
    if (is3D && mode !== 'pan' && button !== 1 && !shiftKey) return 'orbit';
    if (mode === 'pan' || mode === 'orbit' || button === 1 || shiftKey) {
      return 'pan';
    }
    return 'select';
  };

  window.CBGeom = G;
})();
