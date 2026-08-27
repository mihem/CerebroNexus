/*----------------------------------------------------------------------------*
 * Shared 2-D geometry kernels for the canvas engines (coordviews.js, trekker.js).
 *
 * Both engines render the SAME cells as points on a canvas and need the same
 * primitives. Each used to keep its own copy; the niche radius rule had already
 * drifted between them (<= vs <). Keeping ONE copy here makes that class of drift
 * impossible — change the loop once and both engines follow.
 *
 * Pure functions over plain (typed) arrays — no engine state, no DOM. Loaded
 * before both engines (see shiny_UI.R) so `window.CBGeom` is ready when their
 * IIFEs run. Only the genuinely shared kernels live here: `nearest` (hover hit-
 * test, per-engine visibility + hit radius) and `project` (unrelated bodies that
 * merely share a name) deliberately stay inline in each engine.
 *----------------------------------------------------------------------------*/
(function () {
  var G = {};

  // Ray-casting point-in-polygon. poly = [[x, y], ...]. Was byte-identical in
  // both engines.
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
  // ONLY in these flags, kept explicit at the call site instead of as two
  // divergent loop bodies:
  //   inclusive — <= r2 (coordviews) vs < r2 (trekker)
  //   skipNaN   — skip null/NaN coords, i.e. unpositioned cells (coordviews)
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

  window.CBGeom = G;
})();
