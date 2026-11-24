// A colour-only change of a panel's GPU points (updateGpuColors()) writes one
// 32-bit word per cell and does not visit the cells again to check the mask
// and positions it already has. Its output is the same as the byte-by-byte
// version it replaced, whose text is kept here as the reference.
const fs = require('fs'), assert = require('assert/strict');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(from, to) {
  const start = source.indexOf(from), end = source.indexOf(to, start);
  assert.ok(start >= 0 && end > start, 'missing ' + from);
  return source.slice(start, end);
}
const REFERENCE = `  function updateGpuColorsReference(p, state, hiSet, shownMask) {
    var previous = p.gpuData, oldState = p.gpuDataState, unit = state.unit;
    var order = previous && paintOrder(p);
    if (!previous || !oldState || !previous.vertexIndex || previous.ordered ||
        !unit || unit._pending || panelColorMode(p) === RGB_MODE ||
        (order && !gpuPaintOrder(p, order, previous.count)) ||
        Object.keys(state).some(function (key) {
          return GPU_COLOUR_KEYS.indexOf(key) < 0 && state[key] !== oldState[key];
        })) return null;
    var n = D.n, vertexIndex = previous.vertexIndex, space = spaceById[p.spaceId];
    var cells = space && space._cells && space._cells.length <= n ? space._cells : null;
    if ((previous.vertexCells || null) !== cells) return null;
    var visits = cells ? cells.length : n, drawn = 0, at, i;
    for (at = 0; at < visits; at++) {
      i = cells ? cells[at] : at;
      var shown = !!(unit.ok[i] && shownMask[i]);
      if (shown !== (vertexIndex[at] > 0)) return null;
      if (shown) drawn++;
    }
    if (drawn !== previous.count) return null;
    var colors = new Uint8Array(previous.colors.length);
    // Layers follow only the selection/niche: keep them, and their upload,
    // when only the colours change.
    var sameLayers = state.selection === oldState.selection &&
      state.niche === oldState.niche;
    var layers = sameLayers ? previous.layers : new Uint32Array(previous.layers.length);
    var group = catOf(panelColorMode(p));
    var palette = group ? group.levels.map(function (_level, index) {
      return gpuColor((group.colors && group.colors[index]) || PAL[index % PAL.length]);
    }) : null;
    var missing = group ? gpuColor('#cccccc') : null;
    // A continuous colouring: each colour is one of 256 steps, as in
    // buildGpuData(); a value off the scale (NaN) takes colorOf().
    var scale = group ? null : continuousColours(p), stepRgba = scale ? [] : null;
    var opacity = pointOpacityOf(p), opaque = true, foreground = false;
    for (at = 0; at < visits; at++) {
      i = cells ? cells[at] : at;
      var vertex = vertexIndex[at] - 1;
      if (vertex < 0) continue;
      var fg = !!(hiSet && hiSet.has(i));
      var alpha = hiSet ? (fg ? 0.95 : 0.05) : opacity;
      var category = group && group.values[i];
      var rgba;
      if (group) {
        rgba = category == null || category < 0 || category >= palette.length ? missing : palette[category];
      } else if (scale) {
        var value = scale.values[i];
        var step = scale.missing && (value == null || isNaN(value)) ? -1
          : Math.round(clipT(value, scale.span, scale.range) * 255);
        rgba = step >= 0 && step <= 255
          ? (stepRgba[step] || (stepRgba[step] = gpuColor(scale.colour(step))))
          : gpuColor(colorOf(i, p));
      } else {
        rgba = gpuColor(colorOf(i, p));
      }
      if (rgba[3] !== 255) opaque = false;
      colors[vertex * 4] = rgba[0];
      colors[vertex * 4 + 1] = rgba[1];
      colors[vertex * 4 + 2] = rgba[2];
      colors[vertex * 4 + 3] = Math.round(rgba[3] * alpha);
      if (!sameLayers) layers[vertex] = fg ? 1 : 0;
      foreground = foreground || fg;
    }
    p.gpuDataState = state;
    p.gpuData = {positions: previous.positions, colors: colors, layers: layers,
      count: previous.count, foreground: foreground, opaque: opaque,
      vertexIndex: vertexIndex, vertexCells: cells, ordered: false,
      drawOrder: order ? (previous.drawOrder && previous.drawOrderOf === order
        ? previous.drawOrder : gpuDrawOrder(order, cells, vertexIndex, previous.count)) : null,
      drawOrderOf: order || null};
    return p.gpuData;
  }
`;

// ---- what the function reads --------------------------------------------------
const RGB_MODE = '__rgb__', PAL = ['#ff0000', '#00ff00', '#0000ff', '#ffff00'];
var D = {n: 0}, spaceById = {}, _clipVal = null, CURRENT = null;
const panelColorMode = p => p.mode;
const paintOrder = () => null, gpuPaintOrder = () => null;
const catOf = mode => mode === 'group' ? CURRENT.group : null;
const continuousColours = () => CURRENT.scale;
const pointOpacityOf = () => CURRENT.opacity;
const colorOf = i => (i % 2 ? '#e6e7ea' : 'rgba(10,20,30,0.5)');
function gpuColor(css) {
  // A CSS colour as [r, g, b, a], from its own text.
  const m = /rgba?\((\d+),(\d+),(\d+)(?:,([\d.]+))?\)/.exec(css);
  if (m) return [+m[1], +m[2], +m[3], Math.round(255 * (m[4] === undefined ? 1 : +m[4]))];
  let h = 0; for (const ch of css) h = (h * 31 + ch.charCodeAt(0)) >>> 0;
  return [h & 255, (h >> 8) & 255, (h >> 16) & 255, 255];
}
function clipT(v, span, range) {
  var r = range || _clipVal;
  if (!r) return v / span;
  if (r.hi <= r.lo) return 0;
  return Math.max(0, Math.min(1, (v - r.lo) / (r.hi - r.lo)));
}
eval(between('  var GPU_LITTLE_ENDIAN', '  function updateGpuColors('));
eval(REFERENCE);
eval(between('  function updateGpuColors(', '  function buildGpuData('));

// ---- scenarios ------------------------------------------------------------------
let seed = 11;
const rnd = () => { seed = (Math.imul(seed, 1103515245) + 12345) >>> 0; return seed / 4294967296; };
function scenario(name, make, {hiSet = null, sameMask = true, subset = false, n = 3000} = {}) {
  D = {n};
  const cellsAll = subset ? Uint32Array.from({length: n >> 1}, (_, i) => i * 2 + (i % 5 === 0 ? 1 : 0)) : null;
  spaceById.s = {_cells: cellsAll};
  const visits = cellsAll ? cellsAll.length : n;
  const ok = Uint8Array.from({length: n}, () => rnd() < 0.9 ? 1 : 0);
  const shownMask = Uint8Array.from({length: n}, () => rnd() < 0.85 ? 1 : 0);
  const vertexIndex = new Uint32Array(visits); let count = 0;
  for (let at = 0; at < visits; at++) { const i = cellsAll ? cellsAll[at] : at; if (ok[i] && shownMask[i]) vertexIndex[at] = ++count; }
  CURRENT = make(n);
  const run = fn => {
    const unit = {ok, _pending: false};
    const state = {unit, mode: CURRENT.mode, opacity: CURRENT.opacity, selection: null, niche: null};
    const previous = {colors: new Uint8Array(count * 4), layers: new Uint32Array(count), vertexIndex, count,
      vertexCells: cellsAll, ordered: false, drawOrder: null, shownMask: sameMask ? shownMask : null, okRef: sameMask ? ok : null};
    const p = {spaceId: 's', mode: CURRENT.mode, gpuData: previous, gpuDataState: {unit, mode: 'other', opacity: CURRENT.opacity, selection: null, niche: null}};
    const out = fn(p, state, hiSet, shownMask);
    return out && {colors: Array.from(out.colors), layers: Array.from(out.layers), count: out.count, opaque: out.opaque,
      foreground: out.foreground, vertexIndex: Array.from(out.vertexIndex)};
  };
  const before = run(updateGpuColorsReference), after = run(updateGpuColors);
  assert.deepEqual(after, before, name);
  assert.ok(before && before.count > 0, name + ' has points');
  return before;
}
const gene = n => ({mode: '__gene__', opacity: 1, group: null,
  scale: {values: Float32Array.from({length: n}, () => rnd() < 0.05 ? NaN : rnd() * 255), span: 255, range: null, missing: false,
    colour: step => 'rgb(' + step + ',' + (255 - step) + ',80)'}});
scenario('gene, no clip range', gene);
scenario('gene, on a subset of cells', gene, {subset: true});
scenario('gene, translucent', n => Object.assign(gene(n), {opacity: 0.6}));
scenario('gene, a clip range', n => { const c = gene(n); c.scale.range = {lo: 30, hi: 200}; return c; });
scenario('gene, an empty clip range', n => { const c = gene(n); c.scale.range = {lo: 100, hi: 100}; return c; });
const translucentStep = scenario('gene, a step with alpha', n => { const c = gene(n); c.scale.colour = step => step === 128 ? 'rgba(1,2,3,0.5)' : 'rgb(' + step + ',9,9)'; return c; });
assert.equal(translucentStep.opaque, false, 'a used translucent step makes the points not opaque');
scenario('field with missing values and its own scale', n => ({mode: 'field', opacity: 0.8, group: null,
  scale: {values: Array.from({length: n}, () => rnd() < 0.1 ? null : (rnd() < 0.1 ? NaN : Math.floor(rnd() * 100))), span: 100, range: {lo: 10, hi: 90}, missing: true,
    colour: step => 'rgb(' + (step ^ 85) + ',' + step + ',' + (255 - step) + ')'}}));
scenario('categorical, with missing and out-of-range levels', n => ({mode: 'group', opacity: 1, scale: null,
  group: {levels: ['a', 'b', 'c', 'd'], colors: ['#102030', null, 'rgba(5,6,7,0.4)', '#aabbcc'],
    values: Array.from({length: n}, () => { const r = rnd(); return r < 0.08 ? null : r < 0.12 ? -1 : r < 0.16 ? 9 : Math.floor(rnd() * 4); })}}));
scenario('categorical, subset and opacity', n => ({mode: 'group', opacity: 0.5, scale: null,
  group: {levels: ['a', 'b'], colors: ['#102030', '#aabbcc'], values: Int32Array.from({length: n}, () => Math.floor(rnd() * 2))}}), {subset: true});
scenario('a selection takes the general path', gene, {hiSet: new Set([3, 5, 8, 21, 55, 144, 377, 610, 987])});
// Without the very same mask and positions, they are checked, and a mask that
// no longer fits is refused as it was.
scenario('mask checked when it is not the vertices own', gene, {sameMask: false});
D = {n: 6}; spaceById.s = {_cells: null}; CURRENT = gene(6);
const vertexIndex = Uint32Array.from([1, 0, 2, 0, 0, 3]), ok = Uint8Array.from([1, 1, 1, 1, 1, 1]);
const wrongMask = Uint8Array.from([1, 1, 1, 0, 0, 1]);
for (const fn of [updateGpuColorsReference, updateGpuColors]) {
  const unit = {ok, _pending: false};
  const p = {spaceId: 's', mode: '__gene__', gpuData: {colors: new Uint8Array(12), layers: new Uint32Array(3), vertexIndex, count: 3, vertexCells: null, ordered: false, shownMask: null, okRef: null},
    gpuDataState: {unit, mode: 'other', opacity: 1, selection: null, niche: null}};
  assert.equal(fn(p, {unit, mode: '__gene__', opacity: 1, selection: null, niche: null}, null, wrongMask), null, 'a mask that does not fit the vertices is refused');
}
console.log('PASS the colour update writes the bytes the byte-by-byte version wrote');
