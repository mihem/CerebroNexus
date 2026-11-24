// The Spatial section grid in cell_views.js: shared slice geometry, the
// browser's choice of sections and columns, and synced zoom.
const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(from, to) {
  const start = source.indexOf(from), end = source.indexOf(to, start);
  assert.ok(start >= 0 && end > start, 'missing ' + from);
  return source.slice(start, end);
}

// ---- state the extracted functions read ------------------------------------
var D = { n: 9, rows: 'all', cells: ['1', '2', '3', '4', '5', '6', '7', '8', '9'] };
var singleActive = 'spatial_projection';
var singleGridSpaces = [], singleGridExtras = [], singleSpaceIds = [], spaceById = {}, panels = [];
var gridSyncing = false, _layoutKey = 'stale', focusPanel = null;
var calls = [];
function alignedRows(keys) {
  return keys.map(function (key) {
    var at = D.cells.indexOf(String(key)); return at < 0 ? null : at;
  });
}
function clampView(p, view) { return view; }
function project(p) { calls.push('project:' + p.key); }
function drawPanel(p) { calls.push('draw:' + p.key); }
function isSpatialSpace() { return true; }
function loadSpaceImage(space) { space._imgToken = 1; calls.push('image:' + space.id); }
function ensurePanelSlots() {}
function buildPanels() {}
function layoutPanels() {
  panels.forEach(function (p, i) { p.spaceId = singleSpaceIds[i] || null; });
}
function resizeAll() { calls.push('resize'); }
function drawAll() { calls.push('drawAll'); }
var elements = {};
var document = {
  getElementById: function (id) { return elements[id] || null; }
};
var PANEL_MIN_SIDE = 250, BALANCED_SIZE_TOLERANCE = 0.85;
function panelCap() { return 0; }

eval(between('  function keysArePositional(', '  function buildSpecialistPanels('));
eval(between('  function bestOverviewGrid(', '  // `ifChanged`'));

// ---- keys that are the view's cells --------------------------------------------
// A frame's own keys name its cells in order: no barcode Map is built.
assert.strictEqual(keysArePositional(D.cells.slice()), true);
assert.strictEqual(keysArePositional([1, 2, 3, 4, 5, 6, 7, 8, 9]), true, 'numeric keys as sent');
assert.strictEqual(keysArePositional(['9', '8', '7', '6', '5', '4', '3', '2', '1']), false);
assert.strictEqual(keysArePositional(['1', '2']), false);
assert.strictEqual(keysArePositional([]), false);

// ---- shared geometry ---------------------------------------------------------
// Three sections in frame order; keys are the frame's own rows.
var frame = {
  x: Float32Array.from([1, 2, 3, 4, 5, 6, 7, 8, 9]),
  y: Float32Array.from([9, 8, 7, 6, 5, 4, 3, 2, 1]),
  panels: [{ start: 0, count: 3 }, { start: 3, count: 2 }, { start: 5, count: 4 }]
};
var shared = sharedSliceGeometry(frame, D.cells.slice());
assert.ok(shared, 'disjoint slices share their geometry');
assert.ok(shared.x instanceof Float32Array, 'in the precision the frame sent');
assert.deepStrictEqual(Array.from(shared.x), [1, 2, 3, 4, 5, 6, 7, 8, 9]);
assert.deepStrictEqual(shared.cells.map(function (cells) { return Array.from(cells); }),
  [[0, 1, 2], [3, 4], [5, 6, 7, 8]]);
// Keys in another order than the view's cells: each slice still names the
// view cells its keys map to, in canonical order.
var shuffled = sharedSliceGeometry(frame, ['9', '8', '7', '6', '5', '4', '3', '2', '1']);
assert.deepStrictEqual(Array.from(shuffled.cells[0]), [6, 7, 8]);
assert.strictEqual(shuffled.x[8], 1);
// The grid's embedding has positions of its own; the sections still share.
var withEmbedding = sharedSliceGeometry(Object.assign({}, frame, {
  panels: [{ x: [1], y: [1] }].concat(frame.panels)
}), D.cells.slice());
assert.strictEqual(withEmbedding.cells[0], null);
assert.deepStrictEqual(Array.from(withEmbedding.cells[3]), [5, 6, 7, 8]);
// Two slices naming one cell, or a single slice: each panel keeps arrays of
// its own.
assert.strictEqual(sharedSliceGeometry(Object.assign({}, frame, {
  panels: [{ start: 0, count: 4 }, { start: 3, count: 2 }]
}), D.cells.slice()), null);
assert.strictEqual(sharedSliceGeometry(Object.assign({}, frame, {
  panels: [{ start: 0, count: 9 }, { x: [1], y: [1] }]
}), D.cells.slice()), null);
console.log('PASS grid sections share one pair of position arrays');

// ---- shown sections and their order -------------------------------------------
['A1', 'A2', 'B1'].forEach(function (name, i) {
  var id = 'single::spatial_projection::section-' + (i + 1);
  spaceById[id] = { id: id, _gridSection: name };
  singleGridSpaces.push(id);
});
assert.deepStrictEqual(gridShownSpaces(), singleGridSpaces, 'unbound control shows all');
elements.spatial_projection_sections = { selectize: { items: ['B1', 'A1'] } };
assert.deepStrictEqual(gridShownSpaces(), [singleGridSpaces[2], singleGridSpaces[0]]);
// A name the frame does not have is ignored; an empty choice shows all.
elements.spatial_projection_sections.selectize.items = ['X', 'A2'];
assert.deepStrictEqual(gridShownSpaces(), [singleGridSpaces[1]]);
elements.spatial_projection_sections.selectize.items = [];
assert.deepStrictEqual(gridShownSpaces(), singleGridSpaces);
// A frame of some sections only (the server's cell budget): a section beyond
// them waits for a frame, one among them is arranged at once.
assert.strictEqual(gridFrameHolds(['B1', 'A1']), true);
assert.strictEqual(gridFrameHolds('A2'), true);
assert.strictEqual(gridFrameHolds(['A1', 'C7']), false);
// The embedding is always shown, first.
singleGridExtras = ['single::spatial_projection::projection'];
assert.deepStrictEqual(gridShownSpaces(),
  ['single::spatial_projection::projection'].concat(singleGridSpaces));
singleGridExtras = [];
// Not the Spatial page: no grid.
singleActive = 'overview_projection';
assert.strictEqual(sectionGridActive(), false);
assert.strictEqual(gridColumnsChoice(3), null);
singleActive = 'spatial_projection';

// Rearranging asks the server for nothing: it reorders the panels and loads
// the image of a section shown for the first time.
panels = [{ key: 'A' }, { key: 'B' }, { key: 'C' }];
singleSpaceIds = singleGridSpaces.slice(0, 1);
spaceById[singleGridSpaces[0]]._imgToken = 1;
elements.spatial_projection_sections.selectize.items = ['B1', 'A1'];
calls = [];
applySectionGrid();
assert.deepStrictEqual(singleSpaceIds, [singleGridSpaces[2], singleGridSpaces[0]]);
assert.deepStrictEqual(panels.map(function (p) { return p.spaceId; }),
  [singleGridSpaces[2], singleGridSpaces[0], null]);
assert.deepStrictEqual(calls, ['image:' + singleGridSpaces[2], 'resize', 'drawAll']);
assert.strictEqual(_layoutKey, null, 'the layout is fitted again');
console.log('PASS the browser shows the chosen sections in the chosen order');

// ---- columns ---------------------------------------------------------------------
elements.spatial_projection_grid_columns = { value: 'auto' };
assert.strictEqual(gridColumnsChoice(4), null);
elements.spatial_projection_grid_columns.value = '3';
assert.strictEqual(gridColumnsChoice(4), 3);
assert.strictEqual(gridColumnsChoice(2), 2, 'never more columns than panels');
var fixed = bestOverviewGrid(6, 1200, 800, 18, 40, 8, 160, 3);
assert.strictEqual(fixed.cols, 3);
assert.strictEqual(fixed.rows, 2);
var auto = bestOverviewGrid(6, 1200, 800, 18, 40, 8, 160, null);
assert.ok(auto.cols >= 1 && auto.cols <= 6);
// Every page's panels share one floor, PANEL_MIN_SIDE (250 px).
var narrow = bestOverviewGrid(8, 1200, 700, 18, 40, 8, PANEL_MIN_SIDE, null);
assert.ok(narrow.side >= PANEL_MIN_SIDE, 'eight sections fit above the floor');
// Panels are shaped like their sections, and their rows fill the window.
var rowsHeight = function (pack) {
  return pack.rowSizes.reduce(function (sum, row) { return sum + row.height + 58; }, 0) +
    (pack.rowSizes.length - 1) * 8;
};
var fit = sectionGridPacking(12, 1600, 1000, 18, 58, 8, 1.3, null);
assert.ok(rowsHeight(fit) <= 1000, 'twelve sections fit the window');
assert.ok(fit.rowSizes.every(function (row) {
  return row.height <= Math.round(row.width / 1.3) + 1 && row.height >= PANEL_MIN_SIDE;
}), 'no taller than the section needs, and readable');
assert.strictEqual(fit.rowSizes.reduce(function (n, row) { return n + row.count; }, 0), 12);
// Five sections: every panel one size, so all are drawn at one scale; a
// short last row leaves its slots empty.
var five = sectionGridPacking(5, 1600, 1200, 18, 58, 8, 1.5, null);
assert.ok(five.rowSizes.length > 1 && five.rowSizes.every(function (row) {
  return row.width === five.width && row.height === five.height;
}), 'one panel size: ' + JSON.stringify(five.rowSizes));
assert.ok(five.cols * (five.width + 18) + (five.cols - 1) * 8 >= 1600 - five.cols - 8,
  'the full rows are as wide as the window');
// Linked views: panels of mixed shapes fill the window's height, one size.
var mixed = sectionGridPacking(5, 1400, 1100, 18, 58, 8, [1, 1.6, 1.6, 1.6, 1.2], null, true);
assert.ok(Math.abs(rowsHeight(mixed) - 1100) <= mixed.rows + 1, 'fills the height: ' + JSON.stringify(mixed));
assert.ok(mixed.rowSizes.every(function (row) {
  return row.width === mixed.width && row.height === mixed.height;
}));
// Wide sections take fewer columns than tall ones in the same window.
var wide = sectionGridPacking(6, 2400, 1400, 18, 58, 8, 3, null);
var tall = sectionGridPacking(6, 2400, 1400, 18, 58, 8, 0.4, null);
assert.ok(wide.cols < tall.cols, 'wide ' + wide.cols + ' vs tall ' + tall.cols);
// The columns asked for are kept, every panel one column wide.
var two = sectionGridPacking(12, 1600, 800, 18, 58, 8, 1.3, 2);
assert.strictEqual(two.cols, 2);
assert.ok(two.rowSizes.every(function (row) { return row.width === two.width; }));
// A window too small for every section scrolls rather than shrinking them away.
var crowded = sectionGridPacking(40, 1200, 500, 18, 58, 8, 1, null);
assert.ok(crowded.height >= PANEL_MIN_SIDE && crowded.width >= PANEL_MIN_SIDE, JSON.stringify(crowded));
assert.strictEqual(sectionGridRowOf(five, 0), five.rowSizes[0]);
assert.strictEqual(sectionGridRowOf(five, 4), five.rowSizes[five.rowSizes.length - 1]);
// The sections' shape is the median of their extents.
var keptShown = singleSpaceIds;
singleSpaceIds = ['s1', 's2', 's3'];
spaceById.s1 = { _gridSection: 'a', xRange: [0, 200], yRange: [0, 100] };
spaceById.s2 = { _gridSection: 'b', xRange: [0, 300], yRange: [0, 100] };
spaceById.s3 = { _gridSection: 'c', xRange: [0, 100], yRange: [0, 100] };
assert.strictEqual(sectionGridAspect(), 2);
singleSpaceIds = ['umap'];
spaceById.umap = { id: 'umap' };
assert.strictEqual(sectionGridAspect(), 1, 'no section: square');
singleSpaceIds = keptShown;
delete spaceById.umap;
console.log('PASS the grid takes the columns asked for');

// ---- synced zoom -----------------------------------------------------------------
panels = [
  { key: 'A', spaceId: singleSpaceIds[0], view: { cx: 0.25, cy: 0.75, span: 0.5 } },
  { key: 'B', spaceId: singleSpaceIds[1], view: null },
  { key: 'C', spaceId: singleGridSpaces[1], view: null }   // a hidden section
];
calls = [];
syncGridView(panels[0]);
assert.strictEqual(panels[1].view, null, 'off unless asked for');
elements.spatial_projection_grid_sync_zoom = { checked: true };
syncGridView(panels[0]);
assert.deepStrictEqual(panels[1].view, { cx: 0.25, cy: 0.75, span: 0.5 });
assert.notStrictEqual(panels[1].view, panels[0].view, 'a copy, not the same object');
assert.strictEqual(panels[2].view, null, 'a section not shown keeps its view');
assert.deepStrictEqual(calls, ['project:B', 'draw:B']);
// The embedding neither leads nor follows a section's camera.
singleGridExtras = ['umap'];
spaceById.umap = { id: 'umap' };
var embedding = { key: 'E', spaceId: 'umap', view: { cx: 0.9, cy: 0.9, span: 0.2 } };
panels.push(embedding);
singleSpaceIds = singleSpaceIds.concat(['umap']);
calls = [];
syncGridView(embedding);
assert.deepStrictEqual(calls, [], 'an embedding view is not copied');
syncGridView(panels[0]);
assert.deepStrictEqual(embedding.view, { cx: 0.9, cy: 0.9, span: 0.2 });
panels.pop(); singleSpaceIds.pop(); singleGridExtras = [];
// A reset resets them all; the copy never echoes back to its source.
panels[0].view = null;
syncGridView(panels[0]);
assert.strictEqual(panels[1].view, null);
assert.strictEqual(gridSyncing, false);
console.log('PASS synced zoom gives every shown section the same relative view');
// An enlarged panel reads like a photo viewer: the others are one strip of
// thumbnails as long as the enlarged panel, scrolled between two arrows when
// they do not fit, so many sections never shrink it. The strip goes below or
// to the left, wherever the enlarged content is drawn larger. A very wide
// section in a wide window: below.
var few = focusGridPacking(3, 1310, 726, 18, 58, 8, 4);
assert.strictEqual(few.place, 'below');
assert.strictEqual(few.scroll, false);
assert.strictEqual(few.view, 1310, 'three thumbnails need no arrows');
assert.strictEqual(few.mainW, 1310 - 18, 'the enlarged panel takes the full width');
assert.ok(few.side >= FOCUS_THUMB_MIN && few.side <= FOCUS_THUMB_MAX, JSON.stringify(few));
assert.ok(few.mainH + 58 + few.side + 58 + 8 <= 726, 'both rows fit the window');
var many = focusGridPacking(40, 1310, 726, 18, 58, 8, 4);
assert.strictEqual(many.scroll, true);
assert.strictEqual(many.mainW, few.mainW, 'forty sections leave the enlarged panel as wide');
assert.strictEqual(many.mainH, few.mainH, 'and as tall');
assert.strictEqual(many.view + 2 * (FOCUS_ARROW + 8), 1310, 'the strip and its arrows span the width');
assert.strictEqual(many.content, 40 * many.step - 8);
assert.strictEqual(focusGridPacking(4, 500, 200, 18, 58, 8, 1), null,
  'no room: the page scrolls as before');
// A square plot in the same window: the strip moves to the left, where the
// plot is drawn larger; with its arrows it is exactly as tall as the
// enlarged panel.
var square = focusGridPacking(40, 1310, 726, 18, 58, 8, 1);
assert.strictEqual(square.place, 'left');
assert.strictEqual(square.mainH, 726 - 58, 'the enlarged panel takes the full height');
assert.strictEqual(square.mainW + 18 + 8 + square.ctxW + 18, 1310, 'the strip fits beside it');
assert.ok(Math.min(square.mainW, square.mainH) > Math.min(many.mainW, many.mainH),
  'the square plot is larger than with the strip below');
assert.strictEqual(square.scroll, true);
assert.strictEqual(square.view + 2 * (FOCUS_ARROW + 8), square.mainH + 58,
  'strip and arrows are as tall as the enlarged panel');
assert.strictEqual(square.thumb, square.side + 58);
// In a narrow, tall window a tall section still goes to the left, a wide
// one below.
assert.strictEqual(focusGridPacking(6, 900, 1300, 18, 58, 8, 0.5).place, 'left');
assert.strictEqual(focusGridPacking(6, 900, 1300, 18, 58, 8, 2).place, 'below');
console.log('PASS an enlarged panel is a photo with a scrolling strip below or to the left');

// ---- a streaming grid ----------------------------------------------------------
// The first frame holds section B's rows only; A and C follow as batches.
var streamed = {
  x: Float32Array.from([40, 50]), y: Float32Array.from([4, 5]),
  group_code: Int8Array.from([1, 0]),
  panels: [
    { id: 'projection', projection: true, x: [0.4, 0.5], y: [-4, -5] },
    { section: 'A', start: 0, count: 3 }, { section: 'B', start: 3, count: 2 },
    { section: 'C', start: 5, count: 4 }
  ],
  grid_stream: { rows: 9, section: ['B'], start: [3], count: [2], batches: 3 }
};
gridStreamExpand(streamed);
assert.strictEqual(streamed.x.length, 9, 'the first frame takes the frame length');
assert.ok(streamed.x instanceof Float32Array, 'in the precision sent');
assert.deepStrictEqual(Array.from(streamed.x.slice(3, 5)), [40, 50]);
assert.ok(isNaN(streamed.x[0]) && isNaN(streamed.x[8]), 'cells still to come have no position');
assert.deepStrictEqual(Array.from(streamed.group_code), [-1, -1, -1, 1, 0, -1, -1, -1, -1]);
assert.strictEqual(streamed.panels[0].x.length, 9, 'the embedding too');
assert.deepStrictEqual(Array.from(streamed.panels[0].x.slice(3, 5)), [0.4, 0.5]);
assert.strictEqual(gridStreamPending(streamed, 'A'), true);
assert.strictEqual(gridStreamPending(streamed, 'B'), false);
gridStreamExpand(streamed);
assert.strictEqual(streamed.x[3], 40, 'a frame is padded once');
// Batches of an older frame are dropped, those of another view kept, those
// of this frame written.
var partA = { id: 'spatial_projection', render_token: 5, section: 'A', start: 0, count: 3,
  x: [10, 20, 30], y: [1, 2, 3], group_code: [2, 2, 0], projection: { x: [0.1, 0.2, 0.3], y: [-1, -2, -3] } };
gridRowsWaiting = [partA, { id: 'spatial_projection', render_token: 4 }, { id: 'expression_projection', render_token: 1 }];
drainGridRows('spatial_projection', { render_token: 5, data: streamed });
assert.deepStrictEqual(gridRowsWaiting.map(function (part) { return part.id; }), ['expression_projection']);
assert.deepStrictEqual(Array.from(streamed.x.slice(0, 5)), [10, 20, 30, 40, 50]);
assert.deepStrictEqual(Array.from(streamed.group_code.slice(0, 3)), [2, 2, 0]);
assert.strictEqual(gridStreamPending(streamed, 'A'), false);
assert.strictEqual(gridStreamPending(streamed, 'C'), true);
// The last batch, drawn: the panel of C takes its positions and group codes,
// draws again and no longer waits; the legend counts every cell.
var groupValues = new Array(9).fill(-1), groupC = { values: groupValues, levels: ['a', 'b', 'c'] };
var catOf = function () { return groupC; }, fieldForMode = function () { return null; };
var legendCounts = new Map([[groupValues, 'stale']]);
function renderLegend() { calls.push('legend'); }
function updateMoranBadges(only) { calls.push('moran:' + Object.keys(only).join(',')); }
var sharedX = new Float32Array(9).fill(NaN), sharedY = new Float32Array(9).fill(NaN);
spaceById = { sC: { x: sharedX, y: sharedY, _gridPending: true, _unit: {} } };
panels = [{ key: 'C', spaceId: 'sC' }];
gridLive = { data: streamed, D: D, mode: 'g', spaces: { C: ['sC'] }, projection: null, rows: null };
calls = [];
var partC = { id: 'spatial_projection', render_token: 5, section: 'C', start: 5, count: 4,
  x: [60, 70, 80, 90], y: [6, 7, 8, 9], group_code: Int8Array.from([1, 9, -1, 0]), done: true };
gridStreamWrite(streamed, partC);
// A file read still to be drawn (it waits for an animation frame) keeps the
// stream open even though every batch and file is in.
gridFileParts = [{ id: 'spatial_projection', fromFile: true, section: ['B'] }];
gridStreamPaint(partC);
gridStreamSettle();
assert.notStrictEqual(gridLive, null, 'a file read waits to be drawn');
gridFileParts = [];
gridStreamSettle();
assert.deepStrictEqual(Array.from(sharedX.slice(5)), [60, 70, 80, 90]);
assert.strictEqual(spaceById.sC._gridPending, false);
assert.strictEqual(spaceById.sC._unit, null, 'its extent is measured anew');
assert.deepStrictEqual(groupValues.slice(5), [1, -1, -1, 0], 'a code beyond the levels is no group');
assert.deepStrictEqual(calls, ['project:C', 'draw:C', 'moran:sC', 'legend'],
  "its Moran's I is measured once its cells are in");
assert.strictEqual(legendCounts.has(groupValues), false);
assert.strictEqual(gridLive, null, 'the stream is over');
assert.strictEqual(streamed.grid_stream.done, true);
assert.strictEqual(gridStreamPending(streamed, 'C'), false);
console.log('PASS a streaming grid draws each batch of sections as it comes');

// ---- a streaming grid names its cells batch by batch ----------------------------
eval(between('  function expandRowRange(', '  function frameSubsetRows('));
eval(between('  function unitArraysOf(', '  function deferredUnitOf('));
assert.deepStrictEqual(Array.from(expandRowRange({ kind: 'ranges', start: [10, 40], length: [2, 3] })),
  [10, 11, 40, 41, 42], 'runs of rows');
assert.deepStrictEqual(Array.from(expandRowRange({ kind: 'range', start: 5, length: 2 })), [5, 6]);
assert.deepStrictEqual(expandRowRange([3, 1]), [3, 1], 'rows one by one as they are');
// The first frame names section B's cells (canonical rows 7 and 2); the
// names of A come with its batch, by canonical rows in runs.
var named = {
  x: [1, 2], y: [1, 2],
  selection_key: ['h', 'c'], selection_rows: Int32Array.from([7, 2]),
  panels: [{ section: 'A', start: 0, count: 3 }, { section: 'B', start: 3, count: 2 }],
  grid_stream: { rows: 5, section: ['B'], start: [3], count: [2], batches: 2,
    names: ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] }
};
gridStreamExpand(named);
assert.deepStrictEqual(named.selection_key, ['', '', '', 'h', 'c'], 'a cell still to come has no name');
assert.deepStrictEqual(Array.from(named.selection_rows), [-1, -1, -1, 7, 2]);
gridStreamWrite(named, { section: 'A', start: 0, count: 3, x: [0, 0, 0], y: [0, 0, 0],
  selection_index: { kind: 'ranges', start: [4], length: [3] }, done: true });
assert.deepStrictEqual(named.selection_key, ['e', 'f', 'g', 'h', 'c']);
assert.deepStrictEqual(Array.from(named.selection_rows), [4, 5, 6, 7, 2]);
// Without a cell-name resource the batch carries the names themselves.
var byName = { x: [1], y: [1], selection_key: ['q'],
  panels: [{ section: 'A', start: 0, count: 1 }, { section: 'B', start: 1, count: 2 }],
  grid_stream: { rows: 3, section: ['A'], start: [0], count: [1], batches: 2 } };
gridStreamExpand(byName);
gridStreamWrite(byName, { section: 'B', start: 1, count: 2, x: [2, 3], y: [2, 3],
  selection_key: ['r', 's'], done: true });
assert.deepStrictEqual(byName.selection_key, ['q', 'r', 's']);
// The sections of a grid share one pair of unit position arrays.
var pair = { nx: new Float32Array(5), ny: new Float32Array(5) };
assert.strictEqual(unitArraysOf({ _unitArrays: pair }, 5), pair);
assert.strictEqual(unitArraysOf({ _unitArrays: pair }, 6), null, 'only at the frame length');
assert.strictEqual(unitArraysOf({}, 5), null);
console.log('PASS a streaming grid names its cells batch by batch');

// ---- a panel of some cells keeps its maps over its own cells --------------------
eval(between('  function sortedIndexOf(', '  function updateGpuSelection('));
var own = Int32Array.from([2, 5, 7, 11]);
assert.strictEqual(sortedIndexOf(own, 7), 2);
assert.strictEqual(sortedIndexOf(own, 6), -1);
assert.strictEqual(sortedIndexOf(own, 11), 3);
// The selected cells the panel holds, by slot: looked up when few, walked when many.
var held = [];
forEachHeldCell(new Set([7, 99]), own, function (slot, cell) { held.push([slot, cell]); });
assert.deepStrictEqual(held, [[2, 7]]);
held = [];
var many = new Set(); for (var c = 0; c < 200; c++) many.add(c);
forEachHeldCell(many, own, function (slot, cell) { held.push([slot, cell]); });
assert.deepStrictEqual(held, [[0, 2], [1, 5], [2, 7], [3, 11]]);
held = [];
forEachHeldCell(new Set([3]), null, function (slot, cell) { held.push([slot, cell]); });
assert.deepStrictEqual(held, [[3, 3]], 'a panel of every cell: the slot is the cell');
// Its paint order: its own cells, lowest value first, as in the order of all.
D = { n: 12 };
var allOrder = Uint32Array.from([11, 0, 7, 1, 2, 3, 4, 6, 8, 9, 10, 5]);
var sequence = panelPaintSequence(own, allOrder);
assert.deepStrictEqual(Array.from(sequence).map(function (slot) { return own[slot]; }), [11, 7, 2, 5]);
assert.strictEqual(panelPaintSequence(own, allOrder), sequence, 'kept for the same order');
console.log('PASS a panel of some cells keeps its maps over its own cells');

// ---- Moran's I finds each cell's six nearest through buckets ---------------------
eval(between('  function sixNearest(', '  function spatialMoran('));
// What comparing every pair gives: nearest first, the lower index first at
// one distance.
function sixNearestPairs(px, py) {
  var n = px.length, out = [];
  for (var i = 0; i < n; i++) {
    var nearest = [];
    for (var j = 0; j < n; j++) {
      if (j === i) continue;
      var dx = px[i] - px[j], dy = py[i] - py[j], d = dx * dx + dy * dy;
      var at = nearest.length;
      while (at > 0 && nearest[at - 1].d > d) at--;
      nearest.splice(at, 0, { j: j, d: d });
      if (nearest.length > 6) nearest.pop();
    }
    out.push(nearest);
  }
  return out;
}
var seed = 7;
function random() { seed = (seed * 16807) % 2147483647; return seed / 2147483647; }
[
  // scattered, on a lattice (many equal distances), clumped, and a line
  { n: 1000, at: function () { return [random() * 900, random() * 400]; } },
  { n: 600, at: function () { return [Math.floor(random() * 30), Math.floor(random() * 20)]; } },
  { n: 400, at: function () { var c = random() < 0.5 ? 0 : 1000; return [c + random(), c + random()]; } },
  { n: 50, at: function () { return [Math.floor(random() * 10), 0]; } },
  { n: 7, at: function () { return [1, 1]; } }
].forEach(function (shape) {
  var px = new Float64Array(shape.n), py = new Float64Array(shape.n);
  for (var k = 0; k < shape.n; k++) { var p = shape.at(); px[k] = p[0]; py[k] = p[1]; }
  var fast = sixNearest(px, py), pairs = sixNearestPairs(px, py);
  for (var q = 0; q < shape.n; q++) {
    assert.deepStrictEqual(fast[q].map(function (e) { return e.j; }),
      pairs[q].map(function (e) { return e.j; }), 'point ' + q + ' of ' + shape.n);
  }
});
console.log("PASS Moran's I finds each cell's six nearest through buckets");

// ---- sections whose positions are in Viewer Pack files ---------------------------
// Section A's positions come from a file; B's and C's travel in the batches.
// A waits for both its batch and its file.
var filed = {
  x: [7, 8], y: [7, 8], group_code: [0, 1, 1, 0],
  panels: [{ section: 'A', start: 0, count: 2, geometry_resource: { url: 'a.bin' } },
    { section: 'B', start: 2, count: 2 }, { section: 'C', start: 4, count: 1 }],
  grid_stream: { rows: 5, section: ['A', 'B'], start: [0, 2], count: [2, 2], batches: 2 }
};
gridStreamExpand(filed);
assert.ok(filed.x instanceof Float32Array, 'positions are single precision');
assert.deepStrictEqual(Array.from(filed.x).map(function (v) { return isNaN(v) ? null : v; }),
  [null, null, 7, 8, null], "the batch's positions go to B; A's wait for its file");
assert.deepStrictEqual(Array.from(filed.group_code), [0, 1, 1, 0, -1], 'its colours do not');
assert.strictEqual(gridStreamFromFile(filed.grid_stream, 'A'), true);
assert.strictEqual(gridStreamPending(filed, 'A'), true, 'its batch is in, its file is not');
assert.strictEqual(gridStreamPending(filed, 'B'), false);
gridStreamWrite(filed, { fromFile: true, section: ['A'], start: [0], count: [2],
  x: Float32Array.from([1, 2]), y: Float32Array.from([3, 4]) });
assert.deepStrictEqual(Array.from(filed.x.slice(0, 4)), [1, 2, 7, 8]);
assert.strictEqual(gridStreamPending(filed, 'A'), false);
assert.strictEqual(gridStreamComplete(filed.grid_stream), false, 'C is still to come');
gridStreamWrite(filed, { section: ['C'], start: [4], count: [1], x: [9], y: [9], group_code: [0], done: true });
assert.strictEqual(filed.x[4], 9);
assert.strictEqual(gridStreamComplete(filed.grid_stream), true);
console.log('PASS sections read from files wait for their file and their batch');

// ---- the files are fetched a few at a time, in panel order -----------------------
// All at once they would hold back the batches that bring the colours.
(function () {
  var requested = [], waiting = [], inFlight = 0, most = 0;
  var sections = ['A', 'B', 'C', 'D', 'E'];
  var many = {
    x: [], y: [],
    panels: sections.map(function (s, i) {
      return { section: s, start: i, count: 1, geometry_resource: { url: s + '.bin' } };
    }),
    grid_stream: { rows: 5, section: [], start: [], count: [], batches: 2 }
  };
  gridStreamExpand(many);
  var view = { data: many, dataset_context: 'ctx' };
  singleView = function () { return view; };
  requestAnimationFrame = function () { return 1; };
  fetchValidatedSingleGeometryResource = function (resource) {
    requested.push(resource.url);
    inFlight++; most = Math.max(most, inFlight);
    return new Promise(function (resolve) {
      waiting.push(function () {
        inFlight--;
        resolve({ coordinates: { x: Float32Array.of(1), y: Float32Array.of(2) } });
      });
    });
  };
  gridStreamFetchFiles('spatial_projection', view, {});
  assert.deepStrictEqual(requested, ['A.bin', 'B.bin', 'C.bin'], 'three at first, in panel order');
  function settle() {
    if (!waiting.length) {
      assert.deepStrictEqual(requested, ['A.bin', 'B.bin', 'C.bin', 'D.bin', 'E.bin']);
      assert.strictEqual(most, 3, 'never more than three at once');
      sections.forEach(function (s) { assert.ok(many.grid_stream.filesLoaded[s], s + ' is in'); });
      console.log('PASS the grid fetches its files a few at a time, in panel order');
      return;
    }
    waiting.shift()();
    setTimeout(settle, 0);
  }
  settle();
})();
