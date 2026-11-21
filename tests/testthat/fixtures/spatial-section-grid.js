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
var gridSyncing = false, _layoutKey = 'stale';
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
var MIN_SIDE = 300, BALANCED_SIZE_TOLERANCE = 0.85;

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
// The grid's lower floor admits layouts the linked lenses' 300px would not.
var narrow = bestOverviewGrid(8, 1200, 500, 18, 40, 8, 160, null);
assert.ok(narrow.side >= 160, 'eight sections fit above the grid floor');
// Panels are shaped like their sections, and their rows fill the window.
var rowsHeight = function (pack) {
  return pack.rowSizes.reduce(function (sum, row) { return sum + row.height + 58; }, 0) +
    (pack.rowSizes.length - 1) * 8;
};
var fit = sectionGridPacking(12, 1600, 800, 18, 58, 8, 1.3, null);
assert.ok(rowsHeight(fit) <= 800, 'twelve sections fit the window');
assert.ok(fit.rowSizes.every(function (row) {
  return row.height <= Math.round(row.width / 1.3) + 1 && row.height >= 120;
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
var wide = sectionGridPacking(6, 1600, 900, 18, 58, 8, 3, null);
var tall = sectionGridPacking(6, 1600, 900, 18, 58, 8, 0.4, null);
assert.ok(wide.cols < tall.cols, 'wide ' + wide.cols + ' vs tall ' + tall.cols);
// The columns asked for are kept, every panel one column wide.
var two = sectionGridPacking(12, 1600, 800, 18, 58, 8, 1.3, 2);
assert.strictEqual(two.cols, 2);
assert.ok(two.rowSizes.every(function (row) { return row.width === two.width; }));
// A window too small for every section scrolls rather than shrinking them away.
var crowded = sectionGridPacking(40, 1200, 500, 18, 58, 8, 1, null);
assert.ok(crowded.height >= 120 && crowded.width >= 120, JSON.stringify(crowded));
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
// An enlarged panel reads like a photo viewer: it spans the width on top and
// the others are one strip of thumbnails below, paged when they do not fit,
// so many sections never shrink it.
var few = focusGridPacking(3, 1310, 726, 18, 58, 8);
assert.strictEqual(few.place, 'photo');
assert.strictEqual(few.paged, false);
assert.strictEqual(few.perPage, 3);
assert.strictEqual(few.mainW, 1310 - 18, 'the enlarged panel takes the full width');
assert.ok(few.side >= FOCUS_THUMB_MIN && few.side <= FOCUS_THUMB_MAX, JSON.stringify(few));
assert.ok(few.mainH + 58 + few.side + 58 + 8 <= 726, 'both rows fit the window');
var many = focusGridPacking(40, 1310, 726, 18, 58, 8);
assert.strictEqual(many.paged, true);
assert.strictEqual(many.mainW, few.mainW, 'forty sections leave the enlarged panel as wide');
assert.strictEqual(many.mainH, few.mainH, 'and as tall');
assert.ok(many.perPage * (many.ctxW + 18) + (many.perPage - 1) * 8 + 2 * (FOCUS_ARROW + 8) <= 1310,
  'a page of thumbnails and its arrows fit the width');
assert.strictEqual(many.pages, Math.ceil(40 / many.perPage));
assert.strictEqual(focusGridPacking(4, 500, 200, 18, 58, 8), null,
  'no room: the page scrolls as before');
console.log('PASS an enlarged panel is a photo above a paged strip of the others');
