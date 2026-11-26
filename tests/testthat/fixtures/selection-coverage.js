// The chips of how many selected cells each lens maps are counted a slice of
// the main thread at a time (renderSelectionCoverage()), and a panel whose GPU
// renderer was given up gets its screen positions before Canvas draws it
// (ensureScreenProjection()).
const fs = require('fs'), assert = require('assert/strict');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(from, to) {
  const start = source.indexOf(from), end = source.indexOf(to, start);
  assert.ok(start >= 0 && end > start, 'missing ' + from);
  return source.slice(start, end);
}
const chips = [];
const coverage = {hidden: true, set textContent(v) { if (v === '') chips.length = 0; }, appendChild(c) { chips.push(c); }};
let now = 0, sel = new Set(), timers = [];
const document = {createElement: () => ({setAttribute() {}})};
const performance = {now: () => (now += 20)};
function setTimeout(fn) { timers.push(fn); return timers.length; }
function clearTimeout(id) { if (id) timers[id - 1] = null; }
const $ = id => id === 'cv-selcoverage' ? coverage : null;
const fmt = n => String(n);
const compactSpaceLabel = (space, fallback) => fallback;
const spaceById = {};
let panels = [];
eval(between('  // How many of the selected cells each lens maps', '  function renderSelbar()'));

// Four panels over cells 0..9; each maps the cells below its limit.
function makePanel(id, limit, base) {
  spaceById[id] = {label: id, _baseSpaceId: base};
  const ok = new Uint8Array(10); for (let i = 0; i < limit; i++) ok[i] = 1;
  return {spaceId: id, ok};
}
panels = [makePanel('a', 10), makePanel('b', 6), makePanel('c', 3), makePanel('d', 8)];
panels.push({spaceId: null, ok: null});
sel = new Set([0, 1, 2, 4, 7, 9]);
// The first slice is counted at once; with the clock past 12 ms after every
// chip, the rest wait for a timer, one chip a turn.
const tick = () => {};
const wrapped = panels.map(p => p);
renderSelectionCoverage('a');
assert.equal(chips.length, 1, 'the first chip is counted at once');
assert.equal(chips[0].textContent, 'b 4');
assert.equal(coverage.hidden, false);
while (timers.some(Boolean)) {
  const fn = timers.find(Boolean); timers[timers.indexOf(fn)] = null; tick(); fn();
  if (chips.length > 3) break;
}
assert.deepEqual(chips.map(c => c.textContent), ['b 4', 'c 3', 'd 5'], 'the source lens is left out, the others counted');
assert.equal(chips[1].title, 'c: 3 of 6 selected cells mapped');
// A new selection stops the counting of the old one.
timers = []; now = 0; chips.length = 0;
renderSelectionCoverage('a');
const first = chips.length;
sel = new Set([1]);
while (timers.some(Boolean)) { const fn = timers.find(Boolean); timers[timers.indexOf(fn)] = null; fn(); }
assert.equal(chips.length, first, 'nothing more is added for a selection that has gone');
// A panel with no screen positions gets them before Canvas draws it.
const projected = [];
function project(p, cpu) { projected.push([p.key, cpu]); }
eval(between('  function ensureScreenProjection(p) {', '  // The panel\'s data rectangle on screen'));
ensureScreenProjection({key: 'x', gpuTransformOnly: false, sx: null});
ensureScreenProjection({key: 'y', gpuTransformOnly: true, sx: null});
ensureScreenProjection({key: 'z', gpuTransformOnly: false, sx: new Float32Array(3)});
assert.deepEqual(projected, [['x', true], ['y', true]]);
console.log('PASS the coverage chips are counted in turns and a released panel is projected before Canvas draws it');
