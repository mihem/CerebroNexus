// Multi-panel grid drawing slices work across animation frames within a 10 ms
// budget, prioritises visible-in-viewport panels before near-margin panels, and
// cancels stale queued slices when a newer draw starts.
const fs = require('fs'), assert = require('assert/strict');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(from, to) {
  const start = source.indexOf(from), end = source.indexOf(to, start);
  assert.ok(start >= 0 && end > start, 'missing ' + from);
  return source.slice(start, end);
}

var focusPanel = null, drawAllDeferred = false, awaitingSupplement = null, D = {n: 10};
var panels = [];
var nowMs = 0, rafQueue = [], rafId = 0, cancelled = [];
var drawn = [], resets = 0, shownCounts = [];
function offStrip() { return false; }
function onStageScroll() {}
function resizePanel(p, w, h) { p.W = w; p.H = h; p._pendingSize = null; }
function shownState(p) { return {mask: null, count: 10}; }
function renderShownCount(n) { shownCounts.push(n); }
function updateResetButtons() { resets++; }
function draw(p) {
  drawn.push(p.key);
  // Each panel costs 6 ms of simulated work: 2 panels exceed the 10 ms budget.
  nowMs += 6;
}

var performance = { now: () => nowMs };
var window = {
  innerHeight: 1000,
  requestAnimationFrame(fn) { const id = ++rafId; rafQueue.push({id, fn}); return id; },
  cancelAnimationFrame(id) { cancelled.push(id); rafQueue = rafQueue.filter(e => e.id !== id); }
};

eval(between('  var OFFSTAGE_MIN_PANELS', '  function resizePanel('));
eval(between('  function drawAll()', '  // One panel whose own view changed'));

const pane = top => ({getBoundingClientRect: () => ({top, bottom: top + 200, width: 300})});
// Mix buffer panels (top -220 [bottom -20] and +1100) with in-viewport panels (top 400 and 100).
const specs = [
  ['bufAbove', -220],
  ['bufBelow', 1100],
  ['visMid', 400],
  ['visTop', 100],
  ['visBot', 750],
  ['farOff', 2000],
  ['bufBelow2', 1180],
  ['visLow', 600]
];
panels = specs.map(([key, top]) => ({key, spaceId: 's_' + key, pane: pane(top)}));
stagePanels = panels.length; // >= 8 enables time-slicing

drawAll();
// First slice runs immediately and yields after >= 10 ms (2 panels * 6 ms = 12 ms),
// drawing the top in-viewport panels first before any near-margin buffer panel.
assert.deepEqual(drawn, ['visTop', 'visMid'], 'in-viewport top panels draw first in slice 1');
assert.equal(rafQueue.length, 1, 'remaining panels scheduled on next rAF');
assert.equal(shownCounts.length, 1, 'shown count updated on first panel');
assert.equal(panels[5]._resizedUndrawn, true, 'far off-stage panel stays deferred');

// Drain second slice: remaining in-viewport panels finish before buffer panels.
rafQueue.shift().fn();
assert.deepEqual(drawn, ['visTop', 'visMid', 'visLow', 'visBot']);
assert.equal(rafQueue.length, 1);

// A newer drawAll() cancels the pending slice and starts fresh.
const pendingId = rafQueue[0].id;
drawn = [];
drawAll();
assert.ok(cancelled.includes(pendingId), 'superseded slice is cancelled');
assert.deepEqual(drawn, ['visTop', 'visMid']);

// Drain all remaining slices to completion.
while (rafQueue.length) rafQueue.shift().fn();
assert.deepEqual(
  drawn,
  ['visTop', 'visMid', 'visLow', 'visBot', 'bufAbove', 'bufBelow', 'bufBelow2'],
  'all 7 on-stage panels finish in viewport-first order, skipping farOff'
);
assert.equal(resets, 1, 'updateResetButtons called once when queue drains');

// Small grids (< 8 panels) still draw synchronously in one turn.
stagePanels = 3;
panels = panels.slice(0, 3);
drawn = [];
drawAll();
assert.equal(drawn.length, 3, 'small view draws synchronously');
assert.equal(rafQueue.length, 0, 'no rAF slice needed for < 8 panels');

console.log('PASS grid time-sliced draw queue');
