// A panel of a grid on WebGL2 that has no context yet and is out of the window
// waits for one (gpuCandidate()): it is not projected for Canvas, which made
// two arrays of every cell for each of 44 panels on the Allen atlas. One in the
// window, or one that cannot have WebGL2, is not a candidate and is drawn on Canvas.
const fs = require('fs'), assert = require('assert/strict');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(from, to) {
  const start = source.indexOf(from), end = source.indexOf(to, start);
  assert.ok(start >= 0 && end > start, 'missing ' + from);
  return source.slice(start, end);
}
var gpuAcceleration = true, GPU_MIN_CELLS = 100, D = {n: 1000};
var spaceById = {s: {_unit: {nz: false}}};
function windowDistance(p) { return p.distance; }
eval(between('  function waitsForGpu(', '  function hideGpu('));
const panel = extra => Object.assign({spaceId: 's', gpu: null, gpuPool: true, distance: 600}, extra);
assert.equal(gpuCandidate(panel()), true, 'out of the window, no context yet: waits');
assert.equal(gpuEligible(panel()), false, 'but is not drawn by the GPU, and nothing throws');
assert.equal(gpuCandidate(panel({distance: 0})), false, 'in the window: drawn on Canvas if it has no context');
assert.equal(gpuCandidate(panel({gpuPool: false})), false, 'not a WebGL2 panel without a renderer: Canvas');
const ready = {isReady: () => true}, notReady = {isReady: () => false};
assert.equal(gpuCandidate(panel({gpu: ready, distance: 0})), true, 'a panel with a renderer');
assert.equal(gpuEligible(panel({gpu: ready, distance: 0})), true);
assert.equal(gpuEligible(panel({gpu: notReady, distance: 0})), false, 'a renderer that is not ready yet');
gpuAcceleration = false;
assert.equal(gpuCandidate(panel()), false, 'GPU acceleration off');
gpuAcceleration = true; D = {n: 50};
assert.equal(gpuCandidate(panel()), false, 'a small data set stays on Canvas');
D = {n: 1000}; spaceById.s._unit.nz = true;
assert.equal(gpuCandidate(panel()), false, 'a unit the GPU cannot draw');
console.log('PASS a panel out of the window waits for its WebGL2 context instead of drawing on Canvas');
