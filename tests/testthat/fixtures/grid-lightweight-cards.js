// Lightweight multi-panel grid cards: buildPanels() allocates neither
// interaction canvases nor minimap contexts up front; drawHoverOverlay()
// attaches interactionCanvas lazily on hover/drag; attachGpu() uses the single
// shared WebGL2 context on multi-panel grids even when WebGPU exists and skips
// redundant microtask redraws for synchronously ready renderers.
const fs = require('fs'), assert = require('assert/strict'), vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(from, to) {
  const start = source.indexOf(from), end = source.indexOf(to, start);
  assert.ok(start >= 0 && end > start, 'missing ' + from);
  return source.slice(start, end);
}

// 1. buildPanels() allocates neither interaction layer nor minimap context eagerly.
const bp = between('  function buildPanels()', '  function canFocusPanel()');
assert.equal(bp.includes('attachInteractionLayer('), false, 'no eager interaction layer in buildPanels');
assert.equal(bp.includes('mini.getContext('), false, 'no eager minimap context in buildPanels');

// 2. attachGpu() selects createSharedWebGl on >=8-panel grids even with navigator.gpu,
//    and create() on single/small/focused views; watchGpu() skips ready.then() when isReady().
let sharedCalls = 0, dedicatedCalls = 0, readyDraws = 0;
const syncRenderer = kind => ({
  kind,
  ready: Promise.resolve(),
  failed: new Promise(() => {}),
  isReady: () => true,
  resize() {}
});
const asyncRenderer = kind => ({
  kind,
  ready: Promise.resolve(),
  failed: new Promise(() => {}),
  isReady: () => false,
  resize() {}
});
const ctx = {
  gpuAcceleration: true,
  focusPanel: null,
  stagePanels: 12,
  OFFSTAGE_MIN_PANELS: 8,
  D: {n: 10},
  navigator: {gpu: {}},
  window: {
    devicePixelRatio: 1,
    CerebroPointRenderer: {
      createSharedWebGl() { sharedCalls++; return syncRenderer('shared'); },
      create() { dedicatedCalls++; return asyncRenderer('webgpu'); }
    }
  },
  createLayerCanvas: () => ({remove() {}}),
  failOver() {},
  draw() { readyDraws++; },
  drawPanel() {}
};
vm.createContext(ctx);
vm.runInContext(between('  function watchGpu(p, renderer)', '  function setGpuAcceleration('), ctx);

const gridPanel = {spaceId: 's1', W: 200, H: 200, gpu: null, gpuCanvas: null, _gpuDisabled: false};
ctx.attachGpu(gridPanel);
assert.equal(gridPanel.gpu.kind, 'shared', 'grid uses shared WebGL2 even when navigator.gpu exists');
assert.equal(sharedCalls, 1);
assert.equal(dedicatedCalls, 0);

// Enlarged Focus panel keeps dedicated WebGPU.
ctx.focusPanel = 'A';
const focusPanelObj = {spaceId: 's2', W: 800, H: 600, gpu: null, gpuCanvas: null, _gpuDisabled: false};
ctx.attachGpu(focusPanelObj);
assert.equal(focusPanelObj.gpu.kind, 'webgpu', 'focused panel uses dedicated WebGPU');
assert.equal(dedicatedCalls, 1);

// Small view (< 8 panels) keeps dedicated WebGPU.
ctx.focusPanel = null;
ctx.stagePanels = 2;
const smallPanel = {spaceId: 's3', W: 500, H: 500, gpu: null, gpuCanvas: null, _gpuDisabled: false};
ctx.attachGpu(smallPanel);
assert.equal(smallPanel.gpu.kind, 'webgpu', 'small view uses dedicated WebGPU');

// 3. Lazy interaction layer in drawHoverOverlay(): no-op when nothing hovered/dragged,
//    attaches once on actual hover.
let attachedInteraction = 0;
const hoverCtx = {
  hoverCell: null, pick: null, singleActive: 'spatial_projection', linkedHoverOn: false, hoverPanel: null,
  attachInteractionLayer(p) {
    attachedInteraction++;
    p.interactionCtx = { clearRect() {}, beginPath() {}, arc() {}, stroke() {} };
  },
  strokeOutline() {}, pointScreenX: () => 10, pointScreenY: () => 10
};
vm.createContext(hoverCtx);
vm.runInContext(between('  function drawHoverOverlay(p)', '  function drawHoverAll()'), hoverCtx);
const hp = {W: 200, H: 200, ok: [1], _renderPointSize: 2, drag: false, moved: false, interactionCtx: null};
hoverCtx.drawHoverOverlay(hp);
assert.equal(attachedInteraction, 0, 'idle panel allocates no interaction layer');
hoverCtx.hoverCell = 0;
hoverCtx.drawHoverOverlay(hp);
assert.equal(attachedInteraction, 1, 'hovered panel attaches interaction layer lazily');

// Check microtask queue: sync shared renderer triggered 0 duplicate readyDraws,
// while the 2 async WebGPU renderers each trigger 1 readyDraw.
setImmediate(() => {
  assert.equal(readyDraws, 2, 'synchronously ready shared renderer skips duplicate microtask draw');
  console.log('PASS lightweight grid cards and shared GPU');
});
