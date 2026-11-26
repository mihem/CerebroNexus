// A browser without a GPU shows 2 panels at a time until the reader lifts the
// limit. Part 1 runs cell_points_gpu.js's check on fake browsers (real GPU,
// software renderer, no WebGL, fallback adapter); part 2 runs the panel limit
// code of cell_views.js on fake selections.
const fs = require('fs'), assert = require('assert/strict'), vm = require('vm');
const gpuSource = fs.readFileSync(process.argv[2], 'utf8');
const viewSource = fs.readFileSync(process.argv[3], 'utf8');

function browser({adapter, renderer, search = '', stored = null}) {
  const classes = new Set(), listeners = {}, inputs = [], storage = {};
  if (stored != null) storage.cerebro_panel_cap_unlocked = stored;
  const gl = renderer == null ? null : {
    getExtension: name => name === 'WEBGL_debug_renderer_info'
      ? {UNMASKED_RENDERER_WEBGL: 1} : (name === 'WEBGL_lose_context' ? {loseContext() {}} : null),
    getParameter: () => renderer
  };
  const document = {
    documentElement: {classList: {
      toggle: (name, on) => { if (on) classes.add(name); else classes.delete(name); }
    }},
    getElementById: () => null,
    createElement: () => ({getContext: () => gl}),
    addEventListener: (type, fn) => { listeners[type] = fn; }
  };
  const navigator = adapter === undefined ? {} : {gpu: {
    requestAdapter: () => Promise.resolve(adapter),
    getPreferredCanvasFormat: () => 'bgra8unorm'
  }};
  const window = {
    document, location: {search}, setTimeout,
    localStorage: {getItem: k => storage[k] ?? null, setItem: (k, v) => { storage[k] = v; }},
    Shiny: {setInputValue: (id, value) => inputs.push([id, value])}
  };
  vm.runInNewContext(gpuSource, {window, navigator, performance, setTimeout, Promise});
  return {panel: window.CerebroPanelCap, classes, inputs, storage, listeners};
}

async function part1() {
  // A GPU behind WebGPU.
  let b = browser({adapter: {isFallbackAdapter: false}, renderer: null});
  assert.equal(b.panel.cap(), 0, 'nothing is limited before the check');
  await b.panel.ready;
  assert.equal(b.panel.cap(), 0);
  assert.equal(b.panel.limited(), false);
  // No WebGPU, a GPU behind WebGL2.
  b = browser({renderer: 'ANGLE (NVIDIA, NVIDIA GeForce RTX 3060, D3D11)'});
  await b.panel.ready;
  assert.equal(b.panel.cap(), 0);
  // A software "fallback adapter" and no WebGL2.
  b = browser({adapter: {isFallbackAdapter: true}, renderer: null});
  await b.panel.ready;
  assert.equal(b.panel.cap(), 2);
  assert.ok(b.classes.has('cerebro-no-gpu') && b.classes.has('cerebro-panel-cap'));
  assert.deepEqual(b.inputs.pop(), ['cerebro_panel_cap', 2], 'the server is told');
  // A fallback adapter but a GPU behind WebGL2.
  b = browser({adapter: {isFallbackAdapter: true}, renderer: 'AMD Radeon'});
  await b.panel.ready;
  assert.equal(b.panel.cap(), 0);
  // WebGL2 that is software rendering, handed out by a browser that allows it.
  for (const software of [
    'ANGLE (Google, Vulkan 1.3.0 (SwiftShader Device (Subzero)), SwiftShader driver)',
    'llvmpipe (LLVM 15.0.7, 256 bits)', 'Microsoft Basic Render Driver'
  ]) {
    b = browser({adapter: null, renderer: software});
    await b.panel.ready;
    assert.equal(b.panel.cap(), 2, software);
  }
  // Neither WebGPU nor WebGL2: Canvas only.
  b = browser({renderer: null});
  await b.panel.ready;
  assert.equal(b.panel.cap(), 2);
  // ?gpu_cap=off skips the check.
  b = browser({renderer: null, search: '?gpu_cap=off'});
  await b.panel.ready;
  assert.equal(b.panel.cap(), 0);
  // The reader lifts the limit and puts it back; the page hears each change.
  b = browser({renderer: null});
  const heard = [];
  b.panel.onChange(cap => heard.push(cap));
  await b.panel.ready;
  b.panel.setUnlocked(true);
  assert.equal(b.panel.cap(), 0);
  assert.equal(b.storage.cerebro_panel_cap_unlocked, '1', 'kept in the browser');
  assert.ok(b.classes.has('cerebro-no-gpu') && !b.classes.has('cerebro-panel-cap'));
  assert.deepEqual(b.inputs.pop(), ['cerebro_panel_cap', 0]);
  b.panel.setUnlocked(false);
  assert.equal(b.panel.cap(), 2);
  assert.deepEqual(heard, [2, 0, 2]);
  // A reader who lifted it before keeps it lifted.
  b = browser({renderer: null, stored: '1'});
  await b.panel.ready;
  assert.equal(b.panel.cap(), 0);
  // Without a page (the renderer alone) nothing is limited.
  const bare = {};
  vm.runInNewContext(gpuSource, {window: bare, navigator: {}, performance});
  await bare.CerebroPanelCap.ready;
  assert.equal(bare.CerebroPanelCap.cap(), 0);
}

function part2() {
  const start = viewSource.indexOf('  // How many more panels a picker may take under the panel limit');
  const end = viewSource.indexOf('  // The limit came or went (cell_points_gpu.js)');
  assert.ok(start > 0 && end > start, 'limit code found');
  let cap = 2, selectedProjections, selectedSpatial;
  const panelCap = () => cap;
  const trim = new Function('panelCap', 'state', `
    var selectedProjections = state.p, selectedSpatial = state.s;
    ${viewSource.slice(start, end)}
    return {capRoom: capRoom, trim: function () {
      var cut = trimSelectionToCap(); return [cut, selectedProjections, selectedSpatial]; }};
  `);
  const state = {p: ['umap'], s: ['A', 'B', 'C']};
  let api = trim(panelCap, state);
  // The default Linked views selection: an embedding and every section.
  assert.deepEqual(api.trim(), [true, ['umap'], ['A']]);
  // Beside one embedding and one section each field has room for one: its own.
  assert.equal(api.capRoom('proj'), 1);
  assert.equal(api.capRoom('spatial'), 1);
  // Beside two sections an embedding has no room.
  state.p = []; state.s = ['A', 'B'];
  api = trim(panelCap, state);
  assert.equal(api.capRoom('proj'), 0);
  // Two embeddings leave no room for a section.
  state.p = ['umap', 'tsne', 'pca']; state.s = ['A'];
  api = trim(panelCap, state);
  assert.deepEqual(api.trim(), [true, ['umap', 'tsne'], []]);
  // No limit: nothing is cut.
  cap = 0;
  state.p = ['umap', 'tsne']; state.s = ['A', 'B', 'C'];
  api = trim(panelCap, state);
  assert.deepEqual(api.trim(), [false, ['umap', 'tsne'], ['A', 'B', 'C']]);
  assert.equal(api.capRoom('spatial'), Infinity);
}

part1().then(part2).then(() => {
  console.log('PASS the check tells a GPU from software, the limit can be lifted, a selection is cut to it');
}).catch(error => { console.error(error); process.exit(1); });
