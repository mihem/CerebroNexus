// WebGL2 without WebGPU. Part 1 runs the real per-context renderer on a fake gl
// (release() gives the context back and is not a failure); part 2 runs the
// failover code of cell_views.js: a renderer that failed is replaced by a
// shared WebGL2 one, a few times, then the panel drops to Canvas.
const fs = require('fs'), assert = require('assert/strict'), vm = require('vm');
const gpuSource = fs.readFileSync(process.argv[2], 'utf8');
const viewSource = fs.readFileSync(process.argv[3], 'utf8');

// ---- part 1: the renderer --------------------------------------------------
{
  let lost = 0;
  const gl = {
    createShader: () => ({}), shaderSource() {}, compileShader() {},
    getShaderParameter: () => true, deleteShader() {},
    createProgram: () => ({}), attachShader() {}, linkProgram() {},
    getProgramParameter: () => true, getUniformLocation: () => ({}),
    createBuffer: () => ({}), useProgram() {}, bindBuffer() {}, bufferData() {},
    bufferSubData() {}, enableVertexAttribArray() {}, vertexAttribIPointer() {},
    vertexAttribPointer() {}, viewport() {}, getParameter: () => 'test adapter',
    getExtension: name => name === 'WEBGL_lose_context' ? {loseContext() { lost++; }} : null
  };
  const listeners = {};
  const canvas = {
    style: {}, clientWidth: 100, clientHeight: 100,
    addEventListener(type, fn) { listeners[type] = fn; }, getContext: () => gl
  };
  const scope = {window: {}, navigator: {}, performance};
  vm.runInNewContext(gpuSource, scope);
  const renderer = scope.window.CerebroPointRenderer.createWebGl(canvas);
  assert.equal(renderer.isReady(), true);
  let failed = false;
  renderer.failed.then(() => { failed = true; });
  renderer.release();
  renderer.release();
  assert.equal(lost, 1, 'the context is given back once');
  assert.equal(renderer.isReady(), false);
  // The browser reports the loss of a context we gave up: not a failure.
  listeners.webglcontextlost({preventDefault() {}});
  setImmediate(() => {
    assert.equal(failed, false, 'a released context is not a failure');
    // A context the browser takes is one.
    const other = scope.window.CerebroPointRenderer.createWebGl(canvas);
    let otherFailed = false;
    other.failed.then(() => { otherFailed = true; });
    listeners.webglcontextlost({preventDefault() {}});
    setImmediate(() => { assert.equal(otherFailed, true); part2(); });
  });
}

// ---- part 2: a failed renderer is replaced -------------------------------------
function part2() {
  const start = viewSource.indexOf('  // `again`: the renderer is a WebGL2 one');
  const end = viewSource.indexOf('  function watchGpu(p, renderer)');
  assert.ok(start > 0 && end > start, 'failover code found');
  let made = 0;
  const makeRenderer = shared => ({
    id: ++made, stats: () => ({backend: 'webgl2', shared}), resize() {}
  });
  const newPanel = gpu => ({
    W: 300, H: 250, gpu, disabled: false,
    gpuCanvas: {parentNode: {replaceChild() {}}, _cerebroPointRenderer: null}
  });
  const build = api => {
    const context = {
      window: {CerebroPointRenderer: api, devicePixelRatio: 1}, console,
      createLayerCanvas: () => ({parentNode: {replaceChild() {}}, _cerebroPointRenderer: null}),
      watchGpu() {}, disableGpu(p) { p.gpu = null; p.disabled = true; }
    };
    vm.createContext(context);
    vm.runInContext(viewSource.slice(start, end), context);
    return vm.runInContext('failOver', context);
  };
  const api = {createSharedWebGl: () => makeRenderer(true), createWebGl: () => makeRenderer(false)};
  const failOver = build(api);

  // WebGPU fails: the panel takes a shared WebGL2 renderer.
  let p = newPanel({stats: () => ({backend: 'webgpu'})});
  assert.equal(failOver(p), true);
  assert.deepEqual(p.gpu.stats(), {backend: 'webgl2', shared: true});
  assert.equal(p.disabled, false);

  // The shared context is lost: each panel takes a renderer on the new one, a
  // few times, and then drops to Canvas.
  p = newPanel(makeRenderer(true));
  for (let loss = 1; loss <= 3; loss++) {
    const before = p.gpu;
    assert.equal(failOver(p), true, 'loss ' + loss + ' is recovered');
    assert.notEqual(p.gpu, before, 'a new renderer after loss ' + loss);
  }
  assert.equal(failOver(p), false, 'the fourth loss drops the panel to Canvas');
  assert.equal(p.disabled, true);

  // A per-context WebGL2 renderer that fails, with nothing to take its place
  // but itself, drops to Canvas as before.
  p = newPanel(makeRenderer(false));
  assert.equal(failOver(p), false);
  assert.equal(p.disabled, true);

  // No WebGL2 renderer to make: Canvas.
  const bare = build({});
  p = newPanel({stats: () => ({backend: 'webgpu'})});
  assert.equal(bare(p), false);
  assert.equal(p.disabled, true);

  // WebGL2 cannot be made for the panel: Canvas.
  const broken = build({createSharedWebGl() { throw new Error('no context'); }});
  p = newPanel({stats: () => ({backend: 'webgpu'})});
  assert.equal(broken(p), false);
  assert.equal(p.disabled, true);
  console.log('PASS the renderer releases its context without failing; a panel whose renderer failed takes a shared WebGL2 one, a few times, then Canvas');
}
