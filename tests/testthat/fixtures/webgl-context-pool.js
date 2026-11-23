// WebGL2 without WebGPU: a grid of more than 16 panels never holds more than
// the page's contexts. Part 1 runs the real renderer on a fake gl (release()
// gives the context back and is not a failure); part 2 runs the panel pool
// code of cell_views.js against 59 fake panels.
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

// ---- part 2: the panels' contexts -------------------------------------------
function part2() {
  const start = viewSource.indexOf('  // ---- WebGL2 contexts of the panels of a grid');
  const end = viewSource.indexOf('  function watchGpu(p, renderer)');
  assert.ok(start > 0 && end > start, 'pool code found');
  let active = 0, peak = 0, created = 0;
  const makeRenderer = () => {
    active++; created++; peak = Math.max(peak, active);
    let live = true;
    return {
      stats: () => ({backend: 'webgl2'}),
      resize() {}, isReady: () => live,
      release() { if (live) { live = false; active--; } }
    };
  };
  const panels = Array.from({length: 59}, (_, i) => ({
    key: 'p' + i, gpuPool: true, gpu: null, W: 300, H: 250, gpuCanvas: {parentNode: {replaceChild(next) {}}, _cerebroPointRenderer: null},
    pane: {rect: {top: i * 300, bottom: i * 300 + 290, width: 300}, getBoundingClientRect() { return this.rect; }}
  }));
  const context = {
    panels, D: {n: 3938808}, GPU_MIN_CELLS: 1, gpuAcceleration: true,
    window: {innerHeight: 1000, CerebroPointRenderer: {createWebGl: () => makeRenderer()}},
    document: {}, console,
    createLayerCanvas: () => ({parentNode: null, _cerebroPointRenderer: null}),
    watchGpu() {}, offStrip: () => false, offStage: () => false, webglPool: true
  };
  // The pool code and the two helpers it calls, as one script.
  vm.createContext(context);
  vm.runInContext(viewSource.slice(start, end), context);
  const scroll = top => panels.forEach((p, i) => {
    p.pane.rect = {top: i * 300 - top, bottom: i * 300 + 290 - top, width: 300};
  });
  const inView = () => panels.filter(p => p.pane.rect.bottom > 0 && p.pane.rect.top < 1000);
  const wake = list => list.forEach(p => vm.runInContext('wakeGpu', context)(p));

  // Open: the panels in the window take contexts, in order.
  wake(inView());
  assert.equal(inView().filter(p => p.gpu).length, inView().length);
  assert.ok(peak <= 15, 'contexts never pass the limit: ' + peak);
  // Scroll to the end in steps: panels coming into the window get contexts, the
  // farthest ones give theirs up.
  for (let top = 0; top <= 59 * 300 - 1000; top += 400) {
    scroll(top);
    wake(inView());
    assert.ok(active <= 15, 'at ' + top + ': ' + active);
    assert.ok(inView().filter(p => p.gpu).length >= Math.min(15, inView().length),
      'the panels in the window have contexts at ' + top);
  }
  assert.ok(created > 15, 'contexts were made again as panels came and went');
  // A window with more panels than contexts: the ones beyond have none, and
  // none of the panels in it loses its own to a farther one.
  scroll(0);
  panels.forEach(p => { p.pane.rect = {top: 0, bottom: 200, width: 300}; });
  panels.forEach(p => { if (p.gpu) vm.runInContext('releaseGpu', context)(p); });
  assert.equal(active, 0);
  wake(panels);
  assert.equal(active, 15, 'the limit holds when every panel asks');
  assert.equal(panels.filter(p => p.gpu).length, 15);
  console.log('PASS the renderer releases its context without failing; 59 panels never hold more than 15 contexts while scrolling');
}
