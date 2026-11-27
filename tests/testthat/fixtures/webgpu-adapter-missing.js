// A page whose WebGPU gave no adapter (navigator.gpu exists, as in Chrome without
// Vulkan) tells its panels so: webGpuFailed() is true once the adapter request has
// failed, and false while WebGPU works or is not there at all.
const fs = require('fs'), assert = require('assert/strict'), vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
const tick = () => new Promise(resolve => setImmediate(resolve));
function load(navigator) {
  const scope = {window: {}, navigator, performance};
  vm.runInNewContext(source, scope);
  return scope.window.CerebroPointRenderer;
}
(async () => {
  let api = load({});
  await tick();
  assert.equal(api.webGpuFailed(), false, 'no navigator.gpu: not a WebGPU failure');
  api = load({gpu: {requestAdapter: () => Promise.resolve(null), getPreferredCanvasFormat: () => 'bgra8unorm'}});
  assert.equal(api.webGpuFailed(), false, 'not known before the request has answered');
  await tick(); await tick();
  assert.equal(api.webGpuFailed(), true, 'no adapter');
  api = load({gpu: {requestAdapter: () => Promise.resolve({requestDevice: () => Promise.reject(new Error('no device'))}),
    getPreferredCanvasFormat: () => 'bgra8unorm'}});
  await tick(); await tick(); await tick();
  assert.equal(api.webGpuFailed(), true, 'no device');
  api = load({gpu: {requestAdapter: () => Promise.reject(new Error('blocked')), getPreferredCanvasFormat: () => 'bgra8unorm'}});
  await tick(); await tick();
  assert.equal(api.webGpuFailed(), true, 'the request itself fails');
  console.log('PASS a page without a WebGPU adapter says so, and one without navigator.gpu does not');
})().catch(error => { console.error(error); process.exit(1); });
