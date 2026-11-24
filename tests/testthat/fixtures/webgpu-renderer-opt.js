const fs = require('fs'), assert = require('assert/strict'), vm = require('vm');
const gpuSource = fs.readFileSync(process.argv[2], 'utf8');
const viewSource = fs.readFileSync(process.argv[3], 'utf8');

function makeHarness(options) {
  options = options || {};
  let adapterCalls = 0, deviceCalls = 0;
  const devices = [];

  function makeDevice() {
    deviceCalls++;
    let resolveLost;
    const lost = new Promise(resolve => { resolveLost = resolve; });
    const listeners = {};
    const stats = {
      addListenerCalls: 0,
      lostThenCalls: 0,
      buffersCreated: 0,
      buffersDestroyed: 0,
      bindGroupsCreated: 0,
      submits: 0,
      draws: 0,
      uniformWrites: [],
      uniformArrayRefs: new Set(),
      attributeWrites: []
    };
    const trackedLost = {
      then(onFulfilled, onRejected) {
        stats.lostThenCalls++;
        return lost.then(onFulfilled, onRejected);
      }
    };
    let nextBufferId = 0;
    const device = {
      limits: options.limits || {
        maxStorageBuffersPerShaderStage: 8,
        maxStorageBuffersInVertexStage: 8,
        maxStorageBufferBindingSize: 256
      },
      lost: trackedLost,
      addEventListener(name, fn) {
        stats.addListenerCalls++;
        (listeners[name] = listeners[name] || []).push(fn);
      },
      createShaderModule: () => ({}),
      createRenderPipelineAsync: async descriptor => ({
        entry: descriptor.vertex.entryPoint,
        getBindGroupLayout: group => ({group})
      }),
      createBuffer(desc) {
        stats.buffersCreated++;
        const buf = {
          id: ++nextBufferId,
          size: desc.size,
          usage: desc.usage,
          destroyed: false,
          destroy() {
            if (!buf.destroyed) {
              buf.destroyed = true;
              stats.buffersDestroyed++;
            }
          }
        };
        return buf;
      },
      createBindGroup(desc) {
        stats.bindGroupsCreated++;
        return {entries: desc.entries.map(e => e.resource.buffer)};
      },
      createCommandEncoder() {
        return {
          beginRenderPass() {
            return {
              setPipeline() {},
              setBindGroup() {},
              setVertexBuffer() {},
              draw(_vertices, instances) { stats.draws++; },
              end() {}
            };
          },
          finish: () => ({})
        };
      },
      queue: {
        writeBuffer(buffer, _offset, data, byteOffset, byteLength) {
          if (byteOffset === undefined) {
            stats.uniformArrayRefs.add(data);
            stats.uniformWrites.push({
              bufferId: buffer.id,
              values: Array.from(data)
            });
          } else {
            stats.attributeWrites.push({
              bufferId: buffer.id,
              bytes: Buffer.from(data, byteOffset, byteLength)
            });
          }
        },
        submit() { stats.submits++; },
        onSubmittedWorkDone: async () => {}
      },
      _stats: stats,
      _triggerLost(info) { resolveLost(info); },
      _triggerError(error) {
        (listeners.uncapturederror || []).slice().forEach(fn => fn({error}));
      }
    };
    devices.push(device);
    return device;
  }

  const scope = {
    window: {},
    performance,
    devicePixelRatio: 1,
    navigator: {
      gpu: {
        async requestAdapter() {
          adapterCalls++;
          return {
            info: {vendor: 'test-vendor'},
            requestDevice: async () => makeDevice()
          };
        },
        getPreferredCanvasFormat: () => 'rgba8unorm'
      }
    },
    GPUBufferUsage: {UNIFORM: 1, COPY_DST: 2, VERTEX: 4, STORAGE: 8},
    GPUTextureUsage: {RENDER_ATTACHMENT: 1, COPY_SRC: 2}
  };
  vm.runInNewContext(gpuSource, scope);

  function makeCanvas(w, h) {
    const ctxStats = {configured: 0, unconfigured: 0, textures: 0};
    const context = {
      configure() { ctxStats.configured++; },
      unconfigure() { ctxStats.unconfigured++; },
      getCurrentTexture() {
        ctxStats.textures++;
        return {createView: () => ({})};
      }
    };
    const canvas = {
      style: {},
      clientWidth: w || 1280,
      clientHeight: h || 720,
      width: w || 1280,
      height: h || 720,
      getContext: type => type === 'webgpu' ? context : null,
      _ctxStats: ctxStats
    };
    return canvas;
  }

  return {
    api: scope.window.CerebroPointRenderer,
    makeCanvas,
    devices,
    counts: () => ({adapterCalls, deviceCalls})
  };
}

// 1. Frame deduplication + uniform buffer reuse.
async function testFrameCacheAndUniforms() {
  const h = makeHarness();
  const canvas = h.makeCanvas(1280, 720);
  const renderer = h.api.create(canvas);
  await renderer.ready;
  const stats = h.devices[0]._stats;

  const count = 4;
  const data = {
    positions: new Float32Array(count * 2),
    colors: new Uint8Array(count * 4),
    layers: new Uint32Array(count),
    count,
    foreground: false
  };
  renderer.setData(data);
  const opts = {
    pointSize: 4,
    view: {cx: 0.5, cy: 0.5, span: 1},
    rect: {x: 0, y: 0, width: 1280, height: 720}
  };
  assert.equal(renderer.draw(opts), true);
  assert.equal(stats.submits, 1);
  assert.equal(canvas._ctxStats.textures, 1);
  assert.equal(stats.uniformWrites.length, 1);
  assert.equal(stats.uniformWrites[0].values[18], -1);

  // Repeating identical frame skips getCurrentTexture(), writeBuffer() and submit().
  renderer.setData(data);
  assert.equal(renderer.draw(opts), true);
  assert.equal(stats.submits, 1, 'unchanged WebGPU frame must skip submit');
  assert.equal(canvas._ctxStats.textures, 1, 'unchanged WebGPU frame must skip getCurrentTexture');
  assert.equal(stats.uniformWrites.length, 1);
  assert.equal(renderer.stats().drawSubmitMs, 0);

  // Every scalar or nested field change invalidates the cached frame.
  for (const change of [
    () => { opts.view.cx = 0.6; },
    () => { opts.view.cy = 0.4; },
    () => { opts.view.span = 0.8; },
    () => { opts.rect.x = 2; },
    () => { opts.rect.y = 3; },
    () => { opts.rect.width = 1200; },
    () => { opts.rect.height = 700; },
    () => { opts.pointSize = 6; },
    () => { opts.border = {width: 1, color: [255, 0, 0, 255]}; },
    () => { opts.border.width = 2; },
    () => { opts.border.color[1] = 128; },
    () => { opts.border.color[3] = 200; }
  ]) {
    const before = stats.submits;
    change();
    assert.equal(renderer.draw(opts), true);
    assert.equal(stats.submits, before + 1);
    assert.equal(renderer.draw(opts), true);
    assert.equal(stats.submits, before + 1);
  }

  // clear(), resize(), and setData() changes invalidate the cached frame.
  let n = stats.submits;
  renderer.clear();
  n++; // clear submits an empty pass
  assert.equal(stats.submits, n);
  renderer.draw(opts);
  assert.equal(stats.submits, ++n);

  renderer.resize(1280, 720, 1);
  renderer.draw(opts);
  assert.equal(stats.submits, ++n);

  renderer.resize(1280, 720, 2);
  renderer.draw(opts);
  assert.equal(stats.submits, ++n);

  renderer.setData({...data, colors: new Uint8Array(count * 4)});
  renderer.draw(opts);
  assert.equal(stats.submits, ++n);

  // Foreground two-pass writes pass 0 and pass 1 via the same reused Float32Array.
  const writesBefore = stats.uniformWrites.length;
  renderer.setData({...data, foreground: true});
  renderer.draw(opts);
  assert.equal(stats.submits, ++n);
  assert.equal(stats.uniformWrites.length, writesBefore + 2);
  assert.equal(stats.uniformWrites[writesBefore].values[18], 0);
  assert.equal(stats.uniformWrites[writesBefore + 1].values[18], 1);
  assert.equal(stats.uniformArrayRefs.size, 1, 'uniform Float32Array is reused across all draws');

  // Identical foreground frame skips cleanly.
  renderer.draw(opts);
  assert.equal(stats.submits, n);

  // Zero pointSize clears once and skips repeated empty draws.
  renderer.draw({...opts, pointSize: 0});
  assert.equal(stats.submits, ++n);
  renderer.draw({...opts, pointSize: 0});
  assert.equal(stats.submits, n);

  console.log('PASS webgpu frame cache and zero-alloc uniforms');
}

// 2. In-place GPUBuffer and GPUBindGroup reuse on setData.
async function testBufferAndBindGroupReuse() {
  const h = makeHarness({
    limits: {
      maxStorageBuffersPerShaderStage: 8,
      maxStorageBuffersInVertexStage: 8,
      maxStorageBufferBindingSize: 80 // 10 points max for ordered draw
    }
  });
  const canvas = h.makeCanvas(100, 100);
  const renderer = h.api.create(canvas);
  await renderer.ready;
  const stats = h.devices[0]._stats;
  const initBuffers = stats.buffersCreated; // 2 uniform buffers
  const initGroups = stats.bindGroupsCreated; // 4 uniform bind groups

  const makeSet = n => ({
    positions: new Float32Array(n * 2),
    colors: new Uint8Array(n * 4),
    layers: new Uint32Array(n),
    drawOrder: new Uint32Array(n),
    count: n,
    foreground: false
  });

  // Initial ordered upload: 4 buffers + 1 pointsGroup bind group.
  let d = makeSet(8);
  renderer.setData(d);
  assert.equal(stats.buffersCreated, initBuffers + 4);
  assert.equal(stats.buffersDestroyed, 0);
  assert.equal(stats.bindGroupsCreated, initGroups + 1);

  // Recolouring + reordering same point count: 0 buffer allocations, 0 destroys, 0 bind groups.
  const writesBefore = stats.attributeWrites.length;
  renderer.setData({...d, colors: new Uint8Array(8 * 4).fill(9), drawOrder: new Uint32Array(8)});
  assert.equal(stats.buffersCreated, initBuffers + 4, 'same-size update reuses GPUBuffers');
  assert.equal(stats.buffersDestroyed, 0);
  assert.equal(stats.bindGroupsCreated, initGroups + 1, 'in-place update keeps GPUBindGroup');
  assert.equal(stats.attributeWrites.length, writesBefore + 2);

  // Moderate shrink (8 -> 5, 64 <= 40*2): still reused in place.
  const shrunk5 = makeSet(5);
  renderer.setData(shrunk5);
  assert.equal(stats.buffersCreated, initBuffers + 4);
  assert.equal(stats.buffersDestroyed, 0);
  assert.equal(stats.bindGroupsCreated, initGroups + 1);

  // Growth beyond capacity (8 -> 10): reallocates all 4 buffers and rebuilds pointsGroup.
  const grown10 = makeSet(10);
  renderer.setData(grown10);
  assert.equal(stats.buffersCreated, initBuffers + 8);
  assert.equal(stats.buffersDestroyed, 4);
  assert.equal(stats.bindGroupsCreated, initGroups + 2);

  // Large shrink (>2x: 10 -> 3): shrinks buffers to reclaim VRAM and rebuilds pointsGroup.
  const small3 = makeSet(3);
  renderer.setData(small3);
  assert.equal(stats.buffersCreated, initBuffers + 12);
  assert.equal(stats.buffersDestroyed, 8);
  assert.equal(stats.bindGroupsCreated, initGroups + 3);

  // Oversized non-ordered upload (> maxStorageBufferBindingSize=80 -> 12 points = 96 B),
  // followed by ordered upload of 8 points (64 B <= 80 B): must reallocate positions below limit.
  const big12 = {
    positions: new Float32Array(12 * 2),
    colors: new Uint8Array(12 * 4),
    layers: new Uint32Array(12),
    count: 12,
    foreground: false
  };
  renderer.setData(big12); // destroys orderBuffer + reallocates 3 buffers
  const beforeBack = stats.buffersCreated;
  renderer.setData(makeSet(8));
  assert.ok(stats.buffersCreated >= beforeBack + 2, 'oversized buffer shrinks back within storage limit');

  // Invalid drawOrder (> orderLimit) throws without destroying existing buffers.
  const destroyedBeforeErr = stats.buffersDestroyed;
  assert.throws(() => renderer.setData({
    positions: new Float32Array(11 * 2),
    colors: new Uint8Array(11 * 4),
    layers: new Uint32Array(11),
    drawOrder: new Uint32Array(11),
    count: 11,
    foreground: false
  }), /in order/);
  assert.equal(stats.buffersDestroyed, destroyedBeforeErr);

  console.log('PASS webgpu in-place GPUBuffer and GPUBindGroup reuse');
}

// 3. Single shared-device listeners, release() cleanup, and device.lost recovery.
async function testSharedDeviceLifecycleAndRelease() {
  const h = makeHarness();
  const panels = [];
  for (let i = 0; i < 60; i++) {
    const canvas = h.makeCanvas(60, 60);
    panels.push({canvas, renderer: h.api.create(canvas)});
  }
  await Promise.all(panels.map(p => p.renderer.ready));

  const dev0 = h.devices[0];
  assert.equal(dev0._stats.addListenerCalls, 1, 'one uncapturederror listener for 60 panels');
  assert.equal(dev0._stats.lostThenCalls, 1, 'one device.lost handler for 60 panels');

  // Give panel 0 data and release it: buffers destroyed, unconfigured, NOT failed.
  const p0 = panels[0];
  let p0Failed = false;
  p0.renderer.failed.then(() => { p0Failed = true; });
  p0.renderer.setData({
    positions: new Float32Array(4),
    colors: new Uint8Array(8),
    layers: new Uint32Array(2),
    drawOrder: new Uint32Array(2),
    count: 2,
    foreground: false
  });
  const destroyedBefore = dev0._stats.buffersDestroyed;
  p0.renderer.release();
  p0.renderer.release(); // idempotent
  assert.equal(p0.renderer.isReady(), false);
  assert.equal(p0.renderer.stats().ready, false);
  assert.equal(p0.renderer.draw({pointSize: 2}), false);
  assert.equal(p0.canvas._ctxStats.unconfigured, 1);
  // 4 attribute/order buffers + 2 uniform buffers = 6 buffers destroyed.
  assert.equal(dev0._stats.buffersDestroyed, destroyedBefore + 6);

  // Release before ready resolves: never configures or allocates.
  const earlyCanvas = h.makeCanvas(50, 50);
  const early = h.api.create(earlyCanvas);
  let earlyFailed = false;
  early.failed.then(() => { earlyFailed = true; });
  early.release();
  await early.ready;
  assert.equal(early.isReady(), false);
  assert.equal(earlyCanvas._ctxStats.configured, 0);

  // Uncaptured error fails remaining 59 panels, not released p0 or early.
  dev0._triggerError(new Error('driver fault'));
  await Promise.all(panels.slice(1).map(p => p.renderer.failed));
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(p0Failed, false, 'released panel does not fail on later device error');
  assert.equal(earlyFailed, false);
  assert.equal(panels.slice(1).every(p => !p.renderer.isReady()), true);

  // device.lost resets sharedResources so a fresh WebGPU panel acquires a new device.
  dev0._triggerLost({reason: 'destroyed', message: 'gpu reset'});
  await new Promise(resolve => setImmediate(resolve));
  const freshCanvas = h.makeCanvas(80, 80);
  const fresh = h.api.create(freshCanvas);
  await fresh.ready;
  assert.equal(fresh.isReady(), true);
  assert.equal(h.devices.length, 2, 'device.lost allows a fresh WebGPU device');

  // Verify cell_views.js calls release() when disabling or replacing a WebGPU renderer.
  const start = viewSource.indexOf('  function disableGpu(p)');
  const end = viewSource.indexOf('  function watchGpu(p, renderer)');
  assert.ok(start > 0 && end > start);
  let releasedOnReplace = 0, releasedOnDisable = 0;
  const ctx = {
    D: null,
    window: {CerebroPointRenderer: {createSharedWebGl: () => ({resize() {}, stats: () => ({backend: 'webgl2', shared: true})})}, devicePixelRatio: 1},
    createLayerCanvas: () => ({parentNode: {replaceChild() {}}, _cerebroPointRenderer: null}),
    hideGpu() {},
    watchGpu() {},
    project() {}
  };
  vm.createContext(ctx);
  vm.runInContext(viewSource.slice(start, end), ctx);
  const pReplace = {
    W: 100, H: 100,
    gpu: {stats: () => ({backend: 'webgpu'}), release() { releasedOnReplace++; }},
    gpuCanvas: {parentNode: {replaceChild() {}}, _cerebroPointRenderer: null}
  };
  assert.equal(ctx.failOver(pReplace), true);
  assert.equal(releasedOnReplace, 1, 'failOver releases failed WebGPU renderer before replacing');

  const pDisable = {
    W: 100, H: 100,
    gpu: {stats: () => ({backend: 'webgpu'}), release() { releasedOnDisable++; }}
  };
  ctx.disableGpu(pDisable);
  assert.equal(releasedOnDisable, 1, 'disableGpu releases GPU renderer');

  console.log('PASS webgpu shared-device listeners, release cleanup and failover release');
}

(async () => {
  await testFrameCacheAndUniforms();
  await testBufferAndBindGroupReuse();
  await testSharedDeviceLifecycleAndRelease();
})().catch(error => { console.error(error); process.exitCode = 1; });
