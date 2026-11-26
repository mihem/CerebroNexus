(function (global) {
  'use strict';

  var SHADER_SOURCE = `
struct Uniforms {
  view: vec4f,
  rect: vec4f,
  canvas: vec4f,
  border: vec4f,
  style: vec4f,
}

@group(0) @binding(0) var<uniform> uniforms: Uniforms;

struct FlatVertexInput {
  @location(0) position: vec2f,
  @location(1) color: vec4f,
  @builtin(vertex_index) vertexIndex: u32,
}

struct VertexInput {
  @location(0) position: vec2f,
  @location(1) color: vec4f,
  @location(2) layer: u32,
  @builtin(vertex_index) vertexIndex: u32,
}

struct VertexOutput {
  @builtin(position) position: vec4f,
  @location(0) offset: vec2f,
  @location(1) color: vec4f,
}

// Places one point's quad corner. All vertex entry points end here: flat
// (no layer buffer), layered (vertex buffers), and ordered (storage buffers).
fn place(position: vec2f, color: vec4f, layer: u32, vertexIndex: u32) -> VertexOutput {
  var corners = array<vec2f, 4>(
    vec2f(-1.0, -1.0), vec2f(1.0, -1.0),
    vec2f(-1.0, 1.0), vec2f(1.0, 1.0)
  );
  let corner = corners[vertexIndex];
  let outerRadius = uniforms.canvas.z + uniforms.canvas.w * 0.5 + 1.0;
  let zoomed = (position - uniforms.view.xy) / uniforms.view.z + vec2f(0.5);
  let screen = vec2f(
    uniforms.rect.x + zoomed.x * uniforms.rect.z,
    uniforms.rect.y + (1.0 - zoomed.y) * uniforms.rect.w
  ) + corner * outerRadius;
  let clip = vec2f(
    screen.x / uniforms.canvas.x * 2.0 - 1.0,
    1.0 - screen.y / uniforms.canvas.y * 2.0
  );
  let hidden = uniforms.style.z >= 0.0 &&
    f32(layer) != uniforms.style.z;
  var output: VertexOutput;
  output.position = select(vec4f(clip, 0.0, 1.0), vec4f(2.0, 2.0, 0.0, 1.0), hidden);
  output.offset = corner * outerRadius;
  output.color = color;
  return output;
}

@vertex
fn vertex_flat(input: FlatVertexInput) -> VertexOutput {
  return place(input.position, input.color, 0u, input.vertexIndex);
}

@vertex
fn vertex_main(input: VertexInput) -> VertexOutput {
  return place(input.position, input.color, input.layer, input.vertexIndex);
}

// The k-th point drawn is order[k]: points keep their place in the buffers
// and a new paint order (highest expression last) is one array of indices.
@group(1) @binding(0) var<storage, read> points: array<vec2f>;
@group(1) @binding(1) var<storage, read> colours: array<u32>;
@group(1) @binding(2) var<storage, read> pointLayers: array<u32>;
@group(1) @binding(3) var<storage, read> order: array<u32>;

@vertex
fn vertex_ordered(@builtin(vertex_index) vertexIndex: u32,
                  @builtin(instance_index) drawn: u32) -> VertexOutput {
  let i = order[drawn];
  var layer: u32 = 0u;
  if (uniforms.style.z >= 0.0) {
    layer = pointLayers[i];
  }
  return place(points[i], unpack4x8unorm(colours[i]), layer, vertexIndex);
}

@fragment
fn fragment_main(input: VertexOutput) -> @location(0) vec4f {
  let distanceFromCenter = length(input.offset);
  let radius = uniforms.style.x;
  let borderWidth = uniforms.style.y;
  let outerRadius = radius + borderWidth * 0.5;
  let aa = max(fwidth(distanceFromCenter), 0.75);
  let coverage = 1.0 - smoothstep(
    outerRadius - aa, outerRadius + aa, distanceFromCenter
  );
  if (coverage <= 0.0) { discard; }
  var borderMix = 0.0;
  if (borderWidth > 0.0) {
    borderMix = smoothstep(
      radius - borderWidth * 0.5 - aa,
      radius - borderWidth * 0.5 + aa,
      distanceFromCenter
    );
  }
  var baseColor = input.color;
  if (uniforms.style.w >= 0.0) {
    baseColor.a = uniforms.style.w;
  }
  // The outline fades with the point (the Canvas fallback's globalAlpha does
  // too): at a low point opacity or for a point the lasso left out it would
  // otherwise stay a ring of white while its fill is gone.
  var edge = uniforms.border;
  edge.a = edge.a * baseColor.a;
  var color = mix(baseColor, edge, borderMix);
  color.a *= coverage;
  return color;
}`;

  function sameAttr(prev, next, key, idKey) {
    if (!prev) return false;
    if (next[idKey] != null) return prev[idKey] === next[idKey];
    return prev[key] === next[key];
  }

  // Validates point data for setData and returns upload instructions plus the
  // lightweight record kept by the renderer. Callers that pass monotonic
  // `*Id` tokens can reuse scratch TypedArrays without pinning them here;
  // callers without tokens keep reference-equality semantics.
  function checkData(next, label, previous) {
    if (!next) throw new Error(label + ' point data is incomplete.');
    var count = Number(next.count) || 0;
    var foreground = !!next.foreground;
    var samePositions = sameAttr(previous, next, 'positions', 'positionsId');
    var sameColors = sameAttr(previous, next, 'colors', 'colorsId');
    var hasLayers = !!(next.layers ||
      (next.layersId != null && next.layersId > 0));
    var hasDrawOrder = !!(next.drawOrder ||
      (next.drawOrderId != null && next.drawOrderId > 0));
    if (!samePositions && !next.positions) {
      throw new Error(label + ' point data is incomplete.');
    }
    if (!sameColors && !next.colors) {
      throw new Error(label + ' point data is incomplete.');
    }
    if (foreground && !hasLayers) {
      throw new Error(label + ' point data is incomplete.');
    }
    var sameLayers = !hasLayers
      ? (!!previous && !previous.hasLayers)
      : (!!previous && previous.hasLayers &&
          sameAttr(previous, next, 'layers', 'layersId'));
    var sameDrawOrder = !hasDrawOrder
      ? (!!previous && !previous.hasDrawOrder)
      : (!!previous && previous.hasDrawOrder &&
          sameAttr(previous, next, 'drawOrder', 'drawOrderId'));
    if (hasLayers && !sameLayers && !next.layers) {
      throw new Error(label + ' point data is incomplete.');
    }
    if (hasDrawOrder && !sameDrawOrder && !next.drawOrder) {
      throw new Error(label + ' point data is incomplete.');
    }
    if ((!samePositions && next.positions.length < count * 2) ||
        (!sameColors && next.colors.length < count * 4) ||
        (hasLayers && !sameLayers && next.layers.length < count) ||
        (hasDrawOrder && !sameDrawOrder && next.drawOrder.length < count)) {
      throw new Error(label + ' point buffers are shorter than count.');
    }
    return {
      uploadPositions: !samePositions ? next.positions : null,
      uploadColors: !sameColors ? next.colors : null,
      uploadLayers: hasLayers && !sameLayers ? next.layers : null,
      uploadDrawOrder: hasDrawOrder && !sameDrawOrder ? next.drawOrder : null,
      unchanged: !!previous && samePositions && sameColors && sameLayers &&
        sameDrawOrder && previous.count === count &&
        previous.foreground === foreground,
      record: {
        positions: next.positionsId != null ? null : next.positions,
        positionsId: next.positionsId != null ? next.positionsId : null,
        colors: next.colorsId != null ? null : next.colors,
        colorsId: next.colorsId != null ? next.colorsId : null,
        layers: !hasLayers || next.layersId != null ? null : next.layers,
        layersId: hasLayers && next.layersId != null ? next.layersId : null,
        drawOrder: !hasDrawOrder || next.drawOrderId != null ? null : next.drawOrder,
        drawOrderId: hasDrawOrder && next.drawOrderId != null ? next.drawOrderId : null,
        count: count,
        foreground: foreground,
        hasLayers: hasLayers,
        hasDrawOrder: hasDrawOrder
      }
    };
  }

  // A draw's inputs as plain numbers: an unchanged frame is not drawn again.
  // Callers may mutate their view/rect/alpha objects in place, hence the copy.
  function drawFrame(options, width, height, pixelRatio, canvas) {
    var view = options.view || { cx: 0.5, cy: 0.5, span: 1 };
    var rect = options.rect || { x: 0, y: 0, width: width, height: height };
    var border = options.border || null;
    var borderValue = border && border.color ? border.color : [0, 0, 0, 0];
    var alpha = options.alpha || null;
    var bgAlpha = alpha && alpha.background != null ? Number(alpha.background) : -1;
    var fgAlpha = alpha && alpha.foreground != null ? Number(alpha.foreground) : -1;
    return [
      Number(view.cx), Number(view.cy), Math.max(1e-9, Number(view.span)),
      Number(rect.x), Number(rect.y), Number(rect.width), Number(rect.height),
      Math.max(0, Number(options.pointSize) || 0),
      border ? Math.max(0, Number(border.width) || 0) : 0,
      borderValue[0], borderValue[1], borderValue[2], borderValue[3],
      width, height, pixelRatio, canvas.width, canvas.height,
      bgAlpha, fgAlpha
    ];
  }
  function sameFrame(a, b) {
    return !!a && a.every(function (value, i) { return value === b[i]; });
  }

  // Reusable scratch buffer for widening compact Uint8Array layers to u32 on
  // WebGPU (written synchronously by queue.writeBuffer, never retained).
  var webGpuLayerScratch = null;
  function webGpuLayerBytes(array) {
    if (!array || array.BYTES_PER_ELEMENT !== 1) return array;
    var n = array.length;
    if (!webGpuLayerScratch || webGpuLayerScratch.length < n) {
      webGpuLayerScratch = new Uint32Array(n);
    }
    for (var i = 0; i < n; i++) webGpuLayerScratch[i] = array[i];
    return webGpuLayerScratch.subarray(0, n);
  }

  var sharedResources = null;
  function prepare() {
    if (sharedResources) return sharedResources;
    var pending = (async function () {
      if (!navigator.gpu) throw new Error('WebGPU is unavailable.');
      var adapter = await navigator.gpu.requestAdapter({
        powerPreference: 'high-performance'
      });
      if (!adapter) throw new Error('No WebGPU adapter is available.');
      var device = await adapter.requestDevice();
      var format = navigator.gpu.getPreferredCanvasFormat();
      var module = device.createShaderModule({ code: SHADER_SOURCE });
      var fragment = {
        module: module,
        entryPoint: 'fragment_main',
        targets: [{
          format: format,
          blend: {
            color: {
              srcFactor: 'src-alpha',
              dstFactor: 'one-minus-src-alpha',
              operation: 'add'
            },
            alpha: {
              srcFactor: 'one',
              dstFactor: 'one-minus-src-alpha',
              operation: 'add'
            }
          }
        }]
      };
      var flatPipeline = await device.createRenderPipelineAsync({
        layout: 'auto',
        vertex: {
          module: module,
          entryPoint: 'vertex_flat',
          buffers: [
            {
              arrayStride: 8,
              stepMode: 'instance',
              attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x2' }]
            },
            {
              arrayStride: 4,
              stepMode: 'instance',
              attributes: [{ shaderLocation: 1, offset: 0, format: 'unorm8x4' }]
            }
          ]
        },
        fragment: fragment,
        primitive: { topology: 'triangle-strip' }
      });
      var pipeline = await device.createRenderPipelineAsync({
        layout: 'auto',
        vertex: {
          module: module,
          entryPoint: 'vertex_main',
          buffers: [
            {
              arrayStride: 8,
              stepMode: 'instance',
              attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x2' }]
            },
            {
              arrayStride: 4,
              stepMode: 'instance',
              attributes: [{ shaderLocation: 1, offset: 0, format: 'unorm8x4' }]
            },
            {
              arrayStride: 4,
              stepMode: 'instance',
              attributes: [{ shaderLocation: 2, offset: 0, format: 'uint32' }]
            }
          ]
        },
        fragment: fragment,
        primitive: { topology: 'triangle-strip' }
      });
      // Drawing through an order needs storage buffers in the vertex stage;
      // without them every order stays on the CPU, as before.
      var orderedPipeline = null;
      var dummyLayerBuffer = null;
      var limits = device.limits || {};
      var maxStorageBindingBytes = limits.maxStorageBufferBindingSize || 0;
      if ((limits.maxStorageBuffersPerShaderStage || 0) >= 4 &&
          (limits.maxStorageBuffersInVertexStage === undefined ||
            limits.maxStorageBuffersInVertexStage >= 4)) {
        try {
          orderedPipeline = await device.createRenderPipelineAsync({
            layout: 'auto',
            vertex: { module: module, entryPoint: 'vertex_ordered', buffers: [] },
            fragment: fragment,
            primitive: { topology: 'triangle-strip' }
          });
          dummyLayerBuffer = device.createBuffer({
            size: 8,
            usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
          });
        } catch (error) {
          orderedPipeline = null;
          dummyLayerBuffer = null;
        }
      }
      var resources = {
        adapter: adapter, device: device, format: format,
        flatPipeline: flatPipeline, pipeline: pipeline,
        orderedPipeline: orderedPipeline,
        dummyLayerBuffer: dummyLayerBuffer,
        maxStorageBindingBytes: maxStorageBindingBytes,
        // Cells one storage binding holds: positions take 8 bytes a cell.
        orderLimit: orderedPipeline
          ? Math.floor(maxStorageBindingBytes / 8) : 0,
        renderers: [],
        lost: false,
        lostInfo: null
      };
      // One listener pair on the shared device dispatches to active panels
      // instead of stacking one closure per panel on the singleton device.
      if (device.lost && device.lost.then) {
        device.lost.then(function (info) {
          resources.lost = true;
          resources.lostInfo = info || 'WebGPU device lost.';
          if (sharedResources === pending) sharedResources = null;
          resources.renderers.slice().forEach(function (handle) {
            handle.lose(resources.lostInfo);
          });
          resources.renderers = [];
        });
      }
      if (device.addEventListener) {
        device.addEventListener('uncapturederror', function (event) {
          var err = (event && event.error) || 'Uncaptured WebGPU error.';
          resources.renderers.slice().forEach(function (handle) {
            handle.error(err);
          });
        });
      }
      return resources;
    })();
    sharedResources = pending;
    return sharedResources;
  }

  function createWebGpu(canvas) {
    var initializationStarted = performance.now();
    var shared = null;
    var adapter = null;
    var device = null;
    var context = null;
    var flatPipeline = null;
    var pipeline = null;
    var orderedPipeline = null;
    var dummyLayerBuffer = null;
    var orderLimit = 0;
    var maxStorageBindingBytes = 0;
    var uniformBuffers = [];
    var flatBindGroups = [];
    var bindGroups = [];
    var orderedBindGroups = [];
    var uniformData = new Float32Array(20);
    var positionSlot = { buffer: null, size: 0 };
    var colorSlot = { buffer: null, size: 0 };
    var layerSlot = { buffer: null, size: 0 };
    var orderSlot = { buffer: null, size: 0 };
    var pointsGroup = null;
    var pointsGroupUsesLayer = false;
    var data = null;
    var lastFrame = null;
    var width = 1;
    var height = 1;
    var pixelRatio = 1;
    var ready = false;
    var contextLost = false;
    var released = false;
    var gpuError = '';
    var metrics = {
      backend: 'webgpu', ready: false, pointCount: 0, positionUploads: 0
    };
    var resolveFailure;
    var failed = new Promise(function (resolve) { resolveFailure = resolve; });

    function destroySlot(slot) {
      if (slot.buffer) {
        try { slot.buffer.destroy(); } catch (error) { /* already gone */ }
      }
      slot.buffer = null;
      slot.size = 0;
    }

    function cleanupGpuObjects() {
      destroySlot(positionSlot);
      destroySlot(colorSlot);
      destroySlot(layerSlot);
      destroySlot(orderSlot);
      uniformBuffers.forEach(function (buffer) {
        if (buffer) {
          try { buffer.destroy(); } catch (error) { /* already gone */ }
        }
      });
      uniformBuffers = [];
      flatBindGroups = [];
      bindGroups = [];
      orderedBindGroups = [];
      pointsGroup = null;
      pointsGroupUsesLayer = false;
      if (context && context.unconfigure) {
        try { context.unconfigure(); } catch (error) { /* already gone */ }
      }
    }

    function detachFromShared() {
      if (!shared) return;
      var at = shared.renderers.indexOf(handle);
      if (at >= 0) shared.renderers.splice(at, 1);
    }

    function fail(error) {
      if (!ready && gpuError) return;
      lastFrame = null;
      gpuError = error && (error.message || error.reason)
        ? error.message || error.reason : String(error || 'WebGPU failed.');
      ready = false;
      metrics.ready = false;
      detachFromShared();
      cleanupGpuObjects();
      resolveFailure(gpuError);
    }

    var handle = {
      lose: function (info) {
        contextLost = true;
        if (!released) fail(info || 'WebGPU device lost.');
      },
      error: function (error) {
        if (!released) fail(error || 'Uncaptured WebGPU error.');
      }
    };

    // Releases this panel's GPU buffers and canvas swap chain without failing
    // the renderer or disturbing other panels on the shared device.
    function release() {
      if (released) return;
      released = true;
      ready = false;
      metrics.ready = false;
      lastFrame = null;
      data = null;
      detachFromShared();
      cleanupGpuObjects();
    }

    function resize(cssWidth, cssHeight, dpr) {
      lastFrame = null;
      width = Math.max(1, Number(cssWidth) || 1);
      height = Math.max(1, Number(cssHeight) || 1);
      pixelRatio = Math.max(1, Number(dpr) || 1);
      canvas.width = Math.max(1, Math.round(width * pixelRatio));
      canvas.height = Math.max(1, Math.round(height * pixelRatio));
      canvas.style.width = width + 'px';
      canvas.style.height = height + 'px';
    }

    function makeUniformBuffer() {
      return device.createBuffer({
        size: 80,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
      });
    }

    var initialized = (async function () {
      var resources = await prepare();
      if (released) return;
      if (resources.lost) {
        contextLost = true;
        throw resources.lostInfo || new Error('WebGPU device lost.');
      }
      shared = resources;
      adapter = resources.adapter;
      device = resources.device;
      flatPipeline = resources.flatPipeline;
      pipeline = resources.pipeline;
      orderedPipeline = resources.orderedPipeline;
      dummyLayerBuffer = resources.dummyLayerBuffer;
      orderLimit = resources.orderLimit;
      maxStorageBindingBytes = resources.maxStorageBindingBytes;
      metrics.adapter = adapter.info
        ? [adapter.info.vendor, adapter.info.architecture,
          adapter.info.device, adapter.info.description].filter(Boolean).join(' ')
        : '';
      context = canvas.getContext('webgpu');
      if (!context) throw new Error('Could not create a WebGPU canvas context.');
      var format = resources.format;
      context.configure({
        device: device,
        format: format,
        alphaMode: 'premultiplied',
        usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.COPY_SRC
      });
      uniformBuffers = [makeUniformBuffer(), makeUniformBuffer()];
      flatBindGroups = uniformBuffers.map(function (buffer) {
        return device.createBindGroup({
          layout: flatPipeline.getBindGroupLayout(0),
          entries: [{ binding: 0, resource: { buffer: buffer } }]
        });
      });
      bindGroups = uniformBuffers.map(function (buffer) {
        return device.createBindGroup({
          layout: pipeline.getBindGroupLayout(0),
          entries: [{ binding: 0, resource: { buffer: buffer } }]
        });
      });
      orderedBindGroups = orderedPipeline ? uniformBuffers.map(function (buffer) {
        return device.createBindGroup({
          layout: orderedPipeline.getBindGroupLayout(0),
          entries: [{ binding: 0, resource: { buffer: buffer } }]
        });
      }) : [];
      shared.renderers.push(handle);
      ready = true;
      metrics.ready = true;
      metrics.initializationMs = performance.now() - initializationStarted;
    })().catch(function (error) {
      if (!released) fail(error);
      throw error;
    });

    // Writes `array` into `slot`, reusing the existing GPUBuffer in place when
    // its capacity still fits (and is not more than 2x oversized) so recolouring
    // or reordering avoids buffer reallocation and bind-group rebuilds.
    // Returns true when the underlying GPUBuffer handle changed.
    function uploadSlot(slot, array, usage) {
      // 8 bytes: the smallest storage binding of positions (one vec2f), 4-byte aligned.
      var needed = Math.max(8, Math.ceil(array.byteLength / 4) * 4);
      var exceedsStorageLimit = !!(orderedPipeline && maxStorageBindingBytes &&
        slot.size > maxStorageBindingBytes && needed <= maxStorageBindingBytes);
      var reallocated = !(slot.buffer && slot.size >= needed &&
        slot.size <= needed * 2 && !exceedsStorageLimit);
      if (reallocated) {
        destroySlot(slot);
        slot.buffer = device.createBuffer({
          size: needed,
          usage: (usage === undefined ? GPUBufferUsage.VERTEX : usage) |
            (orderedPipeline ? GPUBufferUsage.STORAGE : 0) | GPUBufferUsage.COPY_DST
        });
        slot.size = needed;
      }
      if (array.byteLength) {
        device.queue.writeBuffer(
          slot.buffer,
          0,
          array.buffer,
          array.byteOffset,
          array.byteLength
        );
      }
      return reallocated;
    }

    function setData(next) {
      if (!ready) throw new Error('WebGPU renderer is not ready.');
      var checked = checkData(next, 'WebGPU', data);
      var record = checked.record;
      if (record.hasDrawOrder && (!orderedPipeline || record.count > orderLimit)) {
        throw new Error('WebGPU cannot draw these points in order.');
      }
      if (checked.unchanged) return;
      lastFrame = null;
      var started = performance.now();
      var buffersChanged = false;
      if (checked.uploadPositions) {
        if (uploadSlot(positionSlot, checked.uploadPositions)) buffersChanged = true;
        metrics.positionUploads++;
      }
      if (checked.uploadColors) {
        if (uploadSlot(colorSlot, checked.uploadColors)) buffersChanged = true;
      }
      if (checked.uploadLayers) {
        if (uploadSlot(layerSlot, webGpuLayerBytes(checked.uploadLayers))) buffersChanged = true;
      }
      if (checked.uploadDrawOrder) {
        if (uploadSlot(orderSlot, checked.uploadDrawOrder, 0)) buffersChanged = true;
      }
      // Keep layerSlot/orderSlot GPUBuffers pooled across temporary filter or
      // paint-order toggles so toggling back does not reallocate megabyte GPU
      // buffers; cleanupGpuObjects() still destroys them on release/loss.
      var wantLayerBinding = !!(record.hasLayers && layerSlot.buffer);
      if (record.hasDrawOrder &&
          (buffersChanged || !pointsGroup || pointsGroupUsesLayer !== wantLayerBinding)) {
        pointsGroupUsesLayer = wantLayerBinding;
        pointsGroup = device.createBindGroup({
          layout: orderedPipeline.getBindGroupLayout(1),
          entries: [
            positionSlot.buffer, colorSlot.buffer,
            wantLayerBinding ? layerSlot.buffer : dummyLayerBuffer, orderSlot.buffer
          ].map(function (buffer, binding) {
            return { binding: binding, resource: { buffer: buffer } };
          })
        });
      } else if (!record.hasDrawOrder) {
        pointsGroup = null;
        pointsGroupUsesLayer = false;
      }
      data = record;
      metrics.pointCount = record.count;
      metrics.orderedDraw = record.hasDrawOrder;
      metrics.uploadSubmitMs = performance.now() - started;
    }

    function fillUniforms(frame, passValue, passAlpha) {
      var radius = frame[7] * 0.5;
      uniformData[0] = frame[0];
      uniformData[1] = frame[1];
      uniformData[2] = frame[2];
      uniformData[3] = 0;
      uniformData[4] = frame[3];
      uniformData[5] = frame[4];
      uniformData[6] = frame[5];
      uniformData[7] = frame[6];
      uniformData[8] = width;
      uniformData[9] = height;
      uniformData[10] = radius;
      uniformData[11] = frame[8];
      uniformData[12] = frame[9] / 255;
      uniformData[13] = frame[10] / 255;
      uniformData[14] = frame[11] / 255;
      uniformData[15] = frame[12] / 255;
      uniformData[16] = radius;
      uniformData[17] = frame[8];
      uniformData[18] = passValue;
      uniformData[19] = passAlpha;
      return uniformData;
    }

    // Clears the canvas and, when given, records the point draws in one pass.
    function submitPass(drawPoints) {
      var encoder = device.createCommandEncoder();
      var pass = encoder.beginRenderPass({
        colorAttachments: [{
          view: context.getCurrentTexture().createView(),
          clearValue: { r: 0, g: 0, b: 0, a: 0 },
          loadOp: 'clear',
          storeOp: 'store'
        }]
      });
      if (drawPoints) drawPoints(pass);
      pass.end();
      device.queue.submit([encoder.finish()]);
    }

    function draw(options) {
      if (!ready || !data) return false;
      var started = performance.now();
      // Hover and drag outlines live on the separate 2-D overlay. Leaving the
      // swap chain untouched preserves the last presented point layer on screen.
      var frame = drawFrame(options || {}, width, height, pixelRatio, canvas);
      if (sameFrame(lastFrame, frame)) {
        metrics.drawSubmitMs = 0;
        return true;
      }
      lastFrame = null;
      var hasPoints = !!(data.count && frame[7]);
      if (hasPoints) {
        if (data.foreground) {
          fillUniforms(frame, 0, frame[18]);
          device.queue.writeBuffer(uniformBuffers[0], 0, uniformData);
          uniformData[18] = 1;
          uniformData[19] = frame[19];
          device.queue.writeBuffer(uniformBuffers[1], 0, uniformData);
        } else {
          fillUniforms(frame, -1, frame[18]);
          device.queue.writeBuffer(uniformBuffers[0], 0, uniformData);
        }
      }
      submitPass(hasPoints ? function (pass) {
        var groups = bindGroups;
        if (data.hasDrawOrder) {
          pass.setPipeline(orderedPipeline);
          pass.setBindGroup(1, pointsGroup);
          groups = orderedBindGroups;
        } else if (data.hasLayers) {
          pass.setPipeline(pipeline);
          pass.setVertexBuffer(0, positionSlot.buffer);
          pass.setVertexBuffer(1, colorSlot.buffer);
          pass.setVertexBuffer(2, layerSlot.buffer);
        } else {
          pass.setPipeline(flatPipeline);
          pass.setVertexBuffer(0, positionSlot.buffer);
          pass.setVertexBuffer(1, colorSlot.buffer);
          groups = flatBindGroups;
        }
        if (data.foreground) {
          pass.setBindGroup(0, groups[0]);
          pass.draw(4, data.count);
          pass.setBindGroup(0, groups[1]);
          pass.draw(4, data.count);
        } else {
          pass.setBindGroup(0, groups[0]);
          pass.draw(4, data.count);
        }
      } : null);
      metrics.drawSubmitMs = performance.now() - started;
      lastFrame = frame;
      return true;
    }

    function clear() {
      lastFrame = null;
      if (ready) submitPass(null);
    }

    resize(canvas.clientWidth || 1, canvas.clientHeight || 1,
      global.devicePixelRatio || 1);
    return {
      ready: initialized,
      failed: failed,
      isReady: function () { return ready; },
      // True when this many points can be drawn through a drawOrder: the
      // point buffers keep their own order and drawOrder[k] is the k-th drawn.
      drawsInOrder: function (count) {
        return ready && !!orderedPipeline && count <= orderLimit;
      },
      supportsUniformAlpha: function () { return true; },
      supportsBufferIds: function () { return true; },
      release: release,
      resize: resize,
      setData: setData,
      draw: draw,
      clear: clear,
      idle: function () {
        return device ? device.queue.onSubmittedWorkDone() : initialized;
      },
      stats: function () {
        return Object.assign({}, metrics, {
          contextLost: contextLost,
          error: gpuError
        });
      }
    };
  }

  var WEBGL_VERTEX_SOURCE = `#version 300 es
precision highp float;
precision highp int;

layout(location = 0) in vec2 position;
layout(location = 1) in vec4 color;
layout(location = 2) in uint layer;

uniform vec3 view;
uniform vec4 rect;
uniform vec2 canvasSize;
uniform float pointSize;
uniform float borderWidth;
uniform float pixelRatio;
uniform float passValue;

out vec4 pointColor;

void main() {
  vec2 zoomed = (position - view.xy) / view.z + vec2(0.5);
  vec2 screen = vec2(
    rect.x + zoomed.x * rect.z,
    rect.y + (1.0 - zoomed.y) * rect.w
  );
  vec2 clip = vec2(
    screen.x / canvasSize.x * 2.0 - 1.0,
    1.0 - screen.y / canvasSize.y * 2.0
  );
  bool hidden = passValue >= 0.0 && float(layer) != passValue;
  gl_Position = hidden
    ? vec4(2.0, 2.0, 0.0, 1.0)
    : vec4(clip, 0.0, 1.0);
  gl_PointSize = max(1.0, (pointSize + borderWidth + 2.0) * pixelRatio);
  pointColor = color;
}`;

  var WEBGL_FRAGMENT_SOURCE = `#version 300 es
precision highp float;

in vec4 pointColor;
uniform float pointSize;
uniform float borderWidth;
uniform float pixelRatio;
uniform vec4 borderColor;
uniform float passAlpha;
out vec4 outputColor;

void main() {
  float diameter = max(1.0, (pointSize + borderWidth + 2.0) * pixelRatio);
  float distanceFromCenter = length((gl_PointCoord - vec2(0.5)) * diameter);
  float radius = pointSize * pixelRatio * 0.5;
  float border = borderWidth * pixelRatio;
  float outerRadius = radius + border * 0.5;
  float aa = 0.85;
  float coverage = 1.0 - smoothstep(
    outerRadius - aa, outerRadius + aa, distanceFromCenter
  );
  if (coverage <= 0.0) discard;
  float borderMix = border > 0.0
    ? smoothstep(
        radius - border * 0.5 - aa,
        radius - border * 0.5 + aa,
        distanceFromCenter
      )
    : 0.0;
  vec4 baseColor = pointColor;
  if (passAlpha >= 0.0) baseColor.a = passAlpha;
  // The outline fades with the point, as the Canvas fallback's globalAlpha does.
  vec4 edge = borderColor;
  edge.a *= baseColor.a;
  outputColor = mix(baseColor, edge, borderMix);
  outputColor.a *= coverage;
}`;

  // The point program of a WebGL2 context and its uniforms' locations.
  function webGlProgram(gl) {
    function compile(type, source) {
      var shader = gl.createShader(type);
      gl.shaderSource(shader, source);
      gl.compileShader(shader);
      if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        var message = gl.getShaderInfoLog(shader) || 'WebGL2 shader failed.';
        gl.deleteShader(shader);
        throw new Error(message);
      }
      return shader;
    }

    var vertex = compile(gl.VERTEX_SHADER, WEBGL_VERTEX_SOURCE);
    var fragment = compile(gl.FRAGMENT_SHADER, WEBGL_FRAGMENT_SOURCE);
    var program = gl.createProgram();
    gl.attachShader(program, vertex);
    gl.attachShader(program, fragment);
    gl.linkProgram(program);
    gl.deleteShader(vertex);
    gl.deleteShader(fragment);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      var linkMessage = gl.getProgramInfoLog(program) || 'WebGL2 link failed.';
      gl.deleteProgram(program);
      throw new Error(linkMessage);
    }
    return {
      program: program,
      locations: {
        view: gl.getUniformLocation(program, 'view'),
        rect: gl.getUniformLocation(program, 'rect'),
        canvasSize: gl.getUniformLocation(program, 'canvasSize'),
        pointSize: gl.getUniformLocation(program, 'pointSize'),
        borderWidth: gl.getUniformLocation(program, 'borderWidth'),
        borderColor: gl.getUniformLocation(program, 'borderColor'),
        pixelRatio: gl.getUniformLocation(program, 'pixelRatio'),
        passValue: gl.getUniformLocation(program, 'passValue'),
        passAlpha: gl.getUniformLocation(program, 'passAlpha')
      }
    };
  }

  // Sets the blending and uniforms of `frame` and draws the bound points.
  function drawWebGlPoints(gl, locations, frame, count, foreground, width, height, pixelRatio, indexed) {
    gl.enable(gl.BLEND);
    gl.blendFuncSeparate(
      gl.SRC_ALPHA,
      gl.ONE_MINUS_SRC_ALPHA,
      gl.ONE,
      gl.ONE_MINUS_SRC_ALPHA
    );
    gl.uniform3f(locations.view, frame[0], frame[1], frame[2]);
    gl.uniform4f(locations.rect, frame[3], frame[4], frame[5], frame[6]);
    gl.uniform2f(locations.canvasSize, width, height);
    gl.uniform1f(locations.pointSize, frame[7]);
    gl.uniform1f(locations.borderWidth, frame[8]);
    gl.uniform4f(locations.borderColor,
      frame[9] / 255, frame[10] / 255, frame[11] / 255, frame[12] / 255);
    gl.uniform1f(locations.pixelRatio, pixelRatio);
    if (!count || !frame[7]) return;
    var passes = foreground
      ? [[0, frame[18]], [1, frame[19]]]
      : [[-1, frame[18]]];
    passes.forEach(function (pass) {
      gl.uniform1f(locations.passValue, pass[0]);
      gl.uniform1f(locations.passAlpha, pass[1]);
      if (indexed) {
        gl.drawElements(gl.POINTS, count, gl.UNSIGNED_INT, 0);
      } else {
        gl.drawArrays(gl.POINTS, 0, count);
      }
    });
  }

  // Points one attribute of the bound vertex array at `buffer`, filled with
  // `array`. `uploaded` holds each attribute's bytes: an array of the same
  // size is written into the existing store instead of reallocating it.
  function uploadWebGlAttribute(gl, uploaded, buffer, location, array, integer) {
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    if (uploaded[location] === array.byteLength) {
      gl.bufferSubData(gl.ARRAY_BUFFER, 0, array);
    } else {
      gl.bufferData(gl.ARRAY_BUFFER, array, gl.STATIC_DRAW);
      uploaded[location] = array.byteLength;
    }
    gl.enableVertexAttribArray(location);
    if (integer) {
      var intType = array.BYTES_PER_ELEMENT === 1
        ? gl.UNSIGNED_BYTE : gl.UNSIGNED_INT;
      gl.vertexAttribIPointer(location, 1, intType, 0, 0);
    } else {
      gl.vertexAttribPointer(
        location,
        location === 0 ? 2 : 4,
        location === 1 ? gl.UNSIGNED_BYTE : gl.FLOAT,
        location === 1,
        0,
        0
      );
    }
  }

  function uploadWebGlOrder(gl, uploaded, buffer, array) {
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer);
    if (uploaded[3] === array.byteLength) {
      gl.bufferSubData(gl.ELEMENT_ARRAY_BUFFER, 0, array);
    } else {
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, array, gl.STATIC_DRAW);
      uploaded[3] = array.byteLength;
    }
  }

  function createWebGl(canvas) {
    var initializationStarted = performance.now();
    // The fragment shader already smooths each point's edge by its coverage;
    // multisampling on top of it costs about 140 ms per million-point frame
    // on an integrated GPU (Intel UHD 770) for no visible difference.
    var gl = canvas.getContext('webgl2', {
      alpha: true,
      antialias: false,
      premultipliedAlpha: true,
      preserveDrawingBuffer: true
    });
    if (!gl) throw new Error('WebGL2 is unavailable.');

    var built = webGlProgram(gl);
    var program = built.program;
    var locations = built.locations;
    var canIndex = !!(gl.ELEMENT_ARRAY_BUFFER && gl.drawElements && gl.UNSIGNED_INT);
    var positionBuffer = gl.createBuffer();
    var colorBuffer = gl.createBuffer();
    var layerBuffer = null;
    var orderBuffer = null;
    var data = null;
    var width = 1;
    var height = 1;
    var pixelRatio = 1;
    var ready = true;
    var contextLost = false;
    var released = false;
    var gpuError = '';
    var metrics = {
      backend: 'webgl2', ready: true, pointCount: 0, positionUploads: 0,
      initializationMs: performance.now() - initializationStarted
    };
    var lastFrame = null;
    var resolveFailure;
    var failed = new Promise(function (resolve) { resolveFailure = resolve; });

    function fail(error) {
      if (!ready && gpuError) return;
      lastFrame = null;
      gpuError = error && error.message ? error.message : String(error || 'WebGL2 failed.');
      ready = false;
      metrics.ready = false;
      resolveFailure(gpuError);
    }

    canvas.addEventListener('webglcontextlost', function (event) {
      event.preventDefault();
      contextLost = true;
      // Given up on purpose (release()): not a failure.
      if (!released) fail('WebGL2 context lost.');
    });

    // Gives the context back at once: a page holds about 16 and drops the
    // oldest of any more. The renderer is not used afterwards.
    function release() {
      if (released) return;
      released = true;
      ready = false;
      lastFrame = null;
      try {
        var extension = gl.getExtension('WEBGL_lose_context');
        if (extension) extension.loseContext();
      } catch (error) { /* already gone */ }
    }

    function resize(cssWidth, cssHeight, dpr) {
      lastFrame = null;
      width = Math.max(1, Number(cssWidth) || 1);
      height = Math.max(1, Number(cssHeight) || 1);
      pixelRatio = Math.max(1, Number(dpr) || 1);
      canvas.width = Math.max(1, Math.round(width * pixelRatio));
      canvas.height = Math.max(1, Math.round(height * pixelRatio));
      canvas.style.width = width + 'px';
      canvas.style.height = height + 'px';
      gl.viewport(0, 0, canvas.width, canvas.height);
    }

    var uploadedBytes = [-1, -1, -1, -1];
    function upload(buffer, location, array, integer) {
      uploadWebGlAttribute(gl, uploadedBytes, buffer, location, array, integer);
    }

    function setData(next) {
      if (!ready) throw new Error('WebGL2 renderer is not ready.');
      var checked = checkData(next, 'WebGL2', data);
      var record = checked.record;
      if (record.hasDrawOrder && !canIndex) {
        throw new Error('WebGL2 draws points in buffer order only.');
      }
      if (checked.unchanged) return;
      lastFrame = null;
      var started = performance.now();
      gl.useProgram(program);
      if (checked.uploadPositions) {
        upload(positionBuffer, 0, checked.uploadPositions, false);
        metrics.positionUploads++;
      }
      if (checked.uploadColors) {
        upload(colorBuffer, 1, checked.uploadColors, false);
      }
      if (checked.uploadLayers) {
        if (!layerBuffer) layerBuffer = gl.createBuffer();
        upload(layerBuffer, 2, checked.uploadLayers, true);
      } else if (!record.hasLayers) {
        if (gl.disableVertexAttribArray) gl.disableVertexAttribArray(2);
        if (gl.vertexAttribI4ui) gl.vertexAttribI4ui(2, 0, 0, 0, 0);
        if (layerBuffer) {
          if (gl.deleteBuffer) gl.deleteBuffer(layerBuffer);
          layerBuffer = null;
          uploadedBytes[2] = -1;
        }
      }
      if (checked.uploadDrawOrder) {
        if (!orderBuffer) orderBuffer = gl.createBuffer();
        uploadWebGlOrder(gl, uploadedBytes, orderBuffer, checked.uploadDrawOrder);
      } else if (!record.hasDrawOrder && orderBuffer) {
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, null);
        if (gl.deleteBuffer) gl.deleteBuffer(orderBuffer);
        orderBuffer = null;
        uploadedBytes[3] = -1;
      }
      data = record;
      metrics.pointCount = record.count;
      metrics.orderedDraw = record.hasDrawOrder;
      metrics.uploadSubmitMs = performance.now() - started;
    }

    function draw(options) {
      if (!ready || !data) return false;
      var started = performance.now();
      // Hover and drag outlines live on the separate 2-D overlay. WebGL keeps
      // its drawing buffer, so an unchanged point layer can remain on screen.
      var frame = drawFrame(options || {}, width, height, pixelRatio, canvas);
      if (sameFrame(lastFrame, frame)) {
        if (gl.getError() !== gl.NO_ERROR) { lastFrame = null; return false; }
        metrics.drawSubmitMs = 0;
        return true;
      }
      lastFrame = null;
      gl.useProgram(program);
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
      drawWebGlPoints(gl, locations, frame, data.count, data.foreground,
        width, height, pixelRatio, data.hasDrawOrder);
      gl.flush();
      metrics.drawSubmitMs = performance.now() - started;
      var succeeded = gl.getError() === gl.NO_ERROR;
      if (succeeded) lastFrame = frame;
      return succeeded;
    }

    function clear() {
      lastFrame = null;
      if (!ready) return;
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
    }

    resize(canvas.clientWidth || 1, canvas.clientHeight || 1,
      global.devicePixelRatio || 1);
    return {
      ready: Promise.resolve(),
      failed: failed,
      isReady: function () { return ready; },
      drawsInOrder: function (count) {
        return ready && canIndex && !(count < 0);
      },
      supportsUniformAlpha: function () { return true; },
      supportsBufferIds: function () { return true; },
      release: release,
      resize: resize,
      setData: setData,
      draw: draw,
      clear: clear,
      idle: function () { return Promise.resolve(); },
      stats: function () {
        return Object.assign({}, metrics, {
          contextLost: contextLost,
          error: gpuError,
          adapter: gl.getParameter(gl.RENDERER) || ''
        });
      }
    };
  }

  // ---- one WebGL2 context for every panel -------------------------------------
  // A page keeps about 16 WebGL contexts and drops the oldest of any more, so
  // one context per panel cannot serve a grid of 59 sections, and making and
  // dropping contexts as panels scroll by costs about 11 ms a panel (context,
  // shaders) plus rebuilding its data. Here every panel shares one hidden
  // context, as WebGPU's panels share one device: the program is compiled
  // once, each panel keeps its own buffers in the context, and a panel is
  // drawn into the corner of the shared canvas and copied into its own canvas
  // (2-D). That canvas keeps the picture while the panel scrolls away and back.
  var sharedWebGlState = null;
  function sharedWebGl() {
    if (sharedWebGlState) return sharedWebGlState;
    var doc = global.document;
    if (!doc || !doc.createElement) throw new Error('WebGL2 is unavailable.');
    var canvas = doc.createElement('canvas');
    canvas.width = 1;
    canvas.height = 1;
    // The drawing buffer is copied out right after each draw, in the same
    // task, so it need not be kept for the compositor.
    var gl = canvas.getContext('webgl2', {
      alpha: true,
      antialias: false,
      premultipliedAlpha: true,
      preserveDrawingBuffer: false
    });
    if (!gl) throw new Error('WebGL2 is unavailable.');
    var built = webGlProgram(gl);
    var state = {
      gl: gl, canvas: canvas, program: built.program, locations: built.locations,
      renderers: [], adapter: gl.getParameter(gl.RENDERER) || ''
    };
    canvas.addEventListener('webglcontextlost', function (event) {
      event.preventDefault();
      // Every panel's buffers went with the context: each renderer fails, and
      // the next one asked for makes a new context.
      if (sharedWebGlState === state) sharedWebGlState = null;
      state.renderers.slice().forEach(function (renderer) {
        renderer.lose('WebGL2 context lost.');
      });
      state.renderers = [];
    });
    sharedWebGlState = state;
    return state;
  }

  function createSharedWebGl(canvas) {
    var initializationStarted = performance.now();
    var shared = sharedWebGl();
    var gl = shared.gl;
    var target = canvas.getContext('2d');
    if (!target) throw new Error('A 2-D canvas is unavailable.');
    var canIndex = !!(gl.ELEMENT_ARRAY_BUFFER && gl.drawElements && gl.UNSIGNED_INT);
    var vertexArray = gl.createVertexArray();
    var positionBuffer = gl.createBuffer();
    var colorBuffer = gl.createBuffer();
    var layerBuffer = null;
    var orderBuffer = null;
    var uploadedBytes = [-1, -1, -1, -1];
    var data = null;
    var width = 1;
    var height = 1;
    var pixelRatio = 1;
    var ready = true;
    var contextLost = false;
    var released = false;
    var gpuError = '';
    var metrics = {
      backend: 'webgl2', shared: true, ready: true, pointCount: 0, positionUploads: 0,
      initializationMs: performance.now() - initializationStarted
    };
    var lastFrame = null;
    var resolveFailure;
    var failed = new Promise(function (resolve) { resolveFailure = resolve; });

    function fail(error) {
      if (!ready && gpuError) return;
      lastFrame = null;
      gpuError = error && error.message ? error.message : String(error || 'WebGL2 failed.');
      ready = false;
      metrics.ready = false;
      resolveFailure(gpuError);
    }
    var handle = {
      lose: function (message) {
        contextLost = true;
        if (!released) fail(message);
      }
    };
    shared.renderers.push(handle);

    // Gives the panel's buffers back; the shared context stays.
    function release() {
      if (released) return;
      released = true;
      ready = false;
      lastFrame = null;
      var at = shared.renderers.indexOf(handle);
      if (at >= 0) shared.renderers.splice(at, 1);
      if (contextLost || gl.isContextLost()) return;
      gl.deleteBuffer(positionBuffer);
      gl.deleteBuffer(colorBuffer);
      if (layerBuffer) gl.deleteBuffer(layerBuffer);
      if (orderBuffer) gl.deleteBuffer(orderBuffer);
      layerBuffer = null;
      orderBuffer = null;
      gl.deleteVertexArray(vertexArray);
    }

    function resize(cssWidth, cssHeight, dpr) {
      lastFrame = null;
      width = Math.max(1, Number(cssWidth) || 1);
      height = Math.max(1, Number(cssHeight) || 1);
      pixelRatio = Math.max(1, Number(dpr) || 1);
      canvas.width = Math.max(1, Math.round(width * pixelRatio));
      canvas.height = Math.max(1, Math.round(height * pixelRatio));
      canvas.style.width = width + 'px';
      canvas.style.height = height + 'px';
    }

    function setData(next) {
      if (!ready) throw new Error('WebGL2 renderer is not ready.');
      var checked = checkData(next, 'WebGL2', data);
      var record = checked.record;
      if (record.hasDrawOrder && !canIndex) {
        throw new Error('WebGL2 draws points in buffer order only.');
      }
      if (checked.unchanged) return;
      lastFrame = null;
      var started = performance.now();
      gl.bindVertexArray(vertexArray);
      if (checked.uploadPositions) {
        uploadWebGlAttribute(gl, uploadedBytes, positionBuffer, 0, checked.uploadPositions, false);
        metrics.positionUploads++;
      }
      if (checked.uploadColors) {
        uploadWebGlAttribute(gl, uploadedBytes, colorBuffer, 1, checked.uploadColors, false);
      }
      if (checked.uploadLayers) {
        if (!layerBuffer) layerBuffer = gl.createBuffer();
        uploadWebGlAttribute(gl, uploadedBytes, layerBuffer, 2, checked.uploadLayers, true);
      } else if (!record.hasLayers) {
        if (gl.disableVertexAttribArray) gl.disableVertexAttribArray(2);
        if (gl.vertexAttribI4ui) gl.vertexAttribI4ui(2, 0, 0, 0, 0);
        if (layerBuffer) {
          if (gl.deleteBuffer) gl.deleteBuffer(layerBuffer);
          layerBuffer = null;
          uploadedBytes[2] = -1;
        }
      }
      if (checked.uploadDrawOrder) {
        if (!orderBuffer) orderBuffer = gl.createBuffer();
        uploadWebGlOrder(gl, uploadedBytes, orderBuffer, checked.uploadDrawOrder);
      } else if (!record.hasDrawOrder && orderBuffer) {
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, null);
        if (gl.deleteBuffer) gl.deleteBuffer(orderBuffer);
        orderBuffer = null;
        uploadedBytes[3] = -1;
      }
      gl.bindVertexArray(null);
      data = record;
      metrics.pointCount = record.count;
      metrics.orderedDraw = record.hasDrawOrder;
      metrics.uploadSubmitMs = performance.now() - started;
    }

    function draw(options) {
      if (!ready || !data) return false;
      var started = performance.now();
      // The panel's canvas still shows an unchanged frame.
      var frame = drawFrame(options || {}, width, height, pixelRatio, canvas);
      if (sameFrame(lastFrame, frame)) {
        metrics.drawSubmitMs = 0;
        return true;
      }
      lastFrame = null;
      var source = shared.canvas, w = canvas.width, h = canvas.height;
      // The shared canvas only grows: resizing it reallocates its buffer.
      if (source.width < w || source.height < h) {
        source.width = Math.max(source.width, w);
        source.height = Math.max(source.height, h);
      }
      gl.useProgram(shared.program);
      gl.bindVertexArray(vertexArray);
      // The panel is drawn in the bottom-left corner of the shared canvas,
      // which is the top-left w x h of its picture counted from its bottom.
      gl.viewport(0, 0, w, h);
      gl.enable(gl.SCISSOR_TEST);
      gl.scissor(0, 0, w, h);
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
      drawWebGlPoints(gl, shared.locations, frame, data.count, data.foreground,
        width, height, pixelRatio, data.hasDrawOrder);
      gl.disable(gl.SCISSOR_TEST);
      gl.bindVertexArray(null);
      if (gl.isContextLost()) return false;
      target.save();
      target.setTransform(1, 0, 0, 1, 0, 0);
      target.globalAlpha = 1;
      target.globalCompositeOperation = 'copy';
      target.drawImage(source, 0, source.height - h, w, h, 0, 0, w, h);
      target.restore();
      metrics.drawSubmitMs = performance.now() - started;
      lastFrame = frame;
      return true;
    }

    function clear() {
      lastFrame = null;
      target.save();
      target.setTransform(1, 0, 0, 1, 0, 0);
      target.clearRect(0, 0, canvas.width, canvas.height);
      target.restore();
    }

    resize(canvas.clientWidth || 1, canvas.clientHeight || 1,
      global.devicePixelRatio || 1);
    return {
      ready: Promise.resolve(),
      failed: failed,
      isReady: function () { return ready; },
      drawsInOrder: function (count) {
        return ready && canIndex && !(count < 0);
      },
      supportsUniformAlpha: function () { return true; },
      supportsBufferIds: function () { return true; },
      release: release,
      resize: resize,
      setData: setData,
      draw: draw,
      clear: clear,
      idle: function () { return Promise.resolve(); },
      stats: function () {
        return Object.assign({}, metrics, {
          contextLost: contextLost,
          error: gpuError,
          adapter: shared.adapter
        });
      }
    };
  }

  function create(canvas) {
    if (navigator.gpu) return createWebGpu(canvas);
    return createWebGl(canvas);
  }

  // WebGPU is there (navigator.gpu) but gave no adapter or no device, as Chrome
  // does without Vulkan: a panel then takes the shared WebGL2 renderer at once
  // instead of making a WebGPU one that fails (59 failures and replacements
  // when a grid of 59 sections opens).
  var webGpuFailed = false;
  if (navigator.gpu) prepare().catch(function () { webGpuFailed = true; });
  global.CerebroPointRenderer = {
    create: create,
    createWebGl: createWebGl,
    createSharedWebGl: createSharedWebGl,
    webGpuFailed: function () { return webGpuFailed; }
  };

  // ---- panels a page without a GPU may show ---------------------------------
  // A browser with no GPU behind WebGPU or WebGL (software rendering, or only
  // Canvas) draws every point on the CPU, so a page of many panels is slow to
  // open and to scroll. It then shows 2 panels at a time until the user lifts
  // the limit (the "Show all panels" preference, kept in the browser).
  // cap() is 0 when nothing is limited. It is 0 until the check, which is
  // asynchronous, has finished; onChange() tells the page when that changes.
  var PANEL_CAP = 2, UNLOCK_KEY = 'cerebro_panel_cap_unlocked';
  var noGpu = false, unlocked = false, capListeners = [];
  var doc = global.document, hasDom = !!(doc && doc.addEventListener && doc.documentElement);
  try { unlocked = global.localStorage.getItem(UNLOCK_KEY) === '1'; } catch (error) { /* no storage */ }

  function capNow() { return noGpu && !unlocked ? PANEL_CAP : 0; }
  function syncUnlockBox() {
    var box = doc.getElementById('panel_cap_unlock_checkbox');
    if (box && box.checked !== unlocked) box.checked = unlocked;
  }
  function capChanged() {
    if (!hasDom) return;
    var root = doc.documentElement;
    root.classList.toggle('cerebro-no-gpu', noGpu);
    root.classList.toggle('cerebro-panel-cap', capNow() > 0);
    syncUnlockBox();
    if (global.Shiny && global.Shiny.setInputValue) {
      global.Shiny.setInputValue('cerebro_panel_cap', capNow());
    }
    capListeners.slice().forEach(function (fn) { fn(capNow()); });
  }
  // A real GPU behind WebGPU (a software "fallback adapter" is none) or WebGL2
  // (failIfMajorPerformanceCaveat refuses software rendering in most browsers;
  // the renderer's name catches what it lets through). ?gpu_cap=off skips it.
  async function hasHardwareGpu() {
    if (global.CerebroPanelCapOff ||
        /[?&]gpu_cap=off\b/.test((global.location && global.location.search) || '')) return true;
    if (navigator.gpu) {
      try {
        var adapter = await navigator.gpu.requestAdapter();
        var info = adapter && adapter.info;
        if (adapter && !adapter.isFallbackAdapter && !(info && info.isFallbackAdapter)) return true;
      } catch (error) { /* try WebGL */ }
    }
    try {
      var canvas = doc.createElement('canvas');
      var gl = canvas.getContext('webgl2', { failIfMajorPerformanceCaveat: true });
      if (gl) {
        // Chrome with software rendering allowed still hands out a context
        // here, so the renderer's name tells software from a GPU.
        var debug = gl.getExtension('WEBGL_debug_renderer_info');
        var renderer = String(debug ? gl.getParameter(debug.UNMASKED_RENDERER_WEBGL) : '');
        var lose = gl.getExtension('WEBGL_lose_context');
        if (lose) lose.loseContext();
        return !/swiftshader|llvmpipe|softpipe|software|basic render/i.test(renderer);
      }
    } catch (error) { /* none */ }
    return false;
  }
  var capReady = !hasDom ? Promise.resolve() : hasHardwareGpu().then(function (hardware) {
    noGpu = !hardware;
    capChanged();
  }, function () { /* unknown: no limit */ });
  if (hasDom) {
    doc.addEventListener('shiny:value', function (event) {
      if (event.name === 'preferences_options') global.setTimeout(syncUnlockBox, 0);
    });
    doc.addEventListener('shiny:connected', capChanged);
    doc.addEventListener('change', function (event) {
      if (event.target && event.target.id === 'panel_cap_unlock_checkbox') {
        global.CerebroPanelCap.setUnlocked(event.target.checked);
      }
    });
    doc.addEventListener('click', function (event) {
      var button = event.target.closest && event.target.closest('[data-cerebro-cap-unlock]');
      if (button) global.CerebroPanelCap.setUnlocked(true);
    });
  }
  global.CerebroPanelCap = {
    max: PANEL_CAP,
    ready: capReady,
    cap: capNow,
    limited: function () { return noGpu; },
    onChange: function (fn) { capListeners.push(fn); },
    setUnlocked: function (value) {
      unlocked = !!value;
      try { global.localStorage.setItem(UNLOCK_KEY, unlocked ? '1' : '0'); } catch (error) { /* no storage */ }
      capChanged();
    }
  };
})(window);
