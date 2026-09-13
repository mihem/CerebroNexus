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

@vertex
fn vertex_main(input: VertexInput) -> VertexOutput {
  var corners = array<vec2f, 4>(
    vec2f(-1.0, -1.0), vec2f(1.0, -1.0),
    vec2f(-1.0, 1.0), vec2f(1.0, 1.0)
  );
  let corner = corners[input.vertexIndex];
  let outerRadius = uniforms.canvas.z + uniforms.canvas.w * 0.5 + 1.0;
  let zoomed = (input.position - uniforms.view.xy) / uniforms.view.z + vec2f(0.5);
  let screen = vec2f(
    uniforms.rect.x + zoomed.x * uniforms.rect.z,
    uniforms.rect.y + (1.0 - zoomed.y) * uniforms.rect.w
  ) + corner * outerRadius;
  let clip = vec2f(
    screen.x / uniforms.canvas.x * 2.0 - 1.0,
    1.0 - screen.y / uniforms.canvas.y * 2.0
  );
  let hidden = uniforms.style.z >= 0.0 &&
    f32(input.layer) != uniforms.style.z;
  var output: VertexOutput;
  output.position = select(vec4f(clip, 0.0, 1.0), vec4f(2.0, 2.0, 0.0, 1.0), hidden);
  output.offset = corner * outerRadius;
  output.color = input.color;
  return output;
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
  var color = mix(input.color, uniforms.border, borderMix);
  color.a *= coverage;
  return color;
}`;

  var sharedResources = null;
  function prepare() {
    if (sharedResources) return sharedResources;
    sharedResources = (async function () {
      if (!navigator.gpu) throw new Error('WebGPU is unavailable.');
      var adapter = await navigator.gpu.requestAdapter({
        powerPreference: 'high-performance'
      });
      if (!adapter) throw new Error('No WebGPU adapter is available.');
      var device = await adapter.requestDevice();
      var format = navigator.gpu.getPreferredCanvasFormat();
      var module = device.createShaderModule({ code: SHADER_SOURCE });
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
        fragment: {
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
        },
        primitive: { topology: 'triangle-strip' }
      });
      return { adapter: adapter, device: device, format: format, pipeline: pipeline };
    })();
    return sharedResources;
  }

  function create(canvas) {
    var adapter = null;
    var device = null;
    var context = null;
    var pipeline = null;
    var uniformBuffers = [];
    var bindGroups = [];
    var positionBuffer = null;
    var colorBuffer = null;
    var layerBuffer = null;
    var data = null;
    var width = 1;
    var height = 1;
    var ready = false;
    var contextLost = false;
    var gpuError = '';
    var metrics = { backend: 'webgpu', ready: false, pointCount: 0 };

    function resize(cssWidth, cssHeight, dpr) {
      width = Math.max(1, Number(cssWidth) || 1);
      height = Math.max(1, Number(cssHeight) || 1);
      dpr = Math.max(1, Number(dpr) || 1);
      canvas.width = Math.max(1, Math.round(width * dpr));
      canvas.height = Math.max(1, Math.round(height * dpr));
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
      adapter = resources.adapter;
      device = resources.device;
      pipeline = resources.pipeline;
      metrics.adapter = adapter.info
        ? [adapter.info.vendor, adapter.info.architecture,
          adapter.info.device, adapter.info.description].filter(Boolean).join(' ')
        : '';
      device.lost.then(function (info) {
        contextLost = true;
        gpuError = info.message || info.reason || 'WebGPU device lost.';
        ready = false;
      });
      device.addEventListener('uncapturederror', function (event) {
        gpuError = event.error && event.error.message
          ? event.error.message : 'Uncaptured WebGPU error.';
      });
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
      bindGroups = uniformBuffers.map(function (buffer) {
        return device.createBindGroup({
          layout: pipeline.getBindGroupLayout(0),
          entries: [{ binding: 0, resource: { buffer: buffer } }]
        });
      });
      ready = true;
      metrics.ready = true;
    })().catch(function (error) {
      gpuError = error.message || String(error);
      throw error;
    });

    function replaceBuffer(current, array) {
      if (current) current.destroy();
      var buffer = device.createBuffer({
        size: Math.max(4, array.byteLength),
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST
      });
      if (array.byteLength) {
        device.queue.writeBuffer(
          buffer,
          0,
          array.buffer,
          array.byteOffset,
          array.byteLength
        );
      }
      return buffer;
    }

    function setData(next) {
      if (!ready) throw new Error('WebGPU renderer is not ready.');
      if (!next || !next.positions || !next.colors || !next.layers) {
        throw new Error('WebGPU point data is incomplete.');
      }
      var count = Number(next.count) || 0;
      if (next.positions.length < count * 2 || next.colors.length < count * 4 ||
          next.layers.length < count) {
        throw new Error('WebGPU point buffers are shorter than count.');
      }
      if (data && data.positions === next.positions && data.colors === next.colors &&
          data.layers === next.layers && data.count === count &&
          data.foreground === !!next.foreground) return;
      var started = performance.now();
      positionBuffer = replaceBuffer(positionBuffer, next.positions);
      colorBuffer = replaceBuffer(colorBuffer, next.colors);
      layerBuffer = replaceBuffer(layerBuffer, next.layers);
      data = {
        positions: next.positions,
        colors: next.colors,
        layers: next.layers,
        count: count,
        foreground: !!next.foreground
      };
      metrics.pointCount = count;
      metrics.uploadSubmitMs = performance.now() - started;
    }

    function uniforms(options, passValue) {
      var view = options.view || { cx: 0.5, cy: 0.5, span: 1 };
      var rect = options.rect || { x: 0, y: 0, width: width, height: height };
      var pointSize = Math.max(0, Number(options.pointSize) || 0);
      var border = options.border;
      var borderWidth = border ? Math.max(0, Number(border.width) || 0) : 0;
      var borderColor = border && border.color ? border.color : [0, 0, 0, 0];
      return new Float32Array([
        Number(view.cx), Number(view.cy), Math.max(1e-9, Number(view.span)), 0,
        Number(rect.x), Number(rect.y), Number(rect.width), Number(rect.height),
        width, height, pointSize / 2, borderWidth,
        borderColor[0] / 255, borderColor[1] / 255,
        borderColor[2] / 255, borderColor[3] / 255,
        pointSize / 2, borderWidth, passValue, 0
      ]);
    }

    function draw(options) {
      if (!ready || !data) return false;
      options = options || {};
      var started = performance.now();
      var encoder = device.createCommandEncoder();
      var pass = encoder.beginRenderPass({
        colorAttachments: [{
          view: context.getCurrentTexture().createView(),
          clearValue: { r: 0, g: 0, b: 0, a: 0 },
          loadOp: 'clear',
          storeOp: 'store'
        }]
      });
      if (data.count && Number(options.pointSize)) {
        pass.setPipeline(pipeline);
        pass.setVertexBuffer(0, positionBuffer);
        pass.setVertexBuffer(1, colorBuffer);
        pass.setVertexBuffer(2, layerBuffer);
        if (data.foreground) {
          device.queue.writeBuffer(uniformBuffers[0], 0, uniforms(options, 0));
          device.queue.writeBuffer(uniformBuffers[1], 0, uniforms(options, 1));
          pass.setBindGroup(0, bindGroups[0]);
          pass.draw(4, data.count);
          pass.setBindGroup(0, bindGroups[1]);
          pass.draw(4, data.count);
        } else {
          device.queue.writeBuffer(uniformBuffers[0], 0, uniforms(options, -1));
          pass.setBindGroup(0, bindGroups[0]);
          pass.draw(4, data.count);
        }
      }
      pass.end();
      device.queue.submit([encoder.finish()]);
      metrics.drawSubmitMs = performance.now() - started;
      return true;
    }

    function clear() {
      if (!ready) return;
      var encoder = device.createCommandEncoder();
      var pass = encoder.beginRenderPass({
        colorAttachments: [{
          view: context.getCurrentTexture().createView(),
          clearValue: { r: 0, g: 0, b: 0, a: 0 },
          loadOp: 'clear',
          storeOp: 'store'
        }]
      });
      pass.end();
      device.queue.submit([encoder.finish()]);
    }

    resize(canvas.clientWidth || 1, canvas.clientHeight || 1,
      global.devicePixelRatio || 1);
    return {
      ready: initialized,
      isReady: function () { return ready; },
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

  prepare().catch(function () {});
  global.CerebroPointRenderer = {
    backend: 'webgpu', create: create, prepare: prepare
  };
})(window);
