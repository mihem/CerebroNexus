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

  // Validates point data for setData and returns the record a renderer keeps.
  function checkData(next, label) {
    if (!next || !next.positions || !next.colors || !next.layers) {
      throw new Error(label + ' point data is incomplete.');
    }
    var count = Number(next.count) || 0;
    if (next.positions.length < count * 2 || next.colors.length < count * 4 ||
        next.layers.length < count) {
      throw new Error(label + ' point buffers are shorter than count.');
    }
    return {
      positions: next.positions,
      colors: next.colors,
      layers: next.layers,
      count: count,
      foreground: !!next.foreground
    };
  }
  function sameData(a, b) {
    return !!a && a.positions === b.positions && a.colors === b.colors &&
      a.layers === b.layers && a.count === b.count && a.foreground === b.foreground;
  }

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

  function createWebGpu(canvas) {
    var initializationStarted = performance.now();
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
    var metrics = {
      backend: 'webgpu', ready: false, pointCount: 0, positionUploads: 0
    };
    var resolveFailure;
    var failed = new Promise(function (resolve) { resolveFailure = resolve; });

    function fail(error) {
      if (!ready && gpuError) return;
      gpuError = error && (error.message || error.reason)
        ? error.message || error.reason : String(error || 'WebGPU failed.');
      ready = false;
      metrics.ready = false;
      resolveFailure(gpuError);
    }

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
        fail(info || 'WebGPU device lost.');
      });
      device.addEventListener('uncapturederror', function (event) {
        fail(event.error || 'Uncaptured WebGPU error.');
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
      metrics.initializationMs = performance.now() - initializationStarted;
    })().catch(function (error) {
      fail(error);
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
      next = checkData(next, 'WebGPU');
      if (sameData(data, next)) return;
      var started = performance.now();
      if (!data || data.positions !== next.positions) {
        positionBuffer = replaceBuffer(positionBuffer, next.positions);
        metrics.positionUploads++;
      }
      if (!data || data.colors !== next.colors) {
        colorBuffer = replaceBuffer(colorBuffer, next.colors);
      }
      if (!data || data.layers !== next.layers) {
        layerBuffer = replaceBuffer(layerBuffer, next.layers);
      }
      data = next;
      metrics.pointCount = next.count;
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
      options = options || {};
      var started = performance.now();
      submitPass(data.count && Number(options.pointSize) ? function (pass) {
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
      } : null);
      metrics.drawSubmitMs = performance.now() - started;
      return true;
    }

    function clear() {
      if (ready) submitPass(null);
    }

    resize(canvas.clientWidth || 1, canvas.clientHeight || 1,
      global.devicePixelRatio || 1);
    return {
      ready: initialized,
      failed: failed,
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
  outputColor = mix(pointColor, borderColor, borderMix);
  outputColor.a *= coverage;
}`;

  function createWebGl(canvas) {
    var initializationStarted = performance.now();
    var gl = canvas.getContext('webgl2', {
      alpha: true,
      antialias: true,
      premultipliedAlpha: true,
      preserveDrawingBuffer: true
    });
    if (!gl) throw new Error('WebGL2 is unavailable.');

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

    var locations = {
      view: gl.getUniformLocation(program, 'view'),
      rect: gl.getUniformLocation(program, 'rect'),
      canvasSize: gl.getUniformLocation(program, 'canvasSize'),
      pointSize: gl.getUniformLocation(program, 'pointSize'),
      borderWidth: gl.getUniformLocation(program, 'borderWidth'),
      borderColor: gl.getUniformLocation(program, 'borderColor'),
      pixelRatio: gl.getUniformLocation(program, 'pixelRatio'),
      passValue: gl.getUniformLocation(program, 'passValue')
    };
    var positionBuffer = gl.createBuffer();
    var colorBuffer = gl.createBuffer();
    var layerBuffer = gl.createBuffer();
    var data = null;
    var width = 1;
    var height = 1;
    var pixelRatio = 1;
    var ready = true;
    var contextLost = false;
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
      fail('WebGL2 context lost.');
    });

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

    function upload(buffer, location, array, integer) {
      gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
      gl.bufferData(gl.ARRAY_BUFFER, array, gl.STATIC_DRAW);
      gl.enableVertexAttribArray(location);
      if (integer) {
        gl.vertexAttribIPointer(location, 1, gl.UNSIGNED_INT, 0, 0);
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

    function setData(next) {
      if (!ready) throw new Error('WebGL2 renderer is not ready.');
      next = checkData(next, 'WebGL2');
      if (sameData(data, next)) return;
      lastFrame = null;
      var started = performance.now();
      gl.useProgram(program);
      if (!data || data.positions !== next.positions) {
        upload(positionBuffer, 0, next.positions, false);
        metrics.positionUploads++;
      }
      if (!data || data.colors !== next.colors) {
        upload(colorBuffer, 1, next.colors, false);
      }
      if (!data || data.layers !== next.layers) {
        upload(layerBuffer, 2, next.layers, true);
      }
      data = next;
      metrics.pointCount = next.count;
      metrics.uploadSubmitMs = performance.now() - started;
    }

    function drawPass(passValue) {
      gl.uniform1f(locations.passValue, passValue);
      gl.drawArrays(gl.POINTS, 0, data.count);
    }

    function draw(options) {
      if (!ready || !data) return false;
      options = options || {};
      var started = performance.now();
      var view = options.view || { cx: 0.5, cy: 0.5, span: 1 };
      var rect = options.rect || { x: 0, y: 0, width: width, height: height };
      var border = options.border || null;
      var borderValue = border && border.color ? border.color : [0, 0, 0, 0];
      // Hover and drag outlines live on the separate 2-D overlay. WebGL keeps
      // its drawing buffer, so an unchanged point layer can remain on screen.
      // Copy scalar values: callers may mutate their view/rect objects in place.
      var frame = [
        Number(view.cx), Number(view.cy), Math.max(1e-9, Number(view.span)),
        Number(rect.x), Number(rect.y), Number(rect.width), Number(rect.height),
        Math.max(0, Number(options.pointSize) || 0),
        border ? Math.max(0, Number(border.width) || 0) : 0,
        borderValue[0], borderValue[1], borderValue[2], borderValue[3],
        width, height, pixelRatio, canvas.width, canvas.height
      ];
      if (lastFrame && frame.every(function (value, i) { return value === lastFrame[i]; })) {
        if (gl.getError() !== gl.NO_ERROR) { lastFrame = null; return false; }
        metrics.drawSubmitMs = 0;
        return true;
      }
      lastFrame = null;
      gl.useProgram(program);
      gl.viewport(0, 0, canvas.width, canvas.height);
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
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
      if (data.count && Number(options.pointSize)) {
        if (data.foreground) {
          drawPass(0);
          drawPass(1);
        } else {
          drawPass(-1);
        }
      }
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

  function create(canvas) {
    if (navigator.gpu) return createWebGpu(canvas);
    return createWebGl(canvas);
  }

  if (navigator.gpu) prepare().catch(function () {});
  global.CerebroPointRenderer = { create: create, createWebGl: createWebGl };
})(window);
