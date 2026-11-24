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

// Places one point's quad corner. Both entry points end here: one reads a
// point from the vertex buffers, the other through the draw order.
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
  return place(points[i], unpack4x8unorm(colours[i]), pointLayers[i], vertexIndex);
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
        next.layers.length < count ||
        (next.drawOrder && next.drawOrder.length < count)) {
      throw new Error(label + ' point buffers are shorter than count.');
    }
    return {
      positions: next.positions,
      colors: next.colors,
      layers: next.layers,
      count: count,
      foreground: !!next.foreground,
      drawOrder: next.drawOrder || null
    };
  }
  function sameData(a, b) {
    return !!a && a.positions === b.positions && a.colors === b.colors &&
      a.layers === b.layers && a.count === b.count && a.foreground === b.foreground &&
      a.drawOrder === b.drawOrder;
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
      var limits = device.limits || {};
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
        } catch (error) {
          orderedPipeline = null;
        }
      }
      return {
        adapter: adapter, device: device, format: format, pipeline: pipeline,
        orderedPipeline: orderedPipeline,
        // Cells one storage binding holds: positions take 8 bytes a cell.
        orderLimit: orderedPipeline
          ? Math.floor((limits.maxStorageBufferBindingSize || 0) / 8) : 0
      };
    })();
    return sharedResources;
  }

  function createWebGpu(canvas) {
    var initializationStarted = performance.now();
    var adapter = null;
    var device = null;
    var context = null;
    var pipeline = null;
    var orderedPipeline = null;
    var orderLimit = 0;
    var uniformBuffers = [];
    var bindGroups = [];
    var orderedBindGroups = [];
    var positionBuffer = null;
    var colorBuffer = null;
    var layerBuffer = null;
    var orderBuffer = null;
    var pointsGroup = null;
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
      orderedPipeline = resources.orderedPipeline;
      orderLimit = resources.orderLimit;
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
      orderedBindGroups = orderedPipeline ? uniformBuffers.map(function (buffer) {
        return device.createBindGroup({
          layout: orderedPipeline.getBindGroupLayout(0),
          entries: [{ binding: 0, resource: { buffer: buffer } }]
        });
      }) : [];
      ready = true;
      metrics.ready = true;
      metrics.initializationMs = performance.now() - initializationStarted;
    })().catch(function (error) {
      fail(error);
      throw error;
    });

    function replaceBuffer(current, array, usage) {
      if (current) current.destroy();
      var buffer = device.createBuffer({
        // 8 bytes: the smallest storage binding of positions (one vec2f).
        size: Math.max(8, array.byteLength),
        usage: (usage === undefined ? GPUBufferUsage.VERTEX : usage) |
          (orderedPipeline ? GPUBufferUsage.STORAGE : 0) | GPUBufferUsage.COPY_DST
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
      if (next.drawOrder && (!orderedPipeline || next.count > orderLimit)) {
        throw new Error('WebGPU cannot draw these points in order.');
      }
      var groupStale = !data || data.positions !== next.positions ||
        data.colors !== next.colors || data.layers !== next.layers;
      if (next.drawOrder && (!data || data.drawOrder !== next.drawOrder)) {
        orderBuffer = replaceBuffer(orderBuffer, next.drawOrder, 0);
        groupStale = true;
      } else if (!next.drawOrder && orderBuffer) {
        orderBuffer.destroy(); orderBuffer = null;
      }
      if (next.drawOrder && (groupStale || !pointsGroup)) {
        pointsGroup = device.createBindGroup({
          layout: orderedPipeline.getBindGroupLayout(1),
          entries: [positionBuffer, colorBuffer, layerBuffer, orderBuffer].map(
            function (buffer, binding) {
              return { binding: binding, resource: { buffer: buffer } };
            })
        });
      } else if (!next.drawOrder) {
        pointsGroup = null;
      }
      data = next;
      metrics.pointCount = next.count;
      metrics.orderedDraw = !!next.drawOrder;
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
        var groups = bindGroups;
        if (data.drawOrder) {
          pass.setPipeline(orderedPipeline);
          pass.setBindGroup(1, pointsGroup);
          groups = orderedBindGroups;
        } else {
          pass.setPipeline(pipeline);
          pass.setVertexBuffer(0, positionBuffer);
          pass.setVertexBuffer(1, colorBuffer);
          pass.setVertexBuffer(2, layerBuffer);
        }
        if (data.foreground) {
          device.queue.writeBuffer(uniformBuffers[0], 0, uniforms(options, 0));
          device.queue.writeBuffer(uniformBuffers[1], 0, uniforms(options, 1));
          pass.setBindGroup(0, groups[0]);
          pass.draw(4, data.count);
          pass.setBindGroup(0, groups[1]);
          pass.draw(4, data.count);
        } else {
          device.queue.writeBuffer(uniformBuffers[0], 0, uniforms(options, -1));
          pass.setBindGroup(0, groups[0]);
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
      // True when this many points can be drawn through a drawOrder: the
      // point buffers keep their own order and drawOrder[k] is the k-th drawn.
      drawsInOrder: function (count) {
        return ready && !!orderedPipeline && count <= orderLimit;
      },
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
        passValue: gl.getUniformLocation(program, 'passValue')
      }
    };
  }

  // A draw's inputs as plain numbers: an unchanged frame is not drawn again.
  // Callers may mutate their view/rect objects in place, hence the copy.
  function webGlFrame(options, width, height, pixelRatio, canvas) {
    var view = options.view || { cx: 0.5, cy: 0.5, span: 1 };
    var rect = options.rect || { x: 0, y: 0, width: width, height: height };
    var border = options.border || null;
    var borderValue = border && border.color ? border.color : [0, 0, 0, 0];
    return [
      Number(view.cx), Number(view.cy), Math.max(1e-9, Number(view.span)),
      Number(rect.x), Number(rect.y), Number(rect.width), Number(rect.height),
      Math.max(0, Number(options.pointSize) || 0),
      border ? Math.max(0, Number(border.width) || 0) : 0,
      borderValue[0], borderValue[1], borderValue[2], borderValue[3],
      width, height, pixelRatio, canvas.width, canvas.height
    ];
  }
  function sameFrame(a, b) {
    return !!a && a.every(function (value, i) { return value === b[i]; });
  }

  // Sets the blending and uniforms of `frame` and draws the bound points.
  function drawWebGlPoints(gl, locations, frame, count, foreground, width, height, pixelRatio) {
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
    var passes = foreground ? [0, 1] : [-1];
    passes.forEach(function (passValue) {
      gl.uniform1f(locations.passValue, passValue);
      gl.drawArrays(gl.POINTS, 0, count);
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
    var positionBuffer = gl.createBuffer();
    var colorBuffer = gl.createBuffer();
    var layerBuffer = gl.createBuffer();
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

    var uploadedBytes = [-1, -1, -1];
    function upload(buffer, location, array, integer) {
      uploadWebGlAttribute(gl, uploadedBytes, buffer, location, array, integer);
    }

    function setData(next) {
      if (!ready) throw new Error('WebGL2 renderer is not ready.');
      next = checkData(next, 'WebGL2');
      if (next.drawOrder) throw new Error('WebGL2 draws points in buffer order only.');
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

    function draw(options) {
      if (!ready || !data) return false;
      var started = performance.now();
      // Hover and drag outlines live on the separate 2-D overlay. WebGL keeps
      // its drawing buffer, so an unchanged point layer can remain on screen.
      var frame = webGlFrame(options || {}, width, height, pixelRatio, canvas);
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
        width, height, pixelRatio);
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
      drawsInOrder: function () { return false; },
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
    var vertexArray = gl.createVertexArray();
    var positionBuffer = gl.createBuffer();
    var colorBuffer = gl.createBuffer();
    var layerBuffer = gl.createBuffer();
    var uploadedBytes = [-1, -1, -1];
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
      gl.deleteBuffer(layerBuffer);
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
      next = checkData(next, 'WebGL2');
      if (next.drawOrder) throw new Error('WebGL2 draws points in buffer order only.');
      if (sameData(data, next)) return;
      lastFrame = null;
      var started = performance.now();
      gl.bindVertexArray(vertexArray);
      if (!data || data.positions !== next.positions) {
        uploadWebGlAttribute(gl, uploadedBytes, positionBuffer, 0, next.positions, false);
        metrics.positionUploads++;
      }
      if (!data || data.colors !== next.colors) {
        uploadWebGlAttribute(gl, uploadedBytes, colorBuffer, 1, next.colors, false);
      }
      if (!data || data.layers !== next.layers) {
        uploadWebGlAttribute(gl, uploadedBytes, layerBuffer, 2, next.layers, true);
      }
      gl.bindVertexArray(null);
      data = next;
      metrics.pointCount = next.count;
      metrics.uploadSubmitMs = performance.now() - started;
    }

    function draw(options) {
      if (!ready || !data) return false;
      var started = performance.now();
      // The panel's canvas still shows an unchanged frame.
      var frame = webGlFrame(options || {}, width, height, pixelRatio, canvas);
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
        width, height, pixelRatio);
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
      drawsInOrder: function () { return false; },
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

  if (navigator.gpu) prepare().catch(function () {});
  global.CerebroPointRenderer = {
    create: create,
    createWebGl: createWebGl,
    createSharedWebGl: createSharedWebGl
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
