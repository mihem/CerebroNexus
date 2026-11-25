const fs = require('fs'), assert = require('assert/strict'), vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');

async function check(backend) {
  const uploads = [];
  const record = array => uploads.push(Buffer.from(array.buffer, array.byteOffset, array.byteLength));
  const gl = {
    createShader: () => ({}), shaderSource() {}, compileShader() {},
    getShaderParameter: () => true, deleteShader() {},
    createProgram: () => ({}), attachShader() {}, linkProgram() {},
    getProgramParameter: () => true, getUniformLocation: () => ({}),
    createBuffer: () => ({}), useProgram() {}, bindBuffer() {},
    bufferData: (_target, array) => record(array),
    bufferSubData: (_target, _offset, array) => record(array), enableVertexAttribArray() {},
    vertexAttribIPointer() {}, vertexAttribPointer() {}, viewport() {},
    getParameter: () => 'test adapter'
  };
  const device = {
    createShaderModule: () => ({}),
    createRenderPipelineAsync: async () => ({getBindGroupLayout: () => ({})}),
    lost: new Promise(() => {}), addEventListener() {},
    createBuffer: () => ({destroy() {}}), createBindGroup: () => ({}),
    queue: {writeBuffer(_buffer, _offset, data, offset, length) {
      uploads.push(Buffer.from(data, offset, length));
    }}
  };
  const gpu = {
    requestAdapter: async () => ({requestDevice: async () => device}),
    getPreferredCanvasFormat: () => 'rgba8unorm'
  };
  const context = {configure() {}};
  const canvas = {
    style: {}, clientWidth: 100, clientHeight: 100, addEventListener() {},
    getContext: type => type === 'webgpu' ? context : gl
  };
  const scope = {
    window: {}, navigator: backend === 'webgpu' ? {gpu} : {}, performance,
    GPUBufferUsage: {UNIFORM: 1, COPY_DST: 2, VERTEX: 4},
    GPUTextureUsage: {RENDER_ATTACHMENT: 1, COPY_SRC: 2}
  };
  // Execute the complete renderer and use its existing public API. Only the
  // hardware boundary is replaced, so no source extraction/parser is needed.
  vm.runInNewContext(source, scope);
  const renderer = scope.window.CerebroPointRenderer.create(canvas);
  await renderer.ready;
  assert.equal(renderer.isReady(), true);
  const positions = new Float32Array([0, 0, 1, 1]);
  const colors = new Uint8Array([1, 2, 3, 255, 4, 5, 6, 255]);
  const layers = new Uint32Array([0, 0]);
  const bytes = array => Buffer.from(array.buffer, array.byteOffset, array.byteLength);
  renderer.setData({positions, colors, layers, count: 2, foreground: false});
  assert.deepEqual(uploads, [bytes(positions), bytes(colors), bytes(layers)]);
  renderer.setData({positions, colors, layers, count: 2, foreground: false});
  assert.equal(uploads.length, 3);
  const updated = colors.slice(); updated[3] = 13;
  renderer.setData({positions, colors: updated, layers, count: 2, foreground: false});
  assert.equal(uploads.length, 4);
  assert.deepEqual(uploads[3], bytes(updated));
  const newLayers = layers.slice(); newLayers[1] = 1;
  renderer.setData({positions, colors: updated, layers: newLayers, count: 2, foreground: true});
  assert.equal(uploads.length, 5);
  assert.deepEqual(uploads[4], bytes(newLayers));
  renderer.setData({positions, colors: updated, layers: newLayers, count: 1, foreground: true});
  assert.equal(uploads.length, 5);
  assert.equal(renderer.stats().pointCount, 1);
  const newPositions = positions.slice(); newPositions[0] = 0.25;
  renderer.setData({positions: newPositions, colors: updated, layers: newLayers, count: 2, foreground: true});
  assert.equal(uploads.length, 6);
  assert.deepEqual(uploads[5], bytes(newPositions));
  assert.equal(renderer.stats().positionUploads, 2);
  assert.throws(() => renderer.setData({positions: new Float32Array(1), colors, layers, count: 2}), /shorter/);
  console.log('PASS', backend, 'complete renderer: exact uploads, unchanged buffers and count-only changes');
}
// A device with storage buffers in the vertex stage draws through an order:
// the points stay where they are, the order is its own upload, and the
// ordered pipeline reads them; without the order the plain pipeline draws.
async function checkOrder() {
  const uploads = [], calls = [];
  const device = {
    limits: {maxStorageBuffersPerShaderStage: 8, maxStorageBufferBindingSize: 8 * 4},
    createShaderModule: () => ({}),
    createRenderPipelineAsync: async descriptor => ({
      entry: descriptor.vertex.entryPoint, getBindGroupLayout: group => ({group})
    }),
    lost: new Promise(() => {}), addEventListener() {},
    createBuffer: () => ({destroy() {}}),
    createBindGroup: descriptor => ({entries: descriptor.entries.length}),
    createCommandEncoder: () => ({
      beginRenderPass: () => ({
        setPipeline: pipeline => calls.push(['pipeline', pipeline.entry]),
        setBindGroup: (index, group) => calls.push(['group', index, group.entries]),
        setVertexBuffer: index => calls.push(['vertex', index]),
        draw: (vertices, instances) => calls.push(['draw', instances]),
        end() {}
      }),
      finish: () => ({})
    }),
    queue: {
      writeBuffer(_buffer, _offset, data, offset, length) {
        if (offset !== undefined) uploads.push(Buffer.from(data, offset, length));
      },
      submit() {}
    }
  };
  const scope = {
    window: {}, performance,
    navigator: {gpu: {
      requestAdapter: async () => ({requestDevice: async () => device}),
      getPreferredCanvasFormat: () => 'rgba8unorm'
    }},
    GPUBufferUsage: {UNIFORM: 1, COPY_DST: 2, VERTEX: 4, STORAGE: 8},
    GPUTextureUsage: {RENDER_ATTACHMENT: 1, COPY_SRC: 2}
  };
  const canvas = {style: {}, clientWidth: 10, clientHeight: 10, addEventListener() {},
    getContext: () => ({configure() {}, getCurrentTexture: () => ({createView: () => ({})})})};
  vm.runInNewContext(source, scope);
  const renderer = scope.window.CerebroPointRenderer.create(canvas);
  await renderer.ready;
  assert.equal(renderer.drawsInOrder(4), true);
  assert.equal(renderer.drawsInOrder(5), false, 'more points than one storage binding holds');
  const bytes = array => Buffer.from(array.buffer, array.byteOffset, array.byteLength);
  const positions = new Float32Array([0, 0, 1, 1, 2, 2]);
  const colors = new Uint8Array(12), layers = new Uint32Array(3);
  const drawOrder = new Uint32Array([2, 0, 1]);
  renderer.setData({positions, colors, layers, count: 3, foreground: false, drawOrder});
  assert.deepEqual(uploads, [bytes(positions), bytes(colors), bytes(layers), bytes(drawOrder)]);
  assert.equal(renderer.draw({pointSize: 2}), true);
  assert.deepEqual(calls, [['pipeline', 'vertex_ordered'], ['group', 1, 4], ['group', 0, 1], ['draw', 3]]);
  assert.equal(renderer.stats().orderedDraw, true);
  // A new order alone uploads only the order.
  const reorder = new Uint32Array([1, 2, 0]);
  renderer.setData({positions, colors, layers, count: 3, foreground: false, drawOrder: reorder});
  assert.equal(uploads.length, 5);
  assert.deepEqual(uploads[4], bytes(reorder));
  calls.length = 0;
  renderer.setData({positions, colors, layers, count: 3, foreground: false});
  renderer.draw({pointSize: 2});
  assert.deepEqual(calls.slice(0, 4), [['pipeline', 'vertex_main'], ['vertex', 0], ['vertex', 1], ['vertex', 2]]);
  assert.equal(uploads.length, 5, 'dropping the order keeps the points');
  assert.throws(() => renderer.setData({positions, colors, layers, count: 3,
    drawOrder: new Uint32Array(2)}), /shorter/);
  // Without storage buffers in the vertex stage there is no ordered pipeline.
  device.limits = {maxStorageBuffersPerShaderStage: 8, maxStorageBuffersInVertexStage: 0,
    maxStorageBufferBindingSize: 1 << 20};
  const plainScope = Object.assign({}, scope, {window: {}});
  vm.runInNewContext(source, plainScope);
  const plain = plainScope.window.CerebroPointRenderer.create(canvas);
  await plain.ready;
  assert.equal(plain.drawsInOrder(1), false);
  console.log('PASS webgpu draws through an order: order-only uploads, ordered pipeline, plain without it');
}
async function checkWebGlOrder() {
  const uploads = [], orderUploads = [], drawCalls = [];
  let deleted = 0;
  const gl = {
    POINTS: 1, UNSIGNED_INT: 2, UNSIGNED_BYTE: 3, FLOAT: 4,
    ARRAY_BUFFER: 5, ELEMENT_ARRAY_BUFFER: 6, STATIC_DRAW: 7,
    COLOR_BUFFER_BIT: 8, BLEND: 9, SRC_ALPHA: 10, ONE_MINUS_SRC_ALPHA: 11,
    ONE: 12, NO_ERROR: 0, RENDERER: 13,
    createShader: () => ({}), shaderSource() {}, compileShader() {},
    getShaderParameter: () => true, deleteShader() {},
    createProgram: () => ({}), attachShader() {}, linkProgram() {},
    getProgramParameter: () => true, getUniformLocation: (_p, name) => name,
    createBuffer: () => ({}), deleteBuffer() { deleted++; },
    useProgram() {}, bindBuffer() {},
    bufferData(target, array) {
      (target === gl.ELEMENT_ARRAY_BUFFER ? orderUploads : uploads).push(
        ['data', Buffer.from(array.buffer, array.byteOffset, array.byteLength)]
      );
    },
    bufferSubData(target, _offset, array) {
      (target === gl.ELEMENT_ARRAY_BUFFER ? orderUploads : uploads).push(
        ['sub', Buffer.from(array.buffer, array.byteOffset, array.byteLength)]
      );
    },
    enableVertexAttribArray() {}, disableVertexAttribArray() {},
    vertexAttribI4ui() {}, vertexAttribIPointer() {}, vertexAttribPointer() {},
    viewport() {}, clearColor() {}, clear() {}, enable() {}, blendFuncSeparate() {},
    uniform1f() {}, uniform2f() {}, uniform3f() {}, uniform4f() {}, flush() {},
    drawArrays: (_m, _f, count) => drawCalls.push(['arrays', count]),
    drawElements: (_m, count, type) => drawCalls.push(['elements', count, type]),
    getError: () => 0, getParameter: () => 'test adapter'
  };
  const scope = {window: {}, navigator: {}, performance};
  vm.runInNewContext(source, scope);
  const renderer = scope.window.CerebroPointRenderer.create({
    style: {}, clientWidth: 10, clientHeight: 10, addEventListener() {}, getContext: () => gl});
  assert.equal(renderer.drawsInOrder(3), true);
  const positions = new Float32Array([0, 0, 1, 1, 2, 2]);
  const colors = new Uint8Array(12), drawOrder = new Uint32Array([2, 0, 1]);
  renderer.setData({positions, colors, layers: null, count: 3, foreground: false, drawOrder});
  assert.equal(uploads.length, 2, 'layers: null skips layer buffer upload');
  assert.equal(orderUploads.length, 1);
  assert.equal(orderUploads[0][0], 'data');
  assert.equal(renderer.draw({pointSize: 2}), true);
  assert.deepEqual(drawCalls, [['elements', 3, gl.UNSIGNED_INT]]);
  // A new order of the same size reuses ELEMENT_ARRAY_BUFFER via bufferSubData.
  renderer.setData({positions, colors, layers: null, count: 3, foreground: false,
    drawOrder: new Uint32Array([1, 2, 0])});
  assert.equal(uploads.length, 2, 'order-only update keeps positions and colors');
  assert.equal(orderUploads.length, 2);
  assert.equal(orderUploads[1][0], 'sub');
  // Dropping drawOrder deletes orderBuffer and returns to drawArrays.
  renderer.setData({positions, colors, layers: null, count: 3, foreground: false});
  assert.equal(deleted, 1);
  assert.equal(renderer.draw({pointSize: 2}), true);
  assert.deepEqual(drawCalls[1], ['arrays', 3]);
  // A minimal gl without drawElements still reports drawsInOrder() === false.
  const bareGl = Object.assign({}, gl, {drawElements: undefined});
  const bare = scope.window.CerebroPointRenderer.create({
    style: {}, clientWidth: 10, clientHeight: 10, addEventListener() {}, getContext: () => bareGl});
  assert.equal(bare.drawsInOrder(1), false);
  assert.throws(() => bare.setData({positions: new Float32Array(2), colors: new Uint8Array(4),
    layers: new Uint32Array(1), count: 1, drawOrder: new Uint32Array(1)}), /buffer order only/);
  console.log('PASS webgl2 draws through an order via drawElements and skips null layers');
}
(async () => {
  await check('webgpu');
  await check('webgl2');
  await checkOrder();
  await checkWebGlOrder();
})().catch(error => { console.error(error); process.exitCode = 1; });
