test_that("WebGPU failures make the renderer fall back", {
  skip_if_no_node()
  renderer <- viewer_test_path("www", "cell_points_gpu.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const assert = require('assert');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(renderer, quote = '"')
      ),
      "let uncaptured;",
      "const device = {",
      "  lost: new Promise(() => {}),",
      "  addEventListener: (name, handler) => { if (name === 'uncapturederror') uncaptured = handler; },",
      "  createShaderModule: () => ({}),",
      "  createRenderPipelineAsync: async () => ({ getBindGroupLayout: () => ({}) }),",
      "  createBuffer: () => ({ destroy() {} }),",
      "  createBindGroup: () => ({})",
      "};",
      "Object.defineProperty(global, 'navigator', { configurable: true, value: { gpu: {",
      "  requestAdapter: async () => ({ info: {}, requestDevice: async () => device }),",
      "  getPreferredCanvasFormat: () => 'rgba8unorm'",
      "} } });",
      "global.window = global; global.devicePixelRatio = 1;",
      "global.GPUBufferUsage = { UNIFORM: 1, COPY_DST: 2, VERTEX: 4 };",
      "global.GPUTextureUsage = { RENDER_ATTACHMENT: 1, COPY_SRC: 2 };",
      "eval(source);",
      "(async () => {",
      "  const canvas = { clientWidth: 10, clientHeight: 10, style: {},",
      "    getContext: () => ({ configure() {} }) };",
      "  const renderer = CerebroPointRenderer.create(canvas);",
      "  await renderer.ready;",
      "  uncaptured({ error: new Error('driver rejected the frame') });",
      "  await renderer.failed;",
      "  assert.strictEqual(renderer.isReady(), false);",
      "  assert.strictEqual(renderer.stats().ready, false);",
      "  assert.match(renderer.stats().error, /driver rejected the frame/);",
      "})().catch(error => { console.error(error); process.exit(1); });"
    ),
    runner
  )
  status <- system2("node", runner)

  expect_identical(status, 0L)
})

test_that("WebGL2 fallback submits a million points in one GPU draw", {
  skip_if_no_node()
  renderer <- viewer_test_path("www", "cell_points_gpu.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "const assert = require('assert');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(renderer, quote = '"')
      ),
      "const calls = [];",
      "let driverError=0, lost;",
      "const gl = {",
      "  VERTEX_SHADER:1, FRAGMENT_SHADER:2, COMPILE_STATUS:3, LINK_STATUS:4,",
      "  ARRAY_BUFFER:5, STATIC_DRAW:6, UNSIGNED_INT:7, UNSIGNED_BYTE:8,",
      "  FLOAT:9, POINTS:10, COLOR_BUFFER_BIT:11, BLEND:12,",
      "  SRC_ALPHA:13, ONE_MINUS_SRC_ALPHA:14, ONE:15, NO_ERROR:0, RENDERER:16,",
      "  createShader: () => ({}), shaderSource() {}, compileShader() {},",
      "  getShaderParameter: () => true, getShaderInfoLog: () => '',",
      "  deleteShader() {}, createProgram: () => ({}), attachShader() {},",
      "  linkProgram() {}, getProgramParameter: () => true,",
      "  getProgramInfoLog: () => '', deleteProgram() {},",
      "  getUniformLocation: (_program, name) => name, createBuffer: () => ({}),",
      "  viewport() {}, bindBuffer() {}, bufferData() {},",
      "  enableVertexAttribArray() {}, vertexAttribIPointer() {},",
      "  vertexAttribPointer() {}, useProgram() {}, uniform1f() {},",
      "  uniform2f() {}, uniform3f() {}, uniform4f() {}, clearColor() {},",
      "  clear() {}, enable() {}, blendFuncSeparate() {}, flush() {},",
      "  drawArrays: (mode, first, count) => calls.push([mode, first, count]),",
      "  getError: () => driverError, getParameter: () => 'mock-webgl2'",
      "};",
      "const canvas = {clientWidth:1280,clientHeight:720,style:{},",
      "  addEventListener(name,fn) { if(name==='webglcontextlost') lost=fn; },",
      "  getContext(type,opts) { assert.strictEqual(opts.preserveDrawingBuffer,true); return type==='webgl2'?gl:null; }};",
      "Object.defineProperty(global, 'navigator', {configurable:true,value:{}});",
      "global.window=global; global.devicePixelRatio=1; eval(source);",
      "const count=1000000, renderer=CerebroPointRenderer.create(canvas);",
      "const data={positions:new Float32Array(count*2),",
      "  colors:new Uint8Array(count*4),layers:new Uint32Array(count),",
      "  count:count,foreground:false}; renderer.setData(data);",
      "assert.strictEqual(renderer.draw({pointSize:2}), true);",
      "assert.strictEqual(renderer.stats().backend, 'webgl2');",
      "assert.strictEqual(renderer.stats().pointCount, count);",
      "assert.deepStrictEqual(calls, [[gl.POINTS, 0, count]]);",
      "renderer.setData(data); renderer.draw({pointSize:2});",
      "assert.strictEqual(calls.length,1,'unchanged overlay must keep existing point layer');",
      "const opts={pointSize:2,view:{cx:.5,cy:.5,span:1},rect:{x:0,y:0,width:1280,height:720}};",
      "renderer.draw(opts); assert.strictEqual(calls.length,1);",
      "for(const change of [()=>opts.view.cx=.6,()=>opts.view.cy=.6,()=>opts.view.span=.8,",
      "  ()=>opts.rect.x=1,()=>opts.rect.y=1,()=>opts.rect.width=1200,()=>opts.rect.height=700,",
      "  ()=>opts.pointSize=3,()=>opts.border={width:1,color:[255,0,0,255]},",
      "  ()=>opts.border.width=2,()=>opts.border.color[1]=120,()=>opts.border.color[3]=128]) {",
      "  const n=calls.length; change(); renderer.draw(opts); assert.strictEqual(calls.length,n+1);",
      "  renderer.draw(opts); assert.strictEqual(calls.length,n+1);",
      "}",
      "let n=calls.length; renderer.clear(); renderer.draw(opts); assert.strictEqual(calls.length,++n);",
      "renderer.resize(1280,720,1);renderer.draw(opts);assert.strictEqual(calls.length,++n);",
      "renderer.resize(1280,720,2);renderer.draw(opts);assert.strictEqual(calls.length,++n);",
      "renderer.setData({...data,colors:new Uint8Array(count*4)});renderer.draw(opts);assert.strictEqual(calls.length,++n);",
      "renderer.setData({...data,foreground:true});renderer.draw(opts);n+=2;assert.strictEqual(calls.length,n);",
      "renderer.draw(opts);assert.strictEqual(calls.length,n);",
      "driverError=1;assert.strictEqual(renderer.draw(opts),false,'cached frame must not conceal driver errors');",
      "driverError=0;renderer.draw(opts);n+=2;assert.strictEqual(calls.length,n);",
      "assert(calls.every(call=>call[2]===count),'every draw retains all points');",
      "lost({preventDefault(){}});assert.strictEqual(renderer.draw(opts),false);",
      "assert.strictEqual(renderer.stats().contextLost,true);"
    ),
    runner
  )

  expect_identical(system2("node", runner), 0L)
})
