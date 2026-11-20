// Only external boundaries are instrumented: Shiny messages, Canvas commands,
// and renderer inputs. All Viewer state stays private to its complete script.
window.viewerTest = (() => {
  const handlers = {}, requests = [], paints = [], uploads = [], events = [];
  window.Shiny = {
    addCustomMessageHandler(name, fn) {handlers[name] = fn;},
    setInputValue(name, value) {requests.push({name, value});}
  };
  for (const method of ['arc','fill','fillRect','moveTo','lineTo','drawImage','fillText','clearRect','scale']) {
    const original = CanvasRenderingContext2D.prototype[method];
    CanvasRenderingContext2D.prototype[method] = function (...args) {
      paints.push({canvas:this.canvas, method, args, fill:this.fillStyle, alpha:this.globalAlpha,
        matrix:method==='drawImage'?this.getTransform():undefined});
      return original.apply(this, args);
    };
  }
  // The real renderer's upload contract is tested separately against WebGPU
  // and WebGL2. This boundary records the bytes the complete Viewer submits.
  window.CerebroPointRenderer = {create(canvas) {
    let submission;
    return {ready:Promise.resolve(), failed:new Promise(()=>{}),
      isReady:()=>true, resize(){}, clear(){}, destroy(){}, idle:()=>Promise.resolve(),
      setData(data) {
        const copy = {canvas, count:data.count, foreground:data.foreground, positionSource:data.positions,
          positions:Array.from(data.positions.subarray(0,data.count*2)),
          colors:Array.from(data.colors.subarray(0,data.count*4)),
          layers:Array.from(data.layers.subarray(0,data.count))};
        uploads.push(copy);
        submission=copy;
      }, draw(options){if(submission)submission.rect=options.rect;return !window.viewerTest.failGpu;}, stats:()=>({backend:'webgl2',positionUploads:0})};
  }};
  for (const name of ['cerebro:cell-view-ready','cerebro:linkedviews-ready']) {
    window.addEventListener(name, event=>events.push({name,...event.detail}));
  }
  const context = {epoch:'browser-test',dataset_key:'A',generation:1};
  let token = 0, checks = 0;
  function check(value, message) {checks++; if (!value) throw Error(message || 'Behavior assertion '+checks);}
  function equal(actual, expected, label) {check(JSON.stringify(actual)===JSON.stringify(expected),
    (label || 'Output differs')+': '+JSON.stringify(actual).slice(0,240)+' != '+JSON.stringify(expected).slice(0,240));}
  async function settle() {
    await new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)));
    await new Promise(resolve=>setTimeout(resolve,25));
  }
  function send(name, message) {handlers[name](message);}
  function wire(message) {
    const chunks=[];let offset=0;
    function pack(value) {
      if (ArrayBuffer.isView(value)) {
        const type=value instanceof Float64Array?'f64':value instanceof Float32Array?'f32':
          value instanceof Int8Array?'i8':value instanceof Int16Array?'i16':'i32';
        const out={__cv_wire__:type,offset,bytes:value.byteLength,length:value.length};
        chunks.push(new Uint8Array(value.buffer,value.byteOffset,value.byteLength));offset+=value.byteLength;return out;
      }
      if(Array.isArray(value)) return value.map(pack);
      if(value && typeof value==='object') return Object.fromEntries(Object.entries(value).map(([k,v])=>[k,pack(v)]));
      return value;
    }
    const header=new TextEncoder().encode(JSON.stringify(pack(message)));
    const start=4+Math.ceil(header.length/4)*4, buffer=new ArrayBuffer(start+offset);
    new DataView(buffer).setUint32(0,header.length,true);new Uint8Array(buffer,4,header.length).set(header);
    offset=start;for(const chunk of chunks){new Uint8Array(buffer,offset,chunk.length).set(chunk);offset+=chunk.length;}
    return buffer;
  }
  function dataset(key='A',generation=1) {
    Object.assign(context,{dataset_key:key,generation});
    CerebroDatasetContext.receive({phase:'pending',dataset_context:{...context}});
    CerebroDatasetContext.receive({phase:'ready',dataset_context:{...context}});
  }
  function visible(id) {
    document.querySelectorAll('.cerebro-cell-view-host').forEach(el=>el.style.display=
      el.dataset.cellViewId===id?'block':'none');
  }
  function message(id='expression_projection',data={},meta={},extra={}) {
    return {id, dataset_context:{...context},render_token:++token,
      dataset_identity:{cell_fingerprint:'fp-'+context.dataset_key},
      data:{selection_key:['a','b','c'],x:[0,10,20],y:[0,10,0],color:[0,1,2],...data},
      meta:{color_type:'continuous',color_variable:'GENE',...meta},...extra};
  }
  // Sends a binary frame and returns the message the viewer decoded from it.
  function sendWire(name,frame) {
    const real=window.CBViewWire;let decoded=frame;
    window.CBViewWire={supported:real.supported,unpack:buffer=>(decoded=real.unpack(buffer))};
    try {send(name,wire(frame));} finally {window.CBViewWire=real;}
    return decoded;
  }
  async function render(data={},meta={},id='expression_projection',extra={}) {
    visible(id);const frame=sendWire('cell_view_binary',message(id,data,meta,extra));
    await settle();return frame;
  }
  async function recolor(data,meta={},id='expression_projection') {
    sendWire('cell_view_recolor_binary',{id,dataset_context:{...context},render_token:++token,
      data,meta:{color_type:'continuous',color_variable:'GENE',...meta}});await settle();
  }
  function arcs(id='cv-cv-a') {
    const last=paints.findLastIndex(p=>p.canvas.id===id&&p.method==='clearRect');
    return paints.slice(last+1).filter(p=>p.canvas.id===id&&p.method==='arc');
  }
  function resetPaint() {paints.length=0;uploads.length=0;events.length=0;}
  async function hover(index=0,pin=false) {
    const point=arcs()[index];check(point,'Rendered point available for pointer test');
    const canvas=point.canvas,rect=canvas.getBoundingClientRect();
    const options={clientX:rect.left+point.args[0],clientY:rect.top+point.args[1],bubbles:true,buttons:0};
    canvas.dispatchEvent(new MouseEvent('mousemove',options));await settle();
    if(pin){canvas.dispatchEvent(new MouseEvent('mousedown',options));
      window.dispatchEvent(new MouseEvent('mouseup',options));await settle();}
    return document.getElementById('cv-tip-a').innerHTML;
  }
  function linked(extra={}) {
    visible(null);
    const bundle={dataset_id:context.dataset_key,dataset_context:{...context},dataset_fingerprint:'fp-'+context.dataset_key,
      n:3,cells:['a','b','c'],groups:{cluster:{levels:['A','B'],values:[0,1,0],colors:['#ff0000','#0000ff']}},
      cat_extra:{},fields:{},projections:{UMAP:{x:[0,10,20],y:[0,10,0]}},
      spaces:[{id:'expression',x:[0,10,20],y:[0,10,0]}],...extra};
    send('coordviews_data',bundle);return bundle;
  }
  function change(id,value) {const el=document.getElementById(id);check(el,'Control '+id);
    if(el.selectize){el.selectize.setValue(value);return el;}
    if(typeof value==='boolean')el.checked=value;else el.value=value;
    el.dispatchEvent(new Event('change',{bubbles:true}));return el;}
  // A selectized picker's choices, in order (its <select> keeps only the chosen).
  function choices(id) {const s=document.getElementById(id).selectize;
    return Object.values(s.options).sort((a,b)=>a.$order-b.$order);}
  return {handlers,requests,paints,uploads,events,context,check,equal,settle,send,sendWire,wire,dataset,
    visible,message,render,recolor,arcs,resetPaint,hover,linked,change,choices,get checks(){return checks;}};
})();
