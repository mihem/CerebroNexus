window.viewerScenarios = {
  async colors() {
    const t=viewerTest;t.dataset();const exact=1+2**-40;
    for(const sparse of [false,true]) {
      const packet=sparse?{protocol:'sparse-f64-v1',length:4,index:[0,1,3],color:new Float64Array([exact,NaN,-0])}:
        {protocol:'dense-f64-v1',length:4,color:new Float64Array([exact,NaN,0,-0])};
      const frame=await t.render({n:4,selection_key:['a','b','c','d'],x:[0,1,2,3],y:[0,1,0,1],
        sparse_color:packet,color_cache_key:sparse?'sparse':'dense'});
      t.check(frame.data.color instanceof Float64Array,'Hydrated f64 output');
      t.equal(frame.data.color[0],exact,'No float32 rounding');t.check(Number.isNaN(frame.data.color[1]),'Missing stays missing');
      t.equal(frame.data.color[2],0);t.check(Object.is(frame.data.color[3],-0),'Signed zero survives');
      const reused=await t.render({...frame.data,sparse_color:{...packet,color:[99,99,99,99]}});
      t.check(reused.data.color===frame.data.color,'Same key reuses hydrated bytes');
    }
    const packet=value=>({protocol:'dense-f64-v1',length:3,color:[value,value,value]});
    for(const key of ['a','b','c','d'])await t.render({sparse_color:packet(1),color_cache_key:key});
    const kept=await t.render({sparse_color:packet(99),color_cache_key:'a'});t.equal(kept.data.color[0],1);
    await t.render({sparse_color:packet(2),color_cache_key:'e'});
    const evicted=await t.render({sparse_color:packet(3),color_cache_key:'b'});t.equal(evicted.data.color[0],3,'Least-recent key evicted');
    return true;
  },
  async incremental() {
    const t=viewerTest;t.dataset();await t.render({selection_key:['d','b','a'],color:[1,2,3]});
    cerebroCellViews.applyState('expression_projection',{selection:{cells:['b'],geometry:{polygon:[[0,0],[1,0],[1,1]]}},
      view:{mode:'box',viewport:{cx:.4,cy:.6,span:.8}}});await t.settle();
    const canvas=document.getElementById('cv-cv-a'),before=cerebroCellViews.captureState('expression_projection');
    t.resetPaint();await t.recolor({color:new Float64Array([9,NaN,-2]),paint_order:'highest'},{color_variable:'NEW'});
    t.equal(cerebroCellViews.captureState('expression_projection'),before,'Recolor preserves selection, lasso and viewport');
    t.check(document.getElementById('cv-cv-a')===canvas,'Recolor keeps canvas');
    t.check(t.arcs().length>0,'New values repaint');
    t.check(document.getElementById('cv-cbar').textContent.includes('NEW'),'Legend updates');
    await t.recolor({rgb:{r:[255,0,0],g:[0,255,0],b:[0,0,255]},rgb_genes:{r:'A',g:'B',b:'C'}},
      {color_type:'rgb',color_variable:'RGB'});
    t.equal(cerebroCellViews.captureState('expression_projection').cells,['b']);
    t.check(document.getElementById('cv-legend').textContent.includes('A'),'RGB legend updates');
    await t.recolor({color:[1,2,3],colorscale:['#123456','#abcdef'],paint_order:'natural'});
    t.equal(cerebroCellViews.captureState('expression_projection').cells,['b']);
    // A new geometry packet must take the full rendering path and update extents.
    await t.recolor({color:[3,2,1],x:[-5,0,5],y:[1,2,3]});
    t.equal(cerebroCellViews.summary('expression_projection').xExtent.min,-5);
    t.resetPaint();
    const a=t.message('expression_projection',{color:[1,2,3]}),b=t.message('expression_projection',{color:[3,2,1]});
    t.sendWire('cell_view_recolor_binary',a);t.sendWire('cell_view_recolor_binary',b);await t.settle();
    const paints=t.events.filter(e=>e.name==='cerebro:cell-view-ready'&&e.painted);
    t.check(paints.length>0&&paints.every(e=>e.renderToken===b.render_token),'Stale paint cannot announce readiness');
    return true;
  },
  async genePanels() {
    const t = viewerTest, errors = [];
    const onError = event => errors.push(event.message || String(event.reason));
    window.addEventListener('error', onError);
    window.addEventListener('unhandledrejection', onError);
    t.dataset();
    try {
      for (const packed of [false, true]) {
        // Every transition receives exactly one frame: a later update must
        // not be needed to recover from a failed first paint.
        for (const genes of [[], ['A', 'B'], ['B', 'C', 'D'], ['C'], []]) {
          const values = Object.fromEntries(genes.map((gene, i) =>
            [gene, [i, i + 1, i + 2]]));
          const data = genes.length ? packed ? {
            color: null,
            packed_colors: Object.fromEntries(Object.entries(values).map(([gene, color]) =>
              [gene, {protocol: 'dense-f64-v1', length: 3, color}]))
          } : {color: values} : {color: [0, 1, 2]};
          t.resetPaint();
          const frame = await t.render(data, {space_label: 'UMAP'},
            'expression_projection');
          const panes = Array.from(document.querySelectorAll(
            '#expression_projection_cell_view_host .cv-pane:not(.cv-hidden)'));
          t.equal(panes.map(p => p.querySelector('.cv-ptitle').textContent),
            genes.length ? genes : ['GENE'], 'Visible gene panels');
          for (const pane of panes) {
            const canvas = pane.querySelector('.cv-canvas-wrap > canvas:not(.cv-mini)');
            t.check(t.arcs(canvas.id).length >= 3, 'Each panel paints its cells');
          }
          t.check(t.events.some(e => e.name === 'cerebro:cell-view-ready' &&
            e.painted && e.renderToken === frame.render_token), 'First frame completes');
          t.equal(errors, [], 'No errors while changing panel count');
        }
      }
    } finally {
      window.removeEventListener('error', onError);
      window.removeEventListener('unhandledrejection', onError);
    }
    return true;
  },
  async rgbLayouts() {
    const t = viewerTest; t.dataset();
    const channels = ['r', 'g', 'b'];
    const values = {r: [0, 2, 4], g: [0, 10, 20], b: [0, 2, 4]};
    const genes = {r: 'A', g: 'B', b: 'A'};
    const pixels = id => t.arcs(id).slice(-3).map(p => p.fill);
    const reference = {};
    for (const channel of channels) {
      t.resetPaint();
      await t.render({rgb: {[channel]: values[channel]}, rgb_genes: {[channel]: genes[channel]}},
        {color_type: 'rgb', rgb_layout: 'composite'});
      reference[channel] = pixels('cv-cv-a');
    }
    const packed_rgb = Object.fromEntries(channels.map(channel => [channel,
      {protocol: 'dense-f64-v1', length: 3, color: values[channel]}]));
    for (const layout of ['composite', 'separate', 'composite', 'separate']) {
      t.resetPaint();
      const frame = await t.render({packed_rgb, rgb_genes: genes},
        {color_type: 'rgb', rgb_layout: layout}, 'expression_projection');
      const panes = Array.from(document.querySelectorAll(
        '#expression_projection_cell_view_host .cv-pane:not(.cv-hidden)'));
      t.equal(panes.length, layout === 'separate' ? 3 : 1);
      t.check(t.events.some(e => e.painted && e.renderToken === frame.render_token), 'Layout paint completes');
      if (layout === 'separate') {
        // A channel's panel is titled by its gene and marked by the channel's dot.
        t.equal(panes.map(p => p.querySelector('.cv-ptitle').textContent), ['A', 'B', 'A']);
        t.equal(panes.map(p => !!p.querySelector('.cv-ptitle .cv-title-dot')), [true, true, true]);
        channels.forEach((channel, i) => t.equal(pixels('cv-cv-' + String.fromCharCode(97 + i)),
          reference[channel], 'Split channel uses composite intensities and colours'));
        t.check(document.getElementById('cv-legend').textContent.includes('B 0–20.0'),
          'Separate channels publish their own numeric ranges');
      }
    }
    await t.render({packed_rgb, rgb_genes: {r: 'A', g: '', b: 'A'}},
      {color_type: 'rgb', rgb_layout: 'separate'}, 'expression_projection');
    t.equal(Array.from(document.querySelectorAll(
      '#expression_projection_cell_view_host .cv-pane:not(.cv-hidden) .cv-ptitle')).map(p => p.textContent),
      ['A', 'A'], 'Empty channels omitted, repeated genes retain channel identity');
    return true;
  },
  async geometry() {
    const t=viewerTest;t.dataset();
    for(const [width,height] of [[620,220],[220,620]]) {
      const pane=document.querySelector('.cv-pane');pane.style.width=width+'px';pane.style.height=height+'px';
      t.resetPaint();await t.render({x:[0,20,10],y:[0,10,5],point_sizes:[10,6,8]},
        {is_spatial:true},'spatial_projection');
      const points=t.arcs().slice(-3);t.equal(points.length,3);
      const sorted=points.slice().sort((a,b)=>a.args[0]-b.args[0]);
      t.check(Math.abs((sorted[2].args[0]-sorted[0].args[0])/20-
        Math.abs(sorted[2].args[1]-sorted[0].args[1])/10)<1e-6,'Spatial axes preserve equal physical scale');
      t.equal(points.map(p=>p.args[2]).sort((a,b)=>a-b),[3,4,5],'Per-cell radii reach Canvas');
    }
    for(const edges of [[[0,0,20,10]],{x0:[0],y0:[0],x1:[20],y1:[10]},{from:[1,0,9],to:[2,2,1]}]) {
      const extra=Array.isArray(edges)?{shapes:edges.map(([x0,y0,x1,y1])=>({x0,y0,x1,y1}))}:{edges};
      t.resetPaint();await t.render({x:[0,20,10],y:[0,10,5]}, {},'trajectory_projection',{extra});
      t.check(t.paints.some(p=>p.method==='lineTo'),'Trajectory reaches drawing context');
    }
    return true;
  },
  async missing() {
    const t=viewerTest;t.dataset();
    for(const grouped of [false,true]) {
      const data=grouped?{selection_key:[['a','b'],['c']],x:[[0,null],[3]],y:[[2,4],[null]],color:['#ff0000','#0000ff']}:
        {selection_key:['a','b','c'],x:new Float64Array([0,NaN,3]),y:new Float64Array([2,4,NaN])};
      t.resetPaint();await t.render(data,grouped?{color_type:'categorical',traces:['A','B']}: {},'spatial_projection');
      const points=t.arcs();t.check(points.length>0,'Genuine zero remains a point');
      const positions=new Set(points.map(p=>p.args.slice(0,2).join(',')));
      t.equal(positions.size,1,'Missing coordinates never become selectable points at zero');
    }
    return true;
  },
  async panels() {
    const t=viewerTest;t.dataset();
    t.resetPaint();await t.render({color:new Float32Array([4,0,2]),selection_key:['b','a','c'],
      colorscale:[[0,'#aeb5bb'],[1,'#9f251f']],panels:[
        {id:'one',label:'R1',selection_key:['b','c'],x:[1,3],y:[4,6]},
        {id:'two',label:'R2',selection_key:['a'],x:[2],y:[5]}]}, {},'spatial_projection');
    t.equal(new Set(t.arcs('cv-cv-a').map(p=>p.args.slice(0,2).join(','))).size,2);
    t.equal(new Set(t.arcs('cv-cv-b').map(p=>p.args.slice(0,2).join(','))).size,1);
    t.check(document.getElementById('cv-title-a').textContent.includes('R1'));
    t.check(document.getElementById('cv-title-b').textContent.includes('R2'));
    t.check(document.getElementById('cv-cbar').textContent.includes('4'),'Exact maximum in legend');
    return true;
  },
  async hover() {
    const t=viewerTest;t.dataset();
    const columns=[{label:'sample',values:[0,0,0],levels:['one sample']},
      {label:'count',values:[1630,0,0],format:'integer'},
      {label:'score',values:[.901,0,0],format:'fixed',digits:2},
      {label:'missing',values:[null,null,null],levels:['A']}];
    const initial=await t.render({wire_token:5}, {},'trajectory_projection',{hover:{columns}});
    const tip=await t.hover();
    for(const text of ['one sample','1,630','0.90','missing: NA'])t.check(tip.includes(text),'Rendered tooltip '+text+': '+tip);
    t.send('cell_view_aux_binary',t.wire({id:initial.id,dataset_context:{...t.context},render_token:initial.render_token,
      wire_token:5,selection_key:['a','b','c'],hover:{columns:[{...columns[0],levels:'one sample'}]}}));
    await t.settle();t.check((await t.hover()).includes('one sample'),'Auto-unboxed auxiliary dictionary');
    t.resetPaint();await t.render({n:3,selection_key:null,wire_token:7}, {},'expression_projection',{hover:{pending:true,hoverinfo:'skip'}});
    const count=()=>t.requests.filter(r=>r.name==='cell_view_aux_request'&&!r.value.stage).length;
    const metadata=()=>t.requests.filter(r=>r.name==='cell_view_aux_request'&&r.value.id==='expression_projection'&&r.value.stage==='metadata').length;
    // Asked once: while idle after the first paint, or at the first real point.
    const before=count();await t.hover();await t.hover();
    t.check(count()>=1&&count()<=before+1,'Pending IDs requested once');
    t.equal(metadata(),0,'Disabled hover can request IDs but not hover metadata');
    const host=document.getElementById('cv-cv-a');host.dispatchEvent(new MouseEvent('mousemove',{clientX:-100,clientY:-100,bubbles:true}));
    const asked=count();await t.settle();t.equal(count(),asked,'Empty canvas does not request metadata');
    for(const id of ['expression_projection','trajectory_projection']) {
      t.resetPaint();await t.render({n:3,selection_key:null,wire_token:80},{},id,{hover:{pending:true,hoverinfo:'skip'}});
      const point=t.arcs()[0],rect=point.canvas.getBoundingClientRect(),frames=[],raf=window.requestAnimationFrame;
      window.requestAnimationFrame=fn=>{frames.push(fn);return frames.length;};
      point.canvas.dispatchEvent(new MouseEvent('mousemove',{bubbles:true,clientX:rect.left+point.args[0],clientY:rect.top+point.args[1]}));
      // The view may ask for its IDs while idle; none may be asked after it retires.
      const old=()=>t.requests.filter(r=>r.name==='cell_view_aux_request'&&r.value.id===id&&r.value.wire_token===80).length;
      const sent=old();
      t.dataset(id,t.context.generation+1);
      window.requestAnimationFrame=raf;while(frames.length)frames.shift()();await t.settle();
      t.equal(old(),sent,'Retired hover callback cannot request old IDs');
    }
    return true;
  },
  async linked() {
    const t=viewerTest;t.dataset();t.linked({groups:{cluster:{levels:['A','B'],values:[0,1,0],colors:['#ff0000','#0000ff']},
      sample:{levels:['sample-one'],values:null,deferred:true},clone_expansion:{levels:['singleton','expanded'],values:[1,0,1]}}});await t.settle();
    const ordinary=await t.hover();t.check(ordinary.includes('<b>A</b>'));t.check(!ordinary.includes('cv-tip-details'));
    const pinned=await t.hover(0,true);t.check(pinned.includes('expanded'));t.check(!pinned.includes('sample-one'));
    const config=cerebroLinkedViewsState.capture();config.selection={cells:['b'],indices:[1]};
    cerebroLinkedViewsState.apply(config);await t.settle();
    t.change('cv-hover',false);await t.settle();
    t.equal(cerebroLinkedViewsState.summary().selectedCellBarcodes,['b'],'Disabling hover preserves selection');
    t.change('cv-hover',true);await t.settle();
    const noSelection=cerebroLinkedViewsState.capture();noSelection.selection={cells:[],indices:[]};
    t.equal(cerebroLinkedViewsState.apply(noSelection).selectedCells,0);
    t.dataset('B',2);t.resetPaint();t.linked({groups:{cluster:{levels:['NEW'],values:[0,0,0],colors:['#00ff00']}}});await t.settle();
    t.check((await t.hover()).includes('NEW'),'New dataset retires old tooltip labels');
    return true;
  },
  async auxCellNames() {
    const t=viewerTest;t.dataset();
    const id='overview_projection';
    const identity={cell_count:3,cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),
      cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};
    const resource={protocol:'canonical-cell-names-v1',cells:3,bytes:12,
      url:URL.createObjectURL(new Blob(['c-0\0c-1\0c-2\0'])),
      dataset_fingerprint:identity.cell_fingerprint,cell_order_fingerprint:identity.cell_order_fingerprint,
      pack_dataset_fingerprint:identity.pack_dataset_fingerprint};
    const deliver=async (token,index,extra={})=>{
      const frame=await t.render({n:3,selection_key:null,wire_token:token},{},id,{dataset_identity:identity});
      t.send('cell_view_aux_binary',t.wire({id,dataset_context:{...t.context},render_token:frame.render_token,
        wire_token:token,selection_index:index,cell_names_resource:{...resource,...extra},hover:{hoverinfo:'skip'}}));
      for(let i=0;i<5;i++)await t.settle();
    };
    await deliver(31,new Int32Array([2,0,1]));
    t.check((await t.hover(0)).includes('Cell: c-2'),'Point 0 is the third canonical cell');
    t.check((await t.hover(2)).includes('Cell: c-1'),'Point 2 is the second canonical cell');
    const gzip=await new Response(new Blob(['g-0\0g-1\0g-2\0']).stream()
      .pipeThrough(new CompressionStream('gzip'))).blob();
    await deliver(33,new Int32Array([1,2,0]),{url:URL.createObjectURL(gzip),encoding:'gzip'});
    t.check((await t.hover(0)).includes('Cell: g-1'),'A gzip resource is decompressed');
    const every=await t.render({n:3,selection_key:null,wire_token:34},{},id,{dataset_identity:identity});
    t.send('cell_view_aux_binary',t.wire({id,dataset_context:{...t.context},render_token:every.render_token,
      wire_token:34,selection_index_all:true,cell_names_resource:{...resource},hover:{hoverinfo:'skip'}}));
    for(let i=0;i<5;i++)await t.settle();
    t.check((await t.hover(0)).includes('Cell: c-0'),'Every cell in order needs no indices');
    t.check((await t.hover(2)).includes('Cell: c-2'),'Every cell in order needs no indices');
    const failures=()=>t.requests.filter(r=>r.name==='cell_view_names_resource_failed').length;
    t.equal(failures(),0);
    await deliver(32,new Int32Array([0,1,2]),{cell_order_fingerprint:'md5-cell-order-v1:'+'d'.repeat(32)});
    t.equal(failures(),1,'A resource of another cell order is refused and reported');
    t.check(t.requests.some(r=>r.name===id+'_render_request'),'The view asks for a frame with names');
    // A frame naming its cells by row: a refused resource is reported and the
    // view asks for a frame with names, not left blank.
    const renders=()=>t.requests.filter(r=>r.name===id+'_render_request').length;
    const reported=failures(),asked=renders();
    await t.render({n:3,selection_key:null,selection_index:[0,1,2],
      cell_names_resource:{...resource,cell_order_fingerprint:'md5-cell-order-v1:'+'e'.repeat(32)}},
      {},id,{dataset_identity:identity});
    for(let i=0;i<5;i++)await t.settle();
    t.equal(failures(),reported+1,'A frame resource that cannot be used is reported');
    t.equal(renders(),asked+1,'A frame without names asks for one with names');
    // Another data set does not keep the decoded names: the same URL is read
    // again, and here it no longer exists.
    URL.revokeObjectURL(resource.url);
    t.dataset('B',2);await t.settle();
    const beforeSwitch=failures();
    await deliver(35,new Int32Array([0,1,2]));
    t.equal(failures(),beforeSwitch+1,'Cell names are read again for another data set');
    return true;
  },
  async auxAcrossLinked() {
    const t=viewerTest;t.dataset();
    const id='overview_projection';
    const asked=()=>t.requests.filter(r=>r.name==='cell_view_aux_request'&&r.value.id===id).length;
    const frame=await t.render({n:3,selection_key:null,wire_token:21},{},id);
    t.equal(asked(),1,'A progressive frame asks for its cell names');
    t.send('cell_view_aux_binary',t.wire({id,dataset_context:{...t.context},render_token:frame.render_token,
      wire_token:21,selection_key:['a','b','c'],hover:{hoverinfo:'skip'}}));await t.settle();
    t.linked();await t.settle();
    await t.render({n:3,selection_key:null,wire_token:22,aux_reuse:21},{},id);
    t.equal(asked(),1,'The first Linked views bundle keeps the delivered cell names');
    t.dataset('B',2);t.linked();await t.settle();
    await t.render({n:3,selection_key:null,wire_token:23,aux_reuse:21},{},id);
    t.equal(asked(),2,'Another data set downloads its own cell names');
    return true;
  },
  async backgrounds() {
    const t=viewerTest;t.dataset();
    const image=document.createElement('canvas');image.width=2;image.height=2;
    image.getContext('2d').fillRect(0,0,2,2);const uri=image.toDataURL();
    const space=()=>({id:'tissue',background_scope:'Trekker',x:[0,10,20],y:[0,10,0],images:[
      {id:'one',label:'First',asset_key:'background',bounds:{xmin:0,xmax:20,ymin:0,ymax:10},preset:{scaleX:2,scaleY:3,rotation:32,opacity:.4}},
      {id:'two',label:'Second',uri,bounds:{xmin:0,xmax:20,ymin:0,ymax:10}}]});
    const sendAsset=(context,uri)=>t.send('coordviews_asset',{dataset_context:context,
      dataset_fingerprint:'fp-'+context.dataset_key,key:'background',uri});
    const requests=()=>t.requests.filter(r=>r.name==='coordviews_asset_request');
    t.linked({spaces:[space()]});await t.settle();
    t.equal(requests().length,1,'One deferred request');
    const picker=document.getElementById('cv-bg-image-select');
    const none=t.choices('cv-bg-image-select').find(o=>o.text==='None').value;
    t.change('cv-bg-image-select',none);t.change('cv-bg-image-select','one');await t.settle();
    t.equal(requests().length,1,'Pending request deduplicated');
    const a={...t.context};sendAsset(a,uri);await t.settle();
    t.equal(t.choices('cv-bg-image-select').map(o=>o.text),['First','Second','None']);
    t.equal(picker.value,'one','Background choice retained after download');
    t.resetPaint();t.change('cv-bg-image-select',none);await t.settle();
    t.check(!t.paints.some(p=>p.method==='drawImage'&&p.args[0] instanceof HTMLImageElement&&!p.canvas.hasAttribute('data-cv-probe')),'None removes background');
    const drawn=canvas=>t.paints.findLast(p=>p.method==='drawImage'&&p.args[0] instanceof HTMLImageElement&&!p.canvas.hasAttribute('data-cv-probe')&&(!canvas||p.canvas===canvas));
    t.change('cv-bg-image-select','two');await t.settle();
    const plain=drawn();
    t.change('cv-bg-image-select','one');await t.settle();
    t.equal(requests().length,1,'Loaded image reused');
    t.check(plain&&drawn(plain.canvas)!==plain,'Decoded image is drawn');
    // The unit square drawImage() fills, read back in data units through the
    // unadjusted 20 x 10 image: scaled 2 x 3, then turned by 32 degrees. The
    // canvas keeps its matrix in single precision.
    const k=plain.matrix,m=drawn(plain.canvas).matrix,kx=k.a/20,ky=k.d/10;
    const u=[m.a/kx,-m.b/ky],v=[m.c/kx,-m.d/ky],near=(a,b)=>Math.abs(a-b)<1e-3;
    t.check(near(Math.hypot(...u),40)&&near(Math.hypot(...v),30)&&near(u[0]*v[0]+u[1]*v[1],0)&&
      near(Math.abs(Math.atan2(u[1],u[0]))*180/Math.PI,32),'Nonuniform preset scale survives image switching');
    for(const [key,generation] of [['B',2],['A',3]]) {
      t.dataset(key,generation);t.resetPaint();t.linked({spaces:[space()]});await t.settle();
      t.equal(requests().length,generation,'New generation requests its own image');
      sendAsset(a,uri);await t.settle();
      t.check(!t.paints.some(p=>p.method==='drawImage'&&p.args[0] instanceof HTMLImageElement&&!p.canvas.hasAttribute('data-cv-probe')),'Retired image cannot paint');
      sendAsset({...t.context},uri);await t.settle();
      t.check(t.paints.some(p=>p.method==='drawImage'&&p.args[0] instanceof HTMLImageElement&&!p.canvas.hasAttribute('data-cv-probe')),'Fresh image paints');
    }
    const config=cerebroLinkedViewsState.capture();
    const rgbMode=t.choices('cv-pick-color').find(o=>o.text.includes('RGB')).value;
    config.view.colour.mode=rgbMode;
    cerebroLinkedViewsState.apply(config,{mode:rgbMode,r:[255,0,0],g:[0,255,0],b:[0,0,255],genes:['CD3D','MS4A1','LYZ']});
    const legend=document.getElementById('cv-legend').textContent;
    for(const text of ['CD3D','MS4A1','LYZ','Low expression','R + G','R + B','G + B','R + G + B','scaled independently'])t.check(legend.includes(text),text);
    return true;
  },
  async backgroundChannel() {
    const t=viewerTest;t.dataset();const image=document.createElement('canvas');image.width=2;image.height=2;
    const identity={image:'slice'},id='spatial_projection';
    await t.render({}, {is_spatial:true,background_image:image.toDataURL(),image_bounds:{xmin:0,xmax:20,ymin:0,ymax:10},
      background_identity:identity,background_opacity:.4},id,{background_identity:identity});
    const draw=()=>t.paints.findLast(p=>p.method==='drawImage'&&p.args[0] instanceof HTMLImageElement&&!p.canvas.hasAttribute('data-cv-probe'));
    t.check(draw(),'Background decoded and painted');t.equal(draw().alpha,.4);
    for(const patch of [{dataset_context:{...t.context,generation:0}},{background_identity:{image:'other'}}]) {
      t.send('cell_view_background',{id,dataset_context:{...t.context},background_identity:identity,values:{opacity:.1},...patch});
      await t.settle();t.equal(draw().alpha,.4,'Stale background adjustment ignored');
    }
    t.send('cell_view_background',{id,dataset_context:{...t.context},background_identity:identity,values:{opacity:.7}});
    await t.settle();t.equal(draw().alpha,.7,'Matching background adjustment paints');
    t.dataset('B',2);t.check(!cerebroCellViews.summary(id).ready,'Dataset switch invalidates specialist');
    return true;
  },
  async supplement() {
    const t=viewerTest;t.dataset();
    t.linked({progressive:true,progressive_token:4});await t.settle();
    const config=cerebroLinkedViewsState.capture();config.selection={cells:['b'],indices:[1]};cerebroLinkedViewsState.apply(config);
    await t.settle();t.resetPaint();
    const extra={dataset_id:'A',dataset_context:{...t.context},dataset_fingerprint:'fp-A',progressive_token:4,
      spaces:[{id:'other',label:'Other',geometry_ref:{projection:'UMAP',exclude:[1]}}],
      groups:{sample:{levels:['sample-one'],values:[0,0,0],colors:['#111111']}}};
    t.send('coordviews_supplement',t.wire(extra));await t.settle();
    const summary=cerebroLinkedViewsState.summary();t.check(summary.complete,'Supplement completes workspace');
    t.equal(summary.selectedCellBarcodes,['b'],'Selection restored before final paint');
    t.check(t.events.filter(e=>e.name==='cerebro:linkedviews-ready'&&e.painted).length>0,'Ready after painted supplement');
    t.equal(new Set(t.arcs('cv-cv-b').map(p=>p.args.slice(0,2).join(','))).size,2,'Excluded geometry is not drawn');
    const prior=JSON.stringify(cerebroLinkedViewsState.summary());
    t.send('coordviews_supplement',t.wire({...extra,dataset_context:{...t.context,generation:0}}));await t.settle();
    t.equal(JSON.stringify(cerebroLinkedViewsState.summary()),prior,'Stale supplement cannot alter workspace');
    const failures=()=>t.requests.filter(r=>r.name==='coordviews_wire_fallback').length;
    const before=failures();
    t.send('coordviews_supplement',t.wire({...extra,spaces:[{id:'bad',geometry_ref:{projection:'UMAP',exclude:[1,1]}}]}));
    await t.settle();t.check(failures()>before,'Malformed geometry requests fallback');
    t.change('cv-borders',true);await t.settle();t.check(t.arcs().length>0,'Drawing remains usable after rejection');
    return true;
  },
  async spatialScenes() {
    const t=viewerTest;t.dataset();
    t.linked({progressive:true,progressive_token:5});await t.settle();t.resetPaint();
    // A compact scene: canonical rows of its own cells with their x/y only.
    const extra={dataset_id:'A',dataset_context:{...t.context},dataset_fingerprint:'fp-A',progressive_token:5,
      spaces:[{id:'spatial',label:'S1 (spatial)',name:'S1',is_roi_overview:false,
        rows:new Int32Array([2,0]),x:new Float32Array([50,10]),y:new Float32Array([60,20])}]};
    t.send('coordviews_supplement',t.wire(extra));await t.settle();
    t.check(cerebroLinkedViewsState.summary().complete,'Compact spatial supplement completes workspace');
    const canvases=[...new Set(t.paints.filter(p=>p.method==='arc').map(p=>p.canvas.id))];
    const id=canvases.find(c=>t.arcs(c).length===2&&document.getElementById(c).closest('.cv-pane')
      .textContent.includes('S1 (spatial)'));
    t.check(id,'The spatial panel draws the two cells of its scene');
    const points=t.arcs(id).slice().sort((a,b)=>b.args[0]-a.args[0]);
    const canvas=document.getElementById(id),rect=canvas.getBoundingClientRect();
    canvas.dispatchEvent(new MouseEvent('mousemove',{clientX:rect.left+points[0].args[0],
      clientY:rect.top+points[0].args[1],bubbles:true,buttons:0}));await t.settle();
    const tip=document.getElementById(id.replace('cv-cv-','cv-tip-')).innerHTML;
    t.check(tip.includes('Cell:</span> c<'),'The rightmost spatial point is the third cell');
    return true;
  },
  async spatialGpu() {
    const t=viewerTest;t.dataset();const n=20000;
    const x=Float32Array.from({length:n},(_,i)=>i/n),y=Float32Array.from(x,v=>1-v);
    t.linked({n,cells:Array.from({length:n},(_,i)=>'c'+i),
      groups:{cluster:{levels:['A','B'],values:Array.from({length:n},(_,i)=>i%2),colors:['#ff0000','#0000ff']}},
      projections:{UMAP:{x,y}},spaces:[{id:'expression',x,y}],progressive:true,progressive_token:6});
    await t.settle();t.resetPaint();
    const rows=Int32Array.from({length:10},(_,i)=>100+i*3);
    t.send('coordviews_supplement',t.wire({dataset_id:'A',dataset_context:{...t.context},dataset_fingerprint:'fp-A',
      progressive_token:6,spaces:[{id:'spatial',label:'S1 (spatial)',name:'S1',is_roi_overview:false,rows,
        x:Float32Array.from(rows,(_,i)=>i),y:Float32Array.from(rows,(_,i)=>2*i)}]}));
    await t.settle();
    const scene=t.uploads.filter(u=>u.count===10).at(-1);
    t.check(scene,'The spatial panel uploads the ten cells of its scene');
    t.equal(scene.positionSource.length,20,'Its buffers are sized to the scene');
    // Clearing a gene nobody colours by must not rebuild any panel.
    const last=()=>new Map(t.uploads.map(u=>[u.canvas,u.positionSource]));
    const before=last();
    t.send('coordviews_geneval',{ok:false,gene:'',dataset_context:{...t.context}});
    t.change('cv-borders',true);await t.settle();
    const after=last();
    t.check(after.size>=2,'Both panels were drawn again');
    for(const [canvas,positions] of after) t.check(before.get(canvas)===positions,'An unused gene keeps the GPU data');
    return true;
  },
  async gpuColour() {
    const t=viewerTest;t.dataset();const n=20000;
    const x=Float32Array.from({length:n},(_,i)=>i/n),y=Float32Array.from(x,v=>1-v);x[7]=NaN;
    const first=Array.from({length:n},(_,i)=>i%3),second=Array.from({length:n},(_,i)=>(i*7)%5);
    const shades=['#111111','#222222','#333333','#444444','#555555'];
    t.linked({n,cells:Array.from({length:n},(_,i)=>'c'+i),
      groups:{first:{levels:['A','B','C'],values:first,colors:['#ff0000','#00ff00','#0000ff']},
        second:{levels:['p','q','r','s','u'],values:second,colors:shades}},
      projections:{UMAP:{x,y}},spaces:[{id:'expression',x,y}]});
    await t.settle();
    const before=t.uploads.at(-1);t.check(before&&before.count===n-1,'Initial GPU upload');
    t.change('cv-pick-color','second');await t.settle();
    const after=t.uploads.filter(u=>u.canvas===before.canvas).at(-1);
    t.check(after.positionSource===before.positionSource,'A colour change keeps the GPU geometry');
    const expected=[];
    const alpha=before.colors[3];
    for(let i=0;i<n;i++){if(i===7)continue;const v=parseInt(shades[second[i]].slice(1,3),16);expected.push(v,v,v,alpha);}
    t.equal(after.colors,expected,'Colours of the kept geometry follow the new grouping');
    return true;
  },
  async outlinePatch() {
    const t=viewerTest;t.dataset();const id='spatial_projection';
    await t.render({},{is_spatial:true},id);
    t.check(!t.paints.some(p=>p.method==='fill'&&p.fill==='#654321'),'No outlines before the patch');
    const patch=hulls=>t.send('cell_view_extra',{id,dataset_context:{...t.context},
      dataset_fingerprint:'fp-'+t.context.dataset_key,extra:{group_hulls:hulls}});
    t.resetPaint();patch({x:[[0,10,0]],y:[[0,0,10]],color:['#654321']});await t.settle();
    t.check(t.paints.some(p=>p.method==='fill'&&p.fill==='#654321'),'Patched outlines are drawn');
    t.check(t.arcs('cv-cv-a').length===3,'The cells are still drawn');
    t.resetPaint();patch([]);await t.settle();
    t.check(!t.paints.some(p=>p.method==='fill'&&p.fill==='#654321'),'Removed outlines are not drawn');
    return true;
  },
  async zoomOnePanel() {
    const t=viewerTest;t.dataset();
    t.linked({projections:{UMAP:{x:[0,10,20],y:[0,10,0]},TSNE:{x:[5,1,9],y:[2,8,4]}},
      initial_projections:['UMAP','TSNE']});
    // Let the switch from the previous view finish its layout passes first.
    for(let i=0;i<4;i++)await t.settle();
    t.check(document.querySelector('[data-panel="B"][data-act="zin"]'),'Two panels are shown');
    t.resetPaint();
    document.querySelector('[data-panel="A"][data-act="zin"]').click();await t.settle();
    t.check(t.arcs('cv-cv-a').length>0,'The zoomed panel redraws');
    t.equal(t.paints.filter(p=>p.canvas.id==='cv-cv-b').length,0,'The other panel is not redrawn');
    t.resetPaint();
    document.querySelector('[data-panel="A"][data-act="reset"]').click();await t.settle();
    t.equal(t.paints.filter(p=>p.canvas.id==='cv-cv-b').length,0,'Resetting one view leaves the other');
    return true;
  },
  async geneUnavailable() {
    const t=viewerTest;t.dataset();t.linked({});
    for(let i=0;i<3;i++)await t.settle();
    const draws=()=>t.paints.filter(p=>p.canvas.id==='cv-cv-a'&&p.method==='clearRect').length;
    const reply=(extra)=>t.send('coordviews_geneval',{dataset_context:{...t.context},...extra});
    t.change('cv-pick-color','__gene__');for(let i=0;i<2;i++)await t.settle();
    const canvas=document.getElementById('cv-cv-a'),size=()=>canvas.width+'x'+canvas.height,before=size();
    t.resetPaint();reply({gene:'',ok:false});await t.settle();
    // No gene before and none now: only the colour bar's room can call for a draw.
    t.equal(draws(),size()===before?0:1,'Drawn only for a new panel size, and once');
    reply({gene:'G',ok:true,v:[0,128,255],max:3});await t.settle();
    t.resetPaint();reply({gene:'G',ok:false});await t.settle();
    t.check(draws()>0,'A gene that is no longer available is drawn away');
    return true;
  },
  async deferredFilter() {
    const t=viewerTest;t.dataset();
    t.linked({groups:{cluster:{levels:['A','B'],values:[0,1,0],colors:['#ff0000','#0000ff']},
      sample:{levels:['s1','s2'],values:null,deferred:true}}});
    for(let i=0;i<3;i++)await t.settle();
    const draws=()=>t.paints.filter(p=>p.canvas.id==='cv-cv-a'&&p.method==='clearRect').length;
    const asked=()=>t.requests.filter(r=>r.name==='coordviews_attribute_request'&&r.value.name==='sample');
    const boxes=document.querySelectorAll('.cv-filt[data-group="sample"] .cv-filt-menu input[type=checkbox]');
    t.check(boxes.length===2,'The deferred group has a filter');
    // What "None" does (settings_drawer.js): clear every level, one change event.
    t.resetPaint();boxes.forEach(b=>{b.checked=false;});
    boxes[0].dispatchEvent(new Event('change',{bubbles:true}));await t.settle();
    t.equal(asked().length,1,'The filter asks for the group values');
    t.equal(draws(),0,'Nothing is drawn before the values arrive');
    t.send('coordviews_attribute',t.wire({dataset_id:t.context.dataset_key,dataset_context:{...t.context},
      dataset_fingerprint:asked()[0].value.dataset_fingerprint,kind:'groups',name:'sample',
      value:{levels:['s1','s2'],values:[0,1,0]}}));
    await t.settle();
    t.equal(draws(),1,'The arriving values are drawn once');
    t.equal(t.arcs('cv-cv-a').length,0,'None hides every cell');
    return true;
  },
  async deferredResource() {
    const t=viewerTest;
    const identity={cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),
      cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};
    let downloads=0;
    window.fetch=async()=>{downloads++;return {ok:true,arrayBuffer:async()=>new Uint8Array([1,0,2]).buffer};};
    const colors=['#112233','#445566'];
    const resource={protocol:'canonical-metadata-codes-v1',url:'metadata/002.codes.bin',cells:3,dtype:'uint8',bytes:3,
      checksum:'',dataset_fingerprint:identity.cell_fingerprint,cell_order_fingerprint:identity.cell_order_fingerprint,
      pack_dataset_fingerprint:identity.pack_dataset_fingerprint,metadata_name:'sample',code_map:[-1,0,1]};
    const asked=()=>t.requests.filter(r=>r.name==='coordviews_attribute_request'&&r.value.name==='sample');
    const reply=value=>t.send('coordviews_attribute',t.wire({dataset_id:t.context.dataset_key,dataset_context:{...t.context},
      dataset_fingerprint:asked().at(-1).value.dataset_fingerprint,kind:'groups',name:'sample',value}));
    const drawn=()=>t.arcs('cv-cv-a').map(p=>p.fill+'@'+p.args.slice(0,2).join(',')).sort();
    const open=async (key,generation)=>{
      t.dataset(key,generation);
      t.linked({dataset_fingerprint:identity.cell_fingerprint,canonical_order_id:identity.cell_order_fingerprint,
        pack_dataset_fingerprint:identity.pack_dataset_fingerprint,
        groups:{cluster:{levels:['A','B'],values:[0,1,0],colors:['#ff0000','#0000ff']},
          sample:{levels:['s1','s2'],values:null,deferred:true}}});
      for(let i=0;i<3;i++)await t.settle();
      t.change('cv-pick-color','sample');await t.settle();
    };
    // The websocket values, R's NA unpacked as null.
    await open('A',1);const before=asked().length;
    t.check(before>0&&asked().at(-1).value.wire_only===false,'Colouring by a deferred group asks for it');
    t.resetPaint();reply({levels:['s1','s2'],colors,values:[0,null,1]});for(let i=0;i<3;i++)await t.settle();
    const wire=drawn();t.equal(wire.length,3,'Every cell is drawn from the websocket values');
    // The Viewer Pack codes (0 = missing) draw exactly the same cells.
    await open('B',2);t.equal(asked().length,before+1,'A new data set asks once');
    t.resetPaint();reply({levels:['s1','s2'],colors,values_resource:resource});for(let i=0;i<3;i++)await t.settle();
    t.equal(downloads,1,'The codes come from the Pack resource');
    t.equal(drawn(),wire,'Pack codes draw what the websocket values draw');
    t.equal(asked().length,before+1,'No websocket values are asked for');
    // A resource that fails its identity check falls back to the websocket.
    await open('C',3);const failed=asked().length;
    reply({levels:['s1','s2'],colors,values_resource:{...resource,pack_dataset_fingerprint:'wrong'}});
    for(let i=0;i<3;i++)await t.settle();
    t.equal(asked().length,failed+1,'A rejected resource asks again');
    t.equal(asked().at(-1).value.wire_only,true,'The second request asks for websocket values');
    return true;
  },
  async slicePanels() {
    const t=viewerTest;t.dataset();const id='trajectory_projection';
    const identity={cell_count:5,cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),
      cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};
    const resource={protocol:'canonical-cell-names-v1',cells:5,bytes:20,
      url:URL.createObjectURL(new Blob(['c-0\0c-1\0c-2\0c-3\0c-4\0'])),
      dataset_fingerprint:identity.cell_fingerprint,cell_order_fingerprint:identity.cell_order_fingerprint,
      pack_dataset_fingerprint:identity.pack_dataset_fingerprint};
    const range={x_range:[-1,11],y_range:[-1,11]};
    await t.render({selection_key:null,selection_index:new Int32Array([4,0,3,1,2]),cell_names_resource:resource,
      group_code:new Int8Array([0,1,0,1,1]),x:[0,10,0,5,10],y:[0,10,10,5,0],color:null,
      panels:[{id:'group1',label:'A',start:0,count:2,...range},{id:'group2',label:'B',start:2,count:3,...range}]},
      {color_type:'categorical',color_variable:'expansion',traces:['Other cells','Single'],
        group_colors:['#d9d9d9','#ff0000']},id,{dataset_identity:identity},true);
    for(let i=0;i<5;i++)await t.settle();
    t.equal(t.arcs('cv-cv-a').length,2,'The first panel draws its slice');
    t.equal(t.arcs('cv-cv-b').length,3,'The second panel draws its slice');
    const tips=[await t.hover(0),await t.hover(1)].join(' ');
    t.check(tips.includes('Cell: c-4')&&tips.includes('expansion: Other cells'),'Rows are named and coloured by code');
    t.check(tips.includes('Cell: c-0')&&tips.includes('expansion: Single'),'Rows are named and coloured by code');
    return true;
  },
  async keyedPanelsFromRowRange() {
    // A split Spatial frame names its cells by a range of canonical rows and
    // the cell-name resource; its panels name their cells by barcode.
    const t=viewerTest;t.dataset();const id='spatial_projection';
    const identity={cell_count:5,cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),
      cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};
    const resource={protocol:'canonical-cell-names-v1',cells:5,bytes:20,
      url:URL.createObjectURL(new Blob(['c-0\0c-1\0c-2\0c-3\0c-4\0'])),
      dataset_fingerprint:identity.cell_fingerprint,cell_order_fingerprint:identity.cell_order_fingerprint,
      pack_dataset_fingerprint:identity.pack_dataset_fingerprint};
    const range={x_range:[-1,11],y_range:[-1,11]};
    await t.render({selection_key:null,selection_index:{kind:'range',start:0,length:5},cell_names_resource:resource,
      group:['A','B','A','B','B'],color:null,
      panels:[{id:'split-1',label:'A',selection_key:['c-0','c-2'],x:[0,10],y:[0,10],spatial:true,...range},
        {id:'split-2',label:'B',selection_key:['c-1','c-3','c-4'],x:[10,5,0],y:[0,5,10],spatial:true,...range}]},
      {color_type:'categorical',color_variable:'cluster',traces:['A','B'],
        group_colors:['#00ff00','#ff0000']},id,{dataset_identity:identity},true);
    for(let i=0;i<5;i++)await t.settle();
    t.equal(t.arcs('cv-cv-a').length,2,'The first split panel draws its cells');
    t.equal(t.arcs('cv-cv-b').length,3,'The second split panel draws its cells');
    return true;
  },
  async appearanceUnchanged() {
    const t=viewerTest;t.dataset();const id='overview_projection';
    await t.render({},{},id);
    const patch=values=>t.send('cell_view_appearance',{id,dataset_context:{...t.context},
      dataset_fingerprint:'fp-'+t.context.dataset_key,values});
    const draws=()=>t.paints.filter(p=>p.canvas.id==='cv-cv-a'&&p.method==='clearRect').length;
    const shown={group_labels:false,draw_border:true,keep_square:false};
    patch(shown);await t.settle();await t.settle();
    t.resetPaint();patch(shown);await t.settle();
    t.equal(draws(),0,'Values already drawn do not redraw');
    t.resetPaint();patch({...shown,draw_border:false});await t.settle();
    t.check(draws()>0,'A changed value redraws');
    return true;
  },
  async pointSliderLocal() {
    const t=viewerTest;t.dataset();const id='overview_projection';
    await t.render({},{},id);
    const echo=size=>t.send('cerebro_scatter_defaults',{dataset_context:{...t.context},
      values:{point_size:size,point_opacity:1,percentage_cells_to_show:100}});
    echo(5);await t.settle();
    const box=document.createElement('div');box.className='shiny-input-container';
    box.innerHTML='<input id="overview_projection_point_size" value="5">';document.body.append(box);
    const input=box.querySelector('input'),radius=()=>t.arcs()[0].args[2],before=radius();
    const draws=()=>t.paints.filter(p=>p.canvas.id==='cv-cv-a'&&p.method==='clearRect').length;
    t.resetPaint();jQuery(input).trigger('change');
    t.equal(draws(),0,'A value bound without user input does not draw');
    input.dispatchEvent(new PointerEvent('pointerdown',{bubbles:true}));
    input.value='9';t.resetPaint();jQuery(input).trigger('change');
    t.equal(draws(),1,'Moving the slider draws at once, without a server reply');
    t.check(radius()>before,'Points drawn larger');
    t.resetPaint();echo(9);await t.settle();
    t.equal(draws(),0,'The server echo of the drawn value does not redraw');
    box.remove();
    return true;
  },
  async toggleLocal() {
    const t=viewerTest;t.dataset();const id='overview_projection';
    await t.render({},{},id);
    const box=document.createElement('input');box.type='checkbox';box.id='overview_projection_point_border';
    document.body.append(box);
    const draws=()=>t.paints.filter(p=>p.canvas.id==='cv-cv-a'&&p.method==='clearRect').length;
    const patch=values=>t.send('cell_view_appearance',{id,dataset_context:{...t.context},
      dataset_fingerprint:'fp-'+t.context.dataset_key,values});
    patch({draw_border:false});await t.settle();await t.settle();
    box.checked=true;t.resetPaint();box.dispatchEvent(new Event('change',{bubbles:true}));
    t.equal(draws(),1,'A display toggle draws at once, without a server reply');
    t.resetPaint();patch({draw_border:true});await t.settle();
    t.equal(draws(),0,'The server patch with the drawn value does not redraw');
    box.remove();
    return true;
  },
  async paintOrderPatch() {
    const t=viewerTest;t.dataset();const id='expression_projection';
    await t.render({color:[2,0,1],paint_order:'natural'},{},id);
    const order=()=>{const xs=t.arcs().map(p=>p.args[0]),sorted=xs.slice().sort((a,b)=>a-b);
      return xs.map(x=>sorted.indexOf(x));};
    const patch=values=>t.send('cell_view_appearance',{id,dataset_context:{...t.context},
      dataset_fingerprint:'fp-'+t.context.dataset_key,values});
    t.equal(order(),[0,1,2],'Cells are drawn in input order');
    t.resetPaint();patch({paint_order:'highest'});await t.settle();
    t.equal(order(),[1,2,0],'The highest expression is drawn last');
    t.resetPaint();patch({paint_order:'natural'});await t.settle();
    t.equal(order(),[0,1,2],'Input order again');
    return true;
  },
  async singleClientFilters() {
    const t=viewerTest;t.dataset();const id='overview_projection';
    const identity={cell_count:3,cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),
      cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};
    let downloads=0;
    window.fetch=async()=>{downloads++;return {ok:true,arrayBuffer:async()=>new Uint8Array([1,2,1]).buffer};};
    await t.render({},{},id,{dataset_identity:identity});
    t.equal(t.arcs().length,3,'Every cell is drawn');
    const resource={protocol:'canonical-metadata-codes-v1',url:'codes.bin',cells:3,dtype:'uint8',bytes:3,
      checksum:'',dataset_fingerprint:identity.cell_fingerprint,cell_order_fingerprint:identity.cell_order_fingerprint,
      pack_dataset_fingerprint:identity.pack_dataset_fingerprint,metadata_name:'cluster',levels:['A','B'],code_map:[-1,0,1]};
    const filters=allowed=>t.send('cell_view_filters',{id,dataset_context:{...t.context},
      filters:allowed?[{name:'cluster',levels:['A','B'],allowed,resource}]:[]});
    t.resetPaint();filters([1]);for(let i=0;i<3;i++)await t.settle();
    t.equal(t.arcs().length,1,'Only the allowed group is drawn');
    t.resetPaint();filters([0]);for(let i=0;i<3;i++)await t.settle();
    t.equal(t.arcs().length,2,'Another level, from the same codes');
    t.equal(downloads,1,'The codes are downloaded once');
    t.resetPaint();filters(null);for(let i=0;i<3;i++)await t.settle();
    t.equal(t.arcs().length,3,'No filter draws every cell again');
    return true;
  },
  async legendRelayout() {
    const t=viewerTest;t.dataset();
    const many=Array.from({length:60},(_,i)=>'a fairly long group name '+i);
    t.linked({groups:{many:{levels:many,values:[0,1,2],colors:many.map((_,i)=>'#'+(100000+i*997).toString(16).slice(-6))},
      few:{levels:['p'],values:[0,0,0],colors:['#123456']}}});
    for(let i=0;i<4;i++)await t.settle();
    const legend=document.getElementById('cv-legend'),before=legend.offsetHeight;
    t.resetPaint();t.change('cv-pick-color','few');await t.settle();await t.settle();
    t.check(legend.offsetHeight<before,'The shorter legend frees height');
    t.equal(t.paints.filter(p=>p.canvas.id==='cv-cv-a'&&p.method==='clearRect').length,1,'A colour change draws once');
    return true;
  },
  async linkedResources() {
    const t=viewerTest;t.dataset();let downloads=[];
    const identity={cell_count:3,cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),
      cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};
    const r={protocol:'canonical-projection-v1',projection_name:'UMAP',cells:3,dimensions:2,dtype:'float32',url:'projection.bin',bytes:24,
      dataset_fingerprint:identity.cell_fingerprint,cell_order_fingerprint:identity.cell_order_fingerprint,pack_dataset_fingerprint:identity.pack_dataset_fingerprint};
    window.fetch=async url=>{downloads.push(url);return {ok:true,arrayBuffer:async()=>new Float32Array([1,4,2,5,3,6]).buffer};};
    const bundle={dataset_id:'A',dataset_context:{...t.context},dataset_fingerprint:identity.cell_fingerprint,
      canonical_order_id:identity.cell_order_fingerprint,pack_dataset_fingerprint:identity.pack_dataset_fingerprint,
      n:3,cells:['a','b','c'],default_projection:'UMAP',groups:{cluster:{levels:['A'],values:[0,0,0],colors:['#ff0000']}},
      spaces:[{id:'expression'}],projections:{UMAP:{projection_resource:r}}};
    // Ambient identity must not override the bundle's identity.
    window.cerebroSavedViewDataset={...identity,cell_fingerprint:'foreign'};
    t.send('coordviews_binary',t.wire(bundle));await t.settle();
    t.equal(downloads,['projection.bin']);t.check(cerebroLinkedViewsState.summary().ready);
    t.equal(cerebroLinkedViewsState.summary().transport.primary.projectionBytes,24);
    for(const field of ['dataset_fingerprint','cell_order_fingerprint','pack_dataset_fingerprint']) {
      const before=t.requests.filter(r=>r.name==='coordviews_wire_fallback').length;
      t.send('coordviews_binary',t.wire({...bundle,projections:{UMAP:{projection_resource:{...r,[field]:'wrong'}}}}));
      await t.settle();t.check(t.requests.filter(r=>r.name==='coordviews_wire_fallback').length>before,'Reject linked '+field);
    }
    return true;
  },
  async trekker() {
    const t=viewerTest;t.dataset();
    const control=document.createElement('input');control.id='trekker_morph';document.body.append(control);
    await t.render({panels:[{id:'transition',label:'Transition',selection_key:['a','b','c'],x:[0,20,null],y:[0,10,5],
      from_x:[0,20,null],from_y:[0,10,5],to_x:[100,0,50],to_y:[0,200,50]}]}, {},'trekker_projection');
    const positions=[];
    for(const value of [0,.5,1,2,-1]) {
      control.value=value;window.jQuery(control).trigger('input');await t.settle();
      positions.push(t.arcs().map(p=>p.args.slice(0,2)));
      t.equal(positions.at(-1).length,2,'Transition preserves missing coordinates');
    }
    t.equal(positions[2],positions[3],'Upper transition clamp');t.equal(positions[0],positions[4],'Lower transition clamp');
    t.check(JSON.stringify(positions[0])!==JSON.stringify(positions[1]),'Slider actually moves points');
    await t.render();t.resetPaint();control.value=1;window.jQuery(control).trigger('input');await t.settle();
    t.equal(t.arcs().length,0,'Hidden Trekker does not repaint another view');
    return true;
  },
  async cloneCache() {
    const t=viewerTest;t.dataset();const n=20000;
    t.linked({n,cells:Array.from({length:n},(_,i)=>'c'+i),
      groups:{cluster:{levels:['A'],values:Array(n).fill(0),colors:['#123456']}},
      projections:{},spaces:[{id:'clone',label:'Clonal'}],
      clone:{id:Array.from({length:n},(_,i)=>i%2),size:[n/2,n/2],n_clones:2,n_receptor:n,label:['one','two'],tiers:[1,5,20],tier_labels:['Single (1)','Small (2-5)','Medium (6-20)','Large (>20)']}});
    await t.settle();t.resetPaint();
    const same=cerebroLinkedViewsState.capture();cerebroLinkedViewsState.apply(same);await t.settle();
    const copied=()=>t.paints.some(p=>p.method==='drawImage'&&p.canvas.id==='cv-cv-a'&&p.args[0] instanceof HTMLCanvasElement);
    t.check(copied(),'Unchanged clonal chart copies its cached bitmap');
    for(const act of ['zin','zin','reset']) {
      t.resetPaint();document.querySelector('[data-panel="A"][data-act="'+act+'"]').click();await t.settle();
      t.check(t.paints.some(p=>p.canvas.id==='cv-cv-a'&&p.method==='arc'),'Changed viewport redraws clonal points');
    }
    t.resetPaint();t.change('cv-borders',true);await t.settle();t.check(t.paints.some(p=>p.canvas.id==='cv-cv-a'&&p.method==='arc'),'Border change invalidates bitmap');
    const config=cerebroLinkedViewsState.capture();config.selection={cells:['c1','c2'],indices:[1,2]};
    t.resetPaint();cerebroLinkedViewsState.apply(config);await t.settle();
    t.equal(cerebroLinkedViewsState.summary().selectedCellBarcodes,['c1','c2']);
    t.check(t.paints.some(p=>p.canvas.id==='cv-cv-a'&&p.method==='arc'),'Selection change invalidates bitmap');
    document.querySelector('#cv-clone-layout [data-mode="bands"]').click();await t.settle();
    t.check(t.paints.some(p=>p.method==='fillText'&&String(p.args[0]).includes('tier')),'Layout redraws axes');
    return true;
  },
  async prefetch() {
    const t=viewerTest;t.dataset();const id='spatial_projection',downloads=[];
    const host=document.getElementById(id+'_cell_view_host'),pane=document.createElement('div');
    pane.id='shiny-tab-spatial';host.replaceWith(pane);pane.append(host);
    const link=document.createElement('a');link.href='#shiny-tab-spatial';document.body.append(link);
    const identity={cell_count:3,cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),
      cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};
    const resource={protocol:'spatial-geometry-v1',spatial_name:'slice',cells:3,dimensions:2,dtype:'float32',url:'slice.bin',bytes:24,
      dataset_fingerprint:identity.cell_fingerprint,cell_order_fingerprint:identity.cell_order_fingerprint,pack_dataset_fingerprint:identity.pack_dataset_fingerprint};
    window.fetch=async url=>{downloads.push(url);return {ok:true,arrayBuffer:async()=>new Float32Array([1,4,2,5,3,6]).buffer};};
    window.cerebroSavedViewDataset=identity;
    const catalog=(defaults=true)=>t.send('cell_view_resource_catalog',{id,dataset_context:{...t.context},dataset_identity:identity,
      resources:[resource],default_resource_name:defaults?'slice':null});
    catalog(false);t.equal(downloads.length,0,'Registration alone does not download');
    link.click();await t.settle();t.equal(downloads.length,0,'Unknown initial filters prevent prefetch');
    catalog();await t.settle();t.equal(downloads,['slice.bin'],'Late catalog overlaps initial server render');
    const percentage=document.createElement('input');percentage.id=id+'_percentage_cells_to_show';document.body.append(percentage);
    for(const percent of [50,100]) {
      t.dataset('set'+percent,percent);window.cerebroSavedViewDataset=identity;percentage.value=percent;catalog();link.click();await t.settle();
      t.equal(downloads.length,percent===50?1:2,'Only complete geometry is prefetched');
    }
    t.dataset('filtered',101);window.cerebroSavedViewDataset=identity;
    const group=document.createElement('div');group.id=id+'_group_filter_cluster';
    const checkbox=document.createElement('input');checkbox.type='checkbox';checkbox.checked=false;group.append(checkbox);document.body.append(group);
    catalog();link.click();await t.settle();t.equal(downloads.length,2,'Excluded groups prevent full prefetch');
    checkbox.checked=true;catalog();await t.settle();t.equal(downloads.length,3,'All groups permit retry');
    return true;
  },
  async gpuColors() {
    const t=viewerTest;t.dataset();const n=20000;
    const rgb={r:[],g:[],b:[]};for(let r=0;r<256;r+=17)for(let g=0;g<256;g+=17)for(let b=0;b<256;b+=17){rgb.r.push(r);rgb.g.push(g);rgb.b.push(b);}
    const combinations=rgb.r.length;
    for(const key of ['r','g','b'])while(rgb[key].length<n)rgb[key].push(0);
    const x=Float32Array.from({length:n},(_,i)=>i),y=Float32Array.from(x,v=>v%100);
    await t.render({n,selection_key:Array.from({length:n},(_,i)=>'c'+i),x,y,rgb},
      {color_type:'rgb',color_variable:'RGB'});
    const out=t.uploads.at(-1);t.check(out,'RGB reaches GPU');
    const byIndex=new Map();for(let j=0;j<out.count;j++)byIndex.set(Math.round(out.positions[j*2]*(n-1)),out.colors.slice(j*4,j*4+3));
    for(let i=0;i<combinations;i++) {
      const channels=[rgb.r[i],rgb.g[i],rgb.b[i]],max=Math.max(...channels),grey=[217,219,222];
      const expected=max<=28?grey:channels.every(v=>v>28)?[0,0,0]:
        channels.map((v,j)=>Math.round(grey[j]+(v/max*255-grey[j])*max/255));
      t.equal(byIndex.get(i),expected,'RGB blend '+channels);
    }
    const color=Array(n).fill(0);[2,null,1,2,NaN,0,1].forEach((v,i)=>color[i]=v);
    t.resetPaint();await t.render({n,selection_key:Array.from({length:n},(_,i)=>'c'+i),x,y,color,paint_order:'highest'});
    const ordered=t.uploads.at(-1),indices=[];
    for(let i=0;i<ordered.count;i++){const index=Math.round(ordered.positions[i*2]*(n-1));if(index<7)indices.push(index);}
    t.equal(indices,[1,4,5,2,6,0,3],'Missing values first, followed by stable ascending values');
    t.check(ordered.drawOrder===null,'A renderer without a draw order gets its points in paint order');
    // A renderer that draws through an order keeps the points in cell order
    // and is handed the same paint order as indices.
    t.gpuOrder=true;t.resetPaint();await t.render({n,selection_key:Array.from({length:n},(_,i)=>'c'+i),x,y,color,paint_order:'highest'});
    const viaGpu=t.uploads.at(-1),drawn=[];
    t.check(viaGpu.drawOrder&&viaGpu.drawOrder.length===viaGpu.count,'The paint order reaches the renderer');
    for(let j=0;j<viaGpu.count;j++)t.check(Math.round(viaGpu.positions[j*2]*(n-1))===j,'Points stay in cell order');
    for(const vertex of viaGpu.drawOrder){const index=Math.round(viaGpu.positions[vertex*2]*(n-1));if(index<7)drawn.push(index);}
    t.equal(drawn,[1,4,5,2,6,0,3],'The renderer draws the same order');
    const colourOf=(upload,cell)=>{for(let j=0;j<upload.count;j++)if(Math.round(upload.positions[j*2]*(n-1))===cell)return upload.colors.slice(j*4,j*4+4);};
    for(let cell=0;cell<7;cell++)t.equal(colourOf(viaGpu,cell),colourOf(ordered,cell),'Cell '+cell+' keeps its colour');
    t.gpuOrder=false;
    t.failGpu=true;t.resetPaint();await t.render({n,selection_key:null,x:Float32Array.from(x,(v,i)=>i===13?NaN:v),y,color});
    t.equal(t.arcs().length,n-1,'GPU failure falls back to valid CPU points');
    return true;
  },
  async gpu() {
    const t=viewerTest;t.dataset();const n=20000;
    const cells=Array.from({length:n},(_,i)=>'c'+i),x=Float32Array.from({length:n},(_,i)=>i/n),y=Float32Array.from(x,v=>1-v);
    const color=Float64Array.from({length:n},(_,i)=>i%7);x[13]=NaN;
    await t.render({n,selection_key:cells,x,y,color,point_size:4},{},'expression_projection');
    t.check(t.uploads.length>0,'Complete Viewer submitted GPU data');
    const original=t.uploads.at(-1);t.equal(original.count,n-1,'Invalid geometry excluded');
    // Drive a real brush, so this checks the incremental selection path rather
    // than reactivating a saved primary view.
    document.querySelector('[data-panel="A"][data-act="box"]').click();
    const canvas=document.getElementById('cv-cv-a'),bounds=canvas.getBoundingClientRect(),r=original.rect;
    const event=(type,fraction)=>new MouseEvent(type,{bubbles:true,button:0,buttons:type==='mouseup'?0:1,
      clientX:bounds.left+r.x+r.width*fraction,clientY:bounds.top+r.y+r.height*fraction});
    canvas.dispatchEvent(event('mousedown',.2));window.dispatchEvent(event('mousemove',.4));window.dispatchEvent(event('mouseup',.4));
    await t.settle();const brushed=t.uploads.at(-1),selection=cerebroCellViews.captureState('expression_projection').cells;
    t.check(selection.length>1000&&selection.length<6000,'Brush selects the expected region');
    t.check(brushed.positionSource===original.positionSource,'Selection-only update reuses position buffer');
    await t.render({n,selection_key:cells,x,y,color,point_size:4});
    cerebroCellViews.applyState('expression_projection',{selection:{cells:selection}});await t.settle();
    t.equal(t.uploads.at(-1).colors,brushed.colors,'Incremental brush colours equal full reconstruction');
    t.equal(t.uploads.at(-1).layers,brushed.layers,'Incremental brush layers equal full reconstruction');
    for(const selected of [['c1','c2','c13'],['c3'],[],['c9','c55']]) {
      cerebroCellViews.applyState('expression_projection',{selection:{cells:selected}});await t.settle();
      const update=t.uploads.at(-1);
      t.equal(update.positions,original.positions,'Selection preserves geometry');
      const selectedValid=selected.filter(s=>s!=='c13').length;
      t.equal(update.layers.filter(Boolean).length,selected.length?selectedValid:0,'Exact selected foreground');
      // A fresh primary message with the same saved selection is a full rebuild.
      const saved=cerebroCellViews.captureState('expression_projection');
      await t.render({n,selection_key:cells,x,y,color,point_size:4});
      cerebroCellViews.applyState('expression_projection',{selection:{cells:saved.cells}});await t.settle();
      const rebuilt=t.uploads.at(-1);
      t.equal(update.colors,rebuilt.colors,'Selection update equals full rebuild colours');
      t.equal(update.layers,rebuilt.layers,'Selection update equals full rebuild layers');
    }
    // Decorations remain eligible; per-cell size changes and 3-D use Canvas.
    t.resetPaint();await t.render({n,selection_key:cells,x,y,color},{is_spatial:true},'spatial_projection',
      {extra:{group_hulls:{x:[[0,1,0]],y:[[0,0,1]],color:['#123456']}}});
    t.check(t.uploads.length>0,'Hull overlay keeps GPU points');
    t.check(t.paints.some(p=>p.method==='fill'&&p.fill==='#123456'),'Hull fill reaches underlay');
    return true;
  }
};
