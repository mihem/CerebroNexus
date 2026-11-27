'use strict';
const assert=require('node:assert/strict');
const runtime=require('./viewer-runtime.cjs');
const root=process.argv[2], scenario=process.argv[3];
const identity={cell_count:3,cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),
  cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};
const fingerprints={dataset_fingerprint:identity.cell_fingerprint,
  cell_order_fingerprint:identity.cell_order_fingerprint,pack_dataset_fingerprint:identity.pack_dataset_fingerprint};
const resource={protocol:'canonical-projection-v1',projection_name:'UMAP',cells:3,dimensions:2,
  dtype:'float32',url:'projection.bin',bytes:24,...fingerprints};
const subset={protocol:'canonical-subset-v1',kind:'include_uint32',canonical_cells:3,cells:2,
  index_base:0,dtype:'uint32',url:'subset.bin',bytes:8};
const category={protocol:'canonical-metadata-codes-v1',cells:3,dtype:'uint8',url:'codes.bin',
  code_map:[-1,0,1],bytes:3,...fingerprints};
function setup() {
  const t=runtime(root);t.sequence=0;
  t.scope.fetch=async url=>{t.fetches.push(url);return {ok:true,arrayBuffer:async()=>{
    if(url==='subset.bin')return new Uint32Array([0,2]).buffer;
    if(url==='codes.bin')return new Uint8Array([1,2,1]).buffer;
    return new Float32Array([1,4,2,5,3,6]).buffer;
  }};};
  t.frame=(data={},overrides={})=>({id:'spatial_projection',dataset_context:{...t.context},render_token:++t.sequence,
    dataset_identity:identity,meta:{},data:{n:3,projection_resource:{...resource},...data},...overrides});
  t.deliver=async frame=>{t.binary('cell_view_binary',frame);await t.settle();};
  return t;
}
async function rejected(data,overrides={}) {
  const t=setup();await t.deliver(t.frame(data,overrides));
  assert.equal(t.summary().ready,false,'Invalid resource must not create a ready view');
  assert(t.requests.some(x=>x.name.endsWith('_projection_resource_failed')),'Emit retry/failure signal');
  return t;
}
const scenarios={
  async identity() {
    const t=setup();await t.deliver(t.frame());assert.equal(t.summary().ready,true);
    assert.equal(t.summary().xExtent.min,1);assert.equal(t.summary().xExtent.max,3);
    assert.equal(t.summary().yExtent.min,4);assert.deepEqual(t.fetches,['projection.bin']);
    for(const field of Object.keys(fingerprints)) {
      const bad=await rejected({projection_resource:{...resource,[field]:'wrong'}});
      assert.equal(bad.fetches.length,0,'Reject foreign identities before downloading');
    }
    await rejected({n:2});
    const legacy=setup();await legacy.deliver(legacy.frame({projection_resource:{...resource,protocol:'projection-v1'}}));
    assert(legacy.summary().ready,'Legacy geometry still renders');
    const failed=setup();failed.scope.fetch=async()=>{throw Error('offline');};
    await failed.deliver(failed.frame());assert(!failed.summary().ready);
    assert(failed.requests.some(x=>x.value==='projection.bin'));
  },
  async spatial() {
    const r={...resource,protocol:'spatial-geometry-v1',spatial_name:'slice'};
    const t=setup();await t.deliver(t.frame({projection_resource:r}));assert(t.summary().ready);
    for(const field of Object.keys(fingerprints))await rejected({projection_resource:{...r,[field]:'wrong'}});
    await rejected({n:2,projection_resource:r});
    await rejected({projection_resource:{...r,dimensions:3}});
  },
  async subsets() {
    const t=setup();await t.deliver(t.frame({n:2,projection_subset_resource:subset}));
    assert(t.summary().ready);assert.equal(t.summary().xExtent.count,2);
    assert.equal(t.summary().xExtent.max,3);assert.equal(t.summary().yExtent.min,4);
    await t.deliver(t.frame({n:2,projection_subset_resource:subset}));
    assert.equal(t.fetches.filter(x=>x==='subset.bin').length,1,'Reuse immutable subset bytes');
    for(const patch of [{canonical_cells:4},{cells:1},{index_base:1},{bytes:4}]) {
      await rejected({n:2,projection_subset_resource:{...subset,...patch}});
    }
    for(const indices of [[0,0],[0,3]]) {
      const bad=setup(), fetch=bad.scope.fetch;
      bad.scope.fetch=url=>url==='subset.bin'?Promise.resolve({ok:true,arrayBuffer:async()=>new Uint32Array(indices).buffer}):fetch(url);
      await bad.deliver(bad.frame({n:2,projection_subset_resource:subset}));assert(!bad.summary().ready);
    }
    const all=setup();await all.deliver(all.frame({projection_subset_resource:{...subset,kind:'identity',cells:3,bytes:0}}));
    assert(all.summary().ready);assert.equal(all.fetches.length,1);
  },
  async trajectory() {
    const frame={protocol:'trajectory-frame-v1',cells:2,canonical_cells:3,geometry_kind:'canonical_projection',
      projection:resource,subset,state:{url:'codes.bin',cells:2,dtype:'uint8',levels:['A','B']},...fingerprints};
    const t=setup(),fetch=t.scope.fetch;
    t.scope.fetch=url=>url==='codes.bin'?Promise.resolve({ok:true,arrayBuffer:async()=>new Uint8Array([0,1]).buffer}):fetch(url);
    await t.deliver(t.frame({n:2,trajectory_frame_resource:frame}));assert(t.summary().ready);
    for(const patch of [{dataset_fingerprint:'wrong'},{canonical_cells:4},{cells:1},{subset:null},
      {projection:{...resource,cell_order_fingerprint:'wrong'}},{state:null},{geometry_kind:'unknown'}]) {
      await rejected({n:2,trajectory_frame_resource:{...frame,...patch}});
    }
  },
  async metadata() {
    const t=setup();await t.deliver(t.frame({categorical_resource:category}));assert(t.summary().ready);
    assert(t.fetches.includes('codes.bin'));
    for(const field of Object.keys(fingerprints)) await rejected({categorical_resource:{...category,[field]:'wrong'}});
    await rejected({categorical_resource:{...category,cells:2}});
    const bad=setup(),fetch=bad.scope.fetch;
    bad.scope.fetch=url=>url==='codes.bin'?Promise.resolve({ok:true,arrayBuffer:async()=>new Uint8Array([99,1,1]).buffer}):fetch(url);
    await bad.deliver(bad.frame({categorical_resource:category}));assert(!bad.summary().ready);
  },
  async lifecycle() {
    const t=setup();const first=t.frame({x:[1,2,3],y:[4,5,6],projection_resource:null,wire_token:7});
    await t.deliver(first);assert(t.summary().ready);
    t.scope.cerebroCellViews.applyState(first.id,{selection:{cells:['a']}});
    assert.equal(t.scope.cerebroCellViews.captureState(first.id).cells[0],'a');
    t.dataset('B',2);assert(!t.summary().ready);assert.equal(t.scope.cerebroCellViews.captureState(first.id),null);
    for(const context of [null,first.dataset_context,{...t.context,epoch:'other'},{...t.context,dataset_key:'A'}]) {
      const stale={...first,dataset_context:context,render_token:99};
      t.binary('cell_view_binary',stale);
      t.binary('cell_view_aux_binary',{...stale,selection_key:['wrong']});
      t.binary('cell_view_recolor_binary',{...stale,data:{color:[99]}});
    }
    await t.settle();assert(!t.summary().ready);
    await t.deliver(t.frame());assert(t.summary().ready);assert.equal(t.summary().datasetKey,'B');
    await t.deliver({...t.frame({x:[99],y:[99],projection_resource:null}),render_token:1});
    assert.equal(t.summary().xExtent.max,3,'Older render token cannot overwrite fresh geometry');
    t.dataset('A',3);assert(!t.summary().ready);await t.deliver(first);assert(!t.summary().ready);
  },
  async caches() {
    const t=setup();
    for(let generation=1;generation<=10;generation++) {
      t.dataset('dataset-'+generation,generation+1);
      await t.deliver(t.frame());await t.deliver(t.frame());
      assert(t.summary().ready);assert.equal(t.fetches.length,generation,'One fetch per generation, no retired cache reuse');
    }
    let rejectOld,resolveNew;
    t.dataset('old',12);t.scope.fetch=()=>new Promise((resolve,reject)=>{rejectOld=reject;});
    t.binary('cell_view_binary',t.frame());await t.settle();
    t.dataset('new',13);t.scope.fetch=()=>new Promise(resolve=>{resolveNew=resolve;});
    t.binary('cell_view_binary',t.frame());await t.settle();
    rejectOld(Error('retired'));await t.settle();assert(!t.summary().ready);
    resolveNew({ok:true,arrayBuffer:async()=>new Float32Array([1,4,2,5,3,6]).buffer});await t.settle();
    assert(t.summary().ready);
    t.scope.fetch=()=>{throw Error('Late failure evicted current cache');};
    await t.deliver(t.frame());assert(t.summary().ready);
  }
};
(async()=>{
  if(scenario==='all')for(const fn of Object.values(scenarios))await fn();
  else {assert(scenarios[scenario],'Unknown scenario '+scenario);await scenarios[scenario]();}
  console.log('PASS complete-script transport: '+scenario);
})().catch(error=>{console.error(error);process.exitCode=1;});
