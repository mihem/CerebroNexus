const fs = require('fs'), vm = require('vm'), assert = require('assert');
const source = fs.readFileSync(process.argv[2], 'utf8');
const start = source.indexOf('  function updateSingleBackground(');
const end = source.indexOf('\n  function ', start + 1);
const a = {key:'shared-image', roi:'A'}, b = {key:'shared-image', roi:'B'};
const canonicalIdentity = x => x ? JSON.stringify(x) : null;
const view = {background_identity:null, data:{panels:[{image_identity:a},{image_identity:b}]}};
const space = identity => ({_backgroundIdentity:canonicalIdentity(identity), _imgState:{opacity:.6,rotate:0}});
const c = {singleActive:'spatial_projection', singleView:()=>view,
  datasetMessageFresh:m=>m.dataset_context===2, canonicalIdentity,
  singleSpaceIds:['A','B'],spaceById:{A:space(a),B:space(b)},drawAll:()=>{},stashImgState:()=>{}};
vm.createContext(c);vm.runInContext(source.slice(start,end),c);
function edit(identity,values,context=2){return c.updateSingleBackground({
  id:'spatial_projection',dataset_context:context,background_identity:identity,values});}
assert(edit(a,{opacity:.2,rotate:40}));
assert.equal(c.spaceById.A._imgState.opacity,.2);
assert.equal(c.spaceById.B._imgState.opacity,.6);
assert(edit(b,{opacity:.8,offsetX:20,flipY:true}));
assert.equal(c.spaceById.A._imgState.rotate,40);
assert.equal(c.spaceById.B._imgState.offsetX,20);
assert(!edit(a,{opacity:.9},1));assert(!edit({key:'missing'}, {opacity:1}));
c.singleActive='overview_projection';assert(edit(a,{opacity:.3}));
assert.equal(c.spaceById.A._imgState.opacity,.2);
assert.equal(view.backgroundEdits[canonicalIdentity(a)].values.opacity,.3);
assert.equal(view.backgroundEdits[canonicalIdentity(b)].values.opacity,.8);
assert.equal(view.data.panels[0].image_identity,a);
console.log('PASS: independent facets, inactive edits, stale identity/context rejection');
