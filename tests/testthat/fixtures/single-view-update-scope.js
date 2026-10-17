const fs = require('fs'), vm = require('vm'), assert = require('assert');
const source = fs.readFileSync(process.argv[2], 'utf8');
function fn(name) {
  const start = source.indexOf('  function ' + name + '(');
  assert(start >= 0);
  return source.slice(start, source.indexOf('\n  function ', start + 1));
}
const c = {}; vm.createContext(c);
vm.runInContext(fn('sameSingleLayout') + fn('sameSingleImages'), c);
const meta = {space_label:'fov'};
const data = {panels:[{id:'a',label:'ROI A',spatial:true},{id:'b',label:'ROI B',spatial:true}]};
const previous = {meta, data};
const mean = {meta, data:{color:new Float64Array([1,2])}};
const separate = {color:{CD3D:[1,2],MS4A1:[2,1],LYZ:[0,1]}};
assert(!c.sameSingleLayout(mean, meta, separate));
assert(!c.sameSingleLayout({meta,data:separate}, meta, mean.data));
assert(c.sameSingleLayout({meta,data:separate}, meta,
  {color:{CD3D:[2,3],MS4A1:[1,0],LYZ:[1,2]}}));
assert(!c.sameSingleLayout({meta,data:separate}, meta,
  {color:{CD3D:[1,2],MS4A1:[2,1]}}));
assert(c.sameSingleLayout(previous, meta, {...data,n:20}));
assert(!c.sameSingleLayout(previous, meta, {...data,reset_axes:true}));
assert(!c.sameSingleLayout(previous, {space_label:'different'}, data));
assert(!c.sameSingleLayout(previous, meta, {panels:data.panels.slice(0,1)}));
assert(!c.sameSingleLayout(previous, meta, {panels:[...data.panels].reverse()}));
assert(!c.sameSingleLayout(previous, meta, {...data,z:[1,2]}));
assert(!c.sameSingleLayout({}, meta, data));
const image = {id:'ROI A image',uri:'data:image/png;base64,abc',bounds:{xmin:0,xmax:100},preset:{opacity:.6}};
assert(c.sameSingleImages([image], [{...image}]));
assert(c.sameSingleImages(null, []));
for(const change of [{id:'ROI B image'}, {uri:'data:image/png;base64,new'},
  {bounds:{xmin:1,xmax:100}}, {preset:{opacity:.2}}]) {
  assert(!c.sameSingleImages([image], [{...image,...change}]));
}
console.log('PASS: reuse unchanged topology/images; invalidate changed projection, ROI, dimensions or image');
