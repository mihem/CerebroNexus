const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
var D = {}, _ordD, _ordKey, _ordVal, GENE_MODE = 'gene';
var field = {v:[1000,0,500,500,500,null], raw:[9,0,4.0002,4.0001,4.0001,null], paintOrder:false};
function panelColorMode(){return 'field';}
function fieldForMode(){return field;}
eval(source.slice(source.indexOf('  function paintOrder('), source.indexOf('  // ---- colour range')));
assert.strictEqual(paintOrder({}),null);
field.paintOrder=true; _ordD=null;
assert.deepStrictEqual(Array.from(paintOrder({})),[5,1,3,4,2,0]); // NA first, exact values, stable ties
field.paintOrder=false; _ordD=null;
assert.strictEqual(paintOrder({}),null);
// A paint-order patch flips the flag on the same data: the cached order follows.
field.paintOrder=true; assert.deepStrictEqual(Array.from(paintOrder({})),[5,1,3,4,2,0]);
field.paintOrder=false; assert.strictEqual(paintOrder({}),null);
// The bucket sort gives exactly the comparator order it replaces: missing
// first, ascending exact values, ties by index -- across ties, zeros, +-0,
// infinities, an outlier that crowds every other value into one bucket, a
// tiny span, and enough cells to reach the depth-limited comparator.
function comparatorOrder(raw){
  return Array.from({length:raw.length},(_,i)=>i).sort(function(a,b){
    var av=raw[a],bv=raw[b],am=av==null||isNaN(av),bm=bv==null||isNaN(bv);
    return am!==bm?(am?-1:1):am?a-b:(av-bv||a-b);
  });
}
var seed=7;function rnd(){seed=(Math.imul(seed,1103515245)+12345)>>>0;return seed/4294967296;}
var cases=[[],[3],[null,NaN,2,-0,0,2,Infinity,-Infinity,1]];
for(const n of [33,100,5000,60000]){
  cases.push(Array.from({length:n},()=>rnd()<0.6?0:Math.round(rnd()*50)/10));
  cases.push(Float32Array.from({length:n},()=>rnd()<0.05?NaN:rnd()*4));
  cases.push(Array.from({length:n},(_,i)=>i===n>>1?1e6:(rnd()<0.1?null:1+Math.floor(rnd()*400)*1e-12)));
  cases.push(Float64Array.from({length:n},(_,i)=>i%7===0?Infinity:(i%11===0?-Infinity:rnd()-0.5)));
  cases.push(Float64Array.from({length:n},()=>5e-324*Math.floor(rnd()*100)));
}
for(const raw of cases){
  _ordD=null;field.raw=raw;field.v=raw;field.paintOrder=true;
  assert.deepStrictEqual(Array.from(paintOrder({})),comparatorOrder(raw));
}
field.v=[1000,0,500,500,500,null];field.raw=[9,0,4.0002,4.0001,4.0001,null];field.paintOrder=false;_ordD=null;
// Exercise the actual Canvas fallback beyond its colour-batching threshold.
var n=20001, pointsDrawn=false, p={ok:new Uint8Array(n)}, shownMask=new Uint8Array(n);
for(let i=0;i<3;i++){p.ok[i]=1;shownMask[i]=1;}
var hiSet=null, rgb=false, panelPointOpacity=1, border=null, BATCH_MIN=20000, i;
const painted=[];
function paintCell(p,i){painted.push(i);}
eval(source.slice(source.indexOf('    var ord = pointsDrawn ? null : paintOrder(p);'),source.indexOf('    if (!pointsDrawn && cloneState)')));
assert.deepStrictEqual(painted,[0,1,2]);
console.log('PASS input order, ascending exact values, NA/ties, bucket sort = comparator, toggle and large Canvas fallback');
