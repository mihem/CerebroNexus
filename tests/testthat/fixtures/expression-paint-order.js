const assert = require('assert'), fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf8');
var D = {}, _ordD, _ordKey, _ordVal, GENE_MODE = 'gene';
var field = {v:[1000,0,500,500,500,null], raw:[9,0,4.0002,4.0001,4.0001,null], paintOrder:false};
function panelColorMode(){return 'field';}
function fieldForMode(){return field;}
eval(source.slice(source.indexOf('  function paintOrder('), source.indexOf('  // ---- colour range')));
assert.strictEqual(paintOrder({}),null);
field.paintOrder=true; _ordD=null;
assert.deepStrictEqual(paintOrder({}),[5,1,3,4,2,0]); // NA first, exact values, stable ties
field.paintOrder=false; _ordD=null;
assert.strictEqual(paintOrder({}),null);
// Exercise the actual Canvas fallback beyond its colour-batching threshold.
var n=20001, pointsDrawn=false, p={ok:new Uint8Array(n)}, shownMask=new Uint8Array(n);
for(let i=0;i<3;i++){p.ok[i]=1;shownMask[i]=1;}
var hiSet=null, rgb=false, panelPointOpacity=1, border=null, BATCH_MIN=20000, i;
const painted=[];
function paintCell(p,i){painted.push(i);}
eval(source.slice(source.indexOf('    var ord = pointsDrawn ? null : paintOrder(p);'),source.indexOf('    if (!pointsDrawn && cloneState)')));
assert.deepStrictEqual(painted,[0,1,2]);
console.log('PASS input order, ascending exact values, NA/ties, toggle and large Canvas fallback');
