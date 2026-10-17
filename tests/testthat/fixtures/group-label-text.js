const assert = require('assert'), fs = require('fs');
const source=fs.readFileSync(process.argv[2],'utf8');
let labelsOn=true, D={display_defaults:{}}, hidden=new Set(), spaceById={};
const labels=[{li:0,nx:.2,ny:.3,text:'Cluster A'},{li:1,nx:.6,ny:.5,text:'Cluster B'}];
function groupLabelsFor(){return labels;}
function rotUnit(rot,x,y,z){return [x,y,z];}
const calls=[];
const ctx={save(){},restore(){},measureText(t){return {width:t.length*6};},
 fillText(t,x,y){calls.push([t,x,y]);},fillRect(){throw Error('Cluster label background rectangle');}};
eval(source.slice(source.indexOf('  function drawGroupLabels('),source.indexOf('  // ---- minimap')));
const p={W:600,H:400,ctx,_SX:500,_SY:300,_sox:0,_soy:0,spaceId:'test'};
drawGroupLabels(p);assert.equal(calls.length,2);assert.equal(calls[0][0],'Cluster A');
hidden.add(1);drawGroupLabels(p);assert.equal(calls.length,3);
labelsOn=false;drawGroupLabels(p);assert.equal(calls.length,3);
labelsOn=true;spaceById.test={_unit:{nz:[0,0]}};p.rot={};labels[0].nz=0;
drawGroupLabels(p);assert.equal(calls.length,4);
console.log('PASS shared 2D/3D labels render text without rectangles and preserve visibility');
