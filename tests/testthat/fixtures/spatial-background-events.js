const fs=require('fs'),vm=require('vm'),assert=require('assert');
const src=fs.readFileSync(process.argv[2],'utf8');
const code=src.slice(src.indexOf('(function registerSpatialBackgroundControlPayload()'),src.indexOf('// The button says Copy'));
for(const jquery of [true,false]) {
 const native={},jq={},frames=new Map(),sent=[];let seq=0,attached=true,fresh=true;
 const prefix='spatial_projection_background_';
 const inputs={};
 for(const [id,value] of Object.entries({opacity:.8,offset_x:10,offset_y:20,scale:1,scale_x:1,scale_y:1,rotate:0})) inputs[prefix+id]={id:prefix+id,value};
 for(const id of ['scale_lock','flip_x','flip_y'])inputs[prefix+id]={id:prefix+id,checked:id==='scale_lock'};
 const identity={dataset:'arbitrary',spatial_name:'section',key:'image'};
 const scope={querySelector:s=>inputs[s.slice(1)],getAttribute:k=>({'data-dataset-epoch':'epoch','data-dataset-key':'arbitrary','data-dataset-generation':'3','data-background-identity':JSON.stringify(identity)})[k]};
 for(const input of Object.values(inputs))input.closest=()=>scope;
 const document={documentElement:{contains:()=>attached},addEventListener:(k,fn)=>native[k]=fn};
 const window={Shiny:{setInputValue:(key,value)=>sent.push(value)},CerebroDatasetContext:{normalize:c=>c,accepts:()=>fresh},
 requestAnimationFrame:fn=>{frames.set(++seq,fn);return seq;},cancelAnimationFrame:id=>frames.delete(id)};
 if(jquery)window.jQuery=()=>({on:(names,fn)=>names.split(' ').forEach(n=>jq[n.split('.')[0]]=fn)});
 vm.runInNewContext(code,{window,document,Shiny:window.Shiny,Number,JSON});
 function event(id,type='change'){const cb=(jquery?jq:native)[type];assert(cb,'event handler missing');cb({target:inputs[prefix+id]});}
 function flush(){for(const fn of frames.values()){frames.clear();fn();}}
 inputs[prefix+'opacity'].value=.25;event('opacity');flush();assert.equal(sent.at(-1).values.opacity,.25);
 inputs[prefix+'offset_x'].value=30;event('offset_x');inputs[prefix+'rotate'].value=45;event('rotate');flush();
 assert.equal(sent.length,2);assert.equal(sent.at(-1).values.offsetX,30);assert.equal(sent.at(-1).values.rotate,45);
 inputs[prefix+'scale_num']={id:prefix+'scale_num',value:.7,closest:()=>scope};event('scale_num','input');flush();
 assert.equal(sent.at(-1).values.scaleX,.7);assert.equal(sent.at(-1).values.scaleY,.7);
 inputs[prefix+'flip_x'].checked=true;event('flip_x');flush();assert.equal(sent.at(-1).values.flipX,true);
 // Detached controls and obsolete dataset generations must never send updates.
 const count=sent.length;event('opacity');attached=false;flush();assert.equal(sent.length,count);
 attached=true;fresh=false;event('opacity');flush();assert.equal(sent.length,count);
 fresh=true;inputs[prefix+'opacity'].value=.8;event('opacity');flush();assert.equal(sent.at(-1).values.opacity,.8);
 assert.equal(sent.at(-1).background_identity.key,'image');
}
console.log('PASS jQuery/native sliders, numeric boxes, atomic changes, checkbox/reset and stale scopes');
