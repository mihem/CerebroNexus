const assert=require('assert/strict'),fs=require('fs');
const source=fs.readFileSync(process.argv[2],'utf8');
const extract=(start,end)=>source.slice(source.indexOf(start),source.indexOf(end));
let singleActive='expression_projection',colorBy='gene',singleSpaceIds=['one'];
let D={cells:['cell-B','cell-A'],dataset_context:{generation:1},groups:{}};
const spaceById={one:{_hoverColumns:[]}},HOVER_MAX_GROUPS=6,GENE_MODE='__gene__';
const panel={spaceId:'one'};
function panelColorMode(){return colorBy;}
function catOf(){return null;}
function fieldOf(p){assert.equal(p,panel);return {label:'CD3D'};}
function fieldValue(){return 2.5;}
function singleView(){return {meta:{color_variable:'Gene'}};}
function groupLabel(s){return s;}
function cellLabel(i){return D.cells[i];}
function fmtVal(n){return String(n);}
function esc(s){return String(s).replace(/</g,'&lt;');}
eval(extract('  function hoverHtml(', '  // ---- single-cell detail card'));
assert.match(hoverHtml(0,false,panel),/Cell: cell-B/);
assert.match(hoverHtml(0,false,panel),/CD3D: 2.5/);
assert(!hoverHtml(0,false,panel).includes('cv-tip-details'));
assert.match(hoverHtml(0,true,panel),/cv-tip-details/);
D.cells=[];assert.match(hoverHtml(0,false,panel),/identifier available in Details/);
let cardCell=0,cardMeta=null,cardRequest=null,cardRequestSerial=0,rendered=0,aux=0;
const sent=[];
const Shiny={setInputValue:(name,value)=>sent.push({name,value})};
function cardOpen(){return cardCell!=null;}
function requestSingleAux(){aux++;}
function datasetMessageFresh(m){return m.dataset_context.generation===D.dataset_context.generation;}
function sameDatasetContext(a,b){return a.generation===b.generation;}
function renderCard(){rendered++;}
eval(extract('  function requestCardMetadata()', '  function cardOpen()'));
requestCardMetadata();assert.equal(aux,1);assert.equal(sent.length,0);
D.cells=['cell-B','cell-A'];requestCardMetadata();requestCardMetadata();
assert.equal(sent.length,1);assert.equal(sent[0].value.cell,'cell-B');
assert.equal(sent[0].value.index,undefined); // Never use a Linked dataset row index.
const reply={...sent[0].value,rows:[{k:'cluster',v:'B'}]};
receiveCardMetadata({...reply,dataset_context:{generation:0}});assert.equal(rendered,0);
receiveCardMetadata({...reply,view_id:'linked'});assert.equal(rendered,0);
receiveCardMetadata({...reply,cell:'cell-A'});assert.equal(rendered,0);
receiveCardMetadata({...reply,request_id:0});assert.equal(rendered,0);
receiveCardMetadata(reply);assert.equal(rendered,1);assert.equal(cardMeta.cell,'cell-B');
cardCell=null;receiveCardMetadata(reply);assert.equal(rendered,1);
cardCell=1;cardRequest=null;requestCardMetadata();receiveCardMetadata(reply);assert.equal(rendered,1);
receiveCardMetadata({...sent[1].value,error:'Cell metadata unavailable.'});
assert.equal(rendered,2);assert.equal(cardMeta.error,'Cell metadata unavailable.');
// Tap and drag share a canvas without turning page scrolling into a cell pick.
let picked=[],touchListeners={},mouseListeners={};
const p={canvas:{addEventListener:(n,fn)=>touchListeners[n]=fn,getBoundingClientRect:()=>({left:0,top:0})}};
const window={addEventListener:(n,fn)=>mouseListeners[n]=fn};
function pickCell(p,i){picked.push(i);}
function clearTransientHover(){}
function interactionIsStatic(){return false;}
function nearest(){return 1;}
eval(extract('  function wireBrush(', '  // ---- build panels from DOM'));
wireBrush(p);
touchListeners.pointerdown({pointerType:'touch',pointerId:1,clientX:10,clientY:10});
touchListeners.pointerup({pointerId:1,clientX:12,clientY:10});assert.deepEqual(picked,[1]);
touchListeners.pointerdown({pointerType:'touch',pointerId:2,clientX:10,clientY:10});
touchListeners.pointerup({pointerId:2,clientX:40,clientY:10});assert.deepEqual(picked,[1]);
touchListeners.pointerdown({pointerType:'touch',pointerId:3,clientX:10,clientY:10});
touchListeners.pointercancel();touchListeners.pointerup({pointerId:3,clientX:10,clientY:10});assert.deepEqual(picked,[1]);
console.log('PASS cell summaries, stable-ID detail lookup, stale reply rejection, touch tap and scroll separation');
