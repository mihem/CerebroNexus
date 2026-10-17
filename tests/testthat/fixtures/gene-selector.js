'use strict';
const assert = require('assert/strict'), fs = require('fs'), vm = require('vm');
const pending = [], changes = [];
const selectize = {
  items: ['CD3D'], options: {CD3D: {value:'CD3D',label:'CD3D'}},
  settings: {valueField:'value',labelField:'label'},
  getValue() { return this.items.slice(); },
  clear(silent) { this.items=[]; if(!silent) changes.push([]); },
  clearOptions(silent) { this.options={}; this.clear(silent); },
  addOption(option) { this.options[option.value]=option; },
  setValue(value,silent) {
    this.clear(silent);
    this.items=(Array.isArray(value)?value:value?[value]:[]).filter(v=>this.options[v]);
    if(!silent) changes.push(this.getValue());
  },
  load(fn) { fn(choices=>(choices||[]).forEach(option=>this.addOption(option))); }
};
const element={id:'expression_genes_input',selectize};
const binding={name:'shiny.selectInput',async receiveMessage(el,data) {
  const s=el.selectize;
  if(data.url) {
    s.clear(); s.clearOptions();
    s.load(callback=>pending.push(genes=>{
      callback(genes.map(value=>({value,label:value})));
      if(data.value!==undefined)s.setValue(data.value);
    }));
  } else if(data.value!==undefined) s.setValue(data.value);
  await Promise.resolve();
  changes.push(s.getValue());
  return 'original-result';
}};
const context={window:{Shiny:{inputBindings:{getBindings:()=>[{binding}]}}},
  document:{addEventListener(event,fn){fn();}}};
context.Shiny=context.window.Shiny;
vm.runInNewContext(fs.readFileSync(process.argv[2],'utf8'),context);
(async()=>{
  assert.equal(await binding.receiveMessage(element,{url:'first',value:['CD3D']}),'original-result');
  assert(changes.every(v=>v.join()==='CD3D'),'refresh must not publish an empty selection');
  sEdit(['NKG7']); pending.shift()(['MS4A1']);
  assert.deepEqual(selectize.items,['NKG7']);
  await binding.receiveMessage(element,{url:'second'});
  sEdit([]); pending.shift()(['MS4A1']);
  assert.deepEqual(selectize.items,[],'a deliberate clear survives a late search');
  await binding.receiveMessage(element,{url:'old',value:['CD3D']});
  await binding.receiveMessage(element,{url:'new',value:['MS4A1']});
  pending.pop()(['MS4A1']); pending.shift()(['OLD_DATASET_GENE']);
  assert.deepEqual(selectize.items,['MS4A1']);
  assert(!selectize.options.OLD_DATASET_GENE,'retired searches cannot add old choices');
  await binding.receiveMessage(element,{url:'canonical',value:['Ms4a1']});
  assert.deepEqual(selectize.items,['Ms4a1']);
  sEdit(['CD3E']); pending.shift()(['Ms4a1']);
  assert.deepEqual(selectize.items,['CD3E']);
  for(const channel of ['r','g','b']) {
    element.id='expression_rgb_gene_'+channel; changes.length=0;
    await binding.receiveMessage(element,{url:'rgb',value:'CD3D'});
    assert(changes.every(v=>v.join()==='CD3D'));
    sEdit([]); pending.shift()(['CD3D']);
    assert.deepEqual(selectize.items,[]);
  }
  await binding.receiveMessage(element,{value:[]});
  assert.deepEqual(selectize.items,[]);
  element.id='unrelated'; changes.length=0;
  await binding.receiveMessage(element,{url:'ordinary',value:['MS4A1']});
  assert(changes.some(v=>v.length===0),'unrelated bindings retain their normal behavior');
  console.log('Gene selector atomic refresh assertions passed');
})().catch(e=>{console.error(e);process.exitCode=1;});
function sEdit(values){values.forEach(value=>selectize.addOption({value,label:value}));selectize.setValue(values);}
