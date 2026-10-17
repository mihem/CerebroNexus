'use strict';
const fs = require('fs');
const vm = require('vm');
const assert = require('assert/strict');
const path = require('path');
const handlers = {}, events = {}, messages = {};
function element(value) {
  const classes = new Set(), attrs = {};
  return { value, textContent: '', classList: {
    toggle(k, on) { if (on) classes.add(k); else classes.delete(k); },
    contains(k) { return classes.has(k); }
  }, setAttribute(k, v) { attrs[k] = v; }, removeAttribute(k) { delete attrs[k]; },
  getAttribute(k) { return attrs[k]; }, getClientRects() { return [1]; },
  closest() { return this; }, querySelector() { return null; } };
}
const ids = ['expression_analysis_mode', 'expression_genes_input',
  'expression_projection_genes_in_separate_panels', 'expression_rgb_gene_r',
  'expression_rgb_gene_g', 'expression_rgb_gene_b', 'expression_select_gene_set',
  'expression_rgb_layout'];
const elements = Object.fromEntries(ids.map(id => [id, element('')]));
elements[ids[0]].value = '';
elements[ids[0]].querySelector = () => ({value:'Gene(s)'});
elements[ids[2]].value = 'single';
elements.expression_feedback = element('');
elements.expression_selection_feedback = element('');
elements['shiny-tab-geneExpression'] = element('');
elements.expression_projection_cell_view_host = element('');
const label = elements.expression_feedback;
let context = {epoch:'e', dataset_key:'Mouse', generation:1}, phase = 'ready';
const document = { readyState:'complete', getElementById(id) { return elements[id]; } };
const jq = {on(names, fn) { names.split(' ').forEach(n => { handlers[n.split('.')[0]] = fn; }); return jq; }};
const window = {jQuery() { return jq; },
  addEventListener(name, fn) { events[name] = fn; },
  Shiny: { addCustomMessageHandler(name, fn) { messages[name] = fn; } },
  CerebroDatasetContext: {phase() {return phase;}, accepts(m) {
    return phase === 'ready' && JSON.stringify(m.dataset_context) === JSON.stringify(context);
  }}
};
vm.runInNewContext(fs.readFileSync(process.argv[2] || path.resolve(__dirname,
  '../../../inst/viewer/www/gene-feedback.js'), 'utf8'), {window, document});
let count = 0;
function check(value) { assert.ok(value); count++; }
function key(gene, mode='single', rgb='\x1c\x1c') { return [mode, gene, rgb].join('\x1d'); }
function values() { return Object.fromEntries(ids.map(id => [id,
  id === ids[0] ? 'Gene(s)' : elements[id].value])); }
function map(renderKey, controls=values(), dataset_context=context) {
  messages.cerebro_gene_feedback({render_key:renderKey, controls, dataset_context});
}
function paint(renderKey, dataset_context=context, painted=true) {
  events['cerebro:cell-view-ready']({detail:{id:'expression_projection',renderKey,dataset_context,painted}});
}
function select(gene) {
  elements[ids[1]].value = gene;
  handlers.change({target:elements[ids[1]]});
}
// DOM test doubles retain IDs just like native controls.
ids.forEach(id => { elements[id].id = id; });
check(label.hidden && label.textContent === '');
assert(!elements['shiny-tab-geneExpression'].classList.contains('cerebro-expression-empty'));
assert(!elements.expression_projection_cell_view_host.inert);
map(key('')); paint(key(''));
select('Gad1'); const oldControls = values();
check(label.textContent.includes('Updating: Gad1'));
check(!label.hidden);
check(label.getAttribute('aria-busy') === 'true');
map(key('Gad1')); paint(key('Gad1'), context, false);
check(label.getAttribute('aria-busy') === 'true');
paint(key('Gad1'));
check(label.hidden && label.textContent === '');
assert(!elements['shiny-tab-geneExpression'].classList.contains('cerebro-expression-empty'));
assert(!elements.expression_projection_cell_view_host.inert);
check(label.hidden);
check(!label.getAttribute('aria-busy'));
events['cerebro:specialist-state']({detail:{viewId:'expression_projection',
  dataset_context:context,selectionPending:true,selectedCells:745}});
check(elements.expression_selection_feedback.textContent.includes('745 cells selected'));
events['cerebro:specialist-state']({detail:{viewId:'expression_projection',
  dataset_context:context,selectionPending:false,selectedCells:745}});
check(elements.expression_selection_feedback.hidden);
select('Gad2'); map(key('Gad2')); select('Slc17a7');
paint(key('Gad2')); map(key('Gad1'), oldControls);
check(label.textContent.includes('Updating: Slc17a7'));
check(label.textContent.includes('Last confirmed: Gad2'));
map(key('Slc17a7')); paint(key('Slc17a7'));
check(label.hidden && label.textContent === '');
// Missing and case-insensitive input use the server's canonical mapping.
select('not-a-gene'); map(key('')); paint(key(''));
check(label.hidden && label.textContent === '');
assert(!elements.expression_projection_cell_view_host.inert);
check(label.hidden);
select('gad1'); paint(key('Gad1')); map(key('Gad1'));
check(label.hidden && label.textContent === '');
// Revisit never invents a new loading cycle.
handlers['shown']({});
check(!label.getAttribute('aria-busy'));
// Context gate rejects a retired generation even after A -> B -> A.
const oldContext = context;
phase='pending'; events['cerebro:dataset-context']({detail:{phase,changed:false}});
check(!label.textContent.includes('Gad1'));
check(!elements[ids[1]].classList.contains('cerebro-control-pending'));
paint(key('Gad1'), oldContext); check(!label.textContent.includes('Showing:'));
context={epoch:'e',dataset_key:'Demo',generation:2}; phase='ready';
select('MS4A1'); map(key('MS4A1')); paint(key('MS4A1'));
check(label.hidden && label.textContent === '');
context={epoch:'e',dataset_key:'Mouse',generation:3};
events['cerebro:dataset-context']({detail:{phase,changed:true}});
select('Gad2'); map(key('Gad2')); paint(key('Gad1'),oldContext);
check(label.textContent.includes('Updating: Gad2'));
paint(key('Gad2')); check(label.hidden && label.textContent === '');
elements[ids[2]].value='rgb'; elements[ids[3]].value='Gad1';
handlers.change({target:elements[ids[2]]});
check(label.textContent.includes('Updating: RGB: Gad1, —, —'));
map(key('Gad1','rgb','Gad1\x1c\x1c')); paint(key('Gad1','rgb','Gad1\x1c\x1c'));
check(label.hidden && label.textContent === '');
check(!label.getAttribute('aria-busy'));
elements[ids[7]].value='separate';
handlers.change({target:elements[ids[7]]});
check(!label.hidden);
const splitKey=key('Gad1','rgb','Gad1\x1c\x1c')+'\x1dseparate';
map(splitKey); paint(key('Gad1','rgb','Gad1\x1c\x1c'));
check(!label.hidden); // A composite paint cannot complete the split request.
paint(splitKey); check(label.hidden);
console.log(`${count} Gene feedback assertions passed`);
