const assert = require('assert');
const fs = require('fs');
const vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
const code = source.slice(source.lastIndexOf('  ready(function () {'), source.lastIndexOf('}());'));
function session(reducedMotion = false) {
  let now = 0, serial = 0, overlay, pane = null;
  const tasks = new Map(), handlers = {}, events = {};
  const classes = () => { const values = new Set(); return {add: x => values.add(x), remove: x => values.delete(x), contains: x => values.has(x)}; };
  const document = {
    getElementById: () => null,
    querySelector: () => pane,
    body: {classList: classes(), appendChild: e => {overlay = e;}},
    createElement: () => {
      const nodes = {};
      return {classList: classes(), querySelector: s => nodes[s] || (nodes[s] = {textContent:'', style:{}})};
    },
    addEventListener: (name, fn) => {events[name] = fn;}
  };
  const schedule = (fn, delay, interval) => {const id = ++serial; tasks.set(id, {fn, at: now + delay, interval}); return id;};
  const window = {
    addEventListener: (name, fn) => {events[name] = fn;},
    performance: {now: () => now},
    setTimeout: (fn, delay) => schedule(fn, delay, 0),
    clearTimeout: id => tasks.delete(id),
    setInterval: (fn, delay) => schedule(fn, delay, delay),
    clearInterval: id => tasks.delete(id),
    matchMedia: () => ({matches: reducedMotion}),
    Shiny: {addCustomMessageHandler: (name, fn) => {handlers[name] = fn;}}
  };
vm.runInNewContext(fs.readFileSync(require('path').join(require('path').dirname(process.argv[2]), 'page-lifecycle.js'), 'utf8'), {window});
  vm.runInNewContext(code, {window, document, ready: fn => fn()});
  return {
    get overlay() {return overlay;},
    get busy() {return document.body.classList.contains('cerebro-dataset-is-loading');},
    setPane: value => {pane = value;},
    emit: (name, detail = {}) => events[name]({detail}),
    window,
    update: message => handlers.cerebro_dataset_load(Object.assign({label:'Dataset A',percent:10,detail:'Loading'},message)),
    tick: ms => {
      const end = now + ms;
      for (;;) {
        const next = Array.from(tasks).filter(([, t]) => t.at <= end).sort((a,b) => a[1].at - b[1].at)[0];
        if (!next) break;
        const [id, task] = next; now = task.at; tasks.delete(id);
        if(task.interval) tasks.set(id, {...task, at:now+task.interval});
        task.fn();
      }
      now = end;
    }
  };
}
const visible = s => s.overlay.classList.contains('is-visible');
// A fast load blocks outgoing interactions but never flashes a visible overlay.
{
  const s = session(); s.update(); assert(s.busy); assert(!s.overlay.hidden); assert(!visible(s));
  s.tick(80); s.update({done:true,percent:100}); assert(s.overlay.hidden); assert(!s.busy);
  s.tick(1000); assert(!visible(s));
}
// Repeated progress updates do not postpone revealing the card or restart time.
{
  const s = session(); s.update(); s.tick(90); s.update({percent:30}); s.tick(60); assert(visible(s));
  s.tick(100); s.update({done:true,percent:100}); s.tick(139); assert(visible(s));
  s.tick(1); assert(!visible(s)); assert(!s.overlay.hidden);
  s.tick(120); assert(s.overlay.hidden); assert(!s.busy);
}
// A subsequent load cancels both the completion hold and an in-flight fade.
for (const duringFade of [false,true]) {
  const s = session(); s.update(); s.tick(400); s.update({done:true,percent:100});
  if(duringFade) s.tick(40);
  s.update({label:'Dataset B',percent:5}); s.tick(300);
  assert(visible(s)); assert(!s.overlay.hidden); assert(s.busy);
  assert.strictEqual(s.overlay.querySelector('.cerebro-dataset-loading-label').textContent,'Dataset B');
  s.update({done:true,percent:100}); s.tick(360); assert(s.overlay.hidden);
}
// Reduced motion has no exit animation; the label remains literal text.
{
  const s = session(true); s.update({label:'Tissue <section> & nuclei'}); s.tick(500);
  assert.strictEqual(s.overlay.querySelector('.cerebro-dataset-loading-label').textContent,'Tissue <section> & nuclei');
  s.update({done:true,percent:100}); s.tick(0); assert(s.overlay.hidden);
}
console.log('PASS fast, slow, repeated progress, consecutive loads, interrupted fade and reduced motion');
// Dataset materialization must not uncover an outgoing or unfinished plot.
{
  const s = session();
  const pane = {dataset:{},querySelector:()=>null}; s.setPane(pane);
  s.update(); s.tick(500); s.update({done:true,percent:100}); s.tick(500);
  assert(s.busy); assert(visible(s));
  assert(Number.parseInt(s.overlay.querySelector('.cerebro-dataset-loading-percent').textContent,10) < 100);
  pane.dataset.cerebroPageReady = 'true'; s.emit('cerebro:page-ready'); s.tick(150);
  assert.strictEqual(s.overlay.querySelector('.cerebro-dataset-loading-percent').textContent,'100%');
  assert(!s.busy); assert(s.overlay.hidden);
}
// A terminal result releases the overlay without claiming a painted page.
{
  const s = session(), pane = {dataset:{},querySelector:()=>null};
  s.setPane(pane);
  s.update(); s.tick(500); s.update({done:true,percent:100});
  s.emit('cerebro:view-terminal',{page:'unrelated'}); s.tick(150);
  assert(s.busy);
  s.window.CerebroPageLifecycle.setTerminal(pane,true); s.emit('cerebro:view-terminal'); s.tick(150);
  assert(!s.busy); assert(s.overlay.hidden);
}
// A late completion from an older selection must neither relabel nor dismiss.
{
  const s = session(); s.window.CerebroDatasetContext = {acceptsProgress:m=>m.selection_nonce===2};
  s.update({label:'New dataset',selection_nonce:2}); s.tick(500);
  s.update({label:'Old dataset',selection_nonce:1,done:true}); s.tick(500);
  assert(s.busy); assert.strictEqual(s.overlay.querySelector('.cerebro-dataset-loading-label').textContent,'New dataset');
  s.update({selection_nonce:2,done:true}); s.tick(150); assert(!s.busy);
}
// Failed loads release the workspace so the picker can recover in-place.
{
  const s = session(); s.update(); s.tick(300);
  s.emit('cerebro:dataset-context', {phase:'error'});
  assert(!s.busy); assert(s.overlay.hidden);
  s.update({label:'Recovered dataset'}); s.tick(200); assert(visible(s));
  s.update({done:true}); s.tick(400); assert(!s.busy);
}
