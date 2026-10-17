const assert = require('assert/strict');
const fs = require('fs');
const vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
function setup(reduced = false) {
  let now = 0, serial = 0, reloads = 0, removed = false, revealed = false;
  const events = new Map(), timers = new Map();
  const pane = {dataset: {}, querySelector: () => null};
  const text = {textContent:'Opening workspace…'};
  const retry = {hidden: true, addEventListener: (_, fn) => { retry.click = fn; }};
  const cover = {style:{}, querySelector: key => key.includes('message') ? text : retry,
    remove: () => {removed = true;}};
  const document = {readyState:'complete', getElementById: () => cover,
    querySelector: () => pane, documentElement: {classList: {remove: () => {revealed = true;}}}};
  const window = {addEventListener: (key, fn) => events.set(key, fn),
    removeEventListener: key => events.delete(key)};
vm.runInNewContext(fs.readFileSync(require('path').join(require('path').dirname(process.argv[2]), 'page-lifecycle.js'), 'utf8'), {window});
  vm.runInNewContext(source, {document, window,
    setTimeout: (fn, delay) => {const id = ++serial;timers.set(id,{fn,at:now+delay});return id;},
    clearTimeout: id => timers.delete(id), requestAnimationFrame: fn => fn(),
    matchMedia: () => ({matches:reduced}), location: {reload: () => reloads++}});
  return {pane, retry, text, events,
    get removed() {return removed;}, get revealed() {return revealed;}, get reloads() {return reloads;},
    emit: (key, detail) => events.get(key)?.({detail}),
    tick(ms) {now += ms;for(const [id,t] of timers) if(t.at<=now){timers.delete(id);t.fn();}}
  };
}
// An unrelated ready event must not expose the incomplete current page.
const a = setup();a.emit('cerebro:page-ready');assert(!a.revealed);
assert(a.retry.hidden);a.tick(15000);assert(!a.retry.hidden);
a.retry.click();assert.equal(a.reloads,1);
a.pane.dataset.cerebroPageReady='true';a.emit('cerebro:page-ready');
assert(a.revealed);assert(!a.removed);a.tick(120);assert(a.removed);
assert.equal(a.events.size,0);
// Reduced motion, explicit terminal results and failed loads all release it.
const b = setup(true);
b.emit('cerebro:view-terminal', {page:'unrelated'});assert(!b.revealed);
b.pane.dataset.cerebroPageTerminal='true';
b.emit('cerebro:view-terminal');b.tick(0);assert(b.removed);
const c = setup();c.emit('cerebro:dataset-context',{phase:'pending'});assert(!c.revealed);
c.emit('cerebro:dataset-context',{phase:'error'});c.tick(120);assert(c.removed);
console.log('PASS startup readiness, slow recovery, terminal, error and reduced motion');
