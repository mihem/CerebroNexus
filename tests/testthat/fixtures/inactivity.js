const assert = require('assert');
const timers = new Map(), events = {}, sent = [];
let now = 0, serial = 0, focused = 'original';
const warning = {open:false, showModal(){this.open=true;}, close(){this.open=false;},
  addEventListener(name,fn){events['dialog:'+name]=fn;}};
const button = {focus(){focused='continue';}, addEventListener(name,fn){events['button:'+name]=fn;}};
const countdown = {textContent:''};
const original = {isConnected:true,focus(){focused='original';}};
const classes = new Set();
global.document = {documentElement:{classList:{contains:name=>classes.has(name)}},
  activeElement:original, dispatchEvent(){}, getElementById(id){
  return {'session-timeout-warning':warning,'session-timeout-countdown':countdown,'session-timeout-continue':button}[id];
}};
global.CustomEvent = function(){};
global.window = {addEventListener:(name,fn)=>events[name]=fn};
global.Shiny = {setInputValue:(id,value)=>sent.push([id,value])};
global.setTimeout = (fn,delay)=>{ const id=++serial;timers.set(id,{fn,at:now+delay});return id; };
global.clearTimeout = id=>timers.delete(id);
global.setInterval = ()=>++serial;
global.clearInterval = ()=>{};
Date.now = ()=>now;
function advance(ms) {
  const target=now+ms;
  for (;;) {
    const next=[...timers].sort((a,b)=>a[1].at-b[1].at)[0];
    if (!next || next[1].at>target) break;
    timers.delete(next[0]);now=next[1].at;next[1].fn();
  }
  now=target;
}
eval(require('fs').readFileSync(process.argv[2],'utf8'));
advance(540000);
assert(warning.open);assert.equal(focused,'continue');assert.equal(countdown.textContent,60);
events['button:click']();assert(!warning.open);assert.equal(focused,'original');
advance(60000);assert.equal(sent.length,0,'old expiry must be cancelled');
events.keydown();advance(540000);assert(warning.open);
let cancelled=false;events['dialog:cancel']({preventDefault(){cancelled=true;}});
assert(cancelled);assert(!warning.open);
advance(600000);assert.deepEqual(sent,[['timeOut','600s']]);assert(!warning.open);
// Denial closes an existing warning and activity cannot restart expiry.
events.mousemove();advance(540000);assert(warning.open);
classes.add('cerebro-access-is-denied');events['cerebro:access-denied']();
assert(!warning.open);assert.equal(timers.size,0);
events.keydown();advance(600000);
assert.equal(timers.size,0);assert.deepEqual(sent,[['timeOut','600s']]);
