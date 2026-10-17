'use strict';

// A transport-only host for the complete Viewer scripts. There is deliberately
// no visible canvas: drawing and pointer behavior are exercised in Chrome.
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

function wire(message) {
  const header = new TextEncoder().encode(JSON.stringify(message));
  const buffer = new ArrayBuffer(4 + Math.ceil(header.length / 4) * 4);
  new DataView(buffer).setUint32(0, header.length, true);
  new Uint8Array(buffer, 4, header.length).set(header);
  return buffer;
}

module.exports = function runtime(root) {
  const handlers = {}, requests = [], events = {}, hosts = {}, fetches = [];
  const document = {
    readyState: 'complete',
    getElementById: id => hosts[id] || null,
    querySelector: () => null, querySelectorAll: () => [],
    addEventListener() {}
  };
  const jq = {on() { return jq; }, off() { return jq; }};
  const scope = {console, performance, TextDecoder, TextEncoder, ArrayBuffer,
    DataView, Uint8Array, Int8Array, Int16Array, Int32Array, Uint16Array,
    Uint32Array, Float32Array, Float64Array, Set, Map, document,
    requestAnimationFrame() {}, setTimeout() {}, clearTimeout() {}, setInterval() {},
    ResizeObserver: class { observe() {} },
    CustomEvent: class {constructor(type, options) {this.type=type;this.detail=options.detail;}},
    addEventListener(name, fn) {(events[name] ||= []).push(fn);},
    dispatchEvent(event) {(events[event.type] || []).forEach(fn=>fn(event));},
    jQuery: () => jq,
    Shiny: {addCustomMessageHandler(name, fn) { handlers[name] = fn; },
      setInputValue(name, value) {requests.push({name, value});}},
    fetch: async url => {fetches.push(url);throw Error('Unexpected fetch: '+url);}
  };
  scope.window = scope;
  vm.createContext(scope);
  for (const name of ['dataset-context.js', 'background-model.js', 'cell_views_state.js', 'cell_views_wire.js', 'cell_views.js']) {
    vm.runInContext(fs.readFileSync(path.join(root, name), 'utf8'), scope, {filename:name});
  }
  const context = {epoch:'test-session',dataset_key:'A',generation:1};
  scope.CerebroDatasetContext.receive({phase:'ready',dataset_context:context});
  function send(name, message) {return handlers[name](message);}
  async function settle() {for(let i=0;i<12;i++) await new Promise(setImmediate);}
  return {scope, handlers, requests, hosts, fetches, context, send, settle,
    binary(name, message) {send(name, wire(message));},
    summary(id='spatial_projection') {return scope.cerebroCellViews.summary(id);},
    dataset(dataset_key, generation) {
      Object.assign(context, {dataset_key, generation});
      scope.CerebroDatasetContext.receive({phase:'pending',dataset_context:{...context}});
      scope.CerebroDatasetContext.receive({phase:'ready',dataset_context:{...context}});
    }};
};
