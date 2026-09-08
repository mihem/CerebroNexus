#!/usr/bin/env node

'use strict';

const fs = require('fs');
const path = require('path');
const root = path.resolve(process.argv[2] || '.');
const sourcePath = path.join(root, 'inst/viewer/www/cell_views.js');
let source = fs.readFileSync(sourcePath, 'utf8');
const closeAt = source.lastIndexOf('\n})();');
if (closeAt < 0) throw new Error('cell_views.js closure not found');
const hooks = `
  shown = function () { return true; };
  window.__viewerBench = {
    setData: function (value) { D = value; },
    buildHitGrid: buildHitGrid,
    nearest: nearest,
    nearestLinear: nearestLinear
  };
`;
source = source.slice(0, closeAt) + hooks + source.slice(closeAt);

global.window = global;
window.devicePixelRatio = 1;
window.addEventListener = () => {};
global.document = {
  readyState: 'loading',
  addEventListener: () => {},
  getElementById: () => null,
  querySelectorAll: () => [],
  createElement: () => ({ getContext: () => null })
};
eval(source);

function median(values) {
  const sorted = values.slice().sort((left, right) => left - right);
  return sorted[Math.floor(sorted.length / 2)];
}

function elapsedMs(work, repeats = 7) {
  const values = [];
  for (let repeat = 0; repeat < repeats; repeat++) {
    const start = process.hrtime.bigint();
    work();
    values.push(Number(process.hrtime.bigint() - start) / 1e6);
  }
  return median(values);
}

const n = 500000;
const width = 1000;
const height = 1000;
const panel = {
  W: width,
  H: height,
  sx: new Float32Array(n),
  sy: new Float32Array(n),
  ok: new Uint8Array(n)
};
panel.ok.fill(1);
let random = 20260908;
for (let index = 0; index < n; index++) {
  random = (Math.imul(random, 1664525) + 1013904223) >>> 0;
  panel.sx[index] = random / 4294967296 * width;
  random = (Math.imul(random, 1664525) + 1013904223) >>> 0;
  panel.sy[index] = random / 4294967296 * height;
}
__viewerBench.setData({ n: n });

const queries = [];
for (let index = 0; index < 200; index++) {
  const cell = Math.imul(index + 1, 2654435761) >>> 0;
  const at = cell % n;
  queries.push([panel.sx[at], panel.sy[at]]);
}
function runLookup(lookup) {
  let checksum = 0;
  for (const query of queries) checksum += lookup(panel, query[0], query[1]);
  return checksum;
}

__viewerBench.buildHitGrid(panel);
const indexedCheck = runLookup(__viewerBench.nearest);
const linearCheck = runLookup(__viewerBench.nearestLinear);
if (indexedCheck !== linearCheck) throw new Error('indexed lookup mismatch');

const linearMs = elapsedMs(() => runLookup(__viewerBench.nearestLinear));
const indexedMs = elapsedMs(() => runLookup(__viewerBench.nearest));
const buildMs = elapsedMs(() => {
  panel._hitGrid = null;
  __viewerBench.buildHitGrid(panel);
}, 5);
const perEventBefore = linearMs / queries.length;
const perEventAfter = indexedMs / queries.length;

const panels = 4;
const cellPasses = 3;
const output = {
  hit_test: {
    scale: `${n.toLocaleString('en-US')} cells; ${queries.length} pointer events`,
    before_ms_per_event: perEventBefore,
    after_ms_per_event: perEventAfter,
    time_delta_pct: (perEventAfter / perEventBefore - 1) * 100,
    index_build_ms: buildMs,
    break_even_events: buildMs / (perEventBefore - perEventAfter)
  },
  hover_overlay: {
    scale: `${n.toLocaleString('en-US')} cells; ${panels} panels`,
    before_cell_iterations_per_hover: n * panels * cellPasses,
    after_cell_iterations_per_hover: 0,
    before_mask_bytes_per_hover: n * panels,
    after_mask_bytes_per_hover: 0
  }
};
console.log(JSON.stringify(output, null, 2));
