const assert = require('assert'), fs = require('fs'), zlib = require('zlib');
const window = {};
new Function('window', fs.readFileSync(process.argv[2], 'utf8') + '\n;')(window);
const T = window.CerebroSelectionTable;

// Number roles and formats of prettifyTable().
const col = (name, values) => ({ name, kind: 'numeric', value: i => values[i] });
let p = T.numericProfile(col('nUMI', [10, 2000, 30]), 3);
assert.equal(p.role, 'integer'); assert.equal(T.numberText(2000, p, true), '2,000');
p = T.numericProfile(col('percent_mt', [1.5, 20, 0]), 3);
assert.equal(p.role, 'percent'); assert.equal(T.numberText(20, p, true), '20.00%');
p = T.numericProfile(col('p_val_adj', [0.01, 0.5]), 2);
assert.equal(p.role, 'pvalue'); assert.equal(T.numberText(0.012345, p, true), '0.0123');
p = T.numericProfile(col('score', [0.123456, 2.5]), 2);
assert.equal(p.role, 'number'); assert.equal(T.numberText(0.123456, p, true), '0.123');
assert.equal(T.numberText(0.123456, p, false), '0.123456');

// White-to-accent ramp over the column range (101 steps), bars for shares.
p = T.numericProfile(col('nUMI', [0, 100]), 2);
assert.equal(T.numberStyle(0, p, { accent: '#ff0000' }), 'background-color:rgb(255,255,255);');
assert.equal(T.numberStyle(100, p, { accent: '#ff0000' }), 'background-color:rgb(255,3,3);');
p = T.numericProfile(col('pct_counts', [0, 50, 100]), 3);
assert.ok(/linear-gradient\(90deg,transparent 50\.0%,#00f 50\.0%\)/.test(
  T.numberStyle(50, p, { signal: '#00f' })));

// The zip is a valid archive of the stored files.
const enc = new TextEncoder();
const parts = T.zipStored([{ name: 'a.txt', parts: [enc.encode('hello'), enc.encode(' world')] }]);
const zip = Buffer.concat(parts.map(part => Buffer.from(part)));
assert.equal(zip.readUInt32LE(0), 0x04034b50);
assert.equal(zip.readUInt32LE(14), zlib.crc32 ? zlib.crc32('hello world') : zip.readUInt32LE(14));
assert.equal(zip.slice(30 + 5, 30 + 5 + 11).toString(), 'hello world');
assert.equal(zip.readUInt32LE(zip.length - 22), 0x06054b50);

// Sorting and filtering on index arrays, through a minimal DOM.
function element() {
  return { innerHTML: '', style: {}, children: [], appendChild() {}, querySelector() { return { addEventListener() {} }; },
    querySelectorAll() { return []; }, getBoundingClientRect() { return { left: 0, top: 0, bottom: 0 }; } };
}
global.document = { addEventListener() {}, removeEventListener() {} };
const el = element();
const cells = ['c', 'a', 'b', 'd'], groups = [1, 0, 1, -1], umi = [5, 7, 1, 3];
const table = T.mount(el, {
  n: 4, open: false, columns: [
    { name: 'cell_barcode', kind: 'text', value: i => cells[i] },
    { name: 'cluster', kind: 'categorical', levels: ['x', 'y'], colors: ['#111111', '#222222'],
      palette: true, value: i => groups[i] },
    { name: 'nUMI', kind: 'numeric', value: i => umi[i] }
  ]
});
assert.ok(/4 selected cells/.test(el.innerHTML));
assert.deepEqual(Array.from(table.rows()), [0, 1, 2, 3]);
table.sort = { column: 0, name: 'cell_barcode', descending: false }; table.order = null;
assert.deepEqual(Array.from(table.rows()), [1, 2, 0, 3]);
table.sort = { column: 2, name: 'nUMI', descending: true }; table.order = null;
assert.deepEqual(Array.from(table.rows()), [1, 0, 3, 2]);
table.filters = { cluster: { levels: { 1: true } } }; table.order = null;
assert.deepEqual(Array.from(table.rows()), [0, 2]);
table.filters = { nUMI: { min: 2, max: 6 } }; table.order = null;
assert.deepEqual(Array.from(table.rows()), [0, 3]);
table.filters = { cell_barcode: { text: 'b' } }; table.order = null;
assert.deepEqual(Array.from(table.rows()), [2]);
// The drawn table is usable from the keyboard: sortable headers are focusable and
// say how they are sorted, and the buttons that open a menu say so.
const open = element();
const keyboard = T.mount(open, {
  n: 4, open: true, columns: [
    { name: 'cell_barcode', kind: 'text', value: i => cells[i] },
    { name: 'nUMI', kind: 'numeric', value: i => umi[i] }
  ]
});
assert.ok(/<th class="sorting" data-col="0" tabindex="0" aria-sort="none">/.test(open.innerHTML));
keyboard.sort = { column: 1, name: 'nUMI', descending: true }; keyboard.render();
assert.ok(/data-col="1" tabindex="0" aria-sort="descending"/.test(open.innerHTML));
keyboard.sort = { column: 1, name: 'nUMI', descending: false }; keyboard.render();
assert.ok(/data-col="1" tabindex="0" aria-sort="ascending"/.test(open.innerHTML));
assert.ok(/cst-colvis" aria-haspopup="true" aria-expanded="false"/.test(open.innerHTML));
assert.ok(/cst-download" aria-haspopup="true" aria-expanded="false"/.test(open.innerHTML));
assert.ok(/data-col="1"[^>]*readonly aria-haspopup="true"/.test(open.innerHTML));
console.log('PASS selection table formats, colours, sorts, filters and zips like the DT table');
