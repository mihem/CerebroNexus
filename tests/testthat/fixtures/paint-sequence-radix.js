// The paint sequence of a panel of some cells (panelPaintSequence()) is found by a
// radix sort of its cells' ranks. It is the sequence the float-key sort it
// replaced found, whose text is kept here as the reference: positions by the
// rank of their cell, ties (cells outside the order, repeated cells) by position.
const fs = require('fs'), assert = require('assert/strict');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(from, to) {
  const start = source.indexOf(from), end = source.indexOf(to, start);
  assert.ok(start >= 0 && end > start, 'missing ' + from);
  return source.slice(start, end);
}
const REFERENCE = `  function panelPaintSequence(cells, order) {
    var cached = paintSequences && paintSequences.get(cells);
    if (cached && cached.order === order) return cached.seq;
    if (paintRank.order !== order) {
      var rank = new Int32Array(D.n);
      for (var r = 0; r < order.length; r++) rank[order[r]] = r;
      paintRank = { order: order, rank: rank };
    }
    var ranks = paintRank.rank, k = cells.length, pos;
    // rank * k + position sorts numerically, without a comparator.
    var keys = new Float64Array(k);
    for (pos = 0; pos < k; pos++) keys[pos] = ranks[cells[pos]] * k + pos;
    keys.sort();
    var seq = new Int32Array(k);
    for (pos = 0; pos < k; pos++) seq[pos] = keys[pos] % k;
    if (paintSequences) paintSequences.set(cells, { order: order, seq: seq });
    return seq;
  }
`;
var D = {n: 0};
function sequenceOf(code, name, cells, order) {
  // Each version has its own caches.
  const run = new Function('D', 'paintSequences', 'paintRank', code + '; return {call: function (c, o) { return ' + name + '(c, o); }};');
  return run(D, new WeakMap(), {order: null, rank: null}).call(cells, order);
}
const newCode = between('  function panelPaintSequence(', '  // A paint order the GPU can follow');
let seed = 3;
const rnd = () => { seed = (Math.imul(seed, 1103515245) + 12345) >>> 0; return seed / 4294967296; };
function shuffled(n) { const a = Uint32Array.from({length: n}, (_, i) => i); for (let i = n - 1; i > 0; i--) { const j = Math.floor(rnd() * (i + 1)); [a[i], a[j]] = [a[j], a[i]]; } return a; }
function check(name, n, orderLength, makeCells) {
  D.n = n;
  const order = shuffled(n).subarray(0, orderLength);
  const cells = makeCells(n);
  const before = sequenceOf(REFERENCE, 'panelPaintSequence', cells, order);
  const after = sequenceOf(newCode, 'panelPaintSequence', cells, order);
  assert.ok(after instanceof Int32Array, name + ' is an Int32Array');
  assert.deepEqual(Array.from(after), Array.from(before), name);
}
const some = k => n => Uint32Array.from({length: k}, () => Math.floor(rnd() * n));
check('a small panel', 50, 50, n => Uint32Array.from({length: 20}, (_, i) => i * 2));
check('one cell', 10, 10, () => Uint32Array.from([4]));
check('no cells', 10, 10, () => new Uint32Array(0));
check('ranks below 2,048', 2000, 2000, n => Uint32Array.from({length: 700}, (_, i) => i * 2));
check('ranks past 2,048 and 4.2 million digits', 50000, 50000, some(20000));
check('repeated cells tie by position', 3000, 3000, some(6000));
check('cells missing from the order rank 0', 5000, 3000, n => Uint32Array.from({length: 4000}, (_, i) => i));
check('an order of one', 8, 1, () => Uint32Array.from([0, 1, 2]));
check('a million cells, a big panel', 1200000, 1200000, some(66000));
check('past the third digit', 4300000, 4300000, n => Uint32Array.from({length: 1000}, (_, i) => n - 1 - i * 3));
// The same sequence is kept for the same cells and order.
D.n = 100; const order = shuffled(100), cells = Uint32Array.from({length: 30}, (_, i) => i * 3);
const kept = new Function('D', 'paintSequences', 'paintRank', newCode + '; return panelPaintSequence;')(D, new WeakMap(), {order: null, rank: null});
assert.equal(kept(cells, order), kept(cells, order), 'cached by the cells array and order');
console.log('PASS a panel paint sequence by radix sort is the one the float-key sort found');
