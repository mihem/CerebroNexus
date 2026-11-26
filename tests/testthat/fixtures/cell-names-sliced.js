// The cell names of a grid are decoded a slice at a time (decodeCellNames()), so
// no task holds the page for the 0.6 to 0.77 s the 3.9 million names of the Allen
// atlas took. The names are the ones one decode and split gives.
const fs = require('fs'), assert = require('assert/strict');
const source = fs.readFileSync(process.argv[2], 'utf8');
const start = source.indexOf('  function decodeCellNames(buffer) {');
const end = source.indexOf('  function fetchCellNames(', start);
assert.ok(start >= 0 && end > start, 'decodeCellNames found');
// Every check of the clock says a slice has run out of time.
let clock = 0;
const performance = {now: () => (clock += 20)};
eval(source.slice(start, end));
const whole = buffer => {
  const names = new TextDecoder('utf-8').decode(buffer).split('\0');
  names.pop();
  return names;
};
let seed = 5;
const rnd = () => { seed = (Math.imul(seed, 1103515245) + 12345) >>> 0; return seed / 4294967296; };
const encode = names => new TextEncoder().encode(names.map(n => n + '\0').join('')).buffer;
async function check(label, buffer) {
  const sliced = await decodeCellNames(buffer);
  assert.deepEqual(sliced, whole(buffer), label);
}
(async () => {
  await check('a few names', encode(['a', 'b', 'c']));
  await check('one name', encode(['only']));
  await check('no names', new Uint8Array(0).buffer);
  await check('empty names between others', encode(['a', '', '', 'b', '']));
  await check('multi-byte names', encode(['céllule', '細胞', '😀cell', 'ß', 'Ωmega']));
  // A byte-order mark at the start of the file is dropped once; one inside a name stays.
  const bom = new Uint8Array([0xEF, 0xBB, 0xBF]);
  const body = new TextEncoder().encode('a\0b\0');
  const withBom = new Uint8Array(bom.length + body.length); withBom.set(bom); withBom.set(body, bom.length);
  await check('a byte-order mark at the start', withBom.buffer);
  const inside = new TextEncoder().encode('a\0\uFEFFb\0');
  await check('a byte-order mark inside a name', inside.buffer);
  // Several slices: names of every length, some multi-byte, cut anywhere by the
  // slice size, 3.5 MB in all.
  const names = [];
  let bytes = 0;
  while (bytes < 3.5 * 1024 * 1024) {
    const len = 1 + Math.floor(rnd() * 40);
    let name = '';
    for (let i = 0; i < len; i++) name += rnd() < 0.05 ? '細' : String.fromCharCode(33 + Math.floor(rnd() * 90));
    names.push(name); bytes += new TextEncoder().encode(name).length + 1;
  }
  await check('3.5 MB in several slices', encode(names));
  // A name that ends exactly on a slice boundary.
  const filler = 'x'.repeat((1 << 20) - 1);
  await check('a name ending on the first slice boundary', encode([filler, 'next', 'after']));
  await check('a name straddling the first slice boundary', encode(['ab', filler, 'next']));
  // Not NUL-terminated: the last name goes with the pop, as it did.
  const loose = new TextEncoder().encode('a\0b\0c').buffer;
  await check('not NUL-terminated', loose);
  // Slices give way to the event loop: other tasks run between them.
  let ran = 0;
  const timer = setInterval(() => { ran++; }, 0);
  clock = 0;
  await decodeCellNames(encode(names));
  clearInterval(timer);
  assert.ok(ran > 0, 'other tasks ran between the slices');
  console.log('PASS the cell names decoded in slices are the names one decode gives, and the event loop runs between them');
})().catch(error => { console.error(error); process.exit(1); });
