const assert = require('node:assert/strict');
const fs = require('node:fs');
const zlib = require('node:zlib');
require(require('node:path').resolve(process.argv[2]));
const input = JSON.parse(fs.readFileSync(process.argv[3], 'utf8'));
const loader = globalThis.CerebroExpressionAssets;
const resources = input.rows.map((row, i) => ({
  protocol: 'expression-f64-gzip-v1', gene: row.gene, cells: row.cells,
  bytes: fs.statSync(`${input.root}/${row.path}`).size,
  url: `cerebro-expression-test/${String(i + 1).padStart(6, '0')}.bin.gz?v=${'a'.repeat(32)}`,
  pack_dataset_fingerprint: input.fingerprint, cell_order_fingerprint: input.cells,
  path: row.path
}));
const byUrl = new Map(resources.map(r => [r.url, r]));
let requests = 0;
globalThis.fetch = async url => {
  requests++;
  return new Response(fs.readFileSync(`${input.root}/${byUrl.get(url).path}`));
};
(async () => {
  const [first, again] = await Promise.all([loader.load(resources[0]), loader.load(resources[0])]);
  assert.equal(first, again);
  assert.equal(requests, 1);
  assert.equal(first[0], 1 + 2 ** -40);
  assert.equal(first[17], Infinity);
  assert.ok(Number.isNaN(first[18]));
  assert.equal(first[19], -3);
  assert.equal(await loader.load(resources[0]), first);
  assert.equal(requests, 1);
  const dense = await loader.load(resources[1]);
  assert.equal(dense[19], 20 / 7);
  assert.deepEqual(await loader.load(resources[2]), new Float64Array(20));
  const bytes = zlib.gunzipSync(fs.readFileSync(`${input.root}/${resources[0].path}`));
  const buffer = Uint8Array.from(bytes).buffer;
  assert.throws(() => loader.decode(buffer, 21), /shape mismatch/);
  assert.throws(() => loader.decode(buffer.slice(0, -1), 20), /byte count/);
  new DataView(buffer).setUint32(16, 20, true);
  assert.throws(() => loader.decode(buffer, 20), /index mismatch/);
  const wrong = {...resources[0], cell_order_fingerprint: 'other-order'};
  assert.equal(loader.peek(wrong), null);
  await assert.rejects(loader.load({...resources[0], url: 'https://evil.example/a'}), /Invalid/);
  assert.ok(loader.stats().bytes <= loader.stats().budget);
  loader.clear();
  assert.equal(loader.peek(resources[0]), null);
  globalThis.fetch = async () => new Response('missing', {status: 404});
  await assert.rejects(loader.load(resources[0]), /404/);
  assert.equal(loader.peek(resources[0]), null);
  console.log('expression assets OK');
})().catch(error => { console.error(error); process.exitCode = 1; });
