const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const source = fs.readFileSync(process.argv[2], "utf8");
const token = "ab".repeat(32);

async function check(pathname, search, prefix, key = "2f0a") {
  const requests = [];
  const view = {
    version: 1, id: "overview_projection", cells: 1,
    colors: ["#aabbcc"], code_map: [0], point_size: 5, point_opacity: 1,
    projection: { url: "cerebro-first-view/2f0a.abcdef.xy.bin", bytes: 8 },
    categories: { url: "cerebro-first-view/2f0a.abcdef.codes.bin", bytes: 1 }
  };
  const window = {
    location: { pathname, search }, Promise, TextEncoder,
    requestAnimationFrame() {}, addEventListener() {}, removeEventListener() {},
    fetch(url) {
      requests.push(url);
      return Promise.resolve(url.endsWith(".json") ? {
        ok: true, json: async () => view
      } : {
        ok: true, arrayBuffer: async () => new ArrayBuffer(url.endsWith(".xy.bin") ? 8 : 1)
      });
    }
  };
  vm.runInNewContext(source, {
    window, document: { readyState: "complete", documentElement: {} },
    Promise, URLSearchParams, TextEncoder, Float32Array, Uint8Array, Uint32Array, Float64Array
  });
  await new Promise(setImmediate);
  assert.deepEqual(requests, [
    prefix + key + ".json",
    prefix + "2f0a.abcdef.xy.bin",
    prefix + "2f0a.abcdef.codes.bin"
  ]);
}

(async () => {
  await check("/", "", "cerebro-first-view/");
  await check("/", "?token=" + token, "cerebro-first-view-" + token + "/");
  await check("/cerebro-" + token + ".html", "", "cerebro-first-view-" + token + "/");
  await check("/proxy/cerebro-" + token + ".html", "", "cerebro-first-view-" + token + "/",
    Buffer.from("/proxy/\n").toString("hex"));
  console.log("PASS protected preview URLs");
})().catch(error => { console.error(error); process.exitCode = 1; });
