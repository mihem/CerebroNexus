/* Lossless, gene-sized Viewer Pack assets. No renderer or Shiny dependency. */
(function (root) {
  'use strict';
  var cache = new Map(), pending = new Map(), bytes = 0;
  var budget = 64 * 1024 * 1024;
  var metrics = { requests: 0, hits: 0, transferredBytes: 0, decodeMs: 0 };
  function key(resource) {
    if (!resource || resource.protocol !== 'expression-f64-gzip-v1' ||
        !Number.isSafeInteger(resource.cells) || resource.cells < 1 ||
        !Number.isSafeInteger(resource.bytes) || resource.bytes < 1 ||
        !/^[a-zA-Z0-9_-]+\/[0-9]+\.bin\.gz\?v=[a-f0-9]{32}$/.test(resource.url) ||
        !resource.pack_dataset_fingerprint || !resource.cell_order_fingerprint) {
      throw new Error('Invalid expression resource');
    }
    return [resource.pack_dataset_fingerprint, resource.cell_order_fingerprint,
      resource.gene, resource.url, resource.cells].join('|');
  }
  function decode(buffer, cells) {
    if (buffer.byteLength < 16) throw new Error('Truncated expression header');
    var header = new DataView(buffer), count = header.getUint32(8, true);
    var sparse = header.getUint32(12, true), offset = 16;
    if (header.getUint32(0, true) !== 0x31454743 ||
        header.getUint32(4, true) !== cells || count > cells || sparse > 1 ||
        (!sparse && count !== cells)) throw new Error('Expression shape mismatch');
    if (sparse) offset += count * 4 + (8 - (count * 4) % 8) % 8;
    if (buffer.byteLength !== offset + count * 8) {
      throw new Error('Expression byte count mismatch');
    }
    var input = new DataView(buffer, offset), values = new Float64Array(cells);
    var previous = -1;
    for (var i = 0; i < count; i++) {
      var at = sparse ? header.getUint32(16 + i * 4, true) : i;
      if (at <= previous || at >= cells) throw new Error('Expression index mismatch');
      values[at] = input.getFloat64(i * 8, true);
      previous = at;
    }
    return values;
  }
  function peek(resource) {
    var id = key(resource), found = cache.get(id);
    if (found) { cache.delete(id); cache.set(id, found); }
    return found || null;
  }
  function load(resource) {
    var id;
    try { id = key(resource); } catch (error) { return Promise.reject(error); }
    var found = peek(resource);
    if (found) { metrics.hits++; return Promise.resolve(found); }
    if (pending.has(id)) return pending.get(id);
    metrics.requests++;
    var promise = root.fetch(resource.url, { credentials: 'same-origin' })
      .then(function (response) {
        if (!response.ok) throw new Error('Expression HTTP ' + response.status);
        return response.arrayBuffer();
      }).then(function (compressed) {
        if (compressed.byteLength !== resource.bytes) {
          throw new Error('Expression compressed size mismatch');
        }
        metrics.transferredBytes += compressed.byteLength;
        var started = performance.now();
        var stream = new Blob([compressed]).stream().pipeThrough(new DecompressionStream('gzip'));
        return new Response(stream).arrayBuffer().then(function (buffer) {
          var values = decode(buffer, resource.cells);
          metrics.decodeMs += performance.now() - started;
          if (values.byteLength <= budget) {
            while (bytes + values.byteLength > budget && cache.size) {
              var oldest = cache.keys().next().value;
              bytes -= cache.get(oldest).byteLength; cache.delete(oldest);
            }
            cache.set(id, values); bytes += values.byteLength;
          }
          return values;
        });
      }).finally(function () { pending.delete(id); });
    pending.set(id, promise);
    return promise;
  }
  // gene -> resource maps, one per Pack. Built only from a verified resource
  // the server handed out, so every derived URL stays in its directory.
  var indexes = new Map();
  var INDEX_URL = /^([a-zA-Z0-9_-]+\/)index\.json\?v=[a-f0-9]{32}$/;
  var FILE = /^[0-9]+\.bin\.gz$/, MD5 = /^[a-f0-9]{32}$/;
  function indexKey(resource) {
    var index = resource && resource.index;
    if (!index || !INDEX_URL.test(index.url)) return null;
    return [resource.pack_dataset_fingerprint, resource.cell_order_fingerprint,
      resource.cells, index.url].join('|');
  }
  function buildIndex(resource, json) {
    var dir = INDEX_URL.exec(resource.index.url)[1], genes = new Map();
    if (!json || json.protocol !== 'expression-index-v1' ||
        json.cells !== resource.cells || !Array.isArray(json.genes)) {
      throw new Error('Invalid expression index');
    }
    var n = json.genes.length;
    ['files', 'bytes', 'checksums', 'low', 'high'].forEach(function (name) {
      if (!Array.isArray(json[name]) || json[name].length !== n) {
        throw new Error('Invalid expression index');
      }
    });
    for (var i = 0; i < n; i++) {
      if (!FILE.test(json.files[i]) || !MD5.test(json.checksums[i])) continue;
      genes.set(String(json.genes[i]), {
        protocol: 'expression-f64-gzip-v1', gene: String(json.genes[i]),
        url: dir + json.files[i] + '?v=' + json.checksums[i],
        cells: resource.cells, bytes: json.bytes[i], checksum: json.checksums[i],
        range: [json.low[i], json.high[i]],
        dataset_fingerprint: resource.dataset_fingerprint,
        cell_order_fingerprint: resource.cell_order_fingerprint,
        pack_dataset_fingerprint: resource.pack_dataset_fingerprint,
        index: resource.index
      });
    }
    return genes;
  }
  function loadIndex(resource) {
    var id = indexKey(resource);
    if (!id) return Promise.resolve(null);
    var entry = indexes.get(id);
    if (entry) return entry.promise;
    entry = {map: null};
    entry.promise = root.fetch(resource.index.url, { credentials: 'same-origin' })
      .then(function (response) {
        if (!response.ok) throw new Error('Expression index HTTP ' + response.status);
        return response.json();
      }).then(function (json) {
        entry.map = buildIndex(resource, json);
        return entry.map;
      }).catch(function (error) { indexes.delete(id); throw error; });
    indexes.set(id, entry);
    return entry.promise;
  }
  // Synchronous: the resource for `gene` in the Pack of `resource`, or null.
  function resourceFor(resource, gene) {
    var id = indexKey(resource), entry = id && indexes.get(id);
    return (entry && entry.map && entry.map.get(String(gene))) || null;
  }
  root.CerebroExpressionAssets = {
    load: load, peek: peek, decode: decode,
    loadIndex: loadIndex, resourceFor: resourceFor,
    stats: function () { return Object.assign({ bytes: bytes, budget: budget }, metrics); },
    clear: function () { cache.clear(); indexes.clear(); bytes = 0; }
  };
})(typeof window !== 'undefined' ? window : globalThis);
