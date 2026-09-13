(function (global) {
  'use strict';

  function percentile(values, fraction) {
    var sorted = values.slice().sort(function (a, b) { return a - b; });
    return sorted[Math.min(sorted.length - 1,
      Math.max(0, Math.ceil(sorted.length * fraction) - 1))];
  }

  async function timed(renderer, action) {
    var started = performance.now();
    action();
    await renderer.idle();
    return performance.now() - started;
  }

  function pointData(count, rgb) {
    var positions = new Float32Array(count * 2);
    var colors = new Uint8Array(count * 4);
    var layers = new Uint32Array(count);
    var state = 0x12345678;
    for (var i = 0; i < count; i++) {
      state ^= state << 13; state ^= state >>> 17; state ^= state << 5;
      var x = state >>> 0;
      state ^= state << 13; state ^= state >>> 17; state ^= state << 5;
      var y = state >>> 0;
      positions[i * 2] = x / 4294967295;
      positions[i * 2 + 1] = y / 4294967295;
      if (rgb) {
        colors[i * 4] = x & 255;
        colors[i * 4 + 1] = y & 255;
        colors[i * 4 + 2] = (x >>> 8) & 255;
        layers[i] = (x & 3) ? 0 : 1;
      } else {
        var group = x % 10;
        colors[i * 4] = (group * 47 + 31) & 255;
        colors[i * 4 + 1] = (group * 83 + 71) & 255;
        colors[i * 4 + 2] = (group * 131 + 19) & 255;
      }
      colors[i * 4 + 3] = 128;
    }
    return {
      positions: positions,
      colors: colors,
      layers: layers,
      count: count,
      foreground: rgb
    };
  }

  async function frames(renderer, count, pointSize, batchSize) {
    var values = [];
    for (var i = 0; i < count; i++) {
      values.push((await timed(renderer, function () {
        for (var j = 0; j < batchSize; j++) {
          var frame = i * batchSize + j;
          if (!renderer.draw({
            view: {
              cx: 0.48 + (frame % 3) * 0.01,
              cy: 0.52 - (frame % 4) * 0.01,
              span: 0.62 + (frame % 5) * 0.07
            },
            rect: { x: 10, y: 10, width: 1260, height: 700 },
            pointSize: pointSize,
            border: null
          })) throw new Error('The renderer rejected a benchmark frame.');
        }
      })) / batchSize);
    }
    return values;
  }

  global.runMillionCellBenchmark = async function (options) {
    options = options || {};
    var count = Math.max(1, Number(options.count) || 1000000);
    var repeats = Math.max(3, Number(options.repeats) || 15);
    var batchSize = Math.max(1, Number(options.batchSize) || 1);
    var canvas = document.getElementById('benchmark-canvas');
    if (!global.CerebroPointRenderer) throw new Error('Renderer module not loaded.');

    var initializeStarted = performance.now();
    var renderer = global.CerebroPointRenderer.create(canvas);
    await renderer.ready;
    var initializeMs = performance.now() - initializeStarted;
    if (!renderer.isReady()) throw new Error('Renderer did not become ready.');
    renderer.resize(1280, 720, 1);

    var buildStarted = performance.now();
    var categorical = pointData(count, false);
    var categoricalBuildMs = performance.now() - buildStarted;
    var uploadMs = await timed(renderer, function () {
      renderer.setData(categorical);
    });
    var firstFrameMs = (await frames(renderer, 1, 1, 1))[0];
    var categoricalFrames = await frames(renderer, repeats, 1, batchSize);
    var categoricalImageBytes = Math.max(0,
      Math.round((canvas.toDataURL('image/png').length - 22) * 0.75));

    buildStarted = performance.now();
    var rgb = pointData(count, true);
    var rgbBuildMs = performance.now() - buildStarted;
    var rgbUploadMs = await timed(renderer, function () {
      renderer.setData(rgb);
    });
    var rgbFrames = await frames(renderer, repeats, 1, batchSize);
    var stats = renderer.stats();
    var imageBytes = Math.max(0,
      Math.round((canvas.toDataURL('image/png').length - 22) * 0.75));

    return {
      backend: stats.backend,
      points: count,
      repeats: repeats,
      framesPerSample: batchSize,
      bufferMiB: (count * 16) / 1048576,
      initializeMs: initializeMs,
      categoricalBuildMs: categoricalBuildMs,
      uploadMs: uploadMs,
      firstFrameMs: firstFrameMs,
      panZoomMedianMs: percentile(categoricalFrames, 0.5),
      panZoomP95Ms: percentile(categoricalFrames, 0.95),
      categoricalImageBytes: categoricalImageBytes,
      rgbBuildMs: rgbBuildMs,
      rgbUploadMs: rgbUploadMs,
      rgbMedianMs: percentile(rgbFrames, 0.5),
      rgbP95Ms: percentile(rgbFrames, 0.95),
      imageBytes: imageBytes,
      contextLost: !!stats.contextLost,
      gpuError: stats.error || 0,
      adapter: stats.adapter || '',
      userAgent: navigator.userAgent
    };
  };
})(window);
