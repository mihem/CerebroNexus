"use strict";

const fs = require("fs");
const path = require("path");
const vm = require("vm");

const delegated = {};
const nativeHandlers = {};
const inputs = [];
const custom = {};
const frames = [];
const arcs = [];
const imageLoads = [];
const imageDraws = [];
const rasterBuildDraws = [];
const imageFailures = {};
const imageDimensions = {};
let timerId = 0;
const timers = new Map();
const context = new Proxy(
  {
    measureText: function (value) {
      return {width: String(value).length * 6};
    },
    arc: function (x, y) { arcs.push({x: x, y: y}); },
    drawImage: function (image) { imageDraws.push(image); },
  },
  {
    get: function (target, key) {
      if (!(key in target)) target[key] = function () {};
      return target[key];
    },
    set: function (target, key, value) {
      target[key] = value;
      return true;
    },
  }
);
const canvas = {
  id: "enhance-alignment_spatial_plot",
  clientWidth: 500,
  clientHeight: 400,
  width: 0,
  height: 0,
  dataset: {},
  getContext: function () { return context; },
  addEventListener: function () {},
  getBoundingClientRect: function () { return {left: 0, top: 0}; },
};
context.canvas = canvas;
const slider = {
  id: "enhance-img_scale",
  type: "text",
  value: "0.001",
  min: "0",
  max: "10",
  step: "0.001",
};
const number = {
  id: "enhance-img_scale_number",
  type: "number",
  value: "0.001",
  min: "0",
  max: "10",
  step: "0.001",
};
const percentageSlider = {
  id: "enhance-cell_percentage",
  type: "text",
  value: "100",
  min: "10",
  max: "100",
  step: "1",
};
const percentageNumber = {
  id: "enhance-cell_percentage_number",
  type: "number",
  value: "100",
  min: "10",
  max: "100",
  step: "1",
};
const elements = {
  "enhance-alignment_spatial_plot": canvas,
  "enhance-img_scale": slider,
  "enhance-img_scale_number": number,
  "enhance-cell_percentage": percentageSlider,
  "enhance-cell_percentage_number": percentageNumber,
};

global.document = {
  activeElement: null,
  getElementById: function (id) { return elements[id] || null; },
  addEventListener: function (type, handler) {
    nativeHandlers[type] = nativeHandlers[type] || [];
    nativeHandlers[type].push(handler);
  },
  createElement: function (tag) {
    if (tag !== "canvas") return {};
    return {
      width: 0,
      height: 0,
      getContext: function () {
        return {
          drawImage: function (image) { rasterBuildDraws.push(image); },
        };
      },
    };
  },
};
global.performance = {now: function () { return 10; }};
global.Image = function () {
  Object.defineProperty(this, "src", {
    set: function (value) {
      imageLoads.push(value);
      const dimensions = imageDimensions[value] || {width: 100, height: 100};
      this.naturalWidth = dimensions.width;
      this.naturalHeight = dimensions.height;
      if (imageFailures[value] > 0) {
        imageFailures[value] -= 1;
        if (typeof this.onerror === "function") this.onerror();
        return;
      }
      if (typeof this.onload === "function") this.onload();
    },
  });
};
global.window = {
  devicePixelRatio: 1,
  navigator: {deviceMemory: 2},
  addEventListener: function () {},
  requestAnimationFrame: function (callback) {
    frames.push(callback);
    return frames.length;
  },
  cancelAnimationFrame: function () {},
  setTimeout: function (callback) {
    timerId += 1;
    timers.set(timerId, callback);
    return timerId;
  },
  clearTimeout: function (id) { timers.delete(id); },
};
function jq(node) {
  return {
    on: function (events, selector, handler) {
      if (node === document) {
        events.split(/\s+/).forEach(function (event) {
          delegated[event.split(".")[0]] = handler;
        });
      }
      return this;
    },
    data: function (key, value) {
      node.__data = node.__data || {};
      if (arguments.length > 1) {
        node.__data[key] = value;
        return this;
      }
      return node.__data[key];
    },
  };
}
jq.fn = {};
window.jQuery = jq;
global.jQuery = jq;
slider.__data = {
  ionRangeSlider: {
    update: function (options) {
      if (Number.isFinite(Number(options.step))) {
        slider.step = String(options.step);
      }
      if (Number.isFinite(Number(options.from))) {
        const step = Number(slider.step) || 1;
        const rounded = Math.round(Number(options.from) / step) * step;
        slider.value = String(Number(rounded.toPrecision(15)));
      }
    },
  },
};
global.Shiny = {
  setInputValue: function (id, value) { inputs.push({id: id, value: value}); },
  addCustomMessageHandler: function (id, handler) { custom[id] = handler; },
};
window.Shiny = global.Shiny;

const canvasSource = process.argv[2] || path.resolve(
  __dirname,
  "..",
  "..",
  "inst",
  "builder",
  "www",
  "builder-spatial-canvas.js"
);
vm.runInThisContext(fs.readFileSync(canvasSource, "utf8"), {
  filename: canvasSource,
});

const controls = {
  coordinateRotation: 0,
  coordinateScale: 2,
  dx: 0,
  dy: 0,
  scale: 0.001,
  rotation: 0,
  flip_x: false,
  flip_y: false,
  image_opacity: 0.8,
  point_opacity: 0.85,
  point_size: 5,
  cell_percentage: 100,
};
const cachedImageWithUri = {
  sourceKey: "image-a",
  uri: "data:image/png;base64,AA==",
  baseBounds: {xmin: 0, xmax: 10, ymin: 0, ymax: 20},
  controls: controls,
};
const cachedImageWithoutUri = Object.assign({}, cachedImageWithUri);
delete cachedImageWithoutUri.uri;
const overlay = {
  available: true,
  viewKey: "overlay",
  dataset: "dataset-a",
  snapshotIdentity: "snapshot-a",
  section: "section-a",
  generation: 1,
  resetToken: 0,
  layout: "overlay",
  activeRoi: "",
  activeImage: "A",
  bounds: {xmin: 0, xmax: 10, ymin: 0, ymax: 20},
  image: cachedImageWithUri,
  roiImages: {},
  points: {
    x: [0, 10],
    y: [0, 20],
    group: ["A", "B"],
    color: ["#111", "#222"],
    count: [1, 1],
  },
  controls: controls,
};
custom.builder_spatial_canvas_scene(overlay);
frames.shift()();
const overlayViewerFrames = window.__builderSpatialCanvasMetrics.viewerFrames.map(
  function (frame) { return Object.assign({}, frame); }
);
const overlayPoints = arcs.slice(0, 2).map(function (point) {
  return Object.assign({}, point);
});
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const overlayViewport = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_viewports";
}).slice(-1)[0].value;

canvas.clientWidth = 540;
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {generation: 2}));
frames.shift()();
const resizedOverlayViewport = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_viewports";
}).slice(-1)[0].value;
canvas.clientWidth = 500;

slider.value = "0.01";
let stopped = 0;
delegated.change.call(slider, {
  type: "change",
  currentTarget: slider,
  originalEvent: undefined,
  stopImmediatePropagation: function () { stopped += 1; },
});
delegated.input.call(slider, {
  type: "input",
  currentTarget: slider,
  originalEvent: undefined,
  stopImmediatePropagation: function () { stopped += 1; },
});
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const commits = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
});
const numberValueAfterIon = number.value;

slider.step = "0.01";
number.step = "0.01";
number.value = "0.001";
const numberEvent = {
  target: number,
  stopImmediatePropagation: function () {},
};
nativeHandlers.input.forEach(function (handler) { handler(numberEvent); });
nativeHandlers.change.forEach(function (handler) { handler(numberEvent); });
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const tinyScaleCommits = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).slice(commits.length);
const tinyNumberValueAfterEntry = String(number.value);
const tinySliderValueAfterEntry = String(slider.value);
const tinyStepAfterEntry = String(slider.step);

const separate = {
  available: true,
  viewKey: "separate",
  dataset: "dataset-a",
  snapshotIdentity: "snapshot-a",
  section: "section-a",
  generation: 3,
  resetToken: 0,
  layout: "separate",
  activeRoi: "A",
  activeImage: "A",
  bounds: {xmin: 0, xmax: 30, ymin: 0, ymax: 20},
  image: null,
  roiImages: {},
  roiBounds: {
    A: {xmin: -100, xmax: 10, ymin: 0, ymax: 200},
    B: {xmin: 20, xmax: 30, ymin: 0, ymax: 20},
  },
  roiPointAppearance: {},
  roiCoordinateTransforms: {},
  points: {
    x: [-45, 10, 20, 30],
    y: [100, 20, 0, 20],
    group: ["C1", "C2", "C1", "C2"],
    roi: ["A", "A", "B", "B"],
    color: ["#111", "#222", "#111", "#222"],
    count: [2, 2, 2, 2],
  },
  controls: Object.assign({}, controls, {coordinateRotation: 90}),
};
slider.step = "0.02";
number.step = "0.02";
arcs.length = 0;
custom.builder_spatial_canvas_scene(separate);
frames.shift()();
const separateViewerFrames = window.__builderSpatialCanvasMetrics.viewerFrames.map(
  function (frame) { return Object.assign({}, frame); }
);
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const separateViewport = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_viewports";
}).slice(-1)[0].value;
const separateFirstPoint = arcs[0];
const authoritativeStep = String(slider.step);

const resetRaceCommitStart = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).length;
nativeHandlers.pointerdown.forEach(function (handler) {
  handler({target: slider});
});
slider.value = "0.02";
nativeHandlers.input.forEach(function (handler) {
  handler({
    target: slider,
    stopImmediatePropagation: function () {},
  });
});
custom.builder_spatial_canvas_scene(Object.assign({}, separate, {
  generation: 4,
  resetToken: 1,
  controls: Object.assign({}, separate.controls, {scale: 0.001}),
}));
nativeHandlers.pointerup.forEach(function (handler) { handler({}); });
const resetRaceCommits = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).slice(resetRaceCommitStart);
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});

custom.builder_spatial_canvas_clear({viewKey: "separate", generation: 4});
const imageDrawsBeforeReactivation = imageDraws.length;
delegated.input.call(slider, {
  type: "input",
  currentTarget: slider,
  originalEvent: undefined,
  stopImmediatePropagation: function () {},
});
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "null-controls",
  generation: 5,
  resetToken: 1,
  controls: Object.assign({}, controls, {coordinateRotation: -94.7}),
  image: cachedImageWithoutUri,
}));
frames.shift()();
const imageDrawsAfterReactivation = imageDraws.length;
const rotationAfterReactivation =
  window.__builderSpatialCanvasMetrics.latestCoordinateRotation;
const nullControlsViewport = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_viewports";
}).slice(-1)[0].value;
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});

const retryUri = "local://retry-image";
imageFailures[retryUri] = 2;
const retryDrawStart = imageDraws.length;
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "retry-image",
  generation: 6,
  resetToken: 1,
  image: Object.assign({}, cachedImageWithUri, {
    sourceKey: "image-retry",
    uri: retryUri,
  }),
}));
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "retry-image",
  generation: 7,
  resetToken: 1,
  image: {
    sourceKey: "image-retry",
    baseBounds: cachedImageWithUri.baseBounds,
  },
}));
while (frames.length) frames.shift()();

const retryImageLoads = imageLoads.filter(function (uri) {
  return uri === retryUri;
}).length;
const retryImageDraws = imageDraws.length - retryDrawStart;
const retryImageErrors = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_image_load_error";
});
const retryImageError = retryImageErrors.length ?
  retryImageErrors[retryImageErrors.length - 1].value : null;

const deferredUri = "local://deferred-roi-image";
const deferredImage = {
  sourceKey: "image-deferred",
  uri: deferredUri,
  baseBounds: cachedImageWithUri.baseBounds,
  controls: controls,
};
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "overlay-deferred",
  generation: 8,
  resetToken: 1,
  image: null,
  roiImages: {A: [deferredImage]},
}));
const deferredOverlayLoads = imageLoads.filter(function (uri) {
  return uri === deferredUri;
}).length;
const deferredWithoutUri = Object.assign({}, deferredImage);
delete deferredWithoutUri.uri;
custom.builder_spatial_canvas_scene(Object.assign({}, separate, {
  viewKey: "separate-deferred",
  generation: 9,
  resetToken: 1,
  roiImages: {A: [deferredWithoutUri]},
}));
const deferredSeparateLoads = imageLoads.filter(function (uri) {
  return uri === deferredUri;
}).length;
while (frames.length) frames.shift()();
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
while (frames.length) frames.shift()();

const dragCommitStart = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).length;
nativeHandlers.pointerdown.forEach(function (handler) {
  handler({target: slider});
});
[0.2, 0.4, 0.8, 1.2].forEach(function (value) {
  slider.value = String(value);
  nativeHandlers.input.forEach(function (handler) {
    handler({
      target: slider,
      stopImmediatePropagation: function () {},
    });
  });
});
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const dragCommitsBeforeRelease = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).length - dragCommitStart;
nativeHandlers.pointerup.forEach(function (handler) { handler({}); });
const dragCommitsAfterRelease = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).length - dragCommitStart;

const largeAUri = "local://large-cache-a";
const largeBUri = "local://large-cache-b";
imageDimensions[largeAUri] = {width: 10000, height: 10000};
imageDimensions[largeBUri] = {width: 10000, height: 10000};
const largeImage = function (key, uri) {
  return {
    sourceKey: key,
    uri: uri,
    baseBounds: cachedImageWithUri.baseBounds,
    controls: controls,
  };
};
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "large-cache-a",
  generation: 10,
  image: largeImage("large-cache-a", largeAUri),
  roiImages: {},
}));
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "large-cache-b",
  generation: 11,
  image: largeImage("large-cache-b", largeBUri),
  roiImages: {},
}));
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "large-cache-a-return",
  generation: 12,
  image: {
    sourceKey: "large-cache-a",
    baseBounds: cachedImageWithUri.baseBounds,
    controls: controls,
  },
  roiImages: {},
}));
const evictedImageReloads = imageLoads.filter(function (uri) {
  return uri === largeAUri;
}).length;
while (frames.length) frames.shift()();
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "large-cache-a-return",
  generation: 13,
  image: {
    sourceKey: "large-cache-a",
    baseBounds: cachedImageWithUri.baseBounds,
    controls: controls,
  },
  roiImages: {},
}));
while (frames.length) frames.shift()();
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
while (frames.length) frames.shift()();

const percentagePoints = Array.from({length: 100}, function (_, index) {
  return index;
});
arcs.length = 0;
custom.builder_spatial_canvas_scene(Object.assign({}, overlay, {
  viewKey: "percentage",
  generation: 14,
  image: null,
  bounds: {xmin: 0, xmax: 99, ymin: 0, ymax: 99},
  points: {
    x: percentagePoints,
    y: percentagePoints,
    group: percentagePoints.map(function () { return "A"; }),
    color: percentagePoints.map(function () { return "#111"; }),
    count: percentagePoints.map(function () { return 100; }),
  },
  controls: Object.assign({}, controls, {cell_percentage: 20}),
}));
frames.shift()();
const percentage20Points = window.__builderSpatialCanvasMetrics.visiblePoints;
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});
const percentageSceneStart =
  window.__builderSpatialCanvasMetrics.sceneMessages;
const percentageCommitStart = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).length;
nativeHandlers.pointerdown.forEach(function (handler) {
  handler({target: percentageSlider});
});
percentageSlider.value = "50";
nativeHandlers.input.forEach(function (handler) {
  handler({
    target: percentageSlider,
    stopImmediatePropagation: function () {},
  });
});
arcs.length = 0;
frames.shift()();
const percentage50Points = window.__builderSpatialCanvasMetrics.visiblePoints;
nativeHandlers.pointerup.forEach(function (handler) { handler({}); });
const percentageCommits = inputs.filter(function (entry) {
  return entry.id === "builder_spatial_alignment_controls";
}).slice(percentageCommitStart);
const percentageSceneMessages =
  window.__builderSpatialCanvasMetrics.sceneMessages - percentageSceneStart;
Array.from(timers.entries()).forEach(function (entry) {
  timers.delete(entry[0]);
  entry[1]();
});

console.log(JSON.stringify({
  overlay: overlayViewport,
  resizedOverlay: resizedOverlayViewport,
  overlayViewerFrames: overlayViewerFrames,
  overlayPoints: overlayPoints,
  separate: separateViewport,
  separateViewerFrames: separateViewerFrames,
  separateFirstPoint: separateFirstPoint,
  nullControlsViewKey: nullControlsViewport.viewKey,
  numberValue: numberValueAfterIon,
  tinyNumberValue: tinyNumberValueAfterEntry,
  tinySliderValue: tinySliderValueAfterEntry,
  tinyStep: tinyStepAfterEntry,
  authoritativeStep: authoritativeStep,
  tinyScaleCommits: tinyScaleCommits,
  commits: commits,
  resetRaceCommits: resetRaceCommits,
  imageLoads: imageLoads.filter(function (uri) {
    return uri === cachedImageWithUri.uri;
  }).length,
  imageDrawsAfterClear: imageDrawsAfterReactivation - imageDrawsBeforeReactivation,
  rotationAfterClear: rotationAfterReactivation,
  retryImageLoads: retryImageLoads,
  retryImageDraws: retryImageDraws,
  retryImageErrors: retryImageErrors.length,
  retryImageErrorViewKey: retryImageError && retryImageError.viewKey,
  retryImageErrorGeneration: retryImageError && retryImageError.generation,
  retryImageErrorSnapshot: retryImageError && retryImageError.snapshotIdentity,
  deferredOverlayLoads: deferredOverlayLoads,
  deferredSeparateLoads: deferredSeparateLoads,
  evictedImageReloads: evictedImageReloads,
  decodedCacheEvictions:
    window.__builderSpatialCanvasMetrics.decodedCacheEvictions,
  imageRenderCacheBuilds:
    window.__builderSpatialCanvasMetrics.imageRenderCacheBuilds,
  rasterBuildDraws: rasterBuildDraws.length,
  dragCommitsBeforeRelease: dragCommitsBeforeRelease,
  dragCommitsAfterRelease: dragCommitsAfterRelease,
  percentage20Points: percentage20Points,
  percentage50Points: percentage50Points,
  percentageSceneMessages: percentageSceneMessages,
  percentageCommits: percentageCommits,
  stopped: stopped,
  pendingTimers: timers.size,
}));
