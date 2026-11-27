"use strict";

// Drives the region drawing of builder-spatial-canvas.js against a minimal DOM:
// a shape is clicked on the screen positions of known points, and the polygon
// sent to Shiny must come back in those points' raw coordinates, whatever
// rotation and scale the canvas applies to the points.

const fs = require("fs");
const path = require("path");
const vm = require("vm");

const inputs = [];
const custom = {};
const frames = [];
const arcs = [];
const nativeHandlers = {};
const canvasHandlers = {};

const context = new Proxy(
  {
    measureText: function (value) {
      return {width: String(value).length * 6};
    },
    arc: function (x, y) { arcs.push({x: x, y: y}); },
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
  style: {},
  getContext: function () { return context; },
  addEventListener: function (type, handler) {
    canvasHandlers[type] = canvasHandlers[type] || [];
    canvasHandlers[type].push(handler);
  },
  getBoundingClientRect: function () { return {left: 0, top: 0}; },
};
context.canvas = canvas;

function button(action) {
  const attributes = {"data-region-action": action};
  return {
    disabled: false,
    classList: {toggle: function () {}},
    getAttribute: function (name) { return attributes[name]; },
    setAttribute: function (name, value) { attributes[name] = value; },
  };
}
const buttons = {};
["draw", "undo", "apply", "clear", "clear_all"].forEach(function (action) {
  buttons[action] = button(action);
});
const status = {textContent: ""};
const tooltip = {hidden: false, textContent: "", style: {}};
const panel = {
  querySelectorAll: function () {
    return Object.keys(buttons).map(function (key) { return buttons[key]; });
  },
};
const elements = {
  "enhance-alignment_spatial_plot": canvas,
  "enhance-alignment_spatial_plot-tooltip": tooltip,
  "builder-region-panel": panel,
  "builder-region-status": status,
};

global.document = {
  activeElement: null,
  getElementById: function (id) { return elements[id] || null; },
  addEventListener: function (type, handler) {
    nativeHandlers[type] = nativeHandlers[type] || [];
    nativeHandlers[type].push(handler);
  },
  createElement: function () { return {}; },
};
global.performance = {now: function () { return 10; }};
global.window = {
  devicePixelRatio: 1,
  addEventListener: function () {},
  requestAnimationFrame: function (callback) {
    frames.push(callback);
    return frames.length;
  },
  cancelAnimationFrame: function () {},
  setTimeout: function () { return 1; },
  clearTimeout: function () {},
};
global.Shiny = {
  setInputValue: function (id, value) { inputs.push({id: id, value: value}); },
  addCustomMessageHandler: function (id, handler) { custom[id] = handler; },
};
window.Shiny = global.Shiny;

const canvasSource = process.argv[2];
vm.runInThisContext(fs.readFileSync(canvasSource, "utf8"), {
  filename: canvasSource,
});

function render() {
  arcs.length = 0;
  while (frames.length) frames.shift()();
}
function press(action) {
  const target = {
    closest: function (selector) {
      return selector === "[data-region-action]" ? buttons[action] : null;
    },
  };
  nativeHandlers.click.forEach(function (handler) {
    handler({target: target});
  });
}
function clickCanvas(point, detail) {
  canvasHandlers.click.forEach(function (handler) {
    handler({clientX: point.x, clientY: point.y, detail: detail || 1});
  });
}
function regionEvents() {
  return inputs.filter(function (entry) {
    return entry.id === "builder_region_event";
  }).map(function (entry) { return entry.value; });
}

const rawPoints = [
  {x: 10, y: 10},
  {x: 90, y: 20},
  {x: 70, y: 80},
  {x: 20, y: 60},
];
function scene(overrides) {
  return Object.assign({
    available: true,
    viewKey: "view-a",
    dataset: "dataset-a",
    snapshotIdentity: "snapshot-a",
    section: "slice1",
    generation: 1,
    resetToken: 0,
    layout: "overlay",
    activeRoi: "",
    bounds: {xmin: 0, xmax: 100, ymin: 0, ymax: 100},
    regions: [],
    regionSummary: null,
    roiImages: {},
    points: {
      x: rawPoints.map(function (p) { return p.x; }),
      y: rawPoints.map(function (p) { return p.y; }),
      group: ["A", "A", "A", "A"],
      color: ["#111", "#111", "#111", "#111"],
      count: [1, 1, 1, 1],
    },
    controls: {
      coordinateRotation: 30,
      coordinateScale: 1.5,
      point_opacity: 0.85,
      point_size: 5,
      cell_percentage: 100,
    },
  }, overrides || {});
}

const out = {};

// 1. Draw on rotated, scaled points: the shape comes back in raw coordinates.
custom.builder_spatial_canvas_scene(scene());
render();
const screenPoints = arcs.slice(0, rawPoints.length).map(function (p) {
  return {x: p.x, y: p.y};
});
out.statusBefore = status.textContent;
press("draw");
out.drawingPressed = buttons.draw.getAttribute("aria-pressed");
screenPoints.forEach(function (point) { clickCanvas(point, 1); });
out.statusWhileDrawing = status.textContent;
// A click on the first point closes the shape.
clickCanvas(screenPoints[0], 1);
out.statusAfterClose = status.textContent;
press("apply");
const applied = regionEvents();
out.applied = applied.length ? applied[applied.length - 1] : null;
out.rawPoints = rawPoints;

// 2. Undo removes the last point of the shape being drawn.
custom.builder_spatial_canvas_scene(scene({generation: 2, viewKey: "view-b"}));
render();
const eventsBefore = regionEvents().length;
press("draw");
clickCanvas(screenPoints[0], 1);
clickCanvas(screenPoints[1], 1);
press("undo");
out.statusAfterUndo = status.textContent;
// Fewer than three points: applying sends nothing.
press("apply");
out.eventsAfterShortApply = regionEvents().length - eventsBefore;

// 3. Stored regions are shown with their summary and can be cleared.
custom.builder_spatial_canvas_scene(scene({
  generation: 3,
  viewKey: "view-b",
  regions: [{
    x: rawPoints.map(function (p) { return p.x; }),
    y: rawPoints.map(function (p) { return p.y; }),
  }],
  regionSummary: {kept: 1250, total: 5020, sectionKept: 1250, sectionTotal: 5020},
}));
render();
out.statusStored = status.textContent;
out.clearEnabled = !buttons.clear.disabled;
press("clear");
const afterClear = regionEvents();
out.clearEvent = afterClear[afterClear.length - 1];
press("clear_all");
const afterClearAll = regionEvents();
out.clearAllEvent = afterClearAll[afterClearAll.length - 1];

// 4. Separate ROI panels and single ROIs have no raw coordinates to draw on.
custom.builder_spatial_canvas_scene(scene({
  generation: 4,
  viewKey: "view-c",
  layout: "separate",
}));
render();
const separateBefore = regionEvents().length;
press("draw");
out.drawingInSeparate = buttons.draw.getAttribute("aria-pressed");
out.statusSeparate = status.textContent;
out.drawDisabledInSeparate = buttons.draw.disabled;
custom.builder_spatial_canvas_scene(scene({
  generation: 5,
  viewKey: "view-d",
  activeRoi: "A",
}));
render();
out.drawDisabledInRoi = buttons.draw.disabled;
out.separateEvents = regionEvents().length - separateBefore;

// 5. Drawing hides the hover tooltip, ignores duplicate double-click vertices,
//    clarifies replacing a stored region, and Escape discards drafts only.
custom.builder_spatial_canvas_scene(scene({
  generation: 6,
  viewKey: "view-e",
  regions: [{
    x: rawPoints.map(function (p) { return p.x; }),
    y: rawPoints.map(function (p) { return p.y; }),
  }],
  regionSummary: {kept: 1250, total: 5020, sectionKept: 1250, sectionTotal: 5020},
}));
render();
tooltip.hidden = false;
press("draw");
out.tooltipHiddenWhileDrawing = tooltip.hidden;
clickCanvas(screenPoints[0], 1);
clickCanvas(screenPoints[1], 1);
clickCanvas(screenPoints[2], 1);
// Double-click at the third vertex: detail=1 repeats the vertex and is ignored,
// then detail=2 closes a 3-vertex polygon.
clickCanvas(screenPoints[2], 1);
clickCanvas(screenPoints[2], 2);
out.statusReplacingStored = status.textContent;
const beforeEscape = regionEvents().length;
nativeHandlers.keydown.forEach(function (handler) {
  handler({key: "Escape", target: {tagName: "BODY"}});
});
out.statusAfterEscape = status.textContent;
out.eventsAfterEscape = regionEvents().length - beforeEscape;
press("draw");
clickCanvas(screenPoints[0], 1);
clickCanvas(screenPoints[1], 1);
clickCanvas(screenPoints[2], 1);
clickCanvas(screenPoints[2], 1);
clickCanvas(screenPoints[2], 2);
press("apply");
const dblApplied = regionEvents();
out.doubleClickVertexCount = dblApplied[dblApplied.length - 1].polygons[0].x.length;

console.log(JSON.stringify(out));
