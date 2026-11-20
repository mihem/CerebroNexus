// A background image's offset, scale and flips are read in its own unrotated
// frame; its rotation comes last, about the data origin, the same way R
// rotates the cell coordinates (rotateSpatialCoordinates). So an alignment
// found on the unrotated slice stays aligned once the slice is rotated, and a
// rotated, flipped image is never mirrored relative to its cells.
const fs = require('fs'), vm = require('vm'), assert = require('assert');
const path = require('path');
const source = fs.readFileSync(process.argv[2], 'utf8');
function fn(name) {
  const start = source.indexOf('  function ' + name + '(');
  assert(start >= 0, name);
  const end = source.indexOf('\n  function ', start + 1);
  return source.slice(start, end < 0 ? undefined : end);
}
const models = {window: {}};
vm.runInNewContext(fs.readFileSync(path.join(path.dirname(process.argv[2]),
  'background-model.js'), 'utf8'), models);
const backgrounds = models.window.CerebroBackgroundModel;

const near = (a, b, what) => assert(
  Math.abs(a[0] - b[0]) < 1e-9 && Math.abs(a[1] - b[1]) < 1e-9,
  what + ': ' + a + ' != ' + b);
const rotate = (p, degrees) => {
  const t = degrees * Math.PI / 180;
  return [p[0] * Math.cos(t) - p[1] * Math.sin(t),
    p[0] * Math.sin(t) + p[1] * Math.cos(t)];
};
const align = (extra) => Object.assign({show: true, opacity: 1, offsetX: 0,
  offsetY: 0, scaleX: 1, scaleY: 1, flipX: false, flipY: false, rotate: 0},
  extra);

// Rotation alone (the MERFISH demo: rotation = 90, no flip) rotates the bounds
// about the origin, as before.
const portrait = {xmin: 0, xmax: 6, ymin: 0, ymax: 10};
const merfish = backgrounds.corners(portrait, align({rotate: 90}));
near(merfish[0], [-10, 0], 'MERFISH top-left');
near(merfish[1], [-10, 6], 'MERFISH top-right');
near(merfish[2], [0, 0], 'MERFISH bottom-left');

// Without rotation, offset/scale/flip act about the bounds centre, as before.
const wide = {xmin: 0, xmax: 4, ymin: 0, ymax: 2};
const plain = backgrounds.corners(wide,
  align({offsetX: 3, offsetY: -1, scaleX: 2, scaleY: 0.5, flipY: true}));
near(plain[0], [1, -0.5], 'unrotated top-left');
near(plain[1], [9, -0.5], 'unrotated top-right');
near(plain[2], [1, 0.5], 'unrotated bottom-left');

// The Xenium poster case: the alignment of the unrotated slice plus
// rotation = 90 lands where rotation = 90, flip_x, offset_x = 10 used to.
const xenium = backgrounds.corners(wide,
  align({offsetY: -10, flipY: true, rotate: 90}));
near(xenium[0], [10, 0], 'Xenium top-left');
near(xenium[1], [10, 4], 'Xenium top-right');
near(xenium[2], [8, 0], 'Xenium bottom-left');

// For every combination, rotating the alignment equals rotating the aligned
// unrotated image: flips do not swap axes and offsets turn with the image.
const combos = [
  {flipX: true}, {flipY: true}, {flipX: true, flipY: true},
  {offsetX: 7, offsetY: -3}, {scaleX: 1.5, scaleY: 0.8, flipY: true},
  {offsetX: -2, offsetY: 5, scaleX: 0.7, scaleY: 1.3, flipX: true}
];
const far = {xmin: 1000, xmax: 1600, ymin: 2000, ymax: 2400};
for (const combo of combos) {
  for (const degrees of [90, 180, 270, -30, 28]) {
    for (const bounds of [wide, far]) {
      const flat = backgrounds.corners(bounds, align(combo));
      const turned = backgrounds.corners(bounds,
        align(Object.assign({rotate: degrees}, combo)));
      for (let k = 0; k < 3; k++) {
        near(turned[k], rotate(flat[k], degrees),
          JSON.stringify(combo) + ' at ' + degrees + ' corner ' + k);
      }
    }
  }
}

// drawImage maps exactly those corners through the cells' own projection,
// including an anisotropic one, onto the unit square of the image.
function trackingContext() {
  let m = [1, 0, 0, 1, 0, 0];
  const stack = [], drawn = [];
  const mul = (a, b) => [
    a[0] * b[0] + a[2] * b[1], a[1] * b[0] + a[3] * b[1],
    a[0] * b[2] + a[2] * b[3], a[1] * b[2] + a[3] * b[3],
    a[0] * b[4] + a[2] * b[5] + a[4], a[1] * b[4] + a[3] * b[5] + a[5]
  ];
  const at = (x, y) => [m[0] * x + m[2] * y + m[4], m[1] * x + m[3] * y + m[5]];
  return {
    drawn, globalAlpha: 1,
    save() { stack.push(m.slice()); }, restore() { m = stack.pop(); },
    translate(x, y) { m = mul(m, [1, 0, 0, 1, x, y]); },
    scale(x, y) { m = mul(m, [x, 0, 0, y, 0, 0]); },
    rotate() { throw new Error('image must not be rotated on screen'); },
    transform(a, b, c, d, e, f) { m = mul(m, [a, b, c, d, e, f]); },
    drawImage(img, x, y, w, h) {
      drawn.push({tl: at(x, y), tr: at(x + w, y), bl: at(x, y + h)});
    }
  };
}
const toScreen = (x, y) => [5 + 2 * x, 7 - 3 * y];
const state = align({offsetY: -10, flipY: true, rotate: 90, opacity: 0.4});
const sandbox = {
  backgrounds,
  singleActive: 'spatial_projection', linkedBackgroundsOn: false,
  spaceById: {s: {_imgEl: {}, _imgReady: true, _imgState: state}},
  currentImage: () => ({bounds: wide, preset: {}}),
  imageRenderState: (img, s) => s,
  dataToScreen: (p, x, y) => toScreen(x, y)
};
vm.createContext(sandbox);
vm.runInContext(fn('drawImage'), sandbox);
const context = trackingContext();
sandbox.drawImage({spaceId: 's', ctx: context}, context);
assert.strictEqual(context.drawn.length, 1, 'drawImage drew no image');
const drawn = context.drawn[0];
near(drawn.tl, toScreen(10, 0), 'screen top-left');
near(drawn.tr, toScreen(10, 4), 'screen top-right');
near(drawn.bl, toScreen(8, 0), 'screen bottom-left');
assert.strictEqual(context.globalAlpha, 1, 'opacity restored');
// Linked views saved as version 1 keep their old geometry: the image centre
// rotated with the cells, the offset in data axes, then flips and scales after
// the rotation. legacyState must give the alignment that draws there now.
const oldCorners = (bounds, s) => {
  const c = [(bounds.xmin + bounds.xmax) / 2, (bounds.ymin + bounds.ymax) / 2];
  const rc = rotate(c, s.rotate);
  const fx = s.scaleX * (s.flipX ? -1 : 1), fy = s.scaleY * (s.flipY ? -1 : 1);
  const place = (x, y) => {
    const ru = rotate([x, y], s.rotate);
    return [rc[0] + s.offsetX + fx * (ru[0] - rc[0]),
      rc[1] + s.offsetY + fy * (ru[1] - rc[1])];
  };
  return [place(bounds.xmin, bounds.ymax), place(bounds.xmax, bounds.ymax),
    place(bounds.xmin, bounds.ymin)];
};
// The reviewer's case: the same version-1 state drew these corners before.
const reviewed = align({offsetY: -10, flipY: true, rotate: 90});
const legacyXenium = backgrounds.legacyState({bounds: wide}, reviewed);
const migrated = backgrounds.corners(wide, legacyXenium);
near(migrated[0], [-2, -6], 'version-1 top-left');
near(migrated[1], [-2, -10], 'version-1 top-right');
near(migrated[2], [0, -6], 'version-1 bottom-left');
assert.strictEqual(legacyXenium.offsetX, -10, 'right angles convert exactly');
assert.strictEqual(legacyXenium.offsetY, 0, 'right angles convert exactly');
const legacyCombos = combos.concat([
  {scaleX: 2, scaleY: 0.5}, {scaleX: 1.5, scaleY: 1.5, flipX: true, offsetX: 4}
]);
const baked = {geometryBaked: true, offsetX: 3, offsetY: -2, scaleX: 2,
  scaleY: 0.5, flipX: true, rotation: 40};
for (const combo of legacyCombos) {
  for (const degrees of [0, 90, 180, 270, -90, 450, -30, 28]) {
    for (const bounds of [wide, far]) {
      const uneven = combo.scaleX && combo.scaleY && combo.scaleX !== combo.scaleY;
      const right = degrees % 90 === 0;
      for (const preset of [{}, baked]) {
        const img = {bounds, preset};
        const saved = align(Object.assign({rotate: degrees}, combo));
        if (preset.geometryBaked) {
          // A baked image stores its preset plus the user's change.
          saved.offsetX += preset.offsetX; saved.offsetY += preset.offsetY;
          saved.scaleX *= preset.scaleX; saved.scaleY *= preset.scaleY;
          saved.flipX = saved.flipX !== preset.flipX;
          saved.rotate += preset.rotation;
        }
        const what = JSON.stringify(combo) + ' at ' + degrees +
          (preset.geometryBaked ? ' baked' : '');
        const state = backgrounds.legacyState(img, saved);
        if (uneven && !right) {
          assert.strictEqual(state, null, what + ': a sheared image has no equivalent');
          continue;
        }
        assert(state, what + ': no conversion');
        const before = oldCorners(bounds, backgrounds.renderState(img, saved));
        const after = backgrounds.corners(bounds, backgrounds.renderState(img, state));
        for (let k = 0; k < 3; k++) {
          assert(Math.hypot(before[k][0] - after[k][0], before[k][1] - after[k][1]) < 1e-7,
            what + ' corner ' + k + ': ' + after[k] + ' != ' + before[k]);
        }
      }
    }
  }
}

// Restoring a version-1 file converts each saved alignment, leaves a
// version-2 file alone, and refuses a sheared one instead of moving it.
const restore = {
  backgrounds,
  configError: (message) => { throw new Error(message); },
  finiteNumber: (value, fallback, min, max) => {
    value = Number(value);
    return isFinite(value) ? Math.max(min, Math.min(max, value)) : fallback;
  }
};
vm.createContext(restore);
vm.runInContext(fn('savedAlignmentState') + '\n' + fn('legacyBackgroundStates'), restore);
const samples = [{name: 'fov', images: [{id: 'tissue', bounds: wide}]}];
const savedView = (version, alignment) => ({version, view: {spatial_backgrounds: [{
  section: 'fov', mode: 'image', image_id: 'tissue', opacity: 0.4,
  alignment: Object.assign({show: true, offset_x: 0, offset_y: -10, scale_x: 1,
    scale_y: 1, rotation: 90, flip_x: false, flip_y: true, lock_aspect: true},
  alignment)}]}});
const restored = restore.legacyBackgroundStates(savedView(1, {}), samples).fov;
near(backgrounds.corners(wide, restored)[0], [-2, -6], 'restored version-1 top-left');
assert.strictEqual(restored.opacity, 0.4, 'opacity kept');
assert.strictEqual(Object.keys(
  restore.legacyBackgroundStates(savedView(2, {}), samples)).length, 0,
'version 2 is restored as saved');
assert.throws(() => restore.legacyBackgroundStates(
  savedView(1, {scale_x: 2, rotation: 30}), samples), /uneven scale/);

console.log('PASS background image: flips and offsets in the image frame, rotation last about the origin; version-1 views converted');
