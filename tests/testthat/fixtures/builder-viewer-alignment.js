// The Viewer must draw a Builder-aligned background where the Builder preview
// showed it. Runs the real drawImage() of the Builder preview
// (builder-spatial-canvas.js) and of the Viewer (cell_views.js) against a
// canvas that tracks its transform, and compares where each puts the image's
// top-left, top-right and bottom-left corners.
// argv: cell_views.js, builder-spatial-canvas.js, cases.json
const fs = require('fs'), vm = require('vm'), assert = require('assert');
const viewerSource = fs.readFileSync(process.argv[2], 'utf8');
const builderSource = fs.readFileSync(process.argv[3], 'utf8');
const cases = JSON.parse(fs.readFileSync(process.argv[4], 'utf8'));
function fn(source, name) {
  const start = source.indexOf('  function ' + name + '(');
  assert(start >= 0, name);
  const end = source.indexOf('\n  function ', start + 1);
  return source.slice(start, end < 0 ? undefined : end);
}

// A 2D context that keeps the current affine matrix [a b c d e f] and records
// the screen positions of the corners of each drawn image rectangle.
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
    rotate(r) { m = mul(m, [Math.cos(r), Math.sin(r), -Math.sin(r), Math.cos(r), 0, 0]); },
    transform(a, b, c, d, e, f) { m = mul(m, [a, b, c, d, e, f]); },
    drawImage(img, x, y, w, h) {
      drawn.push({tl: at(x, y), tr: at(x + w, y), bl: at(x, y + h)});
    }
  };
}
// Screen = data with the y axis flipped, for both pages.
const toScreen = (x, y) => [x, -y];

const viewer = {
  singleActive: 'spatial_projection', linkedBackgroundsOn: false,
  spaceById: {}, currentImage: null,
  imageRenderState: (img, state) => state,
  dataToScreen: (p, x, y) => toScreen(x, y)
};
vm.createContext(viewer);
vm.runInContext(['rotateDataPoint', 'drawImage'].map((n) => fn(viewerSource, n)).join('\n'), viewer);

const builder = {
  NONPOSITIVE_SCALE_FALLBACK: 0.02, state: {}, Math,
  imageKey: () => 'image', touchDecodedImage() {},
  displayImage: (key, loaded) => loaded
};
vm.createContext(builder);
vm.runInContext(['finite', 'drawImage'].map((n) => fn(builderSource, n)).join('\n'), builder);

const dist = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1]);
for (const c of cases) {
  const image = {baseBounds: c.bounds};
  builder.state = {image: {}, images: {}, controls: c.builder};
  const bctx = trackingContext();
  builder.drawImage(bctx, {image}, (p) => {
    const s = toScreen(p.x, p.y);
    return {x: s[0], y: s[1]};
  });
  const s = c.viewer;
  viewer.spaceById = {s: {_imgEl: {}, _imgReady: true, _imgState: {
    show: true, opacity: 1, offsetX: s.offset_x, offsetY: s.offset_y,
    scaleX: s.scale_x, scaleY: s.scale_y, flipX: s.flip_x, flipY: s.flip_y,
    rotate: s.rotation}}};
  viewer.currentImage = () => ({bounds: c.bounds, preset: {}});
  const vctx = trackingContext();
  viewer.drawImage({spaceId: 's', ctx: vctx}, vctx);
  assert.strictEqual(bctx.drawn.length, 1, c.name + ': Builder drew no image');
  assert.strictEqual(vctx.drawn.length, 1, c.name + ': Viewer drew no image');
  const b = bctx.drawn[0], v = vctx.drawn[0];
  const residual = Math.max(dist(b.tl, v.tl), dist(b.tr, v.tr), dist(b.bl, v.bl));
  assert(residual < 1e-6, c.name + ': Viewer differs from the Builder preview by ' + residual);
  if (c.expected_tl) {
    assert(dist(b.tl, toScreen(c.expected_tl[0], c.expected_tl[1])) < 1e-6,
      c.name + ': Builder top-left ' + b.tl);
  }
}
console.log('PASS Builder alignment renders identically in the Viewer (' + cases.length + ' cases)');
