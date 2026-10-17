'use strict';
const assert = require('assert/strict');
const fs = require('fs');
const vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
function extract(name, next) {
  const start = source.indexOf('  function ' + name + '(');
  const end = source.indexOf('  function ' + next + '(', start);
  assert(start >= 0 && end > start);
  return source.slice(start, end);
}
// Model Selectize's real retention rule: clearOptions retains selected items.
const picker = {
  value: 'other-roi', options: {'other-roi': {value: 'other-roi'}},
  clear(silent) { assert(silent); this.value = ''; },
  clearOptions(silent) { assert(silent); this.options = this.value ? {[this.value]: this.options[this.value]} : {}; },
  addOption(options) { options.forEach(o => this.options[o.value] = o); },
  setValue(v, silent) { assert(silent); this.value = v; }, enable() {}, disable() {}
};
const section = {style: {}};
const elements = {'cv-img-pick-ctl': {style: {}, closest: () => section},
  'cv-bg-image-select': {selectize: picker}};
let active = {id: 'roi-a', images: [{id:'image-a',preset:{opacity:.6}}]};
const context = {
  $: id => elements[id], backgroundSpaces: () => active ? [active] : [],
  activeSpatial: () => active, spatialImages: sp => sp.images,
  activeSpatialId: null, IMG_NONE: '__none__', singleActive: null,
  seedImgControls() {}, window: {}, linkedBackgroundsOn: true
};
vm.createContext(context);
vm.runInContext(extract('renderImagePicker', 'setSpatialImage'), context);
context.renderImagePicker();
assert.equal(section.style.display, '');
assert.deepEqual(Object.keys(picker.options), ['image-a', '__none__']);
assert.equal(picker.value, 'image-a');
active = null;
context.renderImagePicker();
assert.equal(section.style.display, 'none');

// A stale option/event cannot apply an image from a different ROI.
active = {id:'roi-a',images:[{id:'image-a',preset:{opacity:.6}}]};
let stashed = 0;
Object.assign(context, {stashImgState() {stashed++;}, loadSpaceImage() {},
  imgChoice: {}, backgroundStateKey: sp => sp.id, updateSpaceScopedControls() {}, drawAll() {}});
vm.runInContext(extract('setSpatialImage', 'activateSpatial'), context);
context.setSpatialImage('other-roi', active);
assert.equal(stashed, 0);
assert.equal(active._customImageId, undefined);
context.setSpatialImage('image-a', active);
assert.equal(stashed, 1);
assert.equal(active._customImageId, 'image-a');

// Deferred pixels must not leave the previous image's transform in the editor.
let requested;
const nextImage = {id:'new-image',asset_key:'pixels',preset:{opacity:.6}};
Object.assign(context, {
  imgToken:0, _layoutKey:null, currentImage: () => nextImage,
  viewOperation() {}, requestDeferredAsset: key => {requested=key;},
  imgKey: (sp,img) => sp.id+'|'+img.id, imgStates:{},
  presetState: img => ({...img.preset})
});
vm.runInContext(extract('loadSpaceImage', 'deferredAssetCacheKey'), context);
active._imgState = {opacity:.2};
context.loadSpaceImage(active);
assert.equal(requested, 'pixels');
assert.equal(active._imgState.opacity, .6);
active._imgState.opacity=.3;
assert.equal(nextImage.preset.opacity,.6);
console.log('PASS linked background visibility, ROI isolation and deferred image state');
// Backgrounds must stay below GPU points even without Spatial's background_scope.
const layers = {spaceById: {}};
vm.createContext(layers);
vm.runInContext(extract('needsUnderlay', 'updateGpuSelection'), layers);
for (const space of [{image:{id:'embedded'}}, {images:[{id:'external',deferred:true}]},
  {background_scope:'roi'}, {trajectory:true}, {hulls:[{}]}]) {
  layers.spaceById.s = space;
  assert.equal(layers.needsUnderlay({spaceId:'s'}), true);
}
layers.spaceById.s = {images:[]};
assert.equal(layers.needsUnderlay({spaceId:'s'}), false);
assert.equal(layers.needsUnderlay({spaceId:'missing'}), false);
vm.runInContext(extract('exportPanelLayers', 'rotateDataPoint'), layers);
const order=[];
const panel={underlayCanvas:'image',gpuCanvas:{style:{display:'block'}},canvas:'labels',interactionCanvas:'hover'};
layers.exportPanelLayers({drawImage(c){order.push(c);}},panel,0,0,10,10);
assert.deepEqual(order,['image',panel.gpuCanvas,'labels','hover']);
panel.gpuCanvas.style.display='none'; order.length=0;
layers.exportPanelLayers({drawImage(c){order.push(c);}},panel,0,0,10,10);
assert.deepEqual(order,['image','labels','hover']);
console.log('PASS background underlay and GPU/Canvas export stacking');
