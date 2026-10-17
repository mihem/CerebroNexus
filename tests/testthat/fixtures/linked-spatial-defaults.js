const assert = require('assert/strict');
const fs = require('fs');
const vm = require('vm');
const source = fs.readFileSync(process.argv[2], 'utf8');
const start = source.indexOf('    var initialSamples = spatialSamples();');
const end = source.indexOf('    var activeName = ', start);
assert(start >= 0 && end > start);
function choose(names, previousSelected = [], dataChanged = true, hadSpatial = false) {
  const context = {spatialSamples: () => names.map(name => typeof name === 'string' ? {name} : name),
    previousSelected, dataChanged, hadSpatial};
  vm.runInNewContext(source.slice(start, end), context);
  return Array.from(context.selectedSpatial);
}
assert.deepEqual(choose([]), []);
assert.deepEqual(choose(['one']), ['one']);
const scenes = ['fov-a', 'fov-a::roi-1', 'fov-b'];
assert.deepEqual(choose(scenes), scenes);
assert.deepEqual(choose(scenes, [], false, false), scenes); // progressive arrival
assert.deepEqual(choose(scenes, ['fov-b'], false, true), ['fov-b']);
assert.deepEqual(choose(scenes, [], false, true), []); // deliberately cleared
assert.deepEqual(choose(scenes, ['removed', 'fov-b'], false, true), ['fov-b']);
assert.deepEqual(choose(scenes, ['old'], true, true), scenes); // new dataset
const mixed = [{name: 'arbitrary', is_roi_overview: true},
  {name: 'section-A', is_roi_overview: false}, {name: 'section-B', is_roi_overview: false},
  {name: 'single-fov', is_roi_overview: false}];
assert.deepEqual(choose(mixed), ['section-A', 'section-B', 'single-fov']);
assert.deepEqual(choose(mixed, [], false, false), ['section-A', 'section-B', 'single-fov']);
assert.deepEqual(choose(mixed, ['arbitrary'], false, true), ['arbitrary']);
assert.deepEqual(choose(mixed, [], false, true), []);
assert.deepEqual(choose(mixed, ['arbitrary'], true, true), ['section-A', 'section-B', 'single-fov']);
// A literal label is not a scope discriminator.
assert.deepEqual(choose([{name: 'All ROIs', is_roi_overview: false}]), ['All ROIs']);
console.log('PASS individual-scene defaults, progressive arrival and retained choices');
