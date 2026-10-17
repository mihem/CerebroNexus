// Run the production navigation and supplement handlers with renderer/DOM ports
// stubbed. The browser regression exercises the real canvas with spatial data.
const fs = require('fs'), vm = require('vm'), assert = require('assert');
const source = fs.readFileSync(process.argv[2], 'utf8');
function fn(name) {
  const start = source.indexOf('  function ' + name + '(');
  assert(start >= 0, name);
  let end = source.indexOf('\n  function ', start + 1);
  return source.slice(start, end < 0 ? undefined : end);
}
const visibility = source.slice(source.indexOf('    function reportVisibility()'),
  source.indexOf('    setInterval(reportVisibility, 250);'));
const ids = ['overview_projection', 'spatial_projection', 'expression_projection',
  'trajectory_projection', 'ir_clonalUMAP_projection', 'trekker_projection', 'hla_motif_network'];
function session() {
  const base = {dataset_id: 'ds1', dataset_context: 'generation-1',
    dataset_fingerprint: 'fp', progressive_token: 'p1', progressive: true,
    n: 100, groups: {}, cat_extra: {}, fields: {}, projections: {}, spaces: []};
  const c = {D: base, linkedBundle: base, linkedState: {selection: [42]},
    singleActive: null, singleSpaceIds: [], singleSpaceModes: {},
    lastVis: null, page: 'linked', surfaceHome: {panes: {}}, rebuildingBase: false,
    deferredLinkedSupplements: [], pendingCloneSupplement: null, transportMetrics: {},
    linkedRequestTiming: {}, singleTiming: {}, saved: {}, restored: [], hydrated: [],
    frames: [], fallbacks: 0, performance: {now: () => 1},
    CBViewState: {specialistLifecycle: {begin: () => ({})},
      telemetry: {transport: () => ({}), snapshot: x => x}},
    Shiny: {setInputValue: () => {}}, window: {},
    $: () => ({offsetParent: c.page === 'linked' ? {} : null}),
    visibleSingleId: () => ids.includes(c.page) ? c.page : null,
    singleHost: () => ({querySelector: () => ({contains: () => true})}),
    singleView: id => c.missing === id ? null : {id},
    requestSingleView: () => {}, reportRendererStatus: () => {}, syncSharedScatter: () => {},
    syncPointControls: () => {}, drawAll: () => {},
    activateSingle: id => {c.stashSingleState(); c.singleActive = id; c.D = {id, n: 7};},
    stashSingleState: () => {if (c.singleActive) c.saved[c.singleActive] = c.D;},
    restoreLinkedSurface: () => {}, showUnavailable: () => {c.D = null;},
    setLinkedPresentationReady: value => {c.presented = value;},
    workspaceSummary: () => ({primaryReady: true}), reportWorkspaceReady: () => {},
    onData: bundle => {c.D = bundle;}, restoreWorkspace: state => c.restored.push(state),
    datasetMessageFresh: extra => !!extra && extra.dataset_context === 'generation-1',
    sameDatasetContext: (a, b) => a === b, configFingerprint: () => 'fp',
    hydrateLinkedGeometry: extra => {assert.strictEqual(c.D, c.linkedBundle); c.hydrated.push(extra); return extra;},
    hydrateSparseLinked: x => x, requestWireFallback: () => c.fallbacks++,
    sanitiseColors: () => {}, fillColorPicker: () => {}, renderGroupFilters: () => {}, renderLegend: () => {},
    // reportVisibility() clears the transient hover and any pinned card when the
    // visible surface changes; the browser regression covers their real bodies.
    clearTransientHover: () => {}, unpinTip: () => {}, closeCard: () => {},
    applyCloneSupplement: extra => {c.D.clone = extra.clone; return true;}
  };
  c.window = {CBViewWire: {unpack: buffer => buffer.extra},
    requestAnimationFrame: callback => c.frames.push(callback), setTimeout: callback => callback()};
  vm.createContext(c);
  vm.runInContext(['activateLinked', 'linkedSupplementFresh', 'applyHydratedSupplement', 'onBinarySupplement']
    .map(fn).join('\n') + visibility, c);
  c.go = page => {c.page = page; c.reportVisibility();};
  c.extra = overrides => Object.assign({dataset_id: 'ds1', dataset_context: 'generation-1',
    dataset_fingerprint: 'fp', progressive_token: 'p1', groups: {arrived: true}}, overrides);
  return c;
}

let routes = 0;
for (const from of ids) {
  for (const via of ['direct', 'groups', 'about', 'loadData']) {
    for (const to of [...ids, 'linked']) {
      const s = session();
      s.go(from); const before = s.D;
      if (via !== 'direct') {s.go(via); assert.strictEqual(s.saved[from], before);}
      s.go(to);
      if (to === 'linked') {
        assert.strictEqual(s.D, s.linkedBundle, `${from} -> ${via} -> Linked`);
        assert.strictEqual(s.singleActive, null);
        assert.deepStrictEqual(s.restored[0], {selection: [42]});
      } else {
        assert.strictEqual(s.singleActive, to);
        assert.strictEqual(s.D.id, to);
      }
      routes++;
    }
  }
  // Pending/uncached targets must save the outgoing page's state too.
  const pending = session(); pending.go(from); const before = pending.D;
  pending.missing = 'spatial_projection'; pending.go('about'); pending.go(pending.missing);
  assert.strictEqual(pending.saved[from], before);
  pending.go('linked'); assert.strictEqual(pending.D, pending.linkedBundle);

  // Supplement arriving while any specialist is mounted survives a neutral tab.
  const s = session(); s.go(from); const specialist = s.D;
  s.onBinarySupplement({byteLength: 10, extra: s.extra()});
  assert.strictEqual(s.D, specialist); assert.strictEqual(s.hydrated.length, 0);
  s.go('groups'); s.go('linked');
  assert.strictEqual(s.D.groups.arrived, true); assert.strictEqual(s.D.progressive, false);
  assert.strictEqual(s.hydrated.length, 1); assert.strictEqual(s.deferredLinkedSupplements.length, 0);
  assert.strictEqual(s.fallbacks, 0);
}

// Reject retired dataset generations, tokens, and fingerprints before queueing.
for (const override of [{dataset_context: 'generation-0'}, {dataset_id: 'ds0'},
  {progressive_token: 'p0'}, {dataset_fingerprint: 'old'}]) {
  const s = session(); s.go(ids[0]); s.onBinarySupplement({extra: s.extra(override)});
  assert.strictEqual(s.deferredLinkedSupplements.length, 0);
}
// A queued response must also be revalidated if a new primary replaces it.
{
  const s = session(); s.go(ids[0]); s.onBinarySupplement({extra: s.extra()});
  s.linkedBundle = Object.assign({}, s.linkedBundle, {progressive_token: 'p2'});
  s.go('linked'); assert.strictEqual(s.D.groups.arrived, undefined);
}
// Clone delivery uses an animation frame: switching pages during that frame
// must defer its hydrated payload and replay it only on returning to Linked.
{
  const s = session();
  const clone = s.extra({clone: {cells: [1]}, spaces: [{id: 'clone'}], progressive_complete: false});
  s.onBinarySupplement({extra: clone}); assert(s.pendingCloneSupplement);
  s.onBinarySupplement({extra: s.extra()}); assert.strictEqual(s.frames.length, 1);
  s.go(ids[1]); s.frames.shift()(); assert.strictEqual(s.D.clone, undefined);
  s.go('about'); s.go('linked'); assert.deepStrictEqual(s.D.clone, {cells: [1]});
  assert.strictEqual(s.fallbacks, 0);
}
console.log(`${routes} page routes and delayed supplement regressions passed`);

// A specialist canvas parked through a neutral page must never become the
// first Linked frame when this dataset has no cached Linked bundle yet.
for (const via of ['direct', 'groups', 'loadData']) {
  const s = session(); s.linkedBundle = null; s.go(ids[0]);
  if (via !== 'direct') s.go(via);
  s.go('linked');
  assert.strictEqual(s.D, null); assert.strictEqual(s.presented, false);
}

// Linked readiness is about ownership as well as cell count: even a same-size
// specialist or an older generation cannot acknowledge the Linked workspace.
{
  const base = {n:7, dataset_context:'current', progressive:true};
  const c = {D:base, linkedBundle:base, singleActive:null,
    panels:[{spaceId:'umap',canvas:{dataset:{pointCount:7}}}],
    datasetMessageFresh:x=>x.dataset_context==='current',configFingerprint:()=> 'fp',
    sel:null,selectedCellIds:()=>[],colorBy:'group',selectedProjections:['umap'],
    selectedSpatial:[],activeSpatial:()=>null,transportMetrics:{}};
  vm.createContext(c); vm.runInContext(fn('workspaceSummary'),c);
  assert(c.workspaceSummary().primaryReady); assert(!c.workspaceSummary().complete);
  c.D.progressive = false; assert(c.workspaceSummary().complete);
  c.singleActive = ids[0]; assert(!c.workspaceSummary().primaryReady);
  c.singleActive = null; c.D = {...base}; assert(!c.workspaceSummary().primaryReady);
  c.D = base; base.dataset_context='retired'; assert(!c.workspaceSummary().primaryReady);
}

// Background visibility is independent from coordinates/selection and applies
// only to Linked views; specialist pages still draw their own selected image.
{
  const checkbox = {}, panel = {spaceId: 'spatial', miniBg: {}, view: {cx: 5}};
  const c = {linkedBackgroundsOn: true, singleActive: null, panels: [panel],
    $: () => checkbox, spaceById: {spatial: {}}, calls: 0, drawn: 0,
    currentImage: () => {c.calls++; return null;}, viewOperation: () => {}, renderImagePicker: () => {}};
  vm.createContext(c);
  vm.runInContext(['drawImage','syncLinkedBackgroundsDefault','setLinkedBackgrounds','spatialSamples'].map(fn).join('\n'),c);
  c.drawAll = () => {c.drawn++; c.drawImage(panel);};
  c.setLinkedBackgrounds(false);
  assert.strictEqual(c.calls, 0);
  assert.strictEqual(checkbox.checked, false);
  assert.strictEqual(panel.miniBg, null);
  assert.strictEqual(panel.view.cx, 5);
  c.syncLinkedBackgroundsDefault(false);
  assert.strictEqual(c.linkedBackgroundsOn, false);
  c.singleActive = 'spatial_projection'; c.drawImage(panel);
  assert.strictEqual(c.calls, 1);
  c.singleActive = null; c.setLinkedBackgrounds(true);
  assert.strictEqual(c.calls, 2);
  c.setLinkedBackgrounds(false); c.syncLinkedBackgroundsDefault(true);
  assert.strictEqual(c.linkedBackgroundsOn, true);
  c.spatialTemplate = {name:'section-x',label:'section-x · zone-y (spatial)',x:[1],y:[2]};
  assert.strictEqual(c.spatialSamples()[0].name,'section-x');
  assert.strictEqual(c.spatialSamples()[0].label,c.spatialTemplate.label);
}

// Internal restores keep ROI point defaults active; importing portable JSON
// applies its display settings without leaking runtime-only fields to disk.
{
  const c = {exportWorkspace: () => ({view: {}}), pointSizeEdited: false, pointOpacityEdited: false};
  vm.createContext(c);
  vm.runInContext(fn('exportRuntimeWorkspace') + fn('workspacePointOverrides'), c);
  let saved = c.exportRuntimeWorkspace();
  assert.strictEqual(c.workspacePointOverrides(saved).size, false);
  assert.strictEqual(c.workspacePointOverrides(saved).opacity, false);
  assert.strictEqual(JSON.stringify(saved), '{"view":{}}');
  assert.strictEqual(c.workspacePointOverrides(JSON.parse(JSON.stringify(saved))).size, true);
  c.pointSizeEdited = true;
  saved = c.exportRuntimeWorkspace();
  assert.strictEqual(c.workspacePointOverrides(saved).size, true);
  assert.strictEqual(c.workspacePointOverrides(saved).opacity, false);
}
