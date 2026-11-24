// Which panels of a page of many are on stage (near the window) is told by an
// IntersectionObserver with a margin of one window height each way, and read
// from the panels' rectangles only where there is none (offStage()).
const fs = require('fs'), assert = require('assert/strict');
const source = fs.readFileSync(process.argv[2], 'utf8');
function between(from, to) {
  const start = source.indexOf(from), end = source.indexOf(to, start);
  assert.ok(start >= 0 && end > start, 'missing ' + from);
  return source.slice(start, end);
}
var focusPanel = null, wakes = 0;
function offStrip() { return false; }
function onStageScroll() { wakes++; }
const observers = [];
function IntersectionObserverStub(callback, options) {
  this.callback = callback; this.options = options; this.targets = [];
  observers.push(this);
}
IntersectionObserverStub.prototype.observe = function (target) { this.targets.push(target); };
var window = {innerHeight: 1000, IntersectionObserver: IntersectionObserverStub};
eval(between('  var OFFSTAGE_MIN_PANELS', '  var stageFrame = 0;'));

const pane = top => ({getBoundingClientRect: () => ({top, bottom: top + 290, width: 300})});
const makePanels = n => Array.from({length: n}, (_, i) => ({key: 'p' + i, spaceId: 's' + i, pane: pane(i * 300)}));

// A few panels are all on stage.
let few = makePanels(3);
stagePanels = 3;
assert.equal(few.some(p => offStage(p)), false, 'below the minimum nothing is off stage');

// Many panels: the observer is made once, with its margin, and watches each pane once.
let panels = makePanels(20);
stagePanels = 20;
assert.equal(observeStagePanels(panels), true);
assert.equal(observers.length, 1);
assert.deepEqual(observers[0].options, {root: null, rootMargin: '100% 0px'});
assert.equal(observers[0].targets.length, 20);
observeStagePanels(panels);
assert.equal(observers[0].targets.length, 20, 'a pane is watched once');
// Before the observer has told anything the rectangles are read.
assert.equal(offStage(panels[0]), false, 'in the window');
assert.equal(offStage(panels[19]), true, 'far below it (top 5700 of a window of 1000)');
// Then the observer decides.
const entry = (p, isIntersecting) => ({target: p.pane, isIntersecting});
observers[0].callback([entry(panels[19], true), entry(panels[0], false)]);
assert.equal(offStage(panels[19]), false, 'told to be near');
assert.equal(offStage(panels[0]), true, 'told to be away');
// A panel that comes near with a drawing or a size waiting wakes the drawer; one
// with nothing waiting does not.
wakes = 0;
panels[5]._resizedUndrawn = true;
observers[0].callback([entry(panels[5], true)]);
assert.equal(wakes, 1);
observers[0].callback([entry(panels[6], true)]);
assert.equal(wakes, 1, 'nothing waiting for panel 6');
// An enlarged view counts every panel on stage.
focusPanel = 'A';
assert.equal(offStage(panels[0]), false);
focusPanel = null;

// No IntersectionObserver: the rectangles are read, a window above and below.
window = {innerHeight: 1000};
stageObserver = null;
panels = makePanels(20);
assert.equal(observeStagePanels(panels), false);
assert.equal(offStage(panels[2]), false, 'top 600: in the window');
assert.equal(offStage(panels[6]), false, 'top 1800: within a window height below it');
assert.equal(offStage(panels[9]), true, 'top 2700: past it');
console.log('PASS the stage observer tells near panels from far ones, and the rectangles stand in without it');
