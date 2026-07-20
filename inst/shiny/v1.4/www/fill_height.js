/* ==========================================================================
   Fill-to-viewport height, measured live.

   The systematic answer to "the plot is sometimes too short, sometimes taller
   than the screen." Every viz page has the same shape — sidebar | params | plot
   — but each output used to hardcode its own height: a fixed `640px` (ignores
   the viewport entirely) or `calc(100vh - <N>px)` where N is a hand-measured
   guess at the chrome above the plot. Both break the moment a margin, a title,
   a tab strip or a legend changes: the guess is now wrong and the plot either
   overflows or leaves a dead band.

   The robust height is not a constant. It is:

       height = viewport - (top of this element) - (bottom breathing room)

   `top of this element` is the live sum of everything above it (top bar, box
   title, tab strip, a wrapping legend), read from the DOM with
   getBoundingClientRect(). Nothing is hardcoded, so changing any spacing above
   the plot re-measures on the next frame and the height corrects itself. This
   is the same primitive projection_scatter.js already uses for the scatter
   plots (projectionTargetHeight); this file generalises it to any element that
   opts in with the `cerebro-fill` class.

   Opt in from R:

       div(class = "cerebro-fill", <the output at height = "100%">)

   custom.css makes `.cerebro-fill` a flex column whose child fills it, so the
   output (and any spinner wrapper between) inherits the measured height without
   needing its own resolved-height chain.

   TWO ENGINES, ON PURPOSE (why a fill page and a projection scatter can differ
   by a few dozen px on the same screen): the projection scatter pages were never
   migrated here -- they keep projectionTargetHeight in projection_scatter.js.
   Both use the SAME formula (viewport - top - contentBelow - gap), but this
   file's contentBelow() also reserves the content-wrapper's bottom padding,
   which projection_scatter's does not. So e.g. the HLA network (a fill page)
   sits slightly shorter than the projection scatter. That is the two engines,
   not a bug; unifying them touches every viz page and is deliberately deferred.
   ========================================================================== */
(function () {
  "use strict";

  var FILL_CLASS = "cerebro-fill";
  /* A small safety margin only. The real bottom breathing room comes from
     contentBelow(), which measures the remaining section content and the
     content-wrapper's padding-bottom. This is just a couple of pixels of slack
     so sub-pixel rounding can never tip the page into a scrollbar. */
  var BOTTOM_GAP = 4;
  /* Never collapse below this, however cramped the viewport: a plot shorter than
     this is useless, better to let the page scroll. */
  var MIN_HEIGHT = 240;

  /* height = viewport - element.top - contentBelow - gap, floored, clamped.
     Pure: measurements in, pixels out. `contentBelow` is everything that must
     stay visible under this element (a details panel, a note, a download row)
     plus the card's own bottom padding, so the WHOLE card fits the viewport
     rather than the plot filling it and shoving the rest off-screen. */
  function targetHeight(viewportHeight, elementTop, contentBelow, gap, minimum) {
    return Math.max(
      minimum,
      Math.floor(viewportHeight - elementTop - contentBelow - gap)
    );
  }

  function px(value) {
    var n = parseFloat(value);
    return isFinite(n) ? n : 0;
  }

  function isVisible(el) {
    return Boolean(
      el &&
      typeof el.getClientRects === "function" &&
      el.getClientRects().length > 0
    );
  }

  function ancestorWithClass(el, className) {
    var node = el;
    while (node) {
      if (node.classList && node.classList.contains(className)) {
        return node;
      }
      node = node.parentElement;
    }
    return null;
  }

  /* Observe only the layout chain that can move or resize this fill. Stopping
     at section.content deliberately excludes body/html: observing the whole
     document makes unrelated Shiny output changes schedule every plot. */
  function layoutTargets(el) {
    var targets = [];
    var node = el;
    while (
      node &&
      node !== document.body &&
      node !== document.documentElement
    ) {
      targets.push(node);
      if (node.classList && node.classList.contains("content")) {
        break;
      }
      node = node.parentElement;
    }
    return targets;
  }

  /* Measure the content that remains after the fill inside section.content;
     its parent content-wrapper is the actual scroll container. This replaces
     a deep sibling walk with one invariant:

       remaining = content.scrollHeight - fill bottom within content

     If the fill grows by 100 px, both scrollHeight and its bottom grow by 100,
     so the result stays stable. The wrapper's bottom padding lives outside the
     section.content scroll height and is therefore added once. */
  function contentBelow(el) {
    /* Measure within the fill's OWN plot column (.cerebro-viz-col) when it has
       one, so a TALLER sibling column — a long parameter / legend / evidence
       panel beside the plot — cannot inflate `remaining` and squeeze the fill to
       its minimum. `content.scrollHeight` is the height of the tallest column, so
       using it lets a tall left column shrink the plot on the right. Fall back to
       section.content for any fill that is not inside a plot column (behaviour
       unchanged there). For a well-behaved page whose plot column is the tallest,
       both frames give the same result. */
    var content = ancestorWithClass(el, "content");
    var frame = el.closest && el.closest(".cerebro-viz-col");
    if (!frame) {
      frame = content;
    }
    if (!frame || typeof frame.getBoundingClientRect !== "function") {
      return 0;
    }
    var wrapper = content ?
      ancestorWithClass(content.parentElement, "content-wrapper") : null;
    var frameRect = frame.getBoundingClientRect();
    var fillRect = el.getBoundingClientRect();
    var fillBottom = fillRect.bottom - frameRect.top + (frame.scrollTop || 0);
    var remaining = Math.max(0, (frame.scrollHeight || 0) - fillBottom);
    var wrapperPadding = wrapper ?
      px(window.getComputedStyle(wrapper).paddingBottom) : 0;
    return remaining + wrapperPadding;
  }

  function sizeOne(el) {
    if (!el || typeof window.innerHeight !== "number") {
      return;
    }
    /* A hidden element (for example, one inside an inactive tab) has no client
       rectangles. Skip it until layout resumes; the observers below re-fire
       when the tab becomes visible. This also handles hidden elements whose
       stale bounding box does not happen to start at zero. */
    if (!isVisible(el)) {
      return;
    }
    var top = el.getBoundingClientRect().top;
    var h = targetHeight(
      window.innerHeight,
      top,
      contentBelow(el),
      BOTTOM_GAP,
      MIN_HEIGHT
    );
    /* Idempotent: only write when the value actually changed. This is what keeps
       the ResizeObserver below from looping — resizing this element changes the
       page height, which fires the observer, which recomputes the SAME height
       (our own top did not move), so the guard stops here instead of ping-pong. */
    if (el.__cerebroFillH !== h) {
      el.__cerebroFillH = h;
      el.style.height = h + "px";
    }
    /* Reveal only once the measured height has SETTLED — two consecutive frames
       computing the same height. The first measurement after an output arrives
       is not final: a spinner swaps for the real output, sibling content lays
       out, a widget reports its size a frame later, and each nudges contentBelow
       (hence the target height). Revealing on the first measurement would show
       one height and then correct to another — the "short-then-tall" flash. This
       is the same two-equal-frames discipline projection_scatter.js uses
       (shouldRevealProjection); generalised here so plain fills reveal the same
       way. When not yet settled, record this height and force a confirming frame
       so a re-measure is guaranteed even without an external trigger. */
    if (!el.classList.contains("is-filled")) {
      if (shouldReveal(el.__cerebroSettledH, h)) {
        el.classList.add("is-filled");
      } else {
        el.__cerebroSettledH = h;
        scheduleSize();
      }
    }
  }

  /* Pure: reveal once a measurement equals the previous one. Undefined previous
     (first ever measurement) is never a match, so reveal always waits for a
     confirming frame. */
  function shouldReveal(previousHeight, height) {
    return previousHeight === height;
  }

  var ro = null;
  var observedTargets = typeof window.WeakSet === "function" ?
    new window.WeakSet() : null;

  function observeLayout(el) {
    if (!ro) {
      return;
    }
    var targets = layoutTargets(el);
    for (var i = 0; i < targets.length; i++) {
      if (!observedTargets || !observedTargets.has(targets[i])) {
        ro.observe(targets[i]);
        if (observedTargets) {
          observedTargets.add(targets[i]);
        }
      }
    }
  }

  function sizeAll() {
    var els = document.getElementsByClassName(FILL_CLASS);
    for (var i = 0; i < els.length; i++) {
      observeLayout(els[i]);
      sizeOne(els[i]);
    }
  }

  /* Coalesce bursts of triggers into one measurement per frame. */
  var pending = false;
  function scheduleSize() {
    if (pending) {
      return;
    }
    pending = true;
    window.requestAnimationFrame(function () {
      pending = false;
      sizeAll();
    });
  }

  window.addEventListener("resize", scheduleSize);

  /* Chrome above the plot can change height WITHOUT a window resize — a legend
     wraps to another row, a caveat banner appears, the tab strip changes. Watch
     the fill's content ancestry so those local changes re-measure it without
     making unrelated changes elsewhere in the app schedule every plot. */
  if (typeof window.ResizeObserver === "function") {
    ro = new window.ResizeObserver(scheduleSize);
  }

  /* Shiny re-renders outputs and swaps tab panes after the initial paint, so a
     fill element can arrive (or become visible) well after load. Re-measure when
     Shiny reports a value and when a Bootstrap tab is shown. */
  document.addEventListener("shiny:value", scheduleSize);
  document.addEventListener("shiny:connected", scheduleSize);
  if (window.jQuery) {
    window.jQuery(document).on(
      "shown.bs.tab shiny:visualchange",
      scheduleSize
    );
  }

  document.addEventListener("DOMContentLoaded", scheduleSize);
  scheduleSize();

  /* Exposed for unit testing the pure height formula and re-measuring on demand. */
  window.cerebroFill = {
    _targetHeight: targetHeight,
    _isVisible: isVisible,
    _layoutTargets: layoutTargets,
    _contentBelow: contentBelow,
    _shouldReveal: shouldReveal,
    resize: scheduleSize
  };
})();
