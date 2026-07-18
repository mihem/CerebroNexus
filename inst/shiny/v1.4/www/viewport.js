/* Shared viewport sizing lifecycle.
 *
 * ONE place decides how tall a plot card is and WHEN it becomes visible, so
 * every tab behaves identically instead of each carrying its own copy of the
 * measure/settle/reveal dance.
 *
 * Model: a visual card exposes a `cerebro-viewport-host` (the element we size)
 * inside a `cerebro-viewport-gate` (kept hidden by CSS until settled). This
 * controller owns only the common mechanics — frame coalescing (one measure per
 * animation frame), scoped observation, the target-height formula, and the
 * "reveal after two equal-height frames" discipline that hides the first-paint
 * jump. Renderer-specific work lives in an ADAPTER:
 *   - the built-in `stageAdapter` below drives ordinary Shiny/Plotly output
 *     cards (e.g. the immune-repertoire tabs), and
 *   - projection_scatter.js registers its own adapter for the scatter engine.
 *
 * Why two equal frames before reveal: the first measurement after data lands is
 * not final (a spinner swaps for the output, a legend wraps, a widget reports
 * its size a frame later), so revealing immediately shows one height and then
 * corrects to another — the visible "short-then-tall" flash. Waiting for two
 * frames that agree means the first frame the user sees is already final.
 */
(function () {
  "use strict";

  var DEFAULT_GAP = 18;
  var DEFAULT_MIN_HEIGHT = 240;
  var HOST_CLASS = "cerebro-viewport-host";
  var GATE_CLASS = "cerebro-viewport-gate";
  var READY_CLASS = "is-sized";
  var NATURAL_CLASS = "cerebro-viewport-natural";
  var states = new WeakMap();
  var hosts = new Set();
  var settledOutputs = new WeakSet();

  function targetHeight(viewportHeight, top, contentBelow, gap, minimum) {
    return Math.max(
      minimum,
      Math.floor(viewportHeight - top - contentBelow - gap)
    );
  }

  function shouldReveal(previousHeight, height) {
    return previousHeight === height;
  }

  function isVisible(host) {
    return Boolean(
      host &&
      typeof host.getClientRects === "function" &&
      host.getClientRects().length > 0
    );
  }

  function ancestorWithClass(host, className) {
    var node = host;
    while (node) {
      if (node.classList && node.classList.contains(className)) {
        return node;
      }
      node = node.parentElement;
    }
    return null;
  }

  function layoutTargets(host) {
    var targets = [];
    var node = host;
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

  function gateFor(host) {
    return host && typeof host.closest === "function" ?
      host.closest("." + GATE_CLASS) : null;
  }

  function setGateReady(host, ready) {
    var gate = gateFor(host);
    if (!gate || !gate.classList) {
      return;
    }
    if (ready) {
      gate.classList.add(READY_CLASS);
    } else {
      gate.classList.remove(READY_CLASS);
    }
  }

  /* Everything below the sized host inside its nearest visual box. Both box
   * and host bottoms move by the same delta when the host grows, so this value
   * is invariant under the controller's own height write. */
  function contentBelow(host) {
    if (!host || typeof host.getBoundingClientRect !== "function") {
      return 0;
    }
    var root = typeof host.closest === "function" ? host.closest(".box") : null;
    if (!root) {
      root = ancestorWithClass(host, "content");
    }
    if (!root || typeof root.getBoundingClientRect !== "function") {
      return 0;
    }
    return Math.max(
      0,
      root.getBoundingClientRect().bottom -
        host.getBoundingClientRect().bottom
    );
  }

  function sameTargets(left, right) {
    if (left.length !== right.length) {
      return false;
    }
    for (var i = 0; i < left.length; i++) {
      if (left[i] !== right[i]) {
        return false;
      }
    }
    return true;
  }

  function refreshObserver(host, state) {
    if (typeof window.ResizeObserver !== "function") {
      return;
    }
    var adapter = state.adapter;
    var targets = adapter.observeTargets ?
      adapter.observeTargets(host, state) : layoutTargets(host);
    targets = (targets || []).filter(Boolean);
    if (sameTargets(state.observedTargets, targets)) {
      return;
    }
    if (!state.observer) {
      state.observer = new window.ResizeObserver(function () {
        schedule(host);
      });
    }
    for (var i = 0; i < state.observedTargets.length; i++) {
      state.observer.unobserve(state.observedTargets[i]);
    }
    for (var j = 0; j < targets.length; j++) {
      state.observer.observe(targets[j]);
    }
    state.observedTargets = targets;
  }

  function run(host, state) {
    var adapter = state.adapter;
    if (!adapter || !isVisible(host) || typeof window.innerHeight !== "number") {
      return;
    }
    refreshObserver(host, state);
    var measurement = adapter.measure(host, state);
    if (!measurement) {
      return;
    }
    /* Resolve the floor through measurement -> adapter -> default, so an adapter
     * (or one measurement) can raise the minimum without callers repeating the
     * constant. */
    var minimum = measurement.minimum;
    if (minimum == null) {
      minimum = adapter.minimum == null ? DEFAULT_MIN_HEIGHT : adapter.minimum;
    }
    var gap = adapter.gap == null ? DEFAULT_GAP : adapter.gap;
    /* An adapter that measured its own height (natural mode) uses it verbatim;
     * otherwise the height fills the viewport minus this element's live top and
     * whatever content sits below it. */
    var height = measurement.height == null ?
      targetHeight(
        window.innerHeight,
        measurement.top,
        measurement.contentBelow,
        gap,
        minimum
      ) :
      Math.floor(measurement.height);
    var width = Math.floor(measurement.width || 0);
    var matches = adapter.matches ?
      adapter.matches(host, height, width, state) :
      state.height === height && state.width === width;

    if (state.height !== height || state.width !== width || !matches) {
      state.height = height;
      state.width = width;
      adapter.apply(host, height, width, state, function () {
        schedule(host);
      });
    }

    var revealed = adapter.isRevealed ? adapter.isRevealed(host, state) : false;
    if (revealed) {
      return;
    }
    var ready = adapter.ready ?
      adapter.ready(host, height, width, state) : true;
    /* A pre-init widget is not evidence that its data-bearing layout has
     * settled. Do not let an unready frame prime the two-frame reveal gate. */
    if (!ready) {
      return;
    }
    if (shouldReveal(state.settledHeight, height)) {
      adapter.reveal(host, state);
      return;
    }
    state.settledHeight = height;
    if (!state.relayoutPending) {
      schedule(host);
    }
  }

  function schedule(host) {
    var state = states.get(host);
    if (!state || state.frame !== null) {
      return;
    }
    state.frame = window.requestAnimationFrame(function () {
      state.frame = null;
      run(host, state);
    });
  }

  function register(host, adapter) {
    if (!host || !adapter) {
      return null;
    }
    var state = states.get(host);
    if (!state) {
      state = {
        adapter: adapter,
        frame: null,
        height: null,
        width: null,
        settledHeight: null,
        relayoutPending: false,
        observer: null,
        observedTargets: []
      };
      states.set(host, state);
      hosts.add(host);
      if (adapter.hide) {
        adapter.hide(host, state);
      }
    } else {
      state.adapter = adapter;
    }
    if (adapter.prepare) {
      adapter.prepare(host, state);
    }
    schedule(host);
    return state;
  }

  function pruneHosts() {
    hosts.forEach(function (host) {
      if (host.isConnected !== false) {
        return;
      }
      var state = states.get(host);
      if (state && state.observer) {
        state.observer.disconnect();
      }
      if (
        state &&
        state.frame !== null &&
        typeof window.cancelAnimationFrame === "function"
      ) {
        window.cancelAnimationFrame(state.frame);
      }
      states.delete(host);
      hosts.delete(host);
    });
  }

  function resizeAll() {
    pruneHosts();
    hosts.forEach(function (host) {
      schedule(host);
    });
  }

  /* ---- stage adapter -------------------------------------------------------
   * The default adapter for ordinary output cards (the immune-repertoire tabs
   * and anything tagged `cerebro-viewport-host`). A "stage" is the active
   * tab-pane inside the host; the helpers below inspect it to decide the height,
   * whether its output has finished rendering, and whether the card opts out of
   * viewport sizing entirely (natural mode, for faceted plots that set their own
   * height and scroll). Projection cards do NOT use this adapter — they register
   * their own from projection_scatter.js. */

  /* The tab-pane currently on screen (or the host itself when it has no tabs);
   * everything the stage adapter measures is scoped to it, never a hidden pane. */
  function activePane(host) {
    if (!host || typeof host.querySelector !== "function") {
      return host;
    }
    return host.querySelector(".tab-pane.active") || host;
  }

  function stageIsNatural(host) {
    var pane = activePane(host);
    return Boolean(
      pane &&
      typeof pane.querySelector === "function" &&
      pane.querySelector("." + NATURAL_CLASS)
    );
  }

  function clearInlineHeight(host) {
    if (!host || !host.style) {
      return;
    }
    if (typeof host.style.removeProperty === "function") {
      host.style.removeProperty("height");
    } else {
      host.style.height = "";
    }
  }

  function projectionStagePresent(host) {
    var pane = activePane(host);
    return Boolean(
      pane &&
      typeof pane.querySelector === "function" &&
      pane.querySelector(".cerebro-projection-host")
    );
  }

  function ordinaryStagePlot(host) {
    if (projectionStagePresent(host)) {
      return null;
    }
    var pane = activePane(host);
    return pane && typeof pane.querySelector === "function" ?
      pane.querySelector(".js-plotly-plot") : null;
  }

  /* The stage minimum applies to the renderer, not to the surrounding tab
   * chrome. Measure the invariant space occupied by tabs, controls and legends
   * so a 240px minimum can never collapse into a tiny plot on narrow screens. */
  function stageMinimumHeight(host) {
    var pane = activePane(host);
    var plot = pane && typeof pane.querySelector === "function" ?
      pane.querySelector(".js-plotly-plot, .shiny-plot-output") : null;
    if (
      plot &&
      typeof plot.getBoundingClientRect === "function" &&
      typeof host.getBoundingClientRect === "function"
    ) {
      return DEFAULT_MIN_HEIGHT + Math.max(
        0,
        host.getBoundingClientRect().height -
          plot.getBoundingClientRect().height
      );
    }
    var tabs = typeof host.querySelector === "function" ?
      host.querySelector(".nav-tabs") : null;
    var tabsHeight = tabs && typeof tabs.getBoundingClientRect === "function" ?
      tabs.getBoundingClientRect().height : 0;
    return DEFAULT_MIN_HEIGHT + Math.max(0, tabsHeight);
  }

  function stagePlotMatches(host) {
    var plot = ordinaryStagePlot(host);
    if (!plot || !plot._fullLayout || !plot.getBoundingClientRect) {
      return !plot;
    }
    var rect = plot.getBoundingClientRect();
    return Boolean(
      Math.abs(plot._fullLayout.height - rect.height) <= 1 &&
      Math.abs(plot._fullLayout.width - rect.width) <= 1
    );
  }

  /* True while the active pane's output is still rendering, so reveal waits for
   * real content rather than flashing an empty card. Pending means any of: a
   * Shiny "recalculating" overlay is up, a Plotly widget has no _fullLayout yet,
   * a static plot's <img> has not loaded, or an html output has not emitted its
   * first shiny:value. An error/alert counts as done (nothing more is coming),
   * so a failed output still reveals instead of hanging hidden forever. */
  function stageHasPendingOutput(host) {
    var pane = activePane(host);
    if (!pane || typeof pane.querySelector !== "function") {
      return false;
    }
    if (pane.querySelector(".shiny-output-error, .alert")) {
      return false;
    }
    if (pane.querySelector(".recalculating")) {
      return true;
    }
    var plotlyOutput = pane.querySelector(".plotly.html-widget-output");
    if (plotlyOutput && !plotlyOutput._fullLayout) {
      return true;
    }
    var staticOutput = pane.querySelector(".shiny-plot-output");
    if (staticOutput) {
      var image = staticOutput.querySelector("img");
      if (
        !image ||
        image.complete === false ||
        (typeof image.naturalWidth === "number" && image.naturalWidth === 0)
      ) {
        return true;
      }
    }
    if (typeof pane.querySelectorAll !== "function") {
      return false;
    }
    var htmlOutputs = pane.querySelectorAll(".shiny-html-output");
    for (var i = 0; i < htmlOutputs.length; i++) {
      if (
        (!htmlOutputs[i].childNodes || htmlOutputs[i].childNodes.length === 0) &&
        !settledOutputs.has(htmlOutputs[i])
      ) {
        return true;
      }
    }
    return false;
  }

  /* Sync a plain (non-projection) Plotly output's internal canvas to the host
   * size. Setting CSS height alone leaves Plotly's SVG at its old size, so a
   * relayout with transition.duration 0 snaps it instantly. relayoutPending
   * prevents overlapping relayouts and tells the reveal gate to wait for the
   * repaint, so the card is never shown at the pre-relayout size. */
  function resizeStagePlot(host, state, done) {
    var plot = ordinaryStagePlot(host);
    if (
      !plot ||
      !plot._fullLayout ||
      stagePlotMatches(host) ||
      state.relayoutPending ||
      typeof Plotly === "undefined" ||
      typeof Plotly.relayout !== "function"
    ) {
      return;
    }
    var rect = plot.getBoundingClientRect();
    if (!(rect.width > 0 && rect.height > 0)) {
      return;
    }
    state.relayoutPending = true;
    var finish = function () {
      state.relayoutPending = false;
      done();
    };
    var relayout = Plotly.relayout(plot, {
      width: Math.floor(rect.width),
      height: Math.floor(rect.height),
      "transition.duration": 0
    });
    if (relayout && typeof relayout.then === "function") {
      relayout.then(finish, finish);
    } else {
      finish();
    }
  }

  var stageAdapter = {
    gap: 18,
    minimum: 240,
    hide: function (host) {
      setGateReady(host, false);
    },
    prepare: function (host, state) {
      var natural = stageIsNatural(host);
      if (state.natural !== natural) {
        state.natural = natural;
        state.height = null;
        state.width = null;
        state.settledHeight = null;
        setGateReady(host, false);
      }
      if (host.classList) {
        host.classList.toggle("is-natural", natural);
      }
      if (natural) {
        clearInlineHeight(host);
      }
    },
    measure: function (host, state) {
      var rect = host.getBoundingClientRect();
      return {
        top: rect.top,
        contentBelow: contentBelow(host),
        width: rect.width,
        minimum: stageMinimumHeight(host),
        height: state.natural ? rect.height : null
      };
    },
    matches: function (host, height, width, state) {
      if (state.height !== height || state.width !== width) {
        return false;
      }
      if (state.natural) {
        return true;
      }
      return parseFloat(host.style.height) === height && stagePlotMatches(host);
    },
    apply: function (host, height, width, state, done) {
      if (state.natural) {
        clearInlineHeight(host);
      } else {
        host.style.height = height + "px";
        resizeStagePlot(host, state, done);
      }
    },
    ready: function (host, height, width, state) {
      if (projectionStagePresent(host)) {
        return false;
      }
      return Boolean(
        !state.relayoutPending &&
        !stageHasPendingOutput(host) &&
        stagePlotMatches(host)
      );
    },
    isRevealed: function (host) {
      var gate = gateFor(host);
      return Boolean(gate && gate.classList.contains(READY_CLASS));
    },
    reveal: function (host) {
      setGateReady(host, true);
    },
    observeTargets: layoutTargets
  };

  function registerStages() {
    if (!document || typeof document.getElementsByClassName !== "function") {
      return;
    }
    pruneHosts();
    var stageHosts = document.getElementsByClassName(HOST_CLASS);
    for (var i = 0; i < stageHosts.length; i++) {
      register(stageHosts[i], stageAdapter);
    }
  }

  function registerStageNear(event) {
    pruneHosts();
    var target = event && event.target;
    if (!target) {
      return;
    }
    var nearest = typeof target.closest === "function" ?
      target.closest("." + HOST_CLASS) : null;
    if (nearest) {
      register(nearest, stageAdapter);
    }
    if (typeof target.querySelectorAll !== "function") {
      return;
    }
    var nested = target.querySelectorAll("." + HOST_CLASS);
    for (var i = 0; i < nested.length; i++) {
      if (nested[i] !== nearest) {
        register(nested[i], stageAdapter);
      }
    }
  }

  function handleShinyValue(event) {
    var target = event && event.target;
    if (
      target &&
      target.classList &&
      target.classList.contains("shiny-html-output")
    ) {
      settledOutputs.add(target);
    }
    registerStageNear(event);
    var source = target && typeof target.closest === "function" ?
      target.closest(".cerebro-viewport-source") : null;
    if (
      source &&
      typeof source.querySelector === "function" &&
      !source.querySelector("." + HOST_CLASS)
    ) {
      setGateReady(source, true);
    }
  }

  /* Wiring. Shiny inserts and swaps these cards well after load (dynamically
   * inserted IR tabs, tab switches), so hosts are (re)registered as they appear
   * — on shiny:value, tab-shown and connect — not once at startup. A window
   * resize re-measures every live host and prunes any that were removed. */
  window.addEventListener("resize", resizeAll);

  if (document && typeof document.addEventListener === "function") {
    if (!window.jQuery) {
      document.addEventListener("shiny:value", handleShinyValue);
    }
    document.addEventListener("shiny:connected", registerStages);
    document.addEventListener("DOMContentLoaded", registerStages);
  }
  if (window.jQuery) {
    window.jQuery(document).on(
      "shiny:value",
      handleShinyValue
    );
    window.jQuery(document).on(
      "shown.bs.tab shiny:visualchange",
      registerStageNear
    );
  }

  window.cerebroViewport = {
    register: register,
    resize: schedule,
    resizeAll: resizeAll,
    _targetHeight: targetHeight,
    _shouldReveal: shouldReveal,
    _isVisible: isVisible,
    _layoutTargets: layoutTargets,
    _contentBelow: contentBelow,
    _gateFor: gateFor,
    _stageAdapter: stageAdapter,
    _registerStages: registerStages
  };
  if (
    typeof window.dispatchEvent === "function" &&
    typeof Event === "function"
  ) {
    window.dispatchEvent(new Event("cerebro:viewport-ready"));
  }
  registerStages();
})();
