/* Shared page completion contract, independent of visible feedback markup. */
(function () {
  'use strict';
  function isReady(pane) {
    return !!pane && pane.dataset.cerebroPageReady === 'true';
  }
  function isTerminal(pane) {
    return !!pane && pane.dataset.cerebroPageTerminal === 'true';
  }
  function setTerminal(pane, terminal) {
    if (!pane) return;
    if (terminal) {
      pane.dataset.cerebroPageTerminal = 'true';
      delete pane.dataset.cerebroPageReady;
    } else delete pane.dataset.cerebroPageTerminal;
  }
  function setReady(pane) {
    if (!pane) return;
    setTerminal(pane, false);
    pane.dataset.cerebroPageReady = 'true';
  }
  function reset(pane) {
    if (!pane) return;
    delete pane.dataset.cerebroPageReady;
    delete pane.dataset.cerebroPageTerminal;
  }
  window.CerebroPageLifecycle = Object.freeze({
    isReady: isReady,
    isTerminal: isTerminal,
    isSettled: function (pane) { return isReady(pane) || isTerminal(pane); },
    setReady: setReady,
    setTerminal: setTerminal,
    reset: reset
  });
})();
