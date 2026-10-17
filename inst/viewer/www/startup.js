/* The first page owns startup completion; no eager loading of other datasets. */
(function () {
  'use strict';
  function init() {
    var cover = document.getElementById('cerebro-startup');
    if (!cover) return;
    var finished = false;
    function finish(allowError) {
      var pane = document.querySelector('.tab-pane.active[id^="shiny-tab-"]');
      if (finished || !pane || (allowError !== true && !window.CerebroPageLifecycle.isSettled(pane))) return;
      finished = true;
      clearTimeout(slow);
      document.documentElement.classList.remove('cerebro-starting');
      // Cross a paint boundary after removing the shield on the actual layout.
      requestAnimationFrame(function () {
        cover.style.opacity = '0';
        setTimeout(function () { cover.remove(); },
          matchMedia('(prefers-reduced-motion: reduce)').matches ? 0 : 120);
      });
      window.removeEventListener('cerebro:page-ready', finish);
      window.removeEventListener('cerebro:view-terminal', finish);
      window.removeEventListener('cerebro:dataset-context', failed);
    }
    function failed(event) {
      // Let the normal error and global dataset picker remain usable.
      if (event.detail && event.detail.phase === 'error') finish(true);
    }
    var slow = setTimeout(function () {
      if (finished) return;
      cover.querySelector('[data-startup-message]').textContent = 'Still preparing your workspace…';
      cover.querySelector('[data-startup-retry]').hidden = false;
    }, 15000);
    cover.querySelector('[data-startup-retry]').addEventListener('click', function () { location.reload(); });
    window.addEventListener('cerebro:page-ready', finish);
    window.addEventListener('cerebro:view-terminal', finish);
    window.addEventListener('cerebro:dataset-context', failed);
    finish();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init, {once:true});
  else init();
})();
