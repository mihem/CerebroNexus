(function () {
  'use strict';

  var activeDrawer = null;
  var activeButton = null;

  function setModalMode(drawer) {
    drawer.setAttribute(
      'aria-modal',
      window.matchMedia('(max-width: 900px)').matches ? 'true' : 'false'
    );
  }

  function closeDrawer(restoreFocus) {
    if (!activeDrawer) return;
    var drawer = activeDrawer;
    var button = activeButton;
    drawer.classList.remove('is-open');
    drawer.setAttribute('aria-hidden', 'true');
    if (button) button.setAttribute('aria-expanded', 'false');
    window.clearTimeout(drawer._cerebroUnmountTimer);
    drawer._cerebroUnmountTimer = window.setTimeout(function () {
      if (!drawer.classList.contains('is-open')) {
        drawer.classList.remove('is-mounted');
      }
    }, 240);
    activeDrawer = null;
    activeButton = null;
    if (restoreFocus !== false && button && drawer.contains(document.activeElement)) {
      button.focus();
    }
  }

  function openDrawer(drawer, button) {
    document.dispatchEvent(new CustomEvent('cerebro:overlay-opening', {
      detail: { owner: 'settings:' + drawer.id }
    }));
    closeDrawer(false);
    setModalMode(drawer);
    window.clearTimeout(drawer._cerebroUnmountTimer);
    drawer.classList.add('is-mounted');
    void drawer.offsetWidth;
    drawer.classList.add('is-open');
    drawer.setAttribute('aria-hidden', 'false');
    button.setAttribute('aria-expanded', 'true');
    activeDrawer = drawer;
    activeButton = button;
    window.requestAnimationFrame(function () {
      var close = drawer.querySelector('[data-cerebro-drawer-close]');
      if (close && activeDrawer === drawer) close.focus();
    });
  }

  document.addEventListener('click', function (event) {
    var trigger = event.target.closest('[data-cerebro-drawer-target]');
    if (trigger) {
      var drawer = document.getElementById(
        trigger.getAttribute('data-cerebro-drawer-target')
      );
      if (!drawer) return;
      if (activeDrawer === drawer) closeDrawer();
      else openDrawer(drawer, trigger);
      return;
    }
    if (event.target.closest('[data-cerebro-drawer-close]')) closeDrawer();
  });

  document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape' && activeDrawer) {
      event.preventDefault();
      closeDrawer();
    }
  });

  document.addEventListener('cerebro:overlay-opening', function (event) {
    if (
      activeDrawer &&
      event.detail &&
      event.detail.owner !== 'settings:' + activeDrawer.id
    ) {
      closeDrawer(false);
    }
  });

  window.addEventListener('resize', function () {
    if (activeDrawer) setModalMode(activeDrawer);
  });
})();
