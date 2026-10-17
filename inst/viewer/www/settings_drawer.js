(function () {
  'use strict';

  var activeDrawer = null;
  var activeButton = null;
  var inertBackground = [];
  var horizontalPositions = Object.create(null);
  var drag = null;

  function horizontalRoom(drawer) {
    return Math.max(0, document.documentElement.clientWidth - drawer.offsetWidth);
  }

  function positionDrawer(drawer) {
    var right = isModal() ? 0 : (horizontalPositions[drawer.id] || 0) * horizontalRoom(drawer);
    drawer.style.setProperty('--cerebro-drawer-right', right + 'px');
    drawer.classList.toggle('is-floating', right > 0.5);
  }

  function moveDrawer(drawer, right) {
    var room = horizontalRoom(drawer);
    right = Math.max(0, Math.min(room, right));
    horizontalPositions[drawer.id] = room ? right / room : 0;
    positionDrawer(drawer);
  }

  function finishDrag() {
    if (!drag) return;
    var previous = drag;
    drag = null;
    previous.drawer.classList.remove('is-dragging');
    if (previous.handle.hasPointerCapture(previous.pointer)) {
      previous.handle.releasePointerCapture(previous.pointer);
    }
  }

  function ensureDragHandle(drawer) {
    var titlebar = drawer.querySelector('.cerebro-settings-titlebar');
    if (!titlebar || titlebar.querySelector('.cerebro-settings-drag')) return;
    var handle = document.createElement('button');
    handle.type = 'button';
    handle.className = 'cerebro-settings-drag';
    handle.setAttribute('aria-label', 'Move Settings panel horizontally');
    var tip = document.createElement('span');
    tip.className = 'cerebro-settings-drag-tip';
    tip.id = drawer.id + '_drag_tip';
    tip.setAttribute('role', 'tooltip');
    tip.textContent = 'Drag to move · or use ← → keys';
    handle.setAttribute('aria-describedby', tip.id);
    handle.appendChild(tip);
    titlebar.appendChild(handle);
    handle.addEventListener('pointerdown', function (event) {
      if (event.button !== 0 || activeDrawer !== drawer || isModal()) return;
      finishDrag();
      drag = { drawer: drawer, handle: handle, pointer: event.pointerId,
        startX: event.clientX, right: (horizontalPositions[drawer.id] || 0) * horizontalRoom(drawer) };
      handle.setPointerCapture(event.pointerId);
      drawer.classList.add('is-dragging');
      handle.focus({ preventScroll: true });
      event.preventDefault();
    });
    handle.addEventListener('pointermove', function (event) {
      if (!drag || drag.pointer !== event.pointerId) return;
      moveDrawer(drawer, drag.right - (event.clientX - drag.startX));
    });
    handle.addEventListener('pointerup', finishDrag);
    handle.addEventListener('pointercancel', finishDrag);
    handle.addEventListener('lostpointercapture', finishDrag);
    handle.addEventListener('keydown', function (event) {
      if (isModal() || activeDrawer !== drawer || !/^(ArrowLeft|ArrowRight|Home|End)$/.test(event.key)) return;
      var room = horizontalRoom(drawer);
      var right = (horizontalPositions[drawer.id] || 0) * room;
      moveDrawer(drawer, event.key === 'Home' ? room : event.key === 'End' ? 0 :
        right + (event.key === 'ArrowLeft' ? 24 : -24));
      event.preventDefault();
    });
  }

  function isModal() {
    return window.matchMedia('(max-width: 900px)').matches;
  }

  function setBackgroundInert(drawer, inert) {
    inertBackground.forEach(function (item) {
      if (item.element.isConnected) item.element.inert = item.wasInert;
    });
    inertBackground = [];
    if (!inert) return;
    Array.prototype.forEach.call(document.body.children, function (element) {
      if (element === drawer) return;
      inertBackground.push({ element: element, wasInert: element.inert });
      element.inert = true;
    });
    drawer.inert = false;
  }

  function setModalMode(drawer) {
    var modal = isModal();
    drawer.setAttribute('aria-modal', modal ? 'true' : 'false');
    setBackgroundInert(drawer, modal);
  }

  function drawerHomePageIsActive(drawer) {
    var home = drawer && drawer._cerebroHomeParent;
    var page = home && home.closest('.tab-pane');
    return !!(page && page.classList.contains('active'));
  }

  function restoreDrawer(drawer) {
    var parent = drawer._cerebroHomeParent;
    var next = drawer._cerebroHomeNext;
    if (!parent || !parent.isConnected) return false;
    parent.insertBefore(drawer, next && next.parentNode === parent ? next : null);
    return true;
  }

  function closeDrawer(restoreFocus, immediate) {
    if (!activeDrawer) return;
    finishDrag();
    var drawer = activeDrawer;
    var button = activeButton;
    drawer.classList.remove('is-open');
    drawer.setAttribute('aria-hidden', 'true');
    if (button) button.setAttribute('aria-expanded', 'false');
    setBackgroundInert(drawer, false);
    window.clearTimeout(drawer._cerebroUnmountTimer);
    if (!restoreDrawer(drawer)) {
      if (window.Shiny && window.Shiny.unbindAll) {
        window.Shiny.unbindAll(drawer);
      }
      drawer.remove();
    }
    var unmount = function () {
      if (!drawer.classList.contains('is-open')) {
        drawer.classList.remove('is-mounted');
      }
    };
    if (immediate) unmount();
    else drawer._cerebroUnmountTimer = window.setTimeout(unmount, 140);
    activeDrawer = null;
    activeButton = null;
    if (restoreFocus !== false && button && button.isConnected) {
      button.focus();
    }
  }

  function openDrawer(drawer, button) {
    document.dispatchEvent(new CustomEvent('cerebro:overlay-opening', {
      detail: { owner: 'settings:' + drawer.id }
    }));
    closeDrawer(false);
    window.clearTimeout(drawer._cerebroUnmountTimer);
    if (drawer.parentNode !== document.body) {
      drawer._cerebroHomeParent = drawer.parentNode;
      drawer._cerebroHomeNext = drawer.nextSibling;
      document.body.appendChild(drawer);
    }
    setModalMode(drawer);
    ensureDragHandle(drawer);
    drawer.classList.add('is-mounted');
    positionDrawer(drawer);
    void drawer.offsetWidth;
    drawer.classList.add('is-open');
    drawer.setAttribute('aria-hidden', 'false');
    button.setAttribute('aria-expanded', 'true');
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue(drawer.id + '_render_request', Date.now(), {
        priority: 'event'
      });
    }
    activeDrawer = drawer;
    activeButton = button;
    window.requestAnimationFrame(function () {
      var close = drawer.querySelector('[data-cerebro-drawer-close]');
      if (close && activeDrawer === drawer) close.focus();
    });
  }

  function closeGroupFilters(except) {
    document.querySelectorAll('.cerebro-group-filters .cv-filt').forEach(function (filter) {
      if (filter === except) return;
      var menu = filter.querySelector('.cv-filt-menu');
      var button = filter.querySelector('.cv-filt-btn');
      if (menu) menu.style.display = 'none';
      if (button) {
        button.classList.remove('is-open');
        button.setAttribute('aria-expanded', 'false');
      }
    });
  }

  function updateGroupFilterCount(filter) {
    var boxes = filter.querySelectorAll('input[type="checkbox"]');
    var selected = filter.querySelectorAll('input[type="checkbox"]:checked');
    var count = filter.querySelector('.cv-filt-ct');
    if (count) count.textContent = selected.length + '/' + boxes.length;
  }

  document.addEventListener('click', function (event) {
    var filterButton = event.target.closest('.cerebro-group-filters .cv-filt-btn');
    if (filterButton) {
      var filter = filterButton.closest('.cv-filt');
      var menu = filter.querySelector('.cv-filt-menu');
      var opening = menu && menu.style.display === 'none';
      closeGroupFilters(filter);
      if (menu) menu.style.display = opening ? '' : 'none';
      filterButton.classList.toggle('is-open', opening);
      filterButton.setAttribute('aria-expanded', opening ? 'true' : 'false');
      if (opening && menu) {
        window.requestAnimationFrame(function () {
          menu.scrollIntoView({ block: 'nearest', inline: 'nearest' });
        });
      }
      return;
    }

    var filterAction = event.target.closest(
      '.cerebro-group-filters .cv-filt-acts button'
    );
    if (filterAction) {
      var actionFilter = filterAction.closest('.cv-filt');
      var checked = filterAction.getAttribute('data-act') === 'all';
      actionFilter.querySelectorAll('input[type="checkbox"]').forEach(function (box) {
        box.checked = checked;
      });
      updateGroupFilterCount(actionFilter);
      var firstBox = actionFilter.querySelector('input[type="checkbox"]');
      if (firstBox) firstBox.dispatchEvent(new Event('change', { bubbles: true }));
      return;
    }

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
    if (!event.target.closest('.cerebro-group-filters .cv-filt')) {
      closeGroupFilters();
    }
  });

  document.addEventListener('change', function (event) {
    var filter = event.target.closest('.cerebro-group-filters .cv-filt');
    if (filter && event.target.matches('input[type="checkbox"]')) {
      updateGroupFilterCount(filter);
    }
  });

  document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape' && activeDrawer) {
      event.preventDefault();
      closeDrawer();
    }
    if (event.key === 'Tab' && activeDrawer && isModal()) {
      var candidates = Array.prototype.filter.call(
        activeDrawer.querySelectorAll(
          'button:not([disabled]), a[href], input:not([disabled]), ' +
          'select:not([disabled]), textarea:not([disabled]), ' +
          '[tabindex]:not([tabindex="-1"])'
        ),
        function (element) { return element.getClientRects().length > 0; }
      );
      if (!candidates.length) return;
      var first = candidates[0];
      var last = candidates[candidates.length - 1];
      if (event.shiftKey &&
          (document.activeElement === first || !activeDrawer.contains(document.activeElement))) {
        event.preventDefault();
        last.focus();
      } else if (!event.shiftKey &&
          (document.activeElement === last || !activeDrawer.contains(document.activeElement))) {
        event.preventDefault();
        first.focus();
      }
    }
    if (event.key === 'Escape') closeGroupFilters();
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
    finishDrag();
    if (activeDrawer) {
      setModalMode(activeDrawer);
      positionDrawer(activeDrawer);
    }
  });

  window.jQuery(document).on(
    'shiny:outputinvalidated.cerebroSettings shiny:value.cerebroSettings',
    function (event) {
      if (
        activeDrawer &&
        activeDrawer._cerebroHomeParent &&
        event.target.classList &&
        event.target.classList.contains('shiny-bound-output') &&
        event.target.contains(activeDrawer._cerebroHomeParent)
      ) {
        if (
          activeDrawer.hasAttribute('data-cerebro-drawer-sticky-in-page') &&
          drawerHomePageIsActive(activeDrawer)
        ) {
          return;
        }
        closeDrawer(false, true);
      }
    }
  );

  window.jQuery(document).on('shown.bs.tab.cerebroSettings', function (event) {
    if (
      activeDrawer &&
      activeDrawer.hasAttribute('data-cerebro-drawer-sticky-in-page') &&
      drawerHomePageIsActive(activeDrawer)
    ) {
      return;
    }
    if (!activeDrawer || !activeDrawer.contains(event.target)) {
      closeDrawer(false);
    }
  });
})();
