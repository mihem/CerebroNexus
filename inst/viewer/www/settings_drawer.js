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

  function restoreDrawer(drawer) {
    var parent = drawer._cerebroHomeParent;
    var next = drawer._cerebroHomeNext;
    if (!parent || !parent.isConnected) return false;
    parent.insertBefore(drawer, next && next.parentNode === parent ? next : null);
    return true;
  }

  function closeDrawer(restoreFocus, immediate) {
    if (!activeDrawer) return;
    var drawer = activeDrawer;
    var button = activeButton;
    drawer.classList.remove('is-open');
    drawer.setAttribute('aria-hidden', 'true');
    if (button) button.setAttribute('aria-expanded', 'false');
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
    else drawer._cerebroUnmountTimer = window.setTimeout(unmount, 240);
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
    if (drawer.parentNode !== document.body) {
      drawer._cerebroHomeParent = drawer.parentNode;
      drawer._cerebroHomeNext = drawer.nextSibling;
      document.body.appendChild(drawer);
    }
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
    if (activeDrawer) setModalMode(activeDrawer);
  });

  window.jQuery(document).on(
    'shiny:outputinvalidated.cerebroSettings shiny:value.cerebroSettings',
    function (event) {
      if (
        activeDrawer &&
        activeDrawer._cerebroHomeParent &&
        event.target.contains(activeDrawer._cerebroHomeParent)
      ) {
        closeDrawer(false, true);
      }
    }
  );

  window.jQuery(document).on(
    'shiny:outputinvalidated.cerebroGeneControls',
    '#expression_projection_input_type_UI',
    function () {
      this.classList.add('is-changing');
    }
  );

  window.jQuery(document).on(
    'shiny:value.cerebroGeneControls',
    '#expression_projection_input_type_UI',
    function () {
      var output = this;
      window.requestAnimationFrame(function () {
        output.classList.remove('is-changing');
      });
    }
  );

  window.jQuery(document).on('shown.bs.tab.cerebroSettings', function (event) {
    if (!activeDrawer || !activeDrawer.contains(event.target)) {
      closeDrawer(false);
    }
  });
})();
