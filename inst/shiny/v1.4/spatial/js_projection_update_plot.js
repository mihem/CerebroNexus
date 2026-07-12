// =============================================================================
// Spatial projection: thin wrappers over the shared projection-scatter renderer
// (inst/shiny/www/projection_scatter.js) plus the spatial-only UI helpers
// (scroll-down indicator, Additional-parameters collapse/scroll behaviour).
//
// The rendering logic (custom legend, persistent x|y selection, group labels,
// group hulls, container sizing, modebar-off) now lives ONCE in the shared
// module. Every projection tab renders through it. Here we only:
//   1. keep the exact js$ function NAMES spatial's UI.R registers, so the R
//      dispatcher (func_projection_update_plot.R) is unchanged;
//   2. inject the plot id ('spatial_projection') into meta before delegating;
//   3. keep the background sync (js_spatial_background.js) and the spatial-only
//      page-chrome helpers.
// =============================================================================

const SPATIAL_PLOT_ID = 'spatial_projection';

// Register this plot so the shared Delete/Esc key handler clears its selection.
if (window.cerebroProjection) {
  window.cerebroProjection.registerPlot(SPATIAL_PLOT_ID);
}

// The R side calls js$fn(meta, data, hover, group_centers, container[, hulls]).
// shinyjs delivers those positional args as ONE array `params`; unpack it, tag
// meta with the plot id, and hand off to the shared renderer. The spatial-only
// hull payload rides in `extra`.
shinyjs.updatePlot2DContinuousSpatial = function (params) {
  const [meta, data, hover, group_centers, container] = params;
  meta.plot_id = SPATIAL_PLOT_ID;
  window.cerebroProjection.render2DContinuous(meta, data, hover, group_centers, container, {
    coexpr_colors: meta.coexpr_colors,
  });
};

shinyjs.updatePlot3DContinuousSpatial = function (params) {
  const [meta, data, hover, group_centers, container] = params;
  meta.plot_id = SPATIAL_PLOT_ID;
  window.cerebroProjection.render3DContinuous(meta, data, hover, group_centers, container, {});
};

shinyjs.updatePlot2DCategoricalSpatial = function (params) {
  const [meta, data, hover, group_centers, container, group_hulls] = params;
  meta.plot_id = SPATIAL_PLOT_ID;
  window.cerebroProjection.render2DCategorical(meta, data, hover, group_centers, container, {
    group_hulls: group_hulls,
  });
};

shinyjs.updatePlot3DCategoricalSpatial = function (params) {
  const [meta, data, hover, group_centers, container] = params;
  meta.plot_id = SPATIAL_PLOT_ID;
  window.cerebroProjection.render3DCategorical(meta, data, hover, group_centers, container, {});
};

// R calls this before assembling a render to size the plot to its container.
shinyjs.getContainerDimensions = function () {
  return window.cerebroProjection.getContainerDimensions(SPATIAL_PLOT_ID);
};

// Clear-selection button / Esc / Delete.
shinyjs.spatialClearSelection = function () {
  window.cerebroProjection.clearSelection(SPATIAL_PLOT_ID);
};

// =============================================================================
// Spatial-only page chrome (unchanged behaviour, kept out of the shared module
// because only spatial has these controls).
// =============================================================================

// Scroll down indicator functions
shinyjs.showScrollDownIndicator = function (message) {
  shinyjs.hideScrollDownIndicator();

  const indicator = document.createElement('div');
  indicator.id = 'scroll-down-indicator';
  indicator.className = 'scroll-down-indicator';
  indicator.innerHTML = `
    <div class="scroll-down-arrow">
      <svg viewBox="0 0 24 24">
        <polyline points="6 9 12 15 18 9"></polyline>
      </svg>
    </div>
    <div class="scroll-down-text">${message || 'Charts generated below'}</div>
  `;

  document.body.appendChild(indicator);

  indicator.onclick = function () {
    window.scrollBy({ top: 300, behavior: 'smooth' });
    shinyjs.hideScrollDownIndicator();
  };

  let scrollTimeout;
  const onScroll = function () {
    clearTimeout(scrollTimeout);
    scrollTimeout = setTimeout(function () {
      shinyjs.hideScrollDownIndicator();
      window.removeEventListener('scroll', onScroll);
    }, 100);
  };
  window.addEventListener('scroll', onScroll);

  const onClickOutside = function (e) {
    if (!indicator.contains(e.target)) {
      shinyjs.hideScrollDownIndicator();
      document.removeEventListener('click', onClickOutside);
    }
  };
  setTimeout(function () {
    document.addEventListener('click', onClickOutside);
  }, 100);

  indicator.dataset.cleanup = 'true';
  indicator._onScroll = onScroll;
  indicator._onClickOutside = onClickOutside;
};

shinyjs.hideScrollDownIndicator = function () {
  const indicator = document.getElementById('scroll-down-indicator');
  if (indicator) {
    if (indicator._onScroll) {
      window.removeEventListener('scroll', indicator._onScroll);
    }
    if (indicator._onClickOutside) {
      document.removeEventListener('click', indicator._onClickOutside);
    }
    indicator.classList.add('hiding');
    setTimeout(function () {
      if (indicator.parentElement) {
        indicator.remove();
      }
    }, 400);
  }
};

// When the user OPENS the Additional-parameters box, collapse the
// Main-parameters box so the tall background controls lift up the page.
(function () {
  var additionalId = 'spatial_additional_parameters_wrapper';

  function collapseMainParameters() {
    var mainWrapper = document.getElementById('spatial_main_parameters_wrapper');
    if (!mainWrapper) return;
    var box = mainWrapper.querySelector('.box');
    if (!box || box.classList.contains('collapsed-box')) return;
    var btn = box.querySelector('[data-widget="collapse"]');
    if (btn) btn.click();
  }

  var observed = null;
  var wasCollapsed = true;

  var observer = new MutationObserver(function () {
    if (!observed) return;
    var nowCollapsed = observed.classList.contains('collapsed-box');
    if (wasCollapsed && !nowCollapsed) {
      collapseMainParameters();
    }
    wasCollapsed = nowCollapsed;
  });

  setInterval(function () {
    var wrapper = document.getElementById(additionalId);
    var box = wrapper ? wrapper.querySelector('.box') : null;
    if (box && box !== observed) {
      observer.disconnect();
      observed = box;
      wasCollapsed = box.classList.contains('collapsed-box');
      observer.observe(box, {
        attributes: true,
        attributeFilter: ['class']
      });
    }
  }, 500);
})();

// 'More below' scroll hint for the Additional-parameters panel.
(function () {
  var wrapperId = 'spatial_additional_parameters_wrapper';

  function ensureHint(wrapper) {
    var hint = document.getElementById('spatial_additional_scroll_hint');
    if (!hint) {
      hint = document.createElement('div');
      hint.id = 'spatial_additional_scroll_hint';
      hint.textContent = '⌄';
      hint.title = 'Scroll for more';
      wrapper.appendChild(hint);
    }
    return hint;
  }

  function update() {
    var wrapper = document.getElementById(wrapperId);
    if (!wrapper) return;
    var body = wrapper.querySelector('.box-body');
    var box = wrapper.querySelector('.box');
    var hint = ensureHint(wrapper);
    var collapsed = box && box.classList.contains('collapsed-box');
    var scrollable = body && body.scrollHeight - body.clientHeight > 4;
    var atBottom =
      body && body.scrollTop >= body.scrollHeight - body.clientHeight - 4;
    if (!collapsed && scrollable && !atBottom) {
      hint.classList.add('is-visible');
    } else {
      hint.classList.remove('is-visible');
    }
  }

  document.addEventListener(
    'scroll',
    function (e) {
      if (
        e.target &&
        e.target.classList &&
        e.target.classList.contains('box-body') &&
        e.target.closest('#' + wrapperId)
      ) {
        update();
      }
    },
    true
  );
  window.addEventListener('resize', update);
  setInterval(update, 800);
})();
