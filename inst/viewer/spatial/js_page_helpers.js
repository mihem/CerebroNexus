// Spatial-only page chrome.
shinyjs.showScrollDownIndicator = function (message) {
  shinyjs.hideScrollDownIndicator();
  const page = document.getElementById('shiny-tab-spatial');
  if (!page || !page.classList.contains('active') ||
      (window.CerebroDatasetContext && window.CerebroDatasetContext.phase() !== 'ready')) return;

  const indicator = document.createElement('button');
  indicator.type = 'button';
  indicator.id = 'scroll-down-indicator';
  indicator.className = 'scroll-down-indicator';
  indicator.innerHTML = `
    <span class="scroll-down-arrow">
      <svg viewBox="0 0 24 24" width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true">
        <polyline points="6 9 12 15 18 9"></polyline>
      </svg>
    </span>
    <span class="scroll-down-text"></span>
  `;
  indicator.querySelector('.scroll-down-text').textContent =
    message || 'Charts generated below';
  document.body.appendChild(indicator);

  indicator.onclick = function () {
    const reducedMotion = window.matchMedia &&
      window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    const content = page.closest('.content-wrapper');
    if (content) content.scrollBy({ top: 300, behavior: reducedMotion ? 'auto' : 'smooth' });
    shinyjs.hideScrollDownIndicator();
  };

  const onScroll = function () {
    shinyjs.hideScrollDownIndicator();
  };
  // .content-wrapper scroll events do not bubble, so listen in capture phase.
  window.addEventListener('scroll', onScroll, true);

  const onClickOutside = function (event) {
    if (!indicator.contains(event.target)) {
      shinyjs.hideScrollDownIndicator();
      document.removeEventListener('click', onClickOutside);
    }
  };
  document.addEventListener('click', onClickOutside);

  indicator._onScroll = onScroll;
  indicator._onClickOutside = onClickOutside;
};

shinyjs.hideScrollDownIndicator = function () {
  const indicator = document.getElementById('scroll-down-indicator');
  if (!indicator) return;
  if (indicator._onScroll) {
    window.removeEventListener('scroll', indicator._onScroll, true);
  }
  if (indicator._onClickOutside) {
    document.removeEventListener('click', indicator._onClickOutside);
  }
  indicator.remove();
};

window.addEventListener('cerebro:dataset-context', function (event) {
  if (event.detail && event.detail.phase === 'pending') shinyjs.hideScrollDownIndicator();
});
if (window.jQuery) {
  window.jQuery(document).on('shiny:inputchanged', function (event) {
    if (event.name === 'sidebar' && event.value !== 'spatial') shinyjs.hideScrollDownIndicator();
  });
}

// Send background controls as one identity-scoped snapshot. Dynamic Shiny UI
// replaces these inputs when the dataset, section, ROI, or image changes; a
// group of independent input values cannot prove that they came from the same
// rendered control generation.
(function registerSpatialBackgroundControlPayload() {
  const sliderIds = [
    'spatial_projection_background_opacity',
    'spatial_projection_background_offset_x',
    'spatial_projection_background_offset_y',
    'spatial_projection_background_scale',
    'spatial_projection_background_scale_x',
    'spatial_projection_background_scale_y',
    'spatial_projection_background_rotate'
  ];
  const checkboxIds = [
    'spatial_projection_background_scale_lock',
    'spatial_projection_background_flip_x',
    'spatial_projection_background_flip_y'
  ];
  const watchedIds = sliderIds.concat(
    sliderIds.map(function (id) { return id + '_num'; }),
    checkboxIds
  );
  let pendingFrame = null;
  let sequence = 0;

  function inScope(scope, id) {
    return scope && scope.querySelector('#' + id);
  }

  function numberValue(scope, id) {
    const input = inScope(scope, id);
    const value = input ? Number(input.value) : NaN;
    return Number.isFinite(value) ? value : null;
  }

  function checkedValue(scope, id) {
    const input = inScope(scope, id);
    return input ? !!input.checked : null;
  }

  function scopeContext(scope) {
    if (!scope) return null;
    const generation = Number(scope.getAttribute('data-dataset-generation'));
    const context = {
      epoch: scope.getAttribute('data-dataset-epoch') || '',
      dataset_key: scope.getAttribute('data-dataset-key') || '',
      generation: generation
    };
    const api = window.CerebroDatasetContext;
    return api && api.normalize ? api.normalize(context) : null;
  }

  function scopeIdentity(scope) {
    try {
      return JSON.parse(scope.getAttribute('data-background-identity') || 'null');
    } catch (ignore) {
      return null;
    }
  }

  function sendSnapshot(scope, source) {
    pendingFrame = null;
    if (!scope || !document.documentElement.contains(scope) ||
        !window.Shiny || !Shiny.setInputValue) return;
    const datasetContext = scopeContext(scope);
    const backgroundIdentity = scopeIdentity(scope);
    const contextApi = window.CerebroDatasetContext;
    if (!datasetContext || !backgroundIdentity || !contextApi ||
        !contextApi.accepts({ dataset_context: datasetContext })) return;

    const locked = checkedValue(
      scope,
      'spatial_projection_background_scale_lock'
    );
    let scaleX = locked
      ? numberValue(scope, 'spatial_projection_background_scale')
      : numberValue(scope, 'spatial_projection_background_scale_x');
    let scaleY = locked
      ? scaleX
      : numberValue(scope, 'spatial_projection_background_scale_y');
    const values = {
      opacity: numberValue(scope, 'spatial_projection_background_opacity'),
      offsetX: numberValue(scope, 'spatial_projection_background_offset_x'),
      offsetY: numberValue(scope, 'spatial_projection_background_offset_y'),
      scaleX: scaleX,
      scaleY: scaleY,
      flipX: checkedValue(scope, 'spatial_projection_background_flip_x'),
      flipY: checkedValue(scope, 'spatial_projection_background_flip_y'),
      rotate: numberValue(scope, 'spatial_projection_background_rotate')
    };

    // Exact-number boxes update their paired Shiny slider on a round trip. Use
    // the value that produced this browser event immediately so the snapshot
    // remains atomic even before that mirror update returns.
    const numericMap = {
      spatial_projection_background_opacity_num: 'opacity',
      spatial_projection_background_offset_x_num: 'offsetX',
      spatial_projection_background_offset_y_num: 'offsetY',
      spatial_projection_background_scale_num: 'scaleX',
      spatial_projection_background_scale_x_num: 'scaleX',
      spatial_projection_background_scale_y_num: 'scaleY',
      spatial_projection_background_rotate_num: 'rotate'
    };
    const numericName = source && numericMap[source.id];
    const numericValue = source ? Number(source.value) : NaN;
    if (numericName && Number.isFinite(numericValue)) {
      values[numericName] = numericValue;
      if (source.id === 'spatial_projection_background_scale_num' && locked) {
        values.scaleY = numericValue;
      }
    }

    Shiny.setInputValue('spatial_projection_background_controls', {
      dataset_context: datasetContext,
      background_identity: backgroundIdentity,
      values: values,
      nonce: ++sequence
    }, { priority: 'event' });
  }

  function scheduleSnapshot(event) {
    const source = event && event.target;
    if (!source || watchedIds.indexOf(source.id) < 0) return;
    const scope = source.closest('#spatial_projection_background_control_scope');
    if (!scope) return;
    if (pendingFrame != null && window.cancelAnimationFrame) {
      window.cancelAnimationFrame(pendingFrame);
    }
    const schedule = window.requestAnimationFrame || function (callback) {
      return window.setTimeout(callback, 0);
    };
    pendingFrame = schedule(function () { sendSnapshot(scope, source); });
  }

  // Shiny/ionRangeSlider emit jQuery change events for drags and server-side
  // updates (including Reset). Native listeners cannot see those events.
  // jQuery also forwards native input/change, so use one path to avoid doubles.
  if (window.jQuery) {
    window.jQuery(document).on(
      'input.cerebroSpatialBackground change.cerebroSpatialBackground',
      scheduleSnapshot
    );
  } else {
    document.addEventListener('input', scheduleSnapshot, true);
    document.addEventListener('change', scheduleSnapshot, true);
  }
})();

// The button says Copy, so copy the generated preset instead of only revealing
// a code block. The textarea fallback keeps local HTTP Shiny sessions working,
// where the secure Clipboard API may be unavailable.
(function registerSpatialPresetCopy() {
  function canonicalIdentity(value) {
    if (value == null) return '';
    if (typeof value !== 'object') return JSON.stringify(value);
    if (Array.isArray(value)) {
      return '[' + value.map(canonicalIdentity).join(',') + ']';
    }
    return '{' + Object.keys(value).sort().map(function (name) {
      return JSON.stringify(name) + ':' + canonicalIdentity(value[name]);
    }).join(',') + '}';
  }

  function stillCurrent(message) {
    const contextApi = window.CerebroDatasetContext;
    if (!contextApi || !contextApi.accepts || !contextApi.accepts(message)) {
      return false;
    }
    const scope = document.getElementById(
      'spatial_projection_background_control_scope'
    );
    let currentIdentity = null;
    try {
      currentIdentity = JSON.parse(
        scope && scope.getAttribute('data-background-identity') || 'null'
      );
    } catch (ignore) {
      return false;
    }
    const incomingIdentity = canonicalIdentity(message.background_identity);
    if (!incomingIdentity || incomingIdentity !== canonicalIdentity(currentIdentity)) {
      return false;
    }
    const summary = window.cerebroCellViews &&
      window.cerebroCellViews.summary &&
      window.cerebroCellViews.summary('spatial_projection');
    return !summary || !summary.ready ||
      summary.backgroundIdentity === incomingIdentity ||
      (summary.backgroundIdentities || []).indexOf(incomingIdentity) >= 0;
  }

  function copyText(text) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(text);
    }
    const area = document.createElement('textarea');
    area.value = text;
    area.style.position = 'fixed';
    area.style.opacity = '0';
    document.body.appendChild(area);
    area.select();
    const copied = document.execCommand('copy');
    area.remove();
    return copied ? Promise.resolve() : Promise.reject(new Error('copy failed'));
  }

  function register() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return false;
    Shiny.addCustomMessageHandler('spatial_copy_preset', function (message) {
      if (!message || !stillCurrent(message)) return;
      const button = document.getElementById(
        'spatial_projection_background_copy_preset'
      );
      const previous = button ? button.innerHTML : '';
      function status(label) {
        if (!button || !document.documentElement.contains(button) ||
            !stillCurrent(message)) return;
        button.textContent = label;
        setTimeout(function () {
          if (document.documentElement.contains(button) && stillCurrent(message)) {
            button.innerHTML = previous;
          }
        }, 1200);
      }
      copyText(message && message.text ? message.text : '').then(function () {
        status('Copied');
      }).catch(function () { status('Copy failed'); });
    });
    return true;
  }

  if (!register()) {
    document.addEventListener('shiny:connected', register, { once: true });
  }
})();
