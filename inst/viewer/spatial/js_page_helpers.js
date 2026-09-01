// Spatial-only page chrome.
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
    <div class="scroll-down-text"></div>
  `;
  indicator.querySelector('.scroll-down-text').textContent =
    message || 'Charts generated below';
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

  const onClickOutside = function (event) {
    if (!indicator.contains(event.target)) {
      shinyjs.hideScrollDownIndicator();
      document.removeEventListener('click', onClickOutside);
    }
  };
  setTimeout(function () {
    document.addEventListener('click', onClickOutside);
  }, 100);

  indicator._onScroll = onScroll;
  indicator._onClickOutside = onClickOutside;
};

shinyjs.hideScrollDownIndicator = function () {
  const indicator = document.getElementById('scroll-down-indicator');
  if (!indicator) return;
  if (indicator._onScroll) {
    window.removeEventListener('scroll', indicator._onScroll);
  }
  if (indicator._onClickOutside) {
    document.removeEventListener('click', indicator._onClickOutside);
  }
  indicator.classList.add('hiding');
  setTimeout(function () {
    if (indicator.parentElement) indicator.remove();
  }, 400);
};

// The button says Copy, so copy the generated preset instead of only revealing
// a code block. The textarea fallback keeps local HTTP Shiny sessions working,
// where the secure Clipboard API may be unavailable.
(function registerSpatialPresetCopy() {
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
      const button = document.getElementById(
        'spatial_projection_background_copy_preset'
      );
      const previous = button ? button.innerHTML : '';
      function status(label) {
        if (!button) return;
        button.textContent = label;
        setTimeout(function () { button.innerHTML = previous; }, 1200);
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
