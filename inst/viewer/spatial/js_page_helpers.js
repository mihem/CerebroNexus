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

  // The strip of an enlarged section ends in an arrow at the bottom right of
  // the window, where the hint sits: the hint moves to the left of the arrow
  // it would cover, so the arrow stays usable.
  const avoidStripArrows = function () {
    indicator.style.right = '';
    const box = indicator.getBoundingClientRect();
    page.querySelectorAll('.cv-strip-arrow:not([hidden])').forEach(function (arrow) {
      const a = arrow.getBoundingClientRect();
      if (a.width && a.left < box.right && a.right > box.left &&
          a.top < box.bottom && a.bottom > box.top) {
        indicator.style.right = Math.ceil(window.innerWidth - a.left + 12) + 'px';
      }
    });
  };
  avoidStripArrows();
  // The panels are laid out again a moment after the window changes size, so
  // the hint looks again once they have settled.
  let settle = 0;
  const onResize = function () {
    avoidStripArrows();
    clearTimeout(settle);
    settle = setTimeout(avoidStripArrows, 500);
  };
  window.addEventListener('resize', onResize);
  indicator._onResize = onResize;

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
  if (indicator._onResize) {
    window.removeEventListener('resize', indicator._onResize);
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
