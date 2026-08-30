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
