/* Presentation only: shinymanager still owns credential validation and routing. */
(function () {
  function init() {
    const root = document.getElementById('auth-auth-mod');
    const button = document.getElementById('auth-go_auth');
    if (!root || !button || !window.jQuery) return;
    const $document = window.jQuery(document);
    let pending = false;
    let previousError = null;
    let submitted = false;

    function setPending(value) {
      pending = value;
      button.disabled = value;
      button.classList.toggle('is-signing-in', value);
      button.setAttribute('aria-busy', String(value));
      button.setAttribute('aria-label', value ? 'Signing in…' : 'Sign in');
      const status = document.getElementById('cerebro-auth-status');
      if (status) status.textContent = value ? 'Signing in…' : '';
    }

    // Covers mouse, keyboard and the provider's Enter-to-submit binding. Prevent
    // duplicate input events as well as disabling the visible button.
    $document.on('shiny:inputchanged.cerebroAuth', function (event) {
      if (event.name !== 'auth-go_auth' || !event.value) return;
      if (pending) {
        event.preventDefault();
        return;
      }
      previousError = document.getElementById('auth-msg_auth');
      submitted = true;
      setPending(true);
    });
    $document.on('shiny:disconnected.cerebroAuth', function () {
      setPending(false);
      button.disabled = true;
    });
    $document.on('shiny:connected.cerebroAuth', function () {
      if (!pending) button.disabled = false;
    });

    const observer = new MutationObserver(function () {
      if (!root.isConnected) {
        // The provider removes the form before its authenticated reload. Keep
        // a neutral bridge on screen; never copy inputs, credentials or tokens.
        if (submitted && !document.getElementById('cerebro-auth-handoff')) {
          const bridge = document.createElement('div');
          bridge.id = 'cerebro-auth-handoff';
          bridge.setAttribute('role', 'status');
          bridge.style.cssText = 'position:fixed;inset:0;z-index:2147483000;display:grid;place-items:center;background:#f7f7f9;color:#51545c;font:16px Arial,sans-serif';
          const copy = document.createElement('div');
          copy.style.textAlign = 'center';
          const brand = root.querySelector('.cerebro-auth-brand');
          if (brand) {
            const logo = brand.cloneNode(true);
            logo.style.cssText = 'width:200px;margin:0 auto 24px';
            copy.appendChild(logo);
          }
          const message = document.createElement('div');
          message.textContent = 'Opening workspace…';
          copy.appendChild(message);
          bridge.appendChild(copy);
          document.body.appendChild(bridge);
        }
        observer.disconnect();
        $document.off('.cerebroAuth');
        return;
      }
      const error = document.getElementById('auth-msg_auth');
      if (pending && error && error !== previousError) {
        submitted = false;
        setPending(false);
        document.getElementById('auth-user_pwd')?.focus();
      }
    });
    observer.observe(root.parentNode, { childList: true, subtree: true });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init, { once: true });
  } else init();
})();
