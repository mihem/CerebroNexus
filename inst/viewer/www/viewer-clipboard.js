/* Shared, bounded clipboard helper for Viewer surfaces. */
(function (root, factory) {
  'use strict';
  var api = factory(root);
  if (typeof module === 'object' && module.exports) module.exports = api;
  root.cerebroClipboard = api;
})(typeof window === 'undefined' ? globalThis : window, function (root) {
  'use strict';

  function fallbackCopy(text) {
    var input = null;
    var copied = false;
    var previousFocus = root.document && root.document.activeElement;
    try {
      input = root.document.createElement('textarea');
      input.value = text;
      input.setAttribute('readonly', 'readonly');
      input.style.cssText = 'position:fixed;left:-9999px;top:0;opacity:0';
      root.document.body.appendChild(input);
      input.focus();
      input.select();
      copied = root.document.execCommand('copy');
    } catch (ignore) {
      copied = false;
    } finally {
      if (input && input.parentNode) input.parentNode.removeChild(input);
      if (previousFocus && typeof previousFocus.focus === 'function') {
        previousFocus.focus();
      }
    }
    return copied;
  }

  function copyText(text) {
    var clipboard = root.navigator && root.navigator.clipboard;
    if (!clipboard || typeof clipboard.writeText !== 'function') {
      return Promise.resolve(fallbackCopy(text));
    }
    return new Promise(function (resolve) {
      var settled = false;
      function finish(copied) {
        if (settled) return;
        settled = true;
        root.clearTimeout(timer);
        resolve(!!copied || fallbackCopy(text));
      }
      var timer = root.setTimeout(function () { finish(false); }, 1000);
      try {
        Promise.resolve(clipboard.writeText(text)).then(
          function () { finish(true); },
          function () { finish(false); }
        );
      } catch (ignore) {
        finish(false);
      }
    });
  }

  return { copyText: copyText };
});
