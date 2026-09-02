/* Share-view JSON dialog and Shiny transport. */
(function () {
  'use strict';

  var activeViewId = 'linked_views';
  var exportReady = false;
  var exportBusy = false;
  var lastFocus = null;
  var pending = null;
  var pendingTimer = null;
  var sequence = 0;
  var shinyBound = false;

  function byId(id) { return document.getElementById(id); }

  function adapterFor(viewId) {
    if (viewId === 'linked_views') return window.cerebroLinkedViewsState || null;
    var specialist = window.cerebroSpecialistViews;
    return specialist && specialist.get ? specialist.get(viewId) : null;
  }

  function adapter() { return adapterFor(activeViewId); }

  function adapterForConfig(config) {
    if (config && config.schema === 'cerebronexus-specialist-view') {
      return config.page && config.page.id ? adapterFor(config.page.id) : null;
    }
    return adapterFor('linked_views');
  }

  function viewIdForConfig(config) {
    return config && config.schema === 'cerebronexus-specialist-view' && config.page
      ? config.page.id
      : 'linked_views';
  }

  function nextNonce() {
    sequence += 1;
    return 'cv-config-' + Date.now().toString(36) + '-' + sequence.toString(36);
  }

  function status(message, tone) {
    var element = byId('cv-config-status');
    if (!element) return;
    element.textContent = message || '';
    element.classList.toggle('is-error', tone === 'error');
    element.classList.toggle('is-success', tone === 'success');
    element.classList.toggle('is-working', tone === 'working');
  }

  function setUploadLoading(loading) {
    var upload = byId('coordviews_config_upload');
    var host = upload && upload.closest('.cv-config-upload');
    if (host) host.classList.toggle('is-uploading', !!loading);
  }

  function refreshExportControls() {
    var button = byId('cv-config-download');
    if (!button) return;
    button.disabled = exportBusy || !exportReady;
    button.title = exportReady
      ? ''
      : 'Select at least one cell before sharing this view';
  }

  function setBusy(busy) {
    exportBusy = !!busy;
    refreshExportControls();
  }

  function clearPending() {
    if (pendingTimer) window.clearTimeout(pendingTimer);
    pendingTimer = null;
    pending = null;
    setBusy(false);
  }

  function startPending(nonce, action) {
    pending = { nonce: nonce, action: action };
    if (pendingTimer) window.clearTimeout(pendingTimer);
    pendingTimer = window.setTimeout(function () {
      if (!pending || pending.nonce !== nonce) return;
      var failedAction = pending.action;
      clearPending();
      setUploadLoading(false);
      status(
        failedAction === 'apply'
          ? 'Open did not finish. Reload this page and try again.'
          : 'Download did not finish. Reload this page and try again.',
        'error'
      );
    }, 10000);
  }

  function setReadyFor(viewId, ready, selectedCells) {
    var selected = Number(selectedCells) > 0;
    var buttons = document.querySelectorAll(
      '.cv-config-open[data-view-id="' + viewId + '"], ' +
      '.cerebro-config-open[data-view-id="' + viewId + '"]'
    );
    Array.prototype.forEach.call(buttons, function (button) {
      button.disabled = !ready;
      button.setAttribute('aria-disabled', ready ? 'false' : 'true');
      button.title = ready ? 'Share this view' : 'This view is waiting for its plot';
    });
    if (viewId !== activeViewId) return;
    exportReady = !!ready && selected;
    refreshExportControls();
  }

  function refreshActiveState() {
    var state = adapter();
    var summary = state && state.summary ? state.summary() : null;
    setReadyFor(
      activeViewId,
      !!(state && state.ready && state.ready()),
      summary && summary.selectedCells
    );
  }

  function updateDialogCopy() {
    var linked = activeViewId === 'linked_views';
    var title = byId('cv-config-title');
    var privacy = byId('cv-config-privacy');
    var pngHelp = byId('cv-config-png-help');
    if (title) title.textContent = 'Share view';
    if (privacy) {
      privacy.textContent = linked
        ? 'Download this display state and its selected cell barcodes as JSON, then open it in another compatible CerebroNexus session. Source data stays on this device.'
        : 'Download the active cohort and page settings as JSON, then open it in another compatible CerebroNexus session. Source data stays on this device.';
    }
    if (pngHelp) {
      pngHelp.textContent = linked
        ? 'Download all visible linked panels in their current layout.'
        : 'Download the current plot as an image.';
    }
  }

  function openDialog() {
    var dialog = byId('cv-config-dialog');
    if (!dialog || dialog.open) return;
    status('');
    updateDialogCopy();
    refreshActiveState();
    setUploadLoading(false);
    lastFocus = document.activeElement;
    dialog.showModal();
    var close = byId('cv-config-close');
    if (close) close.focus();
  }

  function closeDialog() {
    var dialog = byId('cv-config-dialog');
    if (dialog && dialog.open) dialog.close();
  }

  function requestDownload() {
    var state = adapter();
    if (!state || !state.ready || !state.ready()) {
      status('This view is not ready to download.', 'error');
      return;
    }
    if (!exportReady) {
      status('Select at least one cell before sharing this view.', 'error');
      return;
    }
    if (pending || typeof Shiny === 'undefined' || !Shiny.setInputValue) {
      status('The connection is not ready. Try again in a moment.', 'error');
      return;
    }
    var config;
    try {
      config = state.capture();
    } catch (error) {
      status(error && error.message ? error.message : 'The view could not be prepared.', 'error');
      return;
    }
    var nonce = nextNonce();
    startPending(nonce, 'prepare');
    setBusy(true);
    status('Preparing your download...', 'working');
    Shiny.setInputValue('coordviews_config_request', {
      nonce: nonce,
      action: 'prepare',
      config: config
    }, { priority: 'event' });
  }

  function download(result) {
    if (typeof result.json !== 'string') {
      status('The JSON was validated, but could not be downloaded. Try again.', 'error');
      return;
    }
    var link = null;
    var url = null;
    try {
      var blob = new window.Blob(
        [result.json],
        { type: 'application/json;charset=utf-8' }
      );
      url = window.URL.createObjectURL(blob);
      link = document.createElement('a');
      link.href = url;
      link.download = result.filename || 'linked-views.json';
      link.style.display = 'none';
      document.body.appendChild(link);
      link.click();
      status(
        'Downloaded the configuration for ' + result.selected_cells + ' selected cells.',
        'success'
      );
    } catch (ignore) {
      status('The download could not start. Try again.', 'error');
    } finally {
      if (link && link.parentNode) link.parentNode.removeChild(link);
      if (url) window.setTimeout(function () { window.URL.revokeObjectURL(url); }, 1000);
    }
  }

  function applyConfig(result) {
    try {
      var target = adapterForConfig(result.config);
      if (!target) throw new Error('This configuration uses a view that is unavailable here.');
      activeViewId = viewIdForConfig(result.config);
      var summary = target.apply(result.config, result.colour_data || null);
      status(
        'Restored ' + summary.selectedCells + ' selected cells and view settings.',
        'success'
      );
    } catch (error) {
      status(
        error && error.message
          ? error.message
          : 'This configuration uses a view that is unavailable here.',
        'error'
      );
    }
  }

  function receive(result) {
    if (!result || !pending || String(result.nonce) !== pending.nonce) return;
    var expectedAction = pending.action;
    clearPending();
    if (result.action !== expectedAction) {
      status('This page received an unexpected response. Reload and try again.', 'error');
      return;
    }
    if (expectedAction === 'apply') {
      var upload = byId('coordviews_config_upload');
      if (upload) upload.value = '';
      setUploadLoading(false);
    }
    if (!result.ok) {
      status(result.message || 'The configuration could not be processed.', 'error');
      return;
    }
    if (expectedAction === 'apply') applyConfig(result);
    else download(result);
  }

  function beginUpload() {
    if (pending || typeof Shiny === 'undefined' || !Shiny.setInputValue) {
      status('The connection is not ready. Try again in a moment.', 'error');
      return;
    }
    var nonce = nextNonce();
    startPending(nonce, 'apply');
    setBusy(true);
    setUploadLoading(true);
    status('Opening view JSON...', 'working');
    Shiny.setInputValue('coordviews_config_upload_nonce', nonce, {
      priority: 'event'
    });
  }

  function pngFeedback(button, ok) {
    var label = button && button.querySelector('span');
    if (!label) return;
    var original = label.textContent;
    label.textContent = ok ? 'Downloaded' : 'Could not download';
    window.setTimeout(function () {
      if (document.contains(button)) label.textContent = original;
    }, 1600);
  }

  function downloadPNG(viewId, button) {
    var target = adapterFor(viewId);
    var ok = false;
    try {
      ok = !!(target && typeof target.downloadPNG === 'function' && target.downloadPNG());
    } catch (ignore) {
      ok = false;
    }
    pngFeedback(button, ok);
    if (!ok) {
      status('This plot is not ready to download.', 'error');
    }
  }

  function connectShiny() {
    if (shinyBound || typeof Shiny === 'undefined' ||
        !Shiny.addCustomMessageHandler || !Shiny.setInputValue) return false;
    shinyBound = true;
    Shiny.addCustomMessageHandler('coordviews_config_result', receive);
    Shiny.addCustomMessageHandler('cerebro_saved_view_dataset', function (identity) {
      window.cerebroSavedViewDataset = identity;
      refreshActiveState();
    });
    return true;
  }

  function boot() {
    var dialog = byId('cv-config-dialog');
    var open = byId('cv-config-open');
    if (!dialog || !open) return;
    document.body.appendChild(dialog);

    document.addEventListener('click', function (event) {
      var trigger = event.target && event.target.closest
        ? event.target.closest('[data-view-id].cv-config-open, [data-view-id].cerebro-config-open')
        : null;
      if (!trigger) return;
      activeViewId = trigger.dataset.viewId || 'linked_views';
      openDialog();
    });
    byId('cv-config-close').addEventListener('click', closeDialog);
    byId('cv-config-png').addEventListener('click', function () {
      downloadPNG(activeViewId, this);
    });
    byId('cv-config-download').addEventListener('click', requestDownload);
    var upload = byId('coordviews_config_upload');
    if (upload) upload.addEventListener('change', beginUpload);
    dialog.addEventListener('close', function () {
      status('');
      setUploadLoading(false);
      var target = lastFocus && document.contains(lastFocus) ? lastFocus : open;
      lastFocus = null;
      if (target) target.focus();
    });
    document.addEventListener('keydown', function (event) {
      if (event.key !== 'Escape' || !dialog.open) return;
      event.preventDefault();
      closeDialog();
    });
    window.addEventListener('cerebro:linkedviews-ready', function (event) {
      var detail = event.detail || {};
      setReadyFor('linked_views', !!detail.ready, detail.selectedCells);
    });
    window.addEventListener('cerebro:linkedviews-selection', function () {
      var state = adapterFor('linked_views');
      var summary = state && state.summary ? state.summary() : null;
      setReadyFor(
        'linked_views',
        !!(state && state.ready && state.ready()),
        summary && summary.selectedCells
      );
    });
    window.addEventListener('cerebro:specialist-state', function (event) {
      var detail = event.detail || {};
      var state = adapterFor(detail.viewId);
      setReadyFor(detail.viewId, !!(state && state.ready()), detail.selectedCells);
    });
    refreshActiveState();

    if (window.jQuery) window.jQuery(document).one('shiny:connected', connectShiny);
    else document.addEventListener('shiny:connected', connectShiny, { once: true });
    connectShiny();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }
})();
