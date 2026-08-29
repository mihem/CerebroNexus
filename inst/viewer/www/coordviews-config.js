/* Portable Linked views configuration dialog and Shiny transport. */
(function () {
  'use strict';

  var pending = null;
  var sequence = 0;
  var lastFocus = null;
  var exportReady = false;
  var exportBusy = false;
  var pendingTimer = null;
  var snapshotNameRequest = null;
  var pendingShare = null;
  var pendingShareTimer = null;
  var shareAvailable = false;
  var shareTtlDays = null;
  var shareUnavailableMessage = 'Share links are unavailable. Set CEREBRONEXUS_LINKED_VIEW_SHARE_DB to a writable persistent path.';
  var shareUrlHandled = false;
  var shareUrlToken = null;
  var pendingSharedApply = null;
  var latestShare = null;
  var shinyBound = false;
  var snapshotRecords = null;
  var activeViewId = 'linked_views';
  var SNAPSHOT_KEY = 'cerebro.linked-views.snapshots.v1';
  var SNAPSHOT_LIMIT = 12;
  var SNAPSHOT_BYTES = 4 * 1024 * 1024;

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
      ? config.page.id : 'linked_views';
  }
  function nextNonce() {
    sequence += 1;
    return 'cv-config-' + Date.now().toString(36) + '-' + sequence.toString(36);
  }
  function status(message, tone) {
    var element = byId('cv-config-status');
    if (!element) return;
    element.replaceChildren();
    element.textContent = message;
    element.classList.toggle('is-error', tone === 'error');
    element.classList.toggle('is-success', tone === 'success');
    element.classList.toggle('is-working', tone === 'working');
  }
  function shareOpenStatus(message, tone) {
    var element = byId('cv-share-open-status');
    if (!element) return;
    element.textContent = message || '';
    element.hidden = !message;
    element.classList.toggle('is-error', tone === 'error');
  }
  function clearPending() {
    if (pendingTimer) window.clearTimeout(pendingTimer);
    pendingTimer = null;
    pending = null;
    setBusy(false);
  }
  function startPending(nonce, action, purpose, consumer) {
    pending = {
      nonce: nonce,
      action: action,
      purpose: purpose || action,
      consumer: consumer || null
    };
    if (pendingTimer) window.clearTimeout(pendingTimer);
    pendingTimer = window.setTimeout(function () {
      if (!pending || pending.nonce !== nonce) return;
      var failedPurpose = pending.purpose;
      clearPending();
      setUploadLoading(false);
      status(
        failedPurpose === 'apply'
          ? 'Restore did not finish. Reload this page and try again.'
          : 'Saving did not finish. Reload this page and try again.',
        'error'
      );
    }, 10000);
  }
  function snapshotName(value) {
    return typeof value === 'string' ? value.trim().replace(/\s+/g, ' ').slice(0, 80) : '';
  }
  function currentFingerprint() {
    try {
      var state = adapter();
      var summary = state && state.ready() && state.summary ? state.summary() : null;
      return summary && typeof summary.datasetFingerprint === 'string'
        ? summary.datasetFingerprint
        : window.cerebroSavedViewDataset &&
          window.cerebroSavedViewDataset.cell_fingerprint || null;
    } catch (ignore) { return null; }
  }
  function snapshotStorageKey() {
    var path = window.location.pathname.replace(/\/+$/, '') || '/';
    return SNAPSHOT_KEY + ':' + encodeURIComponent(path);
  }
  function readSnapshots() {
    if (snapshotRecords) return snapshotRecords;
    try {
      var stored = window.localStorage && window.localStorage.getItem(snapshotStorageKey());
      var parsed = stored ? JSON.parse(stored) : { records: [] };
      snapshotRecords = Array.isArray(parsed.records) ? parsed.records.filter(function (record) {
        return record && typeof record.id === 'string' && typeof record.name === 'string' &&
          typeof record.saved_at === 'string' && typeof record.json === 'string' &&
          typeof record.fingerprint === 'string' && typeof record.view_id === 'string';
      }) : [];
    } catch (ignore) { snapshotRecords = []; }
    return snapshotRecords;
  }
  function writeSnapshots(records) {
    var sorted = records.slice().sort(function (a, b) {
      return String(b.saved_at).localeCompare(String(a.saved_at));
    }).slice(0, SNAPSHOT_LIMIT);
    var document = { version: 1, records: sorted };
    var text = JSON.stringify(document);
    while (sorted.length && text.length > SNAPSHOT_BYTES) {
      sorted.pop(); document.records = sorted; text = JSON.stringify(document);
    }
    if (!sorted.length && records.length) throw new Error('This view is too large to save in this browser.');
    try { window.localStorage.setItem(snapshotStorageKey(), text); }
    catch (error) { throw new Error('This browser has no space left for saved views.'); }
    snapshotRecords = sorted;
    return sorted;
  }
  function shareUrl(token, datasetLabel) {
    var url = new URL(window.location.href);
    url.pathname = url.pathname.replace(/\/admin\/?$/, '/');
    url.searchParams.delete('linked_view');
    if (datasetLabel) url.searchParams.set('dataset', datasetLabel);
    url.hash = 'linked_view=' + encodeURIComponent(token);
    return url.toString();
  }
  function copyShareButton(record) {
    var copy = snapshotButton('Copy link', function () {
      if (copy.dataset.copying === 'true') return;
      copy.dataset.copying = 'true';
      copy.textContent = 'Copying…';
      window.cerebroClipboard.copyText(shareUrl(record.token, record.dataset_label)).then(function (ok) {
        copy.textContent = ok ? 'Copied ✓' : 'Try again';
        copy.classList.toggle('is-copy-success', ok);
        status(ok ? 'Share link copied.' : 'Clipboard access was blocked.', ok ? 'success' : 'error');
        if (document.contains(copy)) copy.focus();
        window.setTimeout(function () {
          copy.textContent = 'Copy link';
          copy.classList.remove('is-copy-success');
          delete copy.dataset.copying;
        }, 1400);
      });
    }, record);
    copy.classList.add('cv-copy-link');
    copy.setAttribute('aria-live', 'polite');
    return copy;
  }
  function renderShareResult(record) {
    var list = byId('cv-share-list');
    var create = byId('cv-share-create');
    var shareRegion = byId('cv-config-share');
    if (shareRegion) {
      shareRegion.hidden = false;
      shareRegion.classList.toggle(
        'is-disabled',
        !shareAvailable || exportBusy || !exportReady
      );
    }
    if (create) create.disabled = exportBusy ||
      !!pendingShare || !shareAvailable || !exportReady;
    if (!list) return;
    list.replaceChildren();
    if (!record || !record.token) return;
    var row = document.createElement('div'); row.className = 'cv-share-row';
    var text = document.createElement('span');
    text.textContent = 'Expires ' + snapshotDate(record.expires_at);
    row.appendChild(text);
    row.appendChild(copyShareButton(record));
    list.appendChild(row);
  }
  function visibleSnapshots() {
    var fingerprint = currentFingerprint();
    if (!fingerprint) return [];
    return readSnapshots().filter(function (record) {
      return record.fingerprint === fingerprint && record.view_id === activeViewId;
    });
  }
  function snapshotDate(value) {
    var date = new Date(value);
    return isNaN(date.getTime()) ? '' : date.toLocaleString();
  }
  function snapshotButton(label, action, record) {
    var button = document.createElement('button');
    button.type = 'button'; button.className = 'cv-snapshot-action';
    button.textContent = label;
    button.addEventListener('click', function () { action(record); });
    return button;
  }
  function renderSnapshots() {
    var list = byId('cv-snapshot-list');
    var save = byId('cv-snapshot-save');
    var saveRegion = document.querySelector('.cv-config-save-local');
    if (saveRegion) saveRegion.classList.toggle('is-disabled', exportBusy || !exportReady);
    if (save) {
      save.disabled = exportBusy || !exportReady;
      save.title = exportReady ? '' : 'Select at least one cell before saving this view';
    }
    if (!list) return;
    list.replaceChildren();
    var records = visibleSnapshots();
    if (!records.length) {
      var empty = document.createElement('p');
      empty.className = 'cv-snapshot-empty';
      empty.textContent = 'No saved views for this cell population yet.';
      list.appendChild(empty);
      return;
    }
    records.forEach(function (record) {
      var row = document.createElement('div'); row.className = 'cv-snapshot-row';
      var mark = document.createElement('span'); mark.className = 'cv-snapshot-mark';
      mark.setAttribute('aria-hidden', 'true'); mark.textContent = '⌑'; row.appendChild(mark);
      var details = document.createElement('div'); details.className = 'cv-snapshot-details';
      var name = document.createElement('strong'); name.textContent = record.name;
      var time = document.createElement('span'); time.textContent = snapshotDate(record.saved_at);
      details.appendChild(name); details.appendChild(time); row.appendChild(details);
      var primary = document.createElement('div'); primary.className = 'cv-snapshot-primary';
      primary.appendChild(snapshotButton('Open', restoreSnapshot, record)); row.appendChild(primary);
      var actions = document.createElement('div'); actions.className = 'cv-snapshot-actions';
      actions.appendChild(snapshotButton('Download', downloadSnapshot, record));
      actions.appendChild(snapshotButton('Rename', renameSnapshot, record));
      actions.appendChild(snapshotButton('Delete', deleteSnapshot, record));
      row.appendChild(actions); list.appendChild(row);
    });
  }
  function refreshExportControls() {
    ['cv-config-download', 'cv-config-copy'].forEach(function (id) {
      var button = byId(id);
      if (!button) return;
      button.disabled = exportBusy || !exportReady;
      button.title = exportReady
        ? ''
        : 'Select at least one cell before exporting this view';
    });
  }
  function setBusy(busy) {
    exportBusy = !!busy;
    refreshExportControls();
    renderSnapshots();
    renderShareResult(latestShare);
  }
  function setUploadLoading(loading) {
    var upload = byId('coordviews_config_upload');
    var host = upload && upload.closest('.cv-config-upload');
    if (host) host.classList.toggle('is-uploading', !!loading);
  }
  function setReadyFor(viewId, ready, selectedCells) {
    var buttons = document.querySelectorAll('[data-view-id="' + viewId + '"]');
    var selected = Number(selectedCells) > 0;
    var enabled = !!ready && selected;
    Array.prototype.forEach.call(buttons, function (button) {
      button.disabled = !enabled;
      button.setAttribute('aria-disabled', enabled ? 'false' : 'true');
      button.title = !ready
        ? 'This view is waiting for its plot'
        : !selected
          ? 'Select at least one cell before sharing this view'
          : 'Save, open, import, export, or share this view';
    });
    if (viewId !== activeViewId) return;
    exportReady = enabled;
    if (latestShare && latestShare.fingerprint !== currentFingerprint()) latestShare = null;
    refreshExportControls();
    renderSnapshots();
    renderShareResult(latestShare);
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
    var state = adapter();
    var linked = activeViewId === 'linked_views';
    var label = state && state.label ? state.label : 'Linked workspace';
    var kicker = document.querySelector('#cv-config-dialog .cv-config-kicker');
    if (kicker) kicker.textContent = linked ? 'Portable linked workspace' : 'Portable saved view';
    if (byId('cv-config-title')) {
      byId('cv-config-title').textContent = linked ? 'Linked workspace' : label + ' view';
    }
    if (byId('cv-config-privacy')) {
      byId('cv-config-privacy').textContent = linked
        ? 'Portable JSON includes selected cell barcodes and view settings. Source data stays on this device.'
        : 'Portable JSON includes the active cohort and page settings. Source data stays on this device.';
    }
  }
  function sendShareRequest(action, record, config) {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue || pendingShare) return;
    var nonce = nextNonce();
    pendingShare = { nonce: nonce, action: action };
    if (pendingShareTimer) window.clearTimeout(pendingShareTimer);
    if (action === 'share_open') shareOpenStatus('Opening shared view…', 'working');
    else status('Creating share link…', 'working');
    var payload = { nonce: nonce, action: action };
    if (action === 'share_create') payload.config = config;
    if (record) payload.token = record.token;
    pendingShareTimer = window.setTimeout(function () {
      if (!pendingShare || pendingShare.nonce !== nonce) return;
      pendingShare = null; pendingShareTimer = null;
      renderShareResult(latestShare);
      if (action === 'share_open') {
        shareOpenStatus('The shared view could not be opened. Try again.', 'error');
      } else status('The share link could not be saved. Try again.', 'error');
    }, 10000);
    Shiny.setInputValue('coordviews_share_request', payload, { priority: 'event' });
  }
  function sendShare(action, record) {
    var state = adapter();
    if (action !== 'share_create') {
      sendShareRequest(action, record, null);
      return;
    }
    if (!shareAvailable) {
      status(shareUnavailableMessage, 'error');
      return;
    }
    if (!state || !state.ready() || !exportReady) {
      status('Select at least one cell before creating a share link.', 'error'); return;
    }
    if (pendingShare) return;
    try {
      sendShareRequest(action, null, state.capture());
      renderShareResult(latestShare);
    } catch (error) {
      status(error && error.message ? error.message : 'The view could not be prepared.', 'error');
    }
  }
  function receiveShare(result) {
    if (!result || !pendingShare || String(result.nonce) !== pendingShare.nonce) return;
    var completedShare = pendingShare;
    var action = completedShare.action; pendingShare = null;
    if (pendingShareTimer) window.clearTimeout(pendingShareTimer);
    pendingShareTimer = null;
    renderShareResult(latestShare);
    if (result.action !== action) {
      if (action === 'share_open') {
        shareOpenStatus('The page received an unexpected share response.', 'error');
      } else status('The page received an unexpected share response.', 'error');
      return;
    }
    if (!result.ok) {
      if (action === 'share_open') {
        shareOpenStatus(result.message || 'The shared view could not be opened.', 'error');
      } else status(result.message || 'The share request failed.', 'error');
      return;
    }
    if (action === 'share_create') {
      latestShare = {
        token: result.token,
        expires_at: result.expires_at,
        dataset_label: result.dataset_label || '',
        fingerprint: currentFingerprint(),
        view_id: activeViewId
      };
      renderShareResult(latestShare);
      status('Share link ready.', 'success');
      window.dispatchEvent(new CustomEvent('cerebro:share-created', {
        detail: { token: result.token }
      }));
    } else if (action === 'share_open') {
      finishApply(result, true);
    }
  }
  function openShareFromUrl() {
    if (shareUrlHandled) return;
    if (!shareUrlToken) {
      var url = new URL(window.location.href);
      var fragment = new URLSearchParams(url.hash.replace(/^#/, ''));
      shareUrlToken = fragment.get('linked_view');
      if (shareUrlToken) {
        fragment.delete('linked_view');
        url.hash = fragment.toString();
        window.history.replaceState({}, '', url.toString());
      }
    }
    if (!shareUrlToken || !window.cerebroSavedViewDataset ||
        typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    shareUrlHandled = true;
    sendShare('share_open', { token: shareUrlToken });
    shareUrlToken = null;
  }
  function openDialog() {
    var dialog = byId('cv-config-dialog');
    if (!dialog || dialog.open) return;
    status('');
    updateDialogCopy();
    refreshActiveState();
    setUploadLoading(false);
    renderSnapshots();
    renderShareResult(latestShare);
    lastFocus = document.activeElement;
    dialog.showModal();
    var close = byId('cv-config-close');
    if (close) close.focus();
  }
  function closeDialog() {
    var dialog = byId('cv-config-dialog');
    if (dialog && dialog.open) dialog.close();
    status('');
    setUploadLoading(false);
  }
  function preparedStatus(action) {
    if (action === 'copy') return 'Preparing JSON to copy…';
    if (action === 'save') return 'Saving this view…';
    return 'Preparing your download…';
  }
  function withPreparedConfig(action, consumer) {
    var state = adapter();
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
    startPending(nonce, 'prepare', action, consumer);
    setBusy(true);
    status(preparedStatus(action), 'working');
    Shiny.setInputValue('coordviews_config_request', {
      nonce: nonce,
      action: 'prepare',
      config: config
    }, { priority: 'event' });
  }
  function request(action) {
    var state = adapter();
    if (!state || !state.ready()) {
      status('This view is not ready to save.', 'error');
      return;
    }
    if (!exportReady) {
      status('Select at least one cell before exporting this view.', 'error');
      return;
    }
    withPreparedConfig(action, function (prepared) {
      if (action === 'copy') finishCopy(prepared);
      else finishDownload(prepared);
    });
  }
  function finishCopy(result) {
    if (typeof result.json !== 'string') {
      status('The JSON was validated, but could not be copied. Use Download JSON.', 'error');
      return;
    }
    window.cerebroClipboard.copyText(result.json).then(function (copied) {
      var button = byId('cv-config-copy');
      var label = button && button.querySelector('span');
      if (button && label) {
        label.textContent = copied ? 'Copy it' : 'Try again';
        button.classList.toggle('is-copy-success', copied);
        window.setTimeout(function () {
          if (!document.contains(button)) return;
          label.textContent = 'Copy JSON';
          button.classList.remove('is-copy-success');
        }, 1600);
      }
      status(
        copied
          ? 'Copied the configuration for ' + result.selected_cells + ' selected cells.'
          : 'Clipboard access was blocked. Use Download JSON instead.',
        copied ? 'success' : 'error'
      );
    });
  }
  function finishDownload(result) {
    if (typeof result.json !== 'string') {
      status('The JSON was validated, but could not be downloaded. Try again.', 'error');
      return;
    }
    var link = null;
    var url = null;
    try {
      if (typeof window.Blob !== 'function' || !window.URL ||
        typeof window.URL.createObjectURL !== 'function') {
        throw new Error('Object URL downloads are unavailable');
      }
      var blob = new window.Blob(
        [result.json],
        { type: 'application/json;charset=utf-8' }
      );
      url = window.URL.createObjectURL(blob);
      link = document.createElement('a');
      link.href = url;
      link.download = typeof result.filename === 'string' && result.filename
        ? result.filename
        : 'linked-views.json';
      link.style.display = 'none';
      document.body.appendChild(link);
      link.click();
      status(
        'Downloaded the configuration for ' + result.selected_cells + ' selected cells.',
        'success'
      );
    } catch (ignore) {
      status('The download could not start. Try Download JSON again.', 'error');
    } finally {
      if (link && link.parentNode) link.parentNode.removeChild(link);
      if (url) {
        window.setTimeout(function () {
          try {
            if (window.URL && typeof window.URL.revokeObjectURL === 'function') {
              window.URL.revokeObjectURL(url);
            }
          } catch (ignore) { /* the document may already be closing */ }
        }, 1000);
      }
    }
  }
  function finishApply(result, shared) {
    try {
      var target = adapterForConfig(result.config);
      if (shared && viewIdForConfig(result.config) === 'linked_views' &&
          (!target || !target.ready())) {
        pendingSharedApply = result;
        shareOpenStatus('Opening shared view…', 'working');
        return false;
      }
      if (!target) throw new Error('This configuration uses a view that is unavailable here.');
      pendingSharedApply = null;
      activeViewId = viewIdForConfig(result.config);
      latestShare = null;
      var summary = target.apply(result.config, result.colour_data || null);
      var message = 'Restored ' + summary.selectedCells + ' selected cells and view settings.';
      if (shared) shareOpenStatus('', 'success');
      else status(message, 'success');
      return true;
    } catch (error) {
      var message = error && error.message
        ? error.message
        : 'This configuration uses a view that is unavailable here.';
      if (shared) shareOpenStatus(message, 'error');
      else status(message, 'error');
      return false;
    }
  }
  function saveSnapshotLocally(name, prepared) {
    if (!prepared || typeof prepared.json !== 'string') {
      throw new Error('This view is not ready to save.');
    }
    var records = readSnapshots().filter(function (record) {
      return record.view_id !== activeViewId || record.name !== name;
    });
    records.unshift({ id: nextNonce(), name: name, saved_at: new Date().toISOString(),
      fingerprint: currentFingerprint(), view_id: activeViewId, json: prepared.json });
    writeSnapshots(records);
    renderSnapshots();
    status('Saved “' + name + '” on this device.', 'success');
  }
  function openSnapshotNameDialog(mode, record, trigger) {
    var dialog = byId('cv-snapshot-name-dialog'), input = byId('cv-snapshot-name-input');
    if (!dialog || !input) return;
    snapshotNameRequest = { mode: mode, record: record || null, trigger: trigger || document.activeElement };
    byId('cv-snapshot-name-title').textContent = mode === 'rename' ? 'Rename saved view' : 'Save current view';
    byId('cv-snapshot-name-help').textContent = mode === 'rename'
      ? 'Choose a short name that makes this saved view easy to find.'
      : 'Give this view a short name so you can find it later.';
    byId('cv-snapshot-name-confirm').textContent = mode === 'rename' ? 'Rename view' : 'Save view';
    input.value = mode === 'rename' ? record.name : '';
    dialog.showModal(); input.focus(); input.select();
  }
  function closeSnapshotNameDialog() {
    var dialog = byId('cv-snapshot-name-dialog'); if (dialog && dialog.open) dialog.close();
  }
  function confirmSnapshotName() {
    var requestState = snapshotNameRequest, input = byId('cv-snapshot-name-input');
    var name = snapshotName(input && input.value); if (!requestState || !name) return;
    closeSnapshotNameDialog();
    if (requestState.mode === 'save') {
      withPreparedConfig('save', function (prepared) {
        try { saveSnapshotLocally(name, prepared); }
        catch (error) { status(error && error.message ? error.message : 'This view could not be saved.', 'error'); }
      });
      return;
    }
    if (name === requestState.record.name) return;
    try {
      writeSnapshots(readSnapshots().map(function (item) {
        return item.id === requestState.record.id
          ? Object.assign({}, item, { name: name })
          : item;
      }));
      renderSnapshots();
    } catch (error) { status(error.message, 'error'); }
  }
  function saveSnapshot() {
    openSnapshotNameDialog('save', null, document.activeElement);
  }
  function restoreSnapshot(record) {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) {
      status('The connection is not ready. Try again in a moment.', 'error');
      return;
    }
    var nonce = nextNonce();
    if (pending) return;
    startPending(nonce, 'apply', 'apply');
    setBusy(true); status('Restoring “' + record.name + '”…', 'working');
    Shiny.setInputValue('coordviews_config_request', {
      nonce: nonce, action: 'apply', config_json: record.json
    }, { priority: 'event' });
  }
  function downloadSnapshot(record) {
    var selected = 0;
    try { selected = (JSON.parse(record.json).selection.cells || []).length; } catch (ignore) { /* validated on restore */ }
    finishDownload({ json: record.json, selected_cells: selected,
      filename: 'linked-view-' + record.name.replace(/[^a-z0-9]+/gi, '-').replace(/^-|-$/g, '') + '.json' });
  }
  function renameSnapshot(record) {
    openSnapshotNameDialog('rename', record, document.activeElement);
  }
  function deleteSnapshot(record) {
    try {
      writeSnapshots(readSnapshots().filter(function (item) { return item.id !== record.id; }));
      renderSnapshots(); status('Deleted “' + record.name + '”.', 'success');
    } catch (error) { status(error.message, 'error'); }
  }
  function receive(result) {
    if (!result || !pending || String(result.nonce) !== pending.nonce) return;
    var request = pending;
    var action = request.action;
    clearPending();
    if (result.action !== action) {
      status('This page did not receive the expected response. Reload and try again.', 'error');
      return;
    }
    if (action === 'apply') {
      var upload = byId('coordviews_config_upload');
      if (upload) upload.value = '';
      setUploadLoading(false);
    }
    if (!result.ok) {
      status(result.message || 'The configuration could not be opened.', 'error');
      return;
    }
    if (action === 'apply') finishApply(result);
    else if (action === 'prepare' && request.consumer) request.consumer(result);
  }
  function beginUpload() {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var nonce = nextNonce();
    startPending(nonce, 'apply', 'apply');
    setBusy(true);
    setUploadLoading(true);
    status('');
    Shiny.setInputValue('coordviews_config_upload_nonce', nonce, {
      priority: 'event'
    });
  }
  function connectShiny() {
    if (shinyBound || typeof Shiny === 'undefined' ||
      !Shiny.addCustomMessageHandler || !Shiny.setInputValue) return false;
    shinyBound = true;
    Shiny.addCustomMessageHandler('coordviews_config_result', receive);
    Shiny.addCustomMessageHandler('coordviews_share_result', receiveShare);
    Shiny.addCustomMessageHandler('coordviews_share_status', function (result) {
      shareAvailable = !!(result && result.available);
      shareTtlDays = result && Number(result.ttl_days) || null;
      if (result && typeof result.message === 'string' && result.message) {
        shareUnavailableMessage = result.message;
      }
      var retention = byId('cv-share-retention');
      if (retention) {
        retention.textContent = shareAvailable
          ? 'Creates a read-only link that expires after ' + shareTtlDays +
            (shareTtlDays === 1 ? ' day.' : ' days.')
          : shareUnavailableMessage;
      }
      renderShareResult(latestShare);
    });
    Shiny.addCustomMessageHandler('cerebro_saved_view_dataset', function (identity) {
      window.cerebroSavedViewDataset = identity;
      refreshActiveState();
      renderSnapshots();
      openShareFromUrl();
    });
    openShareFromUrl();
    return true;
  }
  function boot() {
    var open = byId('cv-config-open');
    var dialog = byId('cv-config-dialog');
    if (!open || !dialog) return;

    document.body.appendChild(dialog);
    var nameDialog = byId('cv-snapshot-name-dialog');
    if (nameDialog) document.body.appendChild(nameDialog);
    document.addEventListener('click', function (event) {
      var trigger = event.target && event.target.closest
        ? event.target.closest('[data-view-id].cv-config-open, .cerebro-share-open[data-view-id]')
        : null;
      if (!trigger) return;
      activeViewId = trigger.dataset.viewId || 'linked_views';
      latestShare = null;
      openDialog();
    });
    byId('cv-config-close').addEventListener('click', closeDialog);
    byId('cv-config-copy').addEventListener('click', function () { request('copy'); });
    byId('cv-config-download').addEventListener('click', function () {
      request('download');
    });
    var save = byId('cv-snapshot-save');
    if (save) save.addEventListener('click', saveSnapshot);
    var share = byId('cv-share-create');
    if (share) share.addEventListener('click', function () { sendShare('share_create'); });
    var upload = byId('coordviews_config_upload');
    if (upload) upload.addEventListener('change', beginUpload);
    byId('cv-snapshot-name-close').addEventListener('click', closeSnapshotNameDialog);
    byId('cv-snapshot-name-cancel').addEventListener('click', closeSnapshotNameDialog);
    byId('cv-snapshot-name-confirm').addEventListener('click', confirmSnapshotName);
    byId('cv-snapshot-name-input').addEventListener('keydown', function (event) {
      if (event.key === 'Enter') { event.preventDefault(); confirmSnapshotName(); }
    });
    byId('cv-snapshot-name-dialog').addEventListener('close', function () {
      var target = snapshotNameRequest && snapshotNameRequest.trigger;
      snapshotNameRequest = null;
      if (target && document.contains(target)) target.focus();
    });
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
      openShareFromUrl();
      if (detail.ready && pendingSharedApply) {
        finishApply(pendingSharedApply, true);
      }
    });
    window.addEventListener('cerebro:linkedviews-selection', function (event) {
      var state = adapterFor('linked_views');
      var summary = state && state.summary ? state.summary() : null;
      setReadyFor('linked_views', !!(state && state.ready()), summary && summary.selectedCells);
    });
    window.addEventListener('cerebro:specialist-state', function (event) {
      var detail = event.detail || {};
      var state = adapterFor(detail.viewId);
      setReadyFor(detail.viewId, !!(state && state.ready()), detail.selectedCells);
    });
    refreshActiveState();
    renderSnapshots();

    if (window.jQuery) window.jQuery(document).one('shiny:connected', connectShiny);
    else document.addEventListener('shiny:connected', connectShiny, { once: true });
    connectShiny();
    window.setTimeout(openShareFromUrl, 0);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }
})();
