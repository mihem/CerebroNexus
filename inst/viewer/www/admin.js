(function () {
  'use strict';

  var records = [];
  var pending = null;
  var shinyBound = false;
  var searchQuery = '';
  function byId(id) { return document.getElementById(id); }
  function nonce() {
    return Date.now().toString(36) + '-' + Math.random().toString(36).slice(2);
  }
  function status(message, kind) {
    var node = byId('viewer-admin-status');
    if (!node) return;
    node.textContent = message || '';
    node.className = 'viewer-admin-status' + (kind ? ' is-' + kind : '');
  }
  function loginStatus(message, kind) {
    var node = byId('viewer-admin-login-status');
    if (!node) return;
    node.textContent = message || '';
    node.className = 'viewer-admin-login-status' + (kind ? ' is-' + kind : '');
  }
  function setAccess(result) {
    var allowed = !!(result && result.allowed);
    var login = byId('viewer-admin-login');
    var content = byId('viewer-admin-content');
    if (login) login.style.display = allowed ? 'none' : '';
    if (content) content.style.display = allowed ? '' : 'none';
    if (!allowed) {
      records = [];
      pending = null;
      render();
    }
    if (!allowed && result && result.locked) {
      loginStatus('Too many failed attempts. Try again later.', 'error');
    } else if (!allowed && result && result.invalid) {
      loginStatus('Incorrect username or password.', 'error');
    } else if (allowed) {
      loginStatus('');
      send('list');
    }
  }
  function appBasePath() {
    var path = window.location.pathname.replace(/\/admin\/?$/, '/');
    return path || '/';
  }
  function shareUrl(token, datasetLabel) {
    var url = new URL(appBasePath(), window.location.origin);
    url.searchParams.delete('linked_view');
    if (datasetLabel) url.searchParams.set('dataset', datasetLabel);
    url.hash = 'linked_view=' + encodeURIComponent(token);
    return url.toString();
  }
  function formatDate(value) {
    var date = new Date(value);
    return isNaN(date.getTime()) ? value : date.toLocaleString();
  }
  function button(label, action, token, datasetLabel) {
    var item = document.createElement('button');
    item.type = 'button'; item.className = 'viewer-admin-action';
    item.dataset.action = action; item.dataset.token = token; item.textContent = label;
    if (datasetLabel) item.dataset.datasetLabel = datasetLabel;
    return item;
  }
  function visibleRecords() {
    if (!searchQuery) return records;
    return records.filter(function (record) {
      return [
        record.dataset_label,
        record.fingerprint,
        record.creator,
        record.status
      ].join(' ').toLowerCase().includes(searchQuery);
    });
  }
  function render() {
    var host = byId('viewer-admin-share-list');
    if (!host) return;
    host.replaceChildren();
    var shown = visibleRecords();
    if (!shown.length) {
      var empty = document.createElement('p');
      empty.className = 'viewer-admin-empty';
      empty.textContent = records.length
        ? 'No share links match this search.'
        : 'No current or recent share links.';
      host.appendChild(empty); return;
    }
    var table = document.createElement('table');
    table.className = 'viewer-admin-table';
    var head = document.createElement('thead');
    head.innerHTML = '<tr><th>Cell population</th><th>Status</th><th>Created</th><th>Expires</th><th>Created by</th><th><span class="sr-only">Actions</span></th></tr>';
    table.appendChild(head);
    var body = document.createElement('tbody');
    shown.forEach(function (record) {
      var row = document.createElement('tr');
      row.className = 'is-' + (record.status || 'active');
      var dataset = document.createElement('td');
      var name = document.createElement('strong');
      name.textContent = record.dataset_label || 'Unnamed dataset';
      var fingerprint = document.createElement('small');
      fingerprint.textContent = String(record.fingerprint).slice(0, 16) + '…';
      dataset.appendChild(name); dataset.appendChild(fingerprint); row.appendChild(dataset);
      var statusCell = document.createElement('td');
      var statusBadge = document.createElement('span');
      statusBadge.className = 'viewer-admin-badge is-' + (record.status || 'active');
      statusBadge.textContent = record.status || 'active';
      statusCell.appendChild(statusBadge); row.appendChild(statusCell);
      [formatDate(record.created_at), formatDate(record.expires_at), record.creator || 'Anonymous'].forEach(function (text) {
        var cell = document.createElement('td'); cell.textContent = text; row.appendChild(cell);
      });
      var actions = document.createElement('td'); actions.className = 'viewer-admin-actions';
      if (record.status === 'active') {
        actions.appendChild(button('Copy link', 'copy', record.token, record.dataset_label));
        actions.appendChild(button('Revoke', 'revoke', record.token));
      }
      row.appendChild(actions); body.appendChild(row);
    });
    table.appendChild(body); host.appendChild(table);
  }
  function send(action, token) {
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue || pending) return;
    pending = { nonce: nonce(), action: action, token: token || '' };
    status(action === 'revoke' ? 'Revoking link…' : 'Refreshing links…', 'working');
    Shiny.setInputValue('viewer_admin_request', pending, { priority: 'event' });
  }
  function receive(result) {
    if (!result) return;
    if (pending && result.nonce && result.nonce !== pending.nonce) return;
    var requested = pending;
    pending = null;
    if (!result.ok) {
      status(result.message || 'The Admin request failed.', 'error'); return;
    }
    if (result.action === 'list') {
      var nextRecords = Array.isArray(result.records) ? result.records : [];
      var changed = JSON.stringify(nextRecords) !== JSON.stringify(records);
      records = nextRecords;
      if (changed) render();
      if (requested || changed) {
        var active = records.filter(function (record) {
          return record.status === 'active';
        }).length;
        status(active + ' active, ' + (records.length - active) + ' recent inactive.');
      }
    } else if (result.action === 'revoke') {
      records.forEach(function (record) {
        if (record.token !== result.token) return;
        record.status = 'revoked';
        record.revoked_at = result.revoked_at || '';
      });
      render(); status('Share link revoked.', 'success');
    }
  }
  function connectShiny() {
    if (shinyBound || typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) return;
    shinyBound = true;
    Shiny.addCustomMessageHandler('viewer_admin_result', receive);
    Shiny.addCustomMessageHandler('viewer_admin_access', setAccess);
  }
  function boot() {
    var signIn = byId('viewer-admin-sign-in');
    var submitLogin = function () {
      if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
      var user = byId('viewer-admin-user');
      var password = byId('viewer-admin-password');
      loginStatus('Signing in…', 'working');
      Shiny.setInputValue('viewer_admin_login', {
        user: user ? user.value : '',
        password: password ? password.value : '',
        nonce: nonce()
      }, { priority: 'event' });
      if (password) password.value = '';
    };
    if (signIn) signIn.addEventListener('click', submitLogin);
    ['viewer-admin-user', 'viewer-admin-password'].forEach(function (id) {
      var field = byId(id);
      if (field) field.addEventListener('keydown', function (event) {
        if (event.key === 'Enter') { event.preventDefault(); submitLogin(); }
      });
    });
    var refresh = byId('viewer-admin-refresh');
    if (refresh) refresh.addEventListener('click', function () { send('list'); });
    var search = byId('viewer-admin-search');
    if (search) search.addEventListener('input', function () {
      searchQuery = search.value.trim().toLowerCase();
      render();
    });
    var logout = byId('viewer-admin-logout');
    if (logout) logout.addEventListener('click', function () {
      if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
      Shiny.setInputValue('viewer_admin_logout', nonce(), { priority: 'event' });
    });
    window.addEventListener('cerebro:share-created', function () {
      var content = byId('viewer-admin-content');
      if (content && content.style.display !== 'none') send('list');
    });
    var list = byId('viewer-admin-share-list');
    if (list) list.addEventListener('click', function (event) {
      var target = event.target.closest('[data-action]');
      if (!target) return;
      if (target.dataset.action === 'copy') {
        if (target.dataset.copying === 'true') return;
        target.dataset.copying = 'true'; target.textContent = 'Copying…';
        window.cerebroClipboard.copyText(shareUrl(target.dataset.token, target.dataset.datasetLabel)).then(function (ok) {
          target.textContent = ok ? 'Copied ✓' : 'Copy failed';
          status(ok ? 'Share link copied.' : 'Clipboard access was blocked.', ok ? 'success' : 'error');
          if (document.contains(target)) target.focus();
          window.setTimeout(function () {
            target.textContent = 'Copy link'; delete target.dataset.copying;
          }, 1400);
        });
      } else if (target.dataset.action === 'revoke') {
        if (window.confirm('Revoke this share link? It will stop working immediately.')) {
          send('revoke', target.dataset.token);
        }
      }
    });
    if (window.jQuery) window.jQuery(document).one('shiny:connected', connectShiny);
    else document.addEventListener('shiny:connected', connectShiny, { once: true });
    connectShiny();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', boot);
  else boot();
})();
