/* A-style presentation of existing Gene keys. Never sends inputs or opens data. */
(function () {
  'use strict';
  var ids = ['expression_analysis_mode', 'expression_genes_input',
    'expression_projection_genes_in_separate_panels', 'expression_rgb_gene_r',
    'expression_rgb_gene_g', 'expression_rgb_gene_b', 'expression_select_gene_set',
    'expression_rgb_layout'];
  var mapped = null, painted = null, requested = null, installed = false;
  function array(value) {
    return value == null ? [] : (Array.isArray(value) ? value : [value]).map(String);
  }
  function controls() {
    var result = {};
    ids.forEach(function (id) {
      var el = document.getElementById(id);
      var checked = el && el.querySelector('input[type="radio"]:checked');
      result[id] = !el ? [] : array(el.selectize ? el.selectize.getValue() :
        checked ? checked.value : el.multiple ? Array.from(el.selectedOptions).map(function (o) { return o.value; }) : el.value);
    });
    return result;
  }
  function signature(values) {
    // Inactive RGB controls can be prefilled after a gene change. They must not
    // create another operation or keep the single-gene state pending forever.
    var mode = array(values[ids[2]])[0] || '';
    var analysis = array(values[ids[0]])[0] || '';
    var active = analysis === 'Gene set' ? [ids[6]] :
      mode === 'rgb' ? ids.slice(3, 6).concat(ids[7]) : [ids[1]];
    return JSON.stringify([analysis, mode].concat(active.map(function (id) {
      return array(values[id]).filter(Boolean);
    })));
  }
  function requestLabel(values) {
    var mode = array(values[ids[2]])[0];
    var names = array(values[ids[0]])[0] === 'Gene set' ? array(values[ids[6]]) :
      mode === 'rgb' ? ids.slice(3, 6).map(function (id) {
        return array(values[id])[0] || '—';
      }) : array(values[ids[1]]);
    return (mode === 'rgb' ? 'RGB: ' : '') + (names.join(', ') || 'No gene selected');
  }
  function paintedLabel(key) {
    var parts = String(key).split('\u001d');
    if (parts[0] === 'rgb') return 'RGB: ' + (parts[2] || '').split('\u001c').map(function (v) {
      return v || '—';
    }).join(', ');
    return (parts[1] || '').split('\u001c').filter(Boolean).join(', ') || 'No gene selected';
  }
  function fresh(message) {
    return window.CerebroDatasetContext && window.CerebroDatasetContext.accepts(message);
  }
  function display() {
    var el = document.getElementById('expression_feedback');
    if (!el) return;
    var values = controls(), sig = signature(values);
    var pending = requested === sig && !(mapped && fresh(mapped) &&
      signature(mapped.controls) === sig && painted === mapped.render_key);
    el.textContent = pending
      ? (painted == null ? '' : 'Last confirmed: ' + paintedLabel(painted) + ' · ') +
        'Updating: ' + requestLabel(values)
      : '';
    // The picker and plot title already identify a completed expression view.
    // A gene-free projection is a useful grey overview, not an empty state.
    el.hidden = !pending;
    el.classList.toggle('is-pending', pending);
    if (pending) el.setAttribute('aria-busy', 'true');
    else el.removeAttribute('aria-busy');
    ids.forEach(function (id) {
      var control = document.getElementById(id);
      var group = control && control.closest('.form-group');
      if (group) group.classList.toggle('cerebro-control-pending', pending &&
        id !== ids[0] && !!group.getClientRects().length);
    });
  }
  function change(event) {
    if (!event.target || ids.indexOf(event.target.id) < 0) return;
    if (!window.CerebroDatasetContext || window.CerebroDatasetContext.phase() !== 'ready') return;
    requested = signature(controls());
    display();
  }
  function install() {
    if (installed || !window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    installed = true;
    window.Shiny.addCustomMessageHandler('cerebro_gene_feedback', function (message) {
      if (!fresh(message)) return;
      mapped = message;
      display();
    });
  }
  function init() {
    if (window.jQuery) {
      window.jQuery(document).on('change.cerebroGeneFeedback', change)
        .on('shiny:connected.cerebroGeneFeedback', install)
        .on('shown.bs.tab.cerebroGeneFeedback', display);
    }
    window.addEventListener('cerebro:cell-view-ready', function (event) {
      var d = event.detail;
      if (!d || d.id !== 'expression_projection' || !d.painted ||
          d.renderKey == null || !fresh(d)) return;
      painted = d.renderKey;
      display();
    });
    window.addEventListener('cerebro:specialist-state', function (event) {
      var d = event.detail;
      var el = document.getElementById('expression_selection_feedback');
      if (!el || !d || d.viewId !== 'expression_projection' || !fresh(d)) return;
      el.hidden = !d.selectionPending;
      el.textContent = d.selectionPending ? d.selectedCells.toLocaleString() +
        ' cells selected · Loading cell identities for summaries and table…' : '';
    });
    window.addEventListener('cerebro:dataset-context', function (event) {
      var d = event.detail || {};
      if (d.changed || d.phase !== 'ready') {
        mapped = painted = requested = null;
        var el = document.getElementById('expression_selection_feedback');
        if (el) { el.hidden = true; el.textContent = ''; }
        display();
      }
    });
    install();
    display();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();
}());
