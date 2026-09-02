/* Saved-view adapters for the single-plot analysis pages. */
(function () {
  'use strict';

  var SPECS = {
    overview_projection: {
      label: 'Projection', tab: 'overview', engine: 'canvas', prefix: 'overview_'
    },
    spatial_projection: {
      label: 'Spatial', tab: 'spatial', engine: 'canvas', prefix: 'spatial_'
    },
    expression_projection: {
      label: 'Gene expression', tab: 'geneExpression', engine: 'canvas',
      prefix: 'expression_'
    },
    trajectory_projection: {
      label: 'Trajectory', tab: 'trajectory', engine: 'canvas',
      prefix: 'trajectory_'
    },
    ir_clonalUMAP_projection: {
      label: 'Immune repertoire', tab: 'immune_repertoire', engine: 'canvas',
      prefix: 'ir_'
    },
    hla_motif_network: {
      label: 'HLA & TCR Motifs', tab: 'hla_tcr_motifs', engine: 'network',
      prefix: 'hla_'
    }
  };
  var controlRestoreHandler = null;
  var controlRestoreTimer = null;

  function engine(spec) {
    if (spec.engine === 'network') return window.cerebroHlaMotifs;
    return window.cerebroCellViews;
  }

  function controlValue(element) {
    if (!window.jQuery) return undefined;
    var binding = window.jQuery(element).data('shiny-input-binding');
    if (!binding || typeof binding.getValue !== 'function') return undefined;
    try { return binding.getValue(element); }
    catch (ignore) { return undefined; }
  }

  function serialControl(element) {
    if (!element.id || element.type === 'file' || element.type === 'button' ||
        element.type === 'submit' || element.classList.contains('action-button')) return null;
    var value = controlValue(element);
    if (value == null || (typeof value === 'object' && !Array.isArray(value))) return null;
    var values = Array.isArray(value) ? value.slice() : [value];
    if (!values.length || values.length > 256) return null;
    var type = typeof values[0];
    if (type !== 'string' && type !== 'number' && type !== 'boolean') return null;
    if (values.some(function (item) {
      return typeof item !== type || (type === 'number' && !isFinite(item));
    })) return null;
    return {
      id: element.id,
      value_type: type === 'boolean' ? 'logical' : type,
      multiple: Array.isArray(value),
      values: values
    };
  }

  function captureControls(spec) {
    var root = document.getElementById('shiny-tab-' + spec.tab);
    if (!root) return [];
    return Array.from(root.querySelectorAll('.shiny-bound-input[id]'))
      .filter(function (element) { return element.id.indexOf(spec.prefix) === 0; })
      .map(serialControl)
      .filter(Boolean)
      .slice(0, 256);
  }

  function applyControl(control) {
    var element = document.getElementById(control.id);
    if (!element || !window.jQuery) return false;
    var binding = window.jQuery(element).data('shiny-input-binding');
    if (!binding || typeof binding.receiveMessage !== 'function') return false;
    var value = control.multiple ? control.values : control.values[0];
    try {
      binding.receiveMessage(element, { value: value });
      return true;
    } catch (ignore) {
      return false; // a replacement binding may apply it later
    }
  }

  function stopControlRestore() {
    if (controlRestoreHandler && window.jQuery) {
      window.jQuery(document).off(
        'shiny:bound.cerebroSpecialistRestore', controlRestoreHandler
      );
    }
    if (controlRestoreTimer) window.clearTimeout(controlRestoreTimer);
    controlRestoreHandler = null;
    controlRestoreTimer = null;
  }

  function restoreControls(controls) {
    stopControlRestore();
    if (!controls.length) return;
    if (!window.jQuery) {
      controls.forEach(applyControl);
      return;
    }

    var byId = Object.create(null);
    var pending = Object.create(null);
    controls.forEach(function (control) { byId[control.id] = control; });
    controls.forEach(function (control) { pending[control.id] = true; });
    controlRestoreHandler = function (event) {
      var control = event && event.target && byId[event.target.id];
      if (control && applyControl(control)) {
        delete pending[control.id];
        if (!controlRestoreTimer && !Object.keys(pending).length) {
          stopControlRestore();
        }
      }
    };
    window.jQuery(document).on(
      'shiny:bound.cerebroSpecialistRestore', controlRestoreHandler
    );
    controlRestoreTimer = window.setTimeout(function () {
      controlRestoreTimer = null;
      Object.keys(byId).forEach(function (id) {
        if (!pending[id]) delete byId[id];
      });
      if (!Object.keys(pending).length) stopControlRestore();
    }, 10000);
    controls.forEach(function (control) {
      if (applyControl(control)) delete pending[control.id];
    });
  }

  function navigate(spec) {
    var tab = document.getElementById('shiny-tab-' + spec.tab);
    if (tab && tab.classList.contains('active')) return;
    var link = document.querySelector('a[data-value="' + spec.tab + '"]');
    if (link) link.click();
  }

  function adapter(id) {
    var spec = SPECS[id];
    if (!spec) return null;
    return {
      id: id,
      label: spec.label,
      ready: function () {
        var api = engine(spec);
        return !!(api && api.captureState && api.captureState(id));
      },
      capture: function () {
        var api = engine(spec);
        var state = api && api.captureState ? api.captureState(id) : null;
        if (!state) throw new Error(spec.label + ' is not ready to save.');
        if (!state.cells.length) throw new Error('Select at least one cell first.');
        return {
          schema: 'cerebronexus-specialist-view',
          version: 1,
          created_at: null,
          dataset: window.cerebroSavedViewDataset || {
            cell_count: 0, cell_fingerprint: ''
          },
          page: {
            id: id, label: spec.label, tab: spec.tab, engine: spec.engine
          },
          selection: { cells: state.cells, geometry: state.geometry },
          view: state.view,
          controls: captureControls(spec)
        };
      },
      downloadPNG: function () {
        var api = engine(spec);
        return !!(api && api.downloadPNG && api.downloadPNG(id));
      },
      apply: function (config) {
        navigate(spec);
        restoreControls(config.controls || []);
        [0, 250, 750].forEach(function (delay) {
          window.setTimeout(function () {
            var api = engine(spec);
            if (api && api.applyState) api.applyState(id, config);
          }, delay);
        });
        return { selectedCells: (config.selection.cells || []).length };
      },
      summary: function () {
        var api = engine(spec);
        var state = api && api.captureState ? api.captureState(id) : null;
        var identity = window.cerebroSavedViewDataset || {};
        return {
          ready: !!state,
          datasetFingerprint: identity.cell_fingerprint || null,
          selectedCells: state ? state.cells.length : 0,
          page: spec.label
        };
      }
    };
  }

  window.cerebroSpecialistViews = Object.freeze({
    get: adapter,
    fromConfig: function (config) {
      return config && config.page ? adapter(config.page.id) : null;
    }
  });
})();
