/* Feedback acknowledges existing painted tokens. Layout shortcuts reuse the
 * existing square-plot inputs; they introduce no new data/readiness requests. */
(function () {
  'use strict';
  var specs = {
    overview_projection: ['overview', 'Overview', /^overview_projection_(to_display|point_color|point_size|point_opacity|point_border|group_labels|keep_square|percentage_cells_to_show|group_filter_.+)$/],
    expression_projection: ['geneExpression', 'Gene expression', /^expression_projection_(to_display|point_size|point_opacity|point_border|group_labels|keep_square|percentage_cells_to_show|group_filter_.+)$/],
    trajectory_projection: ['trajectory', 'Trajectory', /^trajectory_(selected_method|selected_name|point_color|point_size|point_opacity|projection_group_labels|projection_point_border|projection_keep_square|percentage_cells_to_show|projection_group_filter_.+)$/],
    spatial_projection: ['spatial', 'Spatial', /^spatial_projection_(to_display|plot_type|point_color|point_size|point_opacity|group_labels|point_border|keep_square|percentage_cells_to_show|feature_to_display|coexpr_[rgb]|show_region_outlines|background_image|group_filter_.+)$/],
    hla_motif_network: ['hla_tcr_motifs', 'HLA / TCR motifs', /^hla_(chain|scope|color_by|color_allele|pair_allele_i|pair_allele_ii|min_nodes|by_v|show_isolated|node_scale|legend_mode|group_filter_.+)$/],
    ir_clonalUMAP_projection: ['immune_repertoire', 'Clonal projection', /^$/],
    linked: ['coordinated_views', 'Linked views', /^$/]
  };
  var states = {}, installed = false;
  var layoutControls = {
    overview_projection: ['overview_projection_more_button', 'overview_projection_keep_square'],
    expression_projection: ['expression_projection_more_button', 'expression_projection_keep_square'],
    trajectory_projection: ['trajectory_projection_more_button', 'trajectory_projection_keep_square'],
    ir_clonalUMAP_projection: ['ir_more_button', 'ir_clonalUMAP_keep_square'],
    linked: ['cv-more-btn', 'cv-square-plots']
  };
  function setSquareLayout(id, square) {
    if (!layoutControls[id] || !state(id).layoutEligible) return;
    var inputId = layoutControls[id][1], input = document.getElementById(inputId);
    if (input) {
      if (input.checked === square) return;
      input.checked = square;
      // Native change reaches both Shiny's checkbox binding and Linked views.
      input.dispatchEvent(new Event('change', { bubbles: true }));
    } else if (id !== 'linked' && window.Shiny && window.Shiny.setInputValue) {
      // Settings drawers render lazily. Their existing server input also works
      // before the drawer has been opened for the first time.
      window.Shiny.setInputValue(inputId, square, { priority: 'event' });
    } else return;
    state(id).squareLayout = square;
    draw(id);
  }
  function layoutSwitch(id, el) {
    if (el._layoutSwitch) return el._layoutSwitch;
    var group = document.createElement('span');
    group.className = 'cerebro-layout-switch';
    group.setAttribute('role', 'group');
    var label = document.createElement('span');
    label.className = 'cerebro-layout-label';
    label.id = id + '_layout_label';
    label.textContent = id === 'trajectory_projection' ? 'Trajectory layout' : 'UMAP layout';
    group.setAttribute('aria-labelledby', label.id);
    group.appendChild(label);
    var segments = document.createElement('span');
    segments.className = 'cerebro-layout-segments';
    group._buttons = ['wide', 'square'].map(function (shape) {
      var button = document.createElement('button');
      button.type = 'button';
      button.setAttribute('data-layout-id', id);
      button.setAttribute('data-layout-shape', shape);
      button.title = shape === 'wide' ? 'Use the available plot width' : 'Use equal plot width and height';
      button.innerHTML = '<i class="cerebro-layout-icon is-' + shape + '" aria-hidden="true"></i>' +
        (shape === 'wide' ? 'Wide' : 'Square');
      segments.appendChild(button);
      return button;
    });
    group.appendChild(segments);
    el.appendChild(group);
    el._layoutSwitch = group;
    return group;
  }
  function normal(value) {
    return JSON.stringify((value == null ? [] : Array.isArray(value) ? value : [value]).map(String).sort());
  }
  function state(id) {
    return states[id] || (states[id] = { pending: {}, frames: new Map(), painted: null,
      ready: false, selection: null, auxiliary: '', operation: '', terminal: '' });
  }
  function fresh(message) {
    return window.CerebroDatasetContext && window.CerebroDatasetContext.accepts(message);
  }
  function row(id) {
    var pane = document.getElementById('shiny-tab-' + specs[id][0]);
    if (!pane) return null;
    var el = pane.querySelector('.cerebro-page-feedback');
    if (!el) {
      el = document.createElement('div');
      el.className = 'cerebro-view-feedback cerebro-page-feedback';
      var heading = pane.querySelector('.cerebro-viz-page-heading');
      if (heading) heading.appendChild(el);
      else pane.insertBefore(el, pane.firstChild);
    }
    return el;
  }
  function mark(key, pending) {
    var el = document.getElementById(key);
    var group = el && el.closest('.form-group, .cv-filt');
    if (group) group.classList.toggle('cerebro-control-pending', pending);
  }
  function localStatus(id) {
    var pane = document.getElementById('shiny-tab-' + specs[id][0]);
    if (!pane) return null;
    var region = pane.querySelector('.cerebro-page-loading-region') || pane;
    var el = region.querySelector('.cerebro-local-feedback');
    if (!el) {
      el = document.createElement('div');
      el.className = 'cerebro-view-feedback cerebro-local-feedback';
      el.setAttribute('role', 'status');
      el.setAttribute('aria-live', 'polite');
      region.classList.add('cerebro-feedback-region');
      region.appendChild(el);
    }
    return el;
  }
  function draw(id) {
    var s = state(id), el = row(id);
    if (!el) return;
    var pending = Object.keys(s.pending);
    var text = pending.length ? 'Updating: ' + pending.map(function (key) {
      return s.pending[key].label;
    }).join(' · ') : s.terminal || (s.ready ? s.empty ? 'No cells match these filters' : '' : 'Loading ' + specs[id][1] + '…');
    // Settled selection counts already belong to the Active cohort toolbar.
    text = [text, s.operation, s.expression, s.selection, s.auxiliary].filter(Boolean).join(' · ');
    // Clonal projection joins only the layout advice; its existing specialist
    // lifecycle continues to own loading and errors for the other IR tabs.
    var layoutOnly = id === 'ir_clonalUMAP_projection';
    if (!layoutOnly) window.CerebroPageLifecycle.setTerminal(
      document.getElementById('shiny-tab-' + specs[id][0]), !!s.terminal);
    // Routine feedback lives over the plot; it never adds a page-header row.
    var status = layoutOnly ? null : localStatus(id);
    var failed = !!s.terminal && s.terminalKind !== 'empty' ||
      /unavailable|unable|failed/i.test([s.operation, s.expression].join(' '));
    if (status) {
      status.textContent = text;
      status.hidden = !text;
      status.classList.toggle('is-pending', !!text && !failed && !s.empty &&
        (!s.ready || pending.length > 0 || /loading|updating/i.test(text)));
      status.classList.toggle('is-error', failed);
    }
    var pane = el.closest('.tab-pane');
    var host = document.getElementById(id + '_cell_view_host');
    var layoutVisible = !layoutOnly || (host && host.getClientRects().length > 0);
    var showLayout = layoutVisible && layoutControls[id] && s.layoutEligible && s.ready && !s.empty && !s.terminal &&
        pane && pane.classList.contains('active');
    if (showLayout) {
      var group = layoutSwitch(id, el);
      group._buttons.forEach(function (button, index) {
        button.setAttribute('aria-pressed', String((index === 1) === !!s.squareLayout));
      });
    }
    el.hidden = !showLayout;
    // A terminal result is explicitly not a painted/ready acknowledgement.
    if (host && !layoutOnly) host.classList.toggle('cerebro-view-terminal', !!s.terminal);
  }
  function settle(id) {
    var s = state(id), controls = s.frames.get(s.painted);
    if (!controls) return;
    Object.keys(s.pending).forEach(function (key) {
      if (Object.prototype.hasOwnProperty.call(controls, key) &&
          normal(controls[key]) === s.pending[key].value) {
        delete s.pending[key]; mark(key, false);
      }
    });
  }
  function inputValue(el) {
    if (el.selectize) return el.selectize.getValue();
    if (el.type === 'checkbox') return el.checked;
    if (el.multiple) return Array.from(el.selectedOptions).map(function (o) { return o.value; });
    return el.value;
  }
  // The Immune repertoire tabset and its controls are rebuilt by renderUI
  // whenever ir_data() changes, and Shiny then reports the rebuilt controls'
  // current values as input changes. Those reports are not edits: invalidating
  // on them hides advice that still matches the painted plot, and nothing
  // brings it back until a paint that may never come.
  var irLayoutInputs = {};
  function irLayoutEdited(event, key) {
    var element = event.el || document.getElementById(key);
    var value = normal(
      event.value !== undefined ? event.value : element && inputValue(element)
    );
    var seen = irLayoutInputs[key];
    irLayoutInputs[key] = { element: element, value: value };
    return !!seen && seen.element === element && seen.value !== value;
  }
  function changed(event) {
    var key = event.name || event.target.id;
    if (key === 'ir_tabs' || key === 'ir_p_umap_group_by') {
      if (!irLayoutEdited(event, key)) return;
      state('ir_clonalUMAP_projection').layoutEligible = false;
      draw('ir_clonalUMAP_projection');
      return;
    }
    if (/^(coordviews_gene(?:_[rgb])?|coordviews_expression_mode|cv-pick-color)$/.test(key)) {
      linkedExpressionChange(key);
      return;
    }
    var id = Object.keys(specs).find(function (id) { return specs[id][2].test(key); });
    if (!id || !window.CerebroDatasetContext || window.CerebroDatasetContext.phase() !== 'ready') return;
    var el = document.getElementById(key), group = el && el.closest('.form-group, .cv-filt');
    // Ignore binding/default updates. Selectize, sliders and keyboard input all
    // retain focus within their form group during user-generated changes.
    if (!el || !group || !group.contains(document.activeElement)) return;
    var value = event.name ? event.value : inputValue(el);
    var label = group.querySelector('.cv-filt-btn') || group.querySelector('label');
    var shown = /group_filter_/.test(key) ? (Array.isArray(value) ? value.length : value ? 1 : 0) + ' selected' : el.selectize ? el.selectize.items.map(function (v) {
      var option = el.selectize.options[v];
      return option && option[el.selectize.settings.labelField] || v;
    }).join(', ') : el.tagName === 'SELECT' ? Array.from(el.selectedOptions).map(function (o) { return o.textContent; }).join(', ') : String(value);
    var s = state(id);
    s.pending[key] = { value: normal(value), label: ((label && label.textContent.trim()) || 'Parameter') + ' ' + shown };
    mark(key, true); settle(id); draw(id);
  }
  function linkedControls(rgb) {
    var ids = rgb ? ['coordviews_gene_r', 'coordviews_gene_g', 'coordviews_gene_b'] : ['coordviews_gene'];
    var controls = {};
    ids.forEach(function (id) { var el = document.getElementById(id); controls[id] = el ? inputValue(el) : []; });
    if (!rgb) {
      var mode = document.getElementById('cv-pick-color');
      controls.coordviews_expression_mode = mode && inputValue(mode) === '__gene_panels__' ? 'panels' : 'mean';
    }
    return controls;
  }
  function linkedExpressionChange(key) {
    if (!window.CerebroDatasetContext || window.CerebroDatasetContext.phase() !== 'ready') return;
    var picker = document.getElementById('cv-pick-color');
    var mode = picker && inputValue(picker), rgb = mode === '__rgb__';
    var s = state('linked');
    if (['__gene__', '__gene_panels__', '__rgb__'].indexOf(mode) < 0) {
      s.expression = ''; s.expressionRequest = null; draw('linked'); return;
    }
    // Switching back to an already loaded mode need not send a server request.
    // Only the actual gene/Shiny mode input owns a new pending operation.
    if (key === 'cv-pick-color') {
      s.expression = ''; s.expressionRequest = null; draw('linked'); return;
    }
    var el = document.getElementById(key) || picker;
    var group = el && el.closest('.cv-ctl');
    if (!group || !group.contains(document.activeElement)) return;
    var controls = linkedControls(rgb);
    s.expressionRequest = { rgb: rgb, controls: controls };
    var genes = Object.keys(controls).filter(function (id) { return id !== 'coordviews_expression_mode'; })
      .map(function (id) { return controls[id]; }).flat().filter(Boolean);
    s.expression = genes.length ? 'Updating expression: ' + genes.join(', ') + '…' : '';
    draw('linked');
  }
  function install() {
    if (installed || !window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    installed = true;
    window.Shiny.addCustomMessageHandler('cerebro_view_feedback', function (message) {
      if (!specs[message.id] || !fresh(message) || message.renderToken == null) return;
      var s = state(message.id);
      s.frames.set(message.renderToken, message.controls || {});
      if (s.terminal) {
        s.recoverToken = message.renderToken;
        if (s.painted === message.renderToken) s.terminal = '';
      }
      while (s.frames.size > 8) s.frames.delete(s.frames.keys().next().value);
      settle(message.id); draw(message.id);
    });
    window.Shiny.addCustomMessageHandler('cerebro_linked_expression_feedback', function (message) {
      if (!fresh(message)) return;
      var s = state('linked'), request = s.expressionRequest;
      if (!request || !!message.rgb !== request.rgb) return;
      if (!Object.keys(request.controls).every(function (key) {
        return normal(request.controls[key]) === normal(message.controls[key]);
      })) return;
      var genes = Object.keys(request.controls).filter(function (id) { return id !== 'coordviews_expression_mode'; })
        .map(function (id) { return request.controls[id]; }).flat().filter(Boolean);
      s.expression = !genes.length || message.ok ? '' : 'Expression unavailable for this selection';
      draw('linked');
    });
  }
  function terminal(id, text, kind) {
    var s = state(id);
    s.terminal = text;
    s.terminalKind = kind || 'error';
    s.recoverToken = null;
    Object.keys(s.pending).forEach(function (key) { mark(key, false); });
    s.pending = {};
    draw(id);
    window.dispatchEvent(new CustomEvent('cerebro:view-terminal', { detail: { page: specs[id][0] } }));
  }
  function init() {
    Object.keys(specs).forEach(draw);
    document.addEventListener('click', function (event) {
      var button = event.target.closest('[data-layout-id]');
      if (button) setSquareLayout(button.getAttribute('data-layout-id'),
        button.getAttribute('data-layout-shape') === 'square');
    });
    document.addEventListener('change', function (event) {
      Object.keys(layoutControls).forEach(function (id) {
        if (event.target.id !== layoutControls[id][1]) return;
        state(id).squareLayout = !!event.target.checked;
        draw(id);
      });
    }, true);
    window.addEventListener('cerebro:plot-layout', function (event) {
      var d = event.detail;
      if (!d || !layoutControls[d.id] || !fresh(d)) return;
      var s = state(d.id);
      s.layoutEligible = !!d.eligible;
      s.squareLayout = !!d.square;
      draw(d.id);
    });
    install();
    if (window.jQuery) window.jQuery(document)
      .on('change.cerebroViewFeedback shiny:inputchanged.cerebroViewFeedback', changed)
      .on('shiny:connected.cerebroViewFeedback', install)
      .on('shown.bs.tab.cerebroViewFeedback', function () { Object.keys(specs).forEach(draw); })
      .on('shiny:value.cerebroViewFeedback', function (event) {
        if (event.target.id !== 'hla_motif_note') return;
        // Shiny inserts the HTML after delivering the value event.
        window.setTimeout(function () {
          var marker = event.target.querySelector('[data-view-state]');
          if (marker) terminal('hla_motif_network', marker.textContent.trim(), marker.getAttribute('data-view-state'));
        }, 0);
      })
      .on('shiny:error.cerebroViewFeedback', function (event) {
        if (!event.error || !event.error.message) return;
        // Shiny reports an output that has no bound element on `document`
        // (ShinyApp.receiveError). That target has no closest() and no page to
        // attribute the failure to. Throwing here would abort the rest of the
        // message frame, dropping the input updates and notifications Shiny
        // batched with it.
        var target = event.target;
        if (!target || typeof target.closest !== 'function') return;
        var pane = target.closest('.tab-pane');
        var id = Object.keys(specs).find(function (id) { return pane && pane.id === 'shiny-tab-' + specs[id][0]; });
        if (id && /projection|motif_note/.test(target.id)) terminal(id, 'Unable to update this view. ' + event.error.message);
      });
    window.addEventListener('cerebro:cell-view-ready', function (event) {
      var d = event.detail;
      if (!d || !specs[d.id] || !d.painted || !fresh(d)) return;
      var s = state(d.id);
      if (s.terminal && s.recoverToken !== d.renderToken) return;
      s.ready = true; s.terminal = ''; s.painted = d.renderToken;
      s.empty = d.pointCount === 0;
      settle(d.id); draw(d.id);
    });
    window.addEventListener('cerebro:specialist-state', function (event) {
      var d = event.detail;
      if (!d || !specs[d.viewId] || !fresh(d)) return;
      state(d.viewId).selection = d.selectionPending ? 'Loading identities for summaries and table…' : '';
      draw(d.viewId);
    });
    window.addEventListener('cerebro:linkedviews-ready', function (event) {
      var d = event.detail;
      if (!d || !fresh(d)) return;
      var s = state('linked');
      if (d.primaryReady && d.painted) s.ready = true;
      s.auxiliary = s.ready && !d.complete ? 'Loading annotations and additional views…' : '';
      draw('linked');
    });
    window.addEventListener('cerebro:linkedviews-selection', function (event) {
      var d = event.detail;
      if (!d || !fresh(d)) return;
      state('linked').selection = '';
      draw('linked');
    });
    window.addEventListener('cerebro:view-operation', function (event) {
      var d = event.detail;
      if (!d || !specs[d.id] || !fresh(d)) return;
      // Success/absence is already evident in the plot and image selector.
      // Keep only in-flight work and actionable failures, on every page.
      state(d.id).operation = /loading|updating|unavailable|unable|failed/i.test(d.text || '') ? d.text : '';
      draw(d.id);
    });
    window.addEventListener('cerebro:view-appearance', function (event) {
      var d = event.detail;
      if (!d || !specs[d.id] || !fresh(d)) return;
      var s = state(d.id), prefix = d.id + '_';
      Object.keys(d.values).forEach(function (key) {
        var input = prefix + (key === 'draw_border' ? 'point_border' : key);
        if (s.pending[input] && normal(d.values[key]) === s.pending[input].value) {
          delete s.pending[input]; mark(input, false);
        }
      });
      draw(d.id);
    });
    window.addEventListener('cerebro:dataset-context', function (event) {
      var d = event.detail || {};
      if (!d.changed && d.phase === 'ready') return;
      Object.keys(states).forEach(function (id) { Object.keys(states[id].pending).forEach(function (key) { mark(key, false); }); });
      states = {}; Object.keys(specs).forEach(draw);
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();
}());
