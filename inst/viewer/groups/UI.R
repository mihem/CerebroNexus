##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
groups_plotly_dependencies <- local({
  empty_plot <- plotly::plot_ly(
    x = numeric(),
    y = numeric(),
    type = "bar"
  )
  dependencies <- htmltools::renderTags(empty_plot)$dependencies
  htmltools::attachDependencies(
    tags$span(style = "display: none;", `aria-hidden` = "true"),
    dependencies
  )
})

tab_groups <- tabItem(
  tabName = "groups",
  groups_plotly_dependencies,
  shinyjs::inlineCSS(
    "
    #groups_by_other_group_table .table th {
      text-align: center;
    }
    "
  ),
  div(class = "cerebro-viz-page-heading", h1("Groups")),
  uiOutput("groups_controls_UI", class = "cerebro-groups-controls"),
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Composition by other group"),
        cerebroInfoButton("groups_by_other_group_info")
      ),
      div(
        class = "cerebro-page-loading-region",
        plotly::plotlyOutput("groups_by_other_group_plot"),
        conditionalPanel(
          condition = "input.groups_by_other_group_show_table === true",
          DT::dataTableOutput("groups_by_other_group_table")
        )
      )
    )
  ),
  div(
    id = "groups_expression_metrics_gate",
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Expression metrics"),
          cerebroInfoButton("groups_expression_metrics_info")
        ),
        uiOutput("groups_expression_metrics_UI", class = "cerebro-result-slot")
      )
    )
  ),
  tags$script(HTML(
    "
    (function () {
      var target = document.getElementById('groups_expression_metrics_gate');
      if (!target || target.dataset.observed) return;
      target.dataset.observed = 'true';
      var visible = false;

      function requestMetrics() {
        if (!visible || target.dataset.requested) return;
        target.dataset.requested = 'true';
        Shiny.setInputValue(
          'groups_expression_metrics_render_request',
          Date.now(),
          {priority: 'event'}
        );
        observer.disconnect();
      }

      var observer = new IntersectionObserver(function (entries) {
        visible = entries.some(function (entry) { return entry.isIntersecting; });
        requestMetrics();
      }, {rootMargin: '200px 0px'});

      observer.observe(target);
    })();
    "
  )),
  tags$script(HTML(
    "
    (function () {
      if (window.__cerebroGroupsReadyInstalled) return;
      window.__cerebroGroupsReadyInstalled = true;
      var plotIds = [
        'groups_by_other_group_plot',
        'groups_nUMI_plot',
        'groups_nGene_plot',
        'groups_percent_mt_plot',
        'groups_percent_ribo_plot'
      ];

      function inputValue(id) {
        var element = document.getElementById(id);
        if (!element) return null;
        if (window.jQuery) {
          var binding = window.jQuery(element).data('shiny-input-binding');
          if (binding && typeof binding.getValue === 'function') {
            try { return binding.getValue(element); } catch (ignore) {}
          }
        }
        return element.value == null ? null : element.value;
      }

      function metricFor(plot) {
        return {
          groups_nUMI_plot: 'nUMI',
          groups_nGene_plot: 'nGene',
          groups_percent_mt_plot: 'percent_mt',
          groups_percent_ribo_plot: 'percent_ribo',
          groups_by_other_group_plot: 'composition'
        }[plot.id] || '';
      }

      function report(plot, eventKind) {
        if (!plot || plot.offsetParent === null ||
            !Array.isArray(plot.data) || !plot.data.length) return;
        var identity = window.cerebroSavedViewDataset || {};
        plot.dataset.cerebroGroupsReadyFingerprint =
          String(identity.cell_fingerprint || '');
        window.dispatchEvent(new CustomEvent('cerebro:groups-primary-ready', {
          detail: {
            page: 'groups',
            plotId: plot.id,
            metric: metricFor(plot),
            traceCount: plot.data.length,
            datasetFingerprint: String(identity.cell_fingerprint || ''),
            selectedGroup: inputValue('groups_selected_group'),
            secondGroup: inputValue('groups_by_other_group_second_group'),
            plotType: inputValue('groups_by_other_group_plot_type'),
            benchmarkGeneration:
              Number(window.__cerebroPageBenchGeneration) || 0,
            eventKind: eventKind
          }
        }));
      }

      function bind(plot) {
        if (!plot || plot.__cerebroGroupsReadyBound === plot.on ||
            typeof plot.on !== 'function') return;
        // Plotly.newPlot replaces its emitter on dataset replacement even
        // when Shiny reuses the same output element.
        plot.__cerebroGroupsReadyBound = plot.on;
        plot.on('plotly_afterplot', function () { report(plot, 'primary'); });
      }

      function scan() {
        plotIds.forEach(function (id) {
          var root = document.getElementById(id);
          var plot = root && (root.classList.contains('js-plotly-plot')
            ? root : root.querySelector('.js-plotly-plot'));
          bind(plot);
        });
      }

      var tab = document.getElementById('shiny-tab-groups');
      if (tab) {
        new MutationObserver(scan).observe(tab, {childList: true, subtree: true});
      }
      if (window.jQuery) {
        window.jQuery(document).on(
          'shiny:value.cerebroGroupsPrimaryReady',
          function (event) {
            if (plotIds.indexOf(event.name) >= 0) scan();
          }
        );
      }
      document.addEventListener('click', function (event) {
        var link = event.target && event.target.closest
          ? event.target.closest('a[href=\"#shiny-tab-groups\"]') : null;
        if (!link) return;
        window.requestAnimationFrame(function () {
          scan();
          plotIds.forEach(function (id) {
            var root = document.getElementById(id);
            var plot = root && (root.classList.contains('js-plotly-plot')
              ? root : root.querySelector('.js-plotly-plot'));
            var identity = window.cerebroSavedViewDataset || {};
            if (plot && plot.dataset.cerebroGroupsReadyFingerprint ===
                String(identity.cell_fingerprint || '')) {
              report(plot, 'cached');
            }
          });
        });
      }, true);
      scan();
    })();
    "
  ))
)
