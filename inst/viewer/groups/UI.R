##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
tab_groups <- tabItem(
  tabName = "groups",
  shinyjs::inlineCSS(
    "
    #groups_by_other_group_table .table th {
      text-align: center;
    }
    "
  ),
  div(class = "cerebro-viz-page-heading", h1("Groups")),
  uiOutput(
    "groups_controls_UI",
    class = "cerebro-groups-controls cerebro-viz-toolbar"
  ),
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Composition by other group"),
        cerebroInfoButton("groups_by_other_group_info")
      ),
      div(
        class = "cerebro-page-loading-region",
        div(id = "groups_by_other_group_plot", class = "cerebro-chart"),
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

      // Once a metric is drawn, fetch the other metric tabs while idle.
      window.addEventListener('cerebro:groups-primary-ready', function (event) {
        var detail = event.detail || {};
        if (detail.metric === 'composition' || detail.eventKind !== 'primary') return;
        var ask = function () {
          Shiny.setInputValue(
            'groups_expression_metrics_prefetch',
            detail.selectedGroup,
            {priority: 'event'}
          );
        };
        if (window.requestIdleCallback) window.requestIdleCallback(ask, {timeout: 2000});
        else setTimeout(ask, 200);
      });
    })();
    "
  ))
)
