##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##
tab_gene_expression <- tabItem(
  tabName = "geneExpression",
  ## necessary to ensure alignment of table headers and content
  shinyjs::inlineCSS(
    "
    #expression_details_selected_cells .table th {
      text-align: center;
    }
    #expression_details_selected_cells .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  tagList(
    cerebroVizPageHeader(
      "Gene expression",
      "expression_projection_info",
      "Explore gene expression across cells in projection or trajectory space."
    ),
    fluidRow(
      class = "cerebro-viz-row cerebro-viz-top-layout",
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-toolbar-col",
        div(
          class = "cerebro-viz-toolbar",
          div(
            class = "cerebro-viz-primary",
            div(
              style = "display:none;",
              `aria-hidden` = "true",
              shinyWidgets::radioGroupButtons(
                inputId = "expression_analysis_mode",
                label = NULL,
                choices = c("Gene(s)"),
                selected = "Gene(s)"
              )
            ),
            ## Data, display mode, genes, then the mode's own options: the
            ## order of every page's toolbar.
            uiOutput("expression_projection_select_projection_UI"),
            uiOutput("expression_projection_genes_in_separate_panels_UI"),
            uiOutput(
              "expression_projection_input_type_UI",
              class = "cerebro-gene-input-output"
            ),
            conditionalPanel(
              "input.expression_projection_genes_in_separate_panels == 'rgb'",
              selectInput(
                "expression_rgb_layout",
                "RGB layout",
                choices = c(
                  "Composite RGB" = "composite",
                  "Separate channels" = "separate"
                ),
                selected = "composite"
              )
            ),
            uiOutput("expression_projection_gene_color_mode_UI"),
            uiOutput("expression_genes_displayed")
          ),
          cerebroToolbarActions(
            cerebroSettingsButton(
              "expression_projection_more_button",
              "expression_projection_more"
            ),
            cerebroShareButton("expression_projection")
          ),
          cerebroSettingsDrawer(
            "expression_projection_more",
            cerebroSettingsSection(
              "Appearance",
              tagList(
                uiOutput("expression_projection_additional_parameters_UI"),
                uiOutput("expression_projection_point_border_UI"),
                uiOutput("expression_projection_keep_square_UI")
              ),
              cerebroInfoButton(
                "expression_projection_additional_parameters_info"
              )
            ),
            cerebroSettingsSection(
              "Data",
              uiOutput("expression_projection_data_parameters_UI")
            ),
            cerebroSettingsSection(
              "Group filters",
              uiOutput("expression_projection_group_filters_UI"),
              cerebroInfoButton("expression_projection_group_filters_info")
            )
          ),
          cerebroSelectionStatus(
            "expression_projection",
            "expression_number_of_selected_cells",
            portable = FALSE
          )
        )
      ),
      column(
        width = 12,
        offset = 0,
        class = "cerebro-viz-col",
        div(
          id = "expression_feedback",
          class = "cerebro-view-feedback",
          role = "status",
          `aria-live` = "polite",
          `aria-atomic` = "true",
          hidden = NA
        ),
        cerebroCellViewOutput("expression_projection")
      )
    )
  ),
  div(
    id = "expression_selection_feedback",
    class = "cerebro-secondary-status",
    role = "status",
    `aria-live` = "polite",
    hidden = "hidden"
  ),
  uiOutput("expression_details_selected_cells_UI"),
  uiOutput("expression_in_selected_cells_UI"),
  div(
    id = "expression_summary_gate",
    style = "min-height: 200px;",
    uiOutput("expression_by_group_UI"),
    uiOutput("expression_by_gene_UI")
  ),
  tags$script(HTML(
    "
    (function () {
      var target = document.getElementById('expression_summary_gate');
      if (!target || target.dataset.observed) return;
      target.dataset.observed = 'true';
      // Visible when a quarter of the gate or a quarter of the viewport is in
      // view, whichever is smaller. Read the geometry instead of a fixed ratio
      // threshold: a gate taller than four viewports can never show a quarter
      // of itself, and a threshold below 1 / gate-height would be unreachable.
      var visible = null, published = null;
      function viewportHeight() {
        return window.innerHeight || document.documentElement.clientHeight || 0;
      }
      function visibleNow() {
        var rect = target.getBoundingClientRect();
        var viewport = viewportHeight();
        if (!viewport || rect.height <= 0) return false;
        var intersection = Math.max(0,
          Math.min(rect.bottom, viewport) - Math.max(rect.top, 0));
        return intersection >= 0.25 * Math.min(rect.height, viewport);
      }
      function publish() {
        visible = visibleNow();
        // The first callback runs before Shiny has connected; keep the state
        // and publish it once the input API exists.
        if (!window.Shiny || typeof Shiny.setInputValue !== 'function') return;
        if (published === visible) return;
        published = visible;
        Shiny.setInputValue('expression_summary_viewport_request', visible);
      }
      function schedule() {
        if (schedule.pending) return;
        schedule.pending = true;
        var run = function () { schedule.pending = false; publish(); };
        if (window.requestAnimationFrame) requestAnimationFrame(run);
        else window.setTimeout(run, 50);
      }
      new IntersectionObserver(schedule, {threshold: 0}).observe(target);
      // A gate that grows or shrinks in place changes how much of it counts.
      if (window.ResizeObserver) new ResizeObserver(schedule).observe(target);
      window.addEventListener('scroll', schedule, true);
      window.addEventListener('resize', schedule, true);
      // A reconnect starts a new session with no inputs: publish again.
      if (window.jQuery) window.jQuery(document).on('shiny:connected', function () {
        published = null; publish();
      });
      publish();
    })();
    "
  ))
)
