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
            uiOutput("expression_projection_genes_in_separate_panels_UI"),
            uiOutput("expression_projection_select_projection_UI"),
            uiOutput(
              "expression_projection_input_type_UI",
              class = "cerebro-gene-input-output"
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
                checkboxInput(
                  "expression_projection_keep_square",
                  "Keep plots square",
                  value = FALSE
                )
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
          "Choose a gene to explore expression."
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
      var observer = new IntersectionObserver(function (entries) {
        if (!entries.some(function (entry) { return entry.isIntersecting; })) return;
        Shiny.setInputValue(
          'expression_summary_viewport_request',
          Date.now(),
          {priority: 'event'}
        );
        observer.disconnect();
      }, {threshold: 0.25});
      observer.observe(target);
    })();
    "
  )) #,
  # uiOutput("expression_by_pseudotime_UI")
)
