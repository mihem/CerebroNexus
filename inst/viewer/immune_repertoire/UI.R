##----------------------------------------------------------------------------##
## Tab: Immune Repertoire (unified TCR/BCR)
##
## High-frequency analysis controls stay above the visualization. Low-frequency
## analysis, display, and filtering controls share the app-wide settings drawer.
##----------------------------------------------------------------------------##

## The Clonal UMAP renders through the shared renderer, grouped or not: a
## grouping column only turns its single canvas into one panel per group.
tab_immune_repertoire <- tabItem(
  tabName = "immune_repertoire",
  cerebroVizPageHeader(
    "Immune repertoire",
    "ir_visualizations_info",
    "Explore clonotype abundance, diversity, overlap, and clonal structure."
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
          uiOutput("ir_main_params_UI")
        ),
        cerebroToolbarActions(
          cerebroSettingsButton("ir_more_button", "ir_more"),
          conditionalPanel(
            condition = "input.ir_tabs == 'Clonal UMAP'",
            cerebroShareButton("ir_clonalUMAP_projection")
          )
        ),
        tagAppendAttributes(
          cerebroSettingsDrawer(
            "ir_more",
            uiOutput("ir_appearance_section_UI"),
            cerebroSettingsSection(
              "Analysis",
              uiOutput("ir_more_analysis_UI"),
              cerebroInfoButton("ir_more_analysis_info")
            ),
            cerebroSettingsSection(
              "Group filters",
              uiOutput("ir_group_filters_UI"),
              cerebroInfoButton("ir_group_filters_info")
            )
          ),
          `data-cerebro-drawer-sticky-in-page` = ""
        ),
        uiOutput("ir_selection_status_UI")
      )
    ),
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-col",
      uiOutput("ir_help_panel"),
      uiOutput("ir_visualizations_UI"),
      conditionalPanel(
        condition = "!input.ir_tabs || input.ir_tabs == 'Clonal UMAP'",
        cerebroCellViewOutput("ir_clonalUMAP_projection")
      )
    )
  )
)
