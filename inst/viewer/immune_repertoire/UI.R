##----------------------------------------------------------------------------##
## Tab: Immune Repertoire (unified TCR/BCR)
##
## High-frequency analysis controls stay above the visualization. Low-frequency
## analysis, display, and filtering controls share the app-wide settings drawer.
##----------------------------------------------------------------------------##

## Prepend the shared plotly layout factory and the shared projection-scatter
## renderer, then IR's thin Clonal UMAP wrapper — all in ONE extendShinyjs()
## text so they share a global scope (same pattern as overview/spatial UI.R).
## Only the NON-FACETED Clonal UMAP renders through the shared renderer; the
## faceted variant stays on the static ggplot renderPlot (see visualizations.R).
## Shared projection engine loaded once app-wide (see shiny_UI.R); inline only
## this tab's thin wrappers over the window globals it exposes.
js_code_ir_projection <- cerebro_read_file(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/immune_repertoire/js_projection_update_plot.js"
  )
)

tab_immune_repertoire <- tabItem(
  tabName = "immune_repertoire",
  shinyjs::extendShinyjs(
    text = js_code_ir_projection,
    functions = c(
      "updateClonalUMAP",
      "irClonalUMAPClearSelection",
      "irClonalUMAPZoomToSelection"
    )
  ),
  cerebroVizPageHeader(
    "Immune repertoire",
    "ir_main_parameters_info",
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
        cerebroSettingsButton("ir_more_button", "ir_more"),
        cerebroSettingsDrawer(
          "ir_more",
          cerebroSettingsSection(
            "Analysis",
            uiOutput("ir_more_analysis_UI"),
            cerebroInfoButton("ir_more_analysis_info")
          ),
          cerebroSettingsSection(
            "Appearance",
            uiOutput("ir_additional_params_UI"),
            cerebroInfoButton("ir_additional_parameters_info")
          ),
          cerebroSettingsSection(
            "Group filters",
            uiOutput("ir_group_filters_UI"),
            cerebroInfoButton("ir_group_filters_info")
          )
        )
      )
    ),
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-col",
      cerebroBox(
        title = tagList(
          boxTitle("Immune Repertoire visualizations"),
          cerebroInfoButton("ir_visualizations_info")
        ),
        content = tagList(
          uiOutput("ir_help_panel"),
          uiOutput("ir_visualizations_UI")
        )
      )
    )
  )
)
