##----------------------------------------------------------------------------##
## Tab: Immune Repertoire (unified TCR/BCR)
##
## Layout mirrors the Main tab (gene_expression/UI_projection.R): a left column
## of parameter boxes (Main / Additional / Group filters) and a right column
## holding the visualization tab strip and the current plot.
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
    "/shiny/v1.4/immune_repertoire/js_projection_update_plot.js"
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
  fluidRow(
    class = "cerebro-viz-row",
    ## ---- Left column: parameter boxes ---------------------------------- ##
    column(
      width = 3,
      offset = 0,
      class = "cerebro-param-col",
      tagList(
        cerebroBox(
          title = tagList(
            "Main parameters",
            cerebroInfoButton("ir_main_parameters_info")
          ),
          uiOutput("ir_main_params_UI")
        ),
        cerebroBox(
          title = tagList(
            "Additional parameters",
            cerebroInfoButton("ir_additional_parameters_info")
          ),
          uiOutput("ir_additional_params_UI"),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Group filters",
            cerebroInfoButton("ir_group_filters_info")
          ),
          uiOutput("ir_group_filters_UI"),
          collapsed = TRUE
        )
      )
    ),
    ## ---- Right column: visualization tab strip + current plot ---------- ##
    column(
      width = 9,
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
