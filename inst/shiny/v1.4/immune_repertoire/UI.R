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
js_code_ir_projection <- paste(
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/www/projection_layouts.js"
    )
  ),
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/www/projection_scatter.js"
    )
  ),
  readr::read_file(
    paste0(
      Cerebro.options[["cerebro_root"]],
      "/shiny/v1.4/immune_repertoire/js_projection_update_plot.js"
    )
  ),
  sep = "\n"
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
      shiny::tagAppendAttributes(
        cerebroBox(
          title = tagList(
            boxTitle("Immune Repertoire visualizations"),
            cerebroInfoButton("ir_visualizations_info")
          ),
          content = tagList(
            uiOutput("ir_help_panel"),
            tags$div(
              class = "cerebro-viewport-source",
              uiOutput("ir_visualizations_UI")
            )
          )
        ),
        class = "cerebro-viewport-gate"
      )
    )
  )
)
