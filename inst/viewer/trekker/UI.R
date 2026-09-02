##----------------------------------------------------------------------------##
## Tab: Trekker — a thin specialist-page adapter over cell_views.js.
##----------------------------------------------------------------------------##

tab_trekker <- tabItem(
  tabName = "trekker",
  cerebroVizPageHeader(
    "Trekker",
    "trekker_viz_info",
    "Compare physical position with transcriptome space for the same nuclei."
  ),
  fluidRow(
    class = "cerebro-viz-row cerebro-viz-top-layout",
    column(
      width = 12,
      class = "cerebro-viz-toolbar-col",
      div(
        class = "cerebro-viz-toolbar",
        div(
          class = "cerebro-viz-primary",
          uiOutput("trekker_main_parameters_ui")
        ),
        ## Trekker uses the same client-owned point, filter and image state as
        ## Linked views, so both buttons open the same drawer node. Duplicating
        ## that node would duplicate input ids and split one renderer's state.
        cerebroSettingsButton("trekker_more_button", "cv-more")
      )
    ),
    column(
      width = 12,
      class = "cerebro-viz-col",
      cerebroSelectionStatus(
        "trekker_projection",
        "trekker_number_of_selected_cells",
        portable = FALSE
      ),
      cerebroCellViewOutput("trekker_projection"),
      div(id = "trekker_shared_insights")
    )
  )
)
