##----------------------------------------------------------------------------##
## Tab: Immune Repertoire (unified TCR/BCR)
##
## Layout mirrors the Main tab (gene_expression/UI_projection.R): a left column
## of parameter boxes (Main / Additional / Group filters) and a right column
## holding the visualization tab strip and the current plot.
##----------------------------------------------------------------------------##

tab_immune_repertoire <- tabItem(
  tabName = "immune_repertoire",
  fluidRow(
    ## ---- Left column: parameter boxes ---------------------------------- ##
    column(
      width = 3,
      offset = 0,
      style = "padding: 0px;",
      tagList(
        cerebroBox(
          title = tagList(
            "Main parameters",
            actionButton(
              inputId = "ir_main_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("ir_main_params_UI")
        ),
        cerebroBox(
          title = tagList(
            "Additional parameters",
            actionButton(
              inputId = "ir_additional_parameters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
          ),
          uiOutput("ir_additional_params_UI"),
          collapsed = TRUE
        ),
        cerebroBox(
          title = tagList(
            "Group filters",
            actionButton(
              inputId = "ir_group_filters_info",
              label = "info",
              icon = NULL,
              class = "btn-xs",
              title = "Show additional information for this panel.",
              style = "margin-left: 5px"
            )
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
      style = "padding: 0px;",
      cerebroBox(
        title = boxTitle("Immune Repertoire visualizations"),
        content = tagList(
          uiOutput("ir_help_panel"),
          uiOutput("ir_visualizations_UI")
        )
      )
    )
  )
)
