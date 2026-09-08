##----------------------------------------------------------------------------##
## Tab: Trajectory — expression metrics by state.
##----------------------------------------------------------------------------##

output[["trajectory_expression_metrics_UI"]] <- renderUI({
  req(trajectory_selection_ok())

  tab_panels <- list(
    tabPanel(
      "Number of transcripts",
      uiOutput("trajectory_states_nUMI_UI")
    ),
    tabPanel(
      "Number of expressed genes",
      uiOutput("trajectory_states_nGene_UI")
    )
  )
  if (hasMitoColumn()) {
    tab_panels[[length(tab_panels) + 1L]] <- tabPanel(
      "Mitochondrial gene expression",
      uiOutput("trajectory_states_percent_mt_UI")
    )
  }
  if (hasRiboColumn()) {
    tab_panels[[length(tab_panels) + 1L]] <- tabPanel(
      "Ribosomal gene expression",
      uiOutput("trajectory_states_percent_ribo_UI")
    )
  }
  if (hasEryColumn()) {
    tab_panels[[length(tab_panels) + 1L]] <- tabPanel(
      "Erythrocyte gene expression",
      uiOutput("trajectory_states_percent_ery_UI")
    )
  }

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression metrics"),
        cerebroInfoButton("trajectory_expression_metrics_info")
      ),
      do.call(
        tabBox,
        c(
          list(
            title = NULL,
            width = 12,
            id = "trajectory_expression_metrics_tabs"
          ),
          tab_panels
        )
      )
    )
  )
})

trajectory_metric_by_state <- function(
  id,
  available,
  metric,
  missing_text,
  y_title,
  mode,
  cache_key = id
) {
  ui_id <- paste0("trajectory_states_", id, "_UI")
  text_id <- paste0("trajectory_states_", id, "_text")
  plot_id <- paste0("trajectory_states_", id, "_plot")

  output[[ui_id]] <- renderUI({
    if (available()) {
      plotly::plotlyOutput(plot_id)
    } else {
      textOutput(text_id)
    }
  })
  output[[text_id]] <- renderText(missing_text)
  output[[plot_id]] <- plotly::renderPlotly({
    req(trajectory_selection_ok())
    metric_name <- metric()
    req(metric_name)
    trajectory_data <- getTrajectory(
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]]
    )[["meta"]]
    state_colors <- setNames(
      cerebro_group_colors(length(levels(trajectory_data$state))),
      levels(trajectory_data$state)
    )
    plotlyViolin(
      table = mergeTrajectoryWithMetaData(list(meta = trajectory_data)),
      metric = metric_name,
      coloring_variable = "state",
      colors = state_colors,
      y_title = y_title,
      mode = mode
    )
  }) %>%
    cachePlot(
      input[["trajectory_selected_method"]],
      input[["trajectory_selected_name"]],
      cache_key,
      available_crb_files$selected
    )
}

trajectory_metric_by_state(
  "nUMI",
  function() "nUMI" %in% colnames(getMetaData()),
  function() "nUMI",
  "Column with number of transcript per cell not available.",
  "Number of transcripts",
  "integer"
)
trajectory_metric_by_state(
  "nGene",
  function() "nGene" %in% colnames(getMetaData()),
  function() "nGene",
  "Column with number of expressed genes per cell not available.",
  "Number of expressed genes",
  "integer"
)
trajectory_metric_by_state(
  "percent_mt",
  hasMitoColumn,
  getMitoColumn,
  "Column with percentage of mitochondrial expression not available.",
  "Percentage of transcripts",
  "percent"
)
trajectory_metric_by_state(
  "percent_ribo",
  hasRiboColumn,
  getRiboColumn,
  "Column with percentage of ribosomal expression not available.",
  "Percentage of transcripts",
  "percent"
)
trajectory_metric_by_state(
  "percent_ery",
  hasEryColumn,
  getEryColumn,
  "Column with percentage of erythrocyte/hemoglobin expression not available.",
  "Percentage of transcripts",
  "percent"
)

observeEvent(input[["trajectory_expression_metrics_info"]], {
  showCerebroInfoModal(trajectory_expression_metrics_info)
})

trajectory_expression_metrics_info <- list(
  title = "Number of transcripts",
  text = HTML(
    "Violin plots showing the number of transcripts (nUMI/nCounts), the number of expressed genes (nGene/nFeature), as well as the percentage of transcripts coming from mitochondrial, ribosomal, and erythrocyte/hemoglobin genes in each state."
  )
)
