##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Expression metrics:
## - number of transcripts
## - number of expressed genes
## - percent of transcripts from mitochondrial genes
## - percent of transcripts from ribosomal genes
## - percent of transcripts from erythrocyte/hemoglobin genes
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_expression_metrics_UI"]] <- renderUI({
  req(input[["trajectory_expression_section_visible"]])
  req(trajectory_projection_sent())
  ##
  req(trajectory_selection_ok())

  ## build list of tab panels dynamically based on available data
  tab_panels <- list()

  ## always show nUMI and nGene tabs
  tab_panels[[length(tab_panels) + 1]] <- tabPanel(
    "Number of transcripts",
    uiOutput("trajectory_states_nUMI_UI")
  )
  tab_panels[[length(tab_panels) + 1]] <- tabPanel(
    "Number of expressed genes",
    uiOutput("trajectory_states_nGene_UI")
  )

  ## conditionally add mito tab
  if (hasMitoColumn()) {
    tab_panels[[length(tab_panels) + 1]] <- tabPanel(
      "Mitochondrial gene expression",
      uiOutput("trajectory_states_percent_mt_UI")
    )
  }

  ## conditionally add ribo tab
  if (hasRiboColumn()) {
    tab_panels[[length(tab_panels) + 1]] <- tabPanel(
      "Ribosomal gene expression",
      uiOutput("trajectory_states_percent_ribo_UI")
    )
  }

  ## conditionally add ery tab
  if (hasEryColumn()) {
    tab_panels[[length(tab_panels) + 1]] <- tabPanel(
      "Erythrocyte gene expression",
      uiOutput("trajectory_states_percent_ery_UI")
    )
  }

  ##
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

##----------------------------------------------------------------------------##
## One UI/text/plot output triple per metric. The spec name is both the output
## ID infix (trajectory_states_<name>_*) and the plot cache key.
##----------------------------------------------------------------------------##

trajectory_expression_metric_specs <- list(
  nUMI = list(
    available = function() {
      "nUMI" %in% colnames(viewerProjectionFirstFrameMetadata())
    },
    column = function() "nUMI",
    missing = "Column with number of transcript per cell not available.",
    y_title = "Number of transcripts",
    mode = "integer"
  ),
  nGene = list(
    available = function() {
      "nGene" %in% colnames(viewerProjectionFirstFrameMetadata())
    },
    column = function() "nGene",
    missing = "Column with number of expressed genes per cell not available.",
    y_title = "Number of expressed genes",
    mode = "integer"
  ),
  percent_mt = list(
    available = function() hasMitoColumn(),
    column = function() getMitoColumn(),
    missing = "Column with percentage of mitochondrial expression not available.",
    y_title = "Percentage of transcripts",
    mode = "percent"
  ),
  percent_ribo = list(
    available = function() hasRiboColumn(),
    column = function() getRiboColumn(),
    missing = "Column with percentage of ribosomal expression not available.",
    y_title = "Percentage of transcripts",
    mode = "percent"
  ),
  percent_ery = list(
    available = function() hasEryColumn(),
    column = function() getEryColumn(),
    missing = "Column with percentage of erythrocyte/hemoglobin expression not available.",
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
)

for (trajectory_metric in names(trajectory_expression_metric_specs)) {
  local({
    metric <- trajectory_metric
    spec <- trajectory_expression_metric_specs[[metric]]
    output_id <- paste0("trajectory_states_", metric)

    output[[paste0(output_id, "_UI")]] <- renderUI({
      if (spec$available()) {
        plotly::plotlyOutput(paste0(output_id, "_plot"))
      } else {
        textOutput(paste0(output_id, "_text"))
      }
    })

    output[[paste0(output_id, "_text")]] <- renderText({
      spec$missing
    })

    output[[paste0(output_id, "_plot")]] <- plotly::renderPlotly({
      ##
      req(trajectory_selection_ok())

      column <- spec$column()
      req(column)

      ## collect trajectory data
      trajectory_data <- trajectory_data_reactive()[["meta"]]

      ##
      state_colors <- setNames(
        cerebro_group_colors(length(levels(trajectory_data$state))),
        levels(trajectory_data$state)
      )

      ##
      plotlyViolin(
        table = trajectory_cells_reactive(column),
        metric = column,
        coloring_variable = "state",
        colors = state_colors,
        y_title = spec$y_title,
        mode = spec$mode
      )
    }) %>%
      cachePlot(
        input[["trajectory_selected_method"]],
        input[["trajectory_selected_name"]],
        metric,
        available_crb_files$selected
      )
  })
}

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(
  input,
  "trajectory_expression_metrics_info",
  trajectory_expression_metrics_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_expression_metrics_info <- list(
  title = "Number of transcripts",
  text = HTML(
    "Compare transcripts (nUMI/nCounts), expressed genes (nGene/nFeature), and mitochondrial, ribosomal or hemoglobin transcript percentages by state."
  )
)
