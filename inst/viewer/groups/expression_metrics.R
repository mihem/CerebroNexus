##----------------------------------------------------------------------------##
## Expression metrics:
## - number of transcripts
## - number of expressed genes
## - percent of transcripts from mitochondrial genes
## - percent of transcripts from ribosomal genes
##----------------------------------------------------------------------------##

groupsMetricData <- function(group, metric) {
  packed <- viewerPackGroupMetric(viewerPackCurrent(), group, metric)
  if (!is.null(packed)) packed else groupsMetadataColumns(c(group, metric))
}

groups_metric_specs <- list(
  nUMI = list(
    title = "Number of transcripts",
    output = "groups_nUMI_plot",
    y_title = "Number of transcripts",
    mode = "integer"
  ),
  nGene = list(
    title = "Number of expressed genes",
    output = "groups_nGene_plot",
    y_title = "Number of expressed genes",
    mode = "integer"
  ),
  percent_mt = list(
    title = "Mitochondrial gene expression",
    output = "groups_percent_mt_plot",
    y_title = "Percentage of transcripts",
    mode = "percent"
  ),
  percent_ribo = list(
    title = "Ribosomal gene expression",
    output = "groups_percent_ribo_plot",
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
)

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["groups_expression_metrics_UI"]] <- renderUI({
  req(input[["groups_expression_metrics_render_request"]])
  available <- intersect(
    names(groups_metric_specs),
    colnames(viewerProjectionFirstFrameMetadata())
  )
  content <- if (!length(available)) {
    p("Expression metric columns are not available for this data set.")
  } else {
    tabs <- lapply(available, function(metric) {
      spec <- groups_metric_specs[[metric]]
      tabPanel(spec$title, plotly::plotlyOutput(spec$output))
    })
    do.call(
      tabBox,
      c(
        list(title = NULL, width = 12, id = "groups_expression_metrics_tabs"),
        tabs
      )
    )
  }
  content
})

## One violin plot per metric; each renders only while its tab is shown.
for (metric in names(groups_metric_specs)) {
  local({
    metric <- metric
    spec <- groups_metric_specs[[metric]]
    output[[spec$output]] <- plotly::renderPlotly({
      req(
        input[["groups_expression_metrics_render_request"]],
        identical(input[["groups_expression_metrics_tabs"]], spec$title),
        input[["groups_selected_group"]] %in% getGroups()
      )
      plotlyViolin(
        table = groupsMetricData(input[["groups_selected_group"]], metric),
        metric = metric,
        coloring_variable = input[["groups_selected_group"]],
        colors = reactive_group_colors(input[["groups_selected_group"]]),
        y_title = spec$y_title,
        mode = spec$mode
      )
    }) %>%
      cachePlot(
        input[["groups_selected_group"]],
        metric,
        reactive_group_colors(input[["groups_selected_group"]]),
        available_crb_files$selected
      )
  })
}

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "groups_expression_metrics_info",
  groups_expression_metrics_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
groups_expression_metrics_info <- list(
  title = "Number of transcripts",
  text = HTML(
    "Compare transcripts (nUMI/nCounts), expressed genes (nGene/nFeature), and mitochondrial or ribosomal transcript percentages by group."
  )
)
