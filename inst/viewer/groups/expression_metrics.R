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
    output = "groups_nUMI_plot"
  ),
  nGene = list(
    title = "Number of expressed genes",
    output = "groups_nGene_plot"
  ),
  percent_mt = list(
    title = "Mitochondrial gene expression",
    output = "groups_percent_mt_plot"
  ),
  percent_ribo = list(
    title = "Ribosomal gene expression",
    output = "groups_percent_ribo_plot"
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

output[["groups_nUMI_plot"]] <- plotly::renderPlotly({
  req(
    input[["groups_expression_metrics_render_request"]],
    identical(
      input[["groups_expression_metrics_tabs"]],
      "Number of transcripts"
    ),
    input[["groups_selected_group"]] %in% getGroups()
  )
  plotlyViolin(
    table = groupsMetricData(input[["groups_selected_group"]], "nUMI"),
    metric = "nUMI",
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_group_colors(input[["groups_selected_group"]]),
    y_title = "Number of transcripts",
    mode = "integer"
  )
}) %>%
  cachePlot(
    input[["groups_selected_group"]],
    "nUMI",
    reactive_group_colors(input[["groups_selected_group"]]),
    available_crb_files$selected
  )

output[["groups_nGene_plot"]] <- plotly::renderPlotly({
  req(
    input[["groups_expression_metrics_render_request"]],
    identical(
      input[["groups_expression_metrics_tabs"]],
      "Number of expressed genes"
    ),
    input[["groups_selected_group"]] %in% getGroups()
  )
  plotlyViolin(
    table = groupsMetricData(input[["groups_selected_group"]], "nGene"),
    metric = "nGene",
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_group_colors(input[["groups_selected_group"]]),
    y_title = "Number of expressed genes",
    mode = "integer"
  )
}) %>%
  cachePlot(
    input[["groups_selected_group"]],
    "nGene",
    reactive_group_colors(input[["groups_selected_group"]]),
    available_crb_files$selected
  )

output[["groups_percent_mt_plot"]] <- plotly::renderPlotly({
  req(
    input[["groups_expression_metrics_render_request"]],
    identical(
      input[["groups_expression_metrics_tabs"]],
      "Mitochondrial gene expression"
    ),
    input[["groups_selected_group"]] %in% getGroups()
  )
  plotlyViolin(
    table = groupsMetricData(input[["groups_selected_group"]], "percent_mt"),
    metric = "percent_mt",
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_group_colors(input[["groups_selected_group"]]),
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
}) %>%
  cachePlot(
    input[["groups_selected_group"]],
    "percent_mt",
    reactive_group_colors(input[["groups_selected_group"]]),
    available_crb_files$selected
  )

output[["groups_percent_ribo_plot"]] <- plotly::renderPlotly({
  req(
    input[["groups_expression_metrics_render_request"]],
    identical(
      input[["groups_expression_metrics_tabs"]],
      "Ribosomal gene expression"
    ),
    input[["groups_selected_group"]] %in% getGroups()
  )
  plotlyViolin(
    table = groupsMetricData(input[["groups_selected_group"]], "percent_ribo"),
    metric = "percent_ribo",
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_group_colors(input[["groups_selected_group"]]),
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
}) %>%
  cachePlot(
    input[["groups_selected_group"]],
    "percent_ribo",
    reactive_group_colors(input[["groups_selected_group"]]),
    available_crb_files$selected
  )

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
    "Violin plots showing the number of transcripts (nUMI/nCounts), the number of expressed genes (nGene/nFeature), as well as the percentage of transcripts coming from mitochondrial and ribosomal genes in each group."
  )
)
