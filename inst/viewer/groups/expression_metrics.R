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
      tabPanel(spec$title, cerebroChartOutput(spec$output))
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
outputOptions(output, "groups_expression_metrics_UI", priority = 10)

## One violin plot per metric; each renders only while its tab is shown, or
## while the browser prefetches the hidden tabs (below).
groups_metrics_prefetching <- FALSE
for (metric in names(groups_metric_specs)) {
  local({
    metric <- metric
    spec <- groups_metric_specs[[metric]]
    output[[spec$output]] <- renderCerebroChart({
      req(
        input[["groups_expression_metrics_render_request"]],
        groups_metrics_prefetching ||
          identical(input[["groups_expression_metrics_tabs"]], spec$title),
        groups_selected_group() %in% getGroups(),
        ## The tab input outlives a data set switch until the tabs are built
        ## again; a data set without this metric has no plot for it.
        metric %in% colnames(viewerProjectionFirstFrameMetadata())
      )
      cerebroViolinData(
        table = groupsMetricData(groups_selected_group(), metric),
        metric = metric,
        coloring_variable = groups_selected_group(),
        colors = reactive_group_colors(groups_selected_group()),
        y_title = spec$y_title,
        mode = spec$mode
      )
    }) %>%
      cachePlot(
        groups_selected_group(),
        metric,
        reactive_group_colors(groups_selected_group()),
        available_crb_files$selected
      )
  })
}

## After the shown metric is drawn, the browser asks while idle for the other
## tabs of the same group. Their outputs run once while hidden and the browser
## draws them when their tab is shown, without a server round trip.
observeEvent(input[["groups_expression_metrics_prefetch"]], {
  req(identical(
    input[["groups_expression_metrics_prefetch"]],
    groups_selected_group()
  ))
  outputs <- vapply(groups_metric_specs, `[[`, "", "output")
  groups_metrics_prefetching <<- TRUE
  for (id in outputs) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
  session$onFlushed(
    function() {
      groups_metrics_prefetching <<- FALSE
      for (id in outputs) {
        outputOptions(output, id, suspendWhenHidden = TRUE)
      }
    },
    once = TRUE
  )
})

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
