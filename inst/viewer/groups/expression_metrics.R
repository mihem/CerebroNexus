##----------------------------------------------------------------------------##
## Expression metrics:
## - number of transcripts
## - number of expressed genes
## - percent of transcripts from mitochondrial genes
## - percent of transcripts from ribosomal genes
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["groups_expression_metrics_UI"]] <- renderUI({
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression metrics"),
        cerebroInfoButton("groups_expression_metrics_info")
      ),
      tabBox(
        title = NULL,
        width = 12,
        id = "groups_expression_metrics_tabs",
        tabPanel(
          "Number of transcripts",
          uiOutput("groups_nUMI_UI")
        ),
        tabPanel(
          "Number of expressed genes",
          uiOutput("groups_nGene_UI")
        ),
        tabPanel(
          "Mitochondrial gene expression",
          uiOutput("groups_percent_mt_UI")
        ),
        tabPanel(
          "Ribosomal gene expression",
          uiOutput("groups_percent_ribo_UI")
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Number of transcripts.
##----------------------------------------------------------------------------##
output[["groups_nUMI_UI"]] <- renderUI({
  if ("nUMI" %in% colnames(getMetaData())) {
    plotly::plotlyOutput("groups_nUMI_plot")
  } else {
    textOutput("groups_nUMI_text")
  }
})

output[["groups_nUMI_text"]] <- renderText({
  "Column with number of transcript per cell not available."
})

output[["groups_nUMI_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]] %in% getGroups())
  plotlyViolin(
    table = getMetaData(),
    metric = "nUMI",
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_colors()[[input[["groups_selected_group"]]]],
    y_title = "Number of transcripts",
    mode = "integer"
  )
}) %>%
  cachePlot(
    input[["groups_selected_group"]],
    "nUMI",
    available_crb_files$selected
  )

##----------------------------------------------------------------------------##
## Number of expressed genes.
##----------------------------------------------------------------------------##
output[["groups_nGene_UI"]] <- renderUI({
  if ("nGene" %in% colnames(getMetaData())) {
    plotly::plotlyOutput("groups_nGene_plot")
  } else {
    textOutput("groups_nGene_text")
  }
})

output[["groups_nGene_text"]] <- renderText({
  "Column with number of expressed genes per cell not available."
})

output[["groups_nGene_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]] %in% getGroups())
  plotlyViolin(
    table = getMetaData(),
    metric = "nGene",
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_colors()[[input[["groups_selected_group"]]]],
    y_title = "Number of expressed genes",
    mode = "integer"
  )
}) %>%
  cachePlot(
    input[["groups_selected_group"]],
    "nGene",
    available_crb_files$selected
  )

##----------------------------------------------------------------------------##
## Expression from mitochondrial genes.
##----------------------------------------------------------------------------##
output[["groups_percent_mt_UI"]] <- renderUI({
  if ("percent_mt" %in% colnames(getMetaData())) {
    plotly::plotlyOutput("groups_percent_mt_plot")
  } else {
    textOutput("groups_percent_mt_text")
  }
})

output[["groups_percent_mt_text"]] <- renderText({
  "Column with percentage of mitochondrial expression not available."
})

output[["groups_percent_mt_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]] %in% getGroups())
  plotlyViolin(
    table = getMetaData(),
    metric = "percent_mt",
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_colors()[[input[["groups_selected_group"]]]],
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
}) %>%
  cachePlot(
    input[["groups_selected_group"]],
    "percent_mt",
    available_crb_files$selected
  )

##----------------------------------------------------------------------------##
## Expression from ribosomal genes.
##----------------------------------------------------------------------------##
output[["groups_percent_ribo_UI"]] <- renderUI({
  if ("percent_ribo" %in% colnames(getMetaData())) {
    plotly::plotlyOutput("groups_percent_ribo_plot")
  } else {
    textOutput("groups_percent_ribo_text")
  }
})

output[["groups_percent_ribo_text"]] <- renderText({
  "Column with percentage of ribosomal expression not available."
})

output[["groups_percent_ribo_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]] %in% getGroups())
  plotlyViolin(
    table = getMetaData(),
    metric = "percent_ribo",
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_colors()[[input[["groups_selected_group"]]]],
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
}) %>%
  cachePlot(
    input[["groups_selected_group"]],
    "percent_ribo",
    available_crb_files$selected
  )

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["groups_expression_metrics_info"]], {
  showCerebroInfoModal(groups_expression_metrics_info)
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
groups_expression_metrics_info <- list(
  title = "Number of transcripts",
  text = HTML(
    "Violin plots showing the number of transcripts (nUMI/nCounts), the number of expressed genes (nGene/nFeature), as well as the percentage of transcripts coming from mitochondrial and ribosomal genes in each group."
  )
)
