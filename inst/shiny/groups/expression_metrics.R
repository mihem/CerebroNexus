##----------------------------------------------------------------------------##
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
output[["groups_expression_metrics_UI"]] <- renderUI({
  ## build list of tab panels dynamically based on available data
  tab_panels <- list()

  ## always show nUMI and nGene tabs
  tab_panels[[length(tab_panels) + 1]] <- tabPanel(
    "Number of transcripts",
    uiOutput("groups_nUMI_UI")
  )
  tab_panels[[length(tab_panels) + 1]] <- tabPanel(
    "Number of expressed genes",
    uiOutput("groups_nGene_UI")
  )

  ## conditionally add mito tab
  if (hasMitoColumn()) {
    tab_panels[[length(tab_panels) + 1]] <- tabPanel(
      "Mitochondrial gene expression",
      uiOutput("groups_percent_mt_UI")
    )
  }

  ## conditionally add ribo tab
  if (hasRiboColumn()) {
    tab_panels[[length(tab_panels) + 1]] <- tabPanel(
      "Ribosomal gene expression",
      uiOutput("groups_percent_ribo_UI")
    )
  }

  ## conditionally add ery tab
  if (hasEryColumn()) {
    tab_panels[[length(tab_panels) + 1]] <- tabPanel(
      "Erythrocyte gene expression",
      uiOutput("groups_percent_ery_UI")
    )
  }

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Expression metrics"),
        cerebroInfoButton("groups_expression_metrics_info")
      ),
      do.call(
        tabBox,
        c(
          list(title = NULL, width = 12, id = "groups_expression_metrics_tabs"),
          tab_panels
        )
      )
    )
  )
})

##----------------------------------------------------------------------------##
## Number of transcripts.
##----------------------------------------------------------------------------##
output[["groups_nUMI_UI"]] <- renderUI({
  if ( "nUMI" %in% colnames(getMetaData()) ) {
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
  withProgress(message = 'Generating nUMI plot...', value = 0.5, {
    plotlyViolin(
      table = getMetaData(),
      metric = "nUMI",
      coloring_variable = input[["groups_selected_group"]],
      colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
      y_title = "Number of transcripts",
      mode = "integer"
    )
  })
})

##----------------------------------------------------------------------------##
## Number of expressed genes.
##----------------------------------------------------------------------------##
output[["groups_nGene_UI"]] <- renderUI({
  if ( "nGene" %in% colnames(getMetaData()) ) {
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
  withProgress(message = 'Generating nGene plot...', value = 0.5, {
    plotlyViolin(
      table = getMetaData(),
      metric = "nGene",
      coloring_variable = input[["groups_selected_group"]],
      colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
      y_title = "Number of expressed genes",
      mode = "integer"
    )
  })
})

##----------------------------------------------------------------------------##
## Expression from mitochondrial genes.
##----------------------------------------------------------------------------##
output[["groups_percent_mt_UI"]] <- renderUI({
  if ( hasMitoColumn() ) {
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
  mito_col <- getMitoColumn()
  req(mito_col)
  plotlyViolin(
    table = getMetaData(),
    metric = mito_col,
    coloring_variable = input[["groups_selected_group"]],
    colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
    y_title = "Percentage of transcripts",
    mode = "percent"
  )
})

##----------------------------------------------------------------------------##
## Expression from ribosomal genes.
##----------------------------------------------------------------------------##
output[["groups_percent_ribo_UI"]] <- renderUI({
  if ( hasRiboColumn() ) {
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
  ribo_col <- getRiboColumn()
  req(ribo_col)
  withProgress(message = 'Generating percent_ribo plot...', value = 0.5, {
    plotlyViolin(
      table = getMetaData(),
      metric = ribo_col,
      coloring_variable = input[["groups_selected_group"]],
      colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
      y_title = "Percentage of transcripts",
      mode = "percent"
    )
  })
})

##----------------------------------------------------------------------------#### Expression from erythrocyte/hemoglobin genes.
##----------------------------------------------------------------------------##
output[["groups_percent_ery_UI"]] <- renderUI({
  if ( hasEryColumn() ) {
    plotly::plotlyOutput("groups_percent_ery_plot")
  } else {
    textOutput("groups_percent_ery_text")
  }
})

output[["groups_percent_ery_text"]] <- renderText({
  "Column with percentage of erythrocyte/hemoglobin expression not available."
})

output[["groups_percent_ery_plot"]] <- plotly::renderPlotly({
  req(input[["groups_selected_group"]] %in% getGroups())
  ery_col <- getEryColumn()
  req(ery_col)
  withProgress(message = 'Generating percent_ery plot...', value = 0.5, {
    plotlyViolin(
      table = getMetaData(),
      metric = ery_col,
      coloring_variable = input[["groups_selected_group"]],
      colors = reactive_colors()[[ input[["groups_selected_group"]] ]],
      y_title = "Percentage of transcripts",
      mode = "percent"
    )
  })
})

##----------------------------------------------------------------------------#### Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["groups_expression_metrics_info"]], {
  showModal(
    modalDialog(
      groups_expression_metrics_info[["text"]],
      title = groups_expression_metrics_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
groups_expression_metrics_info <- list(
  title = "Number of transcripts",
  text = HTML("Violin plots showing the number of transcripts (nUMI/nCounts), the number of expressed genes (nGene/nFeature), as well as the percentage of transcripts coming from mitochondrial, ribosomal, and erythrocyte/hemoglobin genes in each group.")
)
