source(cerebro_root_path("viewer/load_data/overview_ui.R"), local = TRUE)

output$load_data_overview <- renderUI({
  viewerOverviewUI(current_dataset_info())
})

output$load_data_annotation_plot <- renderUI({
  annotations <- current_dataset_info()$overview$annotations
  if (!length(annotations)) {
    return(NULL)
  }
  selected <- input$load_data_annotation
  if (is.null(selected) || !selected %in% names(annotations)) {
    selected <- names(annotations)[1L]
  }
  viewerOverviewBars(annotations[[selected]])
})

for (destination in c(
  "overview",
  "geneExpression",
  "groups",
  "spatial",
  "trajectory",
  "immune_repertoire",
  "markerGenes",
  "enrichedPathways",
  "extra_material"
)) {
  local({
    target <- destination
    observeEvent(
      input[[paste0("dataset_open_", target)]],
      {
        updateTabItems(session, "sidebar", selected = target)
      },
      ignoreInit = TRUE
    )
  })
}
