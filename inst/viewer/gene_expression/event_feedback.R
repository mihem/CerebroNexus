## Presentation only: map raw controls to the existing canonical render key.
## Use the non-debounced selector value so a newer input cannot be labelled with
## an older debounced frame. No expression, geometry or metadata is requested.
observe({
  req(identical(input[["sidebar"]], "geneExpression"))
  context <- viewer_loaded_dataset_context()
  genes <- expression_selected_genes_input()
  mode <- input[["expression_projection_genes_in_separate_panels"]]
  rgb_layout <- input[["expression_rgb_layout"]]
  req(mode)
  controls <- c(
    "expression_analysis_mode",
    "expression_genes_input",
    "expression_projection_genes_in_separate_panels",
    "expression_rgb_gene_r",
    "expression_rgb_gene_g",
    "expression_rgb_gene_b",
    "expression_select_gene_set",
    "expression_rgb_layout"
  )
  values <- isolate(stats::setNames(
    lapply(controls, function(id) {
      as.character(input[[id]] %||% character())
    }),
    controls
  ))
  session$sendCustomMessage(
    "cerebro_gene_feedback",
    list(
      dataset_context = context,
      controls = values,
      render_key = expressionProjectionRenderKey(genes, mode, rgb_layout)
    )
  )
})
