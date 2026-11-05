##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##
## The scatter controls Overview and Gene expression share.
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/overview/scatter_parameters.R"
  ),
  local = TRUE
)
files_to_load <- list.files(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/gene_expression"),
  pattern = "func_|obj_|UI_|out_|event_",
  full.names = TRUE
)

for (i in files_to_load) {
  source(i, local = TRUE)
}

## A data set without projections has nothing to draw expression on: say so,
## so the page settles instead of waiting for a plot.
observe({
  req(data_set())
  if (length(availableProjections())) {
    return()
  }
  session$sendCustomMessage(
    "cerebro_view_terminal",
    list(
      id = "expression_projection",
      kind = "empty",
      text = paste(
        "This data set has no dimensional reductions, so expression",
        "cannot be shown on a projection."
      ),
      dataset_context = viewer_loaded_dataset_context()
    )
  )
})
