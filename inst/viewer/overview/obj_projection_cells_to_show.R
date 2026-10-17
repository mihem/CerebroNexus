##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
## With a Viewer Pack the browser fetches the full canonical projection from a
## static resource and hides a deterministic share of cells itself, so the "%
## of cells" slider no longer resamples and resends coordinates from R.
overview_projection_client_percentage <- reactive({
  pack <- viewerPackCurrent()
  is.list(pack) && isTRUE(pack$canonical_order)
})

overview_projection_cells_to_show <- reactive({
  percentage <- if (isTRUE(overview_projection_client_percentage())) {
    100
  } else {
    current_scatter_percentage()
  }
  viewerProjectionCellIndices(
    "overview_projection",
    metadata = viewerProjectionFirstFrameMetadata(),
    percentage = percentage
  )
})
