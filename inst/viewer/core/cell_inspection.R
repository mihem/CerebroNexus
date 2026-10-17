# Shared by every cell view, including sessions that never open Linked views.
viewerCellDetailRow <- function(metadata, cell) {
  if (!is.data.frame(metadata)) return(NULL)
  ids <- if ("cell_barcode" %in% colnames(metadata)) metadata$cell_barcode else rownames(metadata)
  index <- match(cell, ids)
  if (is.na(index)) return(NULL)
  metadata[index, , drop = FALSE]
}

viewerCellDetailValue <- function(value) {
  if (length(value) != 1L || is.na(value)) return("NA")
  if (is.numeric(value)) {
    return(format(value, digits = 15, scientific = FALSE, trim = TRUE))
  }
  as.character(value)
}

observeEvent(input[["coordviews_cell_detail"]], {
  request <- input[["coordviews_cell_detail"]]
  dataset_context <- viewer_loaded_dataset_context()
  req(is.list(request), viewerDatasetContextEqual(request$dataset_context, dataset_context))
  cell <- as.character(request$cell)
  req(length(cell) == 1L, !is.na(cell), nzchar(cell))
  metadata <- tryCatch(viewerCellDetailRow(getMetaData(), cell), error = function(e) NULL)
  response <- list(
    dataset_context = dataset_context, cell = cell,
    client_index = request$client_index, view_id = request$view_id,
    request_id = request$request_id
  )
  if (is.null(metadata)) {
    response$error <- "Cell metadata unavailable."
  } else {
    response$rows <- lapply(setdiff(colnames(metadata), "cell_barcode"), function(column) {
      list(k = column, v = viewerCellDetailValue(metadata[[column]][1L]))
    })
  }
  session$sendCustomMessage("coordviews_cell_meta", response)
})
