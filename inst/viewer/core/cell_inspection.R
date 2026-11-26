# Shared by every cell view, including sessions that never open Linked views.
viewerCellDetailRow <- function(metadata, cell) {
  if (!is.data.frame(metadata)) {
    return(NULL)
  }
  ids <- if ("cell_barcode" %in% colnames(metadata)) {
    metadata$cell_barcode
  } else {
    rownames(metadata)
  }
  index <- match(cell, ids)
  if (is.na(index)) {
    return(NULL)
  }
  metadata[index, , drop = FALSE]
}

## The clicked cell's metadata row. The browser sends the cell's index in its
## frame, which is the canonical row when the frame holds every cell: the row
## is taken from the first-frame metadata at that index once the Pack's name
## at that row is the cell's name (one name, not a match over every name).
## Otherwise the cell is looked up by name, as before.
viewerCellDetailRowAt <- function(client_index, cell) {
  row <- suppressWarnings(as.integer(client_index)) + 1L
  at_row <- tryCatch(
    {
      frame <- viewerProjectionFirstFrameMetadata()
      if (
        length(row) == 1L &&
          !is.na(row) &&
          is.data.frame(frame) &&
          row >= 1L &&
          row <= nrow(frame) &&
          identical(as.character(viewerCellBarcodesAt(row)), cell)
      ) {
        frame[row, , drop = FALSE]
      }
    },
    error = function(error) NULL
  )
  if (is.null(at_row)) {
    viewerCellDetailRow(getMetaData(), cell)
  } else {
    at_row
  }
}

viewerCellDetailValue <- function(value) {
  if (length(value) != 1L || is.na(value)) {
    return("NA")
  }
  if (is.numeric(value)) {
    return(format(value, digits = 15, scientific = FALSE, trim = TRUE))
  }
  as.character(value)
}

observeEvent(input[["coordviews_cell_detail"]], {
  request <- input[["coordviews_cell_detail"]]
  dataset_context <- viewer_loaded_dataset_context()
  req(
    is.list(request),
    viewerDatasetContextEqual(request$dataset_context, dataset_context)
  )
  cell <- as.character(request$cell)
  req(length(cell) == 1L, !is.na(cell), nzchar(cell))
  metadata <- tryCatch(
    viewerCellDetailRowAt(request$client_index, cell),
    error = function(e) NULL
  )
  response <- list(
    dataset_context = dataset_context,
    cell = cell,
    client_index = request$client_index,
    view_id = request$view_id,
    request_id = request$request_id
  )
  if (is.null(metadata)) {
    response$error <- "Cell metadata unavailable."
  } else {
    response$rows <- lapply(
      setdiff(colnames(metadata), "cell_barcode"),
      function(column) {
        list(k = column, v = viewerCellDetailValue(metadata[[column]][1L]))
      }
    )
  }
  session$sendCustomMessage("coordviews_cell_meta", response)
})
