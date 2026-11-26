##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Table of selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

trajectory_selection_present <- viewerSelectionPresence(
  "trajectory_projection"
)

output[["trajectory_selected_cells_table_UI"]] <- renderUI({
  req(
    trajectory_selected_method(),
    trajectory_selected_name(),
    trajectory_selection_present()
  )

  cerebroSelectedCellsTableUI("trajectory")
})

##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##

output[["trajectory_details_selected_cells_table"]] <- viewerLazyRender(
  function() {
    DT::renderDataTable({
      ## The browser lists a selection it reported by row.
      req(!viewerClientSelection("trajectory_projection", "table"))
      ## Selection identity is sufficient; appearance controls are initialized
      ## lazily and must not prevent a selected-cell table from rendering.
      req(trajectory_selection_ok())

      selected_cells <- trajectory_projection_selected_cells()
      ## A selection by row is matched by position; the table then names the
      ## selected cells only. A selection by name needs every name.
      selected <- viewerSelectionRows(selected_cells)
      cells_df <- trajectory_cells_reactive(
        colnames(viewerProjectionFirstFrameMetadata()),
        barcodes = is.null(selected)
      )
      if (!is.null(selected)) {
        cells_df <- cells_df[
          viewerRowMask(selected, cells_df[["cell_index"]]),
          ,
          drop = FALSE
        ]
        cells_df[["cell_barcode"]] <- viewerCellBarcodesAt(
          cells_df[["cell_index"]]
        )
        selected_cells <- data.frame(
          selection_key = as.character(cells_df[["cell_barcode"]])
        )
      }

      ## The shared table expects the coordinates as its first two columns and
      ## drops them from the output; the trajectory's own DR_1/DR_2 stay in place.
      ## Selections match by barcode, falling back to "DR_1-DR_2" coordinates for
      ## older Plotly event payloads without a stable selection_key.
      cerebroSelectedCellsTable(
        cells_df = cbind(cells_df[, c("DR_1", "DR_2"), drop = FALSE], cells_df),
        empty_data = viewerProjectionFirstFrameMetadata(),
        selection = selected_cells,
        fallback_key = "identifier",
        number_formatting = input[[
          "trajectory_details_selected_cells_table_number_formatting"
        ]],
        color_highlighting = input[[
          "trajectory_details_selected_cells_table_color_highlighting"
        ]],
        download_file_name = "trajectory_details_of_selected_cells"
      )
    })
  }
)

##----------------------------------------------------------------------------##
## The trajectory's own columns for the table of selected cells the browser
## builds: one file of the cells' canonical rows (int32, 0-based), DR_1, DR_2,
## pseudotime (float32) and state codes (int32, -1 missing), written once per
## trajectory and process. The cells are those with a pseudotime, as in
## trajectory_cells_reactive().
##----------------------------------------------------------------------------##
trajectory_table_resource <- reactive({
  trajectory <- trajectory_data_reactive()
  meta <- trajectory[["meta"]]
  cell_index <- trajectory_row_index_reactive()
  keep <- !is.na(meta[["pseudotime"]])
  state <- meta[["state"]]
  levels <- if (is.factor(state)) {
    levels(state)
  } else {
    unique(as.character(state[!is.na(state)]))
  }
  codes <- match(as.character(state[keep]), levels) - 1L
  codes[is.na(codes)] <- -1L
  directory <- file.path(tempdir(), "cerebro-trajectory-table")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- file.path(
    directory,
    paste0(
      rlang::hash(list(
        viewerDatasetIdentity(),
        trajectory_selected_method(),
        trajectory_selected_name(),
        sum(keep)
      )),
      ".bin"
    )
  )
  if (!file.exists(file)) {
    staged <- paste0(file, ".part")
    con <- file(staged, "wb")
    writeBin(
      as.integer(cell_index[keep] - 1L),
      con,
      size = 4L,
      endian = "little"
    )
    for (column in c("DR_1", "DR_2", "pseudotime")) {
      writeBin(
        as.numeric(meta[[column]][keep]),
        con,
        size = 4L,
        endian = "little"
      )
    }
    writeBin(as.integer(codes), con, size = 4L, endian = "little")
    close(con)
    file.rename(staged, file)
  }
  list(
    url = paste0(
      viewerStaticResourcePrefix("trajectory-table", directory),
      "/",
      basename(file)
    ),
    cells = sum(keep),
    state_levels = I(as.character(levels))
  )
})

observeEvent(input[["trajectory_table_resource_request"]], {
  request <- input[["trajectory_table_resource_request"]]
  context <- viewer_loaded_dataset_context()
  req(viewerDatasetContextEqual(request$dataset_context, context))
  resource <- tryCatch(trajectory_table_resource(), error = function(e) NULL)
  session$sendCustomMessage(
    "trajectory_table_resource",
    c(list(dataset_context = context), resource %||% list(unavailable = TRUE))
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(
  input,
  "trajectory_details_selected_cells_table_info",
  cerebroSelectedCellsTableInfo()
)
