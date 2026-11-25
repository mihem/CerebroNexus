##----------------------------------------------------------------------------##
## Expression levels of cells in projection.
##
## Cache only exact full-canonical, single-gene rows. Keeping this cache outside
## the reactive graph avoids stale bindCache() hits while still making revisits
## cheap. Dataset identity and a small LRU bound prevent cross-dataset reuse and
## unbounded million-value vectors.
##----------------------------------------------------------------------------##
expression_projection_row_cache <- new.env(parent = emptyenv())
expression_projection_row_cache_order <- character()
expression_projection_row_cache_limit <- 4L
## The non-zero cells of a cached row read from an H5 sidecar, under the same
## key and evicted with it (see expressionProjectionRowEntries()). The colour
## packet is built from them instead of scanning the dense row again.
expression_projection_row_entries <- new.env(parent = emptyenv())
expression_projection_progress_handle <- NULL
expression_projection_progress_key <- NULL

expressionProjectionProgressClose <- function(key = NULL) {
  if (
    !is.null(key) &&
      !identical(as.character(key), expression_projection_progress_key)
  ) {
    return(invisible(FALSE))
  }
  if (!is.null(expression_projection_progress_handle)) {
    try(expression_projection_progress_handle$close(), silent = TRUE)
  }
  expression_projection_progress_handle <<- NULL
  expression_projection_progress_key <<- NULL
  invisible(TRUE)
}

expressionProjectionProgressStart <- function(key) {
  expressionProjectionProgressClose()
  progress_session <- if (exists("session", inherits = TRUE)) {
    get("session", inherits = TRUE)
  } else {
    shiny::getDefaultReactiveDomain()
  }
  if (
    is.null(progress_session) ||
      is.null(tryCatch(
        progress_session$progressStack,
        error = function(error) NULL
      ))
  ) {
    return(invisible(NULL))
  }
  expression_projection_progress_handle <<- shiny::Progress$new(
    progress_session,
    min = 0,
    max = 1
  )
  expression_projection_progress_key <<- as.character(key)
  expression_projection_progress_handle$set(
    message = "Loading gene expression...",
    detail = "Reading expression values...",
    value = 0.15
  )
  invisible(NULL)
}

expressionProjectionProgressUpdate <- function(value, detail) {
  if (is.null(expression_projection_progress_handle)) {
    return(invisible(NULL))
  }
  expression_projection_progress_handle$set(value = value, detail = detail)
  invisible(NULL)
}

expressionProjectionDatasetKey <- function() {
  identity <- tryCatch(viewerDatasetIdentity(), error = function(error) NULL)
  selected <- tryCatch(
    as.character(available_crb_files$selected),
    error = function(error) ""
  )
  key <- as.character(identity$pack_fingerprint %||% "")
  if (length(key) != 1L || is.na(key) || !nzchar(key)) {
    key <- selected
  }
  if (length(key) != 1L || is.na(key) || !nzchar(key)) {
    key <- as.character(identity$fingerprint %||% "")
  }
  if (length(key) == 1L && !is.na(key) && nzchar(key)) key else NULL
}

expressionProjectionFullCellSelection <- function(data, cells) {
  n_cells <- viewerExpressionCellCount(data)
  length(n_cells) == 1L &&
    !is.na(n_cells) &&
    length(cells) == n_cells &&
    identical(as.integer(cells), seq_len(n_cells))
}

expressionProjectionRowCacheGet <- function(key) {
  if (
    !is.null(key) &&
      exists(key, envir = expression_projection_row_cache, inherits = FALSE)
  ) {
    expression_projection_row_cache_order <<- c(
      setdiff(expression_projection_row_cache_order, key),
      key
    )
    return(get(key, envir = expression_projection_row_cache, inherits = FALSE))
  }
  NULL
}

expressionProjectionRowEntriesDrop <- function(key) {
  if (
    exists(key, envir = expression_projection_row_entries, inherits = FALSE)
  ) {
    rm(list = key, envir = expression_projection_row_entries)
  }
}

expressionProjectionRowCacheSet <- function(key, value, entries = NULL) {
  if (!is.null(key)) {
    assign(key, value, envir = expression_projection_row_cache)
    if (is.null(entries)) {
      expressionProjectionRowEntriesDrop(key)
    } else {
      assign(key, entries, envir = expression_projection_row_entries)
    }
    expression_projection_row_cache_order <<- c(
      setdiff(expression_projection_row_cache_order, key),
      key
    )
    while (
      length(expression_projection_row_cache_order) >
        expression_projection_row_cache_limit
    ) {
      evict <- expression_projection_row_cache_order[[1L]]
      expression_projection_row_cache_order <<-
        expression_projection_row_cache_order[-1L]
      rm(list = evict, envir = expression_projection_row_cache)
      expressionProjectionRowEntriesDrop(evict)
    }
  }
  invisible(value)
}

expressionProjectionCachedRows <- function(data, cells, genes) {
  genes <- unique(as.character(genes))
  dataset_key <- expressionProjectionDatasetKey()
  cacheable <- !is.null(dataset_key) &&
    expressionProjectionFullCellSelection(data, cells)
  keys <- stats::setNames(
    lapply(genes, function(gene) {
      if (cacheable) paste(dataset_key, gene, sep = "::") else NULL
    }),
    genes
  )
  values <- stats::setNames(
    lapply(keys, expressionProjectionRowCacheGet),
    genes
  )
  missing <- genes[vapply(values, is.null, logical(1))]
  if (length(missing) && cacheable && is.numeric(cells)) {
    read <- expressionProjectionH5Rows(data, missing)
    for (gene in names(read)) {
      values[[gene]] <- read[[gene]]$value
      expressionProjectionRowCacheSet(
        keys[[gene]],
        read[[gene]]$value,
        read[[gene]]$entries
      )
    }
    missing <- setdiff(missing, names(read))
  }
  if (length(missing)) {
    fetched <- viewerExpressionValues(data, cells, missing)
    for (gene in intersect(missing, names(fetched))) {
      value <- unname(fetched[[gene]])
      values[[gene]] <- value
      expressionProjectionRowCacheSet(keys[[gene]], value)
    }
  }
  values[!vapply(values, is.null, logical(1))]
}

## Full rows of genes from an H5 sidecar, which holds each gene as its
## non-zero cells: list(value = the dense row, entries) per gene, the dense row
## being what viewerExpressionValues() returns for every cell. NULL without a
## sidecar or when it lacks one of the genes (the caller then reads them as
## before).
expressionProjectionH5Rows <- function(data, genes) {
  rows <- attr(data, "cerebro_h5_rows", exact = TRUE)
  if (!is.list(rows) || !is.function(rows$each_sparse) || !length(genes)) {
    return(NULL)
  }
  n_cells <- rows$n_cells()
  read <- stats::setNames(vector("list", length(genes)), genes)
  found <- rows$each_sparse(genes, function(i, entries) {
    ## As .h5ExpressionRows()$row() fills it.
    value <- numeric(n_cells)
    value[entries$index] <- entries$value
    read[[i]] <<- list(
      value = value,
      entries = expressionProjectionRowEntries(entries, n_cells)
    )
  })
  if (isTRUE(found)) read
}

## The cells expressionSparseColor() would find in the dense row filled from
## `entries` (1-based `index`, `value`), and their values: list(length,
## count, index = 0-based integer, color). Explicit zeros in the file are
## dropped, as the dense scan does not see them. `index` and `color` are kept
## only below the sparse packet's density (the packet is dense above it).
## NULL for entries that are not strictly increasing cells of the row, whose
## dense row the scan must read instead.
expressionProjectionRowEntries <- function(
  entries,
  n_cells,
  max_density = 0.5
) {
  index <- entries$index
  value <- as.double(entries$value)
  if (
    length(index) != length(value) ||
      !isFALSE(is.unsorted(index, strictly = TRUE)) ||
      (length(index) && (index[[1L]] < 1 || index[[length(index)]] > n_cells))
  ) {
    return(NULL)
  }
  keep <- is.na(value) | value != 0
  if (!all(keep)) {
    index <- index[keep]
    value <- value[keep]
  }
  count <- length(index)
  if (count >= n_cells * max_density) {
    return(list(length = n_cells, count = count))
  }
  list(
    length = n_cells,
    count = count,
    index = as.integer(index - 1),
    color = value
  )
}

## The entries kept with `gene`'s cached row, when `values` is that row (the
## very vector in the normal path, so identical() returns at once).
expressionProjectionCachedEntries <- function(values, gene) {
  dataset_key <- expressionProjectionDatasetKey()
  gene <- as.character(gene)
  if (is.null(dataset_key) || length(gene) != 1L || is.na(gene)) {
    return(NULL)
  }
  key <- paste(dataset_key, gene, sep = "::")
  entries <- get0(
    key,
    envir = expression_projection_row_entries,
    inherits = FALSE
  )
  if (
    is.null(entries) ||
      !identical(
        values,
        get0(key, envir = expression_projection_row_cache, inherits = FALSE),
        num.eq = FALSE
      )
  ) {
    return(NULL)
  }
  entries
}

expressionProjectionCachedRow <- function(data, cells, gene) {
  value <- expressionProjectionCachedRows(data, cells, gene)[[gene]]
  value
}

expressionProjectionGeneMeans <- function(data, genes) {
  genes <- unique(as.character(genes))
  dataset_key <- expressionProjectionDatasetKey()
  rows <- stats::setNames(
    lapply(genes, function(gene) {
      expressionProjectionRowCacheGet(
        if (!is.null(dataset_key)) {
          paste(dataset_key, gene, sep = "::")
        } else {
          NULL
        }
      )
    }),
    genes
  )
  # Never densify an uncached gene set just to compute its means. The native
  # backend reduction remains the fallback for subsets and larger gene sets.
  if (any(vapply(rows, is.null, logical(1)))) {
    return(data$getMeanExpressionForGenes(genes))
  }
  data.frame(
    gene = names(rows),
    expression = unname(vapply(rows, mean, numeric(1))),
    stringsAsFactors = FALSE
  )
}

expressionProjectionRenderKey <- function(
  genes,
  display_mode,
  rgb_layout = "composite"
) {
  present <- as.character(genes[["genes_to_display_present"]] %||% character())
  rgb <- genes[["rgb_genes"]] %||% list()
  rgb <- vapply(
    c("r", "g", "b"),
    function(channel) {
      value <- as.character(rgb[[channel]] %||% "")
      if (length(value) && !is.na(value[[1L]])) value[[1L]] else ""
    },
    character(1)
  )
  key <- paste(
    as.character(display_mode %||% "single"),
    paste(present, collapse = "\034"),
    paste(rgb, collapse = "\034"),
    sep = "\035"
  )
  if (identical(display_mode, "rgb")) {
    key <- paste(
      key,
      if (identical(rgb_layout, "separate")) "separate" else "composite",
      sep = "\035"
    )
  }
  key
}

expression_projection_render_key <- reactive({
  expressionProjectionRenderKey(
    expression_selected_genes(),
    expression_projection_display_mode(),
    input[["expression_rgb_layout"]]
  )
})

expressionProjectionPaintedCurrent <- function() {
  painted <- input[["expression_projection_rendered_state"]]
  is.list(painted) &&
    viewerDatasetContextEqual(
      painted$dataset_context,
      viewer_loaded_dataset_context()
    ) &&
    identical(painted$render_key, expression_projection_render_key())
}

## Every paint republishes the rendered state as an event, including repaints
## of an unchanged key. Summaries, selected-cell outputs and their million-value
## reads must follow readiness changes only.
expression_projection_summary_ready_state <- reactiveVal(FALSE)
observe(
  {
    ## Not ready while its inputs are unavailable (e.g. before a dataset loads).
    ready <- isTRUE(tryCatch(
      expressionProjectionPaintedCurrent(),
      error = function(error) FALSE
    ))
    viewerPublishChanged(expression_projection_summary_ready_state, ready)
  },
  priority = 100
)
expression_projection_summary_ready <- reactive({
  expression_projection_summary_ready_state()
})

observeEvent(
  input[["expression_projection_rendered_state"]],
  {
    if (expressionProjectionPaintedCurrent()) {
      expressionProjectionProgressClose(expression_projection_render_key())
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["sidebar"]],
  {
    if (!identical(input[["sidebar"]], "geneExpression")) {
      expressionProjectionProgressClose()
    }
  },
  ignoreInit = FALSE
)

if (
  exists("session", inherits = TRUE) &&
    is.function(tryCatch(session$onSessionEnded, error = function(error) NULL))
) {
  session$onSessionEnded(function() expressionProjectionProgressClose())
}

## Prime the H5 row-access path only after the empty UMAP has painted.
## This keeps the fast page-open path intact while moving one-time method and
## backend initialization out of the user's first gene selection.
expression_projection_backend_warmed <- reactiveVal(FALSE)
expression_projection_warm_retry <- reactiveVal(0L)
observeEvent(
  list(
    input[["expression_projection_rendered_state"]],
    expression_projection_warm_retry()
  ),
  {
    req(expressionProjectionPaintedCurrent())
    if (isTRUE(expression_projection_backend_warmed())) {
      return()
    }
    genes <- expression_selected_genes()[["genes_to_display_present"]]
    if (length(genes)) {
      return()
    }
    ## A thin CRB's cell names are still being checked in a daemon: look again
    ## later instead of waiting for it on the session's R thread.
    pending <- attr(data_set(), "cerebro_h5_cells_pending", exact = TRUE)
    if (is.function(pending) && isTRUE(pending())) {
      later::later(
        function() {
          ## later() callbacks outlive a closed session. Reading its reactives
          ## then raises outside any session and stops the whole app.
          if (
            isTRUE(tryCatch(session$isClosed(), error = function(error) FALSE))
          ) {
            return(invisible(NULL))
          }
          expression_projection_warm_retry(
            isolate(expression_projection_warm_retry()) + 1L
          )
        },
        0.25
      )
      return()
    }
    available <- getGeneNames()
    if (!length(available)) {
      expression_projection_backend_warmed(TRUE)
      return()
    }
    try(
      viewerExpressionRow(data_set(), 1L, available[[1L]]),
      silent = TRUE
    )
    expression_projection_backend_warmed(TRUE)
  },
  ignoreInit = TRUE
)

## Read the expression values for one levels key. The caller runs this inside
## its reactive, so every dependency below still belongs to that reactive.
expression_projection_read_levels <- function(
  cells_to_show,
  genes_data,
  genes_present,
  display_mode
) {
  n_cells <- length(cells_to_show)
  ## Layout only changes the projection; it must not re-read expression rows.
  render_key <- isolate(expression_projection_render_key())
  painted <- isolate(input[["expression_projection_rendered_state"]])
  painted_key <- if (is.list(painted)) painted$render_key
  ## Visibility/request state controls the progress UI, not expression reads.
  ## Navigation and request acknowledgements must not invalidate these values.
  if (
    identical(isolate(input[["sidebar"]]), "geneExpression") &&
      !is.null(isolate(input[["expression_projection_render_request"]])) &&
      !isTRUE(isolate(expression_projection_summary_ready())) &&
      (length(genes_present) || !is.null(painted_key))
  ) {
    expressionProjectionProgressStart(render_key)
  } else {
    expressionProjectionProgressClose()
  }
  success <- FALSE
  on.exit(
    {
      if (!success) expressionProjectionProgressClose()
    },
    add = TRUE
  )

  if (length(genes_present) == 0) {
    ## No gene means one constant renderer colour, not a synthetic million-cell
    ## expression vector. The transport marks this empty value as the explicit
    ## zero-colour fast path.
    expression_levels <- numeric()
  } else {
    if (
      exists("expression_projection_parameters_plot", mode = "function", inherits = TRUE)
    ) {
      req(expression_projection_parameters_plot())
    } else {
      req(expression_projection_coordinates())
    }
    ## All branches keep the requested slice in canonical index order. The
    ## class accessors dispatch those indices across dgCMatrix and DelayedArray
    ## without a barcode lookup.
    if (identical(display_mode, "rgb")) {
      expressionProjectionProgressUpdate(
        0.3,
        "Calculating RGB co-expression..."
      )
      rgb_genes <- genes_data[["rgb_genes"]]
      requested_genes <- intersect(
        unique(unlist(rgb_genes, use.names = FALSE)),
        genes_present
      )
      expression_values <- expressionProjectionCachedRows(
        data_set(),
        cells_to_show,
        requested_genes
      )
      expression_levels <- lapply(rgb_genes, function(gene) {
        if (is.null(gene) || !gene %in% names(expression_values)) {
          return(rep(0, n_cells))
        }
        unname(expression_values[[gene]])
      })
    } else if (identical(display_mode, "separate")) {
      expressionProjectionProgressUpdate(
        0.3,
        "Extracting multiple gene panels..."
      )
      expression_levels <- expressionProjectionCachedRows(
        data_set(),
        cells_to_show,
        genes_present
      )
    } else if (length(genes_present) == 1) {
      expressionProjectionProgressUpdate(
        0.3,
        "Extracting single gene expression..."
      )
      expression_levels <- expressionProjectionCachedRow(
        data_set(),
        cells_to_show,
        genes_present[[1L]]
      )
    } else if (length(genes_present) >= 2 && length(genes_present) <= 9L) {
      expressionProjectionProgressUpdate(0.3, "Calculating mean expression...")
      rows <- expressionProjectionCachedRows(
        data_set(),
        cells_to_show,
        genes_present
      )
      expression_levels <- unname(Reduce(`+`, rows) / length(rows))
    } else if (length(genes_present) >= 2) {
      expressionProjectionProgressUpdate(0.3, "Calculating mean expression...")
      ## Per-cell mean across the requested genes, restricted to cells_to_show.
      ## An H5 sidecar is read row by row, as for one gene.
      cells <- viewerExpressionCells(data_set(), cells_to_show)
      expression_levels <- viewerH5ExpressionMean(
        data_set(),
        cells,
        genes_present
      )
      if (is.null(expression_levels)) {
        expression_levels <- unname(
          data_set()$getMeanExpressionForCells(
            cells = cells,
            genes = genes_present
          )
        )
      }
    }
    expressionProjectionProgressUpdate(
      0.55,
      "Preparing colours for transfer..."
    )
  }
  success <- TRUE
  expression_levels
}

## Plot parameters (projection, order, hover) rebuild the coordinates and
## re-run the reactive below, but the values depend only on these inputs. Keep
## the last result so such changes do not re-read million-cell expression.
expression_projection_levels_memo <- viewerMemoOne()

## A refused sidecar must not escape an observer and end the session. Keep the
## validation strict, show the reason, and silently abort this paint so the user
## can still switch to another data set. Never substitute zero expression.
expressionProjectionCheckSidecar <- function(read) {
  tryCatch(
    read(),
    cerebro_cell_names_mismatch = function(error) {
      expressionProjectionProgressClose()
      shiny::showNotification(
        conditionMessage(error),
        id = "expression-sidecar-mismatch",
        type = "error",
        duration = 15
      )
      shiny::req(FALSE)
    }
  )
}

expression_projection_expression_levels <- reactive(expressionProjectionCheckSidecar(
  function() {
    req(
      expression_projection_cells_to_show(),
      expression_selected_genes()
    )

    cells_to_show <- expression_projection_cells_to_show()
    ## Keep the canonical numeric indices returned by the shared projection
    ## sampler. Converting them to barcodes here only makes the backend match
    ## the same million names back to the original column indices.
    genes_data <- expression_selected_genes()

    ## The debounced selection can briefly retain genes from the previous data
    ## set during a switch. Re-filter before touching the expression backend.
    genes_present <- intersect(
      genes_data$genes_to_display_present,
      getGeneNames()
    )
    n_dimensions <- if (
      exists("expression_projection_parameters_plot", mode = "function", inherits = TRUE)
    ) {
      as.integer(expression_projection_parameters_plot()[["n_dimensions"]])
    } else {
      ncol(expression_projection_coordinates())
    }
    display_mode <- expressionSummaryMode(
      expression_projection_display_mode(),
      length(genes_present),
      n_dimensions
    )
    levels_key <- list(
      data_set(),
      cells_to_show,
      genes_present,
      display_mode,
      if (identical(display_mode, "rgb")) genes_data[["rgb_genes"]]
    )
    expression_projection_levels_memo(levels_key, function() {
      expression_projection_read_levels(
        cells_to_show,
        genes_data,
        genes_present,
        display_mode
      )
    })
  }
))

expression_summary_data <- reactive({
  req(
    expression_projection_cells_to_show(),
    expression_selected_genes(),
    expression_projection_display_mode()
  )
  genes <- unique(intersect(
    as.character(expression_selected_genes()[["genes_to_display_present"]]),
    getGeneNames()
  ))
  req(length(genes) > 0)

  n_dimensions <- if (
    exists("expression_projection_parameters_plot", mode = "function", inherits = TRUE)
  ) {
    as.integer(expression_projection_parameters_plot()[["n_dimensions"]])
  } else {
    ncol(expression_projection_coordinates())
  }
  spec <- expressionSummarySpec(
    expression_projection_display_mode(),
    genes,
    expression_selected_genes()[["rgb_genes"]],
    n_dimensions
  )
  expression_levels <- expression_projection_expression_levels()
  spec$series <- lapply(spec$series, function(series) {
    series$values <- if (identical(spec$kind, "mean")) {
      expression_levels
    } else {
      expression_levels[[series$key]]
    }
    series
  })
  spec$genes <- genes
  spec
})
