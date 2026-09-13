##----------------------------------------------------------------------------##
## Expression levels of cells in projection.
##
## bindCache() was attempted here but backed out: the reactive depends on
## expression_selected_genes(), which is an eventReactive that req()s
## input$expression_analysis_mode, and the chain with isolate() inside a
## bindCache key reliably produced inconsistent body-execution behaviour on
## repeated gene switches (some clicks hit cache even when the gene had just
## changed, risking stale plots). Leaving the per-gene compute in place for
## now; step 4's extractExpression refactor is the safer place to reclaim
## repeated-click latency on this reactive.
##----------------------------------------------------------------------------##
expression_projection_expression_levels <- reactive({
  req(
    expression_projection_cells_to_show(),
    expression_selected_genes()
  )

  withProgress(message = 'Calculating expression levels...', value = 0.2, {
    cells_to_show <- expression_projection_cells_to_show()
    ## Keep the canonical numeric indices returned by the shared projection
    ## sampler. Converting them to barcodes here only makes the backend match
    ## the same million names back to the original column indices.
    n_cells <- length(cells_to_show)
    genes_data <- expression_selected_genes()

    ## expression_selected_genes() is an eventReactive bound to the
    ## "Plot Expression" button, so its cached `genes_to_display_present` is
    ## NOT refreshed on a dataset switch. If the user previously plotted
    ## genes that exist in the old dataset but not in the new one, the cache
    ## still holds them and getExpressionMatrix(genes=) below would crash
    ## with vctrs::vec_slice "Element X doesn't exist". Re-filter against
    ## the current dataset's gene names every time this reactive fires.
    genes_present <- intersect(
      genes_data$genes_to_display_present,
      getGeneNames()
    )
    display_mode <- expressionSummaryMode(
      input[["expression_projection_genes_in_separate_panels"]],
      length(genes_present),
      ncol(expression_projection_coordinates())
    )

    if (length(genes_present) == 0) {
      expression_levels <- if (identical(display_mode, "rgb")) {
        list(r = rep(0, n_cells), g = rep(0, n_cells), b = rep(0, n_cells))
      } else {
        rep(0, n_cells)
      }
    } else {
      req(expression_projection_coordinates())
      ## All branches keep the requested slice in canonical index order. The
      ## class accessors dispatch those indices across dgCMatrix, DelayedArray,
      ## and IterableMatrix without a barcode lookup.
      if (identical(display_mode, "rgb")) {
        incProgress(0.3, detail = "Calculating RGB co-expression...")
        rgb_genes <- genes_data[["rgb_genes"]]
        requested_genes <- intersect(
          unique(unlist(rgb_genes, use.names = FALSE)),
          genes_present
        )
        expression_values <- viewerExpressionValues(
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
        incProgress(0.3, detail = "Extracting multiple gene panels...")
        expression_levels <- viewerExpressionValues(
          data_set(),
          cells_to_show,
          genes_present
        )
      } else if (length(genes_present) == 1) {
        incProgress(0.3, detail = "Extracting single gene expression...")
        expression_levels <- unname(viewerExpressionRow(
          data_set(),
          cells_to_show,
          genes_present[[1L]]
        ))
      } else if (length(genes_present) >= 2) {
        incProgress(0.3, detail = "Calculating mean expression...")
        ## Per-cell mean across the requested genes, restricted to cells_to_show.
        expression_levels <- unname(
          data_set()$getMeanExpressionForCells(
            cells = viewerExpressionCells(data_set(), cells_to_show),
            genes = genes_present
          )
        )
      }
    }
    return(expression_levels)
  })
})

expression_summary_data <- reactive({
  req(
    expression_projection_cells_to_show(),
    expression_selected_genes(),
    input[["expression_projection_genes_in_separate_panels"]]
  )
  genes <- unique(intersect(
    as.character(expression_selected_genes()[["genes_to_display_present"]]),
    getGeneNames()
  ))
  req(length(genes) > 0)

  spec <- expressionSummarySpec(
    input[["expression_projection_genes_in_separate_panels"]],
    genes,
    expression_selected_genes()[["rgb_genes"]],
    ncol(expression_projection_coordinates())
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
