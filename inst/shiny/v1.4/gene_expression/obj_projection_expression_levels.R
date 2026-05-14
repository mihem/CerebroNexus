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

  # message('--> trigger "expression_projection_expression_levels"')

  withProgress(message = 'Calculating expression levels...', value = 0.2, {
    cells_to_show <- expression_projection_cells_to_show()
    ## expression_projection_cells_to_show() returns numeric row ids (see
    ## obj_projection_cells_to_show.R: `cells_to_show <- cells_df$row_id`),
    ## not cell barcodes. Passing numeric ids into getExpressionMatrix(cells=)
    ## works by accident on dgCMatrix (R's `[` accepts column positions) but
    ## breaks the RleMatrix branch in class-Cerebro.R, which calls
    ## match(cells, colnames(self$expression)) -- matching numbers against
    ## barcode strings returns NA. Translate once here so every backend sees
    ## the documented contract: cells = character barcodes.
    cells_to_show_bc <- colnames(data_set()$expression)[cells_to_show]
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

    if (length(genes_present) == 0) {
      expression_levels <- rep(0, n_cells)
    } else {
      req(expression_projection_coordinates())
      ## All branches below go through data_set()$getExpressionMatrix(cells, genes)
      ## with character barcodes (cells_to_show_bc) instead of subscripting
      ## data_set()$expression directly. The helper materialises only the
      ## requested gene x cell slice, avoiding the previous pattern of
      ## extracting a full row (all cells) and subsetting afterwards. Using
      ## barcodes lets the helper dispatch correctly across dgCMatrix (named
      ## [ ] subset), RleMatrix (match() against colnames), and IterableMatrix,
      ## so the former IterableMatrix special case is no longer needed.
      if (
        ncol(expression_projection_coordinates()) == 2 &&
          input[["expression_projection_genes_in_separate_panels"]] == TRUE &&
          length(genes_present) >= 2 &&
          length(genes_present) <= 9
      ) {
        incProgress(0.3, detail = "Extracting matrix for multiple panels...")
        expression_matrix <- data_set()$getExpressionMatrix(
          cells = cells_to_show_bc,
          genes = genes_present
        )
        expression_matrix <- Matrix::t(expression_matrix)
        expression_levels <- list()
        for (i in 1:ncol(expression_matrix)) {
          expression_levels[[colnames(expression_matrix)[
            i
          ]]] <- as.vector(expression_matrix[, i])
        }
      } else if (length(genes_present) == 1) {
        incProgress(0.3, detail = "Extracting single gene expression...")
        expression_matrix <- data_set()$getExpressionMatrix(
          cells = cells_to_show_bc,
          genes = genes_present
        )
        expression_levels <- unname(as.numeric(expression_matrix))
      } else if (length(genes_present) >= 2) {
        incProgress(0.3, detail = "Calculating mean expression...")
        ## Per-cell mean across the requested genes, restricted to cells_to_show.
        expression_levels <- unname(
          data_set()$getMeanExpressionForCells(
            cells = cells_to_show_bc,
            genes = genes_present
          )
        )
      }
    }
    # message(str(expression_levels))
    return(expression_levels)
  })
})
