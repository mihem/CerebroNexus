##----------------------------------------------------------------------------##
## Spatial autocorrelation (Moran's I) of the displayed ImageFeaturePlot gene.
##
## Reports how spatially clustered the selected gene's expression is: ~+1 when
## high/low cells segregate into patches, ~0 for a random spatial pattern. Only
## computed in ImageFeaturePlot mode. The O(n^2) neighbour weighting is capped by
## down-sampling to a fixed number of cells, so the score stays responsive on
## large slides (it's an estimate of the same statistic on a random subset).
##----------------------------------------------------------------------------##
output[["spatial_projection_morans_i"]] <- renderText({
  plot_parameters <- spatial_projection_parameters_plot()
  req(identical(plot_parameters[["plot_type"]], "ImageFeaturePlot"))
  gene <- plot_parameters[["feature_to_display"]]
  req(gene, gene %in% getGeneNames())

  metadata <- getMetaData()
  spatial_data <- getSpatialData(plot_parameters[["projection"]])
  coords <- spatial_data$coordinates
  req(nrow(coords) >= 2)

  ## Align expression to coordinates BY BARCODE, not by position: the coordinate
  ## table is stored in .getSpatialData()'s own cell order (a possibly reordered
  ## subset), so a positional pairing would match each cell's expression to a
  ## different cell's (x, y). Restrict to the barcodes present in both tables.
  if ("cell_barcode" %in% colnames(metadata)) {
    cells <- as.character(metadata$cell_barcode)
  } else {
    cells <- rownames(metadata)
  }
  common <- intersect(cells, rownames(coords))
  req(length(common) >= 2)

  expression_data <- data_set()$getExpressionMatrix(
    cells = common,
    genes = gene
  )
  req(!is.null(expression_data), gene %in% rownames(expression_data))
  expr <- as.vector(expression_data[gene, common])
  coords <- coords[common, , drop = FALSE]

  ## Down-sample for the O(n^2) neighbour search so large slides stay responsive.
  max_cells <- 2000
  n <- length(common)
  idx <- seq_len(n)
  if (n > max_cells) {
    set.seed(42) # stable score across re-renders
    idx <- sort(sample(idx, max_cells))
  }

  score <- morans_i(
    coords[[1]][idx],
    coords[[2]][idx],
    expr[idx],
    k = 6
  )
  if (is.na(score)) {
    return("not enough cells")
  }
  paste0(
    formatC(score, format = "f", digits = 3),
    if (n > max_cells) paste0(" (", max_cells, "-cell subsample)") else ""
  )
}) %>%
  cachePlot(
    spatial_projection_parameters_plot()[["projection"]],
    spatial_projection_parameters_plot()[["feature_to_display"]]
  )

## Keep it computed while the Additional-parameters box is collapsed, so the
## value is ready the moment the user expands it.
outputOptions(
  output,
  "spatial_projection_morans_i",
  suspendWhenHidden = FALSE
)
