##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_info"]], {
  showModal(
    modalDialog(
      expression_projection_info$text,
      title = expression_projection_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_projection_info <- list(
  title = "Dimensional reduction",
  text = HTML(
    "
    Interactive projection of cells into two- or three-dimensional space based on their expression profile.<br>
    <ul>
      <li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44).</li>
      <li>Cell color reflects the log-normalised expression of entered genes. If more than 1 gene is entered or a gene set is selected, the color reflects the average expression of all genes. Genes must be in separate lines or separated by a space, comma, or semicolon. Reported below the projection are the genes that are present and absent in this data set. Absent genes could either have been annotated with a different name or were not expressed in any of the cells. Matching of gene names is case-insensitive, that means Myc/MYC/myc are treated equally.</li>
      <li>Cells can be plotted either randomly (to give a more unbiased perspective) or in the order of expression (with highest expression plotted last), sometimes resulting in a more appealing figure.</li>
      <li>The last two slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs).</li>
    </ul>
    The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow.<br>
    <b>Show genes in separate panels</b><br>
    When selecting multiple genes as input (up to 8), this option shows each gene in a coordinated panel instead of averaging expression across genes. Selection, hover, pan, zoom, and the active color-scale settings are shared across the panels.
    "
  )
)
