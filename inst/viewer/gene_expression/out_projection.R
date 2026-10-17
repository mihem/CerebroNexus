##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "expression_projection_info",
  expression_projection_info
)

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
      <li>Cell colour reflects the log-normalised expression of entered genes. If more than 1 gene is entered or a gene set is selected, the colour reflects the average expression of all genes. Genes must be in separate lines or separated by a space, comma, or semicolon. Reported below the projection are the genes that are present and absent in this data set. Absent genes could either have been annotated with a different name or were not expressed in any of the cells. Matching of gene names is case-insensitive, that means Myc/MYC/myc are treated equally.</li>
      <li>Cells can be plotted either randomly (to give a more unbiased perspective) or in the order of expression (with highest expression plotted last), sometimes resulting in a more appealing figure.</li>
      <li>The last two slider elements on the left can be used to resize the projection axes. This can be particularly useful when a projection contains a population of cell that is very far away from the rest and therefore creates a big empty space (which is not uncommon for UMAPs).</li>
    </ul>
    The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow.<br>
    <b>Display mode</b><br>
    Mean expression averages the selected genes per cell. Separate panels shows each gene for 2–9 selected genes in a two-dimensional projection; other cases use mean expression. Both modes retain the complete gene list.<br>
    RGB co-expression initially assigns the first three available genes in selection order to R, G and B. Additional genes remain in the original list. Returning to RGB preserves edited channels, including empty channels, until the original list changes. Genes chosen in a channel are added to the gene list when leaving RGB.<br>
    <b>RGB layout</b><br>
    Composite RGB overlays the three channels. Separate channels shows one panel per populated channel, using the same intensities and colours. Each channel is scaled from zero to its own maximum in the displayed cells; brightness cannot be used to compare absolute expression across genes. Zero/low expression is grey; simultaneous expression in all three channels is black in the composite. Empty channels are omitted from the separate layout, and repeated genes retain their channel identities.
    "
  )
)
