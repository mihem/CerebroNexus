##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_info"]], {
  showModal(
    modalDialog(
      spatial_projection_info[["text"]],
      title = spatial_projection_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
spatial_projection_info <- list(
  title = "Dimensional reduction",
  text = HTML(
    "
    Interactive projection of cells into 2-dimensional space based on their expression profile.
    <ul>
      <li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44).</li>
      <li>Cells can be coloured by the sample they came from, the cluster they were assigned, the number of transcripts or expressed genes, percentage of mitochondrial and ribosomal gene expression, an apoptotic score (calculated based on the expression of few marker genes; more info in the 'Sample info' tab on the left), or cell cycle status (determined using the Seurat and Cyclone method).</li>
      <li>Confidence ellipses show the 95% confidence regions.</li>
      <li>Samples and clusters can be removed from the plot individually to highlight a contrast of interest.</li>
      <li>Point size, point opacity, and the percentage of displayed cells can be changed in More settings.</li>
    </ul>
    The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow."
  )
)
