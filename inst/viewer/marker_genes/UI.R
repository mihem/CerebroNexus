##----------------------------------------------------------------------------##
## Tab: Marker genes
##----------------------------------------------------------------------------##
tab_marker_genes <- tabItem(
  tabName = "markerGenes",
  shinyjs::inlineCSS(
    "
    #marker_genes_table .table th {
      text-align: center;
    }
    "
  ),
  cerebroResultPage(
    "Marker genes",
    "marker_genes_select_method_and_table_UI",
    "marker_genes_table_or_text_UI",
    "marker_genes_info"
  )
)
