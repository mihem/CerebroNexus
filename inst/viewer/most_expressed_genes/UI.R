##----------------------------------------------------------------------------##
## Tab: Most expressed genes
##----------------------------------------------------------------------------##
tab_most_expressed_genes <- tabItem(
  tabName = "mostExpressedGenes",
  shinyjs::inlineCSS(
    "
    #most_expressed_genes_table .table th {
      text-align: center;
    }
    "
  ),
  cerebroResultPage(
    "Most expressed genes",
    "most_expressed_genes_select_group_UI",
    "most_expressed_genes_table_or_text_UI",
    "most_expressed_genes_info"
  )
)
