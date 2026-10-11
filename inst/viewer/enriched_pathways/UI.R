##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##----------------------------------------------------------------------------##

tab_enriched_pathways <- tabItem(
  tabName = "enrichedPathways",
  shinyjs::inlineCSS(
    "
    #enriched_pathways_table .table th {
      text-align: center;
    }
    #enriched_pathways_table .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  cerebroResultPage(
    "Enriched pathways",
    "enriched_pathways_select_method_and_table_UI",
    "enriched_pathways_table_or_text_UI",
    "enriched_pathways_info"
  )
)
