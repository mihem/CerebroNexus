##----------------------------------------------------------------------------##
## Tab: Gene (set) expression
##----------------------------------------------------------------------------##
tab_gene_expression <- tabItem(
  tabName = "geneExpression",
  ## necessary to ensure alignment of table headers and content
  shinyjs::inlineCSS(
    "
    #expression_details_selected_cells .table th {
      text-align: center;
    }
    #expression_details_selected_cells .dt-middle {
      vertical-align: middle;
    }
    "
  ),
  uiOutput("expression_projection_UI"),
  uiOutput("expression_details_selected_cells_UI"),
  uiOutput("expression_in_selected_cells_UI"),
  uiOutput("expression_by_group_UI"),
  uiOutput("expression_by_gene_UI") #,
  # uiOutput("expression_by_pseudotime_UI")
)
