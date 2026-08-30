##----------------------------------------------------------------------------##
## Tab: Spatial
##----------------------------------------------------------------------------##
## Drawing and image placement live in the app-wide cell-view engine. Spatial
## keeps only its page-specific scroll indicator.
js_code_spatial_page <- cerebro_read_file(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/spatial/js_page_helpers.js"
  )
)

tab_spatial <- tabItem(
  tabName = "spatial",
  ## necessary to ensure alignment of table headers and content
  shinyjs::inlineCSS(
    "
    #spatial_details_selected_cells_table .table th {
      text-align: center;
    }
    #spatial_details_selected_cells_table .dt-middle {
      vertical-align: middle;
    }

    "
  ),
  shinyjs::extendShinyjs(
    text = js_code_spatial_page,
    functions = c("showScrollDownIndicator", "hideScrollDownIndicator")
  ),
  uiOutput("spatial_projection_UI"),
  uiOutput("spatial_selected_cells_plot_UI"),
  uiOutput("spatial_selected_cells_table_UI")
)
