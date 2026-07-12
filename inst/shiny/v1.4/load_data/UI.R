##----------------------------------------------------------------------------##
## Tab: Load data
##----------------------------------------------------------------------------##
tab_load_data <- tabItem(
  tabName = "loadData",
  ## Order reflects priority: pick a dataset, see its stats, then the low-frequency
  ## preferences sink to the bottom of the page.
  uiOutput("load_data_select_file_UI"),
  uiOutput("load_data_sample_info_UI"),
  ## Low-frequency advanced settings — visually de-emphasised and sunk to the
  ## bottom (see .cerebro-advanced in www/custom.css).
  div(class = "cerebro-advanced", uiOutput("preferences_options"))
)
