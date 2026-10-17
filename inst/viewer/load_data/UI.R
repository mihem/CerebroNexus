##----------------------------------------------------------------------------##
## Tab: Load data
##----------------------------------------------------------------------------##
tab_load_data <- tabItem(
  tabName = "loadData",
  tags$h1("Data info"),
  ## Dataset selection is global; this page contains its metadata and upload UI.
  uiOutput("load_data_select_file_UI"),
  uiOutput("load_data_overview")
)
