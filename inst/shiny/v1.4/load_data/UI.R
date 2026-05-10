##----------------------------------------------------------------------------##
## Tab: Load data
##----------------------------------------------------------------------------##
tab_load_data <- tabItem(
  tabName = "loadData",
  uiOutput("load_data_select_file_UI"),
  uiOutput("preferences_options"),
  uiOutput("load_data_sample_info_UI")
)
