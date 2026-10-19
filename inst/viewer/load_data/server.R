##----------------------------------------------------------------------------##
## Tab: Load data
##----------------------------------------------------------------------------##
for (module_file in c(
  "select_file.R",
  "sample_info.R",
  "preferences.R"
)) {
  source(cerebro_root_path("viewer", "load_data", module_file), local = TRUE)
}
