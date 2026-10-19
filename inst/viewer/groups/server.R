##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
for (module_file in c(
  "select_group.R",
  "composition.R",
  "expression_metrics.R"
)) {
  source(cerebro_root_path("viewer", "groups", module_file), local = TRUE)
}
