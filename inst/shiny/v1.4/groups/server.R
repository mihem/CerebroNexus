##----------------------------------------------------------------------------##
## Tab: Groups
##----------------------------------------------------------------------------##
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/groups/select_group.R"
  ),
  local = TRUE
)
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/groups/composition.R"),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/groups/expression_metrics.R"
  ),
  local = TRUE
)
