##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##----------------------------------------------------------------------------##

source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/enriched_pathways/select_content.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/enriched_pathways/table.R"
  ),
  local = TRUE
)
