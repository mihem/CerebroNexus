##----------------------------------------------------------------------------##
## Tab: Spatial
##----------------------------------------------------------------------------##
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/viewer/core/viewer_content_contract.R"
  ),
  local = TRUE
)
files_to_load <- list.files(
  paste0(Cerebro.options[["cerebro_root"]], "/viewer/spatial"),
  pattern = "func_|obj_|UI_|out_|event_",
  full.names = TRUE
)

for (i in files_to_load) {
  source(i, local = TRUE)
}
