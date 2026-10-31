builder_test_inst_path <- function(...) {
  relative <- file.path(...)
  vapply(
    relative,
    function(item) {
      path <- testthat::test_path("..", "..", "inst", item)
      if (!file.exists(path)) {
        path <- system.file(item, package = "CerebroNexus")
      }
      path
    },
    character(1),
    USE.NAMES = FALSE
  )
}

builder_profile_inst_path <- builder_test_inst_path
builder_table_inst_path <- builder_test_inst_path
builder_content_immune_inst_path <- builder_test_inst_path
builder_content_spatial_inst_path <- builder_test_inst_path
builder_spatial_test_inst_path <- builder_test_inst_path
spatial_contract_inst_path <- builder_test_inst_path

builder_test_description_path <- function() {
  path <- testthat::test_path("..", "..", "DESCRIPTION")
  if (!file.exists(path)) {
    path <- system.file(
      "DESCRIPTION",
      package = "CerebroNexus",
      mustWork = TRUE
    )
  }
  path
}
