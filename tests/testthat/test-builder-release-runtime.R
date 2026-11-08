test_that("release runtime resolves contracts without source namespace injection", {
  skip_without_installed_package()
  skip_if_not_installed("callr")
  root <- normalizePath(builder_test_inst_path("builder"))
  result <- callr::r(
    function(root) {
      suppressPackageStartupMessages(library(CerebroNexus))
      Sys.unsetenv("CEREBRO_PACKAGE_SOURCE")
      setwd(root)
      runtime <- new.env(parent = globalenv())
      runtime$`%||%` <- function(a, b) if (is.null(a)) b else a
      source(file.path(root, "prerequisite.R"), local = runtime)
      source(file.path(root, "coordinator.R"), local = runtime)
      files <- runtime$builder_release_runtime_files()
      for (name in setdiff(names(files), "root")) {
        runtime$builder_source_utf8(files[[name]], runtime)
      }
      plan <- structure(
        list(
          app_contract_version = 1L,
          make_app = TRUE,
          dataset_order = "ds1",
          items = list(list(
            id = "ds1",
            name = "Visium",
            filename = "visium.crb",
            extra_material_table_index = list(
              stable = list(display_name = "Markers")
            )
          )),
          app_options = list(),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
        ),
        class = c("builder_build_plan", "list")
      )
      tryCatch(
        {
          contract <- runtime$.builder_app_plan_contract(plan)
          plan$app_contract_version <- 0L
          invalid <- tryCatch(
            runtime$.builder_coordinator_app_contract(plan),
            error = conditionMessage
          )
          list(contract = contract, invalid = invalid)
        },
        error = conditionMessage
      )
    },
    args = list(root = root)
  )
  expect_true(
    is.list(result),
    info = if (is.character(result)) result else NULL
  )
  if (is.list(result)) {
    expect_identical(result$contract$app_contract_version, 1L)
    expect_identical(
      result$contract$items[[1]]$extra_material_table_index,
      list(stable = list(display_name = "Markers"))
    )
    expect_match(result$invalid, "contract-v1 BuildPlan")
  }
})
