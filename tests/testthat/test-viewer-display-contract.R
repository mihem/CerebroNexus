test_that("display defaults preserve legacy bundles and page overrides", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_content_contract.R"), env)
  defaults <- env$viewerDisplayDefaults(list(), "dataset")
  expect_false(defaults$group_labels)
  expect_identical(defaults$gene_plotting_order, "Input order")
  options <- list(
    viewer_content = list(
      dataset = list(
        display = list(
          gene_plotting_order = "Random",
          trajectory_show_trend_line = FALSE,
          projection_draw_border = TRUE
        )
      )
    )
  )
  expect_identical(
    env$viewerPageDisplayDefaults(options, "dataset", "gene")$plotting_order,
    "Input order"
  )
  expect_false(
    env$viewerPageDisplayDefaults(
      options,
      "dataset",
      "trajectory"
    )$show_trend_line
  )
  expect_true(
    env$viewerPageDisplayDefaults(options, "dataset", "projection")$draw_border
  )
  options$viewer_content$dataset$display <- list(unknown = TRUE)
  expect_identical(env$viewerDisplayDefaults(options, "dataset"), defaults)
})
