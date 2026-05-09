library(shinytest2)

test_that("{shinytest2} recording: inst", {
  local_app_support(test_path("../../inst"))
  app <- AppDriver$new(test_path("../../inst"), name = "inst", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  ## Data Info tab: verify key values from the loaded example.crb
  cells_box <- app$get_value(output = "load_data_number_of_cells")
  expect_true(grepl("501", cells_box$html))

  organism_box <- app$get_value(output = "load_data_organism")
  expect_true(grepl("hg", organism_box$html))

  date_box <- app$get_value(output = "load_data_date_of_export")
  expect_true(grepl("2020-09-21", date_box$html))

  app$stop()
})


test_that("{shinytest2} recording: main", {
  local_app_support(test_path("../../inst"))
  app <- AppDriver$new(test_path("../../inst"), name = "main", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "overview")
  app$wait_for_idle(timeout = 10000)

  ## verify the projection renders
  plot_val <- app$get_value(output = "overview_projection")
  expect_false(is.null(plot_val))

  ## get unfiltered cell count
  cells_all <- app$get_value(export = "overview_cells_to_show")
  expect_true(length(cells_all) > 0)

  ## filter to cluster 0 only and verify fewer cells are shown
  app$set_inputs(overview_projection_group_filter_seurat_clusters = "0")
  app$wait_for_idle(timeout = 10000)
  cells_filtered <- app$get_value(export = "overview_cells_to_show")
  expect_true(length(cells_filtered) < length(cells_all))

  ## verify input parameters are applied
  app$set_inputs(overview_projection_point_size = 9)
  app$set_inputs(overview_projection_point_opacity = 0.9)
  app$set_inputs(overview_projection_percentage_cells_to_show = 60)
  app$wait_for_idle(timeout = 10000)

  app$expect_values(
    input = c(
      "overview_projection_point_size",
      "overview_projection_point_opacity",
      "overview_projection_percentage_cells_to_show",
      "overview_projection_group_filter_seurat_clusters"
    ),
    output = FALSE,
    export = FALSE
  )
  app$stop()
})
