library(shinytest2)

## Locate inst/ whether running via devtools::test() or R CMD check
inst_dir <- system.file(package = "cerebroAppLite")
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

test_that("{shinytest2} recording: overview", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "overview", height = 950, width = 1619)
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
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "main", height = 950, width = 1619)
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
  app$set_inputs(overview_projection_percentage_cells_to_show = 100)
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


test_that("{shinytest2} recording: groups", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "groups", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "groups")
  app$wait_for_idle(timeout = 10000)

  ## composition plot renders
  plot_val <- app$get_value(output = "groups_by_other_group_plot")
  expect_false(is.null(plot_val))

  ## switch to percent view
  app$set_inputs(groups_by_other_group_show_as_percent = TRUE)
  app$wait_for_idle(timeout = 10000)
  plot_pct <- app$get_value(output = "groups_by_other_group_plot")
  expect_false(is.null(plot_pct))

  ## show table
  app$set_inputs(groups_by_other_group_show_table = TRUE)
  app$wait_for_idle(timeout = 10000)
  table_val <- app$get_value(output = "groups_by_other_group_table")
  expect_false(is.null(table_val))

  app$expect_values(
    input = c(
      "groups_by_other_group_show_as_percent",
      "groups_by_other_group_show_table"
    ),
    output = FALSE,
    export = FALSE
  )
  app$stop()
})

test_that("{shinytest2} recording: marker_genes", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "marker_genes",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "markerGenes")
  app$wait_for_idle(timeout = 10000)

  ## select seurat_clusters (only group with actual marker genes)
  app$set_inputs(marker_genes_selected_table = "seurat_clusters", wait_ = FALSE)
  app$wait_for_idle(timeout = 10000)

  ## table renders
  table_val <- app$get_value(output = "marker_genes_table")
  expect_false(is.null(table_val))

  ## verify expected columns are present in the table header
  parsed <- jsonlite::fromJSON(table_val, simplifyVector = FALSE)
  container_html <- parsed$x$container
  for (col in c(
    "gene",
    "p_val",
    "avg_logFC",
    "pct.1",
    "pct.2",
    "p_val_adj",
    "on_cell_surface"
  )) {
    expect_true(
      grepl(col, container_html, fixed = TRUE),
      label = paste("column present:", col)
    )
  }

  ## "no markers found" and "no data" messages should not be shown —
  ## table_or_text_UI should contain the table, not a text message
  ui_val <- app$get_value(output = "marker_genes_table_or_text_UI")
  expect_false(grepl("no_markers_found|no_data", ui_val$html, fixed = FALSE))

  app$expect_values(
    input = c(
      "marker_genes_selected_method",
      "marker_genes_selected_table",
      "marker_genes_table_filter_switch"
    ),
    output = FALSE,
    export = FALSE
  )
  app$stop()
})


test_that("{shinytest2} recording: gene_expression", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "gene_expression",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "geneExpression")
  app$wait_for_idle(timeout = 10000)

  ## projection UI renders without any gene selected
  proj_ui <- app$get_value(output = "expression_projection_UI")
  expect_false(is.null(proj_ui))

  ## select MS4A1 and verify it is found in the data set
  app$set_inputs(expression_genes_input = "MS4A1", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)

  genes_text <- app$get_value(output = "expression_genes_displayed")
  expect_true(grepl("MS4A1", genes_text))
  expect_true(grepl("0 gene(s) are not in data set", genes_text, fixed = TRUE))

  ## projection plot renders after gene selection
  proj_val <- app$get_value(output = "expression_projection")
  expect_false(is.null(proj_val))

  ## verify expression levels have some non-zero values (cells with color)
  expr_levels <- app$get_value(export = "expression_levels")
  expect_true(length(expr_levels) > 0)
  expect_true(any(expr_levels > 0))

  app$stop()
})

test_that("{shinytest2} recording: gene_id_conversion", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "gene_id_conversion",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "geneIdConversion")
  app$wait_for_idle(timeout = 10000)

  table_val <- app$get_value(output = "gene_info")
  expect_false(is.null(table_val))

  app$stop()
})

test_that("{shinytest2} recording: color_management", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "color_management",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "color_management")
  app$wait_for_idle(timeout = 10000)

  ui_val <- app$get_value(output = "color_assignments_UI")
  expect_false(is.null(ui_val))

  app$stop()
})

test_that("{shinytest2} recording: about", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "about", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  app$set_inputs(sidebar = "about")
  app$wait_for_idle(timeout = 10000)

  about_text <- app$get_value(output = "about")
  expect_false(is.null(about_text))
  expect_true(nchar(about_text) > 0)

  app$stop()
})

test_that("createShinyApp bundles a working app", {
  example <- system.file("extdata/v1.4/example.crb", package = "cerebroAppLite")
  skip_if_not(nzchar(example), "example.crb not found")

  tmp <- file.path(tempdir(), "demo.crb")
  app_dir <- file.path(tempdir(), "test_create_app")
  file.copy(example, tmp, overwrite = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)

  createShinyApp(
    cerebro_data = c("mydata" = tmp),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  )

  app <- AppDriver$new(app_dir, height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  cells <- app$get_value(output = "load_data_number_of_cells")
  expect_true(grepl("501", cells$html))

  app$stop()
})
