test_that("ImageFeaturePlot preserves values and uses the saved expression palette", {
  runtime <- new.env(parent = globalenv())
  for (path in c(
    "utility_functions.R",
    "gene_expression/func_color_scale.R",
    "spatial/func_projection_update_plot.R"
  )) {
    sys.source(viewer_test_path(path), envir = runtime)
  }
  rendered <- NULL
  runtime$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover,
    extra,
    deferred_aux,
    dataset_context,
    feedback = NULL
  ) {
    rendered <<- list(meta = meta, data = data)
  }
  parameters <- list(
    plot_type = "ImageFeaturePlot",
    color_variable = "GENE",
    n_dimensions = 2L,
    point_size = 5,
    point_opacity = 1,
    group_labels = FALSE,
    keep_square = TRUE,
    draw_border = FALSE,
    hover_info = FALSE,
    x_range = c(0, 10),
    y_range = c(0, 10),
    background_opacity = 1
  )
  request <- list(
    dataset_context = list(dataset_key = "ds1"),
    cells_df = data.frame(
      cell_barcode = c("b", "a", "c"),
      GENE = c(4, 0, 2),
      roi = c("R1", "R2", "R1")
    ),
    coordinates = data.frame(x = c(1, 2, 3), y = c(4, 5, 6)),
    reset_axes = FALSE,
    plot_parameters = parameters,
    hover_columns = list(),
    hover_info = character()
  )
  runtime$spatial_projection_update_plot(request)
  expect_identical(as.numeric(rendered$data$color), c(4, 0, 2))
  expect_identical(
    rendered$data$colorscale,
    runtime$expressionColorScale("cerebro_orange")
  )
  expect_false(rendered$data$reversescale)
  runtime$Cerebro.options <- list(
    viewer_content = list(
      ds1 = list(
        gene_expression = list(palette = "blues")
      )
    )
  )
  runtime$spatial_projection_update_plot(request)
  expect_identical(
    rendered$data$colorscale,
    runtime$expressionColorScale("blues")
  )
})
