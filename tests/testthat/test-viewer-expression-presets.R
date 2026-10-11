sys.source(
  viewer_test_path("gene_expression", "func_color_scale.R"),
  environment()
)

test_that("expression presets have consistent direction and legacy defaults", {
  defaults <- list(
    palette = "cerebro_orange",
    panel_mode = "shared",
    panel_palette = "default"
  )
  expect_identical(expressionColorSettings(), defaults)
  expect_identical(
    expressionColorSettings(list(palette = NA_character_)),
    defaults
  )
  expect_false(expressionColorSettingsValid(list(palette = "blue")))
  for (palette in unname(expressionPaletteChoices())) {
    scale <- expressionColorScale(palette)
    expect_identical(scale[[1L]][[1L]], 0)
    expect_identical(scale[[length(scale)]][[1L]], 1)
    expect_false(expressionReverseColorScale(palette))
  }
  expect_identical(
    expressionColorScale("Cerebro orange"),
    expressionColorScale("cerebro_orange")
  )
  expect_identical(
    expressionColorScale("cerebro_orange")[[5L]][[2L]],
    "#9f251f"
  )
  genes <- paste0("g", 1:10)
  scales <- expressionPanelColorScales(
    genes,
    "different",
    "cerebro_orange",
    "soft"
  )
  expect_identical(names(scales), genes)
  expect_identical(scales[[1L]], scales[[10L]])
  expect_identical(
    tolower(scales[[1L]][[5L]][[2L]]),
    expressionPanelColors("soft")[[1L]]
  )
})

test_that("Viewer defaults and local panel choices are scoped to a dataset load", {
  scope <- new.env(parent = environment())
  scope$Cerebro.options <- list(
    viewer_content = list(
      a = list(
        gene_expression = list(
          palette = "blues",
          panel_mode = "distinct",
          panel_palette = "soft"
        )
      ),
      b = list()
    )
  )
  scope$viewer_requested_dataset_context <- shiny::reactiveVal(list(
    dataset_key = "a",
    generation = 1
  ))
  scope$input <- shiny::reactiveValues()
  sys.source(viewer_test_path("gene_expression", "obj_color_defaults.R"), scope)
  expect_identical(
    shiny::isolate(scope$current_expression_color_defaults())$palette,
    "blues"
  )
  expect_identical(
    shiny::isolate(scope$expression_panel_color_mode()),
    "different"
  )
  id <- shiny::isolate(scope$expression_panel_mode_input_id())
  scope$input[[id]] <- "shared"
  expect_identical(
    shiny::isolate(scope$expression_panel_color_mode()),
    "shared"
  )
  scope$viewer_requested_dataset_context(list(
    dataset_key = "b",
    generation = 2
  ))
  expect_identical(
    shiny::isolate(scope$current_expression_color_defaults()),
    expressionColorSettings()
  )
  expect_identical(
    shiny::isolate(scope$expression_panel_color_mode()),
    "shared"
  )
  scope$input[[id]] <- "different" # delayed event from dataset A
  expect_identical(
    shiny::isolate(scope$expression_panel_color_mode()),
    "shared"
  )
  scope$viewer_requested_dataset_context(list(
    dataset_key = "a",
    generation = 3
  ))
  expect_identical(
    shiny::isolate(scope$expression_panel_color_mode()),
    "different"
  )
})
