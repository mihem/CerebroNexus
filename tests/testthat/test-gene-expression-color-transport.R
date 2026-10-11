gene_expression_test_function <- function(file, name) {
  expressions <- parse(viewer_test_path("gene_expression", file))
  assignment <- Filter(
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], quote(`<-`)) &&
        identical(as.character(expression[[2L]]), name) &&
        is.call(expression[[3L]]) &&
        identical(expression[[3L]][[1L]], as.name("function"))
    },
    expressions
  )
  stopifnot(length(assignment) == 1L)
  scope <- new.env(parent = globalenv())
  eval(assignment[[1L]], envir = scope)
  scope[[name]]
}

test_that("sparse expression colour transport is exact and density-gated", {
  sparse_color <- gene_expression_test_function(
    "func_projection_update_plot.R",
    "expressionSparseColor"
  )
  values <- numeric(10000L)
  values[c(2L, 100L, 9999L)] <- c(1.5, Inf, NA_real_)
  packed <- sparse_color(values)

  expect_identical(packed$protocol, "sparse-f64-v1")
  expect_identical(packed$length, length(values))
  expect_identical(packed$index, c(1L, 99L, 9998L))
  restored <- numeric(packed$length)
  restored[packed$index + 1L] <- packed$color
  expect_equal(restored, values)
  expect_null(sparse_color(rep(1, 10000L)))
  expect_null(sparse_color(numeric(4095L)))

  wire <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = wire)
  sparse_wire <- wire$cv_wire_pack_message(list(
    data = list(
      sparse_color = packed
    )
  ))
  dense_wire <- wire$cv_wire_pack_message(list(data = list(color = values)))
  expect_lt(length(sparse_wire), length(dense_wire) / 100)
})

test_that("browser hydrates Gene colours without float32 rounding", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- testthat::test_path("fixtures", "gene-color-f64.js")
  expect_identical(
    system2(
      "node",
      c(shQuote(runner), shQuote(viewer_test_path("www", "cell_views.js")))
    ),
    0L
  )
})

test_that("binary colour payload retains R doubles", {
  wire <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = wire)
  values <- rep(0, 4096L)
  values[[1L]] <- 1 + 2^-40
  payload <- wire$cv_wire_pack_message(list(data = list(color = values)))
  header_length <- readBin(
    payload[1:4],
    integer(),
    size = 4L,
    endian = "little"
  )
  header <- rawToChar(payload[seq.int(5L, 4L + header_length)])
  descriptor <- jsonlite::fromJSON(header)$data$color
  expect_identical(descriptor$`__cv_wire__`, "f64")
  data_start <- 5L + header_length + (4L - header_length %% 4L) %% 4L
  data_start <- data_start + descriptor$offset
  decoded <- readBin(
    payload[seq.int(data_start, data_start + 7L)],
    double(),
    size = 8L,
    endian = "little"
  )
  expect_identical(decoded, values[[1L]])
})

test_that("short JSON colour payload also retains double precision", {
  wire <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = wire)
  exact <- 1 + 2^-40
  payload <- wire$cv_wire_pack_message(list(data = list(color = c(exact, 0))))
  header_length <- readBin(
    payload[1:4],
    integer(),
    size = 4L,
    endian = "little"
  )
  header <- rawToChar(payload[seq.int(5L, 4L + header_length)])
  restored <- jsonlite::fromJSON(header)$data$color
  expect_identical(restored[[1L]], exact)
  expect_identical(restored[[2L]], 0)
})

test_that("single-gene primary frames choose sparse colour transport", {
  runtime <- new.env(parent = globalenv())
  runtime$`%||%` <- function(x, y) if (is.null(x)) y else x
  runtime$expressionColorScale <- function(...) "scale"
  runtime$expressionReverseColorScale <- function(...) FALSE
  runtime$viewerDatasetIdentity <- function() {
    list(
      fingerprint = "cells",
      pack_fingerprint = "dataset"
    )
  }
  runtime$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover,
    extra,
    deferred_aux,
    ...
  ) {
    runtime$captured <- data
  }
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = runtime
  )
  values <- numeric(10000L)
  values[c(2L, 5000L)] <- c(2, 4)
  runtime$expression_projection_update_plot(list(
    coordinates = data.frame(x = seq_along(values), y = rev(seq_along(values))),
    reset_axes = FALSE,
    expression_levels = values,
    plot_parameters = list(
      draw_border = FALSE,
      keep_square = TRUE,
      plot_order = "Highest expression on top",
      point_size = 1,
      point_opacity = 1,
      x_range = c(1, length(values)),
      y_range = c(1, length(values)),
      is_trajectory = FALSE,
      hover_info = FALSE,
      projection = "umap",
      n_dimensions = 2L
    ),
    color_settings = list(
      color_scale = "Viridis",
      color_range = c(0, 4),
      color_mode = "same",
      genes = "GeneA"
    ),
    metadata = NULL,
    trajectory = list(),
    display_mode = "single",
    cell_indices = seq_along(values),
    projection_resource_failed = NULL,
    separate_panels = FALSE
  ))

  expect_null(runtime$captured$color)
  expect_identical(runtime$captured$sparse_color$index, c(1L, 4999L))
  expect_identical(
    runtime$captured$color_cache_key,
    "dataset::GeneA::10000"
  )
})

test_that("RGB and separate panels use one packed colour map", {
  runtime <- new.env(parent = globalenv())
  runtime$`%||%` <- function(x, y) if (is.null(x)) y else x
  runtime$expressionColorScale <- function(...) "scale"
  runtime$expressionReverseColorScale <- function(...) FALSE
  runtime$viewerDatasetIdentity <- function() {
    list(
      fingerprint = "cells",
      pack_fingerprint = "dataset"
    )
  }
  runtime$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover,
    extra,
    deferred_aux,
    ...
  ) {
    runtime$captured <- data
  }
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = runtime
  )
  n <- 10000L
  sparse <- numeric(n)
  sparse[c(2L, 5000L)] <- c(2, 4)
  levels <- list(GeneA = sparse, GeneB = rep(1, n))
  base <- list(
    coordinates = data.frame(x = seq_len(n), y = rev(seq_len(n))),
    reset_axes = FALSE,
    expression_levels = levels,
    plot_parameters = list(
      draw_border = FALSE,
      keep_square = TRUE,
      plot_order = "Natural order",
      point_size = 1,
      point_opacity = 1,
      x_range = c(1, n),
      y_range = c(1, n),
      is_trajectory = FALSE,
      hover_info = FALSE,
      projection = "umap",
      n_dimensions = 2L
    ),
    color_settings = list(
      color_scale = "Viridis",
      color_range = c(0, 4),
      color_mode = "shared",
      genes = names(levels),
      rgb_genes = list(r = "GeneA", g = "GeneB", b = NULL)
    ),
    metadata = NULL,
    trajectory = list(),
    display_mode = "separate",
    render_key = "separate",
    cell_indices = seq_len(n),
    projection_resource_failed = NULL,
    separate_panels = TRUE
  )

  runtime$expression_projection_update_plot(base)
  expect_null(runtime$captured$color)
  expect_identical(
    runtime$captured$packed_colors$GeneA$protocol,
    "sparse-f64-v1"
  )
  expect_identical(
    runtime$captured$packed_colors$GeneB$protocol,
    "dense-f64-v1"
  )

  rgb <- base
  rgb$display_mode <- "rgb"
  rgb$separate_panels <- FALSE
  rgb$expression_levels <- list(r = sparse, g = rep(1, n), b = numeric(n))
  runtime$expression_projection_update_plot(rgb)
  expect_null(runtime$captured$color)
  expect_null(runtime$captured[["rgb"]])
  expect_named(runtime$captured$packed_rgb, c("r", "g", "b"))
  expect_identical(runtime$captured$packed_rgb$r$protocol, "sparse-f64-v1")
})

test_that("full canonical expression reads omit the million-index slice", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  data_set <- Cerebro$new()
  data_set$expression <- matrix(
    1:6,
    nrow = 2L,
    dimnames = list(c("A", "B"), c("c1", "c2", "c3"))
  )

  expect_null(scope$viewerExpressionCells(data_set, 1:3))
  values <- scope$viewerExpressionValues(data_set, 1:3, "A")

  expect_identical(values, list(A = c(1, 3, 5)))
})

test_that("gene primary colours do not materialize metadata", {
  source <- paste(
    readLines(
      viewer_test_path("gene_expression", "obj_projection_data_to_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(source, "metadata <- NULL", fixed = TRUE)
  expect_false(grepl(
    "if (no_gene_selected) NULL else expression_projection_data()",
    source,
    fixed = TRUE
  ))
})

test_that("single-gene colour caches are bounded on both sides", {
  server <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "obj_projection_expression_levels.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  browser <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    server,
    "expression_projection_row_cache_limit <- 4L",
    fixed = TRUE
  )
  expect_match(server, "expressionProjectionCachedRow", fixed = TRUE)
  expect_match(server, "expression_projection_backend_warmed", fixed = TRUE)
  expect_match(
    server,
    'input[["expression_projection_rendered_key"]]',
    fixed = TRUE
  )
  expect_match(browser, "SINGLE_COLOR_CACHE_LIMIT = 4", fixed = TRUE)
  expect_match(browser, "hydrateSparseSingleColor(data)", fixed = TRUE)
  expect_match(browser, "hydrateColorPacketMap(", fixed = TRUE)
  expect_match(server, "expressionProjectionCachedRows", fixed = TRUE)
})

test_that("secondary expression summaries wait for the painted primary frame", {
  summaries <- paste(
    vapply(
      c(
        "UI_expression_by_group.R",
        "UI_expression_by_gene.R",
        "UI_expression_in_selected_cells.R",
        "UI_table_of_selected_cells.R"
      ),
      function(file) {
        paste(
          readLines(viewer_test_path("gene_expression", file), warn = FALSE),
          collapse = "\n"
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
  browser <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    summaries,
    "expression_projection_summary_ready()",
    fixed = TRUE
  )
  expect_match(browser, "singleActive + '_rendered_key'", fixed = TRUE)
  expect_match(browser, "eventKind === 'primary'", fixed = TRUE)

  ui <- paste(
    readLines(viewer_test_path("gene_expression", "UI.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ui, "expression_summary_gate", fixed = TRUE)
  expect_match(ui, "IntersectionObserver", fixed = TRUE)
  expect_match(ui, "expression_summary_viewport_request", fixed = TRUE)
  expect_match(ui, "observer.observe(target)", fixed = TRUE)
  expect_false(grepl("armSummaryGate", ui, fixed = TRUE))
  expect_match(
    summaries,
    'input[["expression_summary_viewport_request"]]',
    fixed = TRUE
  )
})

test_that("gene expression progress remains open until the browser paints", {
  levels <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "obj_projection_expression_levels.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  render <- paste(
    readLines(
      viewer_test_path("gene_expression", "event_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(levels, "shiny::Progress$new", fixed = TRUE)
  expect_false(grepl("withProgress(", levels, fixed = TRUE))
  expect_match(
    levels,
    'input[["expression_projection_rendered_key"]]',
    fixed = TRUE
  )
  expect_match(levels, "expressionProjectionProgressClose(", fixed = TRUE)
  expect_match(render, "Transferring expression colours...", fixed = TRUE)
  expect_match(render, "Drawing expression colours...", fixed = TRUE)
})
