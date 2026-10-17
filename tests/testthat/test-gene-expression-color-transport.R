gene_expression_test_function <- function(file, name) {
  assignment <- viewer_r_assignment(
    viewer_test_path("gene_expression", file),
    name
  )
  scope <- new.env(parent = globalenv())
  eval(assignment, envir = scope)
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
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      expression_projection_data <- getMetaData <- function(...) {
        stop("metadata was hydrated")
      }
      viewer_loaded_dataset_context <- function() list(generation = 1L)
      expression_projection_coordinates <- function() {
        data.frame(x = 1:2, y = 2:1)
      }
      expression_projection_parameters_plot <- function() {
        list(is_trajectory = FALSE)
      }
      expression_projection_expression_levels <- function() c(0, 2)
      expression_selected_genes <- function() {
        list(genes_to_display_present = input$genes)
      }
      expression_projection_parameters_color <- function() list(scale = "blue")
      expression_projection_trajectory <- function() list(meta = data.frame())
      cerebroFeedbackControls <- function(...) list()
      data_set <- function() "dataset"
      expression_projection_parameters_other <- list(reset_axes = FALSE)
      expression_projection_render_key <- function() "key"
      expression_projection_cells_to_show <- function() 1:2
      sys.source(
        viewer_test_path("gene_expression", "obj_projection_data_to_plot.R"),
        scope
      )
    },
    {
      for (genes in list(character(), "A")) {
        session$setInputs(
          sidebar = "geneExpression",
          genes = genes,
          expression_projection_genes_in_separate_panels = "single"
        )
        result <- expression_projection_data_to_plot_raw()
        expect_null(result$metadata)
        expect_identical(result$cell_indices, 1:2)
        expect_identical(result$expression_levels, c(0, 2))
        expect_equal(result$coordinates, data.frame(x = 1:2, y = 2:1))
      }
    }
  )
})

test_that("single-gene colour caches are bounded on both sides", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      sys.source(
        viewer_test_path(
          "gene_expression",
          "obj_projection_expression_levels.R"
        ),
        scope
      )
      dataset <- "A"
      viewerDatasetIdentity <- function() list(pack_fingerprint = dataset)
      available_crb_files <- list(selected = "same-file")
      reads <- character()
      viewerExpressionValues <- function(data, cells, genes) {
        reads <<- c(reads, genes)
        setNames(
          lapply(genes, function(gene) data$expression[gene, cells]),
          genes
        )
      }
      matrix <- matrix(
        seq_len(18),
        nrow = 6,
        dimnames = list(LETTERS[1:6], NULL)
      )
      object <- list(expression = matrix)
    },
    {
      for (gene in LETTERS[1:4]) {
        expect_identical(
          expressionProjectionCachedRow(scope$object, 1:3, gene),
          unname(matrix[gene, ])
        )
      }
      expressionProjectionCachedRow(scope$object, 1:3, "A")
      expect_identical(scope$reads, LETTERS[1:4])
      expressionProjectionCachedRow(scope$object, 1:3, "E")
      expressionProjectionCachedRow(scope$object, 1:3, "A")
      expect_identical(scope$reads, LETTERS[1:5])
      expressionProjectionCachedRow(scope$object, 1:3, "B")
      expect_identical(tail(scope$reads, 1), "B")
      scope$dataset <- "B"
      scope$object$expression <- matrix + 100L
      expect_identical(
        expressionProjectionCachedRow(scope$object, 1:3, "A"),
        unname(matrix["A", ] + 100L)
      )
      for (i in 1:2) {
        expect_identical(
          expressionProjectionCachedRow(scope$object, c(3L, 1L), "A"),
          unname(matrix["A", c(3, 1)] + 100L)
        )
      }
      expect_identical(tail(scope$reads, 3), rep("A", 3))
    }
  )
})

test_that("secondary expression summaries wait for the painted primary frame", {
  cases <- c(
    expression_by_group = "UI_expression_by_group.R",
    expression_by_gene = "UI_expression_by_gene.R",
    expression_in_selected_cells = "UI_expression_in_selected_cells.R",
    expression_details_selected_cells = "UI_table_of_selected_cells.R"
  )
  for (id in names(cases)) {
    shiny::testServer(
      function(input, output, session) {
        scope <- environment()
        reads <- 0L
        read_data <- function() {
          reads <<- reads + 1L
          stop("summary data reached")
        }
        expression_projection_data <- expression_projection_parameters_color <- read_data
        expression_projection_render_key <- function() "current"
        sys.source(viewer_test_path("utility_functions.R"), scope)
        viewer_loaded_dataset_context <- function() viewerDatasetContext("session", "A", 1)
        viewer_server_load(
          viewer_server_statements(
            "gene_expression",
            "obj_projection_expression_levels.R"
          ),
          scope,
          names = c(
            "expressionProjectionPaintedCurrent",
            "expression_projection_summary_ready_state",
            "expression_projection_summary_ready"
          ),
          events = "expressionProjectionPaintedCurrent"
        )
        viewer_server_load(
          viewer_server_statements("gene_expression", cases[[id]]),
          scope,
          names = paste0("output$", id)
        )
      },
      {
        session$setInputs(
          expression_summary_viewport_request = 1,
          expression_projection_rendered_state = list(
            render_key = "old", dataset_context = viewer_loaded_dataset_context()
          )
        )
        expect_error(output[[id]], class = "shiny.silent.error")
        expect_identical(scope$reads, 0L)
        if (id %in% c("expression_by_group", "expression_by_gene")) {
          session$setInputs(
            expression_summary_viewport_request = NULL,
            expression_projection_rendered_state = list(
              render_key = "current", dataset_context = viewer_loaded_dataset_context()
            )
          )
          expect_error(output[[id]], class = "shiny.silent.error")
          expect_identical(scope$reads, 0L)
        }
        session$setInputs(
          expression_summary_viewport_request = 1,
          expression_projection_rendered_state = list(
            render_key = "current", dataset_context = viewer_loaded_dataset_context()
          )
        )
        expect_error(output[[id]], "summary data reached")
        expect_gt(scope$reads, 0L)
      }
    )
  }
})
