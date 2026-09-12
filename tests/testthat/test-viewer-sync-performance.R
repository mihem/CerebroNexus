run_projection_indices <- function(metadata, filters, percentage) {
  scope <- new.env(parent = globalenv())
  scope$input <- c(
    list(test_percentage_cells_to_show = percentage),
    stats::setNames(
      filters,
      paste0("test_group_filter_", names(filters))
    )
  )
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  scope$getGroups <- function() names(filters)
  scope$getGroupLevels <- function(group) unique(metadata[[group]])
  scope$viewerProjectionCellIndices("test", metadata)
}

test_that("expression rows are fetched once and aligned by cell", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility_env
  )
  expect_true(is.function(utility_env$viewerExpressionValues))

  calls <- 0L
  requested <- NULL
  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionMatrix <- function(cells, genes) {
    calls <<- calls + 1L
    requested <<- list(cells = cells, genes = genes)
    matrix(
      c(1, 2, 3, 4),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(c("A", "B"), c("c2", "c1"))
    )
  }

  values <- utility_env$viewerExpressionValues(
    data_set,
    cells = c("c1", "c2"),
    genes = c("B", "A", "B", "")
  )

  expect_identical(calls, 1L)
  expect_identical(requested$cells, c("c1", "c2"))
  expect_identical(requested$genes, c("B", "A"))
  expect_identical(values, list(B = c(4, 3), A = c(2, 1)))
})

test_that("single-gene expression uses row access with a matrix fallback", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility_env
  )

  row_calls <- 0L
  matrix_calls <- 0L
  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionRow <- function(gene, cells) {
    row_calls <<- row_calls + 1L
    stats::setNames(c(2, 1), c("c2", "c1"))[cells]
  }
  data_set$getExpressionMatrix <- function(cells, genes) {
    matrix_calls <<- matrix_calls + 1L
    matrix(c(2, 1), nrow = 1L, dimnames = list(genes, cells))
  }

  values <- utility_env$viewerExpressionValues(data_set, c("c1", "c2"), "A")
  expect_identical(row_calls, 1L)
  expect_identical(matrix_calls, 0L)
  expect_identical(values, list(A = c(1, 2)))

  data_set$getExpressionRow <- NULL
  values <- utility_env$viewerExpressionValues(data_set, c("c1", "c2"), "A")
  expect_identical(matrix_calls, 1L)
  expect_identical(values, list(A = c(2, 1)))
})

test_that("expression helpers preserve canonical cell indices", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility_env
  )

  requested <- NULL
  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionRow <- function(gene, cells) {
    requested <<- cells
    c(30, 10)
  }

  values <- utility_env$viewerExpressionValues(data_set, c(3L, 1L), "A")
  expect_identical(requested, c(3L, 1L))
  expect_identical(values, list(A = c(30, 10)))
})

test_that("legacy expression accessors receive cell barcodes", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility_env
  )

  LegacyCerebro <- R6::R6Class(
    NULL,
    public = list(
      expression = NULL,
      requested = NULL,
      initialize = function() {
        self$expression <- matrix(
          1:3,
          nrow = 1L,
          dimnames = list("A", paste0("c", 1:3))
        )
      },
      getExpressionRow = function(gene, cells) {
        if (!is.character(cells)) {
          stop("`cells` must be a character vector.")
        }
        self$requested <- cells
        self$expression[gene, cells]
      }
    ),
    private = list(legacy_serialized_method = TRUE)
  )
  data_set <- LegacyCerebro$new()

  values <- utility_env$viewerExpressionValues(data_set, c(3L, 1L), "A")

  expect_identical(data_set$requested, c("c3", "c1"))
  expect_identical(values, list(A = c(3, 1)))
})

test_that("already aligned expression cells skip the string match", {
  match_lengths <- integer()
  utility_env <- new.env(parent = globalenv())
  utility_env$match <- function(x, table, ...) {
    match_lengths <<- c(match_lengths, length(x))
    base::match(x, table, ...)
  }
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = utility_env
  )

  data_set <- new.env(parent = emptyenv())
  data_set$getExpressionMatrix <- function(cells, genes) {
    matrix(
      seq_len(length(cells) * length(genes)),
      nrow = length(genes),
      dimnames = list(genes, cells)
    )
  }
  utility_env$viewerExpressionValues(
    data_set,
    cells = c("c1", "c2", "c3"),
    genes = c("A", "B")
  )

  expect_false(3L %in% match_lengths)
})

test_that("projection filtering and sampling preserve original row indices", {
  metadata <- data.frame(
    cell_barcode = paste0("cell", seq_len(7L)),
    batch = factor(
      c("drop", "keep", "drop", "keep", "keep", "drop", "keep")
    ),
    state = factor(c("T", "T", "T", "B", "T", "B", "B")),
    matrix(seq_len(140L), nrow = 7L),
    check.names = FALSE
  )
  eligible <- c(2L, 4L, 5L, 7L)
  result <- run_projection_indices(
    metadata,
    list(batch = "keep", state = c("T", "B")),
    50
  )
  expect_length(result, ceiling(length(eligible) * 0.5))
  expect_true(all(result %in% eligible))
  expect_identical(anyDuplicated(result), 0L)

  set.seed(123L)
  expected_all <- sample.int(nrow(metadata))
  set.seed(123L)
  expect_identical(
    run_projection_indices(
      metadata,
      list(batch = c("drop", "keep"), state = c("T", "B")),
      100
    ),
    expected_all
  )

  expect_identical(
    run_projection_indices(
      data.frame(batch = c("keep", NA_character_)),
      list(batch = "keep"),
      100
    ),
    1L
  )
  expect_identical(
    run_projection_indices(metadata, list(batch = character()), 50),
    integer()
  )
})

test_that("the first reactive value bypasses debounce", {
  utility_env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = utility_env)
  compute_count <- 0L
  server <- function(input, output, session) {
    raw <- shiny::reactive({
      shiny::req(input$value)
      compute_count <<- compute_count + 1L
      input$value
    })
    ready <- utility_env$debounceAfterFirst(raw, 10000)
  }

  shiny::testServer(server, {
    session$setInputs(value = "first")
    expect_identical(ready(), "first")
    session$setInputs(value = "second")
    expect_identical(ready(), "first")
    expect_identical(compute_count, 2L)
  })
})

test_that("hidden outputs resolve to their owning sidebar tabs", {
  utility_env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = utility_env)

  expect_identical(
    utility_env$viewerOutputTab(c(
      "overview_plot",
      "groups_plot",
      "trajectory_plot",
      "hla_plot",
      "unknown_output"
    )),
    c("overview", "groups", "trajectory", "hla_tcr_motifs", NA_character_)
  )
})

test_that("viewer source parsing is cached but evaluation stays local", {
  cache_env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("source_cache.R"), envir = cache_env)
  source_file <- tempfile(fileext = ".R")
  writeLines("value <- 1L", source_file)

  first <- new.env(parent = baseenv())
  second <- new.env(parent = baseenv())
  cache_env$viewerSource(source_file, first)
  cache_env$viewerSource(source_file, second)

  expect_identical(first$value, 1L)
  expect_identical(second$value, 1L)
  expect_identical(length(cache_env$.viewer_source_cache), 1L)
})
