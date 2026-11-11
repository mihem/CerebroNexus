run_projection_indices <- function(
  metadata,
  filters,
  percentage,
  groups = names(filters)
) {
  scope <- new.env(parent = globalenv())
  scope$input <- c(
    list(test_percentage_cells_to_show = percentage),
    if (length(filters)) {
      stats::setNames(
        filters,
        paste0("test_group_filter_", names(filters))
      )
    } else {
      list()
    }
  )
  viewer_runtime_env(scope)
  scope$getGroups <- function() groups
  scope$getGroupLevels <- function(group) unique(metadata[[group]])
  scope$viewerProjectionCellIndices("test", metadata)
}

test_that("expression rows are fetched once and aligned by cell", {
  utility_env <- viewer_runtime_env()
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

test_that("single-gene expression uses row access, not a matrix read", {
  utility_env <- viewer_runtime_env()

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
})

test_that("expression helpers preserve canonical cell indices", {
  utility_env <- viewer_runtime_env()

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
  utility_env <- viewer_runtime_env()

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
  viewer_runtime_env(utility_env)

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

  expect_identical(
    run_projection_indices(
      metadata,
      list(batch = c("drop", "keep"), state = c("T", "B")),
      100
    ),
    seq_len(nrow(metadata))
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

  expect_identical(
    run_projection_indices(
      metadata,
      list(),
      100,
      groups = c("batch", "state")
    ),
    seq_len(nrow(metadata))
  )
})

test_that("the first reactive value bypasses debounce", {
  utility_env <- viewer_runtime_env()
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
    expect_identical(compute_count, 0L)
    expect_identical(ready(), "first")
    session$setInputs(value = "second")
    expect_identical(ready(), "first")
    expect_identical(compute_count, 2L)
  })
})

test_that("page-gated debounced values stay idle off the page", {
  utility_env <- viewer_runtime_env()
  compute_count <- 0L
  server <- function(input, output, session) {
    active <- function() identical(input$sidebar, "page")
    raw <- shiny::reactive({
      shiny::req(active(), input$value)
      compute_count <<- compute_count + 1L
      input$value
    })
    ready <- utility_env$debounceAfterFirst(raw, 10000, active = active)
  }

  shiny::testServer(server, {
    session$setInputs(sidebar = "page", value = "first")
    expect_identical(ready(), "first")
    session$setInputs(sidebar = "other")
    count <- compute_count
    ## Edits made elsewhere do not compute this page's value.
    session$setInputs(value = "second")
    session$setInputs(value = "third")
    expect_identical(compute_count, count)
    ## Returning reads the current value at once, not the one held before.
    session$setInputs(sidebar = "page")
    expect_identical(ready(), "third")
  })
})

test_that("hidden outputs resolve to their owning sidebar tabs", {
  utility_env <- viewer_runtime_env()

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

test_that("idle work runs one task per turn, once per process, while active", {
  cache_env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("source_cache.R"), envir = cache_env)
  ran <- character()
  task <- function(name) function() ran <<- c(ran, name)
  tasks <- list(a = task("a"), b = task("b"), c = task("c"))
  active <- TRUE
  cache_env$viewerIdleRun(tasks, function() active, delay = 0)
  later::run_now()
  expect_identical(ran, "a")
  later::run_now()
  active <- FALSE
  later::run_now()
  expect_identical(ran, c("a", "b"))

  active <- TRUE
  cache_env$viewerIdleRun(tasks, function() active, delay = 0)
  for (i in 1:3) {
    later::run_now()
  }
  expect_identical(ran, c("a", "b", "c"))
})

test_that("viewer source runs the installed byte code of a file's helpers", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "viewer", "page"), recursive = TRUE)
  source_file <- file.path(root, "viewer", "page", "helpers.R")
  writeLines(c("helper <- function(x) x + 1", "value <- 2L"), source_file)
  .writeViewerBytecode(root)
  expect_true(file.exists(
    file.path(root, "viewer-bytecode", "page", "helpers.R.rds")
  ))
  compiled <- function(f) {
    any(grepl("<bytecode", utils::capture.output(print(f)), fixed = TRUE))
  }

  cache_env <- new.env(parent = globalenv())
  cache_env$Cerebro.options <- list(cerebro_root = root)
  sys.source(viewer_test_path("source_cache.R"), envir = cache_env)
  session <- new.env(parent = globalenv())
  cache_env$viewerSource(source_file, session)
  expect_true(compiled(session$helper))
  expect_identical(session$helper(1), 2)
  expect_identical(session$value, 2L)

  ## An edited file no longer matches its byte code and runs as parsed.
  writeLines(c("helper <- function(x) x + 10", "value <- 2L"), source_file)
  edited <- new.env(parent = globalenv())
  cache_env$viewerSource(source_file, edited)
  expect_false(compiled(edited$helper))
  expect_identical(edited$helper(1), 11)
})

test_that("the installed byte code gives one named function, compiled", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "viewer"), recursive = TRUE)
  source_file <- file.path(root, "viewer", "shiny_server.R")
  writeLines(
    c(
      "offset <- 1",
      "helper <- function(x) x",
      "server <- function(input, output, session) input + offset"
    ),
    source_file
  )
  .writeViewerBytecode(root)
  bytecode <- readRDS(
    file.path(root, "viewer-bytecode", "shiny_server.R.rds")
  )
  expect_identical(bytecode$names, c("helper", "server"))
  expect_identical(bytecode$index, 2:3)

  ## As the end of inst/viewer/shiny_server.R does: the file's own `server`,
  ## defined in the file's environment, with a compiled body.
  scope <- new.env(parent = globalenv())
  scope$offset <- 1
  eval(bytecode$code[[match("server", bytecode$names)]], scope)
  expect_true(any(grepl(
    "<bytecode",
    utils::capture.output(print(scope$server)),
    fixed = TRUE
  )))
  expect_identical(environment(scope$server), scope)
  expect_identical(scope$server(2), 3)
})

test_that("the Viewer server takes its byte code from the installation", {
  server_file <- readLines(viewer_test_path("shiny_server.R"))
  ## A plain compiler::cmpfun(server) at the end of the file would compile on
  ## every process start again; it may only be the fallback.
  compile <- grep("compiler::cmpfun(server)", server_file, fixed = TRUE)
  expect_length(compile, 1L)
  expect_match(server_file[[compile - 1L]], "if (is.na(", fixed = TRUE)
  expect_true(any(grepl(
    'match("server", .viewer_server_bytecode$names)',
    server_file,
    fixed = TRUE
  )))
})

test_that("viewer source reuses process-safe utility functions", {
  cache_env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("source_cache.R"), envir = cache_env)
  first <- new.env(parent = globalenv())
  second <- new.env(parent = globalenv())

  cache_env$viewerSource(viewer_test_path("utility_functions.R"), first)
  cache_env$viewerSource(viewer_test_path("utility_functions.R"), second)

  expect_identical(first$read_cerebro_file, second$read_cerebro_file)
  expect_identical(first$.cloneCachedCrb, second$.cloneCachedCrb)
  expect_identical(
    get(
      ".cloneCachedCrb",
      envir = environment(first$.attachExternalExpression),
      inherits = TRUE
    )("plain value"),
    "plain value"
  )
  expect_false(identical(
    first$extra_material_table_groups,
    second$extra_material_table_groups
  ))
  dataset <- new.env(parent = emptyenv())
  class(dataset) <- c("Cerebro", "R6")
  dataset$getExtraMaterial <- function() {
    list(tables = list(fixture = data.frame(value = 1L)))
  }
  first$data_set <- function() dataset
  expect_named(
    first$extra_material_table_groups(),
    "Embedded tables"
  )
  expect_false(identical(first$get_or_load_crb, second$get_or_load_crb))
  expect_identical(
    environment(first$get_or_load_crb),
    first
  )
  ## This helper resolves the current session's data_set() when callers omit
  ## its embedded argument, so it must not be evaluated in the process-owned
  ## shared function environment.
  first$data_set <- function() NULL
  second$data_set <- function() NULL
  expect_identical(first$extra_material_table_groups(), list())
  expect_identical(second$extra_material_table_groups(), list())
  expect_identical(environment(first$extra_material_table_groups), first)
  expect_identical(environment(second$extra_material_table_groups), second)
  expect_false(identical(first$cachePlot, second$cachePlot))
})
