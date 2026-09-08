sync_perf_root_candidates <- c(
  file.path(getwd(), "inst", "viewer"),
  file.path(getwd(), "..", "..", "inst", "viewer"),
  testthat::test_path("..", "..", "inst", "viewer")
)
sync_perf_viewer_root <- sync_perf_root_candidates[
  dir.exists(sync_perf_root_candidates)
][1L]
if (is.na(sync_perf_viewer_root)) {
  sync_perf_viewer_root <- system.file("viewer", package = "CerebroNexus")
}

read_sync_perf_viewer <- function(...) {
  paste(
    readLines(file.path(sync_perf_viewer_root, ...), warn = FALSE),
    collapse = "\n"
  )
}

sync_perf_reactive <- function(value) {
  expression <- substitute(value)
  scope <- parent.frame()
  function() eval(expression, envir = scope)
}

find_sync_perf_assignment <- function(expression, name) {
  if (
    is.call(expression) &&
      identical(expression[[1L]], quote(`<-`)) &&
      identical(expression[[2L]], as.name(name))
  ) {
    return(expression[[3L]])
  }
  if (!is.recursive(expression)) {
    return(NULL)
  }
  for (index in seq_along(expression)) {
    if (identical(expression[[index]], quote(expr = ))) {
      next
    }
    part <- expression[[index]]
    found <- find_sync_perf_assignment(part, name)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

run_sync_perf_cell_sampling <- function(view, metadata, filters, percentage) {
  utility <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility
  )

  sample_calls <- list()
  mask_calls <- 0L
  scope <- new.env(parent = globalenv())
  scope$reactive <- sync_perf_reactive
  scope$req <- shiny::req
  scope$input <- c(
    stats::setNames(
      list(percentage),
      paste0(view$prefix, "_percentage_cells_to_show")
    ),
    stats::setNames(
      filters,
      paste0(view$prefix, "_group_filter_", names(filters))
    )
  )
  scope$getGroups <- function() names(filters)
  scope$getGroupLevels <- function(group) unique(metadata[[group]])
  scope$getMetaData <- function() metadata
  scope$cerebroGroupFilterMask <- function(metadata, filters) {
    mask_calls <<- mask_calls + 1L
    utility$cerebroGroupFilterMask(metadata, filters)
  }
  scope$viewerProjectionCellIndices <- utility$viewerProjectionCellIndices
  environment(scope$viewerProjectionCellIndices) <- scope
  scope$sample <- function(x, size, replace = FALSE, prob = NULL) {
    sample_calls[[length(sample_calls) + 1L]] <<- "sample"
    if (missing(size)) {
      return(base::sample(x, replace = replace, prob = prob))
    }
    base::sample(x, size, replace = replace, prob = prob)
  }
  scope$sample.int <- function(n, size, replace = FALSE, prob = NULL) {
    sample_calls[[length(sample_calls) + 1L]] <<- "sample.int"
    base::sample.int(n, size, replace = replace, prob = prob)
  }
  scope$randomlySubsetCells <- utility$randomlySubsetCells
  environment(scope$randomlySubsetCells) <- scope

  sys.source(
    file.path(sync_perf_viewer_root, view$file),
    envir = scope
  )
  list(
    cells = scope[[view$reactive]](),
    sample_calls = unlist(sample_calls, use.names = FALSE),
    mask_calls = mask_calls
  )
}

test_that("the first reactive value bypasses debounce", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  expect_true(is.function(utility_env$debounceAfterFirst))

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
    expect_identical(compute_count, 1L)

    session$setInputs(value = "second")
    expect_identical(ready(), "first")
    expect_identical(compute_count, 2L)
  })
})

test_that("event debounce coalesces expensive work after the first value", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  expect_true(is.function(utility_env$debounceEventAfterFirst))

  compute_count <- 0L
  server <- function(input, output, session) {
    event <- shiny::reactive(input$value)
    value <- shiny::reactive({
      compute_count <<- compute_count + 1L
      input$value
    })
    ready <- utility_env$debounceEventAfterFirst(event, value, 100)
  }

  shiny::testServer(server, {
    session$setInputs(value = "first")
    expect_identical(ready(), "first")
    expect_identical(compute_count, 1L)

    session$setInputs(value = "second")
    session$setInputs(value = "third")
    session$elapse(101)
    expect_identical(ready(), "third")
    expect_identical(compute_count, 2L)
  })
})

test_that("expression rows are fetched once and aligned by cell", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
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
    file.path(sync_perf_viewer_root, "utility_functions.R"),
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

test_that("already aligned expression cells skip the string match", {
  match_lengths <- integer()
  utility_env <- new.env(parent = globalenv())
  utility_env$match <- function(x, table, ...) {
    match_lengths <<- c(match_lengths, length(x))
    base::match(x, table, ...)
  }
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
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

test_that("Viewer hidden outputs activate on their first owning tab visit", {
  server <- read_sync_perf_viewer("shiny_server.R")
  group_filters <- read_sync_perf_viewer(
    "module",
    "group_filters",
    "group_filters_widget.R"
  )

  expect_match(server, "viewer_hidden_output_options", fixed = TRUE)
  expect_match(server, 'input[["sidebar"]]', fixed = TRUE)
  expect_match(server, "viewerOutputTab", fixed = TRUE)
  expect_no_match(server, "viewer_deferred_output_spacing_ms", fixed = TRUE)
  expect_no_match(server, "delay = 1", fixed = TRUE)
  expect_no_match(server, "cerebro_async", fixed = TRUE)
  expect_match(group_filters, 'get0("outputOptions"', fixed = TRUE)
})

test_that("hidden output IDs resolve to their owning sidebar tabs", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  expect_identical(
    utility_env$viewerOutputTab(c(
      "overview_projection_point_border_UI",
      "expression_projection_data_parameters_UI",
      "spatial_projection_main_parameters_UI",
      "coordviews_image_ui",
      "ir_display_panel",
      "trajectory_projection_group_labels_UI",
      "trekker_main_parameters_ui",
      "hla_more_parameters_ui",
      "unknown_output"
    )),
    c(
      "overview",
      "geneExpression",
      "spatial",
      "coordinated_views",
      "immune_repertoire",
      "trajectory",
      "trekker",
      "hla_tcr_motifs",
      NA_character_
    )
  )
})

test_that("visible projections keep debounce after their first render", {
  paths <- c(
    "overview/obj_projection_data_to_plot.R",
    "gene_expression/obj_projection_data_to_plot.R",
    "spatial/obj_projection_data_to_plot.R"
  )

  for (path in paths) {
    expect_match(
      read_sync_perf_viewer(path),
      "debounceEventAfterFirst",
      fixed = TRUE,
      info = path
    )
  }
})

test_that("RGB and linked expression use batched reads", {
  gene_expression <- read_sync_perf_viewer(
    "gene_expression",
    "obj_projection_expression_levels.R"
  )
  spatial <- read_sync_perf_viewer(
    "spatial",
    "obj_projection_data_to_plot.R"
  )
  linked <- read_sync_perf_viewer("coordinated_views", "server.R")

  expect_match(gene_expression, "viewerExpressionValues", fixed = TRUE)
  expect_match(spatial, "viewerExpressionValues", fixed = TRUE)
  expect_match(linked, "cv_gene_values_many", fixed = TRUE)
  expect_match(linked, "viewerExpressionValues", fixed = TRUE)
})

test_that("separate gene panels do not transpose the expression matrix", {
  gene_expression <- read_sync_perf_viewer(
    "gene_expression",
    "obj_projection_expression_levels.R"
  )

  expect_no_match(gene_expression, "Matrix::t", fixed = TRUE)
})

test_that("Spatial full extents are memoized by a session reactive", {
  spatial <- read_sync_perf_viewer(
    "spatial",
    "obj_projection_data_to_plot.R"
  )

  expect_match(
    spatial,
    "spatial_projection_full_extent <- cachePlot(",
    fixed = TRUE
  )
  expect_match(
    spatial,
    "extent <- spatial_projection_full_extent()",
    fixed = TRUE
  )
})

test_that("session cache reuses a Spatial-style A-B-A key", {
  utility_env <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility_env
  )
  compute_count <- 0L
  server <- function(input, output, session) {
    extent <- utility_env$cachePlot(
      shiny::reactive({
        compute_count <<- compute_count + 1L
        input$key
      }),
      input$key
    )
  }

  shiny::testServer(server, {
    session$setInputs(key = "A")
    expect_identical(extent(), "A")
    session$setInputs(key = "B")
    expect_identical(extent(), "B")
    session$setInputs(key = "A")
    expect_identical(extent(), "A")
    expect_identical(compute_count, 2L)
  })
})

test_that("single-gene Viewer routes prefer row extraction", {
  paths <- c(
    "gene_expression/obj_projection_expression_levels.R",
    "spatial/obj_projection_data_to_plot.R",
    "spatial/out_morans_i.R",
    "trekker/server.R"
  )
  for (path in paths) {
    expect_match(
      read_sync_perf_viewer(path),
      "viewerExpressionRow",
      fixed = TRUE,
      info = path
    )
  }
})

test_that("Viewer source parsing is cached but evaluation remains per session", {
  source_cache <- file.path(sync_perf_viewer_root, "source_cache.R")
  expect_true(file.exists(source_cache))
  cache_env <- new.env(parent = globalenv())
  sys.source(source_cache, envir = cache_env)

  source_file <- tempfile(fileext = ".R")
  writeLines("value <- 1L", source_file)
  first <- new.env(parent = baseenv())
  second <- new.env(parent = baseenv())
  cache_env$viewerSource(source_file, first)
  cache_env$viewerSource(source_file, second)
  expect_identical(first$value, 1L)
  expect_identical(second$value, 1L)
  expect_identical(length(cache_env$.viewer_source_cache), 1L)

  writeLines("value <- 200L", source_file)
  third <- new.env(parent = baseenv())
  cache_env$viewerSource(source_file, third)
  expect_identical(third$value, 200L)
})

test_that("specialist cell sampling uses one original-row index sample", {
  views <- list(
    overview = list(
      file = "overview/obj_projection_cells_to_show.R",
      prefix = "overview_projection",
      reactive = "overview_projection_cells_to_show"
    ),
    gene = list(
      file = "gene_expression/obj_projection_cells_to_show.R",
      prefix = "expression_projection",
      reactive = "expression_projection_cells_to_show"
    ),
    spatial = list(
      file = "spatial/obj_projection_cells_to_show.R",
      prefix = "spatial_projection",
      reactive = "spatial_projection_cells_to_show"
    )
  )
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

  for (name in names(views)) {
    result <- run_sync_perf_cell_sampling(
      views[[name]],
      metadata,
      filters = list(batch = "keep", state = c("T", "B")),
      percentage = 50
    )
    expect_identical(
      result$sample_calls,
      "sample.int",
      info = name
    )
    expect_identical(result$mask_calls, 1L, info = name)
    expect_equal(
      length(result$cells),
      ceiling(length(eligible) * 0.5),
      info = name
    )
    expect_true(all(result$cells %in% eligible), info = name)
    expect_equal(
      length(unique(result$cells)),
      length(result$cells),
      info = name
    )
  }

  all_cells <- run_sync_perf_cell_sampling(
    views$overview,
    metadata,
    filters = list(batch = c("drop", "keep"), state = c("T", "B")),
    percentage = 100
  )
  expect_identical(all_cells$cells, seq_len(nrow(metadata)))
  expect_length(all_cells$sample_calls, 0L)
  expect_identical(all_cells$mask_calls, 0L)

  character_missing <- run_sync_perf_cell_sampling(
    views$overview,
    data.frame(
      cell_barcode = c("cell1", "cell2"),
      batch = c("keep", NA_character_)
    ),
    filters = list(batch = "keep"),
    percentage = 100
  )
  expect_identical(character_missing$cells, 1L)
  expect_identical(character_missing$mask_calls, 1L)

  no_cells <- run_sync_perf_cell_sampling(
    views$gene,
    metadata,
    filters = list(batch = character(), state = c("T", "B")),
    percentage = 50
  )
  expect_identical(no_cells$cells, integer())
  expect_length(no_cells$sample_calls, 0L)

  scalar_metadata <- data.frame(
    cell_barcode = paste0("cell", seq_len(50L)),
    batch = replace(rep("drop", 50L), 42L, "keep")
  )
  scalar <- run_sync_perf_cell_sampling(
    views$spatial,
    scalar_metadata,
    filters = list(batch = "keep"),
    percentage = 50
  )
  expect_identical(scalar$cells, 42L)
  expect_identical(scalar$sample_calls, "sample.int")

  for (view in views) {
    source <- read_sync_perf_viewer(view$file)
    expect_match(source, "viewerProjectionCellIndices", fixed = TRUE)
    expect_no_match(source, "cerebroGroupFilterMask", fixed = TRUE)
    expect_no_match(source, "sample.int", fixed = TRUE)
    expect_no_match(source, "dplyr::mutate", fixed = TRUE)
    expect_no_match(source, "dplyr::select", fixed = TRUE)
    expect_no_match(source, "randomlySubsetCells", fixed = TRUE)
  }
})

test_that("projection hover packages an existing metadata subset", {
  expressions <- parse(file.path(sync_perf_viewer_root, "shiny_server.R"))
  definition <- NULL
  for (expression in expressions) {
    definition <- find_sync_perf_assignment(
      expression,
      "hover_info_projections"
    )
    if (!is.null(definition)) {
      break
    }
  }
  is_plain_function <- is.call(definition) &&
    identical(definition[[1L]], quote(`function`))
  expect_true(is_plain_function)

  if (is_plain_function) {
    metadata <- data.frame(
      cell_barcode = paste0("cell", seq_len(5L)),
      nUMI = seq_len(5L) * 10L,
      nGene = seq_len(5L),
      group = LETTERS[seq_len(5L)]
    )
    formatted <- NULL
    format_calls <- 0L
    scope <- list2env(list(
      preferences = list(show_hover_info_in_projections = TRUE),
      buildHoverDataForProjections = function(cells) {
        format_calls <<- format_calls + 1L
        formatted <<- cells
        list(enabled = TRUE, selection_key = I(cells$cell_barcode))
      }
    ))
    hover <- eval(definition, envir = scope)

    displayed <- metadata[c(4L, 2L), , drop = FALSE]
    result <- hover(displayed)
    expect_identical(formatted, displayed)
    expect_true(result$enabled)
    expect_identical(as.character(result$selection_key), c("cell4", "cell2"))
    expect_identical(format_calls, 1L)

    scope$preferences[["show_hover_info_in_projections"]] <- FALSE
    expect_identical(
      hover(metadata[c(1L, 3L), , drop = FALSE]),
      list(enabled = FALSE)
    )
    expect_identical(format_calls, 1L)
  }
})

test_that("projection hover text is assembled in one vectorized pass", {
  utility <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility
  )
  utility$getGroups <- function() c("sample", "cell_type")
  metadata <- data.frame(
    cell_barcode = c("cell1", "cell2"),
    nUMI = c(1200, 34567),
    nGene = c(800, 9012),
    sample = c("A", "B"),
    cell_type = c("T", "B")
  )

  expect_identical(
    as.character(utility$buildHoverInfoForProjections(metadata)),
    c(
      paste0(
        "<b>Cell</b>: cell1<br><b>Transcripts</b>: 1,200",
        "<br><b>Expressed genes</b>: 800",
        "<br><b>sample</b>: A<br><b>cell_type</b>: T"
      ),
      paste0(
        "<b>Cell</b>: cell2<br><b>Transcripts</b>: 34,567",
        "<br><b>Expressed genes</b>: 9,012",
        "<br><b>sample</b>: B<br><b>cell_type</b>: B"
      )
    )
  )
  expect_identical(
    utility$buildHoverInfoForProjections(metadata[FALSE, , drop = FALSE]),
    character()
  )
  source <- read_sync_perf_viewer("utility_functions.R")
  expect_match(source, "do.call(paste0, parts)", fixed = TRUE)
  expect_no_match(source, "hover_info <- glue::glue", fixed = TRUE)
})

test_that("Canvas projection hover stays columnar", {
  utility <- new.env(parent = globalenv())
  sys.source(
    file.path(sync_perf_viewer_root, "utility_functions.R"),
    envir = utility
  )
  utility$getGroups <- function() c("sample", "cell_type")
  metadata <- data.frame(
    cell_barcode = c("cell1", "cell2"),
    nUMI = c(1200, 34567),
    nGene = c(800, 9012),
    sample = c("A", "B"),
    cell_type = c("T", "B")
  )

  hover <- utility$buildHoverDataForProjections(metadata)
  expect_true(hover$enabled)
  expect_identical(as.character(hover$selection_key), metadata$cell_barcode)
  expect_identical(
    lapply(hover$fields, function(field) unclass(field$values)),
    list(metadata$nUMI, metadata$nGene)
  )
  expect_identical(
    lapply(hover$groups, `[[`, "label"),
    c("sample", "cell_type")
  )
  expect_identical(unclass(hover$groups[[1L]]$values), 0:1)
})

test_that("Linked views reuses the saved-view fingerprint", {
  server <- read_sync_perf_viewer("coordinated_views", "server.R")
  expect_match(
    server,
    "cv_build_bundle(data_set(), dataset$fingerprint)",
    fixed = TRUE
  )
  expect_no_match(
    server,
    "cv_config_cell_fingerprint(b$cells)",
    fixed = TRUE
  )
})

test_that("specialist hover consumers reuse their metadata subset", {
  cases <- list(
    overview = list(
      file = "overview/obj_projection_hover_info.R",
      data = "overview_projection_data",
      hover = "overview_projection_hover_info",
      value = integer()
    ),
    gene = list(
      file = "gene_expression/obj_projection_hover_info.R",
      data = "expression_projection_data",
      hover = "expression_projection_hover_info",
      value = c(5L, 2L)
    ),
    spatial = list(
      file = "spatial/obj_projection_hover_info.R",
      data = "spatial_projection_metadata",
      hover = "spatial_projection_hover_info",
      value = c(9L, 3L)
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    data_calls <- 0L
    helper_calls <- 0L
    requested <- NULL
    scope <- new.env(parent = globalenv())
    scope$reactive <- sync_perf_reactive
    scope$req <- shiny::req
    scope$preferences <- list(show_hover_info_in_projections = TRUE)
    displayed <- data.frame(
      cell_barcode = sprintf("cell%d", case$value),
      value = case$value
    )
    scope[[case$data]] <- function() {
      data_calls <<- data_calls + 1L
      displayed
    }
    scope$hover_info_projections <- function(cells_df) {
      helper_calls <<- helper_calls + 1L
      requested <<- cells_df
      if (!nrow(cells_df)) {
        return(stats::setNames(character(), character()))
      }
      stats::setNames(
        paste0("hover-", cells_df$value),
        cells_df$cell_barcode
      )
    }

    sys.source(file.path(sync_perf_viewer_root, case$file), envir = scope)
    result <- scope[[case$hover]]()

    expect_identical(data_calls, 1L, info = name)
    expect_identical(helper_calls, 1L, info = name)
    expect_identical(requested, displayed, info = name)
    expected <- if (length(case$value)) {
      stats::setNames(
        paste0("hover-", case$value),
        paste0("cell", case$value)
      )
    } else {
      stats::setNames(character(), character())
    }
    expect_identical(result, expected, info = name)
  }
})
