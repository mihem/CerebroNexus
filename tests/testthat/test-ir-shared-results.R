## Immune repertoire results that depend only on a data set's files are shared
## by the sessions of a process; everything else stays per session.

ir_shared_scope <- function() {
  scope <- ir_scope(c("ir_dataset_process_key", "ir_process_memo"))
  scope$ir_process_cache <- new.env(parent = emptyenv())
  scope
}

test_that("a data set's process key follows its files", {
  scope <- ir_shared_scope()
  file <- withr::local_tempfile(fileext = ".crb")
  writeLines("one", file)
  ds <- structure(list(), cerebro_dataset_path = file)
  key <- scope$ir_dataset_process_key(ds)
  expect_identical(key$path, normalizePath(file, winslash = "/"))
  expect_identical(scope$ir_dataset_process_key(ds), key)
  writeLines("a longer second version", file)
  expect_false(identical(scope$ir_dataset_process_key(ds), key))
  expect_null(scope$ir_dataset_process_key(NULL))
  expect_null(scope$ir_dataset_process_key(list()))
  expect_null(scope$ir_dataset_process_key(
    structure(
      list(),
      cerebro_dataset_path = file.path(tempdir(), "missing.crb")
    )
  ))
})

test_that("process memo computes once per key and never without one", {
  scope <- ir_shared_scope()
  runs <- 0L
  compute <- function() {
    runs <<- runs + 1L
    runs
  }
  expect_identical(scope$ir_process_memo("units", list("a", 1), compute), 1L)
  expect_identical(scope$ir_process_memo("units", list("a", 1), compute), 1L)
  expect_identical(scope$ir_process_memo("units", list("b", 1), compute), 2L)
  expect_identical(scope$ir_process_memo("other", list("a", 1), compute), 3L)
  expect_identical(scope$ir_process_memo("units", NULL, compute), 4L)
  expect_identical(scope$ir_process_memo("units", NULL, compute), 5L)
})

test_that("sharing units read the metadata at the encoded cells", {
  scope <- ir_scope(c("ir_row_values", "ir_discover_sharing_units"))
  data <- list(
    a = data.frame(barcode = c("c3", "c1"), CTgene = "TRB-a"),
    empty = data.frame(barcode = character(), CTgene = character()),
    b = data.frame(barcode = "c2", CTgene = "TRB-a")
  )
  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3", "unmatched"),
    patient = c("p1", "p2", "p1", "p3"),
    site = c("t", "t", "t", "n")
  )
  encoded <- scope$ir_encode_repertoire(data, metadata$cell_barcode)
  expect_identical(
    scope$ir_discover_sharing_units(encoded, metadata),
    c("sample", "patient")
  )
})

test_that("an immune repertoire without heavy chains draws no isotype plot", {
  scope <- ir_scope(
    c(
      "bcr_isotype_counts",
      "IR_ISOTYPES",
      "ir_isotype_colors",
      "bcr_isotype_plot"
    ),
    "server.R"
  )
  scope$cerebro_group_colors <- function(n) rep("#000000", n)
  tcr <- scope$ir_encode_repertoire(list(
    a = data.frame(barcode = "c1", CTgene = "TRAV1.TRAJ2_TRBV3.TRBJ4"),
    b = data.frame(barcode = "c2", CTgene = NA_character_)
  ))
  expect_null(scope$bcr_isotype_plot(tcr, c("a", "b")))
  bcr <- scope$ir_encode_repertoire(list(
    a = data.frame(barcode = "c1", CTgene = "IGHV1.IGHJ2.IGHG1")
  ))
  expect_s3_class(scope$bcr_isotype_plot(bcr, "a"), "ggplot")
})

test_that("isotype bars hold every group's isotype shares and counts", {
  scope <- ir_scope(
    c("bcr_isotype_counts", "IR_ISOTYPES", "ir_isotype_colors"),
    "server.R"
  )
  scope <- ir_scope(
    c(
      "ir_chart_message",
      "ir_fill_bar_chart",
      "ir_isotype_chart"
    ),
    "visualizations.R",
    scope = scope
  )
  scope$cerebro_group_colors <- function(n) sprintf("#%06d", seq_len(n))
  expect_identical(scope$ir_isotype_chart(NULL, "sample")$type, "empty")
  iso <- data.frame(
    patient = c("p2", "p2", "p1", "p1"),
    isotype = c("IGHG1", "IGHX", "IGHM", "IGHG1"),
    n = c(3L, 1L, 2L, 2L)
  )
  chart <- scope$ir_isotype_chart(iso, "patient")
  expect_identical(as.vector(chart$categories), c("p2", "p1"))
  expect_identical(
    as.vector(chart$series),
    c(scope$IR_ISOTYPES, "NA")
  )
  expect_identical(as.vector(chart$colors)[[10L]], "#cccccc")
  series <- function(name, field) {
    as.vector(chart[[field]][[match(name, chart$series)]])
  }
  expect_equal(series("IGHG1", "values"), c(.75, .5))
  expect_equal(series("IGHM", "values"), c(0, .5))
  expect_equal(series("NA", "values"), c(.25, 0))
  expect_equal(series("IGHG1", "counts"), c(3, 2))
  expect_true(chart$percent)
  expect_identical(chart$x_title, "patient")
  ## Only known isotypes: no NA series.
  chart <- scope$ir_isotype_chart(iso[iso$isotype != "IGHX", ], "patient")
  expect_identical(as.vector(chart$series), scope$IR_ISOTYPES)
  ## The help's isotype plot draws the same isotypes in the same colours.
  scope <- ir_scope("bcr_isotype_plot", "server.R", scope = scope)
  bcr <- scope$ir_encode_repertoire(list(
    a = data.frame(barcode = "c1", CTgene = "IGHV1.IGHJ2.IGHG1")
  ))
  fill <- scope$bcr_isotype_plot(bcr, "a")$scales$get_scales("fill")
  expect_identical(
    unname(fill$palette(length(scope$IR_ISOTYPES))),
    as.vector(chart$colors)
  )
  expect_identical(fill$na.value, "#cccccc")
})

test_that("Abundance lines match plot_ly's split and colour mapping", {
  scope <- ir_scope(
    c(
      "ir_chart_message",
      "ir_clonal_abundance_chart",
      "ir_abundance_units",
      "ir_abundance_density"
    ),
    "visualizations.R"
  )
  for (n_samples in c(2L, 5L, 12L)) {
    samples <- sprintf("s%02d", rev(seq_len(n_samples)))
    counts <- data.frame(
      sample = rep(samples, each = 2L),
      abundance = rep(c(3L, 1L), n_samples),
      n_clones = seq_len(2L * n_samples)
    )
    chart <- scope$ir_clonal_abundance_chart(counts, legend = "patient")
    expected <- suppressWarnings(plotly::plotly_build(plotly::plot_ly(
      counts,
      x = ~abundance,
      y = ~n_clones,
      split = ~sample,
      color = ~sample,
      type = "scatter",
      mode = "lines"
    )))$x$data
    expect_identical(
      as.vector(chart$series),
      vapply(expected, `[[`, "", "name")
    )
    expect_identical(
      plotly::toRGB(as.vector(chart$colors)),
      vapply(expected, function(t) as.character(t$line$color), "")
    )
    ## Each line runs along increasing abundance.
    for (i in seq_along(chart$series)) {
      rows <- counts[counts$sample == chart$series[[i]], ]
      rows <- rows[order(rows$abundance), ]
      expect_equal(as.vector(chart$x[[i]]), rows$abundance)
      expect_equal(as.vector(chart$y[[i]]), rows$n_clones)
    }
    expect_true(chart$x_log)
    expect_false(chart$fill)
    expect_identical(chart$hover, "closest")
    expect_identical(chart$y_format, "integer")
    expect_identical(chart$legend_title, "patient")
  }
  scaled <- scope$ir_clonal_abundance_chart(counts, scale = TRUE)
  expect_true(scaled$fill)
  expect_identical(scaled$y_title, "Density of clones")
  expect_true(all(vapply(scaled$y, max, 0) == 1))
  expect_true(all(lengths(scaled$x) == 512L))
  ordered <- scope$ir_clonal_abundance_chart(
    data.frame(sample = c("b10", "b2"), abundance = 1L, n_clones = 1L),
    order_by = "alphanumeric"
  )
  expect_identical(as.vector(ordered$series), c("b2", "b10"))
  empty <- scope$ir_clonal_abundance_chart(counts[0, ])
  expect_identical(empty$type, "empty")
})

test_that("sessions opening the same data set files share IR plots", {
  render_value <- function(expr, ...) {
    shiny::reactive(substitute(expr), env = parent.frame(), quoted = TRUE)
  }
  calls <- 0L
  file <- withr::local_tempfile(fileext = ".crb")
  writeLines("one", file)
  path <- file
  ## testServer() gives every mock session its own "app" cache, so stand in
  ## for the process's app cache with one shared object.
  app_cache <- cachem::cache_mem()
  server <- function(input, output, session) {
    scope <- environment()
    output <- new.env()
    `%>%` <- dplyr::`%>%`
    renderCerebroChart <- render_value
    available_crb_files <- reactiveValues(selected = "A")
    data_set <- reactive(structure(list(), cerebro_dataset_path = path))
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "server.R"),
      scope,
      names = c(
        "ir_dataset_cache_key",
        "ir_plot_cache_state",
        "ir_plot_cache",
        "ir_bindCache"
      )
    )
    expect_identical(formals(ir_bindCache)$cache, "app")
    shared_bind_cache <- ir_bindCache
    ir_bindCache <- function(x, ...) {
      shared_bind_cache(x, ..., cache = app_cache)
    }
    sys.source(
      viewer_test_path("immune_repertoire", "param_spec.R"),
      envir = scope
    )
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "data.R"),
      scope,
      names = c(
        "ir_dataset_process_key",
        "ir_params",
        "ir_param",
        "ir_param_default",
        "ir_order_by",
        "IR_CLONE_SIZE_NAMES",
        "IR_CLONE_SIZE_DEFAULT",
        "ir_clone_size"
      )
    )
    req_plot_space <- function(...) NULL
    req_scRepertoire <- function() NULL
    getGroups <- function() "sample"
    ir_encoded <- function() list(samples = "sample")
    ir_row_values <- function(...) NULL
    ## The counted clones stand for the plot's work.
    ir_clonecall_col <- function(x) "CTgene"
    ir_homeostasis_counts <- function(...) {
      calls <<- calls + 1L
      data.frame(sample = "sample", abundance = 1L, n_clones = 1L)
    }
    ir_homeostasis_chart <- function(...) list(type = "bar")
    ir_render_chart <- function(expr, ...) expr
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "visualizations.R"),
      scope,
      names = c("IR_PALETTE", "output$ir_plot_clonalHomeostasis")
    )
  }
  draw <- function() {
    shiny::testServer(server, {
      session$setInputs(
        ir_cloneCall = "gene",
        ir_chain = "TRB",
        ir_groupBy = ""
      )
      scope$output$ir_plot_clonalHomeostasis()
    })
  }
  draw()
  expect_identical(calls, 1L)
  draw()
  expect_identical(calls, 1L)
  writeLines("a longer second version", file)
  draw()
  expect_identical(calls, 2L)
  ## Without a file on disk, each session keeps its own plots.
  path <- NULL
  draw()
  draw()
  expect_identical(calls, 4L)
})

test_that("IR plots that show an error are not shared with other sessions", {
  render_value <- function(expr, ...) {
    shiny::reactive(substitute(expr), env = parent.frame(), quoted = TRUE)
  }
  calls <- 0L
  fail <- TRUE
  file <- withr::local_tempfile(fileext = ".crb")
  writeLines("one", file)
  app_cache <- cachem::cache_mem()
  server <- function(input, output, session) {
    scope <- environment()
    output <- new.env()
    `%>%` <- dplyr::`%>%`
    renderCerebroChart <- render_value
    available_crb_files <- reactiveValues(selected = "A")
    data_set <- reactive(structure(list(), cerebro_dataset_path = file))
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "server.R"),
      scope,
      names = c(
        "ir_dataset_cache_key",
        "ir_plot_cache_state",
        "ir_plot_failed",
        "ir_plot_cache",
        "ir_bindCache",
        "safeRenderPlot"
      )
    )
    shared_bind_cache <- ir_bindCache
    ir_bindCache <- function(x, ...) {
      shared_bind_cache(x, ..., cache = app_cache)
    }
    sys.source(
      viewer_test_path("immune_repertoire", "param_spec.R"),
      envir = scope
    )
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "data.R"),
      scope,
      names = c(
        "ir_dataset_process_key",
        "ir_params",
        "ir_param",
        "ir_param_default",
        "ir_order_by",
        "IR_CLONE_SIZE_NAMES",
        "IR_CLONE_SIZE_DEFAULT",
        "ir_clone_size"
      )
    )
    req_plot_space <- function(...) NULL
    req_scRepertoire <- function() NULL
    getGroups <- function() "sample"
    ir_encoded <- function() list(samples = "sample")
    ir_row_values <- function(...) NULL
    ir_clonecall_col <- function(x) "CTgene"
    ir_homeostasis_counts <- function(...) {
      calls <<- calls + 1L
      if (fail) {
        stop("disk unavailable")
      }
      data.frame(sample = "sample", abundance = 1L, n_clones = 1L)
    }
    ir_homeostasis_chart <- function(...) list(type = "bar")
    viewer_server_load(
      viewer_server_statements("immune_repertoire", "visualizations.R"),
      scope,
      names = c(
        "IR_PALETTE",
        "ir_chart_message",
        "ir_render_chart",
        "output$ir_plot_clonalHomeostasis"
      )
    )
  }
  draw <- function() {
    shiny::testServer(server, {
      session$setInputs(
        ir_cloneCall = "gene",
        ir_chain = "TRB",
        ir_groupBy = ""
      )
      plot <<- scope$output$ir_plot_clonalHomeostasis()
    })
  }
  ## The error message is shown, and the next session draws again.
  plot <- NULL
  suppressMessages(draw())
  expect_identical(plot$type, "empty")
  expect_match(plot$message, "disk unavailable", fixed = TRUE)
  suppressMessages(draw())
  expect_identical(calls, 2L)
  ## A drawn plot is shared.
  fail <- FALSE
  draw()
  draw()
  expect_identical(calls, 3L)
})

test_that("grouping values come from the Pack's first-frame metadata", {
  scope <- ir_scope("ir_cell_metadata")
  first_frame <- data.frame(patient = c("p1", "p2", "p1"))
  pack <- list(canonical_order = TRUE)
  scope$viewerProjectionFirstFrameCache <- function() {
    list(meta_data = first_frame)
  }
  scope$viewerPackCurrent <- function() pack
  scope$getMetaData <- function() stop("unexpected metadata hydration")
  expect_identical(scope$ir_cell_metadata(), first_frame)
  ## Without a Pack, or its first frame, the full table.
  hydrated <- data.frame(cell_barcode = c("c1", "c2", "c3"), patient = "p")
  scope$getMetaData <- function() hydrated
  pack <- NULL
  expect_identical(scope$ir_cell_metadata(), hydrated)
  pack <- list()
  scope$viewerProjectionFirstFrameCache <- function() NULL
  expect_identical(scope$ir_cell_metadata(), hydrated)
})
