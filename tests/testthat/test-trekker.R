# test-trekker.R — Trekker single-cell spatial-mapping page.
#
# Covers the parts of the feature that are pure and don't need a browser: the
# Cerebro `trekker` slot round-trip (which also gates whether the tab
# appears), and the two pure helpers that drive the gene picker and the
# meta-field colouring. The pure helpers are sourced from the same inst/ file the
# app sources at runtime (see helper-trekker-helpers.R).

# ---- R6 slot: addTrekker / getTrekker round-trip -------------------------- ##

test_that("getTrekker defaults to NULL so the tab stays hidden", {
  obj <- Cerebro$new()
  # The tab is inserted only when getTrekker() is non-NULL, so a data set
  # without Trekker content keeps it hidden.
  expect_null(obj$getTrekker())
})

test_that("addTrekker stores the payload and getTrekker returns it verbatim", {
  obj <- Cerebro$new()
  payload <- list(
    barcodes = c("AAA", "CCC"),
    clusters = c(0L, 1L),
    moran = list(list(gene = "Plp1"), list(gene = "Mbp"))
  )
  obj$addTrekker(payload)
  # Non-NULL slot is exactly the condition that makes the Trekker tab appear.
  expect_false(is.null(obj$getTrekker()))
  expect_identical(obj$getTrekker(), payload)
})

test_that("addTrekker rejects a non-list", {
  obj <- Cerebro$new()
  expect_error(obj$addTrekker(42), "must be a list")
  expect_null(obj$getTrekker())
})

# ---- trekker_gene_suggest ------------------------------------------------- ##

test_that("trekker_gene_suggest keeps Moran + marker genes present in the matrix", {
  tk <- list(moran = list(list(gene = "Dgkb"), list(gene = "Ghost_gene")))
  gene_names <- c("Dgkb", "Plp1", "Mbp", "Some_other_gene")

  out <- trekker_gene_suggest(tk, gene_names)

  expect_true("Dgkb" %in% out) # a Moran gene that is measured
  expect_true("Plp1" %in% out) # a canonical marker that is measured
  expect_false("Ghost_gene" %in% out) # Moran gene absent from the matrix -> dropped
  expect_true(all(out %in% gene_names)) # never suggests an unmeasured gene
})

test_that("trekker_gene_suggest tolerates an empty Moran list", {
  tk <- list(moran = list())
  out <- trekker_gene_suggest(tk, c("Plp1", "Gad1", "not_a_marker"))
  expect_setequal(out, c("Plp1", "Gad1"))
})

# ---- trekker_numeric_meta_cols -------------------------------------------- ##

test_that("trekker_numeric_meta_cols keeps only numeric, non-constant columns", {
  meta <- data.frame(
    myelination = c(-0.3, 0.5, 1.8),
    percent_mt = c(1.0, 2.0, 3.0),
    constant = c(2, 2, 2),
    cell_type = c("ExN", "InN", "Oligo"),
    stringsAsFactors = FALSE
  )

  out <- trekker_numeric_meta_cols(meta)

  expect_setequal(out, c("myelination", "percent_mt"))
  expect_false("constant" %in% out) # zero-variance dropped (nothing to colour by)
  expect_false("cell_type" %in% out) # non-numeric dropped
})

test_that("trekker_numeric_meta_cols returns empty for NULL / empty input", {
  expect_identical(trekker_numeric_meta_cols(NULL), character(0))
  expect_identical(trekker_numeric_meta_cols(list()), character(0))
})

test_that("trekker_numeric_meta_cols ignores NA when judging constancy", {
  meta <- data.frame(
    all_na = c(NA_real_, NA_real_, NA_real_),
    one_value = c(5, NA, 5),
    varying = c(1, NA, 9)
  )
  out <- trekker_numeric_meta_cols(meta)
  expect_setequal(out, "varying")
})

test_that("Trekker only prepares payloads for a visible, current dataset", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      reads <- 0L
      row_reads <- 0L
      row_error <- NULL
      sent <- list()
      context <- list(epoch = "session", dataset_key = "tiny", generation = 1L)
      viewer_loaded_dataset_context <- function() context
      tk <- list(
        barcodes = c("a", "b"),
        clusters = c(0L, 1L),
        celltype = c("T", "B"),
        x = c(2, 4),
        y = c(3, 5),
        ux = c(-1, 1),
        uy = c(1, -1)
      )
      data_set <- function() {
        list(
          getTrekker = function() {
            reads <<- reads + 1L
            tk
          },
          getExpressionRow = function(...) {
            row_reads <<- row_reads + 1L
            tryCatch(
              .checkThinCrbCells(
                rev(tk$barcodes),
                list(cell_names_hash = .runtimeCellNamesHash(tk$barcodes)),
                "trekker.crb"
              ),
              error = function(error) {
                row_error <<- error
                stop(error)
              }
            )
          }
        )
      }
      trekker_gene_names <- function() "g1"
      current_scatter_defaults <- function() {
        list(point_size = 3, point_opacity = 1, percentage_cells_to_show = 100)
      }
      cerebro_group_colors <- function(n) c("red", "blue")[seq_len(n)]
      cerebroCellViewRender <- function(...) {
        sent[[length(sent) + 1L]] <<- list(...)
      }
      viewer_server_load(
        viewer_server_statements("trekker", "server.R"),
        scope,
        names = "trekker_slot",
        events = "trekker_projection_render_request"
      )
    },
    {
      request <- list(dataset_context = context)
      session$setInputs(
        sidebar = "overview",
        trekker_projection_render_request = request
      )
      expect_identical(scope$reads, 0L)
      expect_length(scope$sent, 0L)
      session$setInputs(
        sidebar = "trekker",
        trekker_projection_render_request = NULL
      )
      expect_identical(scope$reads, 0L)
      session$setInputs(
        trekker_projection_render_request = list(
          dataset_context = list(
            epoch = "session",
            dataset_key = "old",
            generation = 0L
          )
        )
      )
      expect_identical(scope$reads, 0L)
      session$setInputs(trekker_projection_render_request = request)
      expect_identical(scope$reads, 1L)
      expect_length(scope$sent, 1L)
      expect_identical(scope$sent[[1]]$data$selection_key, c("a", "b"))
      expect_identical(scope$sent[[1]]$data$panels[[1]]$x, c(2, 4))
      expect_identical(scope$sent[[1]]$data$panels[[2]]$x, c(-1, 1))
      session$setInputs(sidebar = "overview", trekker_view = "sp")
      expect_length(scope$sent, 1L)
      session$setInputs(sidebar = "trekker")
      expect_length(scope$sent, 2L)
      expect_length(scope$sent[[2]]$data$panels, 1L)
      expect_identical(scope$reads, 1L)
      ## An invalid sidecar must not be turned into a neutral/zero gene paint
      ## by the page's generic missing-expression fallback.
      expect_no_error(session$setInputs(
        trekker_mode = "gene",
        trekker_gene_pick = "g1"
      ))
      expect_false(session$isClosed())
      expect_identical(scope$row_reads, 1L)
      expect_s3_class(scope$row_error, "shiny.silent.error")
      expect_length(scope$sent, 2L)
    }
  )
})
