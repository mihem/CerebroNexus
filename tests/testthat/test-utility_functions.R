## Unit tests for helper functions defined in
## inst/viewer/utility_functions.R.
##
## These functions live in the Shiny app tree (not the package R/ namespace),
## so they are loaded by sourcing the file into a throw-away environment. They
## are pure R and require neither a running app nor Seurat.
##
## Coverage focuses on the edge cases that previously crashed the app at
## runtime (NA-only percentage columns, NULL/NA toggle inputs, a missing
## grouping column) plus the caching contract of the cachePlot() wrapper. See
## the git history of utility_functions.R for context.

## Prefer the source tree so devtools::test() exercises the current checkout;
## use the installed copy only when the source tree is unavailable.
source_candidates <- c(
  file.path(getwd(), "inst", "viewer", "utility_functions.R"),
  file.path(getwd(), "..", "..", "inst", "viewer", "utility_functions.R"),
  testthat::test_path("..", "..", "inst", "viewer", "utility_functions.R")
)
utils_file <- source_candidates[file.exists(source_candidates)][1L]
if (is.na(utils_file)) {
  utils_file <- system.file(
    "viewer",
    "utility_functions.R",
    package = "CerebroNexus"
  )
}
skip_if_not(file.exists(utils_file), "utility_functions.R not found")

utils_env <- new.env()
sys.source(file.path(dirname(utils_file), "crb_cache.R"), envir = utils_env)
sys.source(file.path(dirname(utils_file), "msigdb.R"), envir = utils_env)
source(utils_file, local = utils_env)
## These legacy-response fixtures exercise the older public API fallback.
## Collection-wise queries are covered separately with explicit mock metadata.
utils_env$.msigdbCollectionsFunction <- function() NULL
prettifyTable <- utils_env$prettifyTable
cachePlot <- utils_env$cachePlot
viewerUploadsEnabled <- utils_env$viewerUploadsEnabled
viewerUploadPath <- utils_env$viewerUploadPath

test_that("dataset info resolves only the selected catalog entry", {
  resolve <- utils_env$viewerDatasetInfo
  expect_true(is.function(resolve))
  if (!is.function(resolve)) {
    return()
  }

  info <- list(
    label = "Ren et al. COVID atlas",
    path = "/data/ren.crb",
    cells = 1462702L,
    organism = "Homo sapiens",
    date = "2026-09-17"
  )
  catalog <- list("/data/ren.crb" = info)

  expect_identical(resolve(catalog, "/data/ren.crb"), info)
  expect_null(resolve(catalog, "/data/missing.crb"))
  expect_null(resolve(NULL, "/data/ren.crb"))
  expect_null(resolve(
    list("/data/ren.crb" = list(cells = -1L)),
    "/data/ren.crb"
  ))
})

test_that("catalog-backed Data Info defers the full dataset load", {
  load_required <- utils_env$viewerDatasetLoadRequired
  load_on_tab <- utils_env$viewerDatasetLoadOnTab
  expect_true(is.function(load_required))
  expect_true(is.function(load_on_tab))
  if (!is.function(load_required) || !is.function(load_on_tab)) {
    return()
  }

  info <- list(cells = 1462702L)
  expect_false(load_required("loadData", info))
  expect_true(load_required("overview", info))
  expect_true(load_required("loadData", NULL))
  expect_true(load_required(
    "loadData",
    info,
    initial_tab = "immune_repertoire",
    initial_page_applied = FALSE
  ))
  expect_false(load_required(
    "loadData",
    info,
    initial_tab = "immune_repertoire",
    initial_page_applied = TRUE
  ))

  expect_false(load_on_tab(FALSE, "loadData"))
  expect_true(load_on_tab(FALSE, "overview"))
  expect_true(load_on_tab(TRUE, "loadData"))
})

test_that("Data Info uses the catalog before the full dataset", {
  withr::local_package("shinydashboard")
  shiny::testServer(
    function(input, output, session) {
      data_set <- getMetaData <- getExperiment <- getNumberOfCells <- function() {
        stop("full data read")
      }
      current_dataset_info <- function() {
        list(cells = 12345L, organism = "hg", date = "2026-01-02")
      }
      ## Data info now sources its overview panel from the deployment root. Load
      ## the helpers it uses, then the page itself.
      Cerebro.options <- list(cerebro_root = viewer_app_test_path())
      sys.source(viewer_test_path("core", "dataset_overview.R"), environment())
      sys.source(viewer_test_path("load_data", "overview_ui.R"), environment())
      source(
        viewer_test_path("load_data", "sample_info.R"),
        local = TRUE
      )
    },
    {
      ## The overview panel carries the catalog figures as elements now, instead
      ## of one output per statistic.
      overview <- output$load_data_overview$html
      expect_match(overview, "load_data_number_of_cells", fixed = TRUE)
      expect_match(overview, "12,345", fixed = TRUE)
      expect_match(overview, "Human", fixed = TRUE)
      expect_match(overview, "2026-01-02", fixed = TRUE)
    }
  )
})

test_that("a prefetched CRB prototype enters the process cache", {
  cache_prototype <- utils_env$.cacheCrbPrototype
  expect_true(is.function(cache_prototype))
  if (!is.function(cache_prototype)) {
    return()
  }

  path <- tempfile(fileext = ".crb")
  prototype <- new.env(parent = emptyenv())
  cache_prototype(path, prototype, NULL)
  key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  cache <- get(
    ".crb_process_cache",
    envir = environment(cache_prototype),
    inherits = TRUE
  )

  expect_identical(cache[[key]]$object, prototype)
})

test_that("the lightweight TCR gate scans every sample", {
  repertoire <- list(
    s1 = data.frame(CTgene = NA_character_),
    s2 = data.frame(CTgene = NA_character_),
    s3 = data.frame(CTgene = NA_character_),
    s4 = data.frame(CTgene = "TRAV1_TRAJ1_TRBV2_TRBJ2")
  )

  expect_true(utils_env$viewerHasTcrRepertoire(repertoire))
  expect_false(utils_env$viewerHasTcrRepertoire(NULL))
  expect_false(utils_env$viewerHasTcrRepertoire(list(data.frame(x = 1))))
})

test_that("infinite values are replaced without changing other columns", {
  replaceInfiniteValues <- utils_env$replaceInfiniteValues
  expect_true(is.function(replaceInfiniteValues))
  table <- data.frame(
    dirty = c(-Inf, 1, Inf),
    clean = c(2, 3, 4),
    label = c("-Inf", "ok", "Inf"),
    stringsAsFactors = FALSE
  )

  result <- replaceInfiniteValues(table)

  expect_identical(result$dirty, c(-999, 1, 999))
  expect_identical(result$clean, table$clean)
  expect_identical(result$label, table$label)
})

test_that("spreadsheet formulas are neutralized in cells and column names", {
  table <- data.frame(
    unsafe = c("=1+1", "safe"),
    safe = c("text", "@SUM(A1)"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(table)[[1L]] <- "=HYPERLINK(\"https://example.test\")"

  result <- utils_env$neutralizeSpreadsheetFormulas(table)

  expect_identical(
    names(result),
    c("'=HYPERLINK(\"https://example.test\")", "safe")
  )
  expect_identical(result[[1L]], c("'=1+1", "safe"))
  expect_identical(result[[2L]], c("text", "'@SUM(A1)"))
})

test_that("ordinary Viewer tables escape HTML by default", {
  table <- data.frame(label = "<script>alert(1)</script>")

  populated <- prettifyTable(table, filter = "none", dom = "t")
  empty <- utils_env$prepareEmptyTable(table[0, , drop = FALSE])

  expect_identical(attr(populated$x$options, "escapeIdx"), "true")
  expect_identical(attr(empty$x$options, "escapeIdx"), "true")
})

test_that("Viewer tables can explicitly opt out of HTML escaping", {
  table <- data.frame(label = "<strong>trusted</strong>")

  populated <- prettifyTable(
    table,
    filter = "none",
    dom = "t",
    escape = FALSE
  )

  expect_identical(attr(populated$x$options, "escapeIdx"), "false")
})

test_that("embedded Extra material escapes CRB-controlled HTML", {
  content_file <- file.path(
    dirname(utils_file),
    "extra_material",
    "content.R"
  )
  find_calls <- function(node) {
    if (!is.call(node) && !is.expression(node)) {
      return(list())
    }
    found <- if (
      is.call(node) && identical(node[[1L]], as.name("prettifyTable"))
    ) {
      list(node)
    } else {
      list()
    }
    c(found, unlist(lapply(as.list(node), find_calls), recursive = FALSE))
  }
  calls <- find_calls(parse(content_file))
  expect_length(calls, 1L)

  selection <- list(
    group = list(key = "embedded", label = "Embedded"),
    sheet = list(label = "Unsafe")
  )
  input <- list(
    extra_material_table_number_formatting = FALSE,
    extra_material_table_color_highlighting = FALSE
  )
  evaluation <- list2env(list(
    results_df = data.frame(value = '<img src=x onerror="alert(1)">'),
    selection = selection,
    input = input,
    extra_material_table_filter = function(...) "none",
    prettifyTable = prettifyTable
  ))

  widget <- eval(calls[[1L]], envir = evaluation)

  expect_identical(attr(widget$x$options, "escapeIdx"), "true")
})

test_that("MSigDB helpers retain only compact process-level results", {
  cache <- utils_env$.msigdbCache()
  cache[["catalogue"]] <- data.frame(
    gs_name = c("SET_B", "SET_A"),
    collection = c("C2", "H"),
    stringsAsFactors = FALSE
  )
  gene_key <- utils_env$.msigdbGeneCacheKey("Mus musculus", "SET_B")
  cache[[gene_key]] <- c("GeneA", "GeneB")

  names <- utils_env$getGeneSetNames()

  expect_identical(names, c("SET_A", "SET_B"))
  expect_identical(
    utils_env$getMsigdbGenes("Mus musculus", "SET_B"),
    c("GeneA", "GeneB")
  )
  expect_false(exists("Homo sapiens", envir = cache, inherits = FALSE))
})

test_that("MSigDB catalogue retries failures and malformed responses", {
  cache <- utils_env$.msigdbCache()
  original_query <- utils_env$.msigdbFunction
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  on.exit(
    {
      utils_env$.msigdbFunction <- original_query
      rm(list = ls(cache, all.names = TRUE), envir = cache)
    },
    add = TRUE
  )

  calls <- 0L
  utils_env$.msigdbFunction <- function() {
    function(species) {
      calls <<- calls + 1L
      if (calls == 1L) {
        stop("temporary catalogue failure")
      }
      if (calls == 2L) {
        return(data.frame(gs_name = "SET_A"))
      }
      data.frame(
        gs_name = "SET_A",
        gs_collection = "H",
        gene_symbol = "GENE_A"
      )
    }
  }

  expect_warning(
    first <- utils_env$getMsigdbCatalogue(),
    "temporary catalogue failure"
  )
  expect_equal(nrow(first), 0L)
  expect_false(exists("catalogue", envir = cache, inherits = FALSE))

  expect_warning(
    second <- utils_env$getMsigdbCatalogue(),
    "unexpected columns"
  )
  expect_equal(nrow(second), 0L)
  expect_false(exists("catalogue", envir = cache, inherits = FALSE))

  third <- utils_env$getMsigdbCatalogue()
  expect_identical(third$gs_name, "SET_A")
  expect_true(exists("catalogue", envir = cache, inherits = FALSE))
  expect_identical(calls, 3L)
})

test_that("MSigDB catalogue rejects empty and invalid results", {
  cache <- utils_env$.msigdbCache()
  original_query <- utils_env$.msigdbFunction
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  on.exit(
    {
      utils_env$.msigdbFunction <- original_query
      rm(list = ls(cache, all.names = TRUE), envir = cache)
    },
    add = TRUE
  )

  calls <- 0L
  utils_env$.msigdbFunction <- function() {
    function(species, collection = NULL) {
      if (!is.null(collection)) {
        return(data.frame(
          gs_name = c(" SET_A ", "SET_A", "SET_B"),
          gene_symbol = c(" GENE_B ", "GENE_A", "OTHER")
        ))
      }
      calls <<- calls + 1L
      if (calls == 1L) {
        return(data.frame(
          gs_name = character(),
          gs_collection = character()
        ))
      }
      data.frame(
        gs_name = c("", " ", NA, " SET_A ", "SET_A", "AMBIG", "AMBIG"),
        gs_collection = c("H", "H", "H", " H ", "H", " C2 ", "C5")
      )
    }
  }

  expect_warning(
    first <- utils_env$getMsigdbCatalogue(),
    "empty catalogue"
  )
  expect_equal(nrow(first), 0L)
  expect_false(exists("catalogue", envir = cache, inherits = FALSE))

  second <- utils_env$getMsigdbCatalogue()
  expect_identical(
    second,
    data.frame(
      gs_name = "SET_A",
      collection = "H",
      stringsAsFactors = FALSE
    )
  )
  expect_true(exists("catalogue", envir = cache, inherits = FALSE))
  expect_identical(calls, 2L)
  expect_identical(
    utils_env$getMsigdbGenes("Homo sapiens", "SET_A"),
    c("GENE_A", "GENE_B")
  )
})

test_that("MSigDB gene queries retry failures and ignore invalid names", {
  cache <- utils_env$.msigdbCache()
  original_query <- utils_env$.msigdbFunction
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  on.exit(
    {
      utils_env$.msigdbFunction <- original_query
      rm(list = ls(cache, all.names = TRUE), envir = cache)
    },
    add = TRUE
  )
  cache[["catalogue"]] <- data.frame(
    gs_name = "SET_A",
    collection = "H",
    stringsAsFactors = FALSE
  )

  calls <- 0L
  utils_env$.msigdbFunction <- function() {
    function(species, collection = NULL) {
      calls <<- calls + 1L
      if (calls == 1L) {
        stop("temporary gene failure")
      }
      data.frame(
        gs_name = c("SET_A", "SET_A"),
        gene_symbol = c("GENE_B", "GENE_A")
      )
    }
  }

  key <- utils_env$.msigdbGeneCacheKey("Homo sapiens", "SET_A")
  expect_warning(
    first <- utils_env$getMsigdbGenes("Homo sapiens", "SET_A"),
    "temporary gene failure"
  )
  expect_identical(first, character())
  expect_false(exists(key, envir = cache, inherits = FALSE))

  second <- utils_env$getMsigdbGenes("Homo sapiens", "SET_A")
  expect_identical(second, c("GENE_A", "GENE_B"))
  expect_true(exists(key, envir = cache, inherits = FALSE))

  invalid_key <- utils_env$.msigdbGeneCacheKey(
    "Homo sapiens",
    "NOT_A_SET"
  )
  expect_identical(
    utils_env$getMsigdbGenes("Homo sapiens", "NOT_A_SET"),
    character()
  )
  expect_false(exists(invalid_key, envir = cache, inherits = FALSE))
  expect_identical(calls, 2L)
})

test_that("conditional initial routing is consumed by the first dataset", {
  decide <- utils_env$viewerInitialPageDecision

  first <- decide(
    initial_tab = "spatial",
    tab_name = "spatial",
    should_show = FALSE,
    applied = FALSE
  )
  expect_identical(first, list(applied = TRUE, selected = NULL))

  current_tab <- "overview"
  later <- decide(
    initial_tab = "spatial",
    tab_name = "spatial",
    should_show = TRUE,
    applied = first$applied
  )
  if (!is.null(later$selected)) {
    current_tab <- later$selected
  }
  expect_null(later)
  expect_identical(current_tab, "overview")

  expect_identical(
    decide("spatial", "spatial", TRUE, FALSE),
    list(applied = TRUE, selected = "spatial")
  )
  expect_null(decide("spatial", "trajectory", TRUE, FALSE))
})

test_that("duplicate Extra material labels identify the embedded source", {
  choices <- utils_env$extra_material_table_choices(list(
    embedded = list(key = "embedded", label = "QC tables"),
    external = list(key = "external-file:1", label = "QC tables")
  ))

  expect_identical(
    choices,
    c("QC tables (from CRB)" = "embedded", "QC tables" = "external-file:1")
  )
})

test_that("CRB cache log labels omit directory paths", {
  label <- get0(".crbLogLabel", envir = utils_env, inherits = FALSE)
  expect_true(is.function(label))
  if (!is.function(label)) {
    return(invisible(NULL))
  }
  expect_identical(
    label("/private/shiny/session/uploaded-dataset.crb"),
    "uploaded-dataset.crb"
  )
})

test_that("Viewer uploads require explicit open mode", {
  expect_false(viewerUploadsEnabled(list(mode = "closed")))
  expect_false(viewerUploadsEnabled(list()))
  expect_false(viewerUploadsEnabled(NULL))
  expect_false(viewerUploadsEnabled(list(mode = TRUE)))
  expect_true(viewerUploadsEnabled(list(mode = "open")))
})

test_that("Viewer ignores uploaded files unless upload mode is open", {
  upload <- tempfile(fileext = ".crb")
  writeLines("uploaded", upload)
  withr::defer(unlink(upload))
  input_file <- data.frame(datapath = upload)

  expect_identical(viewerUploadPath(input_file, list(mode = "closed")), "")
  expect_identical(viewerUploadPath(input_file, list()), "")
  expect_identical(viewerUploadPath(list(), list(mode = "open")), "")
  expect_identical(
    viewerUploadPath(input_file, list(mode = "open")),
    upload
  )
  expect_identical(
    viewerUploadPath(data.frame(datapath = NA_character_), list(mode = "open")),
    ""
  )
})

test_that("cell scatter payload rejects incoherent categorical snapshots", {
  payload <- utils_env$cerebroCellViewScatterPayload
  common <- list(
    coordinates = list(c(1, 2), c(3, 4)),
    color_variable = "cluster",
    selection_keys = c("a", "b"),
    point_size = 5,
    point_opacity = 1
  )

  expect_error(
    do.call(
      payload,
      c(
        common,
        list(
          color = NULL,
          color_assignments = NULL
        )
      )
    ),
    "same number of cells"
  )
  expect_error(
    do.call(
      payload,
      c(
        common,
        list(
          color = c("A", "B"),
          color_assignments = c(A = "#000000")
        )
      )
    ),
    "missing categorical levels.*B"
  )
})

test_that("single-cell scatter payloads remain arrays on the wire", {
  skip_if_not_installed("jsonlite")
  payload <- utils_env$cerebroCellViewScatterPayload(
    coordinates = list(1, 2),
    color = "A",
    color_variable = "cluster",
    selection_keys = "cell-1",
    point_size = 5,
    point_opacity = 1,
    color_assignments = c(A = "#123456"),
    hover_info = "one cell"
  )
  wire <- jsonlite::fromJSON(
    jsonlite::toJSON(payload, auto_unbox = TRUE),
    simplifyVector = FALSE
  )

  expect_type(wire$data$x[[1L]], "list")
  expect_type(wire$data$y[[1L]], "list")
  expect_type(wire$data$selection_key[[1L]], "list")
  expect_identical(wire$data$color[[1L]], "#123456")
  expect_type(wire$hover$text[[1L]], "list")
})

test_that("single-view scatter payloads carry their display label", {
  payload <- utils_env$cerebroCellViewScatterPayload(
    coordinates = list(c(1, 2), c(3, 4)),
    color = c(0.1, 0.2),
    color_variable = "expression",
    selection_keys = c("cell-1", "cell-2"),
    point_size = 5,
    point_opacity = 1,
    hover_info = c("first", "second"),
    space_label = "UMAP"
  )

  expect_identical(payload$meta$space_label, "UMAP")

  default_payload <- utils_env$cerebroCellViewScatterPayload(
    coordinates = list(c(1, 2), c(3, 4)),
    color = c(0.1, 0.2),
    color_variable = "expression",
    selection_keys = c("cell-1", "cell-2"),
    point_size = 5,
    point_opacity = 1,
    hover_info = c("first", "second")
  )
  expect_false("space_label" %in% names(default_payload$meta))
})

test_that("direct one-cell renderer messages retain array fields", {
  skip_if_not_installed("jsonlite")

  message <- utils_env$cerebroCellViewMessage(
    "trekker_projection",
    meta = list(
      color_type = "categorical",
      traces = "cluster",
      group_colors = "#123456"
    ),
    data = list(
      selection_key = "cell-1",
      group = "cluster",
      panels = list(list(
        id = "trekker",
        selection_key = "cell-1",
        x = 1,
        y = 2,
        from_x = 3,
        from_y = 4,
        to_x = 5,
        to_y = 6
      ))
    ),
    hover = list(text = "cell-1")
  )
  wire <- jsonlite::fromJSON(
    jsonlite::toJSON(message, auto_unbox = TRUE),
    simplifyVector = FALSE
  )

  expect_type(wire$meta$traces, "list")
  expect_type(wire$meta$group_colors, "list")
  expect_type(wire$data$selection_key, "list")
  expect_type(wire$data$group, "list")
  expect_type(wire$data$panels[[1L]]$selection_key, "list")
  expect_type(wire$data$panels[[1L]]$x, "list")
  expect_type(wire$data$panels[[1L]]$from_x, "list")
  expect_type(wire$hover$text, "list")

  continuous <- utils_env$cerebroCellViewMessage(
    "expression_projection",
    meta = list(color_type = "rgb"),
    data = list(
      selection_key = "cell-1",
      x = 1,
      y = 2,
      color = 0.5,
      rgb = list(r = 10L, g = 20L, b = 30L)
    )
  )
  continuous_wire <- jsonlite::fromJSON(
    jsonlite::toJSON(continuous, auto_unbox = TRUE),
    simplifyVector = FALSE
  )
  expect_type(continuous_wire$data$selection_key, "list")
  expect_type(continuous_wire$data$x, "list")
  expect_type(continuous_wire$data$color, "list")
  expect_type(continuous_wire$data$rgb$r, "list")

  categorical <- utils_env$cerebroCellViewMessage(
    "ir_clonalUMAP_projection",
    meta = list(color_type = "categorical", traces = list("Single")),
    data = list(
      selection_key = list("cell-1"),
      x = list(1),
      y = list(2),
      color = list("#123456")
    ),
    hover = list(text = list("cell-1"))
  )
  categorical_wire <- jsonlite::fromJSON(
    jsonlite::toJSON(categorical, auto_unbox = TRUE),
    simplifyVector = FALSE
  )
  expect_type(categorical_wire$data$x[[1L]], "list")
  expect_type(categorical_wire$data$selection_key[[1L]], "list")
  expect_type(categorical_wire$hover$text[[1L]], "list")
})

test_that("categorical scatter payloads retain cells with missing metadata", {
  payload <- utils_env$cerebroCellViewScatterPayload(
    coordinates = list(c(1, 2), c(3, 4)),
    color = c("A", NA_character_),
    color_variable = "cluster",
    selection_keys = c("cell-1", "cell-2"),
    point_size = 5,
    point_opacity = 1,
    color_assignments = c(A = "#123456"),
    hover_info = c("first", "missing")
  )

  expect_true("(missing)" %in% payload$meta$traces)
  expect_identical(
    sort(unlist(payload$data$selection_key, use.names = FALSE)),
    c("cell-1", "cell-2")
  )
})

test_that("large canonical categorical payloads keep coordinates contiguous", {
  n <- 4096L
  payload <- utils_env$cerebroCellViewScatterPayload(
    coordinates = list(seq_len(n), rev(seq_len(n))),
    color = rep(c("A", "B"), length.out = n),
    color_variable = "cluster",
    selection_keys = seq_len(n),
    point_size = 1,
    point_opacity = 0.5,
    color_assignments = c(A = "#123456", B = "#abcdef"),
    hover = FALSE
  )

  expect_identical(as.numeric(payload$data$x), as.numeric(seq_len(n)))
  expect_identical(as.numeric(payload$data$y), as.numeric(rev(seq_len(n))))
  expect_identical(as.integer(payload$data$selection_key), seq_len(n))
  expect_identical(
    as.integer(payload$data$canonical_group[1:4]),
    c(0L, 1L, 0L, 1L)
  )
  expect_identical(unlist(payload$meta$traces), c("A", "B"))

  packed <- utils_env$cv_wire_pack_message(
    utils_env$cerebroCellViewMessage(
      "overview_projection",
      payload$meta,
      payload$data,
      payload$hover
    )
  )
  header_length <- sum(as.integer(packed[seq_len(4L)]) * 256^(0:3))
  header <- jsonlite::fromJSON(
    rawToChar(packed[4L + seq_len(header_length)]),
    simplifyVector = FALSE
  )
  expect_identical(header$data$x$`__cv_wire__`, "f32")
  expect_identical(header$data$y$`__cv_wire__`, "f32")
})

test_that("large canonical factor payloads encode groups without expanding labels", {
  n <- 4096L
  color <- factor(
    rep(c("B", "A", NA_character_), length.out = n),
    levels = c("unused", "A", "B")
  )
  payload <- utils_env$cerebroCellViewScatterPayload(
    coordinates = list(seq_len(n), rev(seq_len(n))),
    color = color,
    color_variable = "cluster",
    selection_keys = seq_len(n),
    point_size = 1,
    point_opacity = 0.5,
    color_assignments = c(B = "#abcdef", A = "#123456"),
    hover = FALSE
  )

  expect_identical(
    unlist(payload$meta$traces),
    c("B", "A", "(missing)")
  )
  expect_identical(
    as.integer(payload$data$canonical_group[1:6]),
    c(0L, 1L, 2L, 0L, 1L, 2L)
  )
  expect_false("unused" %in% unlist(payload$meta$traces))
})

test_that("canonical scatter resources avoid materializing million-cell vectors", {
  projection <- list(
    protocol = "canonical-projection-v1",
    url = "projection.bin",
    cells = 1000000L,
    dimensions = 2L,
    dtype = "float32"
  )
  categories <- list(
    protocol = "canonical-metadata-codes-v1",
    url = "cluster.codes.bin",
    cells = 1000000L,
    dtype = "uint8",
    levels = c("A", "B"),
    code_map = c(-1L, 0L, 1L)
  )
  payload <- utils_env$cerebroCellViewScatterPayload(
    coordinates = list(),
    color = NULL,
    color_variable = "cluster",
    selection_keys = integer(),
    point_size = 1,
    point_opacity = 0.5,
    color_assignments = c(A = "#123456", B = "#abcdef"),
    hover = FALSE,
    cell_count = 1000000L,
    coordinate_resource = projection,
    categorical_resource = categories
  )

  expect_null(payload$data[["x"]])
  expect_null(payload$data[["y"]])
  expect_null(payload$data$selection_key)
  expect_identical(payload$data$projection_resource, projection)
  expect_identical(payload$data$categorical_resource, categories)
  expect_identical(payload$data$deferred_selection_lengths, 1000000L)
  expect_identical(unlist(payload$meta$traces), c("A", "B"))

  wire_categories <- utils_env$cerebroCellViewScatterPayload(
    coordinates = list(),
    color = rep(c("A", "B"), length.out = 4096L),
    color_variable = "cluster",
    selection_keys = seq_len(4096L),
    point_size = 1,
    point_opacity = 0.5,
    color_assignments = c(A = "#123456", B = "#abcdef"),
    hover = FALSE,
    cell_count = 4096L,
    coordinate_resource = within(projection, cells <- 4096L)
  )
  expect_null(wire_categories$data[["x"]])
  expect_null(wire_categories$data[["y"]])
  expect_identical(
    wire_categories$data$projection_resource$cells,
    4096L
  )
  expect_identical(length(wire_categories$data$canonical_group), 4096L)
})

test_that("selection counts use payload cell IDs", {
  expect_identical(
    utils_env$cerebroSelectionCount(list(
      ids = paste0("cell-", seq_len(2536)),
      source = "trekker",
      geometry = list()
    )),
    2536L
  )
  expect_identical(utils_env$cerebroSelectionCount(c("c1", "c2")), 2L)
})

test_that("selected-cell tables preserve identity and skip eager cell work", {
  helper_env <- new.env(parent = utils_env)
  helper_env$prepareEmptyTable <- function(data) data
  helper_env$prettifyTable <- function(data, ...) data
  helper <- utils_env$cerebroSelectedCellsTable
  environment(helper) <- helper_env

  empty <- data.frame(cell_barcode = character())
  expect_no_error(
    helper(
      stop("cell data evaluated before selection"),
      empty,
      NULL,
      download_file_name = "test"
    )
  )

  cells <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    cell_barcode = c("cell-a", "cell-b"),
    cluster = c("A", "B")
  )
  selected <- helper(
    cells,
    cells[, 3:4, drop = FALSE],
    data.frame(selection_key = "cell-b"),
    download_file_name = "test"
  )
  expect_identical(selected$cell_barcode, "cell-b")
  expect_identical(selected$cluster, "B")
})

test_that("stable selected-cell keys skip coordinate identifiers", {
  helper_env <- new.env(parent = utils_env)
  helper_env$prepareEmptyTable <- function(data) data
  helper_env$prettifyTable <- function(data, ...) data
  helper_env$paste0 <- function(...) {
    stop("coordinate identifier should not be built")
  }
  helper <- utils_env$cerebroSelectedCellsTable
  environment(helper) <- helper_env

  cells <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    cell_barcode = c("cell-a", "cell-b"),
    cluster = c("A", "B")
  )
  selected <- helper(
    cells,
    cells[, 3:4, drop = FALSE],
    data.frame(selection_key = "cell-b"),
    download_file_name = "test"
  )
  expect_identical(selected$cell_barcode, "cell-b")
})

test_that("shared result filtering keeps grouped and all-row semantics", {
  results <- data.frame(group = c("A", "B"), value = c(1, 2))
  filter_rows <- utils_env$cerebroFilterResultRows

  expect_identical(filter_rows(results, TRUE, TRUE, "A"), results)
  expect_identical(filter_rows(results, FALSE, FALSE, "A"), results)
  expect_identical(
    filter_rows(results, FALSE, TRUE, "B"),
    results[2, , drop = FALSE]
  )
})

## ---------------------------------------------------------------------------
## prettifyTable edge cases
## ---------------------------------------------------------------------------

test_that("prettifyTable does not crash on an all-NA percentage column", {
  ## Old code did `if (max(col > 1))`, which returned NA for an all-NA column
  ## and threw "missing value where TRUE/FALSE needed".
  table <- data.frame(
    gene = c("g1", "g2"),
    percent_mt = c(NA_real_, NA_real_)
  )
  expect_no_error(
    prettifyTable(
      table,
      filter = "none",
      dom = "t",
      number_formatting = TRUE,
      columns_percentage = 2
    )
  )
})

test_that("prettifyTable still rescales a 0-100 percentage column to 0-1", {
  table <- data.frame(
    gene = c("g1", "g2", "g3"),
    percent_mt = c(50, NA_real_, 20)
  )
  widget <- prettifyTable(
    table,
    filter = "none",
    dom = "t",
    number_formatting = TRUE,
    columns_percentage = 2
  )
  ## The rescaled values live in the widget's data payload.
  rescaled <- widget$x$data$percent_mt
  expect_equal(rescaled[!is.na(rescaled)], c(0.5, 0.2))
})

test_that("prettifyTable tolerates NA / NULL toggle inputs", {
  table <- data.frame(
    gene = c("g1", "g2"),
    percent_mt = c(10, 20)
  )
  ## materialSwitch can transiently pass NA / NULL during UI re-render.
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", number_formatting = NA)
  )
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", show_buttons = NULL)
  )
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", hide_long_columns = NA)
  )
})

## ---------------------------------------------------------------------------
## cachePlot: the shared bindCache wrapper used by the plot renderers.
##
## Drives a minimal server that caches a counting reactive through cachePlot,
## then asserts the caching contract: the reactive evaluates, an unchanged key
## does not recompute, a changed plot-specific key invalidates the cache, and a
## changed dataset key invalidates the cache. The last case would regress if
## the dataset key were forwarded as an already-evaluated value instead of an
## unevaluated expression, so this also guards the wrapper's cache-key scoping.
## ---------------------------------------------------------------------------

test_that("cachePlot caches by key and invalidates on key or dataset change", {
  skip_if_not_installed("shiny", "1.6.0")

  compute_count <- 0

  server <- function(input, output, session) {
    available_crb_files <- shiny::reactiveValues(selected = "datasetA")
    cached <- shiny::reactive({
      compute_count <<- compute_count + 1
      paste(input$metric, available_crb_files$selected)
    }) %>%
      cachePlot(input$metric, available_crb_files$selected)
    output$val <- shiny::renderText(cached())
  }

  shiny::testServer(server, {
    ## 1. evaluates successfully
    session$setInputs(metric = "nUMI")
    expect_equal(cached(), "nUMI datasetA")
    first <- compute_count
    expect_equal(first, 1)

    ## 2. unchanged keys do not recompute
    cached()
    expect_equal(compute_count, first)

    ## 3. changing a plot-specific key invalidates the cache
    session$setInputs(metric = "nGene")
    expect_equal(cached(), "nGene datasetA")
    expect_equal(compute_count, first + 1)

    ## returning to a previously cached key hits the cache
    session$setInputs(metric = "nUMI")
    cached()
    expect_equal(compute_count, first + 1)

    ## 4. changing the dataset key invalidates the cache
    available_crb_files$selected <- "datasetB"
    session$flushReact()
    expect_equal(cached(), "nUMI datasetB")
    expect_equal(compute_count, first + 2)
  })
})

test_that("Spatial sections are normalized once per loaded dataset", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), env)
  reads <- 0L
  make_dataset <- function() {
    dataset <- new.env()
    dataset$getSpatialData <- function(name, hydrate_molecules = TRUE) {
      reads <<- reads + 1L
      list(coordinates = data.frame(x = 1, y = 2), name = name)
    }
    class(dataset) <- c("Cerebro_v1.3", "environment")
    dataset
  }
  current <- make_dataset()
  env$data_set <- function() current
  expect_identical(env$getSpatialData("slice")$name, "slice")
  expect_identical(env$getSpatialData("slice")$name, "slice")
  expect_identical(reads, 1L)
  env$getSpatialData("other")
  expect_identical(reads, 2L)
  current <- make_dataset()
  env$getSpatialData("slice")
  expect_identical(reads, 3L)
})

test_that("single-entry memos recompute only for a different key", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), env)
  memo <- env$viewerMemoOne()
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    calls
  }
  expect_identical(memo(list("a", 1), compute), 1L)
  expect_identical(memo(list("a", 1), compute), 1L)
  expect_identical(memo(list("a", 2), compute), 2L)
  expect_identical(memo(list("a", 1), compute), 3L)
})

test_that("embedded Spatial images are served once per session by URL", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), env)
  registered <- 0L
  ended <- list()
  mock <- list(
    userData = new.env(),
    registerDataObj = function(name, data, filter) {
      registered <<- registered + 1L
      paste0("session/", name)
    },
    onSessionEnded = function(callback) ended[[length(ended) + 1L]] <<- callback
  )
  png <- paste0(
    "data:image/png;base64,",
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
  )
  url <- env$viewerEmbeddedImageUrl(png, session = mock)
  expect_match(url, "^session/cerebro-image-")
  expect_identical(env$viewerEmbeddedImageUrl(png, session = mock), url)
  expect_identical(registered, 1L)
  ## Other formats keep their data URI.
  svg <- "data:image/svg+xml;base64,PHN2Zy8+"
  expect_identical(env$viewerEmbeddedImageUrl(svg, session = mock), svg)
  expect_identical(env$viewerEmbeddedImageUrl(png, session = NULL), png)
  expect_length(ended, 1L)
})
