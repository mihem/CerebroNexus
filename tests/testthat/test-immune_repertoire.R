# test-immune_repertoire.R — Tests for immune repertoire module
#
# The example dataset (example.crb) carries real 10x immune repertoire data
# (sc5p_v2_hs_PBMC_10k, 5' GEX + TCR + BCR from the same experiment). The
# single donor is partitioned into three demo samples; sample labels do not
# represent distinct biological donors.

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
local_inst <- inst_candidates[file.exists(file.path(
  inst_candidates,
  "shiny/v1.4"
))][1]
if (!is.na(local_inst)) {
  shiny_root <- file.path(local_inst, "shiny/v1.4")
  example_crb <- file.path(local_inst, "extdata/v1.4/example.crb")
} else {
  shiny_root <- system.file("shiny/v1.4", package = "cerebroAppLite")
  example_crb <- system.file(
    "extdata/v1.4/example.crb",
    package = "cerebroAppLite"
  )
}

test_that("immune_repertoire module files parse without errors", {
  mod_files <- c(
    "UI.R",
    "server.R",
    "paired_scatter_helpers.R",
    "data.R",
    "settings.R",
    "tabs.R",
    "help.R",
    "visualizations.R"
  )
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "immune_repertoire", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("immune_repertoire UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "immune_repertoire", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"immune_repertoire"', perl = TRUE)
})

test_that("example.crb contains real immune repertoire data", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  expect_true(is.list(ir))
  expect_true(length(ir) > 0)
  for (nm in names(ir)) {
    df <- ir[[nm]]
    expect_s3_class(df, "data.frame")
    expect_true(all(
      c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict") %in%
        colnames(df)
    ))
    expect_true(nrow(df) > 0)
  }
})

test_that("example.crb IR barcodes align with cell metadata", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  md <- crb$getMetaData()
  ir_bc <- unlist(lapply(ir, function(df) df$barcode), use.names = FALSE)
  overlap <- length(intersect(ir_bc, md$cell_barcode))
  expect_true(overlap > 0)
  # every IR barcode should correspond to a real cell in the dataset
  expect_equal(overlap, length(unique(ir_bc)))
})

test_that("IR grouping variables are recoverable from cell metadata by barcode", {
  # The IR data.frames carry only standard scRepertoire columns. The module
  # joins ANY grouping variable (getGroups(): sample, condition, cell type, ...)
  # onto the IR rows by barcode at runtime. This verifies that join is possible
  # for the example data set, so the Group by dropdown is populated regardless
  # of which columns a producer embedded in the IR table.
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  md <- crb$getMetaData()
  groups <- crb$getGroups()
  expect_true(length(groups) >= 1)

  # replicate the module's join: map each IR barcode to a metadata row
  ir_bc <- unlist(lapply(ir, function(df) df$barcode), use.names = FALSE)
  idx <- match(ir_bc, md$cell_barcode)
  expect_true(all(!is.na(idx)))

  # at least one grouping variable yields >= 2 levels over the IR cells
  multilevel <- vapply(
    groups,
    function(g) {
      length(unique(md[[g]][idx])) >= 2
    },
    logical(1)
  )
  expect_true(any(multilevel))
})

test_that("example.crb IR contains both TCR and BCR clonotypes", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  all_ct <- paste(unlist(lapply(ir, function(df) df$CTgene)), collapse = ";")
  expect_true(grepl("TR[AB]", all_ct)) # TCR present
  expect_true(grepl("IG[HKL]", all_ct)) # BCR present
})

test_that("example.crb IR has TCR chains detectable from CTgene", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  ir <- crb$getImmuneRepertoire()
  all_ct <- paste(unlist(lapply(ir, function(df) df$CTgene)), collapse = ";")
  has_tcr <- grepl("TRA", all_ct) || grepl("TRB", all_ct)
  expect_true(has_tcr)
})

test_that("data.R joins metadata so grouping is not limited to IR columns", {
  # Guards the generic fix: grouping options must come from the data set's
  # metadata (getGroups + barcode join), not only from columns embedded in the
  # IR table. A regression to "shared IR columns only" would silently break
  # grouping for users whose IR tables carry just the standard scRepertoire
  # columns.
  data_file <- file.path(shiny_root, "immune_repertoire", "data.R")
  skip_if_not(file.exists(data_file))
  content <- paste(readLines(data_file), collapse = "\n")
  expect_match(content, "ir_data_annotated")
  expect_match(content, "cell_barcode")
  # grouping options are derived from getGroups() in the settings panel
  settings_file <- file.path(shiny_root, "immune_repertoire", "settings.R")
  settings <- paste(readLines(settings_file), collapse = "\n")
  expect_match(settings, "getGroups\\(\\)")
})

test_that("core IR params update immediately to keep bindCache keys and values aligned", {
  data_file <- file.path(shiny_root, "immune_repertoire", "data.R")
  skip_if_not(file.exists(data_file))
  content <- paste(readLines(data_file), collapse = "\n")
  ir_params_block <- regmatches(
    content,
    regexpr(
      "ir_params <- reactive\\(\\{[\\s\\S]*?\\n\\s*\\}\\)",
      content,
      perl = TRUE
    )
  )
  expect_length(ir_params_block, 1)
  expect_false(grepl(
    "ir_params <- reactive\\(\\{[\\s\\S]*?\\}\\)\\s*%>%\\s*debounce\\(",
    content,
    perl = TRUE
  ))
})

test_that("Comparison-units re-split is removed; grouping is unified on group.by", {
  # scRepertoire's group.by already rbinds the list and re-splits on the chosen
  # column (.groupList), so the in-app ir_sampleCol re-split was a redundant,
  # narrower duplicate of group.by. It has been removed: ir_data() now always
  # returns the original annotated list and grouping flows solely through
  # ir_groupBy / group.by.
  mod_files <- c("data.R", "settings.R", "visualizations.R", "param_spec.R")
  content <- paste(
    vapply(
      mod_files,
      function(f) {
        paste(
          readLines(file.path(shiny_root, "immune_repertoire", f)),
          collapse = "\n"
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
  # The control, its input, the choice helper, and the split are all gone.
  expect_no_match(content, "ir_sampleCol")
  expect_no_match(content, "ir_sample_col_choices")
  expect_no_match(content, "split\\(merged, merged\\[\\[col\\]\\]\\)")
  # Grouping still flows through group.by.
  expect_match(content, "input\\$ir_groupBy")
})

test_that("renderers pass supported scRepertoire parameters", {
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")

  expect_match(
    content,
    "clonalAbundance\\([\\s\\S]{0,300}chain\\s*=\\s*pars\\$chain",
    perl = TRUE
  )
  expect_match(
    content,
    "clonalSizeDistribution\\([\\s\\S]{0,300}chain\\s*=\\s*pars\\$chain",
    perl = TRUE
  )
})

test_that("Diversity x.axis uses grouped bootstrap plot, not scRepertoire's continuous-axis boxplot", {
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")

  expect_match(content, "ir_plot_clonal_diversity")
  # clonalDiversity is now called via do.call (to handle NULL x.axis), but
  # return.boots=TRUE is still present in the argument list.
  expect_match(
    content,
    "return\\.boots\\s*=\\s*TRUE",
    perl = TRUE
  )
  # Custom ggplot uses eff_x_axis (resolved from x_axis or group column)
  expect_match(
    content,
    "geom_boxplot\\([\\s\\S]{0,300}group\\s*=\\s*\\.data\\[\\[eff_x_axis\\]\\]",
    perl = TRUE
  )
})

test_that("clonalScatter render guards against invalid group selection", {
  # clonalScatter compares two groups via x.axis/y.axis (the levels produced by
  # group.by). The render must validate >= 2 distinct groups to avoid
  # "attempt to select less than one element" on a single-element list.
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")
  expect_match(content, "Clonal scatter needs at least 2 groups")
  expect_match(content, "Select two different groups")
})

test_that("safeRenderPlot lets validate/req conditions pass through", {
  # validate()/need()/req() raise a "shiny.silent.error" which is also an
  # `error`. safeRenderPlot must NOT turn it into an error plot, otherwise
  # first-paint NULL inputs render "[IR ERROR] ..." instead of a grey
  # placeholder. The fix re-raises shiny.silent.error from within the error
  # handler (a sibling shiny.silent.error handler would re-catch it).
  srv <- file.path(shiny_root, "immune_repertoire", "server.R")
  skip_if_not(file.exists(srv))
  content <- paste(readLines(srv), collapse = "\n")
  expect_match(content, "shiny.silent.error")
  expect_match(content, "inherits\\(e, \"shiny.silent.error\"\\)")

  # functional check: silent conditions propagate, real errors are caught
  safeRenderPlot <- function(expr, plot_name = "unknown") {
    tryCatch(
      {
        expr
      },
      error = function(e) {
        if (inherits(e, "shiny.silent.error")) {
          stop(e)
        }
        "ERROR_PLOT"
      }
    )
  }
  silent <- tryCatch(
    safeRenderPlot(shiny::validate(shiny::need(FALSE, "x"))),
    shiny.silent.error = function(e) "PASSED",
    error = function(e) "CAUGHT"
  )
  expect_equal(silent, "PASSED")
  expect_equal(safeRenderPlot(stop("boom")), "ERROR_PLOT")
})

test_that("server.R translates cryptic scRepertoire errors to empty-state", {
  # scRepertoire raises opaque internal errors (get1index, subscript out of
  # bounds, ...) when a selection leaves a group empty/single-valued.
  # safeRenderPlot must turn these into a friendly empty-state message rather
  # than dumping the raw error, while still surfacing genuine errors.
  srv <- file.path(shiny_root, "immune_repertoire", "server.R")
  skip_if_not(file.exists(srv))
  content <- paste(readLines(srv), collapse = "\n")
  expect_match(content, "get1index")
  expect_match(content, "No data to display for the current selection")

  # mirror the classifier used in safeRenderPlot
  is_empty_selection <- function(msg) {
    grepl(
      paste0(
        "get1index|subscript out of bounds|less than one element|",
        "undefined columns|replacement has|non-conformable|missing value"
      ),
      msg,
      ignore.case = TRUE
    )
  }
  expect_true(is_empty_selection(
    "attempt to select less than one element in get1index"
  ))
  expect_true(is_empty_selection("subscript out of bounds"))
  expect_false(is_empty_selection("some unrelated real error"))
})

test_that("paired scatter falls back to manual sample selection", {
  helper <- file.path(
    shiny_root,
    "immune_repertoire",
    "paired_scatter_helpers.R"
  )
  expect_true(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, envir = env)

  meta <- data.frame(
    .sample_name = c("sample_1", "sample_2", "sample_3"),
    sample = c("sample_1", "sample_2", "sample_3"),
    orig.ident = c("sample_1", "sample_2", "sample_3"),
    stringsAsFactors = FALSE
  )
  choices <- env$ir_paired_scatter_choices(meta)

  expect_equal(choices$compare_candidates, character(0))
  expect_equal(choices$mode, "manual")
  expect_equal(choices$sample_choices, meta$.sample_name)
})

test_that("ir_bindCache injects dataset identity into cache key", {
  # data_to_load$path in every cache key prevents stale plots when switching
  # datasets; cache = "session" prevents cross-user/session cache leakage.
  srv <- file.path(shiny_root, "immune_repertoire", "server.R")
  skip_if_not(file.exists(srv))
  content <- paste(readLines(srv), collapse = "\n")
  expect_match(content, "data_to_load\\$path")
  expect_match(content, 'cache\\s*=\\s*"session"')
})

test_that("ir_bindCache keeps only global cache keys centralized", {
  # Plot-specific controls must live in the renderer's own bindCache call. The
  # shared helper should not make Homeostasis cloneSize, order.by, or scatter
  # point options invalidate every IR plot.
  srv <- file.path(shiny_root, "immune_repertoire", "server.R")
  skip_if_not(file.exists(srv))
  content <- paste(readLines(srv), collapse = "\n")
  helper <- regmatches(
    content,
    regexpr(
      "ir_bindCache <- function\\(x, \\.\\.\\., cache = \"session\"\\) \\{[\\s\\S]*?\\n\\}",
      content,
      perl = TRUE
    )
  )
  expect_length(helper, 1)
  expect_no_match(
    helper,
    "ir_p_order_by|ir_p_clone_size|ir_d_point_size|ir_d_alpha"
  )
  expect_match(helper, "ir_d_base_size")
  expect_match(helper, "ir_d_title")
  expect_match(helper, "data_to_load\\$path")
})

test_that("example.crb preserves core data fields", {
  skip_if_not(file.exists(example_crb))
  crb <- readRDS(example_crb)
  expect_true(!is.null(crb$getMetaData()))
  expect_true(nrow(crb$getMetaData()) > 0)
  expect_true(!is.null(crb$experiment))
})

test_that("renderers enforce scRepertoire parameter constraints", {
  # Three plots have scRepertoire API constraints that a stale/global control
  # value can violate, causing internal errors. The renderers must enforce
  # valid values rather than trust the global Clone call / aa.length inputs.
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")

  # clonalLength: cloneCall must be a sequence type (nt/aa), never gene/strict
  expect_match(
    content,
    "clonalLength[\\s\\S]{0,400}clone_call <- if \\(isTRUE\\(pars\\$cloneCall %in% c\\(\"nt\", \"aa\"\\)\\)",
    perl = TRUE
  )

  # clonalSizeDistribution: the distribution fit only converges on the strict
  # clone definition for the bundled data, so cloneCall is forced to "strict"
  expect_match(
    content,
    "clonalSizeDistribution\\([\\s\\S]{0,200}cloneCall = \"strict\"",
    perl = TRUE
  )

  # percentAA / positionalEntropy: aa.length is validated to a positive integer
  expect_match(
    content,
    "is.na\\(aa_len\\)[\\s\\S]{0,40}aa_len < 1[\\s\\S]{0,60}aa_len <- 20",
    perl = TRUE
  )
})

test_that("Clonal UMAP does not depend on the hidden Clone call control", {
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")
  block <- regmatches(
    content,
    regexpr(
      "output\\$ir_plot_clonalUMAP <- plotly::renderPlotly\\(\\{[\\s\\S]*?## ---- BCR-specific renderers",
      content,
      perl = TRUE
    )
  )
  expect_length(block, 1)
  expect_match(block, 'clone_call <- "gene"')
  expect_no_match(block, "ir_params\\(\\)\\$cloneCall")
  expect_no_match(block, "input\\$ir_cloneCall")
})

test_that("ir_bindCache keys cover all per-plot ir_param() calls", {
  # Each renderPlot that calls ir_param("ir_p_XXX") must include the
  # corresponding input$ir_p_XXX in its ir_bindCache(...) key list. If a
  # parameter is added to IR_PARAM_SPEC but forgotten in the cache keys,
  # the plot will silently show stale data. This test parses the source to
  # verify coverage for every renderPlot / ir_bindCache pair.
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  lines <- readLines(viz)
  content <- paste(lines, collapse = "\n")

  # Split into renderPlot blocks — each starts with "output$ir_plot_"
  blocks <- strsplit(content, "(?=output\\$ir_plot_)", perl = TRUE)[[1]]
  blocks <- blocks[grepl("output\\$ir_plot_", blocks)]

  misses <- character(0)
  for (blk in blocks) {
    # Extract the render expression (first `{`...`})` body) and the
    # subsequent ir_bindCache(..., input$ir_p_XXX, ...) call, if any.
    render_body <- sub(
      "^[^{]*\\{(.*?)\\}%>%\\s*$",
      "\\1",
      blk
    )
    cache_body <- sub(
      "^.*ir_bindCache\\((.*?)\\)",
      "\\1",
      blk
    )
    if (identical(cache_body, blk)) {
      next
    } # no ir_bindCache for this plot

    # Collect ir_param() calls from the render expression
    param_ids <- regmatches(
      render_body,
      gregexpr('ir_param\\("([^"]+)"', render_body, perl = TRUE)
    )[[1]]
    param_ids <- gsub('ir_param\\("([^"]+)"', '\\1', param_ids)
    param_ids <- unique(param_ids[param_ids != ""])

    if (length(param_ids) == 0) {
      next
    } # no dynamic params → nothing to check

    # Collect input$ keys from the cache call
    cache_keys <- regmatches(
      cache_body,
      gregexpr('input\\$\\w+', cache_body)
    )[[1]]

    for (pid in param_ids) {
      # ir_param("ir_p_metric") → expect input$ir_p_metric in cache keys
      if (!pid %in% gsub('^input\\$', '', cache_keys)) {
        # Extract the plot name from the block header
        plot_name <- sub("^output\\$(\\w+)\\s.*", "\\1", blk)
        misses <- c(misses, sprintf("%s: %s not in cache keys", plot_name, pid))
      }
    }
  }

  expect_equal(
    length(misses),
    0L,
    info = paste(c("Cache key coverage gaps found:", misses), collapse = "\n  ")
  )
})

test_that("renderers pass order.by to scRepertoire functions that support it", {
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")
  # order.by must reach the plotting calls, not just exist as an input.
  expect_match(
    content,
    "order\\.by\\s*=",
    info = "order.by not passed to any renderer"
  )
})

test_that("clonalHomeostasis renderer passes a cloneSize binning", {
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")
  expect_match(
    content,
    "cloneSize\\s*=",
    info = "cloneSize not passed to clonalHomeostasis"
  )
})

test_that("vizGenes renderer passes a y.axis", {
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")
  expect_match(content, "y\\.axis\\s*=", info = "y.axis not passed to vizGenes")
})

test_that("order.by control is declared in param_spec", {
  ps <- file.path(shiny_root, "immune_repertoire", "param_spec.R")
  skip_if_not(file.exists(ps))
  content <- paste(readLines(ps), collapse = "\n")
  expect_match(content, "ir_p_order_by", info = "no order.by control declared")
})

test_that("tab-dependent label uses a NULL-safe %in% guard", {
  # input$ir_tabs is NULL before the tabset registers; `tab %in% c(...)` then
  # returns logical(0), and `if (logical(0))` raises
  # 'argument is of length zero' inside renderUI. Every %in% test on `tab` in
  # the group-label branch must be guarded by !is.null(tab) && (or the
  # is.null(tab) || short-circuit used elsewhere in this render).
  st <- file.path(shiny_root, "immune_repertoire", "settings.R")
  skip_if_not(file.exists(st))
  content <- paste(readLines(st), collapse = "\n")

  # The Compare-by label test must be NULL-guarded.
  expect_match(
    content,
    "group_label <- if \\(\\s*!is\\.null\\(tab\\)[\\s\\S]{0,40}tab %in% c\\(",
    perl = TRUE,
    info = "group_label branch tests `tab %in% c(...)` without a !is.null guard"
  )
})

test_that("Length renderer only facets when a grouping is selected", {
  # Group results by = None means group.by is NULL: there is no grouping, so the
  # plot must be a single combined panel (scRepertoire's native overlay), NOT
  # one facet per loaded sample. Faceting must be gated on a non-NULL groupBy.
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")

  # The clonalLength renderer branches on whether a grouping is set.
  expect_match(
    content,
    "ir_plot_clonalLength[\\s\\S]{0,1200}is\\.null\\(pars\\$groupBy\\)",
    perl = TRUE,
    info = "clonalLength renderer does not gate faceting on is.null(pars$groupBy)"
  )
  # facet builder is still used (for the grouped branch).
  expect_match(
    content,
    "ir_plot_clonalLength[\\s\\S]{0,3500}ir_length_facet_plot\\(",
    perl = TRUE
  )
})
