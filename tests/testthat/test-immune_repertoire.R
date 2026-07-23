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
  shiny_root <- system.file("shiny/v1.4", package = "CerebroNexus")
  example_crb <- system.file(
    "extdata/v1.4/example.crb",
    package = "CerebroNexus"
  )
}

test_that("immune_repertoire module files parse without errors", {
  mod_files <- c(
    "UI.R",
    "server.R",
    "paired_scatter_helpers.R",
    "compare_helpers.R",
    "help_guide.R",
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
  # available_crb_files$selected in every cache key prevents stale plots when
  # switching datasets; cache = "session" prevents cross-user/session leakage.
  srv <- file.path(shiny_root, "immune_repertoire", "server.R")
  skip_if_not(file.exists(srv))
  content <- paste(readLines(srv), collapse = "\n")
  expect_match(content, "available_crb_files\\$selected")
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
  expect_match(helper, "available_crb_files\\$selected")
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
  ## The Clonal UMAP renderers (non-faceted shared-projection observe + faceted
  ## static ggplot) live between the "Draw the non-faceted Clonal UMAP" marker
  ## and the BCR-specific renderers section. Both must colour by clone_call
  ## "gene" and must not read the hidden Clone-call control.
  block <- regmatches(
    content,
    regexpr(
      "## Draw the non-faceted Clonal UMAP[\\s\\S]*?## ---- BCR-specific renderers",
      content,
      perl = TRUE
    )
  )
  expect_length(block, 1)
  expect_match(block, 'clone_call <- "gene"')
  expect_no_match(block, "ir_params\\(\\)\\$cloneCall")
  expect_no_match(block, "input\\$ir_cloneCall")
})

test_that("Clonal UMAP split layout avoids empty facet slots on wide canvases", {
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  content <- paste(readLines(viz), collapse = "\n")
  block <- regmatches(
    content,
    regexpr(
      "ir_umap_split_layout <- function\\([\\s\\S]*?\\n\\}\\n\\nir_umap_grouped_data",
      content,
      perl = TRUE
    )
  )
  expect_length(block, 1)
  block <- sub("\\n\\nir_umap_grouped_data$", "", block)
  env <- new.env(parent = baseenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  eval(parse(text = block), envir = env)

  layout <- env$ir_umap_split_layout(3, width = 1200, height = 450)

  expect_equal(layout$ncol, 3L)
  expect_equal(layout$nrow, 1L)
  expect_gte(layout$panel_px, 300)
  expect_lte(layout$height, 500)

  tall_layout <- env$ir_umap_split_layout(6, width = 900, height = 500)

  expect_equal(tall_layout$ncol, 3L)
  expect_equal(tall_layout$nrow, 2L)
  expect_gte(tall_layout$panel_px, 300)
  expect_gte(tall_layout$height, 600)
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

# ---- Compare (interactive alluvial) ------------------------------------- #

# Source the pure helpers into a throwaway env so the geometry can be tested
# without the Shiny app or scRepertoire (the prep + drawing functions take a
# plain data.frame shaped like clonalCompare(exportTable = TRUE)).
compare_env <- local({
  helper <- file.path(shiny_root, "immune_repertoire", "compare_helpers.R")
  if (!file.exists(helper)) {
    return(NULL)
  }
  e <- new.env(parent = globalenv())
  sys.source(helper, envir = e)
  e
})

# A minimal two-group table: clone A is shared (different sizes), clone B is
# private to sample_1, clone C is private to sample_2.
compare_tab_2grp <- data.frame(
  clones = c("A", "B", "A", "C"),
  Proportion = c(0.5, 0.3, 0.2, 0.4),
  Sample = c("sample_1", "sample_1", "sample_2", "sample_2"),
  stringsAsFactors = FALSE
)

test_that("compare prep stacks two groups and links only shared clones", {
  skip_if(is.null(compare_env))
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_1", "sample_2"),
    proportion = TRUE
  )
  expect_true(prep$ok)
  expect_equal(prep$value_col, "Proportion")
  expect_setequal(prep$clones, c("A", "B", "C"))
  # 4 non-zero clone/group cells -> 4 rectangles.
  expect_equal(nrow(prep$rects), 4L)
  # Only clone A appears in both groups -> exactly one ribbon.
  expect_equal(nrow(prep$ribbons), 1L)
  expect_equal(prep$ribbons$clone, "A")
  expect_equal(prep$ribbons$from_index, 1L)
  expect_equal(prep$ribbons$to_index, 2L)
})

test_that("compare ribbon ends have different widths when the clone changes size", {
  skip_if(is.null(compare_env))
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_1", "sample_2"),
    proportion = TRUE
  )
  rb <- prep$ribbons[prep$ribbons$clone == "A", ]
  src_h <- rb$from_ymax - rb$from_ymin
  tgt_h <- rb$to_ymax - rb$to_ymin
  # A is 0.5 in sample_1 and 0.2 in sample_2, so the two ends differ.
  expect_equal(src_h, 0.5)
  expect_equal(tgt_h, 0.2)
  expect_false(isTRUE(all.equal(src_h, tgt_h)))
})

test_that("compare prep detects the Count column in count mode", {
  skip_if(is.null(compare_env))
  tab <- data.frame(
    clones = c("A", "A"),
    Count = c(10, 4),
    Sample = c("sample_1", "sample_2"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("sample_1", "sample_2"),
    proportion = FALSE
  )
  expect_true(prep$ok)
  expect_equal(prep$value_col, "Count")
  expect_false(prep$proportion)
})

test_that("compare prep stacks every column in one shared clone order", {
  skip_if(is.null(compare_env))
  # A has the largest total, then B, then C. All columns must stack in that
  # SHARED order (A bottom, C top) so a clone keeps a consistent band and its
  # ribbons run parallel without crossing — even though within s2 the sizes
  # would rank differently if each column were sorted on its own.
  tab <- data.frame(
    clones = c("A", "B", "C", "A", "B", "C"),
    Proportion = c(0.6, 0.3, 0.1, 0.1, 0.3, 0.6),
    Sample = c("s1", "s1", "s1", "s2", "s2", "s2"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("s1", "s2"),
    proportion = TRUE
  )
  # A(0.7 total) > B(0.6) > C(0.7)? totals: A=0.7, B=0.6, C=0.7 -> A,C tie by
  # name so order is A, C, B. Assert the SAME order in both columns.
  seg_order <- function(gi) {
    r <- prep$rects[prep$rects$group_index == gi, , drop = FALSE]
    r <- r[order(r$ymin), , drop = FALSE] # bottom -> top
    r$clone
  }
  expect_equal(seg_order(1), prep$clones)
  expect_equal(seg_order(2), prep$clones)
  expect_equal(seg_order(1), seg_order(2))
})

test_that("compare prep reports per-clone totals in clone order", {
  skip_if(is.null(compare_env))
  tab <- data.frame(
    clones = c("A", "B", "A", "C"),
    Proportion = c(0.5, 0.3, 0.2, 0.4),
    Sample = c("s1", "s1", "s2", "s2"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("s1", "s2"),
    proportion = TRUE
  )
  # totals: A = 0.7, C = 0.4, B = 0.3 -> clone order A, C, B.
  expect_equal(prep$clones, c("A", "C", "B"))
  expect_equal(prep$totals, c(0.7, 0.4, 0.3))
  # The legend label appends the total in the current mode.
  expect_equal(
    compare_env$ir_compare_legend_label("A", 0.7, TRUE),
    "A (0.7)"
  )
  expect_equal(
    compare_env$ir_compare_legend_label("A", 21, FALSE),
    "A (21)"
  )
  # No total -> bare (possibly truncated) name.
  expect_equal(compare_env$ir_compare_legend_label("A"), "A")
})

test_that("compare prep links only adjacent groups across three groups", {
  skip_if(is.null(compare_env))
  tab <- data.frame(
    clones = rep("A", 3),
    Proportion = c(0.4, 0.3, 0.5),
    Sample = c("s1", "s2", "s3"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("s1", "s2", "s3"),
    proportion = TRUE
  )
  pairs <- paste0(prep$ribbons$from_index, "->", prep$ribbons$to_index)
  # A spans all three groups: link 1->2 and 2->3, never 1->3.
  expect_setequal(pairs, c("1->2", "2->3"))
})

test_that("compare prep preserves the requested group order", {
  skip_if(is.null(compare_env))
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_2", "sample_1"), # reversed
    proportion = TRUE
  )
  expect_equal(prep$groups, c("sample_2", "sample_1"))
})

test_that("compare prep returns friendly empty states", {
  skip_if(is.null(compare_env))
  # Empty table.
  e1 <- compare_env$ir_prepare_compare_alluvial(
    data.frame(),
    c("a", "b")
  )
  expect_false(e1$ok)
  expect_true(nzchar(e1$message))
  # Only one group present -> nothing to compare.
  one <- data.frame(
    clones = c("A", "B"),
    Proportion = c(0.5, 0.5),
    Sample = c("s1", "s1"),
    stringsAsFactors = FALSE
  )
  e2 <- compare_env$ir_prepare_compare_alluvial(one, c("s1", "s2"))
  expect_false(e2$ok)
  # All-zero values.
  z <- data.frame(
    clones = c("A", "A"),
    Proportion = c(0, 0),
    Sample = c("s1", "s2"),
    stringsAsFactors = FALSE
  )
  e3 <- compare_env$ir_prepare_compare_alluvial(z, c("s1", "s2"))
  expect_false(e3$ok)
})

test_that("compare plotly shows exactly one legend entry per clone", {
  skip_if(is.null(compare_env))
  skip_if_not_installed("plotly")
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_1", "sample_2"),
    proportion = TRUE
  )
  fig <- compare_env$ir_compare_alluvial_plotly(prep, palette = "Harmonic")
  expect_s3_class(fig, "plotly")
  built <- plotly::plotly_build(fig)
  # Rectangles and ribbons are separate traces (so only the rects get a dark
  # border), but exactly one trace per clone carries the legend entry.
  legend_traces <- Filter(
    function(d) isTRUE(d$showlegend) || is.null(d$showlegend),
    built$x$data
  )
  legend_names <- vapply(
    legend_traces,
    function(d) if (is.null(d$name)) "" else d$name,
    character(1)
  )
  legend_names <- legend_names[nzchar(legend_names)]
  expect_equal(length(legend_names), length(prep$clones))
  expect_equal(length(unique(legend_names)), length(prep$clones))
  # The legend-bearing (rectangle) traces use the dark hairline border; the
  # borderless ribbon traces (showlegend = FALSE) use width 0.
  rect_traces <- Filter(
    function(d) !isFALSE(d$showlegend),
    built$x$data
  )
  borders <- vapply(
    rect_traces,
    function(d) if (is.null(d$line$color)) "" else as.character(d$line$color),
    character(1)
  )
  expect_true(all(borders == "#333333"))
  ribbon_traces <- Filter(
    function(d) {
      isFALSE(d$showlegend) && identical(d$mode, "lines")
    },
    built$x$data
  )
  ribbon_widths <- vapply(
    ribbon_traces,
    function(d) {
      if (is.null(d$line$width)) NA_real_ else as.numeric(d$line$width)
    },
    numeric(1)
  )
  expect_true(all(ribbon_widths == 0))
})

test_that("compare hover comes from single-tooltip anchor markers", {
  skip_if(is.null(compare_env))
  skip_if_not_installed("plotly")
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_1", "sample_2"),
    proportion = TRUE
  )
  fig <- compare_env$ir_compare_alluvial_plotly(prep, palette = "Harmonic")
  built <- plotly::plotly_build(fig)
  # The fill traces (lines mode) must not carry hover text — that is what
  # produced the repeated per-vertex tooltip; their hoverinfo is "skip".
  line_traces <- Filter(
    function(d) identical(d$mode, "lines"),
    built$x$data
  )
  # plotly_build recycles hoverinfo to one value per point, so check every
  # element rather than identity against a scalar.
  expect_true(all(vapply(
    line_traces,
    function(d) all(d$hoverinfo == "skip", na.rm = TRUE),
    logical(1)
  )))
  # Hover lives on invisible marker anchors (one text per rectangle, not
  # per vertex), with the marker made transparent.
  anchor_traces <- Filter(
    function(d) identical(d$mode, "markers"),
    built$x$data
  )
  expect_true(length(anchor_traces) >= 1)
  # Each anchor point has exactly one clean hover string; no NA fragments.
  for (d in anchor_traces) {
    txt <- as.character(d$text)
    expect_false(any(is.na(txt)))
    expect_true(all(grepl("Group:", txt, fixed = TRUE)))
    expect_true(all(grepl("Proportion:", txt, fixed = TRUE)))
  }
  # Markers are visually invisible (opacity 0) so only the tooltip shows.
  expect_true(all(vapply(
    anchor_traces,
    function(d) isTRUE(d$marker$opacity == 0),
    logical(1)
  )))
})

test_that("compare plotly hover keeps the full clone and honours the value mode", {
  skip_if(is.null(compare_env))
  skip_if_not_installed("plotly")
  long_clone <- paste(rep("IGHV3-23.IGHJ4", 6), collapse = "_")
  tab <- data.frame(
    clones = c(long_clone, long_clone),
    Count = c(12, 7),
    Sample = c("s1", "s2"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("s1", "s2"),
    proportion = FALSE
  )
  fig <- compare_env$ir_compare_alluvial_plotly(prep)
  built <- plotly::plotly_build(fig)
  # Hover text carries the full (untruncated) clone name plus the group and the
  # value in the current mode (count here).
  hover_txt <- vapply(
    built$x$data,
    function(d) paste(as.character(d$text), collapse = " "),
    character(1)
  )
  expect_true(any(grepl(long_clone, hover_txt, fixed = TRUE)))
  expect_true(any(grepl("Count: 12", hover_txt, fixed = TRUE)))
  expect_true(any(grepl("Group: s1", hover_txt, fixed = TRUE)))
  # Legend label is truncated for display but annotated with the clone total
  # (Count 12 + 7 = 19 here), so size is readable straight from the legend.
  legend_names <- vapply(
    built$x$data,
    function(d) if (is.null(d$name)) "" else d$name,
    character(1)
  )
  expect_true(any(nchar(legend_names) < nchar(long_clone)))
  expect_true(any(grepl("(19)", legend_names, fixed = TRUE)))
  # Count mode -> y axis title names the count AND says the height is the value.
  ytitle <- built$x$layout$yaxis$title
  if (is.list(ytitle)) {
    ytitle <- ytitle$text
  }
  expect_match(ytitle, "Clone count", fixed = TRUE)
  expect_match(ytitle, "block height", fixed = TRUE)
})

test_that("Compare renderer is interactive plotly and drops the area graph param", {
  vis_file <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(vis_file))
  content <- paste(readLines(vis_file), collapse = "\n")
  # Renderer converted to plotly.
  expect_match(
    content,
    "ir_plot_clonalCompare[\\s\\S]{0,80}plotly::renderPlotly",
    perl = TRUE
  )
  # UI output is a plotly output.
  expect_match(
    content,
    "ir_fill_plot\\(\"ir_plot_clonalCompare\",\\s*plotly\\s*=\\s*TRUE\\)",
    perl = TRUE
  )
  # The removed graph selector must not linger.
  param_file <- file.path(shiny_root, "immune_repertoire", "param_spec.R")
  param <- paste(readLines(param_file), collapse = "\n")
  expect_false(grepl("ir_p_compare_graph", param, fixed = TRUE))
})

test_that("IR panel has an info button wired to an illustrated guide modal", {
  ui_file <- file.path(shiny_root, "immune_repertoire", "UI.R")
  guide_file <- file.path(shiny_root, "immune_repertoire", "help_guide.R")
  skip_if_not(file.exists(ui_file) && file.exists(guide_file))
  ui <- paste(readLines(ui_file), collapse = "\n")
  guide <- paste(readLines(guide_file), collapse = "\n")
  # Button in the box title.
  expect_match(
    ui,
    'cerebroInfoButton\\("ir_visualizations_info"\\)',
    perl = TRUE
  )
  # Observer that opens the tabbed modal on click.
  expect_match(
    guide,
    "observeEvent\\(input\\$ir_visualizations_info[\\s\\S]{0,400}showModal",
    perl = TRUE
  )
  # The guide builds one panel per visible tab from IR_GUIDE_TABS.
  expect_match(guide, "IR_GUIDE_TABS", perl = TRUE)
  expect_match(guide, "ir_guide_tab_content", perl = TRUE)
})
