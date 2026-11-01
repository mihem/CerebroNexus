# Execute the production renderer/control bodies without a browser. A failing
# repertoire getter makes accidental eager sidecar hydration observable.
ir_test_root <- testthat::test_path("../../inst/viewer/immune_repertoire")
if (!dir.exists(ir_test_root)) {
  ir_test_root <- system.file(
    "viewer/immune_repertoire",
    package = "CerebroNexus"
  )
}

ir_test_call <- function(expr, name) {
  if (!is.call(expr)) {
    return(NULL)
  }
  if (identical(paste(deparse(expr[[1]]), collapse = ""), name)) {
    return(expr)
  }
  for (part in as.list(expr)[-1L]) {
    found <- ir_test_call(part, name)
    if (!is.null(found)) return(found)
  }
  NULL
}

ir_test_body <- function(file, output, wrapper, env) {
  exprs <- parse(file.path(ir_test_root, file))
  expr <- Filter(
    function(x) {
      is.call(x) &&
        as.character(x[[1]])[1] %in% c("<-", "=") &&
        identical(
          viewer_server_key(x[[2]]),
          paste0("output.", output)
        )
    },
    as.list(exprs)
  )[[1]]
  body <- ir_test_call(expr[[3]], wrapper)[[2]]
  eval(call("function", as.pairlist(list()), body), env)
}

test_that("SizeDist draws the empirical profiles without scRepertoire", {
  env <- new.env(parent = asNamespace("shiny"))
  env$input <- list(ir_chain = "both", ir_groupBy = "")
  env$req_plot_space <- function(...) NULL
  env$ir_param <- function(name, default) default
  env$req_scRepertoire <- function() stop("scRepertoire was loaded")
  env$ir_size_distribution_profile <- function() "compact profile"
  env$ir_size_profile_plot <- function(profiles, method) profiles
  env$ir_render_ggplotly <- function(p, ...) p
  render <- ir_test_body(
    "visualizations.R",
    "ir_plot_clonalSizeDistribution",
    "plotly::renderPlotly",
    env
  )
  expect_identical(render(), "compact profile")
})

test_that("Abundance renderer counts the encoded clones", {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(ir_test_root, "encode.R"), envir = env)
  for (expr in parse(file.path(ir_test_root, "data.R"))) {
    if (
      is.call(expr) &&
        as.character(expr[[1]])[1] %in% c("<-", "=") &&
        is.call(expr[[3]]) &&
        identical(expr[[3]][[1]], as.name("function"))
    ) {
      eval(expr, env)
    }
  }
  env$req_plot_space <- function(...) NULL
  pars <- list(chain = "both", cloneCall = "gene", groupBy = NULL)
  env$ir_params <- function() pars
  env$ir_param <- function(...) FALSE
  env$ir_order_by <- function() NULL
  env$ir_clonecall_col <- function(x) "CTgene"
  env$getImmuneRepertoire <- function() stop("unexpected sidecar hydration")
  env$ir_cell_metadata <- function() NULL
  env$ir_clonal_abundance_plotly <- function(counts, ...) counts
  env$ir_abundance_counts <- function() {
    env$ir_abundance_counts_for(env$ir_params(), env$ir_encoded())
  }
  encoded <- env$ir_encode_repertoire(list(
    s1 = data.frame(
      CTgene = c("TRAV1_TRBV2", "TRAV1_TRBV2", "TRAV3_TRBV2", "IGHV1_IGKV2"),
      CTaa = c("CAA_CBB", "CAA_CBB", "CAC_CBB", "CHH_CKK"),
      group = c("x", "y", "x", "x")
    ),
    s2 = data.frame(CTgene = c("a", "a", "b", ""), CTaa = "C")
  ))
  env$ir_encoded <- function() encoded
  render <- ir_test_body(
    "visualizations.R",
    "ir_plot_clonalAbundance",
    "plotly::renderPlotly",
    env
  )
  ## The blank clone is not counted.
  actual <- render()
  expect_identical(actual$sample, c("s1", "s1", "s2", "s2"))
  expect_identical(actual$abundance, c(1L, 2L, 1L, 2L))
  expect_identical(actual$n_clones, c(2L, 1L, 1L, 1L))
  pars$chain <- NULL
  expect_error(render(), class = "shiny.silent.error")
  ## Another chain or a grouping counts the clones in the app.
  env$ir_clonecall_col <- function(x) "CTaa"
  pars$chain <- "TRB"
  actual <- render()
  expect_identical(actual$sample, "s1")
  expect_identical(actual$abundance, 3L)
  expect_identical(actual$n_clones, 1L)
  pars$chain <- "both"
  pars$groupBy <- "group"
  encoded$columns$group <- factor(c("x", "y", "x", "x", NA, NA, NA, NA))
  actual <- render()
  actual <- actual[order(actual$sample, actual$abundance), ]
  expect_identical(actual$sample, c("x", "x", "y"))
  expect_identical(actual$abundance, c(1L, 2L, 2L))
  expect_identical(actual$n_clones, c(2L, 1L, 1L))
  encoded <- NULL
  expect_error(render(), class = "shiny.silent.error")
})

test_that("IR controls use descriptor chains and retain legacy detection", {
  env <- new.env(parent = asNamespace("shiny"))
  env$has_scRepertoire <- function() TRUE
  env$ir_param <- function(...) "Abundance"
  summary <- list(available = TRUE, chains = c("TRA", "TRB", "IGH"))
  env$getImmuneRepertoireSummary <- function() summary
  env$ir_encoded <- function() stop("unexpected sidecar hydration")
  env$ir_global_control_visible <- function(...) TRUE
  ## Column names come from the first-frame metadata; the full table would
  ## read every cell name of a thin CRB from its H5 file.
  env$viewerProjectionFirstFrameMetadata <- function() {
    data.frame(group = character())
  }
  env$getMetaData <- function() stop("unexpected metadata hydration")
  env$getGroups <- function() c("group", "missing")
  env$input <- list(ir_groupBy = "group")
  env$ir_flow_controls <- function(x) do.call(shiny::tagList, x)
  viewer_server_load(
    viewer_server_statements("immune_repertoire", "settings.R"),
    env,
    names = c(
      "ir_keep_selection",
      "ir_groupby_kind",
      "ir_groupby_memory",
      "ir_remembered_groupby"
    )
  )
  ## The control opens with the grouping it last had for this kind of tab.
  env$ir_groupby_memory(list(results = "group"))
  render <- ir_test_body("settings.R", "ir_main_params_UI", "renderUI", env)
  html <- as.character(render())
  expect_match(html, 'value="TRB"', fixed = TRUE)
  expect_match(html, 'value="IGH"', fixed = TRUE)
  expect_match(html, 'value="group" selected', fixed = TRUE)
  expect_no_match(html, 'value="missing"', fixed = TRUE)
  summary$chains <- character()
  env$ir_encoded <- function() list(chains = "TRD", columns = list())
  html <- as.character(render())
  expect_match(html, 'value="TRD"', fixed = TRUE)
  expect_no_match(html, 'value="IGH"', fixed = TRUE)
  summary$available <- FALSE
  expect_match(
    as.character(render()),
    "No immune repertoire data available",
    fixed = TRUE
  )
})

test_that("Homeostasis renderer counts the encoded clones of every chain", {
  env <- new.env(parent = asNamespace("shiny"))
  env$req_plot_space <- function(...) NULL
  pars <- list(chain = "both", cloneCall = "gene", groupBy = NULL)
  env$ir_params <- function() pars
  order <- NULL
  env$ir_order_by <- function() order
  env$ir_clonecall_col <- function(x) "CTgene"
  env$ir_encoded <- function() "encoded"
  env$ir_row_values <- function(encoded, name) name
  env$ir_homeostasis_counts <- function(encoded, column, chain, groups) {
    list(encoded, column, chain, groups)
  }
  env$ir_homeostasis_plot <- function(counts, clone_size, palette, order) {
    if (is.null(order)) counts else list(counts, order)
  }
  env$ir_clone_size <- function() 1
  env$IR_PALETTE <- "Harmonic"
  env$ir_render_ggplotly <- function(expr, ...) force(expr)
  env$req_scRepertoire <- function() stop("scRepertoire was loaded")
  render <- ir_test_body(
    "visualizations.R",
    "ir_plot_clonalHomeostasis",
    "plotly::renderPlotly",
    env
  )
  expect_identical(render(), list("encoded", "CTgene", "both", NULL))
  pars$chain <- NULL
  expect_error(render(), class = "shiny.silent.error")
  pars$chain <- "TRB"
  pars$groupBy <- "patient"
  env$getGroups <- function() "patient"
  expect_identical(render(), list("encoded", "CTgene", "TRB", "patient"))
  ## Dataset switches can deliver the old grouping before its selector resets.
  env$getGroups <- function() "new_dataset_group"
  expect_error(render(), class = "shiny.silent.error")
  pars$groupBy <- NULL
  order <- "alphanumeric"
  expect_identical(
    render(),
    list(list("encoded", "CTgene", "TRB", NULL), "alphanumeric")
  )
})

test_that("rebuilt IR toolbars keep the chosen clone call and chain", {
  env <- new.env(parent = asNamespace("shiny"))
  env$has_scRepertoire <- function() TRUE
  env$ir_param <- function(...) "Abundance"
  env$getImmuneRepertoireSummary <- function() {
    list(available = TRUE, chains = c("TRA", "TRB"))
  }
  env$ir_encoded <- function() stop("unexpected sidecar hydration")
  env$ir_global_control_visible <- function(...) TRUE
  env$getMetaData <- function() data.frame(group = character())
  env$getGroups <- function() "group"
  env$input <- list(ir_groupBy = "group", ir_cloneCall = "aa", ir_chain = "TRB")
  env$ir_flow_controls <- function(x) do.call(shiny::tagList, x)
  viewer_server_load(
    viewer_server_statements("immune_repertoire", "settings.R"),
    env,
    names = c(
      "ir_keep_selection",
      "ir_groupby_kind",
      "ir_groupby_memory",
      "ir_remembered_groupby"
    )
  )
  render <- ir_test_body("settings.R", "ir_main_params_UI", "renderUI", env)
  html <- as.character(render())
  expect_match(html, '<option value="aa" selected>', fixed = TRUE)
  expect_match(html, '<option value="TRB" selected>', fixed = TRUE)
  ## A chain the new dataset lacks falls back to all chains.
  env$input$ir_chain <- "IGH"
  expect_match(
    as.character(render()),
    '<option value="both" selected>',
    fixed = TRUE
  )
})
