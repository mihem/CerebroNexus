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
        identical(x[[1]], as.name("<-")) &&
        identical(
          paste(deparse(x[[2]]), collapse = ""),
          paste0("output$", output)
        )
    },
    as.list(exprs)
  )[[1]]
  body <- ir_test_call(expr[[3]], wrapper)[[2]]
  eval(call("function", as.pairlist(list()), body), env)
}

test_that("native SizeDist does not initialize the unused analytical fallback", {
  env <- new.env(parent = asNamespace("shiny"))
  env$input <- list(ir_chain = "both", ir_groupBy = "")
  env$req_plot_space <- function(...) NULL
  env$ir_params <- function() list(chain = "both", groupBy = NULL)
  env$ir_param <- function(name, default) default
  data <- list(A = data.frame(CTstrict = rep("x", 100000)))
  env$ir_data <- function() data
  env$req_scRepertoire <- function() stop("analytical fallback")
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
  data <- list(A = data[[1]][1:2, , drop = FALSE])
  expect_error(render(), "analytical fallback")
})

test_that("Abundance renderer reads the current pack before repertoire data", {
  env <- new.env(parent = asNamespace("shiny"))
  for (expr in parse(file.path(ir_test_root, "data.R"))) {
    if (
      is.call(expr) &&
        identical(expr[[1]], as.name("<-")) &&
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
  env$ir_data <- function() stop("unexpected sidecar hydration")
  env$ir_clonal_abundance_plotly <- identity
  packed <- data.frame(sample = "A", abundance = 2L, n_clones = 3L)
  env$viewerPackCurrent <- function() packed
  env$viewerPackImmuneAbundance <- function(pack, column) pack
  render <- ir_test_body(
    "visualizations.R",
    "ir_plot_clonalAbundance",
    "plotly::renderPlotly",
    env
  )
  expect_identical(render(), packed)
  packed <- data.frame(sample = "B", abundance = 1L, n_clones = 8L)
  expect_identical(render(), packed)
  packed <- packed[FALSE, ]
  expect_identical(render(), packed)
  packed <- NULL
  env$ir_data <- function() list(legacy = data.frame(CTgene = c("a", "a", "b")))
  actual <- render()
  expect_identical(actual$sample, c("legacy", "legacy"))
  expect_identical(actual$abundance, c(1L, 2L))
  expect_identical(actual$n_clones, c(1L, 1L))
  pars$chain <- NULL
  expect_error(render(), class = "shiny.silent.error")
  pars$chain <- "TRB"
  env$req_scRepertoire <- function() stop("analytical fallback")
  expect_error(render(), "analytical fallback")
})

test_that("IR controls use descriptor chains and retain legacy detection", {
  env <- new.env(parent = asNamespace("shiny"))
  env$has_scRepertoire <- function() TRUE
  env$ir_param <- function(...) "Abundance"
  summary <- list(available = TRUE, chains = c("TRA", "TRB", "IGH"))
  env$getImmuneRepertoireSummary <- function() summary
  env$ir_data_raw <- function() stop("unexpected sidecar hydration")
  env$detect_chains <- function(raw) if (is.null(raw)) character() else "TRD"
  env$ir_global_control_visible <- function(...) TRUE
  env$getMetaData <- function() data.frame(group = character())
  env$getGroups <- function() c("group", "missing")
  env$input <- list(ir_groupBy = "group")
  env$ir_flow_controls <- function(x) do.call(shiny::tagList, x)
  render <- ir_test_body("settings.R", "ir_main_params_UI", "renderUI", env)
  html <- as.character(render())
  expect_match(html, 'value="TRB"', fixed = TRUE)
  expect_match(html, 'value="IGH"', fixed = TRUE)
  expect_match(html, 'value="group" selected', fixed = TRUE)
  expect_no_match(html, 'value="missing"', fixed = TRUE)
  summary$chains <- character()
  env$ir_data_raw <- function() list(a = data.frame(CTgene = "TRD"))
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

test_that("Homeostasis renderer defers hydration and preserves analytical fallbacks", {
  env <- new.env(parent = asNamespace("shiny"))
  env$req_plot_space <- function(...) NULL
  pars <- list(chain = "both", cloneCall = "gene", groupBy = NULL)
  env$ir_params <- function() pars
  order <- NULL
  env$ir_order_by <- function() order
  env$ir_clonecall_col <- function(x) "CTgene"
  env$ir_data <- function() stop("unexpected sidecar hydration")
  env$ir_homeostasis_counts <- function(data, column) "packed counts"
  env$ir_homeostasis_plot <- function(counts, ...) counts
  env$ir_clone_size <- function() 1
  env$IR_PALETTE <- "Harmonic"
  env$ir_render_ggplotly <- function(expr, ...) force(expr)
  env$req_scRepertoire <- function() stop("analytical fallback")
  render <- ir_test_body(
    "visualizations.R",
    "ir_plot_clonalHomeostasis",
    "plotly::renderPlotly",
    env
  )
  expect_identical(render(), "packed counts")
  pars$chain <- NULL
  expect_error(render(), class = "shiny.silent.error")
  pars$chain <- "TRB"
  expect_error(render(), "analytical fallback")
  pars$chain <- "both"
  pars$groupBy <- "patient"
  env$getGroups <- function() "patient"
  env$ir_data <- function() "current raw data"
  env$ir_grouped_homeostasis_counts <- function(data, column, group) {
    expect_identical(data, "current raw data")
    expect_identical(group, "patient")
    "grouped counts"
  }
  expect_identical(render(), "grouped counts")
  env$getGroups <- function() "new_dataset_group"
  expect_error(render(), class = "shiny.silent.error")
  pars$groupBy <- NULL
  order <- "alphanumeric"
  expect_error(render(), "analytical fallback")
})
