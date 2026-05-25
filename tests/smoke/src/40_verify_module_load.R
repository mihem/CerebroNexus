#!/usr/bin/env Rscript
# 40 - Shared module loadability (group C)
#
# Sources the current projection / group-filter UI fragments out of the
# installed shiny tree and asserts they still parse in isolation.
#
# The old shared module files under inst/shiny/v1.4/module/ no longer exist in
# the current layout, so this smoke test now exercises the files that actually
# back the v1.4 app:
#   * inst/shiny/v1.4/overview/UI_projection.R
#   * inst/shiny/v1.4/overview/UI_projection_group_filters.R
#   * inst/shiny/v1.4/gene_expression/UI_projection.R
#
# Does NOT spin up Shiny. Validates that these UI fragments remain source-able
# with a minimal mock Shiny session and the helper symbols they expect.

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(shiny)
})

shiny_root <- system.file("shiny/v1.4", package = "cerebroAppLite")
stopifnot(nzchar(shiny_root))

env <- new.env()
env$Cerebro.options <- list(cerebro_root = system.file(package = "cerebroAppLite"))
env$session <- shiny::MockShinySession$new()
env$output <- env$session$output
env$input <- env$session$input
env$getGroups <- function() c("cluster", "sample")
env$getGroupLevels <- function(group) c("A", "B")

source(file.path(shiny_root, "utility_functions.R"), local = env)
source(file.path(shiny_root, "overview/UI_projection.R"), local = env)
source(file.path(shiny_root, "overview/UI_projection_group_filters.R"), local = env)
source(file.path(shiny_root, "gene_expression/UI_projection.R"), local = env)

stopifnot(is.list(env$overview_projection_group_filters_info),
      is.list(env$expression_projection_main_parameters_info))

cat("[40] projection/group-filter UI fragments loaded:\n")
cat("       overview/UI_projection.R\n")
cat("       overview/UI_projection_group_filters.R\n")
cat("       gene_expression/UI_projection.R\n")
cat("       overview group-filter info title =",
  env$overview_projection_group_filters_info[["title"]], "\n")
cat("       expression projection info title =",
  env$expression_projection_main_parameters_info[["title"]], "\n")

result_dir <- "result/40_verify_module_load"
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
writeLines("ok", file.path(result_dir, "PASS"))
cat("[40] all checks passed.\n")
