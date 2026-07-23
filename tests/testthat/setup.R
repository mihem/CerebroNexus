## testthat setup: run as if not on CRAN so shinytest2 tests are not skipped
Sys.setenv(NOT_CRAN = "true")

## Booting the full cerebro app to idle can exceed shinytest2's 15s default
## load_timeout under R CMD check (the app starts from the installed/checked
## copy after the whole build+check+vignette pipeline has loaded the runner),
## so bare AppDriver$new() calls flake intermittently at construction. The
## heavy-boot tests already pass load_timeout = 60000 explicitly; raise the
## default here so every $new() gets the same headroom. An explicit
## load_timeout on a call still wins over this option.
options(shinytest2.load_timeout = 60 * 1000)

## The bundled app lives at the installed package root, so shiny::loadSupport()
## warns about the package layout even though sourcing these files is intended.
local_app_support <- function(app_dir, envir = rlang::caller_env()) {
  withCallingHandlers(
    shinytest2::local_app_support(app_dir, envir = envir),
    warning = function(warning) {
      if (
        grepl(
          "Loading R/ subdirectory for Shiny application",
          conditionMessage(warning),
          fixed = TRUE
        )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
