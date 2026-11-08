## Booting the full CerebroNexus app to idle can exceed shinytest2's 15s default
## load_timeout under R CMD check (the app starts from the installed/checked
## copy after the whole build+check+vignette pipeline has loaded the runner),
## so bare AppDriver$new() calls flake intermittently at construction. The
## heavy-boot tests already pass load_timeout = 60000 explicitly; raise the
## default here so every $new() gets the same headroom. An explicit
## load_timeout on a call still wins over this option.
options(shinytest2.load_timeout = 60 * 1000)

## Chrome itself starts before AppDriver's load timeout applies. Give cold CI
## starts the same headroom, and fail CI setup if Chrome cannot start instead
## of letting shinytest2 silently skip browser coverage.
options(chromote.timeout = 60)
if (
  identical(Sys.getenv("CI"), "true") &&
    identical(Sys.getenv("NOT_CRAN"), "true")
) {
  local({
    browser <- chromote::default_chromote_object()$new_session()
    browser$close()
  })
}

## inst/builder/app.R byte-compiles hundreds of closures when it is sourced, so
## the first import does not stall the UI. Tests source it over a hundred times
## and pay seconds each; R's JIT still compiles what a test calls.
options(cerebro.builder.precompile = FALSE)

## The Builder remembers its output folders for a crash sweep; keep that
## record out of the user's cache during tests.
Sys.setenv(CEREBRO_BUILDER_STATE_DIR = tempfile("builder-state-"))
