## testthat setup: run as if not on CRAN so shinytest2 tests are not skipped
Sys.setenv(NOT_CRAN = "true")
