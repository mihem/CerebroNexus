## The Builder's source mode (inst/builder/app_bundle/backend.R) sources
## R/cerebro_io.R alone and calls a few of its readers. Every package function
## those readers reach must be defined in that file: one that lives elsewhere
## in the package is missing there. Under load_all(), as in CI, the package's
## internals are on the search path and hide such a gap, so this test resolves
## the calls itself instead of running the readers.

## Needs the package sources, which a CRAN check does not have: CI only.
skip_on_cran()

test_that("the Builder's source runtime defines what its readers call", {
  source_file <- testthat::test_path("..", "..", "R", "cerebro_io.R")
  builder_dir <- testthat::test_path("..", "..", "inst", "builder")
  skip_if_not(
    file.exists(source_file) && dir.exists(builder_dir),
    "package sources not available"
  )

  runtime <- new.env(parent = baseenv())
  sys.source(source_file, envir = runtime)

  builder_code <- unlist(lapply(
    list.files(
      builder_dir,
      pattern = "[.][Rr]$",
      recursive = TRUE,
      full.names = TRUE
    ),
    readLines,
    warn = FALSE
  ))
  entries <- unique(regmatches(
    builder_code,
    regexpr(
      '(?<=\\.builder_app_runtime_function\\(")[^"]+',
      builder_code,
      perl = TRUE
    )
  ))
  expect_gt(length(entries), 0L)

  package <- asNamespace("CerebroNexus")
  defined_here <- function(name) {
    exists(name, envir = runtime, inherits = FALSE) &&
      is.function(runtime[[name]])
  }
  for (entry in entries) {
    expect_true(defined_here(entry), info = entry)
    reached <- character()
    todo <- entry
    while (length(todo)) {
      name <- todo[[1L]]
      todo <- todo[-1L]
      if (name %in% reached || !defined_here(name)) {
        next
      }
      reached <- c(reached, name)
      todo <- c(
        todo,
        codetools::findGlobals(runtime[[name]], merge = FALSE)$functions
      )
    }
    called <- unique(unlist(lapply(reached, function(name) {
      codetools::findGlobals(runtime[[name]], merge = FALSE)$functions
    })))
    ## Package functions defined in another file of R/.
    missing <- called[
      !vapply(called, defined_here, logical(1)) &
        vapply(called, exists, logical(1), envir = package, inherits = FALSE)
    ]
    expect_identical(missing, character(), info = entry)
  }
})
