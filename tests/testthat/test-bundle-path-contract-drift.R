builder_path_contract_files <- function() {
  source_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "createShinyApp.R"
  )
  runtime_path <- builder_test_inst_path(
    "builder",
    "core",
    "bundle_path_contract.R"
  )
  if (!file.exists(runtime_path)) {
    runtime_path <- system.file(
      "builder",
      "core",
      "bundle_path_contract.R",
      package = "CerebroNexus"
    )
  }
  list(source = source_path, runtime = runtime_path)
}

builder_path_contract_definitions <- function(path) {
  expressions <- parse(file = path, keep.source = FALSE)
  definitions <- list()
  for (expression in expressions) {
    if (
      is.call(expression) &&
        identical(expression[[1L]], as.name("<-")) &&
        is.symbol(expression[[2L]]) &&
        is.call(expression[[3L]]) &&
        identical(expression[[3L]][[1L]], as.name("function"))
    ) {
      definitions[[as.character(expression[[2L]])]] <- paste(
        deparse(expression[[3L]], width.cutoff = 500L),
        collapse = "\n"
      )
    }
  }
  definitions
}

test_that("Builder path safety functions match the generated-App contract", {
  paths <- builder_path_contract_files()

  testthat::skip_if_not(
    file.exists(paths$source),
    "R/ source tree not present (installed-package layout)"
  )
  expect_true(file.exists(paths$runtime))
  source <- builder_path_contract_definitions(paths$source)
  runtime <- builder_path_contract_definitions(paths$runtime)
  expect_gt(length(runtime), 0L)
  builder_only <- c(
    ".windowsPathSegmentInvalid",
    ".nativeFileConditionFields",
    ".nativeFileOperationAttempt",
    ".nativeFileOperationMessage",
    ".normalizeOverridePath",
    ".pathWithin"
  )
  ## The package reaches the CRB contract through .crbContract(); the
  ## standalone Builder runtime carries its own copies of those helpers.
  standalone <- ".portableBundlePath"
  source <- lapply(source, function(definition) {
    gsub(
      ".crbContract()$.runtimeWindowsPathSegmentInvalid",
      ".windowsPathSegmentInvalid",
      definition,
      fixed = TRUE
    )
  })
  shared <- setdiff(names(runtime), c(builder_only, standalone))
  expect_setequal(setdiff(names(runtime), names(source)), builder_only)
  expect_true(all(shared %in% names(source)))
  expect_identical(source[shared], runtime[shared])

  crb_path <- builder_test_inst_path("viewer", "core", "crb_contract.R")
  crb <- builder_path_contract_definitions(crb_path)
  expect_identical(
    runtime[[".windowsPathSegmentInvalid"]],
    crb[[".runtimeWindowsPathSegmentInvalid"]]
  )
  crb_env <- new.env(parent = baseenv())
  sys.source(crb_path, crb_env)
  runtime_env <- new.env(parent = baseenv())
  sys.source(paths$runtime, runtime_env)
  outcome <- function(expression) {
    tryCatch(expression, error = conditionMessage)
  }
  requirement <- paste0(
    "must be one portable relative path using forward slashes, without ",
    "empty, '.', '..', or Windows-incompatible segments."
  )
  candidates <- c(
    "a/b.h5",
    "a//b",
    "../a",
    "a/./b",
    "/a",
    "C:/a",
    "a\\b",
    "a/",
    "CON/b",
    "a/b.",
    "",
    NA_character_
  )
  for (path in candidates) {
    expect_identical(
      outcome(runtime_env$.portableBundlePath(path, "Backend path")),
      outcome(crb_env$.runtimePortableBackendPath(
        path,
        "Backend path",
        requirement
      ))
    )
  }
})
