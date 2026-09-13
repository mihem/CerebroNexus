versionless_inst_dir <- function() {
  installed <- system.file(package = "CerebroNexus")
  if (nzchar(installed) && file.exists(file.path(installed, "app.R"))) {
    return(installed)
  }
  testthat::test_path("..", "..", "inst")
}

test_that("runtime and example directories do not encode a product version", {
  inst_dir <- versionless_inst_dir()
  legacy_viewer_version <- paste0("v1", ".4")

  expect_true(dir.exists(file.path(inst_dir, "viewer")))
  expect_true(dir.exists(file.path(inst_dir, "extdata", "examples")))
  expect_false(dir.exists(file.path(inst_dir, "shiny", legacy_viewer_version)))
  expect_false(dir.exists(file.path(
    inst_dir,
    "extdata",
    legacy_viewer_version
  )))

  relative_directories <- list.dirs(
    inst_dir,
    recursive = TRUE,
    full.names = FALSE
  )
  path_segments <- unlist(strsplit(relative_directories, "/", fixed = TRUE))

  expect_false(any(grepl("^v[0-9]+(?:\\.[0-9]+)+$", path_segments)))
})

test_that("the package exposes one versionless launcher", {
  exports <- getNamespaceExports("CerebroNexus")
  r_dir <- testthat::test_path("..", "..", "R")

  expect_true("launchCerebro" %in% exports)
  expect_false(any(grepl("^launchCerebroV[0-9]", exports)))
  if (dir.exists(r_dir)) {
    r_files <- basename(list.files(r_dir))
    expect_false(any(grepl("^launchCerebroV[0-9].*[.]R$", r_files)))
  }
})

test_that("the package exposes a versionless Cerebro data class", {
  exports <- getNamespaceExports("CerebroNexus")
  r_dir <- testthat::test_path("..", "..", "R")

  expect_true("Cerebro" %in% exports)
  expect_false(any(grepl("^Cerebro_v", exports)))
  if (dir.exists(r_dir)) {
    r_files <- basename(list.files(r_dir))
    expect_false(any(grepl("^class-Cerebro_v.*[.]R$", r_files)))
  }

  object <- Cerebro$new()
  expect_s3_class(object, "Cerebro")
  expect_true(inherits(object, "R6"))
  expect_true(is.function(object$getExpressionMatrix))
})

test_that("every bundled Cerebro fixture uses the versionless data class", {
  example_dir <- file.path(versionless_inst_dir(), "extdata", "examples")
  files <- sort(list.files(example_dir, pattern = "[.]crb$", full.names = TRUE))

  expect_gt(length(files), 0L)
  for (path in files) {
    object <- readCerebro(path)
    expect_true(inherits(object, "Cerebro"), info = basename(path))
  }
})

test_that("current code and documentation do not describe a legacy viewer version", {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  roots <- file.path(root, c("R", "man", "vignettes", "tests"))
  testthat::skip_if_not(
    all(dir.exists(roots)),
    "current source tree is not present in the installed-package layout"
  )
  files <- unlist(lapply(
    roots,
    list.files,
    pattern = "[.](R|Rd|Rmd)$",
    recursive = TRUE,
    full.names = TRUE
  ))
  legacy_viewer_version <- paste0("v1", ".4")
  occurrences <- vapply(
    files,
    function(path) {
      any(grepl(
        legacy_viewer_version,
        tolower(readLines(path, warn = FALSE)),
        fixed = TRUE
      ))
    },
    logical(1)
  )

  expect_false(
    any(occurrences),
    info = paste(files[occurrences], collapse = "\n")
  )
})

test_that("current launcher documentation stays versionless", {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  testthat::skip_if_not(
    dir.exists(file.path(root, "vignettes")),
    "current source tree is not present in the installed-package layout"
  )

  current_guides <- file.path(
    root,
    "vignettes",
    c(
      "launch_cerebro_with_pre-loaded_data_set.Rmd",
      "host_cerebro_on_shinyapps.Rmd"
    )
  )
  guide_text <- paste(
    unlist(lapply(current_guides, readLines, warn = FALSE)),
    collapse = "\n"
  )

  expect_false(grepl("launchCerebroV[0-9]", guide_text))
  expect_false(grepl("version\\s*=\\s*['\"]v[0-9]", guide_text))
  expect_false(grepl("(?:shiny|extdata)/v[0-9]", guide_text))
  expect_false(file.exists(file.path(root, "pkgdown", "_pkgdown.yml")))

  launcher_source <- readLines(
    file.path(root, "R", "launchCerebro.R"),
    warn = FALSE
  )
  expect_false(any(startsWith(launcher_source, "#\"")))
})

test_that("the Cerebro class overview stays versionless", {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  overview <- file.path(root, "vignettes", "overview_of_cerebro_class.Rmd")
  testthat::skip_if_not(
    file.exists(overview),
    "current source tree is not present in the installed-package layout"
  )

  text <- paste(readLines(overview, warn = FALSE), collapse = "\n")
  expect_false(grepl("Cerebro v[0-9]", text))
  expect_match(text, 'title: "Overview of the Cerebro class"', fixed = TRUE)
  expect_match(
    text,
    "\\VignetteIndexEntry{Overview of the Cerebro class}",
    fixed = TRUE
  )
})
