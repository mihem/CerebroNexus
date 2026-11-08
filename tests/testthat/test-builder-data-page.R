builder_data_page_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  # Format labels and the deprecation note come from the format table.
  sys.source(builder_profile_inst_path("builder", "io.R"), envir = runtime)
  runtime$builder_example_directory <- function() list()
  runtime$builder_example_buttons_ui <- function(...) {
    shiny::tags$button(
      class = "builder-data-source example-btn",
      shiny::tags$span(
        class = "builder-data-source-copy",
        "Complete Viewer data"
      ),
      shiny::tags$span(
        class = "builder-data-source-tag is-example",
        "Load Example"
      )
    )
  }
  runtime$builder_stage_footer_ui <- function(status, ...) {
    shiny::tags$footer(
      class = "builder-stage-footer",
      shiny::tags$p(class = "builder-stage-footer-status", status),
      ...
    )
  }
  sys.source(
    builder_profile_inst_path("builder", "ui", "dataset_rail.R"),
    envir = runtime
  )
  runtime
}

builder_data_page_html <- function() {
  runtime <- builder_data_page_runtime()
  htmltools::renderTags(runtime$builder_empty_workbench_ui(
    formats = c("rds", "qs", "qs2"),
    examples = list()
  ))$html
}

test_that("Data page names readers disabled by missing packages", {
  runtime <- builder_data_page_runtime()
  formats <- list(
    list(id = "rds", label = "RDS", available = TRUE),
    list(
      id = "qs",
      label = "qs",
      available = FALSE,
      unavailable_reason = paste(
        "QS unavailable. Use RDS or QS2."
      ),
      unavailable_detail = "The R package 'qs' could not be loaded."
    )
  )
  html <- htmltools::renderTags(shiny::tagList(
    runtime$builder_empty_workbench_ui(formats = formats, examples = list()),
    runtime$builder_format_unavailable_ui(formats)
  ))$html

  expect_match(html, "builder-format-unavailable", fixed = TRUE)
  expect_match(html, "QS unavailable", fixed = TRUE)
  expect_match(html, "<details>", fixed = TRUE)
  expect_match(html, ">RDS<", fixed = TRUE)
  expect_false(grepl(">RDS, QS<", html, fixed = TRUE))
})

test_that("Data page replaces loading copy when every import failed", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- htmltools::renderTags(
    runtime$builder_failed_import_workbench_ui(2L)
  )$html

  expect_match(html, "builder-import-failed-state", fixed = TRUE)
  expect_match(html, "Datasets could not be loaded", fixed = TRUE)
  expect_match(html, "Read the error in the dataset list", fixed = TRUE)
  expect_match(html, 'role="alert"', fixed = TRUE)
  expect_false(grepl("Preparing your workspace", html, fixed = TRUE))
  expect_false(grepl('aria-busy="true"', html, fixed = TRUE))
})

test_that("Add files hints list RDS and QS2 and warn that QS is deprecated", {
  withr::local_package("shiny")
  runtime <- builder_data_page_runtime()
  html <- builder_data_page_html()

  expect_match(html, ">RDS, QS2<", fixed = TRUE)
  expect_false(grepl("QS2, QS", html, fixed = TRUE))
  expect_match(
    html,
    "QS files are deprecated and support will end soon. Save as RDS or QS2.",
    fixed = TRUE
  )

  formats <- list(
    list(id = "rds", label = "RDS", available = TRUE),
    list(id = "qs2", label = "qs2", available = TRUE),
    list(id = "qs", label = "qs", available = TRUE, deprecated = TRUE)
  )
  expect_identical(
    runtime$builder_advertised_format_labels(formats),
    c("RDS", "QS2")
  )
  note <- htmltools::renderTags(
    runtime$builder_deprecated_format_note_ui(formats)
  )$html
  expect_match(note, "builder-format-deprecated", fixed = TRUE)

  # No note when the deprecated reader is unavailable or absent.
  formats[[3L]]$available <- FALSE
  expect_null(runtime$builder_deprecated_format_note_ui(formats))
  expect_null(runtime$builder_deprecated_format_note_ui(formats[1:2]))
  expect_true(isTRUE(
    runtime$builder_formats[[3L]]$deprecated
  ))
})
