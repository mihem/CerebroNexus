selection_status_function <- function() {
  source_file <- viewer_test_path("shiny_UI.R")
  expressions <- parse(source_file)
  index <- which(vapply(
    expressions,
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], as.name("<-")) &&
        identical(expression[[2L]], as.name("cerebroSelectionStatus"))
    },
    logical(1)
  ))
  environment <- list2env(
    list(
      tags = shiny::tags,
      div = shiny::div,
      icon = shiny::icon,
      actionButton = shiny::actionButton,
      htmlOutput = shiny::htmlOutput,
      tagList = shiny::tagList
    ),
    parent = globalenv()
  )
  eval(expressions[[index]], envir = environment)
  environment$cerebroSelectionStatus
}

test_that("selection status exposes Share view when supported", {
  status <- selection_status_function()

  with_portable <- as.character(status("projection", "count"))
  without_portable <- as.character(
    status("trekker_projection", "count", portable = FALSE)
  )

  expect_match(with_portable, "cerebro-config-open", fixed = TRUE)
  expect_match(with_portable, "Share view", fixed = TRUE)
  expect_false(grepl("cerebro-config-open", without_portable, fixed = TRUE))
})
