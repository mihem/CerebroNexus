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

test_that("selection status can omit unsupported sharing", {
  status <- selection_status_function()

  with_share <- as.character(status("projection", "count"))
  without_share <- as.character(
    status("trekker_projection", "count", share = FALSE)
  )

  expect_match(with_share, "cerebro-share-open", fixed = TRUE)
  expect_false(grepl("cerebro-share-open", without_share, fixed = TRUE))
})
