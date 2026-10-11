test_that("Linked views keeps selection enabled independently of hover", {
  ui <- paste(
    readLines(
      viewer_test_path("coordinated_views", "UI.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    ui,
    paste0(
      'shinyWidgets::materialSwitch\\(\\s*',
      'inputId = "cv-hover",\\s*label = "Show linked cell hover:",\\s*value = TRUE'
    ),
    perl = TRUE
  )
  expect_no_match(ui, 'cv-linked-interaction-toggle', fixed = TRUE)
  expect_no_match(javascript, "var LINKED_STATIC_ABOVE", fixed = TRUE)
  expect_match(
    javascript,
    "return singleActive ? singleInteractionIsStatic() : false;",
    fixed = TRUE
  )
  expect_no_match(javascript, "toggleLinkedInteraction", fixed = TRUE)
  expect_match(javascript, "if (interactionIsStatic()) return;", fixed = TRUE)
  expect_no_match(javascript, "linkedInteractionEnabled", fixed = TRUE)
})
