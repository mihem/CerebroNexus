test_that("a browser without a GPU shows 2 panels at a time, and the reader can lift that", {
  expect_node_fixture(
    "panel-cap.js",
    viewer_test_path("www", "cell_points_gpu.js"),
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS the check tells a GPU from software"
  )
})

test_that("the Spatial and Linked views pages carry the limit's note and the preference its switch", {
  note <- as.character(viewer_ui_test_env()$cerebroPanelCapNote())
  expect_match(note, "cerebro-cap-note", fixed = TRUE)
  expect_match(note, "data-cerebro-cap-unlock", fixed = TRUE)
  ui <- paste(
    readLines(viewer_test_path("coordinated_views", "UI.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ui, "cerebroPanelCapNote()", fixed = TRUE)
  preferences <- paste(
    readLines(viewer_test_path("load_data", "preferences.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(preferences, "panel_cap_unlock_checkbox", fixed = TRUE)
  ## Only a browser without a GPU shows the switch.
  css <- paste(
    readLines(viewer_test_path("www", "custom.css"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(css, "html.cerebro-no-gpu .cerebro-pref-no-gpu", fixed = TRUE)
})

test_that("the selection's coverage chips are counted in turns, and a released panel is projected for Canvas", {
  expect_node_fixture(
    "selection-coverage.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS the coverage chips"
  )
})
