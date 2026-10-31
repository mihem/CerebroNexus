generated_app_e2e_reset_runtime()

test_that("Linked views renders both retained spatial sections and images", {
  fixture <- generated_app_e2e_select_dataset("spatial")
  expect_identical(
    generated_app_e2e_visible_pages(),
    fixture$expected$visible_pages
  )
  generated_app_e2e_open_linked_views(fixture)
  driver <- generated_app_e2e_driver()

  driver$wait_for_js(
    "document.getElementById('cv-pick-spatial').selectize != null",
    timeout = 60000
  )
  expect_identical(
    unname(unlist(
      driver$get_js(paste0(
        "Object.keys(document.getElementById('cv-pick-spatial')",
        ".selectize.options)"
      )),
      use.names = FALSE
    )),
    fixture$expected$spatial_sections
  )

  expect_setequal(
    unlist(driver$get_js(
      "document.getElementById('cv-pick-spatial').selectize.getValue()"
    )),
    fixture$expected$spatial_sections
  )
  driver$wait_for_js(
    paste0(
      "document.querySelectorAll(",
      "'.cv-pane:not(.cv-hidden) .cv-ptitle').length>=3"
    ),
    timeout = 60000
  )
  titles <- generated_app_e2e_linked_titles()
  for (section in fixture$expected$spatial_sections) {
    expect_true(any(grepl(section, titles, fixed = TRUE)), info = section)
  }

  ## Linked views uses the first eligible image for each FOV; image selection
  ## belongs to Spatial. Verify both scopes survive in the portable view state.
  driver$wait_for_js(
    "window.cerebroLinkedViewsState.capture().view.spatial_backgrounds.length === 2",
    timeout = 60000
  )
  backgrounds <- driver$get_js(
    "window.cerebroLinkedViewsState.capture().view.spatial_backgrounds"
  )
  expect_setequal(
    vapply(backgrounds, `[[`, character(1), "section"),
    fixture$expected$spatial_sections
  )
  expect_true(all(vapply(
    backgrounds,
    function(x) {
      identical(x$mode, "image") && nzchar(x$image_id)
    },
    logical(1)
  )))

  expect_identical(
    driver$get_js(
      "document.querySelector(\"a[href='#shiny-tab-spatial']\").offsetParent !== null"
    ),
    TRUE
  )
  generated_app_e2e_expect_clean_browser()
})
