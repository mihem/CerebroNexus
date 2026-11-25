## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

builder_stage_contract_source_runtime(environment())

test_that("the region panel is part of the spatial alignment card", {
  html <- builder_stage_html(builder_region_panel_ui())
  for (action in c("draw", "undo", "apply", "clear", "clear_all")) {
    expect_match(
      html,
      paste0('data-region-action="', action, '"'),
      fixed = TRUE
    )
  }
  expect_match(html, 'id="builder-region-panel"', fixed = TRUE)
  expect_match(html, 'id="builder-region-status"', fixed = TRUE)

  card <- builder_stage_html(builder_spatial_alignment_ui(
    "enhance",
    list(
      sections = "slice1",
      choices = c(slice1 = "slice1"),
      scenes = list(list(id = "slice1", label = "slice1"))
    )
  ))
  expect_match(card, "has_region_frame", fixed = TRUE)
  expect_match(card, 'id="builder-region-panel"', fixed = TRUE)
})
