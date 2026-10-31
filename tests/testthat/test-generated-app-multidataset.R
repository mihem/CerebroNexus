generated_app_e2e_reset_runtime()

test_that("dataset selector preserves Builder order and switches exact page contracts", {
  bundle <- generated_app_e2e_bundle()
  options <- generated_app_e2e_selector_options()
  expect_identical(options$labels, unname(bundle$config$dataset_labels))
  expect_identical(options$values, names(bundle$config$crb_file_to_load))

  for (name in names(bundle$fixtures)) {
    fixture <- generated_app_e2e_select_dataset(name)
    expected_value <- .generated_app_e2e_id(name)
    expect_identical(
      generated_app_e2e_value("input", "crb_file_selector"),
      expected_value,
      info = name
    )
    summary <- generated_app_e2e_dataset_summary()
    expect_match(
      summary$cells,
      as.character(fixture$expected$n_cells),
      fixed = TRUE,
      info = name
    )
    expect_match(
      summary$organism,
      generated_app_e2e_organism_label(fixture$expected$organism),
      fixed = TRUE,
      info = name
    )
    expect_identical(
      generated_app_e2e_visible_pages(),
      fixture$expected$visible_pages,
      info = name
    )
    expect_identical(
      intersect(
        generated_app_e2e_visible_pages(),
        fixture$expected$hidden_pages
      ),
      character(),
      info = paste(
        name,
        "must not retain a conditional page from the prior dataset"
      )
    )
  }
  generated_app_e2e_expect_clean_browser()
})

test_that("dataset switching resumes Data info outputs from a conditional page", {
  generated_app_e2e_select_dataset("analysis")
  generated_app_e2e_activate_tab("trajectory")

  fixture <- generated_app_e2e_select_dataset("basic")

  expect_identical(
    generated_app_e2e_value("input", "sidebar"),
    unname(generated_app_e2e_tab_catalog()[["data_info"]])
  )
  summary <- generated_app_e2e_dataset_summary()
  expect_match(
    summary$cells,
    as.character(fixture$expected$n_cells),
    fixed = TRUE
  )
  expect_match(
    summary$organism,
    generated_app_e2e_organism_label(fixture$expected$organism),
    fixed = TRUE
  )
})
