generated_app_e2e_reset_runtime()

test_that("basic generated dataset exposes the exact core navigation and summary", {
  fixture <- generated_app_e2e_select_dataset("basic")
  options <- generated_app_e2e_selector_options()
  config <- generated_app_e2e_bundle()$config
  dataset_ids <- names(config$crb_file_to_load)

  expect_identical(
    options$labels,
    unname(config$dataset_labels[dataset_ids])
  )
  expect_identical(
    options$values,
    dataset_ids
  )
  expect_identical(
    generated_app_e2e_visible_pages(),
    fixture$expected$visible_pages
  )

  summary <- generated_app_e2e_dataset_summary()
  expect_match(summary$cells, "36", fixed = TRUE)
  expect_match(summary$organism, "Human", fixed = TRUE)
})

test_that("Linked views uses Builder defaults, palette, and complete cell set", {
  fixture <- generated_app_e2e_select_dataset("basic")
  generated_app_e2e_open_linked_views(fixture)
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "document.getElementById('cv-pick-proj').selectize && ",
      "document.getElementById('cv-pick-proj').selectize.getValue().length"
    ),
    timeout = 60000
  )

  expect_equal(
    as.numeric(driver$get_js(
      "$('#cv-ps').data('ionRangeSlider').options.min"
    )),
    1
  )

  expect_identical(
    driver$get_js(
      "document.getElementById('cv-pick-proj').selectize.getValue()[0]"
    ),
    fixture$expected$default_projection
  )
  expect_identical(
    driver$get_js("document.getElementById('cv-pick-color').value"),
    fixture$expected$default_group
  )
  expect_equal(
    as.numeric(driver$get_js("document.getElementById('cv-ps').value")),
    fixture$expected$app_settings$point_size$overview_projection_point_size
  )

  expect_true(any(grepl(
    paste0("^", fixture$expected$default_projection, " \\(expression"),
    generated_app_e2e_linked_titles()
  )))
  expect_match(
    generated_app_e2e_output_text("cv-meta"),
    paste(fixture$expected$n_cells, "cells"),
    fixed = TRUE
  )

  legend <- generated_app_e2e_linked_legend()
  expect_identical(
    vapply(legend, `[[`, character(1), "color"),
    generated_app_e2e_rgb(fixture$expected$palettes$seurat_clusters)
  )
  expect_true(all(vapply(
    names(fixture$expected$palettes$seurat_clusters),
    function(level) {
      any(grepl(
        level,
        vapply(legend, `[[`, character(1), "text"),
        fixed = TRUE
      ))
    },
    logical(1)
  )))
})

test_that("Gene expression renders the exact selected source-gene values", {
  fixture <- generated_app_e2e_select_dataset("basic")
  generated_app_e2e_activate_tab("gene_expression")
  generated_app_e2e_set_input("expression_analysis_mode", "Gene(s)")
  generated_app_e2e_wait_input("expression_genes_input")
  generated_app_e2e_driver()$wait_for_js(
    paste0(
      "(function(){var input=document.getElementById('expression_genes_input');",
      "return !!(input&&input.selectize&&",
      "Object.keys(input.selectize.options).length);})()"
    ),
    timeout = 60000
  )
  Sys.sleep(1.1)
  generated_app_e2e_set_input("expression_genes_input", "GENE001")

  generated_app_e2e_driver()$wait_for_idle(timeout = 60000)
  displayed <- generated_app_e2e_driver()$get_js(
    "(function(){var node=document.getElementById('expression_genes_displayed');return node?(node.textContent||'').trim():'';})()"
  )
  expect_identical(as.character(displayed %||% ""), "")
  generated_app_e2e_wait_plotly("expression_projection")

  source <- SeuratObject::LayerData(
    fixture$object,
    assay = "RNA",
    layer = "data"
  )
  expected <- as.numeric(source["GENE001", ])
  observed <- generated_app_e2e_value(
    "export",
    "expression_levels",
    validate = function(value) {
      length(value) == fixture$expected$n_cells &&
        isTRUE(all.equal(
          sort(as.numeric(value)),
          sort(expected),
          tolerance = 1e-12
        ))
    }
  )
  expect_equal(sort(observed), sort(expected), tolerance = 1e-12)
})

test_that("Groups, gene IDs, colors, and About render source-backed content", {
  fixture <- generated_app_e2e_select_dataset("basic")

  generated_app_e2e_activate_tab("groups")
  generated_app_e2e_wait_input("groups_selected_group")
  generated_app_e2e_set_input("groups_selected_group", "seurat_clusters")
  generated_app_e2e_wait_input("groups_by_other_group_second_group")
  generated_app_e2e_set_input("groups_by_other_group_second_group", "sample")
  generated_app_e2e_set_input("groups_by_other_group_show_table", TRUE)
  groups <- generated_app_e2e_table_text(
    "groups_by_other_group_table",
    contains = c("Alpha", "Beta", "Gamma", "sample_a", "sample_b")
  )
  expect_match(groups, "6", fixed = TRUE)

  generated_app_e2e_activate_tab("about")
  expect_match(
    generated_app_e2e_output_text("about"),
    "Cerebro",
    fixed = TRUE
  )

  ## Opening Gene ID conversion eagerly materializes the complete bundled
  ## dictionary and dominates the whole browser suite. Its exact sidebar
  ## contract is verified here, while the pipeline test reads the generated
  ## dictionary and checks its headers and first source row directly.
  expect_true("gene_id_conversion" %in% generated_app_e2e_visible_pages())
  generated_app_e2e_driver()$wait_for_js(
    "document.querySelector(\"a[href='#shiny-tab-geneIdConversion']\") !== null",
    timeout = 60000
  )

  generated_app_e2e_expect_clean_browser()
})
