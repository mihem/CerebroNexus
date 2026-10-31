generated_app_e2e_reset_runtime()

test_that("offline analysis pages render the retained source tables", {
  fixture <- generated_app_e2e_select_dataset("analysis")

  generated_app_e2e_activate_tab("marker_genes")
  generated_app_e2e_wait_input("marker_genes_selected_method")
  expect_identical(
    generated_app_e2e_value("input", "marker_genes_selected_method"),
    "cerebro_seurat"
  )
  expect_identical(
    generated_app_e2e_value("input", "marker_genes_selected_table"),
    "seurat_clusters"
  )
  generated_app_e2e_set_input("marker_genes_table_filter_switch", TRUE)
  markers <- generated_app_e2e_table_text(
    "marker_genes_table",
    contains = c("GENE001", "GENE002", "GENE003", "Delta", "Epsilon")
  )
  expect_match(markers, "2.4", fixed = TRUE)

  generated_app_e2e_activate_tab("most_expressed_genes")
  generated_app_e2e_wait_input("most_expressed_genes_selected_group")
  expect_identical(
    generated_app_e2e_value("input", "most_expressed_genes_selected_group"),
    fixture$expected$default_group
  )
  generated_app_e2e_set_input(
    "most_expressed_genes_table_filter_switch",
    TRUE
  )
  expressed <- generated_app_e2e_table_text(
    "most_expressed_genes_table",
    contains = c(
      "GENE004",
      "GENE005",
      "GENE006",
      "95",
      "90",
      "82",
      "Stored score"
    )
  )
  # The legacy pct field has no unit provenance; retain master's neutral label.
  expect_match(expressed, "Stored score", fixed = TRUE)

  generated_app_e2e_set_input("most_expressed_genes_metric_type", "mean_expr")
  means <- generated_app_e2e_table_text(
    "most_expressed_genes_table",
    contains = c("GENE004", "GENE005", "GENE006", "3.5", "2.75", "1.5")
  )
  expect_match(means, "Mean", fixed = TRUE)

  generated_app_e2e_activate_tab("enriched_pathways")
  generated_app_e2e_wait_input("enriched_pathways_selected_method")
  expect_identical(
    generated_app_e2e_value("input", "enriched_pathways_selected_method"),
    "offline"
  )
  expect_identical(
    generated_app_e2e_value("input", "enriched_pathways_selected_table"),
    "seurat_clusters"
  )
  generated_app_e2e_set_input("enriched_pathways_table_filter_switch", TRUE)
  pathways <- generated_app_e2e_table_text(
    "enriched_pathways_table",
    contains = c("Pathway A", "Pathway B", "Pathway C", "9.5", "8.25")
  )
  expect_match(pathways, "GENE003", fixed = TRUE)

  generated_app_e2e_activate_tab("extra_material")
  generated_app_e2e_wait_input("extra_material_selected_category")
  expect_identical(
    generated_app_e2e_value("input", "extra_material_selected_category"),
    "tables"
  )
  extra <- generated_app_e2e_table_text(
    "extra_material_table",
    contains = c("cells", "28", "genes", "52", "offline fixture")
  )
  expect_match(extra, "source", fixed = TRUE)
})

test_that("offline trajectory selection renders its complete source lineage", {
  generated_app_e2e_select_dataset("analysis")
  generated_app_e2e_activate_tab("trajectory")
  generated_app_e2e_wait_input("trajectory_selected_method")

  expect_identical(
    generated_app_e2e_value("input", "trajectory_selected_method"),
    "monocle2"
  )
  expect_identical(
    generated_app_e2e_value("input", "trajectory_selected_name"),
    "analysis_lineage"
  )

  generated_app_e2e_wait_plotly("trajectory_projection")
  expect_true(
    isTRUE(generated_app_e2e_driver()$get_js(
      paste0(
        "(function(){var host=document.getElementById('trajectory_projection_cell_view_host');",
        "var canvas=host&&host.querySelector('.cv-canvas-wrap > canvas:not(.cv-mini)');",
        "return !!(canvas&&canvas.width>0&&canvas.height>0);})()"
      )
    ))
  )
  generated_app_e2e_expect_clean_browser()
})
