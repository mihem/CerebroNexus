generated_app_e2e_reset_runtime()

.generated_app_e2e_click_inner_tab <- function(id, value, timeout = 60000) {
  selector <- sprintf("#%s a[data-value='%s']", id, value)
  selector_js <- .generated_app_e2e_js_value(selector)
  id_js <- .generated_app_e2e_js_value(id)
  value_js <- .generated_app_e2e_js_value(value)
  driver <- generated_app_e2e_driver()
  driver$wait_for_js(
    paste0(
      "(function(){var link=document.querySelector(",
      selector_js,
      ");if(!link)return false;var active=!!(link.parentElement&&",
      "link.parentElement.classList.contains('active'));var app=window.Shiny&&",
      "Shiny.shinyapp;var current=app&&app.$inputValues?",
      "app.$inputValues[",
      id_js,
      "]:null;if(!active)link.click();return active&&current===",
      value_js,
      ";})()"
    ),
    timeout = timeout
  )
  invisible(value)
}

.generated_app_e2e_select_text <- function(id, timeout = 60000) {
  id_js <- .generated_app_e2e_js_value(id)
  generated_app_e2e_driver()$wait_for_js(
    paste0(
      "(function(){var node=document.getElementById(",
      id_js,
      ");return !!(node && node.options && node.options.length);})()"
    ),
    timeout = timeout
  )
  as.character(generated_app_e2e_driver()$get_js(paste0(
    "(function(){var node=document.getElementById(",
    id_js,
    ");return Array.from(node.options).map(function(option){",
    "return option.textContent.trim();}).join(' | ');})()"
  )))
}

test_that("TCR and HLA fixture renders repertoire data and stored typing", {
  fixture <- generated_app_e2e_select_dataset("immune_tcr_hla")
  expect_identical(
    generated_app_e2e_visible_pages(),
    fixture$expected$visible_pages
  )

  generated_app_e2e_activate_tab("immune_repertoire")
  expect_identical(
    generated_app_e2e_value("input", "ir_tabs"),
    "Clonal UMAP"
  )
  receptor <- generated_app_e2e_value(
    "input",
    "ir_p_umap_receptor",
    validate = function(value) identical(value, "TCR")
  )
  expect_identical(receptor, "TCR")
  generated_app_e2e_set_input("ir_p_umap_projection", "tsne")
  generated_app_e2e_wait_plotly("ir_clonalUMAP_projection")
  expect_identical(
    generated_app_e2e_plotly_point_count("ir_clonalUMAP_projection"),
    fixture$expected$n_cells
  )
  .generated_app_e2e_click_inner_tab("ir_tabs", "Abundance")
  expect_match(.generated_app_e2e_select_text("ir_chain"), "TRB", fixed = TRUE)
  generated_app_e2e_set_input("ir_chain", "TRB")
  generated_app_e2e_wait_plotly("ir_plot_clonalAbundance")

  generated_app_e2e_activate_tab("hla_tcr_motifs")
  .generated_app_e2e_click_inner_tab("hla_tabs", "Data & QC")
  coverage <- generated_app_e2e_table_text(
    "hla_coverage_table",
    contains = c("donor1", "donor2", "HLA-A")
  )
  expect_match(coverage, "6", fixed = TRUE)
  typing <- generated_app_e2e_table_text(
    "hla_normalized_preview",
    contains = c("HLA-A*01:01", "HLA-A*02:01", "synthetic")
  )
  expect_match(typing, "2-field", fixed = TRUE)
})

test_that("BCR-only fixture renders IGH repertoire without an HLA page", {
  fixture <- generated_app_e2e_select_dataset("immune_bcr")
  expect_identical(
    generated_app_e2e_visible_pages(),
    fixture$expected$visible_pages
  )
  expect_false("hla_tcr_motifs" %in% generated_app_e2e_visible_pages())

  generated_app_e2e_activate_tab("immune_repertoire")
  .generated_app_e2e_click_inner_tab("ir_tabs", "Clonal UMAP")
  receptor <- generated_app_e2e_value(
    "input",
    "ir_p_umap_receptor",
    validate = function(value) identical(value, "BCR")
  )
  expect_identical(receptor, "BCR")
  expect_identical(
    generated_app_e2e_value("input", "ir_p_umap_projection"),
    "umap"
  )
  generated_app_e2e_wait_plotly("ir_clonalUMAP_projection")
  expect_identical(
    generated_app_e2e_plotly_point_count("ir_clonalUMAP_projection"),
    fixture$expected$n_cells
  )
  .generated_app_e2e_click_inner_tab("ir_tabs", "Abundance")
  expect_match(.generated_app_e2e_select_text("ir_chain"), "IGH", fixed = TRUE)
  expect_identical(generated_app_e2e_value("input", "ir_chain"), "both")
  generated_app_e2e_wait_plotly("ir_plot_clonalAbundance")
  generated_app_e2e_expect_clean_browser()
})
