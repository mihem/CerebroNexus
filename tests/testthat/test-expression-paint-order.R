test_that("new viewer defaults draw the highest expression on top, unlabelled", {
  sys.source(
    viewer_test_path("core", "viewer_content_contract.R"),
    environment()
  )
  defaults <- builderViewerDisplayDefaults()
  expect_identical(defaults$gene_plotting_order, "Highest expression on top")
  expect_true(all(!unlist(defaults[grepl("group_labels$", names(defaults))])))
  expect_identical(
    builderViewerDisplayDefaults(list(
      gene_plotting_order = "Input order"
    ))$gene_plotting_order,
    "Input order"
  )
  expect_error(
    builderViewerDisplayDefaults(list(gene_plotting_order = "Random")),
    "Viewer display settings are invalid"
  )
  expect_identical(
    builderViewerDisplayDefaults(list(group_labels = TRUE))$group_labels,
    TRUE
  )
})

test_that("expression paint order preserves cells when disabled", {
  expect_node_fixture(
    "expression-paint-order.js",
    viewer_test_path("www", "cell_views.js")
  )
})
