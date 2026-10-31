generated_app_expected_content_pages <- function(expected) {
  unique(c(
    intersect(
      expected$visible_pages,
      generated_app_fixture_pages()$hidden
    ),
    intersect(expected$optional_payloads, c("spatial", "trekker"))
  ))
}

test_that("canonical fixtures freeze into one explicit multi-dataset plan", {
  expect_true(exists("generated_app_e2e_bundle", mode = "function"))
  bundle <- generated_app_e2e_bundle()

  expect_s3_class(bundle$plan, "builder_build_plan")
  expected_ids <- paste0(
    "e2e-",
    c("basic", "analysis", "spatial", "immune-tcr-hla", "immune-bcr", "trekker")
  )
  expect_identical(bundle$plan$dataset_order, expected_ids)
  expect_identical(
    vapply(bundle$plan$items, `[[`, character(1), "id"),
    expected_ids
  )
  expect_true(bundle$plan$make_app)
  expect_identical(bundle$plan$app_options$initial_dataset, "e2e-basic")
  expect_identical(bundle$plan$app_options$initial_dataset_mode, "explicit")
  expect_false(bundle$plan$app_options$show_upload_ui)
  expect_identical(
    bundle$plan$app_options$welcome_message,
    "Generated App E2E"
  )
  expect_identical(
    bundle$plan$app_options$point_size,
    list(
      overview_projection_point_size = 6,
      projection_point_opacity = 1
    )
  )
  expect_false(bundle$plan$app_options$variable_to_compare)
})

test_that("Builder settings survive profile and BuildPlan normalization", {
  bundle <- generated_app_e2e_bundle()

  for (name in names(bundle$fixtures)) {
    fixture <- bundle$fixtures[[name]]
    expected <- fixture$expected
    entry <- bundle$entries[[name]]
    item <- bundle$plan$items[[match(entry$id, bundle$plan$dataset_order)]]

    expect_identical(entry$settings$name, expected$dataset_name, info = name)
    expect_identical(entry$settings$organism, expected$organism, info = name)
    expect_identical(
      entry$settings$groups,
      fixture$builder_settings$groups,
      info = name
    )
    expect_identical(
      entry$settings$default_group,
      expected$default_group,
      info = name
    )
    expect_identical(
      entry$settings$default_projection,
      expected$default_projection,
      info = name
    )
    expect_true(
      all(expected$groups %in% entry$settings$metadata_policy$included),
      info = name
    )

    expect_identical(item$name, expected$dataset_name, info = name)
    expect_identical(item$organism, expected$organism, info = name)
    expect_identical(item$included_groups, expected$groups, info = name)
    expect_identical(
      item$included_projections,
      expected$projections,
      info = name
    )
    expect_identical(item$default_group, expected$default_group, info = name)
    expect_identical(
      item$default_projection,
      expected$default_projection,
      info = name
    )
    expect_identical(
      item$viewer_page_expectations$visible_conditional,
      generated_app_expected_content_pages(expected),
      info = name
    )
    for (group in names(expected$palettes)) {
      configured <- expected$palettes[[group]]
      expect_identical(
        unname(item$colors[[group]][names(configured)]),
        unname(configured),
        info = paste(name, group)
      )
    }
  }
})

test_that("generated CRBs preserve source identities, values, and page causes", {
  bundle <- generated_app_e2e_bundle()
  expect_identical(names(bundle$crbs), names(bundle$fixtures))

  for (name in names(bundle$fixtures)) {
    fixture <- bundle$fixtures[[name]]
    expected <- fixture$expected
    crb <- bundle$crbs[[name]]
    metadata <- crb$getMetaData()
    expression <- crb$getExpressionMatrix()

    expect_identical(nrow(metadata), expected$n_cells, info = name)
    expect_identical(crb$getCellNames(), expected$cell_ids, info = name)
    expect_identical(nrow(expression), expected$n_genes, info = name)
    expect_identical(colnames(expression), expected$cell_ids, info = name)
    expect_identical(rownames(expression), expected$gene_ids, info = name)
    expect_identical(
      crb$getExperiment()$organism,
      expected$organism,
      info = name
    )
    expect_identical(crb$getGroups(), expected$groups, info = name)
    expect_identical(
      crb$availableProjections(),
      expected$projections,
      info = name
    )
    expect_setequal(
      .builder_crb_visible_pages(crb),
      generated_app_expected_content_pages(expected)
    )
  }

  analysis <- bundle$crbs$analysis
  expect_identical(
    analysis$getEnrichedPathways("offline", "seurat_clusters")$Term,
    c("Pathway A", "Pathway B", "Pathway C")
  )
  expect_identical(
    analysis$getExtraTable("fixture_summary")$value,
    c("28", "52", "offline fixture")
  )
  expect_identical(
    analysis$getNamesOfTrajectories("monocle2"),
    "analysis_lineage"
  )

  spatial <- bundle$crbs$spatial
  expect_identical(spatial$availableSpatial(), c("section_a", "section_b"))
  source_images <- builder_image_collection_normalize(
    bundle$entries$spatial$settings$images
  )
  configured_images <- bundle$config$spatial_images[[
    .generated_app_e2e_id("spatial")
  ]]
  for (section in spatial$availableSpatial()) {
    value <- spatial$getSpatialData(section)
    contract <- bundle$fixtures$spatial$expected$image_alignment[[section]]
    image_label <- names(source_images[[section]])[[1L]]
    expected_alignment <- builder_alignment_payload(
      source_images[[section]][[image_label]]
    )
    expected_alignment$source <- image_label

    expect_identical(value[["histology_images", exact = TRUE]], list())
    expect_null(value[["histology_image", exact = TRUE]])
    expect_null(value[["histology_image_bounds", exact = TRUE]])
    expect_identical(value$histology_alignment, expected_alignment)

    descriptors <- configured_images[[section]]
    expect_identical(names(descriptors), image_label)
    descriptor <- descriptors[[image_label]]
    expect_identical(descriptor$label, image_label)
    expect_identical(
      descriptor$bounds,
      unlist(contract$bounds, use.names = TRUE)
    )
    expect_true(.builder_app_safe_relative(descriptor$path))
    expect_false(startsWith(descriptor$path, "data:"))
    expect_true(file.exists(file.path(bundle$app_dir, descriptor$path)))
  }

  expect_false(is.null(bundle$crbs$immune_tcr_hla$getImmuneRepertoire()))
  expect_false(is.null(bundle$crbs$immune_tcr_hla$getHLATyping()))
  expect_false(is.null(bundle$crbs$immune_bcr$getImmuneRepertoire()))
  expect_s3_class(bundle$crbs$immune_bcr$getHLATyping(), "data.frame")
  expect_identical(nrow(bundle$crbs$immune_bcr$getHLATyping()), 0L)
  expect_false(is.null(bundle$crbs$trekker$getTrekker()))
})

test_that("private App config and all outputs remain inside the temporary release", {
  bundle <- generated_app_e2e_bundle()
  config <- bundle$config

  expect_true(dir.exists(bundle$app_dir))
  expect_true(file.exists(file.path(bundle$app_dir, "app.R")))
  expect_true(file.exists(file.path(bundle$app_dir, "cerebro_config.rds")))
  expect_identical(
    names(config$crb_file_to_load),
    unname(vapply(names(bundle$fixtures), .generated_app_e2e_id, character(1)))
  )
  expect_identical(
    config$dataset_labels,
    stats::setNames(
      vapply(
        bundle$fixtures,
        function(fixture) fixture$expected$dataset_name,
        character(1)
      ),
      names(config$crb_file_to_load)
    )
  )
  expect_identical(config$initial_dataset, "e2e-basic")
  expect_false(config$show_upload_ui)
  expect_identical(config$welcome_message, "Generated App E2E")
  expect_identical(
    config$point_size,
    stats::setNames(
      rep(6, length(config$crb_file_to_load)),
      names(config$crb_file_to_load)
    )
  )
  expect_identical(
    config$point_opacity,
    stats::setNames(
      rep(1, length(config$crb_file_to_load)),
      names(config$crb_file_to_load)
    )
  )
  expect_false(config$variable_to_compare)

  root <- paste0(normalizePath(bundle$root, winslash = "/"), "/")
  outputs <- c(bundle$published$built, bundle$app_dir)
  expect_true(all(startsWith(
    normalizePath(outputs, winslash = "/"),
    root
  )))
  expect_true(all(file.exists(bundle$published$built)))
  expect_identical(
    basename(unname(bundle$published$built)),
    unname(vapply(
      bundle$fixtures,
      function(fixture) fixture$expected$output_files$crb,
      character(1)
    ))
  )

  mouse_conversion <- utils::read.delim(
    gzfile(file.path(bundle$app_dir, "extdata", "mm10_gene_ID_name.tsv.gz")),
    nrows = 1L,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_identical(
    names(mouse_conversion),
    c("gencode", "ensembl", "havana", "symbol", "type")
  )
  expect_identical(
    unname(unlist(mouse_conversion[1L, ], use.names = FALSE)),
    c(
      "ENSMUSG00000102693.1",
      "ENSMUSG00000102693",
      "OTTMUSG00000049935.1",
      "4933401J01Rik",
      "TEC"
    )
  )
})
