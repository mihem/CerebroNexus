test_that("generated App fixtures expose independent literal contracts", {
  expect_true(exists("generated_app_fixture_matrix", mode = "function"))

  fixtures <- generated_app_fixture_matrix()
  expect_named(
    fixtures,
    c(
      "basic",
      "analysis",
      "spatial",
      "immune_tcr_hla",
      "immune_bcr",
      "trekker"
    )
  )

  expected_fields <- c(
    "dataset_name",
    "organism",
    "n_cells",
    "n_genes",
    "cell_ids",
    "gene_ids",
    "groups",
    "group_levels",
    "group_counts",
    "projections",
    "projection_coordinates",
    "default_group",
    "default_projection",
    "palettes",
    "visible_pages",
    "hidden_pages",
    "optional_payloads",
    "spatial_sections",
    "image_alignment",
    "output_files",
    "app_settings"
  )
  expect_true(all(vapply(
    fixtures,
    function(fixture) {
      identical(
        names(fixture),
        c(
          "object",
          "attachments",
          "builder_settings",
          "expected"
        )
      ) &&
        identical(names(fixture$expected), expected_fields)
    },
    logical(1)
  )))
})

test_that("basic expression fixture is small, sparse, and human-checkable", {
  fixture <- generated_app_fixture_matrix()$basic
  object <- fixture$object
  expected <- fixture$expected

  expect_s4_class(object, "Seurat")
  expect_equal(dim(object), c(60L, 36L))
  expect_identical(colnames(object), expected$cell_ids)
  expect_identical(rownames(object), expected$gene_ids)
  expect_s4_class(
    SeuratObject::LayerData(object, assay = "RNA", layer = "counts"),
    "sparseMatrix"
  )
  expect_s4_class(
    SeuratObject::LayerData(object, assay = "RNA", layer = "data"),
    "sparseMatrix"
  )
  expect_true(all(
    c(
      "seurat_clusters",
      "sample",
      "treatment",
      "nCount_RNA",
      "nFeature_RNA",
      "percent.mt",
      "qc_missing"
    ) %in%
      colnames(object@meta.data)
  ))
  expect_identical(sum(is.na(object$qc_missing)), 2L)
  expect_identical(SeuratObject::Reductions(object), c("pca", "umap", "tsne"))
  expect_identical(
    stats::setNames(
      lapply(expected$groups, function(group) {
        unclass(table(object@meta.data[[group]]))
      }),
      expected$groups
    ),
    expected$group_counts
  )
  expect_identical(expected$default_group, "seurat_clusters")
  expect_identical(expected$default_projection, "umap")
  expect_identical(
    unname(expected$palettes$seurat_clusters),
    c("#A63D14", "#D97706", "#F2B84B")
  )
  expect_identical(expected$optional_payloads, character())
})

test_that("analysis fixture carries only deterministic offline results", {
  fixture <- generated_app_fixture_matrix()$analysis
  object <- fixture$object
  expected <- fixture$expected

  expect_equal(dim(object), c(52L, 28L))
  expect_identical(
    expected$optional_payloads,
    c(
      "marker_genes",
      "most_expressed_genes",
      "mean_expression",
      "enriched_pathways",
      "trajectory",
      "extra_material"
    )
  )
  expect_identical(
    object@misc$enriched_pathways$offline$seurat_clusters$Term,
    c("Pathway A", "Pathway B", "Pathway C")
  )
  expect_identical(
    object@misc$extra_material$tables$fixture_summary$metric,
    c("cells", "genes", "source")
  )
  expect_identical(
    names(object@misc$trajectories$monocle2),
    "analysis_lineage"
  )
  expect_false(any(grepl(
    "enrichr|https?://",
    capture.output(str(object@misc)),
    ignore.case = TRUE
  )))
})

test_that("specialized fixtures preserve page-gating causes", {
  fixtures <- generated_app_fixture_matrix()

  spatial <- fixtures$spatial
  expect_equal(dim(spatial$object), c(40L, 30L))
  expect_identical(
    SeuratObject::Images(spatial$object),
    c(
      "section_a",
      "section_b"
    )
  )
  expect_identical(
    spatial$expected$spatial_sections,
    c(
      "section_a",
      "section_b"
    )
  )
  expect_true(all(file.exists(vapply(
    spatial$attachments,
    `[[`,
    character(1),
    "path"
  ))))
  expect_false(identical(
    spatial$expected$image_alignment$section_a$bounds,
    spatial$expected$image_alignment$section_b$bounds
  ))

  expect_true(all(
    c("immune_repertoire", "hla_tcr_motifs") %in%
      fixtures$immune_tcr_hla$expected$visible_pages
  ))
  expect_true(
    "immune_repertoire" %in%
      fixtures$immune_bcr$expected$visible_pages
  )
  expect_true(
    "hla_tcr_motifs" %in%
      fixtures$immune_bcr$expected$hidden_pages
  )

  trekker <- fixtures$trekker$object@misc$trekker
  expect_gt(length(trekker$barcodes), 0L)
  expect_lte(length(trekker$barcodes), fixtures$trekker$expected$n_cells)
  expect_identical(length(trekker$x), length(trekker$barcodes))
  expect_identical(length(trekker$y), length(trekker$barcodes))
  expect_identical(length(trekker$ux), length(trekker$barcodes))
  expect_identical(length(trekker$uy), length(trekker$barcodes))
  expect_identical(length(trekker$clusters), length(trekker$barcodes))
  expect_identical(names(fixtures$trekker$object@misc), "trekker")
  expect_length(SeuratObject::Images(fixtures$trekker$object), 0L)
})

test_that("fixture constructors are deterministic and restore caller RNG", {
  set.seed(4100)
  caller_seed <- .Random.seed
  first <- generated_app_fixture_matrix()
  expect_identical(.Random.seed, caller_seed)
  second <- generated_app_fixture_matrix()
  expect_identical(.Random.seed, caller_seed)

  expect_identical(
    lapply(first, function(fixture) {
      serialize(fixture$object, NULL, version = 3L)
    }),
    lapply(second, function(fixture) {
      serialize(fixture$object, NULL, version = 3L)
    })
  )
  expect_identical(
    lapply(first, `[[`, "expected"),
    lapply(second, `[[`, "expected")
  )
})
