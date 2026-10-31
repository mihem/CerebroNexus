builder_viewer_content_source_runtime <- function(local = parent.frame()) {
  builder_profile_source_runtime(local)
  builder_dir <- builder_profile_inst_path("builder")
  for (file in c(
    "recommend.R",
    "inspect.R",
    "preview.R",
    "extras.R",
    "analysis.R",
    "marker_import.R",
    "prerequisite.R",
    "state.R",
    "project.R",
    "plan.R",
    "app_bundle.R",
    "build.R"
  )) {
    sys.source(file.path(builder_dir, file), envir = local)
  }
  sys.source(file.path(builder_dir, "plan", "freeze.R"), envir = local)
  invisible(local)
}

builder_viewer_content_source_runtime()
builder_plan_contract_source_runtime(environment())

builder_viewer_content_plan_entry <- function() {
  entry <- list(
    id = "dataset-a",
    dataset_profile = list(
      identity = list(
        cells = list(canonical_ids = c("cell-a", "cell-b")),
        features = list(canonical_ids = c("Gene1", "Gene2"))
      )
    ),
    profile = list(
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      viewer_content = list(
        metadata = list(
          Phase = list(
            name = "Phase",
            classification = "categorical",
            group_eligible = TRUE,
            distinct_count = 3L,
            sample_values = c("G1", "S", "G2M")
          )
        )
      )
    ),
    levels = list(
      sample = c("one", "two"),
      batch = c("first", "second")
    ),
    settings = list(
      name = "Dataset A",
      organism = "hg",
      assay = "RNA",
      layer = "data",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      groups = "sample",
      included_groups = "sample",
      default_group = "sample",
      reductions = c("umap", "pca"),
      included_projections = c("umap", "pca"),
      default_projection = "pca",
      initial_projections = c("pca", "umap"),
      included_trajectories = list(
        monocle2 = c("lineage_a", "lineage_b")
      ),
      default_trajectory = list(
        method = "monocle2",
        name = "lineage_b"
      ),
      overview_point_size = 8,
      overview_point_opacity = 0.55,
      overview_percentage_cells_to_show = 60,
      cell_cycle_columns = "Phase",
      analyses = character(),
      tables = list(),
      images = list(),
      palette = "cerebro",
      group_color_overrides = list(sample = c(two = "#AA5500")),
      metadata_policy = list(
        included = c(
          "cell_barcode",
          "sample",
          "batch",
          "Phase",
          "nCount_RNA",
          "nFeature_RNA"
        ),
        excluded = character()
      )
    )
  )

  metadata <- c("sample", "batch", "Phase", "nCount_RNA", "nFeature_RNA")
  entry$dataset_profile$schema_version <- 2L
  contract <- builder_task6_entry()$dataset_profile
  entry$dataset_profile$source <- contract$source
  entry$dataset_profile$manifest <- contract$manifest
  entry$dataset_profile$content <- contract$content
  entry$dataset_profile$identity$cells$count <- 2L
  entry$dataset_profile$metadata <- list(
    columns = setNames(
      lapply(metadata, function(name) {
        list(
          name = name,
          class = "character",
          supported = TRUE,
          non_missing = 2L,
          unique_non_missing = 2L
        )
      }),
      metadata
    )
  )
  entry$settings$metadata_policy <- builder_metadata_policy_set_groups(
    builder_recommend_metadata(entry$dataset_profile),
    "sample"
  )
  entry
}

test_that("BuildPlan freezes the complete Viewer-content selection", {
  entry <- builder_viewer_content_plan_entry()
  entry$settings$spatial_point_appearance <- list(
    fov = list(point_opacity = 0.42, point_size = 8)
  )
  plan <- builder_make_plan(
    list(entry),
    withr::local_tempdir(),
    make_app = TRUE
  )

  expect_null(plan$error)
  item <- plan$items[[1L]]
  expect_identical(item$included_groups, "sample")
  expect_identical(item$default_group, "sample")
  expect_identical(item$included_projections, c("pca", "umap"))
  expect_identical(item$default_projection, "pca")
  expect_identical(item$initial_projections, c("pca", "umap"))
  expect_identical(
    item$included_trajectories,
    list(monocle2 = c("lineage_b", "lineage_a"))
  )
  expect_identical(
    item$default_trajectory,
    list(method = "monocle2", name = "lineage_b")
  )
  expect_identical(
    item$artifact_identity$trajectories,
    list(monocle2 = c("lineage_b", "lineage_a"))
  )
  expect_identical(item$overview_point_size, 8)
  expect_identical(item$overview_point_opacity, 0.55)
  expect_identical(
    item$spatial_point_appearance,
    list(fov = list(point_opacity = 0.42, point_size = 8))
  )
  expect_identical(item$overview_percentage_cells_to_show, 60)
  expect_identical(item$cell_cycle, "Phase")
  expect_identical(
    item$artifact_identity$metadata,
    c("cell_barcode", "sample", "nUMI", "nGene", "Phase", "batch")
  )
  expect_contains(item$metadata_policy$included, "batch")
  expect_identical(
    item$group_color_overrides,
    list(sample = c(two = "#AA5500"))
  )
  expect_identical(item$colors$sample[["two"]], "#AA5500")
  expect_identical(
    plan$app_options$point_size,
    list(
      overview_projection_point_size = 8,
      projection_point_opacity = 0.55
    )
  )
})

test_that("Viewer-only edits reuse CRBs and refresh the frozen App item", {
  root <- withr::local_tempdir()
  artifact_path <- file.path(root, "dataset-a.crb")
  writeBin(charToRaw("reusable-crb"), artifact_path)

  entry <- builder_viewer_content_plan_entry()
  entry$revision <- 1L
  entry$snapshot <- list(source_fingerprint = "source-a")
  entry$settings$overview_point_opacity <- 1
  entry$settings$spatial_point_appearance <- list()
  entry$settings$spatial_roi_settings <- list()
  original <- builder_make_plan(list(entry), root, make_app = TRUE)
  expect_null(original$error)

  digest_entry <- entry
  viewer_only <- c(
    "initial_projections",
    "overview_point_size",
    "overview_point_opacity",
    "overview_percentage_cells_to_show",
    "spatial_point_appearance",
    "spatial_roi_settings"
  )
  digest_entry$settings[
    intersect(names(digest_entry$settings), viewer_only)
  ] <- NULL
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = basename(artifact_path),
    fingerprint = builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_revision = 1L,
    built_from_source_fingerprint = "source-a",
    built_from_configuration = builder_project_configuration_digest(
      digest_entry
    ),
    plan_item = original$items[[1L]]
  )
  artifacts <- list(`dataset-a` = artifact)
  changes <- list(
    initial_projections = "pca",
    overview_point_size = 11,
    overview_point_opacity = 0.55,
    overview_percentage_cells_to_show = 80,
    spatial_point_appearance = list(
      fov = list(point_opacity = 0.6, point_size = 7)
    ),
    spatial_roi_settings = list(
      fov = list(
        lesion = list(
          rotation_degrees = 15,
          point_opacity = 0.7,
          point_size = 6
        )
      )
    )
  )

  for (field in names(changes)) {
    changed <- entry
    changed$revision <- 2L
    changed$settings[[field]] <- changes[[field]]
    expect_identical(
      length(builder_project_entries_requiring_crb(
        list(changed),
        artifacts,
        root
      )),
      0L,
      info = field
    )

    build_entry <- builder_project_entries_for_build(
      list(changed),
      artifacts,
      root
    )[[1L]]
    expect_identical(build_entry$load_state, "artifact_ready", info = field)
    frozen <- builder_make_plan(list(build_entry), root, make_app = TRUE)
    expect_null(frozen$error, info = field)
    expect_identical(
      frozen$items[[1L]][[field]],
      changes[[field]],
      info = field
    )
  }
})

test_that("Builder export freezes selected cell-cycle annotations into CRB", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Seurat")
  object <- SeuratObject::pbmc_small
  object$Phase <- factor(
    rep(c("G1", "S", "G2M"), length.out = ncol(object)),
    levels = c("G1", "S", "G2M")
  )
  object$batch <- rep(c("first", "second"), length.out = ncol(object))
  path <- withr::local_tempfile(fileext = ".crb")
  item <- list(
    name = "Cell cycle",
    organism = "hg",
    assay = "RNA",
    layer = "data",
    included_groups = c("groups", "Phase"),
    default_group = "groups",
    cell_cycle = "Phase",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    included_projections = names(object@reductions),
    expression_backend = "embedded",
    analyses = character(),
    metadata_policy = list(
      included = c(
        "cell_barcode",
        "groups",
        "Phase",
        "batch",
        "nCount_RNA",
        "nFeature_RNA"
      )
    )
  )

  expect_no_error(.builder_build_export(object, item, path))

  exported <- readCerebro(path)
  expect_identical(exported$getCellCycle(), "Phase")
  expect_contains(colnames(exported$getMetaData()), "Phase")
  expect_contains(colnames(exported$getMetaData()), "batch")
})

test_that("artifact metadata counts a group used for cell cycle only once", {
  entry <- builder_viewer_content_plan_entry()
  entry$levels$Phase <- c("G1", "S", "G2M")
  entry$settings$groups <- c("sample", "Phase")
  entry$settings$included_groups <- c("sample", "Phase")

  plan <- builder_make_plan(
    list(entry),
    withr::local_tempdir(),
    make_app = FALSE
  )

  expect_null(plan$error)
  metadata <- plan$items[[1L]]$artifact_identity$metadata
  expect_identical(sum(metadata == "Phase"), 1L)
  expect_false("Phase.1" %in% metadata)
})

test_that("generated-App content freezes defaults for every dataset", {
  item <- builder_make_plan(
    list(builder_viewer_content_plan_entry()),
    withr::local_tempdir(),
    make_app = TRUE
  )$items[[1L]]
  item$spatial_point_appearance <- list(
    fov = list(point_opacity = 0.42, point_size = 8)
  )

  frozen <- .builder_app_viewer_content(
    list(item),
    "Dataset A",
    list(overview_projection_point_size = 5)
  )

  expect_identical(
    frozen,
    list(
      `Dataset A` = list(
        default_projection = "pca",
        initial_projections = c("pca", "umap"),
        default_trajectory = list(
          method = "monocle2",
          name = "lineage_b"
        ),
        overview_point_size = 8,
        overview_point_opacity = 0.55,
        overview_percentage_cells_to_show = 60,
        spatial_percentage_cells_to_show = 100,
        spatial_point_appearance = list(
          fov = list(point_opacity = 0.42, point_size = 8)
        ),
        spatial_roi_settings = list(),
        display = builderViewerDisplayDefaults(),
        gene_expression = list(
          palette = "cerebro_orange",
          panel_mode = "shared",
          panel_palette = "default"
        ),
        extra_material_table_index = viewerExtraMaterialTableIndex(NULL)
      )
    )
  )
})

test_that("an explicit legacy App point-size option still wins", {
  plan <- builder_make_plan(
    list(builder_viewer_content_plan_entry()),
    withr::local_tempdir(),
    make_app = TRUE,
    app_options = list(
      point_size = list(overview_projection_point_size = 6)
    )
  )

  expect_null(plan$error)
  expect_identical(
    plan$app_options$point_size,
    list(
      overview_projection_point_size = 6,
      projection_point_opacity = 0.55
    )
  )
})

test_that("normal Review options keep the dataset point-size default", {
  builder_dir <- builder_profile_inst_path("builder")
  sys.source(
    file.path(builder_dir, "ui", "inspect_stage.R"),
    envir = environment()
  )
  sys.source(
    file.path(builder_dir, "ui", "review_stage.R"),
    envir = environment()
  )
  app_options <- builder_review_options_for_plan(
    builder_review_options(point_size = 3)
  )

  plan <- builder_make_plan(
    list(builder_viewer_content_plan_entry()),
    withr::local_tempdir(),
    make_app = TRUE,
    app_options = app_options
  )

  expect_null(plan$error)
  expect_identical(
    plan$app_options$point_size,
    list(
      overview_projection_point_size = 8,
      projection_point_opacity = 0.55
    )
  )
})

test_that("legacy plan callers without trajectory settings preserve payloads", {
  entry <- builder_viewer_content_plan_entry()
  entry$settings$included_trajectories <- NULL
  entry$settings$default_trajectory <- NULL

  plan <- builder_make_plan(
    list(entry),
    withr::local_tempdir(),
    make_app = FALSE
  )

  expect_null(plan$error)
  expect_null(plan$items[[1L]]$included_trajectories)
  expect_null(plan$items[[1L]]$artifact_identity$trajectories)
})

test_that("build preparation prunes and default-orders Viewer content", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  object@misc$trajectories <- list(
    monocle2 = list(
      lineage_a = list(meta = data.frame(a = 1), edges = data.frame(a = 1)),
      lineage_b = list(meta = data.frame(b = 1), edges = data.frame(b = 1))
    ),
    unsupported = list(other = list(raw = TRUE))
  )
  item <- list(
    included_projections = c("pca", "tsne"),
    included_trajectories = list(monocle2 = "lineage_b"),
    default_trajectory = list(method = "monocle2", name = "lineage_b"),
    assay = "RNA",
    layer = "data",
    manifest = list(),
    artifact_identity = list(
      group_levels = list(groups = sort(unique(as.character(object$groups))))
    ),
    tables = list()
  )

  prepared <- .builder_build_prepare(object, item)

  expect_identical(names(prepared@reductions), c("pca", "tsne"))
  expect_identical(names(prepared@misc$trajectories), "monocle2")
  expect_identical(
    names(prepared@misc$trajectories$monocle2),
    "lineage_b"
  )
})

test_that("ordinary safe metadata survives when it is not a formal Group", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  object$batch <- rep(c("first", "second"), length.out = ncol(object))
  item <- list(
    analyses = character(),
    metadata_policy = list(
      included = c(
        "cell_barcode",
        "groups",
        "batch",
        "nCount_RNA",
        "nFeature_RNA"
      )
    )
  )

  prepared <- .builder_build_apply_metadata_policy(object, item)

  expect_contains(colnames(prepared@meta.data), "batch")
  expect_identical(colnames(prepared@meta.data)[[1L]], "groups")
})

test_that("exportFromSeurat preserves Builder-selected PCA beside UMAP", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Seurat")
  object <- SeuratObject::pbmc_small
  object@reductions <- object@reductions[c("pca", "tsne")]
  names(object@reductions)[[2L]] <- "umap"
  path <- withr::local_tempfile(fileext = ".crb")

  expect_no_error(exportFromSeurat(
    object = object,
    assay = "RNA",
    slot = "data",
    file = path,
    experiment_name = "PCA and UMAP",
    organism = "hg",
    groups = "groups",
    main_group = "groups",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    add_all_meta_data = TRUE,
    projections = c("pca", "umap"),
    verbose = FALSE
  ))

  exported <- readCerebro(path)
  expect_identical(names(exported$projections), c("pca", "umap"))
})
