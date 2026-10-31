builder_repo_source("review.R")
builder_repo_source("workflow.R")

builder_test_review_snapshot <- function(
  revision = 1L,
  identity = list(
    schema_version = 1L,
    dataset_order = "dataset-a",
    checks = c(`dataset-a` = "configuration-a:revision:1")
  )
) {
  builder_review_snapshot(
    revision = revision,
    identity = identity,
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      filename = "dataset-a.crb",
      organism = "hg",
      cell_count = 10L,
      gene_count = 20L,
      groups = "cluster",
      included_groups = "cluster",
      reductions = "umap",
      included_projections = "umap",
      default_group = "cluster",
      default_projection = "umap",
      colors = list(cluster = c(A = "#111111")),
      viewer_page_expectations = list(),
      metadata_policy = list(included = "cluster"),
      manifest = list(),
      acknowledgements = character(),
      readiness = "ready",
      expression_backend = "embedded",
      sidecars = character(),
      estimated_disk_bytes = 100
    )),
    make_app = FALSE
  )
}

test_that("Review owns a lightweight configuration snapshot", {
  snapshot <- builder_test_review_snapshot()

  expect_s3_class(snapshot, "builder_review_snapshot")
  expect_true(builder_review_snapshot_valid(snapshot))
  expect_identical(
    builder_review_snapshot_identity(snapshot),
    snapshot$identity
  )
  expect_named(
    snapshot,
    c(
      "revision",
      "identity",
      "readiness",
      "error",
      "dataset_order",
      "items",
      "make_app",
      "viewer_page_expectations",
      "output_release",
      "warnings"
    )
  )
  expect_false(any(
    c(
      "out_dir",
      "targets",
      "existing_targets",
      "source_snapshot_identities",
      "viewer_bundle_asset_claims",
      "private_asset_claims"
    ) %in%
      names(snapshot)
  ))
})

test_that("workflow stores and confirms snapshots rather than BuildPlans", {
  snapshot <- builder_test_review_snapshot()
  state <- builder_workflow_state()

  expect_named(
    state,
    c("stage", "review_snapshot", "confirmation", "revision")
  )

  state <- builder_reduce_workflow(
    state,
    list(type = "open_review", snapshot = snapshot)
  )
  expect_identical(state$stage, "review")
  expect_identical(state$review_snapshot, snapshot)

  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", snapshot = snapshot)
  )
  expect_identical(state$stage, "build")
  expect_true(builder_workflow_confirmation_matches(state, snapshot))
  expect_identical(
    state$confirmation$snapshot_revision,
    snapshot$revision
  )
})

test_that("workflow does not accept a full BuildPlan as Review state", {
  plan <- structure(
    list(readiness = "ready", revision = 1L),
    class = c("builder_build_plan", "list")
  )

  expect_error(
    builder_reduce_workflow(
      builder_workflow_state(),
      list(type = "open_review", snapshot = plan)
    ),
    "review snapshot",
    ignore.case = TRUE
  )
})

test_that("Review projection is built only from loaded facts and settings", {
  entry <- builder_minimal_entry(id = "dataset-a", name = "Dataset A")
  entry$revision <- 3L
  entry$profile$n_cells <- 10L
  entry$profile$n_genes <- 20L
  entry$snapshot <- list(closure_bytes = 100)
  entry$settings$included_groups <- "cluster"
  entry$settings$included_projections <- "umap"
  entry$settings$included_trajectories <- list(monocle2 = "lineage-a")
  entry$settings$default_group <- "cluster"
  entry$settings$default_projection <- "umap"
  entry$settings$tables <- list(large = list(table = raw(1024L * 1024L)))
  entry$settings$images <- list(
    section = list(source_uri = strrep("x", 1024L * 1024L))
  )
  state <- list(
    readiness = "ready",
    analyses = character(),
    manifest = list(),
    metadata_policy = list(included = "cluster"),
    acknowledgements = character(),
    page_expectations = list()
  )
  identity <- list(
    schema_version = 1L,
    dataset_order = "dataset-a",
    checks = c(`dataset-a` = "configuration-a:revision:3")
  )

  snapshot <- builder_review_snapshot_from_entries(
    entries = list(entry),
    states = list(state),
    identity = identity
  )

  expect_true(builder_review_snapshot_valid(snapshot))
  expect_identical(snapshot$items[[1L]]$cell_count, 10L)
  expect_identical(snapshot$items[[1L]]$included_projections, "umap")
  expect_identical(
    snapshot$items[[1L]]$included_trajectories,
    list(monocle2 = "lineage-a")
  )
  expect_false(any(
    c(
      "tables",
      "images",
      "source_snapshot_identity",
      "private_assets",
      "viewer_bundle_assets"
    ) %in%
      names(snapshot$items[[1L]])
  ))
  expect_lt(as.numeric(object.size(snapshot)), as.numeric(object.size(entry)))
})

test_that("Review recovers a legacy CRB spatial section catalogue", {
  entry <- builder_minimal_entry(id = "dataset-a", name = "Dataset A")
  entry$revision <- 4L
  entry$load_state <- "artifact_ready"
  entry$snapshot <- NULL
  entry$profile <- list()
  entry$settings$images <- list(
    fov = list(
      `H&E` = list(source_path = "managed-image.jpg"),
      DAPI = list(source_path = "managed-image-2.jpg")
    )
  )
  sections <- paste0("fov", c("", paste0(".", 2:16)))
  entry$project_artifact <- list(
    plan_item = list(
      filename = "dataset-a.crb",
      spatial_alignment = list(),
      artifact_identity = list(spatial_sections = sections),
      expression_backend = "embedded",
      sidecars = character(),
      estimated_runtime = "no optional analysis runtime",
      estimated_disk_bytes = 100
    )
  )
  state <- list(
    readiness = "artifact_ready",
    analyses = character(),
    manifest = list(),
    metadata_policy = list(),
    acknowledgements = character(),
    page_expectations = list()
  )
  identity <- list(
    schema_version = 1L,
    dataset_order = "dataset-a",
    checks = c(`dataset-a` = "artifact:legacy")
  )

  snapshot <- builder_review_snapshot_from_entries(
    entries = list(entry),
    states = list(state),
    identity = identity
  )
  alignment <- snapshot$items[[1L]]$spatial_alignment

  expect_identical(alignment$section_count, 16L)
  expect_identical(alignment$image_count, 2L)
  expect_identical(alignment$points_only, sections[-1L])
  expect_false(any(c("rotation", "scale", "dx", "dy") %in% names(alignment)))
})

test_that("Review UI consumes the lightweight snapshot directly", {
  runtime <- new.env(parent = globalenv())
  builder_profile_source_runtime(runtime)
  builder_stage_contract_source_runtime(runtime)
  snapshot <- builder_test_review_snapshot()

  model <- runtime$builder_review_model(snapshot)

  expect_identical(model$dataset_count, 1L)
  expect_identical(model$datasets[[1L]]$name, "Dataset A")
  expect_true(model$can_build)
})

test_that("Configure and Review transitions never freeze a BuildPlan", {
  review_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  workflow_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl(
    "frozen_review_plan <- reactive",
    review_server,
    fixed = TRUE
  ))
  expect_match(
    review_server,
    "builder_review_snapshot_from_entries",
    fixed = TRUE
  )
  continue_block <- regmatches(
    workflow_server,
    regexpr(
      "observeEvent\\(input\\$continue_to_review, \\{[\\s\\S]+?render_build_workbench <- function",
      workflow_server,
      perl = TRUE
    )
  )
  expect_length(continue_block, 1L)
  expect_match(continue_block, "current_review_snapshot()", fixed = TRUE)
  expect_false(grepl("builder_freeze_plan", continue_block, fixed = TRUE))
  expect_false(grepl("frozen_review_plan", continue_block, fixed = TRUE))
})

test_that("Build retry checks the lightweight snapshot before preparing", {
  build <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  recovery <- regmatches(
    build,
    regexpr(
      "builder_build_recovery_ready <- function\\(\\) \\{[\\s\\S]+?\\n\\}",
      build,
      perl = TRUE
    )
  )

  expect_length(recovery, 1L)
  expect_match(recovery, "current_review_snapshot()", fixed = TRUE)
  expect_false(grepl(
    "freeze_materialized_plan_for_output",
    recovery,
    fixed = TRUE
  ))
  expect_false(grepl("freeze_plan_for_output", recovery, fixed = TRUE))
})

test_that("workflow transitions no longer need browser-side loading patches", {
  javascript <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  review_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("setDatasetCheckBusy", javascript, fixed = TRUE))
  expect_false(grepl("builder_dataset_check_state", javascript, fixed = TRUE))
  expect_false(grepl(
    "builder_dataset_check_state",
    review_server,
    fixed = TRUE
  ))
  expect_false(grepl("dataset_check_finishing", review_server, fixed = TRUE))
  expect_false(grepl("Opening Review", javascript, fixed = TRUE))
  expect_false(grepl("Opening Build", javascript, fixed = TRUE))
})
