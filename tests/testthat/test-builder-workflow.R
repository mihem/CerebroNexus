builder_repo_source("review.R")
builder_repo_source("workflow.R")

builder_workflow_test_identity <- function(
  check = "configuration-a:revision:1"
) {
  list(
    schema_version = 1L,
    dataset_order = "dataset-a",
    checks = c(`dataset-a` = check)
  )
}

builder_workflow_test_snapshot <- function(
  revision = "1",
  identity = builder_workflow_test_identity(),
  make_app = TRUE
) {
  builder_review_snapshot(
    revision = revision,
    identity = identity,
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      filename = "dataset-a.crb",
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
    make_app = make_app
  )
}

builder_workflow_test_plan <- function(
  review_identity = builder_workflow_test_identity(),
  out_dir = tempfile("builder-output-"),
  welcome_message = "Welcome"
) {
  structure(
    list(
      error = NULL,
      readiness = "ready",
      revision = 1L,
      review_identity = review_identity,
      dataset_order = "dataset-a",
      out_dir = out_dir,
      make_app = TRUE,
      app_contract_version = 1L,
      overwrite = FALSE,
      items = list(list(id = "dataset-a", readiness = "ready")),
      targets = file.path(out_dir, "dataset-a.crb"),
      app_options = list(welcome_message = welcome_message),
      app_auth = list(enabled = FALSE)
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("BuildPlan owns the identity of the checked Review snapshot", {
  identity <- builder_workflow_test_identity()
  plan <- builder_workflow_test_plan(review_identity = identity)

  expect_identical(builder_review_plan_identity(plan), identity)

  relocated <- plan
  relocated$out_dir <- tempfile("relocated-output-")
  relocated$targets <- file.path(relocated$out_dir, "dataset-a.crb")
  expect_identical(builder_review_plan_identity(relocated), identity)

  changed <- plan
  changed$review_identity <- builder_workflow_test_identity(
    "configuration-a:revision:2"
  )
  expect_false(identical(
    builder_review_plan_identity(changed),
    identity
  ))

  plan$review_identity <- NULL
  expect_error(
    builder_review_plan_identity(plan),
    "confirmed review identity"
  )
})

test_that("final build identity adds output-only settings", {
  plan <- builder_workflow_test_plan()
  changed <- plan
  changed$app_options$welcome_message <- "Changed at Build"

  expect_identical(
    builder_final_build_identity(plan)$review,
    builder_final_build_identity(changed)$review
  )
  expect_false(identical(
    builder_final_build_identity(plan),
    builder_final_build_identity(changed)
  ))
})

test_that("workflow advances through snapshot review and confirmation", {
  snapshot <- builder_workflow_test_snapshot()
  state <- builder_workflow_state()

  expect_identical(
    state,
    structure(
      list(
        stage = "upload",
        review_snapshot = NULL,
        confirmation = NULL,
        revision = 0L
      ),
      class = c("builder_workflow_state", "list")
    )
  )

  state <- builder_reduce_workflow(state, list(type = "datasets_ready"))
  expect_identical(state$stage, "configure")
  expect_error(
    builder_reduce_workflow(
      state,
      list(type = "confirm_review", snapshot = snapshot)
    ),
    "Review must be open"
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

  state <- builder_reduce_workflow(state, list(type = "back_to_review"))
  expect_identical(state$stage, "review")
  state <- builder_reduce_workflow(state, list(type = "back_to_settings"))
  expect_identical(state$stage, "configure")
  expect_true(builder_workflow_confirmation_matches(state, snapshot))

  state <- builder_reduce_workflow(state, list(type = "invalidate"))
  expect_identical(state$stage, "configure")
  expect_null(state$review_snapshot)
  expect_null(state$confirmation)
})

test_that("workflow transitions do not serialize the Review snapshot", {
  snapshot <- builder_workflow_test_snapshot()
  snapshot$items[[1L]]$summary <- raw(1024L * 1024L)
  runtime <- environment(builder_reduce_workflow)
  had_serialize <- exists("serialize", envir = runtime, inherits = FALSE)
  original <- if (had_serialize) get("serialize", envir = runtime) else NULL
  assign(
    "serialize",
    function(...) stop("workflow serialized Review state", call. = FALSE),
    envir = runtime
  )
  on.exit(
    {
      if (had_serialize) {
        assign("serialize", original, envir = runtime)
      } else if (exists("serialize", envir = runtime, inherits = FALSE)) {
        rm("serialize", envir = runtime)
      }
    },
    add = TRUE
  )

  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", snapshot = snapshot)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", snapshot = snapshot)
  )

  expect_identical(state$stage, "build")
})

test_that("available stages follow snapshot and confirmation state", {
  snapshot <- builder_workflow_test_snapshot()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "datasets_ready")
  )
  expect_identical(
    builder_workflow_stage_availability(state, TRUE),
    c(upload = TRUE, configure = TRUE, review = FALSE, build = FALSE)
  )

  state <- builder_reduce_workflow(
    state,
    list(type = "open_review", snapshot = snapshot)
  )
  expect_identical(
    builder_workflow_stage_availability(state, TRUE),
    c(upload = TRUE, configure = TRUE, review = TRUE, build = FALSE)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", snapshot = snapshot)
  )
  expect_true(all(builder_workflow_stage_availability(state, TRUE)))

  revisited <- builder_reduce_workflow(
    state,
    list(type = "navigate", stage = "upload")
  )
  expect_identical(revisited$review_snapshot, snapshot)
  expect_true(builder_workflow_confirmation_matches(revisited, snapshot))
})

test_that("confirmation rejects a changed Review snapshot", {
  snapshot <- builder_workflow_test_snapshot()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", snapshot = snapshot)
  )
  changed <- builder_workflow_test_snapshot(
    identity = builder_workflow_test_identity(
      "configuration-a:revision:2"
    )
  )

  expect_error(
    builder_reduce_workflow(
      state,
      list(type = "confirm_review", snapshot = changed)
    ),
    "configuration changed"
  )
})

test_that("opening Review preserves only a matching confirmation", {
  snapshot <- builder_workflow_test_snapshot()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", snapshot = snapshot)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", snapshot = snapshot)
  )

  matching <- builder_reduce_workflow(
    state,
    list(type = "open_review", snapshot = snapshot)
  )
  expect_true(builder_workflow_confirmation_matches(matching, snapshot))

  same_identity_new_revision <- snapshot
  same_identity_new_revision$revision <- "2"
  invalidated_revision <- builder_reduce_workflow(
    state,
    list(type = "open_review", snapshot = same_identity_new_revision)
  )
  expect_null(invalidated_revision$confirmation)

  changed <- builder_workflow_test_snapshot(
    identity = builder_workflow_test_identity(
      "configuration-a:revision:2"
    )
  )
  invalidated <- builder_reduce_workflow(
    state,
    list(type = "open_review", snapshot = changed)
  )
  expect_null(invalidated$confirmation)
})

test_that("workflow reset events clear Review state", {
  snapshot <- builder_workflow_test_snapshot()
  state <- builder_reduce_workflow(
    builder_workflow_state(),
    list(type = "open_review", snapshot = snapshot)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", snapshot = snapshot)
  )

  for (event in list(
    list(type = "invalidate", stage = "upload"),
    list(type = "empty"),
    list(type = "datasets_ready")
  )) {
    reset <- builder_reduce_workflow(state, event)
    expect_null(reset$review_snapshot)
    expect_null(reset$confirmation)
    expect_true(.builder_workflow_state_valid(reset))
  }
})

test_that("workflow guards malformed state and events", {
  snapshot <- builder_workflow_test_snapshot()
  state <- builder_workflow_state()

  expect_false(builder_workflow_confirmation_matches(list(), snapshot))
  expect_false(builder_workflow_confirmation_matches(state, snapshot))
  expect_error(
    builder_reduce_workflow(list(), list(type = "empty")),
    "valid Builder workflow event"
  )
  expect_error(
    builder_reduce_workflow(
      state,
      list(type = "open_review", snapshot = list())
    ),
    "review snapshot"
  )
  expect_error(
    builder_reduce_workflow(state, list(type = "future_event")),
    "not supported"
  )
})

test_that("workflow validator rejects impossible Review and Build states", {
  state <- builder_workflow_state()
  impossible_review <- state
  impossible_review$stage <- "review"
  expect_false(.builder_workflow_state_valid(impossible_review))

  impossible_build <- state
  impossible_build$stage <- "build"
  expect_false(.builder_workflow_state_valid(impossible_build))

  snapshot <- builder_workflow_test_snapshot()
  malformed_build <- impossible_build
  malformed_build$review_snapshot <- snapshot
  malformed_build$confirmation <- list(
    identity = builder_workflow_test_identity("wrong"),
    snapshot_revision = snapshot$revision
  )
  expect_false(.builder_workflow_state_valid(malformed_build))
})

test_that("every reducer branch returns a valid workflow state", {
  snapshot <- builder_workflow_test_snapshot()
  state <- builder_workflow_state()
  state <- builder_reduce_workflow(state, list(type = "datasets_ready"))
  state <- builder_reduce_workflow(
    state,
    list(type = "open_review", snapshot = snapshot)
  )
  state <- builder_reduce_workflow(
    state,
    list(type = "confirm_review", snapshot = snapshot)
  )
  state <- builder_reduce_workflow(state, list(type = "back_to_review"))
  state <- builder_reduce_workflow(state, list(type = "back_to_settings"))
  state <- builder_reduce_workflow(state, list(type = "invalidate"))
  state <- builder_reduce_workflow(state, list(type = "empty"))

  expect_true(.builder_workflow_state_valid(state))
})

test_that("app loads Review snapshot support between state and workflow", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  app_state <- grep('source("state.R", local = TRUE)', app, fixed = TRUE)
  app_review <- grep('source("review.R", local = TRUE)', app, fixed = TRUE)
  app_workflow <- grep('source("workflow.R", local = TRUE)', app, fixed = TRUE)

  expect_identical(app_review, app_state + 1L)
  expect_identical(app_workflow, app_review + 1L)
  expect_true(all(c("review.R", "workflow.R") %in% builder_app_source_files))
})
