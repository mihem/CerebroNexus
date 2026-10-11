# test-trajectory.R — Tests for trajectory module

shiny_root <- system.file("viewer", package = "CerebroNexus")
# The full T+B demo now carries the monocle2 B-cell trajectory (the former
# standalone trajectory-only demo was consolidated into it).
trajectory_crb <- system.file(
  "extdata/examples/demo_full_tcr_bcr.crb",
  package = "CerebroNexus"
)

test_that("all trajectory module files parse without errors", {
  mod_files <- c(
    "UI.R",
    "server.R",
    "projection.R",
    "projection_plot.R",
    "distribution_along_pseudotime.R",
    "expression_metrics.R",
    "select_method_and_name.R",
    "selected_cells_table.R",
    "states_by_group.R"
  )
  for (f in mod_files) {
    fpath <- file.path(shiny_root, "trajectory", f)
    skip_if_not(file.exists(fpath), message = paste("Missing:", f))
    expect_no_error(parse(file = fpath))
  }
})

test_that("trajectory UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "trajectory", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"trajectory"', perl = TRUE)
})

test_that("full T+B demo trajectory class methods work", {
  skip_if_not(file.exists(trajectory_crb))
  crb <- readCerebro(trajectory_crb)
  methods <- crb$getMethodsForTrajectories()
  expect_true(is.character(methods))
  expect_true(length(methods) > 0)
  expect_true("monocle2" %in% methods)
})

test_that("full T+B demo trajectory data is accessible and complete", {
  skip_if_not(file.exists(trajectory_crb))
  crb <- readCerebro(trajectory_crb)
  methods <- crb$getMethodsForTrajectories()
  skip_if(length(methods) == 0)
  names <- crb$getNamesOfTrajectories(methods[1])
  expect_true(is.character(names))
  expect_true(length(names) > 0)
  traj <- crb$getTrajectory(methods[1], names[1])
  expect_true(is.list(traj))
  expect_true(all(c("meta", "edges") %in% names(traj)))
  expect_true(is.data.frame(traj$meta))
  expect_true(is.data.frame(traj$edges))
  expect_true(nrow(traj$meta) > 0)
  expect_true("pseudotime" %in% colnames(traj$meta))
  expect_true("state" %in% colnames(traj$meta))
  expect_true("B_cell_maturation" %in% crb$getNamesOfTrajectories("monocle2"))
})

test_that("utility wrappers for trajectory exist", {
  crb <- readCerebro(trajectory_crb)
  expect_true(is.function(crb$getMethodsForTrajectories))
  expect_true(is.function(crb$getNamesOfTrajectories))
  expect_true(is.function(crb$getTrajectory))
})

test_that("getTrajectory bug is fixed in class definition", {
  cls <- Cerebro
  methods_text <- paste(
    deparse(cls$public_methods$getTrajectory),
    collapse = "\n"
  )
  expect_match(methods_text, "getNamesOfTrajectories", fixed = TRUE)
})

test_that("trajectory helper utilities are defined in the app scope", {
  # The Trajectory tab calls these free functions (mito/ribo/ery metric sub-tabs
  # and the pseudotime comparison-variable selector). They were missing from dev
  # and only surfaced when the tab was actually mounted, so guard their presence
  # in utility_functions.R. Cross-line-tolerant per project convention.
  util_src <- paste(
    readLines(file.path(shiny_root, "utility_functions.R")),
    collapse = "\n"
  )
  for (fn in c(
    "getVariableToCompareChoices",
    "getMitoColumn",
    "hasMitoColumn",
    "getRiboColumn",
    "hasRiboColumn",
    "getEryColumn",
    "hasEryColumn"
  )) {
    expect_match(
      util_src,
      paste0(fn, "[\\s]{0,3}<-[\\s]{0,3}function"),
      perl = TRUE,
      info = fn
    )
  }
})

test_that("trajectory debounce captures the raw reactive", {
  source <- paste(
    readLines(file.path(shiny_root, "trajectory", "projection_plot.R")),
    collapse = "\n"
  )

  expect_match(
    source,
    "trajectory_projection_prepared_raw <- reactive({",
    fixed = TRUE
  )
  expect_match(
    source,
    paste(
      "trajectory_projection_prepared <- debounceAfterFirst(",
      "trajectory_projection_prepared_raw,",
      sep = "\n  "
    ),
    fixed = TRUE
  )
})

test_that("trajectory first frame has a static host and stable appearance", {
  ui <- paste(
    readLines(file.path(shiny_root, "trajectory", "UI.R"), warn = FALSE),
    collapse = "\n"
  )
  controls <- paste(
    readLines(
      file.path(shiny_root, "trajectory", "select_method_and_name.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  projection <- paste(
    readLines(
      file.path(shiny_root, "trajectory", "projection_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  settings <- paste(
    readLines(
      file.path(shiny_root, "trajectory", "projection.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    ui,
    'cerebroCellViewOutput("trajectory_projection")',
    fixed = TRUE
  )
  expect_false(grepl('uiOutput("trajectory_projection_UI")', ui, fixed = TRUE))
  expect_match(
    controls,
    'output[["trajectory_primary_controls_UI"]]',
    fixed = TRUE
  )
  expect_match(controls, '"trajectory_selected_method"', fixed = TRUE)
  expect_match(controls, '"trajectory_selected_name"', fixed = TRUE)
  expect_match(controls, '"trajectory_point_color"', fixed = TRUE)
  expect_false(grepl(
    'input[["trajectory_projection_group_labels"]]',
    projection,
    fixed = TRUE
  ))
  expect_match(
    projection,
    "group_labels = isolate(trajectory_projection_appearance$group_labels)",
    fixed = TRUE
  )
  expect_match(settings, '"cell_view_appearance"', fixed = TRUE)
})

test_that("trajectory static first frame is narrow and keeps deferred identity", {
  projection <- paste(
    readLines(
      file.path(shiny_root, "trajectory", "projection_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    projection,
    "trajectory_static_first_frame <- reactive",
    fixed = TRUE
  )
  expect_match(
    projection,
    "viewerTrajectoryFrameAsset(method, name)",
    fixed = TRUE
  )
  expect_match(projection, "viewerPackTrajectoryEdges(", fixed = TRUE)
  expect_match(
    projection,
    'identical(input[["trajectory_point_color"]], "state")',
    fixed = TRUE
  )
  expect_match(projection, "trajectory_frame_resource = resource", fixed = TRUE)
  expect_match(
    projection,
    "deferred_selection_lengths = resource$cells",
    fixed = TRUE
  )
  expect_match(projection, "trajectory_cells_reactive(", fixed = TRUE)
  expect_match(
    projection,
    "selection_rows = seq_len(resource$cells)",
    fixed = TRUE
  )
})

test_that("trajectory summaries require their own visibility gate", {
  gates <- c(
    distribution_along_pseudotime.R = "trajectory_distribution_section_visible",
    states_by_group.R = "trajectory_states_section_visible",
    expression_metrics.R = "trajectory_expression_section_visible"
  )
  for (path in names(gates)) {
    source <- paste(
      readLines(file.path(shiny_root, "trajectory", path), warn = FALSE),
      collapse = "\n"
    )
    expect_match(source, gates[[path]], fixed = TRUE, info = path)
    expect_match(
      source,
      "req(trajectory_projection_sent())",
      fixed = TRUE,
      info = path
    )
  }
})

test_that("specialist timing separates primary, cached, and auxiliary events", {
  source <- paste(
    readLines(file.path(shiny_root, "www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  state_source <- paste(
    readLines(
      file.path(shiny_root, "www", "cell_views_state.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(state_source, "S.specialistLifecycle = {", fixed = TRUE)
  expect_match(
    state_source,
    "begin: function (store, id, renderRequestSent)",
    fixed = TRUE
  )
  expect_match(
    state_source,
    "renderRequestSent: !!renderRequestSent",
    fixed = TRUE
  )
  expect_match(
    source,
    "CBViewState.specialistLifecycle.begin(",
    fixed = TRUE
  )
  expect_match(source, "eventKind: eventKind", fixed = TRUE)
  expect_match(source, "reportSelection('aux')", fixed = TRUE)
  expect_match(source, "'cached'", fixed = TRUE)
  expect_match(source, "__cerebroResetSpecialistBench", fixed = TRUE)
  expect_match(source, "recordSpecialistPayload('primary'", fixed = TRUE)
  expect_match(source, "recordSpecialistPayload('aux'", fixed = TRUE)
  expect_false(grepl(
    "p.canvas.addEventListener('pointerenter', requestSingleAux)",
    source,
    fixed = TRUE
  ))
})

test_that("Trajectory tab is wired into the app UI and server", {
  # Guard the integration points so a future refactor that drops the wiring
  # (as pr05 originally shipped it — module present but never mounted) fails
  # loudly. Cross-line-tolerant regex per project convention (air may reflow).
  ui_src <- paste(
    readLines(file.path(shiny_root, "shiny_UI.R")),
    collapse = "\n"
  )
  expect_match(ui_src, "trajectory/UI\\.R")
  expect_match(ui_src, "tab_trajectory")
  expect_match(ui_src, 'conditionalSidebarItem\\("Trajectory", "trajectory"')

  server_src <- paste(
    readLines(file.path(shiny_root, "shiny_server.R")),
    collapse = "\n"
  )
  expect_match(server_src, "trajectory/server\\.R")
  expect_match(
    server_src,
    'toggleConditionalTab\\([\\s\\S]{0,80}"trajectory"',
    perl = TRUE
  )
})
