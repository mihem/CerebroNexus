# test-trajectory.R — Tests for trajectory module

shiny_root <- system.file("shiny/v1.4", package = "CerebroNexus")
# The full T+B demo now carries the monocle2 B-cell trajectory (the former
# standalone trajectory-only demo was consolidated into it).
trajectory_crb <- system.file(
  "extdata/v1.4/demo_full_tcr_bcr.crb",
  package = "CerebroNexus"
)

test_that("all trajectory module files parse without errors", {
  mod_files <- c(
    "UI.R",
    "server.R",
    "projection.R",
    "projection_plot.R",
    "projection_export.R",
    "distribution_along_pseudotime.R",
    "expression_metrics.R",
    "number_of_expressed_genes_by_state.R",
    "number_of_transcripts_by_state.R",
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
  crb <- readRDS(trajectory_crb)
  methods <- crb$getMethodsForTrajectories()
  expect_true(is.character(methods))
  expect_true(length(methods) > 0)
  expect_true("monocle2" %in% methods)
})

test_that("full T+B demo trajectory data is accessible and complete", {
  skip_if_not(file.exists(trajectory_crb))
  crb <- readRDS(trajectory_crb)
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
  crb <- readRDS(trajectory_crb)
  expect_true(is.function(crb$getMethodsForTrajectories))
  expect_true(is.function(crb$getNamesOfTrajectories))
  expect_true(is.function(crb$getTrajectory))
})

test_that("getTrajectory bug is fixed in class definition", {
  cls <- Cerebro_v1.3
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
  expect_match(ui_src, "sidebar_item_trajectory_placeholder")

  server_src <- paste(
    readLines(file.path(shiny_root, "shiny_server.R")),
    collapse = "\n"
  )
  expect_match(server_src, "trajectory/server\\.R")
  expect_match(
    server_src,
    'insertConditionalTab\\([\\s\\S]{0,80}"trajectory"',
    perl = TRUE
  )
})
