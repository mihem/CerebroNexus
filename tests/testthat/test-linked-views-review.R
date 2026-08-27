inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
inst_dir <- inst_candidates[file.exists(file.path(inst_candidates, "app.R"))][1]
if (is.na(inst_dir)) {
  inst_dir <- system.file(package = "CerebroNexus")
}

viewer_dir <- file.path(inst_dir, "viewer")
linked_dir <- file.path(viewer_dir, "coordinated_views")

test_that("Linked views is additive to the existing specialist pages", {
  expect_true(file.exists(file.path(linked_dir, "UI.R")))
  expect_true(file.exists(file.path(linked_dir, "bundle.R")))
  expect_true(file.exists(file.path(linked_dir, "server.R")))

  expect_true(file.exists(file.path(viewer_dir, "overview", "UI.R")))
  expect_true(file.exists(file.path(viewer_dir, "spatial", "UI.R")))
  expect_true(file.exists(file.path(viewer_dir, "trekker", "UI.R")))

  ui <- paste(
    readLines(file.path(viewer_dir, "shiny_UI.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ui, '"Projection"', fixed = TRUE)
  expect_match(ui, '"Linked views"', fixed = TRUE)
  expect_match(ui, 'tab_overview', fixed = TRUE)
  expect_match(ui, 'tab_spatial', fixed = TRUE)
  expect_match(ui, 'tab_trekker', fixed = TRUE)
  expect_match(ui, 'tab_coordinated_views', fixed = TRUE)
})

test_that("Linked views chooses a useful categorical default", {
  clone_file <- file.path(viewer_dir, "clone_contract.R")
  bundle_file <- file.path(linked_dir, "bundle.R")
  expect_true(file.exists(clone_file))
  expect_true(file.exists(bundle_file))

  env <- new.env(parent = globalenv())
  sys.source(clone_file, envir = env)
  sys.source(bundle_file, envir = env)

  choose_default <- env$cv_default_group
  expect_true(is.function(choose_default))
  expect_identical(
    choose_default(c("donor", "Cell Tyep", "Sample")),
    "Cell Tyep"
  )
  expect_identical(
    choose_default(c("donor", "cell-type", "sample")),
    "cell-type"
  )
  expect_identical(
    choose_default(c("donor", "SAMPLE")),
    "SAMPLE"
  )

  set.seed(42)
  fallback <- choose_default(c("donor", "batch", "condition"))
  expect_true(fallback %in% c("donor", "batch", "condition"))
  expect_null(choose_default(character()))
})

test_that("Linked views keeps alpha and offers fluid or square plots", {
  ui <- paste(
    readLines(file.path(linked_dir, "UI.R"), warn = FALSE),
    collapse = "\n"
  )
  js <- paste(
    readLines(file.path(viewer_dir, "www", "coordviews.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(ui, '"Point opacity"', fixed = TRUE)
  expect_match(ui, '"cv-opacity"', fixed = TRUE)
  expect_match(ui, 'id = "cv-square-plots"', fixed = TRUE)
  expect_match(ui, '"Keep plots square"', fixed = TRUE)
  expect_no_match(ui, 'id = "cv-square-plots", checked', fixed = TRUE)
  expect_match(js, "function resizePanel(p, width, height)", fixed = TRUE)
  expect_match(js, "var keepPlotsSquare = false", fixed = TRUE)
  expect_match(js, "keepPlotsSquare ?", fixed = TRUE)
  expect_false(grepl("function resizePanelSquare", js, fixed = TRUE))
})

test_that("desktop sidebar starts open and can yield its full width", {
  css <- paste(
    readLines(file.path(viewer_dir, "www", "custom.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    css,
    "html body.skin-blue .main-header .navbar .sidebar-toggle",
    fixed = TRUE
  )
  expect_match(css, "sidebar-collapse .content-wrapper", fixed = TRUE)
  expect_match(css, "margin-left: 0 !important", fixed = TRUE)
  expect_match(css, "translateX(-230px)", fixed = TRUE)
  expect_match(css, "transition: left .3s ease-in-out", fixed = TRUE)
  expect_match(css, "width: 20px", fixed = TRUE)
  expect_match(css, "height: 42px", fixed = TRUE)
  expect_match(css, "content: \"\"", fixed = TRUE)
  expect_match(css, "border-left: 2px solid currentColor", fixed = TRUE)
  expect_match(css, "3px 0 7px -2px", fixed = TRUE)
  expect_match(css, "5px 0 11px -2px", fixed = TRUE)

  toggle_blocks <- regmatches(
    css,
    gregexpr(
      paste0(
        "html body.skin-blue \\.main-header \\.navbar ",
        "\\.sidebar-toggle \\{[^}]+\\}"
      ),
      css
    )
  )[[1]]
  expect_equal(
    sum(grepl("background: var(--c-surface-3)", toggle_blocks, fixed = TRUE)),
    2
  )
  expect_equal(
    sum(grepl("color: var(--c-text-2)", toggle_blocks, fixed = TRUE)),
    2
  )
  expect_match(
    css,
    paste0(
      "sidebar-toggle:hover {\n",
      "  background: var(--c-amber-100);\n",
      "  color: var(--c-amber-700);"
    ),
    fixed = TRUE
  )
  expect_no_match(css, "sidebar-toggle:hover,\n", fixed = TRUE)
})

test_that("specialist pages use the Linked views control hierarchy", {
  layout_files <- c(
    overview = file.path(viewer_dir, "overview", "UI_projection.R"),
    trajectory = file.path(viewer_dir, "trajectory", "projection.R"),
    immune = file.path(viewer_dir, "immune_repertoire", "UI.R"),
    hla = file.path(viewer_dir, "hla_tcr_motifs", "UI.R")
  )

  for (path in layout_files) {
    ui <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(ui, 'class = "cerebro-viz-toolbar"', fixed = TRUE, info = path)
    expect_match(ui, "cerebroSettingsButton(", fixed = TRUE, info = path)
    expect_match(ui, "cerebroSettingsDrawer(", fixed = TRUE, info = path)
    expect_no_match(ui, 'title = "Main parameters"', fixed = TRUE, info = path)
    expect_no_match(
      ui,
      'title = "Additional parameters"',
      fixed = TRUE,
      info = path
    )
    expect_no_match(ui, 'title = "Group filters"', fixed = TRUE, info = path)
  }

  overview <- paste(
    readLines(layout_files[["overview"]], warn = FALSE),
    collapse = "\n"
  )
  expect_no_match(overview, "shinyWidgets::dropdownButton(", fixed = TRUE)
  expect_match(overview, '"Appearance"', fixed = TRUE)
  expect_match(overview, '"Data"', fixed = TRUE)
  expect_match(overview, '"Group filters"', fixed = TRUE)

  trajectory <- paste(
    readLines(layout_files[["trajectory"]], warn = FALSE),
    collapse = "\n"
  )
  expect_match(trajectory, '"Appearance"', fixed = TRUE)
  expect_match(trajectory, '"Data"', fixed = TRUE)
  expect_match(trajectory, '"Group filters"', fixed = TRUE)

  immune <- paste(
    readLines(layout_files[["immune"]], warn = FALSE),
    collapse = "\n"
  )
  expect_match(immune, '"Analysis"', fixed = TRUE)
  expect_match(immune, '"Appearance"', fixed = TRUE)
  expect_match(immune, '"Group filters"', fixed = TRUE)

  hla <- paste(
    readLines(layout_files[["hla"]], warn = FALSE),
    collapse = "\n"
  )
  expect_match(hla, 'uiOutput("hla_parameters_ui")', fixed = TRUE)
  expect_match(hla, 'uiOutput("hla_more_parameters_ui")', fixed = TRUE)
  expect_match(hla, '"Analysis"', fixed = TRUE)
  expect_match(hla, '"Appearance"', fixed = TRUE)
  expect_match(hla, '"Evidence status"', fixed = TRUE)

  ui_helpers <- paste(
    readLines(file.path(viewer_dir, "shiny_UI.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ui_helpers, "cerebroSettingsButton <- function", fixed = TRUE)
  expect_match(ui_helpers, "cerebroVizPageHeader <- function", fixed = TRUE)
  expect_match(ui_helpers, "cerebroSettingsDrawer <- function", fixed = TRUE)
  expect_match(ui_helpers, 'cerebro_js("settings_drawer.js"', fixed = TRUE)

  css <- paste(
    readLines(file.path(viewer_dir, "www", "custom.css"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(css, ".cerebro-viz-toolbar {", fixed = TRUE)
  expect_match(css, ".cerebro-viz-page-heading {", fixed = TRUE)
  expect_match(css, ".cerebro-more-btn {", fixed = TRUE)
  expect_match(css, ".cerebro-settings-drawer {", fixed = TRUE)
  expect_match(css, "position: fixed", fixed = TRUE)
  expect_match(css, "height: 42px", fixed = TRUE)

  expect_true(file.exists(file.path(viewer_dir, "www", "settings_drawer.js")))
})

test_that("specialist page headings and compact toolbars match Linked views", {
  expected <- list(
    overview = c("Projection", "overview_projection_main_parameters_info"),
    trajectory = c("Trajectory", "trajectory_projection_main_parameters_info"),
    immune = c("Immune repertoire", "ir_main_parameters_info"),
    hla = c("HLA & TCR Motifs", "hla_parameters_info")
  )
  paths <- c(
    overview = file.path(viewer_dir, "overview", "UI_projection.R"),
    trajectory = file.path(viewer_dir, "trajectory", "projection.R"),
    immune = file.path(viewer_dir, "immune_repertoire", "UI.R"),
    hla = file.path(viewer_dir, "hla_tcr_motifs", "UI.R")
  )

  for (name in names(paths)) {
    ui <- paste(readLines(paths[[name]], warn = FALSE), collapse = "\n")
    expect_match(ui, "cerebroVizPageHeader(", fixed = TRUE, info = name)
    expect_match(ui, expected[[name]][[1]], fixed = TRUE, info = name)
    expect_match(ui, expected[[name]][[2]], fixed = TRUE, info = name)
  }

  css <- paste(
    readLines(file.path(viewer_dir, "www", "custom.css"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(css, "height: 38px", fixed = TRUE)
  expect_match(css, "max-width: 240px", fixed = TRUE)
  expect_match(css, "margin-left: auto", fixed = TRUE)
  expect_match(css, ".cerebro-viz-primary .ir-flow-controls", fixed = TRUE)

  ir_settings <- paste(
    readLines(
      file.path(viewer_dir, "immune_repertoire", "settings.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(ir_settings, 'class = "ir-flow-controls"', fixed = TRUE)
  expect_match(ir_settings, 'class = "ir-flow-control-item"', fixed = TRUE)
})

test_that("specialist drawer controls match the Linked views control style", {
  projection_ui <- paste(
    readLines(
      file.path(
        viewer_dir,
        "overview",
        "UI_projection_additional_parameters.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  group_label_ui <- paste(
    readLines(
      file.path(viewer_dir, "overview", "UI_projection_show_group_label.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  border_ui <- paste(
    readLines(
      file.path(viewer_dir, "overview", "UI_projection_point_border.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  css <- paste(
    readLines(file.path(viewer_dir, "www", "custom.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    projection_ui,
    'inputId = "overview_projection_keep_square"',
    fixed = TRUE
  )
  expect_match(projection_ui, '"Keep plot square"', fixed = TRUE)
  expect_match(projection_ui, 'class = "cerebro-square-option"', fixed = TRUE)
  expect_match(group_label_ui, "checkboxInput(", fixed = TRUE)
  expect_match(border_ui, "checkboxInput(", fixed = TRUE)
  expect_no_match(group_label_ui, "awesomeCheckbox(", fixed = TRUE)
  expect_no_match(border_ui, "awesomeCheckbox(", fixed = TRUE)

  expect_match(
    css,
    ".cerebro-settings-drawer .bootstrap-select.form-control",
    fixed = TRUE
  )
  expect_match(
    css,
    ".cerebro-settings-drawer .bootstrap-select > .dropdown-toggle",
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      "body .form-control:not(.colourpicker-input)",
      ":not(.shiny-colour-input):not(.bootstrap-select):not(.selectpicker)"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    ".cerebro-settings-drawer .checkbox input[type=checkbox]",
    fixed = TRUE
  )
  expect_match(css, ".cerebro-square-option", fixed = TRUE)
})

test_that("IR keeps low-frequency analysis controls in More settings", {
  spec <- paste(
    readLines(
      file.path(viewer_dir, "immune_repertoire", "param_spec.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  settings <- paste(
    readLines(
      file.path(viewer_dir, "immune_repertoire", "settings.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(spec, "IR_MORE_PARAM_IDS <- c(", fixed = TRUE)
  expect_match(spec, '"ir_p_n_boots"', fixed = TRUE)
  expect_match(spec, '"ir_p_rare_n_boots"', fixed = TRUE)
  expect_match(spec, '"ir_p_order_by"', fixed = TRUE)
  expect_match(spec, '"ir_p_umap_show_all"', fixed = TRUE)
  expect_match(
    settings,
    'output$ir_more_analysis_UI <- renderUI({',
    fixed = TRUE
  )
  expect_match(settings, 'uiOutput("ir_primary_param_panel")', fixed = TRUE)
  expect_match(
    settings,
    'output$ir_primary_param_panel <- renderUI({',
    fixed = TRUE
  )
})
