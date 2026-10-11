viewer_design_source <- function(...) {
  paste(readLines(viewer_test_path(...), warn = FALSE), collapse = "\n")
}

test_that("mobile settings drawer behaves as a modal", {
  source <- viewer_design_source("www", "settings_drawer.js")

  expect_match(source, "event.key === 'Tab'", fixed = TRUE)
  expect_match(source, "aria-modal", fixed = TRUE)
  expect_match(source, "inert", fixed = TRUE)
})

test_that("data controls and conversion table remain usable on narrow screens", {
  preferences <- viewer_design_source("load_data", "preferences.R")
  conversion <- viewer_design_source("gene_id_conversion", "server.R")
  utility <- viewer_design_source("utility_functions.R")

  expect_no_match(preferences, "prettySwitch", fixed = TRUE)
  expect_match(
    preferences,
    'checkboxInput\\([\\s\\S]*?"webgl_checkbox"',
    perl = TRUE
  )
  expect_match(
    preferences,
    'checkboxInput\\([\\s\\S]*?"hover_info_in_projections_checkbox"',
    perl = TRUE
  )
  expect_match(conversion, "scrollX = TRUE", fixed = TRUE)
  expect_no_match(conversion, "autoHideNavigation", fixed = TRUE)
  expect_no_match(utility, "autoHideNavigation", fixed = TRUE)
})

test_that("colour management renders one information action", {
  source <- paste(
    viewer_design_source("color_management", "UI.R"),
    viewer_design_source("color_management", "server.R")
  )

  expect_match(
    source,
    'cerebroVizPageHeader\\([\\s\\S]*?"color_assignments_info"',
    perl = TRUE
  )
  expect_no_match(
    source,
    'cerebroInfoButton("color_assignments_info")',
    fixed = TRUE
  )
})

test_that("viewer shell provides landmark, skip link, and page headings", {
  ui <- viewer_design_source("shiny_UI.R")
  data_ui <- viewer_design_source("load_data", "UI.R")

  expect_match(ui, 'class = "cerebro-skip-link"', fixed = TRUE)
  expect_match(ui, 'href = "#main-content"', fixed = TRUE)
  expect_match(ui, 'tags$main(', fixed = TRUE)
  expect_match(ui, 'id = "main-content"', fixed = TRUE)
  expect_match(ui, "tags$h1(title)", fixed = TRUE)
  expect_match(data_ui, 'tags$h1("Data info")', fixed = TRUE)
})

test_that("legacy result selectors use real visible labels", {
  sources <- list(
    marker = viewer_design_source("marker_genes", "select_content.R"),
    groups = viewer_design_source("groups", "select_group.R"),
    most = viewer_design_source("most_expressed_genes", "select_group.R"),
    pathways = viewer_design_source("enriched_pathways", "select_content.R")
  )

  for (name in names(sources)) {
    expect_no_match(sources[[name]], "label = NULL", fixed = TRUE, info = name)
    expect_no_match(sources[[name]], "</h2>", fixed = TRUE, info = name)
  }
  expect_match(sources$marker, 'label = "Method"', fixed = TRUE)
  expect_match(sources$marker, 'label = "Grouping variable"', fixed = TRUE)
  expect_match(sources$groups, 'label = "Grouping variable"', fixed = TRUE)
  expect_match(sources$most, 'label = "Grouping variable"', fixed = TRUE)
  expect_match(sources$pathways, 'label = "Method"', fixed = TRUE)
  expect_match(sources$pathways, 'label = "Grouping variable"', fixed = TRUE)

  server <- viewer_design_source("shiny_server.R")
  expect_match(server, 'label = "Sample data set"', fixed = TRUE)
})

test_that("canvas views expose a text alternative and hide minimaps", {
  ui <- viewer_design_source("coordinated_views", "UI.R")
  canvas <- viewer_design_source("www", "cell_views.js")

  expect_match(ui, 'role = "img"', fixed = TRUE)
  expect_match(ui, '`aria-describedby`', fixed = TRUE)
  expect_match(ui, 'class = "cv-mini"', fixed = TRUE)
  expect_match(ui, '`aria-hidden` = "true"', fixed = TRUE)
  expect_match(canvas, "setAttribute('role', 'img')", fixed = TRUE)
  expect_match(canvas, "setAttribute('aria-describedby'", fixed = TRUE)
})

test_that("shared column filters are named from their headers", {
  utility <- viewer_design_source("utility_functions.R")

  expect_match(utility, "initComplete = DT::JS", fixed = TRUE)
  expect_match(utility, "aria-label", fixed = TRUE)
  expect_match(utility, "Filter ", fixed = TRUE)
})

test_that("async output updates are announced", {
  ui <- viewer_design_source("shiny_UI.R")
  shell <- viewer_design_source("www", "viewer-shell.js")
  canvas <- viewer_design_source("www", "cell_views.js")
  css <- viewer_design_source("www", "custom.css")

  expect_match(ui, 'role = "status"', fixed = TRUE)
  expect_match(ui, '`aria-live` = "polite"', fixed = TRUE)
  expect_match(ui, 'id = "cerebro-page-loader"', fixed = TRUE)
  expect_match(css, ".cerebro-page-loader", fixed = TRUE)
  expect_match(shell, "aria-busy", fixed = TRUE)
  expect_match(shell, "Updating content", fixed = TRUE)
  expect_match(shell, "Content updated", fixed = TRUE)
  expect_match(shell, "pruneBusyOutputs", fixed = TRUE)
  expect_match(shell, "shiny:idle.cerebroPageLoader", fixed = TRUE)
  expect_match(shell, "shiny:busy.cerebroPageLoader", fixed = TRUE)
  expect_match(shell, "scheduleFinish", fixed = TRUE)
  expect_match(shell, "MutationObserver", fixed = TRUE)
  expect_match(shell, "requestAnimationFrame", fixed = TRUE)
  expect_match(shell, "cerebro:cell-view-ready", fixed = TRUE)
  expect_match(shell, "cerebro:linkedviews-ready", fixed = TRUE)
  expect_match(shell, "event.detail.painted", fixed = TRUE)
  expect_match(canvas, "cerebro:cell-view-ready", fixed = TRUE)
  expect_match(canvas, "linkedWorkspacePainted", fixed = TRUE)
  expect_match(canvas, "painted: true", fixed = TRUE)
})

test_that("inactivity warning gives users a way to continue", {
  ui <- viewer_design_source("shiny_UI.R")

  expect_no_match(ui, "window.onmousemove", fixed = TRUE)
  expect_no_match(ui, "window.onkeypress", fixed = TRUE)
  expect_match(ui, "addEventListener", fixed = TRUE)
  expect_match(ui, "Continue session", fixed = TRUE)
  expect_match(ui, "session-timeout-warning", fixed = TRUE)
  expect_match(ui, "tags$script(HTML(inactivity))", fixed = TRUE)
})

test_that("visual tokens remain readable and responsive", {
  css <- viewer_design_source("www", "custom.css")
  coordviews <- viewer_design_source("www", "coordviews.css")

  expect_match(css, "--c-text-3:       #6f6f75", fixed = TRUE)
  expect_no_match(
    css,
    "\\.cerebro-config-open \\{[\\s\\S]*?color: #ffffff;",
    perl = TRUE
  )
  expect_no_match(
    coordviews,
    "background: var\\(--c-amber\\);[[:space:]]*color: #ffffff;",
    perl = TRUE
  )
  expect_no_match(css, ".cerebro-advanced {\n  opacity:", perl = TRUE)
  expect_match(css, ".cerebro-viz-page-meta", fixed = TRUE)
  expect_match(css, "white-space: normal", fixed = TRUE)
  expect_match(css, "@media (pointer: coarse)", fixed = TRUE)
  expect_match(css, "scroll-behavior: auto", fixed = TRUE)
  expect_match(css, ".skin-blue .sidebar-menu > li > a > .fa", fixed = TRUE)
  expect_match(css, ".small-box > .inner", fixed = TRUE)
  expect_match(coordviews, "@media (pointer: coarse)", fixed = TRUE)
})

test_that("network motion follows the reduced-motion preference", {
  source <- viewer_design_source("www", "hla_motifs.js")

  expect_match(source, "prefers-reduced-motion: reduce", fixed = TRUE)
})

test_that("empty and welcome states explain the next step", {
  empty_sources <- c(
    viewer_design_source("marker_genes", "select_content.R"),
    viewer_design_source("marker_genes", "table.R"),
    viewer_design_source("most_expressed_genes", "table.R"),
    viewer_design_source("enriched_pathways", "select_content.R"),
    viewer_design_source("enriched_pathways", "table.R")
  )
  app <- paste(
    readLines(file.path(viewer_app_test_path(), "app.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(any(grepl("No data available.", empty_sources, fixed = TRUE)))
  expect_false(any(grepl("Data not available", empty_sources, fixed = TRUE)))
  expect_true(any(grepl("Choose", empty_sources, fixed = TRUE)))
  expect_no_match(app, "custom welcome message", fixed = TRUE)
  expect_match(app, "single-cell and spatial", fixed = TRUE)
})
