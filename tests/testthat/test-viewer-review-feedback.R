viewer_source <- function(...) {
  viewer_root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(viewer_root)) {
    viewer_root <- testthat::test_path("../../inst/viewer")
  }
  paste(readLines(file.path(viewer_root, ...), warn = FALSE), collapse = "\n")
}

viewer_path <- function(...) {
  viewer_root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(viewer_root)) {
    viewer_root <- testthat::test_path("../../inst/viewer")
  }
  file.path(viewer_root, ...)
}

test_that("Viewer copy uses British colour spelling", {
  sidebar <- viewer_source("shiny_UI.R")
  management <- viewer_source("color_management", "server.R")
  tables <- list(
    marker_genes = viewer_source("marker_genes", "table.R"),
    linked = viewer_source("coordinated_views", "server.R"),
    trajectory = viewer_source("trajectory", "selected_cells_table.R"),
    spatial = viewer_source("spatial", "UI_selected_cells_table.R"),
    pathways = viewer_source("enriched_pathways", "table.R"),
    extra = viewer_source("extra_material", "content.R"),
    expression = viewer_source(
      "gene_expression",
      "UI_table_of_selected_cells.R"
    ),
    projection = viewer_source("overview", "UI_selected_cells_table.R")
  )
  expression <- viewer_source(
    "gene_expression",
    "UI_projection_gene_color_mode.R"
  )

  expect_match(sidebar, 'menuItem\\([[:space:]]*"Colour management"')
  expect_match(management, 'title = "Colours for groups"', fixed = TRUE)
  for (name in names(tables)) {
    expect_match(
      tables[[name]],
      'label = "Highlight values with colours:"',
      fixed = TRUE,
      info = name
    )
  }
  expect_match(expression, 'label = "Panel colours"', fixed = TRUE)
  expect_match(expression, '"Distinct colours" = "different"', fixed = TRUE)
})

test_that("Cell-view colouring controls share one label", {
  controls <- list(
    linked = viewer_source("coordinated_views", "UI.R"),
    projection = viewer_source("overview", "UI_projection_main_parameters.R"),
    spatial = viewer_source("spatial", "UI_projection_main_parameters.R"),
    trajectory = viewer_source("trajectory", "projection.R"),
    trekker = viewer_source("trekker", "server.R"),
    hla = viewer_source("hla_tcr_motifs", "settings.R")
  )

  for (name in names(controls)) {
    expect_match(controls[[name]], '"Colour by"', fixed = TRUE, info = name)
    expect_no_match(
      controls[[name]],
      "Colour (cells|nodes) by",
      info = name
    )
  }
  expect_match(controls$hla, '"HLA allele"', fixed = TRUE)
})

test_that("Informational Canvas text uses the readable secondary token", {
  coordviews <- viewer_source("www", "coordviews.css")
  trekker <- viewer_source("www", "trekker.css")

  for (selector in c(
    ".cv-read-sub",
    ".cv-empty, .coordviews-page .cv-empty-sm",
    ".cv-ctable th",
    ".cv-hint",
    ".cv-field-table-title span",
    ".cv-field-values span",
    ".cv-tk-insights-toggle small",
    ".cv-tk-cell-empty",
    ".cv-tk-cell-bc"
  )) {
    expect_match(
      coordviews,
      paste0(gsub("([.()])", "\\\\\\1", selector), "[^}]*var\\(--c-text-2\\)"),
      info = selector
    )
  }
  for (selector in c(".tk-empty", ".tk-stat .tk-k", ".tk-table th")) {
    expect_match(
      trekker,
      paste0(gsub("([.()])", "\\\\\\1", selector), "[^}]*var\\(--c-text-2\\)"),
      info = selector
    )
  }
})

test_that("Projection defaults to cell type when available", {
  ui <- viewer_source("overview", "UI_projection_main_parameters.R")

  expect_match(ui, '"cell_type" %in% color_choices', fixed = TRUE)
})

test_that("Cell-view More settings expose only effective appearance controls", {
  linked <- viewer_source("coordinated_views", "UI.R")
  overview <- paste(
    viewer_source("overview", "UI_projection.R"),
    viewer_source("overview", "UI_projection_show_group_label.R")
  )
  spatial <- paste(
    viewer_source("spatial", "UI_projection.R"),
    viewer_source("spatial", "UI_projection_show_group_label.R")
  )
  expression <- viewer_source("gene_expression", "UI_projection.R")
  trajectory <- viewer_source("trajectory", "projection.R")
  trajectory_export <- viewer_source("trajectory", "projection_export.R")
  repertoire <- viewer_source("immune_repertoire", "settings.R")
  repertoire_spec <- viewer_source("immune_repertoire", "param_spec.R")
  hla <- viewer_source("hla_tcr_motifs", "settings.R")

  for (id in c("cv-labels", "cv-borders", "cv-square-plots")) {
    expect_match(linked, paste0('"', id, '"'), fixed = TRUE)
  }
  for (id in c(
    "overview_projection_group_labels",
    "overview_projection_point_border",
    "overview_projection_keep_square"
  )) {
    expect_match(overview, id, fixed = TRUE)
  }
  for (id in c(
    "spatial_projection_group_labels",
    "spatial_projection_point_border",
    "spatial_projection_keep_square"
  )) {
    expect_match(spatial, id, fixed = TRUE)
  }
  for (id in c(
    "trajectory_projection_group_labels",
    "trajectory_projection_point_border",
    "trajectory_projection_keep_square"
  )) {
    expect_match(trajectory, id, fixed = TRUE)
  }
  expect_no_match(
    trajectory,
    "trajectory_projection_pdf_group_labels",
    fixed = TRUE
  )
  expect_no_match(
    trajectory_export,
    "trajectory_projection_pdf_group_labels",
    fixed = TRUE
  )
  expect_match(
    trajectory_export,
    'input[["trajectory_projection_group_labels"]]',
    fixed = TRUE
  )
  expect_match(
    trajectory_export,
    'categorical <- identical(color_variable, "state") ||',
    fixed = TRUE
  )
  expect_match(
    trajectory_export,
    'cells_df[[color_variable]] <- factor(cells_df[[color_variable]])',
    fixed = TRUE
  )
  for (id in c(
    "ir_clonalUMAP_group_labels",
    "ir_clonalUMAP_point_border",
    "ir_clonalUMAP_keep_square"
  )) {
    expect_match(repertoire, id, fixed = TRUE)
  }
  expect_no_match(repertoire_spec, 'id = "ir_d_base_size"', fixed = TRUE)
  expect_no_match(repertoire_spec, 'id = "ir_d_legend_size"', fixed = TRUE)
  for (id in c(
    "expression_projection_point_border",
    "expression_projection_keep_square"
  )) {
    expect_match(expression, id, fixed = TRUE)
  }
  expect_no_match(
    expression,
    "expression_projection_group_labels",
    fixed = TRUE
  )
  expect_no_match(hla, "keep_square", fixed = TRUE)
  expect_no_match(hla, "group_labels", fixed = TRUE)
  expect_no_match(hla, "point_border", fixed = TRUE)
})

test_that("Cell-view appearance uses the existing payload lifecycle", {
  utility <- viewer_source("utility_functions.R")
  javascript <- viewer_source("www", "cell_views.js")
  sources <- paste(
    utility,
    viewer_source("shiny_UI.R"),
    viewer_source("overview", "UI_projection.R"),
    viewer_source("spatial", "UI_projection.R"),
    viewer_source("gene_expression", "UI_projection.R"),
    viewer_source("trajectory", "projection.R"),
    viewer_source("immune_repertoire", "settings.R")
  )

  expect_no_match(sources, "cerebroCellViewSetting", fixed = TRUE)
  expect_match(utility, "appearance = list(", fixed = TRUE)
  expect_match(
    javascript,
    "var appearance = payload.meta.appearance;",
    fixed = TRUE
  )
  expect_match(javascript, "if (appearance) {", fixed = TRUE)
  expect_match(
    javascript,
    "if (bordersControl) bordersOn = !!bordersControl.checked;",
    fixed = TRUE
  )
  expect_match(javascript, "bordersOn", fixed = TRUE)
  expect_match(javascript, "appearance.keep_square", fixed = TRUE)
  expect_match(
    javascript,
    paste0(
      "if \\(dataChanged\\) \\{\\s+",
      "labelsOn = true;\\s+",
      "bordersOn = false;\\s+",
      "keepPlotsSquare = false;"
    )
  )
})

test_that("cell scatter pages share one dataset point appearance", {
  server <- viewer_source("shiny_server.R")
  pages <- list(
    viewer_source("overview", "UI_projection_additional_parameters.R"),
    viewer_source("spatial", "UI_projection_additional_parameters.R"),
    viewer_source("gene_expression", "UI_projection_additional_parameters.R"),
    viewer_source("trajectory", "projection.R")
  )

  expect_match(server, "current_scatter_defaults", fixed = TRUE)
  expect_match(server, "cell_point_size", fixed = TRUE)
  expect_match(server, "cell_point_opacity", fixed = TRUE)
  for (page in pages) {
    expect_match(page, "current_scatter_defaults()", fixed = TRUE)
    expect_match(page, 'preferences[["cell_point_size"]]', fixed = TRUE)
    expect_match(page, 'preferences[["cell_point_opacity"]]', fixed = TRUE)
  }
})

test_that("Projection pages use automatic ranges instead of axis sliders", {
  pages <- c("overview", "spatial", "gene_expression")
  for (page in pages) {
    ui <- viewer_source(page, "UI_projection.R")
    params <- viewer_source(page, "obj_projection_parameters_plot.R")
    expect_no_match(ui, '"Axes"', fixed = TRUE)
    expect_no_match(ui, "projection_scales_UI", fixed = TRUE)
    expect_no_match(params, "manual_range", fixed = TRUE)
    expect_false(file.exists(viewer_path(page, "UI_projection_scales.R")))
  }

  expect_match(
    viewer_source("overview", "obj_projection_parameters_plot.R"),
    "getXYranges(projection_data)",
    fixed = TRUE
  )
  expect_match(
    viewer_source("gene_expression", "obj_projection_parameters_plot.R"),
    "getXYranges(range_data)",
    fixed = TRUE
  )
  spatial_params <- viewer_source("spatial", "obj_projection_parameters_plot.R")
  expect_match(spatial_params, "x_range = NULL", fixed = TRUE)
  expect_match(spatial_params, "y_range = NULL", fixed = TRUE)
})

test_that("Standalone cell views cannot enter linked-view focus", {
  js <- viewer_source("www", "cell_views.js")

  expect_match(js, "function canFocusPanel", fixed = TRUE)
  expect_match(js, "return !singleActive;", fixed = TRUE)
  expect_match(js, "if (!canFocusPanel()) return;", fixed = TRUE)
})

test_that("Standalone cell-view toolbars reach the panel top-right", {
  css <- viewer_source("www", "coordviews.css")
  expect_match(
    css,
    ".coordviews-page.cerebro-cell-view-host .cv-pane-head",
    fixed = TRUE
  )

  shared_views <- c(
    "overview/UI_projection.R",
    "spatial/UI_projection.R",
    "gene_expression/UI_projection.R",
    "trajectory/projection.R",
    "immune_repertoire/visualizations.R",
    "trekker/UI.R"
  )
  for (view in shared_views) {
    expect_match(
      viewer_source(view),
      "cerebroCellViewOutput(",
      fixed = TRUE,
      info = view
    )
  }
})

test_that("Spatial geometry does not constrain fluid linked views", {
  js <- viewer_source("www", "cell_views.js")

  expect_match(js, "space.stretch || isSpatialSpace(space)", fixed = TRUE)
  expect_match(js, "function panelDataAspect", fixed = TRUE)
  expect_match(js, "Number(sp._unit.aspect)", fixed = TRUE)
  expect_match(js, "function fitAspectRow", fixed = TRUE)
  expect_match(js, "if (!aspects.every(Boolean)) return null;", fixed = TRUE)
  expect_no_match(js, "if (!aspects.some(Boolean)) return null;", fixed = TRUE)
  expect_match(js, "fitAspectRow(vis, usableW", fixed = TRUE)
  expect_match(js, "var aspect = panelDataAspect(p);", fixed = TRUE)
})

test_that("Trekker uses the shared top toolbar and settings drawer", {
  ui <- viewer_source("trekker", "UI.R")
  server <- viewer_source("trekker", "server.R")

  expect_match(
    ui,
    'class = "cerebro-viz-row cerebro-viz-top-layout"',
    fixed = TRUE
  )
  expect_match(ui, 'class = "cerebro-viz-toolbar"', fixed = TRUE)
  expect_match(ui, '"trekker_more_button"', fixed = TRUE)
  expect_match(ui, '"cv-more"', fixed = TRUE)
  expect_match(ui, 'cerebroCellViewOutput("trekker_projection")', fixed = TRUE)
  expect_no_match(ui, 'class = "cerebro-param-col"', fixed = TRUE)
  expect_match(server, 'output[["trekker_main_parameters_ui"]]', fixed = TRUE)
  expect_match(server, 'cerebroCellViewRender(', fixed = TRUE)
  expect_no_match(server, 'sendCustomMessage("trekker_data"', fixed = TRUE)
  expect_no_match(server, "shinyWidgets::pickerInput", fixed = TRUE)
})

test_that("Canvas panels separate view reset from selection clear", {
  ui <- viewer_source("coordinated_views", "UI.R")
  javascript <- viewer_source("www", "cell_views.js")

  expect_match(ui, 'class = "cv-tbtn cv-clear-btn"', fixed = TRUE)
  expect_match(ui, '`data-act` = "clear"', fixed = TRUE)
  expect_match(javascript, "function updateResetButtons", fixed = TRUE)
  expect_match(
    javascript,
    "button.disabled = !(p.view || p.rot);",
    fixed = TRUE
  )
  expect_match(javascript, "if (act === 'clear')", fixed = TRUE)
  expect_match(
    javascript,
    "if (gname.indexOf('__single_') === 0) return;",
    fixed = TRUE
  )
})

test_that("Trekker transition accepts Shiny slider change events", {
  javascript <- viewer_source("www", "cell_views.js")
  server <- viewer_source("trekker", "server.R")
  css <- viewer_source("www", "custom.css")

  expect_match(javascript, "function updateTrekkerTransition", fixed = TRUE)
  expect_match(
    javascript,
    "window.jQuery(document)",
    fixed = TRUE
  )
  expect_match(
    javascript,
    "input.cvTrekkerTransition change.cvTrekkerTransition",
    fixed = TRUE
  )
  expect_match(javascript, "'#trekker_morph'", fixed = TRUE)
  expect_match(javascript, ".off(", fixed = TRUE)
  expect_match(javascript, ".on(", fixed = TRUE)
  expect_no_match(
    javascript,
    "document.addEventListener('input', updateTrekkerTransition);",
    fixed = TRUE
  )
  expect_no_match(
    javascript,
    "document.addEventListener('change', updateTrekkerTransition);",
    fixed = TRUE
  )
  expect_match(javascript, "function transitionUnit", fixed = TRUE)
  expect_match(javascript, "fromUnit: unitOf", fixed = TRUE)
  expect_match(
    javascript,
    "space._unit = transitionUnit(source.fromUnit, source.toUnit, transition);",
    fixed = TRUE
  )
  expect_match(server, 'class = "trekker-transition-control"', fixed = TRUE)
  expect_match(server, "ticks = FALSE", fixed = TRUE)
  expect_match(
    css,
    ".trekker-transition-control .irs-single",
    fixed = TRUE
  )
  expect_no_match(css, "#trekker_morph + .irs--shiny", fixed = TRUE)
})

test_that("Settings drawer controls use one spacing system", {
  css <- viewer_source("www", "custom.css")

  expect_match(
    css,
    ".cerebro-settings-drawer .bootstrap-select.form-control {\n  height: auto !important;",
    fixed = TRUE
  )
  expect_match(
    css,
    "gap: 14px 22px;",
    fixed = TRUE
  )
  expect_match(
    css,
    ".cerebro-settings-content .form-group,\n.cerebro-settings-content .checkbox {\n  min-width: 0;\n  margin: 0;",
    fixed = TRUE
  )
  expect_no_match(
    css,
    ".cerebro-settings-drawer .form-group {\n  margin-bottom:",
    fixed = TRUE
  )
  expect_no_match(
    css,
    ".cerebro-settings-drawer .checkbox {\n  margin:",
    fixed = TRUE
  )
})

test_that("All settings drawers use the shared shell and responsive grid", {
  ui <- paste(
    readLines(viewer_path("shiny_UI.R"), warn = FALSE),
    collapse = "\n"
  )
  css <- viewer_source("www", "custom.css")
  linked_ui <- viewer_source("coordinated_views", "UI.R")
  linked_css <- viewer_source("www", "coordviews.css")
  linked_js <- viewer_source("www", "cell_views.js")
  ir_ui <- viewer_source("immune_repertoire", "UI.R")
  ir_settings <- viewer_source("immune_repertoire", "settings.R")
  hla_ui <- viewer_source("hla_tcr_motifs", "UI.R")
  drawer_js <- viewer_source("www", "settings_drawer.js")
  shell_js <- viewer_source("www", "viewer-shell.js")

  expect_match(ui, 'class = "cerebro-settings-content"', fixed = TRUE)
  expect_match(ui, 'class = "cerebro-settings-drawer"', fixed = TRUE)
  expect_no_match(
    ui,
    'class = paste(c("cerebro-settings-drawer"',
    fixed = TRUE
  )
  expect_match(css, "width: clamp(400px, 30vw, 480px);", fixed = TRUE)
  expect_match(
    css,
    "background: color-mix(in srgb, var(--c-surface) 90%, transparent);",
    fixed = TRUE
  )
  expect_match(
    css,
    "grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));",
    fixed = TRUE
  )
  expect_match(
    css,
    ".cerebro-viz-toolbar .shiny-panel-conditional:has(> .form-group)",
    fixed = TRUE
  )
  expect_match(css, ".cerebro-page-focus-target:focus", fixed = TRUE)
  expect_match(css, "outline: none !important;", fixed = TRUE)
  expect_match(
    shell_js,
    'target.classList.add("cerebro-page-focus-target")',
    fixed = TRUE
  )
  expect_match(
    css,
    ".cerebro-settings-content .shiny-input-container:has(.irs--shiny)",
    fixed = TRUE
  )
  expect_no_match(css, "max-width: 170px;", fixed = TRUE)
  expect_match(css, "grid-column: 1 / -1;", fixed = TRUE)
  expect_match(linked_ui, "cerebroSettingsDrawer(", fixed = TRUE)
  expect_match(linked_ui, '"Background image"', fixed = TRUE)
  expect_match(linked_ui, '"Appearance"', fixed = TRUE)
  expect_match(linked_ui, '"Data"', fixed = TRUE)
  expect_match(linked_ui, '"Spatial mapping"', fixed = TRUE)
  expect_match(linked_ui, '"Group filters"', fixed = TRUE)
  expect_match(
    linked_ui,
    'class = "coordviews-page"',
    fixed = TRUE
  )
  expect_no_match(linked_ui, 'class = "cv-more-clip"', fixed = TRUE)
  expect_no_match(linked_ui, 'class = "cv-more-inner"', fixed = TRUE)
  expect_no_match(linked_ui, 'class = "cv-more-section', fixed = TRUE)
  expect_no_match(linked_css, "#cv-more", fixed = TRUE)
  expect_no_match(linked_css, ".cv-more-titlebar", fixed = TRUE)
  expect_no_match(linked_css, ".cv-more-close", fixed = TRUE)
  for (id in c("cv-labels", "cv-borders", "cv-square-plots")) {
    expect_match(linked_ui, paste0('"', id, '"'), fixed = TRUE)
  }
  for (id in c("cv-ps", "cv-opacity", "cv-pct", "cv-dissolve", "cv-niche")) {
    expect_match(
      linked_ui,
      paste0('sliderInput\\([[:space:]]*"', id, '"')
    )
  }
  expect_match(linked_js, "document.addEventListener('input'", fixed = TRUE)
  expect_match(linked_js, "document.addEventListener('change'", fixed = TRUE)
  expect_match(linked_js, "updateLinkedToggle(target)", fixed = TRUE)
  expect_match(ir_ui, 'uiOutput("ir_appearance_section_UI")', fixed = TRUE)
  expect_match(ir_settings, '"Appearance"', fixed = TRUE)
  expect_match(ir_settings, "has_display && !has_cell_options", fixed = TRUE)
  hla_appearance_pos <- regexpr('"Appearance"', hla_ui, fixed = TRUE)[1]
  hla_analysis_pos <- regexpr('"Analysis"', hla_ui, fixed = TRUE)[1]
  expect_gt(hla_appearance_pos, 0)
  expect_gt(hla_analysis_pos, hla_appearance_pos)
  expect_match(
    linked_ui,
    paste0(
      'selectInput\\([[:space:]]*"cv-clip",',
      '[[:space:]]*label = "Colour range"'
    )
  )
  expect_no_match(linked_ui, "cv_range <-", fixed = TRUE)
  expect_no_match(linked_ui, "cv_range(", fixed = TRUE)
  expect_no_match(linked_ui, "cv-chk", fixed = TRUE)
  expect_no_match(linked_css, ".cv-chk", fixed = TRUE)
  expect_no_match(linked_css, ".cv-range", fixed = TRUE)
  expect_no_match(linked_css, "cerebro-settings", fixed = TRUE)
  expect_no_match(linked_js, "positionRangeVal", fixed = TRUE)
  expect_no_match(linked_js, "positionAllRangeVals", fixed = TRUE)
  expect_no_match(linked_js, "cv-ps-val", fixed = TRUE)
  expect_match(linked_js, "function setLinkedSliderValue", fixed = TRUE)
  expect_match(linked_js, ".data('ionRangeSlider')", fixed = TRUE)
  expect_match(drawer_js, "function restoreDrawer(drawer)", fixed = TRUE)
  expect_match(drawer_js, "drawer._cerebroHomeParent", fixed = TRUE)
  expect_match(
    drawer_js,
    "shiny:outputinvalidated.cerebroSettings shiny:value.cerebroSettings",
    fixed = TRUE
  )
  expect_match(
    linked_js,
    "ctl.closest('.cerebro-settings-section')",
    fixed = TRUE
  )
})

test_that("Info modals render above settings drawers", {
  css <- viewer_source("www", "custom.css")

  expect_match(css, "body .modal-backdrop {\n  z-index: 1700", fixed = TRUE)
  expect_match(css, "body .modal {\n  z-index: 1710", fixed = TRUE)
})

test_that("Settings drawer pickers have only one visible border", {
  css <- viewer_source("www", "custom.css")

  expect_match(
    css,
    paste0(
      ".cerebro-settings-drawer .form-control",
      ":not(.bootstrap-select):not(.selectpicker)"
    ),
    fixed = TRUE
  )
})

test_that("Specialist group filters share inclusive empty-selection behavior", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_path("utility_functions.R"), envir = env)
  metadata <- data.frame(
    sample = c("A", "A", "B"),
    cell_type = c("T", "B", "T"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    env$cerebroGroupFilterMask(
      metadata,
      list(sample = "A", cell_type = "T")
    ),
    c(TRUE, FALSE, FALSE)
  )
  expect_equal(
    env$cerebroGroupFilterMask(
      metadata,
      list(sample = character(), cell_type = c("T", "B"))
    ),
    rep(FALSE, 3)
  )

  for (path in list(
    c("overview", "obj_projection_cells_to_show.R"),
    c("spatial", "obj_projection_cells_to_show.R"),
    c("gene_expression", "obj_projection_cells_to_show.R")
  )) {
    expect_match(
      do.call(viewer_source, as.list(path)),
      "cerebroGroupFilterMask",
      fixed = TRUE
    )
  }
})

test_that("Cell scatter pages share one percentage default", {
  server <- viewer_source("shiny_server.R")
  expect_match(server, "cell_percentage_cells_to_show", fixed = TRUE)
  expect_no_match(
    server,
    "gene_expression_plot_percentage_cells_to_show",
    fixed = TRUE
  )
  expect_no_match(
    server,
    "overview_default_percentage_cells_to_show",
    fixed = TRUE
  )
  expect_no_match(
    server,
    "gene_expression_default_percentage_cells_to_show",
    fixed = TRUE
  )

  for (path in list(
    c("overview", "UI_projection_additional_parameters.R"),
    c("spatial", "UI_projection_additional_parameters.R"),
    c("gene_expression", "UI_projection_additional_parameters.R"),
    c("trajectory", "projection.R")
  )) {
    source <- do.call(viewer_source, as.list(path))
    expect_match(source, "cell_percentage_cells_to_show", fixed = TRUE)
  }

  linked <- viewer_source("coordinated_views", "UI.R")
  expect_match(linked, "min = 10", fixed = TRUE)
  expect_match(linked, "step = 10", fixed = TRUE)
})

test_that("Projection renders an empty filter result instead of retaining cells", {
  for (path in list(
    c("overview", "obj_projection_data.R"),
    c("overview", "obj_projection_coordinates.R"),
    c("overview", "obj_projection_hover_info.R")
  )) {
    source <- do.call(viewer_source, as.list(path))
    expect_no_match(
      source,
      "req(overview_projection_cells_to_show())",
      fixed = TRUE
    )
  }
  data_to_plot <- viewer_source("overview", "obj_projection_data_to_plot.R")
  expect_match(data_to_plot, "nrow(cells_df) == 0L", fixed = TRUE)
})

test_that("Specialist group filters reuse the Linked views control", {
  widget <- viewer_source(
    "module",
    "group_filters",
    "group_filters_widget.R"
  )
  javascript <- viewer_source("www", "settings_drawer.js")
  shared_css <- viewer_source("www", "custom.css")
  linked_css <- viewer_source("www", "coordviews.css")
  linked_ui <- viewer_source("coordinated_views", "UI.R")
  cell_views <- viewer_source("www", "cell_views.js")

  expect_match(
    widget,
    'class = "cerebro-group-filters"',
    fixed = TRUE
  )
  expect_match(widget, 'class = "cv-filt-btn"', fixed = TRUE)
  expect_match(widget, 'class = "cv-filt-menu"', fixed = TRUE)
  expect_match(widget, 'class = "cv-filt-item"', fixed = TRUE)
  expect_match(widget, 'class = "cv-dot"', fixed = TRUE)
  expect_match(javascript, ".cerebro-group-filters .cv-filt", fixed = TRUE)
  expect_match(shared_css, ".cerebro-group-filters .cv-filt", fixed = TRUE)
  expect_no_match(linked_css, ".coordviews-page .cv-filt", fixed = TRUE)
  expect_match(linked_ui, "cerebro-group-filters", fixed = TRUE)
  expect_no_match(cell_views, "function closeFilterMenus", fixed = TRUE)

  for (path in list(
    c("overview", "UI_projection_group_filters.R"),
    c("spatial", "UI_projection_group_filters.R"),
    c("gene_expression", "UI_projection_group_filters.R"),
    c("trajectory", "projection.R")
  )) {
    source <- do.call(viewer_source, as.list(path))
    expect_match(source, "registerGroupFiltersUI", fixed = TRUE)
    expect_no_match(source, "pickerInput", fixed = TRUE)
  }

  ir_settings <- viewer_source("immune_repertoire", "settings.R")
  expect_match(ir_settings, "groupFilterControl(", fixed = TRUE)
  expect_no_match(ir_settings, "pickerInput", fixed = TRUE)
  expect_match(ir_settings, 'identical(p$type, "select")', fixed = TRUE)

  trajectory_plot <- viewer_source("trajectory", "projection_plot.R")
  expect_match(trajectory_plot, "cerebroGroupFilterMask", fixed = TRUE)
  expect_match(trajectory_plot, "trajectory_lines = list()", fixed = TRUE)
})

test_that("Gene expression display and colour modes are linked", {
  display_ui <- viewer_source(
    "gene_expression",
    "UI_projection_genes_separate_panels.R"
  )
  colour_ui <- viewer_source(
    "gene_expression",
    "UI_projection_gene_color_mode.R"
  )
  input_ui <- viewer_source(
    "gene_expression",
    "UI_projection_input_type.R"
  )

  css <- viewer_source("www", "custom.css")
  js <- viewer_source("www", "settings_drawer.js")

  expect_match(display_ui, 'label = "Display mode"', fixed = TRUE)
  expect_match(display_ui, '"Mean expression" = "combined"', fixed = TRUE)
  expect_match(display_ui, '"Separate panels" = "separate"', fixed = TRUE)
  expect_match(display_ui, '"RGB co-expression" = "rgb"', fixed = TRUE)
  expect_match(colour_ui, 'label = "Panel colours"', fixed = TRUE)
  expect_match(colour_ui, '"Shared scale" = "shared"', fixed = TRUE)
  expect_match(colour_ui, '"Distinct colours" = "different"', fixed = TRUE)
  expect_match(
    colour_ui,
    'n_genes <= 1 || display_mode != "separate"',
    fixed = TRUE
  )
  expect_no_match(colour_ui, "shinyjs::disabled", fixed = TRUE)
  expect_match(
    input_ui,
    'class = "cerebro-gene-rgb-row cerebro-control-enter"',
    fixed = TRUE
  )
  expect_match(input_ui, 'class = "cerebro-gene-rgb-channel"', fixed = TRUE)
  expect_match(
    css,
    ".cerebro-gene-control.cerebro-control-unlocked .selectize-input",
    fixed = TRUE
  )
  expect_no_match(css, ".cerebro-gene-control.is-disabled", fixed = TRUE)
  expect_match(
    css,
    ".cerebro-gene-rgb-channel:nth-child(3)",
    fixed = TRUE
  )
  expect_match(
    js,
    "shiny:outputinvalidated.cerebroGeneControls",
    fixed = TRUE
  )
  expect_match(css, "prefers-reduced-motion: reduce", fixed = TRUE)
})

test_that("Gene expression panels share one global numeric range", {
  env <- new.env(parent = globalenv())
  sys.source(
    viewer_path("gene_expression", "func_color_scale.R"),
    envir = env
  )

  levels <- list(GeneA = c(0, 1), GeneB = c(2, 7))
  expect_equal(env$expressionValueRange(levels), c(0, 7))
  expect_equal(env$expressionValueRange(c(0.2, 0.9)), c(0.2, 0.9))
})

test_that("Different gene colours are stable and visually distinct", {
  root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(root)) {
    root <- testthat::test_path("../../inst/viewer")
  }
  env <- new.env(parent = globalenv())
  sys.source(
    file.path(root, "gene_expression", "func_color_scale.R"),
    envir = env
  )

  genes <- paste0("Gene", seq_len(9))
  shared <- env$expressionPanelColorScales(genes, "shared", "Cerebro orange")
  different <- env$expressionPanelColorScales(
    genes,
    "different",
    "Cerebro orange"
  )

  scale_key <- function(scale) paste(capture.output(dput(scale)), collapse = "")
  expect_length(unique(vapply(shared, scale_key, character(1))), 1)
  expect_length(unique(vapply(different, scale_key, character(1))), 9)
  expect_identical(
    different,
    env$expressionPanelColorScales(genes, "different", "Cerebro orange")
  )
})

test_that("Canvas renderer accepts a palette per gene panel", {
  js <- viewer_source("www", "cell_views.js")
  css <- viewer_source("www", "coordviews.css")

  expect_match(js, "data.panel_colorscales", fixed = TRUE)
  expect_match(js, "panelScale", fixed = TRUE)
  expect_match(js, "Shared expression range", fixed = TRUE)
  expect_match(js, "cv-panel-scale", fixed = TRUE)
  expect_match(
    css,
    ".coordviews-page .cv-focus-btn {\n  position: absolute;",
    fixed = TRUE
  )
  expect_match(css, ".coordviews-page .cv-panel-scale", fixed = TRUE)
})

test_that("cell scatter pages debounce only complete render snapshots", {
  parameter_files <- c(
    viewer_path("overview", "obj_projection_parameters_plot.R"),
    viewer_path("spatial", "obj_projection_parameters_plot.R"),
    viewer_path("gene_expression", "obj_projection_parameters_plot.R")
  )
  snapshot_files <- c(
    viewer_path("overview", "obj_projection_data_to_plot.R"),
    viewer_path("spatial", "obj_projection_data_to_plot.R"),
    viewer_path("gene_expression", "obj_projection_data_to_plot.R")
  )

  for (path in parameter_files) {
    source <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_no_match(source, "debounce(", fixed = TRUE)
  }
  for (path in snapshot_files) {
    source <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(source, "<- debounce(", fixed = TRUE)
  }

  spatial_update <- viewer_source(
    "spatial",
    "func_projection_update_plot.R"
  )
  expect_no_match(spatial_update, "colnames(metadata)[1]", fixed = TRUE)
})
