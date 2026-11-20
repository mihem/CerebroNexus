## The Spatial page's section grid: one frame of every section's cells, one
## slice panel per section (inst/viewer/spatial/func_spatial_helpers.R and
## func_projection_update_plot.R).

## A renderer scope with the spatial helpers and the frame builder. Rendered
## frames are kept instead of being sent.
section_grid_renderer <- function(options = list(), private_url = NULL) {
  renderer <- viewer_runtime_env()
  renderer$Cerebro.options <- options
  for (file in c(
    file.path("core", "func_color_scale.R"),
    file.path("spatial", "func_spatial_helpers.R")
  )) {
    sys.source(viewer_test_path(file), envir = renderer)
  }
  sys.source(
    viewer_test_path("spatial", "func_projection_update_plot.R"),
    envir = renderer
  )
  renderer$viewerPrivateImageUrl <- private_url %||%
    function(path, session = NULL) paste0("private://", basename(path))
  renderer$rendered <- list()
  renderer$cerebroCellViewRender <- function(id, meta, data, hover, ...) {
    renderer$rendered[[length(renderer$rendered) + 1L]] <- list(
      id = id,
      meta = meta,
      data = data
    )
  }
  renderer
}

## Three sections of 3, 2 and 4 cells, laid out apart in x as the synthetic
## multi-section objects are (helper-synthetic-spatial.R), with canonical rows
## that interleave in the metadata.
section_grid_data <- function() {
  index <- list(
    sectionA1 = c(1L, 4L, 7L),
    sectionA2 = c(2L, 5L),
    sectionB1 = c(3L, 6L, 8L, 9L)
  )
  coordinates <- Map(
    function(rows, offset) {
      data.frame(
        x = offset + seq_along(rows) * 10,
        y = seq_along(rows) * 5,
        row.names = paste0("cell", rows)
      )
    },
    index,
    c(0, 500, 1000)
  )
  list(index = index, coordinates = coordinates)
}

test_that("the grid is offered for several sections and Dim or Feature plots", {
  sections <- c("a", "b")
  expect_true(spatial_grid_active("grid", sections, "ImageDimPlot"))
  expect_true(spatial_grid_active("grid", sections, "ImageFeaturePlot"))
  ## RGB co-expression stays a single-section view.
  expect_false(spatial_grid_active("grid", sections, "Co-expression (RGB)"))
  expect_false(spatial_grid_active("grid", "a", "ImageDimPlot"))
  expect_false(spatial_grid_active("single", sections, "ImageDimPlot"))
  expect_false(spatial_grid_active(NULL, sections, "ImageDimPlot"))
})

test_that("grid cells follow the sections in turn and remember their counts", {
  data <- section_grid_data()
  cells <- spatial_grid_cells(
    names(data$index),
    function(section) data$index[[section]]
  )
  expect_identical(c(cells), c(1L, 4L, 7L, 2L, 5L, 3L, 6L, 8L, 9L))
  expect_identical(
    spatial_grid_of(cells),
    list(sections = names(data$index), counts = c(3L, 2L, 4L))
  )
  ## A frame of one section carries no grid.
  expect_null(spatial_grid_of(1:3))

  ## Scope and group filters narrow each section; a section left without
  ## cells keeps its place with a count of 0.
  kept <- spatial_grid_cells(
    names(data$index),
    function(section) data$index[[section]],
    keep = function(rows) rows[rows != 2L & rows != 5L & rows != 9L]
  )
  expect_identical(c(kept), c(1L, 4L, 7L, 3L, 6L, 8L))
  expect_identical(spatial_grid_of(kept)$counts, c(3L, 0L, 3L))

  ## "% of cells" samples each section on its own, so every section keeps its
  ## share and its cells stay in section order.
  set.seed(1)
  sampled <- spatial_grid_cells(
    names(data$index),
    function(section) data$index[[section]],
    percentage = 50
  )
  expect_identical(spatial_grid_of(sampled)$counts, c(2L, 1L, 2L))
  grid <- spatial_grid_of(sampled)
  starts <- cumsum(grid$counts) - grid$counts
  for (i in seq_along(grid$sections)) {
    rows <- sampled[starts[[i]] + seq_len(grid$counts[[i]])]
    expect_true(all(rows %in% data$index[[grid$sections[[i]]]]))
    expect_false(is.unsorted(match(rows, data$index[[grid$sections[[i]]]])))
  }
})

test_that("grid coordinates come from each cell's own section", {
  data <- section_grid_data()
  cells <- spatial_grid_cells(
    names(data$index),
    function(section) data$index[[section]],
    keep = function(rows) rev(rows)
  )
  layout <- spatial_grid_coordinates(
    cells,
    function(section) data$coordinates[[section]],
    function(section) data$index[[section]],
    function(section) if (identical(section, "sectionA2")) 90 else 0
  )
  expect_identical(layout$sections, names(data$index))
  expect_identical(layout$starts, c(0L, 3L, 5L))
  expect_identical(layout$counts, c(3L, 2L, 4L))
  expect_identical(nrow(layout$coordinates), 9L)
  ## Section A1 reversed: rows 7, 4, 1 at x 30, 20, 10.
  expect_equal(layout$coordinates$x[1:3], c(1030, 1020, 1010) - 1000)
  ## Section A2 rotated by 90 degrees: (x, y) -> (-y, x).
  expect_equal(layout$coordinates$x[4:5], c(-10, -5))
  expect_equal(layout$coordinates$y[4:5], c(520, 510))
  ## Every section's panel spans its whole section, with a 2% margin.
  expect_equal(layout$x_ranges[[3]], c(1010 - 0.6, 1040 + 0.6))
  expect_equal(layout$y_ranges[[1]], c(5 - 0.2, 15 + 0.2))

  ## A cell that is not in the section it is listed under is an error, not
  ## a point drawn at an arbitrary place.
  wrong <- cells
  wrong[[1L]] <- 2L
  expect_error(
    spatial_grid_coordinates(
      wrong,
      function(section) data$coordinates[[section]],
      function(section) data$index[[section]]
    ),
    "not in their section: sectionA1"
  )
})

test_that("each grid panel shows its own section's image, and none without one", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "spatial-assets"))
  for (section in c("sectionA1", "sectionB1")) {
    write_dummy_png(file.path(root, "spatial-assets", paste0(section, ".png")))
  }
  bounds <- list(
    sectionA1 = list(xmin = 0, xmax = 50, ymin = 0, ymax = 20),
    sectionB1 = list(xmin = 1000, xmax = 1050, ymin = 0, ymax = 25)
  )
  options <- list(
    cerebro_root = root,
    spatial_images = list(
      demo = lapply(
        stats::setNames(names(bounds), names(bounds)),
        function(section) {
          list(
            `H&E` = list(
              path = file.path("spatial-assets", paste0(section, ".png")),
              bounds = bounds[[section]]
            )
          )
        }
      )
    )
  )
  renderer <- section_grid_renderer(options)
  data <- section_grid_data()
  backgrounds <- lapply(
    stats::setNames(names(data$index), names(data$index)),
    function(section) {
      renderer$spatial_grid_section_background(
        options,
        "demo",
        section,
        list(coordinates = data$coordinates[[section]])
      )
    }
  )
  expect_null(backgrounds$sectionA2)
  expect_identical(backgrounds$sectionB1$identity$spatial_name, "sectionB1")
  expect_identical(
    backgrounds$sectionA1$descriptor$path,
    file.path("spatial-assets", "sectionA1.png")
  )

  cells <- spatial_grid_cells(
    names(data$index),
    function(section) data$index[[section]]
  )
  layout <- spatial_grid_coordinates(
    cells,
    function(section) data$coordinates[[section]],
    function(section) data$index[[section]]
  )
  panels <- renderer$spatial_grid_panels(
    layout,
    Filter(Negate(is.null), backgrounds),
    root
  )
  expect_identical(
    vapply(panels, `[[`, character(1), "section"),
    names(data$index)
  )
  expect_identical(vapply(panels, `[[`, integer(1), "start"), c(0L, 3L, 5L))
  expect_identical(vapply(panels, `[[`, integer(1), "count"), c(3L, 2L, 4L))
  expect_true(all(vapply(panels, `[[`, logical(1), "spatial")))
  expect_identical(panels[[1]]$background_image, "private://sectionA1.png")
  expect_identical(panels[[3]]$background_image, "private://sectionB1.png")
  expect_equal(
    unlist(panels[[3]]$image_bounds),
    unlist(bounds$sectionB1)
  )
  expect_identical(panels[[3]]$image_identity$spatial_name, "sectionB1")
  ## The section without an image has no image fields at all.
  expect_false(any(
    c("background_image", "image_bounds", "image_identity") %in%
      names(panels[[2]])
  ))

  ## An image the server refuses (outside the trusted folders) leaves its
  ## panel without one instead of failing the frame.
  refused <- backgrounds
  refused$sectionA1$descriptor$path <- "../elsewhere.png"
  refused$sectionA1$image_allowlist <- "../elsewhere.png"
  panels <- suppressMessages(renderer$spatial_grid_panels(
    layout,
    Filter(Negate(is.null), refused),
    root
  ))
  expect_false("background_image" %in% names(panels[[1]]))
  expect_identical(panels[[3]]$background_image, "private://sectionB1.png")
})

test_that("a grid's embedding panel draws every row of the frame", {
  expect_identical(spatial_grid_projection_panel(NULL), list())
  expect_identical(
    spatial_grid_projection_panel(list(label = "umap", x = 1:2, y = 1)),
    list()
  )
  panel <- spatial_grid_projection_panel(
    list(label = "umap", x = c(0, 10, 5), y = c(1, 2, 3))
  )[[1]]
  expect_identical(panel$id, "projection")
  expect_true(panel$projection)
  ## Not a slice and not a section: it has positions of its own.
  expect_false(any(c("start", "count", "section", "spatial") %in% names(panel)))
  expect_equal(panel$x_range, c(-0.2, 10.2))
})

## The frame a grid of the three sections renders, coloured by `color`.
render_section_grid <- function(color, plot_type = "ImageDimPlot") {
  renderer <- section_grid_renderer()
  data <- section_grid_data()
  cells <- spatial_grid_cells(
    names(data$index),
    function(section) data$index[[section]]
  )
  layout <- spatial_grid_coordinates(
    cells,
    function(section) data$coordinates[[section]],
    function(section) data$index[[section]]
  )
  metadata <- data.frame(value = color[c(cells)], cell_index = c(cells))
  renderer$spatial_projection_update_plot(list(
    dataset_context = list(dataset_key = "demo"),
    cells_df = metadata,
    coordinates = layout$coordinates,
    reset_axes = FALSE,
    color_assignments = if (is.numeric(color)) {
      NULL
    } else {
      c(T = "#1b9e77", B = "#d95f02", NK = "#7570b3")
    },
    resource_first = FALSE,
    cell_indices = c(cells),
    plot_parameters = list(
      projection = "sectionA1",
      layout = "grid",
      grid_sections = names(data$index),
      grid = layout[setdiff(names(layout), "coordinates")],
      grid_backgrounds = list(),
      color_variable = "value",
      plot_type = plot_type,
      split_by = "",
      n_dimensions = 2L,
      point_size = 5,
      point_opacity = 1,
      draw_border = FALSE,
      group_labels = FALSE,
      keep_square = FALSE,
      hover_info = FALSE,
      x_range = spatial_grid_range(layout$coordinates$x),
      y_range = spatial_grid_range(layout$coordinates$y),
      background_descriptor = NULL
    )
  ))
  renderer$rendered[[1]]
}

test_that("a grid frame colours every section on one continuous scale", {
  ## Section A2 holds the extremes: one scale means the other sections are
  ## read against them, not stretched over their own range.
  expression <- c(1, -2, 3, 4, 9, 6, 2.5, 3.5, 4.5)
  frame <- render_section_grid(expression, "ImageFeaturePlot")
  expect_identical(frame$id, "spatial_projection")
  expect_identical(frame$meta$color_type, "continuous")
  expect_equal(frame$data$color_range, c(-2, 9))
  ## One colour vector for the frame; no panel carries its own scale.
  expect_length(frame$data$color, 9L)
  expect_null(frame$data$panel_colorscales)
  expect_false(any(vapply(
    frame$data$panels,
    function(panel) any(c("color", "colorscale") %in% names(panel)),
    logical(1)
  )))
  ## Slices index the flat frame and copy none of its points.
  expect_identical(
    vapply(frame$data$panels, `[[`, integer(1), "start"),
    c(0L, 3L, 5L)
  )
  expect_false(any(vapply(
    frame$data$panels,
    function(panel) any(c("x", "y", "selection_key") %in% names(panel)),
    logical(1)
  )))
  expect_length(frame$data$x, 9L)
  expect_identical(c(frame$data$selection_key), 1:9)
  expect_identical(frame$meta$space_label, "Section grid")
})

test_that("a grid frame colours a group the same in every section", {
  groups <- c("T", "B", "NK", "T", "B", "NK", "T", NA, "B")
  frame <- render_section_grid(groups)
  expect_identical(frame$meta$color_type, "categorical")
  traces <- unlist(frame$meta$traces)
  ## Flat codes into the frame's traces, in frame order.
  cells <- c(1L, 4L, 7L, 2L, 5L, 3L, 6L, 8L, 9L)
  shown <- groups[cells]
  shown[is.na(shown)] <- "(missing)"
  expect_identical(traces[frame$data$group_code + 1L], shown)
  expect_identical(
    unlist(frame$meta$group_colors)[match(c("T", "B", "NK"), traces)],
    c("#1b9e77", "#d95f02", "#7570b3")
  )
  expect_identical(
    unlist(frame$meta$group_colors)[match("(missing)", traces)],
    "#7b8794"
  )
  expect_length(frame$data$x, 9L)
  expect_length(frame$data$y, 9L)
  expect_null(frame$data$canonical_group)
})

test_that("the browser shares, arranges and zooms the section grid", {
  expect_node_fixture(
    "spatial-section-grid.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS synced zoom gives every shown section the same relative view"
  )
})

test_that("the author's Spatial layout is a display setting, grid only with sections", {
  ## Older projects and Apps carry no layout: they open one section.
  expect_identical(builderViewerDisplayDefaults()$spatial_layout, "single")
  display <- builderViewerDisplayDefaults(list(spatial_layout = "grid"))
  expect_identical(display$spatial_layout, "grid")
  expect_identical(
    builderViewerDisplayDefaults(
      jsonlite::fromJSON(jsonlite::toJSON(display, auto_unbox = TRUE))
    ),
    display
  )
  expect_error(
    builderViewerDisplayDefaults(list(spatial_layout = "mosaic")),
    "invalid"
  )
  options <- list(viewer_content = list(ds = list(display = display)))
  page <- viewerPageDisplayDefaults(options, "ds", "spatial")
  expect_identical(page$layout, "grid")
  ## The grid needs more than one section; without settings, one section.
  expect_identical(spatial_default_layout(page, c("a", "b")), "grid")
  expect_identical(spatial_default_layout(page, "a"), "single")
  expect_identical(
    spatial_default_layout(
      viewerPageDisplayDefaults(list(), "x", "spatial"),
      c("a", "b")
    ),
    "single"
  )
  expect_identical(spatial_default_layout(NULL, c("a", "b")), "single")
})

test_that("the Builder offers the Spatial layout with the author's choice", {
  skip_if_not_installed("shiny")
  runtime <- new.env(parent = globalenv())
  builder_stage_contract_source_runtime(runtime)
  render <- function(layout) {
    entry <- list(
      id = "ds1",
      settings = list(viewer_display = list(spatial_layout = layout))
    )
    builder_stage_html(runtime$builder_viewer_display_ui("core", entry, 1L))
  }
  html <- render("grid")
  expect_match(html, 'data-display-field="spatial_layout"', fixed = TRUE)
  expect_match(html, "Open Spatial as", fixed = TRUE)
  expect_match(html, '<option value="grid" selected="selected">', fixed = TRUE)
  expect_match(html, "Square panels", fixed = TRUE)
  expect_match(
    render("single"),
    '<option value="single" selected="selected">',
    fixed = TRUE
  )
})
