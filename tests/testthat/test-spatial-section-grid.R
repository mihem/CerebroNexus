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
  renderer$cerebroCellViewRecolor <- function(id, meta, data, ...) {
    renderer$rendered[[length(renderer$rendered) + 1L]] <- list(
      id = id,
      meta = meta,
      data = data,
      recolor = TRUE
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

test_that("the grid is offered for several sections and every plot type", {
  sections <- c("a", "b")
  expect_true(spatial_grid_active("grid", sections, "ImageDimPlot"))
  expect_true(spatial_grid_active("grid", sections, "ImageFeaturePlot"))
  expect_true(spatial_grid_active("grid", sections, "Co-expression (RGB)"))
  expect_false(spatial_grid_active("grid", sections, "Something else"))
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

test_that("the grid's picker chooses one picture per section", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "spatial-assets"))
  for (file in c("a-he", "a-dapi", "b-he")) {
    write_dummy_png(file.path(root, "spatial-assets", paste0(file, ".png")))
  }
  picture <- function(file) {
    list(
      path = file.path("spatial-assets", paste0(file, ".png")),
      bounds = list(xmin = 0, xmax = 50, ymin = 0, ymax = 20)
    )
  }
  options <- list(
    cerebro_root = root,
    spatial_images = list(
      demo = list(
        sectionA1 = list(`H&E` = picture("a-he"), DAPI = picture("a-dapi")),
        sectionB1 = list(`H&E` = picture("b-he"))
      )
    )
  )
  renderer <- section_grid_renderer(options)
  data <- section_grid_data()
  record <- function(section) list(coordinates = data$coordinates[[section]])
  all_sections <- names(data$index)
  groups <- renderer$spatial_grid_background_groups(
    options,
    "demo",
    c("sectionB1", "sectionA1", "sectionA2"),
    all_sections,
    record
  )
  ## A group per section with pictures, in the order shown; "No picture" first.
  expect_identical(names(groups), c("sectionB1", "sectionA1"))
  a1 <- match("sectionA1", all_sections)
  expect_identical(
    groups$sectionA1$tokens,
    stats::setNames(
      paste0("grid-", a1, "-background-", 0:2),
      c("No picture", "H&E", "DAPI")
    )
  )
  picks <- renderer$spatial_grid_background_picks(
    c(paste0("grid-", a1, "-background-2"), "grid-99-background-1", "junk"),
    all_sections
  )
  expect_identical(picks, list(sectionA1 = 2L))
  chosen <- renderer$spatial_grid_section_background(
    options,
    "demo",
    "sectionA1",
    record("sectionA1"),
    pick = picks$sectionA1
  )
  expect_identical(chosen$descriptor$path, picture("a-dapi")$path)
  ## No pick: the first picture; 0: none; a pick past the end: the first.
  first <- renderer$spatial_grid_section_background(
    options,
    "demo",
    "sectionA1",
    record("sectionA1")
  )
  expect_identical(first$descriptor$path, picture("a-he")$path)
  expect_null(renderer$spatial_grid_section_background(
    options,
    "demo",
    "sectionA1",
    record("sectionA1"),
    pick = 0L
  ))
  expect_identical(
    renderer$spatial_grid_section_background(
      options,
      "demo",
      "sectionB1",
      record("sectionB1"),
      pick = 2L
    )$descriptor$path,
    picture("b-he")$path
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
render_section_grid <- function(
  color,
  plot_type = "ImageDimPlot",
  renderer = section_grid_renderer(),
  fresh_render = FALSE
) {
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
    fresh_render = fresh_render,
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
  renderer$rendered[[length(renderer$rendered)]]
}

test_that("a grid coloured anew over the same cells sends only its colours", {
  renderer <- section_grid_renderer()
  groups <- c("T", "B", "NK", "T", "B", "NK", "T", "B", "NK")
  first <- render_section_grid(groups, renderer = renderer)
  expect_null(first$recolor)
  expect_length(first$data$x, 9L)
  ## Another colouring: no positions, no panels, the codes of the new groups.
  again <- render_section_grid(rev(groups), renderer = renderer)
  expect_true(again$recolor)
  ## The frame holds the cells section by section.
  frame_rows <- c(1L, 4L, 7L, 2L, 5L, 3L, 6L, 8L, 9L)
  expect_identical(
    unlist(again$meta$traces)[again$data$group_code + 1L],
    rev(groups)[frame_rows]
  )
  expression <- c(1, -2, 3, 4, 9, 6, 2.5, 3.5, 4.5)
  numeric <- render_section_grid(
    expression,
    "ImageFeaturePlot",
    renderer = renderer
  )
  expect_true(numeric$recolor)
  expect_equal(numeric$data$color_range, c(-2, 9))
  ## A render request (a remounted canvas, another data set) gets the whole
  ## frame, and so does the grid of a new renderer.
  fresh <- render_section_grid(groups, renderer = renderer, fresh_render = TRUE)
  expect_null(fresh$recolor)
  expect_length(fresh$data$panels, 3L)
})

test_that("a grid frame holds every section, or as many as its budget takes", {
  all <- c("s1", "s2", "s3", "s4", "s5", "s6")
  cells <- c(s1 = 40, s2 = 30, s3 = 30, s4 = 20, s5 = 50, s6 = 10)
  count <- function(section) cells[[section]]
  ## Within the budget: every section, whatever is chosen.
  expect_identical(
    spatial_grid_frame_sections(all, c("s1", "s4"), count, budget = 180),
    all
  )
  ## Past it: the chosen ones, then the others nearest to them, while they fit.
  expect_identical(
    spatial_grid_frame_sections(all, c("s2", "s5"), count, budget = 120),
    c("s1", "s2", "s5")
  )
  expect_identical(
    spatial_grid_frame_sections(all, c("s1", "s6"), count, budget = 90),
    c("s1", "s2", "s6")
  )
  ## The frame on screen stays while it holds every chosen section ...
  previous <- c("s1", "s2", "s5")
  expect_identical(
    spatial_grid_frame_sections(
      all,
      c("s5", "s1"),
      count,
      previous = previous,
      budget = 120
    ),
    previous
  )
  ## ... and gives way to one around a section it lacks.
  expect_identical(
    spatial_grid_frame_sections(
      all,
      c("s3", "s6"),
      count,
      previous = previous,
      budget = 120
    ),
    c("s2", "s3", "s4", "s6")
  )
  ## Chosen sections are kept even past the budget.
  expect_identical(
    spatial_grid_frame_sections(all, c("s1", "s5"), count, budget = 60),
    c("s1", "s5")
  )
})

test_that("a grid of many cells streams its sections in batches, shown first", {
  counts <- c(30L, 20L, 40L, 10L, 50L)
  ## The first batch ends at `first` cells, the others at `step`; sections
  ## shown first, then the rest in frame order.
  expect_identical(
    spatial_grid_stream_batches(counts, c(3L, 1L), first = 50, step = 60),
    list(c(3L, 1L), c(2L, 4L, 5L))
  )
  expect_identical(
    spatial_grid_stream_batches(counts, integer(), first = 25, step = 45),
    list(1L, c(2L, 3L), c(4L, 5L))
  )
  expect_identical(
    spatial_grid_stream_batches(counts, c(9L, NA, 2L), first = 1e6),
    list(c(2L, 1L, 3L, 4L, 5L))
  )
  ## A batch's rows: each section's slice of the frame in turn.
  expect_identical(
    spatial_grid_stream_rows(c(3L, 1L), c(0L, 30L, 50L), c(30L, 20L, 40L)),
    c(51:90, 1:30)
  )

  ## Through the frame builder: section B1 (4 cells) is shown first, the two
  ## of sample A follow as the second batch.
  renderer <- section_grid_renderer()
  renderer$spatial_grid_stream_min <- 5
  renderer$spatial_grid_stream_first <- 4
  renderer$spatial_grid_stream_step <- 10
  renderer$spatial_grid_shown_sections <- function() c("sectionB1", "sectionA2")
  streams <- list()
  renderer$spatial_grid_stream_send <- function(stream, render_token, ...) {
    streams[[length(streams) + 1L]] <<- stream
  }
  ## The cells are named by canonical row from a cell-name resource.
  renderer$viewerProjectionFirstFrameMetadata <- function() data.frame(x = 1:9)
  renderer$viewerProjectionMetadataColumns <- function(...) character()
  renderer$getGroups <- function() character()
  renderer$viewerCellNamesResourceFor <- function(rows) list(cells = 9L)
  head_aux <- NULL
  renderer$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover,
    ...,
    deferred_aux = NULL
  ) {
    head_aux <<- deferred_aux()
    renderer$rendered[[length(renderer$rendered) + 1L]] <- list(
      id = id,
      meta = meta,
      data = data
    )
  }
  groups <- c("T", "B", "NK", "T", "B", "NK", "T", "B", "NK")
  head <- render_section_grid(groups, renderer = renderer)
  expect_length(streams, 1L)
  expect_identical(
    head$data$grid_stream[c("rows", "batches")],
    list(rows = 9L, batches = 2L)
  )
  expect_identical(unclass(head$data$grid_stream$section), "sectionB1")
  expect_identical(unclass(head$data$grid_stream$start), 5L)
  ## The first frame carries every panel and every cell's key, but the
  ## positions and colours of section B1 only.
  expect_length(head$data$panels, 3L)
  expect_length(head$data$selection_key, 9L)
  expect_identical(as.numeric(head$data$x), c(10, 20, 30, 40) + 1000)
  traces <- unlist(head$meta$traces)
  frame_rows <- c(1L, 4L, 7L, 2L, 5L, 3L, 6L, 8L, 9L)
  expect_identical(traces[head$data$group_code + 1L], groups[frame_rows][6:9])
  ## The first message names the cells of section B1 only (canonical rows 3,
  ## 6, 8 and 9, 0-based); the batch names its own.
  expect_identical(head_aux$selection_index, c(2L, 5L, 7L, 8L))
  expect_identical(head_aux$cell_names_resource, list(cells = 9L))
  rest <- streams[[1L]]$part(2L)
  expect_identical(rest$selection_index, c(1L, 4L, 0L, 3L, 6L))
  expect_identical(rest$section, c("sectionA2", "sectionA1"))
  expect_identical(rest$start, c(3L, 0L))
  expect_identical(rest$count, c(2L, 3L))
  expect_identical(rest$x, c(510, 520, 10, 20, 30))
  expect_identical(
    traces[rest$group_code + 1L],
    groups[frame_rows][c(4:5, 1:3)]
  )
  ## Until its last batch is sent a new colouring gets a whole frame.
  again <- render_section_grid(rev(groups), renderer = renderer)
  expect_null(again$recolor)
  expect_length(streams, 2L)

  ## A frame of fewer cells, or of one section, is sent whole.
  renderer$spatial_grid_stream_min <- 100
  whole <- render_section_grid(groups, renderer = renderer)
  expect_null(whole$data$grid_stream)
  expect_length(whole$data$x, 9L)
  expect_length(streams, 2L)
})

test_that("a grid section read from a Viewer Pack file carries no positions", {
  data <- section_grid_data()
  cells <- spatial_grid_cells(
    names(data$index),
    function(section) data$index[[section]]
  )
  file <- list(
    protocol = "spatial-geometry-v1",
    url = "pack/002.bin",
    cells = 2L
  )
  layout <- spatial_grid_coordinates(
    cells,
    function(section) data$coordinates[[section]],
    function(section) data$index[[section]],
    geometry_of = function(section, rows) {
      if (identical(section, "sectionA2")) file
    }
  )
  ## Its rows have no position; its extent is still its own.
  expect_equal(layout$coordinates$x[1:3], c(10, 20, 30))
  expect_true(all(is.na(layout$coordinates$x[4:5])))
  expect_equal(layout$x_ranges[[2]], c(509.8, 520.2))
  expect_identical(layout$geometry[[2]], file)
  expect_null(layout$geometry[[1]])
  panels <- spatial_grid_panels(layout)
  expect_identical(panels[[2]]$geometry_resource, file)
  expect_false("geometry_resource" %in% names(panels[[1]]))

  ## Its batch carries the section's keys and colours but no positions.
  frame <- list(
    x = layout$coordinates$x,
    y = layout$coordinates$y,
    group_code = 0:8,
    panels = panels
  )
  stream <- spatial_grid_stream_split(
    frame,
    character(),
    min_cells = 5,
    first = 4,
    step = 10
  )
  expect_identical(
    unclass(stream$head$grid_stream$section),
    c("sectionA1", "sectionA2")
  )
  expect_equal(stream$head$x, c(10, 20, 30))
  expect_identical(stream$head$group_code, 0:4)
  expect_equal(stream$part(2L)$x, c(1010, 1020, 1030, 1040))

  ## A frame whose positions are all in files streams even as one batch.
  every <- spatial_grid_coordinates(
    cells,
    function(section) data$coordinates[[section]],
    function(section) data$index[[section]],
    geometry_of = function(section, rows) file
  )
  one <- spatial_grid_stream_split(
    list(
      x = every$coordinates$x,
      y = every$coordinates$y,
      group_code = 0:8,
      panels = spatial_grid_panels(every)
    ),
    character(),
    min_cells = 5,
    first = 100
  )
  expect_identical(one$batches, 1L)
  expect_length(one$head$x, 0L)
})

test_that("a streaming grid's cells travel as runs of rows when they are", {
  ## One block of the data set: its start and length.
  expect_identical(
    spatial_grid_row_runs(100:199),
    list(kind = "ranges", start = I(100L), length = I(100L))
  )
  expect_identical(
    spatial_grid_row_runs(c(0:49, 500:549)),
    list(kind = "ranges", start = I(c(0L, 500L)), length = I(c(50L, 50L)))
  )
  ## Cells scattered through the data set travel one by one.
  scattered <- as.integer(seq(1, 199, by = 2))
  expect_identical(spatial_grid_row_runs(scattered), scattered)
  expect_identical(spatial_grid_row_runs(5L), 5L)

  ## The keys of each batch: canonical rows, or names without a resource.
  stream <- list(
    rows_of = function(k) list(1:2, 3:5)[[k]],
    part = function(k) list(batch = k)
  )
  by_rows <- spatial_grid_stream_keys(
    stream,
    list(
      selection_index = list(kind = "range", start = 10L, length = 5L),
      cell_names_resource = "names",
      hover = list(hoverinfo = "skip")
    )
  )
  expect_identical(by_rows$head_aux$selection_index, c(10L, 11L))
  expect_identical(by_rows$head_aux$cell_names_resource, "names")
  expect_identical(by_rows$head_aux$hover, list(hoverinfo = "skip"))
  expect_identical(
    by_rows$part(2L),
    list(batch = 2L, selection_index = c(12L, 13L, 14L))
  )
  by_name <- spatial_grid_stream_keys(
    stream,
    list(selection_key = I(c("a", "b", "c", "d", "e")))
  )
  expect_identical(by_name$head_aux$selection_key, c("a", "b"))
  expect_identical(by_name$part(2L)$selection_key, c("c", "d", "e"))
})

test_that("a grid's metadata is its columns at its cells, without row names", {
  full <- data.frame(
    group = factor(c("a", "b", "a", "c")),
    other = 1:4,
    row.names = c("w", "x", "y", "z")
  )
  metadata <- spatial_grid_metadata(full, c(4L, 1L), "group")
  expect_identical(names(metadata), c("group", "cell_index"))
  expect_identical(
    metadata$group,
    factor(c("c", "a"), levels = c("a", "b", "c"))
  )
  expect_identical(metadata$cell_index, c(4L, 1L))
  expect_identical(nrow(metadata), 2L)
  expect_identical(.row_names_info(metadata), -2L)
})

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

test_that("the browser draws a streaming grid batch by batch", {
  expect_node_fixture(
    "spatial-section-grid.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS a streaming grid draws each batch of sections as it comes"
  )
  expect_node_fixture(
    "spatial-section-grid.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS a streaming grid names its cells batch by batch"
  )
})

test_that("a panel of some cells keeps its maps over its own cells", {
  expect_node_fixture(
    "spatial-section-grid.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS a panel of some cells keeps its maps over its own cells"
  )
})

test_that("Moran's I finds each cell's six nearest as comparing every pair does", {
  expect_node_fixture(
    "spatial-section-grid.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS Moran's I finds each cell's six nearest through buckets"
  )
})

test_that("the browser draws a section read from a file once its batch and file are in", {
  expect_node_fixture(
    "spatial-section-grid.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS sections read from files wait for their file and their batch"
  )
})

test_that("the browser fetches a grid's files a few at a time, in panel order", {
  expect_node_fixture(
    "spatial-section-grid.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS the grid fetches its files a few at a time, in panel order"
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
  ## The package's internal functions (builderViewerDisplayDefaults()) are
  ## reachable from the test environment, not from the global one, when the
  ## package is installed rather than loaded with load_all().
  runtime <- new.env(parent = environment())
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
  expect_match(html, "Open Spatial with", fixed = TRUE)
  expect_match(html, '<option value="grid" selected="selected">', fixed = TRUE)
  expect_match(html, "Square panels", fixed = TRUE)
  expect_match(
    render("single"),
    '<option value="single" selected="selected">',
    fixed = TRUE
  )
})
