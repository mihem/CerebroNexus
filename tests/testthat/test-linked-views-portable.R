test_that("current Linked views state round-trips through the JSON validator", {
  root <- system.file("viewer", package = "CerebroNexus")
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(root, "coordinated_views", "config.R"), envir = helpers)
  cells <- c("cell-1", "cell-2")

  config <- list(
    schema = "cerebronexus-linked-view",
    version = 1L,
    created_at = "2026-08-27T12:00:00Z",
    dataset = list(
      cell_count = 2L,
      cell_fingerprint = helpers$cv_config_cell_fingerprint(cells)
    ),
    selection = list(
      cells = "cell-1",
      source = "umap",
      geometry = list(
        space = "projection::umap",
        mode = "box",
        polygon = list(c(0.1, 0.1), c(0.5, 0.1), c(0.5, 0.5), c(0.1, 0.5))
      )
    ),
    view = list(
      colour = list(
        mode = "__gene_panels__",
        genes = "CD3D",
        gene = NULL,
        rgb_genes = character(),
        clip = 0.05
      ),
      projections = "umap",
      spatial_sections = character(),
      active_spatial = NULL,
      filters = structure(list(), names = character()),
      hidden_levels = list(list(group = "cell_type", levels = "T cells")),
      display = list(
        percentage_cells = 100,
        point_size = 3,
        point_opacity = 0.8,
        group_labels = TRUE,
        cell_borders = TRUE,
        selection_mode = "box",
        clone_layout = "stack",
        keep_square = TRUE
      ),
      focus_space = "projection::umap",
      lenses = list(list(
        space = "projection::umap",
        viewport = list(cx = 0.5, cy = 0.5, span = 1),
        rotation = NULL
      )),
      spatial_backgrounds = list(),
      trekker = list(
        dissolve_percentage = 0,
        evidence = FALSE,
        niche_radius = 250
      )
    )
  )

  prepared <- helpers$cv_config_prepare(config, cells = cells)
  restored <- helpers$cv_config_decode(prepared$json, cells = rev(cells))

  expect_true(restored$view$display$keep_square)
  expect_true(restored$view$display$cell_borders)
  expect_identical(restored$view$colour$genes, "CD3D")
  expect_identical(restored$view$focus_space, "projection::umap")
  expect_identical(restored$view$hidden_levels[[1]]$levels, "T cells")
  expect_match(prepared$json, '"genes": ["CD3D"]', fixed = TRUE)
})

test_that("Linked views round-trip without a selection geometry", {
  root <- system.file("viewer", package = "CerebroNexus")
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(root, "coordinated_views", "config.R"), envir = helpers)
  cells <- c("cell-1", "cell-2")
  config <- list(
    schema = "cerebronexus-linked-view",
    version = 1L,
    created_at = "2026-08-27T12:00:00Z",
    dataset = list(
      cell_count = 2L,
      cell_fingerprint = helpers$cv_config_cell_fingerprint(cells)
    ),
    selection = list(cells = "cell-1", source = "umap", geometry = NULL),
    view = list(
      colour = list(
        mode = "cell_type",
        genes = character(),
        gene = NULL,
        rgb_genes = character(),
        clip = 0.05
      ),
      projections = "umap",
      spatial_sections = character(),
      active_spatial = NULL,
      filters = structure(list(), names = character()),
      hidden_levels = list(),
      display = list(
        percentage_cells = 100,
        point_size = 3,
        point_opacity = 0.8,
        group_labels = TRUE,
        cell_borders = FALSE,
        selection_mode = "lasso",
        clone_layout = "stack",
        keep_square = FALSE
      ),
      focus_space = "projection::umap",
      lenses = list(list(
        space = "projection::umap",
        viewport = list(cx = 0.5, cy = 0.5, span = 1),
        rotation = NULL
      )),
      spatial_backgrounds = list(),
      trekker = list(
        dissolve_percentage = 0,
        evidence = FALSE,
        niche_radius = 250
      )
    )
  )

  prepared <- helpers$cv_config_prepare(config, cells = cells)
  restored <- helpers$cv_config_decode(prepared$json, cells = cells)

  expect_null(restored$selection$geometry)
})

test_that("cell identity validation is unique and order-independent", {
  root <- system.file("viewer", package = "CerebroNexus")
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(root, "coordinated_views", "bundle.R"), envir = helpers)

  expect_error(helpers$cv_cell_ids(c("cell-1", "cell-1")), "duplicate")
  expect_error(helpers$cv_cell_ids(c("cell-1", "")), "missing")
  expect_identical(
    helpers$cv_cell_fingerprint(c("cell-2", "cell-1")),
    helpers$cv_cell_fingerprint(c("cell-1", "cell-2"))
  )
})

test_that("specialist configuration is validated and round-trips", {
  root <- system.file("viewer", package = "CerebroNexus")
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(root, "coordinated_views", "config.R"), envir = helpers)

  config <- list(
    schema = "cerebronexus-specialist-view",
    version = 1L,
    page = list(
      id = "overview_projection",
      label = "Projection",
      tab = "overview",
      engine = "canvas"
    ),
    selection = list(
      cells = c("cell-1"),
      geometry = list(
        mode = "lasso",
        panel = 0L,
        polygon = list(c(0.1, 0.2), c(0.4, 0.2), c(0.3, 0.5))
      )
    ),
    view = list(
      viewport = list(x0 = 0, x1 = 1, y0 = 0, y1 = 1),
      rotation = list(x = 0, y = 0),
      mode = "lasso",
      zoomed = FALSE,
      hidden_groups = character()
    ),
    controls = list(list(
      id = "overview_projection_select_projection",
      value_type = "string",
      multiple = FALSE,
      values = "umap"
    ))
  )

  prepared <- helpers$cv_config_prepare(
    config,
    cells = c("cell-1", "cell-2"),
    now = as.POSIXct("2026-08-27 12:00:00", tz = "UTC")
  )
  restored <- helpers$cv_config_decode(
    prepared$json,
    cells = c("cell-2", "cell-1")
  )

  expect_identical(restored$schema, "cerebronexus-specialist-view")
  expect_identical(restored$selection$cells, "cell-1")
  expect_identical(restored$page$id, "overview_projection")

  unzoomed <- config
  unzoomed$view["viewport"] <- list(NULL)
  expect_null(
    helpers$cv_config_prepare(
      unzoomed,
      cells = c("cell-1", "cell-2")
    )$config$view$viewport
  )

  zoomed <- config
  zoomed$view$viewport <- list(cx = 0.5, cy = 0.4, span = 0.75)
  zoomed$view$zoomed <- TRUE
  expect_identical(
    helpers$cv_config_prepare(
      zoomed,
      cells = c("cell-1", "cell-2")
    )$config$view$viewport,
    list(cx = 0.5, cy = 0.4, span = 0.75)
  )

  network <- config
  network$page <- list(
    id = "hla_motif_network",
    label = "HLA & TCR Motifs",
    tab = "hla_tcr_motifs",
    engine = "network"
  )
  network$controls[[1]]$id <- "hla_motifs_chain"
  expect_identical(
    helpers$cv_config_prepare(
      network,
      cells = c("cell-1", "cell-2")
    )$config$view$viewport,
    list(x0 = 0, x1 = 1, y0 = 0, y1 = 1)
  )

  spatial <- config
  spatial$page <- list(
    id = "spatial_projection",
    label = "Spatial",
    tab = "spatial",
    engine = "canvas"
  )
  spatial$controls[[1]]$id <- "spatial_projection_select_projection"
  expect_identical(
    helpers$cv_config_prepare(
      spatial,
      cells = c("cell-1", "cell-2")
    )$config$page$engine,
    "canvas"
  )
  expect_error(
    helpers$cv_config_prepare(
      within(config, controls[[1]]$id <- "hla_motifs_chain"),
      cells = c("cell-1", "cell-2")
    ),
    "not allowed"
  )
})
