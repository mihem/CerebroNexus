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

test_that("view JSON uploads validate metadata and actual file size", {
  root <- system.file("viewer", package = "CerebroNexus")
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(root, "coordinated_views", "config.R"), envir = helpers)
  text <- '{"schema":"test"}'
  upload <- list(
    name = "view.json",
    size = nchar(text, type = "bytes"),
    text = text
  )
  error_code <- function(expr) {
    tryCatch(
      {
        force(expr)
        NA_character_
      },
      cv_config_error = function(error) error$code
    )
  }

  expect_identical(helpers$cv_config_read_upload(upload), text)
  expect_identical(
    error_code(helpers$cv_config_read_upload(upload["name"])),
    "invalid_file"
  )
  expect_identical(
    error_code(helpers$cv_config_read_upload(modifyList(
      upload,
      list(name = "view.txt")
    ))),
    "invalid_file"
  )
  expect_identical(
    error_code(helpers$cv_config_read_upload(
      modifyList(upload, list(size = 1)),
      max_bytes = nchar(text) - 1L
    )),
    "too_large"
  )
  expect_identical(
    error_code(helpers$cv_config_read_upload(
      modifyList(upload, list(size = nchar(text) + 1L)),
      max_bytes = nchar(text)
    )),
    "too_large"
  )
  expect_identical(
    helpers$cv_config_read_upload(upload, max_bytes = nchar(text)),
    text
  )
  expect_identical(
    error_code(helpers$cv_config_read_upload(modifyList(
      upload,
      list(size = -1)
    ))),
    "too_large"
  )
  expect_identical(
    error_code(helpers$cv_config_read_upload(modifyList(
      upload,
      list(size = "17")
    ))),
    "invalid_file"
  )
})

test_that("cell identity validation is unique and order-independent", {
  root <- system.file("viewer", package = "CerebroNexus")
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(root, "coordinated_views", "bundle.R"), envir = helpers)
  sys.source(file.path(root, "coordinated_views", "config.R"), envir = helpers)

  expect_error(helpers$cv_cell_ids(c("cell-1", "cell-1")), "duplicate")
  expect_error(helpers$cv_cell_ids(c("cell-1", "")), "missing")
  expect_identical(
    helpers$cv_config_cell_fingerprint(c("cell-2", "cell-1")),
    helpers$cv_config_cell_fingerprint(c("cell-1", "cell-2"))
  )
})

test_that("stored cell fingerprints bypass runtime hashing", {
  root <- system.file("viewer", package = "CerebroNexus")
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(root, "coordinated_views", "config.R"), envir = helpers)
  stored <- paste0("md5-cell-set-v1:", paste(rep("a", 32L), collapse = ""))

  expect_identical(
    helpers$cv_config_dataset_fingerprint(c("cell-1", "cell-2"), stored),
    stored
  )
  expect_identical(
    helpers$cv_config_dataset_fingerprint(c("cell-2", "cell-1"), NULL),
    helpers$cv_config_cell_fingerprint(c("cell-1", "cell-2"))
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

  hla <- config
  hla$page <- list(
    id = "hla_motif_network",
    label = "HLA & TCR Motifs",
    tab = "hla_tcr_motifs",
    engine = "canvas"
  )
  hla$controls[[1]]$id <- "hla_motifs_chain"
  prepared_hla <- helpers$cv_config_prepare(
    hla,
    cells = c("cell-1", "cell-2")
  )$config
  expect_identical(prepared_hla$page$engine, "canvas")
  expect_null(prepared_hla$view$viewport)

  legacy_hla <- hla
  legacy_hla$page$engine <- "network"
  expect_error(
    helpers$cv_config_prepare(
      legacy_hla,
      cells = c("cell-1", "cell-2")
    ),
    "page identity is invalid"
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
test_that("empty Linked selections do not build the full cell index", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs'), assert = require('assert');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(viewer_test_path("www", "cell_views.js"), quote = '"')
      ),
      "const start = source.indexOf('  function validateWorkspace(');",
      "eval(source.slice(start, source.indexOf('\\n  function ', start + 1)));",
      "const D = {projections:{umap:{}}, groups:{cell_type:{}}};",
      "const GENE_MODE='gene', GENE_PANELS_MODE='genes', RGB_MODE='rgb';",
      "function configFingerprint(){return 'same';}",
      "function spatialSamples(){return [];}",
      "function catOf(mode){return mode==='cell_type';}",
      "function fieldForMode(){return false;}",
      "function baseOrderedSpaces(){return ['projection::umap'];}",
      "function configError(message){throw new Error(message);}",
      "let indexCalls=0;",
      "function singleIndex(){indexCalls++;return new Map([['a',0],['b',1]]);}",
      "const config={dataset:{cell_fingerprint:'same'},selection:{cells:[]},view:{",
      "projections:['umap'],spatial_sections:[],colour:{mode:'cell_type'},filters:{},lenses:[]}};",
      "validateWorkspace(config); assert.strictEqual(indexCalls,0);",
      "config.dataset.cell_fingerprint='other';",
      "assert.throws(()=>validateWorkspace(config),/different cell population/);",
      "assert.strictEqual(indexCalls,0); config.dataset.cell_fingerprint='same';",
      "config.view.projections=['missing'];",
      "assert.throws(()=>validateWorkspace(config),/projection/);",
      "config.view.projections=['umap']; config.selection.cells=['b'];",
      "assert.strictEqual(validateWorkspace(config).cells.get('b'),1);",
      "assert.strictEqual(indexCalls,1); config.selection.cells=['missing'];",
      "assert.throws(()=>validateWorkspace(config),/selects cells/);",
      "assert.strictEqual(indexCalls,2);",
      "console.log('selection index validation passed');"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_identical(output, "selection index validation passed")
})
