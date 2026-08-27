# test-coordinated-views.R — Tests for the Linked views bundle builders.
#
# cv_build_bundle() and its cv_build_* / cv_* helpers (coordinated_views/bundle.R)
# turn a loaded Cerebro object into ONE client bundle. They are pure — no
# input/output/reactive scope — so we source the file into an isolated env and
# drive it directly. Two past regressions are guarded here explicitly:
#   * a single-level group / single clonotype serialising as a JSON scalar
#     instead of an array (the I()/AsIs contract in cv_group/cv_space/cv_clone),
#     which made the client throw mid-update and keep the previous data set; and
#   * a standard `spatial` slot and a `trekker` slot collapsing into one space
#     instead of two — each builder keeps its own space identity.

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
local_inst <- inst_candidates[file.exists(file.path(
  inst_candidates,
  "viewer"
))][1]
if (!is.na(local_inst)) {
  bundle_file <- file.path(
    local_inst,
    "viewer/coordinated_views/bundle.R"
  )
  tcr_crb <- file.path(local_inst, "extdata/examples/demo_full_tcr_bcr.crb")
  trekker_crb <- file.path(local_inst, "extdata/examples/demo_trekker.crb")
} else {
  bundle_file <- system.file(
    "viewer/coordinated_views/bundle.R",
    package = "CerebroNexus"
  )
  tcr_crb <- system.file(
    "extdata/examples/demo_full_tcr_bcr.crb",
    package = "CerebroNexus"
  )
  trekker_crb <- system.file(
    "extdata/examples/demo_trekker.crb",
    package = "CerebroNexus"
  )
}

## Source the pure builders into an isolated env (no app scope). The app-only
## helpers cerebro_group_colors() / Cerebro.options are absent here; bundle.R
## guards each, so cv_build_bundle() uses its fallback palette.
##
## clone_contract.R is NOT optional and is deliberately not guarded: it defines
## what a clone is, and the app sources it before any module for the same reason
## this env has to. A bundle built without it would silently answer that question
## on its own again, which is the divergence the file exists to prevent.
contract_file <- file.path(dirname(bundle_file), "..", "clone_contract.R")
cv_env <- new.env()
have_bundle <- nzchar(bundle_file) &&
  file.exists(bundle_file) &&
  file.exists(contract_file)
if (have_bundle) {
  sys.source(contract_file, envir = cv_env)
  sys.source(bundle_file, envir = cv_env)
}

test_that("bundle.R parses and defines the builder API", {
  skip_if_not(have_bundle, "coordinated_views/bundle.R not found")
  expect_no_error(parse(file = bundle_file))
  api <- c(
    "cv_group",
    "cv_space",
    "cv_clone",
    "cv_build_groups",
    "cv_build_projections",
    "cv_build_trajectories",
    "cv_build_spatial",
    "cv_build_trekker",
    "cv_build_clone",
    "cv_build_bundle"
  )
  for (fn in api) {
    expect_true(is.function(get0(fn, envir = cv_env)), info = fn)
  }
})

test_that("trajectory coordinates and graph enter Linked views", {
  skip_if_not(have_bundle, "coordinated_views/bundle.R not found")

  trajectory <- list(
    meta = data.frame(
      DR_1 = c(10, 20),
      DR_2 = c(30, 40),
      pseudotime = c(0, 1),
      state = factor(c("1", "2")),
      row.names = c("cell_b", "cell_c")
    ),
    edges = data.frame(
      source_dim_1 = 10,
      source_dim_2 = 30,
      target_dim_1 = 20,
      target_dim_2 = 40
    )
  )
  crb <- list(
    getMethodsForTrajectories = function() "monocle2",
    getNamesOfTrajectories = function(method) {
      expect_identical(method, "monocle2")
      "B_cell_maturation"
    },
    getTrajectory = function(method, name) {
      expect_identical(method, "monocle2")
      expect_identical(name, "B_cell_maturation")
      trajectory
    }
  )

  spaces <- cv_env$cv_build_trajectories(
    crb,
    c("cell_a", "cell_b", "cell_c")
  )

  expect_length(spaces, 1)
  expect_identical(spaces[[1]]$id, "trajectory::monocle2::B_cell_maturation")
  expect_identical(spaces[[1]]$label, "B_cell_maturation (trajectory)")
  expect_equal(unclass(spaces[[1]]$x), c(NA, 10, 20))
  expect_equal(unclass(spaces[[1]]$y), c(NA, 30, 40))
  expect_true(isTRUE(spaces[[1]]$trajectory))
  expect_length(spaces[[1]]$edges, 1)
})

test_that("Linked views treats projections as a multi-panel selection", {
  ui_file <- file.path(dirname(bundle_file), "UI.R")
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  skip_if_not(file.exists(ui_file) && file.exists(js_file))

  ui <- paste(readLines(ui_file, warn = FALSE), collapse = "\n")
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  expect_match(
    ui,
    'tags\\$select\\(id = "cv-pick-proj", multiple = "multiple"\\)'
  )
  expect_match(js, "var selectedProjections = []", fixed = TRUE)
  expect_match(js, "function rebuildProjectionInstances()", fixed = TRUE)
  expect_match(js, "function setSelectedProjections(names)", fixed = TRUE)
  expect_match(js, "selectedProjections.forEach", fixed = TRUE)
  expect_match(js, "plugins: ['remove_button']", fixed = TRUE)
})

test_that("Linked views gives Colour by the shared Selectize control", {
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  skip_if_not(file.exists(js_file))
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
  expect_match(js, "function fillColorPicker()", fixed = TRUE)
  expect_match(js, "window.jQuery(sel).selectize", fixed = TRUE)
  expect_match(js, "sel.selectize.setValue(colorBy, true)", fixed = TRUE)
})

test_that("Linked views expands Colour by within the available viewport", {
  css_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.css")
  skip_if_not(file.exists(css_file))
  css <- paste(readLines(css_file, warn = FALSE), collapse = "\n")
  expect_match(
    css,
    "#cv-pick-color + .selectize-control .selectize-dropdown-content",
    fixed = TRUE
  )
  expect_match(css, "calc(100dvh - 240px)", fixed = TRUE)
})

test_that("Linked views places the shared legend above the panel grid", {
  ui_file <- file.path(dirname(bundle_file), "UI.R")
  skip_if_not(file.exists(ui_file))
  ui <- paste(readLines(ui_file, warn = FALSE), collapse = "\n")
  legend_pos <- regexpr('id = "cv-legend"', ui, fixed = TRUE)[[1]]
  cbar_pos <- regexpr('id = "cv-cbar"', ui, fixed = TRUE)[[1]]
  panes_pos <- regexpr('class = "cv-panes"', ui, fixed = TRUE)[[1]]
  expect_gt(legend_pos, 0)
  expect_gt(cbar_pos, 0)
  expect_gt(panes_pos, 0)
  expect_lt(legend_pos, panes_pos)
  expect_lt(cbar_pos, panes_pos)
})

test_that("Linked views hides its empty pane slots before data arrives", {
  ui_file <- file.path(dirname(bundle_file), "UI.R")
  skip_if_not(file.exists(ui_file))
  ui <- paste(readLines(ui_file, warn = FALSE), collapse = "\n")
  expect_match(ui, 'class = "cv-pane cv-hidden"', fixed = TRUE)
})

test_that("Linked views reflows after its workspace becomes wider", {
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  skip_if_not(file.exists(js_file))
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  expect_match(js, "resizeObserver._cvPanesObserved", fixed = TRUE)
  expect_match(js, "resizeObserver.observe(panesHost)", fixed = TRUE)
})

test_that("Linked views keeps a committed lasso through view-only zooms", {
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  skip_if_not(file.exists(js_file))
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  zoom_body <- function(name) {
    start <- regexpr(paste0("function ", name, "()"), js, fixed = TRUE)[[1]]
    expect_gt(start, 0)
    substr(js, start, start + 500L)
  }
  expect_false(grepl("clearLassos", zoom_body("toggleZoom"), fixed = TRUE))
})

test_that("Linked views chooses its grid from both viewport dimensions", {
  ui_file <- file.path(dirname(bundle_file), "UI.R")
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  skip_if_not(file.exists(ui_file) && file.exists(js_file))
  ui <- paste(readLines(ui_file, warn = FALSE), collapse = "\n")
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  expect_match(js, "function bestOverviewGrid", fixed = TRUE)
  expect_match(js, "Math.ceil(panelCount / cols)", fixed = TRUE)
  expect_match(js, "widthSide", fixed = TRUE)
  expect_match(js, "heightSide", fixed = TRUE)
  expect_match(js, "VIEWPORT_GUTTER", fixed = TRUE)
  expect_match(js, "var VIEWPORT_GUTTER = 7", fixed = TRUE)
  expect_match(ui, 'class = "cv-secondary-analysis"', fixed = TRUE)
  expect_match(
    ui,
    'class = "cv-secondary-analysis",\n      style = "display:none"'
  )
})

test_that("Linked views keeps replacement controls contextual and user-facing", {
  ui_file <- file.path(dirname(bundle_file), "UI.R")
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  server_file <- file.path(dirname(bundle_file), "server.R")
  skip_if_not(
    file.exists(ui_file) && file.exists(js_file) && file.exists(server_file)
  )

  ui <- paste(readLines(ui_file, warn = FALSE), collapse = "\n")
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
  server <- paste(readLines(server_file, warn = FALSE), collapse = "\n")

  expect_no_match(ui, "cv-trekker-morph", fixed = TRUE)
  expect_no_match(js, "function setTrekkerMorph", fixed = TRUE)
  expect_no_match(js, "function playTrekkerMorph", fixed = TRUE)
  expect_no_match(server, "cv-img-copy", fixed = TRUE)
  expect_no_match(js, "function copyImgPreset", fixed = TRUE)
  expect_match(ui, "cv-moran-badge", fixed = TRUE)
  expect_match(js, "function updateMoranBadges", fixed = TRUE)
  expect_match(js, "function fieldSummaryHtml", fixed = TRUE)
})

test_that("Trekker depth views form one collapsed insights region", {
  ui_file <- file.path(dirname(bundle_file), "UI.R")
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  css_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.css")
  skip_if_not(
    file.exists(ui_file) && file.exists(js_file) && file.exists(css_file)
  )

  ui <- paste(readLines(ui_file, warn = FALSE), collapse = "\n")
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
  css <- paste(readLines(css_file, warn = FALSE), collapse = "\n")

  expect_match(ui, 'id = "cv-tk-insights"', fixed = TRUE)
  expect_match(ui, 'id = "cv-tk-insights-toggle"', fixed = TRUE)
  expect_match(ui, 'id = "cv-tk-tab-cell"', fixed = TRUE)
  expect_match(ui, 'id = "cv-tk-tab-qc"', fixed = TRUE)
  expect_match(ui, 'id = "cv-tk-tab-moran"', fixed = TRUE)
  expect_match(ui, 'id = "cv-tk-panel-stage"', fixed = TRUE)
  expect_match(ui, 'id = "cv-tk-cell-body"', fixed = TRUE)
  expect_no_match(ui, 'id = "cv-tk-modal"', fixed = TRUE)

  expect_match(js, "function openTrekkerInsights", fixed = TRUE)
  expect_match(js, "function selectTrekkerInsight", fixed = TRUE)
  expect_match(js, "function animateTrekkerInsight", fixed = TRUE)
  expect_match(js, "function trekkerScrollHost", fixed = TRUE)
  expect_match(js, "function fillTrekkerInsights", fixed = TRUE)
  expect_match(js, "cv-tk-cell-block--position", fixed = TRUE)
  expect_match(js, "cv-tk-cell-block--evidence", fixed = TRUE)
  expect_match(js, "cv-tk-cell-block--metadata", fixed = TRUE)

  expect_match(css, "#cv-tk-cell-body > .cv-tk-cell-block", fixed = TRUE)
  expect_match(
    css,
    "grid-template-columns: repeat(4, minmax(0, 1fr))",
    fixed = TRUE
  )
  expect_match(css, ".cv-tk-panel-stage.is-switching", fixed = TRUE)
})

test_that("cv_group/cv_space/cv_clone force JSON arrays even at length 1", {
  skip_if_not(have_bundle)
  skip_if_not_installed("jsonlite")
  arr <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  }

  ## A single-level group: values/levels/colors must all still be JSON arrays.
  g <- cv_env$cv_group(0L, "OnlyLevel", "#123456")
  expect_s3_class(g$levels, "AsIs")
  expect_s3_class(g$colors, "AsIs")
  expect_match(arr(g$levels), "^\\[")
  expect_match(arr(g$colors), "^\\[")
  expect_match(arr(g$values), "^\\[")

  ## A single-cell space: x/y are arrays, id/label stay scalar.
  s <- cv_env$cv_space("umap", "x (expression)", 1, 2)
  expect_match(arr(s$x), "^\\[")
  expect_match(arr(s$y), "^\\[")

  ## A single-clonotype bundle: id/label/size are arrays; the true scalars
  ## (n_clones / n_receptor) stay bare so the client reads them as numbers.
  cl <- cv_env$cv_clone(0L, "CASSLGX", 1L, 1L, 1L)
  expect_match(arr(cl$size), "^\\[")
  expect_match(arr(cl$label), "^\\[")
  expect_false(startsWith(arr(cl$n_clones), "["))
})

test_that("cv_color_patch carries only palette updates for the current bundle", {
  skip_if_not(have_bundle)
  bundle <- list(
    dataset_id = "example.crb",
    groups = list(
      cluster = cv_env$cv_group(c(0L, 1L), c("A", "B"), c("#aaaaaa", "#bbbbbb"))
    ),
    cat_extra = list(
      donor = cv_env$cv_group(c(0L, 1L), c("d1", "d2"), c("#cccccc", "#dddddd"))
    )
  )

  patch <- cv_env$cv_color_patch(
    bundle,
    list(cluster = c(A = "#111111", B = "#222222"))
  )

  expect_named(patch, c("dataset_id", "groups", "cat_extra"))
  expect_identical(patch$dataset_id, "example.crb")
  expect_equal(as.character(patch$groups$cluster), c("#111111", "#222222"))
  expect_equal(as.character(patch$cat_extra$donor), c("#cccccc", "#dddddd"))
  expect_s3_class(patch$groups$cluster, "AsIs")
})

## A minimal but realistically-shaped IR table: the CT* columns spell out chain
## names, because that is what the receptor scoping reads. c1/c2 share a CTgene
## clone that CTstrict splits in two -- the exact case where reading the wrong
## column changes the answer rather than just the formatting.
cv_ir_fixture <- function() {
  list(data.frame(
    barcode = c("c1", "c2", "c3", "b1"),
    CTgene = c(
      "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      "TRAV12-1.TRAJ20.TRAC_TRBV20-1.TRBJ1-2.TRBC1",
      "IGHV3-23.IGHJ4.IGHM_IGKV1-5.IGKJ1.IGKC"
    ),
    CTstrict = c(
      "TRAV1-2.TRAJ33.TRAC_CAVMDSNYQLIW_TRBV6-4.TRBJ2-1.TRBC2_CASSAAA",
      "TRAV1-2.TRAJ33.TRAC_CAVMDSNYQLIW_TRBV6-4.TRBJ2-1.TRBC2_CASSBBB",
      "TRAV12-1.TRAJ20.TRAC_CAVXX_TRBV20-1.TRBJ1-2.TRBC1_CASSCCC",
      "IGHV3-23.IGHJ4.IGHM_CARDXX_IGKV1-5.IGKJ1.IGKC_CQQYX"
    ),
    CTaa = c("CAVM_CASSAAA", "CAVM_CASSBBB", "CAVX_CASSCCC", "CARD_CQQY"),
    stringsAsFactors = FALSE
  ))
}

test_that("cv_clone_per_cell aligns clone identity to cells, NA when unmatched", {
  skip_if_not(have_bundle)
  ir <- cv_ir_fixture()
  cells <- c("c3", "c1", "cX") # cX carries no receptor
  out <- cv_env$cv_clone_per_cell(ir, cells)
  expect_equal(
    out$clone,
    c(
      "TRAV12-1.TRAJ20.TRAC_TRBV20-1.TRBJ1-2.TRBC1",
      "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      NA
    )
  )
  expect_equal(out$ctaa, c("CAVX_CASSCCC", "CAVM_CASSAAA", NA))
  ## No IR at all -> NULL (the immune axis is simply skipped).
  expect_null(cv_env$cv_clone_per_cell(NULL, cells))
})

test_that("cv_clone_per_cell calls clones the way the Clonal UMAP does", {
  skip_if_not(have_bundle)
  ir <- cv_ir_fixture()
  cells <- c("c1", "c2", "c3")
  out <- cv_env$cv_clone_per_cell(ir, cells)
  ## c1 and c2 are ONE clone by CTgene and TWO by CTstrict. Reading the stricter
  ## column here reported more, smaller clones than the Clonal UMAP did for the
  ## same cells -- a different scientific answer under the same word.
  expect_equal(out$clone[1], out$clone[2])
  expect_equal(length(unique(out$clone)), 2)
})

test_that("cv_clone_per_cell keeps to one receptor class", {
  skip_if_not(have_bundle)
  ir <- cv_ir_fixture()
  ## b1 is a B cell. Mixing it in would put BCR and TCR clonotypes in one ranking
  ## and one expansion legend, which no page in the app does.
  out <- cv_env$cv_clone_per_cell(ir, c("c1", "b1"))
  expect_equal(out$receptor, "TCR")
  expect_false(is.na(out$clone[1]))
  expect_true(is.na(out$clone[2]))
  ## ... and asking for the other class gives its cells instead.
  bcr <- cv_env$cv_clone_per_cell(ir, c("c1", "b1"), receptor = "BCR")
  expect_true(is.na(bcr$clone[1]))
  expect_false(is.na(bcr$clone[2]))
})

test_that("both clone pages read one definition of a clone", {
  skip_if_not(have_bundle)
  ## The contract itself.
  expect_equal(cv_env$CEREBRO_CLONE_BINS, c(0, 1, 5, 20, 100, Inf))
  expect_equal(length(cv_env$CEREBRO_CLONE_LABELS), 5)
  expect_match(cv_env$CEREBRO_CLONE_LABELS[5], "Hyperexpanded")
  expect_equal(cv_env$cerebro_clonecall_col(), "CTgene")

  ## And that neither page has quietly gone back to declaring its own. This is a
  ## source check because the divergence was not a wrong value anywhere -- both
  ## pages were self-consistent -- but two right-looking definitions of one term.
  ir_data <- file.path(
    dirname(bundle_file),
    "..",
    "immune_repertoire",
    "data.R"
  )
  skip_if_not(file.exists(ir_data))
  txt <- paste(readLines(ir_data, warn = FALSE), collapse = "\n")
  expect_match(txt, "IR_CLONE_BINS <- CEREBRO_CLONE_BINS", fixed = TRUE)
  expect_match(txt, "IR_CLONE_LABELS <- CEREBRO_CLONE_LABELS", fixed = TRUE)

  bundle_txt <- paste(readLines(bundle_file, warn = FALSE), collapse = "\n")
  expect_false(grepl("breaks = c(0, 1, 5, 20", bundle_txt, fixed = TRUE))
  expect_match(bundle_txt, "cerebro_clone_expansion(", fixed = TRUE)
})

test_that("cv_build_fields turns every numeric meta column into a colouring", {
  skip_if_not(have_bundle)
  md <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    cluster = c("a", "b", "a"),
    nUMI = c(100, 200, 300),
    percent.mt = c(1.5, NA, 3.5),
    constant = c(7, 7, 7),
    stringsAsFactors = FALSE
  )
  f <- cv_env$cv_build_fields(md)
  ## QC-looking columns are exactly what users colour by — they must be offered.
  expect_true("meta:nUMI" %in% names(f))
  expect_true("meta:percent.mt" %in% names(f))
  ## no colouring can be built from a constant column or a non-numeric one
  expect_false("meta:constant" %in% names(f))
  expect_false("meta:cluster" %in% names(f))
  expect_false("meta:cell_barcode" %in% names(f))
  ## the quantised vector is aligned to the rows, spans the full scale, and
  ## carries the true range so the client can show real values, not 0-255.
  fu <- f[["meta:nUMI"]]
  expect_length(fu$v, nrow(md))
  expect_equal(as.integer(fu$v), c(0L, fu$scale %/% 2L, fu$scale))
  expect_equal(fu$min, 100)
  expect_equal(fu$max, 300)
  ## NA stays NA so the client can draw it as "no value" rather than as zero.
  expect_true(is.na(f[["meta:percent.mt"]]$v[2]))
})

test_that("cv_build_extra_groups covers categorical columns that are not groups", {
  skip_if_not(have_bundle)
  md <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    cluster = c("a", "b", "a"),
    dextramer_allele = c("A*02:01", "A*02:01", "B*07:02"),
    stringsAsFactors = FALSE
  )
  eg <- cv_env$cv_build_extra_groups(md, "cluster", function(g, lev) {
    cv_env$cv_colors_for(lev)
  })
  ## registered groups stay out (they are already offered, and they own the
  ## group filters); unregistered categorical columns come in.
  expect_false("cluster" %in% names(eg$groups))
  expect_true("dextramer_allele" %in% names(eg$groups))
  expect_equal(
    as.character(eg$groups$dextramer_allele$levels),
    c("A*02:01", "B*07:02")
  )
  expect_equal(as.integer(eg$groups$dextramer_allele$values), c(0L, 0L, 1L))
  expect_length(eg$skipped, 0)

  ## A column with as many levels as cells is an identifier, not a grouping —
  ## not colourable, but REPORTED rather than dropped, so the picker can say why
  ## a column the Projection tab offers is missing here.
  md$barcode_copy <- md$cell_barcode
  eg2 <- cv_env$cv_build_extra_groups(md, "cluster", function(g, lev) {
    cv_env$cv_colors_for(lev)
  })
  expect_false("barcode_copy" %in% names(eg2$groups))
  expect_true("barcode_copy" %in% names(eg2$skipped))
  expect_equal(eg2$skipped$barcode_copy, 3L)
})

test_that("cv_build_projections records dimensionality instead of dropping it", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  crb <- list(
    availableProjections = function() c("umap", "umap_3D"),
    getProjection = function(n) {
      m <- if (n == "umap_3D") {
        matrix(1:6, nrow = 2, dimnames = list(cells, c("x", "y", "z")))
      } else {
        matrix(1:4, nrow = 2, dimnames = list(cells, c("x", "y")))
      }
      m
    }
  )
  pj <- cv_env$cv_build_projections(crb, cells)
  expect_equal(pj$umap$ndim, 2L)
  expect_equal(pj$umap_3D$ndim, 3L)
  ## The third dimension travels so the client can rotate the embedding rather
  ## than show a flattened shadow of it. A 2-D projection must NOT carry a z —
  ## the client uses its presence to decide which panels can be turned.
  expect_null(pj$umap$z)
  expect_length(pj$umap_3D$z, length(cells))
  expect_equal(as.numeric(pj$umap_3D$z), c(5, 6))
  ## and it is array-wrapped, like every other per-cell vector in the bundle
  expect_s3_class(pj$umap_3D$z, "AsIs")
})

test_that("cv_build_bundle still works when no grouping variable is registered", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2", "c3")
  md <- data.frame(
    cell_barcode = cells,
    nUMI = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() md,
    getGroups = function() character(0),
    availableProjections = function() "umap",
    getProjection = function(n) {
      matrix(1:6, nrow = 3, dimnames = list(cells, c("x", "y")))
    },
    availableSpatial = function() NULL,
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL
  )
  b <- cv_env$cv_build_bundle(crb)
  ## Projection colours cells by ANY meta column, so an object with no
  ## registered group is perfectly usable there — Linked views must not go blank.
  expect_type(b, "list")
  expect_equal(b$n, 3L)
  expect_true("meta:nUMI" %in% names(b$fields))
  ## with nothing categorical to fall back on, the default colouring is the
  ## first continuous field, expressed as the client's field mode string.
  expect_equal(b$default_group, paste0(cv_env$cv_field_mode, "meta:nUMI"))
})

test_that("Linked views prefers cell type while consuming Viewer defaults", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2", "c3")
  md <- data.frame(
    cell_barcode = cells,
    cell_type = c("A", "B", "A"),
    region = c("R1", "R1", "R2"),
    row.names = cells,
    stringsAsFactors = FALSE
  )
  crb <- list(
    getMetaData = function() md,
    getGroups = function() c("cell_type", "region"),
    getParameters = function() list(main_group = "region"),
    availableProjections = function() c("umap", "tsne"),
    getProjection = function(name) {
      offset <- if (identical(name, "tsne")) 10 else 0
      matrix(
        seq_len(6) + offset,
        nrow = 3,
        dimnames = list(cells, c("x", "y"))
      )
    },
    availableSpatial = function() character(),
    getTrekker = function() NULL,
    getImmuneRepertoire = function() NULL
  )
  cv_env$Cerebro.options <- list(
    viewer_content = list(
      ds = list(
        default_projection = "tsne",
        default_trajectory = NULL,
        overview_point_size = 5,
        overview_percentage_cells_to_show = 60
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  for (point_size in c(0, 5, 20)) {
    cv_env$Cerebro.options$viewer_content$ds$overview_point_size <- point_size
    bundle <- cv_env$cv_build_bundle(crb)
    expect_identical(bundle$default_projection, "tsne")
    expect_identical(bundle$default_group, "cell_type")
    expect_identical(
      bundle$default_point_size,
      point_size,
      info = paste("point size", point_size)
    )
    expect_identical(bundle$default_percentage_cells_to_show, 60)
  }
})

test_that("spatial and Trekker spaces do not require an expression projection", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  md <- data.frame(cell_barcode = cells, row.names = cells)
  base <- list(
    getMetaData = function() md,
    getGroups = function() character(),
    availableProjections = function() character(),
    getProjection = function(name) NULL,
    getImmuneRepertoire = function() NULL
  )

  spatial_crb <- base
  spatial_crb$availableSpatial <- function() "slice-a"
  spatial_crb$getSpatialData <- function(name) {
    list(
      coordinates = data.frame(
        x = c(1, 2),
        y = c(3, 4),
        row.names = cells
      )
    )
  }
  spatial_crb$getTrekker <- function() NULL

  spatial_bundle <- cv_env$cv_build_bundle(spatial_crb)
  expect_type(spatial_bundle, "list")
  expect_length(spatial_bundle$projections, 0L)
  expect_null(spatial_bundle$default_projection)
  expect_identical(
    vapply(spatial_bundle$spaces, `[[`, character(1), "id"),
    "spatial"
  )

  trekker_crb <- base
  trekker_crb$availableSpatial <- function() character()
  trekker_crb$getSpatialData <- function(name) NULL
  trekker_crb$getTrekker <- function() {
    list(
      x = c(10, 20),
      y = c(30, 40),
      barcodes = cells,
      fields = list(),
      evidence = list(),
      qc = NULL,
      moran = NULL
    )
  }

  trekker_bundle <- cv_env$cv_build_bundle(trekker_crb)
  expect_type(trekker_bundle, "list")
  expect_length(trekker_bundle$projections, 0L)
  expect_null(trekker_bundle$default_projection)
  expect_identical(
    vapply(trekker_bundle$spaces, `[[`, character(1), "id"),
    "trekker"
  )
})

test_that("Builder Trekker backgrounds and appearance reach Linked views", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  md <- data.frame(cell_barcode = cells, row.names = cells)
  crb <- list(
    getMetaData = function() md,
    getGroups = function() character(),
    getParameters = function() list(),
    availableProjections = function() character(),
    availableSpatial = function() character(),
    getImmuneRepertoire = function() NULL,
    getTrekker = function() {
      list(
        x = c(10, 20),
        y = c(30, 40),
        barcodes = cells,
        fields = list(),
        evidence = list(),
        qc = NULL,
        moran = NULL,
        histology_image = "data:image/png;base64,AA==",
        histology_image_bounds = c(
          xmin = 0,
          xmax = 30,
          ymin = 20,
          ymax = 50
        ),
        histology_alignment = list(
          source = "trekker.png",
          image_opacity = 0.7,
          point_opacity = 0.65,
          point_size = 9
        )
      )
    }
  )

  cv_env$Cerebro.options <- list(
    viewer_content = list(
      ds = list(overview_point_size = 5)
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  bundle <- cv_env$cv_build_bundle(crb)
  trekker <- bundle$spaces[[which(
    vapply(
      bundle$spaces,
      `[[`,
      character(1),
      "id"
    ) ==
      "trekker"
  )]]
  expect_length(trekker$images, 1L)
  expect_identical(trekker$images[[1L]]$label, "trekker.png")
  expect_identical(
    trekker$images[[1L]]$uri,
    "data:image/png;base64,AA=="
  )
  expect_identical(trekker$images[[1L]]$preset$opacity, 0.7)
  expect_identical(trekker$background_scope, "Trekker")
  ## Linked views keeps Overview as the shared-control seed while each spatial
  ## space carries its own Builder appearance until the user changes that
  ## shared control.
  expect_identical(bundle$default_point_size, 5)
  expect_identical(trekker$builder_point_opacity, 0.65)
  expect_identical(trekker$builder_point_size, 9)

  js <- paste(
    readLines(file.path(dirname(bundle_file), "..", "www", "coordviews.js")),
    collapse = "\n"
  )
  expect_match(js, "function pointSizeOf(p)", fixed = TRUE)
  expect_match(js, "function pointOpacityOf(p)", fixed = TRUE)
})

test_that("a clone's label names its dominant CDR3 and says how many it hides", {
  skip_if_not(have_bundle)
  ## A clone is called on CTgene, and one CTgene clone routinely covers several
  ## CDR3s in realistic receptor data can do this. Naming the row after
  ## whichever CDR3 came first presented one sequence while the row selected
  ## cells carrying the others, under a column header that read "CDR3".
  ir <- list(data.frame(
    barcode = c("c1", "c2", "c3", "c4"),
    CTgene = rep("TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2", 4),
    CTstrict = paste0(
      "TRAV1-2.TRAJ33.TRAC_CAV",
      c("A", "B", "B", "B"),
      "_TRBV6-4.TRBJ2-1.TRBC2"
    ),
    ## "CASSB" is carried by three of the four cells; "CASSA" by one, and it
    ## comes first, which is exactly what the old label picked.
    CTaa = c("CASSA", "CASSB", "CASSB", "CASSB"),
    stringsAsFactors = FALSE
  ))
  crb <- list(getImmuneRepertoire = function() ir)
  cells <- c("c1", "c2", "c3", "c4")
  out <- cv_env$cv_build_clone(crb, cells, length(cells))

  expect_false(is.null(out))
  expect_equal(out$bundle$n_clones, 1)
  expect_equal(as.character(out$bundle$label[1]), "CASSB")
  expect_equal(as.integer(out$bundle$n_cdr3[1]), 2L)
})

test_that("a single-CDR3 clone is not annotated", {
  skip_if_not(have_bundle)
  ir <- list(data.frame(
    barcode = c("c1", "c2"),
    CTgene = rep("TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2", 2),
    CTstrict = rep("TRAV1-2.TRAJ33.TRAC_CAVX_TRBV6-4.TRBJ2-1.TRBC2", 2),
    CTaa = c("CASSX", "CASSX"),
    stringsAsFactors = FALSE
  ))
  crb <- list(getImmuneRepertoire = function() ir)
  out <- cv_env$cv_build_clone(crb, c("c1", "c2"), 2)
  expect_equal(as.character(out$bundle$label[1]), "CASSX")
  expect_equal(as.integer(out$bundle$n_cdr3[1]), 1L)
})

## The divergences that clone_contract.R exists to prevent were never visible to
## a test of either page on its own: each was self-consistent. This one computes
## a clone's size the way each page does, on the same object, and compares -- the
## only shape of test that can fail when the two drift apart again.
test_that("both pages give a clone the same size", {
  skip_if_not(have_bundle)
  skip_if_not(nzchar(tcr_crb) && file.exists(tcr_crb))

  crb <- readRDS(tcr_crb)
  md <- crb$getMetaData()
  cells <- as.character(md$cell_barcode)
  ir <- crb$getImmuneRepertoire()

  ## Linked views: align to the object's cells, then count.
  cp <- cv_env$cv_clone_per_cell(ir, cells)
  cv_sizes <- table(cp$clone[!is.na(cp$clone)])

  ## Immune repertoire: the same contract, applied independently -- one row per
  ## barcode, receptor-scoped, restricted to cells the object has, then counted.
  clone_col <- cv_env$cerebro_clonecall_col()
  receptor <- cv_env$cerebro_receptors_present(ir)[1]
  rows <- do.call(
    rbind,
    lapply(ir, function(df) {
      keep <- cv_env$cerebro_rows_in_receptor(df, receptor, clone_col)
      df <- df[keep, , drop = FALSE]
      if (!nrow(df)) {
        return(NULL)
      }
      data.frame(
        barcode = as.character(df$barcode),
        clone = as.character(df[[clone_col]]),
        stringsAsFactors = FALSE
      )
    })
  )
  rows <- rows[!is.na(rows$clone) & nzchar(rows$clone), , drop = FALSE]
  rows <- rows[!duplicated(rows$barcode), , drop = FALSE]
  rows <- rows[rows$barcode %in% cells, , drop = FALSE]
  ir_sizes <- table(rows$clone)

  expect_equal(length(cv_sizes), length(ir_sizes))
  expect_setequal(names(cv_sizes), names(ir_sizes))
  expect_equal(
    as.integer(cv_sizes[names(ir_sizes)]),
    as.integer(ir_sizes)
  )

  ## ... and therefore the same expansion level for every clone.
  expect_equal(
    as.character(cv_env$cerebro_clone_expansion(as.integer(cv_sizes))),
    as.character(cv_env$cerebro_clone_expansion(
      as.integer(ir_sizes[names(cv_sizes)])
    ))
  )
})

test_that("receptor detection does not stop at the third sample", {
  skip_if_not(have_bundle)
  ## detect_chains() samples the first three list entries, which is fine for
  ## guessing a default gene family and wrong for deciding which receptor a page
  ## defaults to: a data set whose FOURTH sample is the only one carrying TCR
  ## would have shown BCR on one page and TCR on the other.
  bcr <- function(i) {
    data.frame(
      barcode = paste0("b", i),
      CTgene = "IGHV3-23.IGHJ4.IGHM_IGKV1-5.IGKJ1.IGKC",
      CTstrict = "IGHV3-23.IGHJ4.IGHM_CARDX_IGKV1-5.IGKJ1.IGKC_CQQY",
      CTaa = "CARD_CQQY",
      stringsAsFactors = FALSE
    )
  }
  ir <- list(
    bcr(1),
    bcr(2),
    bcr(3),
    data.frame(
      barcode = "t1",
      CTgene = "TRAV1-2.TRAJ33.TRAC_TRBV6-4.TRBJ2-1.TRBC2",
      CTstrict = "TRAV1-2.TRAJ33.TRAC_CAVX_TRBV6-4.TRBJ2-1.TRBC2_CASSX",
      CTaa = "CAVX_CASSX",
      stringsAsFactors = FALSE
    )
  )
  expect_setequal(cv_env$cerebro_receptors_present(ir), c("TCR", "BCR"))
})

test_that("the histology bar offers both scale axes and a way back", {
  ## A preset can be non-uniform. A single Scale control had to collapse the two
  ## axes into one number, and every other control in the bar then rewrote the
  ## pair from it -- so a nudge to the opacity squared the image up. Pinned at
  ## the source because the bar only renders for a data set carrying an image,
  ## which no test app loads.
  path <- file.path(
    dirname(bundle_file),
    "server.R"
  )
  skip_if_not(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(txt, "cv-img-scalex", fixed = TRUE)
  expect_match(txt, "cv-img-scaley", fixed = TRUE)
  expect_match(txt, "cv-img-lock", fixed = TRUE)
  expect_match(txt, "cv-img-reset", fixed = TRUE)
  expect_no_match(txt, "\"cv-img-scale\"")
})


## The card's Positioning section is only as good as what the builder actually
## ships. It used to be tested against a hand-written payload, which is how it
## came to read a `fields.bead_noise` that no builder has ever produced while
## printing position confidence twice -- once from `conf`, once from the field of
## the same name. These assertions are on the real output for the real object.
test_that("the trekker bundle carries what a placement is judged on", {
  skip_if_not(have_bundle)
  skip_if_not(file.exists(trekker_crb))

  b <- cv_env$cv_build_bundle(readRDS(trekker_crb))
  expect_false(is.null(b$trekker))

  ## position_confidence is a FIELD -- a colouring -- so the card reads it from
  ## there. `conf` remains the bare vector the dissolve slider indexes.
  expect_true("position_confidence" %in% names(b$fields))
  expect_false(is.null(b$trekker$conf))
  purity <- b$fields$spatial_purity
  expect_identical(purity$source, "trekker")
  expect_true(nzchar(purity$desc))
  expect_true(length(purity$by_type) > 0)
  expect_true(all(vapply(
    purity$by_type,
    function(x) !is.null(x$type) && !is.null(x$median),
    logical(1)
  )))

  ## The two numbers the dedicated page prints beside confidence, which say
  ## whether that confidence is worth anything, now travel with it.
  expect_false(is.null(b$trekker$conf_noise))
  expect_false(is.null(b$trekker$conf_sb))
  expect_equal(length(b$trekker$conf_noise), b$n)
  expect_equal(length(b$trekker$conf_sb), b$n)

  ## Evidence is not just a ring: the detail card must be able to explain why a
  ## nucleus was placed there without sending the user back to the old page.
  expect_equal(length(b$trekker$evidence_img), b$n)
  evidence <- Filter(Negate(is.null), unclass(b$trekker$evidence_img))
  expect_true(length(evidence) > 0)
  expect_true(all(startsWith(unlist(evidence), "data:image/")))

  ## No field is invented: everything the card lists as Trekker's own is a key
  ## the builder produced.
  tk_fields <- Filter(function(k) !startsWith(k, "meta:"), names(b$fields))
  expect_true(length(tk_fields) > 0)
  expect_false("bead_noise" %in% tk_fields)
})

test_that("the histology bar exists when any section carries an image", {
  ## A space's own `image` is its FIRST sample's. A data set whose first section
  ## has no histology therefore rendered no bar at all, and switching to a
  ## section that does have one revealed an empty box. Pinned at the source: the
  ## bar only renders for a data set with an image, which no test app loads.
  path <- file.path(dirname(bundle_file), "server.R")
  skip_if_not(file.exists(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(
    txt,
    "for \\(smp in \\(s\\$samples[\\s\\S]{0,120}smp\\$image",
    perl = TRUE
  )
})

test_that("each section offers only its own configured backgrounds", {
  skip_if_not(have_bundle)
  ## Embedded and external used to be exclusive -- an object carrying its own
  ## histology silently dropped whatever the deployment had configured -- and
  ## only the FIRST configured file was read. Two files of the same basename
  ## also have to stay apart, so the id cannot be the basename alone.
  tmp <- file.path(tempdir(), "cv_imgs")
  dir.create(
    file.path(tmp, "spatial-assets", "a"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(tmp, "spatial-assets", "b"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  png <- file.path(tmp, "spatial-assets", "a", "he.png")
  png2 <- file.path(tmp, "spatial-assets", "b", "he.png")
  ## A real 1x1 PNG written byte-wise: no graphics device needed, so this does
  ## not depend on one being available in the check environment.
  px <- as.raw(c(
    0x89,
    0x50,
    0x4e,
    0x47,
    0x0d,
    0x0a,
    0x1a,
    0x0a,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x48,
    0x44,
    0x52,
    0x00,
    0x00,
    0x00,
    0x01,
    0x00,
    0x00,
    0x00,
    0x01,
    0x08,
    0x06,
    0x00,
    0x00,
    0x00,
    0x1f,
    0x15,
    0xc4,
    0x89,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x44,
    0x41,
    0x54,
    0x78,
    0x9c,
    0x63,
    0xf8,
    0xcf,
    0xc0,
    0x50,
    0x0f,
    0x00,
    0x04,
    0x85,
    0x01,
    0x80,
    0x84,
    0xa9,
    0x8c,
    0x21,
    0x00,
    0x00,
    0x00,
    0x00,
    0x49,
    0x45,
    0x4e,
    0x44,
    0xae,
    0x42,
    0x60,
    0x82
  ))
  writeBin(px, png)
  writeBin(px, png2)
  skip_if_not(file.exists(png) && file.exists(png2))
  skip_if_not_installed("base64enc")

  ## The builders read these two app-scope objects when they exist; this is the
  ## same shape the running app provides.
  cv_env$Cerebro.options <- list(
    cerebro_root = tmp,
    spatial_images = list(
      ds = list(
        `section-a` = list(
          `H&E` = list(
            path = "spatial-assets/a/he.png",
            bounds = c(xmin = 1, xmax = 11, ymin = 2, ymax = 12)
          )
        ),
        `section-b` = c(`H&E` = "spatial-assets/b/he.png")
      )
    ),
    spatial_image_settings = list(
      ds = list(
        `section-a` = list(
          `H&E` = list(
            offset_x = 4,
            offset_y = -3,
            scale_x = 1.5,
            scale_y = 0.75,
            flip_x = TRUE,
            flip_y = FALSE,
            rotation = 90,
            image_opacity = 0.6,
            point_opacity = 0.55,
            point_size = 8
          )
        )
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  first <- cv_env$cv_external_images("section-a")
  second <- cv_env$cv_external_images("section-b")
  expect_length(first, 1L)
  expect_length(second, 1L)
  expect_identical(first[[1L]]$label, "H&E")
  expect_identical(second[[1L]]$label, "H&E")
  expect_false(identical(first[[1L]]$id, second[[1L]]$id))
  expect_equal(
    unlist(first[[1L]]$bounds, use.names = TRUE),
    c(xmin = 1, xmax = 11, ymin = 2, ymax = 12)
  )
  expect_identical(
    first[[1L]]$preset,
    list(
      offsetX = 4,
      offsetY = -3,
      scaleX = 1.5,
      scaleY = 0.75,
      flipX = TRUE,
      flipY = FALSE,
      rotation = 90,
      opacity = 0.6,
      pointOpacity = 0.55,
      pointSize = 8
    )
  )

  js <- paste(
    readLines(file.path(dirname(bundle_file), "..", "www", "coordviews.js")),
    collapse = "\n"
  )
  expect_match(js, "pr.rotation != null ? pr.rotation : 0", fixed = TRUE)
  expect_match(js, "c.rotate(-state.rotate * Math.PI / 180)", fixed = TRUE)
})

test_that("per-image settings also apply to embedded backgrounds", {
  skip_if_not(have_bundle)
  cells <- c("c1", "c2")
  crb <- list(getSpatialData = function(name) {
    list(
      coordinates = data.frame(
        x = c(1, 2),
        y = c(3, 4),
        row.names = cells
      ),
      histology_images = list(
        Embedded = list(
          histology_image = "data:image/png;base64,AA==",
          histology_image_bounds = c(
            xmin = 0,
            xmax = 3,
            ymin = 0,
            ymax = 5
          ),
          histology_alignment = list(
            source = "Embedded",
            dx = -4,
            dy = 6,
            scale = 1.25,
            rotation = -32,
            flip_x = TRUE,
            flip_y = FALSE,
            image_opacity = 0.7,
            point_opacity = 0.35,
            point_size = 9
          )
        )
      ),
      histology_alignment = list(
        source = "Embedded",
        image_opacity = 0.7,
        point_opacity = 0.8,
        point_size = 5
      )
    )
  })
  cv_env$Cerebro.options <- list(
    spatial_image_settings = list(
      ds = list(
        fov = list(
          Embedded = list(offset_x = 2, flip_y = TRUE, rotation = 45)
        )
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  built <- cv_env$cv_spatial_one(crb, cells, "fov", allow_external = TRUE)
  expect_identical(
    built$images[[1L]]$preset,
    list(
      offsetX = -4,
      offsetY = 6,
      scaleX = 1.25,
      scaleY = 1.25,
      flipX = TRUE,
      flipY = FALSE,
      rotation = -32,
      opacity = 0.7,
      pointOpacity = 0.35,
      pointSize = 9,
      geometryBaked = TRUE
    )
  )
  js <- paste(
    readLines(file.path(dirname(bundle_file), "..", "www", "coordviews.js")),
    collapse = "\n"
  )
  expect_match(js, "function imageRenderState(img, state)", fixed = TRUE)
  expect_match(js, "if (!pr.geometryBaked) return state;", fixed = TRUE)
})

test_that("the alignment bar follows the chosen background, not the data set", {
  ## With "None" chosen there is nothing on screen for those controls to adjust,
  ## so the bar goes away with the image. Keyed on the CURRENT choice rather than
  ## on whether the data set has an image at all, which is what it used to ask.
  js <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  skip_if_not(file.exists(js))
  txt <- paste(readLines(js, warn = FALSE), collapse = "\n")
  expect_match(
    txt,
    "hasImg = !!\\(sp && currentImage\\(sp\\)\\)",
    perl = TRUE
  )
  expect_no_match(txt, "hasImg = D.spaces.some", fixed = TRUE)
})

test_that("bundling two images of the same basename keeps both", {
  ## The ids kept them apart in the bundle, but createShinyApp() copied both to
  ## data/<basename> and let the second overwrite the first, so two entries in
  ## the picker resolved to one file. The earlier test called the builder on the
  ## original absolute paths and never went through the packaging that breaks it.
  tmp <- file.path(tempdir(), "cv_pack")
  unlink(tmp, recursive = TRUE)
  dir.create(file.path(tmp, "a"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "b"), recursive = TRUE, showWarnings = FALSE)
  px <- as.raw(c(
    0x89,
    0x50,
    0x4e,
    0x47,
    0x0d,
    0x0a,
    0x1a,
    0x0a,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x48,
    0x44,
    0x52,
    0x00,
    0x00,
    0x00,
    0x01,
    0x00,
    0x00,
    0x00,
    0x01,
    0x08,
    0x06,
    0x00,
    0x00,
    0x00,
    0x1f,
    0x15,
    0xc4,
    0x89,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x44,
    0x41,
    0x54,
    0x78,
    0x9c,
    0x63,
    0xf8,
    0xcf,
    0xc0,
    0x50,
    0x0f,
    0x00,
    0x04,
    0x85,
    0x01,
    0x80,
    0x84,
    0xa9,
    0x8c,
    0x21,
    0x00,
    0x00,
    0x00,
    0x00,
    0x49,
    0x45,
    0x4e,
    0x44,
    0xae,
    0x42,
    0x60,
    0x82
  ))
  p1 <- file.path(tmp, "a", "he.png")
  p2 <- file.path(tmp, "b", "he.png")
  writeBin(px, p1)
  writeBin(px, p2)

  example <- system.file(
    "extdata/examples/demo_spatial_visium.crb",
    package = "CerebroNexus"
  )
  skip_if_not(nzchar(example))
  crb_one <- file.path(tmp, "demo-one.crb")
  crb_two <- file.path(tmp, "demo-two.crb")
  file.copy(example, crb_one, overwrite = TRUE)
  file.copy(example, crb_two, overwrite = TRUE)
  spatial_name <- readRDS(crb_one)$availableSpatial()[[1L]]
  app_dir <- file.path(tmp, "app")
  createShinyApp(
    cerebro_data = c("ds one" = crb_one, "ds two" = crb_two),
    result_dir = app_dir,
    spatial_images = list(
      `ds one` = stats::setNames(list(c(`H&E` = p1)), spatial_name),
      `ds two` = stats::setNames(list(c(`H&E` = p2)), spatial_name)
    ),
    launch_browser = FALSE,
    verbose = FALSE
  )

  copied <- list.files(
    file.path(app_dir, "spatial-assets"),
    pattern = "[.]png$",
    recursive = TRUE
  )
  expect_equal(length(copied), 2)
  ## ... and the bundled configuration points at the two distinct files rather
  ## than twice at one. (The paths live in cerebro_config.rds, not app.R.)
  cfg <- readRDS(file.path(app_dir, "cerebro_config.rds"))
  paths <- unname(unlist(cfg$spatial_images))
  expect_equal(length(unique(paths)), 2)
  expect_true(all(file.exists(file.path(app_dir, paths))))
})

test_that("external images retain their spatial-entry ownership", {
  path <- bundle_file
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(txt, "cv_external_images(nm)", fixed = TRUE)
  expect_no_match(txt, "for (ex in cv_external_images())", fixed = TRUE)
})

test_that("the alignment sliders contain the preset they are given", {
  ## A preset is a calibration someone measured. A slider ranged on the
  ## coordinate span alone clamps anything outside it, and because the bar is
  ## read back as a whole the clamped number is then written into the state by an
  ## unrelated nudge -- the alignment quietly becoming one nobody chose.
  path <- file.path(dirname(bundle_file), "server.R")
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(txt, "abs(pr$offsetX", fixed = TRUE)
  expect_match(txt, "scale_lo", fixed = TRUE)
  expect_no_match(txt, 'rng("cv-img-scalex", 0.3, 3', fixed = TRUE)
})

test_that("More settings is an accessible drawer rather than a draggable window", {
  skip_if(is.na(local_inst), "viewer sources not found")

  ui <- paste(
    readLines(
      file.path(local_inst, "viewer/coordinated_views/UI.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  js <- paste(
    readLines(
      file.path(local_inst, "viewer/www/coordviews.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  css <- paste(
    readLines(
      file.path(local_inst, "viewer/www/coordviews.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(ui, '`role` = "dialog"', fixed = TRUE)
  expect_match(ui, '`aria-hidden` = "true"', fixed = TRUE)
  expect_no_match(ui, "data-cv-more-drag-handle", fixed = TRUE)
  expect_no_match(ui, "Drag to move", fixed = TRUE)
  expect_no_match(js, "beginMoreDrag", fixed = TRUE)
  expect_no_match(js, "moreFloating", fixed = TRUE)
  expect_match(css, "@media (prefers-reduced-motion: reduce)", fixed = TRUE)
  expect_match(css, "#cv-more,", fixed = TRUE)
  expect_match(css, "transition: none", fixed = TRUE)
  expect_no_match(css, "transition: transform .3s", fixed = TRUE)
})

test_that("external backgrounds require matching PNG or JPEG magic bytes", {
  skip_if_not(have_bundle)
  skip_if_not_installed("base64enc")
  tmp <- file.path(tempdir(), "cv_external_magic")
  unlink(tmp, recursive = TRUE)
  assets <- file.path(tmp, "spatial-assets")
  dir.create(assets, recursive = TRUE)
  png_magic <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  jpeg_magic <- as.raw(c(0xff, 0xd8, 0xff, 0xe0))
  writeBin(png_magic, file.path(assets, "valid.png"))
  writeBin(jpeg_magic, file.path(assets, "valid.jpg"))
  writeLines("not an image", file.path(assets, "text.png"))
  writeBin(png_magic, file.path(assets, "mismatch.jpg"))
  cv_env$Cerebro.options <- list(
    cerebro_root = tmp,
    spatial_images = list(
      ds = list(
        fov = c(
          png = "spatial-assets/valid.png",
          jpeg = "spatial-assets/valid.jpg",
          text = "spatial-assets/text.png",
          mismatch = "spatial-assets/mismatch.jpg"
        )
      )
    )
  )
  cv_env$available_crb_files <- list(
    selected = "f.crb",
    files = c(ds = "f.crb")
  )
  on.exit(
    {
      rm("Cerebro.options", envir = cv_env)
      rm("available_crb_files", envir = cv_env)
    },
    add = TRUE
  )

  images <- cv_env$cv_external_images("fov")
  expect_identical(
    vapply(images, `[[`, character(1), "label"),
    c("png", "jpeg")
  )
  expect_match(images[[1L]]$uri, "^data:image/png;base64,")
  expect_match(images[[2L]]$uri, "^data:image/jpeg;base64,")
})

test_that("background state keys namespace FOV and direct modalities", {
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  skip_if_not(file.exists(js_file))
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
  expect_match(js, "function backgroundStateKey\\(sp\\)")
  expect_match(js, "return 'fov:' \\+ \\(sp.id \\|\\| spatialName\\(sp\\)\\)")
  expect_match(js, "return 'space:' \\+ \\(sp.id \\|\\| 'unknown'\\)")
})

test_that("same-dataset refresh preserves percentage and group filters", {
  js_file <- file.path(dirname(bundle_file), "..", "www", "coordviews.js")
  skip_if_not(file.exists(js_file))
  js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
  expect_match(
    js,
    "if \\(dataChanged\\) \\{[\\s\\S]{0,1200}rebuildPctMask\\(\\)",
    perl = TRUE
  )
  expect_no_match(
    js,
    "pctMask = null; groupFilter = \\{\\};[[:space:]]*rebuildPctMask\\(\\)"
  )
})
