## Trajectory information (pseudotime, PAGA, scVelo velocity) from h5ad files.
##
## small_trajectory.h5ad was written by real scanpy (neighbors, diffmap, dpt,
## paga over `cell_type`) on 120 cells arranged on a circle of radius 5; the
## scVelo-shaped parts (latent_time, velocity_pseudotime with NaN, and
## obsm["velocity_umap"] = unit tangent of the circle) are constructed so the
## expected result is known. See make_fixtures.py.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

fixture <- anndata_test_fixture
read_fixture <- function(name, ...) {
  suppressWarnings(suppressMessages(readH5adAsSCE(fixture(name), ...)))
}
trajectories_of <- function(sce) S4Vectors::metadata(sce)$trajectories

test_that("PAGA becomes edges between group centroids, one trajectory per pseudotime", {
  sce <- read_fixture("small_trajectory.h5ad")
  paga <- trajectories_of(sce)$paga
  expect_identical(
    names(paga),
    c(
      "cell_type (dpt_pseudotime)",
      "cell_type (velocity_pseudotime)",
      "cell_type (latent_time)"
    )
  )
  trajectory <- paga[["cell_type (latent_time)"]]

  ## the structure the Viewer needs
  expect_identical(
    colnames(trajectory$meta),
    c("DR_1", "DR_2", "pseudotime", "state")
  )
  expect_identical(
    colnames(trajectory$edges),
    c(
      "source",
      "target",
      "weight",
      "source_dim_1",
      "source_dim_2",
      "target_dim_1",
      "target_dim_2"
    )
  )
  expect_identical(rownames(trajectory$meta), colnames(sce))
  expect_s3_class(trajectory$meta$state, "factor")
  expect_identical(
    levels(trajectory$meta$state),
    c("T cell", "B cell", "Monocyte")
  )

  ## cells sit where the UMAP puts them, pseudotime is the obs column
  umap <- SingleCellExperiment::reducedDim(sce, "UMAP")
  expect_equal(unname(as.matrix(trajectory$meta[, 1:2])), unname(umap))
  expect_equal(
    trajectory$meta$pseudotime,
    as.numeric(SummarizedExperiment::colData(sce)$latent_time)
  )

  ## nodes are the centroids of the groups' cells in that embedding
  centroid <- function(label) {
    colMeans(umap[SummarizedExperiment::colData(sce)$cell_type == label, ])
  }
  edges <- trajectory$edges
  expect_gt(nrow(edges), 0L)
  for (i in seq_len(nrow(edges))) {
    expect_equal(
      unname(c(edges$source_dim_1[i], edges$source_dim_2[i])),
      unname(centroid(edges$source[i]))
    )
    expect_equal(
      unname(c(edges$target_dim_1[i], edges$target_dim_2[i])),
      unname(centroid(edges$target[i]))
    )
  }
  ## the three arcs touch pairwise, with scanpy's connectivities as weights
  expect_setequal(
    paste(edges$source, edges$target, sep = "|"),
    c("T cell|B cell", "T cell|Monocyte", "B cell|Monocyte")
  )
  expect_true(all(edges$weight >= 0.01 & edges$weight <= 1))
})

test_that("pseudotime keeps scanpy's and scVelo's values and turns non-finite into NA", {
  sce <- read_fixture("small_trajectory.h5ad")
  obs <- SummarizedExperiment::colData(sce)
  paga <- trajectories_of(sce)$paga

  velocity_pt <- paga[["cell_type (velocity_pseudotime)"]]$meta$pseudotime
  expect_identical(which(is.na(velocity_pt)), which(seq_len(120L) %% 10L == 4L))
  expect_equal(
    velocity_pt[!is.na(velocity_pt)],
    obs$velocity_pseudotime[!is.na(velocity_pt)]
  )

  dpt <- paga[["cell_type (dpt_pseudotime)"]]$meta$pseudotime
  expect_false(anyNA(dpt))
  expect_equal(dpt, as.numeric(obs$dpt_pseudotime))
  expect_equal(range(dpt), c(0, 1), tolerance = 1e-6)
})

test_that("scVelo velocity becomes arrows that follow the velocity field", {
  sce <- read_fixture("small_trajectory.h5ad")
  ## obsm["velocity_umap"] holds vectors, not positions
  expect_false(any(grepl(
    "velocity",
    names(SingleCellExperiment::reducedDims(sce))
  )))

  velocity <- trajectories_of(sce)$scvelo
  expect_identical(
    names(velocity),
    c(
      "velocity_umap (dpt_pseudotime)",
      "velocity_umap (velocity_pseudotime)",
      "velocity_umap (latent_time)"
    )
  )
  edges <- velocity[["velocity_umap (latent_time)"]]$edges
  expect_gt(nrow(edges), 0L)
  expect_identical(nrow(edges) %% 3L, 0L)
  shaft <- edges[!grepl("_tip$", edges$source), ]
  barbs <- edges[grepl("_tip$", edges$source), ]
  expect_identical(nrow(barbs), 2L * nrow(shaft))

  tail <- cbind(shaft$source_dim_1, shaft$source_dim_2)
  vec <- cbind(shaft$target_dim_1 - tail[, 1], shaft$target_dim_2 - tail[, 2])
  ## on the circle the field is the counter-clockwise tangent: the cross
  ## product of position and arrow is positive and the arrow is (nearly)
  ## perpendicular to the radius
  cross <- tail[, 1] * vec[, 2] - tail[, 2] * vec[, 1]
  expect_true(all(cross > 0))
  radial <- (tail[, 1] * vec[, 1] + tail[, 2] * vec[, 2]) /
    (sqrt(rowSums(tail^2)) * sqrt(rowSums(vec^2)))
  expect_true(all(abs(radial) < 0.4))

  ## each head: two barbs from the tip, pointing back along the shaft
  barb_vec <- cbind(
    barbs$target_dim_1 - barbs$source_dim_1,
    barbs$target_dim_2 - barbs$source_dim_2
  )
  shaft_of_barb <- vec[match(sub("_tip$", "", barbs$source), shaft$source), ]
  expect_true(all(rowSums(barb_vec * shaft_of_barb) < 0))
  expect_equal(
    sqrt(rowSums(barb_vec^2)) / sqrt(rowSums(shaft_of_barb^2)),
    rep(0.3, nrow(barbs)),
    tolerance = 1e-8
  )
})

test_that("a pseudotime column alone gives the edge-less 'pseudotime' method", {
  sce <- read_fixture("small_pseudotime.h5ad")
  trajectories <- trajectories_of(sce)
  expect_identical(names(trajectories), "pseudotime")
  trajectory <- trajectories$pseudotime$dpt_pseudotime
  expect_identical(nrow(trajectory$edges), 0L)
  expect_identical(ncol(trajectory$edges), 7L)
  expect_equal(trajectory$meta$pseudotime, seq(0, 1, length.out = 120L))
  expect_identical(nlevels(trajectory$meta$state), 3L) # cell_type
})

test_that("a file without trajectory data is read as before", {
  sce <- read_fixture("small_csr.h5ad")
  expect_null(trajectories_of(sce))
  expect_identical(
    names(SingleCellExperiment::reducedDims(sce)),
    c("PCA", "UMAP")
  )
  expect_identical(names(S4Vectors::metadata(sce)), "marker_genes")
})

test_that("PAGA or velocity without a pseudotime are skipped, with a message", {
  path <- withr::local_tempfile(fileext = ".h5ad")
  file.copy(fixture("small_trajectory.h5ad"), path)
  rhdf5::h5delete(path, "obs/dpt_pseudotime")
  rhdf5::h5delete(path, "obs/latent_time")
  rhdf5::h5delete(path, "obs/velocity_pseudotime")
  messages <- character()
  sce <- withCallingHandlers(
    suppressWarnings(readH5adAsSCE(path, verbose = TRUE)),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_null(trajectories_of(sce))
  expect_true(any(grepl("needs a pseudotime", messages)))
})

test_that("the trajectories reach the .crb and the Viewer's methods", {
  out <- withr::local_tempdir()
  path <- suppressMessages(suppressWarnings(convertH5adToCerebro(
    fixture("small_trajectory.h5ad"),
    out,
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  )))
  obj <- readCerebro(path)
  expect_setequal(obj$getMethodsForTrajectories(), c("paga", "scvelo"))
  expect_length(obj$getNamesOfTrajectories("paga"), 3L)
  trajectory <- obj$getTrajectory("paga", "cell_type (dpt_pseudotime)")
  expect_identical(rownames(trajectory$meta), obj$getMetaData()$cell_barcode)
  expect_true(all(
    c("DR_1", "DR_2", "pseudotime", "state") %in% colnames(trajectory$meta)
  ))
  expect_identical(nrow(trajectory$edges), 3L)
  ## the Viewer lists these methods and the dataset advertises the page
  utility <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = utility)
  expect_setequal(
    utility$viewerSupportedTrajectoryMethods(obj$getMethodsForTrajectories()),
    c("paga", "scvelo")
  )
  expect_true(.viewerDatasetCapabilities(obj)$trajectory)
})

test_that("exportToH5ad writes pseudotime as obs columns, NaN where a cell has none", {
  crb <- suppressWarnings(suppressMessages(convertH5adToCerebro(
    anndata_test_fixture("small_trajectory.h5ad"),
    withr::local_tempdir(),
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  )))
  out <- file.path(withr::local_tempdir(), "pt.h5ad")
  suppressMessages(exportToH5ad(crb, out))
  back <- suppressWarnings(suppressMessages(readH5adAsSCE(out)))
  columns <- colnames(SummarizedExperiment::colData(back))
  expect_true("dpt_pseudotime" %in% columns)
  source <- suppressWarnings(suppressMessages(readH5adAsSCE(
    anndata_test_fixture("small_trajectory.h5ad")
  )))
  expect_true(
    "dpt_pseudotime" %in% colnames(SummarizedExperiment::colData(source))
  )
  shared <- !is.na(source$dpt_pseudotime)
  expect_equal(
    back$dpt_pseudotime[shared],
    source$dpt_pseudotime[shared],
    tolerance = 1e-6
  )
})
