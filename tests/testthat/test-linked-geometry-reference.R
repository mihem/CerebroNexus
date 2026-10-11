test_that('Linked references only validated canonical two-dimensional trajectory frames', {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path('coordinated_views', 'bundle.R'), scope)
  scope$viewerPackTrajectoryFrame <- function(pack, method, name) pack$frame
  space <- list(
    trajectory = TRUE,
    method = 'm',
    name = 't',
    x = I(c(1, NA, 3)),
    y = I(c(4, NA, 6))
  )
  extra <- list(spaces = list(space))
  primary <- list(
    n = 3L,
    canonical_order_id = 'order',
    projections = list(umap = list(ndim = 2L))
  )
  pack <- list(
    cell_count = 3L,
    manifest = list(cell_order_fingerprint = 'order'),
    frame = list(
      geometry_kind = 'canonical_projection',
      projection_name = 'umap',
      cells = 2L
    )
  )
  object <- structure(list(), cerebro_viewer_pack = pack)
  result <- scope$cv_reference_trajectory_geometry(
    extra,
    primary,
    object
  )$spaces[[1]]
  expect_null(result$x)
  expect_null(result$y)
  expect_equal(as.integer(result$geometry_ref$exclude), 1L)
  primary$projections$umap$ndim <- 3L
  expect_identical(
    scope$cv_reference_trajectory_geometry(extra, primary, object),
    extra
  )
  primary$projections$umap$ndim <- 2L
  primary$canonical_order_id <- 'other'
  expect_identical(
    scope$cv_reference_trajectory_geometry(extra, primary, object),
    extra
  )
})

test_that('Linked browser shares full geometry and preserves exact excluded rows', {
  node <- Sys.which('node')
  skip_if(!nzchar(node))
  result <- system2(
    node,
    c(
      shQuote(test_path('fixtures', 'linked-geometry-reference.js')),
      shQuote(viewer_test_path('www', 'cell_views.js'))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, 'status'), info = paste(result, collapse = '\n'))
})
