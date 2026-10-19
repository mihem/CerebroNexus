## Fixtures that each load complete viewer scripts under Node and exit non-zero
## on the first failed assertion.

test_that("GPU renderers upload only attributes whose arrays changed", {
  expect_node_fixture(
    "gpu-attribute-upload.js",
    viewer_test_path("www", "cell_points_gpu.js")
  )
})

test_that("Indexed point picking matches the full scan across views and filters", {
  expect_node_fixture("point-grid.js", viewer_test_path("www", "cv-geom.js"))
})

test_that("background image rotates in data space with the cells", {
  expect_node_fixture(
    "background-rotation.js",
    viewer_test_path("www", "cell_views.js")
  )
})

test_that("shared renderer restores every page and retains late Linked supplements", {
  expect_node_fixture(
    "cell-view-navigation.js",
    viewer_test_path("www", "cell_views.js")
  )
})

test_that("gene search refresh is atomic and rejects late selections and choices", {
  expect_node_fixture(
    "gene-selector.js",
    viewer_test_path("www", "gene-selector.js")
  )
})

test_that("shared cluster labels are text without background boxes", {
  expect_node_fixture(
    "group-label-text.js",
    viewer_test_path("www", "cell_views.js")
  )
})

test_that("Spatial background accepts slider and native input events", {
  expect_node_fixture(
    "spatial-background-events.js",
    viewer_test_path("spatial", "js_page_helpers.js")
  )
})

test_that("shared lifecycle and background models preserve view semantics", {
  expect_node_fixture(
    "viewer-shared-models.js",
    dirname(viewer_test_path("www", "cell_views.js")),
    pass = "PASS backgrounds"
  )
})

test_that("Linked background settings isolate ROI images and deferred state", {
  expect_node_fixture(
    "linked-background-controls.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS linked background"
  )
})

test_that("Linked views defaults to individual spatial scenes and preserves edits", {
  expect_node_fixture(
    "linked-spatial-defaults.js",
    viewer_test_path("www", "cell_views.js"),
    pass = "PASS individual-scene defaults"
  )
})

test_that("startup waits for the active page and releases errors and empty results", {
  expect_node_fixture(
    "startup-lifecycle.js",
    viewer_test_path("www", "startup.js"),
    pass = "PASS startup readiness"
  )
})
