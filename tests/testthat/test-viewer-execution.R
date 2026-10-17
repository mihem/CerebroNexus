test_that("Canonical resource messages enforce exact dataset identity", {
  viewer_node_test("identity")
})

test_that("Spatial resources validate identity and shape", {
  viewer_node_test("spatial")
})

test_that("Canonical subsets decode exact rows and reject invalid mappings", {
  viewer_node_test("subsets")
})

test_that("Trajectory frames validate every resource", {
  viewer_node_test("trajectory")
})

test_that("Canonical metadata messages validate dictionaries and dataset identity", {
  viewer_node_test("metadata")
})

test_that("Dataset changes retire specialist state and all stale message channels", {
  viewer_node_test("lifecycle")
})

test_that("Resource caches reuse current bytes and reject retired requests", {
  viewer_node_test("caches")
})

test_that("HLA navigation respects reduced motion", {
  viewer_node_test("motion")
})

test_that("Gene hydration preserves f64 values, missing values and cache reuse", {
  viewer_browser_test("colors")
})

test_that("Gene recoloring preserves viewport and selection and rejects stale paints", {
  viewer_browser_test("incremental")
})

test_that("Gene display modes paint every panel on the first frame", {
  viewer_browser_test("genePanels")
})

test_that("Canvas draws physical coordinates, per-cell radii and trajectory edges", {
  viewer_browser_test("geometry")
})

test_that("Missing coordinates remain absent in JSON, grouped and binary views", {
  viewer_browser_test("missing")
})

test_that("Specialist panels align colors and geometry by cell identity", {
  viewer_browser_test("panels")
})

test_that("Hover dictionaries render labels and pending IDs are requested on demand", {
  viewer_browser_test("hover")
})

test_that("Linked tooltips, deferred groups and selection survive hover toggles", {
  viewer_browser_test("linked")
})

test_that("Background selection and assets belong to their dataset generation", {
  viewer_browser_test("backgrounds")
})

test_that("Supplements restore selection and reject stale or malformed geometry", {
  viewer_browser_test("supplement")
})

test_that("Linked resource hydration validates the primary bundle identity", {
  viewer_browser_test("linkedResources")
})

test_that("Trekker transition controls move valid points and respect page visibility", {
  viewer_browser_test("trekker")
})

test_that("Clonal bitmap caching reuses unchanged content and redraws changes", {
  viewer_browser_test("cloneCache")
})

test_that("Spatial resource prefetch respects filters and late catalogs", {
  viewer_browser_test("prefetch")
})

test_that("GPU colors, paint order and CPU fallback preserve exact output", {
  viewer_browser_test("gpuColors")
})

test_that("GPU selection updates agree with full rebuilds and preserve overlays", {
  viewer_browser_test("gpu")
})

test_that("Background messages require matching dataset and image identities", {
  viewer_browser_test("backgroundChannel")
})
