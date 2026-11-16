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

test_that("Single views take cell names from the canonical cell-name resource", {
  viewer_browser_test("auxCellNames")
})

test_that("Cell names delivered to a single view survive the first Linked views bundle", {
  viewer_browser_test("auxAcrossLinked")
})

test_that("Background selection and assets belong to their dataset generation", {
  viewer_browser_test("backgrounds")
})

test_that("Supplements restore selection and reject stale or malformed geometry", {
  viewer_browser_test("supplement")
})

test_that("Compact spatial scenes draw and identify only their own cells", {
  viewer_browser_test("spatialScenes")
})

test_that("Spatial GPU buffers cover their scene and survive an unused gene", {
  viewer_browser_test("spatialGpu")
})

test_that("A colour change keeps GPU geometry and recolours every point", {
  viewer_browser_test("gpuColour")
})

test_that("Region outlines patch the drawn frame in place", {
  viewer_browser_test("outlinePatch")
})

test_that("Zooming one linked panel redraws only that panel", {
  viewer_browser_test("zoomOnePanel")
})

test_that("An unavailable gene redraws only when a gene was drawn", {
  viewer_browser_test("geneUnavailable")
})

test_that("A filter on a deferred group draws once its values arrive", {
  viewer_browser_test("deferredFilter")
})

test_that("A deferred group from Viewer Pack codes draws as its websocket values", {
  viewer_browser_test("deferredResource")
})

test_that("Slice panels draw shared rows named through the cell-name resource", {
  viewer_browser_test("slicePanels")
})

test_that("An appearance patch with the drawn values does not redraw", {
  viewer_browser_test("appearanceUnchanged")
})

test_that("A point size slider draws in the browser before R replies", {
  viewer_browser_test("pointSliderLocal")
})

test_that("A display toggle draws in the browser before R replies", {
  viewer_browser_test("toggleLocal")
})

test_that("A paint-order patch reorders the drawn cells", {
  viewer_browser_test("paintOrderPatch")
})

test_that("Group filters sent as codes filter the drawn frame in the browser", {
  viewer_browser_test("singleClientFilters")
})

test_that("A colour change that resizes the legend draws once", {
  viewer_browser_test("legendRelayout")
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
