## Zoom-to-selection must keep the data aspect ratio: after zooming, one data
## unit must span the same number of pixels on x and y, so the selected region
## is never stretched. The pure math lives in projection_scatter.js as
## computeEqualAspectRange(); exercise it under node here.

repo_file <- function(...) {
  parts <- c(...)
  stripped <- if (length(parts) && identical(parts[[1L]], "inst")) {
    parts[-1L]
  } else {
    parts
  }
  # 1) installed package (R CMD check) or load_all-shimmed source location
  if (length(stripped)) {
    p <- system.file(
      do.call(file.path, as.list(stripped)),
      package = "CerebroNexus"
    )
    if (nzchar(p)) {
      return(p)
    }
  }
  # 2) fall back to the source tree (devtools::test_dir run from the repo)
  testthat::test_path("..", "..", ...)
}

run_node <- function(body) {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  js_path <- repo_file(
    "inst",
    "shiny",
    "v1.4",
    "www",
    "projection_scatter.js"
  )
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = {};",
      "global.document = { addEventListener: function () {} };",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(js_path, quote = "\"")
      ),
      "const f = window.cerebroProjection._computeEqualAspectRange;",
      body
    ),
    runner
  )
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("the zoomed ranges match the plot-area pixel aspect ratio", {
  # Selection 2 wide x 2 tall (square), plot area 400x200 (2:1). To keep equal
  # data-units-per-pixel the x range must grow to twice the y range.
  out <- run_node(paste0(
    "const r = f(0, 2, 0, 2, 400, 200);",
    "const xw = r.xRange[1] - r.xRange[0];",
    "const yh = r.yRange[1] - r.yRange[0];",
    "console.log(JSON.stringify([xw / yh, xw >= 2, yh >= 2]));"
  ))
  expect_equal(attr(out, "status"), NULL)
  expect_equal(out, "[2,true,true]")
})

test_that("a tall selection is letterboxed, not stretched, in a wide plot", {
  # Selection 1 wide x 4 tall in a 300x300 (1:1) plot: keep y, widen x to match
  # the 1:1 ratio; the selection sits centered with whitespace on the sides. An
  # 8% margin is added on all sides so the box never touches the plot edge, so
  # the padded span is 4 * 1.08 = 4.32, still centered on x = 0.5.
  out <- run_node(paste0(
    "const r = f(0, 1, 0, 4, 300, 300);",
    "const xw = r.xRange[1] - r.xRange[0];",
    "const yh = r.yRange[1] - r.yRange[0];",
    "const xc = (r.xRange[0] + r.xRange[1]) / 2;",
    "console.log(JSON.stringify([",
    "  +xw.toFixed(3), +yh.toFixed(3), +xc.toFixed(3)",
    "]));"
  ))
  expect_equal(attr(out, "status"), NULL)
  expect_equal(out, "[4.32,4.32,0.5]")
})

test_that("the selection is always fully contained (only expand, never crop)", {
  # Wide selection (4 wide x 1 tall) in a square plot: keep x, grow y to match,
  # then pad 8% on all sides -> 4 * 1.08 = 4.32, centered on y = 0.5.
  out <- run_node(paste0(
    "const r = f(0, 4, 0, 1, 300, 300);",
    "const xw = r.xRange[1] - r.xRange[0];",
    "const yh = r.yRange[1] - r.yRange[0];",
    "const yc = (r.yRange[0] + r.yRange[1]) / 2;",
    "console.log(JSON.stringify([",
    "  +xw.toFixed(3), +yh.toFixed(3), +yc.toFixed(3)",
    "]));"
  ))
  expect_equal(attr(out, "status"), NULL)
  expect_equal(out, "[4.32,4.32,0.5]")
})

test_that("the framed box has a margin so it never touches the plot edge", {
  # A square selection in a square plot: the framed range must be strictly larger
  # than the selection (padding on every side), and stay centered and 1:1.
  out <- run_node(paste0(
    "const r = f(0, 2, 0, 2, 300, 300);",
    "const xw = r.xRange[1] - r.xRange[0];",
    "const yh = r.yRange[1] - r.yRange[0];",
    "console.log(JSON.stringify([",
    "  xw > 2, yh > 2, +(xw / yh).toFixed(3),",
    "  +((r.xRange[0] + r.xRange[1]) / 2).toFixed(3)",
    "]));"
  ))
  expect_equal(attr(out, "status"), NULL)
  expect_equal(out, "[true,true,1,1]")
})

test_that("toggle flips between zoom-in and reset with matching dragmode", {
  # nextZoomAction(isZoomed): not zoomed -> zoom in and lock (dragmode false);
  # zoomed -> reset and unlock (dragmode 'select').
  out <- run_node(paste0(
    "const g = window.cerebroProjection._nextZoomAction;",
    "const a = g(false);", # currently NOT zoomed -> zoom in
    "const b = g(true);", # currently zoomed -> reset
    "console.log(JSON.stringify([",
    "  [a.zoomIn, a.zoomed, a.dragmode],",
    "  [b.zoomIn, b.zoomed, b.dragmode]",
    "]));"
  ))
  expect_equal(attr(out, "status"), NULL)
  expect_equal(out, "[[true,true,false],[false,false,\"select\"]]")
})

test_that("the zoom marker is a dashed rect on the selection bounds", {
  out <- run_node(paste0(
    "const shp = window.cerebroProjection._zoomMarkerShape(",
    "  { xMin: -4, xMax: -2, yMin: 0.5, yMax: 2.5 });",
    "console.log(JSON.stringify([",
    "  shp.type, shp.x0, shp.x1, shp.y0, shp.y1, shp.line.dash, shp.name",
    "]));"
  ))
  expect_equal(attr(out, "status"), NULL)
  expect_equal(
    out,
    "[\"rect\",-4,-2,0.5,2.5,\"dash\",\"cerebro-zoom-marker\"]"
  )
})

test_that("reset strips only the zoom marker, keeping tab shapes", {
  # A plot whose layout has a trajectory path shape plus a stale zoom marker.
  out <- run_node(paste0(
    "const strip = window.cerebroProjection._shapesWithoutZoomMarker;",
    "const plot = { _fullLayout: { shapes: [",
    "  { name: 'trajectory-path', type: 'line' },",
    "  { name: 'cerebro-zoom-marker', type: 'rect' }",
    "] } };",
    "const kept = strip(plot);",
    "console.log(JSON.stringify([kept.length, kept[0].name]));"
  ))
  expect_equal(attr(out, "status"), NULL)
  expect_equal(out, "[1,\"trajectory-path\"]")
})

test_that("zooming clears the native editable selection and reset restores it", {
  src <- paste(
    readLines(repo_file(
      "inst",
      "shiny",
      "v1.4",
      "www",
      "projection_scatter.js"
    )),
    collapse = "\n"
  )
  # Zoom-in stashes the native selection then clears it (removes drag handles).
  expect_match(
    src,
    paste0(
      "zoomSavedSelections\\.set\\(plotId, harvestSelectionOutline\\(plotId\\)\\)",
      "[\\s\\S]{0,400}?selections: \\[\\]"
    ),
    perl = TRUE
  )
  # Reset restores the stashed native selection.
  expect_match(
    src,
    paste0(
      "zoomSavedSelections\\.get\\(plotId\\)[\\s\\S]{0,400}?",
      "selections: savedSelections"
    ),
    perl = TRUE
  )
})

test_that("a degenerate (zero-area) selection does not divide by zero", {
  # A single point / zero-width selection returns a finite, centered range.
  out <- run_node(paste0(
    "const r = f(5, 5, 5, 5, 400, 200);",
    "const ok = isFinite(r.xRange[0]) && isFinite(r.xRange[1]) &&",
    "  isFinite(r.yRange[0]) && isFinite(r.yRange[1]) &&",
    "  r.xRange[1] > r.xRange[0] && r.yRange[1] > r.yRange[0];",
    "console.log(JSON.stringify(ok));"
  ))
  expect_equal(attr(out, "status"), NULL)
  expect_equal(out, "true")
})
