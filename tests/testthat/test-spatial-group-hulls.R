# test-spatial-group-hulls.R — unit tests for per-group convex hulls used to
# outline spatial regions.
#
# The categorical spatial plot can outline each colour group by its convex hull
# so the tissue regions read at a glance. This is the pure geometry behind that:
# given point coordinates and a group label per point, return the closed convex
# hull (x/y vertex vectors, first vertex repeated at the end) for each group
# that has enough points to enclose an area.

hulls <- compute_group_hulls

test_that("returns one closed hull per group with >= 3 points", {
  # Group A: a unit square (4 points). Group B: a triangle (3 points).
  x <- c(0, 1, 1, 0, 10, 11, 10)
  y <- c(0, 0, 1, 1, 10, 10, 11)
  g <- c("A", "A", "A", "A", "B", "B", "B")
  out <- hulls(x, y, g)
  expect_setequal(names(out), c("A", "B"))
  # closed ring: last vertex equals first
  expect_equal(out$A$x[1], out$A$x[length(out$A$x)])
  expect_equal(out$A$y[1], out$A$y[length(out$A$y)])
})

test_that("the square's hull spans its full extent", {
  x <- c(0, 1, 1, 0)
  y <- c(0, 0, 1, 1)
  g <- c("A", "A", "A", "A")
  out <- hulls(x, y, g)
  expect_equal(range(out$A$x), c(0, 1))
  expect_equal(range(out$A$y), c(0, 1))
})

test_that("an interior point is not a hull vertex", {
  # four corners + one centre point; the centre must not appear on the hull
  x <- c(0, 2, 2, 0, 1)
  y <- c(0, 0, 2, 2, 1)
  g <- rep("A", 5)
  out <- hulls(x, y, g)
  # hull is the 2x2 square (4 unique corners + closing repeat = 5 vertices)
  expect_equal(length(unique(paste(out$A$x, out$A$y))), 4)
})

test_that("groups with fewer than 3 points are dropped", {
  # "A" has 3 non-collinear points (a triangle) and survives; "solo" has one
  # point and is dropped.
  x <- c(0, 2, 1, 5)
  y <- c(0, 0, 2, 5)
  g <- c("A", "A", "A", "solo")
  out <- hulls(x, y, g)
  expect_true("A" %in% names(out))
  expect_false("solo" %in% names(out))
})

test_that("three collinear points do not form a hull", {
  # collinear points enclose no area — not a usable region outline
  x <- c(0, 1, 2)
  y <- c(0, 1, 2)
  g <- rep("line", 3)
  out <- hulls(x, y, g)
  expect_false("line" %in% names(out))
})

test_that("returns an empty list when there are no groups", {
  out <- hulls(numeric(0), numeric(0), character(0))
  expect_equal(out, list())
})

test_that("NA coordinates are ignored", {
  x <- c(0, 1, 1, 0, NA)
  y <- c(0, 0, 1, 1, NA)
  g <- c("A", "A", "A", "A", "A")
  out <- hulls(x, y, g)
  expect_equal(range(out$A$x), c(0, 1))
})

test_that("the shared cell-view engine consumes and draws Spatial hulls", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(engine, "function singleHulls", fixed = TRUE)
  expect_match(engine, "function drawHulls", fixed = TRUE)
  expect_match(engine, "drawHulls(p);", fixed = TRUE)
})
