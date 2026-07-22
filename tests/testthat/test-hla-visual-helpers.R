test_that("categorical colours stay unique beyond the base palette length", {
  levels <- paste0("level_", seq_len(25))

  colours <- hla_distinct_colors(levels)

  expect_equal(names(colours), levels)
  expect_length(unique(unname(colours)), length(levels))
})

test_that("empty colour levels return an empty named vector", {
  expect_equal(
    hla_distinct_colors(character(0)),
    stats::setNames(character(0), character(0))
  )
})

## ---- node radius: area, not radius, carries the count ------------------ ##

test_that("node area is proportional to the clone count", {
  # The property that matters: the eye compares AREAS, so area/count must be
  # constant. Encoding the count on the radius (vis-network's default for
  # `value`) squares the difference instead.
  n <- c(1, 2, 3, 4, 5, 10, 25)
  r <- hla_node_radius(n)
  area_per_unit <- pi * r^2 / n
  expect_equal(area_per_unit, rep(area_per_unit[1], length(n)))
})

test_that("a single-unit node sits at the minimum radius", {
  expect_equal(hla_node_radius(1), HLA_NODE_R_MIN)
})

test_that("radius is capped, and the documented threshold is where it bites", {
  expect_equal(hla_node_radius(HLA_NODE_MAX_EXACT), HLA_NODE_R_MAX)
  expect_equal(hla_node_radius(HLA_NODE_MAX_EXACT + 1), HLA_NODE_R_MAX)
  expect_equal(hla_node_radius(1e6), HLA_NODE_R_MAX)
  # Above the cap proportionality is GONE; the constant must not be quietly
  # wrong, because the caption states it as the point where that happens.
  expect_lt(
    pi * hla_node_radius(1e6)^2 / 1e6,
    pi * hla_node_radius(1)^2 / 1
  )
})

test_that("radius floors degenerate counts to one unit", {
  expect_equal(hla_node_radius(0), HLA_NODE_R_MIN)
  expect_equal(hla_node_radius(NA), HLA_NODE_R_MIN)
  expect_equal(hla_node_radius(-5), HLA_NODE_R_MIN)
  expect_equal(hla_node_radius(numeric(0)), numeric(0))
})

test_that("radius grows as the square root, never linearly", {
  # Pins the actual shape: doubling the count must multiply the radius by
  # sqrt(2), not by 2.
  expect_equal(hla_node_radius(4) / hla_node_radius(1), 2)
  expect_equal(hla_node_radius(2) / hla_node_radius(1), sqrt(2))
})

test_that("the node size multiplier scales radii without changing the encoding", {
  base <- hla_node_radius(c(1, 4, 16))
  expect_equal(hla_node_radius(c(1, 4, 16), 1), base)
  expect_equal(hla_node_radius(c(1, 4, 16), 2), base * 2)
  # the cap scales by the same factor, so "area = count" still reads the same
  expect_equal(hla_node_radius(1e6, 2), HLA_NODE_R_MAX * 2)
  # invalid or out-of-range multipliers fall back / clamp rather than distort
  expect_equal(hla_node_radius(4, 0), hla_node_radius(4, 1))
  expect_equal(hla_node_radius(4, NA), hla_node_radius(4, 1))
  expect_equal(hla_node_radius(4, 99), hla_node_radius(4, HLA_NODE_SCALE_MAX))
  expect_equal(hla_node_radius(numeric(0), 2), numeric(0))
})
