spatial_contract_inst_path <- function(...) {
  relative <- file.path(...)
  path <- testthat::test_path("..", "..", "inst", relative)
  if (!file.exists(path)) {
    path <- system.file(relative, package = "CerebroNexus")
  }
  path
}

spatial_contract_core_path <- spatial_contract_inst_path(
  "viewer",
  "core",
  "spatial_coordinate_contract.R"
)
spatial_transform_core_path <- spatial_contract_inst_path(
  "viewer",
  "core",
  "spatial_coordinate_transform.R"
)
if (file.exists(spatial_contract_core_path)) {
  sys.source(spatial_contract_core_path, envir = environment())
}
if (file.exists(spatial_transform_core_path)) {
  sys.source(spatial_transform_core_path, envir = environment())
}

test_that("the shared contract accepts every exporter coordinate alias", {
  contract <- .spx_coordinate_contract()

  for (alias in contract$x) {
    data <- stats::setNames(
      data.frame(one = 1:2, two = 3:4),
      c(alias, "y")
    )
    expect_identical(
      .spx_find_coordinate_columns(data),
      list(x = alias, y = "y"),
      info = paste("x alias", alias)
    )
  }
  for (alias in contract$y) {
    data <- stats::setNames(
      data.frame(one = 1:2, two = 3:4),
      c("x", alias)
    )
    expect_identical(
      .spx_find_coordinate_columns(data),
      list(x = "x", y = alias),
      info = paste("y alias", alias)
    )
  }
})
test_that("arbitrary numeric columns are not spatial coordinates", {
  data <- data.frame(foo = c(1, 2), bar = c(3, 4))

  expect_null(.spx_find_coordinate_columns(data))
  expect_identical(
    .spx_find_coordinate_columns(
      data,
      coord_cols = c("foo", "bar")
    ),
    list(x = "foo", y = "bar")
  )
})

test_that("the shared contract accepts every exporter barcode alias", {
  contract <- .spx_coordinate_contract()
  expected <- c("cell-a", "cell-b")

  for (alias in contract$barcode) {
    data <- stats::setNames(
      data.frame(value = expected, stringsAsFactors = FALSE),
      alias
    )
    expect_identical(
      .spx_find_barcode_column(data, expected),
      alias,
      info = paste("barcode alias", alias)
    )
  }
  expect_null(.spx_find_barcode_column(
    data.frame(sample = expected),
    expected
  ))
})

test_that("barcode alias precedence and overlap match the exporter", {
  data <- data.frame(
    cell_id = c("cell-a", "outside"),
    ID = c("cell-a", "cell-b"),
    name = c("cell-b", "cell-a"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    .spx_find_barcode_column(data, c("cell-a", "cell-b")),
    "ID"
  )
  data$cell_id <- c("cell-a", "cell-b")
  expect_identical(
    .spx_find_barcode_column(data, c("cell-a", "cell-b")),
    "cell_id"
  )
})

test_that("AsIs barcode columns retain exporter compatibility", {
  character_data <- data.frame(
    barcode = I(c("cell-a", "cell-b")),
    stringsAsFactors = FALSE
  )
  integer_data <- data.frame(barcode = I(c(101L, 102L)))

  expect_identical(
    .spx_find_barcode_column(
      character_data,
      c("cell-a", "cell-b")
    ),
    "barcode"
  )
  expect_identical(
    .spx_find_barcode_column(integer_data, c("101", "102")),
    "barcode"
  )
})

test_that("classed atomic barcodes never dispatch custom conversion", {
  touched <- FALSE
  assign(
    "as.character.spx_barcode_trap",
    function(value, ...) {
      touched <<- TRUE
      stop("untrusted method executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("as.character.spx_barcode_trap", envir = .GlobalEnv),
    add = TRUE
  )

  data <- data.frame(
    barcode = c("cell-a", "cell-b"),
    stringsAsFactors = FALSE
  )
  attr(data$barcode, "class") <- "spx_barcode_trap"

  expect_identical(
    .spx_find_barcode_column(data, c("cell-a", "cell-b")),
    "barcode"
  )
  expect_false(touched)

  data$barcode <- I(list("cell-a", "cell-b"))
  expect_null(.spx_find_barcode_column(data, c("cell-a", "cell-b")))
  expect_false(touched)
})

test_that("classed column names never dispatch custom methods", {
  touched <- FALSE
  assign(
    "as.character.spx_contract_trap",
    function(value, ...) {
      touched <<- TRUE
      stop("untrusted method executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm("as.character.spx_contract_trap", envir = .GlobalEnv),
    add = TRUE
  )

  data <- data.frame(x = 1:2, y = 3:4)
  attr(data, "names") <- structure(
    c("x", "y"),
    class = "spx_contract_trap"
  )
  expect_null(.spx_find_coordinate_columns(data))
  expect_null(.spx_find_barcode_column(data, c("1", "2")))
  expect_false(touched)
})

test_that("the exporter consumes the shared coordinate contract", {
  exporter_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "seurat_utils.R"
  )
  skip_if_not(
    file.exists(exporter_path),
    "source tree not present (installed-package layout)"
  )
  exporter <- readLines(exporter_path, warn = FALSE)
  expect_true(any(grepl(
    ".spx_find_coordinate_columns",
    exporter,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    ".spx_find_barcode_column",
    exporter,
    fixed = TRUE
  )))
})

test_that("the bundled coordinate contract is byte-identical and safe", {
  source_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "spatial_coordinate_contract.R"
  )
  skip_if_not(
    file.exists(source_path),
    "R source tree not present (installed-package layout)"
  )
  expect_true(file.exists(spatial_contract_core_path))
  source_bytes <- readBin(
    source_path,
    what = "raw",
    n = file.info(source_path)$size
  )
  runtime_bytes <- readBin(
    spatial_contract_core_path,
    what = "raw",
    n = file.info(spatial_contract_core_path)$size
  )
  expect_identical(runtime_bytes, source_bytes)

  text <- paste(readLines(source_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("CerebroNexus|cerebroAppLite", text))
  expect_false(grepl(
    "library\\s*\\(|requireNamespace\\s*\\(|getFromNamespace\\s*\\(|::",
    text,
    perl = TRUE
  ))
})

test_that("the bundled coordinate-transform contract is byte-identical and safe", {
  source_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "spatial_coordinate_transform.R"
  )
  skip_if_not(
    file.exists(source_path),
    "R source tree not present (installed-package layout)"
  )
  expect_true(file.exists(spatial_transform_core_path))
  source_bytes <- readBin(
    source_path,
    what = "raw",
    n = file.info(source_path)$size
  )
  runtime_bytes <- readBin(
    spatial_transform_core_path,
    what = "raw",
    n = file.info(spatial_transform_core_path)$size
  )
  expect_identical(runtime_bytes, source_bytes)

  text <- paste(readLines(source_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("CerebroNexus|cerebroAppLite", text))
  expect_false(grepl(
    "library\\s*\\(|requireNamespace\\s*\\(|getFromNamespace\\s*\\(|::",
    text,
    perl = TRUE
  ))
})

test_that("coordinate transforms normalize to immutable canonical provenance", {
  coordinates <- data.frame(x = c(0, 2), y = c(0, 2))
  normalized <- .spx_coordinate_transform_normalize(
    list(
      rotation_degrees = 450,
      scale = 2
    ),
    coordinates
  )

  expect_identical(normalized$schema_version, 1L)
  expect_identical(normalized$rotation_degrees, 90)
  expect_identical(normalized$scale, 2)
  expect_identical(normalized$pivot_method, "bounds_center")
  expect_identical(normalized$convention, "counterclockwise_degrees")
  expect_identical(normalized$pivot, c(x = 1, y = 1))
  expect_identical(
    .spx_coordinate_transform_normalize(NULL, coordinates),
    .spx_coordinate_transform_identity(coordinates)
  )
})

test_that("coordinate fingerprints bind row identity and transformed values", {
  coordinates <- data.frame(
    x = c(0, 2),
    y = c(1, 3),
    row.names = c("cell-a", "cell-b")
  )
  fingerprint <- .spx_coordinate_transform_fingerprint(coordinates)

  expect_identical(
    .spx_coordinate_transform_fingerprint(coordinates),
    fingerprint
  )
  changed <- coordinates
  changed$x[[1L]] <- changed$x[[1L]] + 1
  expect_false(identical(
    .spx_coordinate_transform_fingerprint(changed),
    fingerprint
  ))
  reordered <- coordinates[2:1, , drop = FALSE]
  expect_false(identical(
    .spx_coordinate_transform_fingerprint(reordered),
    fingerprint
  ))
  round_trip_noise <- coordinates
  round_trip_noise$x[[1L]] <- round_trip_noise$x[[1L]] + 1e-16
  expect_identical(
    .spx_coordinate_transform_fingerprint(round_trip_noise),
    fingerprint
  )
})

test_that("coordinate transforms invert around their persisted pivot", {
  coordinates <- data.frame(
    x = c(0, 2, 3),
    y = c(1, 4, -2),
    row.names = c("a", "b", "c")
  )
  spec <- list(rotation_degrees = 37, scale = 1.7)
  provenance <- .spx_coordinate_transform_normalize(spec, coordinates)
  transformed <- .spx_apply_coordinate_transform(coordinates, spec)

  expect_equal(
    .spx_invert_coordinate_transform(transformed, provenance),
    coordinates,
    tolerance = 1e-12
  )
})

test_that("coordinate fingerprints survive a non-orthogonal transform round trip", {
  set.seed(1)
  coordinates <- data.frame(
    x = stats::runif(30, 0, 1000),
    y = stats::runif(30, 0, 1000),
    row.names = paste0("cell-", seq_len(30))
  )
  spec <- list(rotation_degrees = 12, scale = 1.05)
  provenance <- .spx_coordinate_transform_normalize(spec, coordinates)
  transformed <- .spx_apply_coordinate_transform(coordinates, spec)
  restored <- .spx_invert_coordinate_transform(transformed, provenance)

  expect_equal(restored, coordinates)
  expect_identical(
    .spx_coordinate_transform_fingerprint(restored),
    .spx_coordinate_transform_fingerprint(coordinates)
  )
})

test_that("coordinate transform specs are coordinate-independent", {
  expect_identical(
    .spx_coordinate_transform_spec_normalize(list(
      rotation_degrees = -90,
      scale = 0.5
    )),
    list(schema_version = 1L, rotation_degrees = -90, scale = 0.5)
  )
  expect_identical(
    .spx_coordinate_transform_spec_normalize(list(
      rotation_degrees = 540,
      scale = 1
    ))$rotation_degrees,
    -180
  )
  expect_identical(
    .spx_coordinate_transform_spec_normalize(list(
      schema_version = 1L,
      rotation_degrees = 45,
      scale = 1.2
    )),
    list(schema_version = 1L, rotation_degrees = 45, scale = 1.2)
  )
  expect_error(
    .spx_coordinate_transform_spec_normalize(list(
      schema_version = 2L,
      rotation_degrees = 45,
      scale = 1.2
    )),
    "schema_version"
  )
})

test_that("coordinate transforms rotate and scale about the full bounds center", {
  coordinates <- data.frame(
    x = c(0, 2),
    y = c(0, 2),
    metadata = c("a", "b"),
    row.names = c("cell-a", "cell-b")
  )

  transformed <- .spx_apply_coordinate_transform(
    coordinates,
    list(rotation_degrees = 90, scale = 2)
  )

  expect_identical(rownames(transformed), rownames(coordinates))
  expect_identical(transformed$metadata, coordinates$metadata)
  expect_equal(transformed$x, c(3, -1))
  expect_equal(transformed$y, c(-1, 3))
  inverse <- .spx_apply_coordinate_transform(
    transformed,
    list(rotation_degrees = -90, scale = 0.5)
  )
  expect_equal(inverse$x, coordinates$x)
  expect_equal(inverse$y, coordinates$y)
  expect_identical(
    .spx_apply_coordinate_transform(coordinates, NULL),
    coordinates
  )
})

test_that("per-FOV transforms validate ordinary named maps", {
  transforms <- .spx_coordinate_transforms_normalize(
    list(
      section_a = list(rotation_degrees = 90, scale = 2),
      section_b = list(rotation_degrees = -45, scale = 0.5)
    ),
    spatial_names = c("section_a", "section_b"),
    coordinates_by_spatial = list(
      section_a = data.frame(x = c(0, 2), y = c(0, 2)),
      section_b = data.frame(x = c(0, 4), y = c(0, 4))
    )
  )

  expect_identical(names(transforms), c("section_a", "section_b"))
  expect_identical(transforms$section_a$rotation_degrees, 90)
  expect_identical(transforms$section_b$rotation_degrees, -45)
  expect_error(
    .spx_coordinate_transforms_normalize(
      list(missing = list(rotation_degrees = 0, scale = 1)),
      spatial_names = "section_a",
      coordinates_by_spatial = list(section_a = data.frame(x = 0, y = 0))
    ),
    "unknown FOV"
  )
  expect_error(
    .spx_coordinate_transforms_normalize(
      list(section_a = list(rotation_degrees = 0, scale = 1)),
      spatial_names = c("section_a", "section_a"),
      coordinates_by_spatial = list(section_a = data.frame(x = 0, y = 0))
    ),
    "unique"
  )
  expect_error(
    .spx_coordinate_transforms_normalize(
      list(section_a = list(rotation_degrees = 0, scale = 1, extra = TRUE)),
      spatial_names = "section_a",
      coordinates_by_spatial = list(section_a = data.frame(x = 0, y = 0))
    ),
    "unknown"
  )
})

test_that("coordinate transforms reject invalid scalar records", {
  expect_error(
    .spx_coordinate_transform_normalize(
      list(rotation_degrees = Inf, scale = 1),
      data.frame(x = 0, y = 0)
    ),
    "finite"
  )
  expect_error(
    .spx_coordinate_transform_normalize(
      list(rotation_degrees = 0, scale = 0),
      data.frame(x = 0, y = 0)
    ),
    "strictly positive"
  )
  expect_error(
    .spx_coordinate_transforms_normalize(
      "section_a",
      spatial_names = "section_a",
      coordinates_by_spatial = list(section_a = data.frame(x = 0, y = 0))
    ),
    "named list"
  )
})
