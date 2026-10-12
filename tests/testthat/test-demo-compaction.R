test_that("demo repacking preserves the complete payload and the source", {
  script <- test_path("..", "..", "data-raw", "compact_demo_crbs.R")
  skip_if_not(file.exists(script), "Repository-only demo build tool")
  scope <- new.env(parent = globalenv())
  sys.source(script, scope)
  root <- tempfile("demo-compact-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "input")
  dir.create(input)
  object <- Cerebro$new()
  cells <- c("003", "c1", "c2")
  object$setExpression(Matrix::Matrix(
    matrix(
      c(0, pi, NA_real_, 0, 1 / 3, 2),
      nrow = 2,
      dimnames = list(c("g1", "g2"), cells)
    ),
    sparse = TRUE
  ))
  object$setMetaData(data.frame(cell_barcode = cells, group = c("A", "B", "A")))
  object$addProjection(
    "umap",
    data.frame(x = c(3, 1, 2), y = c(-1, 0, 1), row.names = cells)
  )
  path <- file.path(input, "demo.crb")
  qs2::qs_save(object, path)
  checksum <- tools::md5sum(path)
  result <- scope$compact_demo_crbs(input, file.path(root, "output"))
  expect_true(result$identical_payload)
  expect_lte(result$after_bytes, result$before_bytes)
  expect_identical(tools::md5sum(path), checksum)
  expect_identical(
    serialize(
      qs2::qs_read(file.path(root, "output", "demo.crb")),
      NULL,
      version = 3L
    ),
    serialize(qs2::qs_read(path), NULL, version = 3L)
  )
  expect_error(scope$compact_demo_crbs(input, input), "existing data")
  expect_error(
    scope$compact_demo_crbs(input, file.path(root, "output")),
    "existing data"
  )
  expect_identical(tools::md5sum(path), checksum)
})

test_that("demo repacking does not publish corrupt input", {
  script <- test_path("..", "..", "data-raw", "compact_demo_crbs.R")
  skip_if_not(file.exists(script), "Repository-only demo build tool")
  scope <- new.env(parent = globalenv())
  sys.source(script, scope)
  root <- tempfile("demo-compact-invalid-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "input")
  dir.create(input)
  writeLines("not a CRB", file.path(input, "invalid.crb"))
  output <- file.path(root, "output")
  expect_error(scope$compact_demo_crbs(input, output))
  expect_false(file.exists(file.path(output, "invalid.crb")))
})

test_that("Spatial demo verification reads qs2 and retains the injected image", {
  script <- test_path("..", "..", "data-raw", "build_spatial_demos.R")
  skip_if_not(file.exists(script), "Repository-only demo builder")
  # Execute the real post-export function, without downloading/processing data.
  assignment <- Filter(
    function(node) {
      identical(node[[2L]], as.name("export_and_verify"))
    },
    r_test_calls(parse(script), "<-")
  )[[1L]]
  scope <- new.env(parent = globalenv())
  eval(assignment, scope)
  cells <- c("c2", "001", "c1")
  expression <- Matrix::Matrix(
    matrix(1:6, nrow = 2, dimnames = list(c("g1", "g2"), cells)),
    sparse = TRUE
  )
  coordinates <- data.frame(x = 1:3, y = 3:1, row.names = cells)
  object <- Cerebro$new()
  object$setExpression(expression)
  object$setMetaData(data.frame(cell_barcode = cells, sample = "s"))
  object$addSpatialData(
    "fov",
    list(
      coordinates = coordinates,
      expression = expression
    )
  )
  scope$exportFromSeurat <- function(..., file) saveCerebro(object, file)
  path <- tempfile(fileext = ".crb")
  on.exit(unlink(path), add = TRUE)
  bounds <- list(xmin = 0, xmax = 3, ymin = 0, ymax = 3)
  # The builder transports the URI; it does not decode image pixels.
  for (image in list(NULL, "data:image/png;base64,fixture")) {
    out <- suppressMessages(scope$export_and_verify(
      NULL,
      "RNA",
      "sample",
      path,
      "fixture",
      "mm",
      image = image,
      image_bounds = bounds
    ))
    data <- out$getSpatialData("fov")
    expect_identical(data$coordinates, coordinates)
    expect_identical(data$expression, expression)
    expect_identical(readBin(path, "raw", 4L), as.raw(c(11, 14, 10, 193)))
    if (is.null(image)) {
      expect_length(data$histology_images, 0L)
    } else {
      expect_identical(names(data$histology_images), "Tissue background")
      payload <- data$histology_images[[1L]]
      expect_identical(payload$histology_image, image)
      expect_identical(payload$histology_image_bounds, unlist(bounds))
    }
  }
})
