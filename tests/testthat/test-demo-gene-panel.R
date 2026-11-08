panel_script <- testthat::test_path("..", "..", "data-raw", "demo_gene_panel.R")

test_that("demo panels retain protected rare genes, cells and exact values", {
  skip_if_not(file.exists(panel_script))
  e <- new.env(parent = globalenv())
  sys.source(panel_script, e)
  x <- Cerebro$new()
  x$expression <- Matrix::Matrix(
    matrix(
      c(
        0,
        0,
        0,
        0,
        1,
        0,
        0,
        0,
        0.001,
        0,
        0,
        0,
        0,
        pi,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      nrow = 5,
      byrow = TRUE,
      dimnames = list(
        c("zero", "Plp1", "MoranRare", "variable", "unused"),
        c("003", "c2", "c1", "004")
      )
    ),
    sparse = TRUE
  )
  x$meta_data <- data.frame(cell_barcode = colnames(x$expression), nGene = 5L)
  x$projections <- list(umap = data.frame(x = 1:4, y = 4:1))
  x$trekker <- list(moran = list(list(gene = "MoranRare")), evidence = "image")
  x$spatial <- list(
    fov = list(
      expression = x$expression[, 4:1],
      coordinates = data.frame(x = 4:1),
      image = "unchanged"
    )
  )
  y <- e$demo_gene_panel(x, 3L)
  expect_identical(rownames(y$expression), c("Plp1", "MoranRare", "variable"))
  expect_identical(y$expression, x$expression[2:4, , drop = FALSE])
  expect_identical(y$spatial$fov$expression, x$spatial$fov$expression[2:4, ])
  expect_identical(y$spatial$fov$coordinates, x$spatial$fov$coordinates)
  expect_identical(y$meta_data, x$meta_data)
  expect_identical(y$projections, x$projections)
  expect_identical(y$trekker, x$trekker)
  expect_identical(e$demo_gene_panel(y, 3L), y)
  expect_equal(nrow(x$expression), 5L)
  expect_error(e$demo_gene_panel(x, 1L), "Protected genes")
  ## Genes named by an analysis table are protected like the marker genes.
  x$marker_genes <- list(
    method = list(group = data.frame(gene = c("unused", "nowhere"), p = 1:2))
  )
  z <- e$demo_gene_panel(x, 4L)
  expect_true("unused" %in% rownames(z$expression))
  expect_identical(
    z$parameters$demo_gene_panel$protected_genes,
    c("Plp1", "MoranRare", "unused")
  )
  expect_identical(z$marker_genes, x$marker_genes)
  x$marker_genes <- list()
  x$spatial$fov$expression <- x$expression[1:2, ]
  expect_error(e$demo_gene_panel(x, 3L), "Spatial expression")
  expect_equal(nrow(x$expression), 5L)
})

test_that("shipped demo panels retain usable sparse expression and spatial identities", {
  files <- list.files(
    system.file("extdata/examples", package = "CerebroNexus"),
    pattern = "\\.crb$",
    full.names = TRUE
  )
  ## The package tarball ships three of them; the larger demos are downloaded.
  expect_true(all(
    c("example.crb", "demo_full_tcr_bcr.crb", "demo_spatial.crb") %in%
      basename(files)
  ))
  for (file in files) {
    x <- readCerebro(file)
    expect_s4_class(x$expression, "sparseMatrix")
    expect_lte(nrow(x$expression), 100L)
    expect_identical(
      colnames(x$expression),
      as.character(x$meta_data$cell_barcode)
    )
    for (s in x$spatial) {
      expect_identical(rownames(s$expression), rownames(x$expression))
      expect_true(all(colnames(s$expression) %in% colnames(x$expression)))
    }
    if (length(x$trekker$moran)) {
      expected <- vapply(x$trekker$moran, function(m) m$gene, character(1))
      expect_true(all(expected %in% rownames(x$expression)))
      expect_true(all(
        c("Plp1", "Mbp", "Gad1", "Snap25") %in% rownames(x$expression)
      ))
    }
  }
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
  sys.source(panel_script, scope)
  cells <- c("c2", "001", "c1")
  expression <- Matrix::Matrix(
    matrix(
      seq_len(1502L * 3L),
      nrow = 1502L,
      dimnames = list(c("Plp1", "Gad1", paste0("g", seq_len(1500L))), cells)
    ),
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
    expect_equal(nrow(data$expression), 100L)
    expect_true(all(c("Plp1", "Gad1") %in% rownames(data$expression)))
    expect_identical(
      data$expression,
      expression[rownames(data$expression), , drop = FALSE]
    )
    expect_identical(out$expression, data$expression)
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
