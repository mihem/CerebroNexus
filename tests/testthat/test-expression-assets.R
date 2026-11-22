test_that("direct expression assets round-trip exact values in canonical order", {
  root <- withr::local_tempdir()
  cells <- paste0("c", 1:20)
  values <- rbind(
    sparse = c(1 + 2^-40, rep(0, 16), Inf, NA_real_, -3),
    dense = seq_len(20) / 7,
    zero = numeric(20)
  )
  colnames(values) <- cells
  object <- Cerebro$new()
  object$setExpression(values[, 20:1])
  object$setMetaData(data.frame(cell_barcode = cells))
  file <- file.path(root, "test.crb")
  saveCerebro(object, file)
  buildViewerPack(file, viewer_binary = "always", expression_genes = NULL)
  check <- validateViewerPack(file)
  expect_true(check$valid)
  rows <- check$manifest$expression_rows
  expect_identical(rows$gene, rownames(values))
  pack <- paste0(sub("[.]crb$", "", file), ".viewer")
  for (i in seq_len(nrow(rows))) {
    con <- gzfile(file.path(pack, rows$path[[i]]), "rb")
    bytes <- readBin(con, "raw", n = 10000L)
    close(con)
    header <- readBin(bytes[5:16], "integer", n = 3L, size = 4L, endian = "little")
    expect_identical(header[[1L]], 20L)
    count <- header[[2L]]
    sparse <- header[[3L]] == 1L
    offset <- 16L + if (sparse) count * 4L + (8L - count * 4L %% 8L) %% 8L else 0L
    restored <- numeric(20)
    if (count) {
      index <- if (sparse) readBin(bytes[17:(16 + count * 4)], "integer",
        n = count, size = 4L, endian = "little") + 1L else seq_len(20)
      restored[index] <- readBin(bytes[(offset + 1):length(bytes)], "double",
        n = count, size = 8L, endian = "little")
    }
    expect_identical(restored, unname(values[i, ]))
  }
  writeLines(jsonlite::toJSON(list(
    root = pack, rows = rows,
    fingerprint = check$manifest$dataset_fingerprint,
    cells = check$manifest$cell_order_fingerprint
  ), auto_unbox = TRUE, dataframe = "rows"), file.path(root, "input.json"))
  expect_node_fixture(
    "expression-assets.cjs", viewer_test_path("www", "expression-assets.js"),
    file.path(root, "input.json"), pass = "expression assets OK"
  )
  expect_error(buildViewerPack(file, viewer_binary = "always", overwrite = TRUE,
    expression_genes = "missing"), "available gene")
  expect_true(validateViewerPack(file)$valid)
})

test_that("the expression index names every exported gene, also for one gene", {
  root <- withr::local_tempdir()
  cells <- paste0("c", 1:4)
  values <- rbind(G1 = c(0, 1.5, 0, 2), G2 = c(3, 0, 0, 0))
  colnames(values) <- cells
  object <- Cerebro$new()
  object$setExpression(values)
  object$setMetaData(data.frame(cell_barcode = cells))
  file <- file.path(root, "test.crb")
  saveCerebro(object, file)
  pack <- paste0(sub("[.]crb$", "", file), ".viewer")
  for (genes in list(NULL, "G2")) {
    buildViewerPack(file, viewer_binary = "always", expression_genes = genes,
      overwrite = TRUE)
    check <- validateViewerPack(file)
    expect_true(check$valid)
    rows <- check$manifest$expression_rows
    assets <- check$manifest$assets
    index <- jsonlite::read_json(file.path(pack, "expression/index.json"))
    expect_identical(index$protocol, "expression-index-v1")
    expect_identical(index$cells, 4L)
    expect_identical(unlist(index$genes), rows$gene)
    expect_identical(unlist(index$files), basename(rows$path))
    expect_identical(unlist(index$checksums),
      assets$checksum[match(rows$path, assets$path)])
    expect_equal(unlist(index$low), rows$low)
    expect_equal(unlist(index$high), rows$high)
    expect_identical(assets$dtype[assets$path == "expression/index.json"],
      "expression-index-v1")
  }
  buildViewerPack(file, viewer_binary = "always", overwrite = TRUE)
  expect_false(file.exists(file.path(pack, "expression/index.json")))
})

test_that("direct expression markers preserve ranges without materializing values", {
  scope <- viewer_runtime_env()
  sys.source(viewer_test_path("core", "func_color_scale.R"), scope)
  marker <- structure(NA_real_, expression_resource = list(range = c(-1.25, 7.5)))
  expect_identical(scope$expressionValueRange(marker), c(-1.25, 7.5))
})

test_that("direct expression frames and cached previews reject late fetches", {
  skip_on_cran()
  viewer_browser_test("directExpression")
})

test_that("unseen genes fetch on selection and preview before R replies", {
  skip_on_cran()
  viewer_browser_test("directExpressionUnseen")
})

test_that("the first gene of a session is fetched in parallel with R", {
  skip_on_cran()
  viewer_browser_test("directExpressionPrefetch")
})

test_that("a direct marker bypasses source reads and a failed asset falls back", {
  scope <- new.env(parent = asNamespace("shiny"))
  scope$input <- list(sidebar = "other")
  scope$expression_projection_render_key <- function() "key"
  scope$expression_projection_coordinates <- function() data.frame(x = 1:3, y = 3:1)
  scope$expressionProjectionProgressClose <- function(...) NULL
  scope$expressionProjectionProgressUpdate <- function(...) NULL
  scope$data_set <- function() NULL
  reads <- 0L
  scope$expressionProjectionCachedRow <- function(...) {
    reads <<- reads + 1L
    c(0, 1.25, 3)
  }
  scope$viewerExpressionAsset <- function(...) list(url = "gene.bin", range = c(0, 3))
  eval(viewer_r_assignment(
    viewer_test_path("gene_expression", "obj_projection_expression_levels.R"),
    "expression_projection_read_levels"
  ), scope)
  call <- function() shiny::isolate(scope$expression_projection_read_levels(
    1:3, list(), "A", "mean"
  ))
  expect_identical(attr(call(), "expression_resource")$url, "gene.bin")
  expect_identical(reads, 0L)
  scope$input$expression_projection_expression_resource_failed <- "gene.bin"
  expect_identical(call(), c(0, 1.25, 3))
  expect_identical(reads, 1L)
})
