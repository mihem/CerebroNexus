## Fixed historical values, not hashes computed by the installed rlang. The
## regression otherwise passes on either side of the incompatible upgrade.
legacy_cell_hashes <- c(
  rlang_1_1_7 = "dde5c2143cbc410c559b0f8b1e962619",
  rlang_1_3_0 = "c785b7e383c3813f884957018613897e"
)

test_that("ordered-name checksums have fixed, portable UTF-8 bytes", {
  contract <- .crbContract()
  hash <- contract$.runtimeCellNamesHash
  expect_identical(
    hash(c("cell1", "cell2")),
    "md5-cell-order-v1:16ce54fc5b0d149c40e845c9d90e2d74"
  )
  expect_identical(
    hash(character()),
    "md5-cell-order-v1:d41d8cd98f00b204e9800998ecf8427e"
  )
  expect_false(identical(hash(c("ab", "c")), hash(c("a", "bc"))))
  expect_false(identical(hash(c("a:1", "b")), hash(c("b", "a:1"))))
  latin1 <- iconv("caf\u00e9", from = "UTF-8", to = "latin1")
  expect_identical(hash(latin1), hash("caf\u00e9"))

  ## The stable format does not call either rlang hashing API.
  testthat::local_mocked_bindings(
    hash = function(...) stop("object hashing is not a file format"),
    hash_file = function(...) stop("not the specified MD5 algorithm"),
    .package = "rlang"
  )
  expect_identical(
    hash(c("cell1", "cell2")),
    .cerebroCellNamesHash(c("cell1", "cell2"))
  )
})

test_that("both historical rlang formats validate without their original rlang", {
  contract <- .crbContract()
  cells <- c("cell1", "cell2")
  expect_identical(
    contract$.runtimeLegacyCellNamesHash(cells, "serialized"),
    unname(legacy_cell_hashes[[1L]])
  )
  expect_identical(
    contract$.runtimeLegacyCellNamesHash(cells, "walker"),
    unname(legacy_cell_hashes[[2L]])
  )
  ## Force both compatibility paths even on the matching rlang release.
  testthat::local_mocked_bindings(
    hash = function(...) strrep("0", 32L),
    .package = "rlang"
  )
  for (expected in c(legacy_cell_hashes, .cerebroCellNamesHash(cells))) {
    expect_true(contract$.runtimeCellNamesMatch(cells, expected))
    expect_false(contract$.runtimeCellNamesMatch(rev(cells), expected))
    expect_false(contract$.runtimeCellNamesMatch(
      c("cell1", "foreign"),
      expected
    ))
    expect_false(contract$.runtimeCellNamesMatch(
      c("cell1", NA_character_),
      expected
    ))
  }
})

test_that("schema 5 requires a known checksum algorithm and indexed-field list", {
  read_schema <- function(schema) {
    object <- new.env(parent = emptyenv())
    object$crb_schema <- schema
    .crbContract()$.readRuntimeCrbSchema(object, "test.crb")
  }
  schema <- list(
    version = 5L,
    cell_names_hash = .cerebroCellNamesHash(c("cell1", "cell2")),
    projection_rownames = character(),
    n_cells = 2L,
    cell_indexed = character()
  )
  expect_identical(read_schema(schema), schema)
  invalid <- list(
    untagged = utils::modifyList(
      schema,
      list(cell_names_hash = legacy_cell_hashes[[1L]])
    ),
    unknown_algorithm = utils::modifyList(
      schema,
      list(
        cell_names_hash = sub(
          "md5-cell-order-v1",
          "md5-cell-order-v2",
          schema$cell_names_hash
        )
      )
    ),
    missing_indexed = schema[setdiff(names(schema), "cell_indexed")],
    null_indexed = utils::modifyList(
      schema,
      list(cell_indexed = NULL),
      keep.null = TRUE
    ),
    unknown_indexed = utils::modifyList(schema, list(cell_indexed = "unknown"))
  )
  for (broken in invalid) {
    expect_error(read_schema(broken), "unsupported CRB schema")
  }
  for (version in c(3L, 4L)) {
    old <- schema
    old$version <- version
    expect_error(read_schema(old), "unsupported CRB schema")
    old$cell_names_hash <- legacy_cell_hashes[[1L]]
    old$cell_indexed <- if (version == 4L) "immune_repertoire" else NULL
    expect_identical(read_schema(old), old)
    if (version == 4L) {
      old$cell_indexed <- character()
      expect_error(read_schema(old), "unsupported CRB schema")
    }
  }
})

test_that("streaming checksums preserve names across a block boundary", {
  cells <- paste0("cell-", seq_len(65537L))
  contract <- .crbContract()
  path <- withr::local_tempfile()
  writeBin(
    charToRaw(paste0(nchar(cells, type = "bytes"), ":", cells, collapse = "")),
    path
  )
  expect_identical(
    contract$.runtimeCellNamesHash(cells),
    paste0("md5-cell-order-v1:", unname(tools::md5sum(path)))
  )
  ## A character-vector walker reference with one name per write, independent
  ## of the production implementation's vectorized interleaving.
  path <- withr::local_tempfile()
  connection <- file(path, "wb")
  size_words <- if (.Platform$endian == "little") {
    c(65537L, 0L)
  } else {
    c(0L, 65537L)
  }
  writeBin(c(16L, 0L, size_words), connection)
  for (cell in cells) {
    writeBin(c(0L, 0L, nchar(cell, type = "bytes")), connection)
    writeBin(charToRaw(cell), connection)
  }
  close(connection)
  expect_identical(
    contract$.runtimeLegacyCellNamesHash(cells, "walker"),
    unname(rlang::hash_file(path))
  )
})

test_that("package, standalone Viewer and pack checks accept legacy CRBs", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  cells <- c("cell1", "cell2")
  values <- matrix(1:4, 2L, dimnames = list(c("g1", "g2"), cells))
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root, values = values)
  runtime <- viewer_runtime_env()

  for (version in c(3L, 4L, 5L)) {
    hashes <- if (version == 5L) {
      .cerebroCellNamesHash(cells)
    } else {
      legacy_cell_hashes
    }
    for (expected in hashes) {
      payload <- .readCerebroPayload(fixture$crb)
      payload$crb_schema$version <- version
      payload$crb_schema$cell_names_hash <- unname(expected)
      if (version == 3L) {
        payload$crb_schema$cell_indexed <- NULL
      }
      if (version == 4L) {
        payload$crb_schema$cell_indexed <- "immune_repertoire"
      }
      if (version == 5L) {
        payload$crb_schema$cell_indexed <- character()
      }
      saveRDS(payload, fixture$crb)
      expect_identical(readCerebro(fixture$crb)$meta_data$cell_barcode, cells)
      object <- runtime$read_cerebro_file(fixture$crb)
      object <- runtime$.attachExternalExpression(object, fixture$crb)
      expect_identical(object$meta_data$cell_barcode, cells)
      expect_equal(as.matrix(object$expression), values)

      ## The pack's cell-order asset must take the same compatibility path.
      pack <- file.path(root, "pack")
      dir.create(
        file.path(pack, "common"),
        recursive = TRUE,
        showWarnings = FALSE
      )
      asset <- "common/cell_order.qs2"
      qs2::qs_save(cells, file.path(pack, asset))
      manifest <- list(n_cells = 2L, assets = data.frame(path = asset))
      expect_true(.viewerPackMatchesThinCrb(
        fixture$crb,
        pack,
        manifest,
        payload
      ))
      qs2::qs_save(rev(cells), file.path(pack, asset))
      expect_false(.viewerPackMatchesThinCrb(
        fixture$crb,
        pack,
        manifest,
        payload
      ))

      viewer_h5_matrix(values[, 2:1], fixture$h5)
      expect_error(
        readCerebro(fixture$crb),
        class = "cerebro_cell_names_mismatch"
      )
      refused <- runtime$read_cerebro_file(fixture$crb)
      refused <- runtime$.attachExternalExpression(refused, fixture$crb)
      expect_error(refused$meta_data, class = "cerebro_cell_names_mismatch")
      expect_error(refused$expression, class = "cerebro_cell_names_mismatch")
      viewer_h5_matrix(values, fixture$h5)
    }
  }
})

test_that("the background check uses portable and legacy validation", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("mirai")
  runtime <- viewer_runtime_env()
  cells <- c("cell1", "cell2")
  sidecar <- withr::local_tempfile(fileext = ".h5")
  rhdf5::h5createFile(sidecar)
  rhdf5::h5createGroup(sidecar, "expression")
  rhdf5::h5write(cells, sidecar, "expression/genes")
  rhdf5::h5closeAll()
  hashes <- c(legacy_cell_hashes, .cerebroCellNamesHash(cells))
  for (expected in c(hashes, .cerebroCellNamesHash(rev(cells)))) {
    runtime$crb_h5_cell_check_start(sidecar, expected)
    key <- runtime$.crbH5CellCheckKey(sidecar, expected)
    task <- runtime$.crb_h5_cell_checks[[key]]$task
    expect_s3_class(task, "mirai")
    mirai::call_mirai(task)
    expect_identical(
      runtime$crb_h5_cells_verified(sidecar, expected),
      expected %in% hashes
    )
  }
})

test_that("an expression sidecar refusal does not disconnect the session", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      expressionProjectionProgressClose <- function(...) invisible(NULL)
      viewer_server_load(
        viewer_server_statements(
          "gene_expression",
          "obj_projection_expression_levels.R"
        ),
        scope,
        names = "expressionProjectionCheckSidecar"
      )
      value <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$read, {
        result <- expressionProjectionCheckSidecar(function() {
          .runtimeCheckCellNames(
            c("cell2", "cell1"),
            list(cell_names_hash = .runtimeCellNamesHash(c("cell1", "cell2"))),
            "test.crb"
          )
        })
        value(result)
      })
      shiny::observeEvent(input$other_dataset, value("usable"))
    },
    {
      session$flushReact()
      expect_no_error(session$setInputs(read = 1L))
      expect_false(session$isClosed())
      expect_null(value())
      session$setInputs(other_dataset = 1L)
      expect_identical(value(), "usable")
    }
  )
})
