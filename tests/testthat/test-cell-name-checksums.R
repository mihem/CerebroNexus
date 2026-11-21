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
    "xxh3-128-cell-order-v1:172c476c1d6a9824bc3a3c2e962541c9"
  )
  expect_identical(
    hash(character()),
    "xxh3-128-cell-order-v1:99aa06d3014798d86001c324468d497f"
  )
  expect_false(identical(hash(c("ab", "c")), hash(c("a", "bc"))))
  expect_false(identical(hash(c("a:1", "b")), hash(c("b", "a:1"))))
  latin1 <- iconv("caf\u00e9", from = "UTF-8", to = "latin1")
  expect_identical(hash(latin1), hash("caf\u00e9"))
  expect_identical(
    hash(c("caf\u00e9", "\u03b2:1")),
    "xxh3-128-cell-order-v1:e8b7172b67151481abddfc0b410fac42"
  )
  expect_error(hash(NA_character_), "must not contain missing values")

  ## The accelerator does not call either rlang hashing API.
  skip_if_not_installed("digest", minimum_version = "0.6.39")
  testthat::local_mocked_bindings(
    hash = function(...) stop("object hashing is not a file format"),
    hash_file = function(...) stop("the new checksum must not call rlang"),
    .package = "rlang"
  )
  expect_identical(
    hash(c("cell1", "cell2")),
    .cerebroCellNamesHash(c("cell1", "cell2"))
  )
})

test_that("the fast checksum hashes explicit raw bytes, never an R object", {
  skip_if_not_installed("digest", minimum_version = "0.6.39")
  digest_bytes <- digest::digest
  testthat::local_mocked_bindings(
    digest = function(object, algo, serialize, seed) {
      expect_identical(object, as.raw(c(0x41, 0x41, 0, 0x42, 0)))
      expect_identical(algo, "xxh3_128")
      expect_identical(serialize, FALSE)
      expect_identical(seed, 0L)
      digest_bytes(object, algo = algo, serialize = serialize, seed = seed)
    },
    .package = "digest"
  )
  expect_identical(
    .cerebroCellNamesHash(c("AA", "B")),
    "xxh3-128-cell-order-v1:f183324cf31f5adc606566c2648e3481"
  )
})

test_that("the existing-dependency fallback gives exactly the same checksum", {
  hash <- .crbContract()$.runtimeCellNamesHash
  for (cells in list(
    character(),
    c("cell1", "cell2"),
    c("AA", "B"),
    c("caf\u00e9", "\u03b2:1"),
    c("a\nb", "c")
  )) {
    expect_identical(hash(cells, .use_digest = FALSE), hash(cells))
  }
  expect_identical(
    hash(c("AA", "B"), .use_digest = FALSE),
    "xxh3-128-cell-order-v1:f183324cf31f5adc606566c2648e3481"
  )
})

test_that("ordinary checksum calculation works without digest installed", {
  contract <- .crbContract()
  rlang::local_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "digest")) {
        FALSE
      } else {
        base::requireNamespace(package, ...)
      }
    },
    .env = contract
  )
  expect_identical(
    contract$.runtimeCellNamesHash(c("AA", "B")),
    "xxh3-128-cell-order-v1:f183324cf31f5adc606566c2648e3481"
  )
  expect_true(contract$.runtimeCellNamesMatch(
    c("cell1", "cell2"),
    "xxh3-128-cell-order-v1:172c476c1d6a9824bc3a3c2e962541c9"
  ))
})

test_that("an older optional digest uses the existing-dependency fallback", {
  contract <- .crbContract()
  rlang::local_bindings(
    requireNamespace = function(...) TRUE,
    getNamespaceVersion = function(...) "0.6.38",
    .env = contract
  )
  expect_identical(
    contract$.runtimeCellNamesHash(c("AA", "B")),
    "xxh3-128-cell-order-v1:f183324cf31f5adc606566c2648e3481"
  )
})

test_that("both historical rlang formats validate without their original rlang", {
  cells <- c("cell1", "cell2")
  expect_identical(
    .legacyCerebroCellNamesHash(cells, "serialized"),
    unname(legacy_cell_hashes[[1L]])
  )
  expect_identical(
    .legacyCerebroCellNamesHash(cells, "walker"),
    unname(legacy_cell_hashes[[2L]])
  )
  ## Force both compatibility paths even on the matching rlang release.
  testthat::local_mocked_bindings(
    hash = function(...) strrep("0", 32L),
    .package = "rlang"
  )
  for (expected in legacy_cell_hashes) {
    expect_true(.legacyCerebroCellNamesMatch(cells, expected))
    expect_false(.legacyCerebroCellNamesMatch(rev(cells), expected))
    expect_false(.legacyCerebroCellNamesMatch(
      c("cell1", "foreign"),
      expected
    ))
    expect_false(.legacyCerebroCellNamesMatch(
      c("cell1", NA_character_),
      expected
    ))
  }
})

test_that("schema 5 requires a known checksum algorithm and indexed-field list", {
  read_schema <- function(schema, allow_legacy = FALSE) {
    object <- new.env(parent = emptyenv())
    object$crb_schema <- schema
    .crbContract()$.readRuntimeCrbSchema(
      object,
      "test.crb",
      allow_legacy = allow_legacy
    )
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
          "xxh3-128-cell-order-v1",
          "xxh3-128-cell-order-v2",
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
    expect_error(read_schema(old), "unsupported CRB schema")
    expect_identical(read_schema(old, allow_legacy = TRUE), old)
    if (version == 4L) {
      old$cell_indexed <- character()
      expect_error(
        read_schema(old, allow_legacy = TRUE),
        "unsupported CRB schema"
      )
    }
  }
})

test_that("raw checksums preserve cell boundaries in long vectors", {
  cells <- paste0("cell-", seq_len(65537L))
  contract <- .crbContract()
  path <- withr::local_tempfile()
  connection <- file(path, "wb")
  for (cell in cells) {
    ## Independent reference: append a zero byte to each name, not an R
    ## character-vector serialization or a newline-separated text file.
    writeBin(c(charToRaw(cell), as.raw(0)), connection)
  }
  close(connection)
  expect_identical(
    contract$.runtimeCellNamesHash(cells),
    paste0(
      "xxh3-128-cell-order-v1:",
      unname(rlang::hash_file(path))
    )
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
    .legacyCerebroCellNamesHash(cells, "walker"),
    unname(rlang::hash_file(path))
  )
})

test_that("old thin CRBs require explicit conversion before ordinary reads", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  cells <- c("cell1", "cell2")
  values <- matrix(1:4, 2L, dimnames = list(c("g1", "g2"), cells))
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root, values = values)
  runtime <- viewer_runtime_env()
  wait_for_checks <- function() {
    ## Do not rewrite a sidecar while an asynchronous validator still has its
    ## path open. On a loaded machine foreground verification can finish first.
    for (entry in as.list(runtime$.crb_h5_cell_checks)) {
      if (inherits(entry$task, "mirai")) mirai::call_mirai(entry$task)
    }
  }

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
      if (version %in% c(3L, 4L)) {
        local({
          testthat::local_mocked_bindings(
            .h5CellNames = function(...) stop("must not read H5"),
            .legacyCerebroCellNamesMatch = function(...) {
              stop("must not try legacy hashes")
            },
            .package = "CerebroNexus"
          )
          expect_error(
            readCerebro(fixture$crb),
            "portable format.*convertCerebro"
          )
          old <- runtime$read_cerebro_file(fixture$crb)
          expect_error(
            runtime$.attachExternalExpression(old, fixture$crb),
            "portable format.*convertCerebro"
          )
        })
        ## The converter refuses a genuine mismatch and leaves the CRB intact.
        crb_before <- unname(tools::md5sum(fixture$crb))
        viewer_h5_matrix(values[, 2:1], fixture$h5)
        expect_error(
          convertCerebro(fixture$crb),
          class = "cerebro_cell_names_mismatch"
        )
        expect_identical(unname(tools::md5sum(fixture$crb)), crb_before)
        viewer_h5_matrix(values, fixture$h5)
        h5_before <- unname(tools::md5sum(fixture$h5))
        convertCerebro(fixture$crb)
        expect_identical(unname(tools::md5sum(fixture$h5)), h5_before)
        payload <- .readCerebroPayload(fixture$crb)
        expect_identical(payload$crb_schema$version, 5L)
        expect_identical(
          payload$crb_schema$cell_names_hash,
          .cerebroCellNamesHash(cells)
        )
      }
      expect_identical(readCerebro(fixture$crb)$meta_data$cell_barcode, cells)
      object <- runtime$read_cerebro_file(fixture$crb)
      object <- runtime$.attachExternalExpression(object, fixture$crb)
      wait_for_checks()
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
      wait_for_checks()
      expect_error(refused$meta_data, class = "cerebro_cell_names_mismatch")
      expect_error(refused$expression, class = "cerebro_cell_names_mismatch")
      viewer_h5_matrix(values, fixture$h5)
    }
  }
})

test_that("the background check uses only portable validation", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("mirai")
  runtime <- viewer_runtime_env()
  cells <- c("cell1", "cell2")
  sidecar <- withr::local_tempfile(fileext = ".h5")
  rhdf5::h5createFile(sidecar)
  rhdf5::h5createGroup(sidecar, "expression")
  rhdf5::h5write(cells, sidecar, "expression/genes")
  rhdf5::h5closeAll()
  hashes <- .cerebroCellNamesHash(cells)
  expect_false(exists(
    ".runtimeLegacyCellNamesHash",
    envir = runtime,
    inherits = FALSE
  ))
  for (expected in c(
    hashes,
    legacy_cell_hashes,
    .cerebroCellNamesHash(rev(cells))
  )) {
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
