builder_task5_source <- function(file, local = parent.frame()) {
  relative <- file
  path <- builder_test_inst_path(relative)
  if (!file.exists(path)) {
    path <- system.file(relative, package = "CerebroNexus")
  }
  if (file.exists(path)) {
    sys.source(path, envir = local)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

builder_task5_source_runtime <- function(local = parent.frame()) {
  files <- c(
    file.path("viewer", "core", "spatial_coordinate_contract.R"),
    file.path("builder", "spatial.R"),
    file.path("builder", "profile.R"),
    file.path("builder", "preview.R")
  )
  invisible(vapply(
    files,
    builder_task5_source,
    logical(1),
    local = local
  ))
}

builder_task5_source_runtime()

test_that("exact cell matching is barcode-aware and reorderable", {
  expected <- c("cell-a", "cell-b", "cell-c")
  match <- builder_match_cells(rev(expected), expected, mode = "exact")

  expect_true(match$valid)
  expect_identical(match$relation, "full")
  expect_false(match$order_matches)
  expect_identical(match$reorder_index, c(3L, 2L, 1L))
  expect_identical(match$matched_ids, expected)
  expect_identical(match$input_index, c(3L, 2L, 1L))
})

test_that("exact cell matching rejects damage on either identity side", {
  expected <- c("cell-a", "cell-b", "cell-c")
  cases <- list(
    missing = list(ids = expected[-1L], expected = expected),
    extra = list(ids = c(expected, "outside"), expected = expected),
    wrong_same_count = list(
      ids = c(expected[-1L], "outside"),
      expected = expected
    ),
    duplicate_input = list(
      ids = c("cell-a", "cell-a", "cell-c"),
      expected = expected
    ),
    duplicate_expected = list(
      ids = expected,
      expected = c("cell-a", "cell-a", "cell-c")
    ),
    blank_input = list(ids = c("cell-a", "", "cell-c"), expected = expected),
    missing_input = list(
      ids = c("cell-a", NA_character_, "cell-c"),
      expected = expected
    ),
    blank_expected = list(ids = expected, expected = c("cell-a", "", "cell-c"))
  )

  for (name in names(cases)) {
    got <- builder_match_cells(
      cases[[name]]$ids,
      cases[[name]]$expected,
      mode = "exact"
    )
    expect_false(got$valid, info = name)
    expect_identical(got$relation, "invalid", info = name)
  }
})

test_that("spatial subset matching records and excludes outside rows", {
  expected <- c("cell-a", "cell-b", "cell-c")
  match <- builder_match_cells(
    c("cell-b", "outside", "cell-a"),
    expected,
    mode = "subset"
  )

  expect_identical(match$relation, "partial")
  expect_identical(match$input_index, c(3L, 1L))

  expect_false(
    builder_match_cells(
      c("cell-a", "cell-a"),
      expected,
      mode = "subset"
    )$valid
  )
  expect_false(
    builder_match_cells(
      c("", "cell-a"),
      expected,
      mode = "subset"
    )$valid
  )
  expect_false(
    builder_match_cells("outside", expected, mode = "subset")$valid
  )
})

builder_preview_shuffled_object <- function() {
  object <- SeuratObject::pbmc_small
  reduction <- "pca"
  cells <- SeuratObject::Cells(object)

  metadata <- object@meta.data[
    rev(seq_len(nrow(object@meta.data))),
    ,
    drop = FALSE
  ]
  methods::slot(object, "meta.data", check = FALSE) <- metadata

  reductions <- methods::slot(object, "reductions")
  embeddings <- SeuratObject::Embeddings(reductions[[reduction]])
  order <- c(
    seq(2L, nrow(embeddings), by = 2L),
    seq(1L, nrow(embeddings), by = 2L)
  )
  methods::slot(
    reductions[[reduction]],
    "cell.embeddings",
    check = FALSE
  ) <- embeddings[order, , drop = FALSE]
  methods::slot(object, "reductions", check = FALSE) <- reductions

  list(object = object, cells = cells, embeddings = embeddings)
}

test_that("projection preview labels follow barcodes across shuffled rows", {
  skip_if_not_installed("SeuratObject")
  fixture <- builder_preview_shuffled_object()

  frame <- builder_preview_frame(
    fixture$object,
    reduction = "pca",
    group = "groups",
    max_cells = length(fixture$cells)
  )

  expect_identical(frame$cell_barcode, fixture$cells)
  expect_identical(
    frame$group,
    as.character(fixture$object@meta.data[fixture$cells, "groups"])
  )
  expect_identical(
    frame$x,
    as.numeric(fixture$embeddings[fixture$cells, 1L])
  )
  expect_identical(
    frame$y,
    as.numeric(fixture$embeddings[fixture$cells, 2L])
  )
})

test_that("projection preview rejects duplicate component identities", {
  skip_if_not_installed("SeuratObject")
  fixture <- builder_preview_shuffled_object()

  duplicate_metadata <- fixture$object
  metadata <- duplicate_metadata@meta.data
  metadata_ids <- rownames(metadata)
  metadata_ids[[length(metadata_ids)]] <- metadata_ids[[1L]]
  attr(metadata, "row.names") <- metadata_ids
  methods::slot(duplicate_metadata, "meta.data", check = FALSE) <- metadata
  expect_error(
    builder_preview_frame(
      duplicate_metadata,
      "pca",
      "groups",
      max_cells = 1L
    ),
    "duplicate"
  )

  duplicate_reduction <- fixture$object
  reductions <- methods::slot(duplicate_reduction, "reductions")
  embeddings <- SeuratObject::Embeddings(reductions[["pca"]])
  rownames(embeddings)[[nrow(embeddings)]] <- rownames(embeddings)[[1L]]
  methods::slot(
    reductions[["pca"]],
    "cell.embeddings",
    check = FALSE
  ) <- embeddings
  methods::slot(duplicate_reduction, "reductions", check = FALSE) <- reductions
  expect_error(
    builder_preview_frame(
      duplicate_reduction,
      "pca",
      "groups",
      max_cells = 1L
    ),
    "duplicate"
  )

  outside_reduction <- fixture$object
  reductions <- methods::slot(outside_reduction, "reductions")
  embeddings <- SeuratObject::Embeddings(reductions[["pca"]])
  rownames(embeddings)[[nrow(embeddings)]] <- "outside"
  methods::slot(
    reductions[["pca"]],
    "cell.embeddings",
    check = FALSE
  ) <- embeddings
  methods::slot(outside_reduction, "reductions", check = FALSE) <- reductions
  expect_error(
    builder_preview_frame(
      outside_reduction,
      "pca",
      "groups",
      max_cells = 1L
    ),
    "identity"
  )
})

test_that("projection downsampling preserves matched barcode identity", {
  skip_if_not_installed("SeuratObject")
  fixture <- builder_preview_shuffled_object()
  full <- builder_preview_frame(
    fixture$object,
    reduction = "pca",
    group = "groups",
    max_cells = length(fixture$cells)
  )
  sampled <- builder_preview_frame(
    fixture$object,
    reduction = "pca",
    group = "groups",
    max_cells = 17L
  )
  repeated <- builder_preview_frame(
    fixture$object,
    reduction = "pca",
    group = "groups",
    max_cells = 17L
  )

  expect_identical(sampled, repeated)
  expect_identical(
    sampled[, c("x", "y", "group"), drop = FALSE],
    full[
      match(sampled$cell_barcode, full$cell_barcode),
      c("x", "y", "group"),
      drop = FALSE
    ]
  )
})

test_that("preview sampling leaves the caller's random numbers alone", {
  withr::local_seed(123L)
  expected <- stats::runif(3L)
  set.seed(123L)
  first <- .builder_alignment_sample(500L, 20L)
  expect_identical(stats::runif(3L), expected)
  expect_identical(.builder_alignment_sample(500L, 20L), first)
  expect_length(first, 20L)

  rm(".Random.seed", envir = globalenv())
  .builder_alignment_sample(500L, 20L)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("matching ids gives what the set operations on each id vector give", {
  set.seed(7)
  pool <- c(paste0("cell-", 1:40), "", NA)
  for (trial in 1:50) {
    ids <- sample(pool, sample(0:60, 1L), replace = TRUE)
    expected <- sample(pool, sample(1:60, 1L), replace = TRUE)
    got <- builder_match_cells(ids, expected, "subset")
    usable <- ids[!(is.na(ids) | !nzchar(ids))]
    expected_usable <- expected[!(is.na(expected) | !nzchar(expected))]
    matched <- unique(expected_usable[expected_usable %in% usable])
    denominator <- length(unique(expected_usable))
    expect_identical(
      got$missing,
      setdiff(unique(expected_usable), unique(usable))
    )
    expect_identical(
      got$extra,
      setdiff(unique(usable), unique(expected_usable))
    )
    expect_identical(got$matched_ids, matched)
    expect_identical(got$input_index, match(matched, as.character(ids)))
    expect_identical(
      got$coverage,
      if (!denominator) {
        1
      } else {
        length(intersect(unique(usable), unique(expected_usable))) / denominator
      }
    )
  }
})
