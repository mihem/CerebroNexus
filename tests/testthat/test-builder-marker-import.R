builder_repo_source("marker_import.R", local = environment())

test_that("Marker import module exposes its inventory contract", {
  expect_true(exists("builder_marker_import_inventory", mode = "function"))
})

test_that("Marker import inventories delimited files and workbook sheets", {
  skip_if_not(exists("builder_marker_import_inventory", mode = "function"))
  skip_if_not_installed("writexl")
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  xlsx <- tempfile(fileext = ".xlsx")
  utils::write.csv(
    data.frame(gene = c("CD3D", "IL7R"), score = c(3, 2)),
    csv,
    row.names = FALSE
  )
  utils::write.table(
    data.frame(gene = "MS4A1", score = 4),
    tsv,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  writexl::write_xlsx(
    list(B_cells = data.frame(gene = "CD79A"), NK = data.frame(gene = "NKG7")),
    xlsx
  )

  sources <- builder_marker_import_inventory(
    c(csv, tsv, xlsx),
    c("T_cells.csv", "B_cells.tsv", "immune.xlsx")
  )

  expect_length(sources, 4L)
  expect_identical(
    vapply(sources, `[[`, character(1), "source_name"),
    c("T_cells.csv", "B_cells.tsv", "B_cells", "NK")
  )
  expect_identical(vapply(sources, `[[`, integer(1), "rows"), c(2L, 1L, 1L, 1L))
  expect_true(all(vapply(
    sources,
    function(x) is.data.frame(x$raw_table),
    logical(1)
  )))
  expect_true(all(vapply(sources, function(x) is.null(x$error), logical(1))))
})

test_that("Marker import rejects unsupported, missing, empty, and oversized files", {
  skip_if_not(exists("builder_marker_import_inventory", mode = "function"))
  missing <- builder_marker_import_inventory("/not/a/file.csv", "missing.csv")[[
    1L
  ]]
  expect_identical(missing$error, "file_not_found")

  unsupported_path <- tempfile(fileext = ".json")
  writeLines("{}", unsupported_path)
  unsupported <- builder_marker_import_inventory(
    unsupported_path,
    "markers.json"
  )[[1L]]
  expect_identical(unsupported$error, "unsupported_format")

  empty <- tempfile(fileext = ".csv")
  writeLines("gene,score", empty)
  unusable <- builder_marker_import_inventory(empty, "empty.csv")[[1L]]
  expect_identical(unusable$error, "empty_table")

  too_large <- builder_marker_import_inventory(
    unsupported_path,
    "markers.csv",
    sizes = BUILDER_MARKER_IMPORT_MAX_BYTES + 1
  )[[1L]]
  expect_identical(too_large$error, "file_too_large")
})

test_that("single-cluster guesses remain unresolved until explicitly confirmed", {
  skip_if_not(exists("builder_marker_import_inventory", mode = "function"))
  source <- builder_marker_import_source(
    "T_cells.csv",
    NULL,
    data.frame(gene = c("CD3D", "IL7R"), score = c(3, 2))
  )
  known <- c("T cells", "B cells", "NK")

  expect_identical(
    builder_marker_import_infer_level(source$file_name, source$sheet, known),
    "T cells"
  )
  guessed <- builder_marker_import_map_single(
    source,
    "cell_type",
    "T cells",
    known,
    confirmed = FALSE
  )
  expect_false(builder_marker_import_source_ready(guessed))
  expect_identical(guessed$status, "confirmation_required")

  confirmed <- builder_marker_import_map_single(
    source,
    "cell_type",
    "B cells",
    known,
    confirmed = TRUE
  )
  expect_true(builder_marker_import_source_ready(confirmed))
  expect_identical(names(confirmed$table)[[1L]], "cell_type")
  expect_identical(unique(confirmed$table$cell_type), "B cells")
})

test_that("multi-cluster mapping validates the selected column and known levels", {
  skip_if_not(exists("builder_marker_import_inventory", mode = "function"))
  source <- builder_marker_import_source(
    "all.csv",
    NULL,
    data.frame(cluster = c("T", "B"), gene = c("CD3D", "MS4A1"))
  )

  mapped <- builder_marker_import_map_multiple(
    source,
    "cell_type",
    "cluster",
    c("T", "B", "NK")
  )
  expect_true(builder_marker_import_source_ready(mapped))
  expect_identical(names(mapped$table)[[1L]], "cell_type")
  expect_identical(mapped$levels, c("T", "B"))

  unknown <- builder_marker_import_map_multiple(
    source,
    "cell_type",
    "cluster",
    c("T", "NK")
  )
  expect_false(builder_marker_import_source_ready(unknown))
  expect_identical(unknown$error, "unknown_cluster")

  missing <- builder_marker_import_map_multiple(
    source,
    "cell_type",
    "not_a_column",
    c("T", "B")
  )
  expect_identical(missing$error, "missing_cluster_column")
})

test_that("validation blocks unresolved and duplicate sources but allows partial coverage", {
  skip_if_not(exists("builder_marker_import_inventory", mode = "function"))
  known <- c("T", "B", "NK")
  make_source <- function(name, level, confirmed = TRUE) {
    builder_marker_import_map_single(
      builder_marker_import_source(
        name,
        NULL,
        data.frame(gene = paste0("gene-", level))
      ),
      "cluster",
      level,
      known,
      confirmed = confirmed
    )
  }

  partial <- builder_marker_import_validate(
    method = "Scanpy Wilcoxon",
    group = "cluster",
    sources = list(make_source("T.csv", "T"), make_source("B.csv", "B")),
    known_levels = known,
    existing_methods = "cerebro_seurat"
  )
  expect_true(partial$ready)
  expect_identical(partial$coverage$missing, "NK")
  expect_match(partial$warnings, "NK", fixed = TRUE)

  unresolved <- builder_marker_import_validate(
    method = "Scanpy Wilcoxon",
    group = "cluster",
    sources = list(make_source("T.csv", "T", confirmed = FALSE)),
    known_levels = known
  )
  expect_false(unresolved$ready)
  expect_true("unresolved_sources" %in% unresolved$errors)

  duplicate <- builder_marker_import_validate(
    method = "Scanpy Wilcoxon",
    group = "cluster",
    sources = list(make_source("T-1.csv", "T"), make_source("T-2.csv", "T")),
    known_levels = known
  )
  expect_false(duplicate$ready)
  expect_true("duplicate_cluster_assignment" %in% duplicate$errors)

  collision <- builder_marker_import_validate(
    method = "cerebro_seurat",
    group = "cluster",
    sources = list(make_source("T.csv", "T")),
    known_levels = known,
    existing_methods = "cerebro_seurat"
  )
  expect_false(collision$ready)
  expect_true("duplicate_method" %in% collision$errors)
})

test_that("draft preparation guesses mappings but requires row confirmation", {
  sources <- list(
    builder_marker_import_source("T.csv", NULL, data.frame(gene = "CD3D")),
    builder_marker_import_source(
      "all.csv",
      NULL,
      data.frame(cluster = c("T", "B"), gene = c("CD3D", "MS4A1"))
    )
  )
  sources[[1L]]$id <- "source-001"
  sources[[2L]]$id <- "source-002"

  draft <- builder_marker_import_new_draft(
    id = "marker-import-1",
    method = "Scanpy Wilcoxon",
    group = "cluster",
    sources = sources,
    known_levels = c("T", "B", "NK")
  )

  expect_identical(draft$sources[[1L]]$mapping, "single")
  expect_identical(draft$sources[[1L]]$cluster, "T")
  expect_identical(draft$sources[[2L]]$mapping, "multiple")
  expect_identical(draft$sources[[2L]]$cluster_column, "cluster")
  expect_false(draft$validation$ready)

  draft <- builder_marker_import_confirm_source(
    draft,
    "source-001",
    mode = "single",
    value = "T"
  )
  draft <- builder_marker_import_confirm_source(
    draft,
    "source-002",
    mode = "multiple",
    value = "cluster"
  )
  expect_true(draft$validation$ready)
  expect_identical(draft$validation$coverage$missing, "NK")
})

test_that("frozen imports keep only validated safe source fields", {
  source <- builder_marker_import_map_single(
    builder_marker_import_source(
      "T.csv",
      NULL,
      data.frame(gene = c("CD3D", "IL7R"), score = c(4, 3))
    ),
    "cluster",
    "T",
    c("T", "B"),
    confirmed = TRUE
  )
  source$id <- "source-001"
  source$datapath <- "/tmp/private-upload-path.csv"
  record <- list(
    id = "marker-import-1",
    method = "Scanpy Wilcoxon",
    group = "cluster",
    known_levels = c("T", "B"),
    existing_methods = character(),
    sources = list(source),
    validation = list(
      ready = TRUE,
      errors = character(),
      coverage = list(covered = "T", missing = "B"),
      warnings = "No imported rows for: B"
    ),
    ready = TRUE
  )

  frozen <- builder_freeze_marker_imports(list(record))

  expect_length(frozen, 1L)
  expect_named(
    frozen[[1L]],
    c("id", "method", "group", "sources", "coverage", "warnings", "ready")
  )
  expect_null(frozen[[1L]]$sources[[1L]]$raw_table)
  expect_null(frozen[[1L]]$sources[[1L]]$datapath)
  expect_true(is.data.frame(frozen[[1L]]$sources[[1L]]$table))
  expect_false(grepl(
    "private-upload-path",
    paste(capture.output(str(frozen)), collapse = ""),
    fixed = TRUE
  ))
})

test_that("attaching imports preserves existing methods and rejects collisions", {
  skip_if_not_installed("SeuratObject")
  object <- SeuratObject::pbmc_small
  object@misc$marker_genes <- list(cerebro_seurat = list())
  source <- builder_marker_import_map_single(
    builder_marker_import_source(
      "T.csv",
      NULL,
      data.frame(gene = c("CD3D", "IL7R"), score = c(4, 3))
    ),
    "cluster",
    "T",
    c("T", "B"),
    confirmed = TRUE
  )
  record <- list(
    id = "marker-import-1",
    method = "Scanpy Wilcoxon",
    group = "cluster",
    sources = list(source),
    coverage = list(covered = "T", missing = "B"),
    warnings = "No imported rows for: B",
    ready = TRUE
  )

  got <- builder_attach_marker_imports(object, list(record))

  expect_identical(
    names(got@misc$marker_genes),
    c("cerebro_seurat", "Scanpy Wilcoxon")
  )
  expect_identical(
    got@misc$marker_genes[["Scanpy Wilcoxon"]][["cluster"]]$cluster,
    c("T", "T")
  )
  expect_error(
    builder_attach_marker_imports(got, list(record)),
    "already exists"
  )
})
