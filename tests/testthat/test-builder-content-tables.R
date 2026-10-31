builder_table_source_runtime()

test_that("table content always returns the stable source-fact contract", {
  skip_if_not_installed("SeuratObject")
  result <- builder_profile_table_content(
    builder_table_object(),
    builder_table_context()
  )

  expect_named(
    result,
    c(
      "marker_genes",
      "most_expressed_genes",
      "mean_expression",
      "enriched_pathways",
      "trajectory",
      "gene_lists",
      "extra_material"
    ),
    ignore.order = FALSE
  )
  for (record in result) {
    expect_named(
      record,
      c(
        "detected",
        "valid",
        "normalized",
        "diagnostics",
        "requirements",
        "page_candidates"
      ),
      ignore.order = FALSE
    )
    expect_type(record$detected, "logical")
    expect_length(record$detected, 1L)
    expect_false(is.na(record$detected))
    expect_type(record$valid, "logical")
    expect_length(record$valid, 1L)
    expect_false(is.na(record$valid))
    expect_type(record$diagnostics, "character")
    expect_false(anyNA(record$diagnostics))
    expect_type(record$requirements, "character")
    expect_false(anyNA(record$requirements))
    expect_type(record$page_candidates, "character")
    expect_false(anyNA(record$page_candidates))
    expect_false(anyDuplicated(record$page_candidates) > 0L)
  }
  expect_true(all(vapply(result, function(x) x$valid, logical(1))))
  expect_true(all(!vapply(result, function(x) x$detected, logical(1))))
})

test_that("gene-list facts stay bounded and inert", {
  skip_if_not_installed("SeuratObject")
  object <- builder_table_object(list(
    gene_lists = list(
      interferon = c("STAT1", "IRF7"),
      t_cell = c("CD3D", "IL7R")
    )
  ))
  result <- builder_profile_table_content(object, builder_table_context())

  expect_true(result$gene_lists$detected)
  expect_true(result$gene_lists$valid)
  expect_identical(result$gene_lists$normalized$n_lists, 2L)
  expect_identical(result$gene_lists$normalized$n_genes, 4L)
  expect_identical(result$gene_lists$page_candidates, character())
  expect_false("genes" %in% names(result$gene_lists$normalized))
})

test_that("gene-list facts fail closed for invalid containers and members", {
  skip_if_not_installed("SeuratObject")
  cases <- list(
    unnamed = unname(list(c("STAT1"))),
    duplicated = stats::setNames(list("STAT1", "IRF7"), c("ifn", "ifn")),
    non_character = list(ifn = 1:2),
    blank_gene = list(ifn = c("STAT1", "")),
    over_budget = stats::setNames(
      rep(list("STAT1"), 513L),
      paste0("list_", seq_len(513L))
    )
  )

  for (name in names(cases)) {
    object <- builder_table_object(list(gene_lists = cases[[name]]))
    result <- builder_profile_table_content(object, builder_table_context())
    expect_true(result$gene_lists$detected, info = name)
    expect_false(result$gene_lists$valid, info = name)
    expect_identical(
      result$gene_lists$page_candidates,
      character(),
      info = name
    )
  }
})

test_that("marker facts preserve valid tables and exact empty-result sentinels", {
  skip_if_not_installed("SeuratObject")
  object <- builder_table_object(list(
    marker_genes = list(
      method_a = list(cell_type = builder_table_marker()),
      method_b = list(sample = "no_markers_found")
    )
  ))
  marker <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes

  expect_true(marker$detected)
  expect_true(marker$valid)
  expect_identical(marker$page_candidates, "marker_genes")
  expect_identical(marker$normalized$method_a$cell_type$kind, "table")
  expect_identical(
    marker$normalized$method_a$cell_type$group_column,
    "cell_type"
  )
  expect_true(marker$normalized$method_a$cell_type$group_compatible)
  expect_identical(marker$normalized$method_b$sample$kind, "empty_result")
})

test_that("marker pages require at least one compatible grouping", {
  skip_if_not_installed("SeuratObject")
  context <- builder_table_context()
  payloads <- list(
    table = builder_table_marker("absent_group"),
    empty = "no_markers_found"
  )

  for (payload_name in names(payloads)) {
    object <- builder_table_object(list(
      marker_genes = list(
        method = stats::setNames(list(payloads[[payload_name]]), "absent_group")
      )
    ))
    marker <- builder_profile_table_content(object, context)$marker_genes

    expect_false(marker$normalized$method$absent_group$group_compatible)
    expect_false(marker$valid, info = payload_name)
    expect_identical(marker$page_candidates, character(), info = payload_name)
  }
})

test_that("marker facts fail closed for malformed names and payloads", {
  skip_if_not_installed("SeuratObject")
  marker <- list(
    good = list(cell_type = builder_table_marker()),
    bad = list(sample = "no_markers")
  )
  names(marker) <- c("good", "good")
  object <- builder_table_object(list(marker_genes = marker))
  result <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes

  expect_true(result$detected)
  expect_false(result$valid)
  expect_contains(result$diagnostics, "duplicate_method_names")
  expect_identical(result$page_candidates, character())
})

test_that("most and mean expression require compatible groups and metrics", {
  skip_if_not_installed("SeuratObject")
  object <- builder_table_object(list(
    most_expressed_genes = list(
      cell_type = builder_table_most(),
      absent_group = builder_table_most("absent_group")
    ),
    mean_expression = list(
      cell_type = builder_table_mean()
    )
  ))
  result <- builder_profile_table_content(object, builder_table_context())

  expect_false(result$most_expressed_genes$valid)
  expect_identical(
    result$most_expressed_genes$page_candidates,
    "most_expressed_genes"
  )
  expect_true(
    result$most_expressed_genes$normalized$cell_type$group_compatible
  )
  expect_false(
    result$most_expressed_genes$normalized$absent_group$group_compatible
  )
  expect_contains(
    result$most_expressed_genes$diagnostics,
    "incompatible_group"
  )
  expect_true(result$mean_expression$valid)
  expect_identical(result$mean_expression$page_candidates, character())
  expect_contains(result$mean_expression$requirements, "most_expressed_genes")

  object@misc$most_expressed_genes$cell_type$pct[[1L]] <- Inf
  object@misc$mean_expression$cell_type$mean_expr[[1L]] <- NA_real_
  invalid <- builder_profile_table_content(object, builder_table_context())
  expect_false(invalid$most_expressed_genes$valid)
  expect_contains(invalid$most_expressed_genes$diagnostics, "non_finite_pct")
  expect_false(invalid$mean_expression$valid)
  expect_contains(invalid$mean_expression$diagnostics, "non_finite_mean_expr")
})

test_that("enrichment accepts Viewer sentinels without hard-coding methods", {
  skip_if_not_installed("SeuratObject")
  object <- builder_table_object(list(
    enriched_pathways = list(
      custom_method = list(cell_type = builder_table_enrichment()),
      gsva = list(sample = "no_gene_sets_enriched"),
      enrichr = list(cell_type = "no_pathways_found")
    )
  ))
  result <- builder_profile_table_content(
    object,
    builder_table_context()
  )$enriched_pathways

  expect_true(result$valid)
  expect_identical(result$page_candidates, "enriched_pathways")
  expect_identical(
    result$normalized$custom_method$cell_type$kind,
    "table"
  )
  expect_identical(result$normalized$gsva$sample$kind, "empty_result")
  expect_identical(result$normalized$enrichr$cell_type$kind, "empty_result")

  object@misc$enriched_pathways$broken <- list(
    cell_type = "nothing_enriched"
  )
  invalid <- builder_profile_table_content(
    object,
    builder_table_context()
  )$enriched_pathways
  expect_false(invalid$valid)
  expect_contains(invalid$diagnostics, "unsupported_payload")
  expect_identical(invalid$page_candidates, "enriched_pathways")
})

test_that("enrichment sentinels still require a compatible source group", {
  skip_if_not_installed("SeuratObject")
  object <- builder_table_object(list(
    enriched_pathways = list(
      gsva = list(absent_group = "no_gene_sets_enriched")
    )
  ))
  result <- builder_profile_table_content(
    object,
    builder_table_context()
  )$enriched_pathways

  expect_false(result$valid)
  expect_contains(result$diagnostics, "incompatible_group")
  expect_identical(result$page_candidates, character())
})

test_that("trajectory facts validate supported monocle2 identity and shape", {
  skip_if_not_installed("SeuratObject")
  object <- builder_table_object(list(
    trajectories = list(
      monocle2 = list(lineage = builder_table_trajectory()),
      slingshot = list(lineage = list(curves = matrix(1, 1, 1)))
    )
  ))
  trajectory <- builder_profile_table_content(
    object,
    builder_table_context()
  )$trajectory

  expect_false(trajectory$valid)
  expect_identical(trajectory$page_candidates, "trajectory")
  expect_true(trajectory$normalized$monocle2$lineage$valid)
  expect_true(trajectory$normalized$monocle2$lineage$supported)
  expect_identical(
    trajectory$normalized$monocle2$lineage$cell_relation,
    "subset"
  )
  expect_false(trajectory$normalized$slingshot$lineage$supported)
  expect_contains(trajectory$diagnostics, "unsupported_method")

  catalog <- builder_trajectory_catalog(trajectory)
  expect_length(catalog, 2L)
  expect_true(catalog[[1L]]$selectable)
  expect_identical(catalog[[1L]]$method, "monocle2")
  expect_identical(catalog[[1L]]$name, "lineage")
  expect_equal(catalog[[1L]]$coverage, 0.5)
  expect_identical(catalog[[1L]]$state_count, 2L)
  expect_false(catalog[[2L]]$selectable)
  expect_identical(
    catalog[[2L]]$reason,
    "Not supported by this Viewer version."
  )

  bad <- builder_table_trajectory(c("cell1", "ghost"))
  bad$meta$DR_1[[1L]] <- NaN
  bad$edges$weight[[1L]] <- Inf
  invalid_object <- builder_table_object(list(
    trajectories = list(monocle2 = list(lineage = bad))
  ))
  invalid <- builder_profile_table_content(
    invalid_object,
    builder_table_context()
  )$trajectory
  expect_false(invalid$valid)
  expect_identical(invalid$page_candidates, character())
  expect_contains(invalid$diagnostics, "extra_cell_barcodes")
  expect_contains(invalid$diagnostics, "non_finite_meta")
  expect_contains(invalid$diagnostics, "non_finite_edges")
})

test_that("extra material profiles tables, plots, and duplicate names", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("ggplot2")
  plot <- ggplot2::ggplot(
    data.frame(x = 1:2, y = 2:1),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()
  object <- builder_table_object(list(
    extra_material = list(
      tables = list(summary = data.frame(value = 1:2)),
      plots = list(overview = plot)
    )
  ))
  extra <- builder_profile_table_content(
    object,
    builder_table_context()
  )$extra_material

  expect_true(extra$valid)
  expect_identical(extra$page_candidates, "extra_material")
  expect_identical(extra$normalized$tables$summary$rows, 2L)
  expect_true(extra$normalized$plots$overview$recognized)

  object@misc$extra_material <- c(
    list(
      table_index = list(summary = list(workbook_name = "Renamed workbook"))
    ),
    object@misc$extra_material
  )
  renamed <- builder_profile_table_content(
    object,
    builder_table_context()
  )$extra_material
  expect_identical(renamed, extra)

  duplicated <- list(
    data.frame(value = 1),
    data.frame(value = 2)
  )
  names(duplicated) <- c("summary", "summary")
  object@misc$extra_material$tables <- duplicated
  invalid <- builder_profile_table_content(
    object,
    builder_table_context()
  )$extra_material
  expect_false(invalid$valid)
  expect_contains(invalid$diagnostics, "duplicate_material_names")
})

test_that("extra plots reject forged subclasses without dispatch", {
  skip_if_not_installed("SeuratObject")
  forged <- structure(
    list(),
    class = c("builder_table_bomb", "ggplot", "gg")
  )
  object <- builder_table_object(list(
    extra_material = list(plots = list(forged = forged))
  ))
  result <- builder_profile_table_content(
    object,
    builder_table_context()
  )$extra_material

  expect_false(result$valid)
  expect_contains(result$diagnostics, "unsupported_plot")
  expect_identical(result$page_candidates, character())
})

test_that("extra plot class summaries stay bounded", {
  skip_if_not_installed("SeuratObject")
  plot <- structure(
    list(),
    class = rep(c("ggplot", "gg"), 50000L)
  )
  object <- builder_table_object(list(
    extra_material = list(plots = list(repeated = plot))
  ))

  extra <- builder_profile_table_content(
    object,
    builder_table_context()
  )$extra_material
  summary <- extra$normalized$plots$repeated

  expect_true(extra$valid)
  expect_true(summary$recognized)
  expect_identical(summary$class_count, 100000L)
  expect_type(summary$class_preview, "character")
  expect_length(summary$class_preview, 20L)
  expect_lte(length(summary$class_preview), 20L)
  expect_identical(summary$class_truncated_count, 99980L)
  expect_false("class" %in% names(summary))
  expect_lt(as.numeric(object.size(extra)), 20000)
})

test_that("extra plot class preview values have text bounds", {
  skip_if_not_installed("SeuratObject")
  oversized_ascii <- strrep("x", 1024L * 1024L)
  oversized_multibyte <- strrep("界", 1000L)
  plot <- list()
  attr(plot, "class") <- c(
    "ggplot",
    "gg",
    oversized_ascii,
    oversized_multibyte
  )
  object <- builder_table_object(list(
    extra_material = list(plots = list(oversized = plot))
  ))

  extra <- builder_profile_table_content(
    object,
    builder_table_context()
  )$extra_material
  summary <- extra$normalized$plots$oversized

  expect_false(extra$valid)
  expect_false(summary$recognized)
  expect_identical(summary$class_count, 4L)
  expect_length(summary$class_preview, 4L)
  expect_true(all(nchar(summary$class_preview, type = "chars") <= 160L))
  expect_true(all(nchar(summary$class_preview, type = "bytes") <= 256L))
  expect_identical(summary$class_truncated_count, 2L)
  expect_identical(summary$diagnostics, "unsupported_plot")
  expect_lt(as.numeric(object.size(extra)), 20000)
})

test_that("table summaries reject ambiguous columns and stay bounded", {
  skip_if_not_installed("SeuratObject")
  duplicated <- builder_table_marker()
  names(duplicated)[[2L]] <- names(duplicated)[[1L]]
  object <- builder_table_object(list(
    marker_genes = list(method = list(cell_type = duplicated))
  ))
  invalid <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes
  expect_false(invalid$valid)
  expect_contains(invalid$diagnostics, "duplicate_table_columns")

  large <- data.frame(
    cell_type = rep(c("B", "T"), 50000L),
    gene = rep(c("MS4A1", "CD3D"), 50000L),
    avg_log2FC = rep(c(2.1, 1.7), 50000L),
    stringsAsFactors = FALSE
  )
  object@misc$marker_genes <- list(method = list(cell_type = large))
  bounded <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes
  expect_true(bounded$valid)
  expect_lt(as.numeric(object.size(bounded$normalized)), 20000)
  expect_identical(bounded$normalized$method$cell_type$rows, 100000L)

  wide_preview <- as.data.frame(
    stats::setNames(
      rep(list(c("A", "B")), 128L),
      paste0("column_", seq_len(128L))
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  object@misc$marker_genes <- list(
    method = list(cell_type = wide_preview)
  )
  preview <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes$normalized$method$cell_type
  expect_identical(preview$column_count, 128L)
  expect_length(preview$columns, 32L)
  expect_true(preview$columns_truncated)
})

test_that("profile budgets fail closed before copying unbounded keys", {
  skip_if_not_installed("SeuratObject")
  groups <- rep(list(builder_table_marker()), 513L)
  names(groups) <- paste0("group_", seq_along(groups))
  object <- builder_table_object(list(
    marker_genes = list(method = groups)
  ))
  result <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes

  expect_false(result$valid)
  expect_contains(result$diagnostics, "profile_entry_budget_exceeded")
  expect_identical(result$normalized, list())

  wide <- as.data.frame(
    stats::setNames(
      rep(list(c("A", "B")), 257L),
      paste0("column_", seq_len(257L))
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  object@misc$marker_genes <- list(method = list(cell_type = wide))
  result <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes
  expect_false(result$valid)
  expect_contains(result$diagnostics, "profile_column_budget_exceeded")

  oversized_column <- paste(rep.int("x", 1000000L), collapse = "")
  oversized <- data.frame(
    cell_type = c("B", "T"),
    value = c(1, 2),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(oversized) <- c("cell_type", oversized_column)
  object@misc$marker_genes <- list(
    method = list(cell_type = oversized)
  )
  result <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes
  expect_false(result$valid)
  expect_contains(result$diagnostics, "oversized_table_column_names")
  expect_identical(
    result$normalized$method$cell_type$columns,
    character()
  )
  expect_lt(as.numeric(object.size(result$normalized)), 20000)

  object@misc$marker_genes <- list(method = list())
  names(object@misc$marker_genes$method) <- character()
  object@misc$marker_genes$method[[paste(rep("x", 257L), collapse = "")]] <-
    builder_table_marker()
  result <- builder_profile_table_content(
    object,
    builder_table_context()
  )$marker_genes
  expect_false(result$valid)
  expect_contains(result$diagnostics, "oversized_entry_names")
  expect_identical(result$normalized, list())
})

test_that("content profiling never dispatches untrusted nested methods", {
  skip_if_not_installed("SeuratObject")
  sentinel <- new.env(parent = emptyenv())
  sentinel$called <- character()
  record <- function(name) {
    force(name)
    function(...) {
      sentinel$called <- c(sentinel$called, name)
      stop("untrusted method executed", call. = FALSE)
    }
  }
  methods <- c(
    "[[.builder_table_bomb",
    "names.builder_table_bomb",
    "length.builder_table_bomb",
    "anyNA.builder_table_bomb",
    "anyDuplicated.builder_table_bomb",
    "as.data.frame.builder_table_bomb",
    "is.finite.builder_table_bomb"
  )
  old <- lapply(methods, function(name) {
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
      get(name, envir = globalenv(), inherits = FALSE)
    } else {
      NULL
    }
  })
  names(old) <- methods
  on.exit(
    {
      for (name in methods) {
        if (is.null(old[[name]])) {
          if (exists(name, envir = globalenv(), inherits = FALSE)) {
            rm(list = name, envir = globalenv())
          }
        } else {
          assign(name, old[[name]], envir = globalenv())
        }
      }
    },
    add = TRUE
  )
  for (name in methods) {
    assign(name, record(name), envir = globalenv())
  }

  unsafe_names <- data.frame(value = 1:2)
  attr(unsafe_names, "names") <- structure(
    "value",
    class = "builder_table_bomb"
  )
  unsafe_factor <- factor(c("A", "B"))
  attr(unsafe_factor, "levels") <- structure(
    c("A", "B"),
    class = "builder_table_bomb"
  )
  unsafe_factor_table <- structure(
    list(value = unsafe_factor),
    class = "data.frame",
    row.names = c(NA_integer_, -2L)
  )
  unsafe_rows <- data.frame(value = 1:2)
  attr(unsafe_rows, "row.names") <- structure(
    c("row1", "row2"),
    class = "builder_table_bomb"
  )

  object <- builder_table_object(list(
    marker_genes = builder_table_bomb(
      list(method = list(cell_type = builder_table_marker())),
      sentinel
    ),
    trajectories = list(
      monocle2 = list(
        lineage = builder_table_bomb(
          builder_table_trajectory(),
          sentinel
        )
      )
    ),
    extra_material = list(
      tables = list(
        unsafe = builder_table_bomb(data.frame(value = 1), sentinel),
        unsafe_column = structure(
          list(value = builder_table_bomb(1:2, sentinel)),
          class = "data.frame",
          row.names = c(NA_integer_, -2L)
        ),
        unsafe_names = unsafe_names,
        unsafe_factor = unsafe_factor_table,
        unsafe_rows = unsafe_rows
      )
    )
  ))
  result <- builder_profile_table_content(object, builder_table_context())

  expect_identical(sentinel$called, character())
  expect_false(result$marker_genes$valid)
  expect_false(result$trajectory$valid)
  expect_false(result$extra_material$valid)

  outer <- builder_table_object()
  methods::slot(outer, "misc", check = FALSE) <- builder_table_bomb(
    list(
      marker_genes = list(
        method = list(
          cell_type = builder_table_marker()
        )
      )
    ),
    sentinel
  )
  outer_result <- builder_profile_table_content(
    outer,
    builder_table_context()
  )
  expect_identical(sentinel$called, character())
  expect_true(all(!vapply(outer_result, function(x) x$valid, logical(1))))
  expect_true(all(vapply(
    outer_result,
    function(x) {
      "unsafe_misc_container" %in% x$diagnostics
    },
    logical(1)
  )))
})

test_that("the tracked marker example satisfies the same contract", {
  skip_if_not_installed("SeuratObject")
  file <- builder_table_inst_path(
    "extdata",
    "examples",
    "pbmc_seurat.rds"
  )
  expect_true(file.exists(file))
  object <- readRDS(file)
  context <- builder_table_context()
  context$cells <- SeuratObject::Cells(object)
  context$features <- SeuratObject::Features(object)
  context$groups$candidates <- colnames(object@meta.data)

  marker <- builder_profile_table_content(object, context)$marker_genes
  expect_true(marker$detected)
  expect_true(marker$valid)
  expect_identical(marker$page_candidates, "marker_genes")
})

test_that("the repository monocle2 example satisfies the same contract", {
  skip_if_not_installed("SeuratObject")
  file <- builder_table_inst_path(
    "extdata",
    "examples",
    "demo_full_tcr_bcr.crb"
  )
  skip_if_not(file.exists(file))
  cerebro <- qs2::qs_read(file)
  trajectories <- get("trajectories", envir = cerebro, inherits = FALSE)
  metadata <- get("meta_data", envir = cerebro, inherits = FALSE)
  object <- builder_table_object(list(trajectories = trajectories))
  context <- builder_table_context()
  context$cells <- metadata$cell_barcode

  trajectory <- builder_profile_table_content(object, context)$trajectory
  expect_true(trajectory$detected)
  expect_true(trajectory$valid)
  expect_identical(trajectory$page_candidates, "trajectory")
  expect_identical(
    trajectory$normalized$monocle2$B_cell_maturation$cell_relation,
    "subset"
  )
})
