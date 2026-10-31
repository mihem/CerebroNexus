builder_profile_source_runtime()
builder_content_spatial_source_runtime()

test_that("optional spatial content has stable absent records", {
  skip_if_not_installed("SeuratObject")

  object <- builder_profile_pbmc()
  object@meta.data$fake_spatial_x <- seq_len(nrow(object@meta.data))
  object@meta.data$fake_spatial_y <- rev(seq_len(nrow(object@meta.data)))
  result <- builder_profile_spatial_content(
    object,
    builder_content_spatial_context(object)
  )

  expect_named(result, c("spatial", "trekker"))
  builder_content_spatial_expect_record(result$spatial)
  builder_content_spatial_expect_record(result$trekker)
  expect_false(result$spatial$detected)
  expect_true(result$spatial$valid)
  expect_identical(result$spatial$page_candidates, character())
  expect_identical(result$spatial$normalized$sections, list())
  expect_false(result$trekker$detected)
  expect_true(result$trekker$valid)
  expect_identical(result$trekker$page_candidates, character())
})

test_that("a spatial reduction is profiled as spatial content without an image", {
  skip_if_not_installed("SeuratObject")

  object <- builder_profile_pbmc()
  cells <- colnames(object)
  coordinates <- matrix(
    seq_len(length(cells) * 2L),
    nrow = length(cells),
    dimnames = list(cells, c("SPATIAL_1", "SPATIAL_2"))
  )
  object[["spatial"]] <- SeuratObject::CreateDimReducObject(
    embeddings = coordinates,
    key = "SPATIAL_",
    assay = SeuratObject::DefaultAssay(object)
  )
  result <- builder_profile_spatial_content(
    object,
    builder_content_spatial_context(object)
  )$spatial

  expect_true(result$detected)
  expect_true(result$valid)
  expect_identical(result$page_candidates, "spatial")
  expect_identical(names(result$normalized$sections), "spatial")
  expect_identical(
    result$normalized$sections$spatial$source,
    "reduction.spatial"
  )
})

test_that("real Seurat FOV sections are profiled from image coordinates", {
  skip_if_not_installed("SeuratObject")

  expected_counts <- c(
    one_section = 1L,
    three_sections = 3L,
    five_sections = 5L
  )

  for (name in names(expected_counts)) {
    section_names <- sprintf(
      "%s_%02d",
      name,
      seq_len(expected_counts[[name]])
    )
    object <- builder_content_spatial_example_object(section_names)
    result <- builder_profile_spatial_content(
      object,
      builder_content_spatial_context(object)
    )$spatial

    builder_content_spatial_expect_record(result)
    expect_true(result$detected, info = name)
    expect_true(result$valid, info = name)
    expect_identical(result$page_candidates, "spatial", info = name)
    expect_length(
      result$normalized$sections,
      expected_counts[[name]]
    )
    expect_identical(
      names(result$normalized$sections),
      names(object@images),
      info = name
    )
    for (section in result$normalized$sections) {
      expect_true(section$valid, info = name)
      expect_true(section$coordinates$dimensions >= 2L, info = name)
      expect_true(section$coordinates$finite, info = name)
      expect_true(section$barcodes$overlap_count > 0L, info = name)
      expect_true(
        section$barcodes$relation %in% c("partial", "full"),
        info = name
      )
      expect_identical(section$assay, "RNA", info = name)
      expect_true(length(section$compatible_layers) > 0L, info = name)
      expect_named(
        section$coordinate_bounds,
        c("xmin", "xmax", "ymin", "ymax")
      )
    }
  }
})

test_that("spatial section summaries stay bounded", {
  skip_if_not_installed("SeuratObject")

  object <- builder_content_spatial_example_object()
  image <- object@images[[1L]]
  object@images <- rep(list(image), 40L)
  names(object@images) <- sprintf("section_%02d", seq_len(40L))

  result <- builder_profile_spatial_content(
    object,
    builder_content_spatial_context(object)
  )$spatial

  expect_true(result$valid)
  expect_identical(result$normalized$section_count, 40L)
  expect_identical(result$normalized$valid_section_count, 40L)
  expect_length(result$normalized$sections, 32L)
  expect_identical(
    names(result$normalized$sections),
    names(object@images)[seq_len(32L)]
  )
  expect_true(result$normalized$sections_truncated)
})

test_that("invalid sections remain identifiable beyond the section preview", {
  skip_if_not_installed("SeuratObject")

  object <- builder_content_spatial_example_object()
  image <- object@images[[1L]]
  object@images <- rep(list(image), 40L)
  names(object@images) <- sprintf("section_%02d", seq_len(40L))
  methods::slot(object@images[[40L]], "assay") <- "missing"

  result <- builder_profile_spatial_content(
    object,
    builder_content_spatial_context(object)
  )$spatial

  expect_false(result$valid)
  expect_identical(result$normalized$invalid_section_count, 1L)
  expect_length(result$normalized$invalid_section_preview, 1L)
  expect_identical(
    result$normalized$invalid_section_preview[[1L]]$name,
    "section_40"
  )
  expect_true(
    "incompatible_assay" %in%
      result$normalized$invalid_section_preview[[1L]]$diagnostics
  )
  expect_false(result$normalized$invalid_section_preview_truncated)
  expect_true(result$normalized$sections_truncated)
  expect_lt(as.numeric(object.size(result)), 300000)
})

test_that("installed-layout Spatial profiling uses the shared contract", {
  skip_if_not_installed("SeuratObject")

  isolated <- new.env(parent = baseenv())
  files <- builder_content_spatial_source_runtime(isolated)
  object <- builder_content_spatial_example_object()
  result <- isolated$builder_profile_spatial_content(
    object,
    builder_content_spatial_context(object)
  )$spatial

  expect_true(all(file.exists(files)))
  expect_true(result$detected)
  expect_true(result$valid)
  expect_identical(result$page_candidates, "spatial")
  expect_identical(result$normalized$section_count, 1L)
})

test_that("spatial profiles never infer sections from numeric metadata", {
  skip_if_not_installed("SeuratObject")

  object <- builder_profile_pbmc()
  object@meta.data$x <- seq_len(nrow(object@meta.data))
  object@meta.data$y <- rev(seq_len(nrow(object@meta.data)))
  result <- builder_profile_spatial_content(
    object,
    builder_content_spatial_context(object)
  )$spatial

  expect_false(result$detected)
  expect_identical(result$normalized$sections, list())
  expect_true(
    "seurat_images_or_spatial_reduction" %in% result$requirements
  )
})

test_that("section identity and coordinates fail closed", {
  expected <- paste0("cell", seq_len(4L))
  context <- list(
    cells = expected,
    assays = list(
      RNA = list(exportable_layers = c("data", "counts"), exportable = TRUE)
    )
  )
  valid <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = expected[1:2]
  )

  profile <- .builder_profile_spatial_coordinate_record(
    "section",
    valid,
    "RNA",
    context
  )
  expect_true(profile$valid)
  expect_identical(profile$barcodes$relation, "partial")

  with_extra <- rbind(
    valid,
    data.frame(x = 5, y = 6, row.names = "outside-cell")
  )
  extra_profile <- .builder_profile_spatial_coordinate_record(
    "section",
    with_extra,
    "RNA",
    context
  )
  expect_true(extra_profile$valid)
  expect_identical(extra_profile$barcodes$relation, "partial")
  expect_identical(extra_profile$barcodes$outside_count, 1L)
  expect_true("outside_canonical_barcodes" %in% extra_profile$diagnostics)

  duplicate_profile <- .builder_profile_spatial_coordinate_record(
    "section",
    valid,
    "RNA",
    context,
    barcode_ids = rep(expected[[1L]], 2L)
  )
  expect_false(duplicate_profile$valid)
  expect_true("duplicate_barcodes" %in% duplicate_profile$diagnostics)

  non_finite <- valid
  non_finite$x[[1L]] <- Inf
  non_finite_profile <- .builder_profile_spatial_coordinate_record(
    "section",
    non_finite,
    "RNA",
    context
  )
  expect_false(non_finite_profile$valid)
  expect_true("non_finite_coordinates" %in% non_finite_profile$diagnostics)

  one_dimensional <- valid[, "x", drop = FALSE]
  one_dimension_profile <- .builder_profile_spatial_coordinate_record(
    "section",
    one_dimensional,
    "RNA",
    context
  )
  expect_false(one_dimension_profile$valid)
  expect_true(
    "fewer_than_two_coordinate_dimensions" %in%
      one_dimension_profile$diagnostics
  )

  arbitrary_numeric <- stats::setNames(valid, c("foo", "bar"))
  arbitrary_profile <- .builder_profile_spatial_coordinate_record(
    "section",
    arbitrary_numeric,
    "RNA",
    context
  )
  expect_false(arbitrary_profile$valid)
  expect_true(
    "unrecognized_coordinate_columns" %in% arbitrary_profile$diagnostics
  )

  outside <- valid
  rownames(outside) <- c("other-1", "other-2")
  outside_profile <- .builder_profile_spatial_coordinate_record(
    "section",
    outside,
    "RNA",
    context
  )
  expect_false(outside_profile$valid)
  expect_true("no_canonical_barcode_overlap" %in% outside_profile$diagnostics)

  wrong_assay_profile <- .builder_profile_spatial_coordinate_record(
    "section",
    valid,
    "missing",
    context
  )
  expect_false(wrong_assay_profile$valid)
  expect_true("incompatible_assay" %in% wrong_assay_profile$diagnostics)
})

test_that("spatial profiles consume only matched coordinate rows", {
  context <- list(
    cells = c("a", "b", "c"),
    assays = list(
      RNA = list(exportable_layers = c("data", "counts"), exportable = TRUE)
    )
  )
  outside_non_finite <- data.frame(
    x = c(20, NA_real_, 10),
    y = c(200, Inf, 100),
    row.names = c("b", "outside", "a")
  )

  profile <- .builder_profile_spatial_coordinate_record(
    "section",
    outside_non_finite,
    "RNA",
    context
  )
  expect_true(profile$valid)
  expect_true(profile$coordinates$finite)
  expect_identical(profile$barcodes$outside_count, 1L)
  expect_true("outside_canonical_barcodes" %in% profile$diagnostics)
  expect_false("non_finite_coordinates" %in% profile$diagnostics)
  expect_identical(
    profile$coordinate_bounds,
    list(xmin = 10, xmax = 20, ymin = 100, ymax = 200)
  )

  outside_extreme <- outside_non_finite
  outside_extreme["outside", ] <- c(1e12, -1e12)
  extreme_profile <- .builder_profile_spatial_coordinate_record(
    "section",
    outside_extreme,
    "RNA",
    context
  )
  expect_true(extreme_profile$valid)
  expect_identical(
    extreme_profile$coordinate_bounds,
    list(xmin = 10, xmax = 20, ymin = 100, ymax = 200)
  )
})

test_that("spatial profiles reject classed numeric coordinate columns", {
  context <- list(
    cells = c("a", "b"),
    assays = list(
      RNA = list(exportable_layers = c("data", "counts"), exportable = TRUE)
    )
  )
  coordinates <- data.frame(
    x = structure(c(1, 2), class = "builder_profile_coordinate_trap"),
    y = c(3, 4),
    row.names = context$cells
  )

  profile <- .builder_profile_spatial_coordinate_record(
    "section",
    coordinates,
    "RNA",
    context
  )
  expect_false(profile$valid)
  expect_true("non_numeric_coordinates" %in% profile$diagnostics)
})

test_that("spatial profiles reject classed coordinate table containers", {
  touched <- FALSE
  method_names <- paste0(
    c("as.data.frame.", "dim.", "dimnames.", "row.names."),
    "builder_profile_table_trap"
  )
  for (method_name in method_names) {
    assign(
      method_name,
      function(value, ...) {
        touched <<- TRUE
        stop("untrusted profile coordinate table method executed")
      },
      envir = .GlobalEnv
    )
  }
  on.exit(rm(list = method_names, envir = .GlobalEnv), add = TRUE)

  context <- list(
    cells = c("a", "b"),
    assays = list(
      RNA = list(exportable_layers = c("data", "counts"), exportable = TRUE)
    )
  )
  matrix_table <- matrix(
    c(1, 2, 3, 4),
    nrow = 2L,
    dimnames = list(context$cells, c("x", "y"))
  )
  class(matrix_table) <- "builder_profile_table_trap"
  data_frame_table <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = context$cells
  )
  class(data_frame_table) <- c(
    "builder_profile_table_trap",
    "data.frame"
  )

  for (kind in c("matrix", "data_frame")) {
    touched <- FALSE
    coordinate_table <- if (identical(kind, "matrix")) {
      matrix_table
    } else {
      data_frame_table
    }
    profile <- NULL
    expect_no_error(
      profile <- .builder_profile_spatial_coordinate_record(
        "section",
        coordinate_table,
        "RNA",
        context
      )
    )
    if (is.null(profile)) {
      next
    }
    expect_false(profile$valid, info = kind)
    expect_true(
      "invalid_coordinate_table" %in% profile$diagnostics,
      info = kind
    )
    expect_false(touched, info = kind)
  }
})

test_that("misaligned spatial barcodes fail closed without indexing errors", {
  context <- list(
    cells = paste0("cell", seq_len(4L)),
    assays = list(
      RNA = list(exportable_layers = c("data", "counts"), exportable = TRUE)
    )
  )
  coordinates <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = context$cells[1:2]
  )
  barcode_cases <- list(
    more = context$cells[c(1L, 2L, 4L)],
    fewer = context$cells[[1L]]
  )

  for (direction in names(barcode_cases)) {
    profile <- NULL
    expect_no_error(
      profile <- .builder_profile_spatial_coordinate_record(
        "section",
        coordinates,
        "RNA",
        context,
        barcode_ids = barcode_cases[[direction]]
      )
    )
    if (is.null(profile)) {
      next
    }
    expect_false(profile$valid, info = direction)
    expect_true(
      "misaligned_barcodes" %in% profile$diagnostics,
      info = direction
    )
  }
})

test_that("oversized Spatial assay identifiers fail closed with previews", {
  expected <- paste0("cell", seq_len(4L))
  coordinates <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = expected[1:2]
  )
  identifiers <- list(
    ascii = strrep("a", 1024L * 1024L),
    multibyte = strrep("界", 100000L)
  )

  for (kind in names(identifiers)) {
    assay <- identifiers[[kind]]
    context <- list(
      cells = expected,
      assays = stats::setNames(
        list(list(exportable_layers = "data", exportable = TRUE)),
        assay
      )
    )
    profile <- .builder_profile_spatial_coordinate_record(
      "section",
      coordinates,
      assay,
      context
    )

    expect_false(profile$valid, info = kind)
    expect_true(
      "oversized_assay_name" %in% profile$diagnostics,
      info = kind
    )
    expect_identical(profile$assay, NA_character_, info = kind)
    expect_identical(profile$assay_count, 1L, info = kind)
    expect_true(profile$assay_truncated, info = kind)
    expect_type(profile$assay_preview, "character")
    expect_length(profile$assay_preview, 1L)
    expect_true(all(nchar(profile$assay_preview, type = "chars") <= 64L))
    expect_true(all(nchar(profile$assay_preview, type = "bytes") <= 256L))
    expect_lt(as.numeric(object.size(profile)), 100000)
  }
})

test_that("oversized Spatial layer identifiers fail closed with previews", {
  expected <- paste0("cell", seq_len(4L))
  coordinates <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = expected[1:2]
  )
  identifiers <- list(
    ascii = strrep("a", 1024L * 1024L),
    multibyte = strrep("界", 100000L)
  )

  for (kind in names(identifiers)) {
    context <- list(
      cells = expected,
      assays = list(
        RNA = list(
          exportable_layers = identifiers[[kind]],
          exportable = TRUE
        )
      )
    )
    profile <- .builder_profile_spatial_coordinate_record(
      "section",
      coordinates,
      "RNA",
      context
    )

    expect_false(profile$valid, info = kind)
    expect_true(
      "oversized_layer_name" %in% profile$diagnostics,
      info = kind
    )
    expect_identical(profile$assay, "RNA", info = kind)
    expect_identical(profile$compatible_layer_count, 1L, info = kind)
    expect_true(profile$compatible_layers_truncated, info = kind)
    expect_identical(profile$compatible_layers, character(), info = kind)
    expect_type(profile$compatible_layer_preview, "character")
    expect_length(profile$compatible_layer_preview, 1L)
    expect_true(all(
      nchar(profile$compatible_layer_preview, type = "chars") <= 64L
    ))
    expect_true(all(
      nchar(profile$compatible_layer_preview, type = "bytes") <= 256L
    ))
    expect_lt(as.numeric(object.size(profile)), 100000)
  }
})

test_that("classed Spatial layer identifiers never dispatch methods", {
  touched <- FALSE
  assign(
    "as.character.builder_spatial_identifier_trap",
    function(value, ...) {
      touched <<- TRUE
      stop("untrusted identifier method executed")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm(
      "as.character.builder_spatial_identifier_trap",
      envir = .GlobalEnv
    ),
    add = TRUE
  )
  expected <- paste0("cell", seq_len(4L))
  coordinates <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    row.names = expected[1:2]
  )
  context <- list(
    cells = expected,
    assays = list(
      RNA = list(
        exportable_layers = structure(
          "data",
          class = "builder_spatial_identifier_trap"
        ),
        exportable = TRUE
      )
    )
  )

  profile <- .builder_profile_spatial_coordinate_record(
    "section",
    coordinates,
    "RNA",
    context
  )

  expect_false(profile$valid)
  expect_false(touched)
  expect_identical(profile$compatible_layers, character())
  expect_identical(profile$compatible_layer_preview, character())
})

test_that("spatial raster dimensions and bounds are validated", {
  valid <- array(runif(10L * 20L * 3L), dim = c(10L, 20L, 3L))
  profile <- .builder_profile_spatial_raster(valid)

  expect_true(profile$present)
  expect_true(profile$valid)
  expect_identical(profile$width, 20L)
  expect_identical(profile$height, 10L)
  expect_identical(profile$channels, 3L)
  expect_equal(
    profile$pixel_bounds,
    list(xmin = 0, xmax = 20, ymin = 0, ymax = 10)
  )

  invalid <- valid
  invalid[[1L]] <- Inf
  invalid_profile <- .builder_profile_spatial_raster(invalid)
  expect_false(invalid_profile$valid)
  expect_true("non_finite_raster" %in% invalid_profile$diagnostics)

  expect_true(.builder_profile_spatial_raster(NULL)$valid)
  expect_false(.builder_profile_spatial_raster(NULL)$present)
  expect_true(.builder_profile_spatial_raster(numeric())$valid)
  expect_false(.builder_profile_spatial_raster(numeric())$present)
})

test_that("raster validation never dispatches S3 or S4 methods", {
  touched <- character()
  s3_methods <- c(
    "length.builder_spatial_raster_trap",
    "dim.builder_spatial_raster_trap",
    "anyNA.builder_spatial_raster_trap"
  )
  for (method in s3_methods) {
    assign(
      method,
      local({
        method_name <- method
        function(...) {
          touched <<- c(touched, method_name)
          stop("untrusted raster method executed")
        }
      }),
      envir = .GlobalEnv
    )
  }
  on.exit(rm(list = s3_methods, envir = .GlobalEnv), add = TRUE)

  s3_raster <- array(as.numeric(seq_len(12L)), dim = c(2L, 2L, 3L))
  attr(s3_raster, "class") <- "builder_spatial_raster_trap"
  s3_result <- tryCatch(
    .builder_profile_spatial_raster(s3_raster),
    error = identity
  )

  if (!methods::isClass("builder_spatial_s4_numeric")) {
    methods::setClass("builder_spatial_s4_numeric", contains = "numeric")
  }
  methods::setMethod(
    "length",
    "builder_spatial_s4_numeric",
    function(x) {
      touched <<- c(touched, "length.builder_spatial_s4_numeric")
      stop("untrusted S4 raster method executed")
    }
  )
  on.exit(
    methods::removeMethod("length", "builder_spatial_s4_numeric"),
    add = TRUE
  )
  s4_raster <- methods::new(
    "builder_spatial_s4_numeric",
    .Data = as.numeric(seq_len(4L))
  )
  attr(s4_raster, "dim") <- c(2L, 2L)
  s4_result <- tryCatch(
    .builder_profile_spatial_raster(s4_raster),
    error = identity
  )

  expected <- list(
    present = TRUE,
    valid = FALSE,
    width = 0L,
    height = 0L,
    channels = 0L,
    pixel_bounds = NULL,
    diagnostics = "unsafe_raster"
  )
  expect_false(inherits(s3_result, "error"))
  expect_false(inherits(s4_result, "error"))
  expect_identical(s3_result, expected)
  expect_identical(s4_result, expected)
  expect_identical(touched, character())
})

test_that("the current Trekker demo defines a valid small Viewer payload", {
  payload <- .builder_content_spatial_demo_payload()
  context <- builder_content_spatial_trekker_context(payload)
  result <- builder_profile_trekker_payload(payload, context)

  builder_content_spatial_expect_record(result)
  expect_true(result$detected)
  expect_true(result$valid)
  expect_identical(result$page_candidates, "trekker")
  expect_identical(
    result$normalized$cell_count,
    length(payload$barcodes)
  )
  expect_identical(result$normalized$barcode_relation, "partial")
  expect_identical(
    result$normalized$cluster_count,
    length(unique(payload$clusters))
  )
  expect_identical(
    result$normalized$field_names,
    names(payload$fields)
  )
  expect_identical(
    result$normalized$confidence_fields,
    c("prop_top", "prop_noise", "sb_total", "sb_umi_top")
  )
  expect_identical(
    result$normalized$moran_genes,
    vapply(payload$moran, function(entry) entry$gene, character(1))
  )
  expect_identical(
    result$normalized$evidence_count,
    length(payload$evidence)
  )
})

test_that("the complete current Trekker demo stays valid and profiles small", {
  path <- builder_content_spatial_inst_path(
    "extdata",
    "examples",
    "demo_trekker.crb"
  )
  payload <- qs2::qs_read(path)$getTrekker()
  context <- list(
    cells = payload$barcodes,
    features = vapply(
      payload$moran,
      function(entry) entry$gene,
      character(1)
    )
  )
  result <- builder_profile_trekker_payload(payload, context)

  expect_true(result$valid)
  expect_identical(result$page_candidates, "trekker")
  expect_lt(as.numeric(object.size(result)), 100000)
  text <- unlist(result, recursive = TRUE, use.names = FALSE)
  text <- text[vapply(text, is.character, logical(1))]
  expect_false(any(grepl("^data:image/", text)))
})

test_that("a tracked Trekker payload outside the contract is rejected", {
  payload <- .builder_content_spatial_demo_payload()
  payload$clusters <- as.character(payload$clusters)
  context <- builder_content_spatial_trekker_context(payload)
  result <- builder_profile_trekker_payload(payload, context)

  expect_true(result$detected)
  expect_false(result$valid)
  expect_identical(result$page_candidates, character())
  expect_true("invalid_clusters" %in% result$diagnostics)
})

test_that("Trekker cell-aligned vectors and cluster lookup fail closed", {
  payload <- .builder_content_spatial_demo_payload()
  context <- builder_content_spatial_trekker_context(payload)
  cases <- list(
    duplicate_barcodes = builder_content_spatial_mutate(
      payload,
      barcodes[[2L]] <- barcodes[[1L]]
    ),
    unknown_barcodes = builder_content_spatial_mutate(
      payload,
      barcodes[[1L]] <- "not-in-dataset"
    ),
    non_finite_coordinates = builder_content_spatial_mutate(
      payload,
      x[[1L]] <- Inf
    ),
    misaligned_coordinates = builder_content_spatial_mutate(
      payload,
      ux <- ux[-1L]
    ),
    invalid_clusters = builder_content_spatial_mutate(
      payload,
      clusters <- as.character(clusters)
    ),
    negative_clusters = builder_content_spatial_mutate(
      payload,
      clusters[[1L]] <- -1L
    ),
    fractional_clusters = builder_content_spatial_mutate(
      payload,
      clusters <- as.numeric(clusters) + 0.5
    ),
    cluster_without_label = builder_content_spatial_mutate(
      payload,
      clusters[[1L]] <- length(celltype)
    ),
    blank_celltype = builder_content_spatial_mutate(
      payload,
      celltype[[clusters[[1L]] + 1L]] <- ""
    )
  )

  for (code in names(cases)) {
    result <- builder_profile_trekker_payload(cases[[code]], context)
    expected_code <- if (
      code %in%
        c(
          "negative_clusters",
          "fractional_clusters"
        )
    ) {
      "invalid_clusters"
    } else {
      code
    }
    expect_false(result$valid, info = code)
    expect_true(expected_code %in% result$diagnostics, info = code)
    expect_identical(result$page_candidates, character(), info = code)
  }
})

test_that("Trekker confidence and fields enforce the client contract", {
  payload <- .builder_content_spatial_demo_payload()
  context <- builder_content_spatial_trekker_context(payload)
  cases <- list(
    missing_confidence_fields = builder_content_spatial_mutate(
      payload,
      conf$sb_umi_top <- NULL
    ),
    invalid_confidence_vectors = builder_content_spatial_mutate(
      payload,
      conf$prop_top[[1L]] <- NA_real_
    ),
    confidence_out_of_range = builder_content_spatial_mutate(
      payload,
      conf$prop_noise[[1L]] <- 1.5
    ),
    invalid_field_values = builder_content_spatial_mutate(
      payload,
      fields[[1L]]$v[[1L]] <- 256L
    ),
    invalid_field_range = builder_content_spatial_mutate(
      payload,
      fields[[1L]]$min <- fields[[1L]]$max + 1
    ),
    invalid_field_label = builder_content_spatial_mutate(
      payload,
      fields[[1L]]$label <- ""
    ),
    invalid_field_summary = builder_content_spatial_mutate(
      payload,
      fields[[1L]]$by_type[[1L]]$median <- Inf
    )
  )

  for (code in names(cases)) {
    result <- builder_profile_trekker_payload(cases[[code]], context)
    expect_false(result$valid, info = code)
    expect_true(code %in% result$diagnostics, info = code)
  }
})

test_that("Trekker Moran and evidence records are checked against identity", {
  payload <- .builder_content_spatial_demo_payload()
  context <- builder_content_spatial_trekker_context(payload)
  cases <- list(
    unknown_moran_gene = builder_content_spatial_mutate(
      payload,
      moran[[1L]]$gene <- "not-in-expression"
    ),
    invalid_moran_value = builder_content_spatial_mutate(
      payload,
      moran[[1L]]$I <- NaN
    ),
    invalid_evidence_index = builder_content_spatial_mutate(
      payload,
      evidence[[1L]]$cell <- length(barcodes)
    ),
    mismatched_evidence_barcode = builder_content_spatial_mutate(
      payload,
      evidence[[1L]]$bc <- barcodes[[1L]]
    ),
    unsafe_evidence_uri = builder_content_spatial_mutate(
      payload,
      evidence[[1L]]$img <- "data:image/svg+xml;base64,PHN2Zz48L3N2Zz4="
    ),
    external_evidence_uri = builder_content_spatial_mutate(
      payload,
      evidence[[1L]]$img <- "https://example.invalid/evidence.png"
    )
  )

  for (code in names(cases)) {
    result <- builder_profile_trekker_payload(cases[[code]], context)
    expected_code <- if (identical(code, "external_evidence_uri")) {
      "unsafe_evidence_uri"
    } else {
      code
    }
    expect_false(result$valid, info = code)
    expect_true(expected_code %in% result$diagnostics, info = code)
  }
})

test_that("high-level profiling reads Trekker only from the current object", {
  skip_if_not_installed("SeuratObject")

  object <- builder_profile_pbmc()
  payload <- .builder_content_spatial_demo_payload()
  keep <- seq_len(min(length(payload$barcodes), ncol(object)))
  replacement <- SeuratObject::Cells(object)[keep]
  payload$barcodes <- replacement
  payload$evidence <- lapply(payload$evidence, function(entry) {
    entry$bc <- replacement[[entry$cell + 1L]]
    entry
  })
  for (entry in seq_along(payload$moran)) {
    payload$moran[[entry]]$gene <- SeuratObject::Features(object)[[entry]]
  }
  object@misc$trekker <- payload
  result <- builder_profile_spatial_content(
    object,
    builder_content_spatial_context(object)
  )$trekker

  expect_true(result$detected)
  expect_true(result$valid)
  expect_identical(result$page_candidates, "trekker")
})

test_that("classed misc containers never dispatch custom access methods", {
  skip_if_not_installed("SeuratObject")

  object <- builder_profile_pbmc()
  context <- builder_content_spatial_context(object)
  payload <- .builder_content_spatial_demo_payload()
  touched <- FALSE
  assign(
    "names.builder_content_spatial_trap",
    function(value) {
      touched <<- TRUE
      stop("unsafe names dispatch")
    },
    envir = .GlobalEnv
  )
  assign(
    "[[.builder_content_spatial_trap",
    function(value, index, ...) {
      touched <<- TRUE
      stop("unsafe extraction dispatch")
    },
    envir = .GlobalEnv
  )
  assign(
    "length.builder_content_spatial_trap",
    function(value) {
      touched <<- TRUE
      stop("unsafe length dispatch")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm(
      "names.builder_content_spatial_trap",
      "[[.builder_content_spatial_trap",
      "length.builder_content_spatial_trap",
      envir = .GlobalEnv
    ),
    add = TRUE
  )

  methods::slot(object, "misc", check = FALSE) <- structure(
    list(trekker = payload),
    class = "builder_content_spatial_trap"
  )
  outer <- builder_profile_spatial_content(object, context)$trekker
  expect_true(outer$detected)
  expect_false(outer$valid)
  expect_false(touched)

  methods::slot(object, "misc", check = FALSE) <- structure(
    list(other_content = payload),
    class = "builder_content_spatial_trap"
  )
  absent <- builder_profile_spatial_content(object, context)$trekker
  expect_false(absent$detected)
  expect_true(absent$valid)
  expect_identical(absent$diagnostics, character())
  expect_false(touched)

  nested <- payload
  nested$conf <- structure(
    nested$conf,
    class = "builder_content_spatial_trap"
  )
  inner <- builder_profile_trekker_payload(nested, context)
  expect_true(inner$detected)
  expect_false(inner$valid)
  expect_false(touched)
})

test_that("failed coordinate extraction bounds unsafe assay identifiers", {
  skip_if_not_installed("SeuratObject")
  if (!methods::isClass("builder_broken_spatial_image")) {
    methods::setClass(
      "builder_broken_spatial_image",
      slots = c(assay = "ANY", image = "ANY")
    )
  }
  touched <- character()
  methods <- c(
    "length.builder_spatial_assay_trap",
    "as.character.builder_spatial_assay_trap"
  )
  for (method in methods) {
    assign(
      method,
      local({
        method_name <- method
        function(...) {
          touched <<- c(touched, method_name)
          stop("untrusted assay method executed")
        }
      }),
      envir = .GlobalEnv
    )
  }
  on.exit(rm(list = methods, envir = .GlobalEnv), add = TRUE)

  cases <- list(
    ascii = strrep("a", 1024L * 1024L),
    multibyte = strrep("界", 100000L),
    classed = structure(
      "RNA",
      class = "builder_spatial_assay_trap"
    )
  )
  expected_diagnostic <- c(
    ascii = "oversized_assay_name",
    multibyte = "oversized_assay_name",
    classed = "unsafe_assay_name"
  )

  for (kind in names(cases)) {
    object <- builder_profile_pbmc()
    image <- methods::new(
      "builder_broken_spatial_image",
      assay = cases[[kind]],
      image = NULL
    )
    methods::slot(object, "images", check = FALSE) <- list(section = image)
    context <- builder_content_spatial_context(object)
    spatial <- builder_profile_spatial_content(object, context)$spatial
    section <- spatial$normalized$sections[[1L]]

    expect_true(spatial$detected, info = kind)
    expect_false(spatial$valid, info = kind)
    expect_true(
      "coordinate_extraction_failed" %in% section$diagnostics,
      info = kind
    )
    expect_true(
      expected_diagnostic[[kind]] %in% section$diagnostics,
      info = kind
    )
    expect_identical(section$assay, NA_character_, info = kind)
    expect_identical(section$compatible_layers, character(), info = kind)
    expect_type(section$assay_preview, "character")
    expect_true(all(nchar(section$assay_preview, type = "chars") <= 64L))
    expect_true(all(nchar(section$assay_preview, type = "bytes") <= 256L))
    expect_true("assay_count" %in% names(section), info = kind)
    expect_true("assay_truncated" %in% names(section), info = kind)
    expect_true("compatible_layer_count" %in% names(section), info = kind)
    expect_true("compatible_layer_preview" %in% names(section), info = kind)
    expect_true(
      "compatible_layers_truncated" %in% names(section),
      info = kind
    )
    expect_lt(as.numeric(object.size(spatial)), 100000)
  }
  expect_identical(touched, character())
})

test_that("classed images containers never dispatch access methods", {
  skip_if_not_installed("SeuratObject")
  touched <- character()
  methods <- c(
    "length.builder_spatial_images_trap",
    "names.builder_spatial_images_trap",
    "[[.builder_spatial_images_trap"
  )
  for (method in methods) {
    assign(
      method,
      local({
        method_name <- method
        function(...) {
          touched <<- c(touched, method_name)
          stop("untrusted images method executed")
        }
      }),
      envir = .GlobalEnv
    )
  }
  on.exit(rm(list = methods, envir = .GlobalEnv), add = TRUE)

  object <- builder_profile_pbmc()
  context <- builder_content_spatial_context(object)
  images <- structure(
    list(section = list()),
    class = "builder_spatial_images_trap"
  )
  methods::slot(object, "images", check = FALSE) <- images

  spatial <- builder_profile_spatial_content(object, context)$spatial

  expect_true(spatial$detected)
  expect_false(spatial$valid)
  expect_true("unsafe_images_container" %in% spatial$diagnostics)
  expect_identical(spatial$normalized$sections, list())
  expect_identical(spatial$page_candidates, character())
  expect_identical(touched, character())
  expect_lt(as.numeric(object.size(spatial)), 100000)
})

test_that("oversized section names fail closed with a bounded summary", {
  skip_if_not_installed("SeuratObject")

  object <- builder_content_spatial_example_object()
  names(object@images) <- strrep("section-name-", 100000L)
  result <- builder_profile_spatial_content(
    object,
    builder_content_spatial_context(object)
  )$spatial

  expect_true(result$detected)
  expect_false(result$valid)
  expect_true("oversized_section_names" %in% result$diagnostics)
  expect_true(result$normalized$section_names_truncated)
  expect_lte(
    nchar(result$normalized$sections[[1L]]$name, type = "bytes"),
    256L
  )
  expect_true(result$normalized$sections[[1L]]$name_truncated)
  expect_lt(as.numeric(object.size(result)), 100000)
})

test_that("oversized Trekker labels fail closed without bloating profiles", {
  payload <- .builder_content_spatial_demo_payload()
  context <- builder_content_spatial_trekker_context(payload)
  cases <- list(
    oversized_sample_id = builder_content_spatial_mutate(
      payload,
      qc$sample_id <- strrep("x", 1000000L)
    ),
    oversized_field_names = builder_content_spatial_mutate(
      payload,
      names(fields)[[1L]] <- strrep("x", 1000000L)
    ),
    oversized_moran_genes = builder_content_spatial_mutate(
      payload,
      moran[[1L]]$gene <- strrep("x", 1000000L)
    )
  )
  contexts <- lapply(cases, function(value) {
    local_context <- context
    local_context$features <- unique(c(
      local_context$features,
      vapply(value$moran, function(entry) entry$gene, character(1))
    ))
    local_context
  })

  for (code in names(cases)) {
    result <- builder_profile_trekker_payload(cases[[code]], contexts[[code]])
    expect_true(result$detected, info = code)
    expect_false(result$valid, info = code)
    expect_true(code %in% result$diagnostics, info = code)
    expect_lt(as.numeric(object.size(result)), 100000)
  }
  sample_profile <- builder_profile_trekker_payload(
    cases$oversized_sample_id,
    contexts$oversized_sample_id
  )
  expect_true(sample_profile$normalized$sample_id_truncated)
  expect_lte(
    nchar(sample_profile$normalized$sample_id, type = "bytes"),
    256L
  )
  field_profile <- builder_profile_trekker_payload(
    cases$oversized_field_names,
    contexts$oversized_field_names
  )
  expect_true(field_profile$normalized$field_names_truncated)
  expect_true(all(
    nchar(field_profile$normalized$field_names, type = "bytes") <= 256L
  ))
  moran_profile <- builder_profile_trekker_payload(
    cases$oversized_moran_genes,
    contexts$oversized_moran_genes
  )
  expect_true(moran_profile$normalized$moran_genes_truncated)
  expect_true(all(
    nchar(moran_profile$normalized$moran_genes, type = "bytes") <= 256L
  ))
})
