##----------------------------------------------------------------------------##
## Read a Seurat object and work out what the export needs to know.
##
## Pure: takes an object, returns a plain list. No Shiny, so it can be tried
## from the console and tested without a running app.
##----------------------------------------------------------------------------##

#' Layers that represent every cell in an assay.
#'
#' Seurat v5 may split one logical layer into physical layers such as
#' `counts.sample1` and `counts.sample2`. Each physical layer contains only a
#' subset of cells, and passing one of them to the exporter silently truncates
#' the data set. Offer the logical name only when its physical layers form a
#' complete, non-overlapping partition.
#' The grouping column to start with: a known annotation or clustering name
#' first; otherwise the first candidate that is not a spatial grid index
#' (Visium's `array_row`/`array_col` look like groups but are coordinates).
builder_preselect_group <- function(candidates) {
  candidates <- as.character(candidates)
  if (!length(candidates)) {
    return(character())
  }
  known <- c(
    "seurat_clusters",
    "cell_type",
    "celltype",
    "cell_class",
    "annotation",
    "clusters",
    "cluster",
    "leiden",
    "louvain",
    "sample",
    "orig.ident"
  )
  lowered <- tolower(candidates)
  hits <- unlist(
    lapply(known, function(name) candidates[lowered == name]),
    use.names = FALSE
  )
  if (length(hits)) {
    return(unique(hits))
  }
  grid <- grepl(
    "^(array_?(row|col)|(image)?(row|col)|in_tissue|x|y|z)$",
    lowered
  )
  if (any(!grid)) candidates[!grid][[1L]] else candidates[[1L]]
}

builder_layer_choices <- function(
  assay,
  expected_cells,
  layer_api = .builder_profile_layer_api()
) {
  profile <- builder_profile_assay(
    assay,
    expected_cells,
    expected_features = NULL,
    layer_api = layer_api
  )
  choices <- profile$exportable_layers
  preferred <- c("data", "counts", "scale.data")
  c(intersect(preferred, choices), setdiff(choices, preferred))
}

builder_validate_joined_layer <- function(
  data,
  expected_cells,
  expected_features
) {
  if (length(dim(data)) != 2L) {
    stop("Joined layer is not a two-dimensional expression matrix.")
  }
  cells <- builder_identity_profile(colnames(data), expected_cells)
  if (!cells$valid) {
    stop("Joined layer cell identity does not match the profiled partition.")
  }
  features <- builder_identity_profile(rownames(data), expected_features)
  if (!features$valid) {
    stop("Joined layer feature identity does not match the profiled partition.")
  }
  data[
    features$reorder_index,
    cells$reorder_index,
    drop = FALSE
  ]
}

#' Join a safe logical split layer before export.
#'
#' Exact full-cell layers are returned unchanged. A logical split layer is
#' materialized only after `builder_layer_choices()` has proved that its
#' physical members cover every cell exactly once.
builder_prepare_export_layer <- function(
  object,
  assay,
  layer,
  layer_api = .builder_profile_layer_api()
) {
  if (!assay %in% names(object@assays)) {
    stop("Assay `", assay, "` is not present in the object.")
  }
  expected_cells <- SeuratObject::Cells(object)
  assay_profile <- builder_profile_assay(
    object[[assay]],
    expected_cells,
    name = assay,
    expected_features = NULL,
    layer_api = layer_api
  )
  choices <- assay_profile$exportable_layers
  if (!layer %in% choices) {
    physical <- .builder_profile_layer_names(object[[assay]], layer_api)
    reason <- if (!layer %in% physical) {
      paste0("Layer `", layer, "` is not available in assay `", assay, "`.")
    } else {
      paste0(
        "Layer `",
        layer,
        "` does not contain every cell in assay `",
        assay,
        "`."
      )
    }
    stop(
      reason
    )
  }

  selected <- assay_profile$layers[[layer]]
  if (identical(selected$kind, "physical")) {
    return(object)
  }

  local_object <- object
  physical <- .builder_profile_layer_names(object[[assay]], layer_api)
  protected <- physical[startsWith(physical, layer)]
  for (candidate in setdiff(protected, selected$members)) {
    suppressWarnings(
      SeuratObject::LayerData(
        local_object[[assay]],
        layer = candidate
      ) <- NULL
    )
  }
  escaped <- gsub(
    "([\\^$.|?*+()\\[\\]{}\\\\\\-])",
    "\\\\\\1",
    layer,
    perl = TRUE
  )
  local_object <- SeuratObject::JoinLayers(
    local_object,
    assay = assay,
    layers = paste0("^", escaped),
    new = layer
  )
  joined <- suppressWarnings(
    SeuratObject::LayerData(local_object[[assay]], layer = layer)
  )
  joined <- builder_validate_joined_layer(
    joined,
    expected_cells,
    selected$features$ids
  )
  suppressWarnings(
    SeuratObject::LayerData(
      local_object[[assay]],
      layer = layer
    ) <- joined
  )
  local_object
}

#' Return numeric QC values without interpreting factor level codes.
builder_qc_numeric_values <- function(values) {
  if (is.numeric(values)) {
    return(as.numeric(values))
  }
  if (!(is.factor(values) || is.character(values))) {
    return(NULL)
  }
  text <- trimws(as.character(values))
  missing <- is.na(values) | !nzchar(text)
  numeric <- suppressWarnings(as.numeric(text))
  if (!any(!missing) || any(!missing & !is.finite(numeric))) {
    return(NULL)
  }
  numeric[missing] <- NA_real_
  numeric
}

.builder_expression_matrix_summary <- function(object, profile) {
  assay <- profile$default_assay
  layer <- profile$assay_profiles[[assay]]$default_layer
  assay_object <- object[[assay]]
  matrix <- tryCatch(
    if (
      inherits(assay_object, "Assay") &&
        layer %in% methods::slotNames(assay_object)
    ) {
      methods::slot(assay_object, layer)
    } else {
      suppressWarnings(SeuratObject::LayerData(assay_object, layer = layer))
    },
    error = function(error) NULL
  )
  list(
    sparse = inherits(
      matrix,
      c("sparseMatrix", "IterableMatrix", "SparseArray")
    ),
    storage = if (inherits(matrix, "IterableMatrix")) {
      "bpcells"
    } else if (inherits(matrix, "HDF5Matrix")) {
      "h5"
    } else {
      "memory"
    }
  )
}

#' Describe a Seurat object in the terms the exporter cares about.
describe_seurat <- function(object, metadata_groups = NULL) {
  meta <- object@meta.data
  n_cells <- ncol(object)

  assays <- names(object@assays)
  default_assay <- tryCatch(
    SeuratObject::DefaultAssay(object),
    error = function(e) assays[1]
  )

  qc_columns <- names(meta)[grepl(
    paste0(
      "^(nCount|nFeature|nUMI|nGene)|",
      "^total[._]?(count|UMI|gene)s?$"
    ),
    names(meta),
    ignore.case = TRUE
  )]
  qc_numeric_values <- lapply(meta[qc_columns], builder_qc_numeric_values)
  qc_numeric_values <- qc_numeric_values[
    !vapply(qc_numeric_values, is.null, logical(1))
  ]
  numeric_cols <- names(qc_numeric_values)
  qc_choices <- function(assay, kind) {
    if (identical(kind, "umi")) {
      preferred <- c(
        paste0("nCount_", assay),
        "nUMI",
        "nCount_RNA",
        "total_counts",
        "total_UMIs"
      )
      pattern <- "^(nCount|nUMI)|^total[._]?(count|UMI)s?$"
    } else {
      preferred <- c(
        paste0("nFeature_", assay),
        "nGene",
        "nFeature_RNA",
        "total_genes"
      )
      pattern <- "^(nFeature|nGene)|^total[._]?genes?$"
    }
    relevant <- numeric_cols[grepl(pattern, numeric_cols, ignore.case = TRUE)]
    unique(c(intersect(preferred, numeric_cols), relevant))
  }

  assay_profiles <- lapply(assays, function(assay) {
    layers <- builder_layer_choices(
      object[[assay]],
      expected_cells = SeuratObject::Cells(object)
    )
    umi <- qc_choices(assay, "umi")
    genes <- qc_choices(assay, "gene")
    list(
      layers = layers,
      default_layer = if ("data" %in% layers) {
        "data"
      } else if (length(layers)) {
        layers[1]
      } else {
        NULL
      },
      nUMI_choices = umi,
      nGene_choices = genes,
      nUMI = if (length(umi)) umi[1] else NA_character_,
      nGene = if (length(genes)) genes[1] else NA_character_
    )
  })
  names(assay_profiles) <- assays
  default_profile <- assay_profiles[[default_assay]]

  ## A column is usable as a grouping variable when it has more than one value
  ## and not one value per cell. Say why the others were left out rather than
  ## hiding them.
  ## Columns whose name says they are a measurement, not a label. An integer
  ## count column like nFeature_RNA has few enough distinct values to look like
  ## a grouping variable and is never one.
  qc_pattern <- paste0(
    "^(nCount|nFeature|nUMI|nGene)_?|^total[._]?(count|UMI|gene)s?$|",
    "^percent[._]|[._]score$|^S\\.Score$|^G2M\\.Score$"
  )

  legacy_metadata <- metadata_groups$legacy %||% NULL
  reuse_metadata <- is.list(legacy_metadata) &&
    is.character(legacy_metadata$columns) &&
    is.integer(legacy_metadata$distinct) &&
    is.list(legacy_metadata$counts)
  candidates <- character()
  struck <- character()
  group_counts <- list()
  if (reuse_metadata) {
    for (col in legacy_metadata$columns) {
      n <- legacy_metadata$distinct[[col]]
      if (grepl(qc_pattern, col, ignore.case = TRUE)) {
        struck <- c(struck, paste0(col, " (QC metric, not a group)"))
      } else if (n < 2L) {
        struck <- c(struck, paste0(col, " (only one value)"))
      } else if (n >= n_cells) {
        struck <- c(struck, paste0(col, " (one value per cell)"))
      } else if (n > 100L) {
        struck <- c(struck, paste0(col, " (", n, " values; too many)"))
      } else {
        candidates <- c(candidates, col)
        group_counts[[col]] <- legacy_metadata$counts[[col]]
      }
    }
  } else {
    for (col in colnames(meta)) {
      values <- meta[[col]]
      if (!(is.character(values) || is.factor(values) || is.integer(values))) {
        next
      }
      n <- length(unique(values))
      if (grepl(qc_pattern, col, ignore.case = TRUE)) {
        struck <- c(struck, paste0(col, " (QC metric, not a group)"))
      } else if (n < 2) {
        struck <- c(struck, paste0(col, " (only one value)"))
      } else if (n >= n_cells) {
        struck <- c(struck, paste0(col, " (one value per cell)"))
      } else if (n > 100) {
        struck <- c(struck, paste0(col, " (", n, " values; too many)"))
      } else {
        candidates <- c(candidates, col)
      }
    }
  }
  if (length(candidates)) {
    candidate_counts <- if (reuse_metadata) {
      legacy_metadata$distinct[candidates]
    } else {
      vapply(
        candidates,
        function(c) length(unique(meta[[c]])),
        integer(1)
      )
    }
    names(candidates) <- paste0(
      candidates,
      "  (",
      candidate_counts,
      " groups)"
    )
  }

  preselect <- builder_preselect_group(unname(candidates))
  if (!reuse_metadata) {
    group_counts <- lapply(unname(candidates), function(column) {
      values <- as.character(meta[[column]])
      values[is.na(values)] <- "N/A"
      sort(table(values), decreasing = TRUE)
    })
    names(group_counts) <- unname(candidates)
  }
  qc_values <- lapply(numeric_cols, function(column) {
    values <- qc_numeric_values[[column]]
    values <- values[is.finite(values)]
    if (length(values) > 1000L) {
      values <- values[unique(round(seq(
        1,
        length(values),
        length.out = 1000L
      )))]
    }
    values
  })
  names(qc_values) <- numeric_cols

  ## Non-PCA projections take precedence. The exporter keeps one lone PCA as
  ## a warned fallback, but filters PCA whenever another projection is present.
  reductions <- names(object@reductions)
  reduction_preselect <- reductions[
    !grepl("pca", reductions, ignore.case = TRUE)
  ]
  if (!length(reduction_preselect) && length(reductions) == 1L) {
    reduction_preselect <- reductions
  }

  ## Gene name shape is the only cheap signal for species.
  genes <- utils::head(rownames(object), 200)
  organism_guess <- if (!length(genes)) {
    "other"
  } else if (mean(genes == toupper(genes)) > 0.8) {
    "hg"
  } else if (mean(grepl("^[A-Z][a-z]", genes)) > 0.6) {
    "mm"
  } else {
    "other"
  }

  list(
    n_cells = n_cells,
    n_genes = nrow(object),
    assays = assays,
    default_assay = default_assay,
    assay_profiles = assay_profiles,
    ## Kept for older callers; the UI now reads the selected assay profile.
    layers = default_profile$layers,
    default_layer = default_profile$default_layer,
    group_candidates = candidates,
    group_preselect = preselect,
    group_counts = group_counts,
    group_struck = struck,
    reductions = reductions,
    reduction_preselect = reduction_preselect,
    images = tryCatch(names(object@images), error = function(e) character()),
    spatial_scenes = builder_spatial_alignment_sections(object),
    nUMI = default_profile$nUMI,
    nGene = default_profile$nGene,
    qc_values = qc_values,
    organism_guess = organism_guess,
    ## What the object already carries that Cerebro can show. Worth surfacing:
    ## these light up whole pages, and their absence is the usual reason a
    ## page the user expected is missing.
    extras = describe_misc(object),
    suggested_dir = file.path(path.expand("~"), "cerebro")
  )
}

#' What is in `@misc` that the viewer knows how to display.
describe_misc <- function(object) {
  misc <- object@misc
  present <- function(key) !is.null(misc[[key]]) && length(misc[[key]]) > 0
  entries <- list(
    list(
      key = "marker_genes",
      label = "Marker genes",
      found = present("marker_genes")
    ),
    list(
      key = "immune_repertoire",
      label = "Immune repertoire (TCR/BCR)",
      found = present("immune_repertoire") ||
        present("tcr_data") ||
        present("bcr_data")
    ),
    list(
      key = "enriched_pathways",
      label = "Enriched pathways",
      found = present("enriched_pathways")
    ),
    list(
      key = "most_expressed_genes",
      label = "Most expressed genes",
      found = present("most_expressed_genes")
    ),
    list(
      key = "trajectories",
      label = "Trajectory (monocle2)",
      found = present("trajectories")
    ),
    list(
      key = "hla_typing",
      label = "HLA typing",
      found = present("hla_typing")
    ),
    ## exportFromSeurat() reads Trekker from @misc after Builder has applied its
    ## frozen group, colour, and alignment settings. Its absence is why a page
    ## the user expected is missing.
    list(
      key = "trekker",
      label = "Trekker spatial mapping",
      found = present("trekker")
    ),
    list(
      key = "extra_material",
      label = "Supplementary tables / plots",
      found = present("extra_material")
    )
  )
  entries
}
