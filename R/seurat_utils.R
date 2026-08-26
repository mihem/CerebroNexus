## Standard Seurat expression classes. They define deterministic compatibility
## fallback order and legacy semantic descriptions only; request-driven split
## resolution supports arbitrary roots and does not use this list as a gate.
## Ordered longest first where prefix parsing is needed: `scale.data` must be
## recognised before `data`.
.cerebro_layer_roots <- c("scale.data", "counts", "data")
.cerebro_fallback_roots <- c("data", "counts", "scale.data")

#' Semantic root of a (possibly split) Seurat v5 layer name
#'
#' `split(assay, f = ...)` names each layer `<root>.<level>`, where the level is
#' whatever the splitting factor's values are -- sample names (`counts.pbmc_1`)
#' at least as often as integers (`counts.1`). Matching only the numeric form
#' left every sample-split object looking unsplit.
#'
#' Roots are matched against a whitelist rather than stripped with a general
#' rule, because `scale.data` contains a dot itself: `sub("\\.[^.]+$", "", x)`
#' would root it as `scale` and quietly divorce it from its own split layers.
#' Layer names outside the whitelist keep the legacy numeric-suffix handling, so
#' a custom slot such as `foo.1` still resolves to `foo`.
#'
#' @keywords internal
#' @noRd
.layer_semantic_root <- function(x) {
  vapply(
    x,
    function(layer) {
      known <- .cerebro_layer_roots[
        startsWith(layer, paste0(.cerebro_layer_roots, "."))
      ]
      if (length(known) > 0) {
        return(known[[1L]])
      }
      sub("\\.[0-9]+$", "", layer)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Find a unique Seurat v5 layer partition
#'
#' The normal `split.Assay5()` case is linear in the number of cells and layer
#' memberships. An indexed exact-cover search is used only when unrelated
#' overlapping candidates make the normal ownership count inconclusive.
#'
#' @keywords internal
#' @noRd
.find_layer_partition <- function(
  assay_cells,
  memberships,
  max_solutions = 2L,
  max_search_nodes = 100000L,
  max_search_depth = 128L,
  max_conflict_work = 5000000
) {
  if (
    !is.character(assay_cells) ||
      length(assay_cells) == 0L ||
      anyNA(assay_cells) ||
      any(!nzchar(assay_cells)) ||
      anyDuplicated(assay_cells)
  ) {
    stop(
      "`assay_cells` must contain unique, non-empty cell names.",
      call. = FALSE
    )
  }
  if (
    !is.list(memberships) ||
      is.null(names(memberships)) ||
      anyNA(names(memberships)) ||
      any(!nzchar(names(memberships))) ||
      anyDuplicated(names(memberships))
  ) {
    stop(
      "`memberships` must be a list with unique, non-empty layer names.",
      call. = FALSE
    )
  }
  valid_max_solutions <- is.numeric(max_solutions) &&
    length(max_solutions) == 1L &&
    is.finite(max_solutions) &&
    max_solutions == floor(max_solutions) &&
    max_solutions >= 2 &&
    max_solutions <= .Machine$integer.max
  if (!valid_max_solutions) {
    stop(
      "`max_solutions` must be one finite integer of at least 2.",
      call. = FALSE
    )
  }
  max_solutions <- as.integer(max_solutions)
  validate_work_budget <- function(value, label) {
    valid <- is.numeric(value) &&
      length(value) == 1L &&
      is.finite(value) &&
      value == floor(value) &&
      value >= 1
    if (!valid) {
      stop(
        "`",
        label,
        "` must be one finite positive integer.",
        call. = FALSE
      )
    }
    as.double(value)
  }
  max_search_nodes <- validate_work_budget(
    max_search_nodes,
    "max_search_nodes"
  )
  max_search_depth <- validate_work_budget(
    max_search_depth,
    "max_search_depth"
  )
  max_conflict_work <- validate_work_budget(
    max_conflict_work,
    "max_conflict_work"
  )
  assay_cells <- sort(assay_cells, method = "radix")

  memberships <- lapply(
    memberships,
    function(cells) {
      cells <- as.character(cells)
      if (anyNA(cells) || any(!nzchar(cells))) {
        stop(
          "Layer memberships must contain non-empty cell names.",
          call. = FALSE
        )
      }
      unique(cells)
    }
  )
  outside <- base::setdiff(
    unique(unlist(memberships, use.names = FALSE)),
    assay_cells
  )
  if (length(outside) > 0L) {
    stop(
      "Layer memberships contain cells outside the assay: ",
      paste(utils::head(outside, 5L), collapse = ", "),
      call. = FALSE
    )
  }

  n_cells <- length(assay_cells)
  is_partial <- lengths(memberships) > 0L &
    lengths(memberships) < n_cells
  memberships <- memberships[is_partial]
  if (length(memberships) < 2L) {
    return(list(
      status = "none",
      layers = character(),
      solutions = list()
    ))
  }

  memberships <- memberships[order(names(memberships), method = "radix")]
  membership_lengths <- lengths(memberships)
  encoded_memberships <- match(
    unlist(memberships, use.names = FALSE),
    assay_cells
  )
  membership_ends <- cumsum(membership_lengths)
  membership_starts <- c(1L, utils::head(membership_ends, -1L) + 1L)
  layer_cells <- Map(
    function(start, end) encoded_memberships[seq.int(start, end)],
    membership_starts,
    membership_ends
  )
  names(layer_cells) <- names(memberships)
  claimed <- tabulate(
    encoded_memberships,
    nbins = n_cells
  )

  if (all(claimed == 1L)) {
    solution <- names(layer_cells)
    return(list(
      status = "unique",
      layers = solution,
      solutions = list(solution)
    ))
  }

  ## Building every layer-to-layer conflict touches each pair of claims made
  ## on a cell. A pathological prefix can otherwise exhaust memory before the
  ## recursive node budget below gets a chance to help.
  conflict_work <- sum(as.double(claimed)^2)
  if (conflict_work > max_conflict_work) {
    stop(
      "Layer partition search budget exceeded while indexing overlapping ",
      "candidates (",
      format(conflict_work, scientific = FALSE),
      " claim pairs; budget ",
      format(max_conflict_work, scientific = FALSE),
      "). Rename unrelated prefix layers or join the intended layers ",
      "explicitly with SeuratObject::JoinLayers().",
      call. = FALSE
    )
  }

  ## Build sparse integer adjacency once. The recursive search below never
  ## scans character memberships or recomputes full-vector intersections.
  cell_layers <- vector("list", n_cells)
  assignments <- split(
    rep(seq_along(layer_cells), lengths(layer_cells)),
    unlist(layer_cells, use.names = FALSE)
  )
  assignment_cells <- as.integer(names(assignments))
  cell_layers[assignment_cells] <- unname(assignments)

  conflicts <- lapply(
    seq_along(layer_cells),
    function(layer_id) {
      sort(unique(unlist(
        cell_layers[layer_cells[[layer_id]]],
        use.names = FALSE
      )))
    }
  )

  solutions <- list()
  visited_nodes <- 0
  search_partition <- function(covered, available, chosen) {
    visited_nodes <<- visited_nodes + 1
    if (visited_nodes > max_search_nodes) {
      stop(
        "Layer partition search budget exceeded after ",
        format(max_search_nodes, scientific = FALSE),
        " search nodes. Rename unrelated prefix layers or join the intended ",
        "layers explicitly with SeuratObject::JoinLayers().",
        call. = FALSE
      )
    }
    if (length(solutions) >= max_solutions) {
      return(invisible(NULL))
    }
    if (all(covered)) {
      if (length(chosen) >= 2L) {
        solutions[[length(solutions) + 1L]] <<-
          sort(names(layer_cells)[chosen], method = "radix")
      }
      return(invisible(NULL))
    }

    feasible <- which(available)
    feasible <- feasible[vapply(
      feasible,
      function(layer_id) !any(covered[layer_cells[[layer_id]]]),
      logical(1)
    )]
    if (length(feasible) == 0L) {
      return(invisible(NULL))
    }

    choices_per_cell <- tabulate(
      unlist(layer_cells[feasible], use.names = FALSE),
      nbins = n_cells
    )
    if (any(choices_per_cell[!covered] == 0L)) {
      return(invisible(NULL))
    }
    choices_per_cell[covered] <- .Machine$integer.max
    pivot <- which.min(choices_per_cell)
    choices <- sort(base::intersect(cell_layers[[pivot]], feasible))

    if (length(chosen) >= max_search_depth) {
      stop(
        "Layer partition search depth budget exceeded before selecting more ",
        "than ",
        format(max_search_depth, scientific = FALSE),
        " layers. Rename unrelated prefix layers or join the intended layers ",
        "explicitly with SeuratObject::JoinLayers().",
        call. = FALSE
      )
    }
    for (choice in choices) {
      next_covered <- covered
      next_covered[layer_cells[[choice]]] <- TRUE
      next_available <- available
      next_available[conflicts[[choice]]] <- FALSE
      search_partition(
        covered = next_covered,
        available = next_available,
        chosen = c(chosen, choice)
      )
      if (length(solutions) >= max_solutions) {
        break
      }
    }
    invisible(NULL)
  }

  search_partition(
    covered = rep(FALSE, n_cells),
    available = rep(TRUE, length(layer_cells)),
    chosen = integer()
  )

  if (length(solutions) == 0L) {
    return(list(
      status = "none",
      layers = character(),
      solutions = list()
    ))
  }

  solution_keys <- vapply(
    solutions,
    paste,
    collapse = "\r",
    FUN.VALUE = character(1)
  )
  solutions <- solutions[order(solution_keys, method = "radix")]
  if (length(solutions) == 1L) {
    return(list(
      status = "unique",
      layers = solutions[[1L]],
      solutions = solutions
    ))
  }

  list(
    status = "ambiguous",
    layers = character(),
    solutions = solutions
  )
}

.layer_prefix_pattern <- function(root) {
  escaped <- gsub(
    "([\\^$.|?*+()\\[\\]{}\\\\\\-])",
    "\\\\\\1",
    root,
    perl = TRUE
  )
  paste0("^", escaped)
}

.layer_prefix_candidates <- function(assay, root) {
  pattern <- .layer_prefix_pattern(root)
  candidates <- suppressWarnings(
    SeuratObject::Layers(assay, search = pattern)
  )
  unique(base::setdiff(candidates, root))
}

.layer_partition_candidates <- function(candidates, root) {
  escaped <- gsub(
    "([\\^$.|?*+()\\[\\]{}\\\\\\-])",
    "\\\\\\1",
    root,
    perl = TRUE
  )
  candidates[grepl(paste0("^", escaped, "\\."), candidates, perl = TRUE)]
}

.direct_layer_partition_candidates <- function(candidates, root) {
  prefix_length <- nchar(root) + 1L
  suffix <- substring(candidates, prefix_length + 1L)
  candidates[!grepl(".", suffix, fixed = TRUE)]
}

.nested_partition_root <- function(root, layers) {
  prefix_length <- nchar(root) + 1L
  suffix <- substring(layers, prefix_length + 1L)
  if (
    length(suffix) == 0L ||
      !all(grepl(".", suffix, fixed = TRUE))
  ) {
    return(NULL)
  }
  first_segment <- sub("\\..*$", "", suffix)
  if (length(unique(first_segment)) != 1L) {
    return(NULL)
  }
  paste0(root, ".", first_segment[[1L]])
}

.supported_expression_matrix <- function(x) {
  is.matrix(x) || inherits(x, "dgCMatrix")
}

.disk_backed_expression_matrix <- function(x) {
  inherits(x, c("IterableMatrix", "DelayedMatrix", "DelayedArray"))
}

.format_layer_solutions <- function(solutions) {
  paste(
    vapply(
      solutions,
      function(solution) {
        paste0("[", paste(solution, collapse = ", "), "]")
      },
      character(1)
    ),
    collapse = " or "
  )
}

.format_layer_counts <- function(counts) {
  counts <- counts[order(names(counts), method = "radix")]
  paste0(names(counts), "=", counts, collapse = ", ")
}

.stop_incomplete_layer_resolution <- function(
  resolution,
  assay,
  requested_layer
) {
  failure <- resolution$failure
  stop(
    "Exact layer `",
    requested_layer,
    "` is absent from assay `",
    assay,
    "`. No unique disjoint partition covers ",
    failure$total_count,
    " assay cells.\n",
    "Prefix candidates: ",
    paste(resolution$candidates, collapse = ", "),
    "\nLayer cell counts: ",
    .format_layer_counts(failure$candidate_counts),
    "\nPartial candidate union covers ",
    failure$covered_count,
    " of ",
    failure$total_count,
    " assay cells; ",
    failure$overlap_count,
    " cell(s) occur in more than one candidate.\n",
    "Missing assay cells (",
    failure$missing_count,
    "): ",
    if (failure$missing_count == 0L) {
      "none"
    } else {
      paste(failure$missing_examples, collapse = ", ")
    },
    "\nInspect with:\n",
    "  SeuratObject::Layers(object[[\"",
    assay,
    "\"]])\n",
    "  SeuratObject::Cells(object[[\"",
    assay,
    "\"]], layer = \"<layer>\")\n",
    "Rename unrelated prefix layers or join the intended representation ",
    "explicitly with SeuratObject::JoinLayers().",
    call. = FALSE
  )
}

#' Resolve one requested Seurat v5 layer
#'
#' Exact physical layers are authoritative. If an exact root is absent,
#' prefix candidates are accepted only when their cell memberships form one
#' unique complete partition. This function never performs cross-semantic
#' fallback; the facade applies that policy after same-root resolution.
#'
#' @keywords internal
#' @noRd
.resolve_seurat_v5_layer <- function(
  seurat,
  assay,
  requested_layer,
  join_samples = TRUE,
  verbose = FALSE
) {
  assay_object <- seurat[[assay]]
  layer_names <- SeuratObject::Layers(assay_object)
  assay_cells <- SeuratObject::Cells(assay_object)

  if (requested_layer %in% layer_names) {
    return(list(
      data = suppressWarnings(
        SeuratObject::LayerData(assay_object, layer = requested_layer)
      ),
      requested = requested_layer,
      resolved = requested_layer,
      joined = FALSE,
      candidates = character()
    ))
  }

  ## JoinLayers uses a broad `^root` search and can consume names such as
  ## `dataBackup`. Keep that broad set for protection, but only `<root>.*`
  ## names can establish a sample partition for the requested logical root.
  ## Conflating these two sets is what allowed an unrelated custom root to
  ## impersonate `data`.
  protected_candidates <- .layer_prefix_candidates(
    assay_object,
    requested_layer
  )
  candidates <- .layer_partition_candidates(
    protected_candidates,
    requested_layer
  )
  if (length(candidates) == 0L) {
    return(list(
      data = NULL,
      requested = requested_layer,
      resolved = NULL,
      joined = FALSE,
      candidates = protected_candidates
    ))
  }

  memberships <- lapply(
    candidates,
    function(layer) {
      SeuratObject::Cells(assay_object, layer = layer)
    }
  )
  names(memberships) <- candidates
  ## Prefer a complete partition made from direct `<root>.<sample>` children.
  ## Deeper names remain eligible when direct children cannot cover the assay,
  ## because sample labels may themselves contain dots. If every selected name
  ## shares one deeper root, however, the same physical layers are equally a
  ## split custom root (for example `data.imputed.s1/s2`). There is no
  ## provenance in Assay5 that can disambiguate those meanings, so fail closed.
  direct_candidates <- .direct_layer_partition_candidates(
    candidates,
    requested_layer
  )
  direct_partition <- .find_layer_partition(
    assay_cells,
    memberships[direct_candidates]
  )
  partition <- if (identical(direct_partition$status, "unique")) {
    direct_partition
  } else {
    .find_layer_partition(assay_cells, memberships)
  }

  if (identical(partition$status, "ambiguous")) {
    candidate_counts <- lengths(memberships)
    stop(
      "Layer `",
      requested_layer,
      "` in assay `",
      assay,
      "` has more than one valid cell partition: ",
      .format_layer_solutions(partition$solutions),
      ". Refusing to choose one silently.\n",
      "Prefix candidates: ",
      paste(candidates, collapse = ", "),
      "\nLayer cell counts: ",
      .format_layer_counts(candidate_counts),
      "\nInspect candidates with:\n",
      "  SeuratObject::Layers(object[[\"",
      assay,
      "\"]])\n",
      "  SeuratObject::Cells(object[[\"",
      assay,
      "\"]], layer = \"<layer>\")\n",
      "Rename unrelated prefix layers or join the intended representation ",
      "explicitly.",
      call. = FALSE
    )
  }

  nested_root <- if (identical(direct_partition$status, "unique")) {
    NULL
  } else if (identical(partition$status, "unique")) {
    .nested_partition_root(requested_layer, partition$layers)
  } else {
    NULL
  }
  if (!is.null(nested_root)) {
    stop(
      "Layers ",
      paste(partition$layers, collapse = ", "),
      " form a complete partition for both requested root `",
      requested_layer,
      "` and deeper custom root `",
      nested_root,
      "`. Their meaning cannot be inferred safely from names alone. Request `",
      nested_root,
      "` explicitly if that is the intended data, or join the intended `",
      requested_layer,
      "` layers explicitly with SeuratObject::JoinLayers().",
      call. = FALSE
    )
  }

  partial_candidates <- candidates[
    lengths(memberships) > 0L &
      lengths(memberships) < length(assay_cells)
  ]
  if (identical(partition$status, "none")) {
    failure <- NULL
    if (length(partial_candidates) > 0L) {
      candidate_claims <- tabulate(
        match(
          unlist(memberships[partial_candidates], use.names = FALSE),
          assay_cells
        ),
        nbins = length(assay_cells)
      )
      missing_cells <- assay_cells[candidate_claims == 0L]
      failure <- list(
        candidate_counts = lengths(memberships),
        covered_count = sum(candidate_claims > 0L),
        total_count = length(assay_cells),
        overlap_count = sum(candidate_claims > 1L),
        missing_count = length(missing_cells),
        missing_examples = utils::head(missing_cells, 5L)
      )
    }
    return(list(
      data = NULL,
      requested = requested_layer,
      resolved = NULL,
      joined = FALSE,
      candidates = candidates,
      failure = failure
    ))
  }

  if (!isTRUE(join_samples)) {
    stop(
      "Exact layer `",
      requested_layer,
      "` is absent, but split layers ",
      paste(partition$layers, collapse = ", "),
      " form a complete partition. Because `join_samples = FALSE`, they were ",
      "not merged; set `join_samples = TRUE` to merge them.",
      call. = FALSE
    )
  }

  ## JoinLayers must not decide whether a mixed in-memory/disk partition is
  ## supported. Inspect every selected member in deterministic order; otherwise
  ## behaviour depends on which sample suffix sorts first.
  for (partition_layer in partition$layers) {
    source_matrix <- suppressWarnings(
      SeuratObject::LayerData(
        assay_object,
        layer = partition_layer
      )
    )
    if (!.supported_expression_matrix(source_matrix)) {
      return(list(
        data = source_matrix,
        requested = requested_layer,
        resolved = partition_layer,
        joined = FALSE,
        candidates = candidates
      ))
    }
  }

  if (verbose) {
    message(
      "[",
      format(Sys.time(), "%H:%M:%S"),
      "] Joining layer partition for `",
      requested_layer,
      "`: ",
      paste(partition$layers, collapse = ", ")
    )
  }

  ## JoinLayers uses Layers(search = layers). Use the same escaped, anchored
  ## search for discovery and joining. Remove every matching layer outside the
  ## selected partition from the local value so custom names such as
  ## `data.imputed`, `data_imputed`, and `dataBackup` cannot be consumed.
  local_object <- seurat
  protected <- base::setdiff(protected_candidates, partition$layers)
  for (layer in protected) {
    suppressWarnings(
      SeuratObject::LayerData(
        local_object[[assay]],
        layer = layer
      ) <- NULL
    )
  }
  local_object <- SeuratObject::JoinLayers(
    local_object,
    assay = assay,
    layers = .layer_prefix_pattern(requested_layer),
    new = requested_layer
  )
  joined_data <- suppressWarnings(
    SeuratObject::LayerData(
      local_object[[assay]],
      layer = requested_layer
    )
  )
  if (
    .supported_expression_matrix(joined_data) &&
      setequal(colnames(joined_data), assay_cells) &&
      !identical(colnames(joined_data), assay_cells)
  ) {
    joined_data <- joined_data[, assay_cells, drop = FALSE]
  }

  list(
    data = joined_data,
    requested = requested_layer,
    resolved = requested_layer,
    joined = TRUE,
    candidates = candidates
  )
}

#' Filter candidate fallback layers to the same semantic class as the request
#'
#' Given a requested layer name (e.g. "data", "counts", "scale.data") and the
#' layers actually present in an assay, return the acceptable fallback layers.
#' By default only layers sharing the same semantic root are kept, so that a
#' missing "data" layer never silently falls back to "counts" (raw) or
#' "scale.data" (scaled). Seurat v5 split layers ("data.1", "data.s1", ...)
#' share the root of their base layer and are therefore kept. Set
#' \code{allow_cross_semantic = TRUE} for the legacy behaviour where any
#' available layer is an acceptable fallback (requested layer ordered first).
#'
#' @keywords internal
#' @noRd
.filter_same_semantic_layers <- function(
  requested_layer,
  available_layers,
  allow_cross_semantic = FALSE
) {
  root_of <- .layer_semantic_root

  if (isTRUE(allow_cross_semantic)) {
    return(unique(c(
      intersect(requested_layer, available_layers),
      setdiff(available_layers, requested_layer)
    )))
  }

  requested_root <- root_of(requested_layer)
  available_layers[root_of(available_layers) == requested_root]
}

#' Validate expression-matrix cell coverage and order
#'
#' @keywords internal
#' @noRd
.validate_expression_cells <- function(
  expression_data,
  object_cells,
  assay,
  requested_layer,
  resolved_layer = requested_layer
) {
  matrix_cells <- colnames(expression_data)
  if (
    is.null(matrix_cells) ||
      length(matrix_cells) == 0L ||
      anyNA(matrix_cells) ||
      any(!nzchar(matrix_cells)) ||
      anyDuplicated(matrix_cells)
  ) {
    stop(
      "Expression matrix must have unique, non-empty cell names.\n",
      "Assay `",
      assay,
      "`, requested `",
      requested_layer,
      "`, resolved `",
      resolved_layer,
      "`.",
      call. = FALSE
    )
  }
  if (
    !is.character(object_cells) ||
      length(object_cells) == 0L ||
      anyNA(object_cells) ||
      any(!nzchar(object_cells)) ||
      anyDuplicated(object_cells)
  ) {
    stop(
      "`object_cells` must contain unique, non-empty cell names.",
      call. = FALSE
    )
  }

  missing_cells <- base::setdiff(object_cells, matrix_cells)
  unexpected_cells <- base::setdiff(matrix_cells, object_cells)
  if (length(missing_cells) > 0L || length(unexpected_cells) > 0L) {
    format_examples <- function(cells) {
      if (length(cells) == 0L) {
        return("none")
      }
      paste(utils::head(cells, 5L), collapse = ", ")
    }
    stop(
      "Expression matrix cell coverage mismatch for assay `",
      assay,
      "` (requested `",
      requested_layer,
      "`, resolved `",
      resolved_layer,
      "`).\n",
      "Missing object cells (",
      length(missing_cells),
      "): ",
      format_examples(missing_cells),
      "\n",
      "Unexpected matrix cells (",
      length(unexpected_cells),
      "): ",
      format_examples(unexpected_cells),
      "\n",
      "The matrix covers ",
      length(matrix_cells),
      " of the object's ",
      length(object_cells),
      " cells. Inspect the assay layers and JoinLayers result before continuing.",
      call. = FALSE
    )
  }

  if (identical(matrix_cells, object_cells)) {
    return(expression_data)
  }
  expression_data[, object_cells, drop = FALSE]
}

#' @keywords internal
#' @noRd
.getExpressionMatrix <- function(
  seurat,
  assay = "RNA",
  slot = "data",
  join_samples = TRUE,
  allow_cross_semantic_fallback = FALSE,
  verbose = FALSE,
  return_resolution = FALSE
) {
  if (
    !is.logical(return_resolution) ||
      length(return_resolution) != 1L ||
      is.na(return_resolution)
  ) {
    stop("`return_resolution` must be TRUE or FALSE.", call. = FALSE)
  }
  seurat_version <- as.character(utils::packageVersion("Seurat"))
  is_seurat_v5 <- utils::compareVersion(seurat_version, "5.0.0") >= 0
  requested_layer <- as.character(slot)
  resolved_layer <- requested_layer
  joined_layers <- FALSE

  if (!is_seurat_v5) {
    expr_matrix <- tryCatch(
      {
        Seurat::GetAssayData(seurat, assay = assay, slot = slot)
      },
      error = function(e) {
        stop(
          "Failed to get expression matrix from Seurat v4 object.\n",
          "  Assay: ",
          assay,
          "\n",
          "  Slot: ",
          slot,
          "\n",
          "  Error: ",
          e$message,
          "\n",
          "Suggestions:\n",
          "  1. Check if the assay '",
          assay,
          "' exists in your Seurat object using: names(seurat@assays)\n",
          "  2. Check if the slot '",
          slot,
          "' exists in the assay using: names(seurat@assays$",
          assay,
          "@layers)\n",
          "  3. Try using a different assay or slot (e.g., assay='RNA', slot='data')"
        )
      }
    )
  } else {
    if (!assay %in% names(seurat@assays)) {
      stop(
        "Assay '",
        assay,
        "' not found in Seurat v5 object.\n",
        "Available assays: ",
        paste(names(seurat@assays), collapse = ", "),
        "\n",
        "Suggestions:\n",
        "  1. Use one of the available assays listed above\n",
        "  2. Check your Seurat object structure using: names(seurat@assays)"
      )
    }

    layer_name <- as.character(slot)
    if (length(layer_name) != 1L || is.na(layer_name) || !nzchar(layer_name)) {
      stop("`slot` must be one non-empty layer name.", call. = FALSE)
    }

    resolution <- .resolve_seurat_v5_layer(
      seurat = seurat,
      assay = assay,
      requested_layer = layer_name,
      join_samples = join_samples,
      verbose = verbose
    )

    ## Cross-semantic fallback is deliberately narrow and deterministic.
    ## Arbitrary requested roots never silently turn into a standard expression
    ## class. Standard roots are tried in a fixed compatibility order, and a
    ## replacement is accepted only if it covers every assay cell.
    if (
      is.null(resolution$data) &&
        isTRUE(allow_cross_semantic_fallback) &&
        layer_name %in% .cerebro_fallback_roots
    ) {
      assay_cells <- SeuratObject::Cells(seurat[[assay]])
      fallback_roots <- base::setdiff(
        .cerebro_fallback_roots,
        layer_name
      )
      for (fallback_root in fallback_roots) {
        fallback <- .resolve_seurat_v5_layer(
          seurat = seurat,
          assay = assay,
          requested_layer = fallback_root,
          join_samples = join_samples,
          verbose = verbose
        )
        if (is.null(fallback$data)) {
          next
        }

        fallback_is_disk_backed <- .disk_backed_expression_matrix(
          fallback$data
        )
        fallback_is_complete <- fallback_is_disk_backed ||
          (.supported_expression_matrix(fallback$data) &&
            setequal(colnames(fallback$data), assay_cells))
        if (!fallback_is_complete) {
          next
        }

        warning(
          "Requested layer `",
          layer_name,
          "` not found in `",
          assay,
          "` assay; falling back to `",
          fallback_root,
          "`. Values now reflect `",
          fallback_root,
          "`, not `",
          layer_name,
          "`.",
          call. = FALSE
        )
        resolution <- fallback
        break
      }
    }

    if (
      is.null(resolution$data) &&
        !is.null(resolution$failure)
    ) {
      .stop_incomplete_layer_resolution(
        resolution = resolution,
        assay = assay,
        requested_layer = layer_name
      )
    }

    if (is.null(resolution$data)) {
      available_layers <- SeuratObject::Layers(seurat[[assay]])
      prefix_detail <- if (length(resolution$candidates) > 0L) {
        paste0(
          "Exact layer `",
          layer_name,
          "` is absent. Prefix-matching layers (",
          paste(resolution$candidates, collapse = ", "),
          ") do not form a split partition.\n"
        )
      } else {
        ""
      }
      stop(
        paste0(
          prefix_detail,
          "Layer `",
          layer_name,
          "` could not be found in `",
          assay,
          "` assay, and no same-semantic fallback layer is available.\n",
          "Available layers: ",
          paste(available_layers, collapse = ", "),
          "\n",
          "Suggestions:\n",
          "  1. Use one of the available layers listed above\n",
          "  2. Check if the assay has been properly initialized\n",
          "  3. Verify the assay structure using: Layers(seurat[[\"",
          assay,
          "\"]])\n",
          "  4. To allow cross-semantic fallback (e.g. data -> counts), call ",
          ".getExpressionMatrix(..., allow_cross_semantic_fallback = TRUE)"
        ),
        call. = FALSE
      )
    }

    expr_matrix <- resolution$data
    resolved_layer <- resolution$resolved
    joined_layers <- isTRUE(resolution$joined)
  }

  if (is.null(expr_matrix)) {
    stop(
      "Expression matrix is NULL.\n",
      "  Assay: ",
      assay,
      "\n",
      "  Slot/Layer: ",
      slot,
      "\n",
      "  Seurat version: ",
      seurat_version,
      "\n",
      "Suggestions:\n",
      "  1. Verify that the assay contains data\n",
      "  2. Check the assay structure using: ",
      if (is_seurat_v5) {
        paste0('Layers(seurat[["', assay, '"]])')
      } else {
        paste0("names(seurat@assays$", assay, "@layers)")
      },
      "\n",
      "  3. Try using a different assay or slot"
    )
  }

  if (is.matrix(expr_matrix) || inherits(expr_matrix, "dgCMatrix")) {
    if (nrow(expr_matrix) == 0 || ncol(expr_matrix) == 0) {
      stop(
        "Expression matrix is empty (0 rows or 0 columns).\n",
        "  Assay: ",
        assay,
        "\n",
        "  Slot/Layer: ",
        slot,
        "\n",
        "  Matrix dimensions: ",
        nrow(expr_matrix),
        " rows x ",
        ncol(expr_matrix),
        " columns\n",
        "Suggestions:\n",
        "  1. Check if your Seurat object contains cells and genes\n",
        "  2. Verify using: ncol(seurat) (cells) and nrow(seurat) (genes)\n",
        "  3. Ensure the assay has been properly populated with data"
      )
    }
  } else {
    ## A disk-backed assay lands here: BPCells hands back an `IterableMatrix`
    ## (often wrapped, e.g. `RenameDims`), DelayedArray a `DelayedMatrix`. The
    ## class name alone says nothing about what went wrong or what to do, and
    ## an object whose matrix already lives on disk is a reasonable thing to
    ## want to export -- reading it is simply not supported yet. Say which of
    ## the two situations this is.
    disk_backed <- inherits(
      expr_matrix,
      c("IterableMatrix", "DelayedMatrix", "DelayedArray")
    )
    stop(
      "Expression matrix is not a valid matrix type.\n",
      "  Expected: matrix or dgCMatrix\n",
      "  Received: ",
      class(expr_matrix)[1],
      "\n",
      "  Assay: ",
      assay,
      "\n",
      "  Slot/Layer: ",
      slot,
      "\n",
      if (!identical(resolved_layer, requested_layer)) {
        paste0("  Physical source layer: ", resolved_layer, "\n")
      } else {
        ""
      },
      if (disk_backed) {
        ## Convert every layer rather than the requested one. On a split assay
        ## `LayerData(layer = "counts")` reaches for a layer that is not there
        ## and settles for one of the `counts.*` layers, so converting "the"
        ## layer would quietly leave the object holding a single sample -- the
        ## very failure this file exists to prevent. Looping is correct whether
        ## or not the assay is split.
        paste0(
          "This assay's data already lives on disk. Reading a disk-backed ",
          "assay is not supported yet; bring every layer into memory first ",
          "(this materialises the whole matrix, which is what the on-disk ",
          "backend was avoiding):\n",
          "  for (nm in SeuratObject::Layers(seurat[[\"",
          assay,
          "\"]])) {\n",
          "    SeuratObject::LayerData(seurat, assay = \"",
          assay,
          "\", layer = nm) <-\n",
          "      methods::as(\n",
          "        SeuratObject::LayerData(seurat, assay = \"",
          assay,
          "\", layer = nm),\n",
          "        \"dgCMatrix\"\n",
          "      )\n",
          "  }\n",
          "Convert every layer, not just `",
          slot,
          "`: on a split assay a request for a root layer settles for one of ",
          "its `<root>.<sample>` layers, so converting only that one would ",
          "leave the object holding a single sample.\n",
          "Note that this is unrelated to `expression_matrix_mode`, which ",
          "controls how the exported .crb stores its matrix, not how the ",
          "source object holds it."
        )
      } else {
        paste0(
          "Suggestions:\n",
          "  1. Check the assay structure in your Seurat object\n",
          "  2. Verify the data type using: class(GetAssayData(seurat, assay = \"",
          assay,
          "\"",
          if (is_seurat_v5) {
            paste0(', layer = "', slot, '"))')
          } else {
            paste0(', slot = "', slot, '"))')
          }
        )
      }
    )
  }

  if (isTRUE(return_resolution)) {
    return(list(
      data = expr_matrix,
      assay = as.character(assay),
      requested = requested_layer,
      resolved = resolved_layer,
      joined = joined_layers,
      fallback = !identical(requested_layer, resolved_layer)
    ))
  }
  return(expr_matrix)
}

# Internal utilities shared across extraction helpers ---------------------------
#' @keywords internal
#' @noRd

.spx_msg <- function(..., verbose = FALSE) {
  if (isTRUE(verbose)) {
    message("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
  }
}

.spx_try <- function(expr) {
  suppressWarnings(suppressMessages(try(expr, silent = TRUE)))
}

.spx_is_try_error <- function(x) inherits(x, "try-error")

.spx_collapse <- function(x) {
  if (length(x) == 0) "none" else paste(x, collapse = ", ")
}

.spx_escape_regex <- function(x) {
  gsub("([\\^$.|?*+()\\[\\]{}\\\\\\-])", "\\\\\\1", x, perl = TRUE)
}

.spx_is_matrix_like <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  d <- .spx_try(dim(x))
  if (.spx_is_try_error(d) || is.null(d) || length(d) != 2) {
    return(FALSE)
  }
  TRUE
}

.spx_has_slot <- function(obj, slot_name) {
  if (!isS4(obj)) {
    return(FALSE)
  }
  slot_name %in% methods::slotNames(obj)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Extract spatial coordinates and expression from a Seurat object --------------
#
# Multi-strategy extraction supporting Visium, FOV, Xenium, and generic images:
#   1. Image-based: GetTissueCoordinates with centroids/segmentation/molecules
#      fallback chain + direct slot access (S4)
#   2. Metadata: scan meta.data columns for coordinate-like columns
#   3. Automatic x/y column detection from 70+ common naming conventions
#   4. Duplicate-cell summarisation (vectorised rowsum, not per-cell rbind)
#   5. Returns requested and physical expression-layer names separately
#
#' @keywords internal
#' @noRd
.getSpatialData <- function(
  object,
  image = NULL,
  layer = "data",
  assay = NULL,
  slot = NULL,
  coord_source = c(
    "auto",
    "centroids",
    "segmentation",
    "metadata",
    "molecules"
  ),
  coord_cols = NULL,
  join_samples = TRUE,
  image_policy = c("first", "all"),
  allow_molecule_fallback = FALSE,
  warn_on_image_overlap = TRUE,
  verbose = FALSE,
  expression_data = NULL,
  expression_layer = NULL
) {
  coord_source <- match.arg(coord_source)
  image_policy <- match.arg(image_policy)

  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Seurat package is required but not installed.", call. = FALSE)
  }
  if (!requireNamespace("SeuratObject", quietly = TRUE)) {
    stop("SeuratObject package is required but not installed.", call. = FALSE)
  }

  if (is.null(slot)) {
    slot <- layer
  }
  if (is.null(expression_layer)) {
    expression_layer <- slot
  }
  if (
    !is.character(expression_layer) ||
      length(expression_layer) != 1L ||
      is.na(expression_layer) ||
      !nzchar(expression_layer)
  ) {
    stop("`expression_layer` must be one non-empty layer name.", call. = FALSE)
  }
  if (is.null(assay)) {
    assay <- Seurat::DefaultAssay(object)
  }

  msg <- function(...) .spx_msg(..., verbose = verbose)

  get_gtc_fun <- function() {
    for (pkg in c("Seurat", "SeuratObject")) {
      if (
        requireNamespace(pkg, quietly = TRUE) &&
          exists(
            "GetTissueCoordinates",
            envir = asNamespace(pkg),
            inherits = FALSE
          )
      ) {
        return(get("GetTissueCoordinates", envir = asNamespace(pkg)))
      }
    }
    NULL
  }
  GetTC <- function(x, ...) {
    fun <- get_gtc_fun()
    if (is.null(fun)) {
      return(structure("GetTissueCoordinates not found", class = "try-error"))
    }
    .spx_try(fun(x, ...))
  }

  ## Drop columns with NA/empty names and de-duplicate the rest. Some coordinate
  ## sources (e.g. Slide-seq `GetTissueCoordinates`) return a data.frame with a
  ## stray unnamed column; such a name later breaks column subsetting by name.
  sanitize_cols <- function(df) {
    if (is.null(df) || ncol(df) == 0) {
      return(df)
    }
    nms <- colnames(df)
    keep <- !is.na(nms) & nzchar(nms)
    if (!all(keep)) {
      df <- df[, keep, drop = FALSE]
    }
    if (ncol(df) > 0) {
      colnames(df) <- make.unique(colnames(df))
    }
    df
  }

  as_df <- function(x) {
    if (is.null(x) || .spx_is_try_error(x)) {
      return(NULL)
    }
    if (is.data.frame(x)) {
      return(sanitize_cols(x))
    }
    if (is.matrix(x)) {
      return(sanitize_cols(as.data.frame(x, stringsAsFactors = FALSE)))
    }
    out <- .spx_try(as.data.frame(x, stringsAsFactors = FALSE))
    if (.spx_is_try_error(out) || is.null(out)) {
      return(NULL)
    }
    sanitize_cols(out)
  }

  find_xy_cols <- function(df, user_cols = NULL, hard_error = FALSE) {
    .spx_find_coordinate_columns(
      df,
      coord_cols = user_cols,
      hard_error = hard_error
    )
  }

  find_best_cell_col <- function(df, valid_cells) {
    .spx_find_barcode_column(df, valid_cells)
  }

  summarise_duplicate_cells <- function(df) {
    if (nrow(df) == 0) {
      return(df)
    }
    cell_ids <- rownames(df)
    if (!anyDuplicated(cell_ids)) {
      return(df)
    }

    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    other_cols <- setdiff(names(df), num_cols)
    f <- factor(cell_ids, levels = unique(cell_ids))

    num_summary <- if (length(num_cols) > 0) {
      mat <- as.matrix(df[, num_cols, drop = FALSE])
      mat[!is.finite(mat)] <- NA
      not_na <- !is.na(mat)
      mat0 <- mat
      mat0[is.na(mat0)] <- 0
      sums <- rowsum(mat0, group = f, reorder = FALSE)
      cnts <- rowsum(not_na + 0, group = f, reorder = FALSE)
      means <- sums / cnts
      means[!is.finite(means)] <- NA
      as.data.frame(means, stringsAsFactors = FALSE)
    } else {
      data.frame(row.names = levels(f))
    }

    if (length(other_cols) > 0) {
      first_rows <- !duplicated(f)
      other_summary <- df[first_rows, other_cols, drop = FALSE]
      rownames(other_summary) <- as.character(f[first_rows])
      other_summary <- other_summary[rownames(num_summary), , drop = FALSE]
      out <- cbind(num_summary, other_summary)
    } else {
      out <- num_summary
    }
    rownames(out) <- levels(f)
    out
  }

  standardize_coord_df <- function(
    df,
    valid_cells,
    source = NA_character_,
    image_name = NA_character_,
    user_cols = NULL
  ) {
    df <- as_df(df)
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }

    xy <- find_xy_cols(df, user_cols = user_cols, hard_error = FALSE)
    if (is.null(xy)) {
      return(NULL)
    }

    df$x <- suppressWarnings(as.numeric(df[[xy$x]]))
    df$y <- suppressWarnings(as.numeric(df[[xy$y]]))
    if (all(is.na(df$x)) || all(is.na(df$y))) {
      return(NULL)
    }

    current_overlap <- if (!is.null(rownames(df))) {
      sum(rownames(df) %in% valid_cells)
    } else {
      0
    }

    best_col <- find_best_cell_col(df, valid_cells)
    if (!is.null(best_col)) {
      best_overlap <- sum(as.character(df[[best_col]]) %in% valid_cells)
      if (best_overlap >= current_overlap) {
        rownames(df) <- as.character(df[[best_col]])
      }
    }

    keep <- !is.na(rownames(df)) & rownames(df) != ""
    df <- df[keep, , drop = FALSE]
    if (nrow(df) == 0) {
      return(NULL)
    }

    df <- summarise_duplicate_cells(df)
    if (sum(rownames(df) %in% valid_cells) == 0) {
      return(NULL)
    }

    df$.coordinate_source <- source
    df$.image <- image_name
    df$.cell <- rownames(df)
    df
  }

  rbind_fill <- function(dfs) {
    dfs <- Filter(Negate(is.null), dfs)
    if (length(dfs) == 0) {
      return(NULL)
    }
    all_cols <- unique(unlist(lapply(dfs, colnames)))
    dfs <- lapply(dfs, function(d) {
      for (cc in setdiff(all_cols, colnames(d))) {
        d[[cc]] <- NA
      }
      d[, all_cols, drop = FALSE]
    })
    out <- do.call(rbind, dfs)

    if (".cell" %in% colnames(out)) {
      dup <- duplicated(out$.cell)
      if (any(dup) && isTRUE(warn_on_image_overlap)) {
        warning(
          "Duplicate cells found across images; keeping first occurrence. ",
          "n duplicates = ",
          sum(dup),
          call. = FALSE
        )
      }
      out <- out[!dup, , drop = FALSE]
      rownames(out) <- out$.cell
    }
    out
  }

  # 1. Expression matrix ------------------------------------------------------
  if (!is.null(expression_data)) {
    expr_data <- expression_data
  } else if (exists(".getExpressionMatrix", mode = "function")) {
    expr_data <- .getExpressionMatrix(
      seurat = object,
      assay = assay,
      slot = slot,
      join_samples = join_samples,
      verbose = verbose
    )
  } else {
    seurat_version <- as.character(utils::packageVersion("Seurat"))
    is_v5 <- utils::compareVersion(seurat_version, "5.0.0") >= 0
    expr_data <- if (is_v5) {
      Seurat::GetAssayData(object, assay = assay, layer = slot)
    } else {
      Seurat::GetAssayData(object, assay = assay, slot = slot)
    }
  }

  valid_cells <- colnames(expr_data)
  if (is.null(valid_cells) || length(valid_cells) == 0) {
    stop("Expression matrix has no cell names.", call. = FALSE)
  }

  # 2. Resolve images ---------------------------------------------------------
  img_avail <- .spx_try(Seurat::Images(object))
  if (.spx_is_try_error(img_avail) || is.null(img_avail)) {
    img_avail <- character(0)
  }

  if (is.null(image)) {
    image_names <- if (length(img_avail) == 0) {
      character(0)
    } else if (image_policy == "all") {
      img_avail
    } else {
      img_avail[1]
    }
  } else if (length(image) == 1 && identical(image, "all")) {
    image_names <- img_avail
  } else {
    image_names <- image
  }

  # 3. Image-based coordinate extraction --------------------------------------
  extract_from_one_image <- function(image_name) {
    msg("Extracting coords from image: ", image_name)
    image_obj <- .spx_try(object[[image_name]])
    if (.spx_is_try_error(image_obj) || is.null(image_obj)) {
      msg("Image object not found: ", image_name)
      return(NULL)
    }

    candidates <- list()
    candidates[["object.GetTissueCoordinates"]] <-
      GetTC(object, image = image_name)
    candidates[["image.GetTissueCoordinates.default"]] <- GetTC(image_obj)

    which_values <- switch(
      coord_source,
      "auto" = c("centroids", "cells", "segmentation"),
      "centroids" = c("centroids"),
      "segmentation" = c("segmentation"),
      "metadata" = character(0),
      "molecules" = c("molecules")
    )
    if (isTRUE(allow_molecule_fallback) && coord_source == "auto") {
      which_values <- unique(c(which_values, "molecules"))
    }

    for (ww in which_values) {
      candidates[[paste0("image.GetTissueCoordinates.", ww)]] <-
        GetTC(image_obj, which = ww)
    }

    if (isS4(image_obj)) {
      sn <- methods::slotNames(image_obj)
      direct_slots <- intersect(
        c(
          "coordinates",
          "coords",
          "centroids",
          "cells",
          "cell.centroids",
          "cell_centroids"
        ),
        sn
      )
      for (ss in direct_slots) {
        candidates[[paste0("slot.", ss)]] <- .spx_try(methods::slot(
          image_obj,
          ss
        ))
      }

      if ("boundaries" %in% sn) {
        boundaries <- .spx_try(methods::slot(image_obj, "boundaries"))
        if (!.spx_is_try_error(boundaries) && !is.null(boundaries)) {
          if (is.environment(boundaries)) {
            boundaries <- as.list(boundaries)
          }
          if (is.list(boundaries) && length(boundaries) > 0) {
            for (bn in names(boundaries)) {
              bobj <- boundaries[[bn]]
              candidates[[paste0(
                "boundary.",
                bn,
                ".GetTissueCoordinates"
              )]] <- GetTC(bobj)
              if (isS4(bobj)) {
                for (ss in intersect(
                  c("coordinates", "coords"),
                  methods::slotNames(bobj)
                )) {
                  candidates[[paste0("boundary.", bn, ".slot.", ss)]] <-
                    .spx_try(methods::slot(bobj, ss))
                }
              }
            }
          }
        }
      }
    }

    processed <- list()
    for (nm in names(candidates)) {
      processed[[nm]] <- standardize_coord_df(
        candidates[[nm]],
        valid_cells = valid_cells,
        source = nm,
        image_name = image_name,
        user_cols = coord_cols
      )
      if (!is.null(processed[[nm]])) {
        msg(
          "Match for `",
          image_name,
          "`: ",
          nm,
          " (cells=",
          nrow(processed[[nm]]),
          ")"
        )
        if (coord_source == "auto") return(processed[[nm]])
      }
    }
    rbind_fill(processed)
  }

  coords_from_images <- NULL
  if (length(image_names) > 0 && coord_source != "metadata") {
    coords_from_images <- rbind_fill(lapply(
      image_names,
      extract_from_one_image
    ))
  }

  # 4. Metadata fallback ------------------------------------------------------
  extract_from_metadata <- function() {
    msg("Trying metadata coordinate fallback.")
    meta <- .spx_try(object[[]])
    if (.spx_is_try_error(meta) || is.null(meta) || nrow(meta) == 0) {
      return(NULL)
    }

    meta$.cell <- rownames(meta)
    if (!is.null(image) && length(image) == 1 && !identical(image, "all")) {
      image_cols <- intersect(
        c(
          "image",
          "Image",
          "slice",
          "Slice",
          "fov",
          "FOV",
          "field",
          "field_of_view",
          "sample",
          "sample_id"
        ),
        colnames(meta)
      )
      for (ic in image_cols) {
        vals <- as.character(meta[[ic]])
        if (any(vals == image, na.rm = TRUE)) {
          meta <- meta[vals == image, , drop = FALSE]
          break
        }
      }
    }

    standardize_coord_df(
      meta,
      valid_cells = valid_cells,
      source = "metadata",
      image_name = if (is.null(image)) {
        NA_character_
      } else {
        paste(image, collapse = ",")
      },
      user_cols = coord_cols
    )
  }

  coords <- coords_from_images
  if (is.null(coords) || nrow(coords) == 0) {
    coords <- extract_from_metadata()
  }

  if ((is.null(coords) || nrow(coords) == 0) && !is.null(coord_cols)) {
    stop(
      "Could not find `coord_cols` (",
      paste(coord_cols, collapse = ", "),
      ") in any candidate coordinate source.",
      call. = FALSE
    )
  }

  if (is.null(coords) || nrow(coords) == 0) {
    stop(
      "Could not retrieve spatial coordinates from the Seurat object.\n",
      "Available images: ",
      .spx_collapse(img_avail),
      "\n",
      "Try: Seurat::Images(object), class(object[[image]]), or pass coord_cols=.",
      call. = FALSE
    )
  }

  # 5. Match expression and coords --------------------------------------------
  common_cells <- intersect(rownames(coords), colnames(expr_data))
  if (length(common_cells) == 0) {
    stop(
      "No common cells between coords (",
      nrow(coords),
      ") and expression (",
      ncol(expr_data),
      "). assay=",
      assay,
      ", layer=",
      slot,
      call. = FALSE
    )
  }

  coords <- coords[common_cells, , drop = FALSE]
  expr_data <- expr_data[, common_cells, drop = FALSE]

  front <- intersect(
    c(".cell", ".image", ".coordinate_source", "x", "y"),
    colnames(coords)
  )
  coords <- coords[, c(front, setdiff(colnames(coords), front)), drop = FALSE]

  list(
    coordinates = coords,
    expression = expr_data,
    assay = assay,
    requested_layer = slot,
    layer = expression_layer,
    image = unique(coords$.image),
    coordinate_source = unique(coords$.coordinate_source)
  )
}
