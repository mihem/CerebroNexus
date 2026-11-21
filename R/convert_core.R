## Format-independent conversion steps -----------------------------------------
##
## The pre-export preparation shared by convertSeuratToCerebro() and
## convertSCEToCerebro(). They work on the cell meta data (`meta`: anything with
## names(), [[, [[<- and rownames(), i.e. a data.frame or a DataFrame) and on
## the free-form `misc` list (Seurat @misc, SCE metadata()), and hand the edited
## copies back; they never see the container object.

#' @keywords internal
#' @noRd
.convertValidateGroups <- function(meta, groups, max_group_levels, verbose) {
  missing_groups <- groups[!groups %in% names(meta)]

  if (length(missing_groups) > 0) {
    if (length(missing_groups) == length(groups)) {
      # All groups are missing - stop execution
      stop(
        paste0(
          "All specified groups are missing from metadata: ",
          paste(missing_groups, collapse = ", "),
          "\nAvailable columns: ",
          paste(names(meta), collapse = ", ")
        ),
        call. = FALSE
      )
    } else {
      # Some groups are missing - warn user
      warning(
        paste0(
          "Some specified groups are missing from metadata and will be skipped: ",
          paste(missing_groups, collapse = ", ")
        ),
        call. = FALSE
      )
      # Remove missing groups from the groups vector
      groups <- groups[groups %in% names(meta)]
    }
  }

  # Convert group values to character and check level counts ----------------##
  groups_to_remove <- character(0)

  for (group_name in groups) {
    # Convert to character
    meta[[group_name]] <- as.character(meta[[
      group_name
    ]])

    # Count unique levels (excluding NA)
    unique_levels <- unique(meta[[group_name]][
      !is.na(meta[[group_name]])
    ])
    n_levels <- length(unique_levels)

    # Check if exceeds maximum
    if (n_levels > max_group_levels) {
      if (verbose) {
        message(paste0(
          "[WARNING] Group '",
          group_name,
          "' has ",
          n_levels,
          " unique levels (> ",
          max_group_levels,
          "), will be excluded."
        ))
      }
      groups_to_remove <- c(groups_to_remove, group_name)
    }
  }

  # Remove groups with too many levels
  if (length(groups_to_remove) > 0) {
    groups <- groups[!groups %in% groups_to_remove]

    # Check if any groups remain
    if (length(groups) == 0) {
      stop(
        paste0(
          "All groups have been excluded due to exceeding max_group_levels (",
          max_group_levels,
          "). Consider increasing max_group_levels or ",
          "using different grouping variables."
        ),
        call. = FALSE
      )
    }

    if (verbose) {
      message(paste0(
        "[INFO] Remaining groups: ",
        paste(groups, collapse = ", ")
      ))
    }
  }

  list(meta = meta, groups = groups)
}

#' @keywords internal
#' @noRd
.convertRenameGroups <- function(
  meta,
  misc,
  groups,
  groups_naming,
  repertoire_sample_col = "orig.ident"
) {
  # Rename group columns according to groups_naming
  if (!is.null(groups_naming) && length(groups_naming) > 0) {
    # Validate groups_naming structure
    if (is.null(names(groups_naming)) || length(names(groups_naming)) == 0) {
      stop(
        "groups_naming must be a named list/vector with names corresponding to existing group names.",
        call. = FALSE
      )
    }
    source_names <- names(groups_naming)
    if (
      anyNA(source_names) ||
        any(!nzchar(source_names)) ||
        anyDuplicated(source_names)
    ) {
      stop(
        "groups_naming source names must be non-empty and unique.",
        call. = FALSE
      )
    }
    # Check if at least one name in groups_naming exists in groups
    valid_names <- names(groups_naming)[names(groups_naming) %in% groups]

    if (length(valid_names) < length(names(groups_naming))) {
      warning(
        paste0(
          "Some names in groups_naming do not exist in the specified groups and will be ignored: ",
          paste(setdiff(names(groups_naming), valid_names), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (length(valid_names) == 0) {
      stop(
        paste0(
          "None of the names in groups_naming exist in the specified groups.\n",
          "groups_naming names: ",
          paste(names(groups_naming), collapse = ", "),
          "\n",
          "Available groups: ",
          paste(groups, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    effective_mappings <- groups_naming[valid_names]
    valid_targets <- vapply(
      effective_mappings,
      function(target) {
        is.character(target) &&
          length(target) == 1L &&
          !is.na(target) &&
          nzchar(target)
      },
      logical(1)
    )
    if (!all(valid_targets)) {
      stop(
        "groups_naming targets must each be a non-empty scalar character name.",
        call. = FALSE
      )
    }
    target_names <- unname(vapply(
      effective_mappings,
      `[[`,
      character(1),
      1L
    ))
    duplicate_target <- duplicated(target_names) |
      duplicated(
        target_names,
        fromLast = TRUE
      )
    if (any(duplicate_target)) {
      stop(
        "Multiple groups map to the same groups_naming target '",
        target_names[which(duplicate_target)[[1L]]],
        "'.",
        call. = FALSE
      )
    }

    # Preflight every effective rename before changing metadata or misc. This
    # keeps the in-memory Seurat object intact if any later mapping conflicts.
    keyed_misc <- c(
      "trees",
      "most_expressed_genes",
      "mean_expression",
      "group_colors"
    )
    for (old_name in valid_names) {
      new_name <- groups_naming[[old_name]]
      if (identical(old_name, new_name)) {
        next
      }
      if (new_name %in% names(meta)) {
        stop(
          "Group rename metadata conflict: '",
          old_name,
          "' -> '",
          new_name,
          "'; the target column already exists.",
          call. = FALSE
        )
      }
      for (collection in keyed_misc) {
        payload <- misc[[collection]]
        if (
          is.list(payload) &&
            old_name %in% names(payload) &&
            new_name %in% names(payload)
        ) {
          stop(
            "Group rename conflict in misc payload '",
            collection,
            "': '",
            old_name,
            "' -> '",
            new_name,
            "'; both keys already exist.",
            call. = FALSE
          )
        }
      }
    }

    for (old_name in valid_names) {
      new_name <- groups_naming[[old_name]]
      if (identical(old_name, new_name)) {
        next
      }
      # Rename column in metadata
      meta[[new_name]] <- meta[[old_name]]
      meta[[old_name]] <- NULL

      # Keep group-indexed analysis payloads aligned with the renamed metadata.
      # exportFromSeurat() registers the new group names before importing these
      # collections, so a stale key would either be dropped (summary tables) or
      # rejected (trees) even though the public rename itself is valid.
      for (collection in keyed_misc) {
        payload <- misc[[collection]]
        if (
          is.list(payload) &&
            old_name %in% names(payload) &&
            !new_name %in% names(payload)
        ) {
          names(payload)[names(payload) == old_name] <- new_name
          misc[[collection]] <- payload
        }
      }

      # Update groups vector with new name
      idx <- which(groups == old_name)
      if (length(idx) > 0) {
        groups[idx] <- new_name
      }
      if (identical(old_name, repertoire_sample_col)) {
        repertoire_sample_col <- new_name
      }
    }
  }

  list(
    meta = meta,
    misc = misc,
    groups = groups,
    repertoire_sample_col = repertoire_sample_col
  )
}

#' @keywords internal
#' @noRd
#' @param group_values Optional named list (grouping variable -> its values). When
#'   the marker table's cluster column holds values of exactly one of them, the
#'   table is stored as ONE table named after that grouping variable, with the
#'   cluster column first and renamed to it. That is the shape the Viewer's
#'   per-group filter expects (and what scanpy's results arrive as); without it,
#'   or when no grouping variable matches, the table is split per cluster value
#'   as the Seurat route has always done.
.convertAttachMarkers <- function(
  misc,
  marker_file,
  marker_method,
  verbose,
  group_values = NULL
) {
  if (!is.null(marker_file) && nzchar(marker_file)) {
    if (!file.exists(marker_file)) {
      stop("marker_file not found: ", marker_file, call. = FALSE)
    }

    markers_df <- .readMarkerFile(marker_file, verbose)

    # Attach to Seurat object under misc$marker_genes.
    # exportFromSeurat expects list(method = list(group = df)).
    # Split markers_df by 'cluster' / 'group' column when available,
    # otherwise dump as a single 'all' group under the chosen method.
    if (!is.null(markers_df) && nrow(markers_df) > 0) {
      group_col <- intersect(c("cluster", "group"), colnames(markers_df))[1]
      matched <- character()
      if (!is.na(group_col) && length(group_values)) {
        cluster_values <- unique(as.character(markers_df[[group_col]]))
        matched <- names(group_values)[vapply(
          group_values,
          function(values) all(cluster_values %in% as.character(values)),
          logical(1)
        )]
      }
      if (length(matched) == 1L) {
        table <- markers_df[
          c(group_col, setdiff(names(markers_df), group_col))
        ]
        names(table)[[1L]] <- matched
        per_group <- stats::setNames(list(table), matched)
      } else if (!is.na(group_col)) {
        per_group <- split(markers_df, markers_df[[group_col]], drop = TRUE)
      } else {
        per_group <- list(all = markers_df)
      }
      misc$marker_genes <- list()
      misc$marker_genes[[marker_method]] <- per_group
    }
  }

  misc
}

#' @keywords internal
#' @noRd
.convertAddProvidedMostExpressed <- function(
  misc,
  most_expressed_genes,
  verbose
) {
  provided <- FALSE
  if (!is.null(most_expressed_genes)) {
    # User provided most_expressed_genes
    if (is.data.frame(most_expressed_genes)) {
      # Convert data.frame to list
      most_expressed_genes <- list(unknown = most_expressed_genes)
      if (verbose) {
        message(
          "[INFO] Converted most_expressed_genes from data.frame to list(unknown = ...)"
        )
      }
    } else if (is.list(most_expressed_genes)) {
      # Skip elements that are not data.frames and name unnamed ones. Work on
      # the original indices; deleting inside the loop shifted them.
      is_table <- vapply(most_expressed_genes, is.data.frame, logical(1))
      for (i in which(!is_table)) {
        warning(paste0(
          "Element ",
          i,
          " in most_expressed_genes is not a data.frame and will be skipped."
        ))
      }
      table_names <- names(most_expressed_genes)
      if (is.null(table_names)) {
        table_names <- character(length(most_expressed_genes))
      }
      unnamed <- which(is_table & (is.na(table_names) | table_names == ""))
      for (j in seq_along(unnamed)) {
        table_names[unnamed[j]] <- paste0("unknown", j)
        if (verbose) {
          message(paste0(
            "[INFO] Assigned name 'unknown",
            j,
            "' to unnamed element ",
            unnamed[j]
          ))
        }
      }
      names(most_expressed_genes) <- table_names
      most_expressed_genes <- most_expressed_genes[is_table]
    } else {
      stop(
        "most_expressed_genes must be either a data.frame or a list of data.frames.",
        call. = FALSE
      )
    }

    # Assign to seurat object
    if (length(most_expressed_genes) > 0) {
      if (
        is.null(misc$most_expressed_genes) ||
          !is.list(misc$most_expressed_genes)
      ) {
        misc$most_expressed_genes <- list()
      }
      misc$most_expressed_genes <- c(
        misc$most_expressed_genes,
        most_expressed_genes
      )
      if (verbose) {
        message(paste0(
          "[INFO] Added ",
          length(most_expressed_genes),
          " most_expressed_genes group(s): ",
          paste(names(most_expressed_genes), collapse = ", ")
        ))
      }
    }

    # Don't calculate if user provided data
    provided <- TRUE
  }

  list(misc = misc, provided = provided)
}

#' @keywords internal
#' @noRd
.convertAsDgCMatrix <- function(x) {
  if (inherits(x, "dgCMatrix")) {
    return(x)
  }
  methods::as(
    methods::as(methods::as(x, "CsparseMatrix"), "generalMatrix"),
    "dMatrix"
  )
}

#' @keywords internal
#' @noRd
.convertComputeGroupSummaries <- function(
  misc,
  meta,
  expr_matrix,
  groups,
  verbose,
  stream = NULL
) {
  # Initialize list structures
  if (
    is.null(misc$most_expressed_genes) ||
      !is.list(misc$most_expressed_genes)
  ) {
    misc$most_expressed_genes <- list()
  }
  if (
    is.null(misc$mean_expression) ||
      !is.list(misc$mean_expression)
  ) {
    misc$mean_expression <- list()
  }

  # One cells x levels indicator matrix per group turns the per-level
  # subsetting into two sparse products over the whole matrix.
  if (is.null(stream)) {
    expr <- .convertAsDgCMatrix(expr_matrix)
    positive <- expr
    positive@x <- as.numeric(expr@x > 0)
    genes <- rownames(expr)
    cell_names <- colnames(expr)
  } else {
    genes <- stream$genes
    cell_names <- stream$cells
  }
  meta_rows <- match(cell_names, rownames(meta))

  # The levels of every group, with the cells that belong to them
  plan <- lapply(stats::setNames(groups, groups), function(group_name) {
    values <- meta[[group_name]]
    group_values <- unique(values)
    group_values <- as.character(group_values[!is.na(group_values)])
    code <- match(as.character(values)[meta_rows], group_values)
    keep <- which(!is.na(code))
    members <- Matrix::sparseMatrix(
      i = keep,
      j = code[keep],
      x = 1,
      dims = c(length(cell_names), length(group_values))
    )
    size <- Matrix::colSums(members)
    present <- which(size > 0)
    list(
      group_values = group_values,
      size = size,
      present = present,
      members = members[, present, drop = FALSE]
    )
  })

  # A matrix that is streamed (too large to read) is summed over all groups in
  # one pass through the file
  streamed <- NULL
  if (!is.null(stream)) {
    if (verbose) {
      message(paste0(
        "[",
        format(Sys.time(), "%H:%M:%S"),
        "] Streaming the matrix for the group summaries..."
      ))
    }
    all_members <- do.call(cbind, lapply(plan, `[[`, "members"))
    sums <- .h5adStreamGroupSums(stream, all_members)
    width <- vapply(plan, function(p) length(p$present), integer(1))
    stop_at <- cumsum(width)
    streamed <- lapply(seq_along(plan), function(g) {
      cols <- seq_len(width[[g]]) + stop_at[[g]] - width[[g]]
      list(
        pct = sums$positive[, cols, drop = FALSE],
        mean_expr = sums$sums[, cols, drop = FALSE]
      )
    })
    names(streamed) <- names(plan)
    rm(sums)
  }

  for (group_name in groups) {
    if (verbose) {
      message(paste0(
        "[",
        format(Sys.time(), "%H:%M:%S"),
        "] Processing group: ",
        group_name
      ))
    }

    group_values <- plan[[group_name]]$group_values
    size <- plan[[group_name]]$size
    present <- plan[[group_name]]$present
    if (!length(present)) {
      misc$most_expressed_genes[[group_name]] <- data.frame()
      misc$mean_expression[[group_name]] <- data.frame()
      next
    }
    members <- plan[[group_name]]$members
    if (is.null(streamed)) {
      pct <- as.matrix(positive %*% members)
      mean_expr <- as.matrix(expr %*% members)
    } else {
      pct <- streamed[[group_name]]$pct
      mean_expr <- streamed[[group_name]]$mean_expr
    }

    # Per level, the genes sorted by the statistic (ties keep the gene order)
    stack <- function(statistic, column) {
      ordered <- lapply(seq_along(present), function(j) {
        value <- statistic[, j]
        order(-value)
      })
      data.frame(
        cluster = rep(group_values[present], each = length(genes)),
        gene = unlist(lapply(ordered, function(o) genes[o])),
        value = unlist(lapply(seq_along(present), function(j) {
          statistic[ordered[[j]], j]
        })),
        stringsAsFactors = FALSE
      ) |>
        stats::setNames(c("cluster", "gene", column))
    }
    n <- size[present]
    misc$most_expressed_genes[[group_name]] <- stack(
      sweep(pct, 2L, n, "/") * 100,
      "pct"
    )
    misc$mean_expression[[group_name]] <- stack(
      sweep(mean_expr, 2L, n, "/"),
      "mean_expr"
    )
  }

  if (verbose) {
    message(paste0(
      "[",
      format(Sys.time(), "%H:%M:%S"),
      "] Most expressed genes and mean expression calculation completed."
    ))
  }

  misc
}
