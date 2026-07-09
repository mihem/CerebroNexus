#' @keywords internal
#' @noRd
.loadImmuneRepertoireData <- function(file_path, data_type, verbose = TRUE) {
  data_type_upper <- toupper(data_type)

  if (is.null(file_path) || !nzchar(file_path)) {
    return(NULL)
  }

  if (!file.exists(file_path)) {
    stop(
      data_type_upper,
      " file not found: ",
      file_path,
      "\n",
      "Suggestions:\n",
      "  1. Check if the file path is correct\n",
      "  2. Verify the file extension is .rds\n",
      "  3. Ensure you have read permissions for the file"
    )
  }

  data <- tryCatch(
    {
      readRDS(file_path)
    },
    error = function(e) {
      stop(
        "Failed to read ",
        data_type_upper,
        " data from: ",
        file_path,
        "\n",
        "  Error: ",
        e$message,
        "\n",
        "Suggestions:\n",
        "  1. Verify the file is a valid .rds file\n",
        "  2. Check if the file was created using saveRDS()\n",
        "  3. Try reading the file directly: readRDS('",
        file_path,
        "')"
      )
    }
  )

  if (is.null(data)) {
    stop(
      data_type_upper,
      " data is NULL after reading from: ",
      file_path,
      "\n",
      "Suggestions:\n",
      "  1. Check if the source file contains valid data\n",
      "  2. Verify the file was not corrupted\n",
      "  3. Try recreating the .rds file"
    )
  }

  if (!is.list(data) || length(data) == 0) {
    stop(
      data_type_upper,
      " data is not a valid list or is empty.\n",
      "  Expected: A list of contig annotations\n",
      "  Received: ",
      class(data)[1],
      " with length ",
      length(data),
      "\n",
      "Suggestions:\n",
      "  1. Verify the data structure matches scRepertoire format\n",
      "  2. Check if the data was properly saved using saveRDS()\n",
      "  3. Ensure the data contains contig annotations"
    )
  }

  if (verbose) {
    message("[INFO] Loaded ", data_type_upper, " data from: ", file_path)
    message(
      "[INFO] ",
      data_type_upper,
      " data contains ",
      length(data),
      " samples"
    )
  }

  return(data)
}

#' Extract immune repertoire data from Seurat metadata
#'
#' When scRepertoire's \code{combineExpression()} has been used, the Seurat
#' metadata contains columns like CTgene, CTnt, CTaa, CTstrict, etc.
#' This function extracts those columns and splits by sample into the
#' list-of-data.frames format expected by scRepertoire visualization functions.
#' TCR and BCR data are kept together; scRepertoire's \code{chain} parameter
#' handles filtering at plot time.
#'
#' @param seurat A Seurat object with scRepertoire columns in meta.data.
#' @param groups Character vector of group column names to include in output.
#' @param sample_col Column name to split samples by; defaults to "orig.ident".
#' @param verbose Logical; print progress messages.
#' @return A named list of data.frames (one per sample), or NULL if no
#'   repertoire data is found.
#' @keywords internal
#' @noRd
.extractRepertoireFromMetadata <- function(
  seurat,
  groups = NULL,
  sample_col = "orig.ident",
  verbose = TRUE
) {
  core_cols <- c("CTgene", "CTnt", "CTaa", "CTstrict")
  meta_names <- names(seurat@meta.data)
  present_core <- core_cols[core_cols %in% meta_names]

  if (length(present_core) == 0) {
    if (verbose) {
      message(
        "[INFO] No scRepertoire columns found in metadata, ",
        "skipping repertoire extraction."
      )
    }
    return(NULL)
  }

  if (verbose) {
    message(paste0(
      "[",
      format(Sys.time(), "%H:%M:%S"),
      "] Found scRepertoire columns in metadata: ",
      paste(present_core, collapse = ", ")
    ))
  }

  # Additional scRepertoire columns to preserve
  optional_cols <- c(
    "clonalProportion",
    "clonalFrequency",
    "cloneSize",
    "Frequency",
    "frequency",
    "cloneType"
  )
  present_optional <- optional_cols[optional_cols %in% meta_names]

  # Identify cells with non-NA repertoire data
  primary_col <- if ("CTgene" %in% present_core) "CTgene" else present_core[1]
  has_data <- !is.na(seurat@meta.data[[primary_col]]) &
    nzchar(as.character(seurat@meta.data[[primary_col]]))

  if (sum(has_data) == 0) {
    if (verbose) {
      message("[INFO] No cells with non-NA repertoire data found.")
    }
    return(NULL)
  }

  # Columns to keep
  cols_to_keep <- unique(c(present_core, present_optional))
  if (!is.null(groups)) {
    cols_to_keep <- unique(c(cols_to_keep, groups[groups %in% meta_names]))
  }

  rep_df <- seurat@meta.data[has_data, cols_to_keep, drop = FALSE]
  rep_df$barcode <- rownames(seurat@meta.data)[has_data]

  # Determine sample column for splitting
  actual_sample_col <- NULL
  for (col in c(sample_col, "orig.ident", "sample", "Sample")) {
    if (col %in% meta_names) {
      actual_sample_col <- col
      break
    }
  }

  if (!is.null(actual_sample_col)) {
    rep_df$.sample_id <- as.character(
      seurat@meta.data[[actual_sample_col]][has_data]
    )
  } else {
    rep_df$.sample_id <- "Sample_1"
  }

  # Split by sample into list-of-data.frames
  result <- split(rep_df, rep_df$.sample_id)
  result <- lapply(result, function(x) {
    x$.sample_id <- NULL
    x
  })

  if (verbose) {
    # Detect data types present
    types <- character(0)
    if ("CTgene" %in% names(rep_df)) {
      ct <- as.character(rep_df$CTgene)
      if (any(grepl("TR[ABDG]", ct))) {
        types <- c(types, "TCR")
      }
      if (any(grepl("IG[HKL]", ct))) types <- c(types, "BCR")
    }
    message(paste0(
      "[INFO] Extracted immune repertoire: ",
      sum(has_data),
      " cells in ",
      length(result),
      " sample(s)",
      if (length(types) > 0) {
        paste0(" [", paste(types, collapse = "+"), "]")
      } else {
        ""
      }
    ))
  }

  return(result)
}

#' @keywords internal
#' @noRd
.readMarkerFile <- function(marker_file, verbose = TRUE) {
  ext <- tolower(tools::file_ext(marker_file))
  markers_df <- NULL

  # Read file based on extension
  if (ext == "csv") {
    markers_df <- utils::read.csv(
      marker_file,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else if (ext %in% c("tsv", "txt", "tab")) {
    markers_df <- utils::read.delim(
      marker_file,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    stop(
      "Unsupported marker_file format: .",
      ext,
      ". Supported: csv, tsv, txt, tab. ",
      "For Excel input, export to CSV first.",
      call. = FALSE
    )
  }

  if (is.null(markers_df) || nrow(markers_df) == 0) {
    stop("marker_file read produced an empty table.", call. = FALSE)
  }

  if (verbose) {
    message(paste0(
      "[",
      format(Sys.time(), "%H:%M:%S"),
      "] Loaded marker_file (",
      nrow(markers_df),
      " rows)."
    ))
  }

  return(markers_df)
}

#' @title
#' Convert Seurat Object to Cerebro Format
#'
#' @description
#' This function reads a Seurat object from a file, optionally renames grouping
#' variables, loads marker gene tables, and exports the data to Cerebro format
#' for visualization.
#'
#' @param seurat_file Character string specifying the path to the Seurat object
#'   file. Supported format: \code{.rds}.
#' @param result_dir Character string specifying the directory where the Cerebro
#'   output file (.crb) will be saved.
#' @param assay Character string specifying which assay to use from the Seurat
#'   object; default: \code{"RNA"}.
#' @param slot Character string specifying which slot to extract expression data
#'   from (e.g., "data", "counts", "scale.data"); default: \code{"data"}.
#' @param experiment_name Character string for the experiment name to be stored
#'   in the Cerebro object; default: \code{"Dura Mater - All Cells"}.
#' @param organism Character string specifying the organism (e.g., "Human",
#'   "Mouse"); default: \code{"Human"}.
#' @param groups Character vector of column names in the Seurat metadata to use
#'   as grouping variables; default: \code{c("seurat_clusters", "orig.ident",
#'   "cell_type_final")}.
#' @param groups_naming Named list for renaming grouping variables. Names are
#'   the old column names, values are the new names; default: \code{NULL}.
#' @param max_group_levels Numeric value specifying the maximum number of unique
#'   levels allowed in a grouping variable. Groups with more unique values than
#'   this threshold will be excluded; default: \code{100}.
#' @param nUMI Character string specifying the column name in metadata containing
#'   the number of UMIs per cell; default: \code{"nCount_RNA"}.
#' @param nGene Character string specifying the column name in metadata containing
#'   the number of expressed genes per cell; default: \code{"nFeature_RNA"}.
#' @param add_all_meta_data Logical indicating whether to include all metadata
#'   columns in the export; default: \code{TRUE}.
#' @param use_delayed_array Logical indicating whether to convert expression
#'   data to DelayedArray format for memory efficiency; default: \code{FALSE}.
#'   Ignored when \code{expression_matrix_mode} is set to an external backend.
#' @param expression_matrix_mode How to persist the expression matrix in the
#'   generated \code{.crb}. One of \code{"embedded"} (default), \code{"bpcells"}
#'   or \code{"h5"}. See \code{\link{exportFromSeurat}} for details; briefly,
#'   \code{"embedded"} stores the matrix inside the \code{.crb} (legacy
#'   behaviour), \code{"bpcells"} writes an on-disk BPCells directory next to
#'   the \code{.crb} and keeps a lightweight handle inside it (typically
#'   reduces \code{.crb} size by ~80% on large sparse matrices), and
#'   \code{"h5"} writes a TENx-format HDF5 file next to the \code{.crb}
#'   that the Shiny runtime loads lazily, minimising RAM and startup time
#'   (recommended default for large datasets). The Shiny runtime
#'   re-resolves both backends relative to the \code{.crb}'s parent
#'   directory, so packaging the \code{.crb} with its sibling
#'   \code{<stem>.bpcells/} or \code{<stem>.h5} together is enough for
#'   portable deployment.
#' @param verbose Logical indicating whether to print progress messages; default:
#'   \code{TRUE}.
#' @param cell_cycle Character vector of column names in metadata containing
#'   cell cycle phase assignments; default: \code{NULL}.
#' @param marker_file Character string specifying the path to a marker gene
#'   table file. Supported formats: .csv, .tsv, .txt, .tab; default:
#'   \code{NULL}.
#' @param marker_method Character string specifying the name of the method used
#'   to identify marker genes (will be used as a label in Cerebro); default:
#'   \code{"Diff. Expression"}.
#' @param add_most_expressed_genes Logical indicating whether to calculate the
#'   most expressed genes for each group; default: \code{TRUE}.
#' @param most_expressed_genes Optional pre-calculated most expressed genes data.
#'   Can be either a data.frame (will be converted to list(unknown = ...)) or a
#'   list of data.frames. If list elements are unnamed, they will be assigned
#'   names like "unknown1", "unknown2", etc.; default: \code{NULL}.
#' @param bcr_file Character string specifying the path to a BCR data file
#'   (.rds format). The data will be merged into the unified
#'   \code{immune_repertoire} slot of the Seurat object before export;
#'   default: \code{NULL}.
#' @param tcr_file Character string specifying the path to a TCR data file
#'   (.rds format). The data will be merged into the unified
#'   \code{immune_repertoire} slot of the Seurat object before export;
#'   default: \code{NULL}.
#'
#' @return
#' This function does not return a value. It saves a Cerebro object (.crb file)
#' to the specified \code{result_dir}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the Seurat object from \code{seurat_file}
#'   \item Renames grouping columns if \code{groups_naming} is provided
#'   \item Loads marker gene tables from \code{marker_file} if provided:
#'     \itemize{
#'       \item For Excel files with multiple sheets, each sheet becomes a
#'         separate group
#'       \item For single-sheet files, data is split by the first column
#'     }
#'   \item Exports the processed data using \code{\link{exportFromSeurat}}
#'   \item Saves the result as \code{cerebro_<basename>.crb} in \code{result_dir}
#'   \item Cleans up memory by removing the Seurat object and calling garbage
#'     collection
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' convertSeuratToCerebro(
#'   seurat_file = "path/to/seurat_object.rds",
#'   result_dir = "path/to/output"
#' )
#'
#' # With custom grouping and renaming
#' convertSeuratToCerebro(
#'   seurat_file = "seurat_object.rds",
#'   result_dir = "output",
#'   groups = c("cluster", "sample", "celltype"),
#'   groups_naming = list("cluster" = "Cluster", "celltype" = "Cell Type"),
#'   marker_file = "markers.csv"
#' )
#' }
#'
#' @seealso \code{\link{exportFromSeurat}}
#'
#' @export
convertSeuratToCerebro <- function(
  seurat_file,
  result_dir,
  assay = "RNA",
  slot = "data",
  experiment_name = "Dura Mater - All Cells",
  organism = "Human",
  groups = c("seurat_clusters", "orig.ident", "cell_type_final"),
  groups_naming = NULL,
  max_group_levels = 100,
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA",
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "bpcells", "h5"),
  verbose = TRUE,
  cell_cycle = NULL,
  marker_file = NULL,
  marker_method = "Diff. Expression",
  add_most_expressed_genes = TRUE,
  most_expressed_genes = NULL,
  bcr_file = NULL,
  tcr_file = NULL
) {
  expression_matrix_mode <- match.arg(expression_matrix_mode)
  if (inherits(seurat_file, "Seurat")) {
    seurat <- seurat_file
  } else {
    if (!file.exists(seurat_file)) {
      stop("seurat_file not found: ", seurat_file, call. = FALSE)
    }
    ext <- tolower(tools::file_ext(seurat_file))
    seurat <- switch(
      ext,
      rds = readRDS(seurat_file),
      stop(
        "Unsupported seurat_file format: .",
        ext,
        ". Use .rds (saved with saveRDS).",
        call. = FALSE
      )
    )
  }

  # Validate groups exist in metadata ----------------------------------------##
  missing_groups <- groups[!groups %in% names(seurat@meta.data)]

  if (length(missing_groups) > 0) {
    if (length(missing_groups) == length(groups)) {
      # All groups are missing - stop execution
      stop(
        paste0(
          "All specified groups are missing from metadata: ",
          paste(missing_groups, collapse = ", "),
          "\nAvailable columns: ",
          paste(names(seurat@meta.data), collapse = ", ")
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
      groups <- groups[groups %in% names(seurat@meta.data)]
    }
  }

  # Convert group values to character and check level counts ----------------##
  groups_to_remove <- character(0)

  for (group_name in groups) {
    # Convert to character
    seurat@meta.data[[group_name]] <- as.character(seurat@meta.data[[
      group_name
    ]])

    # Count unique levels (excluding NA)
    unique_levels <- unique(seurat@meta.data[[group_name]][
      !is.na(seurat@meta.data[[group_name]])
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

  # Rename group columns according to groups_naming
  if (!is.null(groups_naming) && length(groups_naming) > 0) {
    # Validate groups_naming structure
    if (is.null(names(groups_naming)) || length(names(groups_naming)) == 0) {
      stop(
        "groups_naming must be a named list/vector with names corresponding to existing group names.",
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

    for (old_name in valid_names) {
      new_name <- groups_naming[[old_name]]
      # Rename column in metadata
      seurat@meta.data[[new_name]] <- seurat@meta.data[[old_name]]
      seurat@meta.data[[old_name]] <- NULL

      # Update groups vector with new name
      idx <- which(groups == old_name)
      if (length(idx) > 0) {
        groups[idx] <- new_name
      }
    }
  }

  # Load marker table (optional) --------------------------------------------##
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
      if (!is.na(group_col)) {
        per_group <- split(markers_df, markers_df[[group_col]], drop = TRUE)
      } else {
        per_group <- list(all = markers_df)
      }
      seurat@misc$marker_genes <- list()
      seurat@misc$marker_genes[[marker_method]] <- per_group
    }
  }
  # Handle most_expressed_genes input ---------------------------------------##
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
      # Check if list elements are data.frames and handle unnamed elements
      unnamed_count <- 0
      for (i in seq_along(most_expressed_genes)) {
        # Check if element is a data.frame
        if (!is.data.frame(most_expressed_genes[[i]])) {
          warning(paste0(
            "Element ",
            i,
            " in most_expressed_genes is not a data.frame and will be skipped."
          ))
          most_expressed_genes[[i]] <- NULL
          next
        }

        # Handle unnamed elements
        if (
          is.null(names(most_expressed_genes)[i]) ||
            names(most_expressed_genes)[i] == ""
        ) {
          unnamed_count <- unnamed_count + 1
          names(most_expressed_genes)[i] <- paste0("unknown", unnamed_count)
          if (verbose) {
            message(paste0(
              "[INFO] Assigned name 'unknown",
              unnamed_count,
              "' to unnamed element ",
              i
            ))
          }
        }
      }
      # Remove NULL elements (those that weren't data.frames)
      most_expressed_genes <- most_expressed_genes[
        !sapply(most_expressed_genes, is.null)
      ]
    } else {
      stop(
        "most_expressed_genes must be either a data.frame or a list of data.frames.",
        call. = FALSE
      )
    }

    # Assign to seurat object
    if (length(most_expressed_genes) > 0) {
      if (
        is.null(seurat@misc$most_expressed_genes) ||
          !is.list(seurat@misc$most_expressed_genes)
      ) {
        seurat@misc$most_expressed_genes <- list()
      }
      seurat@misc$most_expressed_genes <- c(
        seurat@misc$most_expressed_genes,
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
    add_most_expressed_genes <- FALSE
  }

  # Calculate most expressed genes and mean expression from Seurat object ---##
  if (add_most_expressed_genes) {
    if (verbose) {
      message(paste0(
        "[",
        format(Sys.time(), "%H:%M:%S"),
        "] Calculating most expressed genes and mean expression for each group..."
      ))
    }

    # Get expression matrix. Top-level conversion entry point: opt into legacy
    # cross-semantic layer fallback so a Seurat v5 counts-only assay requested
    # at the default slot = "data" still converts (as it did on master) instead
    # of hard-stopping. The fallback warns, so it is never silent.
    expr_matrix <- .getExpressionMatrix(
      seurat,
      assay = assay,
      slot = slot,
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )

    # Initialize list structures
    if (
      is.null(seurat@misc$most_expressed_genes) ||
        !is.list(seurat@misc$most_expressed_genes)
    ) {
      seurat@misc$most_expressed_genes <- list()
    }
    if (
      is.null(seurat@misc$mean_expression) ||
        !is.list(seurat@misc$mean_expression)
    ) {
      seurat@misc$mean_expression <- list()
    }

    # Calculate for each group
    for (group_name in groups) {
      if (verbose) {
        message(paste0(
          "[",
          format(Sys.time(), "%H:%M:%S"),
          "] Processing group: ",
          group_name
        ))
      }

      group_values <- unique(seurat@meta.data[[group_name]])
      group_values <- group_values[!is.na(group_values)]

      pct_results <- list()
      expr_results <- list()

      for (group_value in group_values) {
        # Get cells belonging to this group value using cell names (more robust)
        cells_in_group <- rownames(seurat@meta.data)[
          seurat@meta.data[[group_name]] == group_value
        ]
        # Filter to cells that exist in the expression matrix
        cells_in_group <- cells_in_group[
          cells_in_group %in% colnames(expr_matrix)
        ]

        if (length(cells_in_group) == 0) {
          next
        }

        # Get expression subset for this group
        expr_subset <- expr_matrix[, cells_in_group, drop = FALSE]

        # Calculate percentage of cells expressing each gene
        gene_pct <- Matrix::rowSums(expr_subset > 0) /
          length(cells_in_group) *
          100

        # Calculate mean expression per gene
        gene_mean <- Matrix::rowMeans(expr_subset)

        # Create data frame for percentage and sort
        gene_pct_df <- data.frame(
          gene = names(gene_pct),
          pct = as.numeric(gene_pct),
          stringsAsFactors = FALSE
        )
        gene_pct_df[["cluster"]] <- as.character(group_value)
        gene_pct_df <- gene_pct_df[order(-gene_pct_df$pct), ]
        rownames(gene_pct_df) <- NULL

        # Create data frame for mean expression and sort
        gene_expr_df <- data.frame(
          gene = names(gene_mean),
          mean_expr = as.numeric(gene_mean),
          stringsAsFactors = FALSE
        )
        gene_expr_df[["cluster"]] <- as.character(group_value)
        gene_expr_df <- gene_expr_df[order(-gene_expr_df$mean_expr), ]
        rownames(gene_expr_df) <- NULL

        pct_results[[as.character(group_value)]] <- gene_pct_df
        expr_results[[as.character(group_value)]] <- gene_expr_df
      }

      # Merge all data frames into one and reorder columns with cluster first
      if (length(pct_results) > 0) {
        pct_results_df <- do.call(rbind, pct_results)
        rownames(pct_results_df) <- NULL
        pct_results_df <- pct_results_df[, c(
          "cluster",
          setdiff(names(pct_results_df), "cluster")
        )]
        seurat@misc$most_expressed_genes[[group_name]] <- pct_results_df
      } else {
        seurat@misc$most_expressed_genes[[group_name]] <- data.frame()
      }

      if (length(expr_results) > 0) {
        expr_results_df <- do.call(rbind, expr_results)
        rownames(expr_results_df) <- NULL
        expr_results_df <- expr_results_df[, c(
          "cluster",
          setdiff(names(expr_results_df), "cluster")
        )]
        seurat@misc$mean_expression[[group_name]] <- expr_results_df
      } else {
        seurat@misc$mean_expression[[group_name]] <- data.frame()
      }
    }

    if (verbose) {
      message(paste0(
        "[",
        format(Sys.time(), "%H:%M:%S"),
        "] Most expressed genes and mean expression calculation completed."
      ))
    }
  }

  # Immune repertoire data --------------------------------------------------##
  # Priority: external files (bcr_file/tcr_file) > metadata extraction
  # All data is stored in the unified immune_repertoire slot.

  bcr_data <- .loadImmuneRepertoireData(bcr_file, "BCR", verbose)
  tcr_data <- .loadImmuneRepertoireData(tcr_file, "TCR", verbose)

  if (!is.null(bcr_data) || !is.null(tcr_data)) {
    # External files provided — merge into immune_repertoire
    seurat@misc$immune_repertoire <- c(
      if (!is.null(bcr_data)) bcr_data else list(),
      if (!is.null(tcr_data)) tcr_data else list()
    )
  }

  # Fallback: extract from Seurat metadata (scRepertoire columns)
  if (
    is.null(seurat@misc$immune_repertoire) ||
      length(seurat@misc$immune_repertoire) == 0
  ) {
    rep_data <- .extractRepertoireFromMetadata(
      seurat,
      groups = groups,
      verbose = verbose
    )
    if (!is.null(rep_data) && length(rep_data) > 0) {
      seurat@misc$immune_repertoire <- rep_data
    }
  }

  # Get the base name for the file
  if (is.character(seurat_file)) {
    base_name <- tools::file_path_sans_ext(basename(seurat_file))
  } else {
    base_name <- gsub("[^A-Za-z0-9._-]", "_", experiment_name)
  }
  file_name <- paste0("cerebro_", base_name, ".crb")
  source_label <- if (is.character(seurat_file)) {
    basename(seurat_file)
  } else {
    paste0("<Seurat:", experiment_name, ">")
  }

  # Export to cerebro format
  tryCatch(
    {
      exportFromSeurat(
        seurat,
        assay = assay,
        slot = slot,
        file = file.path(result_dir, file_name),
        experiment_name = experiment_name,
        organism = organism,
        groups = groups,
        nUMI = nUMI,
        nGene = nGene,
        add_all_meta_data = add_all_meta_data,
        cell_cycle = cell_cycle,
        verbose = verbose,
        use_delayed_array = use_delayed_array,
        expression_matrix_mode = expression_matrix_mode
      )
      cat("Successfully exported:", file_name, "\n")
    },
    error = function(e) {
      cat("Error processing", source_label, ":", e$message, "\n")
    }
  )

  # Clean up the Seurat object to save memory
  rm(seurat)
  gc() # Call garbage collector
}
