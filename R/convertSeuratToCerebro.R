#' @keywords internal
#' @noRd
.loadImmuneRepertoireData <- function(file_path, data_type, verbose = TRUE) {
  data_type_upper <- toupper(data_type)

  if (is.null(file_path) || !nzchar(file_path)) {
    return(NULL)
  }

  if (!file.exists(file_path)) {
    stop(
      data_type_upper, " file not found: ", file_path, "\n",
      "Suggestions:\n",
      "  1. Check if the file path is correct\n",
      "  2. Verify the file extension is .qs\n",
      "  3. Ensure you have read permissions for the file"
    )
  }

  data <- tryCatch({
    qs::qread(file_path)
  }, error = function(e) {
    stop(
      "Failed to read ", data_type_upper, " data from: ", file_path, "\n",
      "  Error: ", e$message, "\n",
      "Suggestions:\n",
      "  1. Verify the file is a valid .qs file\n",
      "  2. Check if the file was created using qs::qsave()\n",
      "  3. Try reading the file directly: qs::qread('", file_path, "')"
    )
  })

  if (is.null(data)) {
    stop(
      data_type_upper, " data is NULL after reading from: ", file_path, "\n",
      "Suggestions:\n",
      "  1. Check if the source file contains valid data\n",
      "  2. Verify the file was not corrupted\n",
      "  3. Try recreating the .qs file"
    )
  }

  if (!is.list(data) || length(data) == 0) {
    stop(
      data_type_upper, " data is not a valid list or is empty.\n",
      "  Expected: A list of contig annotations\n",
      "  Received: ", class(data)[1], " with length ", length(data), "\n",
      "Suggestions:\n",
      "  1. Verify the data structure matches scRepertoire format\n",
      "  2. Check if the data was properly saved using qs::qsave()\n",
      "  3. Ensure the data contains contig annotations"
    )
  }

  if (verbose) {
    message("[INFO] Loaded ", data_type_upper, " data from: ", file_path)
    message("[INFO] ", data_type_upper, " data contains ", length(data), " samples")
  }

  return(data)
}

#' @keywords internal
#' @noRd
.readMarkerFile <- function(marker_file, marker_method, verbose = TRUE) {
  ext <- tolower(tools::file_ext(marker_file))
  markers_list <- NULL

  # Read file based on extension
  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel files (xls/xlsx).", call. = FALSE)
    }
    sheet_names <- readxl::excel_sheets(marker_file)
    if (length(sheet_names) > 1) {
      markers_list <- lapply(sheet_names, function(sheet_name) {
        df <- readxl::read_excel(marker_file, sheet = sheet_name)
        as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      })
      names(markers_list) <- sheet_names
    } else {
      markers <- readxl::read_excel(marker_file, sheet = sheet_names[1])
      markers <- as.data.frame(markers, stringsAsFactors = FALSE, check.names = FALSE)
    }
  } else if (ext == "csv") {
    markers <- utils::read.csv(marker_file, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("tsv", "txt", "tab")) {
    markers <- utils::read.delim(marker_file, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unsupported marker_file format: .", ext, ". Supported: xls, xlsx, csv, tsv, txt, tab.", call. = FALSE)
  }

  # Process single table if markers_list is not yet set
  if (is.null(markers_list)) {
    if (nrow(markers) == 0) stop("marker_file read produced an empty table.", call. = FALSE)

    group_col <- names(markers)[1] # First column is grouping
    if (length(names(markers)) >= 1) {
       if (is.factor(markers[[group_col]])) markers[[group_col]] <- as.character(markers[[group_col]])
       markers[[group_col]][is.na(markers[[group_col]])] <- "N/A"
       markers_list <- split(markers, markers[[group_col]], drop = TRUE)
    } else {
       markers_list <- list(custom = markers)
    }
  }

  if (verbose) {
       total_rows <- sum(vapply(markers_list, nrow, integer(1)))
       message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] Loaded marker_file (",
                      total_rows, " rows) as method '", marker_method,
                      "' with ", length(markers_list), " group(s)."))
  }

  return(markers_list)
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
#'   file (should be in .qs format).
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
#' @param main_group Character string specifying the main grouping variable,
#'   must be one of the variables in \code{groups}; default: \code{NULL}.
#' @param nUMI Character string specifying the column name in metadata containing
#'   the number of UMIs per cell; default: \code{"nCount_RNA"}.
#' @param nGene Character string specifying the column name in metadata containing
#'   the number of expressed genes per cell; default: \code{"nFeature_RNA"}.
#' @param add_all_meta_data Logical indicating whether to include all metadata
#'   columns in the export; default: \code{TRUE}.
#' @param format Format of output file. Can be either \code{"qs"} or
#' \code{"rds"}. Defaults to \code{"qs"}.
#' @param use_delayed_array Logical indicating whether to convert expression
#'   data to DelayedArray format for memory efficiency; default: \code{FALSE}.
#' @param verbose Logical indicating whether to print progress messages; default:
#'   \code{TRUE}.
#' @param cell_cycle Character vector of column names in metadata containing
#'   cell cycle phase assignments; default: \code{NULL}.
#' @param marker_file Character string specifying the path to a marker gene
#'   table file. Supported formats: .xls, .xlsx, .csv, .tsv, .txt, .tab;
#'   default: \code{NULL}.
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
#'   (.qs format) to be added to the Seurat object's misc slot; default:
#'   \code{NULL}.
#' @param tcr_file Character string specifying the path to a TCR data file
#'   (.qs format) to be added to the Seurat object's misc slot; default:
#'   \code{NULL}.
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
#'   seurat_file = "path/to/seurat_object.qs",
#'   result_dir = "path/to/output"
#' )
#'
#' # With custom grouping and renaming
#' convertSeuratToCerebro(
#'   seurat_file = "seurat_object.qs",
#'   result_dir = "output",
#'   groups = c("cluster", "sample", "celltype"),
#'   groups_naming = list("cluster" = "Cluster", "celltype" = "Cell Type"),
#'   main_group = "Cell Type",
#'   marker_file = "markers.xlsx"
#' )
#' }
#'
#' @seealso \code{\link{exportFromSeurat}}
#'
#' @export
convertSeuratToCerebro <- function(seurat_file,
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
                                    verbose = TRUE,
                                    cell_cycle = NULL,
                                    marker_file = NULL,
                                    marker_method = "Diff. Expression",
                                    add_most_expressed_genes = TRUE,
                                    most_expressed_genes = NULL,
                                    bcr_file = NULL,
                                    tcr_file = NULL,
                                    format = "qs") {
  if (!file.exists(seurat_file)) {
    stop("seurat_file not found: ", seurat_file, call. = FALSE)
  }
  seurat <- qs::qread(seurat_file)


  # Validate groups exist in metadata ----------------------------------------##
  missing_groups <- groups[!groups %in% names(seurat@meta.data)]

  if (length(missing_groups) > 0) {
    if (length(missing_groups) == length(groups)) {
      # All groups are missing - stop execution
      stop(paste0("All specified groups are missing from metadata: ",
                  paste(missing_groups, collapse = ", "),
                  "\nAvailable columns: ",
                  paste(names(seurat@meta.data), collapse = ", ")),
        call. = FALSE)
    } else {
      # Some groups are missing - warn user
      warning(paste0("Some specified groups are missing from metadata and will be skipped: ", paste(missing_groups, collapse = ", ")), call. = FALSE)
      # Remove missing groups from the groups vector
      groups <- groups[groups %in% names(seurat@meta.data)]
    }
  }


  # Convert group values to character and check level counts ----------------##
  groups_to_remove <- character(0)

  for (group_name in groups) {
    # Convert to character
    seurat@meta.data[[group_name]] <- as.character(seurat@meta.data[[group_name]])

    # Count unique levels (excluding NA)
    unique_levels <- unique(seurat@meta.data[[group_name]][!is.na(seurat@meta.data[[group_name]])])
    n_levels <- length(unique_levels)

    # Check if exceeds maximum
    if (n_levels > max_group_levels) {
      if (verbose) {
        message(paste0("[WARNING] Group '", group_name, "' has ", n_levels,
                       " unique levels (> ", max_group_levels,
                       "), will be excluded."))
      }
      groups_to_remove <- c(groups_to_remove, group_name)
    }
  }

  # Remove groups with too many levels
  if (length(groups_to_remove) > 0) {
    groups <- groups[!groups %in% groups_to_remove]

    # Check if any groups remain
    if (length(groups) == 0) {
      stop(paste0("All groups have been excluded due to exceeding max_group_levels (",
                  max_group_levels, "). Consider increasing max_group_levels or ",
                  "using different grouping variables."),
           call. = FALSE)
    }

    if (verbose) {
      message(paste0("[INFO] Remaining groups: ", paste(groups, collapse = ", ")))
    }
  }


  # Rename group columns according to groups_naming
  if (!is.null(groups_naming) && length(groups_naming) > 0) {
    # Validate groups_naming structure
    if (is.null(names(groups_naming)) || length(names(groups_naming)) == 0) {
      stop("groups_naming must be a named list/vector with names corresponding to existing group names.",
           call. = FALSE)
    }

    # Check if at least one name in groups_naming exists in groups
    valid_names <- names(groups_naming)[names(groups_naming) %in% groups]

    if (length(valid_names) < length(names(groups_naming))) {
      warning(paste0("Some names in groups_naming do not exist in the specified groups and will be ignored: ",
                     paste(setdiff(names(groups_naming), valid_names), collapse = ", ")),
              call. = FALSE)
    }


    if (length(valid_names) == 0) {
      stop(paste0("None of the names in groups_naming exist in the specified groups.\n",
                  "groups_naming names: ", paste(names(groups_naming), collapse = ", "), "\n",
                  "Available groups: ", paste(groups, collapse = ", ")),
           call. = FALSE)
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
  if (!is.null(marker_file) && nzchar(marker_file) &&
      !is.null(marker_method) && nzchar(marker_method)) {
    if (!file.exists(marker_file)) {
      stop("marker_file not found: ", marker_file, call. = FALSE)
    }

    markers_list <- .readMarkerFile(marker_file, marker_method, verbose)

    # Attach to Seurat object under misc$marker_genes
    if (!is.null(markers_list) && length(markers_list) > 0) {
      if (is.null(seurat@misc$marker_genes) || !is.list(seurat@misc$marker_genes)) {
        seurat@misc$marker_genes <- list()
      }
      seurat@misc$marker_genes[[marker_method]] <- markers_list
    }
  }
  # Handle most_expressed_genes input ---------------------------------------##
  if (!is.null(most_expressed_genes)) {
    # User provided most_expressed_genes
    if (is.data.frame(most_expressed_genes)) {
      # Convert data.frame to list
      most_expressed_genes <- list(unknown = most_expressed_genes)
      if (verbose) {
        message("[INFO] Converted most_expressed_genes from data.frame to list(unknown = ...)")
      }
    } else if (is.list(most_expressed_genes)) {
      # Check if list elements are data.frames and handle unnamed elements
      unnamed_count <- 0
      for (i in seq_along(most_expressed_genes)) {
        # Check if element is a data.frame
        if (!is.data.frame(most_expressed_genes[[i]])) {
          warning(paste0("Element ", i, " in most_expressed_genes is not a data.frame and will be skipped."))
          most_expressed_genes[[i]] <- NULL
          next
        }

        # Handle unnamed elements
        if (is.null(names(most_expressed_genes)[i]) || names(most_expressed_genes)[i] == "") {
          unnamed_count <- unnamed_count + 1
          names(most_expressed_genes)[i] <- paste0("unknown", unnamed_count)
          if (verbose) {
            message(paste0("[INFO] Assigned name 'unknown", unnamed_count, "' to unnamed element ", i))
          }
        }
      }
      # Remove NULL elements (those that weren't data.frames)
      most_expressed_genes <- most_expressed_genes[!sapply(most_expressed_genes, is.null)]
    } else {
      stop("most_expressed_genes must be either a data.frame or a list of data.frames.", call. = FALSE)
    }

    # Assign to seurat object
    if (length(most_expressed_genes) > 0) {
      if (is.null(seurat@misc$most_expressed_genes) || !is.list(seurat@misc$most_expressed_genes)) {
        seurat@misc$most_expressed_genes <- list()
      }
      seurat@misc$most_expressed_genes <- c(seurat@misc$most_expressed_genes, most_expressed_genes)
      if (verbose) {
        message(paste0("[INFO] Added ", length(most_expressed_genes),
                       " most_expressed_genes group(s): ",
                       paste(names(most_expressed_genes), collapse = ", ")))
      }
    }

    # Don't calculate if user provided data
    add_most_expressed_genes <- FALSE
  }

  # Calculate most expressed genes from Seurat object ----------------------##
  if (add_most_expressed_genes) {
    if (verbose) {
      message(paste0("[", format(Sys.time(), "%H:%M:%S"),
                     "] Calculating most expressed genes for each group..."))
    }

    # Get expression matrix
    expr_matrix <- .getExpressionMatrix(seurat, assay = assay, slot = slot, join_samples = TRUE)

    # Initialize list structure
    if (is.null(seurat@misc$most_expressed_genes) || !is.list(seurat@misc$most_expressed_genes)) {
      seurat@misc$most_expressed_genes <- list()
    }

    # Calculate for each group
    for (group_name in groups) {
      if (verbose) {
        message(paste0("[", format(Sys.time(), "%H:%M:%S"),
                       "] Processing group: ", group_name))
      }

      group_values <- unique(seurat@meta.data[[group_name]])
      group_values <- group_values[!is.na(group_values)]

      group_results <- list()

      for (group_value in group_values) {
        # Get cells belonging to this group value using cell names (more robust)
        cells_in_group <- rownames(seurat@meta.data)[seurat@meta.data[[group_name]] == group_value]
        # Filter to cells that exist in the expression matrix
        cells_in_group <- cells_in_group[cells_in_group %in% colnames(expr_matrix)]

        if (length(cells_in_group) == 0) next

        # Calculate percentage of cells expressing each gene
        expr_subset <- expr_matrix[, cells_in_group, drop = FALSE]
        gene_pct <- Matrix::rowSums(expr_subset > 0) / length(cells_in_group) * 100

        # Create data frame and sort by percentage
        gene_df <- data.frame(
          gene = names(gene_pct),
          pct = as.numeric(gene_pct),
          stringsAsFactors = FALSE
        )
        gene_df[["cluster"]] = as.character(group_value)
        gene_df <- gene_df[order(-gene_df$pct), ]
        rownames(gene_df) <- NULL

        group_results[[as.character(group_value)]] <- gene_df
      }

      # Merge all data frames into one and reorder columns with cluster first
      if (length(group_results) > 0) {
        group_results_df <- do.call(rbind, group_results)
        rownames(group_results_df) <- NULL
        # Reorder columns with cluster first
        group_results_df <- group_results_df[, c("cluster", setdiff(names(group_results_df), "cluster"))]
        seurat@misc$most_expressed_genes[[group_name]] <- group_results_df
      } else {
        seurat@misc$most_expressed_genes[[group_name]] <- data.frame()
      }
    }

    if (verbose) {
      message(paste0("[", format(Sys.time(), "%H:%M:%S"),
                     "] Most expressed genes calculation completed."))
    }
  }


  # add BCR data if provided -----------------------------------------------##
  bcr_data <- .loadImmuneRepertoireData(bcr_file, "BCR", verbose)
  if (!is.null(bcr_data)) {
    seurat@misc$bcr_data <- bcr_data
  }

  # add TCR data if provided -----------------------------------------------##
  tcr_data <- .loadImmuneRepertoireData(tcr_file, "TCR", verbose)
  if (!is.null(tcr_data)) {
    seurat@misc$tcr_data <- tcr_data
  }

  # Get the base name for the file
  base_name <- gsub("\\.qs$", "", basename(seurat_file))
  file_name <- paste0("cerebro_", base_name, ".crb")

  # Export to cerebro format
  tryCatch({
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
      format = format
    )
    cat("Successfully exported:", file_name, "\n")
  }, error = function(e) {
    cat("Error processing", basename(seurat_file), ":", e$message, "\n")
  })

  # Clean up the Seurat object to save memory
  rm(seurat)
  gc()  # Call garbage collector
}
