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
#'   An explicit name must exist; use \code{NULL} to auto-detect a standard
#'   sample column.
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
    .logStep(
      "Found scRepertoire columns in metadata: ",
      paste(present_core, collapse = ", ")
    )
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
  if (!is.null(sample_col)) {
    if (
      !is.character(sample_col) ||
        length(sample_col) != 1L ||
        is.na(sample_col) ||
        !nzchar(sample_col)
    ) {
      stop(
        "`sample_col` must be NULL or one non-empty column name.",
        call. = FALSE
      )
    }
    if (!(sample_col %in% meta_names)) {
      stop(
        "`sample_col` `",
        sample_col,
        "` is not present in the Seurat metadata.",
        call. = FALSE
      )
    }
    actual_sample_col <- sample_col
  } else {
    for (col in c("orig.ident", "sample", "Sample")) {
      if (col %in% meta_names) {
        actual_sample_col <- col
        break
      }
    }
  }

  if (!is.null(actual_sample_col)) {
    rep_df$.sample_id <- as.character(
      seurat@meta.data[[actual_sample_col]][has_data]
    )
    invalid_sample <- is.na(rep_df$.sample_id) | !nzchar(rep_df$.sample_id)
    if (any(invalid_sample)) {
      stop(
        "Metadata column `",
        actual_sample_col,
        "` has missing or empty sample identities for ",
        sum(invalid_sample),
        " cell(s) with repertoire data. Example barcode(s): ",
        paste(
          utils::head(rep_df$barcode[invalid_sample], 5L),
          collapse = ", "
        ),
        ". Fill the sample identities or choose a complete `sample_col`.",
        call. = FALSE
      )
    }
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
    .logStep("Loaded marker_file (", nrow(markers_df), " rows).")
  }

  return(markers_df)
}

#' @title
#' Convert Seurat Object to Cerebro Format
#'
#' @description
#' This function accepts a Seurat object or reads one from an RDS file,
#' optionally renames grouping variables, loads marker gene tables, and exports
#' the data to Cerebro format for visualization.
#'
#' @param seurat_file A Seurat object in memory, or a character string specifying
#'   its \code{.rds} file path.
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
#'   generated \code{.crb}. One of \code{"embedded"} (default) or \code{"h5"}.
#'   See \code{\link{exportFromSeurat}} for details; briefly, \code{"embedded"}
#'   stores the matrix inside the \code{.crb}, and \code{"h5"} writes a
#'   TENx-format HDF5 file next to the \code{.crb} that the Viewer loads lazily,
#'   minimising RAM and startup time (recommended for large datasets). The
#'   Viewer finds the file relative to the \code{.crb}'s parent directory, so
#'   packaging the \code{.crb} with its sibling \code{<stem>.h5} together is
#'   enough for portable deployment.
#' @param codec Serialization codec for the CRB payload. Defaults to
#'   \code{"qs2"}; use \code{"rds"} when direct compatibility with
#'   \code{readRDS()} is required.
#' @param spatial_images Optional manifest in
#'   \code{spatial entry -> image label -> path} form. Spatial-entry names must
#'   match \code{SeuratObject::Images(seurat_file)}. Each entry may contain one
#'   or more arbitrarily named PNG, JPEG/JPG, or SVG files. A named character
#'   vector is path shorthand; a leaf may instead be
#'   \code{list(path = ..., bounds = c(xmin = ..., xmax = ..., ymin = ...,
#'   ymax = ...))}. Missing bounds are derived from the exported coordinates.
#'   Supplied images are embedded in the generated CRB; existing embedded images
#'   declared by the Seurat object are retained.
#' @param h5_compression_level,immune_sidecar,immune_sidecar_threshold Passed to
#'   \code{\link{exportFromSeurat}}: the gzip level of the H5 file (default 0,
#'   uncompressed), and whether and from which size the immune repertoire is
#'   stored in a sibling file.
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
#' Seurat uses the same named image collection for several platform-specific
#' structures: a name can identify a Visium slice, Xenium or MERFISH field of
#' view (FOV), or another Seurat v5 spatial image. It is a structural
#' spatial-entry key, not necessarily a donor or sample name. An entry can carry
#' multiple user-named backgrounds, or coordinates only. Seurat v4
#' \code{VisiumV1} images are rejected with guidance to rebuild them as
#' \code{VisiumV2} images.
#'
#' @examples
#' \dontrun{
#' library(CerebroNexus)
#'
#' input_dir <- system.file("extdata/examples", package = "CerebroNexus")
#' convertSeuratToCerebro(
#'   seurat_file = file.path(input_dir, "pbmc_seurat.rds"),
#'   result_dir = "output",
#'   assay = "RNA",
#'   slot = "data",
#'   experiment_name = "PBMC example",
#'   organism = "Human",
#'   groups = c("sample", "seurat_clusters"),
#'   groups_naming = list("seurat_clusters" = "cluster")
#' )
#' # Creates output/cerebro_pbmc_seurat.crb.
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
  expression_matrix_mode = c("embedded", "h5"),
  codec = c("qs2", "rds"),
  spatial_images = NULL,
  verbose = TRUE,
  cell_cycle = NULL,
  marker_file = NULL,
  marker_method = "Diff. Expression",
  add_most_expressed_genes = TRUE,
  most_expressed_genes = NULL,
  bcr_file = NULL,
  tcr_file = NULL,
  h5_compression_level = 0L,
  immune_sidecar = c("auto", "always", "never"),
  immune_sidecar_threshold = 100000L
) {
  expression_matrix_mode <- match.arg(expression_matrix_mode)
  immune_sidecar <- match.arg(immune_sidecar)
  codec <- match.arg(codec)
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

  # Validate groups, rename them, attach markers and summaries ---------------##
  validated <- .convertValidateGroups(
    seurat@meta.data,
    groups,
    max_group_levels,
    verbose
  )
  seurat@meta.data <- validated$meta
  groups <- validated$groups

  # Metadata repertoire extraction defaults to orig.ident. Keep that identity
  # tied to the column when groups_naming renames it below.
  renamed <- .convertRenameGroups(
    seurat@meta.data,
    seurat@misc,
    groups,
    groups_naming,
    repertoire_sample_col = "orig.ident"
  )
  seurat@meta.data <- renamed$meta
  seurat@misc <- renamed$misc
  groups <- renamed$groups
  repertoire_sample_col <- renamed$repertoire_sample_col

  # Load marker table (optional)
  seurat@misc <- .convertAttachMarkers(
    seurat@misc,
    marker_file,
    marker_method,
    verbose
  )

  ## Reused by exportFromSeurat() when conversion needs the matrix for summary
  ## statistics first. Keeping one validated resolution avoids joining a split
  ## Assay5 twice and holding two complete materialised matrices at once.
  expr_resolution <- NULL

  # Handle most_expressed_genes input ---------------------------------------##
  provided <- .convertAddProvidedMostExpressed(
    seurat@misc,
    most_expressed_genes,
    verbose
  )
  seurat@misc <- provided$misc
  if (provided$provided) {
    # Don't calculate if user provided data
    add_most_expressed_genes <- FALSE
  }

  # Calculate most expressed genes and mean expression from Seurat object ---##
  if (add_most_expressed_genes) {
    if (verbose) {
      .logStep(
        "Calculating most expressed genes and mean expression for each group..."
      )
    }

    # Get expression matrix (a missing layer is an error, never substituted).
    expr_resolution <- .getValidatedExpression(
      seurat,
      assay,
      slot,
      layer_argument = "slot"
    )
    expr_matrix <- expr_resolution$data

    seurat@misc <- .convertComputeGroupSummaries(
      seurat@misc,
      seurat@meta.data,
      expr_matrix,
      groups,
      verbose
    )
  }

  # Immune repertoire data --------------------------------------------------##
  # Priority: external files (bcr_file/tcr_file) > metadata extraction
  # All data is stored in the unified immune_repertoire slot.

  ## Same entry point a user calling exportFromSeurat() directly would use, so
  ## both routes build the slot the same way and get the same shape check.
  ## Priority is unchanged: given files replace whatever is in the slot, and
  ## the meta.data columns are read only when there is nothing else.
  has_repertoire_file <- (!is.null(bcr_file) && nzchar(bcr_file)) ||
    (!is.null(tcr_file) && nzchar(tcr_file))

  if (has_repertoire_file) {
    seurat <- addImmuneRepertoire(
      seurat,
      bcr = bcr_file,
      tcr = tcr_file,
      groups = groups,
      from_metadata = FALSE,
      verbose = verbose
    )
  } else if (
    is.null(seurat@misc$immune_repertoire) ||
      length(seurat@misc$immune_repertoire) == 0
  ) {
    seurat <- addImmuneRepertoire(
      seurat,
      groups = groups,
      sample_col = repertoire_sample_col,
      verbose = verbose
    )
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
        expression_matrix_mode = expression_matrix_mode,
        h5_compression_level = h5_compression_level,
        codec = codec,
        immune_sidecar = immune_sidecar,
        immune_sidecar_threshold = immune_sidecar_threshold,
        spatial_images = spatial_images,
        .expression_resolution = expr_resolution
      )
      if (!isTRUE(getOption("cerebro.quiet_runtime", FALSE))) {
        cat("Successfully exported:", file_name, "\n")
      }
    },
    error = function(e) {
      stop(
        "Error processing ",
        source_label,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  invisible(file.path(result_dir, file_name))
}
