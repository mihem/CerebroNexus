#' Assemble Cell Ranger contig annotations into clones
#'
#' Turning per-contig rows into the per-cell clone calls the app reads is
#' scRepertoire's job, and it is a job with real subtleties (chain pairing,
#' multi-chain cells, clone definitions). Delegate rather than reimplement.
#'
#' @keywords internal
#' @noRd
.combineContigAnnotations <- function(
  paths,
  receptor,
  sample_names = NULL,
  verbose = TRUE
) {
  if (!requireNamespace("scRepertoire", quietly = TRUE)) {
    stop(
      "Reading Cell Ranger contig annotations needs the scRepertoire package, ",
      "which assembles contigs into clones. Install it, or pass the output of ",
      "scRepertoire::combineTCR() / combineBCR() directly.",
      call. = FALSE
    )
  }

  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths) > 0) {
    stop(
      "Contig annotation file(s) not found: ",
      paste(missing_paths, collapse = ", "),
      call. = FALSE
    )
  }

  path_names <- names(paths)
  has_path_names <-
    !is.null(path_names) &&
    length(path_names) == length(paths) &&
    !anyNA(path_names) &&
    all(nzchar(path_names)) &&
    !anyDuplicated(path_names)
  if (is.null(sample_names) && !has_path_names) {
    stop(
      "Cell Ranger CSV paths need explicit sample identities. Name the path ",
      "vector (for example c(donorA = path_a, donorB = path_b)) or pass ",
      "`sample_names`.",
      call. = FALSE
    )
  }
  if (is.null(sample_names)) {
    sample_names <- unname(path_names)
  }
  if (
    !is.character(sample_names) ||
      length(sample_names) != length(paths) ||
      anyNA(sample_names) ||
      any(!nzchar(sample_names)) ||
      anyDuplicated(sample_names)
  ) {
    stop(
      "`sample_names` must contain one unique, non-empty name for each of the ",
      length(paths),
      " contig annotation file(s).",
      call. = FALSE
    )
  }
  if (
    has_path_names &&
      !identical(unname(path_names), unname(sample_names))
  ) {
    stop(
      "The CSV path names and `sample_names` do not match. Supply one ",
      "unambiguous sample mapping.",
      call. = FALSE
    )
  }

  if (verbose) {
    message(
      "[INFO] Reading ",
      length(paths),
      " ",
      toupper(receptor),
      " contig annotation file(s)"
    )
  }

  contigs <- lapply(paths, function(path) {
    utils::read.csv(path, stringsAsFactors = FALSE)
  })

  combined <- if (identical(toupper(receptor), "BCR")) {
    scRepertoire::combineBCR(contigs, samples = sample_names)
  } else {
    scRepertoire::combineTCR(contigs, samples = sample_names)
  }

  ## `samples =` prefixes every barcode with the sample name. The cell names
  ## have to carry the same prefix or nothing joins; the caller's shape check
  ## reports it when they do not.
  combined
}

#' Merge repertoires that describe the same samples
#'
#' A sample is one biological sample, not one receptor type, so `donorA`'s T
#' cells and `donorA`'s B cells belong in one table. Concatenating the two
#' lists instead produces two entries called `donorA`, and `x[["donorA"]]`
#' returns only the first -- so one receptor type silently disappears, while
#' the app and the HLA page read the list's names as sample identifiers.
#'
#' Cells are mutually exclusive between the two (a cell carries a TCR or a
#' BCR), which is why row-binding them is the right merge and is what the
#' vignettes have always told people to do by hand.
#'
#' @keywords internal
#' @noRd
.mergeRepertoiresBySample <- function(...) {
  parts <- Filter(
    function(part) !is.null(part) && length(part) > 0,
    list(...)
  )
  if (length(parts) == 0) {
    return(NULL)
  }
  if (length(parts) == 1) {
    return(parts[[1]])
  }

  sample_names <- unique(unlist(lapply(parts, names), use.names = FALSE))
  merged <- lapply(sample_names, function(sample_name) {
    frames <- Filter(
      Negate(is.null),
      lapply(parts, function(part) part[[sample_name]])
    )
    if (length(frames) == 1) {
      return(frames[[1]])
    }
    merged_barcodes <- unlist(
      lapply(frames, function(frame) {
        if ("barcode" %in% names(frame)) {
          unique(as.character(frame[["barcode"]]))
        } else {
          character()
        }
      }),
      use.names = FALSE
    )
    overlap <- unique(merged_barcodes[duplicated(merged_barcodes)])
    if (length(overlap) > 0L) {
      stop(
        "The same cell appears in both TCR and BCR inputs for sample `",
        sample_name,
        "`: ",
        paste(utils::head(overlap, 5L), collapse = ", "),
        ". Repertoire tables have one row per cell; resolve doublets or choose ",
        "one receptor assignment before merging.",
        call. = FALSE
      )
    }
    ## TCR and BCR tables carry the same scRepertoire columns in the ordinary
    ## case, but not necessarily -- fill rather than fail on a missing one.
    all_columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
    filled <- lapply(frames, function(frame) {
      for (column in setdiff(all_columns, names(frame))) {
        ## Length has to follow the frame: a sample can legitimately have no
        ## rows -- every receptor filtered out -- and assigning a scalar to a
        ## zero-row data.frame is an error ("replacement has 1 row, data has 0").
        frame[[column]] <- rep(NA, nrow(frame))
      }
      frame[, all_columns, drop = FALSE]
    })
    bound <- do.call(rbind, filled)
    rownames(bound) <- NULL
    bound
  })
  names(merged) <- sample_names
  merged
}

#' Resolve one repertoire argument into a named list of data.frames
#'
#' @keywords internal
#' @noRd
.resolveRepertoireInput <- function(
  input,
  receptor,
  sample_names = NULL,
  verbose = TRUE
) {
  if (is.null(input)) {
    return(NULL)
  }

  argument_label <- paste0("the `", tolower(receptor), "` argument")

  if (is.character(input)) {
    if (anyNA(input)) {
      stop(
        argument_label,
        " contains a missing file path.",
        call. = FALSE
      )
    }
    ## `convertSeuratToCerebro()` passes its `bcr_file` / `tcr_file` through
    ## unchanged, and those default to NULL but are often "" in scripts.
    input <- input[nzchar(input)]
    if (length(input) == 0) {
      return(NULL)
    }
    extensions <- tolower(tools::file_ext(input))

    input <- if (all(extensions == "rds")) {
      if (length(input) != 1) {
        stop(
          argument_label,
          " holds several .rds paths. Pass one .rds holding the whole named ",
          "list, or pass the list itself.",
          call. = FALSE
        )
      }
      .loadImmuneRepertoireData(input, receptor, verbose)
    } else if (all(extensions == "csv")) {
      .combineContigAnnotations(
        input,
        receptor = receptor,
        sample_names = sample_names,
        verbose = verbose
      )
    } else {
      stop(
        argument_label,
        " holds file paths that are neither all .rds nor all .csv: ",
        paste(unique(extensions), collapse = ", "),
        ". Pass a .rds holding a named list, Cell Ranger ",
        "filtered_contig_annotations.csv files, or the list itself.",
        call. = FALSE
      )
    }
  }

  .validateImmuneRepertoire(input, source_label = argument_label)
  input
}

#' Add immune repertoire data to a Seurat object
#'
#' Puts TCR and/or BCR data into \code{object@misc$immune_repertoire} in the
#' shape CerebroNexus reads: a list named by sample, each element a data.frame
#' whose \code{barcode} column matches the object's cell names.
#'
#' Before this function existed the slot had to be assigned by hand, following
#' a convention written down in a vignette with no function behind it and
#' nothing checking the result. The shape is checked here, so a mistake is
#' reported where it was made rather than surfacing as an empty page.
#'
#' @param object A \code{Seurat} object.
#' @param tcr TCR data, as either the named list \code{scRepertoire::combineTCR()}
#'   returns, the path to an \code{.rds} holding such a list, or a vector of
#'   Cell Ranger \code{filtered_contig_annotations.csv} paths (one per sample,
#'   assembled with scRepertoire).
#' @param bcr BCR data, in any of the forms accepted for \code{tcr}.
#' @param sample_col Metadata column identifying the sample, used when reading
#'   the repertoire out of \code{meta.data}; defaults to \code{"orig.ident"}.
#'   An explicit name must exist. Set to \code{NULL} to auto-detect
#'   \code{orig.ident}, \code{sample}, or \code{Sample}, in that order.
#' @param sample_names Sample names for the \code{.csv} form, in the order the
#'   paths are given. CSV inputs require either this argument or a completely
#'   named path vector; sample identities are never guessed from file paths.
#' @param groups Metadata columns to carry into the extracted data.frames when
#'   reading out of \code{meta.data}.
#' @param from_metadata When neither \code{tcr} nor \code{bcr} supplies data
#'   and the unified repertoire slot is absent or empty, read the repertoire
#'   out of scRepertoire's \code{meta.data} columns (\code{CTgene},
#'   \code{CTnt}, \code{CTaa}, \code{CTstrict}) if they are there. Defaults to
#'   \code{TRUE}. An explicit zero-row input remains empty rather than falling
#'   through to metadata.
#' @param verbose Print progress messages; defaults to \code{TRUE}.
#'
#' @return The \code{Seurat} object, with the repertoire in
#'   \code{@misc$immune_repertoire}. Returned unchanged when there is no
#'   repertoire to add.
#'
#' @details Each non-empty sample table must contain \code{barcode},
#'   \code{CTgene}, \code{CTnt}, \code{CTaa}, and \code{CTstrict}. Barcodes
#'   identify cells globally: duplicate rows, cross-sample reuse, and a cell
#'   present in both TCR and BCR inputs are errors. Rows whose barcodes are not
#'   cells in \code{object} are removed with a warning before storage; a complete
#'   mismatch is an error. Zero-row sample tables are treated as absent.
#'
#' @examples
#' \dontrun{
#' ## from scRepertoire's output
#' combined <- scRepertoire::combineTCR(contig_list, samples = donor_ids)
#' seurat <- addImmuneRepertoire(seurat, tcr = combined)
#'
#' ## or straight from Cell Ranger
#' seurat <- addImmuneRepertoire(
#'   seurat,
#'   tcr = file.path(sample_dirs, "filtered_contig_annotations.csv"),
#'   sample_names = donor_ids
#' )
#'
#' ## or from meta.data, after scRepertoire::combineExpression()
#' seurat <- addImmuneRepertoire(seurat)
#'
#' exportFromSeurat(seurat, file = "my_data.crb", experiment_name = "mine",
#'                  organism = "hg", groups = c("sample", "seurat_clusters"))
#' }
#'
#' @export
addImmuneRepertoire <- function(
  object,
  tcr = NULL,
  bcr = NULL,
  sample_col = "orig.ident",
  sample_names = NULL,
  groups = NULL,
  from_metadata = TRUE,
  verbose = TRUE
) {
  if (!methods::is(object, "Seurat")) {
    stop(
      "`object` must be of class 'Seurat', not '",
      class(object)[1],
      "'.",
      call. = FALSE
    )
  }
  validate_flag <- function(value, name) {
    if (
      !is.logical(value) ||
        length(value) != 1L ||
        is.na(value)
    ) {
      stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  validate_flag(from_metadata, "from_metadata")
  validate_flag(verbose, "verbose")

  tcr_data <- .resolveRepertoireInput(tcr, "TCR", sample_names, verbose)
  bcr_data <- .resolveRepertoireInput(bcr, "BCR", sample_names, verbose)

  ## A zero-row list is still an explicit answer: it means that receptor input
  ## contains no cells after filtering. Do not replace that answer with stale
  ## scRepertoire columns from meta.data. Blank character inputs remain
  ## equivalent to the historical "no file supplied" convention.
  input_is_explicit <- function(input) {
    if (is.null(input)) {
      return(FALSE)
    }
    if (is.character(input)) {
      return(length(input) > 0L && any(nzchar(input)))
    }
    TRUE
  }
  has_explicit_input <- input_is_explicit(tcr) || input_is_explicit(bcr)

  if (has_explicit_input) {
    repertoire <- .mergeRepertoiresBySample(tcr_data, bcr_data)
  } else {
    ## The unified slot is authoritative. In particular, a direct
    ## addImmuneRepertoire(object) call must not overwrite a valid slot merely
    ## because meta.data also carries scRepertoire columns.
    repertoire <- object@misc$immune_repertoire
  }
  repertoire <- .dropEmptyRepertoireSamples(repertoire)

  if (
    !has_explicit_input &&
      length(repertoire) == 0 &&
      isTRUE(from_metadata)
  ) {
    repertoire <- .extractRepertoireFromMetadata(
      object,
      groups = groups,
      sample_col = sample_col,
      verbose = verbose
    )
    repertoire <- .dropEmptyRepertoireSamples(repertoire)
  }

  if (is.null(repertoire) || length(repertoire) == 0) {
    if (has_explicit_input) {
      object@misc$immune_repertoire <- NULL
    }
    if (verbose) {
      message(
        if (has_explicit_input) {
          "[INFO] Explicit repertoire input is empty; stored repertoire cleared."
        } else {
          "[INFO] No immune repertoire data to add; object returned unchanged."
        }
      )
    }
    return(object)
  }

  repertoire <- .normalizeImmuneRepertoire(
    repertoire,
    cell_barcodes = colnames(object),
    source_label = "the immune repertoire data"
  )

  object@misc$immune_repertoire <- repertoire
  object
}
