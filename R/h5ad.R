## Native h5ad (AnnData) reader --------------------------------------------------
##
## Reads the AnnData on-disk format (anndata >= 0.8, encoding-version 0.1/0.2)
## straight from HDF5 with rhdf5, so no Python, reticulate or zellkonverter is
## involved. The result is a SingleCellExperiment, the hub every other route
## already converts from.
##
## AnnData keeps cells x genes (obs x var); Cerebro and SCE keep genes x cells.
## A CSR matrix of cells x genes is byte-for-byte the CSC matrix of genes x
## cells, so the common sparse case needs no transpose and no copy of the data.

#' @keywords internal
#' @noRd
.h5adRequire <- function() {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop(
      "Reading h5ad files requires the 'rhdf5' package. Install it via ",
      "BiocManager::install(\"rhdf5\").",
      call. = FALSE
    )
  }
}

#' Catalogue of the file's objects, so existence and object type are looked up
#' once instead of reopening the file for every question.
#' @keywords internal
#' @noRd
.h5adContext <- function(file, root = NULL) {
  if (.zarrIsStore(file)) {
    return(.zarrContext(file, root))
  }
  listing <- rhdf5::h5ls(file, recursive = TRUE, datasetinfo = FALSE)
  parent <- ifelse(listing$group == "/", "", listing$group)
  path <- paste0(parent, "/", listing$name)
  otype <- listing$otype
  prefix <- ""
  if (!is.null(root)) {
    ## MuData keeps one AnnData-shaped group per modality under mod/<name>;
    ## re-root so every reader below sees plain AnnData paths.
    start <- paste0("/", root, "/")
    keep <- startsWith(path, start)
    path <- substring(path[keep], nchar(start))
    otype <- otype[keep]
    prefix <- paste0(root, "/")
  }
  list(file = file, path = path, otype = otype, prefix = prefix)
}

#' Read a dataset relative to the context's root (plain file, or one modality).
#' @keywords internal
#' @noRd
.h5adRead <- function(ctx, path, ...) {
  if (identical(ctx$kind, "zarr")) {
    return(.zarrRead(ctx, path, ...))
  }
  rhdf5::h5read(ctx$file, paste0(ctx$prefix, path), ...)
}

#' @keywords internal
#' @noRd
.h5adHas <- function(ctx, path) {
  paste0("/", sub("^/", "", path)) %in% ctx$path
}

#' @keywords internal
#' @noRd
.h5adIsGroup <- function(ctx, path) {
  idx <- match(paste0("/", sub("^/", "", path)), ctx$path)
  !is.na(idx) && identical(as.character(ctx$otype[[idx]]), "H5I_GROUP")
}

#' @keywords internal
#' @noRd
.h5adChildren <- function(ctx, path) {
  prefix <- paste0("/", sub("^/", "", path), "/")
  paths <- ctx$path[startsWith(ctx$path, prefix)]
  rest <- substring(paths, nchar(prefix) + 1L)
  unique(sub("/.*$", "", rest))
}

#' @keywords internal
#' @noRd
.h5adAttrs <- function(ctx, path) {
  if (identical(ctx$kind, "zarr")) {
    return(.zarrAttrs(ctx, path))
  }
  tryCatch(
    rhdf5::h5readAttributes(ctx$file, paste0(ctx$prefix, path)),
    error = function(e) list()
  )
}

#' @keywords internal
#' @noRd
.h5adEncoding <- function(ctx, path) {
  type <- .h5adAttrs(ctx, path)[["encoding-type"]]
  if (is.null(type)) NA_character_ else as.character(type)[[1L]]
}

#' Drop HDF5's array wrapper and turn bool enums into logicals.
#' @keywords internal
#' @noRd
.h5adVector <- function(x) {
  if (is.factor(x)) {
    lv <- levels(x)
    if (length(lv) && all(lv %in% c("FALSE", "TRUE"))) {
      return(as.character(x) == "TRUE")
    }
    return(as.character(x))
  }
  as.vector(x)
}

#' Read one obs/var column (or index) in any of the AnnData encodings.
#' @keywords internal
#' @noRd
.h5adReadElement <- function(ctx, path) {
  type <- .h5adEncoding(ctx, path)
  if (is.na(type)) {
    type <- if (.h5adIsGroup(ctx, path)) "unknown-group" else "array"
  }
  switch(
    type,
    array = ,
    "string-array" = .h5adVector(.h5adRead(ctx, path)),
    categorical = {
      codes <- as.integer(.h5adVector(.h5adRead(ctx, paste0(path, "/codes"))))
      cats <- .h5adReadElement(ctx, paste0(path, "/categories"))
      ordered <- isTRUE(as.logical(.h5adAttrs(ctx, path)[["ordered"]]))
      codes[codes < 0L] <- NA_integer_
      factor(cats[codes + 1L], levels = as.character(cats), ordered = ordered)
    },
    "nullable-integer" = ,
    "nullable-boolean" = ,
    "nullable-string-array" = {
      values <- .h5adVector(.h5adRead(ctx, paste0(path, "/values")))
      mask <- .h5adVector(.h5adRead(ctx, paste0(path, "/mask")))
      if (identical(type, "nullable-boolean")) {
        values <- as.logical(values)
      }
      values[as.logical(mask)] <- NA
      values
    },
    stop(
      "Unsupported h5ad column encoding `",
      type,
      "` at `",
      path,
      "`.",
      call. = FALSE
    )
  )
}

#' Pre-0.7 AnnData stores obs/var as one compound dataset (a structured array)
#' with the index as its `index` field; categorical columns are integer codes
#' whose labels sit in uns[<column>_categories].
#' @keywords internal
#' @noRd
.h5adReadLegacyFrame <- function(ctx, path) {
  table <- .h5adRead(ctx, path)
  if (!is.data.frame(table) || !ncol(table)) {
    stop("Could not read `", path, "` as an AnnData table.", call. = FALSE)
  }
  index_name <- intersect(c("index", "_index"), names(table))[1L]
  if (is.na(index_name)) {
    index_name <- names(table)[[1L]]
  }
  index <- as.character(.h5adVector(table[[index_name]]))
  columns <- lapply(table[setdiff(names(table), index_name)], .h5adVector)
  for (column in names(columns)) {
    labels_path <- paste0("uns/", column, "_categories")
    if (is.integer(columns[[column]]) && .h5adHas(ctx, labels_path)) {
      columns[[column]] <- .h5adCodesToFactor(
        columns[[column]],
        as.character(.h5adVector(.h5adRead(ctx, labels_path)))
      )
    }
  }
  list(index = index, columns = columns)
}

#' @keywords internal
#' @noRd
.h5adCodesToFactor <- function(codes, labels) {
  codes <- as.integer(codes)
  codes[codes < 0L] <- NA_integer_
  factor(labels[codes + 1L], levels = labels)
}

#' Read an AnnData dataframe group (obs, var, raw/var) as an index plus columns.
#' @keywords internal
#' @noRd
.h5adReadDataFrame <- function(ctx, path, verbose = FALSE) {
  if (!.h5adHas(ctx, path)) {
    stop("h5ad file has no `", path, "` table.", call. = FALSE)
  }
  if (!.h5adIsGroup(ctx, path)) {
    return(.h5adReadLegacyFrame(ctx, path))
  }
  attrs <- .h5adAttrs(ctx, path)
  children <- .h5adChildren(ctx, path)
  index_name <- attrs[["_index"]]
  if (is.null(index_name) || !index_name %in% children) {
    index_name <- intersect(c("_index", "index"), children)[1L]
  }
  if (is.na(index_name)) {
    stop("h5ad table `", path, "` has no index.", call. = FALSE)
  }
  index <- as.character(.h5adReadElement(ctx, paste0(path, "/", index_name)))

  columns <- attrs[["column-order"]]
  columns <- if (is.null(columns)) {
    setdiff(children, c(index_name, "__categories"))
  } else {
    as.character(columns)
  }
  columns <- columns[nzchar(columns) & columns %in% children]
  skipped <- character()
  values <- lapply(columns, function(column) {
    tryCatch(
      {
        value <- .h5adReadElement(ctx, paste0(path, "/", column))
        labels_path <- paste0(path, "/__categories/", column)
        if (is.integer(value) && .h5adHas(ctx, labels_path)) {
          value <- .h5adCodesToFactor(
            value,
            as.character(.h5adVector(.h5adRead(ctx, labels_path)))
          )
        }
        value
      },
      error = function(e) {
        skipped <<- c(skipped, column)
        if (verbose) {
          message(
            "[INFO] Skipping h5ad column `",
            column,
            "`: ",
            conditionMessage(e)
          )
        }
        NULL
      }
    )
  })
  names(values) <- columns
  values <- values[!vapply(values, is.null, logical(1))]
  not_a_column <- vapply(values, length, integer(1)) != length(index)
  skipped <- c(skipped, names(values)[not_a_column])
  values <- values[!not_a_column]
  ## the per-column notes above are shown only with verbose = TRUE
  if (length(skipped) && !verbose) {
    message(
      "Skipped ",
      length(skipped),
      " column(s) of `",
      path,
      "` in an encoding that is not read: ",
      paste(utils::head(skipped, 5L), collapse = ", "),
      if (length(skipped) > 5L) ", ...",
      " (verbose = TRUE says why)."
    )
  }
  list(index = index, columns = values)
}

#' @keywords internal
#' @noRd
.h5adAsFrame <- function(table) {
  frame <- data.frame(row.names = table$index, check.names = FALSE)
  for (column in names(table$columns)) {
    frame[[column]] <- table$columns[[column]]
  }
  frame
}

#' Read a cells x genes AnnData matrix as genes x cells.
#' @keywords internal
#' @noRd
.h5adReadMatrix <- function(ctx, path, n_obs, n_vars) {
  type <- .h5adEncoding(ctx, path)
  attrs <- .h5adAttrs(ctx, path)
  if (is.na(type) && !is.null(attrs[["h5sparse_format"]])) {
    type <- paste0(attrs[["h5sparse_format"]], "_matrix")
    attrs[["shape"]] <- attrs[["h5sparse_shape"]]
  }
  if (is.na(type) && !.h5adIsGroup(ctx, path)) {
    type <- "array"
  }

  if (type %in% c("csr_matrix", "csc_matrix")) {
    shape <- as.integer(attrs[["shape"]])
    if (!identical(shape, c(as.integer(n_obs), as.integer(n_vars)))) {
      stop(
        "`",
        path,
        "` has shape ",
        paste(shape, collapse = " x "),
        " but obs/var describe ",
        n_obs,
        " x ",
        n_vars,
        ".",
        call. = FALSE
      )
    }
    ## rhdf5 returns 1-d arrays; dropping the dim in place avoids copying
    ## hundreds of millions of values the way as.vector() does.
    data <- .h5adRead(ctx, paste0(path, "/data"))
    dim(data) <- NULL
    data <- as.numeric(data)
    indices <- .h5adRead(ctx, paste0(path, "/indices"))
    dim(indices) <- NULL
    indptr <- .h5adRead(ctx, paste0(path, "/indptr"))
    dim(indptr) <- NULL
    if (length(data) > .Machine$integer.max) {
      stop(
        "`",
        path,
        "` has more than 2^31 - 1 non-zero values, beyond a dgCMatrix.",
        call. = FALSE
      )
    }
    indices <- as.integer(indices)
    indptr <- as.integer(indptr)
    csr <- identical(type, "csr_matrix")
    ## genes x cells: a CSR (cells x genes) is the CSC of genes x cells as is
    dims <- if (csr) c(n_vars, n_obs) else c(n_obs, n_vars)
    major <- if (csr) n_obs else n_vars
    if (length(indptr) != major + 1L || length(indices) != length(data)) {
      stop(
        "`",
        path,
        "` has inconsistent indptr/indices/data lengths.",
        call. = FALSE
      )
    }
    built <- tryCatch(
      methods::new("dgCMatrix", i = indices, p = indptr, x = data, Dim = dims),
      error = function(e) NULL
    )
    if (is.null(built)) {
      ## unsorted or repeated indices within a slice. Sorting each slice (a
      ## radix sort) is all an unsorted matrix needs; repeated entries are added
      ## up by the triplet rebuild. Rebuilding every Visium layer from triplets
      ## took 4 s.
      slice <- rep.int(seq_len(major), diff(indptr))
      ordered <- order(slice, indices, method = "radix")
      sorted_indices <- indices[ordered]
      ## the sort keeps every entry inside its slice: an index repeated within a
      ## slice is two equal neighbours that do not straddle a slice boundary
      same_as_previous <- diff(sorted_indices) == 0L
      starts <- indptr[-c(1L, length(indptr))]
      same_as_previous[starts[starts > 0L & starts < length(slice)]] <- FALSE
      repeated <- any(same_as_previous)
      rm(same_as_previous)
      built <- if (repeated) {
        methods::as(
          Matrix::sparseMatrix(
            i = indices + 1L,
            j = slice,
            x = data,
            dims = dims
          ),
          "CsparseMatrix"
        )
      } else {
        methods::new(
          "dgCMatrix",
          i = sorted_indices,
          p = indptr,
          x = data[ordered],
          Dim = dims
        )
      }
    }
    return(if (csr) built else methods::as(Matrix::t(built), "CsparseMatrix"))
  }

  if (identical(type, "array")) {
    ## C-order (cells x genes) is read by rhdf5 as an R (genes x cells) array
    dense <- .h5adRead(ctx, path)
    if (
      !identical(
        as.integer(dim(dense)),
        c(as.integer(n_vars), as.integer(n_obs))
      )
    ) {
      stop("Dense matrix `", path, "` does not match obs/var.", call. = FALSE)
    }
    storage.mode(dense) <- "double"
    return(methods::as(dense, "CsparseMatrix"))
  }
  stop(
    "Unsupported matrix encoding `",
    type,
    "` at `",
    path,
    "`.",
    call. = FALSE
  )
}

#' @keywords internal
#' @noRd
.h5adReductionName <- function(name) {
  name <- sub("^X_", "", name)
  if (tolower(name) %in% c("pca", "umap", "tsne")) toupper(name) else name
}

#' obsm entries with >= 2 numeric columns, as cells x dims matrices.
#' @keywords internal
#' @noRd
.h5adReadObsm <- function(ctx, cells, verbose = FALSE) {
  if (!.h5adHas(ctx, "obsm")) {
    return(list())
  }
  result <- list()
  if (!.h5adIsGroup(ctx, "obsm")) {
    ## legacy: one compound dataset whose array fields are the embeddings
    parts <- tryCatch(
      .h5adRead(ctx, "obsm", compoundAsDataFrame = FALSE),
      error = function(e) list()
    )
    for (entry in names(parts)) {
      embedding <- parts[[entry]]
      if (is.matrix(embedding)) {
        embedding <- t(embedding)
      }
      if (
        !is.matrix(embedding) ||
          nrow(embedding) != length(cells) ||
          ncol(embedding) < 2L
      ) {
        next
      }
      storage.mode(embedding) <- "double"
      dimnames(embedding) <- list(
        cells,
        paste0(.h5adReductionName(entry), "_", seq_len(ncol(embedding)))
      )
      result[[.h5adReductionName(entry)]] <- embedding
    }
    return(result)
  }
  for (entry in .h5adChildren(ctx, "obsm")) {
    if (startsWith(entry, "velocity_")) {
      ## scVelo's velocity field: vectors, not positions (see .h5adReadTrajectories)
      next
    }
    path <- paste0("obsm/", entry)
    embedding <- tryCatch(
      {
        if (.h5adIsGroup(ctx, path)) {
          if (!identical(.h5adEncoding(ctx, path), "dataframe")) {
            stop("unsupported encoding")
          }
          table <- .h5adReadDataFrame(ctx, path)
          numeric <- table$columns[vapply(
            table$columns,
            is.numeric,
            logical(1)
          )]
          do.call(cbind, numeric)
        } else {
          raw <- .h5adRead(ctx, path)
          if (is.matrix(raw)) t(raw) else NULL
        }
      },
      error = function(e) NULL
    )
    if (
      is.null(embedding) ||
        !is.matrix(embedding) ||
        nrow(embedding) != length(cells) ||
        ncol(embedding) < 2L
    ) {
      if (verbose) {
        message(
          "[INFO] Skipping obsm entry `",
          entry,
          "` (not a cells x >=2 numeric array)."
        )
      }
      next
    }
    storage.mode(embedding) <- "double"
    if (is.null(colnames(embedding))) {
      colnames(embedding) <- paste0(
        .h5adReductionName(entry),
        "_",
        seq_len(ncol(embedding))
      )
    }
    rownames(embedding) <- cells
    result[[.h5adReductionName(entry)]] <- embedding
  }
  result
}

#' scanpy's rank_genes_groups results as Cerebro marker tables: one table per
#' `groupby` variable whose first column holds that variable's levels, which is
#' what the Viewer needs to offer its per-group filter.
#' @keywords internal
#' @noRd
.h5adReadRankGenes <- function(ctx) {
  if (!.h5adHas(ctx, "uns")) {
    return(list())
  }
  markers <- list()
  for (entry in .h5adChildren(ctx, "uns")) {
    base <- paste0("uns/", entry)
    needed <- paste0(base, c("/names", "/scores"))
    if (
      !.h5adIsGroup(ctx, base) ||
        !all(vapply(needed, function(p) .h5adHas(ctx, p), logical(1)))
    ) {
      next
    }
    read_field <- function(field) {
      path <- paste0(base, "/", field)
      if (.h5adHas(ctx, path)) {
        tryCatch(.h5adRead(ctx, path), error = function(e) NULL)
      }
    }
    gene <- read_field("names")
    if (!is.data.frame(gene)) {
      next
    }
    params <- function(field) {
      path <- paste0(base, "/params/", field)
      if (.h5adHas(ctx, path)) {
        as.character(.h5adVector(.h5adRead(ctx, path)))[[1L]]
      } else {
        NA_character_
      }
    }
    groupby <- params("groupby")
    method <- params("method")
    fields <- list(
      score = read_field("scores"),
      avg_log2FC = read_field("logfoldchanges"),
      p_val = read_field("pvals"),
      p_val_adj = read_field("pvals_adj")
    )
    group_labels <- names(gene)
    labels_path <- paste0("uns/", groupby, "_categories")
    if (
      !is.na(groupby) &&
        all(grepl("^[0-9]+$", group_labels)) &&
        .h5adHas(ctx, labels_path)
    ) {
      ## legacy files name the columns by group code, not label
      labels <- as.character(.h5adVector(.h5adRead(ctx, labels_path)))
      names(gene) <- labels[as.integer(group_labels) + 1L]
      for (field in names(fields)) {
        if (is.data.frame(fields[[field]])) {
          names(fields[[field]]) <- labels[
            as.integer(names(fields[[field]])) + 1L
          ]
        }
      }
    }
    tables <- lapply(names(gene), function(level) {
      table <- data.frame(
        level = level,
        gene = as.character(gene[[level]]),
        stringsAsFactors = FALSE
      )
      for (field in names(fields)) {
        if (
          is.data.frame(fields[[field]]) && level %in% names(fields[[field]])
        ) {
          table[[field]] <- as.numeric(fields[[field]][[level]])
        }
      }
      table[!is.na(table$gene) & nzchar(table$gene), , drop = FALSE]
    })
    table <- do.call(rbind, tables)
    if (is.null(table) || !nrow(table)) {
      next
    }
    rownames(table) <- NULL
    group_name <- if (is.na(groupby)) entry else groupby
    names(table)[[1L]] <- group_name
    label <- paste0("scanpy: ", if (is.na(method)) entry else method)
    markers[[label]][[group_name]] <- table
  }
  markers
}

#' Marker tables that exportToH5ad() stores under uns["cerebro"]["marker_genes"],
#' so a Cerebro -> h5ad -> Cerebro trip keeps them.
#' @keywords internal
#' @noRd
.h5adReadCerebroMarkers <- function(ctx) {
  root <- "uns/cerebro/marker_genes"
  if (!.h5adHas(ctx, root)) {
    return(list())
  }
  markers <- list()
  for (method in .h5adChildren(ctx, root)) {
    method_path <- paste0(root, "/", method)
    for (table in .h5adChildren(ctx, method_path)) {
      frame <- tryCatch(
        {
          raw <- .h5adReadDataFrame(ctx, paste0(method_path, "/", table))
          frame <- .h5adAsFrame(raw)
          rownames(frame) <- NULL
          frame
        },
        error = function(e) NULL
      )
      if (is.data.frame(frame) && nrow(frame)) {
        markers[[method]][[table]] <- frame
      }
    }
  }
  markers
}

#' Use a var column (feature_name, gene_symbols, ...) as the gene names. By
#' default this happens only when the var index is made of Ensembl IDs and such
#' a column exists, which is how CELLxGENE files are laid out.
#' @keywords internal
#' @noRd
.h5adGeneNames <- function(index, columns, gene_names = NULL) {
  if (is.null(gene_names)) {
    looks_ensembl <- mean(grepl(
      "^ENS[A-Z]*[GTP][0-9]{6,}",
      utils::head(index, 200L)
    )) >
      0.9
    candidates <- intersect(
      c(
        "feature_name",
        "gene_symbols",
        "gene_symbol",
        "gene_name",
        "gene_names",
        "symbol"
      ),
      names(columns)
    )
    if (!looks_ensembl || !length(candidates)) {
      return(index)
    }
    gene_names <- candidates[[1L]]
  } else if (!gene_names %in% names(columns)) {
    stop(
      "`gene_names = \"",
      gene_names,
      "\"` is not a column of var: ",
      paste(names(columns), collapse = ", "),
      call. = FALSE
    )
  }
  symbols <- as.character(columns[[gene_names]])
  missing <- is.na(symbols) | !nzchar(symbols)
  symbols[missing] <- index[missing]
  message(
    "[INFO] Using var column `",
    gene_names,
    "` as gene names (the var index is kept in rowData as `var_index`)."
  )
  symbols
}

#' Read an h5ad file as a SingleCellExperiment
#'
#' @description
#' Reads the AnnData on-disk format (anndata 0.8 or later) directly from HDF5
#' with \pkg{rhdf5}, without Python or \pkg{zellkonverter}, and returns a
#' \code{SingleCellExperiment}. From there the object can go to
#' \code{\link{convertSCEToCerebro}}, or to any other tool that reads SCE.
#'
#' @details
#' The mapping is:
#' \itemize{
#'   \item \code{X} becomes the assay \code{"X"}; each entry of \code{layers}
#'     becomes an assay of the same name (so \code{layers["counts"]} is the
#'     assay \code{"counts"}).
#'   \item \code{obs} / \code{var} become \code{colData} / \code{rowData}.
#'     Categorical, string, boolean and nullable integer/boolean/string columns
#'     are all decoded; missing values stay \code{NA}.
#'   \item \code{obsm} entries with at least two numeric columns become
#'     \code{reducedDims}; a leading \code{X_} is dropped and \code{pca},
#'     \code{umap} and \code{tsne} are upper-cased (\code{X_umap} is
#'     \code{"UMAP"}).
#'   \item scanpy's \code{uns["rank_genes_groups"]} results become
#'     \code{metadata(sce)$marker_genes}, one table per \code{groupby}
#'     variable; marker tables written by \code{\link{exportToH5ad}} under
#'     \code{uns["cerebro"]["marker_genes"]} are read back as they were.
#'   \item immune receptor chains written by scirpy (an awkward array in
#'     \code{obsm["airr"]}, or the \code{airr} modality of a MuData file) become
#'     \code{metadata(sce)$immune_repertoire}: a list named by sample, one row
#'     per cell with \code{barcode}, \code{CTgene}, \code{CTnt}, \code{CTaa} and
#'     \code{CTstrict}, assembled into clones by \pkg{scRepertoire} as on the
#'     Seurat route. Only productive chains of TCR/BCR loci with a CDR3
#'     (\code{junction_aa}) count; a cell that shows both TCR and BCR chains
#'     keeps the type with more chains (then more UMIs). Cells are matched to
#'     the expression data by name.
#'   \item trajectories become \code{metadata(sce)$trajectories}, the
#'     structure of \code{object@misc$trajectories}. For every pseudotime
#'     column in \code{obs} (\code{dpt_pseudotime},
#'     \code{velocity_pseudotime}, \code{latent_time}, any
#'     \code{*_pseudotime}) one trajectory is made per source: scanpy's PAGA
#'     (\code{uns["paga"]}) as method \code{"paga"}, with the groups as nodes
#'     at the centroid of their cells in the UMAP (t-SNE) and the
#'     connectivities of at least 0.01 as edges; scVelo's
#'     \code{obsm["velocity_<basis>"]} as method \code{"scvelo"}, the velocity
#'     averaged on a grid (25 x 25 at most) and drawn as arrows; and, when the file has
#'     neither, just the pseudotime as method \code{"pseudotime"}. The
#'     \code{velocity_*} entries of \code{obsm} are not embeddings and are
#'     not added to \code{reducedDims}. Gene-level velocity (\code{layers}
#'     \code{velocity}) and the directed PAGA transitions are not used.
#'   \item scanpy's category colours (\code{uns["<column>_colors"]}) become
#'     \code{metadata(sce)$group_colors}, a named vector (level to colour) per
#'     categorical column, and are applied as the default colours of those
#'     groups in the Viewer. A column is used only when it has one valid colour
#'     for each of its levels.
#'   \item Visium tissue images in \code{uns["spatial"][<library_id>]["images"]}
#'     become \code{metadata(sce)$spatial_images}, one entry per library,
#'     each with its image as an embedded PNG and its extent in spot-coordinate
#'     space: spot coordinates (\code{obsm["spatial"]}) are full-resolution
#'     pixels and an image is the full-resolution picture scaled by its
#'     \code{tissue_*_scalef}, so it covers \code{[0, width/scale]} by
#'     \code{[0, height/scale]}. The image keeps its own orientation (row 0 on
#'     top); flip it for display in the Spatial tab, as for Seurat objects.
#' }
#' Sparse \code{X} (CSR or CSC) and dense \code{X} are supported. A CSR matrix
#' is used as it is, because the CSR of cells x genes is the CSC of genes x
#' cells. Other \code{uns}, \code{obsp}, \code{varm} and \code{varp} content is
#' not read.
#'
#' @param file Path to a \code{.h5ad} file, or a \code{.h5mu} (MuData) file
#'   together with \code{modality}. Files written by anndata before 0.7
#'   (compound-dataset layout, \code{raw.X}) are read too.
#' @param use_raw Use \code{raw/X} (and \code{raw/var}) as the expression
#'   matrix, as scanpy workflows often keep the full, unfiltered gene set
#'   there. \code{layers} are then not read, because their genes differ.
#' @param extra_modalities For a MuData file: other modalities to read next to
#'   the expression one (for example \code{"prot"} for antibody counts), or
#'   \code{"all"} for every modality except the expression and receptor ones. Each
#'   becomes an alternative experiment of the result, with its own features and
#'   the cells of the expression modality (cells it lacks get zeros).
#' @param modality For a MuData file (\code{.h5mu}), the modality to read, e.g.
#'   \code{"gex"}. Optional when the file has one modality or one obviously
#'   expression-like name (\code{rna}, \code{gex}).
#' @param spatial_images Which Visium tissue images to embed: \code{"auto"}
#'   (default; \code{hires} when present, otherwise \code{lowres}), a character
#'   vector of image labels (\code{"hires"}, \code{"lowres"},
#'   \code{"fullres"}), or \code{FALSE} for none. Needs the \pkg{png} package.
#' @param gene_names Name of a \code{var} column to use as the gene names, e.g.
#'   \code{"feature_name"} (CELLxGENE) or \code{"gene_symbols"}. By default the
#'   \code{var} index is used, except when it is made of Ensembl IDs and such a
#'   column exists. The original index stays in \code{rowData} as
#'   \code{var_index}.
#' @param airr Immune receptor data to import: \code{"auto"} (default) uses the
#'   MuData modality named \code{airr}, or \code{obsm["airr"]} of a plain h5ad,
#'   if the file has one and imports nothing otherwise. Give a name to use
#'   another modality (or \code{obsm} entry); \code{FALSE} or \code{NULL} skips
#'   receptor data. The AIRR modality is never taken for the expression
#'   modality.
#' @param airr_sample Column of \code{obs} that names the sample of each cell,
#'   which keys the immune repertoire. By default \code{orig.ident},
#'   \code{sample}, \code{Sample} or \code{sample_id} is used if present, else
#'   all cells are one sample (\code{"Sample_1"}).
#' @param verbose Print what was skipped.
#' @param .root Internal: HDF5 group that holds the data set (used for the
#'   modalities inside a MuData file). Leave \code{NULL}.
#'
#' @return A \code{SingleCellExperiment} with genes in rows and cells in
#'   columns. Duplicate gene names are made unique with a message; duplicate
#'   cell names are an error.
#'
#' @examplesIf requireNamespace("rhdf5") && requireNamespace("SingleCellExperiment")
#' \donttest{
#' h5ad <- file.path(tempdir(), "example.h5ad")
#' exportToH5ad(
#'   system.file("extdata/examples/example.crb", package = "CerebroNexus"),
#'   h5ad
#' )
#' sce <- readH5adAsSCE(h5ad)
#' convertSCEToCerebro(sce, file.path(tempdir(), "output"), groups = "sample")
#' }
#'
#' @seealso \code{\link{convertH5adToCerebro}}, \code{\link{convertSCEToCerebro}}
#'
#' @export
readH5adAsSCE <- function(
  file,
  use_raw = FALSE,
  modality = NULL,
  gene_names = NULL,
  airr = "auto",
  airr_sample = NULL,
  spatial_images = "auto",
  extra_modalities = NULL,
  verbose = FALSE,
  .root = NULL
) {
  .h5adRequire()
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    stop(
      "The 'SingleCellExperiment' package is needed to read h5ad files.",
      call. = FALSE
    )
  }
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    stop("h5ad file not found: ", paste(file, collapse = ", "), call. = FALSE)
  }
  ctx <- tryCatch(
    .h5adContext(file, root = .root),
    error = function(e) {
      stop(
        "`",
        file,
        "` is not a readable HDF5/h5ad file: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  modalities <- if (.h5adHas(ctx, "mod")) {
    .h5adChildren(ctx, "mod")
  } else {
    character(0)
  }
  airr_source <- .h5adFindAirr(ctx, airr, modalities, file)
  if (.h5adHas(ctx, "mod")) {
    ## MuData (.h5mu): read one modality, which is an AnnData in its own right
    available <- modalities
    if (is.null(modality)) {
      ## the receptor modality holds no expression to read
      candidates <- setdiff(
        available,
        if (identical(airr_source$kind, "modality")) airr_source$name
      )
      if (!length(candidates)) {
        candidates <- available
      }
      guess <- candidates[
        tolower(candidates) %in% c("rna", "gex", "expression")
      ]
      if (length(guess) != 1L && length(candidates) != 1L) {
        stop(
          "`",
          file,
          "` is a MuData file with modalities ",
          paste(available, collapse = ", "),
          ". Choose the expression one with `modality =`.",
          call. = FALSE
        )
      }
      modality <- if (length(guess) == 1L) guess else candidates
    }
    if (!modality %in% available) {
      stop(
        "Modality `",
        modality,
        "` not found; the file has: ",
        paste(available, collapse = ", "),
        call. = FALSE
      )
    }
    if (verbose) {
      message("[INFO] Reading MuData modality `", modality, "`.")
    }
    ctx <- .h5adContext(file, root = paste0("mod/", modality))
  } else if (!is.null(modality)) {
    stop("`modality` is only for MuData (.h5mu) files.", call. = FALSE)
  }
  if (!.h5adHas(ctx, "X") && !.h5adHas(ctx, "layers")) {
    stop(
      "`",
      file,
      "` has no `X` matrix or layers; is it an h5ad file?",
      call. = FALSE
    )
  }

  obs <- .h5adReadDataFrame(ctx, "obs", verbose)
  cells <- obs$index
  if (anyNA(cells) || any(!nzchar(cells)) || anyDuplicated(cells)) {
    stop(
      "h5ad obs index (cell names) must be unique and non-empty.",
      call. = FALSE
    )
  }

  var_path <- "var"
  x_path <- "X"
  if (isTRUE(use_raw)) {
    ## current files use raw/X; anndata < 0.7 wrote raw.X
    raw_x <- c("raw/X", "raw.X")[c(
      .h5adHas(ctx, "raw/X"),
      .h5adHas(ctx, "raw.X")
    )][1L]
    if (is.na(raw_x)) {
      stop("`use_raw = TRUE` but the file has no `raw/X`.", call. = FALSE)
    }
    x_path <- raw_x
    var_path <- sub("X$", "var", raw_x)
  }
  var <- .h5adReadDataFrame(ctx, var_path, verbose)
  genes <- var$index
  genes <- .h5adGeneNames(genes, var$columns, gene_names)
  var$columns$var_index <- var$index
  if (anyNA(genes) || any(!nzchar(genes))) {
    stop("h5ad var index (gene names) must be non-empty.", call. = FALSE)
  }
  if (anyDuplicated(genes)) {
    message(
      "[INFO] ",
      sum(duplicated(genes)),
      " duplicated gene name(s) were made unique with make.unique()."
    )
    genes <- make.unique(genes)
  }

  assays <- list()
  with_dimnames <- function(m) {
    dimnames(m) <- list(genes, cells)
    m
  }
  if (.h5adHas(ctx, x_path)) {
    assays[["X"]] <- with_dimnames(.h5adReadMatrix(
      ctx,
      x_path,
      length(cells),
      length(genes)
    ))
  }
  if (!isTRUE(use_raw) && .h5adHas(ctx, "layers")) {
    for (layer in .h5adChildren(ctx, "layers")) {
      assays[[layer]] <- tryCatch(
        with_dimnames(.h5adReadMatrix(
          ctx,
          paste0("layers/", layer),
          length(cells),
          length(genes)
        )),
        error = function(e) {
          message("[INFO] Skipping layer `", layer, "`: ", conditionMessage(e))
          NULL
        }
      )
    }
    assays <- assays[!vapply(assays, is.null, logical(1))]
  }
  if (!length(assays)) {
    stop("No readable expression matrix found in `", file, "`.", call. = FALSE)
  }

  row_data <- .h5adAsFrame(list(index = genes, columns = var$columns))
  embeddings <- .h5adReadObsm(ctx, cells, verbose)
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = assays,
    colData = .h5adAsFrame(obs),
    rowData = row_data,
    reducedDims = embeddings
  )
  if (identical(extra_modalities, "all")) {
    ## every modality but the expression one and the receptor one
    extra_modalities <- setdiff(
      modalities,
      c(
        modality,
        if (identical(airr_source$kind, "modality")) airr_source$name,
        ## a receptor modality has no X to read, also when airr = FALSE
        modalities[tolower(modalities) == "airr"]
      )
    )
  }
  if (length(extra_modalities)) {
    sce <- .h5adAddAltModalities(
      sce,
      file,
      modalities,
      extra_modalities,
      modality,
      verbose
    )
  }
  markers <- c(.h5adReadRankGenes(ctx), .h5adReadCerebroMarkers(ctx))
  if (length(markers)) {
    sce@metadata$marker_genes <- markers
  }
  if (!is.null(airr_source)) {
    repertoire <- .h5adReadRepertoire(
      file,
      ctx_expression = ctx,
      source = airr_source,
      cells = cells,
      col_data = .h5adAsFrame(obs),
      sample_col = airr_sample,
      verbose = verbose
    )
    if (length(repertoire)) {
      sce@metadata$immune_repertoire <- repertoire
    }
  }
  trajectories <- .h5adReadTrajectories(ctx, obs, embeddings, verbose)
  if (length(trajectories)) {
    sce@metadata$trajectories <- trajectories
  }
  colors <- .h5adReadColors(ctx, obs$columns)
  if (length(colors)) {
    sce@metadata$group_colors <- colors
  }
  images <- .h5adReadSpatialImages(ctx, spatial_images, verbose)
  if (length(images)) {
    sce@metadata$spatial_images <- images
  }
  sce
}

#' Read further modalities of a MuData file as alternative experiments.
#'
#' Features keep their own names; cells are those of the main experiment (a cell
#' the modality does not have gets zeros, a cell it has extra is dropped).
#' @keywords internal
#' @noRd
.h5adAddAltModalities <- function(
  sce,
  file,
  available,
  wanted,
  main,
  verbose = FALSE
) {
  if (!length(available)) {
    stop("`extra_modalities` is only for MuData (.h5mu) files.", call. = FALSE)
  }
  unknown <- setdiff(wanted, available)
  if (length(unknown)) {
    stop(
      "Modality `",
      unknown[[1L]],
      "` not found; the file has: ",
      paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  for (name in setdiff(wanted, main)) {
    ctx <- .h5adContext(file, root = paste0("mod/", name))
    obs <- .h5adReadDataFrame(ctx, "obs", verbose)
    var <- .h5adReadDataFrame(ctx, "var", verbose)
    genes <- make.unique(as.character(var$index))
    values <- .h5adReadMatrix(ctx, "X", length(obs$index), length(genes))
    triplets <- Matrix::summary(methods::as(values, "CsparseMatrix"))
    column <- match(obs$index, colnames(sce))[triplets$j]
    keep <- !is.na(column)
    full <- Matrix::sparseMatrix(
      i = triplets$i[keep],
      j = column[keep],
      x = triplets$x[keep],
      dims = c(length(genes), ncol(sce)),
      dimnames = list(genes, colnames(sce))
    )
    SingleCellExperiment::altExp(
      sce,
      name
    ) <- SummarizedExperiment::SummarizedExperiment(
      assays = list(X = full)
    )
    if (verbose) {
      message(
        "[INFO] Read modality `",
        name,
        "` (",
        length(genes),
        " features) as an ",
        "alternative experiment; ",
        sum(!colnames(sce) %in% obs$index),
        " cell(s) of the expression modality have no values there."
      )
    }
  }
  sce
}
