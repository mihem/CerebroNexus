## Allen Brain Cell Atlas, whole mouse brain MERFISH (Yao et al. 2023, Nature
## 624:317): 59 coronal sections of one adult mouse brain (C57BL6J-638850),
## 550 genes, the cells mapped to the whole mouse brain taxonomy. See
## allen.ATTRIBUTION.md.

.viewerAllenBaseUrl <- "https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/"

## Size and MD5 of each file are the ones the atlas publishes in its release
## manifest (releases/20231215/manifest.json, `size` and `file_hash`).
.viewerAllenFiles <- function() {
  file <- function(path, size, md5) {
    list(url = paste0(.viewerAllenBaseUrl, path), size = size, md5 = md5)
  }
  list(
    cells = file(
      paste0(
        "metadata/MERFISH-C57BL6J-638850/20231215/views/",
        "cell_metadata_with_cluster_annotation.csv"
      ),
      1018613800,
      "b4cddf29d092d5cb29f73af1cc156f7a"
    ),
    genes = file(
      "metadata/MERFISH-C57BL6J-638850/20231215/gene.csv",
      48416,
      "d9cee1242bc13374b5e2beeea827de3e"
    ),
    expression = file(
      paste0(
        "expression_matrices/MERFISH-C57BL6J-638850/20230830/",
        "C57BL6J-638850-log2.h5ad"
      ),
      7627589574,
      "0630f7a36d36ebd336cd8d0564b07b14"
    )
  )
}

.viewerAllenPaths <- function(cache_dir = NULL) {
  if (!exists(".demoLargeCacheDir", mode = "function")) {
    stop(
      "Source data-raw/large-demo/download_helpers.R before building the demo data.",
      call. = FALSE
    )
  }
  root <- file.path(.demoLargeCacheDir(cache_dir), "allen")
  prepared <- .demoPreparedDir(root)
  source <- file.path(root, "source")
  list(
    root = root,
    source_dir = source,
    files = c(
      cells = file.path(source, "cell_metadata_with_cluster_annotation.csv"),
      genes = file.path(source, "gene.csv"),
      expression = file.path(source, "C57BL6J-638850-log2.h5ad")
    ),
    crb = file.path(prepared, "cerebro_allen_merfish.crb"),
    sidecar = file.path(prepared, "cerebro_allen_merfish.h5"),
    stamp = file.path(prepared, "demo-version")
  )
}

## The taxonomy levels kept as groups, each with the colour the atlas gives
## its terms. The 5,322 clusters are too many to pick from or tell apart; the
## 1,201 supertypes are the finest level kept.
.viewerAllenLevels <- c("class", "subclass", "supertype", "neurotransmitter")

## Reads the cell table: one row per cell, the section ordered from front to
## back, the taxonomy and the colours of its terms. Returns the cells, their
## coordinates and the group colours.
.viewerAllenReadCells <- function(file) {
  columns <- c(
    "cell_label",
    "brain_section_label",
    "average_correlation_score",
    "x",
    "y",
    "z",
    .viewerAllenLevels,
    paste0(.viewerAllenLevels, "_color")
  )
  table <- data.table::fread(
    file,
    select = columns,
    colClasses = list(character = "cell_label"),
    showProgress = FALSE,
    data.table = FALSE,
    na.strings = ""
  )
  missing <- setdiff(columns, names(table))
  if (length(missing)) {
    stop(
      "The Allen cell table lacks columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(table$cell_label) || anyNA(table$brain_section_label)) {
    stop(
      "The Allen cell table needs unique cells, each on a section.",
      call. = FALSE
    )
  }
  ## Sections are named by their number on the slide series and ordered by
  ## their position along the brain (z).
  number <- as.integer(sub("^.*[.]", "", table$brain_section_label))
  position <- tapply(table$z, number, stats::median)
  order <- as.integer(names(position))[order(position)]
  section <- factor(
    sprintf("Section %02d", number),
    levels = sprintf("Section %02d", order)
  )

  cells <- data.frame(
    cell_barcode = table$cell_label,
    section = section,
    row.names = table$cell_label,
    stringsAsFactors = FALSE
  )
  colors <- list()
  for (level in .viewerAllenLevels) {
    terms <- table[[level]]
    terms[is.na(terms)] <- "N/A"
    cells[[level]] <- factor(
      terms,
      levels = sort(unique(terms), method = "radix")
    )
    color <- table[[paste0(level, "_color")]]
    known <- !is.na(color) & terms != "N/A"
    first <- !duplicated(terms) & known
    colors[[level]] <- stats::setNames(toupper(color[first]), terms[first])
  }
  cells$mapping_score <- table$average_correlation_score
  cells$z <- table$z
  ## The atlas's y axis grows downwards (image rows); the Viewer's grows
  ## upwards, so y is mirrored to keep the section upright.
  coordinates <- data.frame(
    x = table$x,
    y = -table$y,
    row.names = table$cell_label
  )
  list(cells = cells, coordinates = coordinates, colors = colors)
}

## Gene symbols for the h5ad's gene identifiers; a symbol given twice keeps its
## identifier.
.viewerAllenGeneNames <- function(identifiers, file) {
  genes <- data.table::fread(file, showProgress = FALSE, data.table = FALSE)
  symbols <- genes$gene_symbol[match(identifiers, genes$gene_identifier)]
  if (anyNA(symbols) || length(identifiers) != nrow(genes)) {
    stop("The Allen gene table does not name every gene.", call. = FALSE)
  }
  repeated <- symbols %in% symbols[duplicated(symbols)]
  symbols[repeated] <- paste0(symbols[repeated], "_", identifiers[repeated])
  symbols
}

.viewerAllenH5adNames <- function(h5ad, axis) {
  index <- rhdf5::h5readAttributes(h5ad, axis)[["_index"]]
  if (is.null(index)) {
    index <- "_index"
  }
  as.character(as.vector(rhdf5::h5read(h5ad, paste0(axis, "/", index))))
}

## The expression of `cells`, genes x cells, from the h5ad's X: stored by cell
## (CSR) or dense, both read as they are, cells in the order of `cells`.
.viewerAllenReadExpression <- function(h5ad, cells, gene_file) {
  obs <- .viewerAllenH5adNames(h5ad, "obs")
  genes <- .viewerAllenGeneNames(.viewerAllenH5adNames(h5ad, "var"), gene_file)
  column <- match(cells, obs)
  if (anyNA(column)) {
    stop(
      format(sum(is.na(column)), big.mark = ","),
      " cells of the Allen cell table have no expression.",
      call. = FALSE
    )
  }
  encoding <- rhdf5::h5readAttributes(h5ad, "X")[["encoding-type"]]
  encoding <- if (is.null(encoding)) "array" else as.character(encoding)
  if (identical(encoding, "csr_matrix")) {
    indptr <- as.vector(rhdf5::h5read(
      h5ad,
      "X/indptr",
      bit64conversion = "double"
    ))
    if (length(indptr) != length(obs) + 1L) {
      stop(
        "The Allen expression matrix does not match its cells.",
        call. = FALSE
      )
    }
    values <- rhdf5::h5read(h5ad, "X/data")
    dim(values) <- NULL
    rows <- rhdf5::h5read(h5ad, "X/indices")
    dim(rows) <- NULL
    ## A cell's run of the arrays is a column of the genes x cells matrix.
    if (!identical(column, seq_along(obs))) {
      lengths <- diff(indptr)[column]
      keep <- sequence(lengths, from = indptr[column] + 1)
      values <- values[keep]
      rows <- rows[keep]
      indptr <- c(0, cumsum(lengths))
    }
    expression <- methods::new(
      "dgCMatrix",
      i = as.integer(rows),
      p = as.integer(indptr),
      x = as.numeric(values),
      Dim = c(length(genes), length(cells))
    )
  } else if (identical(encoding, "array")) {
    ## Blocks of cells: rhdf5 returns the h5ad's cells x genes as genes x cells.
    blocks <- split(column, ceiling(seq_along(column) / 250000L))
    expression <- do.call(
      cbind,
      lapply(blocks, function(block) {
        values <- rhdf5::h5read(h5ad, "X", index = list(NULL, block))
        methods::as(Matrix::Matrix(values, sparse = TRUE), "CsparseMatrix")
      })
    )
  } else {
    stop(
      "The Allen expression matrix is stored as ",
      encoding,
      ", not by cell.",
      call. = FALSE
    )
  }
  dimnames(expression) <- list(genes, cells)
  expression
}

## Joint embedding of all sections: PCA on the scaled genes, then UMAP. The
## atlas publishes no embedding of the MERFISH cells, so it is computed here and
## labelled as such. Four million cells: fewer neighbours and the faster, still
## deterministic per seed optimisation of uwot.
.viewerAllenEmbedding <- function(
  expression,
  n_pcs = 30L,
  threads = max(
    1L,
    min(16L, parallel::detectCores(logical = FALSE)),
    na.rm = TRUE
  ),
  seed = 20231213L
) {
  .demoRequire(c("irlba", "uwot"), "Allen")
  x <- Matrix::t(expression)
  centre <- Matrix::colMeans(x)
  spread <- sqrt(pmax(Matrix::colMeans(x^2) - centre^2, 1e-12))
  set.seed(seed)
  pca <- irlba::irlba(x, nv = n_pcs, center = centre, scale = spread)
  rm(x)
  scores <- pca$u %*% diag(pca$d, nrow = length(pca$d))
  umap <- uwot::umap(
    scores,
    n_neighbors = 15L,
    min_dist = 0.3,
    n_threads = threads,
    n_sgd_threads = threads,
    fast_sgd = TRUE,
    seed = seed
  )
  data.frame(
    Dim_1 = umap[, 1L],
    Dim_2 = umap[, 2L],
    row.names = colnames(expression)
  )
}

## A spatial entry of the Viewer: the section's coordinates, and an empty
## expression matrix (the Viewer reads expression from the H5 sidecar).
.viewerAllenSpatialEntry <- function(coordinates, genes) {
  list(
    coordinates = coordinates,
    expression = Matrix::sparseMatrix(
      i = integer(),
      j = integer(),
      x = numeric(),
      dims = c(length(genes), nrow(coordinates)),
      dimnames = list(genes, NULL)
    ),
    histology_images = list()
  )
}

.viewerAllenBuildObject <- function(paths) {
  message("Reading the Allen cell table...")
  table <- .viewerAllenReadCells(paths$files[["cells"]])
  metadata <- table$cells
  message("Reading the Allen expression matrix...")
  expression <- .viewerAllenReadExpression(
    paths$files[["expression"]],
    metadata$cell_barcode,
    paths$files[["genes"]]
  )
  metadata$nGene <- as.integer(diff(expression@p))
  message(
    "Expression: ",
    format(nrow(expression), big.mark = ","),
    " genes x ",
    format(ncol(expression), big.mark = ","),
    " cells"
  )

  message("Writing the H5 expression sidecar...")
  dir.create(dirname(paths$sidecar), recursive = TRUE, showWarnings = FALSE)
  CerebroNexus:::.writeTENxExpression(
    methods::as(Matrix::t(expression), "CsparseMatrix"),
    paths$sidecar,
    group = "expression",
    level = 1L
  )

  message("Computing the joint embedding...")
  projection <- .viewerAllenEmbedding(expression)
  genes <- rownames(expression)
  rm(expression)

  object <- Cerebro$new()
  object$setVersion(utils::packageVersion("CerebroNexus"))
  object$addExperiment(
    "experiment_name",
    "Allen whole mouse brain MERFISH (Yao et al. 2023)"
  )
  object$addExperiment("organism", "Mus musculus")
  object$addExperiment("date_of_analysis", "2023-12-15")
  object$addExperiment("date_of_export", as.character(Sys.Date()))
  ## The 34 classes: the 338 subclasses fill a page with their legend.
  object$addParameters("main_group", "class")
  object$addParameters(
    "source",
    "Allen Brain Cell Atlas, MERFISH-C57BL6J-638850"
  )
  object$setExpressionBackend("h5", basename(paths$sidecar))
  object$setMetaData(metadata)
  for (group in c("section", .viewerAllenLevels)) {
    object$addGroup(group, levels(metadata[[group]]))
  }
  object$addTechnicalInfo("group_colors", table$colors)
  object$addProjection("umap", projection)
  ## One spatial entry per section, front to back.
  coordinates <- split(table$coordinates, metadata$section)
  for (section in levels(metadata$section)) {
    object$addSpatialData(
      section,
      .viewerAllenSpatialEntry(coordinates[[section]], genes)
    )
  }
  object$addExtraTable(
    "data_provenance",
    data.frame(
      field = c(
        "publication",
        "source",
        "cells",
        "genes",
        "sections",
        "expression",
        "embedding",
        "license"
      ),
      value = c(
        "Yao et al., Nature 624:317 (2023)",
        "Allen Brain Cell Atlas, MERFISH-C57BL6J-638850 (release 20231215)",
        as.character(nrow(metadata)),
        as.character(length(genes)),
        as.character(nlevels(metadata$section)),
        "the atlas's log2-normalised values (C57BL6J-638850-log2.h5ad)",
        "PCA and UMAP computed here",
        "CC BY-NC 4.0 (Allen Institute terms of use)"
      ),
      stringsAsFactors = FALSE
    )
  )
  saveCerebro(object, paths$crb)
  .demoStampWrite(paths$stamp, .viewerAllenRecipe)
  paths$crb
}

.viewerAllenComplete <- function(paths) {
  file.exists(paths$crb) &&
    !dir.exists(paths$crb) &&
    file.exists(paths$sidecar) &&
    !dir.exists(paths$sidecar) &&
    .demoStampCurrent(paths$stamp, .viewerAllenRecipe)
}

prepareViewerAllenDemoData <- function(cache_dir = NULL) {
  paths <- .viewerAllenPaths(cache_dir)
  if (.viewerAllenComplete(paths)) {
    return(paths$crb)
  }
  .demoRequire(
    c("data.table", "Matrix", "rhdf5", "irlba", "uwot"),
    "Allen"
  )
  manifest <- .viewerAllenFiles()
  for (name in names(manifest)) {
    message("Preparing Allen source: ", basename(paths$files[[name]]))
    .demoDownload(manifest[[name]], paths$files[[name]])
  }
  ## Build privately. A failed preparation must not delete a working data set.
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile("prepared-stage-", paths$root)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  staged <- paths
  for (name in c("crb", "sidecar", "stamp")) {
    staged[[name]] <- file.path(stage, basename(paths[[name]]))
  }
  .viewerAllenBuildObject(staged)
  if (!.viewerAllenComplete(staged)) {
    stop("Allen preparation did not finish.", call. = FALSE)
  }
  .demoPublishStage(stage, dirname(paths$crb), paths$root, "Allen")
  paths$crb
}

## What the stamp of these files records (.demoRecipe()), taken now: after the
## last function of this script.
.viewerAllenRecipe <- .demoRecipe(environment(), "Allen")
