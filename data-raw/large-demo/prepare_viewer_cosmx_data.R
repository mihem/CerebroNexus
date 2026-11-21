## NanoString CosMx SMI non-small-cell lung cancer (NSCLC) FFPE data set
## (He et al. 2022, Nature Biotechnology 40:1794): 8 sections of 5 tissues, 960
## genes, one flat-file archive per section. See cosmx.ATTRIBUTION.md.

.viewerCosMxBaseUrl <- paste0(
  "https://nanostring-public-share.s3.us-west-2.amazonaws.com/",
  "SMI-Compressed/"
)

## Size and MD5 of each archive are the ones NanoString publishes in
## `SMI-Compressed/md5sums.txt` and the S3 object listing.
.viewerCosMxSamples <- function() {
  data.frame(
    sample = c(
      "Lung5_Rep1",
      "Lung5_Rep2",
      "Lung5_Rep3",
      "Lung6",
      "Lung9_Rep1",
      "Lung9_Rep2",
      "Lung12",
      "Lung13"
    ),
    size = c(
      1574011761,
      1631848310,
      1410950200,
      1523509181,
      1078783664,
      1769589563,
      1179692183,
      1143788384
    ),
    md5 = c(
      "58d02b183fb2446c8c6f9b5c15ba1321",
      "06c17753c2f7a545beafd8e777ba756a",
      "26676775c1ca5970bfe522e3f8ed136a",
      "730341a340f5978ce09beb0e36b1ae6d",
      "0c9ff318a6f72625e40381cc0d10504c",
      "016924767fb4d73e117d6a43aa45ec7f",
      "3e2fe4ffeb60fc2ad0a8bf49c6a9f093",
      "123aacbc67d0709a235b324b6642c129"
    ),
    stringsAsFactors = FALSE
  )
}

.viewerCosMxPaths <- function(cache_dir = NULL, samples = NULL) {
  if (!exists(".demoLargeCacheDir", mode = "function")) {
    stop(
      "Source data-raw/large-demo/download_helpers.R before building the demo data.",
      call. = FALSE
    )
  }
  if (is.null(samples)) {
    samples <- .viewerCosMxSamples()$sample
  }
  root <- file.path(.demoLargeCacheDir(cache_dir), "cosmx")
  prepared <- .demoPreparedDir(root)
  list(
    root = root,
    source_dir = file.path(root, "source"),
    archives = stats::setNames(
      file.path(root, "source", paste0(samples, "_SMI_Flat_data.tar.gz")),
      samples
    ),
    crb = file.path(prepared, "cerebro_cosmx_nsclc.crb"),
    sidecar = file.path(prepared, "cerebro_cosmx_nsclc.h5"),
    stamp = file.path(prepared, "demo-version")
  )
}

.viewerCosMxFiles <- function(samples = .viewerCosMxSamples()) {
  files <- lapply(seq_len(nrow(samples)), function(index) {
    name <- samples$sample[[index]]
    list(
      url = paste0(
        .viewerCosMxBaseUrl,
        name,
        "/",
        name,
        "%20SMI%20Flat%20data.tar.gz"
      ),
      size = samples$size[[index]],
      md5 = samples$md5[[index]]
    )
  })
  stats::setNames(files, samples$sample)
}

## The three CSV members of a section archive that the Viewer needs; the
## archive also holds images and a multi-gigabyte transcript table.
.viewerCosMxMembers <- function(sample) {
  directory <- paste0(sample, "/", sample, "-Flat_files_and_images/")
  stats::setNames(
    paste0(directory, sample, c("_exprMat_file", "_metadata_file"), ".csv"),
    c("expression", "metadata")
  )
}

.viewerCosMxExtract <- function(archive, sample, exdir) {
  members <- .viewerCosMxMembers(sample)
  status <- suppressWarnings(
    utils::untar(archive, files = unname(members), exdir = exdir)
  )
  extracted <- file.path(exdir, members)
  if (!identical(as.integer(status), 0L) || !all(file.exists(extracted))) {
    stop(
      "The CosMx archive for ",
      sample,
      " does not contain the expected flat files.",
      call. = FALSE
    )
  }
  stats::setNames(extracted, names(members))
}

.viewerCosMxStains <- c(
  mean_membrane = "Mean.MembraneStain",
  mean_panck = "Mean.PanCK",
  mean_cd45 = "Mean.CD45",
  mean_cd3 = "Mean.CD3",
  mean_dapi = "Mean.DAPI"
)

## Reads one section into a raw-count genes x cells matrix and a cell table.
## Rows of the expression file with `cell_ID` 0 hold the transcripts that were
## not assigned to a cell; they are dropped. Negative-control probes are not
## genes: their total per cell goes into the cell table.
.viewerCosMxReadSample <- function(files, sample) {
  expression <- data.table::fread(
    files[["expression"]],
    showProgress = FALSE,
    data.table = FALSE
  )
  metadata <- data.table::fread(
    files[["metadata"]],
    showProgress = FALSE,
    data.table = FALSE
  )
  required <- c(
    "fov",
    "cell_ID",
    "Area",
    "AspectRatio",
    "Width",
    "Height",
    "CenterX_global_px",
    "CenterY_global_px",
    unname(.viewerCosMxStains)
  )
  missing <- setdiff(required, names(metadata))
  if (length(missing) || !all(c("fov", "cell_ID") %in% names(expression))) {
    stop(
      "The CosMx flat files of ",
      sample,
      " lack columns: ",
      paste(
        c(missing, setdiff(c("fov", "cell_ID"), names(expression))),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  expression <- expression[expression$cell_ID != 0L, , drop = FALSE]
  key <- function(table) paste(table$fov, table$cell_ID, sep = "_")
  row <- match(key(metadata), key(expression))
  if (anyNA(row) || anyDuplicated(key(metadata))) {
    stop(
      "The CosMx metadata and expression files of ",
      sample,
      " describe different cells.",
      call. = FALSE
    )
  }
  expression <- expression[row, , drop = FALSE]

  features <- setdiff(names(expression), c("fov", "cell_ID"))
  negative <- grepl("^NegPrb", features)
  counts <- as.matrix(expression[, features[!negative], drop = FALSE])
  barcodes <- paste(sample, metadata$fov, metadata$cell_ID, sep = "_")
  neg_probe_counts <- if (any(negative)) {
    rowSums(as.matrix(expression[, features[negative], drop = FALSE]))
  } else {
    rep(0, nrow(counts))
  }
  counts <- Matrix::Matrix(t(counts), sparse = TRUE)
  counts <- methods::as(methods::as(counts, "generalMatrix"), "CsparseMatrix")
  colnames(counts) <- barcodes

  cells <- data.frame(
    cell_barcode = barcodes,
    sample = sample,
    tissue = sub("_Rep[0-9]+$", "", sample),
    fov = sprintf("%s_F%02d", sample, as.integer(metadata$fov)),
    nUMI = as.integer(Matrix::colSums(counts)),
    nGene = as.integer(Matrix::colSums(counts > 0)),
    neg_probe_counts = as.integer(neg_probe_counts),
    area = metadata$Area,
    aspect_ratio = metadata$AspectRatio,
    width = metadata$Width,
    height = metadata$Height,
    row.names = barcodes,
    stringsAsFactors = FALSE
  )
  for (name in names(.viewerCosMxStains)) {
    cells[[name]] <- metadata[[.viewerCosMxStains[[name]]]]
  }
  ## The slide's y axis grows downwards (image rows); the Viewer's grows
  ## upwards, so y is mirrored to keep the section upright.
  coordinates <- data.frame(
    x = metadata$CenterX_global_px,
    y = -metadata$CenterY_global_px,
    row.names = barcodes
  )
  list(counts = counts, cells = cells, coordinates = coordinates)
}

## Joint embedding of all sections: PCA on the log-normalised genes, then UMAP.
## NanoString publishes no embedding with the flat files, so it is computed
## here and labelled as such.
.viewerCosMxEmbedding <- function(
  expression,
  n_pcs = 30L,
  threads = max(
    1L,
    min(8L, parallel::detectCores(logical = FALSE)),
    na.rm = TRUE
  ),
  seed = 20211015L
) {
  .demoRequire(c("irlba", "uwot"), "CosMx")
  x <- Matrix::t(expression)
  centre <- Matrix::colMeans(x)
  spread <- sqrt(pmax(Matrix::colMeans(x^2) - centre^2, 1e-12))
  set.seed(seed)
  pca <- irlba::irlba(x, nv = n_pcs, center = centre, scale = spread)
  scores <- pca$u %*% diag(pca$d, nrow = length(pca$d))
  umap <- uwot::umap(
    scores,
    n_neighbors = 30L,
    min_dist = 0.3,
    n_threads = threads,
    seed = seed
  )
  data.frame(
    Dim_1 = umap[, 1L],
    Dim_2 = umap[, 2L],
    row.names = rownames(x)
  )
}

## A spatial entry of the Viewer: the section's coordinates and an expression
## matrix. The Viewer reads expression from the H5 sidecar, so the entry's own
## matrix is empty. It keeps the genes as row names and has no column names, so
## the cell names, which the coordinates already hold, are not stored twice.
.viewerCosMxSpatialEntry <- function(coordinates, genes) {
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

.viewerCosMxBuildObject <- function(
  paths,
  work,
  samples = .viewerCosMxSamples()
) {
  sections <- vector("list", nrow(samples))
  names(sections) <- samples$sample
  for (index in seq_len(nrow(samples))) {
    sample <- samples$sample[[index]]
    message(sprintf(
      "Reading CosMx section %d of %d: %s",
      index,
      nrow(samples),
      sample
    ))
    files <- .viewerCosMxExtract(
      paths$archives[[sample]],
      sample,
      file.path(work, sample)
    )
    section <- .viewerCosMxReadSample(files, sample)
    unlink(file.path(work, sample), recursive = TRUE)
    section$counts <- .demoLogNormalize(section$counts)
    sections[[index]] <- section
  }
  expression <- do.call(cbind, lapply(sections, `[[`, "counts"))
  metadata <- do.call(rbind, lapply(sections, `[[`, "cells"))
  rownames(metadata) <- metadata$cell_barcode
  if (!identical(colnames(expression), metadata$cell_barcode)) {
    stop("The CosMx expression and cell tables disagree.", call. = FALSE)
  }
  for (column in c("sample", "tissue", "fov")) {
    metadata[[column]] <- factor(metadata[[column]])
  }
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
  projection <- .viewerCosMxEmbedding(expression)
  projection <- projection[metadata$cell_barcode, , drop = FALSE]

  object <- Cerebro$new()
  object$setVersion(utils::packageVersion("CerebroNexus"))
  object$addExperiment(
    "experiment_name",
    "CosMx SMI NSCLC FFPE (He et al. 2022)"
  )
  object$addExperiment("organism", "Homo sapiens")
  object$addExperiment("date_of_analysis", "2021-10-15")
  object$addExperiment("date_of_export", as.character(Sys.Date()))
  object$addParameters("main_group", "sample")
  object$addParameters("source", "NanoString CosMx SMI NSCLC FFPE dataset")
  object$setExpressionBackend("h5", basename(paths$sidecar))
  object$setMetaData(metadata)
  for (group in c("sample", "tissue", "fov")) {
    object$addGroup(group, sort(levels(metadata[[group]]), method = "radix"))
  }
  object$addProjection("umap", projection)
  ## One spatial entry per section.
  for (sample in names(sections)) {
    object$addSpatialData(
      sample,
      .viewerCosMxSpatialEntry(
        sections[[sample]]$coordinates,
        rownames(expression)
      )
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
        "He et al., Nature Biotechnology 40:1794 (2022)",
        "NanoString CosMx SMI NSCLC FFPE dataset (flat files)",
        as.character(nrow(metadata)),
        as.character(nrow(expression)),
        as.character(nrow(samples)),
        "log1p of counts per 10,000 per cell; negative-control probes excluded",
        "PCA and UMAP computed here",
        "none stated on the data set page"
      ),
      stringsAsFactors = FALSE
    )
  )
  saveCerebro(object, paths$crb)
  .demoStampWrite(paths$stamp, .viewerCosMxRecipe)
  paths$crb
}

.viewerCosMxComplete <- function(paths) {
  file.exists(paths$crb) &&
    !dir.exists(paths$crb) &&
    file.exists(paths$sidecar) &&
    !dir.exists(paths$sidecar) &&
    .demoStampCurrent(paths$stamp, .viewerCosMxRecipe)
}

prepareViewerCosMxDemoData <- function(cache_dir = NULL) {
  paths <- .viewerCosMxPaths(cache_dir)
  if (.viewerCosMxComplete(paths)) {
    return(paths$crb)
  }
  .demoRequire(
    c("data.table", "Matrix", "rhdf5", "irlba", "uwot"),
    "CosMx"
  )
  manifest <- .viewerCosMxFiles()
  for (name in names(manifest)) {
    message("Preparing CosMx source: ", basename(paths$archives[[name]]))
    .demoDownload(manifest[[name]], paths$archives[[name]])
  }
  ## Build privately. A failed preparation must not delete a working data set.
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(
    "prepared-stage-",
    paths$root
  )
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  work <- file.path(stage, "work")
  dir.create(work)
  staged <- paths
  for (name in c("crb", "sidecar", "stamp")) {
    staged[[name]] <- file.path(stage, basename(paths[[name]]))
  }
  .viewerCosMxBuildObject(staged, work)
  unlink(work, recursive = TRUE)
  if (!.viewerCosMxComplete(staged)) {
    stop("CosMx preparation did not finish.", call. = FALSE)
  }
  .demoPublishStage(stage, dirname(paths$crb), paths$root, "CosMx")
  paths$crb
}

## What the stamp of these files records (.demoRecipe()), taken now: after the
## last function of this script.
.viewerCosMxRecipe <- .demoRecipe(environment(), "CosMx")
