## Kukanja et al. 2024 (Cell 187:1990): the in situ sequencing (ISS) data of
## mouse spinal cord sections in experimental autoimmune encephalomyelitis
## (`eae`, 52 sections, 239 genes) and the Xenium data of human multiple
## sclerosis spinal cord (`ms`, 6 sections, 266 genes), each one AnnData file
## of the authors with their cell types and UMAP. See kukanja.ATTRIBUTION.md.

.viewerKukanjaBaseUrl <- "https://zenodo.org/records/8037425/files/"

## What each data set takes from its h5ad. `groups` and `numbers` map the
## Viewer's column names to the authors' obs columns; size and MD5 are the ones
## Zenodo publishes for the archive.
.viewerKukanjaSets <- function() {
  list(
    eae = list(
      archive = "EAE_rev_ingest_only_new_v4_with_images_tmap.h5ad.zip",
      size = 567853835,
      md5 = "2df533e6be0347839ffc074039242311",
      name = "cerebro_kukanja_eae",
      experiment = "Kukanja et al. 2024, mouse EAE spinal cord (ISS)",
      organism = "Mus musculus",
      technology = "in situ sequencing (ISS)",
      cells = "_index",
      groups = c(
        section = "sample_id",
        condition = "condition",
        time = "time",
        region = "region",
        sex = "SEX",
        compartment = "compartment",
        cell_type_broad = "Annotation 1 (broad)",
        cell_type = "Annotation 3 (medium with DA)",
        cell_type_fine = "Annotation 4 (high)",
        disease_associated = "DA"
      ),
      numbers = c(
        area = "area",
        distance_to_boundary_um = "distance_to_boundary_um"
      )
    ),
    ms = list(
      archive = "MS_xenium_data_v5_with_images_tmap.h5ad.zip",
      size = 511633139,
      md5 = "da79033e5a4113184d471debe143e09f",
      name = "cerebro_kukanja_ms",
      experiment = "Kukanja et al. 2024, human MS spinal cord (Xenium)",
      organism = "Homo sapiens",
      technology = "Xenium",
      cells = "cell_id",
      groups = c(
        section = "sample_id",
        condition = "type_spec",
        sex = "sex",
        compartment = "compartment_2",
        cell_type_broad = "Level1",
        cell_type = "Level2",
        cell_type_fine = "Level3"
      ),
      numbers = c(cell_area = "cell_area", nucleus_area = "nucleus_area")
    )
  )
}

.viewerKukanjaPaths <- function(set, cache_dir = NULL) {
  if (!exists(".demoLargeCacheDir", mode = "function")) {
    stop(
      "Source data-raw/large-demo/download_helpers.R before building the demo data.",
      call. = FALSE
    )
  }
  spec <- .viewerKukanjaSets()[[set]]
  root <- file.path(.demoLargeCacheDir(cache_dir), set)
  prepared <- .demoPreparedDir(root)
  list(
    root = root,
    source_dir = file.path(root, "source"),
    archive = file.path(root, "source", spec$archive),
    crb = file.path(prepared, paste0(spec$name, ".crb")),
    sidecar = file.path(prepared, paste0(spec$name, ".h5")),
    stamp = file.path(prepared, "demo-version")
  )
}

## The h5ad inside the archive, unpacked into `exdir`.
.viewerKukanjaExtract <- function(archive, exdir) {
  member <- sub("[.]zip$", "", basename(archive))
  utils::unzip(archive, files = member, exdir = exdir)
  path <- file.path(exdir, member)
  if (!file.exists(path)) {
    stop(
      "The archive ",
      basename(archive),
      " holds no ",
      member,
      ".",
      call. = FALSE
    )
  }
  path
}

## An obs column: a categorical one as its labels (code -1 is missing), any
## other as it is.
.viewerKukanjaObs <- function(h5ad, column) {
  path <- paste0("obs/", column)
  file <- rhdf5::H5Fopen(h5ad, flags = "H5F_ACC_RDONLY")
  type <- NULL
  if (rhdf5::H5Lexists(file, path)) {
    object <- rhdf5::H5Oopen(file, path)
    type <- as.character(rhdf5::H5Iget_type(object))
    rhdf5::H5Oclose(object)
  }
  rhdf5::H5Fclose(file)
  if (is.null(type)) {
    stop("The h5ad has no obs column ", column, ".", call. = FALSE)
  }
  if (identical(type, "H5I_GROUP")) {
    codes <- as.vector(rhdf5::h5read(h5ad, paste0(path, "/codes")))
    categories <- as.character(as.vector(
      rhdf5::h5read(h5ad, paste0(path, "/categories"))
    ))
    values <- categories[ifelse(codes < 0L, NA_integer_, codes + 1L)]
    return(factor(values, levels = categories[categories %in% values]))
  }
  as.vector(rhdf5::h5read(h5ad, path))
}

## The colours scanpy keeps for a categorical column (uns/<column>_colors, in
## the order of its categories), as "#RRGGBB" named by level. Columns without
## them, or with values that are not colours, have none.
.viewerKukanjaColors <- function(h5ad, column) {
  path <- paste0("uns/", column, "_colors")
  colors <- tryCatch(
    as.character(as.vector(rhdf5::h5read(h5ad, path))),
    error = function(error) NULL
  )
  categories <- tryCatch(
    as.character(as.vector(
      rhdf5::h5read(h5ad, paste0("obs/", column, "/categories"))
    )),
    error = function(error) NULL
  )
  if (is.null(colors) || length(colors) != length(categories)) {
    return(NULL)
  }
  colors <- toupper(sub("^(#[0-9A-Fa-f]{6})[0-9A-Fa-f]{2}$", "\\1", colors))
  if (!all(grepl("^#[0-9A-F]{6}$", colors))) {
    return(NULL)
  }
  stats::setNames(colors, categories)
}

## The counts, genes x cells, from X stored by gene (CSC) or by cell (CSR).
.viewerKukanjaCounts <- function(h5ad) {
  attributes <- rhdf5::h5readAttributes(h5ad, "X")
  encoding <- as.character(attributes[["encoding-type"]])
  shape <- as.integer(attributes[["shape"]])
  if (!encoding %in% c("csc_matrix", "csr_matrix") || length(shape) != 2L) {
    stop("The h5ad's X must be a sparse matrix.", call. = FALSE)
  }
  read <- function(name) {
    values <- rhdf5::h5read(h5ad, paste0("X/", name))
    dim(values) <- NULL
    values
  }
  ## A CSC of cells x genes is a dgCMatrix as it is; a CSR one is the dgCMatrix
  ## of genes x cells.
  csc <- identical(encoding, "csc_matrix")
  counts <- methods::new(
    "dgCMatrix",
    i = as.integer(read("indices")),
    p = as.integer(read("indptr")),
    x = as.numeric(read("data")),
    Dim = if (csc) shape else rev(shape)
  )
  if (csc) {
    counts <- methods::as(Matrix::t(counts), "CsparseMatrix")
  }
  genes <- as.character(as.vector(rhdf5::h5read(
    h5ad,
    paste0("var/", rhdf5::h5readAttributes(h5ad, "var")[["_index"]])
  )))
  if (length(genes) != nrow(counts) || anyDuplicated(genes)) {
    stop("The h5ad needs unique gene names.", call. = FALSE)
  }
  rownames(counts) <- genes
  counts
}

## A two-column obsm matrix (rhdf5 returns it as 2 x cells).
.viewerKukanjaObsm <- function(h5ad, name) {
  values <- rhdf5::h5read(h5ad, paste0("obsm/", name))
  if (length(dim(values)) != 2L || nrow(values) < 2L) {
    stop("The h5ad's obsm/", name, " is not a cell matrix.", call. = FALSE)
  }
  t(values[1:2, , drop = FALSE])
}

## Reads one h5ad: the counts, the cell table with the groups and their
## colours, the section coordinates and the authors' UMAP.
.viewerKukanjaRead <- function(h5ad, spec) {
  counts <- .viewerKukanjaCounts(h5ad)
  cells <- as.character(.viewerKukanjaObs(h5ad, spec$cells))
  if (length(cells) != ncol(counts) || anyNA(cells) || anyDuplicated(cells)) {
    stop("The h5ad needs a unique name for every cell.", call. = FALSE)
  }
  colnames(counts) <- cells
  metadata <- data.frame(cell_barcode = cells, row.names = cells)
  colors <- list()
  for (group in names(spec$groups)) {
    column <- spec$groups[[group]]
    values <- .viewerKukanjaObs(h5ad, column)
    if (!is.factor(values)) {
      stop("The obs column ", column, " is not categorical.", call. = FALSE)
    }
    if (anyNA(values)) {
      levels(values) <- c(levels(values), "N/A")
      values[is.na(values)] <- "N/A"
    }
    metadata[[group]] <- values
    palette <- .viewerKukanjaColors(h5ad, column)
    if (!is.null(palette)) {
      colors[[group]] <- palette[intersect(names(palette), levels(values))]
    }
  }
  metadata$nUMI <- as.integer(round(Matrix::colSums(counts)))
  metadata$nGene <- as.integer(diff(counts@p))
  for (name in names(spec$numbers)) {
    metadata[[name]] <- as.numeric(.viewerKukanjaObs(
      h5ad,
      spec$numbers[[name]]
    ))
  }
  ## The authors' aligned coordinates: every section turned the same way. The
  ## image's y axis grows downwards, the Viewer's upwards, so y is mirrored.
  xy <- .viewerKukanjaObsm(h5ad, "xy_loc")
  coordinates <- data.frame(x = xy[, 1L], y = -xy[, 2L], row.names = cells)
  umap <- .viewerKukanjaObsm(h5ad, "X_umap")
  projection <- data.frame(
    Dim_1 = umap[, 1L],
    Dim_2 = umap[, 2L],
    row.names = cells
  )
  list(
    counts = counts,
    metadata = metadata,
    colors = colors,
    coordinates = coordinates,
    projection = projection
  )
}

.viewerKukanjaSpatialEntry <- function(coordinates, genes) {
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

.viewerKukanjaBuildObject <- function(paths, work, spec) {
  message("Unpacking ", basename(paths$archive), "...")
  h5ad <- .viewerKukanjaExtract(paths$archive, work)
  message("Reading the h5ad...")
  data <- .viewerKukanjaRead(h5ad, spec)
  unlink(h5ad)
  metadata <- data$metadata
  expression <- .demoLogNormalize(data$counts)
  message(
    "Expression: ",
    format(nrow(expression), big.mark = ","),
    " genes x ",
    format(ncol(expression), big.mark = ","),
    " cells"
  )
  dir.create(dirname(paths$sidecar), recursive = TRUE, showWarnings = FALSE)
  CerebroNexus:::.writeTENxExpression(
    methods::as(Matrix::t(expression), "CsparseMatrix"),
    paths$sidecar,
    group = "expression",
    level = 1L
  )

  object <- Cerebro$new()
  object$setVersion(utils::packageVersion("CerebroNexus"))
  object$addExperiment("experiment_name", spec$experiment)
  object$addExperiment("organism", spec$organism)
  object$addExperiment("date_of_analysis", "2024-03-12")
  object$addExperiment("date_of_export", as.character(Sys.Date()))
  object$addParameters("main_group", "cell_type")
  object$addParameters("source", "Zenodo record 8037425")
  object$setExpressionBackend("h5", basename(paths$sidecar))
  object$setMetaData(metadata)
  for (group in names(spec$groups)) {
    object$addGroup(group, levels(metadata[[group]]))
  }
  if (length(data$colors)) {
    object$addTechnicalInfo("group_colors", data$colors)
  }
  object$addProjection("umap", data$projection)
  coordinates <- split(data$coordinates, metadata$section)
  for (section in levels(metadata$section)) {
    object$addSpatialData(
      section,
      .viewerKukanjaSpatialEntry(coordinates[[section]], rownames(expression))
    )
  }
  object$addExtraTable(
    "data_provenance",
    data.frame(
      field = c(
        "publication",
        "source",
        "technology",
        "cells",
        "genes",
        "sections",
        "expression",
        "embedding",
        "license"
      ),
      value = c(
        "Kukanja et al., Cell 187:1990 (2024)",
        paste0("Zenodo record 8037425, ", spec$archive),
        spec$technology,
        as.character(nrow(metadata)),
        as.character(nrow(expression)),
        as.character(nlevels(metadata$section)),
        "log1p of counts per 10,000 per cell, from the authors' counts (X)",
        "the authors' UMAP (obsm X_umap)",
        "CC BY 4.0"
      ),
      stringsAsFactors = FALSE
    )
  )
  saveCerebro(object, paths$crb)
  .demoStampWrite(paths$stamp, .viewerKukanjaRecipe)
  paths$crb
}

.viewerKukanjaComplete <- function(paths) {
  file.exists(paths$crb) &&
    !dir.exists(paths$crb) &&
    file.exists(paths$sidecar) &&
    !dir.exists(paths$sidecar) &&
    .demoStampCurrent(paths$stamp, .viewerKukanjaRecipe)
}

## `set`: "eae" (mouse, ISS) or "ms" (human, Xenium).
prepareViewerKukanjaDemoData <- function(
  set = c("eae", "ms"),
  cache_dir = NULL
) {
  set <- match.arg(set)
  spec <- .viewerKukanjaSets()[[set]]
  paths <- .viewerKukanjaPaths(set, cache_dir)
  if (.viewerKukanjaComplete(paths)) {
    return(paths$crb)
  }
  .demoRequire(c("Matrix", "rhdf5"), "Kukanja")
  message("Preparing Kukanja source: ", spec$archive)
  .demoDownload(
    list(
      url = paste0(.viewerKukanjaBaseUrl, spec$archive, "?download=1"),
      size = spec$size,
      md5 = spec$md5
    ),
    paths$archive
  )
  ## Build privately. A failed preparation must not delete a working data set.
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile("prepared-stage-", paths$root)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  work <- file.path(stage, "work")
  dir.create(work)
  staged <- paths
  for (name in c("crb", "sidecar", "stamp")) {
    staged[[name]] <- file.path(stage, basename(paths[[name]]))
  }
  .viewerKukanjaBuildObject(staged, work, spec)
  unlink(work, recursive = TRUE)
  if (!.viewerKukanjaComplete(staged)) {
    stop("Kukanja preparation did not finish.", call. = FALSE)
  }
  .demoPublishStage(stage, dirname(paths$crb), paths$root, "Kukanja")
  paths$crb
}

## What the stamp of these files records (.demoRecipe()), taken now: after the
## last function of this script.
.viewerKukanjaRecipe <- .demoRecipe(environment(), "Kukanja")
