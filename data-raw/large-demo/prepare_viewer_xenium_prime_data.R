## Optional 10x Xenium Prime 5K mouse brain demo. Uses the published cells,
## panel genes, graph clusters and UMAP, plus a four-stain composite PNG.
## No transcript points, segmentation outlines or interactive image channels.
## Source download_helpers.R first. See xenium-prime.ATTRIBUTION.md.

.viewerXeniumPrimeFiles <- function() {
  list(
    outs = list(
      url = paste0(
        "https://cf.10xgenomics.com/samples/xenium/3.0.0/",
        "Xenium_Prime_Mouse_Brain_Coronal_FF/",
        "Xenium_Prime_Mouse_Brain_Coronal_FF_outs.zip"
      ),
      size = 13315451681,
      sha256 = "d624a46a468596969b7021dc12b0fa09690b92f741e6d51642183da8e45a3729"
    )
  )
}

.viewerXeniumPrimePaths <- function(cache_dir = NULL) {
  root <- file.path(.demoLargeCacheDir(cache_dir), "xenium-prime")
  prepared <- .demoPreparedDir(root)
  list(
    root = root,
    archive = file.path(root, "source", "outs.zip"),
    crb = file.path(prepared, "cerebro_xenium_prime.crb"),
    sidecar = file.path(prepared, "cerebro_xenium_prime.h5"),
    image = file.path(prepared, "morphology.png"),
    image_info = file.path(prepared, "morphology.json"),
    stamp = file.path(prepared, "demo-version")
  )
}

.viewerXeniumPrimeExtract <- function(archive, work) {
  members <- c(
    matrix = "cell_feature_matrix.h5",
    cells = "cells.csv.gz",
    analysis = "analysis.tar.gz",
    stats::setNames(
      sprintf("morphology_focus/morphology_focus_%04d.ome.tif", 0:3),
      paste0("channel", 0:3)
    )
  )
  utils::unzip(archive, files = unname(members), exdir = work)
  files <- stats::setNames(file.path(work, members), names(members))
  if (!all(file.exists(files))) {
    stop("The Prime 5K archive lacks required demo files.", call. = FALSE)
  }
  files
}

.viewerXeniumPrimeAnalysis <- function(archive, work, cells) {
  members <- utils::untar(archive, list = TRUE)
  read_table <- function(suffix) {
    member <- members[endsWith(members, suffix)]
    if (length(member) != 1L || grepl("(^|/)\\.\\.(/|$)", member)) {
      stop("Missing or ambiguous Prime 5K analysis: ", suffix, call. = FALSE)
    }
    status <- utils::untar(archive, files = member, exdir = work)
    if (status != 0L) {
      stop("Could not extract the Prime 5K analysis.", call. = FALSE)
    }
    table <- utils::read.csv(file.path(work, member), check.names = FALSE)
    if (!"Barcode" %in% names(table) || anyDuplicated(table$Barcode)) {
      stop("Invalid Prime 5K analysis cell names.", call. = FALSE)
    }
    if (anyNA(table$Barcode) || any(!table$Barcode %in% cells)) {
      stop("Prime 5K analysis and matrix cells disagree.", call. = FALSE)
    }
    table
  }
  clusters <- read_table("clustering/gene_expression_graphclust/clusters.csv")
  projection <- read_table("umap/gene_expression_2_components/projection.csv")
  if (!"Cluster" %in% names(clusters) || ncol(projection) != 3L) {
    stop("Unexpected Prime 5K analysis columns.", call. = FALSE)
  }
  umap <- projection[, -match("Barcode", names(projection)), drop = FALSE]
  if (
    !all(vapply(umap, is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(umap)))
  ) {
    stop("The published Prime 5K UMAP must be finite.", call. = FALSE)
  }
  names(umap) <- c("Dim_1", "Dim_2")
  umap <- umap[match(cells, projection$Barcode), , drop = FALSE]
  rownames(umap) <- cells
  cluster <- clusters$Cluster[match(cells, clusters$Barcode)]
  ## XOA omits 11 cells from this data set's clustering/UMAP. Keep those cells
  ## in Spatial and expression; do not invent an embedding for them.
  cluster <- ifelse(is.na(cluster), "Unassigned", paste("Cluster", cluster))
  list(cluster = factor(cluster), umap = umap)
}

.viewerXeniumPrimeRead <- function(files, work) {
  read <- function(path) {
    rhdf5::h5read(files[["matrix"]], paste0("matrix/", path))
  }
  shape <- as.integer(read("shape"))
  counts <- methods::new(
    "dgCMatrix",
    Dim = shape,
    i = as.integer(read("indices")),
    p = as.integer(read("indptr")),
    x = as.numeric(read("data")),
    Dimnames = list(
      as.character(read("features/name")),
      as.character(read("barcodes"))
    )
  )
  keep <- as.character(read("features/feature_type")) == "Gene Expression"
  counts <- counts[keep, , drop = FALSE]
  if (!nrow(counts) || !ncol(counts) || anyDuplicated(colnames(counts))) {
    stop("The Prime 5K matrix needs genes and unique cells.", call. = FALSE)
  }
  rownames(counts) <- make.unique(rownames(counts))
  cells <- colnames(counts)
  table <- utils::read.csv(gzfile(files[["cells"]]), stringsAsFactors = FALSE)
  required <- c(
    "cell_id",
    "x_centroid",
    "y_centroid",
    "cell_area",
    "nucleus_area"
  )
  if (!all(required %in% names(table)) || anyDuplicated(table$cell_id)) {
    stop(
      "The Prime 5K cell table lacks unique cells or coordinates.",
      call. = FALSE
    )
  }
  index <- match(cells, table$cell_id)
  if (anyNA(index)) {
    stop("The Prime 5K cell table and matrix disagree.", call. = FALSE)
  }
  table <- table[index, , drop = FALSE]
  coordinates <- data.frame(
    x = table$x_centroid,
    y = -table$y_centroid,
    row.names = cells
  )
  if (any(!is.finite(as.matrix(coordinates)))) {
    stop("Prime 5K coordinates must be finite.", call. = FALSE)
  }
  analysis <- .viewerXeniumPrimeAnalysis(files[["analysis"]], work, cells)
  metadata <- data.frame(
    cell_barcode = cells,
    sample = factor("coronal"),
    cluster = analysis$cluster,
    nUMI = as.numeric(Matrix::colSums(counts)),
    nGene = as.numeric(Matrix::colSums(counts > 0)),
    cell_area = table$cell_area,
    nucleus_area = table$nucleus_area,
    row.names = cells
  )
  list(
    counts = counts,
    metadata = metadata,
    coordinates = coordinates,
    umap = analysis$umap
  )
}

.viewerXeniumPrimeBuildObject <- function(
  paths,
  work,
  python,
  image_script,
  recipe
) {
  files <- .viewerXeniumPrimeExtract(paths$archive, work)
  message("Reading the Prime 5K matrix and published analysis...")
  section <- .viewerXeniumPrimeRead(files, work)
  message("Preparing the four-stain morphology composite...")
  status <- system2(
    python,
    shQuote(c(image_script, files[["channel0"]], paths$image, paths$image_info))
  )
  if (
    status != 0L || !file.exists(paths$image) || !file.exists(paths$image_info)
  ) {
    stop("Prime 5K morphology preparation failed.", call. = FALSE)
  }
  info <- jsonlite::fromJSON(paths$image_info)
  co <- section$coordinates
  b <- info$bounds
  if (any(co$x < b$xmin | co$x > b$xmax | co$y < b$ymin | co$y > b$ymax)) {
    stop("Prime 5K cells fall outside the microscopy extent.", call. = FALSE)
  }
  expression <- .demoLogNormalize(section$counts)
  message("Writing the Prime 5K H5 expression sidecar...")
  CerebroNexus:::.writeTENxExpression(
    methods::as(Matrix::t(expression), "CsparseMatrix"),
    paths$sidecar,
    group = "expression",
    level = 1L
  )
  object <- Cerebro$new()
  object$setVersion(utils::packageVersion("CerebroNexus"))
  object$addExperiment(
    "experiment_name",
    "10x Xenium Prime 5K mouse brain (coronal, FF)"
  )
  object$addExperiment("organism", "Mus musculus")
  object$addExperiment("date_of_export", as.character(Sys.Date()))
  object$addParameters("main_group", "cluster")
  object$addParameters(
    "source",
    "10x Genomics Xenium_Prime_Mouse_Brain_Coronal_FF, XOA 3.0.0"
  )
  object$setExpressionBackend("h5", basename(paths$sidecar))
  object$setMetaData(section$metadata)
  for (group in c("sample", "cluster")) {
    object$addGroup(group, levels(section$metadata[[group]]))
  }
  object$addProjection("umap", section$umap)
  ## The composite travels in the CRB (the embedded-image contract MERFISH
  ## uses): the Viewer serves external images only from inside the app, and
  ## serves an embedded one through a private per-session URL.
  png_uri <- paste0(
    "data:image/png;base64,",
    gsub(
      "\n",
      "",
      jsonlite::base64_enc(
        readBin(paths$image, "raw", file.info(paths$image)$size)
      ),
      fixed = TRUE
    )
  )
  object$addSpatialData(
    "coronal",
    list(
      coordinates = co,
      expression = Matrix::sparseMatrix(
        i = integer(),
        j = integer(),
        x = numeric(),
        dims = dim(expression),
        dimnames = list(rownames(expression), NULL)
      ),
      histology_images = list(
        "Four-stain morphology" = list(
          histology_image = png_uri,
          histology_image_bounds = unlist(b[c("xmin", "xmax", "ymin", "ymax")])
        )
      )
    )
  )
  object$addExtraTable(
    "data_provenance",
    data.frame(
      field = c(
        "source",
        "cells",
        "genes",
        "expression",
        "embedding",
        "microscopy",
        "coordinates"
      ),
      value = c(
        .viewerXeniumPrimeFiles()$outs$url,
        as.character(ncol(expression)),
        as.character(nrow(expression)),
        "Gene Expression features only; log1p of counts per 10,000 per cell",
        "Published 10x UMAP and graph clusters; omitted cells remain Unassigned with NA UMAP",
        "Four measured stains; fixed-colour, per-channel contrast-stretched PNG",
        "Micrometres; x = x_centroid, y = -y_centroid; native microscopy extent"
      )
    )
  )
  saveCerebro(object, paths$crb)
  .demoStampWrite(paths$stamp, recipe)
  paths$crb
}

.viewerXeniumPrimeComplete <- function(paths, recipe) {
  files <- unlist(
    paths[c("crb", "sidecar", "image", "image_info")],
    use.names = FALSE
  )
  all(file.exists(files) & !dir.exists(files)) &&
    .demoStampCurrent(paths$stamp, recipe)
}

prepareViewerXeniumPrimeDemoData <- function(
  cache_dir = NULL,
  python = Sys.getenv("CEREBRO_DEMO_PYTHON", Sys.which("python3")),
  image_script = file.path(
    "data-raw",
    "large-demo",
    "prepare_xenium_prime_image.py"
  )
) {
  if (!file.exists(image_script)) {
    stop(
      "Cannot find the Prime 5K image preparation script: ",
      image_script,
      call. = FALSE
    )
  }
  recipe <- paste(.viewerXeniumPrimeRecipe, unname(tools::md5sum(image_script)))
  paths <- .viewerXeniumPrimePaths(cache_dir)
  if (.viewerXeniumPrimeComplete(paths, recipe)) {
    return(paths$crb)
  }
  .demoRequire(c("Matrix", "rhdf5", "jsonlite"), "Xenium Prime 5K")
  if (
    !nzchar(python) ||
      system2(
        python,
        c("-c", shQuote("import numpy, tifffile, imagecodecs, PIL"))
      ) !=
        0L
  ) {
    stop(
      "Prime 5K image preparation needs Python with numpy, tifffile, imagecodecs and Pillow. ",
      "Set CEREBRO_DEMO_PYTHON to that Python executable.",
      call. = FALSE
    )
  }
  message(
    "Preparing Prime 5K source (13.3 GB; only required members are unpacked)..."
  )
  .demoDownload(.viewerXeniumPrimeFiles()$outs, paths$archive)
  stage <- tempfile("prepared-stage-", paths$root)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  work <- file.path(stage, "work")
  dir.create(work)
  staged <- paths
  for (name in c("crb", "sidecar", "image", "image_info", "stamp")) {
    staged[[name]] <- file.path(stage, basename(paths[[name]]))
  }
  .viewerXeniumPrimeBuildObject(staged, work, python, image_script, recipe)
  unlink(work, recursive = TRUE)
  if (!.viewerXeniumPrimeComplete(staged, recipe)) {
    stop("Prime 5K preparation did not finish.", call. = FALSE)
  }
  .demoPublishStage(stage, dirname(paths$crb), paths$root, "Xenium Prime 5K")
  paths$crb
}

.viewerXeniumPrimeRecipe <- .demoRecipe(environment(), "XeniumPrime")
