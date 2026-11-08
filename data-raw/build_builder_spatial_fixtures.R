#!/usr/bin/env Rscript
# Offline synthetic Trekker input for Builder, distinct from the real Viewer Trekker demo.
# Rscript data-raw/build_builder_spatial_fixtures.R [--fixture all|trekker] [--output DIR]
# The Spatial alignment example is real data: data-raw/build_builder_visium_fixture.R.

builder_fixture_stabilize <- function(object) {
  for (name in names(object@commands)) {
    object@commands[[name]]@time.stamp <- as.POSIXct(
      "2026-10-13 00:00:00",
      tz = "UTC"
    )
  }
  object
}

builder_fixture_markers <- function() {
  list(
    endothelial = c(
      "ADGRL4",
      "PECAM1",
      "VWF",
      "KDR",
      "ESAM",
      "CLDN5",
      "EMCN",
      "FLT1"
    ),
    pericyte = c(
      "PDGFRB",
      "RGS5",
      "CSPG4",
      "MCAM",
      "DES",
      "ACTA2",
      "COL4A1",
      "COL4A2"
    ),
    fibroblast = c(
      "COL1A1",
      "COL1A2",
      "DCN",
      "LUM",
      "COL3A1",
      "PDGFRA",
      "SPARC",
      "COL6A1"
    ),
    myeloid = c(
      "AIF1",
      "TYROBP",
      "CTSS",
      "LST1",
      "FCER1G",
      "CSF1R",
      "CTSD",
      "C1QA"
    ),
    astrocyte = c(
      "GFAP",
      "AQP4",
      "ALDH1L1",
      "SLC1A3",
      "SLC1A2",
      "SOX9",
      "APOE",
      "GJA1"
    ),
    neuron = c(
      "RBFOX3",
      "SYT1",
      "SNAP25",
      "TUBB3",
      "NEFL",
      "STMN2",
      "SLC17A7",
      "GAD1"
    ),
    oligodendrocyte = c(
      "MBP",
      "PLP1",
      "MOG",
      "MAG",
      "OLIG1",
      "OLIG2",
      "CLDN11",
      "CNP"
    ),
    cycling = c(
      "MKI67",
      "TOP2A",
      "PCNA",
      "TYMS",
      "UBE2C",
      "CCNB1",
      "CDK1",
      "BIRC5"
    )
  )
}

builder_fixture_tissue_image <- function(file, index) {
  set.seed(20261013 + index)
  # A mathematical H&E-like illustration, not a measured histology image.
  width <- 1064L
  height <- 728L
  x <- matrix(rep(seq(-1, 1, length.out = width), each = height), height)
  y <- matrix(rep(seq(-1, 1, length.out = height), width), height)
  tissue <- pmin(
    ((x - .32)^2 / .52^2 + y^2 / .8^2),
    ((x + .32)^2 / .52^2 + y^2 / .8^2)
  ) <
    1
  holes <- ((x + .3)^2 / .13^2 + (y + .12)^2 / .11^2 < 1) |
    ((x - .28)^2 / .11^2 + (y - .22)^2 / .1^2 < 1)
  tissue <- tissue & !holes
  noise <- matrix(stats::runif(width * height, -.035, .035), height)
  texture <- sin(x * (18 + index)) * cos(y * (12 + index)) * .04 + noise
  nuclei <- matrix(FALSE, height, width)
  for (i in seq_len(1100)) {
    cx <- sample.int(width - 8L, 1L) + 4L
    cy <- sample.int(height - 8L, 1L) + 4L
    for (dy in -2:2) {
      for (dx in -2:2) {
        if (dx^2 + dy^2 <= 5) nuclei[cy + dy, cx + dx] <- TRUE
      }
    }
  }
  nuclei <- nuclei & tissue
  rgb <- array(0, c(height, width, 3L))
  for (channel in 1:3) {
    plane <- ifelse(
      tissue,
      c(.84, .55, .72)[channel] + texture,
      .98 + noise / 3
    )
    plane[nuclei] <- c(.37, .20, .43)[channel]
    plane[680:684, 394:670] <- .55
    rgb[,, channel] <- pmax(0, pmin(1, plane))
  }
  jpeg::writeJPEG(rgb, file, quality = .9)
}

builder_fixture_trekker <- function(output) {
  previous_plan <- future::plan(future::sequential)
  on.exit(future::plan(previous_plan), add = TRUE)
  set.seed(20260903)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  markers <- builder_fixture_markers()
  genes <- c(
    unname(unlist(markers)),
    paste0("MT-", c("CO1", "CO2", "CO3", "ND1", "ND2", "ND4", "ATP6", "CYB")),
    c("GAPDH", "ACTB", "B2M", "RPLP0", "RPS18", "EEF1A1", "TUBA1A", "MALAT1"),
    paste0("TISSUE", rep(1:4, each = 2), c("A", "B")),
    sprintf("SYN%04d", 1:112)
  )
  cells <- sprintf("SYNTH-%05d", 1:5020)
  tissue <- factor(
    c(rep(paste0("T", 1:4), each = 1250), rep("outlier", 20)),
    levels = c(paste0("T", 1:4), "outlier")
  )
  types <- factor(
    sample(
      names(markers),
      length(cells),
      replace = TRUE,
      prob = c(15, 11, 14, 14, 14, 12, 14, 6)
    ),
    levels = names(markers)
  )
  centers <- rbind(c(2700, 8000), c(7900, 8000), c(2550, 2700), c(8000, 2700))
  xy <- do.call(
    rbind,
    lapply(1:4, function(i) {
      angle <- stats::runif(1250, 0, 2 * pi)
      radius <- sqrt(stats::runif(1250))
      cbind(
        centers[i, 1] + 1050 * radius * cos(angle),
        centers[i, 2] + 750 * radius * sin(angle)
      )
    })
  )
  xy <- rbind(
    xy,
    cbind(stats::runif(20, 100, 10300), stats::runif(20, 100, 10300))
  )
  rownames(xy) <- cells
  lambda <- matrix(
    .04,
    length(genes),
    length(cells),
    dimnames = list(genes, cells)
  )
  lambda[65:80, ] <- .35
  for (type in names(markers)) {
    lambda[markers[[type]], types == type] <- 3
  }
  for (i in 1:4) {
    lambda[paste0("TISSUE", i, c("A", "B")), tissue == paste0("T", i)] <- 1.5
  }
  counts <- Matrix::Matrix(
    matrix(
      stats::rpois(length(lambda), lambda),
      nrow(lambda),
      dimnames = dimnames(lambda)
    ),
    sparse = TRUE
  )
  object <- SeuratObject::CreateSeuratObject(
    counts,
    project = "Synthetic_Trekker_Demo"
  )
  object$cell_type <- types
  object$tissue_id <- tissue
  object$percent.mt <- Seurat::PercentageFeatureSet(object, pattern = "^MT-")
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object <- Seurat::FindVariableFeatures(
    object,
    nfeatures = 150,
    verbose = FALSE
  )
  object <- Seurat::ScaleData(object, verbose = FALSE)
  object <- Seurat::RunPCA(
    object,
    npcs = 20,
    seed.use = 20260903,
    verbose = FALSE
  )
  object <- Seurat::RunUMAP(
    object,
    dims = 1:15,
    seed.use = 20260903,
    umap.method = "uwot",
    verbose = FALSE
  )
  object <- Seurat::FindNeighbors(object, dims = 1:15, verbose = FALSE)
  object <- Seurat::FindClusters(
    object,
    resolution = .35,
    random.seed = 20260903,
    verbose = FALSE
  )
  colnames(xy) <- c("SPATIAL_1", "SPATIAL_2")
  object[["SPATIAL"]] <- SeuratObject::CreateDimReducObject(
    xy,
    key = "SPATIAL_",
    assay = "RNA"
  )
  # Preserve the gallery's SlideSeq orientation: x=-SPATIAL_2, y=SPATIAL_1.
  object[["slice1"]] <- methods::new(
    "SlideSeq",
    coordinates = data.frame(
      x = -xy[, 2],
      y = xy[, 1],
      cells = cells,
      row.names = cells
    ),
    assay = "RNA",
    key = "slice1_"
  )
  object <- Seurat::DietSeurat(
    object,
    layers = "counts",
    dimreducs = c("pca", "umap", "SPATIAL"),
    graphs = NULL
  )
  qs2::qs_save(
    builder_fixture_stabilize(object),
    file.path(output, "trekker_4_tissues.qs2")
  )
  for (i in 1:4) {
    builder_fixture_tissue_image(
      file.path(output, sprintf("tissue_%02d_he.jpg", i)),
      i
    )
  }
  invisible(object)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  fixture <- "all"
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]
  )
  output <- file.path(
    dirname(normalizePath(script)),
    "..",
    "inst",
    "builder",
    "fixtures"
  )
  while (length(args)) {
    if (length(args) < 2L || !args[[1L]] %in% c("--fixture", "--output")) {
      stop("Usage: --fixture all|trekker --output DIR", call. = FALSE)
    }
    if (args[[1L]] == "--fixture") {
      fixture <- args[[2L]]
    } else {
      output <- args[[2L]]
    }
    args <- args[-c(1L, 2L)]
  }
  if (!fixture %in% c("all", "trekker")) {
    stop("Unknown fixture: ", fixture)
  }
  packages <- c("Seurat", "SeuratObject", "Matrix", "qs2", "jpeg", "future")
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "Install fixture build dependencies: ",
      paste(missing, collapse = ", ")
    )
  }
  options(Seurat.object.assay.version = "v5")
  if (fixture %in% c("all", "trekker")) {
    builder_fixture_trekker(file.path(output, "trekker_spatial"))
  }
  message("Built ", fixture, " fixtures in ", normalizePath(output))
}
