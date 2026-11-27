## Real multi-section Visium data sets with their H&E photographs, prepared as
## Builder input: one SingleCellExperiment per data set (.rds), every tissue
## section in it with its own spots and picture. The Builder imports it like any
## other file; convertSCEToSeurat() makes one spatial entry per section and the
## Builder attaches each section's picture to it. See visium.ATTRIBUTION.md.
##
##   dlpfc        human dorsolateral prefrontal cortex, 12 sections of 3 donors,
##                manual cortical layer labels (Maynard et al. 2021)
##   mouse_brain  mouse brain, sagittal anterior and posterior, 2 serial
##                sections each (10x Genomics)
##   breast       human breast cancer, block A, 2 serial sections (10x Genomics)
##   prostate     human prostate, adenocarcinoma and normal, FFPE (10x Genomics)
##
## Each data set is downloaded once (spot counts, positions, scale factors and
## the high-resolution picture of every section), log-normalised, and given a
## PCA, UMAP and clusters, so it opens with an embedding and groups.
.builderVisiumVersion <- 1L

.builderVisium10xBase <- "https://cf.10xgenomics.com/samples/spatial-exp"
.builderVisiumDlpfcMatrix <- "https://spatial-dlpfc.s3.us-east-2.amazonaws.com"
## LieberInstitute/HumanPilot at a fixed commit: positions, scale factors and
## the layer labels of the DLPFC sections.
.builderVisiumDlpfcRepo <- paste0(
  "https://raw.githubusercontent.com/LieberInstitute/HumanPilot/",
  "044446d6bd8fc154aa74f7be62ec67effb1ec376/10X"
)

## Sections of each data set. 10x sections name their Space Ranger release and
## sample; DLPFC sections their sample number. Sizes are the published ones;
## the MD5 sums are those of the files downloaded on 2026-11-20.
.builderVisiumDatasets <- function() {
  tenx <- function(section, release, sample, matrix_size, spatial_size, ...) {
    data.frame(
      section = section,
      release = release,
      sample = sample,
      matrix_size = matrix_size,
      spatial_size = spatial_size,
      ...,
      stringsAsFactors = FALSE
    )
  }
  list(
    dlpfc = list(
      label = "Human DLPFC, 12 Visium sections",
      species = "human",
      groups = c("layer", "cluster", "donor", "section"),
      sections = data.frame(
        ## Named "<donor>_<sample>": a section name must not start with a
        ## digit to stay the same through Seurat.
        section = paste0(
          rep(c("Br5292", "Br5595", "Br8100"), each = 4L),
          "_",
          c(151507:151510, 151669:151672, 151673:151676)
        ),
        sample = as.character(c(
          151507:151510,
          151669:151672,
          151673:151676
        )),
        donor = rep(c("Br5292", "Br5595", "Br8100"), each = 4L),
        matrix_size = c(
          10239582,
          9253574,
          11518800,
          10794504,
          11041430,
          10043925,
          12476058,
          11704470,
          12887164,
          15416986,
          10881953,
          11127959
        ),
        image_size = c(
          6718558,
          6773366,
          7405299,
          7415609,
          5639012,
          5585969,
          6117698,
          6120831,
          6148725,
          5825181,
          5827286,
          5690646
        ),
        positions_size = c(
          186265,
          186319,
          186375,
          186399,
          186818,
          186856,
          186921,
          186985,
          186583,
          186575,
          186612,
          186592
        ),
        scalefactors_size = c(
          163,
          163,
          162,
          162,
          163,
          163,
          162,
          160,
          162,
          162,
          163,
          161
        ),
        matrix_md5 = c(
          "2e6fc8a20a75c527bc4b0432712d6d9b",
          "b3b8b75851e29564303485ff7b523826",
          "fc4d857e07bca429442c68d9c6b8e7cd",
          "0f94f7fa5c50932f8ed9f653fd53b401",
          "ec82054132e340b43c33f12df7004a51",
          "8ed6d0e7a49003119934dfea3fd63c6a",
          "3e236f9ee65b74ff4c4e6f9afeafbeb1",
          "a560695939bb3ad2785640cb8f40aaed",
          "0ba89021d9c9fa03eed3b814e6c99a16",
          "779c5e1414268a6fb1037cc3587cd3e1",
          "2e08d7777b104538a8ecbbe939a858af",
          "9419abdf5f81bbcade16946e9fb8ca0f"
        ),
        image_md5 = c(
          "7e13813696c132561641261e0a716a2b",
          "e771a2637b1f3ad3e881680e906ebed4",
          "298bff98dab100af27091cf9d39c54f5",
          "b93b0f5c165fb832776d284d5c9dafc5",
          "2bda00917d8be3115ae21ab383f69cf9",
          "44c26bad83bb93961ce8fbd75a0aa243",
          "4d7063b8da500a3e87632eb9479c61ee",
          "ffb83289e9c3acdd20681988142cb029",
          "ba57d74ead373da1ad2e88a8ddeac594",
          "42967e0e30b2d76087cd5110d8a0a297",
          "fe493c94373b2af7a2215f6e1034285c",
          "3beb12577e28743b1f201df4fbd199d5"
        ),
        positions_md5 = c(
          "4ba80c8030a4aceb33a2377f0ee06a9c",
          "017d774a29a6fe4f044e8c1d614e6430",
          "d7c14fac347724efb30030d7a188c7fd",
          "41c61d7b552fad9597cc8e5428f1693e",
          "080f71fc8c3c242072c7a3039d3556cd",
          "6d518af3d6e15455d051b5686ea11c79",
          "039f6903f1e4c56602c622c6c63401ed",
          "a7e61d3c9ff3d932bdc80cf1d4cd10d5",
          "ebdbef1acb06109f9137821c6ebc45ba",
          "1884dcb3bc2ae6409e8d5b3f64021dcb",
          "da015973f283401695be820a084ad375",
          "8c44f3f6a470ae162a2fe872005bc68d"
        ),
        scalefactors_md5 = c(
          "ab627270b98e8c9a36ff4c1fd74a1e2b",
          "a582a51f1904d9ffc33a0dcb09025184",
          "33987263b259e2be1131d09e6c3333ed",
          "786661f8719104ff8cd564f869498341",
          "4c75acba6042a294abbdbd72f8623326",
          "7f75c3aac064af0f3c82f60b013e0a70",
          "0c267efc7e0ed1f3837646af20bc0dd6",
          "81845d85a4a371929e2e26132b086571",
          "56419f32df2b4b0ca51677e784cdfcd3",
          "ade478677745487f0030a616bfbd9495",
          "6764c1eb666869f4fb1fa581776c6083",
          "6e4879fe5d3a431b656c017ffd721cf7"
        ),
        stringsAsFactors = FALSE
      ),
      layers = list(size = 1372541, md5 = "f11febc4e8989f2634a4b82b83a29f6c")
    ),
    mouse_brain = list(
      label = "Mouse brain sagittal, 4 Visium sections",
      species = "mouse",
      groups = c("cluster", "region", "section"),
      sections = tenx(
        section = c("Anterior_1", "Anterior_2", "Posterior_1", "Posterior_2"),
        release = "1.1.0",
        sample = c(
          "V1_Mouse_Brain_Sagittal_Anterior",
          "V1_Mouse_Brain_Sagittal_Anterior_Section_2",
          "V1_Mouse_Brain_Sagittal_Posterior",
          "V1_Mouse_Brain_Sagittal_Posterior_Section_2"
        ),
        matrix_size = c(20554697, 19297326, 21068517, 19917713),
        spatial_size = c(9233573, 9419331, 9708526, 9710456),
        matrix_md5 = c(
          "23cb7418e5db2201fdb92a26998b59bf",
          "dff890199474a4a9fb0db96957a2e6da",
          "8fcc093f6d002fc4fe5c5ec156270f08",
          "e6d42c6b8a355d3277c4fa2fcc1c822a"
        ),
        spatial_md5 = c(
          "7debae42dc8e17721efe1ef65774dfb8",
          "73d403f4984d8a4e29149ff5fb34cc0a",
          "77260d120072eb3accdd8fa635fb32cf",
          "be394af190513120415a70748382aee8"
        ),
        region = c("anterior", "anterior", "posterior", "posterior")
      )
    ),
    breast = list(
      label = "Human breast cancer, 2 Visium sections",
      species = "human",
      groups = c("cluster", "section"),
      sections = tenx(
        section = c("Block_A_1", "Block_A_2"),
        release = "1.1.0",
        sample = c(
          "V1_Breast_Cancer_Block_A_Section_1",
          "V1_Breast_Cancer_Block_A_Section_2"
        ),
        matrix_size = c(28201028, 28898028),
        spatial_size = c(9960960, 10345453),
        matrix_md5 = c(
          "646ccf04a0f809b6f371debe2205f849",
          "f58def4b63496e46f0d5f2290f1c18cf"
        ),
        spatial_md5 = c(
          "980c707b0379aab42f394457c538e618",
          "0cc9df2470ddb9c6b1454e7d8e472aca"
        )
      )
    ),
    prostate = list(
      label = "Human prostate, cancer and normal, 2 Visium sections",
      species = "human",
      groups = c("cluster", "tissue", "section"),
      sections = tenx(
        section = c("Adenocarcinoma", "Normal"),
        release = "1.3.0",
        sample = c(
          "Visium_FFPE_Human_Prostate_Cancer",
          "Visium_FFPE_Human_Normal_Prostate"
        ),
        matrix_size = c(23802346, 9306124),
        spatial_size = c(11359003, 12961070),
        matrix_md5 = c(
          "8641fae992c66d7e9e2a79fbc844fd1c",
          "f6663198638131e044e3a57a8269b417"
        ),
        spatial_md5 = c(
          "97ae5b34edf90e9ac5a343f8dbaf8416",
          "a251bcb4ef9337ad5242aef7f792ee56"
        ),
        tissue = c("adenocarcinoma", "normal")
      )
    )
  )
}

.builderVisiumPaths <- function(id, cache_dir = NULL) {
  if (!exists(".demoLargeCacheDir", mode = "function")) {
    stop(
      "Source data-raw/large-demo/download_helpers.R before building the demo data.",
      call. = FALSE
    )
  }
  root <- file.path(.demoLargeCacheDir(cache_dir), paste0("visium-", id))
  prepared <- file.path(root, paste0("builder-v", .builderVisiumVersion))
  list(
    root = root,
    source_dir = file.path(root, "source"),
    rds = file.path(prepared, paste0("visium_", id, "_sce.rds")),
    stamp = file.path(prepared, "demo-version")
  )
}

## The files of one section: list(name = list(url, size)).
.builderVisiumSectionFiles <- function(id, row) {
  if (identical(id, "dlpfc")) {
    sample <- row$sample
    return(list(
      matrix = list(
        url = sprintf(
          "%s/h5/%s_filtered_feature_bc_matrix.h5",
          .builderVisiumDlpfcMatrix,
          sample
        ),
        size = row$matrix_size,
        md5 = row$matrix_md5
      ),
      image = list(
        url = sprintf(
          "%s/images/%s_tissue_hires_image.png",
          .builderVisiumDlpfcMatrix,
          sample
        ),
        size = row$image_size,
        md5 = row$image_md5
      ),
      positions = list(
        url = sprintf(
          "%s/%s/tissue_positions_list.txt",
          .builderVisiumDlpfcRepo,
          sample
        ),
        size = row$positions_size,
        md5 = row$positions_md5
      ),
      scalefactors = list(
        url = sprintf(
          "%s/%s/scalefactors_json.json",
          .builderVisiumDlpfcRepo,
          sample
        ),
        size = row$scalefactors_size,
        md5 = row$scalefactors_md5
      )
    ))
  }
  base <- sprintf(
    "%s/%s/%s/%s",
    .builderVisium10xBase,
    row$release,
    row$sample,
    row$sample
  )
  list(
    matrix = list(
      url = paste0(base, "_filtered_feature_bc_matrix.h5"),
      size = row$matrix_size,
      md5 = row$matrix_md5
    ),
    spatial = list(
      url = paste0(base, "_spatial.tar.gz"),
      size = row$spatial_size,
      md5 = row$spatial_md5
    )
  )
}

## Downloads a section's files into `<source>/<section>/` and returns the paths
## of its matrix, positions, scale factors and picture.
.builderVisiumFetchSection <- function(id, row, source_dir) {
  ## DLPFC sources are kept by sample number, 10x ones by section.
  directory <- file.path(
    source_dir,
    if (identical(id, "dlpfc")) row$sample else row$section
  )
  files <- .builderVisiumSectionFiles(id, row)
  local <- lapply(names(files), function(name) {
    spec <- files[[name]]
    .demoDownload(spec, file.path(directory, basename(spec$url)))
  })
  names(local) <- names(files)
  if (!is.null(local$spatial)) {
    ## A 10x section's positions, scale factors and pictures come as one
    ## archive of its `spatial/` folder.
    unpacked <- file.path(directory, "spatial")
    if (!file.exists(file.path(unpacked, "scalefactors_json.json"))) {
      utils::untar(local$spatial, exdir = directory)
    }
    positions <- file.path(
      unpacked,
      c("tissue_positions_list.csv", "tissue_positions.csv")
    )
    local$positions <- positions[file.exists(positions)][[1L]]
    local$scalefactors <- file.path(unpacked, "scalefactors_json.json")
    local$image <- file.path(unpacked, "tissue_hires_image.png")
  }
  local
}

## A 10x feature-barcode matrix (HDF5) as a sparse genes x spots matrix named
## by gene symbol (made unique) and barcode.
.builderVisiumReadMatrix <- function(path) {
  read <- function(name) as.vector(rhdf5::h5read(path, name))
  shape <- read("matrix/shape")
  counts <- Matrix::sparseMatrix(
    i = read("matrix/indices") + 1L,
    p = read("matrix/indptr"),
    x = as.numeric(read("matrix/data")),
    dims = shape,
    repr = "C"
  )
  rhdf5::h5closeAll()
  dimnames(counts) <- list(
    make.unique(as.character(read("matrix/features/name"))),
    as.character(read("matrix/barcodes"))
  )
  rhdf5::h5closeAll()
  counts
}

## Height and width of a PNG, from its header: the DLPFC pictures are 16-bit,
## and decoding them only to measure them costs time and a warning.
.builderVisiumPngSize <- function(path) {
  header <- readBin(path, "raw", 24L)
  if (
    length(header) < 24L ||
      !identical(header[1:8], as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
  ) {
    stop("Not a PNG picture: ", basename(path), call. = FALSE)
  }
  number <- function(bytes) sum(as.numeric(as.integer(bytes)) * 256^(3:0))
  c(height = number(header[21:24]), width = number(header[17:20]))
}

## Spot positions in full-resolution pixels, spots under tissue only. Space
## Ranger 1 writes them without a header, Space Ranger 2 with one.
.builderVisiumReadPositions <- function(path) {
  columns <- c(
    "barcode",
    "in_tissue",
    "array_row",
    "array_col",
    "pxl_row_in_fullres",
    "pxl_col_in_fullres"
  )
  first <- readLines(path, n = 1L)
  positions <- utils::read.csv(
    path,
    header = startsWith(first, "barcode"),
    col.names = columns,
    stringsAsFactors = FALSE
  )
  positions[positions$in_tissue == 1L, , drop = FALSE]
}

## One section: its counts, spots and picture. The picture is the
## full-resolution image scaled by `tissue_hires_scalef`, so in pixel
## coordinates it spans [0, width / scale] x [0, height / scale]. Spots keep
## the picture's orientation (y is the pixel row), as Visium data read from an
## h5ad or a Seurat object do; the Builder flips the picture to match.
.builderVisiumSection <- function(files, section) {
  counts <- .builderVisiumReadMatrix(files$matrix)
  positions <- .builderVisiumReadPositions(files$positions)
  positions <- positions[
    positions$barcode %in% colnames(counts),
    ,
    drop = FALSE
  ]
  counts <- counts[, positions$barcode, drop = FALSE]
  scale <- jsonlite::fromJSON(files$scalefactors)$tissue_hires_scalef
  dimensions <- .builderVisiumPngSize(files$image)
  bounds <- c(
    xmin = 0,
    xmax = dimensions[[2L]] / scale,
    ymin = 0,
    ymax = dimensions[[1L]] / scale
  )
  coordinates <- cbind(
    x = positions$pxl_col_in_fullres,
    y = positions$pxl_row_in_fullres
  )
  colnames(counts) <- paste0(section, "_", positions$barcode)
  rownames(coordinates) <- colnames(counts)
  image <- readBin(files$image, "raw", file.info(files$image)$size)
  list(
    counts = counts,
    coordinates = coordinates,
    barcodes = positions$barcode,
    picture = list(
      histology_image = paste0(
        "data:image/png;base64,",
        gsub("\n", "", jsonlite::base64_enc(image), fixed = TRUE)
      ),
      histology_image_bounds = bounds,
      image_label = "H&E"
    )
  )
}

## PCA, UMAP and clusters on the log-normalised counts of all sections.
.builderVisiumEmbed <- function(counts, logcounts, seed = 1L) {
  object <- SeuratObject::CreateSeuratObject(counts = counts)
  object <- SeuratObject::SetAssayData(
    object,
    layer = "data",
    new.data = logcounts
  )
  object <- Seurat::FindVariableFeatures(
    object,
    nfeatures = 2000L,
    verbose = FALSE
  )
  object <- Seurat::ScaleData(object, verbose = FALSE)
  object <- Seurat::RunPCA(object, npcs = 30L, seed.use = seed, verbose = FALSE)
  object <- Seurat::FindNeighbors(object, dims = 1:30, verbose = FALSE)
  object <- Seurat::FindClusters(
    object,
    resolution = 0.6,
    random.seed = seed,
    verbose = FALSE
  )
  object <- Seurat::RunUMAP(
    object,
    dims = 1:30,
    seed.use = seed,
    verbose = FALSE
  )
  list(
    pca = SeuratObject::Embeddings(object, "pca"),
    umap = SeuratObject::Embeddings(object, "umap"),
    cluster = paste0("C", as.integer(as.character(object$seurat_clusters)) + 1L)
  )
}

.builderVisiumBuild <- function(id, spec, paths) {
  sections <- spec$sections
  loaded <- lapply(seq_len(nrow(sections)), function(index) {
    row <- sections[index, , drop = FALSE]
    message("  section ", row$section)
    files <- .builderVisiumFetchSection(id, row, paths$source_dir)
    .builderVisiumSection(files, row$section)
  })
  names(loaded) <- sections$section
  genes <- Reduce(intersect, lapply(loaded, function(x) rownames(x$counts)))
  counts <- do.call(
    cbind,
    lapply(loaded, function(x) x$counts[genes, , drop = FALSE])
  )
  logcounts <- .demoLogNormalize(counts)
  section_of <- rep(
    sections$section,
    vapply(
      loaded,
      function(x) {
        ncol(x$counts)
      },
      integer(1)
    )
  )
  cell_data <- data.frame(
    library_id = section_of,
    section = section_of,
    nUMI = Matrix::colSums(counts),
    nGene = Matrix::colSums(counts > 0),
    row.names = colnames(counts),
    stringsAsFactors = FALSE
  )
  ## The other per-section columns of the table (donor, region, tissue).
  extra <- setdiff(
    names(sections),
    c(
      "section",
      "release",
      "sample",
      grep("_(size|md5)$", names(sections), value = TRUE)
    )
  )
  for (column in extra) {
    cell_data[[column]] <- sections[[column]][match(
      section_of,
      sections$section
    )]
  }
  if (identical(id, "dlpfc")) {
    ## Manual cortical layer of each spot (L1-L6, WM); a few have none.
    map <- .demoDownload(
      list(
        url = paste0(.builderVisiumDlpfcRepo, "/barcode_level_layer_map.tsv"),
        size = spec$layers$size,
        md5 = spec$layers$md5
      ),
      file.path(paths$source_dir, "barcode_level_layer_map.tsv")
    )
    layers <- utils::read.delim(
      map,
      header = FALSE,
      col.names = c("barcode", "section", "layer"),
      colClasses = "character"
    )
    barcodes <- unlist(lapply(loaded, `[[`, "barcodes"), use.names = FALSE)
    sample_of <- sections$sample[match(section_of, sections$section)]
    key <- paste(sample_of, barcodes)
    layer <- layers$layer[match(key, paste(layers$section, layers$barcode))]
    layer[is.na(layer)] <- "unlabelled"
    cell_data$layer <- layer
  }
  ## `sample` is a group the Builder preselects and the Spatial page's Sample
  ## control: the donor, the brain region, or else the section. `orig.ident`
  ## has one value, so the Builder leaves it out instead of reading a group
  ## from the spot names.
  sample_field <- intersect(c("donor", "region"), names(cell_data))
  cell_data$sample <- if (length(sample_field)) {
    cell_data[[sample_field[[1L]]]]
  } else {
    cell_data$section
  }
  cell_data$orig.ident <- id
  message("  embedding ", ncol(counts), " spots")
  embedding <- .builderVisiumEmbed(counts, logcounts)
  cell_data$cluster <- embedding$cluster
  coordinates <- do.call(rbind, lapply(loaded, `[[`, "coordinates"))
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts, logcounts = logcounts),
    colData = S4Vectors::DataFrame(cell_data, check.names = FALSE),
    reducedDims = list(
      UMAP = embedding$umap,
      PCA = embedding$pca,
      spatial = coordinates[colnames(counts), , drop = FALSE]
    )
  )
  ## Each section's picture, as readH5adAsSCE() keeps Visium images: the
  ## Builder attaches them to their sections (convertSCEToSeurat()).
  S4Vectors::metadata(sce)$spatial_images <- lapply(loaded, function(x) {
    list(hires = x$picture)
  })
  S4Vectors::metadata(sce)$cerebro_demo <- list(
    id = id,
    label = spec$label,
    species = spec$species,
    groups = spec$groups,
    version = .builderVisiumVersion
  )
  sce
}

## Prepares data set `id` and returns the path of its .rds for the Builder.
prepareBuilderVisiumDemoData <- function(id, cache_dir = NULL) {
  datasets <- .builderVisiumDatasets()
  if (!is.character(id) || length(id) != 1L || !id %in% names(datasets)) {
    stop(
      "Unknown Visium demo `",
      id,
      "`; one of: ",
      paste(names(datasets), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  .demoRequire(
    c(
      "Seurat",
      "SeuratObject",
      "SingleCellExperiment",
      "S4Vectors",
      "rhdf5",
      "jsonlite"
    ),
    paste("Visium", id)
  )
  paths <- .builderVisiumPaths(id, cache_dir)
  if (
    file.exists(paths$rds) &&
      file.exists(paths$stamp) &&
      identical(
        readLines(paths$stamp, warn = FALSE),
        as.character(.builderVisiumVersion)
      )
  ) {
    return(paths$rds)
  }
  spec <- datasets[[id]]
  message("Preparing ", spec$label, " (", id, ")")
  dir.create(paths$source_dir, recursive = TRUE, showWarnings = FALSE)
  sce <- .builderVisiumBuild(id, spec, paths)
  stage <- tempfile("builder-visium-", paths$root)
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  saveRDS(sce, file.path(stage, basename(paths$rds)))
  writeLines(
    as.character(.builderVisiumVersion),
    file.path(stage, "demo-version")
  )
  .demoPublishStage(stage, dirname(paths$rds), paths$root, paste("Visium", id))
  paths$rds
}

## ---- The Viewer data set, built by the Builder ------------------------------

## The Builder's build of a data set: bumped when what it writes changes.
.viewerVisiumVersion <- 1L

.viewerVisiumPaths <- function(id, cache_dir = NULL) {
  root <- .builderVisiumPaths(id, cache_dir)$root
  built <- file.path(
    root,
    paste0("viewer-v", .builderVisiumVersion, ".", .viewerVisiumVersion)
  )
  list(
    root = root,
    built = built,
    demo = file.path(built, "demo.rds"),
    stamp = file.path(built, "demo-version")
  )
}

## The Builder's server code without its pages, as its end-to-end tests load
## it (tests/testthat/helper-builder-end-to-end.R), so the demo is built by the
## same steps as a click on Build.
.viewerVisiumBuilderRuntime <- function(repo_root) {
  .demoRequire(c("testthat", "withr"), "the Builder run of the Visium demos")
  helpers <- new.env(parent = asNamespace("CerebroNexus"))
  for (file in list.files(
    file.path(repo_root, "tests", "testthat"),
    "^helper-.*[.]R$",
    full.names = TRUE
  )) {
    sys.source(file, envir = helpers)
  }
  runtime <- new.env(parent = helpers)
  withr::with_dir(repo_root, helpers$builder_e2e_source_runtime(runtime))
  runtime
}

## What the Builder does when the file is imported and built: inspect it, take
## its default settings and the pictures it brought, tick the data set's groups
## besides the ones the Builder preselects, open Spatial as the grid and build
## the CRB and its Viewer App into `work`. Returns the App's directory.
.viewerVisiumBuildApp <- function(runtime, rds, name, ticked, work) {
  build <- function() {
    inspected <- builder_adapter_inspect(builder_seurat_file_adapter(rds))
    profile <- inspected$legacy_profile
    settings <- builder_default_settings(
      profile,
      name,
      dataset_profile = inspected$profile
    )
    metadata <- builder_recommend_metadata(inspected$profile)
    settings$recommendations <- list(metadata = metadata)
    groups <- intersect(
      unique(c(ticked, profile$group_preselect)),
      profile$group_candidates
    )
    settings$metadata_policy <- builder_e2e_review_metadata_policy(
      metadata,
      groups
    )
    settings$groups <- groups
    settings$included_groups <- groups
    settings$default_group <- groups[[1L]]
    settings$expression_backend <- "embedded"
    settings$viewer_display$spatial_layout <- "grid"
    settings$images <- utils::modifyList(
      settings$images %||% list(),
      builder_source_image_settings(
        inspected$source_images,
        file.path(work, "source-images")
      )
    )
    snapshot <- builder_snapshot_seurat(
      inspected$object,
      file.path(work, "snapshot"),
      available_bytes = 2^40
    )
    entry <- list(
      id = "visium",
      source_id = "visium",
      output_id = "visium",
      path = rds,
      filename = basename(rds),
      source_origin = "upload",
      format = inspected$format,
      profile = profile,
      dataset_profile = inspected$profile,
      snapshot = snapshot,
      revision = 1L,
      levels = inspected$levels %||% list(),
      settings = settings
    )
    plan <- builder_freeze_plan(
      list(entry),
      file.path(work, "release"),
      make_app = TRUE
    )
    if (!is.null(plan$error)) {
      stop(paste(unlist(plan$error), collapse = " "), call. = FALSE)
    }
    stage <- file.path(work, "stage")
    dir.create(stage)
    result <- builder_execute_plan(plan, stage, list(visium = snapshot))
    if (!identical(result$state, "success")) {
      stop(result$error %||% "The Builder could not build it.", call. = FALSE)
    }
    file.path(stage, "cerebro_app")
  }
  ## The Builder's functions, then this call's arguments.
  environment(build) <- list2env(
    list(rds = rds, name = name, ticked = ticked, work = work),
    parent = runtime
  )
  build()
}

## The Builder's Viewer App reduced to what the source Viewer needs: the CRB,
## the section pictures and, in demo.rds, the label, the picture and display
## settings the Builder chose (paths relative to the demo.rds directory).
.viewerVisiumKeepApp <- function(app, stage, label) {
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  crb <- config$crb_file_to_load[[1L]]
  private <- list.files(file.path(app, dirname(crb)), full.names = TRUE)
  stopifnot(file.copy(private, stage))
  dir.create(file.path(stage, "pictures"))
  images <- lapply(config$spatial_images[[1L]], function(section) {
    lapply(section, function(image) {
      stopifnot(file.copy(
        file.path(app, image$path),
        file.path(stage, "pictures")
      ))
      image$path <- file.path("pictures", basename(image$path))
      image
    })
  })
  first <- function(value) if (length(value)) unname(value[[1L]])
  saveRDS(
    list(
      label = label,
      crb = basename(crb),
      spatial_images = images,
      spatial_image_settings = config$spatial_image_settings[[1L]],
      viewer_content = config$viewer_content[[1L]],
      point_size = first(config$point_size),
      point_opacity = first(config$point_opacity)
    ),
    file.path(stage, "demo.rds")
  )
}

## A Visium demo as a Viewer data set: the Builder input file
## (prepareBuilderVisiumDemoData()) built by the Builder's own steps. Returns
## the path of its demo.rds, which viewerAddVisiumDemos() reads
## (inst/viewer/million_cell_demo.R).
prepareViewerVisiumDemoData <- function(id, repo_root, cache_dir = NULL) {
  rds <- prepareBuilderVisiumDemoData(id, cache_dir)
  paths <- .viewerVisiumPaths(id, cache_dir)
  version <- paste(.builderVisiumVersion, .viewerVisiumVersion, sep = ".")
  if (
    file.exists(paths$demo) &&
      file.exists(paths$stamp) &&
      identical(readLines(paths$stamp, warn = FALSE), version)
  ) {
    return(paths$demo)
  }
  spec <- .builderVisiumDatasets()[[id]]
  message("Building ", spec$label, " (", id, ") with the Builder")
  work <- tempfile("builder-", paths$root)
  stage <- tempfile("viewer-visium-", paths$root)
  dir.create(work)
  dir.create(stage)
  on.exit(unlink(c(work, stage), recursive = TRUE), add = TRUE)
  app <- .viewerVisiumBuildApp(
    .viewerVisiumBuilderRuntime(repo_root),
    normalizePath(rds, winslash = "/"),
    id,
    spec$groups,
    work
  )
  .viewerVisiumKeepApp(app, stage, spec$label)
  writeLines(version, file.path(stage, "demo-version"))
  .demoPublishStage(stage, paths$built, paths$root, paste("Visium", id))
  paths$demo
}
