## Test-only synthetic objects for capability tests. Public Builder examples
## are loaded from their committed serialized fixtures instead.

.builder_test_with_seed <- function(seed, code) {
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) {
    caller_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (seed_exists) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed)
  force(code)
}

.builder_fixture_stabilize <- function(object) {
  fixed_time <- as.POSIXct("2026-08-06 00:00:00", tz = "UTC")
  if (length(object@commands)) {
    for (name in names(object@commands)) {
      object@commands[[name]]@time.stamp <- fixed_time
    }
  }
  object
}

.builder_fixture_object <- function(
  n_cells,
  n_genes = 40L,
  samples = "donorA",
  organism = c("hg", "mm")
) {
  organism <- match.arg(organism)
  cells <- paste0("cell", seq_len(n_cells))
  prefix <- if (organism == "hg") {
    c("MT-", "RPS", "RPL")
  } else {
    c("mt-", "Rps", "Rpl")
  }
  genes <- c(
    paste0(prefix[1L], c("CO1", "ND1", "CYB")),
    paste0(prefix[2L], 3:6),
    paste0(prefix[3L], 3:6),
    paste0("Gene", seq_len(n_genes - 11L))
  )
  counts <- Matrix::Matrix(
    stats::rpois(length(genes) * n_cells, lambda = 3),
    nrow = length(genes),
    dimnames = list(genes, cells),
    sparse = TRUE
  )
  object <- SeuratObject::CreateSeuratObject(counts = counts)
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object$sample <- factor(sample(samples, n_cells, replace = TRUE))
  object$cell_type <- factor(
    sample(c("Neuron", "Astrocyte", "Microglia"), n_cells, replace = TRUE)
  )
  object$condition <- factor(sample(c("control", "treated"), n_cells, TRUE))
  embeddings <- matrix(
    stats::rnorm(n_cells * 2L),
    ncol = 2L,
    dimnames = list(cells, c("UMAP_1", "UMAP_2"))
  )
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings,
    key = "UMAP_",
    assay = "RNA"
  )
  object
}

.builder_fixture_add_section <- function(object, name, cells, origin, span) {
  coordinates <- data.frame(
    x = stats::runif(length(cells), 0, span[1L]) + origin[1L],
    y = stats::runif(length(cells), 0, span[2L]) + origin[2L],
    cell = cells
  )
  object[[name]] <- SeuratObject::CreateFOV(
    coords = list(
      centroids = SeuratObject::CreateCentroids(coordinates)
    ),
    type = "centroids",
    assay = "RNA",
    key = paste0(tolower(name), "_")
  )
  object
}

.builder_fixture_immune_repertoire <- function(object, chain) {
  cells <- colnames(object)
  by_sample <- split(cells, as.character(object$sample))
  prefix <- switch(
    chain,
    TRA = "TRAV1.TRAJ1",
    TRB = "TRBV1.TRBJ1",
    IGH = "IGHV1.IGHJ1"
  )
  lapply(by_sample, function(barcodes) {
    data.frame(
      barcode = barcodes,
      CTgene = rep(prefix, length(barcodes)),
      CTnt = paste0("ACGT", seq_along(barcodes)),
      CTaa = paste0("CASSLGQ", seq_along(barcodes), "F"),
      CTstrict = paste0(chain, "_clone_", seq_along(barcodes)),
      stringsAsFactors = FALSE
    )
  })
}

.builder_fixture_hla <- function(samples) {
  typing <- expand.grid(
    sample = samples,
    locus = c("A", "B", "C"),
    copy = 1:2,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  typing$donor_id <- typing$sample
  typing$allele <- sprintf(
    "HLA-%s*%02d:%02d",
    typing$locus,
    seq_len(nrow(typing)),
    typing$copy
  )
  typing$locus <- paste0("HLA-", typing$locus)
  typing$resolution <- "2-field"
  typing[, c("sample", "donor_id", "locus", "copy", "allele", "resolution")]
}

.builder_fixture_immune <- function(mode) {
  object <- .builder_fixture_object(
    24L,
    samples = c("donor1", "donor2")
  )
  if (mode %in% c("tcr_hla", "tcr_only")) {
    object@misc$immune_repertoire <-
      .builder_fixture_immune_repertoire(object, "TRB")
  }
  if (identical(mode, "bcr_only")) {
    object@misc$immune_repertoire <-
      .builder_fixture_immune_repertoire(object, "IGH")
  }
  if (mode %in% c("tcr_hla", "hla_only")) {
    object@misc$hla_typing <- .builder_fixture_hla(levels(object$sample))
    object@misc$hla_typing_source_type <- "synthetic"
  }
  if (identical(mode, "metadata_tcr")) {
    table <- do.call(
      rbind,
      .builder_fixture_immune_repertoire(object, "TRB")
    )
    table <- table[match(colnames(object), table$barcode), , drop = FALSE]
    for (column in c("CTgene", "CTnt", "CTaa", "CTstrict")) {
      object@meta.data[[column]] <- table[[column]]
    }
  }
  if (identical(mode, "legacy_tcr")) {
    object@misc$tcr_data <- .builder_fixture_immune_repertoire(object, "TRB")
  }
  object
}

.builder_fixture_spatial <- function() {
  object <- .builder_fixture_object(30L, organism = "mm")
  cells <- colnames(object)
  object <- .builder_fixture_add_section(
    object,
    "section_a",
    cells[seq_len(15L)],
    c(10, 20),
    c(96, 72)
  )
  object <- .builder_fixture_add_section(
    object,
    "section_b",
    cells[16:30],
    c(250, 40),
    c(80, 64)
  )
  object[["tsne"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      SeuratObject::Embeddings(object, "umap") + 0.5,
      ncol = 2L,
      dimnames = list(cells, c("TSNE_1", "TSNE_2"))
    ),
    key = "TSNE_",
    assay = "RNA"
  )
  object
}
