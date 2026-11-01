## Pan-cancer T-cell atlas (Zheng et al., Science 374:abe6474, 2021): 397,810
## T cells of 316 patients and 21 cancer types, with TCR sequences and the
## published CD8 T-cell exhaustion continuum. See tcell.ATTRIBUTION.md.
.viewerTcellVersion <- 2L

.viewerTcellZenodo <- "https://zenodo.org/api/records/5461803/files/"

## Zenodo record 10.5281/zenodo.5461803 (CC BY 4.0); size and MD5 are the ones
## the record lists.
.viewerTcellFiles <- function() {
  list(
    expression = list(
      url = paste0(.viewerTcellZenodo, "data.expression.tar.gz/content"),
      size = 14004090334,
      md5 = "e4abd31e6a8dfe8b9b33b6f22b264b3f"
    ),
    metainfo = list(
      url = paste0(.viewerTcellZenodo, "data.metaInfo.tar.gz/content"),
      size = 60547827,
      md5 = "be18bdf0f2a6ac7cd54d8d7ce64dc2a8"
    ),
    tcr = list(
      url = paste0(.viewerTcellZenodo, "data.tcr.tar.gz/content"),
      size = 16783417,
      md5 = "63b1633d6d7558ef0ec04503f9a39060"
    )
  )
}

.viewerTcellPaths <- function(cache_dir = NULL) {
  if (!exists(".demoLargeCacheDir", mode = "function")) {
    stop(
      "Source data-raw/large-demo/download_helpers.R before building the demo data.",
      call. = FALSE
    )
  }
  root <- file.path(.demoLargeCacheDir(cache_dir), "tcell")
  prepared <- file.path(root, paste0("cerebro-v", .viewerTcellVersion))
  list(
    root = root,
    source_dir = file.path(root, "source"),
    expression = file.path(root, "source", "data.expression.tar.gz"),
    metainfo = file.path(root, "source", "data.metaInfo.tar.gz"),
    tcr = file.path(root, "source", "data.tcr.tar.gz"),
    crb = file.path(prepared, "cerebro_tcell_pancancer.crb"),
    sidecar = file.path(prepared, "cerebro_tcell_pancancer.h5"),
    immune = file.path(prepared, "cerebro_tcell_pancancer.immune.qs2"),
    stamp = file.path(prepared, "demo-version")
  )
}

.viewerTcellMembers <- list(
  meta = "data/metaInfo/panC.freq.all.meta.tb.rds",
  pseudotime = "data/metaInfo/CD8_Tex.dpt_pseudotime.txt.gz",
  clinical = "data/metaInfo/TableS1.clinicalInfo.P316.txt",
  tcr = "data/tcr/byCell/tcr.zhangLab.comb.flt.rds"
)

## Unpacks the named members (or, with wildcards = TRUE, the member patterns)
## of a .tar.gz into `exdir`; the archive is read once.
.viewerTcellExtract <- function(archive, members, exdir, wildcards = FALSE) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  tar <- Sys.which("tar")
  if (!nzchar(tar)) {
    stop("The T-cell preparation needs a `tar` program.", call. = FALSE)
  }
  status <- system2(
    tar,
    c(
      "-xzf",
      shQuote(archive),
      "-C",
      shQuote(exdir),
      if (wildcards) "--wildcards",
      shQuote(members)
    )
  )
  if (!identical(status, 0L)) {
    stop("Could not unpack ", basename(archive), ".", call. = FALSE)
  }
  invisible(exdir)
}

## ---- cell annotation --------------------------------------------------------

.viewerTcellReadPseudotime <- function(path) {
  table <- as.data.frame(data.table::fread(path))
  names(table)[1L] <- "mini_cluster"
  if (
    !all(c("mini_cluster", "meta.cluster", "dpt_pseudotime") %in% names(table))
  ) {
    stop("The CD8 Tex pseudotime table has unexpected columns.", call. = FALSE)
  }
  table
}

.viewerTcellReadClinical <- function(path) {
  table <- as.data.frame(data.table::fread(path))
  required <- c("Patient ID", "CancerType", "Age", "Gender", "Clinical Stage")
  if (
    !all(required %in% names(table)) || anyDuplicated(table[["Patient ID"]])
  ) {
    stop("The T-cell clinical table has unexpected columns.", call. = FALSE)
  }
  data.frame(
    patient = as.character(table[["Patient ID"]]),
    cancer_name = as.character(table[["CancerType"]]),
    age = as.character(table[["Age"]]),
    gender = as.character(table[["Gender"]]),
    clinical_stage = as.character(table[["Clinical Stage"]]),
    stringsAsFactors = FALSE
  )
}

## One row per cell, in the order of the published cell table, named by the
## unique cell identifier `cellID.uniq`.
.viewerTcellReadCells <- function(
  meta,
  clinical,
  expected_cells = 397810L,
  expected_patients = 316L
) {
  required <- c(
    "cellID",
    "cellID.uniq",
    "patient.uid",
    "cancerType",
    "loc",
    "dataset",
    "dataset.old",
    "stype",
    "meta.cluster",
    "meta.cluster.coarse",
    "miniCluster",
    "tech",
    "dataSource",
    "TCR",
    "Phase",
    "S.Score",
    "G2M.Score",
    "percent.mito"
  )
  missing <- setdiff(required, names(meta))
  if (
    length(missing) ||
      nrow(meta) != expected_cells ||
      anyDuplicated(meta$cellID.uniq) ||
      length(unique(meta$patient.uid)) != expected_patients
  ) {
    stop(
      "The T-cell atlas cell table must describe ",
      format(expected_cells, big.mark = ","),
      " unique cells of ",
      expected_patients,
      " patients",
      if (length(missing)) {
        paste0(" (missing: ", paste(missing, collapse = ", "), ")")
      },
      ".",
      call. = FALSE
    )
  }
  index <- match(meta$patient.uid, clinical$patient)
  if (anyNA(index)) {
    stop("A patient of the cell table has no clinical record.", call. = FALSE)
  }
  clinical <- clinical[index, , drop = FALSE]
  cells <- as.character(meta$cellID.uniq)
  metadata <- data.frame(
    cell_barcode = cells,
    meta_cluster = as.character(meta$meta.cluster),
    meta_cluster_coarse = as.character(meta$meta.cluster.coarse),
    lineage = as.character(meta$stype),
    cancer_type = as.character(meta$cancerType),
    cancer_name = clinical$cancer_name,
    tissue_site = as.character(meta$loc),
    patient = as.character(meta$patient.uid),
    gender = clinical$gender,
    age = clinical$age,
    clinical_stage = clinical$clinical_stage,
    dataset = as.character(meta$dataset),
    technology = as.character(meta$tech),
    data_source = as.character(meta$dataSource),
    tcr_status = as.character(meta$TCR),
    cell_cycle_phase = as.character(meta$Phase),
    s_score = as.numeric(meta$S.Score),
    g2m_score = as.numeric(meta$G2M.Score),
    percent_mito = as.numeric(meta$percent.mito),
    row.names = cells,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (column in setdiff(
    names(metadata),
    c("cell_barcode", "s_score", "g2m_score", "percent_mito")
  )) {
    metadata[[column]] <- factor(metadata[[column]])
  }
  metadata
}

## ---- expression -------------------------------------------------------------

## One per-dataset SingleCellExperiment of the atlas: the authors' normalised
## matrix (`norm_exprs`: log-normalised for 10x data, log2(TPM + 1) for
## Smart-seq2 data) and the unique cell identifiers. Raw counts are not in
## every file, so they are not used.
.viewerTcellReadDataset <- function(path) {
  object <- readRDS(path)
  if (!inherits(object, "SingleCellExperiment")) {
    stop(basename(path), " is not a SingleCellExperiment.", call. = FALSE)
  }
  assays <- SummarizedExperiment::assayNames(object)
  if (!"norm_exprs" %in% assays) {
    stop(basename(path), " lacks the norm_exprs assay.", call. = FALSE)
  }
  as_sparse <- function(x) {
    if (!inherits(x, "dgCMatrix")) {
      x <- methods::as(methods::as(x, "dMatrix"), "generalMatrix")
      x <- methods::as(x, "CsparseMatrix")
    }
    x
  }
  norm <- as_sparse(SummarizedExperiment::assay(object, "norm_exprs"))
  cells <- as.character(SummarizedExperiment::colData(object)$cellID.uniq)
  ## The data sets of the authors' laboratory are keyed by Ensembl identifier
  ## and carry the symbol in `display.name`.
  details <- SummarizedExperiment::rowData(object)
  genes <- if ("display.name" %in% names(details)) {
    as.character(details$display.name)
  } else {
    rownames(norm)
  }
  if (anyNA(cells) || anyDuplicated(cells) || is.null(genes) || anyNA(genes)) {
    stop(
      basename(path),
      " needs unique cell names and gene names.",
      call. = FALSE
    )
  }
  ## Several identifiers can share a symbol: keep the one detected in most cells.
  detected <- tabulate(norm@i + 1L, nbins = nrow(norm))
  by_detection <- order(detected, decreasing = TRUE)
  keep <- by_detection[!duplicated(genes[by_detection])]
  norm <- norm[keep, , drop = FALSE]
  rownames(norm) <- genes[keep]
  colnames(norm) <- cells
  if (mean(.viewerTcellIsSymbol(rownames(norm))) < 0.5) {
    stop(
      basename(path),
      " names most genes by something other than a symbol.",
      call. = FALSE
    )
  }
  norm
}

## The 70 files name genes in several ways: symbols, but also Ensembl
## identifiers (bare or as a suffix of a clone or rRNA name) and bare numbers
## where a symbol was unknown. Only symbols are kept.
.viewerTcellIsSymbol <- function(genes) {
  !grepl("^ENSG[0-9]+$|-ENSG[0-9]+$|^[0-9]+$", genes)
}

## Puts the per-dataset matrices (genes x cells) on the union of their genes and
## joins them; a gene a data set does not carry is zero there. Genes that are not
## symbols, or that fewer than `min_cells` cells express, are dropped.
.viewerTcellCombine <- function(datasets, cells, min_cells = 1L) {
  genes <- sort(unique(unlist(lapply(datasets, rownames))))
  blocks <- lapply(datasets, function(norm) {
    map <- match(rownames(norm), genes)
    triplet <- methods::as(norm, "TsparseMatrix")
    Matrix::sparseMatrix(
      i = map[triplet@i + 1L],
      j = triplet@j + 1L,
      x = triplet@x,
      dims = c(length(genes), ncol(norm)),
      dimnames = list(genes, colnames(norm))
    )
  })
  combined <- do.call(cbind, blocks)
  index <- match(cells, colnames(combined))
  if (anyNA(index)) {
    stop(
      "The expression files lack ",
      format(sum(is.na(index)), big.mark = ","),
      " cells of the cell table.",
      call. = FALSE
    )
  }
  combined <- combined[, index, drop = FALSE]
  used <- tabulate(combined@i + 1L, nbins = nrow(combined)) >= min_cells &
    .viewerTcellIsSymbol(rownames(combined))
  combined[used, , drop = FALSE]
}

## Highly variable genes of one data set the way Seurat's "mean.var.plot" picks
## them: genes in bins of mean expression are ranked by their z-scored
## dispersion.
.viewerTcellVariableGenesOne <- function(expression, n, bins) {
  cells <- ncol(expression)
  mean <- Matrix::rowSums(expression) / cells
  square <- expression
  square@x <- square@x^2
  variance <- Matrix::rowSums(square) / cells - mean^2
  dispersion <- log(variance / pmax(mean, 1e-12) + 1e-12)
  usable <- is.finite(dispersion) & mean > 0.0125 & mean < 6
  score <- rep(-Inf, length(mean))
  if (sum(usable) > bins) {
    breaks <- unique(stats::quantile(
      mean[usable],
      seq(0, 1, length.out = bins + 1L)
    ))
    bin <- cut(mean, breaks = breaks, include.lowest = TRUE)
    for (level in levels(bin)) {
      member <- usable & !is.na(bin) & bin == level
      if (sum(member) > 1L) {
        spread <- stats::sd(dispersion[member])
        score[member] <- (dispersion[member] - mean(dispersion[member])) /
          if (spread > 0) spread else 1
      }
    }
  }
  rownames(expression)[order(score, decreasing = TRUE)[seq_len(min(
    n,
    sum(is.finite(score))
  ))]]
}

## The data sets differ in technology and normalisation, so the genes are chosen
## in each data set of at least `min_cells` cells and ranked by how many data
## sets (and how high) pick them.
.viewerTcellVariableGenes <- function(
  expression,
  batch,
  n = 2000L,
  bins = 20L,
  min_cells = 50L
) {
  batches <- split(seq_len(ncol(expression)), as.character(batch))
  batches <- batches[lengths(batches) >= min_cells]
  if (!length(batches)) {
    stop(
      "No data set has enough cells to choose variable genes.",
      call. = FALSE
    )
  }
  score <- stats::setNames(numeric(nrow(expression)), rownames(expression))
  for (index in batches) {
    picked <- .viewerTcellVariableGenesOne(
      expression[, index, drop = FALSE],
      n,
      bins
    )
    score[picked] <- score[picked] + rev(seq_along(picked))
  }
  names(sort(score, decreasing = TRUE))[seq_len(min(n, sum(score > 0)))]
}

## Cells x genes, each gene centred and scaled within its data set and clipped
## at `clip`, so a data set's own mean and spread do not drive the components.
.viewerTcellScaled <- function(expression, genes, batch, clip = 10) {
  scaled <- matrix(
    0,
    nrow = ncol(expression),
    ncol = length(genes),
    dimnames = list(colnames(expression), genes)
  )
  for (index in split(seq_len(ncol(expression)), as.character(batch))) {
    block <- as.matrix(expression[genes, index, drop = FALSE])
    centre <- rowMeans(block)
    spread <- sqrt(pmax(rowMeans(block^2) - centre^2, 0))
    spread[spread < 1e-8] <- Inf
    block <- (block - centre) / spread
    block[block > clip] <- clip
    block[block < -clip] <- -clip
    scaled[index, ] <- t(block)
  }
  scaled
}

## Joint embedding: PCA on the variable genes, Harmony across the contributing
## data sets, UMAP. The atlas publishes embeddings only for mini-clusters, so
## this one is computed here and labelled as such.
.viewerTcellEmbedding <- function(
  expression,
  batch,
  n_genes = 2000L,
  n_pcs = 30L,
  threads = max(
    1L,
    min(8L, parallel::detectCores(logical = FALSE)),
    na.rm = TRUE
  ),
  seed = 20211217L
) {
  .demoRequire(c("irlba", "harmony", "uwot"), "T-cell")
  genes <- .viewerTcellVariableGenes(expression, batch, n_genes)
  x <- .viewerTcellScaled(expression, genes, batch)
  set.seed(seed)
  pca <- irlba::irlba(x, nv = n_pcs)
  rm(x)
  scores <- pca$u %*% diag(pca$d, nrow = length(pca$d))
  rownames(scores) <- colnames(expression)
  corrected <- harmony::RunHarmony(
    scores,
    data.frame(dataset = as.character(batch)),
    "dataset",
    verbose = FALSE
  )
  umap <- uwot::umap(
    corrected,
    n_neighbors = 30L,
    min_dist = 0.3,
    n_threads = threads,
    seed = seed
  )
  data.frame(
    Dim_1 = umap[, 1L],
    Dim_2 = umap[, 2L],
    row.names = colnames(expression)
  )
}

## ---- immune repertoire --------------------------------------------------------

## The atlas's recombinant identifier is "V_junction_J" for Smart-seq2 cells
## and "V|D|J|C|junction" for 10x cells; the 10x layout is rewritten to the
## Smart-seq2 one.
.viewerTcellIdentifier <- function(identifier) {
  sub(
    "^([^|]*)\\|[^|]*\\|([^|]*)\\|[^|]*\\|([^|]*)$",
    "\\1_\\3_\\2",
    as.character(identifier)
  )
}

## The first and last fields of a "V_junction_J" identifier are the V and J
## genes.
.viewerTcellGenePair <- function(identifier) {
  identifier <- .viewerTcellIdentifier(identifier)
  fields <- strsplit(as.character(identifier), "_", fixed = TRUE)
  vapply(
    fields,
    function(field) {
      if (length(field) < 2L) {
        return(NA_character_)
      }
      paste(field[[1L]], field[[length(field)]], sep = ".")
    },
    character(1)
  )
}

.viewerTcellJoin <- function(first, second) {
  first[is.na(first)] <- ""
  second[is.na(second)] <- ""
  result <- paste(first, second, sep = "_")
  result <- sub("^_", "", result)
  sub("_$", "", result)
}

## scRepertoire-style clone calls from the atlas's TCR table. Of the up to two
## alpha and two beta chains of a cell the first listed (A1, B1) is used; A2 or
## B2 stand in only when it is absent. `CTnt` is the recombinant identifier
## (V, junction nucleotides, J) of both chains: the table has no CDR3
## nucleotide column; its fields are joined with ":".
.viewerTcellRepertoire <- function(tcr, cells) {
  required <- c(
    "Cell_Name",
    "CDR3.A1",
    "CDR3.A2",
    "CDR3.B1",
    "CDR3.B2",
    "Identifier.A1",
    "Identifier.A2",
    "Identifier.B1",
    "Identifier.B2"
  )
  missing <- setdiff(required, names(tcr))
  if (length(missing)) {
    stop(
      "The T-cell TCR table lacks: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  ## 10x rows mark an absent chain with "-".
  pick <- function(first, second) {
    first <- as.character(first)
    second <- as.character(second)
    first[first %in% "-"] <- NA_character_
    second[second %in% "-"] <- NA_character_
    ifelse(is.na(first), second, first)
  }
  alpha_id <- .viewerTcellIdentifier(pick(tcr$Identifier.A1, tcr$Identifier.A2))
  beta_id <- .viewerTcellIdentifier(pick(tcr$Identifier.B1, tcr$Identifier.B2))
  alpha_aa <- pick(tcr$CDR3.A1, tcr$CDR3.A2)
  beta_aa <- pick(tcr$CDR3.B1, tcr$CDR3.B2)
  gene <- .viewerTcellJoin(
    .viewerTcellGenePair(alpha_id),
    .viewerTcellGenePair(beta_id)
  )
  ## "_" separates the two chains in a clone call, so it cannot also separate
  ## the fields inside a chain's identifier.
  nt <- .viewerTcellJoin(gsub("_", ":", alpha_id), gsub("_", ":", beta_id))
  aa <- .viewerTcellJoin(alpha_aa, beta_aa)
  keep <- nzchar(nt) & nzchar(aa)
  data.frame(
    barcode = cells[keep],
    CTgene = gene[keep],
    CTnt = nt[keep],
    CTaa = aa[keep],
    CTstrict = paste(gene[keep], nt[keep], sep = ";"),
    stringsAsFactors = FALSE
  )
}

## The TCR table names cells by `cellID`, not by the unique identifier; this
## maps a row to a cell of the cell table (NA when the name is ambiguous).
.viewerTcellTcrCells <- function(tcr, meta) {
  key <- paste(meta$cellID, meta$dataset.old, sep = "\r")
  ambiguous <- key %in% key[duplicated(key)]
  index <- match(paste(tcr$Cell_Name, tcr$dataset.old, sep = "\r"), key)
  index[!is.na(index) & ambiguous[index]] <- NA_integer_
  as.character(meta$cellID.uniq)[index]
}

## One repertoire sample per patient and tissue site ("patient (T)"), so the
## Viewer's Compare and Clone Sharing pages follow the same patient's clones
## between tumour, adjacent normal tissue and blood, as the paper does.
.viewerTcellRepertoireBySample <- function(repertoire, metadata) {
  repertoire <- repertoire[!is.na(repertoire$barcode), , drop = FALSE]
  if (!nrow(repertoire) || anyDuplicated(repertoire$barcode)) {
    stop("The T-cell TCR table has no usable, unique cells.", call. = FALSE)
  }
  rows <- metadata[repertoire$barcode, , drop = FALSE]
  label <- paste0(rows$patient, " (", rows$tissue_site, ")")
  repertoire <- split(
    repertoire,
    factor(label, levels = sort(unique(label), method = "radix")),
    drop = TRUE
  )
  lapply(repertoire, function(table) {
    rownames(table) <- NULL
    table
  })
}

## ---- trajectory ---------------------------------------------------------------

## The published diffusion pseudotime of the CD8 T-cell continuum
## (Tn -> Tm -> Tem -> Trm -> Tex) belongs to mini-clusters. A CD8 cell takes
## the value of its mini-cluster; cells of other lineages or outside the
## continuum get NA.
.viewerTcellCellPseudotime <- function(meta, pseudotime) {
  index <- match(
    paste(meta$stype, meta$miniCluster),
    paste("CD8", pseudotime$mini_cluster)
  )
  on_path <- !is.na(index) &
    meta$meta.cluster %in% unique(pseudotime$meta.cluster)
  value <- rep(NA_real_, nrow(meta))
  value[on_path] <- pseudotime$dpt_pseudotime[index[on_path]]
  stats::setNames(value, as.character(meta$cellID.uniq))
}

## The published values crowd together (their quartiles span 0.85 to 0.88 of a
## 0 to 1 range), which a colour scale cannot show. The trajectory's
## `pseudotime` is therefore their rank scaled to (0, 1], which keeps the order;
## the published value stays in the cell table as `cd8_dpt`.
.viewerTcellTrajectory <- function(meta, pseudotime, projection) {
  published <- .viewerTcellCellPseudotime(meta, pseudotime)
  on_path <- !is.na(published)
  cells <- names(published)[on_path]
  state <- as.character(meta$meta.cluster)[on_path]
  value <- rank(published[on_path], ties.method = "average") / sum(on_path)
  order_of_states <- names(sort(tapply(value, state, stats::median)))
  frame <- data.frame(
    DR_1 = projection[cells, "Dim_1"],
    DR_2 = projection[cells, "Dim_2"],
    pseudotime = unname(value),
    state = factor(state, levels = order_of_states),
    row.names = cells,
    stringsAsFactors = FALSE
  )
  ## The line joins the state centres in pseudotime order; it summarises the
  ## order of the published pseudotime, it is not a fitted tree.
  centre_1 <- tapply(frame$DR_1, frame$state, stats::median)[order_of_states]
  centre_2 <- tapply(frame$DR_2, frame$state, stats::median)[order_of_states]
  n <- length(order_of_states)
  edges <- data.frame(
    source = order_of_states[-n],
    target = order_of_states[-1L],
    weight = 1,
    source_dim_1 = unname(centre_1[-n]),
    source_dim_2 = unname(centre_2[-n]),
    target_dim_1 = unname(centre_1[-1L]),
    target_dim_2 = unname(centre_2[-1L]),
    stringsAsFactors = FALSE
  )
  list(
    meta = frame,
    edges = edges,
    provenance = list(
      type = "published_pseudotime",
      version = 1L,
      description = paste0(
        "CD8 T-cell continuum (naive, memory, effector memory, tissue-resident ",
        "memory, exhausted) with the diffusion pseudotime published by Zheng ",
        "et al. 2021. The value belongs to the mini-cluster and every CD8 cell ",
        "of it inherits it; it is shown as its rank scaled to (0, 1] (the ",
        "published value is the cell table column cd8_dpt). The coordinates ",
        "are the UMAP computed for this data set and the line joins the state ",
        "medians; it is not a fitted tree."
      )
    )
  )
}

## ---- object -------------------------------------------------------------------

.viewerTcellBuildObject <- function(paths, work) {
  message("Reading the atlas cell table...")
  .viewerTcellExtract(
    paths$metainfo,
    unlist(
      .viewerTcellMembers[c("meta", "pseudotime", "clinical")],
      use.names = FALSE
    ),
    file.path(work, "meta")
  )
  .viewerTcellExtract(
    paths$tcr,
    .viewerTcellMembers$tcr,
    file.path(work, "tcr")
  )
  meta <- readRDS(file.path(work, "meta", .viewerTcellMembers$meta))
  meta <- as.data.frame(meta)
  clinical <- .viewerTcellReadClinical(
    file.path(work, "meta", .viewerTcellMembers$clinical)
  )
  pseudotime <- .viewerTcellReadPseudotime(
    file.path(work, "meta", .viewerTcellMembers$pseudotime)
  )
  metadata <- .viewerTcellReadCells(meta, clinical)
  cells <- rownames(metadata)

  message(
    "Unpacking the per-data-set expression objects (one pass over 14 GB)..."
  )
  .viewerTcellExtract(
    paths$expression,
    c(
      "data/expression/CD4/byDataset/*.sce.rds",
      "data/expression/CD8/byDataset/*.sce.rds"
    ),
    file.path(work, "expression"),
    wildcards = TRUE
  )
  files <- sort(list.files(
    file.path(work, "expression"),
    pattern = "[.]sce[.]rds$",
    recursive = TRUE,
    full.names = TRUE
  ))
  if (length(files) != 70L) {
    stop(
      "Expected 70 per-data-set expression files, found ",
      length(files),
      ".",
      call. = FALSE
    )
  }
  datasets <- vector("list", length(files))
  for (index in seq_along(files)) {
    message(sprintf(
      "Reading expression %d of %d: %s",
      index,
      length(files),
      basename(files[[index]])
    ))
    datasets[[index]] <- .viewerTcellReadDataset(files[[index]])
    unlink(files[[index]])
  }
  expression <- .viewerTcellCombine(datasets, cells, min_cells = 20L)
  rm(datasets)
  invisible(gc())
  metadata$nGene <- as.integer(diff(expression@p))
  metadata$cd8_dpt <- unname(.viewerTcellCellPseudotime(meta, pseudotime))
  message(
    "Expression: ",
    format(nrow(expression), big.mark = ","),
    " genes x ",
    format(ncol(expression), big.mark = ","),
    " cells"
  )

  message("Writing the H5 expression sidecar...")
  dir.create(dirname(paths$sidecar), recursive = TRUE, showWarnings = FALSE)
  ## The matrix is in memory: one process, so it is not copied to workers.
  .demoWriteH5Transposed(expression, paths$sidecar, workers = 1L)

  message("Computing the joint embedding...")
  projection <- .viewerTcellEmbedding(expression, meta$dataset)
  projection <- projection[cells, , drop = FALSE]
  rm(expression)
  invisible(gc())

  message("Reading the TCR table...")
  tcr <- as.data.frame(readRDS(file.path(work, "tcr", .viewerTcellMembers$tcr)))
  tcr_cells <- .viewerTcellTcrCells(tcr, meta)
  repertoire <- .viewerTcellRepertoire(tcr, tcr_cells)
  repertoire <- .viewerTcellRepertoireBySample(repertoire, metadata)
  qs2::qs_save(repertoire, paths$immune)

  trajectory <- .viewerTcellTrajectory(meta, pseudotime, projection)

  object <- Cerebro$new()
  object$setVersion(utils::packageVersion("CerebroNexus"))
  object$addExperiment(
    "experiment_name",
    "Pan-cancer T-cell atlas (Zheng et al. 2021, 398K)"
  )
  object$addExperiment("organism", "Homo sapiens")
  object$addExperiment("date_of_analysis", "2021-12-17")
  object$addExperiment("date_of_export", as.character(Sys.Date()))
  object$addParameters("main_group", "meta_cluster")
  object$addParameters("source", "Zheng et al., Science 2021; Zenodo 5461803")
  object$setExpressionBackend("h5", basename(paths$sidecar))
  object$setMetaData(metadata)
  for (group in c(
    "meta_cluster",
    "meta_cluster_coarse",
    "lineage",
    "cancer_type",
    "tissue_site",
    "patient",
    "dataset",
    "technology",
    "data_source",
    "tcr_status",
    "cell_cycle_phase",
    "gender",
    "clinical_stage"
  )) {
    values <- unique(as.character(metadata[[group]]))
    values <- sort(values[!is.na(values) & nzchar(values)], method = "radix")
    object$addGroup(group, values)
  }
  object$addProjection("harmony_umap", projection)
  object$addTrajectory(
    "published_pseudotime",
    "CD8_T_exhaustion_continuum",
    trajectory
  )
  object$immune_repertoire_backend <- list(
    type = "file",
    file = basename(paths$immune),
    md5 = unname(tools::md5sum(paths$immune)),
    samples = names(repertoire),
    chains = c("TRA", "TRB")[c(
      any(grepl(
        "TRA",
        unlist(lapply(repertoire, `[[`, "CTgene")),
        fixed = TRUE
      )),
      any(grepl(
        "TRB",
        unlist(lapply(repertoire, `[[`, "CTgene")),
        fixed = TRUE
      ))
    )]
  )
  object$addExtraTable(
    "data_provenance",
    data.frame(
      field = c(
        "publication",
        "data_record",
        "cells",
        "patients",
        "tcr_cells",
        "embedding",
        "pseudotime",
        "license"
      ),
      value = c(
        "Zheng et al., Science 374:abe6474 (2021)",
        "Zenodo 10.5281/zenodo.5461803",
        as.character(nrow(metadata)),
        as.character(nlevels(metadata$patient)),
        as.character(sum(vapply(repertoire, nrow, 1L))),
        "Harmony UMAP computed here (the atlas publishes mini-cluster embeddings only)",
        "Published CD8 diffusion pseudotime, inherited from the mini-cluster",
        "CC BY 4.0"
      ),
      stringsAsFactors = FALSE
    )
  )
  saveCerebro(object, paths$crb)
  writeLines(as.character(.viewerTcellVersion), paths$stamp)
  paths$crb
}

.viewerTcellComplete <- function(paths) {
  file.exists(paths$crb) &&
    !dir.exists(paths$crb) &&
    file.exists(paths$sidecar) &&
    !dir.exists(paths$sidecar) &&
    file.exists(paths$immune) &&
    file.exists(paths$stamp) &&
    identical(
      readLines(paths$stamp, warn = FALSE),
      as.character(.viewerTcellVersion)
    )
}

prepareViewerTcellDemoData <- function(cache_dir = NULL) {
  paths <- .viewerTcellPaths(cache_dir)
  if (.viewerTcellComplete(paths)) {
    return(paths$crb)
  }
  .demoRequire(
    c(
      "data.table",
      "SingleCellExperiment",
      "SummarizedExperiment",
      "Matrix",
      "rhdf5",
      "qs2",
      "irlba",
      "harmony",
      "uwot"
    ),
    "T-cell"
  )
  manifest <- .viewerTcellFiles()
  for (name in names(manifest)) {
    message("Preparing T-cell source: ", basename(paths[[name]]))
    .demoDownload(manifest[[name]], paths[[name]])
  }
  ## Build privately. A failed preparation must not delete a working data set.
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(
    paste0("cerebro-v", .viewerTcellVersion, "-stage-"),
    paths$root
  )
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  work <- file.path(stage, "work")
  dir.create(work)
  staged <- paths
  for (name in c("crb", "sidecar", "immune", "stamp")) {
    staged[[name]] <- file.path(stage, basename(paths[[name]]))
  }
  .viewerTcellBuildObject(staged, work)
  unlink(work, recursive = TRUE)
  if (!.viewerTcellComplete(staged)) {
    stop("T-cell preparation did not finish.", call. = FALSE)
  }
  .demoPublishStage(stage, dirname(paths$crb), paths$root, "T-cell")
  paths$crb
}
