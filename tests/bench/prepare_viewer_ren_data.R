.viewerRenVersion <- 2L

.viewerRenCacheDir <- function(path = NULL) {
  if (is.null(path)) {
    configured <- Sys.getenv("CEREBRO_LARGE_CACHE", "")
    if (nzchar(configured)) path <- configured
  }
  if (!is.null(path)) {
    if (
      !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
    ) {
      stop("CEREBRO_LARGE_CACHE must be one non-empty path.", call. = FALSE)
    }
    return(path)
  }
  file.path(tools::R_user_dir("CerebroNexus", "cache"), "large-examples")
}

.viewerRenPaths <- function(cache_dir = NULL) {
  root <- file.path(.viewerRenCacheDir(cache_dir), "ren")
  prepared <- file.path(root, paste0("cerebro-v", .viewerRenVersion))
  list(
    root = root,
    source_dir = file.path(root, "source"),
    archive = file.path(root, "source", "COVID19_ALL.h5ad.tar.gz"),
    h5ad = file.path(root, "source", "COVID19_ALL.h5ad"),
    annotation = file.path(root, "source", "GSE158055_cell_annotation.csv"),
    tcr = file.path(
      root,
      "source",
      "GSE158055_covid19_tcr_vdjnt_pclone.tsv"
    ),
    bcr = file.path(
      root,
      "source",
      "GSE158055_covid19_bcr_vdjnt_pclone.tsv"
    ),
    clinical = file.path(root, "source", "GSE158055_sample_metadata.xlsx"),
    crb = file.path(prepared, "cerebro_ren_covid_1.46m.crb"),
    sidecar = file.path(
      prepared,
      "cerebro_ren_covid_1.46m.bpcells"
    ),
    stamp = file.path(prepared, "demo-version")
  )
}

.viewerRenFiles <- function() {
  hca <- "https://service.azul.data.humancellatlas.org/repository/files/"
  list(
    archive = list(
      url = paste0(
        hca,
        "aa382142-6181-4bb2-9301-7f841c6ee04a?catalog=dcp60&",
        "version=2023-01-12T15%3A44%3A29.577000Z"
      ),
      size = 14333004385,
      sha256 = paste0(
        "8245c63a86c40c582fdade5d74f9696c",
        "321e3247175bd7e0142d4dec1176dcbe"
      )
    ),
    annotation = list(
      url = paste0(
        hca,
        "89328322-365c-4d16-b66a-b8e45c49232b?catalog=dcp60&",
        "version=2023-01-12T15%3A44%3A29.289000Z"
      ),
      size = 70354925,
      sha256 = paste0(
        "05f224caa67d5c9085324d3852244675",
        "69f0b2ed54b28f2be36558e4dc65c7eb"
      )
    ),
    tcr = list(
      url = paste0(
        hca,
        "76ed0e2f-a5a4-40da-9707-8b104abe8fd3?catalog=dcp60&",
        "version=2023-01-12T15%3A44%3A29.549000Z"
      ),
      size = 155752190,
      sha256 = paste0(
        "96b6eada775762a04edb3c9f80b42675",
        "6f9c69d5946f3f0e5bb3e0a37938b68a"
      )
    ),
    bcr = list(
      url = paste0(
        hca,
        "c3581269-024c-4052-a14f-1d3cb44ad08c?catalog=dcp60&",
        "version=2023-01-12T15%3A44%3A29.478000Z"
      ),
      size = 204004592,
      sha256 = paste0(
        "53888ae11ad896c81fe0416dfab678f20",
        "0c6bb39ab59c24f6f9c3163cf2b4dde"
      )
    ),
    clinical = list(
      url = paste0(
        "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE158nnn/",
        "GSE158055/suppl/GSE158055_sample_metadata.xlsx"
      ),
      size = 49685,
      sha256 = paste0(
        "9442a762909f035ecc19f3d1e595b65f",
        "b914c7a3a0bc51eee444cf1dc4625b36"
      )
    )
  )
}

.viewerRenRequire <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "The Ren 1.46M demo requires: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

.viewerRenSha256 <- function(path) {
  tool <- Sys.which("sha256sum")
  args <- shQuote(path)
  if (!nzchar(tool)) {
    tool <- Sys.which("shasum")
    args <- c("-a", "256", shQuote(path))
  }
  if (!nzchar(tool)) {
    return(NA_character_)
  }
  output <- system2(tool, args, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("Could not checksum ", basename(path), ".", call. = FALSE)
  }
  tolower(strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]])
}

.viewerRenValidateFile <- function(path, spec, checksum = FALSE) {
  if (!file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  size <- unname(file.info(path)$size)
  if (!is.finite(size) || size != spec$size) {
    return(FALSE)
  }
  if (isTRUE(checksum)) {
    observed <- .viewerRenSha256(path)
    if (!is.na(observed) && !identical(observed, spec$sha256)) {
      stop("Checksum mismatch for ", basename(path), ".", call. = FALSE)
    }
  }
  TRUE
}

.viewerRenDownload <- function(spec, destination) {
  if (.viewerRenValidateFile(destination, spec)) {
    return(destination)
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  partial <- paste0(destination, ".part")
  if (file.exists(destination)) {
    unlink(destination, force = TRUE)
  }

  curl <- Sys.which("curl")
  if (nzchar(curl)) {
    status <- system2(
      curl,
      c(
        "-fL",
        "--retry",
        "8",
        "--retry-delay",
        "5",
        "--continue-at",
        "-",
        "-o",
        shQuote(partial),
        shQuote(spec$url)
      )
    )
    if (!identical(status, 0L)) {
      stop("Download failed for ", basename(destination), ".", call. = FALSE)
    }
  } else {
    if (file.exists(partial)) {
      unlink(partial, force = TRUE)
    }
    old_options <- options(timeout = max(getOption("timeout"), 86400))
    on.exit(options(old_options), add = TRUE)
    utils::download.file(spec$url, partial, mode = "wb", method = "libcurl")
  }

  if (!.viewerRenValidateFile(partial, spec, checksum = TRUE)) {
    stop(
      "Downloaded file is incomplete: ",
      basename(destination),
      call. = FALSE
    )
  }
  if (!file.rename(partial, destination)) {
    stop("Could not publish ", basename(destination), ".", call. = FALSE)
  }
  destination
}

.viewerRenExtractH5ad <- function(paths) {
  if (file.exists(paths$h5ad) && !dir.exists(paths$h5ad)) {
    return(paths$h5ad)
  }
  h5ad_member <- "COVID19_ALL.h5ad"

  message("Extracting the Ren 1.46M-cell h5ad...")
  stage <- tempfile("ren-h5ad-", dirname(paths$h5ad))
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  utils::untar(paths$archive, files = h5ad_member, exdir = stage)
  extracted <- file.path(stage, h5ad_member)
  if (!file.exists(extracted) || dir.exists(extracted)) {
    stop("Could not extract the Ren .h5ad file.", call. = FALSE)
  }
  if (!file.rename(extracted, paths$h5ad)) {
    stop("Could not publish the extracted Ren .h5ad file.", call. = FALSE)
  }
  paths$h5ad
}

.viewerRenClinicalNames <- function(names) {
  names <- sub("^characteristics:[[:space:]]*", "", names, ignore.case = TRUE)
  names <- tolower(names)
  names <- gsub("[^a-z0-9]+", "_", names)
  names <- gsub("^_+|_+$", "", names)
  make.unique(names, sep = "_")
}

.viewerRenReadClinical <- function(path) {
  table <- as.data.frame(
    readxl::read_excel(
      path,
      sheet = 1L,
      skip = 20L,
      .name_repair = "minimal"
    ),
    stringsAsFactors = FALSE
  )
  names(table) <- .viewerRenClinicalNames(names(table))
  table <- table[grepl("^GSM[0-9]+$", table$geo_accession), , drop = FALSE]
  if (nrow(table) != 284L || anyDuplicated(table$sample_name)) {
    stop(
      "The Ren clinical workbook must contain 284 unique samples.",
      call. = FALSE
    )
  }
  required <- c(
    "sample_name",
    "geo_accession",
    "patients",
    "datasets",
    "age",
    "sex",
    "sample_type",
    "covid_19_severity",
    "sample_time",
    "sampling_day_days_after_symptom_onset",
    "sars_cov_2",
    "bcr_single_cell_sequencing",
    "tcr_single_cell_sequencing",
    "outcome",
    "comorbidities"
  )
  missing <- setdiff(required, names(table))
  if (length(missing)) {
    stop(
      "The Ren clinical workbook is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  data.frame(
    sample = as.character(table$sample_name),
    geo_accession = as.character(table$geo_accession),
    patient = as.character(table$patients),
    dataset = as.character(table$datasets),
    age = as.character(table$age),
    sex = as.character(table$sex),
    tissue = as.character(table$sample_type),
    severity = as.character(table$covid_19_severity),
    stage = as.character(table$sample_time),
    days_after_symptom_onset = as.character(
      table$sampling_day_days_after_symptom_onset
    ),
    sars_cov_2 = as.character(table$sars_cov_2),
    bcr_sequenced = as.character(table$bcr_single_cell_sequencing),
    tcr_sequenced = as.character(table$tcr_single_cell_sequencing),
    outcome = as.character(table$outcome),
    comorbidities = as.character(table$comorbidities),
    stringsAsFactors = FALSE
  )
}

.viewerRenReadCells <- function(annotation_path, clinical_path, cells) {
  annotation <- if (requireNamespace("data.table", quietly = TRUE)) {
    as.data.frame(data.table::fread(
      annotation_path,
      colClasses = "character",
      showProgress = interactive()
    ))
  } else {
    utils::read.csv(
      annotation_path,
      colClasses = "character",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  required <- c("cellName", "sampleID", "celltype", "majorType")
  if (
    nrow(annotation) != 1462702L ||
      !all(required %in% names(annotation)) ||
      anyDuplicated(annotation$cellName)
  ) {
    stop(
      "The Ren annotation must describe 1,462,702 unique cells.",
      call. = FALSE
    )
  }
  index <- match(cells, annotation$cellName)
  if (anyNA(index)) {
    stop(
      "The Ren h5ad and cell annotation barcodes do not match.",
      call. = FALSE
    )
  }
  annotation <- annotation[index, required, drop = FALSE]
  clinical <- .viewerRenReadClinical(clinical_path)
  clinical_index <- match(annotation$sampleID, clinical$sample)
  if (anyNA(clinical_index)) {
    stop(
      "The Ren cell annotations reference an unknown clinical sample.",
      call. = FALSE
    )
  }
  clinical <- clinical[clinical_index, , drop = FALSE]
  metadata <- data.frame(
    cell_barcode = cells,
    sample = as.character(annotation$sampleID),
    geo_accession = clinical$geo_accession,
    patient = clinical$patient,
    cell_type = as.character(annotation$celltype),
    major_type = as.character(annotation$majorType),
    dataset = clinical$dataset,
    tissue = clinical$tissue,
    severity = clinical$severity,
    stage = clinical$stage,
    age = clinical$age,
    sex = clinical$sex,
    days_after_symptom_onset = clinical$days_after_symptom_onset,
    sars_cov_2 = clinical$sars_cov_2,
    outcome = clinical$outcome,
    comorbidities = clinical$comorbidities,
    bcr_sequenced = clinical$bcr_sequenced,
    tcr_sequenced = clinical$tcr_sequenced,
    row.names = cells,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  categorical <- setdiff(names(metadata), "cell_barcode")
  metadata[categorical] <- lapply(metadata[categorical], factor)
  metadata
}

.viewerRenProjection <- function(h5ad, cells) {
  listing <- rhdf5::h5ls(h5ad, recursive = TRUE)
  paths <- ifelse(
    listing$group == "/",
    paste0("/", listing$name),
    paste0(listing$group, "/", listing$name)
  )
  datasets <- paths[listing$otype == "H5I_DATASET"]
  preferred <- c("/obsm/X_umap", "/obsm/X_tsne", "/obsm/X_pca")
  selected <- preferred[preferred %in% datasets]
  if (!length(selected)) {
    stop("The Ren h5ad has no supported published projection.", call. = FALSE)
  }
  selected <- selected[[1L]]
  projection <- rhdf5::h5read(h5ad, selected)
  projection <- as.matrix(projection)
  if (nrow(projection) != length(cells) && ncol(projection) == length(cells)) {
    projection <- t(projection)
  }
  if (nrow(projection) != length(cells) || ncol(projection) < 2L) {
    stop(
      "The Ren h5ad has no cell-aligned two-dimensional projection.",
      call. = FALSE
    )
  }
  projection <- data.frame(
    Dim_1 = as.numeric(projection[, 1L]),
    Dim_2 = as.numeric(projection[, 2L]),
    row.names = cells
  )
  attr(projection, "source") <- selected
  projection
}

.viewerRenProjectionName <- function(projection) {
  source <- attr(projection, "source", exact = TRUE)
  name <- if (is.character(source) && length(source) == 1L && !is.na(source)) {
    sub("^/obsm/X_", "", source)
  } else {
    ""
  }
  if (!name %in% c("umap", "tsne", "pca")) {
    stop("The Ren projection source is not supported.", call. = FALSE)
  }
  name
}

.viewerRenRepertoireChains <- function(repertoire) {
  genes <- unlist(
    lapply(repertoire, function(table) {
      as.character(table$CTgene)
    }),
    use.names = FALSE
  )
  supported <- c("TRA", "TRB", "TRG", "TRD", "IGH", "IGK", "IGL")
  supported[vapply(
    supported,
    function(chain) {
      any(grepl(chain, genes, fixed = TRUE), na.rm = TRUE)
    },
    logical(1)
  )]
}

.viewerRenReadTable <- function(path, columns) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(as.data.frame(data.table::fread(
      path,
      select = columns,
      colClasses = "character",
      na.strings = c("", "None"),
      showProgress = interactive()
    )))
  }
  table <- utils::read.delim(
    path,
    colClasses = "character",
    na.strings = c("", "None"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  table[, columns, drop = FALSE]
}

.viewerRenPaste <- function(...) {
  values <- list(...)
  values <- lapply(values, function(value) {
    value <- as.character(value)
    value[is.na(value)] <- ""
    value
  })
  result <- do.call(paste, c(values, list(sep = ".")))
  result <- gsub("[.]+", ".", result)
  gsub("^[.]|[.]$", "", result)
}

.viewerRenCanonicalRepertoire <- function(table, receptor) {
  if (identical(receptor, "TCR")) {
    gene_a <- .viewerRenPaste(
      table$TCRA_vgene,
      table$TCRA_dgene,
      table$TCRA_jgene,
      table$TCRA_cgene
    )
    gene_b <- .viewerRenPaste(
      table$TCRB_vgene,
      table$TCRB_dgene,
      table$TCRB_jgene,
      table$TCRB_cgene
    )
    nt_a <- table$TCRA_cdr3nt
    nt_b <- table$TCRB_cdr3nt
    aa_a <- table$TCRA_cdr3aa
    aa_b <- table$TCRB_cdr3aa
  } else {
    gene_a <- .viewerRenPaste(
      table$BCRH_vgene,
      table$BCRH_dgene,
      table$BCRH_jgene,
      table$BCRH_cgene
    )
    gene_b <- .viewerRenPaste(
      table[["BCRL/K_vgene"]],
      table[["BCRL/K_dgene"]],
      table[["BCRL/K_jgene"]],
      table[["BCRL/K_cgene"]]
    )
    nt_a <- table$BCRH_cdr3nt
    nt_b <- table[["BCRL/K_cdr3nt"]]
    aa_a <- table$BCRH_cdr3aa
    aa_b <- table[["BCRL/K_cdr3aa"]]
  }
  ct_gene <- .viewerRenChainPair(gene_a, gene_b)
  ct_nt <- .viewerRenChainPair(nt_a, nt_b)
  ct_aa <- .viewerRenChainPair(aa_a, aa_b)
  data.frame(
    barcode = as.character(table$cellBarcode),
    CTgene = ct_gene,
    CTnt = ct_nt,
    CTaa = ct_aa,
    CTstrict = paste(ct_gene, ct_nt, sep = ";"),
    sample = as.character(table$sampleID),
    stringsAsFactors = FALSE
  )
}

.viewerRenChainPair <- function(first, second) {
  first <- as.character(first)
  second <- as.character(second)
  first[is.na(first)] <- ""
  second[is.na(second)] <- ""
  result <- paste(first, second, sep = "_")
  result <- sub("^_", "", result)
  sub("_$", "", result)
}

.viewerRenRepertoire <- function(tcr_path, bcr_path, cells) {
  tcr_columns <- c(
    "cellBarcode",
    "sampleID",
    "TCRA_cgene",
    "TCRA_vgene",
    "TCRA_dgene",
    "TCRA_jgene",
    "TCRA_cdr3aa",
    "TCRA_cdr3nt",
    "TCRB_cgene",
    "TCRB_vgene",
    "TCRB_dgene",
    "TCRB_jgene",
    "TCRB_cdr3aa",
    "TCRB_cdr3nt"
  )
  bcr_columns <- c(
    "cellBarcode",
    "sampleID",
    "BCRH_cgene",
    "BCRH_vgene",
    "BCRH_dgene",
    "BCRH_jgene",
    "BCRH_cdr3aa",
    "BCRH_cdr3nt",
    "BCRL/K_cgene",
    "BCRL/K_vgene",
    "BCRL/K_dgene",
    "BCRL/K_jgene",
    "BCRL/K_cdr3aa",
    "BCRL/K_cdr3nt"
  )
  tcr <- .viewerRenCanonicalRepertoire(
    .viewerRenReadTable(tcr_path, tcr_columns),
    "TCR"
  )
  bcr <- .viewerRenCanonicalRepertoire(
    .viewerRenReadTable(bcr_path, bcr_columns),
    "BCR"
  )
  if (nrow(tcr) != 220968L || nrow(bcr) != 282464L) {
    stop("The Ren receptor tables have unexpected row counts.", call. = FALSE)
  }
  repertoire <- rbind(tcr, bcr)
  if (anyDuplicated(repertoire$barcode)) {
    stop(
      "The Ren receptor tables assign multiple receptors to one cell.",
      call. = FALSE
    )
  }
  if (any(!repertoire$barcode %in% cells)) {
    stop(
      "The Ren receptor tables contain unknown cell barcodes.",
      call. = FALSE
    )
  }
  samples <- repertoire$sample
  repertoire$sample <- NULL
  repertoire <- split(
    repertoire,
    factor(samples, levels = unique(samples)),
    drop = TRUE
  )
  lapply(repertoire, function(table) {
    rownames(table) <- NULL
    table
  })
}

.viewerRenMatrixGroup <- function(h5ad) {
  listing <- rhdf5::h5ls(h5ad, recursive = TRUE)
  paths <- ifelse(
    listing$group == "/",
    paste0("/", listing$name),
    paste0(listing$group, "/", listing$name)
  )
  candidates <- c("/raw/X", "/layers/counts", "/X")
  selected <- candidates[candidates %in% paths]
  if (!length(selected)) {
    stop("The Ren h5ad contains no readable expression matrix.", call. = FALSE)
  }
  sub("^/", "", selected[[1L]])
}

.viewerRenWriteSidecar <- function(h5ad, sidecar) {
  if (dir.exists(sidecar)) {
    unlink(sidecar, recursive = TRUE, force = TRUE)
  }
  dir.create(dirname(sidecar), recursive = TRUE, showWarnings = FALSE)
  message("Converting the Ren count matrix to a BPCells sidecar...")
  matrix <- BPCells::open_matrix_anndata_hdf5(
    h5ad,
    group = .viewerRenMatrixGroup(h5ad)
  )
  if (ncol(matrix) != 1462702L) {
    stop(
      "The Ren expression matrix must contain 1,462,702 cells.",
      call. = FALSE
    )
  }
  if (identical(BPCells::storage_order(matrix), "row")) {
    BPCells::write_matrix_dir(matrix, dir = sidecar)
  } else {
    BPCells::transpose_storage_order(matrix, outdir = sidecar)
  }
  BPCells::open_matrix_dir(sidecar)
}

.viewerRenBuildObject <- function(paths) {
  matrix <- .viewerRenWriteSidecar(paths$h5ad, paths$sidecar)
  cells <- colnames(matrix)
  if (
    !is.character(cells) ||
      length(cells) != 1462702L ||
      anyNA(cells) ||
      any(!nzchar(cells)) ||
      anyDuplicated(cells)
  ) {
    stop(
      "The Ren expression matrix needs 1,462,702 unique cell names.",
      call. = FALSE
    )
  }

  message("Reading Ren cell and clinical annotations...")
  metadata <- .viewerRenReadCells(paths$annotation, paths$clinical, cells)
  message("Reading the published Ren dimensional reduction...")
  projection <- .viewerRenProjection(paths$h5ad, cells)
  message("Reading 503,432 published paired TCR/BCR records...")
  repertoire <- .viewerRenRepertoire(paths$tcr, paths$bcr, cells)
  repertoire_file <- file.path(paths$sidecar, "immune_repertoire.qs2")
  qs2::qs_save(repertoire, repertoire_file)

  object <- Cerebro$new()
  object$setVersion(utils::packageVersion("CerebroNexus"))
  object$addExperiment(
    "experiment_name",
    "Ren et al. COVID-19 immune atlas (1.46M)"
  )
  object$addExperiment("organism", "Homo sapiens")
  object$addExperiment("date_of_analysis", "2021-02-04")
  object$addExperiment("date_of_export", as.character(Sys.Date()))
  object$addParameters("main_group", "major_type")
  object$addParameters("source", "Ren et al., Cell 2021; GEO GSE158055")
  object$addParameters(
    "hca_project",
    "5f607e50-ba22-4598-b1e9-f3d9d7a35dcc"
  )
  object$setExpression(matrix)
  object$setExpressionBackend("bpcells", basename(paths$sidecar))
  object$setMetaData(metadata)
  for (group in c(
    "major_type",
    "cell_type",
    "sample",
    "patient",
    "dataset",
    "tissue",
    "severity",
    "stage",
    "age",
    "sex",
    "sars_cov_2",
    "outcome"
  )) {
    values <- unique(as.character(metadata[[group]]))
    values <- sort(values[!is.na(values) & nzchar(values)], method = "radix")
    object$addGroup(group, values)
  }
  object$addProjection(.viewerRenProjectionName(projection), projection)
  object$immune_repertoire_backend <- list(
    type = "bpcells-file",
    file = basename(repertoire_file),
    md5 = unname(tools::md5sum(repertoire_file)),
    samples = names(repertoire),
    chains = .viewerRenRepertoireChains(repertoire)
  )
  object$addExtraTable(
    "data_provenance",
    data.frame(
      field = c(
        "publication",
        "geo_accession",
        "hca_project",
        "cells",
        "paired_tcr_cells",
        "paired_bcr_cells",
        "license"
      ),
      value = c(
        "Ren et al., Cell 2021",
        "GSE158055",
        "5f607e50-ba22-4598-b1e9-f3d9d7a35dcc",
        "1462702",
        "220968",
        "282464",
        "CC BY 4.0"
      ),
      stringsAsFactors = FALSE
    )
  )
  saveCerebro(object, paths$crb)
  writeLines(as.character(.viewerRenVersion), paths$stamp)
  paths$crb
}

.viewerRenComplete <- function(paths) {
  file.exists(paths$crb) &&
    !dir.exists(paths$crb) &&
    dir.exists(paths$sidecar) &&
    length(list.files(paths$sidecar, all.files = TRUE, no.. = TRUE)) > 0L &&
    file.exists(file.path(paths$sidecar, "immune_repertoire.qs2")) &&
    file.exists(paths$stamp) &&
    identical(
      readLines(paths$stamp, warn = FALSE),
      as.character(.viewerRenVersion)
    )
}

prepareViewerRenDemoData <- function(cache_dir = NULL) {
  paths <- .viewerRenPaths(cache_dir)
  if (.viewerRenComplete(paths)) {
    return(paths$crb)
  }
  .viewerRenRequire(c("BPCells", "rhdf5", "readxl"))
  manifest <- .viewerRenFiles()
  for (name in names(manifest)) {
    message("Preparing Ren source: ", basename(paths[[name]]))
    .viewerRenDownload(manifest[[name]], paths[[name]])
  }
  .viewerRenExtractH5ad(paths)
  ## Build privately. A failed preparation must not delete a working atlas,
  ## including the unversioned cache used by earlier Viewer revisions.
  stage <- tempfile(
    paste0("cerebro-v", .viewerRenVersion, "-stage-"),
    paths$root
  )
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  staged <- paths
  for (name in c("crb", "sidecar", "stamp")) {
    staged[[name]] <- file.path(stage, basename(paths[[name]]))
  }
  .viewerRenBuildObject(staged)
  if (!.viewerRenComplete(staged)) {
    stop("Ren preparation did not finish.")
  }
  target <- dirname(paths$crb)
  backup <- NULL
  if (dir.exists(target)) {
    backup <- tempfile(paste0(basename(target), "-previous-"), paths$root)
    if (!file.rename(target, backup)) {
      stop("Cannot preserve previous Ren cache.")
    }
  }
  if (!file.rename(stage, target)) {
    if (!is.null(backup)) {
      file.rename(backup, target)
    }
    stop("Cannot publish prepared Ren cache; previous cache was preserved.")
  }
  paths$crb
}
