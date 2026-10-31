## The HLA core lives in the package R/ directory: source it from a checkout,
## or take it from the installed namespace. Returns the sourced files.
builder_test_source_hla_core <- function(local = parent.frame()) {
  files <- testthat::test_path(
    "..",
    "..",
    "R",
    c("hla_typing.R", "hla_motif_core.R", "hla_association_core.R")
  )
  if (all(file.exists(files))) {
    for (file in files) {
      sys.source(file, envir = local)
    }
    return(invisible(files))
  }
  list2env(
    utils::getFromNamespace(".viewerHLACore", "CerebroNexus")(),
    envir = local
  )
  invisible(character())
}

builder_content_immune_source_runtime <- function(local = parent.frame()) {
  hla_core <- builder_test_source_hla_core(local)
  content <- builder_content_immune_inst_path(
    "builder",
    "content_immune.R"
  )
  if (file.exists(content)) {
    sys.source(content, envir = local)
  }
  invisible(c(hla_core, content))
}

builder_immune_fixture_object <- function(n = 8L) {
  cells <- paste0("cell", seq_len(n))
  counts <- Matrix::Matrix(
    matrix(
      seq_len(5L * n),
      nrow = 5L,
      dimnames = list(paste0("G", seq_len(5L)), cells)
    ),
    sparse = TRUE
  )
  object <- SeuratObject::CreateSeuratObject(counts)
  object$orig.ident <- rep(c("sample_a", "sample_b"), length.out = n)
  object$sample <- object$orig.ident
  object
}

builder_immune_fixture_context <- function(object) {
  list(
    cells = SeuratObject::Cells(object),
    features = SeuratObject::Features(object),
    metadata = object@meta.data,
    assays = names(object@assays),
    default_assay = SeuratObject::DefaultAssay(object),
    groups = c("orig.ident", "sample"),
    reductions = names(object@reductions),
    source = list(type = "example", location = "immune-fixture")
  )
}

builder_immune_fixture_table <- function(
  barcodes,
  chain = "TRB",
  suffix = "one"
) {
  prefixes <- c(
    TRA = "TRAV1.TRAJ1",
    TRB = "TRBV1.TRBJ1",
    TRG = "TRGV1.TRGJ1",
    TRD = "TRDV1.TRDJ1",
    IGH = "IGHV1.IGHJ1",
    IGK = "IGKV1.IGKJ1",
    IGL = "IGLV1.IGLJ1"
  )
  gene <- unname(prefixes[chain])
  if (!length(gene) || is.na(gene)) {
    gene <- paste0(chain, "V1.", chain, "J1")
  }
  data.frame(
    barcode = barcodes,
    CTgene = rep(gene, length(barcodes)),
    CTnt = paste0("NT_", suffix, "_", seq_along(barcodes)),
    CTaa = paste0("AA_", suffix, "_", seq_along(barcodes)),
    CTstrict = paste0(chain, "_", suffix, "_", seq_along(barcodes)),
    stringsAsFactors = FALSE
  )
}

builder_immune_fixture_all_modalities <- function() {
  object <- builder_immune_fixture_object(300L)
  cells <- SeuratObject::Cells(object)
  sample_names <- c("sample_a", "sample_b")
  sample_cells <- split(
    cells,
    rep(sample_names, length.out = length(cells))
  )
  object@misc$immune_repertoire <- list(
    sample_a = builder_immune_fixture_table(
      sample_cells$sample_a,
      chain = "TRA",
      suffix = "all-a"
    ),
    sample_b = builder_immune_fixture_table(
      sample_cells$sample_b,
      chain = "TRB",
      suffix = "all-b"
    )
  )

  typing <- expand.grid(
    sample = sample_names,
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
  object@misc$hla_typing <- typing[,
    c("sample", "donor_id", "locus", "copy", "allele", "resolution")
  ]
  object@misc$hla_typing_source_type <- "synthetic"
  object
}

builder_immune_fixture_viewer_demo <- function() {
  path <- builder_content_immune_inst_path(
    "extdata",
    "examples",
    "demo_hla_tcr_dextramer.crb"
  )
  if (!file.exists(path)) {
    return(NULL)
  }
  readCerebro(path)
}

builder_immune_expect_record_contract <- function(record) {
  expect_type(record, "list")
  expect_true(is.logical(record$detected))
  expect_length(record$detected, 1L)
  expect_false(is.na(record$detected))
  expect_true(is.logical(record$valid))
  expect_length(record$valid, 1L)
  expect_false(is.na(record$valid))
  expect_true(
    is.null(record$normalized) ||
      is.list(record$normalized) ||
      is.data.frame(record$normalized)
  )
  expect_type(record$diagnostics, "character")
  expect_false(anyNA(record$diagnostics))
  expect_type(record$requirements, "character")
  expect_false(anyNA(record$requirements))
  expect_type(record$page_candidates, "character")
  expect_false(anyNA(record$page_candidates))
  expect_identical(anyDuplicated(record$page_candidates), 0L)
}
