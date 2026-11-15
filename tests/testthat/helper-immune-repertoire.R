make_ir_seurat <- function(n_genes = 30L, n_cells = 8L, with_columns = FALSE) {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  set.seed(11)
  counts <- matrix(
    rpois(n_genes * n_cells, 3),
    nrow = n_genes,
    dimnames = list(
      paste0("g", seq_len(n_genes)),
      paste0("cell", seq_len(n_cells))
    )
  )
  object <- Seurat::CreateSeuratObject(
    counts = methods::as(counts, "CsparseMatrix")
  )
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object$orig.ident <- rep(c("donorA", "donorB"), length.out = n_cells)
  object$sample <- object$orig.ident
  object$cluster <- factor(rep(c("c1", "c2"), length.out = n_cells))
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      rnorm(n_cells * 2L),
      ncol = 2L,
      dimnames = list(colnames(object), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )
  if (with_columns) {
    object$CTgene <- "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1"
    object$CTaa <- paste0(
      "CAVR",
      seq_len(n_cells),
      "F_CASSL",
      seq_len(n_cells),
      "F"
    )
    object$CTnt <- NA_character_
    object$CTstrict <- paste0(object$CTgene, ";", object$CTaa)
  }
  object
}

make_ir_repertoire <- function(object) {
  lapply(split(colnames(object), object$sample), function(barcodes) {
    data.frame(
      barcode = barcodes,
      CTgene = "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1",
      CTnt = NA_character_,
      CTaa = paste0(
        "CAVR",
        seq_along(barcodes),
        "F_CASSL",
        seq_along(barcodes),
        "F"
      ),
      CTstrict = paste0(
        "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1;",
        "CAVR",
        seq_along(barcodes),
        "F_CASSL",
        seq_along(barcodes),
        "F"
      ),
      stringsAsFactors = FALSE
    )
  })
}

export_ir <- function(object, file = tempfile(fileext = ".crb"), ...) {
  exportFromSeurat(
    object = object,
    file = file,
    experiment_name = "immune repertoire contract",
    organism = "hg",
    groups = c("sample", "cluster"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    codec = "rds",
    verbose = FALSE,
    ...
  )
  invisible(file)
}

## A scope holding the Immune repertoire encoder (immune_repertoire/encode.R)
## and parameter spec (param_spec.R),
## the viewer files in `sources`, then the definitions `names` taken from
## immune_repertoire/<file>. Pass `scope` to add definitions to an existing one.
ir_scope <- function(
  names = character(),
  file = "data.R",
  scope = NULL,
  sources = character()
) {
  if (is.null(scope)) {
    scope <- new.env(parent = asNamespace("shiny"))
    scope$`%||%` <- function(a, b) if (is.null(a)) b else a
    sys.source(
      viewer_test_path("immune_repertoire", "encode.R"),
      envir = scope,
      keep.source = FALSE
    )
    ## the analysis parameters and global controls' defaults the page reads
    sys.source(
      viewer_test_path("immune_repertoire", "param_spec.R"),
      envir = scope,
      keep.source = FALSE
    )
  }
  for (path in sources) {
    sys.source(viewer_test_path(path), envir = scope)
  }
  path <- viewer_test_path("immune_repertoire", file)
  for (name in names) {
    eval(viewer_r_assignment(path, name), scope)
  }
  scope
}
