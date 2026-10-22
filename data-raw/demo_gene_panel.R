# Build-time policy for small demonstration datasets, not a production filter.
# Run after analysis so cell identities, embeddings and full-input QC stay fixed.
# Genes that the object's analysis tables and gene lists name. A demo keeps the
# ones its matrix has, so a marker or most-expressed row can still be opened.
demo_referenced_genes <- function(object, genes) {
  named <- character()
  collect <- function(x) {
    if (is.data.frame(x)) {
      for (column in intersect(c("gene", "Gene", "genes"), names(x))) {
        named <<- c(named, as.character(x[[column]]))
      }
      if ("Genes" %in% names(x)) {
        named <<- c(
          named,
          unlist(strsplit(as.character(x[["Genes"]]), ";", fixed = TRUE))
        )
      }
    } else if (is.character(x)) {
      named <<- c(named, x)
    } else if (is.list(x)) {
      lapply(x, collect)
    }
    invisible(NULL)
  }
  for (field in c(
    "gene_lists",
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways"
  )) {
    collect(object[[field]])
  }
  intersect(genes, unique(named))
}

demo_gene_panel <- function(object, max_genes = 100L) {
  m <- object$expression
  stopifnot(
    inherits(m, "sparseMatrix"),
    ncol(m) > 0L,
    length(max_genes) == 1L,
    is.finite(max_genes),
    max_genes >= 1L,
    max_genes == as.integer(max_genes)
  )
  genes <- rownames(m)
  stopifnot(
    length(genes) == nrow(m),
    !anyNA(genes),
    !anyDuplicated(genes),
    all(nzchar(genes)),
    all(is.finite(m@x))
  )
  if (nrow(m) <= max_genes) {
    return(object)
  }

  # Preserve the Trekker picker, myelination signature and representative
  # neural, stromal and immune markers, including rare-cell markers.
  markers <- c(
    "Snap25",
    "Slc17a7",
    "Gad1",
    "Gad2",
    "Plp1",
    "Mbp",
    "Aqp4",
    "Gfap",
    "Cx3cr1",
    "C1qa",
    "Csf1r",
    "Pdgfra",
    "Prox1",
    "Mog",
    "Mag",
    "Mobp",
    "Cldn11",
    "Pecam1",
    "Kdr",
    "Rgs5",
    "Col1a1",
    "Col3a1",
    "Dcn",
    "Lum",
    "Col6a3",
    "Epcam",
    "CD3D",
    "CD3E",
    "CD4",
    "CD8A",
    "CD8B",
    "TRAC",
    "TRBC2",
    "IL7R",
    "CCR7",
    "FOXP3",
    "IL2RA",
    "MS4A1",
    "CD79A",
    "NKG7",
    "GNLY",
    "GZMB",
    "GZMK",
    "PRF1",
    "LYZ",
    "FCGR3A",
    "HLA-A",
    "HLA-B",
    "HLA-C",
    "B2M"
  )
  moran <- vapply(object$trekker$moran, function(x) x$gene, character(1))
  protected <- genes[toupper(genes) %in% toupper(c(markers, moran))]
  if (length(protected) > max_genes) {
    stop("Protected genes exceed demo budget")
  }
  squared <- m
  squared@x <- squared@x^2
  variance <- pmax(0, Matrix::rowMeans(squared) - Matrix::rowMeans(m)^2)
  # Genes named by analysis tables come next, the most variable first, until
  # the budget is used.
  referenced <- setdiff(demo_referenced_genes(object, genes), protected)
  referenced <- referenced[order(
    -variance[match(referenced, genes)],
    match(referenced, genes)
  )]
  protected <- genes[
    genes %in% c(protected, head(referenced, max_genes - length(protected)))
  ]
  detected <- Matrix::rowSums(m != 0) > 0L
  # Stable ties use input order; retain that order in the final sparse matrix.
  ranked <- genes[order(-variance, seq_along(genes))]
  ranked <- ranked[ranked %in% genes[detected]]
  keep <- head(unique(c(protected, ranked)), max_genes)
  keep <- genes[genes %in% keep]
  if (!length(keep)) {
    stop("No expressed genes available for the demo panel")
  }
  spatial <- object$spatial
  for (name in names(spatial)) {
    sm <- spatial[[name]]$expression
    if (
      !inherits(sm, "sparseMatrix") ||
        !all(keep %in% rownames(sm))
    ) {
      stop("Spatial expression does not cover the selected demo genes: ", name)
    }
    spatial[[name]]$expression <- sm[keep, , drop = FALSE]
  }
  # Clone only after validation: a rejected panel must not partly mutate input.
  result <- object$clone(deep = TRUE)
  result$expression <- m[keep, , drop = FALSE]
  result$spatial <- spatial
  result$parameters$demo_gene_panel <- list(
    max_genes = as.integer(max_genes),
    input_genes = nrow(m),
    retained_genes = length(keep),
    protected_genes = protected,
    method = "protected markers, Moran genes and genes named by analysis tables, then normalized-expression variance",
    scope = "Expression subset only; cells, full-input QC and analyses retained"
  )
  result
}

# One release-writing step used by the builders. Keep application codec defaults
# unchanged; these small, embedded demos can afford stronger build-time compression.
save_demo_crb <- function(object, file) {
  object <- demo_gene_panel(object)
  if (
    !is.null(object$expression_backend) &&
      !identical(object$expression_backend$type, "embedded")
  ) {
    stop("Demo panel writer only supports embedded expression")
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(".demo-panel-", dirname(file), fileext = ".crb")
  on.exit(unlink(stage), add = TRUE)
  CerebroNexus::saveCerebro(object, stage, viewer_binary = "never")
  payload <- qs2::qs_read(stage)
  qs2::qs_save(payload, stage, compress_level = 19L, nthreads = 1L)
  stopifnot(identical(
    serialize(payload, NULL, version = 3L),
    serialize(qs2::qs_read(stage), NULL, version = 3L)
  ))
  if (!file.copy(stage, file, overwrite = TRUE)) {
    stop("Cannot write demo: ", file)
  }
  invisible(file)
}
