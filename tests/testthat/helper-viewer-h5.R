## Writes genes x cells `values` (with dimnames) to `path` the way
## exportFromSeurat() does: cells x genes TENx layout under /expression, so
## /expression/genes holds the cell names and /expression/barcodes the genes.
viewer_h5_matrix <- function(values, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) {
    ## An earlier read may still hold the file open; Windows then refuses to
    ## replace it.
    gc(verbose = FALSE)
    if (requireNamespace("rhdf5", quietly = TRUE)) {
      rhdf5::h5closeAll()
    }
    if (!file.remove(path)) {
      stop("Cannot replace the H5 test file ", path, call. = FALSE)
    }
  }
  HDF5Array::writeTENxMatrix(
    methods::as(Matrix::Matrix(t(values), sparse = TRUE), "CsparseMatrix"),
    path,
    group = "expression",
    level = 0L
  )
  invisible(path)
}

## Small H5-backed thin CRB for Viewer tests. `values` is genes x cells with
## dimnames; the matrix goes to `<stem>.h5` next to the CRB the way
## exportFromSeurat() writes it (cells x genes TENx layout, uncompressed).
viewer_h5_fixture <- function(
  root,
  values = NULL,
  stem = "fixture",
  projections = c("umap"),
  metadata = NULL,
  save = TRUE
) {
  if (is.null(values)) {
    values <- matrix(
      c(0, 1, 4, 2, 0, 5, 3, 6, 0, 7, 8, 9),
      nrow = 3L,
      dimnames = list(
        c("g1", "g2", "g3"),
        c("c1", "c2", "c3", "c4")
      )
    )
  }
  cells <- colnames(values)
  genes <- rownames(values)
  h5 <- file.path(root, paste0(stem, ".h5"))
  crb <- file.path(root, paste0(stem, ".crb"))
  viewer_h5_matrix(values, h5)

  object <- Cerebro$new()
  object$setExpressionBackend("h5", basename(h5))
  if (is.null(metadata)) {
    metadata <- data.frame(
      cell_barcode = cells,
      sample = factor(rep(c("a", "b"), length.out = length(cells))),
      row.names = cells
    )
  }
  object$setMetaData(metadata)
  object$addGroup("sample", levels(metadata$sample))
  ## A character vector names canonical projections; a named list of data
  ## frames is stored as given (for example with shuffled row names).
  if (is.character(projections)) {
    projections <- stats::setNames(
      lapply(projections, function(name) {
        data.frame(
          x = seq_along(cells),
          y = rev(seq_along(cells)),
          row.names = cells
        )
      }),
      projections
    )
  }
  for (name in names(projections)) {
    object$addProjection(name, projections[[name]])
  }
  if (save) {
    saveCerebro(object, crb, viewer_binary = "never")
  }
  list(
    crb = crb,
    h5 = h5,
    cells = cells,
    genes = genes,
    values = values,
    object = object
  )
}

## ---- CRB fixtures shared by the reader and bundler tests ------------------ ##

new_embedded_cerebro <- function() {
  object <- Cerebro$new()
  object$setExpressionBackend(type = "embedded")
  object
}

h5_cells <- function(prefix = "cell-") paste0(prefix, seq_len(4L))

h5_counts <- function(cells = h5_cells()) {
  matrix(
    c(0, 1, 4, 2, 0, 5, 3, 6, 0, 7, 8, 9),
    nrow = 3L,
    dimnames = list(paste0("gene-", seq_len(3L)), cells)
  )
}


## A serialized Cerebro object as an older release wrote it: the bindings of
## `source` without `drop`, plus `extra` (the class no longer declares bcr_data
## / tcr_data, so they can only be added to a hand-built payload). Locked like an
## R6 object unless `lock = FALSE`.
legacy_payload <- function(
  extra = list(),
  drop = character(),
  cls = NULL,
  source = Cerebro$new(),
  lock = TRUE
) {
  payload <- new.env(parent = emptyenv())
  for (binding in setdiff(ls(source, all.names = TRUE), drop)) {
    payload[[binding]] <- source[[binding]]
  }
  for (field in names(extra)) {
    payload[[field]] <- extra[[field]]
  }
  class(payload) <- if (is.null(cls)) class(source) else cls
  if (lock) {
    lockEnvironment(payload, bindings = FALSE)
  }
  payload
}

## Each entry is a CRB that older releases wrote and this version does not read.
retired_cerebros <- function(root) {
  cells <- h5_cells()
  embedded <- list(type = "embedded", location = NULL)
  tcr <- list(
    a = data.frame(
      barcode = cells[1:2],
      CTgene = rep("TRAV1.TRAJ1.TRAC_TRBV1.TRBJ1.TRBC1", 2L),
      stringsAsFactors = FALSE
    ),
    b = data.frame(
      barcode = cells[3L],
      CTgene = "TRBV2.TRBJ2.TRBC2",
      stringsAsFactors = FALSE
    )
  )
  bcr <- list(
    b = data.frame(
      barcode = cells[4L],
      CTgene = "IGHV1.IGHJ1.IGHM_IGKV1.IGKJ1.IGKC",
      stringsAsFactors = FALSE
    )
  )
  meta_data <- data.frame(
    cell_barcode = cells,
    sample = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )
  counts <- h5_counts(cells)
  base <- list(
    expression = counts,
    meta_data = meta_data,
    expression_backend = embedded
  )
  with_base <- function(...) utils::modifyList(base, list(...))
  ## Spatial entries as older releases stored them.
  fov_entry <- function(...) {
    c(
      list(
        coordinates = data.frame(
          x = c(10, 20, 30, 40),
          y = c(5, 15, 25, 35),
          row.names = cells
        ),
        expression = counts
      ),
      list(...)
    )
  }
  tissue <- "data:image/png;base64,AA=="
  molecules <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6),
    gene = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )
  old_getter <- function(name) NULL
  environment(old_getter) <- baseenv()

  entries <- list(
    no_descriptor = list(
      payload = legacy_payload(list(
        expression = counts,
        meta_data = meta_data
      )),
      reason = "no expression-backend descriptor",
      upgrades = TRUE
    ),
    absent_descriptor = list(
      payload = legacy_payload(
        list(expression = counts, meta_data = meta_data),
        drop = c("expression_backend", "getExpressionBackend")
      ),
      reason = "no expression-backend descriptor",
      upgrades = TRUE
    ),
    schema_v1 = list(
      payload = legacy_payload(with_base(
        crb_schema = list(version = 1L, cell_names = "expression")
      )),
      reason = "schema version 1",
      upgrades = FALSE
    ),
    schema_v2 = list(
      payload = legacy_payload(with_base(
        crb_schema = list(version = 2L, n_cells = 4L)
      )),
      reason = "schema version 2",
      upgrades = FALSE
    ),
    bcr_tcr_data = list(
      payload = legacy_payload(with_base(tcr_data = tcr, bcr_data = bcr)),
      reason = "bcr_data/tcr_data",
      upgrades = TRUE
    ),
    bcr_data = list(
      payload = legacy_payload(with_base(bcr_data = bcr)),
      reason = "bcr_data/tcr_data",
      upgrades = TRUE
    ),
    tcr_data = list(
      payload = legacy_payload(with_base(tcr_data = tcr)),
      reason = "bcr_data/tcr_data",
      upgrades = TRUE
    ),
    retired_immune = list(
      payload = legacy_payload(with_base(
        immune_repertoire_backend = list(
          type = "retired-file",
          file = "immune_repertoire.qs2",
          md5 = strrep("a", 32L),
          samples = "a",
          chains = "TRB"
        )
      )),
      reason = "retired layout",
      upgrades = FALSE
    ),
    pre_spatial = list(
      payload = legacy_payload(
        with_base(),
        drop = c(
          "spatial",
          "addSpatialData",
          "availableSpatial",
          "getSpatialData"
        ),
        cls = c(paste0("Cerebro_v1", ".3"), "R6")
      ),
      reason = "predates spatial support",
      upgrades = TRUE
    ),
    singular_image = list(
      payload = legacy_payload(with_base(
        spatial = list(
          fov = fov_entry(
            histology_image = tissue,
            histology_image_bounds = list(
              ymax = 40,
              xmin = 0,
              ymin = 0,
              xmax = 100
            )
          )
        )
      )),
      reason = "spatial entry 'fov' stores a singular histology_image",
      upgrades = TRUE
    ),
    singular_bounds_only = list(
      payload = legacy_payload(with_base(
        spatial = list(
          fov = fov_entry(
            histology_images = list(
              DAPI = list(histology_image = tissue)
            ),
            histology_image_bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
          )
        )
      )),
      reason = "spatial entry 'fov' stores a singular histology_image",
      upgrades = TRUE
    ),
    embedded_molecules = list(
      payload = legacy_payload(with_base(
        spatial = list(
          fov = fov_entry(molecules = molecules)
        )
      )),
      reason = "spatial entry 'fov' embeds its molecule table",
      upgrades = TRUE
    ),
    eager_spatial_getter = list(
      payload = legacy_payload(c(
        with_base(spatial = list(fov = fov_entry())),
        list(getSpatialData = old_getter)
      )),
      reason = "predates deferred spatial molecule loading",
      upgrades = TRUE
    ),
    pre_hla = list(
      payload = legacy_payload(
        with_base(),
        drop = c("hla_typing", "addHLATyping", "getHLATyping")
      ),
      reason = "predates HLA typing support",
      upgrades = TRUE
    ),
    pre_trekker = list(
      payload = legacy_payload(
        with_base(),
        drop = c("trekker", "addTrekker", "getTrekker")
      ),
      reason = "predates Trekker support",
      upgrades = TRUE
    ),
    pre_enriched_pathways = list(
      payload = legacy_payload(
        with_base(),
        drop = "getMethodsForEnrichedPathways"
      ),
      reason = "predates the enriched-pathways page",
      upgrades = TRUE
    )
  )
  for (name in names(entries)) {
    path <- file.path(root, paste0(name, ".crb"))
    saveRDS(entries[[name]]$payload, path)
    entries[[name]]$path <- path
  }
  ## A backend type this version does not know is not a retired format with a
  ## named reason: the readers report an invalid descriptor and convertCerebro()
  ## cannot repair it.
  unknown_backend <- file.path(root, "unknown_backend.crb")
  saveRDS(
    legacy_payload(with_base(
      expression_backend = list(type = "retired-store", location = "x.retired"),
      expression = NULL
    )),
    unknown_backend
  )
  list(
    entries = entries,
    unknown_backend = unknown_backend,
    tcr = tcr,
    bcr = bcr,
    counts = counts
  )
}
