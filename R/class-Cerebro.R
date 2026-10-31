setOldClass(Classes = 'package_version')

## The methods of a Cerebro object are saved inside every .crb together with
## the environment they are enclosed in, which reaches the CerebroNexus
## namespace. A createShinyApp() bundle never loads that namespace, so a method
## cannot call a package function there. `getSpatialData()` checks the images
## with the same functions `addSpatialData()` uses, so the object carries a copy
## of them whose environment holds only base R and the helpers they call.
.cerebroStandaloneSpatialImages <- function() {
  helpers <- list(
    .spatialImageBounds = .spatialImageBounds,
    .spatialImageLabels = .spatialImageLabels,
    .spatialImageRoiScope = .spatialImageRoiScope,
    .normalizeEmbeddedSpatialImages = .normalizeEmbeddedSpatialImages
  )
  env <- list2env(helpers, parent = baseenv())
  for (name in names(helpers)) {
    environment(env[[name]]) <- env
  }
  env[[".normalizeEmbeddedSpatialImages"]]
}

#' @title
#' R6 class in which data sets will be stored for visualization in Cerebro.
#'
#' @description
#' A \code{Cerebro} object is an R6 class that contains several types of
#' data that can be visualized in Cerebro.
#'
#' @return
#' A new \code{Cerebro} object.
#'
#' @importFrom R6 R6Class
#
#' @export
#
Cerebro <- R6::R6Class(
  'Cerebro',

  ## public fields and methods
  public = list(
    #' @field version Exporter package version that was used to create the object.
    version = c(),

    #' @field experiment \code{list} that contains meta data about the data set,
    #' including experiment name, species, date of export.
    experiment = list(),

    #' @field technical_info \code{list} that contains technical information
    #' about the analysis, including the R session info.
    technical_info = list(),

    #' @field parameters \code{list} that contains important parameters that
    #' were used during the analysis, e.g. cut-off values for cell filtering.
    parameters = list(),

    #' @field groups \code{list} that contains specified grouping variables and
    #' and the group levels (subgroups) that belong to each of them. For each
    #' grouping variable, a corresponding column with the same name must exist
    #' in the meta data.
    groups = list(),

    #' @field cell_cycle \code{vector} that contains the name of columns in the
    #' meta data that contain cell cycle assignments.
    cell_cycle = c(),

    #' @field gene_lists \code{list} that contains gene lists, e.g.
    #' mitochondrial and/or ribosomal genes.
    gene_lists = list(),

    #' @field expression \code{matrix}-like object that holds transcript counts.
    expression = NULL,

    #' @field expression_backend \code{list} describing where the expression
    #' matrix is stored: \code{list(type = "embedded", location = NULL)} when it
    #' travels inside the \code{.crb}, or \code{list(type = "h5", location =
    #' "<stem>.h5")} for an H5 file next to it.
    expression_backend = NULL,

    #' @field crb_schema Description of fields omitted from a thin CRB and
    #' restored from its H5 expression sidecar. Embedded CRBs leave this as
    #' \code{NULL}.
    crb_schema = NULL,

    #' @field spatial_molecule_backend Descriptor for molecule tables stored in
    #' a sibling spatial sidecar. The runtime-only `root` member is populated
    #' by `readCerebro()` or the generated Viewer. A stored CRB keeps its
    #' molecule tables only in this sidecar.
    spatial_molecule_backend = NULL,

    #' @field immune_repertoire_backend Descriptor for a large immune
    #' repertoire stored in a sibling file (\code{type = "file"}). The
    #' runtime-only `root` member is attached when the CRB is opened; without a
    #' descriptor the repertoire is the embedded `immune_repertoire` field.
    immune_repertoire_backend = NULL,

    #' @field cell_fingerprint Stable identity of the cell population used by
    #' portable saved views. Embedded CRBs leave this as \code{NULL}; the Viewer
    #' then derives it from the cell names.
    cell_fingerprint = NULL,

    #' @field meta_data \code{data.frame} that contains cell meta data.
    meta_data = data.frame(),

    #' @field projections \code{list} that contains projections/dimensional
    #' reductions.
    projections = list(),

    #' @field most_expressed_genes \code{list} that contains a \code{data.frame}
    #' holding the most expressed genes for each grouping variable that was
    #' specified during the call to \code{\link{getMostExpressedGenes}}.
    most_expressed_genes = list(),

    #' @field mean_expression \code{list} that contains a \code{data.frame}
    #' holding the mean expression per gene for each grouping variable.
    mean_expression = list(),

    #' @field marker_genes \code{list} that contains a \code{list} for every
    #' method that was used to calculate marker genes, and a \code{data.frame}
    #' for each grouping variable, e.g. those that were specified during the
    #' call to \code{\link{getMarkerGenes}}.
    marker_genes = list(),

    #' @field enriched_pathways \code{list} that contains a \code{list} for
    #' every method that was used to calculate marker genes, and a
    #' \code{data.frame} for each grouping variable, e.g. those that were
    #' specified during the call to \code{\link{getEnrichedPathways}} or
    #' \code{\link{performGeneSetEnrichmentAnalysis}}.
    enriched_pathways = list(),

    #' @field trees \code{list} that contains a phylogenetic tree (class
    #' \code{phylo}) for grouping variables.
    trees = list(),

    #' @field trajectories \code{list} that contains a \code{list} for every
    #' method that was used to calculate trajectories, and, depending on the
    #' method, a \code{data.frame} or \code{list} for each specific trajectory,
    #' e.g. those extracted with \code{\link{extractMonocleTrajectory}}.
    trajectories = list(),

    #' @field extra_material \code{list} that can contain additional material
    #' related to the data set; tables should be stored in \code{data.frame}
    #' format in a named \code{list} called `tables`
    extra_material = list(),

    #' @field immune_repertoire \code{list} of data.frames (one per sample)
    #'   containing scRepertoire columns (CTgene, CTnt, CTaa, CTstrict, etc.).
    immune_repertoire = list(),

    #' @field hla_typing canonical HLA typing \code{data.frame} (long: one row
    #'   per sample x locus x copy) with data provenance, or NULL. Consumed by
    #'   the HLA & TCR Motifs page for donor-level HLA context. Optional.
    hla_typing = NULL,

    #' @field spatial \code{list} that contains spatial data (coordinates and expression).
    spatial = list(),

    #' @field trekker \code{list} with Trekker single-cell spatial-mapping
    #'   content: canonical and variant (transposed / y-mirrored) coordinates,
    #'   UMAP coordinates, per-nucleus cluster/cell-type, positioning QC in the
    #'   vendor's original field names, the upstream (vendor) Moran's I table,
    #'   and per-nucleus positioning-evidence images (base64 \code{data:} URIs).
    #'   Consumed by the Trekker page. Optional.
    trekker = NULL,

    ##------------------------------------------------------------------------##
    ## methods to interact with the object
    ##------------------------------------------------------------------------##

    #' @description
    #' Create a new \code{Cerebro} object.
    #'
    #' @return
    #' A new \code{Cerebro} object.
    initialize = function() {
      private$normalizeSpatialImages <- .cerebroStandaloneSpatialImages()
      self$experiment <- list(
        experiment_name = NULL,
        organism = NULL,
        date_of_analysis = NULL,
        date_of_export = NULL
      )
    },

    #' @description
    #' Set the exporter package version that was used to generate this
    #' object.
    #'
    #' @param version Version to set.
    setVersion = function(version) {
      self$version <- version
    },

    #' @description
    #' Get the exporter package version that was used to generate this
    #' object.
    #'
    #' @return
    #' Version as \code{package_version} class.
    getVersion = function() {
      return(self$version)
    },

    #' @description
    #' Safety function that will check if a provided group name is present in
    #' the \code{groups} field.
    #'
    #' @param group_name Group name to be tested
    checkIfGroupExists = function(group_name) {
      if (group_name %in% names(self$groups) == FALSE) {
        stop(
          glue::glue('Group `{group_name}` not present in `groups` attribute.'),
          call. = FALSE
        )
      }
    },

    #' @description
    #' Safety function that will check if a provided group name is present in
    #' the meta data.
    #'
    #' @param group_name Group name to be tested.
    checkIfColumnExistsInMetadata = function(group_name) {
      if (group_name %in% colnames(self$meta_data) == FALSE) {
        stop(
          glue::glue('Group `{group_name}` not present in meta data.'),
          call. = FALSE
        )
      }
    },

    #' @description
    #' Add information to \code{experiment} field.
    #'
    #' @param field Name of the information, e.g. \code{organism}.
    #' @param content Actual information, e.g. \code{hg}.
    addExperiment = function(field, content) {
      self$experiment[[field]] <- content
    },

    #' @description
    #' Retrieve information from \code{experiment} field.
    #'
    #' @return
    #' \code{list} of all entries in the \code{experiment} field.
    getExperiment = function() {
      return(self$experiment)
    },

    #' @description
    #' Add information to \code{parameters} field.
    #'
    #' @param field Name of the information, e.g. \code{number_of_PCs}.
    #' @param content Actual information, e.g. \code{30}.
    addParameters = function(field, content) {
      self$parameters[[field]] <- content
    },

    #' @description
    #' Retrieve information from \code{parameters} field.
    #'
    #' @return
    #' \code{list} of all entries in the \code{parameters} field.
    getParameters = function() {
      return(self$parameters)
    },

    #' @description
    #' Add information to \code{technical_info} field.
    #'
    #' @param field Name of the information, e.g. \code{R}.
    #' @param content Actual information, e.g. \code{4.0.2}.
    addTechnicalInfo = function(field, content) {
      self$technical_info[[field]] <- content
    },

    #' @description
    #' Retrieve information from \code{technical_info} field.
    #'
    #' @return
    #' \code{list} of all entries in the \code{technical_info} field.
    getTechnicalInfo = function() {
      return(self$technical_info)
    },

    #' @description
    #' Add group to the groups registered in the \code{groups} field.
    #'
    #' @param group_name Group name.
    #' @param levels \code{vector} of group levels (subgroups).
    addGroup = function(group_name, levels) {
      self$checkIfColumnExistsInMetadata(group_name)
      self$groups[[group_name]] <- levels
    },

    #' @description
    #' Retrieve all names in the \code{groups} field.
    #'
    #' @return
    #' \code{vector} of registered groups.
    getGroups = function() {
      return(names(self$groups))
    },

    #' @description
    #' Retrieve group levels for a group registered in the \code{groups} field.
    #'
    #' @param group_name Group name for which to retrieve group levels.
    #'
    #' @return
    #' \code{vector} of group levels.
    getGroupLevels = function(group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$groups[[group_name]])
    },

    #' @description
    #' Set meta data for cells.
    #'
    #' @param table \code{data.frame} that contains meta data for cells. The
    #' number of rows must be equal to the number of rows of projections and
    #' the number of columns in the transcript count matrix.
    setMetaData = function(table) {
      if (!is.data.frame(table) && !inherits(table, "DFrame")) {
        stop("Meta data must be a data frame or DFrame.")
      }
      if (inherits(table, "DFrame")) {
        table <- as.data.frame(table)
      }

      if (!is.null(self$expression)) {
        if (nrow(table) != ncol(self$expression)) {
          stop(glue::glue(
            "Number of rows in meta data ({nrow(table)}) must match number of columns in expression matrix ({ncol(self$expression)})."
          ))
        }
      }
      self$meta_data <- table
    },

    #' @description
    #' Retrieve meta data for cells.
    #'
    #' @return
    #' \code{data.frame} containing meta data.
    getMetaData = function() {
      return(self$meta_data)
    },

    #' @description
    #' Add a gene list to the \code{gene_lists}.
    #'
    #' @param name Name of the gene list.
    #' @param genes \code{vector} of genes.
    addGeneList = function(name, genes) {
      self$gene_lists[[name]] <- genes
    },

    #' @description
    #' Retrieve gene lists from the \code{gene_lists}.
    #'
    #' @return
    #' \code{list} of all entries in the \code{gene_lists} field.
    getGeneLists = function() {
      return(self$gene_lists)
    },

    #' @description
    #' Set transcript count matrix.
    #'
    #' @param counts \code{matrix}-like object that contains transcript counts
    #' for cells in the data set. Number of columns must be equal to the number
    #' of rows in the \code{meta_data} field.
    #' @param backend Optional backend tag. If left \code{NULL} the object is
    #' tagged \code{"embedded"} (the matrix lives inside the \code{.crb}
    #' itself). Callers exporting with step-7.2 external-storage modes should
    #' pass \code{setExpressionBackend()} directly instead of relying on this
    #' default.
    setExpression = function(counts, backend = NULL) {
      if (
        !inherits(
          counts,
          c(
            "matrix",
            "dgCMatrix",
            "RleMatrix",
            "DelayedMatrix"
          )
        )
      ) {
        warning(
          "Expression data should ideally be a matrix-like object (matrix, dgCMatrix, RleMatrix, etc)."
        )
      }
      if (!is.null(self$meta_data) && nrow(self$meta_data) > 0) {
        if (ncol(counts) != nrow(self$meta_data)) {
          stop(glue::glue(
            "Number of columns in expression matrix ({ncol(counts)}) must match number of rows in meta data ({nrow(self$meta_data)})."
          ))
        }
      }
      self$expression <- counts
      if (is.null(backend)) {
        self$setExpressionBackend(type = "embedded")
      }
    },

    #' @description
    #' Tag the object with information about how / where its expression matrix
    #' is stored: \code{"embedded"} with a NULL location when the matrix is
    #' carried inside the serialised \code{.crb}, or \code{"h5"} with the name
    #' of the H5 file that sits next to it.
    #'
    #' @param type Storage backend label, \code{"embedded"} or \code{"h5"}.
    #' @param location File name of the H5 file next to the \code{.crb};
    #' \code{NULL} when \code{type == "embedded"}.
    #' @param md5 MD5 checksum of the H5 file, recorded when it is written and
    #' checked when it is attached; \code{NULL} when not recorded.
    setExpressionBackend = function(
      type = "embedded",
      location = NULL,
      md5 = NULL
    ) {
      allowed <- c("embedded", "h5")
      if (length(type) != 1L || !is.character(type) || !(type %in% allowed)) {
        stop(
          "`type` must be one of: ",
          paste(allowed, collapse = ", "),
          call. = FALSE
        )
      }
      if (type != "embedded" && is.null(location)) {
        stop(
          "External expression backends (type = '",
          type,
          "') require a non-NULL `location`.",
          call. = FALSE
        )
      }
      if (type == "embedded" && !is.null(location)) {
        stop(
          "`location` must be NULL when type = 'embedded'.",
          call. = FALSE
        )
      }
      if (
        !is.null(md5) &&
          (type == "embedded" || !.validMd5(md5))
      ) {
        stop(
          "`md5` must be one MD5 checksum of an external expression file.",
          call. = FALSE
        )
      }
      self$expression_backend <- list(type = type, location = location)
      if (!is.null(md5)) self$expression_backend$md5 <- md5
    },

    #' @description
    #' Read the expression backend tag. Returns a \code{list(type, location)};
    #' an object that has not been tagged yet counts as embedded.
    getExpressionBackend = function() {
      if (is.null(self$expression_backend)) {
        return(list(type = "embedded", location = NULL))
      }
      self$expression_backend
    },

    #' @description
    #' Get names of all cells.
    #'
    #' @return
    #' \code{vector} containing all cell names/barcodes.
    getCellNames = function() {
      return(colnames(self$expression))
    },

    #' @description
    #' Get names of all genes in transcript count matrix.
    #'
    #' @return
    #' \code{vector} containing all gene names in transcript count matrix.
    getGeneNames = function() {
      return(rownames(self$expression))
    },

    #' @description
    #' Retrieve mean expression across all cells in the data set for a set of
    #' genes.
    #'
    #' @param genes Names of genes to extract; no default.
    #'
    #' @return
    #' \code{data.frame} containing specified gene names and their respective
    #' mean expression across all cells in the data set.
    getMeanExpressionForGenes = function(genes) {
      ## Keep the expression block in the backend's native representation
      ## instead of routing through extractExpression(), which intentionally
      ## returns a dense base matrix for backward compatibility.
      mat <- self$getExpressionBlock(genes = genes, cells = NULL)

      ## calculate mean expression per gene with the backend-aware rowMeans
      if (inherits(mat, "DelayedArray")) {
        mean_expression <- DelayedArray::rowMeans(mat)
      } else {
        mean_expression <- Matrix::rowMeans(mat)
      }

      ##
      return(
        data.frame(
          "gene" = genes,
          "expression" = unname(mean_expression)
        )
      )
    },

    #' @description
    #' Retrieve (mean) expression for a single gene or a set of genes for a
    #' given set of cells.
    #'
    #' @param cells Names/barcodes or one-based column indices of cells to
    #' extract; defaults to \code{NULL}, which will return all cells.
    #' @param genes Names of genes to extract; defaults to \code{NULL}, which
    #' will return all genes.
    #'
    #' @return
    #' \code{vector} containing (mean) expression across all specified genes in
    #' each specified cell.
    getMeanExpressionForCells = function(cells = NULL, genes = NULL) {
      if (is.null(genes)) {
        genes <- rownames(self$expression)
      }

      ## Preserve the historical named NaN result for an empty gene set.
      if (length(genes) == 0L) {
        mat <- private$extractExpression(cells = cells, genes = genes)
        return(Matrix::colMeans(mat))
      }

      mat <- self$getExpressionBlock(genes = genes, cells = cells)
      if (ncol(mat) == 0L) {
        return(numeric())
      }

      if (inherits(mat, "DelayedArray")) {
        mean_expression <- DelayedArray::colMeans(mat)
      } else {
        mean_expression <- Matrix::colMeans(mat)
      }

      return(mean_expression)
    },

    #' @description
    #' Retrieve transcript count matrix.
    #'
    #' @param cells Names/barcodes or one-based column indices of cells to
    #' extract; defaults to \code{NULL}, which will return all cells.
    #' @param genes Names of genes to extract; defaults to \code{NULL}, which
    #' will return all genes.
    #'
    #' @return
    #' Dense transcript count matrix for specified cells and genes.
    getExpressionMatrix = function(cells = NULL, genes = NULL) {
      return(private$extractExpression(cells = cells, genes = genes))
    },

    #' @description
    #' Retrieve a single row of the expression matrix as a named numeric vector
    #' WITHOUT going through the dense helper. Prefer this over
    #' \code{getExpressionMatrix(genes = gene)} on large or sparse backends
    #' where materialising a 1 x N dense matrix first is wasteful.
    #'
    #' @param gene Name of a single gene. Must exist in the matrix.
    #' @param cells Names/barcodes or one-based column indices of cells to
    #' extract; \code{NULL} returns all cells.
    #' @return
    #' Named \code{numeric} vector, one entry per requested cell.
    getExpressionRow = function(gene, cells = NULL) {
      if (length(gene) != 1L || is.na(gene) || !is.character(gene)) {
        stop("`gene` must be a single non-NA character value.", call. = FALSE)
      }
      cell_selection <- private$resolveExpressionCells(cells)
      cell_idx <- cell_selection$indices
      cells <- cell_selection$names

      gene_idx <- match(gene, rownames(self$expression))
      if (is.na(gene_idx)) {
        stop(
          "Gene '",
          gene,
          "' not found in expression matrix.",
          call. = FALSE
        )
      }
      ## DelayedArray family (incl. RleMatrix): extract_array with indices
      ## avoids touching cells outside the requested subset.
      if (inherits(self$expression, "DelayedArray")) {
        delayed_idx <- if (is.null(cell_idx)) {
          seq_len(ncol(self$expression))
        } else {
          cell_idx
        }
        mat <- DelayedArray::extract_array(
          self$expression,
          list(gene_idx, delayed_idx)
        )
        out <- as.numeric(mat)
        if (!is.null(cell_selection$restore)) {
          out <- out[cell_selection$restore]
        }
        names(out) <- cells
        return(out)
      }

      ## dgCMatrix / base matrix: [gene, cells] already returns a named
      ## numeric vector without densifying the full matrix.
      out <- if (is.null(cell_idx)) {
        as.numeric(self$expression[gene_idx, ])
      } else {
        as.numeric(self$expression[gene_idx, cell_idx])
      }
      names(out) <- cells
      return(out)
    },

    #' @description
    #' Retrieve a genes x cells sub-matrix in the backend's NATIVE form
    #' (sparse / lazy). Callers that need a dense base matrix must apply
    #' \code{as.matrix()} themselves. Use this to keep sparse-aware downstream
    #' operations (\code{Matrix::rowMeans}, \code{Matrix::colMeans}, etc.)
    #' fast instead of densifying just to aggregate.
    #'
    #' @param genes Non-empty character vector of gene names.
    #' @param cells Names/barcodes or one-based column indices of cells to
    #' extract; \code{NULL} returns all cells.
    #' @return
    #' A sub-matrix of the same concrete class as \code{self$expression}:
    #' \code{dgCMatrix} stays \code{dgCMatrix}, \code{RleMatrix} yields
    #' \code{DelayedMatrix}.
    getExpressionBlock = function(genes, cells = NULL) {
      if (is.null(genes) || length(genes) == 0L) {
        stop("`genes` must be a non-empty character vector.", call. = FALSE)
      }
      if (!is.character(genes) || anyNA(genes)) {
        stop(
          "`genes` must be a character vector of non-NA gene names.",
          call. = FALSE
        )
      }

      cell_selection <- private$resolveExpressionCells(cells)
      cell_idx <- cell_selection$indices

      gene_idx <- match(genes, rownames(self$expression))
      if (anyNA(gene_idx)) {
        missing_genes <- genes[is.na(gene_idx)]
        stop(
          "Gene(s) not found in expression matrix: ",
          paste(utils::head(missing_genes, 5), collapse = ", "),
          if (length(missing_genes) > 5L) " ..." else "",
          call. = FALSE
        )
      }

      if (is.null(cell_idx)) {
        return(self$expression[gene_idx, , drop = FALSE])
      }

      ## Read external backends in storage order, then restore the requested
      ## Viewer order without a barcode lookup.
      block <- self$expression[gene_idx, cell_idx, drop = FALSE]
      if (!is.null(cell_selection$restore)) {
        block <- block[, cell_selection$restore, drop = FALSE]
      }
      block
    },

    #' @description
    #' Add columns containing cell cycle assignments to the \code{cell_cycle}
    #' field.
    #'
    #' @param cols \code{vector} of columns names containing cell cycle
    #' assignments.
    setCellCycle = function(cols) {
      if (length(cols) == 1) {
        self$checkIfColumnExistsInMetadata(cols)
        self$cell_cycle <- cols
      } else {
        for (i in seq_along(cols)) {
          self$checkIfColumnExistsInMetadata(cols[i])
          self$cell_cycle <- c(self$cell_cycle, cols[i])
        }
      }
    },

    #' @description
    #' Retrieve column names containing cell cycle assignments.
    #'
    #' @return
    #' \code{vector} of column names in meta data.
    getCellCycle = function() {
      return(self$cell_cycle)
    },

    #' @description
    #' Add projections (dimensional reductions).
    #'
    #' @param name Name of the projection.
    #' @param projection \code{data.frame} containing positions of cells in
    #' projection.
    addProjection = function(name, projection) {
      ## check if projection with same name already exists
      if (name %in% names(self$projections)) {
        stop(
          glue::glue(
            'A projection with the name `{name}` already exists. ',
            'Please use a different name.'
          ),
          call. = FALSE
        )
      }
      ## check if provided projection is a data frame
      if (!is.data.frame(projection)) {
        stop(
          glue::glue(
            'Provided projection is of type `{class(projection)}` but should ',
            'be a data frame. Please convert it.'
          ),
          call. = FALSE
        )
      }
      ## check dimensions
      if (
        !is.null(self$expression) && nrow(projection) != ncol(self$expression)
      ) {
        stop(glue::glue(
          "Number of rows in projection ({nrow(projection)}) must match number of cells ({ncol(self$expression)})."
        ))
      }
      self$projections[[name]] <- projection
    },

    #' @description
    #' Get list of available projections (dimensional reductions).
    #'
    #' @return
    #' \code{vector} of projections / dimensional reductions that are available.
    availableProjections = function() {
      return(names(self$projections))
    },

    #' @description
    #' Retrieve data for a specific projection.
    #'
    #' @param name Name of projection.
    #'
    #' @return
    #' \code{data.frame} containing the positions of cells in the projection.
    getProjection = function(name) {
      if (name %in% self$availableProjections() == FALSE) {
        stop(glue::glue('Projection `{name}` is not available.'), call. = FALSE)
      } else {
        return(self$projections[[name]])
      }
    },

    #' @description
    #' Add phylogenetic tree to \code{trees} field.
    #'
    #' @param group_name Group name that this tree belongs to.
    #' @param tree Phylogenetic tree as \code{phylo} object.
    addTree = function(group_name, tree) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$trees[[group_name]] <- tree
    },

    #' @description
    #' Retrieve phylogenetic tree for a specific group.
    #'
    #' @param group_name Group name for which to retrieve phylogenetic tree.
    #'
    #' @return
    #' Phylogenetic tree as \code{phylo} object.
    getTree = function(group_name) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      return(self$trees[[group_name]])
    },

    #' @description
    #' Add table of most expressed genes.
    #'
    #' @param group_name Name of grouping variable that the most expressed genes
    #' belong to. Must be registered in the \code{groups} field.
    #' @param table \code{data.frame} that contains the most expressed genes.
    addMostExpressedGenes = function(group_name, table) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$most_expressed_genes[[group_name]] <- table
    },

    #' @description
    #' Retrieve names of grouping variables for which most expressed genes are
    #' available.
    #'
    #' @return
    #' \code{vector} of grouping variables for which most expressed genes are
    #' available.
    getGroupsWithMostExpressedGenes = function() {
      return(names(self$most_expressed_genes))
    },

    #' @description
    #' Retrieve table of most expressed genes for a specific grouping variable.
    #'
    #' @param group_name Name of grouping variable for which to retrieve most
    #' expressed genes.
    #'
    #' @return
    #' \code{data.frame} containing the most expressed genes.
    getMostExpressedGenes = function(group_name) {
      if (
        length(group_name) != 1L ||
          is.na(group_name) ||
          !(group_name %in% names(self$most_expressed_genes))
      ) {
        stop(
          glue::glue(
            'Most expressed genes for `{group_name}` are not available.'
          ),
          call. = FALSE
        )
      }
      return(self$most_expressed_genes[[group_name]])
    },

    #' @description
    #' Add table of mean expression per gene.
    #'
    #' @param group_name Name of grouping variable that the mean expression
    #' belongs to. Must be registered in the \code{groups} field.
    #' @param table \code{data.frame} that contains the mean expression per gene.
    addMeanExpression = function(group_name, table) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$mean_expression[[group_name]] <- table
    },

    #' @description
    #' Retrieve names of grouping variables for which mean expression data is
    #' available.
    #'
    #' @return
    #' \code{vector} of grouping variables for which mean expression is
    #' available.
    getGroupsWithMeanExpression = function() {
      return(names(self$mean_expression))
    },

    #' @description
    #' Retrieve table of mean expression for a specific grouping variable.
    #'
    #' @param group_name Name of grouping variable for which to retrieve mean
    #' expression.
    #'
    #' @return
    #' \code{data.frame} containing the mean expression per gene.
    getMeanExpression = function(group_name) {
      if (
        length(group_name) != 1L ||
          is.na(group_name) ||
          !(group_name %in% names(self$mean_expression))
      ) {
        stop(
          glue::glue('Mean expression for `{group_name}` is not available.'),
          call. = FALSE
        )
      }
      return(self$mean_expression[[group_name]])
    },

    #' @description
    #' Add table of marker genes.
    #'
    #' @param method Name of method that was used to generate the marker genes.
    #' @param name Name of table. This name will be used to select the table in
    #' Cerebro. It is recommended to use the grouping variable, e.g.
    #' \code{sample}.
    #' @param table \code{data.frame} that contains the marker genes.
    addMarkerGenes = function(method, name, table) {
      if (method %in% names(self$marker_genes) == FALSE) {
        self$marker_genes[[method]] <- list()
      }
      self$marker_genes[[method]][[name]] <- table
    },

    #' @description
    #' Retrieve names of methods that were used to generate marker genes.
    #'
    #' @return
    #' \code{vector} of names of methods that were used to generate marker
    #' genes.
    getMethodsForMarkerGenes = function() {
      return(names(self$marker_genes))
    },

    #' @description
    #' Retrieve grouping variables for which marker genes were generated using
    #' a specified method.
    #'
    #' @param method Name of method.
    #'
    #' @return
    #' \code{vector} of grouping variables for which marker genes were
    #' calculated using the specified method.
    getGroupsWithMarkerGenes = function(method) {
      return(names(self$marker_genes[[method]]))
    },

    #' @description
    #' Retrieve table of marker genes for specific method and grouping variable.
    #'
    #' @param method Name of method.
    #' @param name Name of table.
    #'
    #' @return
    #' \code{data.frame} that contains marker genes for the specified
    #' combination of method and grouping variable.
    getMarkerGenes = function(method, name) {
      if (method %in% names(self$marker_genes) == FALSE) {
        stop(
          glue::glue('Method `{method}` is not available for marker genes.'),
          call. = FALSE
        )
      }
      if (name %in% names(self$marker_genes[[method]]) == FALSE) {
        stop(
          glue::glue(
            'A marker gene table with name `{name}` is not available for method `{method}`.'
          ),
          call. = FALSE
        )
      }
      return(self$marker_genes[[method]][[name]])
    },

    #' @description
    #' Add table of enriched pathways.
    #'
    #' @param method Name of method that was used to calculate enriched
    #' pathways.
    #' @param group_name Name of grouping variable that the enriched pathways
    #' belong to. Must be registered in the \code{groups} field.
    #' @param table \code{data.frame} that contains the enriched pathways.
    addEnrichedPathways = function(method, group_name, table) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$enriched_pathways[[method]][[group_name]] <- table
    },

    #' @description
    #' Retrieve names of methods for which enriched pathways are available.
    #'
    #' @return
    #' \code{vector} of methods for which enriched pathways are available.
    getMethodsForEnrichedPathways = function() {
      return(names(self$enriched_pathways))
    },

    #' @description
    #' Retrieve names of grouping variables for which enriched pathways are
    #' available for a specific method.
    #'
    #' @param method Name of method for which to retrieve grouping variables.
    #'
    #' @return
    #' \code{vector} of grouping variables for which enriched pathways are
    #' available.
    getGroupsWithEnrichedPathways = function(method) {
      if (method %in% self$getMethodsForEnrichedPathways() == FALSE) {
        stop(glue::glue('Method `{method}` is not available.'), call. = FALSE)
      } else {
        return(names(self$enriched_pathways[[method]]))
      }
    },

    #' @description
    #' Retrieve table of enriched pathways for a specific method and grouping
    #' variable.
    #'
    #' @param method Name of method for which to retrieve enriched pathways.
    #' @param group_name Name of grouping variable for which to retrieve enriched
    #' pathways.
    #'
    #' @return
    #' \code{data.frame} containing the enriched pathways.
    getEnrichedPathways = function(method, group_name) {
      ## getGroupsWithEnrichedPathways() rejects an unknown method
      if (group_name %in% self$getGroupsWithEnrichedPathways(method) == FALSE) {
        stop(
          glue::glue(
            'Group `{group_name}` is not available for method `{method}`.'
          ),
          call. = FALSE
        )
      }
      return(self$enriched_pathways[[method]][[group_name]])
    },

    #' @description
    #' Add trajectory to \code{trajectories} field.
    #'
    #' @param method Name of method that was used to calculate trajectory.
    #' @param trajectory_name Name of trajectory.
    #' @param trajectory Trajectory data as \code{data.frame} or \code{list}.
    addTrajectory = function(method, trajectory_name, trajectory) {
      self$trajectories[[method]][[trajectory_name]] <- trajectory
    },

    #' @description
    #' Retrieve names of methods for which trajectories are available.
    #'
    #' @return
    #' \code{vector} of methods for which trajectories are available.
    getMethodsForTrajectories = function() {
      return(names(self$trajectories))
    },

    #' @description
    #' Retrieve names of trajectories for a specific method.
    #'
    #' @param method Name of method for which to retrieve trajectories.
    #'
    #' @return
    #' \code{vector} of trajectories for the specified method.
    getNamesOfTrajectories = function(method) {
      if (method %in% self$getMethodsForTrajectories() == FALSE) {
        stop(glue::glue('Method `{method}` is not available.'), call. = FALSE)
      } else {
        return(names(self$trajectories[[method]]))
      }
    },

    #' @description
    #' Retrieve trajectory data for a specific method and trajectory name.
    #'
    #' @param method Name of method for which to retrieve trajectory.
    #' @param trajectory_name Name of trajectory to retrieve.
    #'
    #' @return
    #' Trajectory data as \code{data.frame} or \code{list}.
    getTrajectory = function(method, trajectory_name) {
      ## getNamesOfTrajectories() rejects an unknown method
      if (trajectory_name %in% self$getNamesOfTrajectories(method) == FALSE) {
        stop(
          glue::glue(
            'Trajectory `{trajectory_name}` is not available for method `{method}`.'
          ),
          call. = FALSE
        )
      }
      return(self$trajectories[[method]][[trajectory_name]])
    },

    #' @description
    #' Retrieve the BCR rows of the immune repertoire: for each sample the
    #' cells whose \code{CTgene} names an immunoglobulin chain (IGH, IGK or
    #' IGL). Samples without such cells are dropped.
    #'
    #' @return
    #' Named list of data.frames (one per sample), or an empty list.
    getBCR = function() {
      private$receptorRows("IG[HKL]")
    },

    #' @description
    #' Retrieve the TCR rows of the immune repertoire: for each sample the
    #' cells whose \code{CTgene} names a T-cell receptor chain (TRA, TRB, TRG
    #' or TRD). Samples without such cells are dropped.
    #'
    #' @return
    #' Named list of data.frames (one per sample), or an empty list.
    getTCR = function() {
      private$receptorRows("TR[ABGD]")
    },

    #' @description
    #' Get immune repertoire data: the \code{immune_repertoire} field, or the
    #' sibling file that \code{immune_repertoire_backend} points to (loaded and
    #' checksummed on first use).
    #'
    #' @return Named list of data.frames (one per sample), or empty list.
    getImmuneRepertoire = function() {
      if (length(self$immune_repertoire) > 0) {
        return(private$restoreRepertoireCells(self$immune_repertoire))
      }
      backend <- tryCatch(self$immune_repertoire_backend, error = function(e) {
        NULL
      })
      if (!is.null(backend)) {
        valid <- is.list(backend) &&
          identical(backend$type, "file") &&
          is.character(backend$root) &&
          length(backend$root) == 1L &&
          !is.na(backend$root) &&
          nzchar(backend$root) &&
          is.character(backend$file) &&
          length(backend$file) == 1L &&
          !is.na(backend$file) &&
          nzchar(backend$file) &&
          !backend$file %in% c(".", "..") &&
          !grepl("[/\\\\]", backend$file) &&
          is.character(backend$md5) &&
          length(backend$md5) == 1L &&
          !is.na(backend$md5) &&
          grepl("^[[:xdigit:]]{32}$", backend$md5)
        if (!valid) {
          stop(
            "The immune repertoire sidecar descriptor is invalid.",
            call. = FALSE
          )
        }
        repertoire <- private$readSidecar(
          file.path(backend$root, backend$file),
          backend$md5,
          "The immune repertoire sidecar is missing.",
          "The immune repertoire sidecar checksum does not match."
        )
        if (!is.list(repertoire)) {
          stop("The immune repertoire sidecar is invalid.", call. = FALSE)
        }
        repertoire <- private$restoreRepertoireCells(repertoire)
        self$immune_repertoire <- repertoire
        return(repertoire)
      }
      self$immune_repertoire
    },

    #' @description
    #' Set immune repertoire data.
    #'
    #' @param data Named list of data.frames (one per sample) containing
    #'   scRepertoire columns.
    addImmuneRepertoire = function(data) {
      self$immune_repertoire <- data
      self$immune_repertoire_backend <- NULL
    },

    #' @description
    #' Get HLA typing data (canonical long \code{data.frame}), or an empty
    #' table when none is stored.
    #'
    #' @return A canonical HLA typing \code{data.frame} (possibly zero-row).
    getHLATyping = function() {
      ht <- self$hla_typing
      if (is.data.frame(ht)) {
        return(ht)
      }
      # Nothing stored: return a canonical EMPTY table built with base R only.
      #
      # This must NOT call a package-internal helper (e.g. hla_normalize_typing)
      # to synthesise the empty table. A .crb is an R6 object whose methods are
      # serialized with an enclosing environment that references the
      # CerebroNexus namespace; a createShinyApp() bundle never loads that
      # namespace, so a method reaching into it fails at runtime with
      # "could not find function". The object's own print() calls this getter,
      # so the failure cascades and the app never reaches a stable state.
      # test-hla-class-slot.R pins this empty table against
      # hla_normalize_typing(list()) so the two schemas cannot drift apart.
      empty <- data.frame(
        sample = character(0),
        donor_id = character(0),
        locus = character(0),
        copy = integer(0),
        allele = character(0),
        resolution = character(0),
        source_type = character(0),
        typing_method = character(0),
        source_reference = character(0),
        confidence = numeric(0),
        stringsAsFactors = FALSE
      )
      attr(empty, "qc") <- data.frame(
        sample = character(0),
        value = character(0),
        issue = character(0),
        stringsAsFactors = FALSE
      )
      empty
    },

    #' @description
    #' Set HLA typing data. Accepts a canonical long \code{data.frame}, a wide
    #' \code{data.frame} (sample + HLA-*_1/_2 columns), or a named \code{list}
    #' (sample -> allele vector). Non-canonical inputs are normalized; a table
    #' that already has the canonical columns is validated (unrecognisable
    #' alleles dropped, locus re-derived, copy and provenance coerced) rather
    #' than stored verbatim, so a canonical-looking table cannot smuggle invalid
    #' values into downstream analysis.
    #'
    #' @param data HLA typing in any accepted form.
    #' @param source_type Provenance of the genotype: one of "genotyped",
    #'   "imputed", "synthetic", "unknown".
    #' @param typing_method Optional assay/software string.
    #' @param source_reference Optional traceable reference (file/cohort/model).
    addHLATyping = function(
      data,
      source_type = "unknown",
      typing_method = NA_character_,
      source_reference = NA_character_
    ) {
      if (hla_is_typing_table(data)) {
        self$hla_typing <- hla_validate_typing(data)
      } else {
        self$hla_typing <- hla_normalize_typing(
          data,
          source_type = source_type,
          typing_method = typing_method,
          source_reference = source_reference
        )
      }
      invisible(self)
    },

    #' @description
    #' Add spatial data.
    #'
    #' @param name Name of the spatial data entry (for example a Seurat image or
    #'   Cerebro spatial entry).
    #' @param data \code{list} containing 'coordinates' (data.frame) and
    #'   'expression' (sparse matrix). Embedded backgrounds are stored in
    #'   'histology_images', a uniquely named list of image payloads. Each
    #'   payload contains 'histology_image' (a base64 \code{data:} URI),
    #'   'histology_image_bounds' (xmin/xmax/ymin/ymax in coordinate space), and
    #'   may contain a declarative 'histology_alignment'.
    #'   Bounds omitted from a payload are derived from the coordinates. The
    #'   singular 'histology_image' / 'histology_image_bounds' fields are
    #'   rejected.
    addSpatialData = function(name, data) {
      if (
        !is.list(data) || !all(c("coordinates", "expression") %in% names(data))
      ) {
        stop(
          "Spatial data must be a list containing 'coordinates' and 'expression'."
        )
      }
      self$spatial[[name]] <- .normalizeSpatialDataImages(data, name)
    },

    #' @description
    #' Retrieve spatial data.
    #'
    #' @param name Name of the spatial data entry.
    #' @param hydrate_molecules Whether to read an external molecule table. Set
    #'   to \code{FALSE} for Viewer paths that only consume coordinates,
    #'   expression, or images.
    #'
    #' @return
    #' A canonical spatial-data \code{list} containing 'coordinates',
    #' 'expression', and 'histology_images'.
    getSpatialData = function(name, hydrate_molecules = TRUE) {
      if (
        !is.logical(hydrate_molecules) ||
          length(hydrate_molecules) != 1L ||
          is.na(hydrate_molecules)
      ) {
        stop("`hydrate_molecules` must be TRUE or FALSE.", call. = FALSE)
      }
      if (name %in% names(self$spatial) == FALSE) {
        stop(
          paste0("Spatial data `", name, "` is not available."),
          call. = FALSE
        )
      }
      data <- self$spatial[[name]]
      molecules <- data[["molecules"]]
      if (
        isTRUE(hydrate_molecules) &&
          inherits(molecules, "CerebroSpatialMoleculeRef")
      ) {
        backend <- self$spatial_molecule_backend
        valid_backend <- is.list(backend) &&
          identical(backend$type, "directory") &&
          is.character(backend$root) &&
          length(backend$root) == 1L &&
          !is.na(backend$root) &&
          nzchar(backend$root)
        valid_ref <- is.list(molecules) &&
          is.character(molecules$file) &&
          length(molecules$file) == 1L &&
          !is.na(molecules$file) &&
          nzchar(molecules$file) &&
          !grepl("[/\\\\]", molecules$file) &&
          is.character(molecules$md5) &&
          length(molecules$md5) == 1L &&
          grepl("^[[:xdigit:]]{32}$", molecules$md5)
        if (!valid_backend || !valid_ref) {
          stop("Spatial molecule sidecar descriptor is invalid.", call. = FALSE)
        }
        data[["molecules"]] <- private$readSidecar(
          file.path(backend$root, molecules$file),
          molecules$md5,
          paste0("Spatial molecule sidecar is missing for `", name, "`."),
          paste0(
            "Spatial molecule sidecar checksum does not match for `",
            name,
            "`."
          )
        )
        self$spatial[[name]] <- data
      }
      context <- paste0("Spatial data `", name, "`")
      images <- if ("histology_images" %in% names(data)) {
        data[["histology_images"]]
      } else {
        list()
      }
      ## The checks are the ones `addSpatialData()` applies, kept in
      ## spatial_image_manifest.R; see `.cerebroStandaloneSpatialImages()`.
      data[["histology_images"]] <- private$normalizeSpatialImages(
        images,
        data[["coordinates"]],
        context
      )
      data
    },

    #' @description
    #' Get list of available spatial data entries.
    #'
    #' @return
    #' \code{vector} of spatial data entries that are available.
    availableSpatial = function() {
      return(names(self$spatial))
    },

    #' @description
    #' Get Trekker single-cell spatial-mapping data, or \code{NULL} when none is
    #' stored.
    #'
    #' @return A \code{list} with the Trekker page's content, or \code{NULL}.
    getTrekker = function() {
      self$trekker
    },

    #' @description
    #' Set Trekker single-cell spatial-mapping data.
    #'
    #' @param data \code{list} carrying the Trekker page content (coordinates,
    #'   cell metadata, positioning QC, upstream Moran's I, and positioning
    #'   evidence). See the Trekker page module for the expected structure.
    addTrekker = function(data) {
      if (!is.list(data)) {
        stop("Trekker data must be a list.", call. = FALSE)
      }
      self$trekker <- data
      invisible(self)
    },

    #' @description
    #' Add content to extra material field.
    #'
    #' @param category Name of category. At the moment, only \code{tables} and
    #' \code{plots} are valid categories. Tables must be in \code{data.frame}
    #' format and plots must be created with \code{ggplot2}.
    #' @param name Name of material, will be used to select it in Cerebro.
    #' @param content Data that should be added.
    addExtraMaterial = function(category, name, content) {
      ## valid categories
      valid_categories <- c('tables', 'plots')

      ## proceed only if specified category is valid
      if (category %in% valid_categories == FALSE) {
        stop(
          glue::glue(
            'Category `{category}` is not one of the valid categories ',
            '({paste0(valid_categories, collapse = ", ")}).'
          ),
          call. = FALSE
        )
      }

      ## call function to add table
      if (category == 'tables') {
        self$addExtraTable(name, content)
      }

      ## call function to add table
      if (category == 'plots') {
        self$addExtraPlot(name, content)
      }
    },

    #' @description
    #' Add table to `extra_material` slot.
    #'
    #' @param name Name of material, will be used to select it in Cerebro.
    #' @param table Table that should be added, must be \code{data.frame}.
    addExtraTable = function(name, table) {
      ## stop if table is not a data frame
      if (!is.data.frame(table)) {
        if ('DFrame' %in% class(table)) {
          table <- as.data.frame(table)
        } else {
          stop(
            glue::glue(
              'Cannot add table `{name}` because it is not a data frame.'
            ),
            call. = FALSE
          )
        }
      }

      ## stop if `name` is already used
      if (name %in% names(self$extra_material$tables)) {
        stop(
          glue::glue(
            'A table with name `{name}` already exists in the extra material.'
          ),
          call. = FALSE
        )

        ## add table
      } else {
        self$extra_material$tables[[name]] <- table
      }
    },

    #' @description
    #' Add plot to `extra_material` slot.
    #'
    #' @param name Name of material, will be used to select it in Cerebro.
    #' @param plot Plot that should be added, must be created with
    #' \code{ggplot2} (class: \code{ggplot}).
    addExtraPlot = function(name, plot) {
      ## stop if table is not a data frame
      if ("ggplot" %in% class(plot) == FALSE) {
        stop(
          glue::glue(
            'Cannot add plot `{name}` because it is not of class "ggplot".'
          ),
          call. = FALSE
        )
      }

      ## stop if `name` is already used
      if (name %in% names(self$extra_material$plots)) {
        stop(
          glue::glue(
            'A plot with name `{name}` already exists in the extra material.'
          ),
          call. = FALSE
        )

        ## add table
      } else {
        self$extra_material$plots[[name]] <- plot
      }
    },

    #' @description
    #' Retrieve extra material from \code{extra_material} field.
    #'
    #' @return
    #' \code{list} of all entries in the \code{extra_material} field.
    getExtraMaterial = function() {
      return(self$extra_material)
    },

    #' @description
    #' Get names of categories for which extra material is available.
    #'
    #' @return
    #' \code{vector} with names of available categories.
    getExtraMaterialCategories = function() {
      return(names(self$extra_material))
    },

    #' @description
    #' Check whether there are tables in the extra materials.
    #'
    #' @return
    #' \code{logical} indicating whether there are tables in the extra
    #' materials.
    checkForExtraTables = function() {
      return(!is.null(self$extra_material$tables))
    },

    #' @description
    #' Get names of tables in extra materials.
    #'
    #' @return
    #' \code{vector} containing names of tables in extra materials.
    getNamesOfExtraTables = function() {
      return(names(self$extra_material$tables))
    },

    #' @description
    #' Get table from extra materials.
    #'
    #' @param name Name of table.
    #'
    #' @return
    #' Requested table in \code{data.frame} format.
    getExtraTable = function(name) {
      return(self$extra_material$table[[name]])
    },

    #' @description
    #' Check whether there are plots in the extra materials.
    #'
    #' @return
    #' \code{logical} indicating whether there are plots in the extra
    #' materials.
    checkForExtraPlots = function() {
      return(!is.null(self$extra_material$plots))
    },

    #' @description
    #' Get names of plots in extra materials.
    #'
    #' @return
    #' \code{vector} containing names of plots in extra materials.
    getNamesOfExtraPlots = function() {
      return(names(self$extra_material$plots))
    },

    #' @description
    #' Get plot from extra materials.
    #'
    #' @param name Name of plot.
    #'
    #' @return
    #' Requested plot made with \code{ggplot2}.
    getExtraPlot = function(name) {
      return(self$extra_material$plots[[name]])
    },

    #' @description
    #' Show overview of object and the data it contains.
    print = function() {
      message(
        paste0(
          'class: Cerebro',
          '\n',
          'exporter package version: ',
          self$getVersion(),
          '\n',
          'experiment name: ',
          self$getExperiment()$experiment_name,
          '\n',
          'organism: ',
          self$getExperiment()$organism,
          '\n',
          'date of analysis: ',
          self$getExperiment()$date_of_analysis,
          '\n',
          'date of export: ',
          self$getExperiment()$date_of_export,
          '\n',
          'number of cells: ',
          format(ncol(self$expression), big.mark = ','),
          '\n',
          'number of genes: ',
          format(nrow(self$expression), big.mark = ','),
          '\n',
          'grouping variables (',
          length(self$getGroups()),
          '): ',
          paste0(self$getGroups(), collapse = ', '),
          '\n',
          'cell cycle variables (',
          length(self$cell_cycle),
          '): ',
          paste0(self$cell_cycle, collapse = ', '),
          '\n',
          'projections (',
          length(self$availableProjections()),
          '): ',
          paste0(self$availableProjections(), collapse = ', '),
          '\n',
          'trees (',
          length(self$trees),
          '): ',
          paste0(names(self$trees), collapse = ', '),
          '\n',
          'most expressed genes: ',
          paste0(names(self$most_expressed_genes), collapse = ', '),
          '\n',
          'marker genes:',
          private$showNested('marker_genes'),
          '\n',
          'enriched pathways:',
          private$showNested('enriched_pathways'),
          '\n',
          'trajectories:',
          private$showNested('trajectories'),
          '\n',
          'extra material:',
          private$showNested('extra_material'),
          '\n',
          'Immune repertoire:',
          paste0(names(self$getImmuneRepertoire()), collapse = ', '),
          '\n',
          'HLA typing: ',
          {
            ht <- self$getHLATyping()
            if (is.data.frame(ht) && nrow(ht) > 0) {
              paste0(
                length(unique(ht$sample)),
                ' sample(s), ',
                length(unique(ht$allele)),
                ' allele(s) [',
                paste0(unique(ht$source_type), collapse = ', '),
                ']'
              )
            } else {
              'none'
            }
          },
          '\n',
          'Spatial data:',
          paste0(self$availableSpatial(), collapse = ', '),
          '\n'
        )
      )
    }
  ),

  ## private fields and methods
  private = list(
    ## The spatial image checks of `.normalizeEmbeddedSpatialImages()`, as a
    ## copy that needs no CerebroNexus namespace (set in `initialize()`).
    normalizeSpatialImages = NULL,

    ## Cell names of the metadata rows, for barcodes a thin CRB stores by cell
    ## index (in the Viewer this hydrates the thin metadata).
    storedCellNames = function() {
      cells <- self$meta_data[["cell_barcode"]]
      if (!is.character(cells)) {
        stop("The cell names of the metadata are missing.", call. = FALSE)
      }
      cells
    },

    ## Repertoire barcodes stored as metadata rows get their names back, once.
    restoreRepertoireCells = function(repertoire) {
      indexed <- vapply(
        repertoire,
        function(sample) {
          !is.null(attr(sample, "cerebro_cell_index_column", exact = TRUE))
        },
        logical(1)
      )
      if (!any(indexed)) {
        return(repertoire)
      }
      repertoire <- .restoreRepertoireByCell(
        repertoire,
        private$storedCellNames()
      )
      self$immune_repertoire <- repertoire
      repertoire
    },

    ## Rows of the immune repertoire whose CTgene matches a receptor pattern.
    receptorRows = function(pattern) {
      repertoire <- self$getImmuneRepertoire()
      rows <- lapply(repertoire, function(table) {
        if (!is.data.frame(table) || !"CTgene" %in% names(table)) {
          return(NULL)
        }
        table[grepl(pattern, as.character(table$CTgene)), , drop = FALSE]
      })
      rows[vapply(
        rows,
        function(table) is.data.frame(table) && nrow(table) > 0L,
        logical(1)
      )]
    },

    resolveExpressionCells = function(cells) {
      cell_names <- colnames(self$expression)
      if (is.null(cells)) {
        return(list(indices = NULL, names = cell_names))
      }
      if (is.character(cells) && !anyNA(cells)) {
        indices <- match(cells, cell_names)
        if (anyNA(indices)) {
          missing_cells <- cells[is.na(indices)]
          stop(
            "Cell(s) not found in expression matrix: ",
            paste(utils::head(missing_cells, 5), collapse = ", "),
            if (length(missing_cells) > 5L) " ..." else "",
            call. = FALSE
          )
        }
        return(list(indices = indices, names = cells))
      }
      if (
        is.numeric(cells) &&
          !anyNA(cells) &&
          all(is.finite(cells)) &&
          all(cells == as.integer(cells)) &&
          all(cells >= 1L) &&
          all(cells <= ncol(self$expression))
      ) {
        indices <- as.integer(cells)
        requested_names <- cell_names[indices]
        restore <- NULL
        if (
          length(indices) > 1L &&
            is.unsorted(indices) &&
            inherits(self$expression, "DelayedArray")
        ) {
          read_order <- order(indices)
          indices <- indices[read_order]
          restore <- order(read_order)
        }
        return(list(
          indices = indices,
          names = requested_names,
          restore = restore
        ))
      }
      stop(
        "`cells` must contain valid names/barcodes or one-based column indices.",
        call. = FALSE
      )
    },

    ## Extract expression matrix (helper)
    ##   cells  Names/barcodes of cells to extract; NULL for all.
    ##   genes  Names of genes to extract; NULL for all.
    ## Returns a dense matrix.
    extractExpression = function(cells = NULL, genes = NULL) {
      cell_selection <- private$resolveExpressionCells(cells)
      cell_indices <- cell_selection$indices
      cells <- cell_selection$names

      ## check what kind of matrix the transcription counts are stored as
      ## ... DelayedArray / RleMatrix
      if (inherits(self$expression, 'RleMatrix')) {
        ## resolve gene indices
        if (!is.null(genes)) {
          gene_indices <- match(genes, rownames(self$expression))
        } else {
          gene_indices <- NULL
          genes <- rownames(self$expression)
        }

        ## extract (dense) matrix
        mat <- as.matrix(
          DelayedArray::extract_array(
            self$expression,
            list(gene_indices, cell_indices)
          )
        )
        if (!is.null(cell_selection$restore)) {
          mat <- mat[, cell_selection$restore, drop = FALSE]
        }

        ## assign names (extract_array might lose them or not return them for indices)
        colnames(mat) <- cells
        rownames(mat) <- genes

        return(mat)
      } else {
        ## standard matrix logic
        if (is.null(genes)) {
          genes <- rownames(self$expression)
        }

        mat <- if (is.null(cell_indices)) {
          self$expression[genes, , drop = FALSE]
        } else {
          self$expression[genes, cell_indices, drop = FALSE]
        }
        if (!is.null(cell_selection$restore)) {
          mat <- mat[, cell_selection$restore, drop = FALSE]
        }
        return(as.matrix(mat))
      }
    },

    ## Overview of a list of named lists (marker genes, enriched pathways,
    ## trajectories, extra material) for self$print().
    showNested = function(field) {
      content <- self[[field]]
      text <- list()
      for (name in names(content)) {
        text[[name]] <- paste0(
          '\n  - ',
          name,
          ' (',
          length(names(content[[name]])),
          '): ',
          paste0(names(content[[name]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    ## Read a qs2 or RDS sidecar after checking that it exists and matches its
    ## MD5 checksum; `missing` and `mismatch` are the error messages.
    readSidecar = function(path, md5, missing, mismatch) {
      if (!file.exists(path) || dir.exists(path)) {
        stop(missing, call. = FALSE)
      }
      if (!identical(unname(tools::md5sum(path)), md5)) {
        stop(mismatch, call. = FALSE)
      }
      connection <- file(path, open = "rb")
      magic <- readBin(connection, "raw", n = 4L)
      close(connection)
      if (identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))) {
        qs2::qs_read(path)
      } else {
        readRDS(path)
      }
    }
  )
)
