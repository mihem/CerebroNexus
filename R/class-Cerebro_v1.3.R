setOldClass(Classes = 'package_version')

#' @title
#' R6 class in which data sets will be stored for visualization in Cerebro.
#'
#' @description
#' A \code{Cerebro_v1.3} object is an R6 class that contains several types of
#' data that can be visualized in Cerebro.
#'
#' @return
#' A new \code{Cerebro_v1.3} object.
#'
#' @importFrom R6 R6Class
#
#' @export
#
Cerebro_v1.3 <- R6::R6Class(
  'Cerebro_v1.3',

  ## public fields and methods
  public = list(
    #' @field version cerebroApp version that was used to create the object.
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

    #' @field expression_backend \code{list} describing how/where the expression
    #' matrix is stored. For step 7.1 every newly exported object tags itself
    #' \code{list(type = "embedded", location = NULL)}; future step 7.2 will
    #' introduce \code{type = "h5"} / \code{"bpcells"} with an external
    #' \code{location}. Older \code{.crb} files (serialised before this field
    #' existed) load with \code{expression_backend = NULL}; \code{getExpressionBackend()}
    #' treats that as \code{"embedded"} for backward compatibility.
    expression_backend = NULL,

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
    #'   the HLA & TCR Motifs page for donor-level HLA context. Optional; older
    #'   .crb files simply lack it.
    hla_typing = NULL,

    #' @field bcr_data \code{list} that contains BCR data (kept for backward
    #'   compatibility with older .crb files).
    bcr_data = list(),

    #' @field tcr_data \code{list} that contains TCR data (kept for backward
    #'   compatibility with older .crb files).
    tcr_data = list(),

    #' @field spatial \code{list} that contains spatial data (coordinates and expression).
    spatial = list(),

    #' @field trekker \code{list} with Trekker single-cell spatial-mapping
    #'   content: canonical and variant (transposed / y-mirrored) coordinates,
    #'   UMAP coordinates, per-nucleus cluster/cell-type, positioning QC in the
    #'   vendor's original field names, the upstream (vendor) Moran's I table,
    #'   and per-nucleus positioning-evidence images (base64 \code{data:} URIs).
    #'   Consumed by the Trekker page. Optional; older .crb files simply lack it.
    trekker = NULL,

    ##------------------------------------------------------------------------##
    ## methods to interact with the object
    ##------------------------------------------------------------------------##

    #' @description
    #' Create a new \code{Cerebro_v1.3} object.
    #'
    #' @return
    #' A new \code{Cerebro_v1.3} object.
    initialize = function() {
      self$experiment <- list(
        experiment_name = NULL,
        organism = NULL,
        date_of_analysis = NULL,
        date_of_export = NULL
      )
    },

    #' @description
    #' Set the version of \code{cerebroApp} that was used to generate this
    #' object.
    #'
    #' @param version Version to set.
    setVersion = function(version) {
      self$version <- version
    },

    #' @description
    #' Get the version of \code{cerebroApp} that was used to generate this
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
            "DelayedMatrix",
            "IterableMatrix"
          )
        )
      ) {
        warning(
          "Expression data should ideally be a matrix-like object (matrix, dgCMatrix, RleMatrix, IterableMatrix, etc)."
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
    #' is stored. In step 7.1 every newly exported \code{.crb} is tagged
    #' \code{"embedded"} with a NULL location, meaning the matrix is carried
    #' inside the serialised \code{.crb}. Later steps (7.2 exporter, 7.3
    #' runtime attach) will produce objects tagged \code{"h5"} or
    #' \code{"bpcells"} with an external \code{location}.
    #'
    #' @param type Storage backend label. One of \code{"embedded"},
    #' \code{"h5"}, \code{"bpcells"}. Step 7.1 only recognises
    #' \code{"embedded"} at runtime; the other two are accepted here (so step
    #' 7.2 can set them) but will still need step-7.3 runtime attach to be
    #' useful.
    #' @param location Optional character path (absolute or relative to the
    #' generated app \code{data/} directory) where the external matrix lives.
    #' \code{NULL} when \code{type == "embedded"}.
    setExpressionBackend = function(type = "embedded", location = NULL) {
      allowed <- c("embedded", "h5", "bpcells")
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
      self$expression_backend <- list(type = type, location = location)
    },

    #' @description
    #' Read the expression backend tag. Returns a \code{list(type, location)}.
    #' For \code{.crb} files generated before the \code{expression_backend}
    #' field existed the stored slot is \code{NULL}; this method graciously
    #' falls back to \code{list(type = "embedded", location = NULL)} so that
    #' downstream code does not need to special-case legacy objects.
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
      if (
        inherits(mat, "DelayedArray") ||
          inherits(mat, "DelayedMatrix") ||
          inherits(mat, "RleMatrix")
      ) {
        mean_expression <- DelayedArray::rowMeans(mat)
      } else if (inherits(mat, "IterableMatrix")) {
        mean_expression <- BPCells::rowMeans(mat)
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
    #' @param cells Names/barcodes of cells to extract; defaults to \code{NULL},
    #' which will return all cells.
    #' @param genes Names of genes to extract; defaults to \code{NULL}, which
    #' will return all genes.
    #'
    #' @return
    #' \code{vector} containing (mean) expression across all specified genes in
    #' each specified cell.
    getMeanExpressionForCells = function(cells = NULL, genes = NULL) {
      ## extract dense matrix using helper
      mat <- private$extractExpression(cells = cells, genes = genes)

      ## calculate mean expression per cell (colMeans)
      mean_expression <- Matrix::colMeans(mat)

      return(mean_expression)
    },

    #' @description
    #' Retrieve transcript count matrix.
    #'
    #' @param cells Names/barcodes of cells to extract; defaults to \code{NULL},
    #' which will return all cells.
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
    #' @param cells Names/barcodes of cells to extract; \code{NULL} returns all cells.
    #' @return
    #' Named \code{numeric} vector, one entry per requested cell.
    getExpressionRow = function(gene, cells = NULL) {
      if (length(gene) != 1L || is.na(gene) || !is.character(gene)) {
        stop("`gene` must be a single non-NA character value.", call. = FALSE)
      }
      if (is.null(cells)) {
        cells <- colnames(self$expression)
      }
      if (!is.character(cells) || anyNA(cells)) {
        stop(
          "`cells` must be a character vector of non-NA cell names/barcodes.",
          call. = FALSE
        )
      }

      ## DelayedArray family (incl. RleMatrix): extract_array with indices
      ## avoids touching cells outside the requested subset.
      if (
        inherits(self$expression, "DelayedArray") ||
          inherits(self$expression, "DelayedMatrix") ||
          inherits(self$expression, "RleMatrix")
      ) {
        gene_idx <- match(gene, rownames(self$expression))
        if (is.na(gene_idx)) {
          stop(
            "Gene '",
            gene,
            "' not found in expression matrix.",
            call. = FALSE
          )
        }
        cell_idx <- match(cells, colnames(self$expression))
        missing_cells <- cells[is.na(cell_idx)]
        if (length(missing_cells) > 0L) {
          stop(
            "Cell(s) not found in expression matrix: ",
            paste(utils::head(missing_cells, 5), collapse = ", "),
            if (length(missing_cells) > 5L) " ..." else "",
            call. = FALSE
          )
        }
        mat <- DelayedArray::extract_array(
          self$expression,
          list(gene_idx, cell_idx)
        )
        out <- as.numeric(mat)
        names(out) <- cells
        return(out)
      }

      ## BPCells IterableMatrix: native [gene, cells] returns another
      ## IterableMatrix; coerce 1 x n to numeric.
      if (inherits(self$expression, "IterableMatrix")) {
        sub <- self$expression[gene, cells, drop = FALSE]
        out <- as.numeric(as.matrix(sub))
        names(out) <- cells
        return(out)
      }

      ## dgCMatrix / base matrix: [gene, cells] already returns a named
      ## numeric vector without densifying the full matrix.
      out <- as.numeric(self$expression[gene, cells])
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
    #' @param cells Names/barcodes of cells to extract; \code{NULL} returns all cells.
    #' @return
    #' A sub-matrix of the same concrete class as \code{self$expression}:
    #' \code{dgCMatrix} stays \code{dgCMatrix}, \code{RleMatrix} yields
    #' \code{DelayedMatrix}, \code{IterableMatrix} stays \code{IterableMatrix}.
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
      if (is.null(cells)) {
        cells <- colnames(self$expression)
      }
      if (!is.character(cells) || anyNA(cells)) {
        stop(
          "`cells` must be a character vector of non-NA cell names/barcodes.",
          call. = FALSE
        )
      }

      gene_idx <- match(genes, rownames(self$expression))
      missing_genes <- genes[is.na(gene_idx)]
      if (length(missing_genes) > 0L) {
        stop(
          "Gene(s) not found in expression matrix: ",
          paste(utils::head(missing_genes, 5), collapse = ", "),
          if (length(missing_genes) > 5L) " ..." else "",
          call. = FALSE
        )
      }
      cell_idx <- match(cells, colnames(self$expression))
      missing_cells <- cells[is.na(cell_idx)]
      if (length(missing_cells) > 0L) {
        stop(
          "Cell(s) not found in expression matrix: ",
          paste(utils::head(missing_cells, 5), collapse = ", "),
          if (length(missing_cells) > 5L) " ..." else "",
          call. = FALSE
        )
      }

      ## DelayedArray subsetting by integer indices preserves laziness and
      ## avoids relying on every delayed backend supporting character subscripts.
      if (
        inherits(self$expression, "DelayedArray") ||
          inherits(self$expression, "DelayedMatrix") ||
          inherits(self$expression, "RleMatrix")
      ) {
        return(self$expression[gene_idx, cell_idx, drop = FALSE])
      }

      ## Native character subscripting preserves dgCMatrix/base matrix and
      ## IterableMatrix classes while keeping dimnames aligned to the request.
      self$expression[genes, cells, drop = FALSE]
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
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
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
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
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
    getMethodsWithEnrichedPathways = function() {
      return(names(self$enriched_pathways))
    },

    #' @description
    #' Alias of \code{getMethodsWithEnrichedPathways()}, kept for backwards
    #' compatibility with the Shiny app, which calls this name.
    #'
    #' @return
    #' \code{vector} of methods for which enriched pathways are available.
    getMethodsForEnrichedPathways = function() {
      return(self$getMethodsWithEnrichedPathways())
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
      if (method %in% self$getMethodsWithEnrichedPathways() == FALSE) {
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
      if (method %in% self$getMethodsWithEnrichedPathways() == FALSE) {
        stop(glue::glue('Method `{method}` is not available.'), call. = FALSE)
      } else {
        if (
          group_name %in% self$getGroupsWithEnrichedPathways(method) == FALSE
        ) {
          stop(
            glue::glue(
              'Group `{group_name}` is not available for method `{method}`.'
            ),
            call. = FALSE
          )
        } else {
          return(self$enriched_pathways[[method]][[group_name]])
        }
      }
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
      if (method %in% self$getMethodsForTrajectories() == FALSE) {
        stop(glue::glue('Method `{method}` is not available.'), call. = FALSE)
      } else {
        if (trajectory_name %in% self$getNamesOfTrajectories(method) == FALSE) {
          stop(
            glue::glue(
              'Trajectory `{trajectory_name}` is not available for method `{method}`.'
            ),
            call. = FALSE
          )
        } else {
          return(self$trajectories[[method]][[trajectory_name]])
        }
      }
    },

    #' @description
    #' Retrieve BCR data
    #'
    #' @return
    #' BCR data stored in the object.
    getBCR = function() {
      return(self$bcr_data)
    },

    #' @description
    #' Retrieve TCR data
    #'
    #' @return
    #' TCR data stored in the object.
    getTCR = function() {
      return(self$tcr_data)
    },

    #' @description
    #' Add BCR data.
    #'
    #' @param data \code{list} that contains BCR data.
    addBCRData = function(data) {
      self$bcr_data <- data
    },

    #' @description
    #' Add TCR data.
    #'
    #' @param data \code{list} that contains TCR data.
    addTCRData = function(data) {
      self$tcr_data <- data
    },

    #' @description
    #' Get immune repertoire data. Returns the unified \code{immune_repertoire}
    #' field if available; otherwise falls back to merging legacy
    #' \code{bcr_data} and \code{tcr_data}.
    #'
    #' @return Named list of data.frames (one per sample), or empty list.
    getImmuneRepertoire = function() {
      if (length(self$immune_repertoire) > 0) {
        return(self$immune_repertoire)
      }
      # Backward compatibility: merge legacy bcr + tcr
      merged <- c(self$bcr_data, self$tcr_data)
      return(merged)
    },

    #' @description
    #' Set immune repertoire data.
    #'
    #' @param data Named list of data.frames (one per sample) containing
    #'   scRepertoire columns.
    addImmuneRepertoire = function(data) {
      self$immune_repertoire <- data
    },

    #' @description
    #' Get HLA typing data (canonical long \code{data.frame}), or an empty
    #' table when none is stored. Safe on older objects that predate the field.
    #'
    #' @return A canonical HLA typing \code{data.frame} (possibly zero-row).
    getHLATyping = function() {
      # `hla_typing` may be absent on objects deserialized from older .crb
      # files; guard with a tryCatch so the getter never errors.
      ht <- tryCatch(self$hla_typing, error = function(e) NULL)
      if (is.data.frame(ht)) {
        return(ht)
      }
      # Nothing stored (or an older object predating the field): return a
      # canonical EMPTY table built with base R only.
      #
      # This must NOT call a package-internal helper (e.g. hla_normalize_typing)
      # to synthesise the empty table. A .crb is an R6 object whose methods are
      # serialized with an enclosing environment that references the
      # cerebroAppLite namespace; a createShinyApp() bundle never loads that
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
    #' @param name Name of the spatial data entry (e.g. image name).
    #' @param data \code{list} containing 'coordinates' (data.frame) and
    #'   'expression' (sparse matrix). It may optionally carry an embedded
    #'   histology image as 'image' (a base64 \code{data:} URI string) plus
    #'   'image_bounds' (named list xmin/xmax/ymin/ymax in coordinate space)
    #'   so the Spatial tab can render the real tissue background without an
    #'   external file.
    addSpatialData = function(name, data) {
      if (
        !is.list(data) || !all(c("coordinates", "expression") %in% names(data))
      ) {
        stop(
          "Spatial data must be a list containing 'coordinates' and 'expression'."
        )
      }
      self$spatial[[name]] <- data
    },

    #' @description
    #' Retrieve spatial data.
    #'
    #' @param name Name of the spatial data entry.
    #'
    #' @return
    #' \code{list} containing 'coordinates' and 'expression'.
    getSpatialData = function(name) {
      if (name %in% names(self$spatial) == FALSE) {
        stop(
          glue::glue('Spatial data `{name}` is not available.'),
          call. = FALSE
        )
      }
      return(self$spatial[[name]])
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
    #' stored. Safe on older objects that predate the field (the slot is read
    #' through a \code{tryCatch} so the getter never errors on a legacy .crb).
    #'
    #' @return A \code{list} with the Trekker page's content, or \code{NULL}.
    getTrekker = function() {
      tryCatch(self$trekker, error = function(e) NULL)
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
      if (
        !is.null(self$extra_material) &&
          !is.null(self$extra_material$tables) &&
          is.list(self$extra_material$tables) &&
          name %in% names(self$extra_material$tables)
      ) {
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
      if (
        !is.null(self$extra_material) &&
          !is.null(self$extra_material$plots) &&
          is.list(self$extra_material$plots) &&
          name %in% names(self$extra_material$plots)
      ) {
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
          'class: Cerebro_v1.3',
          '\n',
          'cerebroApp version: ',
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
          private$showMarkerGenes(),
          '\n',
          'enriched pathways:',
          private$showEnrichedPathways(),
          '\n',
          'trajectories:',
          private$showTrajectories(),
          '\n',
          'extra material:',
          private$showExtraMaterial(),
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
    ## Extract expression matrix (helper)
    ##   cells  Names/barcodes of cells to extract; NULL for all.
    ##   genes  Names of genes to extract; NULL for all.
    ## Returns a dense matrix.
    extractExpression = function(cells = NULL, genes = NULL) {
      ## check what kind of matrix the transcription counts are stored as
      ## ... DelayedArray / RleMatrix
      if (inherits(self$expression, 'RleMatrix')) {
        ## resolve cell indices
        if (!is.null(cells)) {
          cell_indices <- match(cells, colnames(self$expression))
        } else {
          cell_indices <- NULL
          cells <- colnames(self$expression)
        }

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

        ## assign names (extract_array might lose them or not return them for indices)
        colnames(mat) <- cells
        rownames(mat) <- genes

        return(mat)
      } else {
        ## standard matrix logic
        if (is.null(cells)) {
          cells <- colnames(self$expression)
        }
        if (is.null(genes)) {
          genes <- rownames(self$expression)
        }

        return(
          as.matrix(self$expression[genes, cells, drop = FALSE])
        )
      }
    },

    #' Print overview of available marker gene results for \code{self$print()}
    #' function.
    showMarkerGenes = function() {
      text <- list()
      for (method in names(self$marker_genes)) {
        text[[method]] <- paste0(
          '\n  - ',
          method,
          ' (',
          length(names(self$marker_genes[[method]])),
          '): ',
          paste0(names(self$marker_genes[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of available enriched pathway results for
    #' \code{self$print()} function.
    showEnrichedPathways = function() {
      text <- list()
      for (method in names(self$enriched_pathways)) {
        text[[method]] <- paste0(
          '\n  - ',
          method,
          ' (',
          length(names(self$enriched_pathways[[method]])),
          '): ',
          paste0(names(self$enriched_pathways[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of available trajectories for \code{self$print()} function.
    showTrajectories = function() {
      text <- list()
      for (method in names(self$trajectories)) {
        text[[method]] <- paste0(
          '\n  - ',
          method,
          ' (',
          length(names(self$trajectories[[method]])),
          '): ',
          paste0(names(self$trajectories[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of extra material for \code{self$print()} function.
    showExtraMaterial = function() {
      text <- list()
      for (category in names(self$extra_material)) {
        text[[category]] <- paste0(
          '\n  - ',
          category,
          ' (',
          length(names(self$extra_material[[category]])),
          '): ',
          paste0(names(self$extra_material[[category]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    }
  )
)
