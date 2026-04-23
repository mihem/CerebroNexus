setOldClass(Classes = 'package_version')

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

    #' @field meta_data \code{data.frame} that contains cell meta data.
    meta_data = data.frame(),

    #' @field projections \code{list} that contains projections/dimensional
    #' reductions.
    projections = list(),

    #' @field most_expressed_genes \code{list} that contains a \code{data.frame}
    #' holding the most expressed genes for each grouping variable that was
    #' specified during the call to \code{\link{getMostExpressedGenes}}.
    most_expressed_genes = list(),

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

    #' @field bcr_data \code{list} that contains BCR data.
    bcr_data = list(),

    #' @field tcr_data \code{list} that contains TCR data.
    tcr_data = list(),

    #' @field spatial \code{list} that contains spatial data (coordinates and expression).
    spatial = list(),

    ##------------------------------------------------------------------------##
    ## methods to interact with the object
    ##------------------------------------------------------------------------##

    #' @description
    #' Create a new \code{Cerebro} object.
    #'
    #' @return
    #' A new \code{Cerebro} object.
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
      if ( group_name %in% names(self$groups) == FALSE ) {
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
      if ( group_name %in% colnames(self$meta_data) == FALSE ) {
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
      if (inherits(table, "DFrame")) table <- as.data.frame(table)

      if (!is.null(self$expression)) {
        if (nrow(table) != ncol(self$expression)) {
           stop(glue::glue("Number of rows in meta data ({nrow(table)}) must match number of columns in expression matrix ({ncol(self$expression)})."))
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
    setExpression = function(counts) {
      if ( !inherits(counts, c("matrix", "dgCMatrix", "RleMatrix", "DelayedMatrix")) ) {
        warning("Expression data should ideally be a matrix-like object (matrix, dgCMatrix, RleMatrix, etc).")
      }
      if (!is.null(self$meta_data) && nrow(self$meta_data) > 0) {
        if (ncol(counts) != nrow(self$meta_data)) {
          stop(glue::glue("Number of columns in expression matrix ({ncol(counts)}) must match number of rows in meta data ({nrow(self$meta_data)})."))
        }
      }
      self$expression <- counts
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

      ## extract dense matrix using helper
      mat <- private$extractExpression(cells = NULL, genes = genes)

      ## calculate mean expression per gene
      mean_expression <- Matrix::rowMeans(mat)

      ##
      return(
        data.frame(
          "gene" = genes,
          "expression" = mean_expression
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
    #' Add columns containing cell cycle assignments to the \code{cell_cycle}
    #' field.
    #'
    #' @param cols \code{vector} of columns names containing cell cycle
    #' assignments.
    setCellCycle = function(cols) {
      if ( length(cols) == 1 ) {
        self$checkIfColumnExistsInMetadata(cols)
        self$cell_cycle <- cols
      } else {
        for ( i in seq_along(cols) ) {
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
      if ( name %in% names(self$projections) ) {
        stop(
          glue::glue(
            'A projection with the name `{name}` already exists. ',
            'Please use a different name.'
          ),
          call. = FALSE
        )
      }
      ## check if provided projection is a data frame
      if ( !is.data.frame(projection) ) {
        stop(
          glue::glue(
            'Provided projection is of type `{class(projection)}` but should ',
            'be a data frame. Please convert it.'
          ),
          call. = FALSE
        )
      }
      ## check dimensions
      if ( !is.null(self$expression) && nrow(projection) != ncol(self$expression) ) {
         stop(glue::glue("Number of rows in projection ({nrow(projection)}) must match number of cells ({ncol(self$expression)})."))
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
      if ( name %in% self$availableProjections() == FALSE ) {
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
    #' Add table of marker genes.
    #'
    #' @param method Name of method that was used to calculate marker genes.
    #' @param group_name Name of grouping variable that the marker genes belong
    #' to. Must be registered in the \code{groups} field.
    #' @param table \code{data.frame} that contains the marker genes.
    addMarkerGenes = function(method, group_name, table) {
      self$checkIfGroupExists(group_name)
      self$checkIfColumnExistsInMetadata(group_name)
      self$marker_genes[[method]][[group_name]] <- table
    },

    #' @description
    #' Retrieve names of methods for which marker genes are available.
    #'
    #' @return
    #' \code{vector} of methods for which marker genes are available.
    getMethodsWithMarkerGenes = function() {
      return(names(self$marker_genes))
    },

    #' @description
    #' Retrieve names of grouping variables for which marker genes are
    #' available for a specific method.
    #'
    #' @param method Name of method for which to retrieve grouping variables.
    #'
    #' @return
    #' \code{vector} of grouping variables for which marker genes are available.
    getGroupsWithMarkerGenes = function(method) {
      if ( method %in% self$getMethodsWithMarkerGenes() == FALSE ) {
        stop(glue::glue('Method `{method}` is not available.'), call. = FALSE)
      } else {
        return(names(self$marker_genes[[method]]))
      }
    },

    #' @description
    #' Retrieve table of marker genes for a specific method and grouping
    #' variable.
    #'
    #' @param method Name of method for which to retrieve marker genes.
    #' @param group_name Name of grouping variable for which to retrieve marker
    #' genes.
    #'
    #' @return
    #' \code{data.frame} containing the marker genes.
    getMarkerGenes = function(method, group_name) {
      if ( method %in% self$getMethodsWithMarkerGenes() == FALSE ) {
        stop(glue::glue('Method `{method}` is not available.'), call. = FALSE)
      } else {
        if ( group_name %in% self$getGroupsWithMarkerGenes(method) == FALSE ) {
          stop(
            glue::glue('Group `{group_name}` is not available for method `{method}`.'),
            call. = FALSE
          )
        } else {
          return(self$marker_genes[[method]][[group_name]])
        }
      }
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
    #' Retrieve names of grouping variables for which enriched pathways are
    #' available for a specific method.
    #'
    #' @param method Name of method for which to retrieve grouping variables.
    #'
    #' @return
    #' \code{vector} of grouping variables for which enriched pathways are
    #' available.
    getGroupsWithEnrichedPathways = function(method) {
      if ( method %in% self$getMethodsWithEnrichedPathways() == FALSE ) {
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
      if ( method %in% self$getMethodsWithEnrichedPathways() == FALSE ) {
        stop(glue::glue('Method `{method}` is not available.'), call. = FALSE)
      } else {
        if ( group_name %in% self$getGroupsWithEnrichedPathways(method) == FALSE ) {
          stop(
            glue::glue('Group `{group_name}` is not available for method `{method}`.'),
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
    getMethodsWithTrajectories = function() {
      return(names(self$trajectories))
    },

    #' @description
    #' Retrieve names of trajectories for a specific method.
    #'
    #' @param method Name of method for which to retrieve trajectories.
    #'
    #' @return
    #' \code{vector} of trajectories for the specified method.
    getTrajectories = function(method) {
      if ( method %in% self$getMethodsWithTrajectories() == FALSE ) {
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
      if ( method %in% self$getMethodsWithTrajectories() == FALSE ) {
        stop(glue::glue('Method `{method}` is not available.'), call. = FALSE)
      } else {
        if ( trajectory_name %in% self$getTrajectories(method) == FALSE ) {
          stop(
            glue::glue('Trajectory `{trajectory_name}` is not available for method `{method}`.'),
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
    #' Add spatial data.
    #'
    #' @param name Name of the spatial data entry (e.g. image name).
    #' @param data \code{list} containing 'coordinates' (data.frame) and 'expression' (sparse matrix).
    addSpatialData = function(name, data) {
      if ( !is.list(data) || !all(c("coordinates", "expression") %in% names(data)) ) {
        stop("Spatial data must be a list containing 'coordinates' and 'expression'.")
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
      if ( name %in% names(self$spatial) == FALSE ) {
        stop(glue::glue('Spatial data `{name}` is not available.'), call. = FALSE)
      }
      return(self$spatial[[name]])
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
      valid_categories <- c('tables','plots')

      ## proceed only if specified category is valid
      if ( category %in% valid_categories == FALSE ) {
        stop(
          glue::glue(
            'Category `{category}` is not one of the valid categories ',
            '({paste0(valid_categories, collapse = ", ")}).'
          ),
          call. = FALSE
        )
      }

      ## call function to add table
      if ( category == 'tables' ) {
        self$addExtraTable(name, content)
      }

      ## call function to add table
      if ( category == 'plots' ) {
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
      if ( !is.data.frame(table) ) {
        if ( 'DFrame' %in% class(table) ) {
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
        self$extra_material$tables[[ name ]] <- table
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
      if ( "ggplot" %in% class(plot) == FALSE ) {
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
        self$extra_material$plots[[ name ]] <- plot
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
      return(self$extra_material$table[[ name ]])
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
      return(self$extra_material$plots[[ name ]])
    },

    #' @description
    #' Show overview of object and the data it contains.
    print = function() {
      message(
        paste0(
          'class: Cerebro_v1.3', '\n',
          'cerebroApp version: ', self$getVersion(), '\n',
          'experiment name: ', self$getExperiment()$experiment_name, '\n',
          'organism: ', self$getExperiment()$organism, '\n',
          'date of analysis: ', self$getExperiment()$date_of_analysis, '\n',
          'date of export: ', self$getExperiment()$date_of_export, '\n',
          'number of cells: ', format(ncol(self$expression), big.mark = ','), '\n',
          'number of genes: ', format(nrow(self$expression), big.mark = ','), '\n',
          'grouping variables (', length(self$getGroups()), '): ',
            paste0(self$getGroups(), collapse = ', '), '\n',
          'cell cycle variables (', length(self$cell_cycle), '): ',
            paste0(self$cell_cycle, collapse = ', '), '\n',
          'projections (', length(self$availableProjections()),'): ',
            paste0(self$availableProjections(), collapse = ', '), '\n',
          'trees (', length(self$trees),'): ',
            paste0(names(self$trees), collapse = ', '), '\n',
          'most expressed genes: ',
            paste0(names(self$most_expressed_genes), collapse = ', '), '\n',
          'marker genes:', private$showMarkerGenes(), '\n',
          'enriched pathways:', private$showEnrichedPathways(), '\n',
          'trajectories:', private$showTrajectories(), '\n',
          'extra material:', private$showExtraMaterial(), '\n',
          'Names of BCR data:', names(self$bcr_data), '\n',
          'Names of TCR data:', names(self$tcr_data), '\n'
        )
      )
    }
  ),

  ## private fields and methods
  private = list(

    #' Extract expression matrix (helper)
    #'
    #' @param cells Names/barcodes of cells to extract; NULL for all.
    #' @param genes Names of genes to extract; NULL for all.
    #' @return Dense matrix.
    extractExpression = function(cells = NULL, genes = NULL) {

      ## check what kind of matrix the transcription counts are stored as
      ## ... DelayedArray / RleMatrix
      if ( inherits(self$expression, 'RleMatrix') ) {

        ## resolve cell indices
        if ( !is.null(cells) ) {
          cell_indices <- match(cells, colnames(self$expression))
        } else {
          cell_indices <- NULL
          cells <- colnames(self$expression)
        }

        ## resolve gene indices
        if ( !is.null(genes) ) {
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
        if ( is.null(cells) ) cells <- colnames(self$expression)
        if ( is.null(genes) ) genes <- rownames(self$expression)

        return(
          as.matrix(self$expression[genes, cells, drop = FALSE])
        )
      }
    },

    #' Print overview of available marker gene results for \code{self$print()}
    #' function.
    showMarkerGenes = function() {
      text <- list()
      for ( method in names(self$marker_genes) ) {
        text[[method]] <- paste0(
          '\n  - ', method, ' (', length(names(self$marker_genes[[method]])), '): ',
          paste0(names(self$marker_genes[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of available enriched pathway results for
    #' \code{self$print()} function.
    showEnrichedPathways = function() {
      text <- list()
      for ( method in names(self$enriched_pathways) ) {
        text[[method]] <- paste0(
          '\n  - ', method, ' (', length(names(self$enriched_pathways[[method]])), '): ',
          paste0(names(self$enriched_pathways[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of available trajectories for \code{self$print()} function.
    showTrajectories = function() {
      text <- list()
      for ( method in names(self$trajectories) ) {
        text[[method]] <- paste0(
          '\n  - ', method, ' (', length(names(self$trajectories[[method]])), '): ',
          paste0(names(self$trajectories[[method]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    },

    #' Print overview of extra material for \code{self$print()} function.
    showExtraMaterial = function() {
      text <- list()
      for ( category in names(self$extra_material) ) {
        text[[category]] <- paste0(
          '\n  - ', category, ' (', length(names(self$extra_material[[category]])), '): ',
          paste0(names(self$extra_material[[category]]), collapse = ', ')
        )
      }
      paste0(text, collapse = ', ')
    }
  )
)
