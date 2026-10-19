## Steps shared by exportFromSCE() and exportFromSeurat(). `meta` is the cell
## meta data (colData() or @meta.data); `extras` is the list that holds the
## analysis results (@metadata or @misc).

.validateExportColumns <- function(
  meta_names,
  groups,
  nUMI,
  nGene,
  cell_cycle,
  main_group = NULL
) {
  missing_groups <- groups[!groups %in% meta_names]
  if (length(missing_groups) > 0) {
    stop(
      'Some group columns could not be found in meta data: ',
      paste0(missing_groups, collapse = ', '),
      call. = FALSE
    )
  }
  if (!is.null(main_group) && !(main_group %in% groups)) {
    stop(
      'Specified main_group `',
      main_group,
      '` is not in the list of groups. Valid options are: ',
      paste(groups, collapse = ', '),
      call. = FALSE
    )
  }
  if (!(nUMI %in% meta_names)) {
    stop(
      'Column with number of transcripts per cell (`',
      nUMI,
      '`) not found in meta data.',
      call. = FALSE
    )
  }
  if (!(nGene %in% meta_names)) {
    stop(
      'Column with number of expressed genes per cell (`',
      nGene,
      '`) not found in meta data.',
      call. = FALSE
    )
  }
  missing_cell_cycle <- cell_cycle[!cell_cycle %in% meta_names]
  if (length(missing_cell_cycle) > 0) {
    stop(
      'Some cell cycle columns could not be found in meta data: ',
      paste0(missing_cell_cycle, collapse = ', '),
      call. = FALSE
    )
  }
}

.newCerebroExport <- function(experiment_name, organism, verbose) {
  .logStep(
    if (verbose) {
      'Initializing Cerebro object...'
    } else {
      'Start collecting data...'
    }
  )
  export <- Cerebro$new()
  export$addExperiment('experiment_name', experiment_name)
  export$addExperiment('organism', organism)
  export$setVersion(utils::packageVersion('CerebroNexus'))
  export
}

## Convert the matrix to an RleArray when `use_delayed_array` asks for it.
.delayedExportExpression <- function(
  expression_data,
  use_delayed_array,
  verbose
) {
  if (
    use_delayed_array == TRUE &&
      inherits(expression_data, c('matrix', 'dgCMatrix')) &&
      requireNamespace("DelayedArray", quietly = TRUE)
  ) {
    if (verbose) {
      .logStep('Storing expression data as DelayedArray...')
    }
    expression_data <- methods::as(expression_data, "RleArray")
  }
  expression_data
}

## Experiment date, parameters, technical info and gene lists.
.addExportExperiment <- function(export, extras) {
  if (!is.null(extras$experiment$date_of_analysis)) {
    export$addExperiment(
      'date_of_analysis',
      extras$experiment$date_of_analysis
    )
  }
  export$addExperiment('date_of_export', Sys.Date())
  for (name in names(extras$parameters)) {
    export$addParameters(name, extras$parameters[[name]])
  }
  for (i in seq_along(extras$technical_info)) {
    export$addTechnicalInfo(
      names(extras$technical_info)[i],
      extras$technical_info[[i]]
    )
  }
  for (i in seq_along(extras$gene_lists)) {
    export$addGeneList(names(extras$gene_lists)[i], extras$gene_lists[[i]])
  }
}

## Character groups become factors and NA values a trailing 'N/A' level.
.exportGroupColumn <- function(values, group, verbose) {
  if (!is.factor(values) && is.character(values)) {
    levels <- sort(unique(values), na.last = NA)
    if (anyNA(values)) {
      values[is.na(values)] <- 'N/A'
      levels <- c(levels, 'N/A')
    }
    return(factor(values, levels = levels))
  }
  if (is.factor(values) && anyNA(values) && !'NA' %in% levels(values)) {
    if (verbose) {
      .logStep('Adding `NA` to factor levels of group `', group, '`...')
    }
    levels <- levels(values)
    values <- as.character(values)
    values[is.na(values)] <- 'N/A'
    return(factor(values, levels = c(levels, 'N/A')))
  }
  values
}

## Meta data table, grouping variables and cell cycle columns.
.addExportMetaData <- function(
  export,
  meta,
  cells,
  groups,
  nUMI,
  nGene,
  cell_cycle,
  add_all_meta_data,
  verbose
) {
  if (verbose) {
    .logStep('Collecting available meta data...')
  }
  meta_data <- data.frame("cell_barcode" = cells, stringsAsFactors = FALSE)
  for (group in groups) {
    meta_data[[group]] <- .exportGroupColumn(meta[[group]], group, verbose)
  }
  meta_data[["nUMI"]] <- meta[[nUMI]]
  meta_data[["nGene"]] <- meta[[nGene]]
  for (column in cell_cycle) {
    values <- meta[[column]]
    meta_data[[column]] <- factor(
      values,
      levels = if (is.factor(values)) levels(values) else unique(values)
    )
  }
  if (add_all_meta_data == TRUE) {
    if (verbose) {
      .logStep('Extracting all meta data columns...')
    }
    other <- setdiff(names(meta), c(groups, nUMI, nGene, cell_cycle))
    for (column in other) {
      meta_data[[column]] <- meta[[column]]
    }
  }
  colnames(meta_data) <- make.unique(colnames(meta_data))
  export$setMetaData(meta_data)
  for (group in groups) {
    export$addGroup(group, levels(meta_data[[group]]))
  }
  if (length(cell_cycle) > 0) {
    export$setCellCycle(cell_cycle)
  }
}

## Non-PCA reductions; PCA only when it is the only one. `embedding(name)`
## returns the cell embeddings of one reduction.
.addExportProjections <- function(export, available, embedding, verbose) {
  if (verbose) {
    .logStep('Extracting dimensional reductions...')
  }
  is_pca <- grepl('pca', available, ignore.case = TRUE)
  if (length(available) == 0) {
    stop('No dimensional reductions available.', call. = FALSE)
  }
  if (length(available) == 1 && is_pca) {
    export$addProjection(available, as.data.frame(embedding(available)))
    warning(
      'Warning: Only PCA as dimensional reduction found, will export ',
      'first and second principal components. Consider using tSNE and/or ',
      'UMAP instead.',
      call. = FALSE
    )
  } else if (any(!is_pca)) {
    if (verbose) {
      .logStep(
        'Will export the following dimensional reductions: ',
        paste(available[!is_pca], collapse = ', ')
      )
    }
    for (projection in available[!is_pca]) {
      export$addProjection(projection, as.data.frame(embedding(projection)))
    }
  }
}

## Tables keyed by group (trees, most expressed genes, mean expression), added
## with `add(group, table)`. A non-NULL `groups` skips tables of other groups.
.addExportGroupTables <- function(
  tables,
  label,
  log,
  add,
  verbose,
  groups = NULL
) {
  if (is.null(tables)) {
    return(invisible())
  }
  if (!is.list(tables)) {
    stop('`', label, '` is not a list.', call. = FALSE)
  }
  if (verbose) {
    .logStep(log)
  }
  for (i in seq_along(tables)) {
    group <- names(tables)[i]
    if (is.null(groups) || group %in% groups) {
      add(group, tables[[i]])
    }
  }
}

## Tables keyed by method and group (marker genes, enriched pathways), added
## with `add(method, group, table)`. `groups` works as above.
.addExportMethodTables <- function(
  tables,
  label,
  log,
  add,
  verbose,
  groups = NULL
) {
  if (is.null(tables)) {
    return(invisible())
  }
  if (!is.list(tables)) {
    stop('`', label, '` is not a list.', call. = FALSE)
  }
  if (verbose) {
    .logStep(log)
  }
  for (method in names(tables)) {
    for (j in seq_along(tables[[method]])) {
      if (is.list(tables[[method]][j])) {
        group <- names(tables[[method]])[j]
        if (is.null(groups) || group %in% groups) {
          add(method, group, tables[[method]][[group]])
        }
      }
    }
  }
}

## Monocle 2 trajectories; other methods are skipped with a warning.
.addExportTrajectories <- function(export, trajectories, verbose) {
  if (length(trajectories) == 0) {
    if (verbose) {
      .logStep('No trajectories to extract...')
    }
    return(invisible())
  }
  if (verbose) {
    .logStep(
      'Will export the following trajectories: ',
      paste(names(trajectories$monocle2), collapse = ', ')
    )
  }
  for (i in seq_along(trajectories)) {
    method <- names(trajectories)[i]
    if (method == 'monocle2') {
      for (j in seq_along(trajectories[[i]])) {
        export$addTrajectory(
          method,
          names(trajectories[[i]])[j],
          trajectories[[i]][[j]]
        )
      }
    } else {
      warning(
        'Warning: Skipping trajectories of method `',
        method,
        '`. At the moment, only trajectories generated with Monocle 2 ',
        '(`monocle2`) are supported.',
        call. = FALSE
      )
    }
  }
}

## Extra tables and plots.
.addExportExtraMaterial <- function(export, extra_material, verbose) {
  if (!is.list(extra_material) || length(extra_material) == 0) {
    return(invisible())
  }
  if (verbose) {
    .logStep('Found extra material to export...')
  }
  for (category in names(extra_material)) {
    if (category %in% c('tables', 'plots')) {
      content <- extra_material[[category]]
      for (i in seq_along(content)) {
        export$addExtraMaterial(
          category = category,
          name = names(content)[i],
          content = content[[i]]
        )
      }
    }
  }
}

.printExportOverview <- function(export) {
  .logStep('Overview of Cerebro object:\n')
  export$print()
}
