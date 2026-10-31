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
      levels <- unique(c(levels, 'N/A'))
    }
    return(factor(values, levels = levels))
  }
  if (is.factor(values) && anyNA(values)) {
    if (verbose) {
      .logStep('Adding `NA` to factor levels of group `', group, '`...')
    }
    levels <- levels(values)
    values <- as.character(values)
    values[is.na(values)] <- 'N/A'
    return(factor(values, levels = unique(c(levels, 'N/A'))))
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
  verbose,
  main_group = NULL
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
    ## Source columns named like the reserved ones would overwrite the cell
    ## barcodes and the chosen nUMI/nGene values, so they are left out.
    other <- setdiff(
      names(meta),
      c("cell_barcode", "nUMI", "nGene", groups, nUMI, nGene, cell_cycle)
    )
    for (column in other) {
      meta_data[[column]] <- meta[[column]]
    }
  }
  colnames(meta_data) <- make.unique(colnames(meta_data))
  export$setMetaData(meta_data)
  for (group in groups) {
    export$addGroup(group, levels(meta_data[[group]]))
  }
  if (!is.null(main_group)) {
    export$addParameters('main_group', main_group)
  }
  if (length(cell_cycle) > 0) {
    export$setCellCycle(cell_cycle)
  }
}

## Non-PCA reductions; PCA only when it is the only one, or every requested
## reduction in the requested order when `explicit`. `embedding(name)` returns
## the cell embeddings of one reduction.
.addExportProjections <- function(
  export,
  available,
  embedding,
  verbose,
  explicit = FALSE
) {
  if (verbose) {
    .logStep('Extracting dimensional reductions...')
  }
  if (length(available) == 0) {
    stop(
      'No dimensional reductions available. Add at least one first, e.g. ',
      'scater::runPCA() / scater::runUMAP() on a SingleCellExperiment, ',
      'scanpy.tl.umap() on an AnnData, or RunUMAP() on a Seurat object.',
      call. = FALSE
    )
  }
  is_pca <- grepl('pca', available, ignore.case = TRUE)
  chosen <- if (explicit) available else available[!is_pca]
  if (!explicit && length(available) == 1 && is_pca) {
    export$addProjection(available, as.data.frame(embedding(available)))
    warning(
      'Warning: Only PCA as dimensional reduction found, will export ',
      'first and second principal components. Consider using tSNE and/or ',
      'UMAP instead.',
      call. = FALSE
    )
  } else if (length(chosen) > 0) {
    if (verbose) {
      .logStep(
        'Will export the following dimensional reductions: ',
        paste(chosen, collapse = ', ')
      )
    }
    for (projection in chosen) {
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

## Immune repertoire, HLA typing and the legacy TCR/BCR slots of `extras` (the
## misc list of a Seurat object, the metadata of an SCE). `where` names it in
## error messages, e.g. "@misc". The legacy slots are checked like the unified
## one and merged into the one repertoire the .crb stores.
.addExportImmune <- function(export, extras, cells, where, verbose) {
  ## `is.list()` was the whole guard, and a data.frame passes it -- its columns
  ## became the sample names and the wrong shape reached the .crb intact.
  unified <- .dropEmptyRepertoireSamples(extras$immune_repertoire)
  has_unified <- !is.null(unified) && length(unified) > 0
  if (has_unified) {
    unified <- .normalizeImmuneRepertoire(
      unified,
      cell_barcodes = cells,
      source_label = paste0('`', where, '$immune_repertoire`')
    )
    if (verbose) {
      .logStep(
        'Extracting immune repertoire data (',
        length(unified),
        ' samples)...'
      )
    }
    export$addImmuneRepertoire(unified)
  }

  ## HLA typing: a canonical long data.frame, a wide sample x locus table, or a
  ## named list (sample -> allele vector). The source type is read from an
  ## optional `hla_typing_source_type` (default "unknown"), so an uploaded or
  ## imputed genotype is never silently treated as directly typed.
  hla <- extras$hla_typing
  if (
    !is.null(hla) &&
      (is.data.frame(hla) || (is.list(hla) && length(hla) > 0))
  ) {
    if (verbose) {
      .logStep('Extracting HLA typing...')
    }
    source_type <- extras$hla_typing_source_type
    if (is.null(source_type) || !nzchar(source_type)) {
      source_type <- 'unknown'
    }
    export$addHLATyping(hla, source_type = source_type)
  }

  if (!has_unified) {
    receptor_slots <- list(tcr_data = 'TCR', bcr_data = 'BCR')
    receptors <- lapply(names(receptor_slots), function(slot) {
      data <- .dropEmptyRepertoireSamples(extras[[slot]])
      if (is.null(data)) {
        return(NULL)
      }
      if (verbose) {
        .logStep('Extracting tables of ', receptor_slots[[slot]], ' data...')
      }
      .normalizeImmuneRepertoire(
        data,
        cell_barcodes = cells,
        source_label = paste0('`', where, '$', slot, '`')
      )
    })
    merged <- .dropEmptyRepertoireSamples(
      do.call(.mergeRepertoiresBySample, receptors)
    )
    if (!is.null(merged) && length(merged) > 0) {
      export$addImmuneRepertoire(.normalizeImmuneRepertoire(
        merged,
        cell_barcodes = cells,
        source_label = 'the merged TCR and BCR repertoire'
      ))
    }
  }
}

## Trajectory methods that are exported: Monocle 2, and what readH5adAsSCE()
## makes from scanpy/scVelo results. Other methods are skipped with a warning.
.exportTrajectoryMethods <- c('monocle2', 'paga', 'scvelo', 'pseudotime')

.addExportTrajectories <- function(
  export,
  trajectories,
  verbose,
  supported_note = NULL
) {
  if (is.null(supported_note)) {
    supported_note <- paste0(
      'only trajectories of the methods ',
      paste0('`', .exportTrajectoryMethods, '`', collapse = ', '),
      ' are supported.'
    )
  }
  if (length(trajectories) == 0) {
    if (verbose) {
      .logStep('No trajectories to extract...')
    }
    return(invisible())
  }
  supported <- intersect(names(trajectories), .exportTrajectoryMethods)
  if (verbose) {
    .logStep(
      'Will export the following trajectories: ',
      paste(unlist(lapply(trajectories[supported], names)), collapse = ', ')
    )
  }
  for (method in names(trajectories)) {
    if (method %in% .exportTrajectoryMethods) {
      for (name in names(trajectories[[method]])) {
        export$addTrajectory(method, name, trajectories[[method]][[name]])
      }
    } else {
      warning(
        'Warning: Skipping trajectories of method `',
        method,
        '`. At the moment, ',
        supported_note,
        call. = FALSE
      )
    }
  }
}

## Extra tables and plots, and the workbook/sheet display names of the tables
## (kept as technical info, not as a content category).
.addExportExtraMaterial <- function(export, extra_material, verbose) {
  if (is.list(extra_material) && length(extra_material) > 0) {
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
  table_index <- viewerExtraMaterialTableIndex(
    extra_material$table_index,
    as.character(names(export$getExtraMaterial()$tables))
  )
  if (length(table_index)) {
    export$addTechnicalInfo("extra_material_table_index", table_index)
  }
}
