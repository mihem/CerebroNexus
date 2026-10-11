##----------------------------------------------------------------------------##
## Upload policy.
##----------------------------------------------------------------------------##
viewerUploadsEnabled <- function(options) {
  is.list(options) && identical(options[["mode"]], "open")
}

viewerUploadPath <- function(input_file, options) {
  datapath <- if (is.list(input_file)) input_file[["datapath"]] else NULL
  if (
    !viewerUploadsEnabled(options) ||
      !is.character(datapath) ||
      length(datapath) != 1L ||
      is.na(datapath) ||
      !nzchar(datapath) ||
      !file.exists(datapath)
  ) {
    return("")
  }
  datapath
}

viewerInitialPageDecision <- function(
  initial_tab,
  tab_name,
  should_show,
  applied
) {
  if (isTRUE(applied) || !identical(initial_tab, tab_name)) {
    return(NULL)
  }
  list(
    applied = TRUE,
    selected = if (isTRUE(should_show)) tab_name else NULL
  )
}

viewerDatasetName <- function(files, selected) {
  if (is.null(files) || is.null(selected) || is.null(names(files))) {
    return(NULL)
  }
  if (selected %in% names(files)) {
    return(as.character(selected))
  }
  index <- which(files == selected)
  if (!length(index)) {
    return(NULL)
  }
  name <- names(files)[[index[[1L]]]]
  if (is.na(name) || !nzchar(name)) NULL else name
}

## A data-set context is the only browser-visible identity for one loaded
## object.  It deliberately contains no file path: `dataset_key` is the stable
## configured id (or an opaque runtime id for single/upload mode), `generation`
## advances before every real load/switch, and `epoch` separates reconnects.
viewerDatasetContext <- function(epoch, dataset_key, generation) {
  valid_text <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  generation <- suppressWarnings(as.numeric(generation))
  if (
    !valid_text(epoch) ||
      !valid_text(dataset_key) ||
      length(generation) != 1L ||
      is.na(generation) ||
      !is.finite(generation) ||
      generation < 1 ||
      generation != floor(generation)
  ) {
    stop("Invalid Viewer data-set context.", call. = FALSE)
  }
  list(
    epoch = as.character(epoch),
    dataset_key = as.character(dataset_key),
    generation = unname(generation)
  )
}

viewerDatasetContextEqual <- function(left, right) {
  if (!is.list(left) || !is.list(right)) {
    return(FALSE)
  }
  left <- tryCatch(
    viewerDatasetContext(
      left[["epoch"]],
      left[["dataset_key"]],
      left[["generation"]]
    ),
    error = function(error) NULL
  )
  right <- tryCatch(
    viewerDatasetContext(
      right[["epoch"]],
      right[["dataset_key"]],
      right[["generation"]]
    ),
    error = function(error) NULL
  )
  !is.null(left) &&
    !is.null(right) &&
    identical(left$epoch, right$epoch) &&
    identical(left$dataset_key, right$dataset_key) &&
    isTRUE(left$generation == right$generation)
}

viewerDatasetContextToken <- function(context) {
  context <- viewerDatasetContext(
    context[["epoch"]],
    context[["dataset_key"]],
    context[["generation"]]
  )
  paste0(
    nchar(context$epoch, type = "bytes"),
    ":",
    context$epoch,
    nchar(context$dataset_key, type = "bytes"),
    ":",
    context$dataset_key,
    ":",
    format(context$generation, scientific = FALSE, trim = TRUE)
  )
}

viewerDatasetPath <- function(files, selected) {
  if (
    !is.null(files) &&
      !is.null(names(files)) &&
      !is.null(selected) &&
      selected %in% names(files)
  ) {
    return(unname(files[[selected]]))
  }
  selected
}

viewerDatasetChoices <- function(files, display_labels = NULL) {
  ids <- names(files)
  if (is.null(ids)) {
    ids <- unname(files)
  }
  labels <- display_labels
  if (
    !is.null(labels) &&
      !is.null(names(labels)) &&
      all(ids %in% names(labels))
  ) {
    labels <- labels[ids]
  }
  if (is.null(labels) || length(labels) != length(ids)) {
    labels <- ids
  }
  stats::setNames(ids, unname(labels))
}

viewerSelectedDatasetName <- function() {
  if (
    exists("viewer_current_dataset_key", mode = "function", inherits = TRUE)
  ) {
    current <- tryCatch(
      viewer_current_dataset_key(),
      error = function(error) NULL
    )
    if (is.character(current) && length(current) == 1L && nzchar(current)) {
      return(current)
    }
  }
  if (!exists("available_crb_files", inherits = TRUE)) {
    return(NULL)
  }
  available <- get("available_crb_files", inherits = TRUE)
  viewerDatasetName(available$files, available$selected)
}

viewerDatasetInfo <- function(catalog, selected) {
  if (
    !is.list(catalog) ||
      !is.character(selected) ||
      length(selected) != 1L ||
      is.na(selected) ||
      !nzchar(selected) ||
      is.null(names(catalog)) ||
      !selected %in% names(catalog)
  ) {
    return(NULL)
  }
  info <- catalog[[selected]]
  if (
    !is.list(info) ||
      !identical(
        unname(as.character(info$path)),
        unname(as.character(selected))
      ) ||
      length(info$cells) != 1L ||
      is.na(info$cells) ||
      !is.numeric(info$cells) ||
      !is.finite(info$cells) ||
      info$cells < 0
  ) {
    return(NULL)
  }
  info
}

viewerDatasetLoadRequired <- function(
  tab,
  info,
  initial_tab = NULL,
  initial_page_applied = TRUE
) {
  if (!isTRUE(initial_page_applied) && !is.null(initial_tab)) {
    tab <- initial_tab
  }
  is.null(info) ||
    (is.character(tab) &&
      length(tab) == 1L &&
      !is.na(tab) &&
      nzchar(tab) &&
      !identical(tab, "loadData"))
}

viewerDatasetLoadOnTab <- function(current, tab) {
  isTRUE(current) ||
    (is.character(tab) &&
      length(tab) == 1L &&
      !is.na(tab) &&
      nzchar(tab) &&
      !identical(tab, "loadData"))
}

viewerColourGroupChoices <- function(
  metadata,
  groups,
  main_group
) {
  if (missing(metadata)) {
    metadata <- tryCatch(
      viewerProjectionFirstFrameMetadata(),
      error = function(e) NULL
    )
  }
  if (missing(groups)) {
    groups <- tryCatch(getGroups(), error = function(e) character())
  }
  if (missing(main_group)) {
    parameters <- tryCatch(getParameters(), error = function(e) NULL)
    main_group <- if (is.list(parameters)) parameters[["main_group"]] else NULL
  }

  columns <- tryCatch(colnames(metadata), error = function(e) character())
  groups <- tryCatch(as.character(groups), error = function(e) character())
  groups <- unique(groups[
    !is.na(groups) & nzchar(groups) & groups %in% columns
  ])
  main_group <- tryCatch(as.character(main_group), error = function(e) {
    character()
  })
  selected <- intersect(
    main_group[!is.na(main_group) & nzchar(main_group)],
    groups
  )

  list(
    choices = groups,
    selected = if (length(selected)) {
      selected[[1L]]
    } else if (length(groups)) {
      groups[[1L]]
    } else {
      NULL
    }
  )
}

viewerScatterDefaults <- function(options, dataset = NULL, page = NULL) {
  resolve <- function(key, fallback, minimum, maximum) {
    value <- options[[key]]
    if (length(value) > 1L && !is.null(dataset) && !is.null(names(value))) {
      value <- value[dataset]
    }
    value <- suppressWarnings(as.numeric(value))
    if (
      length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < minimum ||
        value > maximum
    ) {
      fallback
    } else {
      unname(value)
    }
  }
  defaults <- list(
    point_size = resolve("point_size", 5, 1, 20),
    point_opacity = resolve("point_opacity", 1, 0.1, 1),
    percentage_cells_to_show = resolve(
      "percentage_cells_to_show",
      100,
      10,
      100
    )
  )
  if (
    is.null(page) ||
      length(page) != 1L ||
      is.na(page) ||
      !nzchar(page)
  ) {
    return(defaults)
  }
  page <- as.character(page)[[1L]]
  list(
    point_size = resolve(
      paste0(page, "_point_size"),
      defaults$point_size,
      1,
      20
    ),
    point_opacity = resolve(
      paste0(page, "_point_opacity"),
      defaults$point_opacity,
      0.1,
      1
    ),
    percentage_cells_to_show = resolve(
      paste0(page, "_percentage_cells_to_show"),
      defaults$percentage_cells_to_show,
      10,
      100
    )
  )
}

spatialImagePreset <- function(options, dataset, spatial_name, image_label) {
  defaults <- list(
    offsetX = 0,
    offsetY = 0,
    scaleX = 1,
    scaleY = 1,
    flipX = FALSE,
    flipY = FALSE,
    rotation = 0,
    opacity = 0.6
  )
  keys <- list(dataset, spatial_name, image_label)
  if (
    !is.list(options) ||
      any(vapply(
        keys,
        function(key) {
          length(key) != 1L || is.na(key) || !nzchar(key)
        },
        logical(1)
      ))
  ) {
    return(defaults)
  }
  setting <- options[["spatial_image_settings"]][[dataset]][[spatial_name]][[
    image_label
  ]]
  if (!is.list(setting)) {
    return(defaults)
  }
  number <- function(key, fallback, minimum = -Inf, maximum = Inf) {
    value <- suppressWarnings(as.numeric(setting[[key]]))
    if (
      length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < minimum ||
        value > maximum
    ) {
      fallback
    } else {
      unname(value)
    }
  }
  list(
    offsetX = number("offset_x", defaults$offsetX),
    offsetY = number("offset_y", defaults$offsetY),
    scaleX = number("scale_x", defaults$scaleX, .Machine$double.eps),
    scaleY = number("scale_y", defaults$scaleY, .Machine$double.eps),
    flipX = isTRUE(setting[["flip_x"]]),
    flipY = isTRUE(setting[["flip_y"]]),
    rotation = number("rotation", defaults$rotation),
    opacity = number("image_opacity", defaults$opacity, 0, 1)
  )
}

spatialPlotRotation <- function(options, dataset, spatial_name) {
  configured <- if (is.list(options)) {
    options[["spatial_plot_rotation"]]
  } else {
    NULL
  }
  value <- if (
    !is.null(dataset) &&
      !is.null(spatial_name) &&
      is.list(configured) &&
      dataset %in% names(configured) &&
      spatial_name %in% names(configured[[dataset]])
  ) {
    configured[[dataset]][[spatial_name]]
  } else {
    NULL
  }
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || !is.finite(value)) {
    0
  } else {
    unname(value)
  }
}

rotateSpatialCoordinates <- function(coordinates, degrees) {
  if (is.null(coordinates) || identical(degrees, 0)) {
    return(coordinates)
  }
  theta <- degrees * pi / 180
  x <- coordinates[, 1]
  y <- coordinates[, 2]
  coordinates[, 1] <- x * cos(theta) - y * sin(theta)
  coordinates[, 2] <- x * sin(theta) + y * cos(theta)
  coordinates
}

##----------------------------------------------------------------------------##
## Guarded bindCache wrapper for plot/reactive outputs.
##
## Mirrors the immune_repertoire module's ir_bindCache():
##   - no-op on shiny < 1.6.0, where renderPlotly() %>% bindCache() is not
##     supported (DESCRIPTION only requires shiny >= 1.3.2);
##   - cache = "session" so caches are never shared across users/sessions.
## Pass every cache key via `...`, including the dataset identifier
## (available_crb_files$selected) so switching datasets invalidates the cache.
##
## The keys are captured as quosures with enquos() and spliced back into
## bindCache() with !!!, so their expressions reach bindCache() unevaluated.
## This matters for two reasons: bindCache() builds its reactive dependencies
## from the key *expressions*, so forwarding an already-evaluated value would
## break invalidation (e.g. a dataset switch would keep serving the previous
## dataset's plot); and it avoids relying on this helper being sourced into the
## server environment to see available_crb_files. rlang is already a direct
## dependency, so this adds no new package.
##----------------------------------------------------------------------------##
cachePlot <- function(x, ...) {
  keys <- rlang::enquos(...)
  rlang::inject(shiny::bindCache(x, !!!keys, cache = "session"))
}

## Return the first complete reactive value immediately; debounce only later
## invalidations caused by interactive controls.
debounceAfterFirst <- function(reactive, millis) {
  initialized <- FALSE
  current <- NULL
  revision <- shiny::reactiveVal(0L)
  shiny::reactive({
    revision()
    if (!initialized) {
      current <<- reactive()
      initialized <<- TRUE
      delayed <- shiny::debounce(reactive, millis)
      shiny::observeEvent(
        delayed(),
        {
          next_value <- delayed()
          if (!identical(current, next_value)) {
            current <<- next_value
            revision(shiny::isolate(revision()) + 1L)
          }
        },
        ignoreInit = TRUE
      )
    }
    current
  })
}

viewerOutputTab <- function(ids) {
  prefixes <- c(
    overview_ = "overview",
    expression_ = "geneExpression",
    spatial_ = "spatial",
    coordviews_ = "coordinated_views",
    groups_ = "groups",
    ir_ = "immune_repertoire",
    trajectory_ = "trajectory",
    trekker_ = "trekker",
    hla_ = "hla_tcr_motifs"
  )
  vapply(
    ids,
    function(id) {
      match <- which(startsWith(id, names(prefixes)))
      if (length(match)) unname(prefixes[[match[[1L]]]]) else NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
}

viewerProjectionFirstFrameCache <- function(object = NULL) {
  if (is.null(object)) {
    if (!exists("data_set", mode = "function", inherits = TRUE)) {
      return(NULL)
    }
    object <- data_set()
  }
  cache <- attr(object, "cerebro_projection_first_frame", exact = TRUE)
  if (is.list(cache)) cache else NULL
}

viewerPackCurrent <- function() {
  pack <- attr(data_set(), "cerebro_viewer_pack", exact = TRUE)
  if (is.list(pack)) pack else NULL
}

viewerProjectionFirstFrameMetadata <- function() {
  cache <- viewerProjectionFirstFrameCache()
  if (!is.null(cache$meta_data)) cache$meta_data else getMetaData()
}

viewerProjectionFirstFrameCoordinates <- function(name) {
  cache <- viewerProjectionFirstFrameCache()
  projection <- cache$projections[[name]]
  # Only the validated thin-CRB reader creates this identity-free cache.
  if (!is.null(projection) && name %in% cache$canonical_projections) {
    return(projection)
  }
  projection <- getProjection(name)
  cells <- getMetaData()[["cell_barcode"]]
  viewerAlignProjection(projection, cells)
}

viewerAlignProjection <- function(projection, cells) {
  ids <- rownames(projection)
  if (
    is.null(cells) ||
      is.null(ids) ||
      anyNA(cells) ||
      anyNA(ids) ||
      any(!nzchar(cells)) ||
      any(!nzchar(ids)) ||
      anyDuplicated(cells) ||
      anyDuplicated(ids) ||
      length(ids) != length(cells) ||
      !setequal(ids, cells)
  ) {
    stop("Projection requires unique, matching cell identities.", call. = FALSE)
  }
  if (identical(ids, cells)) {
    projection
  } else {
    projection[match(cells, ids), , drop = FALSE]
  }
}

viewerSharedProjectionName <- function(
  projection,
  n_cells,
  shared = NULL,
  identity = NULL,
  dataset_context = NULL
) {
  if (is.null(shared)) {
    if (!exists("input", inherits = TRUE)) {
      return(NULL)
    }
    shared <- input[["coordviews_shared_base"]]
  }
  if (is.null(identity)) {
    if (!exists("viewerDatasetIdentity", mode = "function", inherits = TRUE)) {
      return(NULL)
    }
    identity <- viewerDatasetIdentity()
  }
  if (
    !is.list(shared) ||
      !viewerDatasetContextEqual(shared$dataset_context, dataset_context) ||
      !is.list(identity) ||
      !is.character(projection) ||
      length(projection) != 1L ||
      is.na(projection) ||
      !nzchar(projection) ||
      !identical(as.character(shared$projection %||% ""), projection) ||
      !identical(
        as.character(shared$dataset_fingerprint %||% ""),
        as.character(identity$fingerprint %||% "")
      ) ||
      !identical(as.integer(shared$cell_count), as.integer(n_cells)) ||
      !nzchar(as.character(identity$order_fingerprint %||% "")) ||
      !identical(
        as.character(shared$canonical_order_id %||% ""),
        as.character(identity$order_fingerprint %||% "")
      )
  ) {
    return(NULL)
  }
  projection
}

viewerProjectionDefaults <- function(
  metadata,
  projections,
  parameters = list()
) {
  color_choices <- setdiff(colnames(metadata), "cell_barcode")
  main_group <- parameters[["main_group"]]
  color_variable <- if (
    is.character(main_group) &&
      length(main_group) == 1L &&
      !is.na(main_group) &&
      main_group %in% color_choices
  ) {
    main_group
  } else if ("cell_type" %in% color_choices) {
    "cell_type"
  } else if (length(color_choices)) {
    color_choices[[1L]]
  } else {
    NULL
  }
  list(
    projection = if (length(projections)) projections[[1L]] else NULL,
    color_variable = color_variable
  )
}

## Apply the shared projection filters and sample original metadata row ids.
viewerProjectionCellIndices <- function(
  prefix,
  metadata = getMetaData(),
  percentage = input[[paste0(prefix, "_percentage_cells_to_show")]]
) {
  groups <- getGroups()
  filter_inputs <- stats::setNames(
    lapply(groups, function(group) {
      input[[paste0(prefix, "_group_filter_", group)]]
    }),
    groups
  )
  if (!length(groups) || all(vapply(filter_inputs, is.null, logical(1)))) {
    cell_count <- nrow(metadata)
    if (!cell_count) {
      return(integer())
    }
    if (percentage < 100) {
      size <- ceiling(cell_count * percentage / 100)
      return(sample.int(cell_count, size))
    }
    return(seq_len(cell_count))
  }
  filters <- stats::setNames(
    lapply(groups, function(group) {
      value <- filter_inputs[[group]]
      if (is.null(value)) getGroupLevels(group) else value
    }),
    groups
  )
  filters <- filters[
    !vapply(
      groups,
      function(group) {
        selected <- filters[[group]]
        values <- metadata[[group]]
        if (!is.factor(values) || !length(selected)) {
          return(FALSE)
        }
        levels <- tryCatch(
          as.character(getGroupLevels(group)),
          error = function(error_condition) character()
        )
        length(selected) == length(levels) &&
          setequal(base::levels(values), levels) &&
          setequal(as.character(selected), levels) &&
          !anyNA(values)
      },
      logical(1)
    )
  ]
  if (!length(filters)) {
    cell_count <- nrow(metadata)
    if (!cell_count) {
      return(integer())
    }
    if (percentage < 100) {
      size <- ceiling(cell_count * percentage / 100)
      return(sample.int(cell_count, size))
    }
    return(seq_len(cell_count))
  }
  indices <- which(cerebroGroupFilterMask(metadata, filters))
  if (!length(indices)) {
    return(indices)
  }
  size <- if (percentage < 100) {
    ceiling(length(indices) * percentage / 100)
  } else {
    return(indices)
  }
  indices[sample.int(length(indices), size)]
}

viewerProjectionMetadataColumns <- function(
  metadata,
  color_variable,
  hover_info,
  groups
) {
  columns <- c("cell_barcode", color_variable)
  if (isTRUE(hover_info)) {
    columns <- c(columns, "nUMI", "nGene", groups)
  }
  unique(columns[columns %in% colnames(metadata)])
}

viewerProjectionFirstFrameColumns <- function(metadata, color_variable) {
  unique(color_variable[color_variable %in% colnames(metadata)])
}

viewerProjectionSubsetRows <- function(
  table,
  indices,
  columns = colnames(table)
) {
  if (is.null(columns)) {
    columns <- colnames(table)
  }
  table <- table[, columns, drop = FALSE]
  if (identical(indices, seq_len(nrow(table)))) {
    return(table)
  }
  table[indices, , drop = FALSE]
}

## Prefer the R6 row accessor so a single-gene request never needs a temporary
## 1 x cells matrix. Older serialized objects fall back to the matrix method.
viewerExpressionCells <- function(data_set, cells) {
  if (!is.numeric(cells)) {
    return(cells)
  }
  accessor <- tryCatch(data_set$getExpressionRow, error = function(e) NULL)
  if (!is.function(accessor)) {
    accessor <- tryCatch(data_set$getExpressionMatrix, error = function(e) NULL)
  }
  method_env <- if (is.function(accessor)) environment(accessor) else NULL
  if (
    is.null(method_env) ||
      !exists("private", envir = method_env, inherits = FALSE)
  ) {
    return(cells)
  }
  private <- get("private", envir = method_env, inherits = FALSE)
  if (
    is.function(tryCatch(
      private$resolveExpressionCells,
      error = function(e) NULL
    ))
  ) {
    n_cells <- tryCatch(ncol(data_set$expression), error = function(e) {
      NA_integer_
    })
    if (
      length(n_cells) == 1L &&
        !is.na(n_cells) &&
        length(cells) == n_cells &&
        identical(as.integer(cells), seq_len(n_cells))
    ) {
      return(NULL)
    }
    return(cells)
  }
  cell_names <- colnames(data_set$expression)
  if (is.null(cell_names)) cells else cell_names[as.integer(cells)]
}

## The Viewer already owns canonical cell indices and does not need expression
## column names. Read small BPCells slices in storage order, then restore the
## requested order on dense vectors. Reordering the lazy matrix first rebuilds
## million-cell dimension names that the Viewer immediately discards.
viewerBpcellsValues <- function(data_set, cells, genes) {
  expression <- tryCatch(data_set$expression, error = function(e) NULL)
  if (
    !inherits(expression, "IterableMatrix") ||
      !is.numeric(cells) ||
      !length(cells) ||
      anyNA(cells) ||
      any(!is.finite(cells)) ||
      any(cells < 1L) ||
      any(cells > ncol(expression)) ||
      any(cells != as.integer(cells)) ||
      !is.character(genes) ||
      !length(genes) ||
      length(genes) > 9L ||
      anyNA(genes)
  ) {
    return(NULL)
  }
  gene_indices <- match(genes, rownames(expression))
  if (anyNA(gene_indices)) {
    return(NULL)
  }
  indices <- as.integer(cells)
  restore <- NULL
  if (is.unsorted(indices)) {
    read_order <- order(indices)
    indices <- indices[read_order]
    restore <- order(read_order)
  }
  if (is.unsorted(indices, strictly = TRUE)) {
    return(NULL)
  }
  slice <- expression[gene_indices, indices, drop = FALSE]
  dimnames(slice) <- list(NULL, NULL)
  values <- as.matrix(slice)
  stats::setNames(
    lapply(seq_along(genes), function(index) {
      value <- as.numeric(values[index, , drop = TRUE])
      if (is.null(restore)) value else value[restore]
    }),
    genes
  )
}

viewerExpressionRow <- function(data_set, cells, gene) {
  cells <- viewerExpressionCells(data_set, cells)
  all_cells <- is.null(cells)
  cell_indices <- is.numeric(cells)
  if (is.character(gene) && length(gene) == 1L && !is.na(gene)) {
    values <- viewerBpcellsValues(data_set, cells, gene)
    if (!is.null(values)) {
      return(values[[1L]])
    }
  }
  get_row <- tryCatch(data_set$getExpressionRow, error = function(e) NULL)
  if (is.function(get_row)) {
    return(as.numeric(get_row(gene = gene, cells = cells)))
  }
  expression_matrix <- data_set$getExpressionMatrix(
    cells = cells,
    genes = gene
  )
  if (is.null(expression_matrix)) {
    return(NULL)
  }
  if (is.null(dim(expression_matrix))) {
    return(as.numeric(expression_matrix))
  }
  row_index <- if (is.null(rownames(expression_matrix))) {
    1L
  } else {
    match(gene, rownames(expression_matrix))
  }
  if (is.na(row_index)) {
    return(NULL)
  }
  cell_names <- colnames(expression_matrix)
  cell_index <- if (
    all_cells ||
      cell_indices ||
      is.null(cell_names) ||
      identical(cells, cell_names)
  ) {
    seq_len(ncol(expression_matrix))
  } else {
    match(cells, cell_names)
  }
  as.numeric(expression_matrix[row_index, cell_index, drop = TRUE])
}

## Fetch several genes in one backend call and align every returned vector to
## the requested cell order. Missing genes are omitted from the result.
viewerExpressionValues <- function(data_set, cells, genes) {
  cells <- viewerExpressionCells(data_set, cells)
  all_cells <- is.null(cells)
  cell_indices <- is.numeric(cells)
  genes <- unique(as.character(unlist(genes, use.names = FALSE)))
  genes <- genes[!is.na(genes) & nzchar(genes)]
  if (!length(genes)) {
    return(list())
  }

  if (length(genes) == 1L) {
    value <- viewerExpressionRow(data_set, cells, genes[[1L]])
    if (is.null(value)) {
      return(list())
    }
    return(stats::setNames(list(value), genes))
  }

  values <- viewerBpcellsValues(data_set, cells, genes)
  if (!is.null(values)) {
    return(values)
  }

  expression_matrix <- data_set$getExpressionMatrix(
    cells = cells,
    genes = genes
  )
  if (is.null(expression_matrix)) {
    return(list())
  }
  if (is.null(dim(expression_matrix))) {
    return(list())
  }

  gene_names <- rownames(expression_matrix)
  if (is.null(gene_names) && nrow(expression_matrix) == length(genes)) {
    gene_names <- genes
  }
  cell_names <- colnames(expression_matrix)
  cell_index <- if (all_cells || cell_indices || is.null(cell_names)) {
    seq_len(ncol(expression_matrix))
  } else if (identical(cells, cell_names)) {
    seq_along(cells)
  } else {
    match(cells, cell_names)
  }

  values <- lapply(genes, function(gene) {
    row_index <- match(gene, gene_names)
    if (is.na(row_index)) {
      return(NULL)
    }
    as.numeric(expression_matrix[row_index, cell_index, drop = TRUE])
  })
  names(values) <- genes
  values[!vapply(values, is.null, logical(1))]
}

cerebroCellViewMessage <- function(
  id,
  meta,
  data,
  hover = list(),
  extra = list(),
  dataset_context = NULL
) {
  wire_array <- function(value) {
    if (is.null(value)) NULL else I(unname(value))
  }
  wire_nested <- function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    if (!is.list(value)) {
      return(wire_array(value))
    }
    lapply(value, wire_array)
  }

  for (field in intersect(
    c("traces", "group_colors", "axes", "coexpr_colors"),
    names(meta)
  )) {
    meta[[field]] <- wire_array(meta[[field]])
  }

  nested <- identical(meta$color_type, "categorical") && is.null(data$panels)
  for (field in intersect(
    c("x", "y", "z", "selection_key", "color", "group", "point_sizes"),
    names(data)
  )) {
    value <- data[[field]]
    data[[field]] <- if (nested || is.list(value)) {
      wire_nested(value)
    } else {
      wire_array(value)
    }
  }
  for (field in intersect(
    c("x_range", "y_range", "color_range"),
    names(data)
  )) {
    data[[field]] <- wire_array(data[[field]])
  }
  if (is.list(data$rgb)) {
    data$rgb <- lapply(data$rgb, wire_array)
  }
  if (is.list(data$panels)) {
    data$panels <- lapply(data$panels, function(panel) {
      for (field in intersect(
        c(
          "selection_key",
          "x",
          "y",
          "z",
          "hover",
          "from_x",
          "from_y",
          "to_x",
          "to_y"
        ),
        names(panel)
      )) {
        panel[[field]] <- wire_array(panel[[field]])
      }
      panel
    })
  }
  if (!is.null(hover$text)) {
    hover$text <- if (nested || is.list(hover$text)) {
      wire_nested(hover$text)
    } else {
      wire_array(hover$text)
    }
  }
  if (is.list(hover$columns)) {
    hover$columns <- lapply(hover$columns, function(column) {
      column$values <- wire_nested(column$values)
      if (!is.null(column$levels)) {
        column$levels <- wire_array(column$levels)
      }
      column
    })
  }
  if (is.list(extra$group_hulls)) {
    for (field in intersect(c("x", "y"), names(extra$group_hulls))) {
      extra$group_hulls[[field]] <- wire_nested(extra$group_hulls[[field]])
    }
  }
  if (is.list(extra$edges)) {
    for (field in intersect(c("x0", "y0", "x1", "y1"), names(extra$edges))) {
      extra$edges[[field]] <- wire_array(extra$edges[[field]])
    }
  }

  list(id = id, meta = meta, data = data, hover = hover, extra = extra)
}

## Binary transport is shared by every cell-scatter page. Keep the codec in the
## always-loaded utility scope rather than behind the deferred Linked Views
## server, otherwise opening a specialist page first falls back to full JSON.
cv_wire_integer_type <- function(values) {
  if (anyNA(values)) {
    return("i32")
  }
  bounds <- range(values)
  if (bounds[[1L]] >= -128L && bounds[[2L]] <= 127L) {
    "i8"
  } else if (bounds[[1L]] >= -32768L && bounds[[2L]] <= 32767L) {
    "i16"
  } else {
    "i32"
  }
}

cv_wire_pack_bundle <- function(
  bundle,
  min_length = 4096L,
  include_cells = TRUE
) {
  if (!isTRUE(include_cells)) {
    bundle$cells <- NULL
  }
  cv_wire_pack_message(bundle, min_length = min_length)
}

cv_wire_json_strings <- function(values) {
  values <- enc2utf8(as.character(values))
  if (!length(values)) {
    return(charToRaw("[]"))
  }
  if (
    !anyNA(values) &&
      all(validUTF8(values)) &&
      !any(grepl('["\\\\[:cntrl:]]', values, perl = TRUE))
  ) {
    return(charToRaw(paste0('["', paste(values, collapse = '","'), '"]')))
  }
  charToRaw(enc2utf8(as.character(jsonlite::toJSON(
    values,
    auto_unbox = FALSE,
    na = "null"
  ))))
}

cv_wire_pack_message <- function(message, min_length = 4096L) {
  chunks <- list()
  data_size <- 0L
  pack <- function(values, type) {
    bytes <- if (identical(type, "json")) {
      cv_wire_json_strings(values)
    } else {
      size <- switch(type, i8 = 1L, i16 = 2L, i32 = 4L, f32 = 4L, f64 = 8L)
      writeBin(
        if (startsWith(type, "i")) as.integer(values) else as.numeric(values),
        raw(),
        size = size,
        endian = "little"
      )
    }
    alignment <- switch(
      type,
      i8 = 1L,
      i16 = 2L,
      i32 = 4L,
      f32 = 4L,
      f64 = 8L,
      1L
    )
    offset <- data_size + ((alignment - data_size %% alignment) %% alignment)
    chunks[[length(chunks) + 1L]] <<- list(offset = offset, bytes = bytes)
    data_size <<- offset + length(bytes)
    list(
      `__cv_wire__` = type,
      length = length(values),
      offset = offset,
      bytes = length(bytes)
    )
  }
  walk <- function(value, field = NULL) {
    if (is.list(value)) {
      value_names <- names(value)
      packed <- lapply(seq_along(value), function(index) {
        child_field <- if (
          !is.null(value_names) && nzchar(value_names[[index]])
        ) {
          value_names[[index]]
        } else {
          field
        }
        walk(value[[index]], child_field)
      })
      names(packed) <- value_names
      return(packed)
    }
    if (
      !is.atomic(value) || length(value) <= 1L || length(value) < min_length
    ) {
      return(value)
    }
    if (is.factor(value) || is.character(value)) {
      return(pack(as.character(value), "json"))
    }
    if (is.integer(value)) {
      return(pack(value, cv_wire_integer_type(value)))
    }
    if (is.numeric(value)) {
      return(pack(
        value,
        if (
          field %in%
            c(
              "x",
              "y",
              "z",
              "from_x",
              "from_y",
              "to_x",
              "to_y",
              "point_sizes"
            )
        ) {
          "f32"
        } else {
          "f64"
        }
      ))
    }
    value
  }

  message <- walk(message)
  message$wire_format <- "binary-v1"
  header <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(
    message,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = 17
  ))))
  header_padding <- (4L - length(header) %% 4L) %% 4L
  ## Concatenate raw blocks once. Indexing the final buffer by every byte
  ## creates large integer index vectors and copies for atlas-sized messages.
  parts <- list(
    writeBin(as.integer(length(header)), raw(), size = 4L, endian = "little"),
    header,
    raw(header_padding)
  )
  cursor <- 0L
  for (chunk in chunks) {
    parts[[length(parts) + 1L]] <- raw(chunk$offset - cursor)
    parts[[length(parts) + 1L]] <- chunk$bytes
    cursor <- chunk$offset + length(chunk$bytes)
  }
  unlist(parts, use.names = FALSE)
}

.cerebro_cell_view_wire_serial <- 0L
.cerebro_cell_view_render_serial <- 0
.cerebro_cell_view_aux_pending <- new.env(parent = emptyenv())

cerebroCellViewDeferredAux <- function(
  selection_rows,
  cell_barcodes,
  hover_columns = list(),
  hover = FALSE
) {
  grouped <- is.list(selection_rows)
  reorder <- function(values) {
    if (grouped) {
      return(lapply(selection_rows, function(rows) I(unname(values[rows]))))
    }
    I(unname(values[selection_rows]))
  }
  columns <- if (isTRUE(hover)) {
    lapply(hover_columns, function(column) {
      column$values <- reorder(column$values)
      column
    })
  } else {
    list()
  }
  list(
    selection_key = reorder(as.character(cell_barcodes)),
    hover = list(
      hoverinfo = if (isTRUE(hover)) "text" else "skip",
      columns = columns
    )
  )
}

cerebroCellViewResolveDeferredAux <- function(message) {
  if (!is.function(message$build)) {
    return(message)
  }
  build <- message$build
  message$build <- NULL
  resolved <- build()
  if (!is.list(resolved)) {
    stop("deferred cell-view auxiliary data must be a list")
  }
  for (name in names(resolved)) {
    message[[name]] <- resolved[[name]]
  }
  message
}

.runtimeDiagnostic <- function(x) {
  if (!isTRUE(getOption("cerebro.quiet_runtime", FALSE))) print(x)
}

cerebroCellViewRecolor <- function(
  id,
  meta,
  data,
  dataset_context,
  feedback = NULL
) {
  dataset_context <- viewerDatasetContext(
    dataset_context$epoch,
    dataset_context$dataset_key,
    dataset_context$generation
  )
  meta$dataset_id <- dataset_context$dataset_key
  meta$dataset_generation <- dataset_context$generation
  message <- cerebroCellViewMessage(id, meta, data)
  .cerebro_cell_view_render_serial <<- .cerebro_cell_view_render_serial + 1
  message$dataset_context <- dataset_context
  message$render_token <- .cerebro_cell_view_render_serial
  cerebroSendViewFeedback(id, message$render_token, dataset_context, feedback)
  fields <- c(
    "color",
    "rgb",
    "rgb_scaled",
    "rgb_genes",
    "colorscale",
    "panel_colorscales",
    "color_range",
    "reversescale",
    "paint_order",
    "render_key",
    "sparse_color",
    "color_cache_key",
    "packed_rgb",
    "packed_rgb_cache_keys",
    "zero_color"
  )
  message$data <- message$data[intersect(fields, names(message$data))]
  if (isTRUE(input[["coordviews_wire_supported"]])) {
    session$sendBinaryMessage(
      "cell_view_recolor_binary",
      cv_wire_pack_message(message)
    )
  } else {
    session$sendCustomMessage("cell_view_recolor", message)
  }
}

cerebroCellViewAuxRequest <- function(request) {
  if (!is.list(request) || is.null(request$id) || is.null(request$wire_token)) {
    return(invisible(FALSE))
  }
  if (
    !viewerDatasetContextEqual(
      request$dataset_context,
      viewer_loaded_dataset_context()
    )
  ) {
    return(invisible(FALSE))
  }
  key <- paste(request$id, request$wire_token, sep = ":")
  message <- .cerebro_cell_view_aux_pending[[key]]
  if (
    is.null(message) ||
      !viewerDatasetContextEqual(
        message$dataset_context,
        request$dataset_context
      )
  ) {
    return(invisible(FALSE))
  }
  if (is.function(message$core_builder)) {
    core <- message$core_builder()
    message$core_builder <- NULL
    message$core <- c(
      list(
        id = message$id,
        wire_token = message$wire_token,
        dataset_context = message$dataset_context,
        render_token = message$render_token
      ),
      core
    )
  }
  if (!is.null(message$core) && !isTRUE(message$core_sent)) {
    core <- message$core
    message$core <- NULL
    message$core_sent <- TRUE
    .cerebro_cell_view_aux_pending[[key]] <- message
    if (!isTRUE(core$hover$pending)) {
      rm(list = key, envir = .cerebro_cell_view_aux_pending)
    }
    session$sendBinaryMessage(
      "cell_view_aux_binary",
      cv_wire_pack_message(core)
    )
    return(invisible(TRUE))
  }
  if (isTRUE(message$core_sent) && !identical(request$stage, "metadata")) {
    return(invisible(FALSE))
  }
  rm(list = key, envir = .cerebro_cell_view_aux_pending)
  core_sent <- isTRUE(message$core_sent)
  message$core_sent <- NULL
  message <- cerebroCellViewResolveDeferredAux(message)
  if (core_sent) {
    message$selection_key <- NULL
  }
  session$sendBinaryMessage(
    "cell_view_aux_binary",
    cv_wire_pack_message(message)
  )
  invisible(TRUE)
}

## Small UI snapshots only. Never enumerate values of all Shiny inputs: those
## include million-cell selections and transport caches. Reads are isolated so
## feedback cannot add a dependency to an existing preparation path.
cerebroFeedbackControls <- function(id, input, overrides = list()) {
  pattern <- switch(
    id,
    overview_projection = "^overview_projection_(to_display|point_color|point_size|point_opacity|point_border|group_labels|keep_square|percentage_cells_to_show|group_filter_.+)$",
    expression_projection = "^expression_projection_(to_display|point_size|point_opacity|point_border|group_labels|keep_square|percentage_cells_to_show|group_filter_.+)$",
    spatial_projection = "^spatial_projection_(to_display|plot_type|point_color|point_size|point_opacity|group_labels|point_border|keep_square|percentage_cells_to_show|feature_to_display|coexpr_[rgb]|show_region_outlines|background_image|group_filter_.+)$",
    trajectory_projection = "^trajectory_(selected_method|selected_name|point_color|point_size|point_opacity|projection_group_labels|projection_point_border|projection_keep_square|percentage_cells_to_show|projection_group_filter_.+)$",
    hla_motif_network = "^hla_(chain|scope|color_by|color_allele|pair_allele_i|pair_allele_ii|min_nodes|by_v|show_isolated|node_scale|legend_mode|group_filter_.+)$",
    "a^"
  )
  shiny::isolate({
    ids <- grep(pattern, names(input), value = TRUE)
    values <- stats::setNames(lapply(ids, function(key) input[[key]]), ids)
    values <- utils::modifyList(values, overrides, keep.null = TRUE)
    lapply(values, function(value) {
      if (is.logical(value)) {
        tolower(as.character(value))
      } else {
        as.character(value)
      }
    })
  })
}

cerebroSendViewFeedback <- function(id, token, context, controls) {
  if (!is.null(controls)) {
    session$sendCustomMessage(
      "cerebro_view_feedback",
      list(
        id = id,
        renderToken = token,
        dataset_context = context,
        controls = controls
      )
    )
  }
}

cerebroCellViewRender <- function(
  id,
  meta,
  data,
  hover = list(),
  extra = list(),
  deferred_aux = NULL,
  core_aux = NULL,
  dataset_context = NULL,
  feedback = NULL
) {
  dataset_context <- viewerDatasetContext(
    dataset_context$epoch,
    dataset_context$dataset_key,
    dataset_context$generation
  )
  meta$dataset_id <- dataset_context$dataset_key
  meta$dataset_generation <- dataset_context$generation
  message <- cerebroCellViewMessage(id, meta, data, hover, extra)
  .cerebro_cell_view_render_serial <<- .cerebro_cell_view_render_serial + 1
  message$dataset_context <- dataset_context
  message$render_token <- .cerebro_cell_view_render_serial
  cerebroSendViewFeedback(id, message$render_token, dataset_context, feedback)
  deferred_selection_lengths <- suppressWarnings(as.integer(
    message$data$deferred_selection_lengths
  ))
  message$data$deferred_selection_lengths <- NULL
  stamp_transport_profile <- function(message) {
    request_at_ms <- tryCatch(
      suppressWarnings(as.numeric(
        input[[paste0(id, "_render_request")]]$nonce
      )),
      error = function(error) NA_real_
    )
    if (length(request_at_ms) != 1L || !is.finite(request_at_ms)) {
      request_at_ms <- NA_real_
    }
    sent_at_ms <- as.numeric(Sys.time()) * 1000
    message$transport_profile <- list(
      request_at_ms = request_at_ms,
      server_prepare_ms = if (is.finite(request_at_ms)) {
        max(0, sent_at_ms - request_at_ms)
      } else {
        NA_real_
      },
      sent_at_ms = sent_at_ms
    )
    message
  }
  if (exists("viewerDatasetIdentity", mode = "function", inherits = TRUE)) {
    identity <- tryCatch(viewerDatasetIdentity(), error = function(error) NULL)
    if (
      is.list(identity) &&
        length(identity$fingerprint) == 1L &&
        !is.na(identity$fingerprint) &&
        nzchar(identity$fingerprint)
    ) {
      message$dataset_identity <- list(
        cell_count = as.integer(identity$cell_count),
        cell_fingerprint = as.character(identity$fingerprint),
        cell_order_fingerprint = as.character(
          identity$order_fingerprint %||% ""
        ),
        pack_dataset_fingerprint = as.character(
          identity$pack_fingerprint %||% ""
        )
      )
    }
  }
  if (
    isTRUE(input[["coordviews_wire_supported"]]) &&
      exists("cv_wire_pack_message", mode = "function", inherits = TRUE)
  ) {
    selection_keys <- message$data$selection_key
    key_groups <- if (is.list(selection_keys)) {
      selection_keys
    } else {
      list(selection_keys)
    }
    n_cells <- if (
      length(deferred_selection_lengths) &&
        all(!is.na(deferred_selection_lengths)) &&
        all(deferred_selection_lengths >= 0L)
    ) {
      sum(deferred_selection_lengths)
    } else {
      sum(vapply(key_groups, length, integer(1)))
    }
    shared_projection <- viewerSharedProjectionName(
      meta$space_label,
      n_cells,
      dataset_context = dataset_context
    )
    shared_zero_color <- isTRUE(message$data$shared_zero_color)
    message$data$shared_zero_color <- NULL
    if (isTRUE(shared_zero_color)) {
      message$data$color <- NULL
      message$data$zero_color <- TRUE
      message$data$n <- n_cells
    }
    if (
      is.character(shared_projection) &&
        length(shared_projection) == 1L &&
        !is.na(shared_projection) &&
        nzchar(shared_projection)
    ) {
      message$shared_projection <- shared_projection
      message$data$x <- NULL
      message$data$y <- NULL
      message$data$z <- NULL
      message$data$n <- n_cells
    }
    progressive <- (!is.null(selection_keys) ||
      (length(deferred_selection_lengths) &&
        is.function(deferred_aux))) &&
      is.null(message$data$panels) &&
      n_cells >= 4096L
    if (progressive) {
      .cerebro_cell_view_wire_serial <<-
        .cerebro_cell_view_wire_serial + 1L
      token <- .cerebro_cell_view_wire_serial
      message$data$n <- n_cells
      message$data$wire_token <- token
      message$data$selection_key <- NULL
      full_hover <- message$hover
      message$hover <- list(hoverinfo = "skip")
      message <- stamp_transport_profile(message)
      session$sendBinaryMessage(
        "cell_view_binary",
        cv_wire_pack_message(message)
      )
      stale <- ls(envir = .cerebro_cell_view_aux_pending, all.names = TRUE)
      stale <- stale[vapply(
        stale,
        function(key) {
          pending <- .cerebro_cell_view_aux_pending[[key]]
          is.list(pending) && identical(pending$id, id)
        },
        logical(1)
      )]
      if (length(stale)) {
        rm(list = stale, envir = .cerebro_cell_view_aux_pending)
      }
      .cerebro_cell_view_aux_pending[[paste(id, token, sep = ":")]] <- if (
        is.function(deferred_aux)
      ) {
        list(
          id = id,
          wire_token = token,
          dataset_context = dataset_context,
          render_token = message$render_token,
          build = deferred_aux,
          core_builder = core_aux
        )
      } else {
        list(
          id = id,
          wire_token = token,
          dataset_context = dataset_context,
          render_token = message$render_token,
          selection_key = selection_keys,
          hover = full_hover
        )
      }
    } else {
      if (is.function(deferred_aux)) {
        auxiliary <- deferred_aux()
        message$data$selection_key <- auxiliary$selection_key
        message$hover <- auxiliary$hover
      }
      message <- stamp_transport_profile(message)
      session$sendBinaryMessage(
        "cell_view_binary",
        cv_wire_pack_message(message)
      )
    }
  } else {
    if (is.function(deferred_aux)) {
      auxiliary <- deferred_aux()
      message$data$selection_key <- auxiliary$selection_key
      message$hover <- auxiliary$hover
    }
    message <- stamp_transport_profile(message)
    session$sendCustomMessage("cell_view_render", message)
  }
  invisible(message$render_token)
}

cerebroCellViewBackground <- function(
  id,
  values,
  dataset_context,
  background_identity = NULL
) {
  dataset_context <- viewerDatasetContext(
    dataset_context[["epoch"]],
    dataset_context[["dataset_key"]],
    dataset_context[["generation"]]
  )
  session$sendCustomMessage(
    "cell_view_background",
    list(
      id = id,
      values = values,
      dataset_context = dataset_context,
      background_identity = background_identity
    )
  )
}

cerebroCellViewScatterPayload <- function(
  coordinates,
  color,
  color_variable,
  selection_keys,
  point_size,
  point_opacity,
  group_labels = TRUE,
  keep_square = FALSE,
  color_assignments = NULL,
  hover_info = NULL,
  hover_columns = NULL,
  hover = TRUE,
  point_line = list(),
  x_range = list(),
  y_range = list(),
  reset_axes = FALSE,
  n_dimensions = 2L,
  space_label = NULL,
  cell_count = NULL,
  coordinate_resource = NULL,
  categorical_resource = NULL
) {
  dimensions <- if (as.integer(n_dimensions) == 3L) 3L else 2L
  resource_coordinates <- is.list(coordinate_resource) &&
    is.character(coordinate_resource$url) &&
    length(coordinate_resource$url) == 1L &&
    nzchar(coordinate_resource$url)
  resource_categories <- is.list(categorical_resource) &&
    is.character(categorical_resource$url) &&
    length(categorical_resource$url) == 1L &&
    nzchar(categorical_resource$url) &&
    length(categorical_resource$levels)
  if (!resource_coordinates && length(coordinates) < dimensions) {
    stop("coordinates do not contain the requested dimensions")
  }
  cell_counts <- c()
  if (!is.null(cell_count)) {
    cell_counts <- c(cell_counts, cell_count = as.integer(cell_count))
  }
  if (resource_coordinates) {
    cell_counts <- c(
      cell_counts,
      coordinates = as.integer(coordinate_resource$cells)
    )
  } else {
    cell_counts <- c(
      cell_counts,
      vapply(coordinates[seq_len(dimensions)], length, integer(1))
    )
  }
  if (resource_categories) {
    cell_counts <- c(
      cell_counts,
      color = as.integer(categorical_resource$cells)
    )
  } else {
    cell_counts <- c(cell_counts, color = length(color))
  }
  if (length(selection_keys)) {
    cell_counts <- c(cell_counts, selection_keys = length(selection_keys))
  }
  if (!length(cell_counts) || anyNA(cell_counts) || any(cell_counts < 0L)) {
    stop("cell_count or valid cell resources are required")
  }
  if (length(unique(cell_counts)) != 1L) {
    stop(
      "coordinates, color, and selection_keys must describe the same number of cells"
    )
  }

  n_cells <- unname(cell_counts[[1L]])
  continuous <- if (resource_categories) FALSE else is.numeric(color)
  has_z <- !resource_coordinates &&
    as.integer(n_dimensions) == 3L &&
    length(coordinates) >= 3L
  meta <- list(
    color_type = if (continuous) "continuous" else "categorical",
    color_variable = color_variable,
    appearance = list(
      group_labels = isTRUE(group_labels),
      draw_border = isTRUE(
        suppressWarnings(as.numeric(point_line[["width"]])) > 0
      ),
      keep_square = isTRUE(keep_square)
    )
  )
  if (!is.null(space_label)) {
    meta[["space_label"]] <- space_label
  }
  data <- list(
    x = if (continuous && !resource_coordinates) {
      I(coordinates[[1L]])
    } else {
      list()
    },
    y = if (continuous && !resource_coordinates) {
      I(coordinates[[2L]])
    } else {
      list()
    },
    selection_key = if (continuous) I(selection_keys) else list(),
    color = if (continuous) I(color) else list(),
    point_size = point_size,
    point_opacity = point_opacity,
    point_line = point_line,
    x_range = x_range,
    y_range = y_range,
    reset_axes = reset_axes
  )
  if (has_z) {
    data[["z"]] <- if (continuous) I(coordinates[[3L]]) else list()
  }

  show_hover <- isTRUE(hover)
  structured_hover <- if (show_hover && length(hover_columns)) {
    lapply(hover_columns, function(column) {
      if (!is.list(column) || is.null(column$label) || is.null(column$values)) {
        stop("hover columns require label and values")
      }
      if (length(column$values) != n_cells) {
        stop("hover columns must describe the same number of cells")
      }
      column
    })
  } else {
    list()
  }
  hover_data <- list(
    hoverinfo = if (show_hover) "text" else "skip",
    text = if (continuous && show_hover && !length(structured_hover)) {
      I(unname(hover_info))
    } else {
      list()
    }
  )
  if (length(structured_hover)) {
    hover_data$columns <- lapply(structured_hover, function(column) {
      column$values <- if (continuous) I(unname(column$values)) else list()
      if (!is.null(column$levels)) {
        column$levels <- I(unname(column$levels))
      }
      column
    })
  }
  if (continuous) {
    if (resource_coordinates) {
      data$x <- NULL
      data$y <- NULL
      data$projection_resource <- coordinate_resource
    }
    return(list(meta = meta, data = data, hover = hover_data))
  }
  if (is.null(color_assignments)) {
    stop("color_assignments are required for categorical cell views")
  }
  if (resource_categories) {
    traces <- as.character(categorical_resource$levels)
    missing_levels <- setdiff(traces, names(color_assignments))
    if (length(missing_levels)) {
      stop(
        "color_assignments are missing categorical levels: ",
        paste(missing_levels, collapse = ", ")
      )
    }
    meta[["traces"]] <- as.list(traces)
    data[["x"]] <- NULL
    data[["y"]] <- NULL
    data[["selection_key"]] <- NULL
    data[["color"]] <- as.list(unname(color_assignments[traces]))
    data[["projection_resource"]] <- coordinate_resource
    data[["categorical_resource"]] <- categorical_resource
    data[["deferred_selection_lengths"]] <- as.integer(n_cells)
    return(list(meta = meta, data = data, hover = hover_data))
  }
  factor_color <- is.factor(color)
  factor_codes <- if (factor_color) as.integer(color) else NULL
  factor_levels <- if (factor_color) levels(color) else NULL
  has_missing_color <- if (factor_color) anyNA(factor_codes) else anyNA(color)
  levels_in_view <- if (factor_color) {
    present <- tabulate(
      factor_codes[!is.na(factor_codes)],
      nbins = length(factor_levels)
    ) >
      0L
    factor_levels[present]
  } else {
    unique(as.character(color[!is.na(color)]))
  }
  if (has_missing_color) {
    levels_in_view <- c(levels_in_view, "(missing)")
  }
  if (
    !("(missing)" %in% names(color_assignments)) &&
      "(missing)" %in% levels_in_view
  ) {
    color_assignments <- c(color_assignments, `(missing)` = "#7b8794")
  }
  missing_levels <- setdiff(levels_in_view, names(color_assignments))
  if (length(missing_levels)) {
    stop(
      "color_assignments are missing categorical levels: ",
      paste(missing_levels, collapse = ", ")
    )
  }

  canonical_layout <- length(color) >= 4096L &&
    identical(as.integer(selection_keys), seq_along(selection_keys)) &&
    !show_hover &&
    !length(structured_hover)
  if (canonical_layout) {
    traces <- names(color_assignments)[
      names(color_assignments) %in% levels_in_view
    ]
    meta[["traces"]] <- as.list(traces)
    if (resource_coordinates) {
      data[["x"]] <- NULL
      data[["y"]] <- NULL
      data[["projection_resource"]] <- coordinate_resource
    } else {
      data[["x"]] <- I(as.numeric(coordinates[[1L]]))
      data[["y"]] <- I(as.numeric(coordinates[[2L]]))
      if (has_z) {
        data[["z"]] <- I(as.numeric(coordinates[[3L]]))
      }
    }
    data[["selection_key"]] <- I(selection_keys)
    data[["color"]] <- as.list(unname(color_assignments[traces]))
    canonical_group <- if (factor_color) {
      group <- match(factor_levels, traces) - 1L
      group <- as.integer(group[factor_codes])
      if (has_missing_color) {
        group[is.na(factor_codes)] <- match("(missing)", traces) - 1L
      }
      group
    } else {
      normalized_color <- as.character(color)
      normalized_color[is.na(normalized_color)] <- "(missing)"
      as.integer(match(normalized_color, traces) - 1L)
    }
    data[["canonical_group"]] <- I(canonical_group)
    return(list(meta = meta, data = data, hover = hover_data))
  }

  color <- as.character(color)
  color[is.na(color)] <- "(missing)"
  meta[["traces"]] <- list()
  cells_by_group <- split(seq_along(color), color)
  hover_names <- names(hover_info)
  aligned_hover <- if (!show_hover) {
    NULL
  } else if (
    !is.null(hover_names) &&
      any(!is.na(hover_names) & nzchar(hover_names))
  ) {
    unname(hover_info[match(selection_keys, hover_names)])
  } else {
    unname(hover_info)
  }
  index <- 1L
  for (group in names(color_assignments)) {
    cells <- cells_by_group[[group]]
    if (is.null(cells)) {
      next
    }
    meta[["traces"]][[index]] <- group
    data[["x"]][[index]] <- I(coordinates[[1L]][cells])
    data[["y"]][[index]] <- I(coordinates[[2L]][cells])
    if (has_z) {
      data[["z"]][[index]] <- I(coordinates[[3L]][cells])
    }
    data[["selection_key"]][[index]] <- I(selection_keys[cells])
    data[["color"]][[index]] <- unname(color_assignments[[group]])
    if (show_hover && !length(structured_hover)) {
      hover_data[["text"]][[index]] <- I(aligned_hover[cells])
    }
    for (column_index in seq_along(structured_hover)) {
      hover_data$columns[[column_index]]$values[[index]] <- I(
        unname(structured_hover[[column_index]]$values[cells])
      )
    }
    index <- index + 1L
  }

  if (
    length(color) >= 4096L &&
      identical(as.integer(selection_keys), seq_along(selection_keys))
  ) {
    data[["canonical_group"]] <- I(as.integer(
      match(color, unlist(meta[["traces"]], use.names = FALSE)) - 1L
    ))
  }

  list(meta = meta, data = data, hover = hover_data)
}

cerebroSelectionCount <- function(selection) {
  ids <- if (is.list(selection) && !is.null(selection$ids)) {
    selection$ids
  } else {
    selection
  }
  length(ids)
}

cerebroSelectionSummary <- function(
  selection,
  source,
  color_variable = NULL,
  metadata = getMetaData(),
  groups = getGroups(),
  main_group = NULL,
  composition = FALSE,
  max_groups = 5L
) {
  keys <- if (
    is.data.frame(selection) && "selection_key" %in% names(selection)
  ) {
    selection[["selection_key"]]
  } else if (is.data.frame(selection) && "cell_barcode" %in% names(selection)) {
    selection[["cell_barcode"]]
  } else if (is.list(selection) && !is.null(selection[["ids"]])) {
    selection[["ids"]]
  } else if (is.character(selection)) {
    selection
  } else {
    character(0)
  }
  keys <- unique(as.character(keys[!is.na(keys) & nzchar(keys)]))
  # Composition has nothing to render until stable selection keys arrive.
  # Return before forcing the lazy metadata/groups defaults: on million-cell
  # datasets those accessors can hydrate the complete per-cell metadata even
  # though the result below would immediately be discarded.
  if (isTRUE(composition) && !length(keys)) {
    return(NULL)
  }
  n_selected <- if (length(keys)) {
    length(keys)
  } else if (is.data.frame(selection)) {
    nrow(selection)
  } else {
    length(selection)
  }
  total <- if (is.data.frame(metadata)) nrow(metadata) else 0L
  unit <- tryCatch(getObservationUnit()$plural, error = function(e) "cells")

  if (is.null(main_group)) {
    parameters <- tryCatch(getParameters(), error = function(e) NULL)
    main_group <- if (is.list(parameters)) parameters[["main_group"]] else NULL
  }
  normalized_groups <- tolower(gsub("[^[:alnum:]]", "", groups))
  cell_type_index <- which(startsWith(normalized_groups, "celltype"))
  cell_type_group <- if (length(cell_type_index)) {
    groups[[cell_type_index[[1L]]]]
  } else {
    NULL
  }

  if (isTRUE(composition)) {
    comp_candidates <- unique(c(
      cell_type_group,
      if (!is.null(color_variable) && color_variable %in% groups) {
        color_variable
      },
      groups
    ))
    comp_candidates <- comp_candidates[
      !is.na(comp_candidates) &
        nzchar(comp_candidates) &
        comp_candidates %in% colnames(metadata)
    ]
    if (
      !length(keys) ||
        !length(comp_candidates) ||
        !"cell_barcode" %in% colnames(metadata)
    ) {
      return(NULL)
    }
    comp_group <- comp_candidates[[1L]]
    values <- as.character(metadata[[comp_group]][
      match(keys, as.character(metadata[["cell_barcode"]]))
    ])
    values <- values[!is.na(values) & nzchar(values)]
    if (!length(values)) {
      return(NULL)
    }

    counts <- sort(table(values), decreasing = TRUE)
    max_groups <- max(1L, as.integer(max_groups[[1L]]))
    shown_counts <- utils::head(counts, max_groups)
    if (length(counts) > max_groups) {
      shown_counts <- c(
        shown_counts,
        Other = sum(utils::tail(counts, -max_groups))
      )
    }
    color_map <- tryCatch(
      reactive_colors()[[comp_group]],
      error = function(e) NULL
    )
    if (is.null(color_map)) {
      levels_here <- names(counts)
      fallback_colors <- tryCatch(
        cerebro_group_colors(length(levels_here)),
        error = function(e) grDevices::hcl.colors(length(levels_here), "Set 2")
      )
      color_map <- stats::setNames(
        fallback_colors,
        levels_here
      )
    }
    total_counted <- sum(counts)
    rows <- lapply(names(shown_counts), function(label) {
      count <- unname(shown_counts[[label]])
      percent <- 100 * count / total_counted
      color <- if (identical(label, "Other")) {
        "#aeb5bb"
      } else {
        unname(color_map[label])
      }
      if (is.null(color) || is.na(color) || !nzchar(color)) {
        color <- "#aeb5bb"
      }
      shiny::tags$div(
        class = "cerebro-selection-composition-row",
        shiny::tags$span(
          class = "cerebro-selection-composition-label",
          shiny::tags$i(style = paste0("background:", color)),
          shiny::tags$span(label)
        ),
        shiny::tags$span(
          class = "cerebro-selection-composition-track",
          shiny::tags$i(
            style = paste0(
              "width:",
              formatC(percent, format = "f", digits = 1),
              "%;background:",
              color
            )
          )
        ),
        shiny::tags$span(
          class = "cerebro-selection-composition-value",
          paste0(
            formatC(count, format = "f", big.mark = ",", digits = 0),
            " · ",
            round(percent),
            "%"
          )
        )
      )
    })
    return(shiny::tags$div(
      class = "cerebro-selection-composition-card",
      role = "status",
      `aria-live` = "polite",
      shiny::tags$div(
        class = "cerebro-selection-composition-head",
        shiny::tags$button(
          type = "button",
          class = "cerebro-selection-composition-drag",
          `aria-label` = "Move composition card; use arrow keys",
          title = "Drag to move",
          shiny::tags$span(
            `aria-hidden` = "true",
            shiny::HTML("&#10247;")
          )
        ),
        shiny::tags$strong("Composition"),
        shiny::tags$span(paste0(
          formatC(n_selected, format = "f", big.mark = ",", digits = 0),
          " / ",
          formatC(total, format = "f", big.mark = ",", digits = 0)
        ))
      ),
      shiny::tags$div(
        class = "cerebro-selection-composition-sub",
        paste0("Selected ", unit, " · by ", comp_group)
      ),
      shiny::tags$div(
        class = "cerebro-selection-composition-rows",
        rows
      )
    ))
  }

  candidates <- unique(c(main_group, cell_type_group, color_variable, groups))
  candidates <- candidates[
    !is.na(candidates) & nzchar(candidates) & candidates %in% colnames(metadata)
  ]

  profile <- NULL
  if (
    length(keys) &&
      length(candidates) &&
      "cell_barcode" %in% colnames(metadata)
  ) {
    values <- as.character(metadata[[candidates[[1L]]]][
      match(keys, as.character(metadata[["cell_barcode"]]))
    ])
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values)) {
      counts <- table(values)
      dominant <- names(counts)[[which.max(counts)]]
      profile <- shiny::tags$span(
        class = "cerebro-selection-status-profile",
        paste0(
          dominant,
          " \u00b7 ",
          round(100 * max(counts) / length(values)),
          "%"
        )
      )
    }
  }

  shiny::tagList(
    shiny::tags$span(
      "Selected ",
      shiny::tags$b(formatC(
        n_selected,
        format = "f",
        big.mark = ",",
        digits = 0
      )),
      " / ",
      formatC(total, format = "f", big.mark = ",", digits = 0),
      " ",
      unit
    ),
    profile,
    if (
      !is.null(source) &&
        length(source) &&
        !is.na(source[[1L]]) &&
        nzchar(source[[1L]])
    ) {
      shiny::tags$span(
        class = "cerebro-selection-status-origin",
        paste("Selected in", source[[1L]])
      )
    }
  )
}

##----------------------------------------------------------------------------##
## Functions to find columns of specific type (for automatic formatting).
##----------------------------------------------------------------------------##
findColumnsInteger <- function(df, columns_to_test) {
  columns_indices <- c()
  for (i in columns_to_test) {
    if (
      any(is.na(df[[i]])) == FALSE &&
        is.numeric(df[[i]]) &&
        all.equal(df[[i]], as.integer(df[[i]]), check.attributes = FALSE) ==
          TRUE
    ) {
      columns_indices <- c(columns_indices, i)
    }
  }
  return(columns_indices)
}

findColumnsPercentage <- function(df) {
  columns_indices <- c()
  for (i in 1:ncol(df)) {
    if (
      grepl(colnames(df)[i], pattern = "pct|percent|%", ignore.case = TRUE) &&
        any(is.na(df[[i]])) == FALSE &&
        is.numeric(df[[i]]) &&
        min(df[[i]], na.rm = TRUE) >= 0 &&
        max(df[[i]], na.rm = TRUE) <= 100
    ) {
      columns_indices <- c(columns_indices, i)
    }
  }
  return(columns_indices)
}

findColumnsPValues <- function(df) {
  pattern_columns_p_value <- "pval|p_val|p-val|p.val|padj|p_adj|p-adj|p.adj|adjp|adj_p|adj-p|adj.p|FDR|qval|q_val|q-val|q.val"
  columns_indices <- c()
  for (i in 1:ncol(df)) {
    if (
      grepl(
        colnames(df)[i],
        pattern = pattern_columns_p_value,
        ignore.case = TRUE
      ) &&
        any(is.na(df[[i]])) == FALSE &&
        is.numeric(df[[i]]) &&
        min(df[[i]], na.rm = TRUE) >= 0 &&
        max(df[[i]], na.rm = TRUE) <= 1
    ) {
      columns_indices <- c(columns_indices, i)
    }
  }
  return(columns_indices)
}

findColumnsLogFC <- function(df) {
  columns_indices <- c()
  for (i in 1:ncol(df)) {
    if (
      grepl(
        colnames(df)[i],
        pattern = "logFC|log-FC|log_FC|log.FC",
        ignore.case = TRUE
      ) &&
        any(is.na(df[[i]])) == FALSE &&
        is.numeric(df[[i]])
    ) {
      columns_indices <- c(columns_indices, i)
    }
  }
  return(columns_indices)
}

##----------------------------------------------------------------------------##
## Functions to prepare and format table.
##----------------------------------------------------------------------------##
replaceInfiniteValues <- function(table) {
  numeric_columns <- which(vapply(table, is.numeric, logical(1)))
  dirty_columns <- numeric_columns[vapply(
    numeric_columns,
    function(column_index) any(is.infinite(table[[column_index]])),
    logical(1)
  )]
  for (column_index in dirty_columns) {
    column <- table[[column_index]]
    infinite <- is.infinite(column)
    column[infinite] <- sign(column[infinite]) * 999
    table[[column_index]] <- column
  }
  table
}

neutralizeSpreadsheetFormulas <- function(table) {
  neutralize <- function(values) {
    values <- as.character(values)
    unsafe <- !is.na(values) & grepl("^[[:space:]]*[=+@-]", values)
    values[unsafe] <- paste0("'", values[unsafe])
    values
  }
  names(table) <- neutralize(names(table))
  text_columns <- which(vapply(
    table,
    function(column) is.character(column) || is.factor(column),
    logical(1)
  ))
  for (column_index in text_columns) {
    table[[column_index]] <- neutralize(table[[column_index]])
  }
  table
}

prettifyTable <- function(
  table,
  filter,
  dom,
  escape = TRUE,
  show_buttons = FALSE,
  number_formatting = FALSE,
  color_highlighting = FALSE,
  hide_long_columns = FALSE,
  columns_percentage = NULL,
  columns_hide = NULL,
  download_file_name = NULL,
  page_length_default = 15,
  page_length_menu = c(15, 30, 50, 100, 1000)
) {
  ## Coerce toggle-like args to a clean scalar logical. Shiny materialSwitch
  ## can transiently pass NULL / NA through input[[...]] while the UI is being
  ## re-rendered, and downstream `if (flag == TRUE)` chokes with "missing value
  ## where TRUE/FALSE needed".
  as_toggle <- function(x, default) {
    if (is.null(x) || length(x) != 1 || is.na(x)) {
      default
    } else {
      isTRUE(as.logical(x))
    }
  }
  number_formatting <- as_toggle(number_formatting, FALSE)
  color_highlighting <- as_toggle(color_highlighting, FALSE)
  show_buttons <- as_toggle(show_buttons, FALSE)
  hide_long_columns <- as_toggle(hide_long_columns, FALSE)
  escape <- as_toggle(escape, TRUE)

  ## replace Inf and -Inf values in numeric columns with 999 or -999,
  ## respectively, because other the columns will be converted to characters
  ## which messes up sorting of values in that column
  table <- replaceInfiniteValues(table)

  table_original <- table

  ## get column type for alignment in table
  ## factors, characters and logical are centered and numeric columns are
  ## right-aligned
  columns_factor <- as.vector(which(unlist(lapply(table, is.factor))))
  columns_character <- as.vector(which(unlist(lapply(table, is.character))))
  columns_logical <- as.vector(which(unlist(lapply(table, is.logical))))
  columns_numeric <- as.vector(which(unlist(lapply(table, is.numeric))))

  ## identify columns which contain integer despite not being stored as
  ## integer type
  classify_values <- number_formatting || color_highlighting
  columns_integer <- if (classify_values) {
    findColumnsInteger(table, columns_numeric)
  } else {
    integer()
  }

  ## identify which columns might contain percentages, p-values, and logFC
  columns_percent <- if (classify_values) {
    findColumnsPercentage(table)
  } else {
    integer()
  }
  columns_p_value <- if (classify_values) {
    findColumnsPValues(table)
  } else {
    integer()
  }
  columns_logFC <- if (classify_values) findColumnsLogFC(table) else integer()

  ## find columns with very long (character) content so that they can be
  ## hidden
  columns_with_long_content <- c()
  if (
    hide_long_columns == TRUE &&
      length(columns_character) >= 1
  ) {
    for (i in columns_character) {
      if (max(stringr::str_length(table[[i]]), na.rm = TRUE) > 200) {
        columns_with_long_content <- c(columns_with_long_content, i)
      }
    }
    ## reduce column indices by 1 because DT works with 0-based indices
    columns_with_long_content <- columns_with_long_content - 1
  }

  ## add manually specified column types
  if (is.null(columns_percentage) == FALSE) {
    columns_percent <- c(columns_percent, columns_percentage)
  }

  ## check whether percentage values were given on a 0-100 scale and convert
  ## them to 0-1 if so. Selected-cells slices often carry NA in percent_mt /
  ## percent_ribo columns; without na.rm, `max(x > 1)` returns NA and the
  ## enclosing `if (NA)` throws "missing value where TRUE/FALSE needed".
  if (number_formatting == TRUE && length(columns_percent) > 0) {
    for (col in columns_percent) {
      col_name <- colnames(table)[col]
      col_values <- table[[col_name]]
      if (is.numeric(col_values) && any(col_values > 1, na.rm = TRUE)) {
        table[, col] <- table[, col] / 100
      }
    }
  }

  ## add manually specified columns to hide
  if (is.null(columns_hide) == FALSE) {
    columns_hide <- columns_hide - 1
  } else {
    columns_hide <- c()
  }

  ## remove columns with p-values from numeric columns to avoid applying color
  ## tiles
  columns_numeric <- columns_numeric[
    columns_numeric %in% columns_p_value == FALSE
  ]

  ## get vector of column indices that contain numeric values which are
  ## neither integer, p-values, percentages, or logFC
  ## these columns will be rounded to significant digits
  columns_only_numeric <- columns_numeric[
    columns_numeric %in%
      c(
        columns_p_value,
        columns_percent,
        columns_integer,
        columns_p_value,
        columns_logFC
      ) ==
      FALSE
  ]

  ## add buttons if specified
  if (show_buttons == TRUE) {
    table_extensions <- c("Buttons", "ColReorder")
    table_buttons <- list(
      "colvis",
      list(
        extend = "collection",
        text = "Download",
        buttons = list(
          list(
            extend = "csv",
            filename = download_file_name,
            title = NULL
          ),
          list(
            extend = "excel",
            filename = download_file_name,
            title = NULL
          )
        )
      )
    )
  } else {
    table_extensions <- c("ColReorder")
    table_buttons <- list()
  }

  ## - create table
  ## - prevent text wrap for characters/factors/logicals
  ## - align characters in left
  ## - align factors/logicals in center
  ## - align numerics to the right
  table <- DT::datatable(
    table,
    class = "stripe table-bordered table-condensed",
    escape = escape,
    extensions = table_extensions,
    filter = filter,
    rownames = FALSE,
    selection = "single",
    style = "bootstrap",
    options = list(
      buttons = table_buttons,
      columnDefs = list(
        list(targets = "_all", className = 'dt-middle'),
        list(
          targets = c(columns_hide, columns_with_long_content),
          visible = FALSE
        )
      ),
      colReorder = list(
        realtime = FALSE
      ),
      dom = dom,
      lengthMenu = page_length_menu,
      pageLength = page_length_default,
      scrollX = TRUE,
      initComplete = DT::JS(
        "function() {",
        "  var api = this.api();",
        "  var table = $(api.table().container());",
        "  var headers = api.columns().header().toArray();",
        "  setTimeout(function() {",
        "    table.find('thead tr:last-child, tfoot tr:first-child').each(function() {",
        "      $(this).children('td').each(function(index) {",
        "        var label = 'Filter ' + $(headers[index]).text().trim();",
        "        $(this).find('input, select').attr('aria-label', label);",
        "      });",
        "    });",
        "  }, 0);",
        "}"
      )
    )
  ) %>%
    DT::formatStyle(
      columns = c(columns_character),
      textAlign = 'left',
      "white-space" = "nowrap"
    ) %>%
    DT::formatStyle(
      columns = c(columns_factor, columns_logical),
      textAlign = 'center',
      "white-space" = "nowrap"
    ) %>%
    DT::formatStyle(
      columns = c(columns_numeric, columns_p_value),
      textAlign = 'right',
      "white-space" = "nowrap"
    )

  # show cellular barcodes in monospace font
  if ('cell_barcode' %in% colnames(table_original)) {
    table <- table %>%
      DT::formatStyle(
        columns = which(colnames(table_original) == 'cell_barcode'),
        target = "cell",
        fontFamily = "courier"
      )
  }

  ## if automatic number formatting is on...
  ## - remove decimals from integers
  ## - show 3 significant decimals for p-values
  ## - show 3 decimals for logFC
  ## - show percentage values with percent symbol and 2 decimals
  ## - show all other numeric values that are none of the above with 3
  ##   significant decimals
  if (number_formatting == TRUE) {
    ## integer values
    if (
      !is.null(columns_integer) &&
        length(columns_integer) > 0
    ) {
      table <- table %>%
        DT::formatRound(
          columns = columns_integer,
          digits = 0,
          interval = 3,
          mark = ","
        )
    }

    ## p-values
    if (
      !is.null(columns_p_value) &&
        length(columns_p_value) > 0
    ) {
      table <- table %>%
        DT::formatSignif(
          columns = columns_p_value,
          digits = 3
        )
    }

    ## logFC
    if (
      !is.null(columns_logFC) &&
        length(columns_logFC) > 0
    ) {
      table <- table %>%
        DT::formatRound(
          columns = columns_logFC,
          digits = 3
        )
    }

    ## percentage
    if (
      !is.null(columns_percent) &&
        length(columns_percent) > 0
    ) {
      table <- table %>%
        DT::formatPercentage(
          columns = columns_percent,
          digits = 2
        )
    }

    ## numeric but none of the above
    if (
      !is.null(columns_only_numeric) &&
        length(columns_only_numeric) > 0
    ) {
      table <- table %>%
        DT::formatSignif(
          columns = columns_only_numeric,
          digits = 3
        )
    }
  }

  if (color_highlighting == TRUE) {
    ## Highlight colours pulled from the shared chart theme (mirrors the
    ## Viewer palette and cerebro_plotly_theme()), so the table
    ## heat/bars read as the same design system as the plots instead of the old
    ## orange/red/pink mix. Magnitude ramps use the accent; magnitude bars use
    ## the signal blue; logical up/down uses the up/down tokens.
    chart_theme <- tryCatch(cerebro_plotly_theme(), error = function(e) NULL)
    ramp_hi <- if (is.null(chart_theme)) "#f97316" else chart_theme$accent
    bar_col <- if (is.null(chart_theme)) "#2f6fd6" else chart_theme$signal
    up_col <- if (is.null(chart_theme)) "#4c9a6b" else chart_theme$up
    down_col <- if (is.null(chart_theme)) "#c05b5b" else chart_theme$down

    ## integer
    if (
      !is.null(columns_integer) &&
        length(columns_integer) > 0 &&
        nrow(table_original) > 1
    ) {
      for (i in columns_integer) {
        range <- range(table_original[[i]])
        if (range[1] != range[2]) {
          table <- table %>%
            DT::formatStyle(
              columns = i,
              backgroundColor = DT::styleInterval(
                seq(range[1], range[2], (range[2] - range[1]) / 100),
                colorRampPalette(colors = c('white', ramp_hi))(102)
              )
            )
        }
      }
    }

    ## p-values
    if (
      !is.null(columns_p_value) &&
        length(columns_p_value) > 0
    ) {
      table <- table %>%
        DT::formatStyle(
          columns = columns_p_value,
          background = DT::styleColorBar(c(1, 0), bar_col),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    }

    ## logFC
    if (
      !is.null(columns_logFC) &&
        length(columns_logFC) > 0 &&
        nrow(table_original) > 1
    ) {
      for (i in columns_logFC) {
        range <- range(table_original[[i]])
        if (range[1] != range[2]) {
          table <- table %>%
            DT::formatStyle(
              columns = i,
              backgroundColor = DT::styleInterval(
                seq(range[1], range[2], (range[2] - range[1]) / 100),
                colorRampPalette(colors = c('white', ramp_hi))(102)
              )
            )
        }
      }
    }

    ## percentage
    if (
      !is.null(columns_percent) &&
        length(columns_percent) > 0
    ) {
      table <- table %>%
        DT::formatStyle(
          columns = columns_percent,
          background = DT::styleColorBar(c(0, 1), bar_col),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    }

    ## numeric values that are non of the above
    if (
      !is.null(columns_only_numeric) &&
        length(columns_only_numeric) > 0 &&
        nrow(table_original) > 1
    ) {
      for (i in columns_only_numeric) {
        range <- range(table_original[[i]])
        if (range[1] != range[2]) {
          table <- table %>%
            DT::formatStyle(
              columns = i,
              backgroundColor = DT::styleInterval(
                seq(range[1], range[2], (range[2] - range[1]) / 100),
                colorRampPalette(colors = c('white', ramp_hi))(102)
              )
            )
        }
      }
    }

    ## logicals
    if (
      !is.null(columns_logical) &&
        length(columns_logical) > 0
    ) {
      table <- table %>%
        DT::formatStyle(
          columns_logical,
          color = DT::styleEqual(c(TRUE, FALSE), c(up_col, down_col)),
          fontWeight = DT::styleEqual(c(TRUE, FALSE), c('bold', 'normal'))
        )
    }

    ## grouping variables
    columns_groups <- which(colnames(table_original) %in% getGroups())
    if (length(columns_groups) > 0) {
      for (i in columns_groups) {
        group <- colnames(table_original)[i]
        if (
          all(
            unique(table_original[[i]]) %in% names(reactive_colors()[[group]])
          )
        ) {
          table <- table %>%
            DT::formatStyle(
              i,
              backgroundColor = DT::styleEqual(
                names(reactive_colors()[[group]]),
                reactive_colors()[[group]]
              ),
              fontWeight = 'bold'
            )
        }
      }
    }

    ## cell cycle assignments
    columns_cell_cycle <- which(colnames(table_original) %in% getCellCycle())
    if (length(columns_cell_cycle) > 0) {
      for (i in columns_cell_cycle) {
        method <- colnames(table_original)[i]
        if (
          all(
            unique(table_original[[i]]) %in% names(reactive_colors()[[method]])
          )
        ) {
          table <- table %>%
            DT::formatStyle(
              i,
              backgroundColor = DT::styleEqual(
                names(reactive_colors()[[method]]),
                reactive_colors()[[method]]
              ),
              fontWeight = 'bold'
            )
        }
      }
    }
  }

  ## return the table
  return(table)
}

##----------------------------------------------------------------------------##
## Function to prepare empty table.
##----------------------------------------------------------------------------##
prepareEmptyTable <- function(table) {
  DT::datatable(
    table,
    class = "stripe table-bordered table-condensed",
    escape = TRUE,
    filter = "none",
    rownames = FALSE,
    selection = "none",
    style = "bootstrap",
    options = list(
      buttons = list(),
      dom = "Brtip",
      lengthMenu = c(20, 50, 100),
      pageLength = 20,
      scrollX = TRUE
    )
  )
}

##----------------------------------------------------------------------------##
## Function to calculate A-by-B tables, e.g. samples by clusters.
##----------------------------------------------------------------------------##
calculateTableAB <- function(
  table,
  groupA,
  groupB,
  mode,
  percent
) {
  ## check if specified group columns exist in table
  if (groupA %in% colnames(table) == FALSE) {
    stop(
      glue::glue(
        "Column specified as groupA (`{groupA}`) could not be found in meta ",
        "data."
      ),
      call. = FALSE
    )
  }

  if (groupB %in% colnames(table) == FALSE) {
    stop(
      glue::glue(
        "Column specified as groupB (`{groupB}`) could not be found in meta ",
        "data."
      ),
      call. = FALSE
    )
  }

  ## subset columns
  table <- table[, c(groupA, groupB)]

  ## factorize group columns A if not already a factor
  if (is.character(table[[groupA]])) {
    levels_groupA <- table[[groupA]] %>% unique() %>% sort()
    table[, groupA] <- factor(
      table[[groupA]],
      levels = levels_groupA,
      exclude = NULL
    )
  } else {
    levels_groupA <- levels(table[, groupA])
  }

  ## factorize group columns B if not already a factor
  if (is.character(table[[groupB]])) {
    levels_groupB <- table[[groupB]] %>% unique() %>% sort()
    table[, groupB] <- factor(
      table[[groupB]],
      levels = levels_groupB,
      exclude = NULL
    )
  } else {
    levels_groupB <- levels(table[, groupB])
  }

  ## prepare table in long format
  table <- table %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(groupA, groupB)))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(groupA, groupB)))) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groupA))) %>%
    dplyr::mutate(total_cell_count = sum(count)) %>%
    dplyr::ungroup()

  ## convert counts to percent
  if (percent == TRUE) {
    table <- table %>%
      dplyr::mutate(count = count / total_cell_count) %>%
      dplyr::select(
        dplyr::all_of(c(groupA, "total_cell_count", groupB, "count"))
      )
  }

  ## bring table into wide format
  if (mode == "wide") {
    table <- table %>%
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(c(groupA, "total_cell_count")),
        names_from = dplyr::all_of(groupB),
        values_from = "count",
        values_fill = 0
      ) %>%
      dplyr::select(
        dplyr::all_of(groupA),
        'total_cell_count',
        dplyr::any_of(levels_groupB)
      )

    ## fix order of columns if cell cycle info was chosen as second group
    if (
      'G1' %in%
        colnames(table) &&
        'G2M' %in% colnames(table) &&
        'S' %in% colnames(table)
    ) {
      table <- table %>%
        dplyr::select(
          dplyr::all_of(c(groupA, 'total_cell_count', 'G1', 'S', 'G2M')),
          dplyr::everything()
        )
    }
  }

  ##
  return(table)
}

##----------------------------------------------------------------------------##
## Assign colors to groups.
##
## Provide table and column name, and this function will check whether the
## content of the column is categorical. If so, it will check whether colors
## have already been assigned to the levels/unique values and return those
## values. Otherwise, it will assign new colors from the default color set.
## The return value is a named vector.
##----------------------------------------------------------------------------##
assignColorsToGroups <- function(table, grouping_variable) {
  ## check if colors are already assigned in reactive_colors()
  ## ... already assigned
  if (grouping_variable %in% names(reactive_colors())) {
    ## take colors from reactive_colors()
    colors_for_groups <- reactive_colors()[[grouping_variable]]

    ## ... not assigned but values are either factors or characters
  } else if (
    is.factor(table[[grouping_variable]]) ||
      is.character(table[[grouping_variable]])
  ) {
    ## check type of values
    ## ... factors
    if (is.factor(table[[grouping_variable]])) {
      ## get factor levels and assign colors
      colors_for_groups <- setNames(
        cerebro_group_colors(length(levels(table[[grouping_variable]]))),
        levels(table[[grouping_variable]])
      )

      ## ... characters
    } else if (is.character(table[[grouping_variable]])) {
      ## get unique values and assign colors
      colors_for_groups <- setNames(
        cerebro_group_colors(length(unique(table[[grouping_variable]]))),
        unique(table[[grouping_variable]])
      )
    }

    ## ... none of the above (e.g. numeric values)
  } else {
    colors_for_groups <- NULL
  }

  ##
  return(colors_for_groups)
}

##----------------------------------------------------------------------------##
## Build hover info for projections.
##----------------------------------------------------------------------------##
cerebroProjectionHoverColumns <- function(
  table,
  groups = getGroups(),
  group_levels = getGroupLevels
) {
  if (!is.data.frame(table)) {
    stop("projection hover data must be a data frame")
  }
  columns <- list()
  for (spec in list(
    c(source = "nUMI", label = "Transcripts"),
    c(source = "nGene", label = "Expressed genes")
  )) {
    if (spec[["source"]] %in% colnames(table)) {
      columns[[length(columns) + 1L]] <- list(
        label = unname(spec[["label"]]),
        format = "integer",
        values = unname(table[[spec[["source"]]]])
      )
    }
  }
  for (group in unique(as.character(groups))) {
    if (is.na(group) || !nzchar(group) || !group %in% colnames(table)) {
      next
    }
    values <- as.character(table[[group]])
    # Registered levels are a small dictionary. Avoid hashing every string
    # again with unique() after the million-cell rows have been shuffled.
    levels <- tryCatch(as.character(group_levels(group)), error = function(e) {
      character()
    })
    levels[is.na(levels)] <- "NA"
    levels <- unique(c(levels, "NA"))
    codes <- match(values, levels)
    codes[is.na(values)] <- match("NA", levels)
    if (anyNA(codes)) {
      # Unregistered metadata and incomplete legacy dictionaries retain the
      # original first-occurrence encoding, including literal/missing NA.
      values[is.na(values)] <- "NA"
      levels <- unique(values)
      codes <- match(values, levels)
    }
    observed <- unique(codes)
    ## Codes already index the registered dictionary; remap by integer lookup
    ## rather than hashing the full cell vector again with match().
    remap <- integer(length(levels))
    remap[observed] <- seq_along(observed) - 1L
    levels <- levels[observed]
    columns[[length(columns) + 1L]] <- list(
      label = group,
      levels = levels,
      values = remap[codes]
    )
  }
  columns
}

buildHoverInfoForProjections <- function(table) {
  ## Count fields have many repeated values. Format each distinct value once,
  ## then restore cell order without changing the displayed precision.
  format_counts <- function(values) {
    distinct <- unique(values)
    formatC(distinct, format = "f", big.mark = ",", digits = 0)[
      match(values, distinct)
    ]
  }
  cell_ids <- if ("cell_barcode" %in% names(table)) {
    table[["cell_barcode"]]
  } else {
    rownames(table)
  }
  template <- c("<b>Cell</b>: {cell_ids}")
  if ("nUMI" %in% names(table)) {
    umi <- format_counts(table[["nUMI"]])
    template <- c(template, "<br><b>Transcripts</b>: {umi}")
  }
  if ("nGene" %in% names(table)) {
    genes <- format_counts(table[["nGene"]])
    template <- c(template, "<br><b>Expressed genes</b>: {genes}")
  }
  hover_info <- glue::glue(paste0(template, collapse = ""))
  ## add info for known grouping variables
  for (group in intersect(getGroups(), names(table))) {
    hover_info <- glue::glue(
      "{hover_info}<br>",
      "<b>{group}</b>: {table[[ group ]]}"
    )
  }
  return(hover_info)
}

##----------------------------------------------------------------------------##
## Apply inclusive group filters consistently across projection-style tabs.
##----------------------------------------------------------------------------##
cerebroGroupFilterMask <- function(metadata, filters) {
  keep <- rep(TRUE, nrow(metadata))
  for (group in names(filters)) {
    if (!group %in% colnames(metadata)) {
      next
    }
    selected <- filters[[group]]
    if (is.null(selected) || !length(selected)) {
      return(rep(FALSE, nrow(metadata)))
    }
    keep <- keep & metadata[[group]] %in% selected
  }
  keep
}

##----------------------------------------------------------------------------##
## Randomly subset cells in data frame, if necessary.
##----------------------------------------------------------------------------##
randomlySubsetCells <- function(table, percentage) {
  ## check if subsetting is necessary
  ## ... percentage is less than 100
  if (percentage < 100) {
    ## calculate how many cells should be left after subsetting
    size_of_subset <- ceiling(percentage / 100 * nrow(table))
    ## get IDs of all cells
    cell_ids <- rownames(table)
    ## subset cell IDs
    subset_of_cell_ids <- cell_ids[sample(seq_along(cell_ids), size_of_subset)]
    ## subset table and return
    return(table[subset_of_cell_ids, ])
    ## ... percentage is 100 -> no subsetting needed
  } else {
    ## return original table
    return(table)
  }
}

##----------------------------------------------------------------------------##
## Calculate X-Y ranges for projections.
##----------------------------------------------------------------------------##
getXYranges <- function(table) {
  ranges <- list(
    x = list(
      min = table[, 1] %>%
        min(na.rm = TRUE) %>%
        "*"(ifelse(. < 0, 1.1, 0.9)) %>%
        round(),
      max = table[, 1] %>%
        max(na.rm = TRUE) %>%
        "*"(ifelse(. < 0, 0.9, 1.1)) %>%
        round()
    ),
    y = list(
      min = table[, 2] %>%
        min(na.rm = TRUE) %>%
        "*"(ifelse(. < 0, 1.1, 0.9)) %>%
        round(),
      max = table[, 2] %>%
        max(na.rm = TRUE) %>%
        "*"(ifelse(. < 0, 0.9, 1.1)) %>%
        round()
    )
  )
  return(ranges)
}

##----------------------------------------------------------------------------##
## Function to get genes for selected gene set.
##----------------------------------------------------------------------------##
getGenesForGeneSet <- function(gene_set) {
  if (
    !is.null(getExperiment()$organism) &&
      getExperiment()$organism == "mm"
  ) {
    species <- "Mus musculus"
  } else if (
    !is.null(getExperiment()$organism) &&
      getExperiment()$organism == "hg"
  ) {
    species <- "Homo sapiens"
  } else {
    species <- "Mus musculus"
  }

  getMsigdbGenes(species, gene_set)
}

##----------------------------------------------------------------------------##
## Helper: match a URL dataset token against available .crb files.
##
## Returns the matched file path or '' if no match.
##----------------------------------------------------------------------------##
match_dataset_by_url <- function(url_dataset, files, file_names = NULL) {
  ## Case A: Match by Name (if names exist)
  if (!is.null(file_names) && url_dataset %in% file_names) {
    return(files[[url_dataset]])
  }
  ## Case B: Match by Filename (basename)
  basenames <- basename(files)
  idx <- which(basenames == url_dataset)
  if (length(idx) == 0) {
    basenames_no_ext <- tools::file_path_sans_ext(basenames)
    idx <- which(basenames_no_ext == url_dataset)
  }
  if (length(idx) > 0) {
    return(files[[idx[1]]])
  }
  ## No match
  return('')
}

match_dataset_id_by_url <- function(url_dataset, files) {
  ids <- names(files)
  if (
    !is.character(url_dataset) ||
      length(url_dataset) != 1L ||
      is.na(url_dataset) ||
      !nzchar(url_dataset) ||
      is.null(ids)
  ) {
    return("")
  }
  if (url_dataset %in% ids) {
    return(url_dataset)
  }
  basenames <- basename(files)
  index <- which(basenames == url_dataset)
  if (!length(index)) {
    index <- which(tools::file_path_sans_ext(basenames) == url_dataset)
  }
  if (length(index)) ids[[index[[1L]]]] else ""
}

##----------------------------------------------------------------------------##
## Functions to interact with data set.
##
## Never directly interact with data set: data_set()
##----------------------------------------------------------------------------##
is_cerebro_dataset <- function(object) {
  is.environment(object) &&
    inherits(object, "R6") &&
    any(startsWith(class(object), "Cerebro"))
}

getExperiment <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getExperiment())
  }
}
getParameters <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getParameters())
  }
}
getTechnicalInfo <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getTechnicalInfo())
  }
}
getGeneLists <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getGeneLists())
  }
}
getGeneNames <- function() {
  cache <- viewerProjectionFirstFrameCache()
  if (!is.null(cache$gene_names)) {
    return(cache$gene_names)
  }
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getGeneNames())
  }
}
getGroups <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getGroups())
  }
}
getGroupLevels <- function(group) {
  cache <- viewerProjectionFirstFrameCache()
  if (!is.null(cache$meta_data) && group %in% colnames(cache$meta_data)) {
    values <- cache$meta_data[[group]]
    return(if (is.factor(values)) levels(values) else unique(values))
  }
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getGroupLevels(group))
  }
}
getCellCycle <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getCellCycle())
  }
}
getMetaData <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getMetaData())
  }
}
.runtimeCerebroCellCount <- function(object) {
  schema <- if (
    is.environment(object) &&
      exists("crb_schema", envir = object, inherits = FALSE)
  ) {
    object$crb_schema
  } else {
    NULL
  }
  if (
    is.list(schema) &&
      identical(schema$version, 2L) &&
      is.integer(schema$n_cells) &&
      length(schema$n_cells) == 1L &&
      !is.na(schema$n_cells)
  ) {
    return(schema$n_cells)
  }
  nrow(object$meta_data)
}
getNumberOfCells <- function() {
  .runtimeCerebroCellCount(data_set())
}
availableProjections <- function() {
  cache <- viewerProjectionFirstFrameCache()
  if (!is.null(cache$projections)) {
    return(names(cache$projections))
  }
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$availableProjections())
  }
}
getProjection <- function(name) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getProjection(name))
  }
}
getMethodsForMarkerGenes <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getMethodsForMarkerGenes())
  }
}
getGroupsWithMarkerGenes <- function(method) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getGroupsWithMarkerGenes(method))
  }
}
getMarkerGenes <- function(method, group) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getMarkerGenes(method, group))
  }
}
getMethodsForTrajectories <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getMethodsForTrajectories())
  }
}
getNamesOfTrajectories <- function(method) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getNamesOfTrajectories(method))
  }
}
getTrajectory <- function(method, name) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getTrajectory(method, name))
  }
}

##----------------------------------------------------------------------------##
## Metadata column detectors + comparison-variable choices.
##
## Restored from an earlier utility layer; the Trajectory tab
## depends on them (mito/ribo/ery expression-metric sub-tabs and the "variable
## to compare" selector along pseudotime). They inspect the current data set's
## metadata columns, so they honour whatever the loaded .crb carries.
##----------------------------------------------------------------------------##
getVariableToCompareChoices <- function() {
  ## default: all metadata columns except cell_barcode
  all_cols <- colnames(getMetaData())[
    !colnames(getMetaData()) %in% c("cell_barcode")
  ]

  ## check if variable_to_compare option exists
  if (
    !exists('Cerebro.options') ||
      is.null(Cerebro.options[['variable_to_compare']])
  ) {
    return(all_cols)
  }

  var_compare <- Cerebro.options[['variable_to_compare']]
  use_groups_intersection <- FALSE

  ## case 1: single boolean TRUE
  if (
    is.logical(var_compare) &&
      length(var_compare) == 1 &&
      !is.na(var_compare)
  ) {
    use_groups_intersection <- var_compare
  } else if (
    ## case 2: named list or vector
    (is.list(var_compare) || is.vector(var_compare)) &&
      !is.null(names(var_compare))
  ) {
    ## get current crb file name
    current_name <- NULL
    if (
      exists("available_crb_files") &&
        !is.null(available_crb_files$files) &&
        !is.null(available_crb_files$selected)
    ) {
      current_name <- viewerDatasetName(
        available_crb_files$files,
        available_crb_files$selected
      )
    }

    ## check if current file name exists in the named list/vector
    if (!is.null(current_name) && current_name %in% names(var_compare)) {
      val <- var_compare[[current_name]]
      if (is.logical(val) && length(val) == 1 && !is.na(val)) {
        use_groups_intersection <- val
      }
    }
  }

  ## if should use intersection of groups and metadata columns
  if (use_groups_intersection) {
    groups <- getGroups()
    if (!is.null(groups) && length(groups) > 0) {
      intersection <- intersect(groups, all_cols)
      if (length(intersection) > 0) {
        return(intersection)
      }
    }
  }

  ## default fallback
  return(all_cols)
}

getMitoColumn <- function() {
  cols <- colnames(getMetaData())
  patterns <- c(
    "^percent[_.]?mt$",
    "^percent[_.]?mito$",
    "^percent[_.]?mitochondrial$",
    "^pct[_.]?mt$",
    "^pct[_.]?mito$",
    "^pct[_.]?mitochondrial$",
    "^mt[_.]?percent$",
    "^mito[_.]?percent$",
    "^mitochondrial[_.]?percent$",
    "^mito[_.]?pct$",
    "^mt[_.]?pct$"
  )
  for (pattern in patterns) {
    matches <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      return(matches[1])
    }
  }
  return(NULL)
}

hasMitoColumn <- function() {
  !is.null(getMitoColumn())
}

getRiboColumn <- function() {
  cols <- colnames(getMetaData())
  patterns <- c(
    "^percent[_.]?ribo$",
    "^percent[_.]?ribosomal$",
    "^pct[_.]?ribo$",
    "^pct[_.]?ribosomal$",
    "^ribo[_.]?percent$",
    "^ribosomal[_.]?percent$",
    "^ribo[_.]?pct$",
    "^ribosomal[_.]?pct$"
  )
  for (pattern in patterns) {
    matches <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      return(matches[1])
    }
  }
  return(NULL)
}

hasRiboColumn <- function() {
  !is.null(getRiboColumn())
}

getEryColumn <- function() {
  cols <- colnames(getMetaData())
  patterns <- c(
    "^percent[_.]?ery$",
    "^percent[_.]?erythrocyte$",
    "^percent[_.]?hb$",
    "^percent[_.]?hgb$",
    "^percent[_.]?hemoglobin$",
    "^percent[_.]?haemoglobin$",
    "^pct[_.]?ery$",
    "^pct[_.]?erythrocyte$",
    "^pct[_.]?hb$",
    "^pct[_.]?hgb$",
    "^pct[_.]?hemoglobin$",
    "^pct[_.]?haemoglobin$",
    "^ery[_.]?percent$",
    "^erythrocyte[_.]?percent$",
    "^hb[_.]?percent$",
    "^hgb[_.]?percent$",
    "^hemoglobin[_.]?percent$",
    "^haemoglobin[_.]?percent$",
    "^ery[_.]?pct$",
    "^hb[_.]?pct$",
    "^hgb[_.]?pct$"
  )
  for (pattern in patterns) {
    matches <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      return(matches[1])
    }
  }
  return(NULL)
}

hasEryColumn <- function() {
  !is.null(getEryColumn())
}
##----------------------------------------------------------------------------##
## Cerebro file reader (RDS or qs2 payload).
##----------------------------------------------------------------------------##
read_cerebro_file <- function(file) {
  connection <- file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  magic <- readBin(connection, "raw", n = 4L)
  if (!identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))) {
    return(readRDS(file))
  }
  if (!requireNamespace("qs2", quietly = TRUE)) {
    stop("A qs2 CRB requires the qs2 package.", call. = FALSE)
  }
  qs2::qs_read(file)
}

##----------------------------------------------------------------------------##
## Process-scoped cache for loaded .crb files (B8).
##
## crb_cache.R owns process-retained prototypes; this session adapter attaches
## lazy backends only to shallow R6 clones, keeping session fields isolated.
##----------------------------------------------------------------------------##
.cloneCachedCrb <- function(object) {
  can_clone <-
    is.environment(object) &&
    exists("clone", envir = object, inherits = FALSE) &&
    is.function(object[["clone"]]) &&
    is.environment(environment(object[["clone"]])) &&
    exists(
      "self",
      envir = environment(object[["clone"]]),
      inherits = FALSE
    ) &&
    identical(
      get("self", envir = environment(object[["clone"]]), inherits = FALSE),
      object
    )
  if (can_clone) {
    return(object$clone(deep = FALSE))
  }
  object
}

.crbLogLabel <- function(path) {
  basename(path)
}

.runtimeBackendPlanError <- function(message, crb_path = NULL) {
  suffix <- if (is.null(crb_path)) {
    ""
  } else {
    paste0(" for CRB '", crb_path, "'")
  }
  stop("The backend plan ", message, suffix, ".", call. = FALSE)
}

.runtimeWindowsPathSegmentInvalid <- function(parts) {
  grepl("[[:cntrl:]<>:\"|?*]", parts) |
    grepl("[. ]$", parts) |
    grepl(
      "^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])($|\\.)",
      parts,
      ignore.case = TRUE
    )
}

.runtimePortableBackendPath <- function(path, context) {
  valid <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path) &&
    !grepl("/$", path) &&
    !grepl("\\\\", path) &&
    !grepl("^(/|~|[A-Za-z]:)", path)
  parts <- if (valid) {
    strsplit(path, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  windows_invalid <- length(parts) > 0L &&
    any(.runtimeWindowsPathSegmentInvalid(parts))
  if (
    !valid ||
      length(parts) == 0L ||
      any(!nzchar(parts)) ||
      any(parts %in% c(".", "..")) ||
      windows_invalid
  ) {
    stop(context, " must be a portable relative path.", call. = FALSE)
  }
  paste(parts, collapse = "/")
}

.runtimeAbsoluteBackendPath <- function(path) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    return(FALSE)
  }
  normalized <- gsub("\\\\", "/", path)
  startsWith(normalized, "/") || grepl("^[A-Za-z]:/", normalized)
}

.validateRuntimeBackendEntry <- function(entry, crb_path) {
  entry_names <- names(entry)
  valid_shape <- is.list(entry) &&
    !is.data.frame(entry) &&
    length(entry) == 3L &&
    !is.null(entry_names) &&
    !anyDuplicated(entry_names) &&
    setequal(entry_names, c("type", "mode", "location"))
  if (!valid_shape) {
    .runtimeBackendPlanError("entry is malformed", crb_path)
  }
  valid_type <- is.character(entry$type) &&
    length(entry$type) == 1L &&
    !is.na(entry$type) &&
    entry$type %in% c("embedded", "h5", "bpcells")
  valid_mode <- is.character(entry$mode) &&
    length(entry$mode) == 1L &&
    !is.na(entry$mode) &&
    entry$mode %in% c("embedded", "bundled", "host_override")
  if (!valid_type || !valid_mode) {
    .runtimeBackendPlanError("entry has an unsupported type or mode", crb_path)
  }
  if (identical(entry$mode, "embedded")) {
    if (!identical(entry$type, "embedded") || !is.null(entry$location)) {
      .runtimeBackendPlanError("embedded entry is inconsistent", crb_path)
    }
  } else if (identical(entry$mode, "bundled")) {
    if (identical(entry$type, "embedded")) {
      .runtimeBackendPlanError("bundled entry is inconsistent", crb_path)
    }
    entry$location <- .runtimePortableBackendPath(
      entry$location,
      "The backend plan bundled backend location"
    )
  } else {
    if (
      identical(entry$type, "embedded") ||
        !.runtimeAbsoluteBackendPath(entry$location)
    ) {
      .runtimeBackendPlanError("host override entry is inconsistent", crb_path)
    }
  }
  list(type = entry$type, mode = entry$mode, location = entry$location)
}

.configuredRuntimeBackendPlan <- function(
  path,
  backend_plan = NULL,
  configured_paths = character()
) {
  if (is.null(backend_plan)) {
    return(NULL)
  }
  if (!is.character(configured_paths) || anyNA(configured_paths)) {
    .runtimeBackendPlanError("has invalid configured CRB paths")
  }
  if (!path %in% configured_paths) {
    return(NULL)
  }
  plan_names <- names(backend_plan)
  valid_plan <- is.list(backend_plan) &&
    !is.data.frame(backend_plan) &&
    length(backend_plan) == 2L &&
    !is.null(plan_names) &&
    !anyDuplicated(plan_names) &&
    setequal(plan_names, c("schema_version", "entries")) &&
    identical(backend_plan$schema_version, 1L) &&
    is.list(backend_plan$entries) &&
    !is.data.frame(backend_plan$entries)
  if (!valid_plan) {
    .runtimeBackendPlanError("is malformed")
  }
  entry_names <- names(backend_plan$entries)
  if (
    length(backend_plan$entries) > 0L &&
      (is.null(entry_names) ||
        anyNA(entry_names) ||
        any(!nzchar(entry_names)) ||
        anyDuplicated(entry_names))
  ) {
    .runtimeBackendPlanError("entries must have unique CRB paths")
  }
  validated <- lapply(seq_along(backend_plan$entries), function(index) {
    .validateRuntimeBackendEntry(
      backend_plan$entries[[index]],
      entry_names[[index]]
    )
  })
  names(validated) <- entry_names
  entry <- validated[[path]]
  if (is.null(entry)) {
    .runtimeBackendPlanError("has no entry for configured CRB", path)
  }
  entry
}

.runtimeBackendCacheIdentity <- function(effective_backend) {
  if (!is.null(effective_backend)) {
    return(list(source = "configured", backend = effective_backend))
  }
  options <- .runtimeCerebroOptions()
  list(
    source = "fallback",
    expression_matrix_h5 = options[["expression_matrix_h5"]],
    expression_matrix_BPCells = options[["expression_matrix_BPCells"]]
  )
}

get_or_load_crb <- function(
  path,
  backend_plan = NULL,
  configured_paths = character()
) {
  effective_backend <- .configuredRuntimeBackendPlan(
    path,
    backend_plan,
    configured_paths
  )
  cache_identity <- .runtimeBackendCacheIdentity(effective_backend)
  cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  .checkCrbBackendIdentity(cache_key, cache_identity)
  cached <- .touchCrbPrototype(cache_key)
  if (!is.null(cached)) {
    print(glue::glue("[{Sys.time()}] CRB cache hit: {.crbLogLabel(path)}"))
    prototype <- cached$object
  } else {
    print(glue::glue(
      "[{Sys.time()}] CRB cache miss, loading: {.crbLogLabel(path)}"
    ))
    prototype <- read_cerebro_file(path)
    .cacheCrbPrototype(path, prototype, cache_identity)
  }

  ## Clone the inert serialized prototype before installing delayed bindings.
  ## R6 clone() forces delayed fields, which previously attached BPCells while
  ## the user was still on Data Info and also mutated the shared prototype.
  obj <- .cloneCachedCrb(prototype)
  obj <- .attachExternalExpression(obj, path, effective_backend)
  obj <- .attachImmuneRepertoireBackend(obj, path, effective_backend)
  obj <- .attachSpatialMoleculeBackend(obj, path)
  obj
}

.attachImmuneRepertoireBackend <- function(
  obj,
  crb_path,
  effective_backend = NULL
) {
  field <- "immune_repertoire_backend"
  if (!is.environment(obj) || !exists(field, envir = obj, inherits = FALSE)) {
    return(obj)
  }
  if (
    bindingIsActive(field, obj) ||
      isTRUE(rlang::env_binding_are_lazy(obj, field))
  ) {
    stop("The immune repertoire sidecar descriptor is invalid.", call. = FALSE)
  }
  backend <- obj[[field]]
  if (is.null(backend)) {
    return(obj)
  }
  valid <- is.list(backend) &&
    identical(backend$type, "bpcells-file") &&
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
  expression_backend <- if (is.null(effective_backend)) {
    .fallbackRuntimeBackendPlan(obj, crb_path)
  } else {
    .validateRuntimeBackendEntry(effective_backend, crb_path)
  }
  valid <- valid && identical(expression_backend$type, "bpcells")
  if (!valid) {
    stop("The immune repertoire sidecar descriptor is invalid.", call. = FALSE)
  }
  backend$samples <- unique(as.character(backend$samples %||% character()))
  backend$chains <- unique(as.character(backend$chains %||% character()))
  if (anyNA(backend$samples) || anyNA(backend$chains)) {
    stop("The immune repertoire sidecar descriptor is invalid.", call. = FALSE)
  }
  backend$samples <- backend$samples[nzchar(backend$samples)]
  backend$chains <- backend$chains[nzchar(backend$chains)]
  root <- if (identical(expression_backend$mode, "host_override")) {
    expression_backend$location
  } else {
    file.path(
      dirname(normalizePath(crb_path, mustWork = FALSE)),
      expression_backend$location
    )
  }
  if (
    !is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)
  ) {
    stop("The immune repertoire sidecar has no BPCells root.", call. = FALSE)
  }
  repertoire_file <- file.path(root, backend$file)
  if (!file.exists(repertoire_file) || dir.exists(repertoire_file)) {
    stop(
      "The immune repertoire sidecar is missing: ",
      repertoire_file,
      call. = FALSE
    )
  }
  backend$root <- root
  obj[[field]] <- backend
  obj
}

.attachSpatialMoleculeBackend <- function(obj, crb_path) {
  field <- "spatial_molecule_backend"
  if (!is.environment(obj) || !exists(field, envir = obj, inherits = FALSE)) {
    return(obj)
  }
  if (
    bindingIsActive(field, obj) ||
      isTRUE(rlang::env_binding_are_lazy(obj, field))
  ) {
    stop("The spatial molecule backend descriptor is invalid.", call. = FALSE)
  }
  backend <- obj[[field]]
  if (is.null(backend)) {
    return(obj)
  }
  valid <- is.list(backend) &&
    identical(backend$type, "directory") &&
    is.character(backend$location) &&
    length(backend$location) == 1L &&
    !is.na(backend$location) &&
    nzchar(backend$location) &&
    !backend$location %in% c(".", "..") &&
    !grepl("[/\\\\]", backend$location)
  if (!valid) {
    stop("The spatial molecule backend descriptor is invalid.", call. = FALSE)
  }
  root <- file.path(
    dirname(normalizePath(crb_path, mustWork = FALSE)),
    backend$location
  )
  if (!dir.exists(root)) {
    stop("The spatial molecule sidecar is missing: ", root, call. = FALSE)
  }
  backend$root <- root
  obj[[field]] <- backend
  obj
}

##----------------------------------------------------------------------------##
## Resolve an external expression backend at load time (B3).
##
## bpcells crbs ship a sibling <stem>.bpcells/ directory; the IterableMatrix
## handle persisted into the crb carries the writer's absolute @dir, which
## breaks once the crb is moved. This helper rebuilds the handle from a path
## rooted at the caller's view of the filesystem.
##
## Configured bundle CRBs receive the build-validated effective backend entry.
## Direct launches and uploads read only the ordinary expression_backend field;
## serialized getter code is never invoked by this internal loading path.
##----------------------------------------------------------------------------##
.readRuntimeCrbSchema <- function(obj, crb_path) {
  field <- "crb_schema"
  if (!is.environment(obj) || !exists(field, envir = obj, inherits = FALSE)) {
    return(NULL)
  }
  if (
    bindingIsActive(field, obj) ||
      isTRUE(rlang::env_binding_are_lazy(obj, field))
  ) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported CRB schema descriptor.",
      call. = FALSE
    )
  }
  schema <- obj[[field]]
  if (is.null(schema)) {
    return(NULL)
  }
  schema_names <- names(schema)
  expected_names <- if (identical(schema$version, 1L)) {
    c("version", "cell_names", "cell_names_md5", "projection_rownames")
  } else if (identical(schema$version, 2L)) {
    c(
      "version",
      "cell_names",
      "cell_names_md5",
      "projection_rownames",
      "n_cells"
    )
  } else {
    character()
  }
  valid <- is.list(schema) &&
    !is.data.frame(schema) &&
    length(expected_names) > 0L &&
    length(schema) == length(expected_names) &&
    !is.null(schema_names) &&
    !anyDuplicated(schema_names) &&
    setequal(schema_names, expected_names) &&
    identical(schema$cell_names, "expression") &&
    is.character(schema$cell_names_md5) &&
    length(schema$cell_names_md5) == 1L &&
    !is.na(schema$cell_names_md5) &&
    grepl("^[[:xdigit:]]{32}$", schema$cell_names_md5) &&
    is.character(schema$projection_rownames) &&
    !anyNA(schema$projection_rownames) &&
    !any(!nzchar(schema$projection_rownames)) &&
    !anyDuplicated(schema$projection_rownames) &&
    (identical(schema$version, 1L) ||
      (is.integer(schema$n_cells) &&
        length(schema$n_cells) == 1L &&
        !is.na(schema$n_cells) &&
        schema$n_cells >= 0L))
  if (!valid) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported CRB schema descriptor.",
      call. = FALSE
    )
  }
  schema
}

.bpcellsCellNamesChecksum <- function(sidecar) {
  names_file <- file.path(sidecar, "col_names")
  if (!file.exists(names_file) || dir.exists(names_file)) {
    stop("The BPCells sidecar has no cell-name index.", call. = FALSE)
  }
  checksum <- unname(tools::md5sum(names_file))
  if (length(checksum) != 1L || is.na(checksum) || !nzchar(checksum)) {
    stop("Could not checksum the BPCells cell-name index.", call. = FALSE)
  }
  checksum
}

.validateThinCrbShape <- function(metadata, projections, schema) {
  expected_cells <- if (identical(schema$version, 2L)) {
    schema$n_cells
  } else {
    nrow(metadata)
  }
  if (
    !is.data.frame(metadata) ||
      nrow(metadata) != expected_cells ||
      "cell_barcode" %in% names(metadata)
  ) {
    stop(
      "The thin CRB cell count does not match its cell metadata.",
      call. = FALSE
    )
  }

  missing_projections <- setdiff(
    schema$projection_rownames,
    names(projections)
  )
  if (length(missing_projections)) {
    stop(
      "The thin CRB is missing projection '",
      missing_projections[[1L]],
      "'.",
      call. = FALSE
    )
  }
  for (name in schema$projection_rownames) {
    projection <- projections[[name]]
    if (!is.data.frame(projection) || nrow(projection) != expected_cells) {
      stop(
        "Projection '",
        name,
        "' does not match the thin CRB cell index.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.hydrateThinCrbFields <- function(metadata, projections, schema, cells) {
  if (
    !is.character(cells) ||
      anyNA(cells) ||
      any(!nzchar(cells)) ||
      anyDuplicated(cells) ||
      nrow(metadata) != length(cells)
  ) {
    stop(
      "The thin CRB and BPCells sidecar have incompatible cell metadata.",
      call. = FALSE
    )
  }
  for (name in schema$projection_rownames) {
    projection <- projections[[name]]
    rownames(projection) <- cells
    projections[[name]] <- projection
  }
  rownames(metadata) <- NULL
  list(
    meta_data = data.frame(
      cell_barcode = cells,
      metadata,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    projections = projections
  )
}

.hydrateThinCrb <- function(obj, crb_path, sidecar, schema, cells = NULL) {
  if (!identical(.bpcellsCellNamesChecksum(sidecar), schema$cell_names_md5)) {
    stop(
      "The BPCells cell-name index does not match CRB '",
      basename(crb_path),
      "'.",
      call. = FALSE
    )
  }
  if (is.null(cells)) {
    cells <- colnames(obj$expression)
  }
  metadata <- obj$meta_data
  projections <- obj$projections
  .validateThinCrbShape(metadata, projections, schema)
  hydrated <- .hydrateThinCrbFields(metadata, projections, schema, cells)
  obj$meta_data <- hydrated$meta_data
  obj$projections <- hydrated$projections
  obj
}

.deferThinCrbHydration <- function(obj, crb_path, sidecar, schema) {
  if (!identical(.bpcellsCellNamesChecksum(sidecar), schema$cell_names_md5)) {
    stop(
      "The BPCells cell-name index does not match CRB '",
      basename(crb_path),
      "'.",
      call. = FALSE
    )
  }
  metadata <- obj$meta_data
  projections <- obj$projections
  .validateThinCrbShape(metadata, projections, schema)
  gene_names_file <- file.path(sidecar, "row_names")
  gene_names <- if (
    file.exists(gene_names_file) && !dir.exists(gene_names_file)
  ) {
    readLines(gene_names_file, warn = FALSE)
  } else {
    NULL
  }
  attr(obj, "cerebro_projection_first_frame") <- list(
    meta_data = metadata,
    projections = projections,
    canonical_projections = schema$projection_rownames,
    aligned_projections = new.env(parent = emptyenv()),
    gene_names = gene_names
  )
  hydrate <- local({
    hydrated <- NULL
    function() {
      if (is.null(hydrated)) {
        cells <- readLines(file.path(sidecar, "col_names"), warn = FALSE)
        hydrated <<- .hydrateThinCrbFields(
          metadata,
          projections,
          schema,
          cells
        )
      }
      hydrated
    }
  })
  delayedAssign(
    "meta_data",
    hydrate()$meta_data,
    eval.env = environment(),
    assign.env = obj
  )
  delayedAssign(
    "projections",
    hydrate()$projections,
    eval.env = environment(),
    assign.env = obj
  )
  obj
}

.readRuntimeBackendDescriptor <- function(obj, crb_path) {
  if (!is.environment(obj)) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  getter_name <- "getExpressionBackend"
  field_name <- "expression_backend"
  getter_exists <- exists(getter_name, envir = obj, inherits = FALSE)
  field_exists <- exists(field_name, envir = obj, inherits = FALSE)
  getter_active <- getter_exists && bindingIsActive(getter_name, obj)
  field_active <- field_exists && bindingIsActive(field_name, obj)
  getter_lazy <- getter_exists &&
    !getter_active &&
    isTRUE(rlang::env_binding_are_lazy(obj, getter_name))
  field_lazy <- field_exists &&
    !field_active &&
    isTRUE(rlang::env_binding_are_lazy(obj, field_name))
  invalid <- getter_active ||
    field_active ||
    getter_lazy ||
    field_lazy ||
    xor(getter_exists, field_exists)
  if (invalid) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  if (!getter_exists && !field_exists) {
    return(list(type = "embedded", location = NULL, legacy = TRUE))
  }
  getter <- get(getter_name, envir = obj, inherits = FALSE)
  if (!is.function(getter)) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  backend <- get(field_name, envir = obj, inherits = FALSE)
  if (is.null(backend)) {
    return(list(type = "embedded", location = NULL, legacy = FALSE))
  }
  valid_type <- is.list(backend) &&
    is.character(backend$type) &&
    length(backend$type) == 1L &&
    !is.na(backend$type) &&
    backend$type %in% c("embedded", "h5", "bpcells")
  valid_location <- valid_type &&
    if (identical(backend$type, "embedded")) {
      is.null(backend$location)
    } else {
      is.character(backend$location) &&
        length(backend$location) == 1L &&
        !is.na(backend$location) &&
        nzchar(backend$location)
    }
  if (!valid_type || !valid_location) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  if (!identical(backend$type, "embedded")) {
    backend$location <- .runtimePortableBackendPath(
      backend$location,
      paste0("The ", backend$type, " backend location")
    )
  }
  list(type = backend$type, location = backend$location, legacy = FALSE)
}

.runtimeCerebroOptions <- function() {
  if (!exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)) {
    return(list())
  }
  options <- get("Cerebro.options", envir = .GlobalEnv)
  if (!is.list(options)) {
    stop("Cerebro.options must be a list.", call. = FALSE)
  }
  options
}

.normalizeRuntimeOverride <- function(path, key) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    stop("Cerebro.options[['", key, "']] must be a path.", call. = FALSE)
  }
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

.fallbackRuntimeBackendPlan <- function(obj, crb_path) {
  backend <- .readRuntimeBackendDescriptor(obj, crb_path)
  options <- .runtimeCerebroOptions()
  override_key <- NULL
  if (isTRUE(backend$legacy)) {
    if (!is.null(options[["expression_matrix_h5"]])) {
      override_key <- "expression_matrix_h5"
      backend$type <- "h5"
    } else if (!is.null(options[["expression_matrix_BPCells"]])) {
      override_key <- "expression_matrix_BPCells"
      backend$type <- "bpcells"
    }
  } else if (!identical(backend$type, "embedded")) {
    candidate <- switch(
      backend$type,
      h5 = "expression_matrix_h5",
      bpcells = "expression_matrix_BPCells"
    )
    if (!is.null(options[[candidate]])) {
      override_key <- candidate
    }
  }
  if (!is.null(override_key)) {
    return(list(
      type = backend$type,
      mode = "host_override",
      location = .normalizeRuntimeOverride(
        options[[override_key]],
        override_key
      )
    ))
  }
  if (isTRUE(backend$legacy) || identical(backend$type, "embedded")) {
    return(list(type = "embedded", mode = "embedded", location = NULL))
  }
  list(type = backend$type, mode = "bundled", location = backend$location)
}

.runtimeBackendRecoveryAdvice <- function(backend, configured) {
  key <- switch(
    backend$type,
    h5 = "expression_matrix_h5",
    bpcells = "expression_matrix_BPCells"
  )
  if (identical(backend$mode, "host_override")) {
    if (configured) {
      return(paste0(
        "Rebuild the app with a valid cerebro_options[['",
        key,
        "']] host override."
      ))
    }
    return(paste0(
      "Set Cerebro.options[['",
      key,
      "']] to a valid host path before loading the CRB."
    ))
  }
  paste0(
    "Did the .",
    if (identical(backend$type, "h5")) "h5" else "bpcells/",
    " sibling get moved or dropped when the CRB was copied?"
  )
}

.attachExternalExpression <- function(
  obj,
  crb_path,
  effective_backend = NULL
) {
  if (!any(grepl("Cerebro", class(obj)))) {
    return(obj)
  }
  crb_schema <- .readRuntimeCrbSchema(obj, crb_path)
  configured <- !is.null(effective_backend)
  if (!configured) {
    be <- .fallbackRuntimeBackendPlan(obj, crb_path)
  } else {
    be <- .validateRuntimeBackendEntry(effective_backend, crb_path)
  }

  if (identical(be$mode, "embedded")) {
    if (!is.null(crb_schema)) {
      stop("A thin CRB requires a BPCells expression backend.", call. = FALSE)
    }
    return(obj)
  }

  if (!is.null(crb_schema) && !identical(be$type, "bpcells")) {
    stop("A thin CRB requires a BPCells expression backend.", call. = FALSE)
  }

  if (identical(be$mode, "host_override")) {
    loc_abs <- be$location
  } else {
    crb_dir <- dirname(normalizePath(crb_path, mustWork = FALSE))
    loc_abs <- file.path(crb_dir, be$location)
  }

  if (be$type == "bpcells") {
    if (!dir.exists(loc_abs)) {
      stop(
        sprintf(
          paste0(
            "Expected BPCells matrix directory at '%s' for crb '%s' ",
            "(backend location '%s'), but the directory does not exist. "
          ),
          loc_abs,
          crb_path,
          be$location
        ),
        .runtimeBackendRecoveryAdvice(be, configured),
        call. = FALSE
      )
    }
    if (!is.null(crb_schema)) {
      if (identical(crb_schema$version, 2L)) {
        obj <- .deferThinCrbHydration(obj, crb_path, loc_abs, crb_schema)
      } else {
        cells <- readLines(file.path(loc_abs, "col_names"), warn = FALSE)
        obj <- .hydrateThinCrb(obj, crb_path, loc_abs, crb_schema, cells)
      }
      delayedAssign(
        "expression",
        {
          if (!requireNamespace("BPCells", quietly = TRUE)) {
            stop(
              paste(
                "bpcells-backed crb requires the BPCells package;",
                "please install it."
              ),
              call. = FALSE
            )
          }
          print(glue::glue(
            "[{Sys.time()}] Attaching bpcells backend: {loc_abs}"
          ))
          BPCells::open_matrix_dir(dir = loc_abs)
        },
        eval.env = environment(),
        assign.env = obj
      )
    } else {
      if (!requireNamespace("BPCells", quietly = TRUE)) {
        stop(
          "bpcells-backed crb requires the BPCells package; please install it.",
          call. = FALSE
        )
      }
      print(glue::glue("[{Sys.time()}] Attaching bpcells backend: {loc_abs}"))
      obj$expression <- BPCells::open_matrix_dir(dir = loc_abs)
    }
  } else if (be$type == "h5") {
    if (!requireNamespace("HDF5Array", quietly = TRUE)) {
      stop(
        "h5-backed crb requires the HDF5Array package; please install it ",
        "via BiocManager::install(\"HDF5Array\").",
        call. = FALSE
      )
    }
    if (!file.exists(loc_abs) || dir.exists(loc_abs)) {
      stop(
        sprintf(
          paste0(
            "Expected h5 file at '%s' for crb '%s' ",
            "(backend location '%s'), but it is missing or not a file. "
          ),
          loc_abs,
          crb_path,
          be$location
        ),
        .runtimeBackendRecoveryAdvice(be, configured),
        call. = FALSE
      )
    }
    print(glue::glue(
      "[{Sys.time()}] Attaching h5 backend (lazy TENxMatrix): {loc_abs}"
    ))

    ## On-disk layout is cells x genes (TENxMatrix orientation, optimised for
    ## per-gene column reads). Cerebro's internal layout is genes x cells, so
    ## we transpose lazily — DelayedArray::t() is O(1), no data is read.
    ## The matrix is never materialised into a dgCMatrix at attach time;
    ## queries stream from disk through the DelayedMatrix path in
    ## getExpressionRow / getExpressionBlock.
    m_disk <- HDF5Array::TENxMatrix(loc_abs, group = "expression")
    obj$expression <- t(m_disk)
  } else {
    stop(
      sprintf(
        "Unknown expression backend type '%s' in crb '%s'.",
        be$type,
        crb_path
      ),
      call. = FALSE
    )
  }

  obj
}

## Wrapper functions for most_expressed_genes module.
getMeanExpression <- function(group_name) {
  if (any(grepl("Cerebro", class(data_set())))) {
    data_set()$getMeanExpression(group_name)
  }
}
getGroupsWithMeanExpression <- function() {
  ds <- data_set()
  if (!any(grepl("Cerebro", class(ds)))) {
    return(character(0))
  }
  tryCatch(ds$getGroupsWithMeanExpression(), error = function(e) character(0))
}
getGroupsWithMostExpressedGenes <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getGroupsWithMostExpressedGenes())
  }
}
viewerGetMostExpressedGenes <- function(group) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getMostExpressedGenes(group))
  }
}

## Wrapper functions for enriched_pathways module.
getMethodsForEnrichedPathways <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getMethodsForEnrichedPathways())
  }
}
getGroupsWithEnrichedPathways <- function(method) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getGroupsWithEnrichedPathways(method))
  }
}
getEnrichedPathways <- function(method, group) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getEnrichedPathways(method, group))
  }
}

## Wrapper functions for extra_material module. Generated external tables are
## private RDS assets and are loaded only after the user chooses a sheet.
extra_material_external_table_cache <- if (
  exists(".extra_material_process_cache", inherits = TRUE)
) {
  get(".extra_material_process_cache", inherits = TRUE)
} else {
  new.env(parent = emptyenv())
}

extra_material_table_filter <- function(row_count, column_count) {
  if (as.double(row_count) * as.double(column_count) >= 2000000) {
    "none"
  } else {
    list(position = "top", clear = TRUE)
  }
}

extra_material_table_groups <- function(
  external_manifest,
  embedded,
  table_index = NULL
) {
  if (missing(external_manifest)) {
    external_manifest <- if (
      exists("Cerebro.options", inherits = TRUE) &&
        is.list(get("Cerebro.options", inherits = TRUE))
    ) {
      get("Cerebro.options", inherits = TRUE)$extra_tables
    } else {
      NULL
    }
  }
  if (missing(embedded)) {
    data <- data_set()
    dataset <- viewerSelectedDatasetName()
    if (
      is.null(table_index) &&
        !is.null(dataset) &&
        exists("Cerebro.options", inherits = TRUE)
    ) {
      table_index <- Cerebro.options$viewer_content[[
        dataset
      ]]$extra_material_table_index
    }
    embedded <- if (is_cerebro_dataset(data)) {
      data$getExtraMaterial()$tables
    } else {
      NULL
    }
  }

  groups <- list()
  if (is.list(embedded) && length(embedded)) {
    valid_label <- function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(value))
    }
    for (index in seq_along(embedded)) {
      table <- embedded[[index]]
      if (!is.data.frame(table)) {
        next
      }
      name <- names(embedded)[index]
      if (!valid_label(name)) {
        name <- paste("Table", index)
      }
      record <- if (is.list(table_index)) table_index[[name]] else NULL
      if (!is.list(record)) {
        record <- list()
      }
      label <- if (valid_label(record$display_name)) {
        record$display_name
      } else {
        name
      }
      workbook <- if (valid_label(record$workbook_name)) {
        record$workbook_name
      } else {
        NULL
      }
      group_key <- if (is.null(workbook)) {
        "embedded"
      } else {
        paste0("embedded-workbook:", workbook)
      }
      matches <- which(vapply(
        groups,
        function(group) identical(group$key, group_key),
        logical(1)
      ))
      if (!length(matches)) {
        group_name <- if (is.null(workbook)) "Embedded tables" else workbook
        unique_name <- make.unique(c(names(groups), group_name))[[
          length(groups) + 1L
        ]]
        groups[[unique_name]] <- list(
          key = group_key,
          label = group_name,
          sheets = list()
        )
        matches <- length(groups)
      }
      group <- matches[[1L]]
      groups[[group]]$sheets <- append(
        groups[[group]]$sheets,
        list(list(
          key = paste0("embedded:", index),
          label = label,
          table = table
        ))
      )
    }
  }

  files <- if (is.list(external_manifest)) external_manifest$files else NULL
  if (is.list(files)) {
    for (index in seq_along(files)) {
      file <- files[[index]]
      sheets <- if (is.list(file)) file$sheets else NULL
      if (!is.list(sheets) || !length(sheets)) {
        next
      }
      valid <- vapply(
        sheets,
        function(sheet) {
          is.list(sheet) &&
            is.character(sheet$key) &&
            length(sheet$key) == 1L &&
            is.character(sheet$label) &&
            length(sheet$label) == 1L &&
            is.character(sheet$path) &&
            length(sheet$path) == 1L
        },
        logical(1)
      )
      sheets <- sheets[valid]
      if (!length(sheets)) {
        next
      }
      label <- names(files)[[index]]
      if (is.null(label) || is.na(label) || !nzchar(label)) {
        label <- paste("File", index)
      }
      groups[[make.unique(c(names(groups), label))[[
        length(groups) + 1L
      ]]]] <- list(
        key = paste0("external-file:", index),
        label = label,
        sheets = sheets
      )
    }
  }
  groups
}

extra_material_table_group <- function(groups, key) {
  if (!is.list(groups) || !length(groups)) {
    return(NULL)
  }
  matches <- vapply(
    groups,
    function(group) identical(group$key, key),
    logical(1)
  )
  if (any(matches)) groups[[which(matches)[[1L]]]] else groups[[1L]]
}

extra_material_table_sheet <- function(group, key) {
  if (!is.list(group) || !is.list(group$sheets) || !length(group$sheets)) {
    return(NULL)
  }
  matches <- vapply(
    group$sheets,
    function(sheet) identical(sheet$key, key),
    logical(1)
  )
  if (any(matches)) group$sheets[[which(matches)[[1L]]]] else group$sheets[[1L]]
}

extra_material_table_choices <- function(groups) {
  labels <- vapply(groups, `[[`, character(1), "label")
  keys <- vapply(groups, `[[`, character(1), "key")
  duplicated_labels <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  labels[duplicated_labels & keys == "embedded"] <- paste0(
    labels[duplicated_labels & keys == "embedded"],
    " (from CRB)"
  )
  stats::setNames(
    keys,
    make.unique(labels, sep = " — ")
  )
}

extra_material_external_table <- function(group, sheet) {
  if (
    exists(
      sheet$key,
      envir = extra_material_external_table_cache,
      inherits = FALSE
    )
  ) {
    return(get(sheet$key, envir = extra_material_external_table_cache))
  }
  root <- if (exists("Cerebro.options", inherits = TRUE)) {
    get("Cerebro.options", inherits = TRUE)$cerebro_root
  } else {
    "."
  }
  table <- tryCatch(
    readRDS(file.path(root, sheet$path)),
    error = function(error) {
      stop(
        "Unable to read table `",
        sheet$label,
        "` from `",
        group$label,
        "`.",
        call. = FALSE
      )
    }
  )
  if (!is.data.frame(table)) {
    stop("The selected Extra material table is invalid.", call. = FALSE)
  }
  table <- neutralizeSpreadsheetFormulas(table)
  assign(sheet$key, table, envir = extra_material_external_table_cache)
  table
}

extra_material_table_selection <- function(
  groups,
  file_key = NULL,
  sheet_key = NULL,
  load = TRUE
) {
  group <- extra_material_table_group(groups, file_key)
  sheet <- extra_material_table_sheet(group, sheet_key)
  if (is.null(group) || is.null(sheet)) {
    return(NULL)
  }
  if (isTRUE(load) && is.null(sheet$table)) {
    sheet$table <- extra_material_external_table(group, sheet)
  }
  list(group = group, sheet = sheet)
}

getExtraMaterialCategories <- function() {
  categories <- if (is_cerebro_dataset(data_set())) {
    data_set()$getExtraMaterialCategories()
  } else {
    character()
  }
  if (length(extra_material_table_groups())) {
    categories <- union(categories, "tables")
  }
  categories
}
checkForExtraTables <- function() {
  length(extra_material_table_groups()) > 0L
}
checkForExtraPlots <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$checkForExtraPlots())
  }
}
getNamesOfExtraPlots <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getNamesOfExtraPlots())
  }
}
getExtraPlot <- function(name) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getExtraPlot(name))
  }
}

## Wrapper for immune repertoire module.
getImmuneRepertoire <- function() {
  ds <- data_set()
  if (!any(grepl("Cerebro", class(ds)))) {
    return(list())
  }
  tryCatch(ds$getImmuneRepertoire(), error = function(e) list())
}

getImmuneRepertoireSummary <- function() {
  ds <- data_set()
  if (is.null(ds)) {
    return(list(available = FALSE, samples = character(), chains = character()))
  }
  backend <- tryCatch(ds$immune_repertoire_backend, error = function(e) NULL)
  if (is.list(backend) && identical(backend$type, "bpcells-file")) {
    return(list(
      available = TRUE,
      samples = as.character(backend$samples %||% character()),
      chains = as.character(backend$chains %||% character())
    ))
  }
  repertoire <- tryCatch(ds$getImmuneRepertoire(), error = function(e) list())
  list(
    available = is.list(repertoire) && length(repertoire) > 0L,
    samples = names(repertoire) %||% character(),
    chains = character()
  )
}

viewerHasTcrRepertoire <- function(repertoire) {
  if (!is.list(repertoire) || !length(repertoire)) {
    return(FALSE)
  }
  any(vapply(
    repertoire,
    function(sample) {
      if (is.null(sample) || !"CTgene" %in% names(sample)) {
        return(FALSE)
      }
      any(grepl("TR[AB]", as.character(sample$CTgene)), na.rm = TRUE)
    },
    logical(1)
  ))
}

## ---- What one row of this data set is ---------------------------------- ##
## Almost every .crb is single-cell, so a row is a cell and the app says so.
## That is not universal: a bulk repertoire data set maps a (donor, clonotype)
## pair onto a row, and calling those "cells" states a measurement that was
## never made.
##
## This is a DECLARED contract, not an inference. A .crb states it in
## `technical_info$observation_unit` (e.g. "analysis unit"); anything that does
## not declare one is single-cell, which is the safe default for every existing
## file. Deriving it instead (say, from an empty expression matrix) would be a
## proxy: it would quietly relabel any data set that merely lacks expression.
##
## Returns a list(singular, plural, title) so callers do not re-derive plurals.
getObservationUnit <- function() {
  ds <- tryCatch(data_set(), error = function(e) NULL)
  ti <- tryCatch(ds$technical_info, error = function(e) NULL)
  unit <- if (is.list(ti) && is.character(ti$observation_unit)) {
    ti$observation_unit[1]
  } else {
    "cell"
  }
  if (!nzchar(unit) || is.na(unit)) {
    unit <- "cell"
  }
  list(
    singular = unit,
    plural = paste0(unit, "s"),
    title = paste0(
      toupper(substring(unit, 1, 1)),
      substring(unit, 2),
      "s"
    )
  )
}

## Wrapper for the HLA & TCR Motifs module. Older .crb objects predate the
## getHLATyping() method / hla_typing field, so the wrapper checks the method
## exists and falls back to an empty canonical table, never erroring.
getHLATyping <- function() {
  ds <- data_set()
  if (!any(grepl("Cerebro", class(ds)))) {
    return(hla_normalize_typing(list(), source_type = "unknown"))
  }
  empty <- hla_normalize_typing(list(), source_type = "unknown")
  if (!is.function(ds$getHLATyping)) {
    return(empty)
  }
  tryCatch(ds$getHLATyping(), error = function(e) empty)
}

## Wrappers for spatial module.
availableSpatial <- function() {
  ds <- data_set()
  if (!any(grepl("Cerebro", class(ds)))) {
    return(character(0))
  }
  tryCatch(ds$availableSpatial(), error = function(e) character(0))
}
getSpatialData <- function(name) {
  ds <- data_set()
  if (!any(grepl("Cerebro", class(ds)))) {
    return(NULL)
  }
  getter <- ds$getSpatialData
  tryCatch(
    {
      if ("hydrate_molecules" %in% names(formals(getter))) {
        return(getter(name, hydrate_molecules = FALSE))
      }
      stored <- tryCatch(ds$spatial[[name]], error = function(error) NULL)
      if (
        is.list(stored) &&
          inherits(stored[["molecules"]], "CerebroSpatialMoleculeRef")
      ) {
        return(stored)
      }
      getter(name)
    },
    error = function(e) NULL
  )
}

## The first Spatial frame only needs coordinates and embedded-image metadata.
## Read those fields directly from canonical storage so a tab click does not
## normalize or copy molecule payloads and other unrelated Spatial content.
## Legacy or malformed entries retain the validated public accessor fallback.
viewerSpatialFirstFrameData <- function(name) {
  ds <- data_set()
  if (!any(grepl("Cerebro", class(ds)))) {
    return(NULL)
  }
  stored <- tryCatch(ds$spatial[[name]], error = function(error) NULL)
  coordinates <- if (is.list(stored)) stored[["coordinates"]] else NULL
  valid_coordinates <- is.data.frame(coordinates) &&
    all(c("x", "y") %in% colnames(coordinates)) &&
    is.numeric(coordinates[["x"]]) &&
    is.numeric(coordinates[["y"]]) &&
    nrow(coordinates) > 0L
  if (!isTRUE(valid_coordinates)) {
    return(getSpatialData(name))
  }
  list(
    coordinates = coordinates,
    histology_images = stored[["histology_images"]],
    histology_image = stored[["histology_image"]],
    histology_image_bounds = stored[["histology_image_bounds"]]
  )
}

## Coordinate-only Spatial consumers run after spatial_projection_parameters_plot
## has validated the exact current dataset + entry through getSpatialData().
## Reuse the canonical stored coordinate reference there instead of invoking the
## legacy getter's image/bounds normalization for every downstream reactive.
## Malformed or legacy storage keeps the fully validated fallback path.
viewerSpatialCoordinates <- function(name) {
  ds <- data_set()
  if (!any(grepl("Cerebro", class(ds)))) {
    return(NULL)
  }
  coordinates <- tryCatch(
    ds$spatial[[name]][["coordinates"]],
    error = function(error) NULL
  )
  valid <- is.data.frame(coordinates) &&
    all(c("x", "y") %in% colnames(coordinates)) &&
    is.numeric(coordinates[["x"]]) &&
    is.numeric(coordinates[["y"]]) &&
    nrow(coordinates) > 0L
  if (isTRUE(valid)) {
    return(coordinates)
  }
  spatial_data <- getSpatialData(name)
  if (is.null(spatial_data)) NULL else spatial_data[["coordinates"]]
}
canonicalGeneSelection <- function(selected, genes) {
  if (!length(selected)) {
    return(selected)
  }
  index <- match(tolower(selected), tolower(genes))
  found <- !is.na(index)
  selected[found] <- genes[index[found]]
  selected
}

serverSideGeneSelector <- function(
  session,
  input_id,
  extra_triggers = function() NULL,
  active = function() TRUE,
  choices = function() getGeneNames(),
  retry = TRUE,
  canonical_selection = FALSE,
  refresh_on_activation = TRUE
) {
  update_generation <- 0L
  registered_dataset <- NULL
  observe({
    update_generation <<- update_generation + 1L
    generation <- update_generation
    extra_triggers()
    if (!isTRUE(refresh_on_activation)) {
      ## Drop the previous R6 reference even when this selector's tab is hidden.
      if (!identical(data_set(), registered_dataset)) {
        registered_dataset <<- NULL
      }
    }
    ## The caller can gate this observer so it does nothing until its own tab is
    ## relevant. The spatial module registers this at module-source time, which
    ## also runs for datasets that carry no spatial data (e.g. the PBMC set); an
    ## ungated observer would then schedule later::later() callbacks that keep
    ## the app from ever reaching idle and break unrelated tabs' tests.
    req(isTRUE(active()))
    dataset <- data_set()
    req(dataset)
    ## Mounted Gene controls keep their registered search endpoint on return.
    ## Re-sending choices starts an AJAX restore that can overwrite new typing.
    if (
      !isTRUE(refresh_on_activation) &&
        identical(dataset, registered_dataset)
    ) {
      return(invisible(NULL))
    }
    genes <- sort(choices())
    req(!is.null(genes), length(genes) > 0)

    initial_selection <- isolate(input[[input_id]])
    send_update <- function(retry_update = FALSE) {
      ## Timers from a previous dataset/activation can run after a slow load.
      ## They must not re-register obsolete choices over the current selector.
      if (
        generation != update_generation ||
          !identical(isolate(data_set()), dataset)
      ) {
        return(invisible(NULL))
      }
      selected <- isolate(input[[input_id]])
      ## A choices refresh briefly clears the browser control before its AJAX
      ## response restores the selection. Do not register that transient empty
      ## value as the selection for a retry (or overwrite a deliberate clear).
      if (
        retry_update &&
          !identical(selected, initial_selection) &&
          (any(nzchar(selected)) || any(nzchar(initial_selection)))
      ) {
        return(invisible(NULL))
      }
      if (isTRUE(canonical_selection)) {
        selected <- canonicalGeneSelection(selected, genes)
      }
      updateSelectizeInput(
        session,
        input_id,
        choices = genes,
        selected = selected,
        server = TRUE
      )
      if (!isTRUE(refresh_on_activation)) registered_dataset <<- dataset
    }

    ## Dynamic renderUI() + updateSelectizeInput(server=TRUE) race on the
    ## client: the selectize binding initialises asynchronously, so an update
    ## message can arrive while the binding doesn't yet exist and gets silently
    ## dropped. onFlushed fires right after R's flush but before the browser
    ## has processed the DOM update, so it's necessary but not sufficient.
    ## Sending the same update again after small timed delays ensures at least
    ## one lands after the binding exists. Preserve any selection made between
    ## retries so a delayed choices update cannot clear newer browser state.
    if (isTRUE(retry)) {
      session$onFlushed(send_update, once = TRUE)
      later::later(function() send_update(TRUE), delay = 0.3)
      later::later(function() send_update(TRUE), delay = 1.0)
    } else {
      later::later(function() send_update(TRUE), delay = 0.3)
    }
  })
}

##----------------------------------------------------------------------------##
## Filter a projection selection down to cells in still-visible groups.
##
## The custom legend lets the user hide a group (Plotly.restyle on the client);
## the shared JS pushes the currently-hidden group names to Shiny under
## <plot_id>_hidden_groups. Selected cells belonging to a hidden group should
## stop counting, so the count and the selected-cells panels reflect only what
## is visible. Shared across the projection tabs (overview / spatial /
## trajectory); each tab builds the identifier->group `metadata` from its own
## coordinate source and passes it in. Pure data transform, no Shiny state.
##
## selection: data.frame of selected cells with an `identifier` column, or NULL.
## metadata:  data.frame with the same `identifier` column plus grouping columns.
## color_variable: name of the column the legend groups by (current "Colour by").
## hidden_groups: character vector of group names currently hidden (may be NULL).
##
## Returns the selection with hidden-group cells removed. NULL stays NULL; an
## empty / absent hidden set, or a color_variable not in the metadata, returns
## the selection unchanged.
##----------------------------------------------------------------------------##
filterSelectionByHiddenGroups <- function(
  selection,
  metadata,
  color_variable,
  hidden_groups
) {
  if (is.null(selection)) {
    return(NULL)
  }
  if (length(hidden_groups) == 0) {
    return(selection)
  }
  stable_key_matches <-
    "selection_key" %in%
    colnames(metadata) &&
    "selection_key" %in% colnames(selection) &&
    any(selection[["selection_key"]] %in% metadata[["selection_key"]])
  key <- if (stable_key_matches) {
    "selection_key"
  } else {
    "identifier"
  }
  if (
    is.null(color_variable) ||
      !color_variable %in% colnames(metadata) ||
      !key %in% colnames(metadata) ||
      !key %in% colnames(selection)
  ) {
    return(selection)
  }

  ## Map each selected identifier to its group, then keep only the cells whose
  ## group is not hidden. match() on identifier avoids a join dependency and
  ## keeps selection row order intact.
  group_by_identifier <- metadata[[color_variable]][
    match(selection[[key]], metadata[[key]])
  ]
  keep <- !(group_by_identifier %in% hidden_groups)
  selection[keep, , drop = FALSE]
}

viewerPersistentSelection <- function(selection, dataset_context) {
  if (
    !is.null(selection) &&
      (!is.list(selection) ||
        !viewerDatasetContextEqual(
          selection[["dataset_context"]],
          dataset_context
        ))
  ) {
    return(NULL)
  }
  if (is.null(selection) || !length(selection[["x"]])) {
    return(NULL)
  }
  x <- as.numeric(selection[["x"]])
  y <- as.numeric(selection[["y"]])
  result <- data.frame(
    x = x,
    y = y,
    identifier = paste0(x, "-", y),
    stringsAsFactors = FALSE
  )
  if (length(selection[["ids"]]) == nrow(result)) {
    result[["selection_key"]] <- as.character(selection[["ids"]])
  }
  result
}

## Prepare only the compared variable and cohort labels for distribution plots.
prepareSelectedCellDistribution <- function(
  coordinates,
  metadata,
  selection,
  color_variable,
  coordinate_keys = FALSE
) {
  cells_df <- cbind(coordinates, metadata)
  if (is.null(selection)) {
    cells_df[["group"]] <- rep("not selected", nrow(cells_df))
  } else {
    cells_df <- dplyr::rename(cells_df, X1 = 1, X2 = 2)
    identifier <- function() paste0(cells_df[["X1"]], "-", cells_df[["X2"]])
    selection_key <- if ("cell_barcode" %in% names(cells_df)) {
      as.character(cells_df[["cell_barcode"]])
    } else if (coordinate_keys) {
      identifier()
    } else {
      as.character(seq_len(nrow(cells_df)))
    }
    cells_df[["group"]] <- factor(
      ifelse(
        selectedCellMask(selection_key, cells_df[c("X1", "X2")], selection),
        "selected",
        "not selected"
      ),
      levels = c("selected", "not selected")
    )
    if (identical(color_variable, "identifier")) {
      cells_df[[color_variable]] <- identifier()
    }
    if (identical(color_variable, "selection_key")) {
      cells_df[[color_variable]] <- selection_key
    }
  }
  cells_df[, unique(c("group", color_variable)), drop = FALSE]
}

selectedCellMask <- function(selection_key, identifier, selection) {
  coordinate_frame <- is.data.frame(identifier)
  if (coordinate_frame) {
    if (ncol(identifier) != 2L) {
      stop("Selection coordinates must have two columns.", call. = FALSE)
    }
    if (length(selection_key) != nrow(identifier)) {
      stop("Selection identity vectors must have equal length.", call. = FALSE)
    }
  }
  if (is.null(selection) || !is.data.frame(selection)) {
    return(rep(FALSE, length(selection_key)))
  }
  stable <- if ("selection_key" %in% colnames(selection)) {
    as.character(selection[["selection_key"]])
  } else {
    character()
  }
  stable <- stable[!is.na(stable) & nzchar(stable)]
  if (length(stable)) {
    return(as.character(selection_key) %in% stable)
  }
  if (is.null(identifier)) {
    return(rep(FALSE, length(selection_key)))
  }
  if (coordinate_frame) {
    identifier <- paste0(identifier[[1L]], "-", identifier[[2L]])
  }
  if (length(selection_key) != length(identifier)) {
    stop("Selection identity vectors must have equal length.", call. = FALSE)
  }
  coordinates <- if ("identifier" %in% colnames(selection)) {
    as.character(selection[["identifier"]])
  } else {
    character()
  }
  as.character(identifier) %in% coordinates
}

cerebroRegisterInfo <- function(input, input_id, info) {
  shiny::observeEvent(input[[input_id]], {
    shiny::showModal(shiny::modalDialog(
      info$text,
      title = info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
}

cerebroSelectedCellsTableInfo <- function() {
  list(
    title = "Details of selected cells",
    text = shiny::HTML(
      "
      Table containing meta data (some columns may be hidden, check the 'Column visibility' button) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.
      <h4>Options</h4>
      <b>Automatically format numbers</b><br>
      When activated, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
      <em>This feature does not work on columns that contain 'NA' values.</em><br>
      <b>Highlight values with colours</b><br>
      Similar to the automatic formatting option, when activated, CerebroNexus will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colours and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colours and/or the identification does not work properly, you can simply deactivate this feature.<br>
      <em>This feature does not work on columns that contain 'NA' values.</em><br>
      <br>
      <em>Columns can be re-ordered by dragging their respective header.</em>"
    )
  )
}

cerebroSelectedCellsPlotInfo <- function() {
  list(
    title = "Plot of selected cells",
    text = shiny::p(
      "Depending on the variable selected to colour cells in the dimensional reduction, this plot will show different things. If you select a categorical variable, e.g. 'sample' or 'cluster', you will get a bar plot showing which groups the cells selected with the box or lasso tool come from. Instead, if you select a continuous variable, e.g. the number of transcripts (nUMI), you will see a violin/box plot showing the distribution of that variable in the selected vs. non-selected cells."
    )
  )
}

cerebroSelectedCellsTableUI <- function(prefix) {
  shiny::fluidRow(cerebroBox(
    title = shiny::tagList(
      boxTitle("Table of selected cells"),
      cerebroInfoButton(paste0(prefix, "_details_selected_cells_table_info"))
    ),
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = paste0(
          prefix,
          "_details_selected_cells_table_number_formatting"
        ),
        label = "Automatically format numbers:",
        value = TRUE,
        status = "primary",
        inline = TRUE
      ),
      shinyWidgets::materialSwitch(
        inputId = paste0(
          prefix,
          "_details_selected_cells_table_color_highlighting"
        ),
        label = "Highlight values with colours:",
        value = TRUE,
        status = "primary",
        inline = TRUE
      ),
      DT::dataTableOutput(paste0(prefix, "_details_selected_cells_table"))
    )
  ))
}

cerebroSelectedCellsPlotUI <- function(prefix, choices) {
  shiny::fluidRow(cerebroBox(
    title = shiny::tagList(
      boxTitle("Plot of selected cells"),
      cerebroInfoButton(paste0(prefix, "_details_selected_cells_plot_info"))
    ),
    shiny::tagList(
      shiny::selectInput(
        paste0(prefix, "_selected_cells_plot_select_variable"),
        label = "Variable to compare:",
        choices = choices
      ),
      plotly::plotlyOutput(paste0(prefix, "_details_selected_cells_plot"))
    )
  ))
}

cerebroSelectedCellsTable <- function(
  cells_df,
  empty_data,
  selection,
  fallback_key = c("row", "identifier"),
  number_formatting = TRUE,
  color_highlighting = TRUE,
  download_file_name
) {
  empty_table <- function() {
    prepareEmptyTable(empty_data[0, , drop = FALSE])
  }
  if (is.null(selection)) {
    return(empty_table())
  }

  fallback_key <- match.arg(fallback_key)
  selection_key <- if ("cell_barcode" %in% colnames(cells_df)) {
    as.character(cells_df[["cell_barcode"]])
  } else {
    as.character(seq_len(nrow(cells_df)))
  }
  stable_selection <- is.data.frame(selection) &&
    "selection_key" %in% colnames(selection) &&
    any(
      !is.na(selection[["selection_key"]]) &
        nzchar(as.character(selection[["selection_key"]]))
    )
  identifier <- if (!stable_selection) {
    paste0(cells_df[[1L]], "-", cells_df[[2L]])
  } else {
    NULL
  }
  if (!stable_selection && identical(fallback_key, "identifier")) {
    selection_key <- identifier
  }
  keep <- selectedCellMask(selection_key, identifier, selection)
  metadata_columns <- setdiff(seq_len(ncol(cells_df)), 1:2)
  cells_df <- cells_df[keep, metadata_columns, drop = FALSE]
  if (!nrow(cells_df)) {
    return(empty_table())
  }
  if ("cell_barcode" %in% colnames(cells_df)) {
    cells_df <- cells_df[,
      c(
        "cell_barcode",
        setdiff(colnames(cells_df), "cell_barcode")
      ),
      drop = FALSE
    ]
  }
  prettifyTable(
    cells_df,
    filter = list(position = "top", clear = TRUE),
    dom = "Brtlip",
    show_buttons = TRUE,
    number_formatting = number_formatting,
    color_highlighting = color_highlighting,
    hide_long_columns = TRUE,
    download_file_name = download_file_name
  )
}

cerebroSelectedCellsPlot <- function(
  cells_df,
  selection,
  color_variable,
  fallback_key = c("row", "identifier")
) {
  fallback_key <- match.arg(fallback_key)
  cells_df <- prepareSelectedCellDistribution(
    cells_df[, 1:2, drop = FALSE],
    cells_df[, -(1:2), drop = FALSE],
    selection,
    color_variable,
    coordinate_keys = identical(fallback_key, "identifier")
  )

  if (
    is.factor(cells_df[[color_variable]]) ||
      is.character(cells_df[[color_variable]])
  ) {
    cells_df <- cells_df[cells_df[["group"]] == "selected", , drop = FALSE]
    if (nrow(cells_df)) {
      cells_df <- cells_df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(color_variable))) %>%
        dplyr::tally() %>%
        dplyr::ungroup()
    } else {
      group_levels <- if (color_variable %in% getGroups()) {
        getGroupLevels(color_variable)
      } else {
        unique(getMetaData()[[color_variable]])
      }
      cells_df <- data.frame(group = group_levels, n = 0) %>%
        dplyr::rename(!!color_variable := group)
    }
    colors_for_groups <- assignColorsToGroups(cells_df, color_variable)
    cells_df[[1L]] <- as.character(cells_df[[1L]])
    plot <- plotly::plot_ly(
      cells_df,
      x = ~ cells_df[[1L]],
      y = ~ cells_df[[2L]],
      type = "bar",
      color = ~ cells_df[[1L]],
      colors = colors_for_groups,
      source = "subset",
      showlegend = FALSE,
      hoverinfo = "y"
    )
    y_axis_title <- "Number of cells"
  } else {
    cells_df <- cells_df[, c("group", color_variable), drop = FALSE]
    cells_df <- compactViolinData(cells_df, color_variable, "group")
    plot <- plotly::plot_ly(
      cells_df,
      x = ~ cells_df[["group"]],
      y = ~ cells_df[[color_variable]],
      type = "violin",
      box = list(visible = FALSE),
      meanline = list(visible = FALSE),
      color = ~ cells_df[[1L]],
      colors = stats::setNames(
        c("#e74c3c", "#7f8c8d"),
        c("selected", "not selected")
      ),
      source = "subset",
      showlegend = FALSE,
      hoverinfo = "y",
      marker = list(size = 5)
    )
    plot <- addExactViolinSummary(plot, cells_df)
    y_axis_title <- colnames(cells_df)[[2L]]
  }

  plot %>%
    plotly::layout(
      title = "",
      xaxis = list(title = "", mirror = TRUE, showline = TRUE),
      yaxis = list(
        title = y_axis_title,
        tickformat = ",.0f",
        hoverformat = ",.0f",
        mirror = TRUE,
        showline = TRUE
      ),
      dragmode = "lasso",
      hovermode = "compare"
    ) %>%
    cerebro_plotly_toolbar()
}

cerebroProjectionInfo <- function() {
  list(
    title = "Dimensional reduction",
    text = shiny::HTML(
      "
      Interactive projection of cells into 2-dimensional space based on their expression profile.
      <ul>
        <li>Both tSNE and UMAP are frequently used algorithms for dimensional reduction in single cell transcriptomics. While they generally allow to make similar conclusions, some differences exist between the two (please refer to Google and/or literature, such as Becht E. et al., Dimensionality reduction for visualizing single-cell data using UMAP. Nature Biotechnology, 2018, 37, 38-44).</li>
        <li>Cells can be coloured by the sample they came from, the cluster they were assigned, the number of transcripts or expressed genes, percentage of mitochondrial and ribosomal gene expression, an apoptotic score (calculated based on the expression of few marker genes; more info in the 'Sample info' tab on the left), or cell cycle status (determined using the Seurat and Cyclone method).</li>
        <li>Confidence ellipses show the 95% confidence regions.</li>
        <li>Samples and clusters can be removed from the plot individually to highlight a contrast of interest.</li>
        <li>Point size, point opacity, and the percentage of displayed cells can be changed in Settings.</li>
      </ul>
      The plot is interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become very slow."
    )
  )
}

registerCellViewPointBorder <- function(output, prefix) {
  output_id <- paste0(prefix, "_point_border_UI")
  output[[output_id]] <- shiny::renderUI({
    shiny::checkboxInput(
      inputId = paste0(prefix, "_point_border"),
      label = "Draw border around cells",
      value = FALSE
    )
  })
  shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
}

cerebroResultTableControls <- function(prefix, results_df, grouped) {
  shiny::fluidRow(
    shiny::column(
      12,
      shinyWidgets::materialSwitch(
        inputId = paste0(prefix, "_table_filter_switch"),
        label = "Show results for all subgroups (no pre-filtering):",
        value = FALSE,
        status = "primary",
        inline = TRUE
      ),
      shinyWidgets::materialSwitch(
        inputId = paste0(prefix, "_table_number_formatting"),
        label = "Automatically format numbers:",
        value = TRUE,
        status = "primary",
        inline = TRUE
      ),
      shinyWidgets::materialSwitch(
        inputId = paste0(prefix, "_table_color_highlighting"),
        label = "Highlight values with colours:",
        value = TRUE,
        status = "primary",
        inline = TRUE
      )
    ),
    shiny::column(
      12,
      shiny::conditionalPanel(
        paste0("!input.", prefix, "_table_filter_switch"),
        cerebroResultSubgroupUI(
          results_df,
          FALSE,
          grouped,
          paste0(prefix, "_table_select_group_level")
        )
      )
    ),
    shiny::column(12, DT::dataTableOutput(paste0(prefix, "_table")))
  )
}

cerebroResultSubgroupUI <- function(results_df, show_all, grouped, input_id) {
  if (isTRUE(show_all) || !isTRUE(grouped)) {
    return(shiny::fluidRow())
  }
  available_groups <- if (is.factor(results_df[[1L]])) {
    levels(results_df[[1L]])
  } else {
    unique(as.character(results_df[[1L]]))
  }
  shiny::fluidRow(shiny::column(
    12,
    shiny::selectInput(
      input_id,
      label = "Filter results for subgroup:",
      choices = available_groups
    )
  ))
}

cerebroFilterResultRows <- function(results_df, show_all, grouped, selected) {
  if (isTRUE(show_all) || !isTRUE(grouped)) {
    return(results_df)
  }
  results_df[results_df[[1L]] == selected, , drop = FALSE]
}

cerebroResultTable <- function(
  results_df,
  download_file_name,
  number_formatting = TRUE,
  color_highlighting = TRUE,
  hide_long_columns = TRUE,
  columns_hide = NULL,
  columns_percentage = NULL
) {
  if (!nrow(results_df)) {
    return(prepareEmptyTable(as.data.frame(results_df)[0, , drop = FALSE]))
  }
  prettifyTable(
    results_df,
    filter = list(position = "top", clear = TRUE),
    dom = "Bfrtlip",
    show_buttons = TRUE,
    number_formatting = number_formatting,
    color_highlighting = color_highlighting,
    hide_long_columns = hide_long_columns,
    columns_hide = columns_hide,
    columns_percentage = columns_percentage,
    download_file_name = download_file_name,
    page_length_default = 20,
    page_length_menu = c(20, 50, 100)
  )
}

##----------------------------------------------------------------------------##
## Is the selected trajectory method/name valid for the CURRENT dataset?
##
## On a dataset switch the Shiny inputs trajectory_selected_method /
## trajectory_selected_name keep their previous values until the selectors
## round-trip. A bare req() on those strings passes even when the new dataset has
## no such method, so getTrajectory() throws "Method `X` is not available." This
## predicate is req()-ed at every getTrajectory() call site so the output bails
## out cleanly instead of erroring while the stale value lingers.
##
## method / name: the currently selected method and trajectory name (may be NULL).
## available_methods: methods present in the current dataset
##   (getMethodsForTrajectories()).
## names_for_method: trajectory names for `method` in the current dataset
##   (getNamesOfTrajectories(method)); pass character(0) when method is absent.
##----------------------------------------------------------------------------##
viewerSupportedTrajectoryMethods <- function(available_methods) {
  available_methods <- unique(as.character(available_methods))
  available_methods[
    !is.na(available_methods) &
      nzchar(available_methods) &
      available_methods %in%
        c(
          "monocle2",
          "marker_guided",
          "illustrative"
        )
  ]
}

trajectorySelectionValid <- function(
  method,
  name,
  available_methods,
  names_for_method
) {
  if (is.null(method) || is.null(name)) {
    return(FALSE)
  }
  if (length(method) != 1 || length(name) != 1 || method == "" || name == "") {
    return(FALSE)
  }
  if (!method %in% available_methods) {
    return(FALSE)
  }
  name %in% names_for_method
}
