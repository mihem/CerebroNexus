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
      !.runtimeIsString(datapath) ||
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

## NULL means there is no catalog entry (an upload) or it does not describe its
## projections yet. Never interpret an incomplete catalog or the first
## progressive frame as absence.
viewerLinkedViewsAvailable <- function(info = NULL, object = NULL) {
  if (!is.null(object)) {
    read <- function(value) tryCatch(force(value), error = function(e) NULL)
    ## A thin CRB the Viewer has not hydrated lists its projections in the
    ## first-frame cache; the accessor would first read every cell name.
    cached <- viewerProjectionFirstFrameCache(object)$projections
    projections <- as.character(
      if (is.null(cached)) {
        read(object$availableProjections())
      } else {
        names(cached)
      }
    )
    projections <- unique(projections[
      !is.na(projections) & nzchar(projections)
    ])
    if (length(projections) > 1L) {
      return(TRUE)
    }
    if (length(read(object$availableSpatial())) > 0L) {
      return(TRUE)
    }
    if (
      any(vapply(
        as.character(read(object$getMethodsForTrajectories())),
        function(method) {
          length(read(object$getNamesOfTrajectories(method))) > 0L
        },
        logical(1)
      ))
    ) {
      return(TRUE)
    }
    if (length(read(object$getTrekker())) > 0L) {
      return(TRUE)
    }
    return(isTRUE(read(viewerImmuneRepertoireSummary(object))$available))
  }
  if (!is.list(info)) {
    return(NULL)
  }
  extra <- list(
    spatial = info$capabilities$spatial,
    trajectory = info$capabilities$trajectory,
    trekker = info$capabilities$trekker,
    immune_repertoire = info$immune_repertoire
  )
  if (any(vapply(extra, isTRUE, logical(1)))) {
    return(TRUE)
  }
  projections <- info$projection_names
  if (!is.character(projections)) {
    return(NULL)
  }
  projections <- unique(projections[!is.na(projections) & nzchar(projections)])
  if (length(projections) > 1L) {
    return(TRUE)
  }
  if (!all(vapply(extra, function(x) identical(x, FALSE), logical(1)))) {
    return(NULL)
  }
  FALSE
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
  generation <- suppressWarnings(as.numeric(generation))
  if (
    !.runtimeIsString(epoch) ||
      !.runtimeIsString(dataset_key) ||
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

## Presentation only: stable dataset ids continue to own switching and caches.
viewerDatasetLabel <- function(files, selected, display_labels = NULL) {
  if (is.null(names(files)) || length(selected) != 1L || is.na(selected)) {
    return("Dataset")
  }
  choices <- viewerDatasetChoices(files, display_labels)
  index <- match(selected, unname(choices))
  if (is.na(index)) {
    return("Dataset")
  }
  label <- names(choices)[[index]]
  if (is.na(label) || !nzchar(trimws(label))) "Dataset" else label
}

viewerSelectedDatasetName <- function() {
  current <- tryCatch(
    viewer_current_dataset_key(),
    error = function(error) NULL
  )
  if (is.character(current) && length(current) == 1L && nzchar(current)) {
    return(current)
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
      !.runtimeIsString(selected) ||
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
  is.null(info) || viewerDatasetLoadOnTab(FALSE, tab)
}

## Tabs that never read the selected dataset. Opening one keeps the CRB
## unloaded until an analysis tab asks for it.
viewer_dataset_free_tabs <- c("loadData", "about", "geneIdConversion")

viewerDatasetLoadOnTab <- function(current, tab) {
  isTRUE(current) ||
    (.runtimeIsString(tab) &&
      !tab %in% viewer_dataset_free_tabs)
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
    if (!is.null(dataset) && !is.null(names(value))) {
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
  # Ordinary projection views share one author configuration.
  if (!identical(page, "spatial")) {
    return(defaults)
  }
  list(
    point_size = resolve("spatial_point_size", 5, 1, 20),
    point_opacity = resolve("spatial_point_opacity", 1, 0.1, 1),
    percentage_cells_to_show = resolve(
      "spatial_percentage_cells_to_show",
      100,
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
    opacity = 1
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

spatialEmbeddedImagePreset <- function(preset, alignment) {
  if (!is.list(alignment)) {
    return(preset)
  }
  number <- function(key, fallback) {
    value <- suppressWarnings(as.numeric(alignment[[key]]))
    if (length(value) != 1L || is.na(value) || !is.finite(value)) {
      fallback
    } else {
      unname(value)
    }
  }
  preset$offsetX <- number("dx", preset$offsetX)
  preset$offsetY <- number("dy", preset$offsetY)
  embedded_scale <- number("scale", preset$scaleX)
  preset$scaleX <- embedded_scale
  preset$scaleY <- embedded_scale
  preset$rotation <- number("rotation", preset$rotation)
  if (!is.null(alignment[["flip_x"]])) {
    preset$flipX <- isTRUE(alignment[["flip_x"]])
  }
  if (!is.null(alignment[["flip_y"]])) {
    preset$flipY <- isTRUE(alignment[["flip_y"]])
  }
  opacity <- number("image_opacity", preset$opacity)
  if (opacity >= 0 && opacity <= 1) {
    preset$opacity <- opacity
  }
  # Existing embedded CRBs store transformed pixels and their final bounds.
  # Retain master's relative-edit drawing contract to avoid applying the saved
  # calibration twice; external images use spatialImagePreset() instead.
  preset$geometryBaked <- TRUE
  preset
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

spatialPointAppearance <- function(options, dataset, spatial_name) {
  setting <- if (
    is.list(options) &&
      .runtimeIsString(dataset) &&
      .runtimeIsString(spatial_name)
  ) {
    options[["viewer_content"]][[dataset]][["spatial_point_appearance"]][[
      spatial_name
    ]]
  } else {
    NULL
  }
  viewerSpatialPointSetting(setting)
}

spatialRoiSettings <- function(options, dataset, spatial_name) {
  settings <- if (
    is.list(options) &&
      .runtimeIsString(dataset) &&
      .runtimeIsString(spatial_name)
  ) {
    options[["viewer_content"]][[dataset]][["spatial_roi_settings"]][[
      spatial_name
    ]]
  } else {
    NULL
  }
  if (is.list(settings) && !is.object(settings)) settings else list()
}

spatialRoiSetting <- function(options, dataset, spatial_name, roi) {
  if (!.runtimeIsString(roi)) {
    return(NULL)
  }
  setting <- spatialRoiSettings(options, dataset, spatial_name)[[roi]]
  viewerSpatialPointSetting(setting, roi = TRUE)
}

rotateSpatialCoordinates <- function(coordinates, degrees, pivot = c(0, 0)) {
  if (is.null(coordinates) || identical(degrees, 0)) {
    return(coordinates)
  }
  theta <- degrees * pi / 180
  x <- coordinates[, 1] - pivot[[1L]]
  y <- coordinates[, 2] - pivot[[2L]]
  coordinates[, 1] <- x * cos(theta) - y * sin(theta) + pivot[[1L]]
  coordinates[, 2] <- x * sin(theta) + y * cos(theta) + pivot[[2L]]
  coordinates
}

spatialRoiPivots <- function(coordinates, roi_values, degrees = 0) {
  coordinates <- rotateSpatialCoordinates(coordinates, degrees)
  if (is.null(coordinates) || length(roi_values) != nrow(coordinates)) {
    return(list())
  }
  rois <- unique(as.character(roi_values))
  rois <- rois[!is.na(rois) & nzchar(rois)]
  pivots <- lapply(rois, function(roi) {
    selected <- !is.na(roi_values) & as.character(roi_values) == roi
    ## Index columns explicitly: vapply over a matrix iterates its cells.
    block <- coordinates[selected, 1:2, drop = FALSE]
    pivot <- vapply(
      1:2,
      function(column) mean(range(block[, column], na.rm = TRUE)),
      numeric(1)
    )
    names(pivot) <- colnames(block)
    if (any(!is.finite(pivot))) NULL else pivot
  })
  valid <- !vapply(pivots, is.null, logical(1))
  stats::setNames(pivots[valid], rois[valid])
}

rotateSpatialCoordinatesByRoi <- function(
  coordinates,
  roi_values,
  settings,
  degrees = 0,
  pivots = NULL
) {
  coordinates <- rotateSpatialCoordinates(coordinates, degrees)
  if (
    is.null(coordinates) ||
      !is.list(settings) ||
      !length(settings) ||
      length(roi_values) != nrow(coordinates)
  ) {
    return(coordinates)
  }
  if (is.null(pivots)) {
    pivots <- spatialRoiPivots(coordinates, roi_values)
  }
  if (!is.list(pivots)) {
    pivots <- list()
  }
  for (roi in intersect(unique(as.character(roi_values)), names(settings))) {
    rotation <- suppressWarnings(as.numeric(
      settings[[roi]][["rotation_degrees"]]
    ))
    if (length(rotation) != 1L || is.na(rotation) || !is.finite(rotation)) {
      next
    }
    selected <- !is.na(roi_values) & as.character(roi_values) == roi
    roi_coordinates <- coordinates[selected, , drop = FALSE]
    pivot <- pivots[[roi]]
    if (
      !is.numeric(pivot) ||
        length(pivot) != 2L ||
        anyNA(pivot) ||
        any(!is.finite(pivot))
    ) {
      next
    }
    coordinates[selected, ] <- rotateSpatialCoordinates(
      roi_coordinates,
      rotation,
      pivot
    )
  }
  coordinates
}

##----------------------------------------------------------------------------##
## bindCache wrapper for plot/reactive outputs.
##
## Mirrors the immune_repertoire module's ir_bindCache():
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
##
## shiny::debounce() evaluates its source eagerly on every invalidation, so a
## page-owned `reactive` should req() its page itself. Pass the same predicate
## as `active`: returning to the page then reads the source immediately instead
## of first replaying the value held from before the page was left.
debounceAfterFirst <- function(reactive, millis, active = NULL) {
  initialized <- FALSE
  current <- NULL
  revision <- shiny::reactiveVal(0L)
  was_inactive <- FALSE
  if (is.function(active)) {
    shiny::observe({
      if (!isTRUE(active())) was_inactive <<- TRUE
    })
  }
  shiny::reactive({
    revision()
    if (is.function(active)) {
      shiny::req(isTRUE(active()))
      if (initialized && was_inactive) {
        current <<- reactive()
      }
      was_inactive <<- FALSE
    }
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

## Single-entry memo for values recomputed with identical inputs within and
## across flushes. Put any data_set() reference in `key`.
viewerMemoOne <- function() {
  state <- new.env(parent = emptyenv())
  function(key, compute) {
    if (isTRUE(state$set) && identical(state$key, key)) {
      return(state$value)
    }
    value <- compute()
    state$key <- key
    state$value <- value
    state$set <- TRUE
    value
  }
}

## Write a reactiveVal only when its value changes, so dependants do not re-run
## for an identical value.
viewerPublishChanged <- function(state, value) {
  if (!identical(shiny::isolate(state()), value)) {
    state(value)
  }
  invisible(value)
}

## A renderUI control's value as page code should read it: the bound input, or,
## until the control has bound, `default()`, the value it will bind with. A
## first frame then need not wait for the controls to reach the browser and
## back, and the binding of an equal value invalidates nothing. Bound inputs
## keep their value even when it is NULL (a cleared multi-select), so presence
## in `input` separates "not yet bound" from "none".
viewerControlValue <- function(input, id, default) {
  value <- shiny::reactiveVal(NULL)
  shiny::observe(
    {
      current <- input[[id]]
      if (is.null(current) && !id %in% shiny::isolate(names(input))) {
        current <- default()
      }
      viewerPublishChanged(value, current)
    },
    priority = 1000
  )
  value
}

## A slider bounded by one preference's min/max/step limits.
viewerLimitedSlider <- function(
  inputId,
  label,
  limits,
  value,
  min = limits[["min"]]
) {
  sliderInput(
    inputId,
    label = label,
    min = min,
    max = limits[["max"]],
    step = limits[["step"]],
    value = value
  )
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
    ## A snapshot, not a dependency: the browser republishes the shared
    ## geometry after every paint, and a render observer subscribed to it
    ## would draw its frame a second time.
    shared <- shiny::isolate(input[["coordviews_shared_base"]])
  }
  if (is.null(identity)) {
    identity <- viewerDatasetIdentity()
  }
  if (
    !is.list(shared) ||
      !viewerDatasetContextEqual(shared$dataset_context, dataset_context) ||
      !is.list(identity) ||
      !.runtimeIsString(projection) ||
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

## The canonical metadata row of each row of a trajectory, in the trajectory's
## order: from the Viewer Pack index, the Pack cell order, or the cell names.
viewerTrajectoryCellIndex <- function(trajectory, method, name) {
  trajectory_cells <- rownames(trajectory[["meta"]])
  pack <- viewerPackCurrent()
  pack_cell_count <- suppressWarnings(as.integer(pack$cell_count))
  valid_pack_count <- length(pack_cell_count) == 1L &&
    !is.na(pack_cell_count)
  index <- viewerPackTrajectoryIndex(pack, method, name)
  valid_index <- length(index) == nrow(trajectory[["meta"]]) &&
    !anyNA(index) &&
    isTRUE(all(index >= 1L))
  if (!isTRUE(valid_index)) {
    index <- NULL
  }
  pack_order_matches <-
    is.null(index) &&
    !is.null(pack) &&
    is.list(pack) &&
    valid_pack_count &&
    length(trajectory_cells) == pack_cell_count &&
    identical(
      viewerPackCellOrderFingerprint(trajectory_cells),
      as.character(pack$manifest$cell_order_fingerprint)
    )
  if (isTRUE(pack_order_matches)) {
    index <- seq_len(pack_cell_count)
  }
  if (is.null(index)) {
    metadata <- getMetaData()
    index <- match(trajectory_cells, metadata[["cell_barcode"]])
  }
  if (anyNA(index)) {
    stop("Trajectory cells do not match the canonical dataset order.")
  }
  as.integer(index)
}

## The group filters of `prefix` that leave cells out: group -> allowed levels.
viewerProjectionGroupFilters <- function(prefix, metadata = getMetaData()) {
  groups <- getGroups()
  filter_ids <- paste0(prefix, "_group_filter_", groups)
  bound_inputs <- shiny::isolate(names(input))
  filter_inputs <- stats::setNames(
    lapply(groups, function(group) {
      input[[paste0(prefix, "_group_filter_", group)]]
    }),
    groups
  )
  if (!length(groups) || !any(filter_ids %in% bound_inputs)) {
    return(list())
  }
  filters <- stats::setNames(
    lapply(groups, function(group) {
      value <- filter_inputs[[group]]
      ## Shiny decodes a checkbox group's empty array as NULL. Only an input
      ## that has not been bound yet means "all"; a bound NULL means None.
      if (is.null(value)) {
        if (paste0(prefix, "_group_filter_", group) %in% bound_inputs) {
          character()
        } else {
          getGroupLevels(group)
        }
      } else {
        value
      }
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
  filters
}

## Apply the shared projection filters and sample original metadata row ids.
viewerProjectionCellIndices <- function(
  prefix,
  metadata = getMetaData(),
  percentage = input[[paste0(prefix, "_percentage_cells_to_show")]],
  filters = viewerProjectionGroupFilters(prefix, metadata)
) {
  ## A `percentage` random sample of the given cells.
  sample_of <- function(indices) {
    if (!length(indices) || percentage >= 100) {
      return(indices)
    }
    size <- ceiling(length(indices) * percentage / 100)
    indices[sample.int(length(indices), size)]
  }
  if (!length(filters)) {
    return(sample_of(seq_len(nrow(metadata))))
  }
  sample_of(which(cerebroGroupFilterMask(metadata, filters)))
}

## The group filters as the browser applies them to a frame of every cell:
## each group's Viewer Pack codes and the indices of its allowed levels. NULL
## when a filtered group has no Pack codes; the server then filters the frame.
viewerClientFilterDescriptors <- function(filters) {
  descriptors <- lapply(names(filters), function(group) {
    levels <- as.character(getGroupLevels(group))
    resource <- viewerMetadataCodesAsset(group, levels)
    if (is.null(resource)) {
      return(NULL)
    }
    allowed <- match(as.character(filters[[group]]), levels) - 1L
    list(
      name = group,
      levels = I(levels),
      allowed = I(allowed[!is.na(allowed)]),
      resource = resource
    )
  })
  if (any(vapply(descriptors, is.null, logical(1)))) {
    return(NULL)
  }
  descriptors
}

## Tell the browser which group filters to apply to the drawn frame of `id`.
viewerSendClientFilters <- function(id, descriptors, dataset_context) {
  session$sendCustomMessage(
    "cell_view_filters",
    list(
      id = id,
      dataset_context = dataset_context,
      filters = unname(descriptors %||% list())
    )
  )
}

## The group `filters()` of projection `id` for the browser to apply to a frame
## of every cell, sent to it while `active()`. NULL, R filters the frame, unless
## `client()` and the browser has not failed to apply them.
viewerClientFilters <- function(id, active, client, filters) {
  client_filters <- shiny::reactive({
    if (
      !isTRUE(client()) ||
        isTRUE(input[[paste0(id, "_client_filters_failed")]])
    ) {
      return(NULL)
    }
    viewerClientFilterDescriptors(filters())
  })
  shiny::observe({
    shiny::req(active())
    viewerSendClientFilters(
      id,
      client_filters(),
      viewer_loaded_dataset_context()
    )
  })
  client_filters
}

## The group filters of projection `id` on sidebar `page`, and the canonical
## rows it draws: `rows(filters)`, given the group filters left to R (none when
## `client()` lets the browser apply them). The rows are published on change
## only, so a filter control binding with the value it had does not rebuild a
## million-cell frame; off the page the last rows are kept. With `wait`, rows
## that fail publish NULL and nothing is drawn until rows are published.
viewerProjectionCells <- function(
  id,
  page,
  client,
  rows,
  priority = 950,
  wait = FALSE
) {
  group_filters <- shiny::reactive({
    viewerProjectionGroupFilters(id, viewerProjectionFirstFrameMetadata())
  })
  client_filters <- viewerClientFilters(
    id,
    function() identical(input[["sidebar"]], page),
    client,
    group_filters
  )
  current <- shiny::reactive({
    rows(if (is.null(client_filters())) group_filters() else list())
  })
  state <- shiny::reactiveVal(NULL)
  shiny::observe(
    {
      if (
        "sidebar" %in%
          shiny::isolate(names(input)) &&
          !identical(input[["sidebar"]], page)
      ) {
        return()
      }
      value <- if (wait) {
        tryCatch(current(), error = function(error) NULL)
      } else {
        current()
      }
      viewerPublishChanged(state, value)
    },
    priority = priority
  )
  list(
    group_filters = group_filters,
    client_filters = client_filters,
    cells_to_show = shiny::reactive({
      if (wait) {
        value <- state()
        shiny::req(!is.null(value))
        value
      } else {
        state() %||% current()
      }
    })
  )
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
## 1 x cells matrix.
viewerExpressionCells <- function(data_set, cells) {
  if (!is.numeric(cells)) {
    return(cells)
  }
  accessor <- tryCatch(data_set$getExpressionRow, error = function(e) NULL)
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
    n_cells <- viewerExpressionCellCount(data_set)
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

## Cells of the expression matrix, without attaching an H5 backend.
viewerExpressionCellCount <- function(data_set) {
  rows <- attr(data_set, "cerebro_h5_rows", exact = TRUE)
  tryCatch(
    if (is.list(rows)) rows$n_cells() else ncol(data_set$expression),
    error = function(e) NA_integer_
  )
}

## Rows of an H5 sidecar straight from the file (all cells, or cell indices);
## NULL when that does not apply, so the caller uses the R6 accessors.
viewerH5ExpressionRows <- function(data_set, cells, genes) {
  rows <- attr(data_set, "cerebro_h5_rows", exact = TRUE)
  if (!is.list(rows) || !(is.null(cells) || is.numeric(cells))) {
    return(NULL)
  }
  values <- lapply(genes, rows$row)
  names(values) <- genes
  values <- values[!vapply(values, is.null, logical(1))]
  if (is.null(cells)) values else lapply(values, `[`, as.integer(cells))
}

## Per-cell mean of several genes from the H5 rows, one gene at a time, so a
## large gene set never holds every row and the TENxMatrix (HDF5Array plus
## every cell name: seconds on a million cells) stays unattached. NULL when
## that does not apply or the file lacks a gene.
viewerH5ExpressionMean <- function(data_set, cells, genes) {
  rows <- attr(data_set, "cerebro_h5_rows", exact = TRUE)
  if (
    !is.list(rows) ||
      !(is.null(cells) || is.numeric(cells)) ||
      !length(genes)
  ) {
    return(NULL)
  }
  ## Every cell: add each gene's non-zero cells into one vector, not a dense
  ## row per gene (twelve genes on a million cells: 12 vectors of 8 MB).
  if (is.null(cells) && is.function(rows$each_sparse)) {
    total <- numeric(rows$n_cells())
    found <- rows$each_sparse(genes, function(i, entries) {
      total[entries$index] <<- total[entries$index] + entries$value
    })
    return(if (found) total / length(genes))
  }
  total <- NULL
  for (gene in genes) {
    value <- rows$row(gene)
    if (is.null(value)) {
      return(NULL)
    }
    if (!is.null(cells)) {
      value <- value[as.integer(cells)]
    }
    total <- if (is.null(total)) value else total + value
  }
  total / length(genes)
}

viewerExpressionRow <- function(data_set, cells, gene) {
  direct <- viewerH5ExpressionRows(data_set, cells, gene)
  if (!is.null(direct) && length(direct)) {
    return(direct[[1L]])
  }
  cells <- viewerExpressionCells(data_set, cells)
  as.numeric(data_set$getExpressionRow(gene = gene, cells = cells))
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

  ## A gene the file lacks is omitted, and the genes it has are not read
  ## again. With none found, the R6 path below reports the missing gene.
  direct <- viewerH5ExpressionRows(data_set, cells, genes)
  if (!is.null(direct) && length(direct)) {
    return(direct)
  }

  if (length(genes) == 1L) {
    value <- viewerExpressionRow(data_set, cells, genes[[1L]])
    if (is.null(value)) {
      return(list())
    }
    return(stats::setNames(list(value), genes))
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
  ## I() on a vector the caller still holds copies it (1.3 M cells: x, y,
  ## colour, keys and hover columns, about 50 MB per frame); a value that is
  ## already a bare AsIs array is sent as it is.
  wire_array <- function(value) {
    if (is.null(value)) {
      NULL
    } else if (inherits(value, "AsIs") && is.null(names(value))) {
      value
    } else {
      I(unname(value))
    }
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
    c(
      "x",
      "y",
      "z",
      "selection_key",
      "selection_index",
      "color",
      "group",
      "group_code",
      "point_sizes"
    ),
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
    ## Element size, which is also the chunk's alignment.
    size <- switch(type, i8 = 1L, i16 = 2L, i32 = 4L, f32 = 4L, f64 = 8L, 1L)
    bytes <- if (identical(type, "json")) {
      cv_wire_json_strings(values)
    } else {
      writeBin(
        if (startsWith(type, "i")) as.integer(values) else as.numeric(values),
        raw(),
        size = size,
        endian = "little"
      )
    }
    offset <- data_size + ((size - data_size %% size) %% size)
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
## Last core aux actually delivered per view: its cell key and wire token. A
## later render with the same cells lets the browser reuse those barcodes.
.cerebro_cell_view_aux_served <- new.env(parent = emptyenv())

## With a cell-name resource, `cell_rows` (canonical rows of the points) are
## sent instead of `cell_barcodes`, and the browser looks the names up.
## Contiguous 0-based rows as list(kind = "range", start, length), else NULL.
cerebroRowRange <- function(rows) {
  if (
    !is.numeric(rows) ||
      length(rows) < 2L ||
      any(diff(as.integer(rows)) != 1L)
  ) {
    return(NULL)
  }
  list(kind = "range", start = as.integer(rows[[1L]]), length = length(rows))
}

cerebroCellViewDeferredAux <- function(
  selection_rows,
  cell_barcodes,
  hover_columns = list(),
  hover = FALSE,
  names_resource = NULL,
  cell_rows = NULL,
  keys = TRUE
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
  ## FALSE when the browser already holds the cells' names (see
  ## cerebroCellViewResolveDeferredAux()).
  key_fields <- if (!isTRUE(keys)) {
    list()
  } else if (is.list(names_resource)) {
    index <- reorder(as.integer(cell_rows) - 1L)
    list(
      ## Contiguous rows (every cell of a projection or trajectory in order)
      ## travel as their range, not as a million integers.
      selection_index = if (!grouped) {
        cerebroRowRange(index) %||% index
      } else {
        index
      },
      cell_names_resource = names_resource
    )
  } else {
    list(selection_key = reorder(as.character(cell_barcodes)))
  }
  c(
    key_fields,
    list(
      hover = list(
        hoverinfo = if (isTRUE(hover)) "text" else "skip",
        columns = columns
      )
    )
  )
}

## `keys = FALSE`: the core delivery already named the cells, so a builder that
## takes `keys` neither reads nor sends them again.
cerebroCellViewResolveDeferredAux <- function(message, keys = TRUE) {
  if (!is.function(message$build)) {
    return(message)
  }
  build <- message$build
  message$build <- NULL
  resolved <- if (!isTRUE(keys) && "keys" %in% names(formals(build))) {
    build(keys = FALSE)
  } else {
    build()
  }
  if (!is.list(resolved)) {
    stop("deferred cell-view auxiliary data must be a list")
  }
  for (name in names(resolved)) {
    message[[name]] <- resolved[[name]]
  }
  message
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
  session$sendBinaryMessage(
    "cell_view_recolor_binary",
    cv_wire_pack_message(message)
  )
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
      if (!is.null(message$aux_key)) {
        .cerebro_cell_view_aux_served[[message$id]] <- list(
          key = message$aux_key,
          wire_token = message$wire_token,
          dataset_context = message$dataset_context
        )
      }
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
  message <- cerebroCellViewResolveDeferredAux(message, keys = !core_sent)
  if (core_sent) {
    message$selection_key <- NULL
  }
  ## The browser never reads aux_key; only this server compares it.
  sent <- message
  sent$aux_key <- NULL
  session$sendBinaryMessage(
    "cell_view_aux_binary",
    cv_wire_pack_message(sent)
  )
  ## All of it was sent at once: a later frame of the same cells names it, as
  ## after a core delivery above.
  if (
    !core_sent &&
      !is.null(message$aux_key) &&
      !(is.null(message$selection_key) && is.null(message$selection_index)) &&
      !isTRUE(message$hover$pending)
  ) {
    .cerebro_cell_view_aux_served[[message$id]] <- list(
      key = message$aux_key,
      wire_token = message$wire_token,
      dataset_context = message$dataset_context
    )
  }
  invisible(TRUE)
}

## Small UI snapshots only. Never enumerate values of all Shiny inputs: those
## include million-cell selections and transport caches. Reads are isolated so
## feedback cannot add a dependency to an existing preparation path.
## Pure display toggles patch the mounted renderer, without fetching geometry.
cerebroObserveLocalAppearance <- function(id, input, session) {
  ids <- c(
    group_labels = paste0(id, "_group_labels"),
    draw_border = paste0(id, "_point_border"),
    keep_square = paste0(id, "_keep_square")
  )
  shiny::observeEvent(
    lapply(ids, function(key) input[[key]]),
    {
      values <- lapply(ids, function(key) input[[key]])
      values <- values[!vapply(values, is.null, logical(1))]
      identity <- viewerDatasetIdentity()
      shiny::req(identity$fingerprint, length(values))
      session$sendCustomMessage(
        "cell_view_appearance",
        list(
          id = id,
          dataset_context = viewer_loaded_dataset_context(),
          dataset_fingerprint = identity$fingerprint,
          values = values
        )
      )
    },
    ignoreInit = TRUE
  )
}

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
  feedback = NULL,
  aux_key = NULL
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
  ## Per-group panels hold only their own cells, so they neither reuse nor
  ## stand in for the dataset-wide coordinates of a projection.
  shared_projection <- if (is.null(message$data$panels)) {
    viewerSharedProjectionName(
      meta$space_label,
      n_cells,
      dataset_context = dataset_context
    )
  }
  shared_zero_color <- isTRUE(message$data$shared_zero_color)
  message$data$shared_zero_color <- NULL
  if (isTRUE(shared_zero_color)) {
    message$data$color <- NULL
    message$data$zero_color <- TRUE
    message$data$n <- n_cells
  }
  if (.runtimeIsString(shared_projection)) {
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
    ## Same dataset and cells as the barcodes the browser already holds:
    ## name that delivery so it can skip requesting them again.
    served <- if (!is.null(aux_key)) .cerebro_cell_view_aux_served[[id]]
    if (
      is.list(served) &&
        identical(served$key, aux_key) &&
        viewerDatasetContextEqual(served$dataset_context, dataset_context)
    ) {
      message$data$aux_reuse <- served$wire_token
    }
    message$data$selection_key <- NULL
    full_hover <- message$hover
    message$hover <- list(hoverinfo = "skip")
  } else if (is.function(deferred_aux)) {
    auxiliary <- deferred_aux()
    message$data$selection_key <- auxiliary$selection_key
    message$hover <- auxiliary$hover
  }

  session$sendBinaryMessage("cell_view_binary", cv_wire_pack_message(message))

  if (progressive) {
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
    .cerebro_cell_view_aux_pending[[paste(id, token, sep = ":")]] <- c(
      list(
        id = id,
        wire_token = token,
        dataset_context = dataset_context,
        render_token = message$render_token
      ),
      if (is.function(deferred_aux)) {
        list(build = deferred_aux, core_builder = core_aux, aux_key = aux_key)
      } else {
        list(selection_key = selection_keys, hover = full_hover)
      }
    )
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
  group_labels = FALSE,
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

  ## A factor is split on its codes: as.character() and match() over every
  ## cell would hash each of a million strings again.
  group_labels <- NULL
  if (factor_color) {
    group_labels <- factor_levels
    group_codes <- factor_codes
    if (has_missing_color) {
      missing_code <- match("(missing)", group_labels)
      if (is.na(missing_code)) {
        group_labels <- c(group_labels, "(missing)")
        missing_code <- length(group_labels)
      }
      group_codes[is.na(group_codes)] <- missing_code
    }
    ## An NA level reads as "(missing)" too: merge it by its label.
    if (anyNA(group_labels)) {
      group_labels <- NULL
    }
  }
  if (is.null(group_labels)) {
    color <- as.character(color)
    color[is.na(color)] <- "(missing)"
    cells_by_group <- split(seq_along(color), color)
  } else {
    cells_by_group <- split(
      seq_along(group_codes),
      structure(group_codes, levels = group_labels, class = "factor")
    )
  }
  meta[["traces"]] <- list()
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
    ## A factor split also lists the levels no cell has.
    cells <- cells_by_group[[group]]
    if (!length(cells)) {
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
    traces <- unlist(meta[["traces"]], use.names = FALSE)
    data[["canonical_group"]] <- I(as.integer(
      if (is.null(group_labels)) {
        match(color, traces) - 1L
      } else {
        (match(group_labels, traces) - 1L)[group_codes]
      }
    ))
  }

  list(meta = meta, data = data, hover = hover_data)
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
  ## A selection reported by row is read by position: no names to match, and
  ## the first-frame metadata has every group column.
  selected_rows <- viewerSelectionRows(selection)
  if (!is.null(selected_rows) && missing(metadata)) {
    metadata <- viewerProjectionFirstFrameMetadata()
  }
  has_cells <- length(keys) > 0L || length(selected_rows) > 0L
  ## The values of `group` for the selected cells.
  selected_values <- function(group) {
    if (!is.null(selected_rows)) {
      return(metadata[[group]][selected_rows[selected_rows <= nrow(metadata)]])
    }
    metadata[[group]][match(keys, as.character(metadata[["cell_barcode"]]))]
  }
  ## Cells per value, without turning every selected cell into a string.
  count_values <- function(values) {
    if (is.factor(values)) {
      counts <- tabulate(as.integer(values), nbins = nlevels(values))
      names(counts) <- levels(values)
      counts <- counts[counts > 0L & nzchar(levels(values))]
      return(as.table(counts))
    }
    values <- as.character(values)
    table(values[!is.na(values) & nzchar(values)])
  }
  # Composition has nothing to render until stable selection keys arrive.
  # Return before forcing the lazy metadata/groups defaults: on million-cell
  # datasets those accessors can hydrate the complete per-cell metadata even
  # though the result below would immediately be discarded.
  if (isTRUE(composition) && !has_cells) {
    return(NULL)
  }
  # Without keys the summary needs only the row count. The first-frame
  # metadata has it; the getMetaData() default would read every cell name of
  # a thin CRB first (1.9 s on 1.46 M cells, right after the first paint).
  if (missing(metadata) && !has_cells) {
    metadata <- viewerProjectionFirstFrameMetadata()
  }
  n_selected <- if (!is.null(selected_rows)) {
    length(selected_rows)
  } else if (length(keys)) {
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
      !has_cells ||
        !length(comp_candidates) ||
        (is.null(selected_rows) && !"cell_barcode" %in% colnames(metadata))
    ) {
      return(NULL)
    }
    comp_group <- comp_candidates[[1L]]
    counts <- count_values(selected_values(comp_group))
    if (!length(counts)) {
      return(NULL)
    }

    counts <- sort(counts, decreasing = TRUE)
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
    has_cells &&
      length(candidates) &&
      (!is.null(selected_rows) || "cell_barcode" %in% colnames(metadata))
  ) {
    counts <- count_values(selected_values(candidates[[1L]]))
    if (length(counts)) {
      dominant <- names(counts)[[which.max(counts)]]
      profile <- shiny::tags$span(
        class = "cerebro-selection-status-profile",
        paste0(
          dominant,
          " \u00b7 ",
          round(100 * max(counts) / sum(counts)),
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

## Numeric columns without NA whose name matches `pattern` and whose values lie
## within [lower, upper].
findColumnsMatching <- function(df, pattern, lower = -Inf, upper = Inf) {
  which(vapply(
    seq_along(df),
    function(i) {
      values <- df[[i]]
      grepl(colnames(df)[i], pattern = pattern, ignore.case = TRUE) &&
        !anyNA(values) &&
        is.numeric(values) &&
        all(values >= lower & values <= upper)
    },
    logical(1)
  ))
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
    findColumnsMatching(table, "pct|percent|%", 0, 100)
  } else {
    integer()
  }
  columns_p_value <- if (classify_values) {
    findColumnsMatching(
      table,
      "pval|p_val|p-val|p.val|padj|p_adj|p-adj|p.adj|adjp|adj_p|adj-p|adj.p|FDR|qval|q_val|q-val|q.val",
      0,
      1
    )
  } else {
    integer()
  }
  columns_logFC <- if (classify_values) {
    findColumnsMatching(table, "logFC|log-FC|log_FC|log.FC")
  } else {
    integer()
  }

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
  columns_percent <- c(columns_percent, columns_percentage)

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
    if (length(columns_integer)) {
      table <- table %>%
        DT::formatRound(
          columns = columns_integer,
          digits = 0,
          interval = 3,
          mark = ","
        )
    }

    ## p-values
    if (length(columns_p_value)) {
      table <- table %>%
        DT::formatSignif(columns = columns_p_value, digits = 3)
    }

    ## logFC
    if (length(columns_logFC)) {
      table <- table %>%
        DT::formatRound(columns = columns_logFC, digits = 3)
    }

    ## percentage
    if (length(columns_percent)) {
      table <- table %>%
        DT::formatPercentage(columns = columns_percent, digits = 2)
    }

    ## numeric but none of the above
    if (length(columns_only_numeric)) {
      table <- table %>%
        DT::formatSignif(columns = columns_only_numeric, digits = 3)
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

    ## 101-step white-to-accent background over each column's own range.
    ramp <- function(table, columns) {
      if (nrow(table_original) <= 1) {
        return(table)
      }
      for (i in columns) {
        ## Over the known values: a column may be NA for some cells (a
        ## pseudotime that exists for one lineage only) or for all of them.
        values <- table_original[[i]]
        values <- values[is.finite(values)]
        if (length(values) < 2L) {
          next
        }
        range <- range(values)
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
      table
    }
    bar <- function(table, columns, data_range) {
      if (!length(columns)) {
        return(table)
      }
      table %>%
        DT::formatStyle(
          columns = columns,
          background = DT::styleColorBar(data_range, bar_col),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    }
    ## Columns whose values all have a colour in the named palette.
    palette_colors <- function(table, palettes) {
      for (i in which(colnames(table_original) %in% palettes)) {
        colors <- reactive_colors()[[colnames(table_original)[i]]]
        if (all(unique(table_original[[i]]) %in% names(colors))) {
          table <- table %>%
            DT::formatStyle(
              i,
              backgroundColor = DT::styleEqual(names(colors), colors),
              fontWeight = 'bold'
            )
        }
      }
      table
    }

    ## Keep this order: a column can receive a ramp and a later bar.
    table <- ramp(table, columns_integer)
    table <- bar(table, columns_p_value, c(1, 0))
    table <- ramp(table, columns_logFC)
    table <- bar(table, columns_percent, c(0, 1))
    table <- ramp(table, columns_only_numeric)

    ## logicals
    if (length(columns_logical)) {
      table <- table %>%
        DT::formatStyle(
          columns_logical,
          color = DT::styleEqual(c(TRUE, FALSE), c(up_col, down_col)),
          fontWeight = DT::styleEqual(c(TRUE, FALSE), c('bold', 'normal'))
        )
    }

    ## grouping variables and cell cycle assignments
    table <- palette_colors(table, getGroups())
    table <- palette_colors(table, getCellCycle())
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
## A-by-B composition tables, e.g. samples by clusters (Groups, Trajectory).
##
## Built as a two-way contingency table: no sort of all cells and no dplyr
## grouping pipeline. Factors keep their levels, characters become factors over
## their sorted values, other columns (e.g. a numeric trajectory state) keep
## their values. Rows are ordered by A, then B, with NA last.
##----------------------------------------------------------------------------##
calculateTableAB <- function(table, groupA, groupB, mode, percent) {
  for (group in c(groupA, groupB)) {
    if (!group %in% colnames(table)) {
      stop(
        "Column `",
        group,
        "` could not be found in meta data.",
        call. = FALSE
      )
    }
  }
  encode <- function(values) {
    raw <- if (is.factor(values) || is.character(values)) {
      NULL
    } else {
      sort(unique(values))
    }
    if (!is.factor(values)) {
      values <- factor(values, levels = sort(unique(values)), exclude = NULL)
    }
    codes <- as.integer(values)
    labels <- levels(values)
    has_na <- anyNA(codes)
    if (has_na) {
      codes[is.na(codes)] <- length(labels) + 1L
    }
    list(codes = codes, labels = labels, raw = raw, has_na = has_na)
  }
  decode <- function(index, encoded) {
    if (!is.null(encoded$raw)) {
      return(encoded$raw[index])
    }
    factor(encoded$labels[index], levels = encoded$labels)
  }
  encoded_a <- encode(table[[groupA]])
  encoded_b <- encode(table[[groupB]])
  n_a <- length(encoded_a$labels) + as.integer(encoded_a$has_na)
  n_b <- length(encoded_b$labels) + as.integer(encoded_b$has_na)
  linear <- (encoded_a$codes - 1L) * n_b + encoded_b$codes
  cell_counts <- tabulate(linear, nbins = n_a * n_b)
  occupied <- which(cell_counts > 0L)
  index_a <- (occupied - 1L) %/% n_b + 1L
  index_b <- (occupied - 1L) %% n_b + 1L

  counts <- data.frame(
    decode(index_a, encoded_a),
    decode(index_b, encoded_b),
    count = cell_counts[occupied],
    total_cell_count = tabulate(encoded_a$codes, nbins = n_a)[index_a],
    check.names = FALSE
  )
  names(counts)[1:2] <- c(groupA, groupB)

  formatTableAB(counts, groupA, groupB, mode, percent, encoded_b$labels)
}

## Percentages and/or wide format of a long calculateTableAB() table.
formatTableAB <- function(
  counts,
  groupA,
  groupB,
  mode,
  percent,
  levels_group_b = levels(counts[[groupB]])
) {
  if (isTRUE(percent)) {
    counts[["count"]] <- counts[["count"]] / counts[["total_cell_count"]]
    counts <- counts[, c(groupA, "total_cell_count", groupB, "count")]
  }

  if (identical(mode, "wide")) {
    counts <- tidyr::pivot_wider(
      counts,
      id_cols = dplyr::all_of(c(groupA, "total_cell_count")),
      names_from = dplyr::all_of(groupB),
      values_from = "count",
      values_fill = 0
    ) %>%
      dplyr::select(
        dplyr::all_of(c(groupA, "total_cell_count")),
        dplyr::any_of(levels_group_b)
      )
    ## cell cycle phases in their natural order
    if (all(c("G1", "G2M", "S") %in% colnames(counts))) {
      counts <- counts %>%
        dplyr::select(
          dplyr::all_of(c(groupA, "total_cell_count", "G1", "S", "G2M")),
          dplyr::everything()
        )
    }
  }

  counts
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
    column <- table[[group]]
    # Registered levels are a small dictionary. Avoid hashing every string
    # again with unique() after the million-cell rows have been shuffled.
    levels <- tryCatch(as.character(group_levels(group)), error = function(e) {
      character()
    })
    levels[is.na(levels)] <- "NA"
    levels <- unique(c(levels, "NA"))
    # A factor maps its own levels, not the label of every cell.
    if (is.factor(column) && !anyNA(levels(column))) {
      values <- NULL
      codes <- match(levels(column), levels)[as.integer(column)]
      codes[is.na(column)] <- match("NA", levels)
    } else {
      values <- as.character(column)
      codes <- match(values, levels)
      codes[is.na(values)] <- match("NA", levels)
    }
    if (anyNA(codes)) {
      if (is.null(values)) {
        values <- as.character(column)
      }
      # Unregistered metadata, and registered levels that miss values
      # (addGroup() does not check them), keep the first-occurrence encoding,
      # including literal/missing NA.
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
    values <- metadata[[group]]
    keep <- keep &
      if (is.factor(values)) {
        ## Look up the levels once, then index by code: %in% would turn every
        ## cell's level into a string first. The last entry stands for NA.
        codes <- as.integer(values)
        codes[is.na(codes)] <- nlevels(values) + 1L
        c(levels(values) %in% selected, NA %in% selected)[codes]
      } else {
        values %in% selected
      }
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
## Helper: match a URL dataset token against the stable dataset ids (the names
## of the configured .crb files).
##
## Returns the matched id or '' if no match.
##----------------------------------------------------------------------------##
## Dataset ids are the names of the configured files: all present, non-empty
## and unique.
viewerDatasetIdsValid <- function(ids) {
  is.character(ids) && !anyNA(ids) && all(nzchar(ids)) && !anyDuplicated(ids)
}

match_dataset_id_by_url <- function(url_dataset, files) {
  ids <- names(files)
  if (
    !.runtimeIsString(url_dataset) ||
      is.null(ids) ||
      !url_dataset %in% ids
  ) {
    return("")
  }
  url_dataset
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

## Getters that forward to the loaded data set's method of the same name and
## return NULL while no data set is loaded.
.viewerForwardGetter <- function(method) {
  force(method)
  function(...) {
    if (is_cerebro_dataset(data_set())) {
      return(data_set()[[method]](...))
    }
  }
}
for (getter in c(
  "getExperiment",
  "getParameters",
  "getGroups",
  "getCellCycle",
  "getMetaData",
  "getProjection",
  "getMethodsForMarkerGenes",
  "getGroupsWithMarkerGenes",
  "getMarkerGenes",
  "getMethodsForTrajectories",
  "getNamesOfTrajectories",
  "getTrajectory",
  "getGroupsWithMostExpressedGenes",
  "getMethodsForEnrichedPathways",
  "getGroupsWithEnrichedPathways",
  "getEnrichedPathways",
  "checkForExtraPlots",
  "getNamesOfExtraPlots",
  "getExtraPlot"
)) {
  assign(getter, .viewerForwardGetter(getter))
}
rm(getter)
viewerGetMostExpressedGenes <- .viewerForwardGetter("getMostExpressedGenes")
getGeneNames <- function() {
  cache <- viewerProjectionFirstFrameCache()
  if (!is.null(cache$gene_names)) {
    return(cache$gene_names)
  }
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getGeneNames())
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
      isTRUE(schema$version %in% c(3L, 4L)) &&
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
  ## The first-frame metadata has the same columns without the cell names; a
  ## thin CRB is not hydrated just to list them.
  all_cols <- setdiff(
    colnames(viewerProjectionFirstFrameMetadata()),
    "cell_barcode"
  )

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

## The first metadata column that matches the earliest of `patterns`, or NULL.
firstColumnMatching <- function(patterns) {
  cols <- colnames(getMetaData())
  for (pattern in patterns) {
    matches <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      return(matches[1])
    }
  }
  NULL
}

getMitoColumn <- function() {
  firstColumnMatching(c(
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
  ))
}

hasMitoColumn <- function() {
  !is.null(getMitoColumn())
}

getRiboColumn <- function() {
  firstColumnMatching(c(
    "^percent[_.]?ribo$",
    "^percent[_.]?ribosomal$",
    "^pct[_.]?ribo$",
    "^pct[_.]?ribosomal$",
    "^ribo[_.]?percent$",
    "^ribosomal[_.]?percent$",
    "^ribo[_.]?pct$",
    "^ribosomal[_.]?pct$"
  ))
}

hasRiboColumn <- function() {
  !is.null(getRiboColumn())
}

getEryColumn <- function() {
  firstColumnMatching(c(
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
  ))
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

.runtimeBackendPlanError <- function(message, crb_path = NULL) {
  suffix <- if (is.null(crb_path)) {
    ""
  } else {
    paste0(" for CRB '", crb_path, "'")
  }
  stop("The backend plan ", message, suffix, ".", call. = FALSE)
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
    entry$type %in% .runtimeBackendTypes
  valid_mode <- is.character(entry$mode) &&
    length(entry$mode) == 1L &&
    !is.na(entry$mode) &&
    entry$mode %in% c("embedded", "bundled")
  if (!valid_type || !valid_mode) {
    .runtimeBackendPlanError("entry has an unsupported type or mode", crb_path)
  }
  if (identical(entry$mode, "embedded")) {
    if (!identical(entry$type, "embedded") || !is.null(entry$location)) {
      .runtimeBackendPlanError("embedded entry is inconsistent", crb_path)
    }
  } else {
    if (identical(entry$type, "embedded")) {
      .runtimeBackendPlanError("bundled entry is inconsistent", crb_path)
    }
    entry$location <- .runtimePortableBackendPath(
      entry$location,
      "The backend plan bundled backend location"
    )
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
  list(source = "fallback")
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
    message(glue::glue("[{Sys.time()}] CRB cache hit: {basename(path)}"))
    prototype <- cached$object
  } else {
    message(glue::glue(
      "[{Sys.time()}] CRB cache miss, loading: {basename(path)}"
    ))
    prototype <- read_cerebro_file(path)
    .cacheCrbPrototype(path, prototype, cache_identity)
  }

  ## Clone the inert serialized prototype before installing delayed bindings.
  ## R6 clone() forces delayed fields, which would attach the H5 matrix while
  ## the user was still on Data Info and also mutate the shared prototype.
  obj <- .cloneCachedCrb(prototype)
  obj <- .attachExternalExpression(obj, path, effective_backend)
  obj <- .attachImmuneRepertoireBackend(obj, path)
  obj <- .attachSpatialMoleculeBackend(obj, path)
  obj
}

## The plain backend descriptor stored in `obj[[field]]`, or NULL when there is
## none. An active or lazy binding is rejected without being evaluated.
.crbBackendDescriptor <- function(obj, field, invalid) {
  if (!is.environment(obj) || !exists(field, envir = obj, inherits = FALSE)) {
    return(NULL)
  }
  if (
    bindingIsActive(field, obj) ||
      isTRUE(rlang::env_binding_are_lazy(obj, field))
  ) {
    stop(invalid, call. = FALSE)
  }
  obj[[field]]
}

.attachImmuneRepertoireBackend <- function(obj, crb_path) {
  invalid <- "The immune repertoire sidecar descriptor is invalid."
  backend <- .crbBackendDescriptor(obj, "immune_repertoire_backend", invalid)
  if (is.null(backend)) {
    return(obj)
  }
  valid <- is.list(backend) &&
    identical(backend$type, "file") &&
    .runtimeIsString(backend$file) &&
    !backend$file %in% c(".", "..") &&
    !grepl("[/\\\\]", backend$file) &&
    is.character(backend$md5) &&
    length(backend$md5) == 1L &&
    !is.na(backend$md5) &&
    grepl("^[[:xdigit:]]{32}$", backend$md5)
  if (!valid) {
    stop(invalid, call. = FALSE)
  }
  backend$samples <- unique(as.character(backend$samples %||% character()))
  backend$chains <- unique(as.character(backend$chains %||% character()))
  if (anyNA(backend$samples) || anyNA(backend$chains)) {
    stop(invalid, call. = FALSE)
  }
  backend$samples <- backend$samples[nzchar(backend$samples)]
  backend$chains <- backend$chains[nzchar(backend$chains)]
  root <- dirname(normalizePath(crb_path, mustWork = FALSE))
  repertoire_file <- file.path(root, backend$file)
  if (!file.exists(repertoire_file) || dir.exists(repertoire_file)) {
    stop(
      "The immune repertoire sidecar is missing: ",
      repertoire_file,
      call. = FALSE
    )
  }
  backend$root <- root
  obj[["immune_repertoire_backend"]] <- backend
  obj
}

.attachSpatialMoleculeBackend <- function(obj, crb_path) {
  invalid <- "The spatial molecule backend descriptor is invalid."
  backend <- .crbBackendDescriptor(obj, "spatial_molecule_backend", invalid)
  if (is.null(backend)) {
    return(obj)
  }
  valid <- is.list(backend) &&
    identical(backend$type, "directory") &&
    .runtimeIsString(backend$location) &&
    !backend$location %in% c(".", "..") &&
    !grepl("[/\\\\]", backend$location)
  if (!valid) {
    stop(invalid, call. = FALSE)
  }
  root <- file.path(
    dirname(normalizePath(crb_path, mustWork = FALSE)),
    backend$location
  )
  if (!dir.exists(root)) {
    stop("The spatial molecule sidecar is missing: ", root, call. = FALSE)
  }
  backend$root <- root
  obj[["spatial_molecule_backend"]] <- backend
  obj
}

##----------------------------------------------------------------------------##
## Resolve an external expression backend at load time (B3).
##
## H5 crbs ship a sibling <stem>.h5 file. This helper opens it lazily from a
## path rooted at the caller's view of the filesystem.
##
## Configured bundle CRBs receive the build-validated effective backend entry.
## Direct launches and uploads read only the ordinary expression_backend field;
## serialized getter code is never invoked by this internal loading path.
##----------------------------------------------------------------------------##

## The H5 file stores the cell names under /expression/genes and the gene names
## under /expression/barcodes (writeTENxMatrix() names the axes the 10x way
## round for a cells x genes matrix). rhdf5 reads them without attaching
## HDF5Array, whose namespace takes seconds to load.
.runtimeH5Names <- function(sidecar, dataset) {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop("An H5-backed crb requires the rhdf5 package.", call. = FALSE)
  }
  as.character(as.vector(rhdf5::h5read(
    sidecar,
    paste0("expression/", dataset)
  )))
}

.runtimeH5CellCount <- function(sidecar) {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop("An H5-backed crb requires the rhdf5 package.", call. = FALSE)
  }
  as.integer(as.vector(rhdf5::h5read(sidecar, "expression/shape")))[[1L]]
}

## One gene's values for every cell, read with rhdf5 from the gene's slice of
## the TENx arrays (/expression/indptr, indices, data). The DelayedArray path
## returns the same values but first loads HDF5Array (seconds for its S4
## classes and methods) and reads every cell name. `verify` runs before the
## first read (the thin-CRB cell-name check).
.h5ExpressionRows <- function(sidecar, verify) {
  state <- new.env(parent = emptyenv())
  prepare <- function() {
    if (is.null(state$genes)) {
      verify()
      state$n_cells <- .runtimeH5CellCount(sidecar)
      state$genes <- .runtimeH5Names(sidecar, "barcodes")
    }
  }
  ## The non-zero cells (1-based `index`) and `value`s of genes, one file
  ## and dataset open for all of them (h5read opens the file for every
  ## array: 3 x 12 times for twelve genes). `visit(i, entries)` gets the i-th
  ## gene's entries. FALSE, with nothing read, when the file lacks a gene.
  each_sparse <- function(genes, visit) {
    prepare()
    columns <- match(genes, state$genes)
    if (anyNA(columns)) {
      return(FALSE)
    }
    file <- rhdf5::H5Fopen(sidecar, flags = "H5F_ACC_RDONLY")
    datasets <- lapply(c("indptr", "indices", "data"), function(name) {
      rhdf5::H5Dopen(file, paste0("expression/", name))
    })
    on.exit({
      for (dataset in datasets) {
        rhdf5::H5Dclose(dataset)
      }
      rhdf5::H5Fclose(file)
    })
    slice <- function(dataset, start, count) {
      space <- rhdf5::H5Dget_space(dataset)
      on.exit(rhdf5::H5Sclose(space), add = TRUE)
      rhdf5::H5Sselect_hyperslab(space, start = start, count = count)
      memory <- rhdf5::H5Screate_simple(count)
      on.exit(rhdf5::H5Sclose(memory), add = TRUE)
      as.vector(rhdf5::H5Dread(
        dataset,
        h5spaceFile = space,
        h5spaceMem = memory,
        bit64conversion = "double"
      ))
    }
    for (i in seq_along(columns)) {
      bounds <- slice(datasets[[1L]], columns[[i]], 2L)
      count <- bounds[[2L]] - bounds[[1L]]
      entries <- if (count > 0) {
        start <- bounds[[1L]] + 1
        list(
          index = slice(datasets[[2L]], start, count) + 1,
          value = slice(datasets[[3L]], start, count)
        )
      } else {
        list(index = integer(), value = numeric())
      }
      visit(i, entries)
    }
    TRUE
  }
  list(
    n_cells = function() {
      prepare()
      state$n_cells
    },
    each_sparse = each_sparse,
    ## NULL for a gene the file does not have.
    row = function(gene) {
      values <- NULL
      each_sparse(gene, function(i, entries) {
        values <<- numeric(state$n_cells)
        values[entries$index] <<- entries$value
      })
      values
    }
  )
}

.checkThinCrbCells <- function(cells, schema, crb_path) {
  if (
    !identical(
      rlang::hash(enc2utf8(as.character(cells))),
      schema$cell_names_hash
    )
  ) {
    stop(
      "The H5 cell names do not match CRB '",
      basename(crb_path),
      "'.",
      call. = FALSE
    )
  }
  invisible(cells)
}

.validateThinCrbShape <- function(metadata, projections, schema) {
  expected_cells <- schema$n_cells
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
      "The thin CRB and H5 sidecar have incompatible cell metadata.",
      call. = FALSE
    )
  }
  for (name in schema$projection_rownames) {
    projection <- projections[[name]]
    ## `cells` passed the checks above and .validateThinCrbShape() found every
    ## projection a data frame of as many rows; rownames<- would look for
    ## duplicates again for every projection.
    attr(projection, "row.names") <- cells
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

## The H5 file's cell names, compared with the CRB's checksum. `state` is shared
## by everything that reads them for one attach, so a data set that has been
## hydrated is not read and hashed a second time before its expression.
.thinCrbCellNames <- function(state, sidecar, schema, crb_path) {
  cells <- .runtimeH5Names(sidecar, "genes")
  .checkThinCrbCells(cells, schema, crb_path)
  state$verified <- TRUE
  cells
}

.deferThinCrbHydration <- function(
  obj,
  crb_path,
  sidecar,
  schema,
  state = new.env(parent = emptyenv())
) {
  if (!identical(.runtimeH5CellCount(sidecar), schema$n_cells)) {
    stop(
      "The thin CRB cell count does not match its H5 sidecar.",
      call. = FALSE
    )
  }
  metadata <- obj$meta_data
  projections <- obj$projections
  .validateThinCrbShape(metadata, projections, schema)
  attr(obj, "cerebro_projection_first_frame") <- list(
    meta_data = metadata,
    projections = projections,
    canonical_projections = schema$projection_rownames,
    aligned_projections = new.env(parent = emptyenv()),
    gene_names = .runtimeH5Names(sidecar, "barcodes")
  )
  hydrate <- local({
    hydrated <- NULL
    function() {
      if (is.null(hydrated)) {
        cells <- .thinCrbCellNames(state, sidecar, schema, crb_path)
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

.fallbackRuntimeBackendPlan <- function(obj, crb_path) {
  .effectiveRuntimeBackendPlan(.readRuntimeBackendDescriptor(obj, crb_path))
}

.runtimeBackendRecoveryAdvice <- function() {
  "Did the .h5 sibling get moved or dropped when the CRB was copied?"
}

## The H5 file a thin CRB attaches and the CRB's cell-name checksum, computed
## as .attachExternalExpression() does; NULL for any other CRB or any problem.
.thinCrbH5CellCheck <- function(obj, crb_path, effective_backend = NULL) {
  tryCatch(
    {
      schema <- .readRuntimeCrbSchema(obj, crb_path)
      be <- if (is.null(effective_backend)) {
        .fallbackRuntimeBackendPlan(obj, crb_path)
      } else {
        .validateRuntimeBackendEntry(effective_backend, crb_path)
      }
      if (is.null(schema) || identical(be$mode, "embedded")) {
        NULL
      } else {
        list(
          path = file.path(
            dirname(normalizePath(crb_path, mustWork = FALSE)),
            be$location
          ),
          expected = schema$cell_names_hash
        )
      }
    },
    error = function(error) NULL
  )
}

## Verified files are remembered in this function's own environment (the
## process environment of source_cache.R), keyed by path, size and
## modification time: each file is hashed once per process until it changes.
.verifyRuntimeH5Checksum <- function(path, md5, crb_path) {
  if (is.null(md5)) {
    return(invisible(TRUE))
  }
  home <- environment(sys.function())
  if (!exists(".runtime_h5_verified", envir = home, inherits = FALSE)) {
    assign(".runtime_h5_verified", new.env(parent = emptyenv()), envir = home)
  }
  verified <- get(".runtime_h5_verified", envir = home, inherits = FALSE)
  if (
    !is.character(md5) ||
      length(md5) != 1L ||
      is.na(md5) ||
      !grepl("^[[:xdigit:]]{32}$", md5)
  ) {
    stop("The Cerebro expression-backend descriptor is invalid.", call. = FALSE)
  }
  info <- file.info(path)
  key <- paste(
    normalizePath(path, mustWork = TRUE),
    info$size,
    format(as.numeric(info$mtime), digits = 17),
    md5,
    sep = "\r"
  )
  if (isTRUE(get0(key, envir = verified, inherits = FALSE))) {
    return(invisible(TRUE))
  }
  started <- proc.time()[["elapsed"]]
  actual <- unname(tools::md5sum(path))
  print(glue::glue(
    "[{Sys.time()}] Verified h5 checksum in ",
    "{round(proc.time()[['elapsed']] - started, 2)} s: {path}"
  ))
  if (!identical(tolower(actual), tolower(md5))) {
    stop(
      "The h5 expression sidecar checksum does not match CRB '",
      basename(crb_path),
      "'.",
      call. = FALSE
    )
  }
  assign(key, TRUE, envir = verified)
  invisible(TRUE)
}

.attachExternalExpression <- function(
  obj,
  crb_path,
  effective_backend = NULL
) {
  if (!any(grepl("Cerebro", class(obj)))) {
    return(obj)
  }
  .assertCurrentRuntimeCrb(obj, crb_path)
  crb_schema <- .readRuntimeCrbSchema(obj, crb_path)
  if (is.null(effective_backend)) {
    be <- .fallbackRuntimeBackendPlan(obj, crb_path)
  } else {
    be <- .validateRuntimeBackendEntry(effective_backend, crb_path)
  }

  if (identical(be$mode, "embedded")) {
    if (!is.null(crb_schema)) {
      stop("A thin CRB requires an H5 expression backend.", call. = FALSE)
    }
    return(obj)
  }

  loc_abs <- file.path(
    dirname(normalizePath(crb_path, mustWork = FALSE)),
    be$location
  )

  ## Check installation without loading the namespace (seconds when cold);
  ## pages that never read expression must not pay for it.
  if (!nzchar(system.file(package = "HDF5Array"))) {
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
      .runtimeBackendRecoveryAdvice(),
      call. = FALSE
    )
  }
  ## The CRB records the checksum of the H5 file it was written with (CRBs from
  ## before that record none): a replaced file with the same cell and gene
  ## names must not be painted as this data set.
  .verifyRuntimeH5Checksum(loc_abs, obj$expression_backend$md5, crb_path)
  cell_names_state <- new.env(parent = emptyenv())
  if (!is.null(crb_schema)) {
    obj <- .deferThinCrbHydration(
      obj,
      crb_path,
      loc_abs,
      crb_schema,
      cell_names_state
    )
  }
  ## On-disk layout is cells x genes (TENxMatrix orientation, optimised for
  ## per-gene column reads). Cerebro's internal layout is genes x cells, so
  ## we transpose lazily — DelayedArray::t() is O(1), no data is read.
  ## The matrix is never materialised into a dgCMatrix at attach time;
  ## queries stream from disk through the DelayedMatrix path in
  ## getExpressionRow / getExpressionBlock. Attach on first expression use.
  ## A thin CRB stores no cell names, so the H5 file's names are compared with
  ## its checksum here: the attach above only compared the cell count, and
  ## expression of another data set with as many cells must not be painted.
  ## The Viewer's process cache (crb_cache.R) checks the names in a daemon.
  process_check <- !is.null(crb_schema) &&
    exists("crb_h5_cell_check_start", mode = "function")
  if (process_check) {
    crb_h5_cell_check_start(loc_abs, crb_schema$cell_names_hash)
  }
  verify_cells <- function() {
    if (!is.null(crb_schema) && !isTRUE(cell_names_state$verified)) {
      if (
        process_check &&
          crb_h5_cells_verified(loc_abs, crb_schema$cell_names_hash)
      ) {
        cell_names_state$verified <- TRUE
        return(invisible())
      }
      ## Callers often wrap the first expression access in tryCatch(), so the
      ## refusal is also logged.
      tryCatch(
        .thinCrbCellNames(cell_names_state, loc_abs, crb_schema, crb_path),
        error = function(error) {
          message(glue::glue("[{Sys.time()}] {conditionMessage(error)}"))
          stop(error)
        }
      )
      if (process_check) {
        crb_h5_cells_confirm(loc_abs, crb_schema$cell_names_hash)
      }
    }
  }
  attr(obj, "cerebro_h5_rows") <- .h5ExpressionRows(loc_abs, verify_cells)
  ## Lets optional work wait for the daemon without blocking the session.
  attr(obj, "cerebro_h5_cells_pending") <- function() {
    process_check &&
      !isTRUE(cell_names_state$verified) &&
      crb_h5_cells_pending(loc_abs, crb_schema$cell_names_hash)
  }
  delayedAssign(
    "expression",
    {
      verify_cells()
      message(glue::glue(
        "[{Sys.time()}] Attaching h5 backend (lazy TENxMatrix): {loc_abs}"
      ))
      t(HDF5Array::TENxMatrix(loc_abs, group = "expression"))
    },
    eval.env = environment(),
    assign.env = obj
  )
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
    stored_index <- tryCatch(
      data$getTechnicalInfo()$extra_material_table_index,
      error = function(error) NULL
    )
    if (
      is.null(table_index) &&
        !is.null(dataset) &&
        exists("Cerebro.options", inherits = TRUE)
    ) {
      table_index <- Cerebro.options$viewer_content[[
        dataset
      ]]$extra_material_table_index
    }
    # Per-app overrides win; unmodified records keep their portable CRB labels.
    if (is.list(stored_index)) {
      table_index <- utils::modifyList(stored_index, table_index %||% list())
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

## Wrapper for immune repertoire module.
getImmuneRepertoire <- function() {
  ds <- data_set()
  if (!is_cerebro_dataset(ds)) {
    return(list())
  }
  tryCatch(ds$getImmuneRepertoire(), error = function(e) list())
}

getImmuneRepertoireSummary <- function() {
  viewerImmuneRepertoireSummary(data_set())
}

## File-backed repertoires describe themselves without reading every sample.
viewerImmuneRepertoireSummary <- function(ds) {
  if (is.null(ds)) {
    return(list(available = FALSE, samples = character(), chains = character()))
  }
  backend <- ds$immune_repertoire_backend
  if (is.list(backend) && identical(backend$type, "file")) {
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

## Wrapper for the HLA & TCR Motifs module: the stored typing table, or an empty
## canonical one when the data set carries none.
getHLATyping <- function() {
  ds <- data_set()
  if (!is_cerebro_dataset(ds)) {
    return(hla_normalize_typing(list(), source_type = "unknown"))
  }
  ds$getHLATyping()
}

## Wrappers for spatial module.
availableSpatial <- function() {
  ds <- data_set()
  if (!is_cerebro_dataset(ds)) {
    return(character(0))
  }
  tryCatch(ds$availableSpatial(), error = function(e) character(0))
}
## The public getter validates and normalizes coordinates and every embedded
## image (about a second for large images). Its result depends only on the
## loaded object, so keep it per section until data_set() changes.
.viewer_spatial_data_cache <- new.env(parent = emptyenv())
getSpatialData <- function(name) {
  ds <- data_set()
  if (!is_cerebro_dataset(ds)) {
    return(NULL)
  }
  cache <- .viewer_spatial_data_cache
  if (!identical(cache$dataset, ds)) {
    rm(list = ls(cache, all.names = TRUE), envir = cache)
    cache$dataset <- ds
  }
  key <- if (is.character(name) && length(name) == 1L && !is.na(name)) {
    paste0("section:", name)
  }
  if (!is.null(key) && !is.null(cache[[key]])) {
    return(cache[[key]])
  }
  value <- viewerReadSpatialData(ds, name)
  if (!is.null(key) && !is.null(value)) {
    assign(key, value, envir = cache)
  }
  value
}
viewerReadSpatialData <- function(ds, name) {
  tryCatch(
    ds$getSpatialData(name, hydrate_molecules = FALSE),
    error = function(e) NULL
  )
}

## Coordinate-only Spatial consumers run after spatial_projection_parameters_plot
## has validated the exact current dataset + entry through getSpatialData().
## Reuse the canonical stored coordinate reference there instead of invoking the
## getter's image/bounds normalization for every downstream reactive.
## Malformed storage keeps the fully validated fallback path.
viewerSpatialCoordinates <- function(name) {
  ds <- data_set()
  if (!is_cerebro_dataset(ds)) {
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

## Gene expression registers four selectors on the same gene list; a locale
## sort of a whole transcriptome takes about half a second. Share one.
.viewer_sorted_gene_choices <- viewerMemoOne()

## Gene selectors search the whole gene list in the browser (www/gene-selector.js).
## A server-side search answered from the first 1000 genes until the typed
## text's request returned, so typing "CD4" highlighted ABCD4 and Enter chose
## it; text typed before the search existed was never searched at all. The
## list is about 270 KB of JSON for 28,000 genes; encode it once per process.
.viewer_gene_choices_json <- viewerMemoOne()

viewerGeneChoicesJSON <- function(data, req) {
  shiny::httpResponse(
    200L,
    "application/json",
    .viewer_gene_choices_json(list(data$genes), function() {
      enc2utf8(as.character(jsonlite::toJSON(as.character(data$genes))))
    }),
    headers = list("Cache-Control" = "private, max-age=3600")
  )
}

## One gene list URL per gene list in a session. The gene selectors of a page
## (single gene, R/G/B channels) share it, so the list reaches the browser once.
viewerGeneChoicesUrl <- function(session, genes) {
  cache <- session$userData$cerebro_gene_choice_urls
  if (is.null(cache)) {
    cache <- session$userData$cerebro_gene_choice_urls <- new.env(
      parent = emptyenv()
    )
    cache$entries <- list()
    cache$count <- 0L
  }
  for (entry in cache$entries) {
    if (identical(entry$genes, genes)) {
      return(entry$url)
    }
  }
  cache$count <- cache$count + 1L
  url <- session$registerDataObj(
    paste0("cerebro-gene-choices-", cache$count),
    list(genes = genes),
    viewerGeneChoicesJSON
  )
  cache$entries <- c(
    list(list(genes = genes, url = url)),
    utils::head(cache$entries, 7L)
  )
  url
}

## Like updateSelectizeInput(session, inputId, choices, selected), but the
## browser fetches the choices from a URL shared by every selector of the same
## genes and searches them itself (www/gene-selector.js).
viewerUpdateGeneSelector <- function(session, inputId, choices, selected) {
  session$sendInputMessage(
    inputId,
    Filter(
      Negate(is.null),
      list(
        value = unname(selected),
        cerebro_gene_choices = viewerGeneChoicesUrl(session, choices)
      )
    )
  )
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
    choice_values <- choices()
    genes <- .viewer_sorted_gene_choices(
      list(choice_values),
      function() sort(choice_values)
    )
    req(!is.null(genes), length(genes) > 0)

    initial_selection <- isolate(input[[input_id]])
    send_update <- function(retry_update = FALSE) {
      ## later() callbacks outlive a closed session. Reading its reactives then
      ## raises outside any session and stops the whole app.
      if (isTRUE(tryCatch(session$isClosed(), error = function(error) FALSE))) {
        return(invisible(NULL))
      }
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
      viewerUpdateGeneSelector(session, input_id, genes, selected)
      if (!isTRUE(refresh_on_activation)) registered_dataset <<- dataset
    }

    ## Dynamic renderUI() + a selector's input message race on the
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
      ## This is the only registration for the dataset. Skipping it when the
      ## selection moved (e.g. an RGB seed) would leave the control searching
      ## the previous dataset; send_update() already uses the current value.
      later::later(function() send_update(), delay = 0.3)
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

## The rows (1-based) of a base64 bit set from the browser: bit r of byte
## r %/% 8 stands for row r + 1 (selectionRowBits() in cell_views.js).
viewerDecodeRowBits <- function(bits) {
  if (!.runtimeIsString(bits)) {
    return(NULL)
  }
  raw <- tryCatch(jsonlite::base64_dec(bits), error = function(e) NULL)
  if (!length(raw)) {
    return(NULL)
  }
  rows <- which(as.logical(rawToBits(raw)))
  if (length(rows)) rows else NULL
}

## TRUE when the browser draws `part` of the selection of view `id` itself
## ("summary", "table", "expression"; Linked views: "plot", "table"): it
## reported the selection by row with `client_<part>` set and holds the
## columns to draw it (see reportSelection() in cell_views.js).
viewerClientSelection <- function(
  id,
  part = "summary",
  input_id = paste0(id, "_persistent_selection")
) {
  selection <- input[[input_id]]
  is.list(selection) &&
    isTRUE(selection[[paste0("client_", part)]]) &&
    viewerDatasetContextEqual(
      selection[["dataset_context"]],
      viewer_loaded_dataset_context()
    )
}

## TRUE while view `id` has a selection. It invalidates only when that
## changes, so the boxes of selection results are not rebuilt (nor the
## selection decoded) for each new selection.
viewerSelectionPresence <- function(
  id,
  input_id = paste0(id, "_persistent_selection")
) {
  present <- shiny::reactiveVal(FALSE)
  shiny::observe({
    selection <- input[[input_id]]
    now <- is.list(selection) &&
      viewerDatasetContextEqual(
        selection[["dataset_context"]],
        viewer_loaded_dataset_context()
      ) &&
      (nzchar(selection[["rows_bits"]] %||% "") ||
        nzchar(selection[["indices_bits"]] %||% "") ||
        length(selection[["indices"]]) > 0L ||
        length(selection[["ids"]]) > 0L ||
        length(selection[["x"]]) > 0L)
    present(isTRUE(now))
  })
  present
}

## The count (`count_id`) and composition (`<id>_composition`) of the cells
## `selection()` holds, unless the browser draws them; the count first, as it
## is the cheapest of the selection results. `source()` names the selected
## space, `color()` the variable the cells are coloured by.
viewerSelectionSummaryOutputs <- function(
  id,
  count_id,
  selection,
  source,
  color = function() NULL,
  require_cells = FALSE
) {
  summary <- function(composition) {
    renderUI({
      if (viewerClientSelection(id)) {
        return(NULL)
      }
      cells <- selection()
      if (require_cells) {
        req(NROW(cells) > 0L)
      }
      cerebroSelectionSummary(
        cells,
        source(),
        color(),
        composition = composition
      )
    })
  }
  output[[count_id]] <<- summary(FALSE)
  output[[paste0(id, "_composition")]] <<- summary(TRUE)
  if (inherits(output, "shinyoutput")) {
    outputOptions(output, count_id, priority = 10)
  }
}

## The canonical rows of a selection reported by row, else NULL.
viewerSelectionRows <- function(selection) {
  if (is.data.frame(selection) && "row" %in% colnames(selection)) {
    as.integer(selection[["row"]])
  } else {
    NULL
  }
}

## Which of `rows` (canonical rows, NA allowed) are among `selected`.
viewerRowMask <- function(selected, rows) {
  n <- max(c(0L, selected, rows), na.rm = TRUE)
  lookup <- logical(n)
  lookup[selected] <- TRUE
  mask <- lookup[rows]
  mask[is.na(mask)] <- FALSE
  mask
}

## The cell-name resource when it names every one of `rows` (canonical rows),
## else NULL.
viewerCellNamesResourceFor <- function(rows) {
  resource <- viewerCellNamesResource()
  if (
    is.list(resource) &&
      length(rows) &&
      !anyNA(rows) &&
      max(rows) <= resource$cells
  ) {
    resource
  }
}

## The cell names of canonical rows. They come from the Viewer Pack when there
## is one, so a thin CRB is not hydrated for them; the names read from it are
## kept for that pack, not read again for every table.
.viewer_pack_cell_names <- new.env(parent = emptyenv())
## Every cell name in canonical order from the Viewer Pack, or NULL.
viewerPackCellNames <- function() {
  pack <- viewerPackCurrent()
  if (!is.list(pack)) {
    return(NULL)
  }
  cache <- .viewer_pack_cell_names
  if (!identical(cache$pack, pack)) {
    cache$pack <- pack
    cache$cells <- viewerPackCellBarcodes(pack)
  }
  cache$cells
}
viewerCellBarcodesAt <- function(rows) {
  cells <- viewerPackCellNames()
  barcodes <- if (
    !is.null(cells) &&
      !anyNA(rows) &&
      all(rows >= 1L & rows <= length(cells))
  ) {
    cells[rows]
  }
  barcodes %||% as.character(getMetaData()[["cell_barcode"]][rows])
}

## A selection of named cells as their canonical rows (a selection by row),
## from the Viewer Pack names: the thin CRB is not hydrated, and the hash is
## built over the selected names rather than over every cell. NULL without a
## Viewer Pack.
viewerSelectionRowsForNames <- function(names) {
  cells <- viewerPackCellNames()
  if (is.null(cells)) {
    return(NULL)
  }
  data.frame(row = which(cells %in% as.character(names)))
}

## Metadata of canonical rows, cell names first, for a table of selected cells.
viewerSelectedRowsMetadata <- function(rows) {
  metadata <- viewerProjectionFirstFrameMetadata()
  rows <- rows[!is.na(rows) & rows >= 1L & rows <= nrow(metadata)]
  table <- metadata[rows, , drop = FALSE]
  if (!"cell_barcode" %in% colnames(table)) {
    table <- data.frame(
      cell_barcode = viewerCellBarcodesAt(rows),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  rownames(table) <- NULL
  table[,
    c("cell_barcode", setdiff(colnames(table), "cell_barcode")),
    drop = FALSE
  ]
}

viewerPersistentSelection <- function(selection, dataset_context) {
  if (
    !is.list(selection) ||
      !viewerDatasetContextEqual(
        selection[["dataset_context"]],
        dataset_context
      )
  ) {
    return(NULL)
  }
  ## Canonical rows, when the browser knows them: no names to match.
  rows <- viewerDecodeRowBits(selection[["rows_bits"]])
  if (!is.null(rows)) {
    return(data.frame(row = rows))
  }
  ## Stable cell IDs identify a selection on their own. Coordinate identifiers
  ## are only the fallback for cells without one; formatting them for every
  ## selected cell took seconds for large selections.
  ids <- as.character(unlist(selection[["ids"]], use.names = FALSE))
  complete_ids <- length(ids) &&
    !anyNA(ids) &&
    all(nzchar(ids)) &&
    (!length(selection[["x"]]) || length(ids) == length(selection[["x"]]))
  if (complete_ids) {
    return(data.frame(selection_key = ids, stringsAsFactors = FALSE))
  }
  if (!length(selection[["x"]])) {
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
  coordinate_keys = FALSE,
  rows = NULL
) {
  selected_rows <- viewerSelectionRows(selection)
  if (
    !is.null(selected_rows) &&
      !is.null(rows) &&
      color_variable %in% colnames(metadata)
  ) {
    ## By row: only the compared column, no cell names or coordinates. Built
    ## as a list: data.frame() would check a million row names.
    values <- metadata[[color_variable]]
    if (length(values) != length(rows)) {
      values <- values[rows]
    }
    cells_df <- list(
      group = structure(
        2L - viewerRowMask(selected_rows, rows),
        levels = c("selected", "not selected"),
        class = "factor"
      ),
      values
    )
    names(cells_df) <- c("group", color_variable)
    return(structure(
      cells_df,
      class = "data.frame",
      row.names = c(NA_integer_, -length(rows))
    ))
  }
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

## `rows`: the canonical rows of the cells, for a selection reported by row.
selectedCellMask <- function(
  selection_key,
  identifier,
  selection,
  rows = NULL
) {
  selected_rows <- viewerSelectionRows(selection)
  if (!is.null(selected_rows)) {
    if (is.null(rows)) {
      return(rep(FALSE, length(selection_key)))
    }
    return(viewerRowMask(selected_rows, rows))
  }
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

## Help for the "Automatically format numbers" and "Highlight values with
## colours" switches of the prettified tables; ends every table info box.
cerebroTableOptionsHelp <- function() {
  "
    <b>Automatically format numbers</b><br>
    When active, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <b>Highlight values with colours</b><br>
    Similar to the automatic formatting option, when active, CerebroNexus will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colours and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colours and/or the identification does not work properly, you can simply deactivate this feature.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
}

cerebroSelectedCellsTableInfo <- function() {
  list(
    title = "Details of selected cells",
    text = shiny::HTML(paste0(
      "
      Table containing meta data (some columns may be hidden, check the 'Column visibility' button) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.
      <h4>Options</h4>",
      cerebroTableOptionsHelp()
    ))
  )
}

cerebroSelectedCellsPlotInfo <- function() {
  list(
    title = "Plot of selected cells",
    text = shiny::p(
      "For categorical colours, bars show the groups in the selected cells. For continuous colours, violin/box plots compare selected and unselected cells."
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
      cerebroChartOutput(paste0(prefix, "_details_selected_cells_plot"))
    )
  ))
}

## `rows`: the canonical rows the table may list (NULL: every cell). A
## selection reported by row builds only its own rows; `cells_df` is then not
## evaluated, so its full projection and metadata are never put together.
cerebroSelectedCellsTable <- function(
  cells_df,
  empty_data,
  selection,
  fallback_key = c("row", "identifier"),
  number_formatting = TRUE,
  color_highlighting = TRUE,
  download_file_name,
  rows = NULL
) {
  empty_table <- function() {
    prepareEmptyTable(empty_data[0, , drop = FALSE])
  }
  if (is.null(selection)) {
    return(empty_table())
  }
  selected_rows <- viewerSelectionRows(selection)
  if (!is.null(selected_rows)) {
    if (!is.null(rows)) {
      selected_rows <- selected_rows[viewerRowMask(rows, selected_rows)]
    }
    table <- viewerSelectedRowsMetadata(selected_rows)
    if (!nrow(table)) {
      return(empty_table())
    }
    return(prettifyTable(
      table,
      filter = list(position = "top", clear = TRUE),
      dom = "Brtlip",
      show_buttons = TRUE,
      number_formatting = number_formatting,
      color_highlighting = color_highlighting,
      hide_long_columns = TRUE,
      download_file_name = download_file_name
    ))
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

## `rows`: the canonical rows of the cells compared (NULL: every cell). A
## selection reported by row reads only the compared column of those rows.
cerebroSelectedCellsPlot <- function(
  cells_df,
  selection,
  color_variable,
  fallback_key = c("row", "identifier"),
  rows = NULL
) {
  fallback_key <- match.arg(fallback_key)
  metadata <- if (!is.null(viewerSelectionRows(selection))) {
    viewerProjectionFirstFrameMetadata()
  }
  cells_df <- if (
    !is.null(metadata) && color_variable %in% colnames(metadata)
  ) {
    if (is.null(rows)) {
      rows <- seq_len(nrow(metadata))
    }
    prepareSelectedCellDistribution(
      NULL,
      metadata,
      selection,
      color_variable,
      rows = rows
    )
  } else {
    prepareSelectedCellDistribution(
      cells_df[, 1:2, drop = FALSE],
      cells_df[, -(1:2), drop = FALSE],
      selection,
      color_variable,
      coordinate_keys = identical(fallback_key, "identifier")
    )
  }

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
    cerebroCountBarData(
      cells_df[[1L]],
      cells_df[[2L]],
      assignColorsToGroups(cells_df, color_variable)
    )
  } else {
    cerebroViolinData(
      cells_df[, c("group", color_variable), drop = FALSE],
      color_variable,
      "group",
      cerebroSelectionColors(),
      color_variable,
      "number"
    )
  }
}

## Colours of the "selected" / "not selected" violins.
cerebroSelectionColors <- function() {
  c(selected = "#e74c3c", `not selected` = "#7f8c8d")
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

registerCellViewPointBorder <- function(
  output,
  prefix,
  defaults = function() list(draw_border = FALSE, keep_square = FALSE)
) {
  output_id <- paste0(prefix, "_point_border_UI")
  output[[output_id]] <- shiny::renderUI({
    shiny::checkboxInput(
      inputId = paste0(prefix, "_point_border"),
      label = "Draw border around cells",
      value = defaults()$draw_border
    )
  })
  shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
  square_id <- paste0(prefix, "_keep_square_UI")
  output[[square_id]] <- shiny::renderUI({
    shiny::checkboxInput(
      inputId = paste0(prefix, "_keep_square"),
      label = "Keep plots square",
      value = defaults()$keep_square
    )
  })
  shiny::outputOptions(output, square_id, suspendWhenHidden = FALSE)
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
  columns_percentage = NULL,
  filter = list(position = "top", clear = TRUE)
) {
  if (!nrow(results_df)) {
    return(prepareEmptyTable(as.data.frame(results_df)[0, , drop = FALSE]))
  }
  prettifyTable(
    results_df,
    filter = filter,
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
          "paga",
          "scvelo",
          "pseudotime",
          "marker_guided",
          "illustrative",
          "published_pseudotime"
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

## An SVG is drawn as an image, which cannot run script. The session route also
## serves it as a document, so it carries a sandboxing Content-Security-Policy;
## this check only turns away the obvious cases (script, foreignObject, event
## handler, javascript: URL, entity declaration) and is not the guard.
viewerSvgIsPlain <- function(path, maximum_bytes = 20 * 1024^2) {
  size <- file.size(path)
  if (is.na(size) || size < 1 || size > maximum_bytes) {
    return(FALSE)
  }
  text <- tryCatch(
    rawToChar(readBin(path, what = "raw", n = size)),
    error = function(error) NA_character_
  )
  if (is.na(text)) {
    return(FALSE)
  }
  grepl("<svg[[:space:]>/]", substr(text, 1L, 4096L), useBytes = TRUE) &&
    !grepl(
      "<script|<foreignObject|<!ENTITY|javascript:|[[:space:]\"']on[a-z]+[[:space:]]*=",
      text,
      ignore.case = TRUE,
      perl = TRUE,
      useBytes = TRUE
    )
}

viewerImageMime <- function(path) {
  if (!isTRUE(file_test("-f", path))) {
    return(NULL)
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("png", "jpg", "jpeg", "svg")) {
    return(NULL)
  }
  if (identical(ext, "svg")) {
    return(if (viewerSvgIsPlain(path)) "image/svg+xml" else NULL)
  }
  bytes <- tryCatch(
    readBin(path, what = "raw", n = 8L),
    error = function(error) raw()
  )
  png_magic <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  jpeg_magic <- as.raw(c(0xff, 0xd8, 0xff))
  if (identical(ext, "png") && identical(bytes, png_magic)) {
    return("image/png")
  }
  if (
    ext %in%
      c("jpg", "jpeg") &&
      length(bytes) >= length(jpeg_magic) &&
      identical(bytes[seq_along(jpeg_magic)], jpeg_magic)
  ) {
    return("image/jpeg")
  }
  NULL
}

# Serve trusted image bytes through a session-scoped route. Filesystem paths and
# Base64 payloads never enter the browser response model.

## Embedded Spatial images are data URIs carried inside every frame (tens of MB
## for large images). Decode each once per session into a private file and send
## its URL instead; the browser fetches and caches it once. Formats the private
## image route does not serve keep their data URI.
viewerEmbeddedImageUrl <- function(data_uri, session = NULL) {
  if (is.null(session)) {
    session <- get0("session", envir = parent.frame(), inherits = TRUE)
  }
  if (
    !is.character(data_uri) ||
      length(data_uri) != 1L ||
      is.na(data_uri) ||
      is.null(session) ||
      !is.environment(session$userData)
  ) {
    return(data_uri)
  }
  header <- regmatches(
    substr(data_uri, 1L, 40L),
    regexec("^data:image/(png|jpeg|jpg);base64,", substr(data_uri, 1L, 40L))
  )[[1L]]
  if (!length(header)) {
    return(data_uri)
  }
  if (is.null(session$userData$cerebro_embedded_image_urls)) {
    session$userData$cerebro_embedded_image_urls <- new.env(parent = emptyenv())
  }
  cache <- session$userData$cerebro_embedded_image_urls
  key <- rlang::hash(data_uri)
  url <- get0(key, envir = cache, inherits = FALSE)
  if (!is.null(url)) {
    return(url)
  }
  file <- tempfile(
    "cerebro-embedded-",
    fileext = if (identical(header[[2L]], "png")) ".png" else ".jpg"
  )
  written <- tryCatch(
    {
      ## substring() stops at 1,000,000 characters by default, which cut
      ## images above ~750 KB short; pass the end explicitly.
      writeBin(
        base64enc::base64decode(
          substring(data_uri, nchar(header[[1L]]) + 1L, nchar(data_uri))
        ),
        file
      )
      TRUE
    },
    error = function(error) FALSE
  )
  url <- if (written) viewerPrivateImageUrl(file, session = session) else NULL
  if (is.null(url)) {
    unlink(file)
    return(data_uri)
  }
  assign(key, url, envir = cache)
  if (is.function(session$onSessionEnded)) {
    session$onSessionEnded(function() unlink(file))
  }
  url
}

## Size and modification time of one regular file, or NULL. A registered image
## is served only while both still match the version that was hashed.
viewerFileVersion <- function(path) {
  info <- suppressWarnings(file.info(path))
  if (nrow(info) != 1L || isTRUE(info$isdir[[1L]])) {
    return(NULL)
  }
  bytes <- unname(as.numeric(info$size[[1L]]))
  modified <- unname(as.numeric(info$mtime[[1L]]))
  if (!is.finite(bytes) || !is.finite(modified)) {
    return(NULL)
  }
  list(bytes = bytes, modified = modified)
}

viewerPrivateImageUrl <- function(path, session = NULL, expected_md5 = NULL) {
  if (is.null(session)) {
    session <- get0("session", envir = parent.frame(), inherits = TRUE)
  }
  if (is.null(session) || !is.function(session$registerDataObj)) {
    return(NULL)
  }
  path <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  mime <- if (is.null(path)) NULL else viewerImageMime(path)
  if (is.null(path) || is.null(mime)) {
    return(NULL)
  }
  version <- viewerFileVersion(path)
  if (is.null(version)) {
    return(NULL)
  }
  bytes <- version$bytes
  modified <- version$modified
  ## Background frames resend this descriptor; hash a file version once.
  if (is.null(session$userData$cerebro_private_image_md5)) {
    session$userData$cerebro_private_image_md5 <- new.env(parent = emptyenv())
  }
  md5_cache <- session$userData$cerebro_private_image_md5
  md5_key <- paste(path, bytes, format(modified, digits = 17), sep = "\r")
  md5 <- get0(md5_key, envir = md5_cache, inherits = FALSE)
  if (is.null(md5)) {
    md5 <- tryCatch(
      unname(as.character(tools::md5sum(path))),
      error = function(error) NA_character_
    )
    if (!is.na(md5)) assign(md5_key, md5, envir = md5_cache)
  }
  if (
    is.na(md5) ||
      (!is.null(expected_md5) && !identical(as.character(expected_md5), md5))
  ) {
    return(NULL)
  }
  if (is.null(session$userData$cerebro_private_image_urls)) {
    session$userData$cerebro_private_image_urls <- new.env(parent = emptyenv())
  }
  cache <- session$userData$cerebro_private_image_urls
  cached <- get0(md5, envir = cache, inherits = FALSE)
  if (!is.null(cached)) {
    return(cached)
  }
  data <- list(
    path = path,
    mime = mime,
    md5 = md5,
    bytes = bytes,
    modified = modified
  )
  url <- session$registerDataObj(
    paste0("cerebro-image-", md5),
    data,
    function(data, request) {
      current <- viewerFileVersion(data$path)
      if (
        is.null(current) ||
          !identical(current$bytes, data$bytes) ||
          !identical(current$modified, data$modified)
      ) {
        return(shiny::httpResponse(404L, "text/plain", "Not found"))
      }
      shiny::httpResponse(
        200L,
        data$mime,
        list(file = data$path, owned = FALSE),
        headers = list(
          "Cache-Control" = "private, max-age=31536000, immutable",
          "ETag" = paste0('"', data$md5, '"'),
          "X-Content-Type-Options" = "nosniff",
          ## An SVG opened as a document must not run script, whatever its
          ## markup; as an image it is unaffected.
          "Content-Security-Policy" = paste(
            "default-src 'none'; style-src 'unsafe-inline'; sandbox"
          )
        )
      )
    }
  )
  assign(md5, url, envir = cache)
  url
}

viewerTrekkerExternalImage <- function() {
  if (!exists("Cerebro.options", inherits = TRUE)) {
    return(NULL)
  }
  options <- get("Cerebro.options", inherits = TRUE)
  dataset <- viewerSelectedDatasetName()
  images <- options[["spatial_images"]]
  if (
    is.null(dataset) ||
      !is.list(images) ||
      !dataset %in% names(images) ||
      !is.list(images[[dataset]])
  ) {
    return(NULL)
  }
  configured <- images[[dataset]][["trekker"]]
  if (!is.list(configured) || !length(configured)) {
    return(NULL)
  }
  configured_names <- names(configured)
  descriptor <- configured[[1L]]
  relative <- if (is.list(descriptor)) descriptor$path else descriptor
  label <- if (
    !is.null(configured_names) &&
      length(configured_names) >= 1L &&
      !is.na(configured_names[[1L]]) &&
      nzchar(configured_names[[1L]])
  ) {
    configured_names[[1L]]
  } else if (is.character(relative) && length(relative) == 1L) {
    basename(relative)
  } else {
    "Trekker background"
  }
  root <- options[["cerebro_root"]]
  path <- authorized_spatial_image_path(relative, relative, root)
  if (is.null(path)) {
    return(NULL)
  }
  url <- viewerPrivateImageUrl(path)
  if (is.null(url)) {
    return(NULL)
  }
  list(
    uri = url,
    bounds = if (is.list(descriptor)) descriptor$bounds else NULL,
    viewport_bounds = if (is.list(descriptor)) {
      descriptor$viewport_bounds
    } else {
      NULL
    },
    preset = spatialImagePreset(options, dataset, "trekker", label),
    label = if (is.list(descriptor)) descriptor$label %||% label else label
  )
}

## The Trekker background shared by the Trekker page and Linked views: a
## configured external image wins over the image embedded in the Trekker slot.
viewerTrekkerBackground <- function(tk) {
  external <- viewerTrekkerExternalImage()
  list(
    external = external,
    uri = external$uri %||% tk[["histology_image", exact = TRUE]],
    bounds = external$bounds %||% tk[["histology_image_bounds", exact = TRUE]]
  )
}

## Server side of a stored-result page (Marker genes, Enriched pathways): a
## method/grouping selector pair, the table-or-message shell and the table.
## Getters and messages are arguments, so each page passes its own and tests
## can stub them. `statuses` maps a stored status string to its message;
## `progress` wraps the table in withProgress; `columns_hide(df)` returns the
## columns hidden by default.
cerebroResultPageServer <- function(
  input,
  output,
  session,
  prefix,
  methods,
  groups,
  results,
  messages,
  statuses = list(),
  progress = NULL,
  columns_hide = function(results_df) NULL
) {
  id <- function(suffix) paste0(prefix, "_", suffix)
  method_id <- id("selected_method")
  table_id <- id("selected_table")
  ## Resolve both initial selectors together. Changing the method updates only
  ## the group choices, so the surrounding controls do not disappear and rebind.
  output[[id("select_method_and_table_UI")]] <- shiny::renderUI({
    available <- methods()
    if (!length(available)) {
      return(shiny::p(messages$no_methods))
    }
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(
          method_id,
          label = "Method",
          choices = available,
          selected = available[[1L]],
          width = "100%"
        )
      ),
      shiny::column(
        6,
        shiny::selectInput(
          table_id,
          label = "Grouping variable",
          choices = groups(available[[1L]]),
          width = "100%"
        )
      )
    )
  })
  shiny::observeEvent(
    input[[method_id]],
    {
      method <- input[[method_id]]
      shiny::req(method %in% methods())
      choices <- groups(method)
      current <- shiny::isolate(input[[table_id]])
      selected <- if (length(current) == 1L && current %in% choices) {
        current
      } else if (length(choices)) {
        choices[[1L]]
      } else {
        character()
      }
      shiny::updateSelectInput(
        session,
        table_id,
        choices = choices,
        selected = selected
      )
    },
    ignoreInit = TRUE
  )
  ## The stored table for the selected method and grouping; shared by the
  ## shell and the table so a selection reads it once.
  selected_result <- shiny::reactive({
    shiny::req(
      input[[method_id]],
      input[[table_id]],
      input[[method_id]] %in% methods(),
      input[[table_id]] %in% groups(input[[method_id]])
    )
    results(input[[method_id]], input[[table_id]])
  })
  ## Table controls, or a message when the data set has no such results.
  output[[id("table_or_text_UI")]] <- shiny::renderUI({
    available <- methods()
    if (!length(available)) {
      return(shiny::p(messages$not_included))
    }
    method <- input[[method_id]]
    shiny::req(method %in% available)
    if (!length(groups(method))) {
      return(shiny::p(messages$no_groups))
    }
    result <- selected_result()
    if (is.data.frame(result)) {
      cerebroResultTableControls(
        prefix,
        result,
        colnames(result)[[1L]] %in% getGroups()
      )
    } else if (
      is.character(result) &&
        length(result) == 1L &&
        result %in% names(statuses)
    ) {
      shiny::p(statuses[[result]])
    } else {
      shiny::p(messages$no_results)
    }
  })
  output[[id("table")]] <- DT::renderDataTable({
    shiny::req(
      input[[method_id]],
      input[[table_id]],
      input[[table_id]] %in% groups(input[[method_id]])
    )
    step <- if (is.null(progress)) function(...) NULL else shiny::incProgress
    render <- function() {
      results_df <- selected_result()
      shiny::req(is.data.frame(results_df))
      step(0.3, detail = "Filtering data...")
      ## Filter for one subgroup only if the user asked for it.
      grouped <- colnames(results_df)[[1L]] %in% getGroups()
      if (!isTRUE(input[[id("table_filter_switch")]]) && grouped) {
        shiny::req(input[[id("table_select_group_level")]])
      }
      results_df <- cerebroFilterResultRows(
        results_df,
        input[[id("table_filter_switch")]],
        grouped,
        input[[id("table_select_group_level")]]
      )
      step(0.6, detail = "Rendering table...")
      cerebroResultTable(
        results_df,
        paste0(
          prefix,
          "_by_",
          input[[method_id]],
          "_",
          input[[table_id]]
        ),
        number_formatting = input[[id("table_number_formatting")]],
        color_highlighting = input[[id("table_color_highlighting")]],
        columns_hide = columns_hide(results_df)
      )
    }
    if (is.null(progress)) {
      render()
    } else {
      shiny::withProgress(message = progress, value = 0, render())
    }
  })
  invisible(NULL)
}

## Drop selected cells whose group the legend hides, so counts and the
## selected-cells panels reflect visible groups only. `hidden_request` is a
## page's <plot_id>_hidden_groups input; a request for another data set is
## ignored. `metadata_fn()` returns the plotted cells and is called only when a
## group is hidden; its `coordinates` columns key cells for selections without
## a stable selection_key. Returns NULL when no selected cell is left.
viewerVisibleSelection <- function(
  selection,
  hidden_request,
  dataset_context,
  color_variable,
  metadata_fn,
  coordinates = 1:2
) {
  hidden_groups <- if (
    is.list(hidden_request) &&
      viewerDatasetContextEqual(
        hidden_request[["dataset_context"]],
        dataset_context
      )
  ) {
    as.character(hidden_request[["groups"]] %||% character())
  } else {
    character()
  }
  if (!length(hidden_groups)) {
    return(selection)
  }
  selected_rows <- viewerSelectionRows(selection)
  if (!is.null(selected_rows)) {
    ## By row: the group of each selected row, from the first-frame metadata.
    metadata <- viewerProjectionFirstFrameMetadata()
    if (is.null(color_variable) || !color_variable %in% colnames(metadata)) {
      return(selection)
    }
    groups <- metadata[[color_variable]]
    hidden <- if (is.factor(groups)) {
      (levels(groups) %in% hidden_groups)[as.integer(groups[selected_rows])]
    } else {
      groups[selected_rows] %in% hidden_groups
    }
    hidden[is.na(hidden)] <- NA %in% hidden_groups
    selection <- selection[!hidden, , drop = FALSE]
    return(if (nrow(selection) == 0) NULL else selection)
  }
  metadata <- metadata_fn()
  metadata[["selection_key"]] <- if ("cell_barcode" %in% colnames(metadata)) {
    as.character(metadata[["cell_barcode"]])
  } else {
    NULL
  }
  stable_selection <- "selection_key" %in%
    colnames(metadata) &&
    "selection_key" %in% colnames(selection) &&
    any(
      !is.na(selection[["selection_key"]]) &
        nzchar(as.character(selection[["selection_key"]]))
    )
  if (!stable_selection) {
    metadata[["identifier"]] <- paste0(
      metadata[[coordinates[[1L]]]],
      "-",
      metadata[[coordinates[[2L]]]]
    )
  }
  selection <- filterSelectionByHiddenGroups(
    selection,
    metadata,
    color_variable,
    hidden_groups
  )
  if (is.null(selection) || nrow(selection) == 0) NULL else selection
}
