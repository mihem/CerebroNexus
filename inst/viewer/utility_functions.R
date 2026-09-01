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

viewerDatasetName <- function(files, selected) {
  if (is.null(files) || is.null(selected) || is.null(names(files))) {
    return(NULL)
  }
  index <- which(files == selected)
  if (!length(index)) {
    return(NULL)
  }
  name <- names(files)[[index[[1L]]]]
  if (is.na(name) || !nzchar(name)) NULL else name
}

viewerScatterDefaults <- function(options, dataset = NULL) {
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
  list(
    point_size = resolve("point_size", 5, 1, 20),
    point_opacity = resolve("point_opacity", 1, 0.1, 1),
    percentage_cells_to_show = resolve(
      "percentage_cells_to_show",
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
  if (utils::packageVersion("shiny") >= "1.6.0") {
    keys <- rlang::enquos(...)
    rlang::inject(
      shiny::bindCache(x, !!!keys, cache = "session")
    )
  } else {
    x
  }
}

##----------------------------------------------------------------------------##
## Canvas projection UIs are created by module files sourced inside server().
## Keep this helper in the same per-session scope so a newly opened session does
## not depend on the process-startup shiny_UI.R environment being reloaded.
cerebroCellViewOutput <- function(id) {
  div(
    id = paste0(id, "_cell_view_host"),
    class = "coordviews-page cerebro-cell-view-host",
    `data-cell-view-id` = id,
    div(
      class = "cerebro-cell-view-surface",
      `aria-live` = "polite"
    ),
    shiny::uiOutput(
      paste0(id, "_composition"),
      class = "cerebro-selection-composition-slot"
    )
  )
}

cerebroCellViewRender <- function(
  id,
  meta,
  data,
  hover = list(),
  extra = list()
) {
  session$sendCustomMessage(
    "cell_view_render",
    list(
      id = id,
      meta = meta,
      data = data,
      hover = hover,
      extra = extra
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
  hover = TRUE,
  point_line = list(),
  x_range = list(),
  y_range = list(),
  reset_axes = FALSE,
  n_dimensions = 2L
) {
  dimensions <- if (as.integer(n_dimensions) == 3L) 3L else 2L
  if (length(coordinates) < dimensions) {
    stop("coordinates do not contain the requested dimensions")
  }
  cell_counts <- c(
    vapply(coordinates[seq_len(dimensions)], length, integer(1)),
    color = length(color),
    selection_keys = length(selection_keys)
  )
  if (length(unique(cell_counts)) != 1L) {
    stop(
      "coordinates, color, and selection_keys must describe the same number of cells"
    )
  }

  continuous <- is.numeric(color)
  has_z <- as.integer(n_dimensions) == 3L && length(coordinates) >= 3L
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
  data <- list(
    x = if (continuous) I(coordinates[[1L]]) else list(),
    y = if (continuous) I(coordinates[[2L]]) else list(),
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
  hover_data <- list(
    hoverinfo = if (show_hover) "text" else "skip",
    text = if (continuous && show_hover) I(unname(hover_info)) else list()
  )
  if (continuous) {
    return(list(meta = meta, data = data, hover = hover_data))
  }
  if (is.null(color_assignments)) {
    stop("color_assignments are required for categorical cell views")
  }
  color <- as.character(color)
  color[is.na(color)] <- "(missing)"
  levels_in_view <- unique(color)
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
    data[["color"]][[index]] <- I(rep(
      unname(color_assignments[[group]]),
      length(cells)
    ))
    if (show_hover) {
      hover_data[["text"]][[index]] <- I(aligned_hover[cells])
    }
    index <- index + 1L
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
        shiny::tags$strong("Composition"),
        shiny::tags$span(paste0(
          "Selected ",
          formatC(n_selected, format = "f", big.mark = ",", digits = 0),
          " / ",
          formatC(total, format = "f", big.mark = ",", digits = 0),
          " ",
          unit
        ))
      ),
      shiny::tags$div(
        class = "cerebro-selection-composition-sub",
        paste("by", comp_group)
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
prettifyTable <- function(
  table,
  filter,
  dom,
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

  ## replace Inf and -Inf values in numeric columns with 999 or -999,
  ## respectively, because other the columns will be converted to characters
  ## which messes up sorting of values in that column
  table <- table %>%
    dplyr::mutate_if(is.numeric, function(x) ifelse(x == Inf, 999, x)) %>%
    dplyr::mutate_if(is.numeric, function(x) ifelse(x == -Inf, -999, x))

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
  columns_integer <- findColumnsInteger(table, columns_numeric)

  ## identify which columns might contain percentages, p-values, and logFC
  columns_percent <- findColumnsPercentage(table)
  columns_p_value <- findColumnsPValues(table)
  columns_logFC <- findColumnsLogFC(table)

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
    autoHideNavigation = TRUE,
    class = "stripe table-bordered table-condensed",
    escape = FALSE,
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
      scrollX = TRUE
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
    ## custom.css --chart-* tokens and cerebro_plotly_theme()), so the table
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
    autoHideNavigation = TRUE,
    class = "stripe table-bordered table-condensed",
    escape = FALSE,
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
buildHoverInfoForProjections <- function(table) {
  ## put together cell ID, number of transcripts and number of expressed genes
  hover_info <- glue::glue(
    "<b>Cell</b>: {table[[ 'cell_barcode' ]]}<br>",
    "<b>Transcripts</b>: {formatC(table[[ 'nUMI' ]], format = 'f', big.mark = ',', digits = 0)}<br>",
    "<b>Expressed genes</b>: {formatC(table[[ 'nGene' ]], format = 'f', big.mark = ',', digits = 0)}"
  )
  ## add info for known grouping variables
  for (group in getGroups()) {
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
## Merge a trajectory's per-cell meta data (DR_1/DR_2/pseudotime/state) with the
## data set's full meta data, aligned BY CELL BARCODE.
##
## A trajectory may cover only a SUBSET of cells (e.g. a monocle2 trajectory
## computed on B cells only). The trajectory meta data frame therefore has fewer
## rows than getMetaData(), and its rownames are the covered cells' barcodes. A
## positional `cbind()` would crash ("differing number of rows") or, worse,
## silently mis-align cells. This joins on the barcode so every cell keeps its
## own coordinates and cells outside the trajectory get NA pseudotime (which the
## callers then drop via `filter(!is.na(pseudotime))`). The full meta data is the
## left side, so the result has one row per cell in getMetaData() order.
##----------------------------------------------------------------------------##
mergeTrajectoryWithMetaData <- function(trajectory_data) {
  trajectory_meta <- trajectory_data[["meta"]]
  trajectory_meta[["cell_barcode"]] <- rownames(trajectory_meta)
  getMetaData() %>%
    dplyr::left_join(trajectory_meta, by = "cell_barcode")
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

  ## - get list of gene set names
  ## - filter for selected gene set
  ## - extract genes that belong to the gene set
  ## - get orthologs for the genes
  ## - convert gene symbols to vector
  ## - only keep unique gene symbols
  ## - sort genes
  msigdbr:::msigdbr_genesets[, 1:2] %>%
    dplyr::filter(.data$gs_name == gene_set) %>%
    dplyr::inner_join(
      .,
      msigdbr:::msigdbr_genes,
      by = "gs_id"
    ) %>%
    dplyr::inner_join(
      .,
      msigdbr:::msigdbr_orthologs %>%
        dplyr::filter(.data$species_name == species) %>%
        dplyr::select(human_entrez_gene, gene_symbol),
      by = "human_entrez_gene"
    ) %>%
    dplyr::pull(gene_symbol) %>%
    unique() %>%
    sort()
}

##----------------------------------------------------------------------------##
## Function to calculate center of groups in projections/trajectories.
##----------------------------------------------------------------------------##
centerOfGroups <- function(coordinates, df, n_dimensions, group) {
  ## Guard against a missing grouping column: callers occasionally pass a
  ## group that isn't present in df (e.g. a metadata column dropped for a
  ## selected-cells slice), which would otherwise make df[[group]] NULL and
  ## crash the tibble construction. Return a typed empty result instead.
  if (is.null(group) || !group %in% colnames(df)) {
    return(tidyr::tibble(
      group = character(),
      x_median = numeric(),
      y_median = numeric(),
      z_median = numeric()
    ))
  }
  ## check number of dimenions in projection
  ## ... 2 dimensions
  if (n_dimensions == 2) {
    ## calculate center for groups and return
    tidyr::tibble(
      x = coordinates[[1]],
      y = coordinates[[2]],
      group = df[[group]]
    ) %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarise(
        x_median = median(x),
        y_median = median(y),
        .groups = 'drop_last'
      ) %>%
      dplyr::ungroup() %>%
      return()
    ## ... 3 dimensions
  } else if (n_dimensions == 3 && is.numeric(coordinates[, 3])) {
    ## calculate center for groups and return
    tidyr::tibble(
      x = coordinates[[1]],
      y = coordinates[[2]],
      z = coordinates[[3]],
      group = df[[group]]
    ) %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarise(
        x_median = median(x),
        y_median = median(y),
        z_median = median(z),
        .groups = 'drop_last'
      ) %>%
      dplyr::ungroup() %>%
      return()
  }
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
availableProjections <- function() {
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
      idx <- which(available_crb_files$files == available_crb_files$selected)
      if (length(idx) > 0 && !is.null(available_crb_files$names)) {
        current_name <- available_crb_files$names[idx[1]]
      }
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
## Cerebro file reader (.rds via readRDS).
##----------------------------------------------------------------------------##
read_cerebro_file <- function(file) {
  readRDS(file)
}

##----------------------------------------------------------------------------##
## Session-scoped cache for loaded .crb files (B8).
##
## Cerebro objects are treated as READ-ONLY within a session. Cache is keyed by
## file path and backend configuration. Changing the backend configuration for
## an already loaded path fails closed; overwriting a .crb in place is NOT
## detected -- start a new app session to pick up either change.
##----------------------------------------------------------------------------##
.crb_cache <- new.env(parent = emptyenv())

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
  cached <- .crb_cache[[path]]
  if (!is.null(cached)) {
    if (!identical(cached$backend_identity, cache_identity)) {
      stop(
        "The cached CRB '",
        path,
        "' backend configuration changed after it was loaded. Start a new ",
        "app session before using the new configuration.",
        call. = FALSE
      )
    }
    print(glue::glue("[{Sys.time()}] CRB cache hit: {.crbLogLabel(path)}"))
    return(cached$object)
  }
  print(glue::glue(
    "[{Sys.time()}] CRB cache miss, loading: {.crbLogLabel(path)}"
  ))
  obj <- read_cerebro_file(path)
  obj <- .attachExternalExpression(obj, path, effective_backend)
  .crb_cache[[path]] <- list(
    object = obj,
    backend_identity = cache_identity
  )
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
  configured <- !is.null(effective_backend)
  if (!configured) {
    be <- .fallbackRuntimeBackendPlan(obj, crb_path)
  } else {
    be <- .validateRuntimeBackendEntry(effective_backend, crb_path)
  }

  if (identical(be$mode, "embedded")) {
    return(obj)
  }

  if (identical(be$mode, "host_override")) {
    loc_abs <- be$location
  } else {
    crb_dir <- dirname(normalizePath(crb_path, mustWork = FALSE))
    loc_abs <- file.path(crb_dir, be$location)
  }

  if (be$type == "bpcells") {
    if (!requireNamespace("BPCells", quietly = TRUE)) {
      stop(
        "bpcells-backed crb requires the BPCells package; please install it.",
        call. = FALSE
      )
    }
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
    print(glue::glue("[{Sys.time()}] Attaching bpcells backend: {loc_abs}"))
    obj$expression <- BPCells::open_matrix_dir(dir = loc_abs)
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

## Wrapper functions for extra_material module.
getExtraMaterialCategories <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getExtraMaterialCategories())
  }
}
checkForExtraTables <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$checkForExtraTables())
  }
}
getNamesOfExtraTables <- function() {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getNamesOfExtraTables())
  }
}
getExtraTable <- function(name) {
  if (is_cerebro_dataset(data_set())) {
    return(data_set()$getExtraTable(name))
  }
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
  tryCatch(ds$getSpatialData(name), error = function(e) NULL)
}
serverSideGeneSelector <- function(
  session,
  input_id,
  extra_triggers = function() NULL,
  active = function() TRUE
) {
  observe({
    extra_triggers()
    ## The caller can gate this observer so it does nothing until its own tab is
    ## relevant. The spatial module registers this at module-source time, which
    ## also runs for datasets that carry no spatial data (e.g. the PBMC set); an
    ## ungated observer would then schedule later::later() callbacks that keep
    ## the app from ever reaching idle and break unrelated tabs' tests.
    req(isTRUE(active()))
    req(data_set())
    genes <- sort(getGeneNames())
    req(!is.null(genes), length(genes) > 0)

    send_update <- function() {
      updateSelectizeInput(
        session,
        input_id,
        choices = genes,
        selected = character(0),
        server = TRUE
      )
    }

    ## Dynamic renderUI() + updateSelectizeInput(server=TRUE) race on the
    ## client: the selectize binding initialises asynchronously, so an update
    ## message can arrive while the binding doesn't yet exist and gets silently
    ## dropped. onFlushed fires right after R's flush but before the browser
    ## has processed the DOM update, so it's necessary but not sufficient.
    ## Sending the same update again after small timed delays ensures at least
    ## one lands after the binding exists. The message is idempotent (same
    ## choices, no selection), so duplicate sends are harmless.
    session$onFlushed(send_update, once = TRUE)
    later::later(send_update, delay = 0.3)
    later::later(send_update, delay = 1.0)
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
## color_variable: name of the column the legend groups by (current "Color by").
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

selectedCellMask <- function(selection_key, identifier, selection) {
  if (length(selection_key) != length(identifier)) {
    stop("Selection identity vectors must have equal length.", call. = FALSE)
  }
  if (is.null(selection) || !is.data.frame(selection)) {
    return(rep(FALSE, length(identifier)))
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
  coordinates <- if ("identifier" %in% colnames(selection)) {
    as.character(selection[["identifier"]])
  } else {
    character()
  }
  as.character(identifier) %in% coordinates
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
