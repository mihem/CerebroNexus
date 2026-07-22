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
## Dynamic default point size for scatter/projection plots.
##
## Picks a sensible default marker size from how many points are drawn and how
## big the plot canvas is, so a dataset of 2k cells and one of 500k cells each
## start out readable instead of both defaulting to a fixed value that is too
## fat for the large one (a solid blob) or too thin for the small one (sparse).
##
##   - Point count (primary): size decreases logarithmically as points grow, so
##     dense plots don't smear into one mass and sparse plots stay visible.
##     Tuned so ~100 pts -> ~9, ~2.7k -> ~6, ~10k -> ~5, ~50k -> ~4, ~200k -> ~2
##     on the reference canvas.
##   - Canvas area (secondary): a larger plot can carry slightly larger points
##     (fills the space), a smaller one shrinks them. Correction is clamped so
##     it only nudges, never dominates.
##
## Returns a value already clamped to [min, max] and rounded to `step`. When the
## point count is unknown/invalid it returns `fallback` so callers can keep the
## old fixed default. Canvas dimensions are optional; omit them (NULL) to size
## on point count alone.
##----------------------------------------------------------------------------##
dynamicPointSize <- function(
  n_points,
  plot_width_px = NULL,
  plot_height_px = NULL,
  min = 1,
  max = 20,
  step = 1,
  fallback = 2
) {
  if (is.null(n_points) || !is.finite(n_points) || n_points < 1) {
    return(fallback)
  }

  ## primary: logarithmic falloff with point count
  base <- 13 - 2.0 * log10(n_points)

  ## secondary: gentle canvas-area correction relative to a ~900x700 reference
  scale <- 1
  if (
    !is.null(plot_width_px) &&
      !is.null(plot_height_px) &&
      is.finite(plot_width_px) &&
      is.finite(plot_height_px) &&
      plot_width_px > 0 &&
      plot_height_px > 0
  ) {
    ref_area <- 900 * 700
    area <- plot_width_px * plot_height_px
    scale <- sqrt(area / ref_area)
    scale <- max(0.75, min(1.35, scale))
  }

  sz <- base * scale
  sz <- max(min, min(max, sz))
  round(sz / step) * step
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
getExperiment <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getExperiment())
  }
}
getParameters <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getParameters())
  }
}
getTechnicalInfo <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getTechnicalInfo())
  }
}
getGeneLists <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getGeneLists())
  }
}
getGeneNames <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getGeneNames())
  }
}
getGroups <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getGroups())
  }
}
getGroupLevels <- function(group) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getGroupLevels(group))
  }
}
getCellCycle <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getCellCycle())
  }
}
getMetaData <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getMetaData())
  }
}
availableProjections <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$availableProjections())
  }
}
getProjection <- function(name) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getProjection(name))
  }
}
getMethodsForMarkerGenes <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getMethodsForMarkerGenes())
  }
}
getGroupsWithMarkerGenes <- function(method) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getGroupsWithMarkerGenes(method))
  }
}
getMarkerGenes <- function(method, group) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getMarkerGenes(method, group))
  }
}
getMethodsForTrajectories <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getMethodsForTrajectories())
  }
}
getNamesOfTrajectories <- function(method) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getNamesOfTrajectories(method))
  }
}
getTrajectory <- function(method, name) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getTrajectory(method, name))
  }
}

##----------------------------------------------------------------------------##
## Metadata column detectors + comparison-variable choices.
##
## Restored from the original cerebroApp v1.3 utility layer; the Trajectory tab
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
## Process-level cache for loaded .crb files (B8).
##
## Cerebro objects are treated as READ-ONLY across sessions. Cache is keyed by
## file path; overwriting a .crb in place is NOT detected -- restart the R
## process to pick up new content.
##----------------------------------------------------------------------------##
.crb_cache <- new.env(parent = emptyenv())

get_or_load_crb <- function(path) {
  if (is.null(.crb_cache[[path]])) {
    print(glue::glue("[{Sys.time()}] CRB cache miss, loading: {path}"))
    obj <- read_cerebro_file(path)
    obj <- .attachExternalExpression(obj, path)
    .crb_cache[[path]] <- obj
  } else {
    print(glue::glue("[{Sys.time()}] CRB cache hit: {path}"))
  }
  .crb_cache[[path]]
}

##----------------------------------------------------------------------------##
## Resolve an external expression backend at load time (B3).
##
## bpcells crbs ship a sibling <stem>.bpcells/ directory; the IterableMatrix
## handle persisted into the crb carries the writer's absolute @dir, which
## breaks once the crb is moved. This helper rebuilds the handle from a path
## rooted at the caller's view of the filesystem.
##
## Path priority:
##   1. Cerebro.options[["expression_matrix_BPCells"]] absolute override
##   2. dirname(crb_path) + getExpressionBackend()$location  (default)
##----------------------------------------------------------------------------##
.attachExternalExpression <- function(obj, crb_path) {
  if (!any(grepl("Cerebro", class(obj)))) {
    return(obj)
  }
  if (!is.function(obj$getExpressionBackend)) {
    ## Legacy crb without an expression_backend field. If the host app has
    ## configured an external matrix override, synthesise the backend tag
    ## from it so the runtime can still attach an h5 / bpcells sibling.
    ## Otherwise fall back to embedded (returned early below).
    opts <- if (
      exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
    ) {
      get("Cerebro.options", envir = .GlobalEnv)
    } else {
      list()
    }
    if (!is.null(opts[["expression_matrix_h5"]])) {
      be <- list(
        type = "h5",
        location = basename(opts[["expression_matrix_h5"]])
      )
    } else if (!is.null(opts[["expression_matrix_BPCells"]])) {
      be <- list(
        type = "bpcells",
        location = basename(opts[["expression_matrix_BPCells"]])
      )
    } else {
      be <- list(type = "embedded", location = NULL)
    }
  } else {
    be <- obj$getExpressionBackend()
  }

  if (is.null(be) || identical(be$type, "embedded")) {
    return(obj)
  }

  override <- NULL
  if (exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)) {
    opts <- get("Cerebro.options", envir = .GlobalEnv)
    override_key <- switch(
      be$type,
      bpcells = "expression_matrix_BPCells",
      h5 = "expression_matrix_h5",
      NULL
    )
    if (!is.null(override_key) && !is.null(opts[[override_key]])) {
      override <- opts[[override_key]]
    }
  }

  if (!is.null(override)) {
    loc_abs <- override
  } else if (!is.null(be$location)) {
    crb_dir <- dirname(normalizePath(crb_path, mustWork = FALSE))
    loc_abs <- file.path(crb_dir, be$location)
  } else {
    stop(
      sprintf(
        "External expression backend '%s' for crb '%s' has no location tag; ",
        be$type,
        crb_path
      ),
      "cannot attach. This crb may have been generated by a buggy exporter.",
      call. = FALSE
    )
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
          "Expected BPCells matrix directory at '%s' (derived from crb '%s' + backend location '%s'), but the directory does not exist. ",
          loc_abs,
          crb_path,
          be$location
        ),
        "Did the .bpcells/ sibling get moved or dropped when the crb was copied? ",
        "You can also point at a different absolute location via ",
        "Cerebro.options[['expression_matrix_BPCells']].",
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
    if (!file.exists(loc_abs)) {
      stop(
        sprintf(
          "Expected h5 file at '%s' (derived from crb '%s' + backend location '%s'), but the file does not exist. ",
          loc_abs,
          crb_path,
          be$location
        ),
        "Did the .h5 sibling get moved or dropped when the crb was copied? ",
        "You can also point at a different absolute location via ",
        "Cerebro.options[['expression_matrix_h5']].",
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
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getGroupsWithMostExpressedGenes())
  }
}
getMostExpressedGenes <- function(group) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getMostExpressedGenes(group))
  }
}

## Wrapper functions for enriched_pathways module.
getMethodsForEnrichedPathways <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getMethodsForEnrichedPathways())
  }
}
getGroupsWithEnrichedPathways <- function(method) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getGroupsWithEnrichedPathways(method))
  }
}
getEnrichedPathways <- function(method, group) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getEnrichedPathways(method, group))
  }
}

## Wrapper functions for extra_material module.
getExtraMaterialCategories <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getExtraMaterialCategories())
  }
}
checkForExtraTables <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$checkForExtraTables())
  }
}
getNamesOfExtraTables <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getNamesOfExtraTables())
  }
}
getExtraTable <- function(name) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getExtraTable(name))
  }
}
checkForExtraPlots <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$checkForExtraPlots())
  }
}
getNamesOfExtraPlots <- function() {
  if ('Cerebro_v1.3' %in% class(data_set())) {
    return(data_set()$getNamesOfExtraPlots())
  }
}
getExtraPlot <- function(name) {
  if ('Cerebro_v1.3' %in% class(data_set())) {
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
  if (
    is.null(color_variable) ||
      !color_variable %in% colnames(metadata) ||
      !"identifier" %in% colnames(metadata) ||
      !"identifier" %in% colnames(selection)
  ) {
    return(selection)
  }

  ## Map each selected identifier to its group, then keep only the cells whose
  ## group is not hidden. match() on identifier avoids a join dependency and
  ## keeps selection row order intact.
  group_by_identifier <- metadata[[color_variable]][
    match(selection[["identifier"]], metadata[["identifier"]])
  ]
  keep <- !(group_by_identifier %in% hidden_groups)
  selection[keep, , drop = FALSE]
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
