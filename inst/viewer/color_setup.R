##----------------------------------------------------------------------------##
## Color management.
##----------------------------------------------------------------------------##
# Qualitative palette for categorical groups (clusters, samples, ...) when the
# user has NOT picked colours in the Color management tab. These are the vibrant
# flatuicolors sets (Dutch + Spanish), used so distinct clusters stay easy to
# tell apart. cerebro_group_colors() below keeps the overflow-safe interpolation
# so many-cluster data sets still get valid colours instead of NAs.
#
# Users can still override any group colour with the colour picker; this is only
# the default fallback (see reactive_colors() below).
colorset_dutch <- c(
  "#FFC312",
  "#C4E538",
  "#12CBC4",
  "#FDA7DF",
  "#ED4C67",
  "#F79F1F",
  "#A3CB38",
  "#1289A7",
  "#D980FA",
  "#B53471",
  "#EE5A24",
  "#009432",
  "#0652DD",
  "#9980FA",
  "#833471",
  "#EA2027",
  "#006266",
  "#1B1464",
  "#5758BB",
  "#6F1E51"
)
colorset_spanish <- c(
  "#40407a",
  "#706fd3",
  "#f7f1e3",
  "#34ace0",
  "#33d9b2",
  "#2c2c54",
  "#474787",
  "#aaa69d",
  "#227093",
  "#218c74",
  "#ff5252",
  "#ff793f",
  "#d1ccc0",
  "#ffb142",
  "#ffda79",
  "#b33939",
  "#cd6133",
  "#84817a",
  "#cc8e35",
  "#ccae62"
)
default_colorset_base <- c(colorset_dutch, colorset_spanish)

## Build n visually distinct qualitative colours from the base palette. For
## n <= length(base) we take the first n base hues (hand-tuned, best contrast).
## For n > length(base) we interpolate across the whole base ring with
## colorRampPalette so a data set with many clusters still gets n *valid*
## colours instead of NAs — the old `default_colorset[seq_along(...)]` slicing
## silently returned NA past 40 groups.
cerebro_group_colors <- function(n) {
  n <- max(0L, as.integer(n))
  if (n == 0L) {
    return(character(0))
  }
  if (n <= length(default_colorset_base)) {
    return(default_colorset_base[seq_len(n)])
  }
  grDevices::colorRampPalette(default_colorset_base)(n)
}

# Cell-cycle phases: the original vibrant four-colour set.
cell_cycle_colorset <- setNames(
  c("#45aaf2", "#f1c40f", "#e74c3c", "#7f8c8d"),
  c("G1", "S", "G2M", "-")
)

##----------------------------------------------------------------------------##
## Assign colors to groups.
##----------------------------------------------------------------------------##
color_input_id <- function(variable, level) {
  configured_files <- if (exists("Cerebro.options")) {
    Cerebro.options[["crb_file_to_load"]]
  } else {
    NULL
  }
  selected_path <- available_crb_files$selected
  configured_index <- match(selected_path, unname(configured_files))
  dataset <- if (length(configured_index) == 1L && !is.na(configured_index)) {
    names(configured_files)[[configured_index]]
  } else {
    selected_path
  }
  encode <- function(value) {
    paste(format(charToRaw(enc2utf8(as.character(value)))), collapse = "")
  }
  paste0(
    "color_",
    encode(dataset),
    "_",
    encode(variable),
    "_",
    encode(level)
  )
}

reactive_colors <- reactive({
  req(data_set())
  ## get cell meta data
  meta_data <- getMetaData()
  colors <- list()

  configured <- resolve_configured_colors(
    color_config = if (exists("Cerebro.options")) {
      Cerebro.options[["colors"]]
    } else {
      NULL
    },
    selected_path = available_crb_files$selected,
    configured_files = if (exists("Cerebro.options")) {
      Cerebro.options[["crb_file_to_load"]]
    } else {
      NULL
    }
  )

  picked_color <- function(variable, level) {
    input[[color_input_id(variable, level)]]
  }

  resolve_palette <- function(variable, levels, defaults) {
    names(defaults) <- levels
    defaults <- apply_configured_colors(defaults, configured[[variable]])
    for (level in levels) {
      picked <- picked_color(variable, level)
      if (!is.null(picked)) {
        defaults[level] <- picked
      }
    }
    defaults
  }

  ## go through all groups
  for (group_name in getGroups()) {
    levels <- getGroupLevels(group_name)
    defaults <- cerebro_group_colors(length(levels))
    names(defaults) <- levels
    if ("N/A" %in% levels) {
      defaults["N/A"] <- "#898989"
    }
    colors[[group_name]] <- resolve_palette(group_name, levels, defaults)
  }
  ## go through columns with cell cycle info
  if (length(getCellCycle()) > 0) {
    for (column in getCellCycle()) {
      states <- unique(as.character(meta_data[[column]]))
      colors[[column]] <- resolve_palette(
        column,
        states,
        cell_cycle_colorset[seq_along(states)]
      )
    }
  }
  return(colors)
})
