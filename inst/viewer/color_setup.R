##----------------------------------------------------------------------------##
## Color management.
##----------------------------------------------------------------------------##
# Qualitative palette for categorical groups (clusters, samples, ...) when the
# user has NOT picked colours in the Color management tab. These are the vibrant
# flatuicolors sets (Dutch + Spanish), used so distinct clusters stay easy to
# tell apart. Past their 40, cerebro_group_colors() below adds generated
# colours, each well apart from the others, so a data set with many clusters
# still gets a distinct colour for every level.
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

## n qualitative colours: the first n of the base palette, and past its 40 the
## base followed by generated colours, each as far as it can be from all the
## others (the Builder's "Cerebro default" is the same, so an App it builds
## draws a Group as the Viewer does). Stretching the base with colorRampPalette
## squeezed in-between shades and moved every level's colour when one was added.
cerebro_group_colors <- function(n) {
  n <- max(0L, as.integer(n))
  if (n == 0L) {
    return(character(0))
  }
  if (n <= length(default_colorset_base)) {
    return(default_colorset_base[seq_len(n)])
  }
  CerebroNexus:::.cerebroExtendPalette(default_colorset_base, n)
}

# Cell-cycle phases: the original vibrant four-colour set.
cell_cycle_colorset <- setNames(
  c("#45aaf2", "#f1c40f", "#e74c3c", "#7f8c8d"),
  c("G1", "S", "G2M", "-")
)

##----------------------------------------------------------------------------##
## Assign colors to groups.
##----------------------------------------------------------------------------##
## The ids of the colour inputs of `variable`'s `levels`: the data set and the
## variable are encoded once, not once per level (a data set's palettes have
## hundreds of levels).
color_input_ids <- function(variable, levels) {
  if (!length(levels)) {
    return(character())
  }
  dataset <- tryCatch(
    viewer_requested_dataset_context()$dataset_key,
    error = function(error) NULL
  )
  dataset <- dataset %||%
    viewerDatasetName(
      Cerebro.options[["crb_file_to_load"]],
      available_crb_files$selected
    ) %||%
    "__single__"
  encode <- function(value) {
    paste(as.character(charToRaw(enc2utf8(as.character(value)))), collapse = "")
  }
  paste0(
    "color_",
    encode(dataset),
    "_",
    encode(variable),
    "_",
    vapply(as.character(levels), encode, character(1), USE.NAMES = FALSE)
  )
}

color_input_id <- function(variable, level) {
  color_input_ids(variable, level)
}

configured_colors_for_current_dataset <- function() {
  resolve_configured_colors(
    color_config = Cerebro.options[["colors"]],
    selected_path = available_crb_files$selected,
    configured_files = Cerebro.options[["crb_file_to_load"]]
  )
}

## The ids of the colour inputs the session has (Color management's pickers).
## Palettes read only these: reading one input per level registered a
## dependency each, about half a millisecond, and a data set's palettes have
## hundreds of levels. Changes only when a picker appears or goes away.
color_picker_ids <- reactiveVal(character(0))
observe(
  {
    ids <- grep("^color_", names(input), value = TRUE)
    if (!setequal(ids, isolate(color_picker_ids()))) {
      color_picker_ids(ids)
    }
  },
  priority = 1000
)

resolve_reactive_palette <- function(variable, levels, defaults) {
  configured <- configured_colors_for_current_dataset()
  names(defaults) <- levels
  defaults <- apply_configured_colors(defaults, configured[[variable]])
  pickers <- color_picker_ids()
  if (!length(pickers)) {
    return(defaults)
  }
  levels <- as.character(levels)
  ids <- color_input_ids(variable, levels)
  for (index in which(ids %in% pickers)) {
    picked <- input[[ids[[index]]]]
    if (!is.null(picked)) {
      defaults[levels[[index]]] <- picked
    }
  }
  defaults
}

## Resolve one categorical group without making the caller depend on every
## colour picker in the data set. Plot-specific reactives should prefer this
## helper over indexing reactive_colors().
reactive_group_colors <- function(group_name) {
  req(data_set(), group_name %in% getGroups())
  levels <- getGroupLevels(group_name)
  defaults <- cerebro_group_colors(length(levels))
  names(defaults) <- levels
  if ("N/A" %in% levels) {
    defaults["N/A"] <- "#898989"
  }
  ## colours the data set brings along (e.g. scanpy's) replace the built-in
  ## defaults; app-level configuration and the user's picks still win
  defaults <- apply_configured_colors(
    defaults,
    stored_group_colors(
      tryCatch(data_set()$technical_info, error = function(error) NULL),
      group_name,
      levels
    )
  )
  resolve_reactive_palette(group_name, levels, defaults)
}

reactive_colors <- reactive({
  req(data_set())
  ## get cell meta data
  cache <- viewerProjectionFirstFrameCache(data_set())
  meta_data <- if (!is.null(cache$meta_data)) cache$meta_data else getMetaData()
  colors <- list()

  ## go through all groups
  for (group_name in getGroups()) {
    colors[[group_name]] <- reactive_group_colors(group_name)
  }
  ## go through columns with cell cycle info
  if (length(getCellCycle()) > 0) {
    for (column in getCellCycle()) {
      states <- unique(as.character(meta_data[[column]]))
      colors[[column]] <- resolve_reactive_palette(
        column,
        states,
        cell_cycle_colorset[seq_along(states)]
      )
    }
  }
  return(colors)
})
