##----------------------------------------------------------------------------##
## Composition of selected group by other group.
##----------------------------------------------------------------------------##

groupsMetadataColumns <- function(columns) {
  metadata <- viewerProjectionFirstFrameMetadata()
  columns <- unique(columns[columns %in% colnames(metadata)])
  viewerProjectionSubsetRows(metadata, seq_len(nrow(metadata)), columns)
}

## Groups only needs a two-way contingency table. Building that table directly
## avoids sorting all cells and starting the full dplyr grouping pipeline on the
## first visit. The returned columns and factor levels match calculateTableAB().
groupsCalculateTableAB <- function(table, groupA, groupB, mode, percent) {
  values_a <- table[[groupA]]
  values_b <- table[[groupB]]
  if (is.character(values_a)) {
    values_a <- factor(
      values_a,
      levels = sort(unique(values_a)),
      exclude = NULL
    )
  }
  if (is.character(values_b)) {
    values_b <- factor(
      values_b,
      levels = sort(unique(values_b)),
      exclude = NULL
    )
  }
  if (!is.factor(values_a)) {
    values_a <- factor(
      values_a,
      levels = sort(unique(values_a)),
      exclude = NULL
    )
  }
  if (!is.factor(values_b)) {
    values_b <- factor(
      values_b,
      levels = sort(unique(values_b)),
      exclude = NULL
    )
  }

  factor_codes <- function(values) {
    codes <- as.integer(values)
    labels <- levels(values)
    has_na <- anyNA(codes)
    if (has_na) {
      codes[is.na(codes)] <- length(labels) + 1L
    }
    list(codes = codes, labels = labels, has_na = has_na)
  }
  encoded_a <- factor_codes(values_a)
  encoded_b <- factor_codes(values_b)
  n_a <- length(encoded_a$labels) + as.integer(encoded_a$has_na)
  n_b <- length(encoded_b$labels) + as.integer(encoded_b$has_na)
  linear <- (encoded_a$codes - 1L) * n_b + encoded_b$codes
  cell_counts <- tabulate(linear, nbins = n_a * n_b)
  occupied <- which(cell_counts > 0L)
  index_a <- (occupied - 1L) %/% n_b + 1L
  index_b <- (occupied - 1L) %% n_b + 1L

  decode <- function(index, encoded) {
    labels <- encoded$labels[index]
    factor(labels, levels = encoded$labels)
  }
  counts <- data.frame(
    decode(index_a, encoded_a),
    decode(index_b, encoded_b),
    count = cell_counts[occupied],
    total_cell_count = tabulate(encoded_a$codes, nbins = n_a)[index_a],
    check.names = FALSE
  )
  names(counts)[1:2] <- c(groupA, groupB)

  groupsFormatComposition(
    counts,
    groupA,
    groupB,
    mode,
    percent,
    levels(values_b)
  )
}

groupsFormatComposition <- function(
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

groups_composition_counts <- reactive({
  first <- input[["groups_selected_group"]]
  second <- input[["groups_by_other_group_second_group"]]
  req(first %in% getGroups(), second %in% getGroups(), first != second)
  groupsCalculateTableAB(
    groupsMetadataColumns(c(first, second)),
    first,
    second,
    "long",
    FALSE
  )
})

groupsCompositionTable <- function(mode, percent) {
  groupsFormatComposition(
    groups_composition_counts(),
    input[["groups_selected_group"]],
    input[["groups_by_other_group_second_group"]],
    mode,
    percent
  )
}

##----------------------------------------------------------------------------##
## Plot showing composition of groups, either as a bar chart or a Sankey plot.
##----------------------------------------------------------------------------##
output[["groups_by_other_group_plot"]] <- plotly::renderPlotly({
  ## only proceed if the two groups are not the same (otherwise it can give an
  ## error when switching between groups)
  req(
    input[["groups_selected_group"]] %in% getGroups(),
    input[["groups_by_other_group_second_group"]] %in% getGroups(),
    input[["groups_selected_group"]] !=
      input[["groups_by_other_group_second_group"]],
    input[["groups_by_other_group_plot_type"]]
  )
  ##
  if (input[["groups_by_other_group_plot_type"]] == "Bar chart") {
    ## calculate table
    composition_df <- groupsCompositionTable(
      mode = "long",
      percent = input[["groups_by_other_group_show_as_percent"]]
    )
    colors <- reactive_group_colors(
      input[["groups_by_other_group_second_group"]]
    )
    ## generate plot
    plot <- plotlyBarChart(
      table = composition_df,
      first_grouping_variable = input[["groups_selected_group"]],
      second_grouping_variable = input[["groups_by_other_group_second_group"]],
      colors = colors,
      percent = input[["groups_by_other_group_show_as_percent"]]
    )
    plot
    ##
  } else if (input[["groups_by_other_group_plot_type"]] == "Sankey plot") {
    ## calculate table
    composition_df <- groupsCompositionTable(
      mode = "long",
      percent = FALSE
    )
    ## get color code for all group levels (from both groups)
    colors_for_groups <- c(
      reactive_group_colors(input[["groups_selected_group"]]),
      reactive_group_colors(input[["groups_by_other_group_second_group"]])
    )
    ## generate plot
    plotlySankeyPlot(
      table = composition_df,
      first_grouping_variable = input[["groups_selected_group"]],
      second_grouping_variable = input[["groups_by_other_group_second_group"]],
      colors_for_groups = colors_for_groups
    )
  }
}) %>%
  cachePlot(
    input[["groups_by_other_group_plot_type"]],
    input[["groups_selected_group"]],
    input[["groups_by_other_group_second_group"]],
    input[["groups_by_other_group_show_as_percent"]],
    reactive_group_colors(input[["groups_selected_group"]]),
    reactive_group_colors(input[["groups_by_other_group_second_group"]]),
    available_crb_files$selected
  )

##----------------------------------------------------------------------------##
## Table showing numbers of plot.
##----------------------------------------------------------------------------##
output[["groups_by_other_group_table"]] <- DT::renderDataTable({
  ## only proceed if the two groups are not the same (otherwise it can give an
  ## error when switching between groups)
  req(
    input[["groups_selected_group"]],
    input[["groups_by_other_group_second_group"]],
    input[["groups_selected_group"]] !=
      input[["groups_by_other_group_second_group"]]
  )
  ## generate table
  composition_df <- groupsCompositionTable(
    mode = "wide",
    percent = input[["groups_by_other_group_show_as_percent"]]
  )
  ## get indices of columns that should be formatted as percent
  if (input[["groups_by_other_group_show_as_percent"]] == TRUE) {
    columns_percentage <- c(3:ncol(composition_df))
  } else {
    columns_percentage <- NULL
  }
  composition_df %>%
    dplyr::rename("# of cells" = total_cell_count) %>%
    prettifyTable(
      filter = "none",
      dom = "Brtlip",
      show_buttons = FALSE,
      number_formatting = TRUE,
      color_highlighting = FALSE,
      hide_long_columns = TRUE,
      columns_percentage = columns_percentage
    )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "groups_by_other_group_info",
  groups_by_other_group_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
groups_by_other_group_info <- list(
  title = "Composition of group by another group",
  text = HTML(
    "This plot allows to see how cell groups are related to each other. This can be represented as a bar char or a Sankey plot. Optionally, a table can be shown below. To highlight composition in very small cell groups, results can be shown as percentages rather than actual cell counts. Groups can be removed from the plot by clicking on them in the legend."
  )
)
