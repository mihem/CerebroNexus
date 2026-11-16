##----------------------------------------------------------------------------##
## Composition of selected group by other group.
##----------------------------------------------------------------------------##

groupsMetadataColumns <- function(columns) {
  metadata <- viewerProjectionFirstFrameMetadata()
  columns <- unique(columns[columns %in% colnames(metadata)])
  viewerProjectionSubsetRows(metadata, seq_len(nrow(metadata)), columns)
}

groups_composition_counts <- reactive({
  first <- groups_selected_group()
  second <- groups_by_other_group_second_group()
  req(first %in% getGroups(), second %in% getGroups(), first != second)
  calculateTableAB(
    groupsMetadataColumns(c(first, second)),
    first,
    second,
    "long",
    FALSE
  )
})

groupsCompositionTable <- function(mode, percent) {
  formatTableAB(
    groups_composition_counts(),
    groups_selected_group(),
    groups_by_other_group_second_group(),
    mode,
    percent
  )
}

##----------------------------------------------------------------------------##
## Plot showing composition of groups, either as a bar chart or a Sankey plot.
##----------------------------------------------------------------------------##
## The counts, the percentages and the Sankey plot of the two groups go in one
## value; the browser shows the one the plot type and percent controls ask for,
## so changing those controls does not come back to R.
output[["groups_by_other_group_plot"]] <- renderCerebroChart({
  ## only proceed if the two groups are not the same (otherwise it can give an
  ## error when switching between groups)
  req(
    groups_selected_group() %in% getGroups(),
    groups_by_other_group_second_group() %in% getGroups(),
    groups_selected_group() != groups_by_other_group_second_group()
  )
  first <- groups_selected_group()
  second <- groups_by_other_group_second_group()
  colors_second <- reactive_group_colors(second)
  bar <- function(percent) {
    cerebroBarChartData(
      table = groupsCompositionTable(mode = "long", percent = percent),
      first = first,
      second = second,
      colors = colors_second,
      percent = percent
    )
  }
  sankey <- cerebroSankeyData(
    table = groupsCompositionTable(mode = "long", percent = FALSE),
    first = first,
    second = second,
    colors_first = reactive_group_colors(first),
    colors_second = colors_second
  )
  list(
    type = "switch",
    inputs = I(c(
      "groups_by_other_group_plot_type",
      "groups_by_other_group_show_as_percent"
    )),
    views = list(
      "Bar chart|FALSE" = bar(FALSE),
      "Bar chart|TRUE" = bar(TRUE),
      "Sankey plot|FALSE" = sankey,
      "Sankey plot|TRUE" = sankey
    ),
    fallback = "Bar chart|FALSE"
  )
}) %>%
  cachePlot(
    groups_selected_group(),
    groups_by_other_group_second_group(),
    reactive_group_colors(groups_selected_group()),
    reactive_group_colors(groups_by_other_group_second_group()),
    available_crb_files$selected
  )

##----------------------------------------------------------------------------##
## Table showing numbers of plot.
##----------------------------------------------------------------------------##
output[["groups_by_other_group_table"]] <- viewerLazyRender(function() {
  DT::renderDataTable({
    ## only proceed if the two groups are not the same (otherwise it can give an
    ## error when switching between groups)
    req(
      groups_selected_group(),
      groups_by_other_group_second_group(),
      groups_selected_group() != groups_by_other_group_second_group()
    )
    ## generate table
    composition_df <- groupsCompositionTable(
      mode = "wide",
      percent = groups_by_other_group_show_as_percent()
    )
    ## get indices of columns that should be formatted as percent
    if (groups_by_other_group_show_as_percent() == TRUE) {
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
    "Compare cell groups as bars, a Sankey plot, or a table. Use percentages to compare groups of different sizes. Click a legend item to hide or show a group."
  )
)
