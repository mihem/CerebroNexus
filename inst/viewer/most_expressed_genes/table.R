##----------------------------------------------------------------------------##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

most_expressed_genes_available_groups <- reactive({
  unique(c(
    getGroupsWithMostExpressedGenes(),
    getGroupsWithMeanExpression()
  ))
})

most_expressed_genes_selected_tables <- reactive({
  selected_group <- input[["most_expressed_genes_selected_group"]]
  req(
    selected_group,
    selected_group %in% most_expressed_genes_available_groups()
  )
  list(
    pct = if (selected_group %in% getGroupsWithMostExpressedGenes()) {
      tryCatch(
        viewerGetMostExpressedGenes(selected_group),
        error = function(e) NULL
      )
    },
    mean_expr = if (selected_group %in% getGroupsWithMeanExpression()) {
      tryCatch(getMeanExpression(selected_group), error = function(e) NULL)
    }
  )
})

most_expressed_genes_selected_table <- reactive({
  metric_type <- input[["most_expressed_genes_metric_type"]]
  req(metric_type)
  most_expressed_genes_selected_tables()[[metric_type]]
})

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
## UI element that shows either a table with a switch to toggle sub-filtering
## of results and the corresponding selector, or a text message if data is
## missing.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_table_or_text_UI"]] <- renderUI({
  if (!length(most_expressed_genes_available_groups())) {
    return(p(
      "Expression summaries are not included in this data set. Choose a data set with stored expression summaries."
    ))
  }
  ## Build available metric choices based on data availability
  metric_choices <- c()
  tables <- most_expressed_genes_selected_tables()
  pct_data <- tables$pct

  if (!is.null(pct_data) && is.data.frame(pct_data) && nrow(pct_data) > 0) {
    metric_choices <- c(metric_choices, "Stored expression score (pct)" = "pct")
  }

  mean_data <- tables$mean_expr

  if (!is.null(mean_data) && is.data.frame(mean_data) && nrow(mean_data) > 0) {
    metric_choices <- c(metric_choices, "Mean expression" = "mean_expr")
  }

  ## If no data available, show message
  if (length(metric_choices) == 0) {
    return(fluidRow(
      column(
        12,
        tags$p(
          "This grouping variable has no stored expression summary. Choose another grouping variable."
        )
      )
    ))
  }

  fluidRow(
    column(
      12,
      selectInput(
        inputId = "most_expressed_genes_metric_type",
        label = "Expression metric:",
        choices = metric_choices,
        selected = {
          current <- isolate(input[["most_expressed_genes_metric_type"]])
          if (length(current) == 1L && current %in% metric_choices) {
            current
          } else {
            metric_choices[[1L]]
          }
        }
      ),
      conditionalPanel(
        "input.most_expressed_genes_metric_type === 'pct'",
        most_expressed_genes_metric_description("pct")
      ),
      conditionalPanel(
        "input.most_expressed_genes_metric_type === 'mean_expr'",
        most_expressed_genes_metric_description("mean_expr")
      )
    ),
    column(
      12,
      shinyWidgets::materialSwitch(
        inputId = "most_expressed_genes_table_filter_switch",
        label = "Show results for all subgroups (no pre-filtering):",
        value = FALSE,
        status = "primary",
        inline = TRUE
      )
    ),
    column(12, uiOutput("most_expressed_genes_filter_subgroups_UI")),
    column(12, DT::dataTableOutput("most_expressed_genes_table"))
  )
})

##----------------------------------------------------------------------------##
## UI element for metric description.
##----------------------------------------------------------------------------##
most_expressed_genes_metric_description <- function(metric_type) {
  if (metric_type == "pct") {
    tagList(
      tags$p(
        style = "color: #888; font-size: 12px; margin-top: -10px; margin-bottom: 5px;",
        "Gene-ranking values as stored in this data set. The definition and scale of pct depend on the generating method; check the data set's analysis documentation. Values are shown without percentage conversion."
      )
    )
  } else {
    tagList(
      tags$p(
        style = "color: #888; font-size: 12px; margin-top: -10px; margin-bottom: 5px;",
        "Average expression level per gene. Calculated from normalized read counts across all cells in each group."
      ),
      tags$p(
        style = "color: #888; font-size: 12px; text-align: center; font-style: italic; margin-bottom: 15px;",
        "Mean Expression = Σ(counts) / (Total cells in group)"
      )
    )
  }
}

##----------------------------------------------------------------------------##
## UI element for sub-filtering of results (if toggled).
##----------------------------------------------------------------------------##
output[["most_expressed_genes_filter_subgroups_UI"]] <- renderUI({
  req(!is.null(input[["most_expressed_genes_table_filter_switch"]]))
  req(!is.null(input[["most_expressed_genes_metric_type"]]))
  selected_group <- input[['most_expressed_genes_selected_group']]
  req(selected_group %in% most_expressed_genes_available_groups())
  results_df <- most_expressed_genes_selected_table()
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  cerebroResultSubgroupUI(
    results_df,
    input[["most_expressed_genes_table_filter_switch"]],
    identical(colnames(results_df)[[1L]], selected_group),
    "most_expressed_genes_table_select_group_level"
  )
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_table"]] <- DT::renderDataTable({
  selected_group <- input[['most_expressed_genes_selected_group']]
  req(selected_group %in% most_expressed_genes_available_groups())
  req(!is.null(input[["most_expressed_genes_metric_type"]]))
  metric_type <- input[["most_expressed_genes_metric_type"]]
  results_df <- most_expressed_genes_selected_table()
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  grouped <- identical(colnames(results_df)[[1L]], selected_group)
  if (!isTRUE(input[["most_expressed_genes_table_filter_switch"]]) && grouped) {
    req(input[["most_expressed_genes_table_select_group_level"]])
  }
  results_df <- cerebroFilterResultRows(
    results_df,
    input[["most_expressed_genes_table_filter_switch"]],
    grouped,
    input[["most_expressed_genes_table_select_group_level"]]
  )
  if (identical(metric_type, "pct") && "pct" %in% colnames(results_df)) {
    ## getMostExpressedGenes() and convertSeuratToCerebro() store different
    ## quantities and scales as pct. Without metric provenance, neither the
    ## values nor their range identify a unit.
    results_df <- dplyr::rename(results_df, "Stored score" = pct)
  } else if (
    !identical(metric_type, "pct") &&
      "mean_expr" %in% colnames(results_df)
  ) {
    results_df <- dplyr::rename(results_df, "Mean expression" = mean_expr)
  }
  cerebroResultTable(
    results_df,
    paste0(
      ifelse(
        metric_type == "pct",
        "stored_expression_score_",
        "mean_expression_"
      ),
      input[["most_expressed_genes_selected_group"]]
    ),
    hide_long_columns = FALSE,
    number_formatting = !identical(metric_type, "pct")
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "most_expressed_genes_info",
  most_expressed_genes_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
most_expressed_genes_info <- list(
  title = "Most expressed genes",
  text = HTML(
    "
    Table showing gene expression statistics for each group. These lists can help to identify/verify the dominant cell types.
    <h4>Expression metrics</h4>
    <b>Stored expression score (pct)</b><br>
    Values are shown as stored, without percentage conversion. The generating methods use pct for different quantities: getMostExpressedGenes stores a gene's fraction of total transcript counts (0–1), while convertSeuratToCerebro can store the percentage of cells with nonzero expression (0–100). These quantities are not interchangeable. The table does not identify which definition was used; consult the data set's analysis documentation before interpreting the values.<br>
    <br>
    <b>Mean expression</b><br>
    The average normalized expression level of each gene across all cells in each group. Higher values indicate genes with stronger overall expression in the group.<br>
    <h4>Options</h4>
    <b>Show results for all subgroups (no pre-filtering)</b><br>
    When active, the subgroup section element will disappear and instead the table will be shown for all subgroups. Subgroups can still be selected through the dedicated column filter, which also allows to select multiple subgroups at once. While using the column filter is more elegant, it can become laggy with very large tables, hence to option to filter the table beforehand. Please note that this feature only works if the first column was recognized as holding assignments to one of the grouping variables, e.g. 'sample' or 'clusters', otherwise your choice here will be ignored and the whole table shown without pre-filtering.<br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
