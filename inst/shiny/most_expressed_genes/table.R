##----------------------------------------------------------------------------##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_table_UI"]] <- renderUI({
  selected_group <- input[['most_expressed_genes_selected_group']]
  if (is.null(selected_group) || selected_group %in% getGroups() == FALSE) {
    fluidRow(
      cerebroBox(
        title = boxTitle("Gene counts"),
        textOutput("most_expressed_genes_message_no_data_found")
      )
    )
  } else {
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Gene counts"),
          cerebroInfoButton("most_expressed_genes_info")
        ),
        uiOutput("most_expressed_genes_table_or_text_UI")
      )
    )
  }
})

##----------------------------------------------------------------------------##
## UI element that shows either a table with a switch to toggle sub-filtering
## of results and the corresponding selector, or a text message if data is
## missing.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_table_or_text_UI"]] <- renderUI({
  selected_group <- input[['most_expressed_genes_selected_group']]

  ## Build available metric choices based on data availability
  metric_choices <- c()

  ## Check if percent expressed data exists
  pct_data <- tryCatch({
    if (selected_group %in% getGroupsWithMostExpressedGenes()) {
      getMostExpressedGenes(selected_group)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(pct_data) && is.data.frame(pct_data) && nrow(pct_data) > 0) {
    metric_choices <- c(metric_choices, "Percent expressed" = "pct")
  }

  ## Check if mean expression data exists
  mean_data <- tryCatch({
    groups_with_mean <- getGroupsWithMeanExpression()
    if (!is.null(groups_with_mean) && selected_group %in% groups_with_mean) {
      getMeanExpression(selected_group)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(mean_data) && is.data.frame(mean_data) && nrow(mean_data) > 0) {
    metric_choices <- c(metric_choices, "Mean expression" = "mean_expr")
  }

  ## If no data available, show message
  if (length(metric_choices) == 0) {
    return(fluidRow(
      column(12,
        tags$p("No expression data available for the selected group.")
      )
    ))
  }

  fluidRow(
    column(12,
      selectInput(
        inputId = "most_expressed_genes_metric_type",
        label = "Expression metric:",
        choices = metric_choices,
        selected = metric_choices[1]
      ),
      uiOutput("most_expressed_genes_metric_description_UI")
    ),
    column(12,
      shinyWidgets::materialSwitch(
        inputId = "most_expressed_genes_table_filter_switch",
        label = "Show results for all subgroups (no pre-filtering):",
        value = FALSE,
        status = "primary",
        inline = TRUE
      )
    ),
    column(12,
      uiOutput("most_expressed_genes_filter_subgroups_UI")
    ),
    column(12,
      DT::dataTableOutput("most_expressed_genes_table")
    )
  )
})

##----------------------------------------------------------------------------##
## UI element for metric description.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_metric_description_UI"]] <- renderUI({
  req(input[["most_expressed_genes_metric_type"]])
  metric_type <- input[["most_expressed_genes_metric_type"]]

  if (metric_type == "pct") {
    tagList(
      tags$p(
        style = "color: #888; font-size: 12px; margin-top: -10px; margin-bottom: 5px;",
        "Percentage of cells expressing each gene. Based on normalized read counts, shows what proportion of cells in each group have detectable expression (count > 0)."
      ),
      tags$p(
        style = "color: #888; font-size: 12px; text-align: center; font-style: italic; margin-bottom: 15px;",
        "Percent = (Number of cells with count > 0) / (Total cells in group) × 100%"
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
})

##----------------------------------------------------------------------------##
## UI element for sub-filtering of results (if toggled).
##----------------------------------------------------------------------------##
output[["most_expressed_genes_filter_subgroups_UI"]] <- renderUI({
  req(!is.null(input[["most_expressed_genes_table_filter_switch"]]))
  req(!is.null(input[["most_expressed_genes_metric_type"]]))
  selected_group <- input[['most_expressed_genes_selected_group']]
  req(selected_group %in% getGroups())
  ## fetch results based on selected metric type safely
  metric_type <- input[["most_expressed_genes_metric_type"]]
  results_df <- tryCatch({
    if (metric_type == "pct") {
      getMostExpressedGenes(selected_group)
    } else {
      getMeanExpression(selected_group)
    }
  }, error = function(e) NULL)
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  ## check if pre-filtering is activated and name of first column in table is
  ## one of the registered groups
  ## ... it's not
  if (input[["most_expressed_genes_table_filter_switch"]] == TRUE || colnames(results_df)[1] %in% getGroups() == FALSE) {
    ## return nothing (empty row)
    fluidRow()
  ## ... it is
  } else {
    ## check for which groups results exist
    if ( is.character(results_df[[1]]) ) {
      available_groups <- unique(results_df[[1]])
    } else if ( is.factor(results_df[[1]]) ) {
      available_groups <- levels(results_df[[1]])
    }
    fluidRow(
      column(12,
        selectInput(
          "most_expressed_genes_table_select_group_level",
          label = "Filter results for subgroup:",
          choices = available_groups
        )
      )
    )
  }
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_table"]] <- DT::renderDataTable({
  selected_group <- input[['most_expressed_genes_selected_group']]
  req(selected_group %in% getGroups())
  req(!is.null(input[["most_expressed_genes_metric_type"]]))
  ## fetch results based on selected metric type safely
  metric_type <- input[["most_expressed_genes_metric_type"]]
  results_df <- tryCatch({
    if (metric_type == "pct") {
      getMostExpressedGenes(selected_group)
    } else {
      getMeanExpression(selected_group)
    }
  }, error = function(e) NULL)
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  ## filter the table for a specific subgroup only if specified by the user,
  ## otherwise show all results
  if (
    input[["most_expressed_genes_table_filter_switch"]] == FALSE &&
    colnames(results_df)[1] %in% getGroups() == TRUE
  ) {
    ## don't proceed if selection of subgroup is not available
    req(input[["most_expressed_genes_table_select_group_level"]])
    ## filter table
    results_df <- results_df[ which(results_df[[1]] == input[["most_expressed_genes_table_select_group_level"]]) , ]
  }

  ## if the table is empty, e.g. because the filtering of results for a specific
  ## subgroup did not work properly, skip the processing and show and empty
  ## table (otherwise the procedure would result in an error)
  if ( nrow(results_df) == 0 ) {
    results_df %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## if there is at least 1 row in the table, create proper table
  } else {
    ## rename value column based on metric type
    if (metric_type == "pct") {
      results_df <- results_df %>%
        dplyr::rename("% of cells expressing" = pct)
      value_col <- 3
    } else {
      results_df <- results_df %>%
        dplyr::rename("Mean expression" = mean_expr)
      value_col <- NULL
    }

    results_df %>%
    prettifyTable(
      filter = list(position = "top", clear = TRUE),
      dom = "Bfrtlip",
      show_buttons = TRUE,
      number_formatting = TRUE,
      color_highlighting = TRUE,
      hide_long_columns = FALSE,
      columns_percentage = value_col,
      download_file_name = paste0(
        ifelse(metric_type == "pct", "percent_expressed_", "mean_expression_"),
        input[["most_expressed_genes_selected_group"]]
      ),
      page_length_default = 20,
      page_length_menu = c(20, 50, 100)
    )
  }
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["most_expressed_genes_message_no_data_found"]] <- renderText({
  "No data available."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["most_expressed_genes_info"]], {
  showModal(
    modalDialog(
      most_expressed_genes_info[["text"]],
      title = most_expressed_genes_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
most_expressed_genes_info <- list(
  title = "Most expressed genes",
  text = HTML("
    Table showing gene expression statistics for each group. These lists can help to identify/verify the dominant cell types.
    <h4>Expression metrics</h4>
    <b>Percent expressed</b><br>
    The percentage of cells in each group that have detectable expression (count > 0) of each gene. For example, if a gene shows 80%, it means 80% of cells in that group express this gene.<br>
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
