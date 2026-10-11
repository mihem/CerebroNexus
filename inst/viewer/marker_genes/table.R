##----------------------------------------------------------------------------##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element that shows table and toggle switches (for sub-filtering of
## results, automatic number formatting, automatic coloring of values), or text
## messages if no marker genes were found or data is missing.
##----------------------------------------------------------------------------##
output[["marker_genes_table_or_text_UI"]] <- renderUI({
  methods <- getMethodsForMarkerGenes()
  if (!length(methods)) {
    return(p(
      "Marker-gene results are not included in this data set. Choose a data set that includes marker-gene results."
    ))
  }
  method <- input[["marker_genes_selected_method"]]
  req(method %in% methods)
  if (!length(getGroupsWithMarkerGenes(method))) {
    return(p(
      "No marker-gene table is available for this method. Choose another method."
    ))
  }
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_method"]] %in% methods,
    input[["marker_genes_selected_table"]],
    input[["marker_genes_selected_table"]] %in%
      getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]])
  )
  ## fetch results
  results_type <- getMarkerGenes(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]]
  )
  if (length(results_type) > 0) {
    if (is.data.frame(results_type)) {
      cerebroResultTableControls(
        "marker_genes",
        results_type,
        colnames(results_type)[[1L]] %in% getGroups()
      )
    } else if (
      is.character(results_type) &&
        results_type == "no_markers_found"
    ) {
      p(
        "No marker genes meet the stored thresholds for this grouping variable. Choose another method or grouping variable."
      )
    }
  } else {
    p(
      "No marker-gene table is available for this method and grouping variable. Choose another method or grouping variable."
    )
  }
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["marker_genes_table"]] <- DT::renderDataTable({
  req(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]],
    input[["marker_genes_selected_table"]] %in%
      getGroupsWithMarkerGenes(input[["marker_genes_selected_method"]])
  )
  ## fetch results
  results_df <- getMarkerGenes(
    input[["marker_genes_selected_method"]],
    input[["marker_genes_selected_table"]]
  )
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))
  ## filter the table for a specific subgroup only if specified by the user
  ## (otherwise show all results)
  grouped <- colnames(results_df)[[1L]] %in% getGroups()
  if (!isTRUE(input[["marker_genes_table_filter_switch"]]) && grouped) {
    req(input[["marker_genes_table_select_group_level"]])
  }
  results_df <- cerebroFilterResultRows(
    results_df,
    input[["marker_genes_table_filter_switch"]],
    grouped,
    input[["marker_genes_table_select_group_level"]]
  )
  cerebroResultTable(
    results_df,
    paste0(
      "marker_genes_by_",
      input[["marker_genes_selected_method"]],
      "_",
      input[["marker_genes_selected_table"]]
    ),
    number_formatting = input[["marker_genes_table_number_formatting"]],
    color_highlighting = input[["marker_genes_table_color_highlighting"]]
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(input, "marker_genes_info", marker_genes_info)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
marker_genes_info <- list(
  title = "Marker genes",
  text = HTML(
    "
    Shown here are the marker genes identified for each group - resembling bulk RNA-seq. These genes should help to functionally interpret the role of a given group of cells or find new markers to purify it.<br>
    CerebroNexus performs this analysis with the 'FindAllMarkers()' function by Seurat, which compares each group to all other groups combined. Only genes that pass thresholds for log-fold change, the percentage of cells that express the gene, p-value, and adjusted p-values are reported. Statistical analysis can be done using different tests Finally, if data is available, the last column reports for each gene if it is associated with gene ontology term GO:0009986 which is an indicator that the respective gene is present on the cell surface (which could make it more interesting to purify a given population).<br>
    Results from other methods and tools can be manually added to the Cerebro object in which case the description above might not be applicable.
    <h4>Options</h4>
    <b>Show results for all subgroups (no pre-filtering)</b><br>
    When active, the subgroup section element will disappear and instead the table will be shown for all subgroups. Subgroups can still be selected through the dedicated column filter, which also allows to select multiple subgroups at once. While using the column filter is more elegant, it can become laggy with very large tables, hence to option to filter the table beforehand. Please note that this feature only works if the first column was recognized as holding assignments to one of the grouping variables, e.g. 'sample' or 'clusters', otherwise your choice here will be ignored and the whole table shown without pre-filtering.<br>
    <b>Automatically format numbers</b><br>
    When active, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <b>Highlight values with colours</b><br>
    Similar to the automatic formatting option, when active, CerebroNexus will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colours and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colours and/or the identification does not work properly, you can simply deactivate this feature.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
