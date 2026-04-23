##----------------------------------------------------------------------------##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["marker_genes_table_UI"]] <- renderUI({
  if (!hasMarkerGenes()) {
    fluidRow(
      cerebroBox(
        title = boxTitle("Marker genes"),
        textOutput("marker_genes_message_no_data_found")
      )
    )
  } else {
    fluidRow(
      cerebroBox(
        title = tagList(
          boxTitle("Marker genes"),
          cerebroInfoButton("marker_genes_info")
        ),
        uiOutput("marker_genes_table_or_text_UI")
      )
    )
  }
})

##----------------------------------------------------------------------------##
## UI element that shows table and toggle switches (for sub-filtering of
## results, automatic number formatting, automatic coloring of values), or text
## messages if no marker genes were found or data is missing.
##----------------------------------------------------------------------------##
output[["marker_genes_table_or_text_UI"]] <- renderUI({
  req(hasMarkerGenes())
  ## fetch results
  results_df <- getMarkerGenes()
  if ( is.data.frame(results_df) && nrow(results_df) > 0 ) {
    fluidRow(
      column(12,
        shinyWidgets::materialSwitch(
          inputId = "marker_genes_table_number_formatting",
          label = "Automatically format numbers:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        ),
        shinyWidgets::materialSwitch(
          inputId = "marker_genes_table_color_highlighting",
          label = "Highlight values with colors:",
          value = TRUE,
          status = "primary",
          inline = TRUE
        )
      ),
      column(12,
        uiOutput("marker_genes_filter_cluster_UI")
      ),
      column(12,
        DT::dataTableOutput("marker_genes_table")
      )
    )
  } else {
    textOutput("marker_genes_table_no_data")
  }
})

##----------------------------------------------------------------------------##
## UI element for cluster filtering dropdown.
##----------------------------------------------------------------------------##
output[["marker_genes_filter_cluster_UI"]] <- renderUI({
  req(hasMarkerGenes())
  ## get available clusters
  clusters <- getMarkerClusters()
  ## only show dropdown if clusters are available
  if (!is.null(clusters) && length(clusters) > 0) {
    fluidRow(
      column(12,
        selectInput(
          "marker_genes_select_cluster",
          label = "Filter by cluster:",
          choices = c("All clusters" = "__ALL__", clusters),
          selected = "__ALL__"
        )
      )
    )
  } else {
    fluidRow()
  }
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["marker_genes_table"]] <- DT::renderDataTable({
  req(hasMarkerGenes())
  ## fetch results
  results_df <- getMarkerGenes()
  ## don't proceed if input is not a data frame
  req(is.data.frame(results_df))

  ## filter by cluster if user selected one
  clusters <- getMarkerClusters()
  if (!is.null(clusters) && length(clusters) > 0 &&
      !is.null(input[["marker_genes_select_cluster"]]) &&
      input[["marker_genes_select_cluster"]] != "__ALL__") {
    ## find the cluster column
    possible_cols <- c("group", "cluster", "cell_type", "celltype", "identity", "ident")
    col_names <- colnames(results_df)
    cluster_col <- NULL
    for (col in possible_cols) {
      idx <- which(tolower(col_names) == tolower(col))
      if (length(idx) > 0) {
        cluster_col <- col_names[idx[1]]
        break
      }
    }
    ## fallback to first column if it's categorical
    if (is.null(cluster_col) && ncol(results_df) > 0) {
      first_col <- results_df[[1]]
      if (is.character(first_col) || is.factor(first_col)) {
        cluster_col <- col_names[1]
      }
    }
    ## filter if cluster column found
    if (!is.null(cluster_col)) {
      results_df <- results_df[results_df[[cluster_col]] == input[["marker_genes_select_cluster"]], ]
    }
  }

  ## if the table is empty, e.g. because the filtering of results for a specific
  ## subgroup did not work properly, skip the processing and show and empty
  ## table (otherwise the procedure would result in an error)
  if ( nrow(results_df) == 0 ) {
    results_df %>%
    as.data.frame() %>%
    dplyr::slice(0) %>%
    prepareEmptyTable()
  ## if there is at least 1 row, create proper table
  } else {
    prettifyTable(
      results_df,
      filter = list(position = "top", clear = TRUE),
      dom = "Bfrtlip",
      show_buttons = TRUE,
      number_formatting = input[["marker_genes_table_number_formatting"]],
      color_highlighting = input[["marker_genes_table_color_highlighting"]],
      hide_long_columns = TRUE,
      download_file_name = "marker_genes",
      page_length_default = 20,
      page_length_menu = c(20, 50, 100)
    )
  }
})

##----------------------------------------------------------------------------##
## Alternative text message if no marker genes were found.
##----------------------------------------------------------------------------##
output[["marker_genes_table_no_markers_found"]] <- renderText({
  "No marker genes were identified for any of the subpopulations of this grouping variable."
})

##----------------------------------------------------------------------------##
## Alternative text message if data is missing.
##----------------------------------------------------------------------------##
output[["marker_genes_table_no_data"]] <- renderText({
  "Data not available. Possible reasons: Only 1 group in this data set or data not generated."
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["marker_genes_info"]], {
  showModal(
    modalDialog(
      marker_genes_info[["text"]],
      title = marker_genes_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
marker_genes_info <- list(
  title = "Marker genes",
  text = HTML("
    Shown here are the marker genes identified for each group - resembling bulk RNA-seq. These genes should help to functionally interpret the role of a given group of cells or find new markers to purify it.<br>
    Cerebro performs this analysis with the 'FindAllMarkers()' function by Seurat, which compares each group to all other groups combined. Only genes that pass thresholds for log-fold change, the percentage of cells that express the gene, p-value, and adjusted p-values are reported. Statistical analysis can be done using different tests Finally, if data is available, the last column reports for each gene if it is associated with gene ontology term GO:0009986 which is an indicator that the respective gene is present on the cell surface (which could make it more interesting to purify a given population).<br>
    Results from other methods and tools can be manually added to the Cerebro object in which case the description above might not be applicable.
    <h4>Options</h4>
    <b>Show results for all subgroups (no pre-filtering)</b><br>
    When active, the subgroup section element will disappear and instead the table will be shown for all subgroups. Subgroups can still be selected through the dedicated column filter, which also allows to select multiple subgroups at once. While using the column filter is more elegant, it can become laggy with very large tables, hence to option to filter the table beforehand. Please note that this feature only works if the first column was recognized as holding assignments to one of the grouping variables, e.g. 'sample' or 'clusters', otherwise your choice here will be ignored and the whole table shown without pre-filtering.<br>
    <b>Automatically format numbers</b><br>
    When active, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <b>Highlight values with colors</b><br>
    Similar to the automatic formatting option, when active, Cerebro will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colors and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colors and/or the identification does not work properly, you can simply deactivate this feature.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
