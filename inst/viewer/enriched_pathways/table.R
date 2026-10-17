##----------------------------------------------------------------------------##
## Tab: Enriched pathways
##
## Table or info text when data is missing.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Reactive to fetch enriched pathways data
##----------------------------------------------------------------------------##

enriched_pathways_data <- reactive({
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_table"]],
    input[["enriched_pathways_selected_method"]] %in%
      getMethodsForEnrichedPathways(),
    input[["enriched_pathways_selected_table"]] %in%
      getGroupsWithEnrichedPathways(input[[
        "enriched_pathways_selected_method"
      ]])
  )
  getEnrichedPathways(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_table"]]
  )
})

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element that shows table and toggle switches (for sub-filtering of
## results, automatic number formatting, automatic coloring of values), or text
## messages if no marker genes or enriched pathways were found or data is
## missing.
##----------------------------------------------------------------------------##

output[["enriched_pathways_table_or_text_UI"]] <- renderUI({
  methods <- getMethodsForEnrichedPathways()
  if (!length(methods)) {
    return(p("Pathway-enrichment results are not included in this data set."))
  }
  method <- input[["enriched_pathways_selected_method"]]
  req(method %in% methods)
  if (!length(getGroupsWithEnrichedPathways(method))) {
    return(p(
      "No enrichment results. Choose another method."
    ))
  }
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_table"]],
    input[["enriched_pathways_selected_table"]] %in%
      getGroupsWithEnrichedPathways(input[[
        "enriched_pathways_selected_method"
      ]])
  )
  results_type <- enriched_pathways_data()
  ## depending on the content of the results slot, show a text message or
  ## switches and table
  if (
    is.character(results_type) &&
      results_type == "no_markers_found"
  ) {
    p(
      "Enrichr needs saved marker genes. Choose another method or grouping."
    )
  } else if (
    is.character(results_type) &&
      results_type == "no_pathways_found"
  ) {
    p(
      "Enrichr found no enriched pathways for this grouping variable. Choose another method or grouping variable."
    )
  } else if (
    is.character(results_type) &&
      results_type == "no_gene_sets_enriched"
  ) {
    p(
      "No gene sets meet the saved GSVA thresholds. Choose another method or grouping."
    )
  } else if (is.data.frame(results_type)) {
    cerebroResultTableControls(
      "enriched_pathways",
      results_type,
      colnames(results_type)[[1L]] %in% getGroups()
    )
  } else {
    p(
      "No enrichment results for this grouping. Choose another method or grouping."
    )
  }
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##

output[["enriched_pathways_table"]] <- DT::renderDataTable({
  ##
  req(
    input[["enriched_pathways_selected_method"]],
    input[["enriched_pathways_selected_table"]],
    input[["enriched_pathways_selected_table"]] %in%
      getGroupsWithEnrichedPathways(input[[
        "enriched_pathways_selected_method"
      ]])
  )

  withProgress(message = "Processing enriched pathways table...", value = 0, {
    ## fetch results
    results_df <- enriched_pathways_data()

    ## don't proceed if input is not a data frame
    req(is.data.frame(results_df))

    incProgress(0.3, detail = "Filtering data...")

    grouped <- colnames(results_df)[[1L]] %in% getGroups()
    if (!isTRUE(input[["enriched_pathways_table_filter_switch"]]) && grouped) {
      req(input[["enriched_pathways_table_select_group_level"]])
    }
    results_df <- cerebroFilterResultRows(
      results_df,
      input[["enriched_pathways_table_filter_switch"]],
      grouped,
      input[["enriched_pathways_table_select_group_level"]]
    )

    incProgress(0.6, detail = "Rendering table...")

    columns_hide <- integer()
    if (
      any(grepl("Term", colnames(results_df))) &&
        any(grepl("Old.P.value", colnames(results_df))) &&
        any(grepl("Old.Adjusted.P.value", colnames(results_df)))
    ) {
      columns_hide <- grep(
        "Old.P.value|Old.Adjusted.P.value",
        colnames(results_df)
      )
    }
    cerebroResultTable(
      results_df,
      paste0(
        "enriched_pathways_by_",
        input[["enriched_pathways_selected_method"]],
        "_",
        input[["enriched_pathways_selected_table"]]
      ),
      number_formatting = input[[
        "enriched_pathways_table_number_formatting"
      ]],
      color_highlighting = input[[
        "enriched_pathways_table_color_highlighting"
      ]],
      columns_hide = columns_hide
    )
  })
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(input, "enriched_pathways_info", enriched_pathways_info)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

enriched_pathways_info <- list(
  title = "Enriched pathways",
  text = HTML(
    "
    At the moment, CerebroNexus supports two different ways to perform pathway enrichment analysis (Enrichr, GSVA). However, in principle results from any method or tool can be added to the Cerebro object.<br>
    <br>
    <b>Enrichr</b><br>
    Using all marker genes identified for a respective group of cells, gene list enrichment analysis is performed using the Enrichr API, including gene ontology terms, KEGG and Wiki Pathways, BioCarta and many others. Terms are sorted based on the combined score. By default, the genes that overlap between the marker gene list and a term are not shown (for better visibility) but the column can be added using the 'Column visibility' button. For the details on the combined score is calculated, please refer to the <a target='_blank' href='http://amp.pharm.mssm.edu/Enrichr/'>Enrichr website</a> and publication.<br>
    <br>
    <b>GSVA</b><br>
    GSVA (Gene Set Variation Analysis) is a method to perform gene set enrichment analysis. Diaz-Mejia and colleagues found GSVA to perform well compared to other tools ('Evaluation of methods to assign cell type labels to cell clusters from single-cell RNA-sequencing data' F1000Research, 2019). Statistics (p-value and adj. p-value) are calculated as done by Diaz-Mejia et al. Columns with the gene lists for each term and the enrichment score are hidden by default but can be made visible through the 'Column visibility' button. More details about GSVA can be found on the <a target='_blank', href='https://bioconductor.org/packages/release/bioc/html/GSVA.html'>GSVA Bioconductor page</a>.
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
