##----------------------------------------------------------------------------##
## Tab: Marker genes
##----------------------------------------------------------------------------##
cerebroResultPageServer(
  input,
  output,
  session,
  "marker_genes",
  methods = getMethodsForMarkerGenes,
  groups = getGroupsWithMarkerGenes,
  results = getMarkerGenes,
  messages = list(
    no_methods = "No marker-gene methods available.",
    not_included = "Marker-gene results are not included in this data set. Choose a data set that includes marker-gene results.",
    no_groups = "No marker results. Choose another method.",
    no_results = "No marker results for this grouping. Choose another method or grouping."
  ),
  statuses = list(
    no_markers_found = "No marker genes meet the saved thresholds. Choose another method or grouping."
  )
)

cerebroRegisterInfo(input, "marker_genes_info", marker_genes_info)

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
