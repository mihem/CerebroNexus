# Most Expressed Genes

## Overview

The **Most expressed genes** tab shows, for every group in the dataset,
the genes with the highest mean expression. Unlike marker genes (which
identify genes that differ *between* groups), most expressed genes
simply rank genes by their average expression *within* each group.

This tab is always visible in the sidebar because expression data is
present in every `.crb` file.

## Quick start

``` r
library(CerebroNexus)
launchCerebro()
```

1.  Launch CerebroNexus and load a `.crb` file
2.  Click **Most expressed genes** in the sidebar
3.  Select a grouping variable (e.g., `seurat_clusters`, `sample`)
4.  The table shows, for each group, the gene name, mean expression, and
    percentage of cells expressing that gene

## Table features

The results table is rendered as an interactive DT table:

- **Search**: filter genes by name across all groups
- **Sort**: click column headers to sort by expression, percentage, or
  group
- **Download**: use the export buttons to save results as CSV or Excel

## Data export

Most expressed gene tables are computed automatically during
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md).
No additional parameters are required.

``` r
exportFromSeurat(seurat_object, file = "my_data.crb")
```

## See also

[`vignette("cerebronexus_workflow_seurat")`](https://mihem.github.io/CerebroNexus/articles/cerebronexus_workflow_seurat.md)
for the complete export workflow.
