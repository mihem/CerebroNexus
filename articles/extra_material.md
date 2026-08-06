# Extra Material

## Overview

The **Extra material** tab provides a space for custom tables or plots
bundled alongside your single-cell data. Common use cases include:

- Cell type annotation results (e.g., SingleR scores)
- Custom QC summary tables
- Publication-ready figures
- Any additional analysis you want to share with collaborators

The tab appears *conditionally* — only when the `.crb` file contains
extra material. You control what appears, so the interface stays clean
when no extra content is needed.

## Quick start

``` r
library(CerebroNexus)
launchCerebro()
```

1.  Launch CerebroNexus and load a `.crb` file with extra material
2.  If extra material is present, **Extra material** appears in the
    sidebar
3.  Select a category (`tables` or `plots`)
4.  Choose a specific table or plot to view

## Content categories

**Tables**: any `data.frame`. Rendered as interactive DT tables with
search, sort, and download.

**Plots**: any `ggplot2` object. Displayed at original resolution with
download support.

## Embedding content

Use the `extra_material` parameter in
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md):

``` r
exportFromSeurat(seurat_object,
  file = "my_data.crb",
  extra_material = list(
    tables = list(SingleR_results = annotation_df),
    plots  = list(umap_overview = umap_plot)
  )
)
```

## See also

[`vignette("export_and_visualize_custom_tables_and_plots")`](https://mihem.github.io/CerebroNexus/articles/export_and_visualize_custom_tables_and_plots.md)
for a detailed walkthrough with examples.
