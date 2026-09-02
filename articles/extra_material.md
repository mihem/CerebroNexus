# Extra Material

## Overview

The **Extra material** tab provides a space for custom tables or plots
bundled alongside your single-cell data. Common use cases include:

- Cell type annotation results (e.g., SingleR scores)
- Custom QC summary tables
- Publication-ready figures
- Any additional analysis you want to share with collaborators

The tab appears *conditionally* — when the `.crb` contains embedded
material or a generated app includes external tables through
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md).
You control what appears, so the interface stays clean when no extra
content is available.

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

## Embedding content in a CRB

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

Embedded tables and plots travel with the `.crb` and remain available
wherever that data file is opened.

## Supplying external tables to a generated app

Use `extra_tables` in
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
when tables should belong to one generated deployment rather than to the
CRB itself:

``` r
createShinyApp(
  cerebro_data = c("PBMC" = "pbmc.crb"),
  result_dir = "pbmc-viewer",
  extra_tables = list(
    "QC summary" = "results/qc.csv",
    "Annotation workbook" = "results/annotations.xlsx"
  ),
  extra_tables_sheets = list(
    "Annotation workbook" = list("Cell labels" = "Sheet1")
  ),
  launch_browser = FALSE
)
```

CSV, TSV, TXT, XLS, XLSX, and XLSM files are converted during the build
to private RDS assets. Non-empty workbook sheets appear separately;
mapped names replace their source sheet names, while unmapped names are
retained. The Viewer loads a sheet only after it is selected. Original
source paths are not written to the generated configuration, and
formula-like text is neutralized before display or download.

In short, `exportFromSeurat(extra_material = ...)` embeds content in the
CRB; `createShinyApp(extra_tables = ...)` adds deployment-specific
external tables without modifying that CRB.

## See also

[`vignette("export_and_visualize_custom_tables_and_plots")`](https://mihem.github.io/CerebroNexus/articles/export_and_visualize_custom_tables_and_plots.md)
for a detailed walkthrough with examples.
