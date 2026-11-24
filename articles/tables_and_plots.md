# Tables and plots in the Viewer

## Overview

Four pages of the Viewer show tables and plots stored in the `.crb`
file. Each page appears in the sidebar only when the loaded data set
contains its content.

| Page | Shows | Filled by |
|----|----|----|
| Most expressed genes | the 100 genes with the largest share of all transcripts in each group | [`getMostExpressedGenes()`](https://www.mheming.com/CerebroNexus/reference/getMostExpressedGenes.md); the `convert*()` functions add them on their own |
| Marker genes | genes that distinguish a group from the other groups | [`getMarkerGenes()`](https://www.mheming.com/CerebroNexus/reference/getMarkerGenes.md), or your own table |
| Enriched pathways | pathway and gene set enrichment results per group | [`getEnrichedPathways()`](https://www.mheming.com/CerebroNexus/reference/getEnrichedPathways.md), [`performGeneSetEnrichmentAnalysis()`](https://www.mheming.com/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md), or your own table |
| Extra material | any other tables, and plots made with ggplot2 | your own tables and plots |

[`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
computes none of these tables itself: it writes what the Seurat object
carries. Without them, the first three pages stay hidden.

## In the Viewer

On **Marker genes**, **Most expressed genes** and **Enriched pathways**,
choose the grouping variable, and for marker genes and pathways the
method. When the first column of a table is that grouping variable,
**Filter results for subgroup** shows one group at a time; switch on
**Show results for all subgroups (no pre-filtering)** to see the whole
table. For data sets made with a `convert*()` function, **Most expressed
genes** can also show the mean expression per group.

The tables can be searched and sorted, and **Download** saves them as
CSV or Excel files. Where they appear, **Automatically format numbers**
and **Highlight values with colours** change only how the values are
shown.

On **Extra material**, choose between tables and plots, then the item.
Plots are converted to interactive plotly graphics. If the conversion
misses something, such as a legend, switch off **Make plot interactive**
to see the ggplot2 plot as it was made.

## Compute the tables

For a Seurat object, the CerebroNexus functions store their results in
the object’s `@misc` slot, where
[`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
picks them up. This example uses `pbmc_small`, which ships with Seurat:

``` r
library(Seurat)
library(CerebroNexus)

data("pbmc_small")
seurat <- UpdateSeuratObject(pbmc_small)
seurat$seurat_clusters <- seurat$RNA_snn_res.1

seurat <- getMostExpressedGenes(
  seurat,
  assay = "RNA",
  groups = c("seurat_clusters", "groups")
)
seurat <- getMarkerGenes(
  seurat,
  assay = "RNA",
  organism = "hg",
  groups = c("seurat_clusters", "groups"),
  name = "cerebro_seurat",
  only_pos = TRUE
)
```

For enriched pathways, run
[`getEnrichedPathways()`](https://www.mheming.com/CerebroNexus/reference/getEnrichedPathways.md)
on the marker genes (it queries the Enrichr web service) or
[`performGeneSetEnrichmentAnalysis()`](https://www.mheming.com/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md)
with your own gene sets; see [Prepare and export a Seurat object in
R](https://www.mheming.com/CerebroNexus/articles/cerebronexus_workflow_seurat.md).

[`convertSeuratToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSeuratToCerebro.md),
[`convertSCEToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSCEToCerebro.md)
and
[`convertH5adToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertH5adToCerebro.md)
add the most expressed genes and the mean expression per group on their
own (`add_most_expressed_genes = TRUE`). They can read a marker gene
table from a file (`marker_file`), and
[`convertH5adToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertH5adToCerebro.md)
takes the marker genes that scanpy ranked; see [From your analysis to an
interactive
Viewer](https://www.mheming.com/CerebroNexus/articles/import_your_analysis.md).

## Add your own tables and plots

### In a Seurat object

Put your results in `@misc` before the export. Marker genes and enriched
pathways are lists of methods, each a list of tables named by grouping
variable. Extra material holds `tables` (data frames) and `plots`
(ggplot2 objects):

``` r
my_markers <- data.frame(
  seurat_clusters = c("0", "1"),
  gene = c("CD3E", "MS4A1"),
  score = c(2.1, 1.7)
)
seurat@misc$marker_genes$my_method <- list(seurat_clusters = my_markers)

my_pathways <- data.frame(
  seurat_clusters = "0",
  term = "T cell activation",
  p_value = 0.001
)
seurat@misc$enriched_pathways$my_enrichment <- list(
  seurat_clusters = my_pathways
)

seurat@misc$extra_material <- list(
  tables = list(
    cell_counts = as.data.frame(table(cluster = seurat$seurat_clusters))
  ),
  plots = list(
    dot_plot = DotPlot(
      seurat,
      features = c("CD3E", "MS4A1", "LYZ"),
      group.by = "seurat_clusters"
    )
  )
)

exportFromSeurat(
  seurat,
  assay = "RNA",
  slot = "data",
  file = "pbmc_tables.crb",
  experiment_name = "pbmc",
  organism = "hg",
  groups = c("seurat_clusters", "groups"),
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA"
)
```

A marker gene table may have any name and any columns. An enriched
pathways table is kept only when its name is one of the exported
`groups`. Name the first column after the grouping variable, as above,
so that the Viewer can filter the table by group.

### In an existing `.crb` file

Data sets from scanpy or SingleCellExperiment have no `@misc` slot. Add
tables and plots to the `.crb` file instead, with the methods of the
Cerebro object:

``` r
library(ggplot2)

crb <- readCerebro("pbmc_tables.crb")
cells <- crb$getMetaData()

crb$addExtraTable(
  "cells_per_cluster",
  as.data.frame(table(cluster = cells$seurat_clusters))
)
crb$addExtraPlot(
  "cells_per_cluster",
  ggplot(cells, aes(seurat_clusters)) + geom_bar()
)
saveCerebro(crb, "pbmc_tables.crb")
```

`addMarkerGenes()`, `addEnrichedPathways()` and
`addMostExpressedGenes()` add the other tables; see
[`?Cerebro`](https://www.mheming.com/CerebroNexus/reference/Cerebro.md).

### For one generated app only

`createShinyApp(extra_tables = ...)` adds CSV, TSV or Excel files to the
Extra material page of a generated app without changing the `.crb` file;
see [External Extra material
tables](https://www.mheming.com/CerebroNexus/articles/create_a_self_contained_shiny_app.html#external-extra-material-tables).

## Open the result

``` r
launchCerebro(crb_file_to_load = "pbmc_tables.crb")
```

## See also

- [Prepare and export a Seurat object in
  R](https://www.mheming.com/CerebroNexus/articles/cerebronexus_workflow_seurat.md)
  for computing the tables, including enriched pathways
- [Overview of the Cerebro
  class](https://www.mheming.com/CerebroNexus/articles/overview_of_cerebro_class.md)
  for the structure of a `.crb` file, and
  [`?Cerebro`](https://www.mheming.com/CerebroNexus/reference/Cerebro.md)
  for its methods
- [Share data sets as a Shiny
  app](https://www.mheming.com/CerebroNexus/articles/create_a_self_contained_shiny_app.md)
