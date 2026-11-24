# Prepare and export a Seurat object in R

## Overview

This vignette starts from a Seurat object in which the analysis is
already done, adds the tables that CerebroNexus pre-computes (most
expressed genes, marker genes, enriched pathways), and exports
everything to a `.crb` file. It does not cover quality control,
normalization, clustering or dimensional reduction; for those steps,
please see the [Seurat tutorials](https://satijalab.org/seurat/).

These steps are optional: a Seurat object can also be imported as it is,
with the Builder or
[`convertSeuratToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSeuratToCerebro.md);
see [From your analysis to an interactive
Viewer](https://www.mheming.com/CerebroNexus/articles/import_your_analysis.md).
The functions shown here take a Seurat object. The same steps, except
the gene set enrichment, can also be switched on in the Builder
([`launchCerebroBuilder()`](https://www.mheming.com/CerebroNexus/reference/launchCerebroBuilder.md));
see [Build a data set without writing
code](https://www.mheming.com/CerebroNexus/articles/build_a_data_set_by_pointing.html).

## Installation

Install CerebroNexus as described in the [installation
instructions](https://www.mheming.com/CerebroNexus/#id_1-installation).
This guide also uses [Seurat](https://satijalab.org/seurat/)
(`install.packages('Seurat')`).

## Start from a processed Seurat object

CerebroNexus expects an object in which the usual analysis is done:

- normalized expression values,
- at least one grouping variable in the meta data, e.g. samples or
  clusters,
- at least one dimensional reduction, e.g. UMAP or t-SNE.

Here we use `pbmc_small`, a processed example of 80 cells and 230 genes
that ships with Seurat. It carries clusters, an arbitrary two-level
grouping (`groups`), and PCA and t-SNE coordinates. We copy the clusters
to `seurat_clusters`, the column name Seurat uses in a standard
analysis. With your own data, load your object instead and use your own
column names.

``` r
library(Seurat)
library(CerebroNexus)

data('pbmc_small')
seurat <- UpdateSeuratObject(pbmc_small)
seurat$seurat_clusters <- seurat$RNA_snn_res.1
```

## CerebroNexus functions

Now we use CerebroNexus functions to obtain more information about the
data set and its groups. Each function stores its result in the Seurat
object, from where the export picks it up.

### Calculate percentage of mitochondrial and ribosomal transcripts

First, for every cell we calculate the percentage of mitochondrial and
ribosomal transcripts of all transcripts using the
[`addPercentMtRibo()`](https://www.mheming.com/CerebroNexus/reference/addPercentMtRibo.md)
function.

``` r
seurat <- addPercentMtRibo(
  seurat,
  organism = 'hg',
  gene_nomenclature = 'name'
)
```

### Get most expressed genes

Another interesting perspective is to look at the 100 most expressed
genes for every group. This can help in understanding the
transcriptional activity of a given cell population.

``` r
seurat <- getMostExpressedGenes(
  seurat,
  assay = 'RNA',
  groups = c('seurat_clusters','groups')
)
```

### Identify marker genes

Marker genes are genes which are particularly strongly or weakly
expressed in a given cell population, e.g. a cluster. This information
can help to distinguish the role of different cell groups in the data
set. We can identify marker genes using the
[`getMarkerGenes()`](https://www.mheming.com/CerebroNexus/reference/getMarkerGenes.md)
function which internally uses the
[`Seurat::FindAllMarkers()`](https://satijalab.org/seurat/reference/FindAllMarkers.html)
function but also stores the results in the Seurat object under the
`misc` slot.

``` r
seurat <- getMarkerGenes(
  seurat,
  assay = 'RNA',
  organism = 'hg',
  groups = c('seurat_clusters','groups'),
  name = 'cerebro_seurat',
  only_pos = TRUE
)
```

### Perform pathway enrichment on marker genes

Using the previously identified marker genes, we can perform a pathway
enrichment analysis using the
[`getEnrichedPathways()`](https://www.mheming.com/CerebroNexus/reference/getEnrichedPathways.md)
function. The function will internally use the
[Enrichr](https://maayanlab.cloud/Enrichr/) API and store the results in
the Seurat object under the `misc` slot. All we have to do is provide
the name that we specified in
[`getMarkerGenes()`](https://www.mheming.com/CerebroNexus/reference/getMarkerGenes.md).

``` r
seurat <- getEnrichedPathways(
  seurat,
  marker_genes_input = 'cerebro_seurat',
  adj_p_cutoff = 0.01,
  max_terms = 100
)
```

### Gene set enrichment

On top of querying the databases of the
[Enrichr](https://maayanlab.cloud/Enrichr/) service, one can also
perform gene set enrichment analysis on gene sets provided as a `.gmt`
file, e.g. from the
[MSigDB](https://www.gsea-msigdb.org/gsea/msigdb/index.jsp). The
[`performGeneSetEnrichmentAnalysis()`](https://www.mheming.com/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md)
function uses the
[GSVA](https://bioconductor.org/packages/release/bioc/html/GSVA.html)
method in combination with additional statistics as published by
[Diaz-Mejia et al.](https://f1000research.com/articles/8-296). It needs
the Bioconductor packages
[GSVA](https://bioconductor.org/packages/release/bioc/html/GSVA.html)
and
[qvalue](https://bioconductor.org/packages/release/bioc/html/qvalue.html):

``` r
install.packages('BiocManager')
BiocManager::install(c('GSVA', 'qvalue'))
```

``` r
example_gene_set <- system.file("extdata/example_gene_set.gmt", package = "CerebroNexus")

seurat <- performGeneSetEnrichmentAnalysis(
  seurat,
  assay = 'RNA',
  GMT_file = example_gene_set,
  groups = c('seurat_clusters','groups')
)
```

## Optional extras

Two more kinds of information are exported if the Seurat object carries
them.

### Cell cycle

If the meta data has a cell cycle column, e.g. `Phase` from Seurat’s
[`CellCycleScoring()`](https://satijalab.org/seurat/reference/CellCycleScoring.html)
(see the [Seurat cell cycle
vignette](https://satijalab.org/seurat/articles/cell_cycle_vignette)),
pass its name to the export with `cell_cycle = 'Phase'`. The viewer then
colours it with its cell cycle palette. The example data has too few
cell cycle genes to calculate it here.

### 3D projections

By default, every dimensional reduction except PCA is exported; one with
three dimensions, such as a UMAP computed with `n.components = 3`, is
shown as a 3D projection.

## Trajectories (optional)

A Monocle 2 trajectory can be attached to the Seurat object with
[`extractMonocleTrajectory()`](https://www.mheming.com/CerebroNexus/reference/extractMonocleTrajectory.md)
before the export, and is then shown in the **Trajectory** tab. See
[Trajectory
analysis](https://www.mheming.com/CerebroNexus/articles/trajectory_analysis.html)
for the code.

## Export to Cerebro format

Finally, we use CerebroNexus
[`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
to export our Seurat object to a `.crb` file which can be loaded into
CerebroNexus.

``` r
crb_file <- paste0('cerebro_pbmc_seurat_', Sys.Date(), '.crb')

exportFromSeurat(
  seurat,
  assay = 'RNA',
  slot = 'data',
  file = crb_file,
  experiment_name = 'pbmc',
  organism = 'hg',
  groups = c('seurat_clusters','groups'),
  nUMI = 'nCount_RNA',
  nGene = 'nFeature_RNA',
  add_all_meta_data = TRUE,
  verbose = FALSE
)
```

By default the expression matrix is embedded in the `.crb`. For large
data sets, add `expression_matrix_mode = "h5"` to write the matrix to a
sibling `.h5` file instead (the only other storage mode); keep the two
files together. A very large immune repertoire is moved to a third
sibling file (`immune_sidecar = "auto"`). See [Working with large
datasets](https://www.mheming.com/CerebroNexus/articles/million_cell_crb_artifacts.html#store-the-expression-matrix-in-an-h5-file).

## Open the result

Open the file in the Viewer:

``` r
launchCerebro(crb_file_to_load = crb_file)
```

The pages **Marker genes**, **Most expressed genes** and **Enriched
pathways** show the tables computed above:

![The Marker genes page of the Viewer, annotated. 1: the Method field
shows cerebro_seurat, the name given to getMarkerGenes(). 2: the
Grouping variable field shows seurat_clusters, one of the groups given
to getMarkerGenes(). 3: the subgroup filter shows cluster 1. 4: the
table columns seurat_clusters, gene, p_val, avg_log2FC, pct.1, pct.2 and
p_val_adj come from Seurat::FindAllMarkers(); the first rows are S100A8,
TYMP, S100A9, LYZ and CST3. 5: the sidebar pages Marker genes, Most
expressed genes and Enriched pathways, which appear once
getMarkerGenes(), getMostExpressedGenes(), getEnrichedPathways() or
performGeneSetEnrichmentAnalysis() have stored their
tables.](../reference/figures/viewer-marker-genes.png)

The Marker genes page for the exported example. The numbers link the
code above to what the page shows.

With this small example,
[`performGeneSetEnrichmentAnalysis()`](https://www.mheming.com/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md)
finds no gene set that meets its thresholds, and the Enriched pathways
page says so for that method. Tables can be downloaded in CSV or Excel
format.

For a direct image export, click **Download PNG** in the plot toolbar.
If the current view supports portable sharing, you can instead open
**Share view** and click **Download PNG** there. The Share view dialog
also lets you download or open a portable JSON configuration containing
the current display state and selected cell barcodes, so the same view
can be restored in another compatible CerebroNexus session without
including the source data.

To share the data set itself, see [Share data sets as a Shiny
app](https://www.mheming.com/CerebroNexus/articles/create_a_self_contained_shiny_app.html).

## See also

If your Seurat v5 assay is split into one layer per sample, see [Seurat
v5 layered assays: complete and safe expression
matrices](https://www.mheming.com/CerebroNexus/articles/seurat_v5_layered_assays.html)
for the automatic resolution rules, custom-layer safeguards, and
troubleshooting steps.

## Session info

``` r
sessionInfo()
```
