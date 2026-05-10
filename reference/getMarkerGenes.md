# Get marker genes for specified grouping variables in Seurat object.

This function gets marker genes for one or multiple grouping variables
in the meta data of the provided Seurat object.

## Usage

``` r
getMarkerGenes(
  object,
  assay = "RNA",
  organism = NULL,
  groups = NULL,
  name = "cerebro_seurat",
  only_pos = TRUE,
  min_pct = 0.7,
  thresh_logFC = 0.25,
  thresh_p_val = 0.01,
  test = "wilcox",
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  Seurat object.

- assay:

  Assay to pull transcripts counts from; defaults to 'RNA'.

- organism:

  Organism information for pulling info about presence of marker genes
  of cell surface; can be omitted if already saved in Seurat object;
  defaults to NULL.

- groups:

  Grouping variables (columns) in object@meta.data for which marker
  genes should be calculated.

- name:

  Name of list that should be used to store the results in
  `object@misc$marker_genes$<name>`; defaults to 'cerebro_seurat'.

- only_pos:

  Identify only over-expressed genes; defaults to TRUE.

- min_pct:

  Only keep genes that are expressed in at least n% of current group of
  cells, defaults to 0.70 (70%).

- thresh_logFC:

  Only keep genes that show an average logFC of at least n; defaults to
  0.25.

- thresh_p_val:

  Threshold for p-value, defaults to 0.01.

- test:

  Statistical test used, defaults to 'wilcox' (Wilcoxon test).

- verbose:

  Print progress bar; defaults to TRUE.

- ...:

  Further parameters can be passed to control Seurat::FindAllMakers().

## Value

Seurat object with marker gene results for the specified grouping
variables stored in `object@misc$marker_genes`.

## Examples

``` r
pbmc <- readRDS(system.file("extdata/v1.4/pbmc_seurat.rds",
  package = "cerebroAppLite"))
pbmc <- getMarkerGenes(
  object = pbmc,
  assay = 'RNA',
  organism = 'hg',
  groups = c('sample','seurat_clusters'),
  name = 'cerebro_seurat',
  only_pos = TRUE,
  min_pct = 0.7,
  thresh_logFC = 0.25,
  thresh_p_val = 0.01,
  test = 'wilcox',
  verbose = TRUE
)
#> [14:33:12] Get marker genes for 2 groups in `sample`...
#> Calculating cluster pbmc_1
#> For a (much!) faster implementation of the Wilcoxon Rank Sum Test,
#> (default method for FindMarkers) please install the presto package
#> --------------------------------------------
#> install.packages('devtools')
#> devtools::install_github('immunogenomics/presto')
#> --------------------------------------------
#> After installation of presto, Seurat will automatically use the more 
#> efficient implementation (no further action necessary).
#> This message will be shown once per session
#> Warning: cannot compute exact p-value with ties
#> Calculating cluster pbmc_2
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact p-value with ties
#> [14:33:12] Get marker genes for 2 groups in `seurat_clusters`...
#> Calculating cluster 0
#> Calculating cluster 1
```
