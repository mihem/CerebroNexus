# Get most expressed genes for specified grouping variables in Seurat object.

This function calculates the most expressed genes for one or multiple
grouping variables in the meta data of the provided Seurat object.

## Usage

``` r
getMostExpressedGenes(object, assay = "RNA", groups = NULL)
```

## Arguments

- object:

  Seurat object.

- assay:

  Assay to pull transcripts counts from; defaults to 'RNA'.

- groups:

  Grouping variables (columns) in `object@meta.data` for which most
  expressed genes should be calculated; defaults to NULL.

## Value

Seurat object with most expressed genes stored for every group level of
the specified groups stored in `object@misc$most_expressed_genes`.

## Examples

``` r
pbmc <- readRDS(system.file("extdata/v1.4/pbmc_seurat.rds",
  package = "cerebroAppLite"))
pbmc <- getMostExpressedGenes(
  object = pbmc,
  assay = 'RNA',
  groups = c('sample','seurat_clusters')
)
#> [15:18:45] Get most expressed genes for 2 groups in `sample`...
#> [15:18:45] Get most expressed genes for 2 groups in `seurat_clusters`...
```
