# Calculate percentage of transcripts of gene list.

Get percentage of transcripts of gene list compared to all transcripts
per cell.

## Usage

``` r
calculatePercentGenes(object, assay = "RNA", genes)
```

## Arguments

- object:

  Seurat object.

- assay:

  Assay to pull counts from; defaults to 'RNA'. Only relevant in Seurat
  v3.0 or higher since the concept of assays wasn't implemented before.

- genes:

  List(s) of genes.

## Value

List of lists containing the percentages of expression for each provided
gene list.

## Examples

``` r
pbmc <- readRDS(system.file("extdata/examples/pbmc_seurat.rds",
  package = "CerebroNexus"))
pbmc <- calculatePercentGenes(
  object = pbmc,
  assay = 'RNA',
  genes = list('example' = c('FCN1','CD3D'))
)
```
