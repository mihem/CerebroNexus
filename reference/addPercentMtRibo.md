# Add percentage of mitochondrial and ribosomal transcripts.

Get percentage of transcripts of gene list compared to all transcripts
per cell.

## Usage

``` r
addPercentMtRibo(object, assay = "RNA", organism, gene_nomenclature)
```

## Arguments

- object:

  Seurat object.

- assay:

  Assay to pull counts from; defaults to 'RNA'. Only relevant in Seurat
  v3.0 or higher since the concept of assays wasn't implemented before.

- organism:

  Organism, can be either human ('hg') or mouse ('mm'). Genes need to
  annotated as gene symbol, e.g. MKI67 (human) / Mki67 (mouse).

- gene_nomenclature:

  Define if genes are saved by their name ('name'), ENSEMBL ID
  ('ensembl') or GENCODE ID ('gencode_v27', 'gencode_vM16').

## Value

Seurat object with two new meta data columns containing the percentage
of mitochondrial and ribosomal gene expression for each cell.

## Examples

``` r
pbmc <- readRDS(system.file("extdata/v1.4/pbmc_seurat.rds",
  package = "cerebroAppLite"))
pbmc <- addPercentMtRibo(
  object = pbmc,
  assay = 'RNA',
  organism = 'hg',
  gene_nomenclature = 'name'
)
#> [14:32:45] No mitochondrial genes found in data set.
#> [14:32:45] Calculate percentage of 1 ribosomal transcript(s) present in the data set...
```
