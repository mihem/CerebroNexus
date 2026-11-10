# Read a Cerebro data file

Reads a CRB written by
[`saveCerebro()`](https://www.mheming.com/CerebroNexus/reference/saveCerebro.md)
or the export functions, as RDS or qs2. An H5-backed expression matrix
is attached lazily and a thin CRB gets its cell names back from the H5
file; spatial molecule tables stay lazy until their FOV is requested. A
CRB from an older release that this version no longer reads (for example
one without an expression-backend descriptor, or one with a singular
spatial image or an embedded spatial molecule table) is rejected with a
pointer to
[`convertCerebro`](https://www.mheming.com/CerebroNexus/reference/convertCerebro.md).

## Usage

``` r
readCerebro(file)
```

## Arguments

- file:

  Input `.crb` path.

## Value

A current Cerebro object.

## Examples

``` r
crb <- system.file("extdata/examples/example.crb", package = "CerebroNexus")
cerebro <- readCerebro(crb)
cerebro
#> class: Cerebro
#> exporter package version: 1.7.3
#> experiment name: pbmc5p_v2_hs
#> organism: hg
#> date of analysis: 
#> date of export: 2026-06-25
#> number of cells: 1,000
#> number of genes: 100
#> grouping variables (3): sample, seurat_clusters, cell_type
#> cell cycle variables (0): 
#> projections (1): umap
#> trees (0): 
#> most expressed genes: sample, seurat_clusters, cell_type
#> marker genes:
#>   - cerebro_seurat (1): seurat_clusters
#> enriched pathways:
#>   - cerebro_seurat_enrichr (1): seurat_clusters, 
#>   - cerebro_GSVA (1): seurat_clusters
#> trajectories:
#> extra material:
#>   - tables (1): cluster_cell_counts
#> Immune repertoire:sample_1, sample_2, sample_3
#> HLA typing: none
#> Spatial data:
```
