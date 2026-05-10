# Perform gene set enrichment analysis with GSVA.

This function calculates enrichment scores, p- and q-value statistics
for provided gene sets for specified groups of cells in given Seurat
object using gene set variation analysis (GSVA). Calculation of p- and
q-values for gene sets is performed as done in "Evaluation of methods to
assign cell type labels to cell clusters from single-cell RNA-sequencing
data", Diaz-Mejia et al., F1000Research (2019).

## Usage

``` r
performGeneSetEnrichmentAnalysis(
  object,
  assay = "RNA",
  GMT_file,
  groups = NULL,
  name = "cerebro_GSVA",
  thresh_p_val = 0.05,
  thresh_q_val = 0.1,
  ...
)
```

## Arguments

- object:

  Seurat object.

- assay:

  Assay to pull counts from; defaults to 'RNA'. Only relevant in Seurat
  v3.0 or higher since the concept of assays wasn't implemented before.

- GMT_file:

  Path to GMT file containing the gene sets to be tested. The Broad
  Institute provides many gene sets which can be downloaded:
  http://software.broadinstitute.org/gsea/msigdb/index.jsp

- groups:

  Grouping variables (columns) in object@meta.data for which gene set
  enrichment analysis should be performed

- name:

  Name of list that should be used to store the results in
  object@misc\$enriched_pathways\$\<name\>; defaults to 'cerebro_GSVA'.

- thresh_p_val:

  Threshold for p-value, defaults to 0.05.

- thresh_q_val:

  Threshold for q-value, defaults to 0.1.

- ...:

  Further parameters passed to \`GSVA::gsvaParam()\` (GSVA \>= 2.0) or
  \`GSVA::gsva()\` (GSVA \< 2.0).

## Value

Seurat object with GSVA results for the specified grouping variables
stored in object@misc\$enriched_pathways\$\<name\>

## Examples

``` r
pbmc <- readRDS(system.file("extdata/v1.4/pbmc_seurat.rds",
  package = "cerebroAppLite"))
example_gene_set <- system.file("extdata/example_gene_set.gmt",
  package = "cerebroAppLite")
pbmc <- performGeneSetEnrichmentAnalysis(
  object = pbmc,
  GMT_file = example_gene_set,
  groups = c('sample','seurat_clusters'),
  thresh_p_val = 0.05,
  thresh_q_val = 0.1
)
#> [15:58:54] Loading gene sets...
#> [15:58:54] Loaded 2 gene sets from GMT file.
#> [15:58:54] Extracting transcript counts from `data` slot of `RNA` assay...
#> [15:58:54] Performing analysis for 2 subgroups of group `sample`...
#> ℹ GSVA version 2.4.7
#> ℹ Searching for rows with constant values
#> ℹ Calculating GSVA ranks
#> ℹ kcdf='auto' (default)
#> ℹ GSVA dense (classical) algorithm
#> ℹ Row-wise ECDF estimation with Gaussian kernels
#> ℹ Calculating row ECDFs
#> ℹ Calculating column ranks
#> ℹ GSVA dense (classical) algorithm
#> ℹ Calculating GSVA scores
#> ✔ Calculations finished
#> [15:58:54] 0 gene sets passed the thresholds across all subgroups of group `sample`.
#> [15:58:54] Performing analysis for 2 subgroups of group `seurat_clusters`...
#> ℹ GSVA version 2.4.7
#> ℹ Searching for rows with constant values
#> ℹ Calculating GSVA ranks
#> ℹ kcdf='auto' (default)
#> ℹ GSVA dense (classical) algorithm
#> ℹ Row-wise ECDF estimation with Gaussian kernels
#> ℹ Calculating row ECDFs
#> ℹ Calculating column ranks
#> ℹ GSVA dense (classical) algorithm
#> ℹ Calculating GSVA scores
#> ✔ Calculations finished
#> [15:58:55] 0 gene sets passed the thresholds across all subgroups of group `seurat_clusters`.
```
