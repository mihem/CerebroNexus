# Export Seurat object to Cerebro.

This function allows to export a Seurat object to visualize in Cerebro.

## Usage

``` r
exportFromSeurat(
  object,
  assay = "RNA",
  slot = "data",
  file,
  experiment_name,
  organism,
  groups,
  cell_cycle = NULL,
  nUMI = "nUMI",
  nGene = "nGene",
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  verbose = FALSE
)
```

## Arguments

- object:

  Seurat object.

- assay:

  Assay to pull expression values from; defaults to `RNA`.

- slot:

  Slot to pull expression values from; defaults to `data`. It is
  recommended to use sparse data (such as log-transformed or raw counts)
  instead of dense data (such as the `scaled` slot) to avoid performance
  bottlenecks in the Cerebro interface.

- file:

  Where to save the output.

- experiment_name:

  Experiment name.

- organism:

  Organism, e.g. `hg` (human), `mm` (mouse), etc.

- groups:

  Names of grouping variables in meta data (`object@meta.data`), e.g.
  `c("sample","cluster")`; at least one must be provided; defaults to
  `NULL`.

- cell_cycle:

  Names of columns in meta data (`object@meta.data`) that contain cell
  cycle information, e.g. `c("Phase")`; defaults to `NULL`.

- nUMI:

  Column in `object@meta.data` that contains information about number of
  transcripts per cell; defaults to `nUMI`.

- nGene:

  Column in `object@meta.data` that contains information about number of
  expressed genes per cell; defaults to `nGene`.

- add_all_meta_data:

  If set to `TRUE`, all further meta data columns will be extracted as
  well.

- use_delayed_array:

  When set to `TRUE`, the expression matrix will be stored as an
  `RleMatrix` (see `DelayedArray` package). This can be useful for very
  large data sets, as the matrix won't be loaded into memory and instead
  values will be read from the disk directly, at the cost of
  performance. Note that it is necessary to install the `DelayedArray`
  package. If set to `FALSE` (default), the expression matrix will be
  copied from the input object as is. It is recommended to use a sparse
  format, such as `dgCMatrix` from the `Matrix` package.

- verbose:

  Set this to `TRUE` if you want additional log messages; defaults to
  `FALSE`.

## Value

No data returned.

## Examples

``` r
pbmc <- readRDS(system.file("extdata/v1.4/pbmc_seurat.rds",
  package = "cerebroAppLite"))
exportFromSeurat(
  object = pbmc,
  file = file.path(tempdir(), 'pbmc_Seurat.crb'),
  experiment_name = 'PBMC',
  organism = 'hg',
  groups = c('sample','seurat_clusters'),
  nUMI = 'nCount_RNA',
  nGene = 'nFeature_RNA',
  use_delayed_array = FALSE,
  verbose = TRUE
)
#> [20:33:46] Initializing Cerebro object...
#> [20:33:46] Collecting available meta data...
#> [20:33:46] Extracting all meta data columns...
#> [20:33:46] Extracting dimensional reductions...
#> [20:33:46] Will export the following dimensional reductions: umap
#> [20:33:46] Extracting tables of marker genes...
#> [20:33:46] No trajectories to extract...
#> [20:33:46] Overview of Cerebro object:
#> class: Cerebro_v1.3
#> cerebroApp version: 1.5.3
#> experiment name: PBMC
#> organism: hg
#> date of analysis: 
#> date of export: 2026-05-10
#> number of cells: 80
#> number of genes: 230
#> grouping variables (2): sample, seurat_clusters
#> cell cycle variables (0): 
#> projections (1): umap
#> trees (0): 
#> most expressed genes: 
#> marker genes:
#>   - cerebro_seurat (1): seurat_clusters
#> enriched pathways:
#> trajectories:
#> extra material:
#> [20:33:46] Saving Cerebro object to: /tmp/nix-shell-4558-2912008037/RtmpOM910A/pbmc_Seurat.crb
#> [20:33:46] Done!
```
