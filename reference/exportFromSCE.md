# Export SingleCellExperiment (SCE) object to Cerebro.

This function allows to export a `SingleCellExperiment` (`SCE`) object
to visualize in Cerebro.

## Usage

``` r
exportFromSCE(
  object,
  assay = "logcounts",
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

  `SingleCellExperiment` (`SCE`) object.

- assay:

  Assay to pull expression values from; defaults to `logcounts`. It is
  recommended to use sparse data (such as log-transformed or raw counts)
  instead of dense data (such as the 'scaled' slot) to avoid performance
  bottlenecks in the Cerebro interface.

- file:

  Where to save the output.

- experiment_name:

  Experiment name.

- organism:

  Organism, e.g. `hg` (human), `mm` (mouse), etc.

- groups:

  Names of grouping variables in meta data
  (`SingleCellExperiment::colData(object)`), e.g.
  `c("sample","cluster")`; at least one must be provided; defaults to
  `NULL`.

- cell_cycle:

  Names of columns in meta data
  (`SingleCellExperiment::colData(object)`) that# contain cell cycle
  information, e.g. `c("Phase")`; defaults to `NULL`.

- nUMI:

  Column in `SingleCellExperiment::colData(object)` that contains
  information about number of transcripts per cell; defaults to `nUMI`.

- nGene:

  Column in `SingleCellExperiment::colData(object)` that contains
  information about number of expressed genes per cell; defaults to
  `nGene`.

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
pbmc <- readRDS(system.file("extdata/v1.4/pbmc_SCE.rds",
  package = "cerebroAppLite"))
exportFromSCE(
  object = pbmc,
  file = file.path(tempdir(), 'pbmc_SCE.crb'),
  experiment_name = 'PBMC',
  organism = 'hg',
  groups = c('sample','cluster'),
  nUMI = 'nUMI',
  nGene = 'nGene',
  use_delayed_array = FALSE,
  verbose = TRUE
)
#> [07:37:51] Initializing Cerebro object...
#> [07:37:51] Collecting available meta data...
#> [07:37:51] Extracting all meta data columns...
#> [07:37:51] Extracting dimensional reductions...
#> [07:37:51] Will export the following dimensional reductions: UMAP
#> [07:37:51] No trajectories to extract...
#> [07:37:51] Overview of Cerebro object:
#> class: Cerebro_v1.3
#> cerebroApp version: 1.7.0
#> experiment name: PBMC
#> organism: hg
#> date of analysis: 
#> date of export: 2026-06-14
#> number of cells: 80
#> number of genes: 230
#> grouping variables (2): sample, cluster
#> cell cycle variables (0): 
#> projections (1): UMAP
#> trees (0): 
#> most expressed genes: 
#> marker genes:
#> enriched pathways:
#> trajectories:
#> extra material:
#> Immune repertoire:
#> Spatial data:
#> [07:37:51] Saving Cerebro object to: /tmp/nix-shell-5227-3309790359/RtmpMnZPWI/pbmc_SCE.crb
#> [07:37:51] Done!
```
