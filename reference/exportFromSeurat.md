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
  main_group = NULL,
  cell_cycle = NULL,
  nUMI = "nUMI",
  nGene = "nGene",
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "bpcells", "h5"),
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

- main_group:

  The primary grouping variable to use for display in Cerebro; must be
  one of the grouping variables specified in `groups`; defaults to
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
  format, such as `dgCMatrix` from the `Matrix` package. Ignored when
  `expression_matrix_mode` is set to an external backend.

- expression_matrix_mode:

  How to persist the expression matrix. One of `"embedded"` (default),
  `"bpcells"`, or `"h5"`.

  - `"embedded"` stores the matrix inside the `.crb` file, as before.
    Compatible with all existing `.crb` readers.

  - `"bpcells"` writes the matrix to a BPCells on-disk directory next to
    the `.crb` and keeps only a lightweight handle in the serialised
    object. Recommended for large sparse matrices. The resulting `.crb`
    is portable as long as the sibling `.bpcells/` directory travels
    with it; the Shiny runtime re-resolves paths via
    `getExpressionBackend()$location` relative to the `.crb`'s parent
    directory (step 7.3 runtime attach).

  - `"h5"` writes the matrix via
    [`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html)
    to a TENx-format sparse HDF5 file next to the `.crb` (sibling
    `<stem>.h5`) and tags the backend with that relative location. The
    on-disk layout matches `inst/extdata/v1.4/example.h5`: a single
    `/expression` group with `data`, `indices`, `indptr`, `shape`,
    `genes`, and `barcodes` datasets. The matrix is stored cells x genes
    (TENx column-favoured, optimised for per-gene reads); the Shiny
    runtime attach reads it back as a lazy
    [`HDF5Array::TENxMatrix`](https://rdrr.io/pkg/HDF5Array/man/TENxMatrix-class.html)
    seed and transposes it lazily to Cerebro's internal genes x cells
    layout via `DelayedArray::t()` (free). The in-memory `dgCMatrix` is
    never materialised on attach, so RAM stays close to the `.crb`
    metadata size. Requires the HDF5Array package.

- verbose:

  Set this to `TRUE` if you want additional log messages; defaults to
  `FALSE`.

## Value

No data returned.

## Immune Repertoire

If `object@misc$immune_repertoire` contains a named list of data.frames
(one per sample, with scRepertoire columns such as CTgene, CTnt, CTaa,
CTstrict), it will be automatically exported into the Cerebro object via
`addImmuneRepertoire()`. Legacy `bcr_data` / `tcr_data` slots are also
supported as a fallback.

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
#> [14:00:04] Initializing Cerebro object...
#> [14:00:04] Adding expression data (embedded)...
#> [14:00:04] Collecting available meta data...
#> [14:00:04] Extracting all meta data columns...
#> [14:00:04] Extracting dimensional reductions...
#> [14:00:04] Will export the following dimensional reductions: umap
#> [14:00:04] Extracting marker genes table...
#> [14:00:04] No trajectories to extract...
#> [14:00:04] Overview of Cerebro object:
#> class: Cerebro_v1.3
#> cerebroApp version: 1.7.3
#> experiment name: PBMC
#> organism: hg
#> date of analysis: 
#> date of export: 2026-06-27
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
#> Immune repertoire:
#> [14:00:04] Saving Cerebro object to: /tmp/nix-shell-4556-2374957467/Rtmphyp89s/pbmc_Seurat.crb
#> [14:00:04] Done!
```
