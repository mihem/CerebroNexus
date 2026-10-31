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
  expression_matrix_mode = c("embedded", "h5"),
  h5_compression_level = 0L,
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  spatial_images = NULL,
  verbose = FALSE,
  immune_sidecar = c("auto", "always", "never"),
  immune_sidecar_threshold = 100000L,
  .expression_resolution = NULL
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

  Where to save the output. External backends require a `.crb` filename
  and store the matrix under a sibling name derived from the stem.

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

  How to persist the expression matrix. One of `"embedded"` (default) or
  `"h5"`.

  - `"embedded"` stores the matrix inside the `.crb` file.

  - `"h5"` writes the matrix via
    [`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html)
    to a TENx-format sparse HDF5 file next to the `.crb` (sibling
    `<stem>.h5`) and tags the backend with that file name. The file
    holds a single `/expression` group with `data`, `indices`, `indptr`,
    `shape`, `genes`, and `barcodes` datasets. The matrix is stored
    cells x genes (TENx column-favoured, optimised for per-gene reads);
    the Viewer reads it back as a lazy
    [`HDF5Array::TENxMatrix`](https://rdrr.io/pkg/HDF5Array/man/TENxMatrix-class.html)
    seed and transposes it lazily to Cerebro's internal genes x cells
    layout via `DelayedArray::t()` (free). The in-memory `dgCMatrix` is
    never materialised on attach, so RAM stays close to the `.crb`
    metadata size. The `.crb` is thin: the cell names are kept once, in
    the H5 file, and the `.crb` records their order and a checksum. The
    gzip level is set by `h5_compression_level`; the default (no
    compression) is several times larger than a compressed file but
    builds and reads faster. Requires the HDF5Array package.

  The CRB and any sidecar are built and validated in a private sibling
  stage. On POSIX systems, new stages and external matrices are
  owner-only; replacing a CRB preserves its existing mode. An existing
  sidecar is replaced only when the current CRB identifies that exact
  path as its backend; backend changes remove the previous owned sidecar
  after the new CRB is committed. Stop all readers before replacing an
  existing export, because a reader can otherwise observe the two-path
  replacement between steps. Ordinary R errors trigger best-effort
  restoration. Process termination and concurrent writers remain outside
  this multi-path transaction guarantee. Only POSIX mode bits are set or
  preserved; ownership, ACLs, extended attributes, and security labels
  remain the deployment system's responsibility on every platform.

- h5_compression_level:

  Gzip compression level, from 0 to 9, of the H5 file written when
  `expression_matrix_mode = "h5"`; ignored by the other modes. The
  default 0 writes the data without compression: the file is about 7
  times larger than at HDF5Array's default level 6 (about 69 GiB instead
  of 10 GiB for 1.5 million human prefrontal cortex cells), but it
  builds about three times faster and single-gene reads are about a
  third faster. Set 6 when disk space matters more than speed.

- codec:

  Serialization codec for the CRB payload. Defaults to `"qs2"`; use
  `"rds"` when direct compatibility with
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html) is required.

- viewer_binary:

  Build the derived Viewer Pack `"auto"`matically for large datasets,
  `"always"`, or `"never"`.

- viewer_binary_threshold:

  Dataset-level cell threshold used by `viewer_binary = "auto"`.

- immune_sidecar:

  Store the immune repertoire in a sibling file `<stem>.immune.qs2`
  (`.rds` with `codec = "rds"`) instead of inside the `.crb`. `"auto"`
  does so for `"h5"` exports whose repertoire has at least
  `immune_sidecar_threshold` rows; `"always"` and `"never"` override
  that. The sidecar must travel with the `.crb`.

- immune_sidecar_threshold:

  Number of repertoire rows (cells with receptor data) used by
  `immune_sidecar = "auto"`.

- spatial_images:

  Optional named list mapping Seurat image names to named image paths or
  descriptors of the form `list(path = ..., bounds = ...)`. Supported
  file extensions are png, jpg, jpeg, and svg. Missing bounds are
  derived from the exported x/y coordinate range. An object whose
  `@images` hold a Seurat v4 `VisiumV1` image is rejected; rebuild it as
  `VisiumV2` with
  [`Seurat::Load10X_Spatial()`](https://satijalab.org/seurat/reference/Load10X_Spatial.html).

- verbose:

  Set this to `TRUE` if you want additional log messages; defaults to
  `FALSE`.

- .expression_resolution:

  Internal handoff used by
  [`convertSeuratToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md)
  to reuse a matrix that has already been resolved and validated. Users
  should leave this as `NULL`.

## Value

No data returned.

## Immune Repertoire

If `object@misc$immune_repertoire` contains a named list of data.frames
(one per sample, with `barcode`, `CTgene`, `CTnt`, `CTaa`, and
`CTstrict`), it will be automatically exported into the Cerebro object
via
[`addImmuneRepertoire()`](https://mihem.github.io/CerebroNexus/reference/addImmuneRepertoire.md).
When that slot is empty, receptors kept apart in `object@misc$tcr_data`
and `object@misc$bcr_data` are merged into it.

## HLA typing

If `object@misc$hla_typing` holds an HLA genotype table – a canonical
long `data.frame`, a wide `sample` + `HLA-*_1/_2` `data.frame`, or a
named list (sample -\> allele vector) – it is exported via
`addHLATyping()`, parallel to the immune repertoire. The provenance in
`object@misc$hla_typing_source_type` (one of `"genotyped"`, `"imputed"`,
`"synthetic"`, `"unknown"`; default `"unknown"`) is carried through, so
a predicted or fabricated genotype is never mistaken for a directly
typed one.

## Examples

``` r
pbmc <- readRDS(system.file("extdata/examples/pbmc_seurat.rds",
  package = "CerebroNexus"))
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
#> [13:38:43] Initializing Cerebro object...
#> [13:38:43] Adding expression data (embedded)...
#> [13:38:43] Collecting available meta data...
#> [13:38:43] Extracting all meta data columns...
#> [13:38:43] Extracting dimensional reductions...
#> [13:38:43] Will export the following dimensional reductions: umap
#> [13:38:43] Extracting marker genes table...
#> [13:38:43] No trajectories to extract...
#> [13:38:43] Checking for spatial data...
#> [13:38:43] Overview of Cerebro object:
#> class: Cerebro
#> exporter package version: 5.6.0
#> experiment name: PBMC
#> organism: hg
#> date of analysis: 
#> date of export: 2026-10-31
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
#> HLA typing: none
#> Spatial data:
#> [13:38:43] Saving Cerebro object to: /tmp/nix-shell-4222-882645369/RtmpQ8lNYp/pbmc_Seurat.crb
#> [13:38:43] Done!
```
