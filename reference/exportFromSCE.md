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
  expression_matrix_mode = c("embedded", "h5"),
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  verbose = FALSE,
  main_group = NULL,
  projections = NULL,
  spatial_reductions = NULL,
  spatial_split_by = NULL
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
  format, such as `dgCMatrix` from the `Matrix` package. Ignored when
  `expression_matrix_mode` is set to an external backend.

- expression_matrix_mode:

  How to persist the expression matrix. One of `"embedded"` (default) or
  `"h5"`; identical to
  [`exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md).
  The `"h5"` backend writes a sibling `<stem>.h5` file next to the
  `.crb`, and `file` must then end in `.crb`.

- codec:

  Serialization codec for the CRB payload. Defaults to `"qs2"`; use
  `"rds"` when direct compatibility with
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html) is required.

- viewer_binary:

  Build the derived Viewer Pack `"auto"`matically for large datasets,
  `"always"`, or `"never"`.

- viewer_binary_threshold:

  Dataset-level cell threshold used by `viewer_binary = "auto"`.

- verbose:

  Set this to `TRUE` if you want additional log messages; defaults to
  `FALSE`.

- main_group:

  Optional entry of `groups` to use as the main group.

- projections:

  Optional character vector naming the `reducedDims(object)` to export.
  By default every non-PCA reduction is exported (PCA only when it is
  the sole reduction).

- spatial_reductions:

  Optional character vector naming the `reducedDims(object)` that hold
  spatial point coordinates (cells x \>= 2). By default a reduction
  named `spatial` (any case) is used, which is how AnnData
  `obsm["spatial"]` arrives; they fill the Spatial tab.

- spatial_split_by:

  Optional `colData` column. When given, the spatial coordinates are
  divided by its values into one section per level (e.g. the tissue
  sections of a MERFISH or multi-slice Visium data set that share one
  coordinate frame), selectable in the Spatial tab.

## Value

No data returned.

## Details

The export is written by the same format-independent core as
[`exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md):
the CRB and any sidecar are built in a private stage and published
together, and `object@metadata` entries (trees, markers, pathways,
immune repertoire, HLA typing, extra material, ...) are read with the
same keys as `object@misc` on the Seurat side. Two entries are specific
to this route. `metadata(object)$group_colors`, a list named by grouping
variable of named colour vectors (level to `"#RRGGBB"`), becomes the
groups' default colours in the Viewer.
`metadata(object)$spatial_images`, a list named by library of images
(each
`list(histology_image = <base64 data URI>, histology_image_bounds = c(xmin, xmax, ymin, ymax))`),
is drawn behind the spatial coordinates: the single library's images go
to the single spatial entry, and with `spatial_split_by` each section
takes the images of the library named like its level. Both are filled in
by
[`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md)
from scanpy's `uns`.

## Examples

``` r
pbmc <- readRDS(system.file("extdata/examples/pbmc_SCE.rds",
  package = "CerebroNexus"))
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
#> [15:22:21] Initializing Cerebro object...
#> [15:22:21] Adding expression data (embedded)...
#> [15:22:21] Collecting available meta data...
#> [15:22:21] Extracting all meta data columns...
#> [15:22:21] Extracting dimensional reductions...
#> [15:22:21] Will export the following dimensional reductions: UMAP
#> [15:22:21] No trajectories to extract...
#> [15:22:21] Overview of Cerebro object:
#> class: Cerebro
#> exporter package version: 5.6.0
#> experiment name: PBMC
#> organism: hg
#> date of analysis: 
#> date of export: 2026-10-31
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
#> HLA typing: none
#> Spatial data:
#> [15:22:21] Saving Cerebro object to: /tmp/nix-shell-4249-2839082002/RtmppwJnih/pbmc_SCE.crb
#> [15:22:21] Done!
```
