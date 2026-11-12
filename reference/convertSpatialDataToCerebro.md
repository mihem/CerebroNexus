# Convert a SpatialData store to a Cerebro object

Reads the store with
[`readSpatialDataAsSCE`](https://www.mheming.com/CerebroNexus/reference/readSpatialDataAsSCE.md)
and exports it with
[`convertSCEToCerebro`](https://www.mheming.com/CerebroNexus/reference/convertSCEToCerebro.md):
one spatial section per region (with its tissue picture, where the store
has one), expression, cell data and any embeddings, markers and colours
the table carries.

## Usage

``` r
convertSpatialDataToCerebro(
  path,
  result_dir,
  table = NULL,
  spatial_images = "auto",
  assay = "X",
  ...
)
```

## Arguments

- path:

  A SpatialData `.zarr` directory.

- result_dir:

  Directory the `.crb` is written to.

- table, spatial_images:

  See
  [`readSpatialDataAsSCE`](https://www.mheming.com/CerebroNexus/reference/readSpatialDataAsSCE.md).

- assay:

  Which matrix to export; default `"X"`.

- ...:

  Further arguments of
  [`convertSCEToCerebro`](https://www.mheming.com/CerebroNexus/reference/convertSCEToCerebro.md)
  (for example `groups`, `organism`, `expression_matrix_mode`).

## Value

The path of the `.crb`, invisibly.

## See also

[`readSpatialDataAsSCE`](https://www.mheming.com/CerebroNexus/reference/readSpatialDataAsSCE.md)

## Examples

``` r
# \donttest{
dir <- tempfile("cerebro-")
dir.create(dir)
convertSpatialDataToCerebro(
  system.file("extdata/examples/small_spatialdata.zip", package = "CerebroNexus"),
  result_dir = dir,
  groups = "cell_type",
  verbose = FALSE
)
#> [20:51:43] Start collecting data...
#> [20:51:43] Adding expression data (embedded)...
#> [20:51:43] Overview of Cerebro object:
#> class: Cerebro
#> exporter package version: 8.0.1
#> experiment name: small_spatialdata
#> organism: Human
#> date of analysis: 
#> date of export: 2026-11-12
#> number of cells: 50
#> number of genes: 30
#> grouping variables (1): cell_type
#> cell cycle variables (0): 
#> projections (2): UMAP, spatial
#> trees (0): 
#> most expressed genes: cell_type
#> marker genes:
#> enriched pathways:
#> trajectories:
#> extra material:
#> Immune repertoire:
#> HLA typing: none
#> Spatial data:spatial
#> [20:51:43] Saving Cerebro object to: /tmp/nix-shell-4510-4209629192/RtmpnPz3zU/cerebro-11dc40a9159a/cerebro_small_spatialdata.crb
#> [20:51:43] Done!
#> Successfully exported: cerebro_small_spatialdata.crb
list.files(dir)
#> [1] "cerebro_small_spatialdata.crb"
unlink(dir, recursive = TRUE)
# }
```
