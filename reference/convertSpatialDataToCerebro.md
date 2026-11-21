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
#> [14:27:58] Start collecting data...
#> [14:27:58] Adding expression data (embedded)...
#> [14:27:58] Overview of Cerebro object:
#> class: Cerebro
#> exporter package version: 8.4.5
#> experiment name: small_spatialdata
#> organism: Human
#> date of analysis: 
#> date of export: 2026-11-21
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
#> [14:27:58] Saving Cerebro object to: /tmp/nix-shell-4250-376363681/Rtmp9qtN2e/cerebro-10d82bc25423/cerebro_small_spatialdata.crb
#> [14:27:58] Done!
#> Successfully exported: cerebro_small_spatialdata.crb
list.files(dir)
#> [1] "cerebro_small_spatialdata.crb"
unlink(dir, recursive = TRUE)
# }
```
