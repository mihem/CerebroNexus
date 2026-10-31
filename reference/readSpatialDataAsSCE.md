# Read a SpatialData store as a SingleCellExperiment

Reads the cell-by-gene table of a SpatialData `.zarr` store natively (no
Python), with the spatial coordinates of its cells and the tissue
pictures that sit in each section's coordinate system, so the result
goes to
[`convertSCEToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md)
like any other SingleCellExperiment.

## Usage

``` r
readSpatialDataAsSCE(
  path,
  table = NULL,
  spatial_images = "auto",
  verbose = FALSE
)
```

## Arguments

- path:

  A SpatialData `.zarr` directory, or a `.zip` of one (as the
  SpatialData sample datasets are distributed).

- table:

  Name of the table under `tables/`; by default `"table"` or the only
  one.

- spatial_images:

  `"auto"` (hires where there is one, otherwise lowres), a vector of
  image labels, or `FALSE` for none.

- verbose:

  Print what was read.

## Value

A SingleCellExperiment.

## Details

The table is an AnnData and is read as described in
[`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md).
Coordinates come from `obsm["spatial"]` when the table has them
(Visium); otherwise from the shapes element the table annotates
(MERFISH, Xenium and other cell-based technologies: the centre of each
shape, matched by the table's instance key). Coordinates are in the
units of the shapes element. The section of a cell is the table's region
column, copied to `colData$library_id`.

Pictures with three channels are attached to the section whose
coordinate system they are placed in, as `metadata(sce)$spatial_images`,
with their extent worked out from the transformations. Points, labels,
other shapes and single-channel images are not read.

Zarr v2 and v3 stores are read (including sharded arrays) with zstd,
gzip, Blosc (lz4, zstd or zlib) or no compression; any other codec stops
with its name. Blosc chunks are decoded in R, lz4 (the numcodecs
default) at about 4 MB/s of output: re-save a large store with zstd, or
use an `.h5ad` file.

## See also

[`convertSpatialDataToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSpatialDataToCerebro.md),
[`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md)
