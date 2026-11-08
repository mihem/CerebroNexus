# Convert a SingleCellExperiment into a Seurat object

Gives a SingleCellExperiment (read from an `.rds`/`.qs2` file, or an
AnnData file opened with
[`readH5adAsSCE()`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md))
the shape the Seurat-based tools expect, so it can go through the
Builder like any other dataset. Cells, genes, cell metadata and every
reduced dimension are carried over; so are the results kept in
`metadata(object)` (marker genes, immune repertoire, trajectories and
category colours), which the export reads from `@misc`.

## Usage

``` r
convertSCEToSeurat(
  object,
  main_assay = "RNA",
  extra_features = NULL,
  verbose = FALSE
)
```

## Arguments

- object:

  A SingleCellExperiment.

- main_assay:

  Name of the Seurat assay to create.

- extra_features:

  Alternative experiments (`altExp`, for example antibody counts
  `"ADT"`) whose features are added to the assay as
  `<experiment>:<feature>`: their names, or `TRUE` for all.

- verbose:

  Print what was converted.

## Value

A Seurat object.

## Details

The expression matrix is placed by what it holds: a `counts` assay
becomes the `counts` layer and `logcounts` the `data` layer. Without
those, the first assay (an AnnData `X`) is used as `counts` when it
holds whole numbers and as `data` otherwise.

Spatial coordinates (`reducedDim(object, "spatial")`) become one Seurat
spatial image (a FOV of cell centroids) per tissue section, named after
the section, so the Builder's spatial pages treat them like any other
spatial data set. Sections are told apart by the `library_id` column.
Without one the coordinates are kept as a single section. When the
coordinates are the only embedding, they are also kept as the projection
`spatial_xy`, so there is one to open the data set with.

The tissue pictures in `metadata(object)$spatial_images` are kept per
section in `@misc$cerebro_source_images` (not in
`cerebro_spatial_images`, which the export would embed in the `.crb`).
The Builder takes them from there and attaches them to their sections as
separate image files, placed at the extent their scale factor gives.
