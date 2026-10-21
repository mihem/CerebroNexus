# Store the expression matrix in H5 format

## Overview

CerebroNexus can store a data set’s expression matrix in a sibling H5
file instead of embedding it in the `.crb`. The app then reads
individual genes from disk without loading the complete matrix into
memory. This usually reduces startup time and memory use for large data
sets, although disk access can be slower than reading an already loaded
in-memory matrix.

The recommended workflow is a single call to
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
with `expression_matrix_mode = "h5"`. The Seurat object does **not**
need to contain an H5-backed matrix: `h5` describes the export format,
not the source storage used by Seurat.

The exporter writes two files:

``` text
pbmc.crb
pbmc.h5
```

The `.crb` records the H5 filename in its expression-backend descriptor.
Keep both files together when moving, sharing, or deploying the data
set.

`embedded` and `h5` are the only expression storage modes
(`expression_matrix_mode = c("embedded", "h5")`). A `.crb` whose storage
this version cannot read has to be re-exported from its source Seurat
object (see [Older files](#older-files-and-retired-formats) below).

## Setup

Install CerebroNexus and the Bioconductor package `HDF5Array`. The
latter provides the TENx HDF5 writer and the lazy matrix reader used at
runtime.

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("HDF5Array")

library(CerebroNexus)
```

## Recommended: export directly from Seurat

Choose the assay and layer (called `slot` for compatibility with older
Seurat versions) exactly as you would for an embedded export. Adding
`expression_matrix_mode = "h5"` makes CerebroNexus write the selected
matrix to a sibling H5 file and store only its relative location in the
`.crb`.

``` r
seurat <- readRDS(
  system.file(
    "extdata/examples/pbmc_seurat.rds",
    package = "CerebroNexus"
  )
)

output_dir <- file.path(tempdir(), "cerebro-h5-example")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
crb_output <- file.path(output_dir, "pbmc.crb")

exportFromSeurat(
  object = seurat,
  assay = "RNA",
  slot = "data",
  file = crb_output,
  experiment_name = "PBMC",
  organism = "hg",
  groups = c("sample", "seurat_clusters"),
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA",
  expression_matrix_mode = "h5",
  verbose = TRUE
)
```

The H5 filename uses the same stem as the `.crb`:

``` r
h5_output <- file.path(output_dir, "pbmc.h5")
file.exists(crb_output)
file.exists(h5_output)

crb <- readCerebro(crb_output)
crb$getExpressionBackend()
#> $type
#> [1] "h5"
#>
#> $location
#> [1] "pbmc.h5"
```

[`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html)
stores the on-disk matrix as cells by genes in the `/expression` group.
CerebroNexus reconnects it lazily and exposes the usual genes-by-cells
matrix to the application.

### A thin `.crb`

The cell names are kept once, in the H5 file: `/expression/genes` holds
the **cell** names and `/expression/barcodes` the **gene** names (the
axis names follow the 10x convention for a genes-by-cells matrix, which
is transposed here). The `.crb` itself is *thin* (schema version 3). It
drops `meta_data$cell_barcode` and the row names of the projections, and
instead stores a checksum of the cell names (`cell_names_hash`) together
with the cell count.
[`readCerebro()`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md)
and the Viewer read the cell names back from the H5 file, verify the
checksum and restore both fields, so the object you work with is
identical to an embedded one:

``` r
head(crb$getMetaData()$cell_barcode)
rownames(crb$projections[[1]])[1:3]
```

If the `.crb` and the H5 file do not belong together, for example
because the H5 file was re-exported with a different cell order, loading
stops with an error instead of attaching expression values to the wrong
cells.

## Launch or bundle the result

For normal interactive use, keep the `.crb` and `.h5` together and
launch the `.crb`. Its descriptor tells CerebroNexus where to find the
matrix, so no separate H5 argument is needed.

``` r
launchCerebro(
  crb_file_to_load = crb_output
)
```

For deployment, pass the `.crb` to
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md).
It reads the same descriptor and copies the referenced H5 sidecar, and
the immune repertoire sidecar if there is one, into the self-contained
app bundle. The files are stored under the app’s private `private-data/`
tree and are not registered as browser-downloadable HTTP resources.
There is no option to point the app at a different matrix path on the
host: the `.crb` and its recorded sidecars are the only source.

``` r
createShinyApp(
  cerebro_data = c("PBMC" = crb_output),
  result_dir = file.path(output_dir, "pbmc_app"),
  launch_browser = FALSE
)
```

If the `.crb` is renamed, do not guess a new H5 name. The descriptor
remains authoritative. Use `crb$getExpressionBackend()` to see the
sidecar that belongs to the file.

### Immune repertoire sidecar

A data set with a very large immune repertoire gets a third file,
`<stem>.immune.qs2`, next to the `.crb` and the `.h5` file. By default
(`immune_sidecar = "auto"` in
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
and
[`saveCerebro()`](https://mihem.github.io/CerebroNexus/reference/saveCerebro.md))
this happens for H5-backed `.crb` files whose repertoire has at least
`immune_sidecar_threshold = 100000L` rows; `"always"` and `"never"`
override that decision. Move and bundle the sidecar together with the
`.crb` like the H5 file.

## Convert an existing embedded `.crb`

The direct export above is preferred for new Seurat exports. The manual
workflow remains useful when only a current, embedded `.crb` is
available, for example after an
[`exportFromSCE()`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md)
workflow, and the source object cannot be exported again.

Load the existing `.crb`, transpose its conventional genes-by-cells
expression matrix, and write it to the `/expression` group:

``` r
crb_input <- system.file(
  "extdata/examples/example.crb",
  package = "CerebroNexus"
)
crb <- readCerebro(crb_input)

legacy_dir <- file.path(tempdir(), "cerebro-legacy-h5")
dir.create(legacy_dir, recursive = TRUE, showWarnings = FALSE)
h5_output <- file.path(legacy_dir, "example.h5")

HDF5Array::writeTENxMatrix(
  Matrix::t(crb$expression),
  h5_output,
  group = "expression"
)
```

Remove the embedded matrix and record the sibling filename before saving
the smaller `.crb`; otherwise the original matrix will still be loaded
into memory, or the saved object will not know where its external matrix
lives. Because the backend is H5,
[`saveCerebro()`](https://mihem.github.io/CerebroNexus/reference/saveCerebro.md)
writes a thin `.crb`: it checks that the H5 cell names match
`meta_data$cell_barcode` in the same order and then keeps only their
checksum.

``` r
crb$expression <- NULL
crb$setExpressionBackend(
  type = "h5",
  location = basename(h5_output)
)
crb_output <- file.path(legacy_dir, "example.crb")
saveCerebro(crb, crb_output)
```

The converted `.crb` is now self-describing. Keep it beside `example.h5`
and launch it without any further argument:

``` r
launchCerebro(
  crb_file_to_load = crb_output
)
```

When the original Seurat object is available,
`exportFromSeurat(..., expression_matrix_mode = "h5")` remains the
preferred, less error-prone route.

## Older files and retired formats

Only the current layout is read.
[`readCerebro()`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md),
[`launchCerebro()`](https://mihem.github.io/CerebroNexus/reference/launchCerebro.md),
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
and the Viewer reject a `.crb` that has no expression-backend
descriptor, uses a thin schema other than version 3, still stores
receptors in the `bcr_data` / `tcr_data` fields, or records an immune
repertoire sidecar of another type than `"file"`, with

``` text
The Cerebro data file 'x.crb' uses a retired format: <reason>.
Convert it once with CerebroNexus::convertCerebro().
```

[`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md)
is the only migration path. It sets a missing descriptor to `embedded`,
merges the old receptor fields into the unified immune repertoire, and
rewrites an H5-backed `.crb` from CerebroNexus 5.1.x (which carried its
cell names in the `.crb`) as a thin one next to its existing H5 file:

``` r
convertCerebro("old_dataset.crb")
```

A `.crb` whose expression storage this version cannot read or repair (an
expression backend other than `embedded` or `h5`, thin schema 1 or 2, or
an immune sidecar of another type than `"file"`) is rejected and cannot
be converted. Re-export it from the source Seurat object with
`exportFromSeurat(..., expression_matrix_mode = "h5")`.

## See also

- [Create a self-contained Shiny
  app](https://mihem.github.io/CerebroNexus/articles/create_a_self_contained_shiny_app.md)
- [Seurat
  workflow](https://mihem.github.io/CerebroNexus/articles/cerebronexus_workflow_seurat.md)
- [`?exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
