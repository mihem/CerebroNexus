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
    "extdata/v1.4/pbmc_seurat.rds",
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

crb <- readRDS(crb_output)
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
It reads the same descriptor and copies the referenced H5 sidecar into
the self-contained app bundle. The H5 file is stored under the app’s
private `private-data/` tree and is not registered as a
browser-downloadable HTTP resource.

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

## Legacy alternative: convert an existing embedded `.crb`

The direct export above is preferred for new Seurat exports. The manual
workflow remains useful when only an older embedded `.crb` is available,
for example after an
[`exportFromSCE()`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md)
workflow, and the source object cannot be exported again.

Load the existing `.crb`, transpose its conventional genes-by-cells
expression matrix, and write it to the `/expression` group:

``` r
crb_input <- system.file(
  "extdata/v1.4/example.crb",
  package = "CerebroNexus"
)
crb <- readRDS(crb_input)

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
lives.

``` r
crb$expression <- NULL
crb$setExpressionBackend(
  type = "h5",
  location = basename(h5_output)
)
crb_output <- file.path(legacy_dir, "example.crb")
saveRDS(crb, crb_output)
```

The converted `.crb` is now self-describing. Keep it beside `example.h5`
and launch it without a host-specific override:

``` r
launchCerebro(
  crb_file_to_load = crb_output
)
```

If an older serialized object does not provide `setExpressionBackend()`,
do not clear and resave it with no descriptor. Re-export it with a
current CerebroNexus version instead. When the original Seurat object is
available, `exportFromSeurat(..., expression_matrix_mode = "h5")`
remains the preferred, less error-prone route.

## See also

- [Create a self-contained Shiny
  app](https://mihem.github.io/CerebroNexus/articles/create_a_self_contained_shiny_app.md)
- [Seurat
  workflow](https://mihem.github.io/CerebroNexus/articles/cerebroApp_workflow_Seurat.md)
- [`?exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
