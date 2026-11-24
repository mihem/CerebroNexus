# Working with large datasets

## Choose an expression backend

CerebroNexus stores the expression matrix either inside the `.crb` or in
a sparse HDF5 file next to it. Select the storage with
`expression_matrix_mode` in
[`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
or
[`convertSeuratToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSeuratToCerebro.md):

| Mode | Practical use | Files to keep together |
|----|----|----|
| `embedded` | Small datasets and simple single-file distribution | `.crb` |
| `h5` | On-disk sparse expression, including single-gene queries and large datasets | `.crb` and its recorded `.h5` sidecar |

With `h5`, the Viewer reads individual genes from disk without loading
the complete matrix into memory. This usually reduces startup time and
memory use for large datasets, although a disk read can be slower than
reading a matrix that is already in memory. `h5` describes the export
format, not how Seurat stores the matrix: the Seurat object does **not**
need an H5-backed matrix.

Use the [benchmark
guide](https://www.mheming.com/CerebroNexus/articles/expression_backend_benchmark.md)
for measured workloads and their limits. Avoid converting a large
delayed or on-disk matrix into a dense matrix merely to inspect or
export it.

## Store the expression matrix in an H5 file

H5 export needs the Bioconductor package `HDF5Array`, which provides the
TENx HDF5 writer and the lazy matrix reader used at runtime:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("HDF5Array")
```

### Export from Seurat

Choose the assay and layer (called `slot` for compatibility with older
Seurat versions) exactly as you would for an embedded export. Adding
`expression_matrix_mode = "h5"` makes CerebroNexus write the selected
matrix to a sibling H5 file and store only its relative location in the
`.crb`:

``` r
library(CerebroNexus)

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

The exporter writes two files with the same stem, `pbmc.crb` and
`pbmc.h5`. The `.crb` records the H5 filename in its expression-backend
descriptor:

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

The on-disk matrix is stored as cells by genes in the `/expression`
group, in the layout of
[`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html).
CerebroNexus reconnects it lazily and exposes the usual genes-by-cells
matrix to the application.

### A thin `.crb`

The cell names are kept once, in the H5 file: `/expression/genes` holds
the **cell** names and `/expression/barcodes` the **gene** names (the
axis names follow the 10x convention for a genes-by-cells matrix, which
is transposed here). The `.crb` itself is *thin* (schema version 5). It
drops `meta_data$cell_barcode` and the row names of the projections, and
instead stores a checksum of the cell names (`cell_names_hash`) together
with the cell count.
[`readCerebro()`](https://www.mheming.com/CerebroNexus/reference/readCerebro.md)
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
cells (see [Data integrity
contracts](https://www.mheming.com/CerebroNexus/articles/data_integrity_contracts.md)).

Schema 5 uses a versioned XXH3-128 checksum (seed 0) of the ordered
UTF-8 names, each terminated by a zero byte, independently of `rlang`
and R-object serialization. Raw-byte encoding and hashing run in native
code without temporary files. Existing schema-3/4 files require a
one-time `convertCerebro("dataset.crb")` with the updated package; stop
apps using the export first. Conversion validates the old checksum and
reuses the H5 matrix without re-exporting expression. Rebuild its Viewer
Pack if conversion did not already rebuild it. This applies to any thin
H5 CRB, not just large datasets; embedded-expression CRBs are
unaffected. Normal ordered-name validation still runs before expression
access in each new Viewer process, without legacy-hash retries. Opening
a schema-5 file requires an updated reader, including updated standalone
apps.

### Launch or bundle an H5-backed dataset

Keep the `.crb` and `.h5` together and launch the `.crb`. Its descriptor
tells CerebroNexus where to find the matrix, so no separate H5 argument
is needed:

``` r
launchCerebro(
  crb_file_to_load = crb_output
)
```

For deployment, pass the `.crb` to
[`createShinyApp()`](https://www.mheming.com/CerebroNexus/reference/createShinyApp.md).
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

A dataset with a very large immune repertoire gets a third file,
`<stem>.immune.qs2`, next to the `.crb` and the `.h5` file. By default
(`immune_sidecar = "auto"` in
[`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
and
[`saveCerebro()`](https://www.mheming.com/CerebroNexus/reference/saveCerebro.md))
this happens for H5-backed `.crb` files whose repertoire has at least
`immune_sidecar_threshold = 100000L` rows; `"always"` and `"never"`
override that decision. Move and bundle the sidecar together with the
`.crb` like the H5 file.

### Convert an existing embedded `.crb`

The direct export above is preferred for new Seurat exports. The manual
workflow remains useful when only a current, embedded `.crb` is
available, for example after an
[`exportFromSCE()`](https://www.mheming.com/CerebroNexus/reference/exportFromSCE.md)
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
  group = "expression",
  level = 1L
)
```

Remove the embedded matrix and record the sibling filename before saving
the smaller `.crb`; otherwise the original matrix will still be loaded
into memory, or the saved object will not know where its external matrix
lives. Because the backend is H5,
[`saveCerebro()`](https://www.mheming.com/CerebroNexus/reference/saveCerebro.md)
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

## Read, save and convert

This small example creates a portable CRB without downloading a dataset:

``` r
library(CerebroNexus)
cells <- c("cell-a", "cell-b", "cell-c")
object <- Cerebro$new()
object$setExpression(matrix(c(1, 0, 2, 3, 0, 4), nrow = 2,
  dimnames = list(c("gene-a", "gene-b"), cells)))
object$setMetaData(data.frame(cell_barcode = cells, sample = c("A", "A", "B")))
object$addProjection("umap", data.frame(x = 1:3, y = c(0, 1, 0), row.names = cells))
work <- tempfile("cerebro-guide-")
dir.create(work)
file <- file.path(work, "example.crb")
saveCerebro(object, file)
restored <- readCerebro(file)
stopifnot(identical(restored$getMetaData()$cell_barcode, cells))
```

[`readCerebro()`](https://www.mheming.com/CerebroNexus/reference/readCerebro.md)
reads RDS and qs2 CRBs; `.crb` describes the logical format, not its
physical codec. `saveCerebro(..., codec = "rds")` requests RDS
compatibility, while the default uses qs2.
[`convertCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertCerebro.md)
re-serializes a CRB while reusing its existing expression data, so input
and output must be in the same directory:

``` r
convertCerebro("study.crb", "study-rds.crb", codec = "rds")
```

[`convertCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertCerebro.md)
is also the one-time migration path for a CRB written by an older
release; see [Older files and retired
formats](#older-files-and-retired-formats).

## Move the complete dataset

Keep paths recorded inside the CRB valid. A typical layout is:

``` text
study.crb
study.h5
study.immune.qs2   (only when the repertoire is large)
study.viewer/
```

Additional spatial molecule/image resources must also travel with their
dataset. Do not rename or partially copy sidecars independently. RDS and
qs2 versions of a CRB can share the same unchanged H5 file. The
cell-order checksum stored in the `.crb` rejects an H5 file whose cells
differ from the ones the `.crb` was written for.

For distribution,
[`createShinyApp()`](https://www.mheming.com/CerebroNexus/reference/createShinyApp.md)
builds a self-contained Viewer directory and copies the required
resources. It resolves colliding CRB basenames and places each Viewer
Pack beside the final CRB name. Follow the [app creation
guide](https://www.mheming.com/CerebroNexus/articles/create_a_self_contained_shiny_app.md).

## Viewer Packs and fallback

Viewer Packs are optional derived assets for projection geometry,
metadata and other prepared views. The CRB remains the source of truth.
Build and validate a pack explicitly:

``` r
pack <- buildViewerPack(file, viewer_binary = "always")
stopifnot(validateViewerPack(file, pack)$valid)
```

Projection rows are aligned by barcode to canonical metadata order.
Missing, duplicate or unmatched identities are rejected, even when row
counts agree. Keep actual barcode row names on manually supplied
projections.

After changing the CRB, rebuild with
`buildViewerPack(file, viewer_binary = "always", overwrite = TRUE)`. A
missing, incompatible or stale pack, such as one written for another
pack schema, is invalid as a whole and falls back to the ordinary data
path; rebuilding it restores the fast paths.

A pack built before the Viewer stored the immune repertoire as integer
columns (`immune/encoded.qs2`) is still valid, but its Immune repertoire
page encodes the repertoire in memory at the first visit and says so:
about 21 s on the 1.46 M-cell Ren data set, about 8 s on later starts
(the encoding is kept in the user cache folder; `CEREBRO_IR_CACHE`
changes it), against about 1 s with a current pack. Rebuild such a pack
once with `buildViewerPack("path/to/data.crb", overwrite = TRUE)`. Do
not suppress checksum failures or edit a manifest to make a pack appear
valid.

## Understand large-group plots

Violin density uses at most 4,096 mid-quantile values per group on the
original measurement scale. It is an approximation and can omit rare
tails. Box quartiles, median, whiskers and mean are computed separately
from all finite observations; hover also reports sample count, minimum
and maximum. Missing/non-finite values are excluded. Approximate density
is labelled in the summary hover. Display compaction never rescales
observations to force an equal mean.

## Launch the demos

The installed package includes small examples:

``` r
launchCerebro(crb_file_to_load = system.file(
  "extdata", "examples", "example.crb", package = "CerebroNexus"))
```

### The complete large-data demo

The optional complete large-data launcher requires a repository
checkout. Use a checkout of a published release tag for reproducible
results, and record the printed commit ID when reporting them. The
launcher loads that checkout directly.

``` bash
git clone https://github.com/mihem/CerebroNexus.git
cd CerebroNexus
git rev-parse HEAD
Rscript -e "install.packages(c('remotes', 'pkgload')); remotes::install_local('.', dependencies = TRUE)"
Rscript run-demo.R
```

These demos keep their expression matrices in H5 sidecars and need no
extra package beyond the ones CerebroNexus installs.

Open `http://127.0.0.1:7451`. The first run downloads and prepares the
official 10x one-million-neuron matrix and the Ren et al. [COVID-19
immune
atlas](https://explore.data.humancellatlas.org/projects/5f607e50-ba22-4598-b1e9-f3d9d7a35dcc)
([GEO
GSE158055](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE158055);
1,462,702 cells, all 64 published cell populations, sample/patient
clinical metadata, and 220,968 paired-TCR plus 282,464 paired-BCR
cells). The initial compressed downloads total about 18 GB. The prepared
H5 files are written uncompressed and take considerable disk space on
top of the extracted matrices, so allow a generous amount of free cache
space. Source and prepared files are cached outside the repository (set
`CEREBRO_LARGE_CACHE` to choose the location), so later runs reuse them.

To test Ren without the 10x dataset, and to separate
download/preparation failures from Viewer errors (`--prepare-only` exits
before starting Shiny):

``` bash
Rscript run-demo.R --data=ren --prepare-only
Rscript run-demo.R --data=ren
```

The launcher prints the URL, the source directory and the Ren data
version (currently 2), and reuses a complete compatible cache. Keep each
`.crb` and its `.h5` file together. Prepared versions use separate
directories (`ren/cerebro-v2/`); preparation never upgrades the earlier
`ren/cerebro/` atlas in place, failed builds are not published, and
source downloads are shared between versions.

The launcher also builds each data set’s Viewer Pack and rebuilds it
when it is missing or predates an asset the viewer reads (the HLA first
frames, the immune repertoire’s integer encoding). Without a pack, the
HLA & TCR motifs page spends about 15 s (about 25 s for the whole first
entry) building the CDR3 Hamming-1 graph at Ren’s scale; with one, the
same entry settles in under two seconds. If you start the viewer against
a prepared `.crb` yourself, build the pack once (about a minute for Ren)
with `buildViewerPack("path/to/data.crb", overwrite = TRUE)`; rerunning
it after re-preparing a CRB is also the repair.

The 10x marker-guided E18 neurogenesis path excludes endothelial,
microglial and oligodendrocyte programs, orders the remaining neural
lineage with early, transitional and late markers, and separates
excitatory and inhibitory terminal branches. It is a labelled
demonstration, not a formal trajectory-inference result. The same app
also includes the bundled PBMC, immune-repertoire, trajectory, spatial,
Trekker and HLA/TCR demos. See the [benchmark
guide](https://www.mheming.com/CerebroNexus/articles/expression_backend_benchmark.md)
before starting.

## Older files and retired formats

Only the current layout is read.
[`readCerebro()`](https://www.mheming.com/CerebroNexus/reference/readCerebro.md),
[`launchCerebro()`](https://www.mheming.com/CerebroNexus/reference/launchCerebro.md),
[`createShinyApp()`](https://www.mheming.com/CerebroNexus/reference/createShinyApp.md)
and the Viewer reject an older `.crb`, for example one that has no
expression-backend descriptor, uses a thin schema other than version 5,
or still stores receptors in the `bcr_data` / `tcr_data` fields, with

``` text
The Cerebro data file 'x.crb' uses a retired format: <reason>.
Convert it once with CerebroNexus::convertCerebro().
```

[`convertCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertCerebro.md)
is the only migration path. It sets a missing descriptor to `embedded`,
merges the old receptor fields into the unified immune repertoire,
updates the ordered-name checksum of schema-3/4 files to portable schema
5, and rewrites an H5-backed `.crb` from CerebroNexus 5.1.x (which
carried its cell names in the `.crb`) as a thin one next to its existing
H5 file:

``` r
convertCerebro("old_dataset.crb")
```

A `.crb` whose expression storage this version cannot read or repair (an
expression backend other than `embedded` or `h5`, thin schema 1 or 2, or
an immune sidecar of another type than `"file"`) is rejected and cannot
be converted. Re-export it from the source Seurat object with
`exportFromSeurat(..., expression_matrix_mode = "h5")`.

## Troubleshooting

- **Missing sidecar:** restore the recorded sibling `.h5` (or
  `.immune.qs2`) file. A thin CRB alone is not a complete external
  dataset.
- **Retired format:** convert the file once with
  [`convertCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertCerebro.md),
  or re-export a dataset whose storage this version cannot read from its
  source Seurat object.
- **Identity/checksum mismatch:** restore the matching sidecar or
  re-export from a consistent source. Equal cell counts do not prove
  correct cell correspondence.
- **Pack rejected or slower fallback:** rebuild the optional pack from
  the current CRB; preserve the CRB and expression data.
- **Memory pressure:** inspect matrix density and format; avoid dense
  realization. A disk backend does not make every upstream Seurat
  operation out-of-core.
- **Slow first operation:** distinguish package loading, backend reads
  and browser painting. A warm revisit is not a cold-start measurement.

## See also

- [Share data sets as a Shiny
  app](https://www.mheming.com/CerebroNexus/articles/create_a_self_contained_shiny_app.md)
- [Prepare and export a Seurat object in
  R](https://www.mheming.com/CerebroNexus/articles/cerebronexus_workflow_seurat.md)
- [Data integrity
  contracts](https://www.mheming.com/CerebroNexus/articles/data_integrity_contracts.md)
- [Large-dataset
  benchmarks](https://www.mheming.com/CerebroNexus/articles/expression_backend_benchmark.md)
- [`?exportFromSeurat`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md),
  [`?saveCerebro`](https://www.mheming.com/CerebroNexus/reference/saveCerebro.md),
  [`?convertCerebro`](https://www.mheming.com/CerebroNexus/reference/convertCerebro.md)
