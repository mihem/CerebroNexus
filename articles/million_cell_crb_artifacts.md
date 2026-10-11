# Working with large datasets

## Choose an expression backend

CerebroNexus supports embedded matrices, sparse HDF5 and BPCells. Select
the backend with `expression_matrix_mode` in
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
or
[`convertSeuratToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md):

| Backend | Practical use | Files to keep together |
|----|----|----|
| `embedded` | Small datasets and simple single-file distribution | `.crb` |
| `h5` | On-disk sparse expression, including single-gene queries | `.crb` and its recorded `.h5` sidecar |
| `bpcells` | On-disk expression, block access and large-data workflows | `.crb` and its complete `.bpcells/` directory |

There is no universal fastest backend. Use the [benchmark
guide](https://mihem.github.io/CerebroNexus/articles/expression_backend_benchmark.md)
for measured workloads and their limits. Avoid converting a large
delayed or on-disk matrix into a dense matrix merely to inspect or
export it.

## Export, read, save and convert

This small example creates a portable CRB without downloading a dataset:

``` r
library(CerebroNexus)
cells <- c("cell-a", "cell-b", "cell-c")
object <- Cerebro$new()
object$expression <- matrix(c(1, 0, 2, 3, 0, 4), nrow = 2,
  dimnames = list(c("gene-a", "gene-b"), cells))
object$setMetaData(data.frame(cell_barcode = cells, sample = c("A", "A", "B")))
object$addProjection("umap", data.frame(x = 1:3, y = c(0, 1, 0), row.names = cells))
work <- tempfile("cerebro-guide-")
dir.create(work)
file <- file.path(work, "example.crb")
saveCerebro(object, file)
restored <- readCerebro(file)
stopifnot(identical(restored$getMetaData()$cell_barcode, cells))
```

For a prepared Seurat object, export with the required
grouping/projection arguments for your dataset (see
[`?exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)).
For example:

``` r
exportFromSeurat(seurat_object, file = "study.crb", experiment_name = "Study",
  organism = "hg", groups = c("sample", "seurat_clusters"),
  expression_matrix_mode = "bpcells")
```

Use
[`readCerebro()`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md)
for legacy RDS and newer CRBs; `.crb` describes the logical format, not
its physical codec. `saveCerebro(..., codec = "rds")` requests RDS
compatibility, while the default uses qs2.
[`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md)
changes a CRB’s serialization without replacing its external expression
data:

``` r
convertCerebro("study.crb", "study-rds.crb", codec = "rds")
```

## Move the complete dataset

Keep paths recorded inside the CRB valid. A typical layout is:

``` text
study.crb
study.bpcells/
study.viewer/
```

Additional spatial molecule/image resources and a lazy immune-repertoire
sidecar must also travel with their dataset. Do not rename or partially
copy sidecars independently. RDS and qs2 versions can share the same
unchanged sidecar. Cell-order checksums reject mismatched BPCells cell
indexes.

For distribution,
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
builds a self-contained Viewer directory and copies the required
resources. It resolves colliding CRB basenames and places each Viewer
Pack beside the final CRB name. Follow the [app creation
guide](https://mihem.github.io/CerebroNexus/articles/create_a_self_contained_shiny_app.md).

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
missing, incompatible or stale asset falls back to the ordinary data
path. Older packs without verified projection identity do not supply
projection coordinates; independent safe assets remain usable in the
Viewer. Old violin assets lacking exact-summary metadata also fall back.
Rebuilding restores the affected fast paths. Do not suppress checksum
failures or edit a manifest to make a pack appear valid.

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

The optional complete large-data launcher requires a repository
checkout:

``` bash
git clone https://github.com/mihem/CerebroNexus.git
cd CerebroNexus
Rscript -e "install.packages(c('remotes', 'pkgload')); remotes::install_local('.', dependencies = TRUE)"
Rscript run-demo.R --ren-only --prepare-only
Rscript run-demo.R --ren-only --no-browser
```

Omit `--ren-only` to prepare both mouse-million and Ren data. The
launcher prints the URL, exact source directory and dataset version, and
reuses compatible prepared caches. Preparation can download large files
and take substantial time; see the [benchmark
guide](https://mihem.github.io/CerebroNexus/articles/expression_backend_benchmark.md)
before starting. Set `CEREBRO_LARGE_CACHE` to choose an external cache
location. The marker-guided mouse trajectory is a labelled
demonstration, not a formal inference result.

## Troubleshooting

- **Missing backend/sidecar:** install the backend package and restore
  the recorded sibling file/directory. A control CRB alone is not a
  complete external dataset.
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
