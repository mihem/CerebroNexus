# Working with large datasets

## Choose an expression backend

CerebroNexus stores the expression matrix either inside the `.crb` or in
a sparse HDF5 file next to it. Select the storage with
`expression_matrix_mode` in
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
or
[`convertSeuratToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md):

| Mode | Practical use | Files to keep together |
|----|----|----|
| `embedded` | Small datasets and simple single-file distribution | `.crb` |
| `h5` | On-disk sparse expression, including single-gene queries and large datasets | `.crb` and its recorded `.h5` sidecar |

An H5-backed `.crb` is thin: the cell names live once in the H5 file and
the `.crb` keeps a checksum of them (see [Data integrity
contracts](https://mihem.github.io/CerebroNexus/articles/data_integrity_contracts.md)).
A very large immune repertoire can also move to a third sibling,
`<stem>.immune.qs2` (`immune_sidecar`, see
[`?saveCerebro`](https://mihem.github.io/CerebroNexus/reference/saveCerebro.md)).

Use the [benchmark
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

For a prepared Seurat object, export with the required
grouping/projection arguments for your dataset (see
[`?exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)).
For example:

``` r
exportFromSeurat(seurat_object, file = "study.crb", experiment_name = "Study",
  organism = "hg", groups = c("sample", "seurat_clusters"),
  expression_matrix_mode = "h5")
```

This writes `study.crb` and `study.h5`.
[`readCerebro()`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md)
reads RDS and qs2 CRBs; `.crb` describes the logical format, not its
physical codec. `saveCerebro(..., codec = "rds")` requests RDS
compatibility, while the default uses qs2.
[`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md)
re-serializes a CRB while reusing its existing expression data:

``` r
convertCerebro("study.crb", "study-rds.crb", codec = "rds")
```

[`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md)
is also the one-time migration path for a CRB written by an older
release. Such a file is rejected with
`uses a retired format ... Convert it once with CerebroNexus::convertCerebro()`.
A CRB whose expression storage this version cannot read or repair is
rejected there too; re-export it from the source Seurat object with
`expression_matrix_mode = "h5"`.

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
Rscript run-demo.R --ren-only --prepare-only
Rscript run-demo.R --ren-only --no-browser
```

The launcher prints the URL, the source directory and the Ren data
version (currently 2), and reuses a complete compatible cache. Keep each
`.crb` and its `.h5` file together. Prepared versions use separate
directories (`ren/cerebro-v2/`); preparation never upgrades the earlier
`ren/cerebro/` atlas in place, failed builds are not published, and
source downloads are shared between versions.

The launcher also builds each data set’s Viewer Pack and rebuilds it
when it is missing or predates an asset the viewer reads. Without a
pack, the HLA & TCR motifs page spends about 15 s (about 25 s for the
whole first entry) building the CDR3 Hamming-1 graph at Ren’s scale;
with one, the same entry settles in under two seconds. If you start the
viewer against a prepared `.crb` yourself, build the pack once (about a
minute for Ren) with
`buildViewerPack("path/to/data.crb", overwrite = TRUE)`; rerunning it
after re-preparing a CRB is also the repair.

The 10x marker-guided E18 neurogenesis path excludes endothelial,
microglial and oligodendrocyte programs, orders the remaining neural
lineage with early, transitional and late markers, and separates
excitatory and inhibitory terminal branches. It is a labelled
demonstration, not a formal trajectory-inference result. The same app
also includes the bundled PBMC, immune-repertoire, trajectory, spatial,
Trekker and HLA/TCR demos. See the [benchmark
guide](https://mihem.github.io/CerebroNexus/articles/expression_backend_benchmark.md)
before starting.

## Troubleshooting

- **Missing sidecar:** restore the recorded sibling `.h5` (or
  `.immune.qs2`) file. A thin CRB alone is not a complete external
  dataset.
- **Retired format:** convert the file once with
  [`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md),
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
