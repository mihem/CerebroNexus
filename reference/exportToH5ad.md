# Export a Cerebro file to AnnData (h5ad)

Writes the contents of a `.crb` file (or a Cerebro object) as an `.h5ad`
file that anndata and scanpy open directly, closing the loop with
[`convertH5adToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md).
It is written natively with rhdf5, without Python. Together with
[`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md)
this makes `.crb` the hub between Seurat, SingleCellExperiment and
AnnData:
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
/
[`exportFromSCE()`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md)
/
[`convertH5adToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md)
go in, `exportToH5ad()` comes out.

## Usage

``` r
exportToH5ad(input, file, overwrite = FALSE, verbose = FALSE)
```

## Arguments

- input:

  Path to a `.crb` file, or a `Cerebro` object.

- file:

  Output `.h5ad` path.

- overwrite:

  Replace an existing `file`; default `FALSE`.

- verbose:

  Report what was skipped.

## Value

`file`, invisibly.

## Details

The mapping is:

- the expression matrix becomes `X`, stored as a CSR matrix of cells x
  genes. Cerebro keeps genes x cells, and the CSC of genes x cells is
  the CSR of cells x genes as it is, so no data are rearranged. It is
  whatever matrix the `.crb` was exported with (typically
  log-normalised), under any backend (embedded, h5, BPCells), and is
  loaded into memory as a sparse matrix.

- cell meta data becomes `obs`: factors and repeated strings as
  categoricals, logical, integer and numeric columns in their own types
  (missing values as nullable arrays or `NaN`).

- projections become `obsm`: `UMAP` is `X_umap`, `TSNE` is `X_tsne`,
  `PCA` is `X_pca`, others `X_<name>`.

- spatial coordinates and tissue images become `obsm["spatial"]` and
  `uns["spatial"]`.

- marker gene tables are stored as data frames under
  `uns["cerebro"]["marker_genes"]`, which
  [`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md)
  reads back.

Missing values in a grouping variable are the explicit `"N/A"` level
that Cerebro gives them on export, so they arrive in `obs` as that
category rather than as `NaN`. Spatial sections become `obsm["spatial"]`
(full-resolution pixel coordinates, `NaN` for cells outside every
section) and, per section, `uns["spatial"][<library>]` with its tissue
images and scale factors, so scanpy and squidpy can draw them; the spot
diameter scanpy wants is estimated from the spot spacing. Immune
receptors become scirpy's `obsm["airr"]` (one record per chain, as an
awkward array; scirpy and
[`readH5adAsSCE()`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md)
read it), and the pseudotime of each trajectory becomes an `obs` column.
HLA typing, trees, trajectory layouts and edges, enriched pathways and
extra material have no AnnData counterpart and are not exported; a
message names what was left out. The file is written to a private
temporary file and moved into place only when complete.

## See also

[`convertH5adToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md),
[`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md),
[`readCerebro`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md)

## Examples

``` r
if (FALSE) { # \dontrun{
exportToH5ad("cerebro_pbmc.crb", "pbmc.h5ad")
# then, in Python: adata = anndata.read_h5ad("pbmc.h5ad")
} # }
```
