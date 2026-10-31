# Use SingleCellExperiment and AnnData data

## Overview

Cerebro does not need a Seurat object. A `.crb` file is the common
format, and three kinds of input lead to it:

| You have | Convert with |
|----|----|
| Seurat object (`.rds`, `.qs2`) | [`convertSeuratToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md) |
| SingleCellExperiment (`.rds`, `.qs2`) | [`convertSCEToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md) |
| AnnData / scanpy file (`.h5ad`), MuData (`.h5mu`) | [`convertH5adToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md) |

Because `.crb` is the hub, the formats also convert into each other: a
Seurat object can be written as `.h5ad` for scanpy, an `.h5ad` file can
be turned into a SingleCellExperiment, and so on. AnnData files are read
natively with `rhdf5`; no Python is needed.

## From an AnnData file

``` r
library(CerebroNexus)

crb <- convertH5adToCerebro(
  "pbmc.h5ad",
  result_dir = "cerebro",
  groups = c("louvain", "sample"),
  organism = "Human"
)
```

This takes what scanpy stored, not only the matrix:

- the embeddings in `obsm` (`X_umap`, `X_pca`, …) as projections;
- the ranked marker genes from `sc.tl.rank_genes_groups`;
- scanpy’s category colours (`uns["<column>_colors"]`);
- Visium coordinates and tissue images (`obsm["spatial"]`,
  `uns["spatial"]`), one spatial section per library;
- scirpy TCR/BCR tables (`airr` argument) and PAGA / scVelo / pseudotime
  trajectories.

Use `use_raw = TRUE` for `adata.raw`, `modality =` to pick one modality
of an `.h5mu` file, and `gene_names =` to name genes by a `var` column
when the index holds Ensembl IDs.

## From a SingleCellExperiment

``` r
crb <- convertSCEToCerebro(
  "sce.rds",
  result_dir = "cerebro",
  assay = "logcounts",
  groups = c("cluster", "sample")
)
```

## From a SpatialData store

``` r
crb <- convertSpatialDataToCerebro(
  "visium.zarr",
  result_dir = "cerebro",
  groups = c("region", "in_tissue"),
  organism = "Mouse"
)
```

The cell table, the coordinates and the tissue pictures are read from
the `.zarr` directory, with one spatial section per region. Coordinates
come from `obsm["spatial"]` when the table has them (Visium) and from
the shapes the table annotates otherwise (MERFISH, Xenium). Stores
compressed with Blosc (lz4, zstd or zlib), zstd or gzip are read; a
store that needs another codec stops with the codec’s name. Blosc chunks
are decoded in R, lz4 (the numcodecs default) at about 4 MB/s, so
re-save a large store with zstd, or use an `.h5ad` file.

## Back to scanpy

``` r
exportToH5ad("cerebro/cerebro_pbmc.crb", "pbmc_back.h5ad")
```

``` python
import anndata as ad
adata = ad.read_h5ad("pbmc_back.h5ad")
```

Expression, cell metadata, projections, marker genes, category colours
and, for spatial data, coordinates and tissue images are written. A
message lists what `.h5ad` has no place for (immune repertoire,
trajectories, trees, …).

## In the Builder

`launchCerebroBuilder()` accepts `.h5ad`, `.h5mu`, a `.zip` of a
SpatialData `.zarr` folder, and SingleCellExperiment `.rds` / `.qs2`
files like Seurat ones, and offers PAGA, scVelo and pseudotime
trajectories next to monocle2. Spatial coordinates arrive as one spatial
section per `library_id`, and Visium tissue pictures are attached to
their sections as images at the position their scale factor gives.
