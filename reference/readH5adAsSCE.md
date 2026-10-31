# Read an h5ad file as a SingleCellExperiment

Reads the AnnData on-disk format (anndata 0.8 or later) directly from
HDF5 with rhdf5, without Python or zellkonverter, and returns a
`SingleCellExperiment`. From there the object can go to
[`convertSCEToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md),
or to any other tool that reads SCE.

## Usage

``` r
readH5adAsSCE(
  file,
  use_raw = FALSE,
  modality = NULL,
  gene_names = NULL,
  airr = "auto",
  airr_sample = NULL,
  spatial_images = "auto",
  extra_modalities = NULL,
  verbose = FALSE,
  .root = NULL
)
```

## Arguments

- file:

  Path to a `.h5ad` file, or a `.h5mu` (MuData) file together with
  `modality`. Files written by anndata before 0.7 (compound-dataset
  layout, `raw.X`) are read too.

- use_raw:

  Use `raw/X` (and `raw/var`) as the expression matrix, as scanpy
  workflows often keep the full, unfiltered gene set there. `layers` are
  then not read, because their genes differ.

- extra_modalities:

  For a MuData file: other modalities to read next to the expression one
  (for example `"prot"` for antibody counts), or `"all"` for every
  modality except the expression and receptor ones. Each becomes an
  alternative experiment of the result, with its own features and the
  cells of the expression modality (cells it lacks get zeros).

- modality:

  For a MuData file (`.h5mu`), the modality to read, e.g. `"gex"`.
  Optional when the file has one modality or one obviously
  expression-like name (`rna`, `gex`).

- gene_names:

  Name of a `var` column to use as the gene names, e.g. `"feature_name"`
  (CELLxGENE) or `"gene_symbols"`. By default the `var` index is used,
  except when it is made of Ensembl IDs and such a column exists. The
  original index stays in `rowData` as `var_index`.

- airr:

  Immune receptor data to import: `"auto"` (default) uses the MuData
  modality named `airr`, or `obsm["airr"]` of a plain h5ad, if the file
  has one and imports nothing otherwise. Give a name to use another
  modality (or `obsm` entry); `FALSE` or `NULL` skips receptor data. The
  AIRR modality is never taken for the expression modality.

- airr_sample:

  Column of `obs` that names the sample of each cell, which keys the
  immune repertoire. By default `orig.ident`, `sample`, `Sample` or
  `sample_id` is used if present, else all cells are one sample
  (`"Sample_1"`).

- spatial_images:

  Which Visium tissue images to embed: `"auto"` (default; `hires` when
  present, otherwise `lowres`), a character vector of image labels
  (`"hires"`, `"lowres"`, `"fullres"`), or `FALSE` for none. Needs the
  png package.

- verbose:

  Print what was skipped.

- .root:

  Internal: HDF5 group that holds the data set (used for the modalities
  inside a MuData file). Leave `NULL`.

## Value

A `SingleCellExperiment` with genes in rows and cells in columns.
Duplicate gene names are made unique with a message; duplicate cell
names are an error.

## Details

The mapping is:

- `X` becomes the assay `"X"`; each entry of `layers` becomes an assay
  of the same name (so `layers["counts"]` is the assay `"counts"`).

- `obs` / `var` become `colData` / `rowData`. Categorical, string,
  boolean and nullable integer/boolean/string columns are all decoded;
  missing values stay `NA`.

- `obsm` entries with at least two numeric columns become `reducedDims`;
  a leading `X_` is dropped and `pca`, `umap` and `tsne` are upper-cased
  (`X_umap` is `"UMAP"`).

- scanpy's `uns["rank_genes_groups"]` results become
  `metadata(sce)$marker_genes`, one table per `groupby` variable; marker
  tables written by
  [`exportToH5ad`](https://mihem.github.io/CerebroNexus/reference/exportToH5ad.md)
  under `uns["cerebro"]["marker_genes"]` are read back as they were.

- immune receptor chains written by scirpy (an awkward array in
  `obsm["airr"]`, or the `airr` modality of a MuData file) become
  `metadata(sce)$immune_repertoire`: a list named by sample, one row per
  cell with `barcode`, `CTgene`, `CTnt`, `CTaa` and `CTstrict`,
  assembled into clones by scRepertoire as on the Seurat route. Only
  productive chains of TCR/BCR loci with a CDR3 (`junction_aa`) count; a
  cell that shows both TCR and BCR chains keeps the type with more
  chains (then more UMIs). Cells are matched to the expression data by
  name.

- trajectories become `metadata(sce)$trajectories`, the structure of
  `object@misc$trajectories`. For every pseudotime column in `obs`
  (`dpt_pseudotime`, `velocity_pseudotime`, `latent_time`, any
  `*_pseudotime`) one trajectory is made per source: scanpy's PAGA
  (`uns["paga"]`) as method `"paga"`, with the groups as nodes at the
  centroid of their cells in the UMAP (t-SNE) and the connectivities of
  at least 0.01 as edges; scVelo's `obsm["velocity_<basis>"]` as method
  `"scvelo"`, the velocity averaged on a grid (25 x 25 at most) and
  drawn as arrows; and, when the file has neither, just the pseudotime
  as method `"pseudotime"`. The `velocity_*` entries of `obsm` are not
  embeddings and are not added to `reducedDims`. Gene-level velocity
  (`layers` `velocity`) and the directed PAGA transitions are not used.

- scanpy's category colours (`uns["<column>_colors"]`) become
  `metadata(sce)$group_colors`, a named vector (level to colour) per
  categorical column, and are applied as the default colours of those
  groups in the Viewer. A column is used only when it has one valid
  colour for each of its levels.

- Visium tissue images in `uns["spatial"][<library_id>]["images"]`
  become `metadata(sce)$spatial_images`, one entry per library, each
  with its image as an embedded PNG and its extent in spot-coordinate
  space: spot coordinates (`obsm["spatial"]`) are full-resolution pixels
  and an image is the full-resolution picture scaled by its
  `tissue_*_scalef`, so it covers `[0, width/scale]` by
  `[0, height/scale]`. The image keeps its own orientation (row 0 on
  top); flip it for display in the Spatial tab, as for Seurat objects.

Sparse `X` (CSR or CSC) and dense `X` are supported. A CSR matrix is
used as it is, because the CSR of cells x genes is the CSC of genes x
cells. Other `uns`, `obsp`, `varm` and `varp` content is not read.

## See also

[`convertH5adToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md),
[`convertSCEToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sce <- readH5adAsSCE("pbmc.h5ad")
convertSCEToCerebro(sce, "output", groups = "cell_type")
} # }
```
