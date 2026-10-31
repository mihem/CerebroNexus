# Convert an AnnData (h5ad) file to a Cerebro file

One-step route from a `.h5ad` file, the container of the Python scverse
ecosystem (AnnData, scanpy), to a `.crb` file that
[`createShinyApp`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
can serve. The file is read natively from HDF5 with
[`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md),
so neither Python nor zellkonverter is needed, and the resulting
`SingleCellExperiment` goes through
[`convertSCEToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md),
so every option of that route applies here too.

## Usage

``` r
convertH5adToCerebro(
  h5ad_file,
  result_dir,
  assay = "X",
  use_raw = FALSE,
  modality = NULL,
  gene_names = NULL,
  airr = "auto",
  airr_sample = NULL,
  extra_modalities = NULL,
  experiment_name = NULL,
  organism = "Human",
  groups = NULL,
  groups_naming = NULL,
  max_group_levels = 100,
  nUMI = NULL,
  nGene = NULL,
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "h5"),
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  verbose = TRUE,
  cell_cycle = NULL,
  main_group = NULL,
  projections = NULL,
  marker_file = NULL,
  marker_method = "Diff. Expression",
  add_most_expressed_genes = TRUE,
  most_expressed_genes = NULL,
  spatial_split_by = NULL,
  spatial_images = "auto"
)
```

## Arguments

- h5ad_file:

  Path to a `.h5ad` file (any anndata version, including the pre-0.7
  layout), or to a MuData `.h5mu` file together with `modality`.

- result_dir:

  Directory the `.crb` is written to (created if missing). The file is
  named `cerebro_<basename>.crb`.

- assay:

  Which matrix to export: `"X"` (default) or the name of an entry of
  `layers`, e.g. `"counts"`. With `use_raw = TRUE` the matrix is `raw/X`
  and this must stay `"X"`.

- use_raw:

  Use `raw/X` (and `raw/var`) as the expression matrix; see
  [`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md).

- modality:

  For a MuData file, the modality to convert (e.g. `"gex"`); see
  [`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md).

- gene_names:

  `var` column to use as gene names (default: the index, or a symbol
  column when the index is Ensembl IDs); see
  [`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md).

- airr:

  Immune receptor (TCR/BCR) data to import: `"auto"` (default) uses the
  MuData modality named `airr`, or `obsm["airr"]` of a plain h5ad, if
  the file has one, and imports nothing otherwise. Give a name to use
  another modality (or `obsm` entry), or `FALSE` to skip receptor data.
  See
  [`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md).

- airr_sample:

  `obs` column naming the sample of each cell, which keys the immune
  repertoire. By default `orig.ident`, `sample`, `Sample` or `sample_id`
  is used if present, else one sample.

- extra_modalities:

  For an `.h5mu` file, other modalities to add to the expression data as
  extra features, for example `"prot"`. Their features are named
  `<modality>:<feature>`. See
  [`convertSCEToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md),
  argument `extra_features`.

- experiment_name, organism, groups_naming, max_group_levels,
  add_all_meta_data, use_delayed_array, expression_matrix_mode, codec,
  viewer_binary, viewer_binary_threshold, verbose, cell_cycle,
  main_group, projections, add_most_expressed_genes,
  most_expressed_genes:

  As in
  [`convertSCEToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md).
  `experiment_name` defaults to the file's base name.

- groups:

  Character vector of `obs` columns to use as grouping variables.
  Required; an error lists the usable columns when omitted.

- nUMI, nGene:

  `obs` columns holding transcripts and expressed genes per cell. By
  default the usual scanpy/Seurat names are looked for (`total_counts`,
  `n_counts`, `nCount_RNA`, `nUMI` and `n_genes_by_counts`, `n_genes`,
  `nFeature_RNA`, `nGene`); when none exists the values are computed
  from the `counts` layer (or from `assay`).

- marker_file, marker_method:

  Optional marker table, as in
  [`convertSCEToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md).
  When given, it replaces the marker results taken from scanpy.

- spatial_split_by:

  `obs` column dividing `obsm["spatial"]` into one section per level
  (e.g. the tissue sections of a MERFISH data set); see
  [`exportFromSCE`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md).

- spatial_images:

  Which Visium tissue images to embed (`"auto"`, a vector of labels, or
  `FALSE`); see
  [`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md).

## Value

The path of the written `.crb`, invisibly.

## Details

Category colours (`uns["<column>_colors"]`) are kept as the groups'
default colours in the Viewer. Visium tissue images are embedded and
drawn behind the spots; when a file has the images of several libraries
(`obs["library_id"]`) each library becomes one section with its own
image. The Viewer's Spatial plot has the y axis pointing up, so a Visium
image, stored with row 0 on top, needs the vertical flip to line up with
the spots: build the app with
`createShinyApp(..., spatial_image_settings = list(<dataset label> = list(spatial = list(hires = list(flip_y = TRUE)))))`.
As for Seurat Visium objects, the coordinates are the file's, so the
whole picture is then the mirror image (top to bottom) of the slide as
drawn by scanpy.

scanpy's `rank_genes_groups` results, if present, are exported as marker
genes. Embeddings in `obsm` (`X_umap`, `X_tsne`, `X_pca`, ...) become
the projections; PCA is dropped when a non-PCA embedding exists, as on
the other routes. Receptor chains written by scirpy (`obsm["airr"]`)
fill the Immune repertoire page.

## See also

[`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md),
[`convertSCEToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md)

## Examples

``` r
if (FALSE) { # \dontrun{
convertH5adToCerebro(
  h5ad_file = "pbmc.h5ad",
  result_dir = "output",
  groups = c("cell_type", "sample"),
  expression_matrix_mode = "h5"
)
} # }
```
