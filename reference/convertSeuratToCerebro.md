# Convert Seurat Object to Cerebro Format

This function accepts a Seurat object or reads one from an RDS file,
optionally renames grouping variables, loads marker gene tables, and
exports the data to Cerebro format for visualization.

## Usage

``` r
convertSeuratToCerebro(
  seurat_file,
  result_dir,
  assay = "RNA",
  slot = "data",
  experiment_name = "Dura Mater - All Cells",
  organism = "Human",
  groups = c("seurat_clusters", "orig.ident", "cell_type_final"),
  groups_naming = NULL,
  max_group_levels = 100,
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA",
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "bpcells", "h5"),
  spatial_images = NULL,
  verbose = TRUE,
  cell_cycle = NULL,
  marker_file = NULL,
  marker_method = "Diff. Expression",
  add_most_expressed_genes = TRUE,
  most_expressed_genes = NULL,
  bcr_file = NULL,
  tcr_file = NULL
)
```

## Arguments

- seurat_file:

  A Seurat object in memory, or a character string specifying its `.rds`
  file path.

- result_dir:

  Character string specifying the directory where the Cerebro output
  file (.crb) will be saved.

- assay:

  Character string specifying which assay to use from the Seurat object;
  default: `"RNA"`.

- slot:

  Character string specifying which slot to extract expression data from
  (e.g., "data", "counts", "scale.data"); default: `"data"`.

- experiment_name:

  Character string for the experiment name to be stored in the Cerebro
  object; default: `"Dura Mater - All Cells"`.

- organism:

  Character string specifying the organism (e.g., "Human", "Mouse");
  default: `"Human"`.

- groups:

  Character vector of column names in the Seurat metadata to use as
  grouping variables; default:
  `c("seurat_clusters", "orig.ident", "cell_type_final")`.

- groups_naming:

  Named list for renaming grouping variables. Names are the old column
  names, values are the new names; default: `NULL`.

- max_group_levels:

  Numeric value specifying the maximum number of unique levels allowed
  in a grouping variable. Groups with more unique values than this
  threshold will be excluded; default: `100`.

- nUMI:

  Character string specifying the column name in metadata containing the
  number of UMIs per cell; default: `"nCount_RNA"`.

- nGene:

  Character string specifying the column name in metadata containing the
  number of expressed genes per cell; default: `"nFeature_RNA"`.

- add_all_meta_data:

  Logical indicating whether to include all metadata columns in the
  export; default: `TRUE`.

- use_delayed_array:

  Logical indicating whether to convert expression data to DelayedArray
  format for memory efficiency; default: `FALSE`. Ignored when
  `expression_matrix_mode` is set to an external backend.

- expression_matrix_mode:

  How to persist the expression matrix in the generated `.crb`. One of
  `"embedded"` (default), `"bpcells"` or `"h5"`. See
  [`exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
  for details; briefly, `"embedded"` stores the matrix inside the `.crb`
  (legacy behaviour), `"bpcells"` writes an on-disk BPCells directory
  next to the `.crb` and keeps a lightweight handle inside it (typically
  reduces `.crb` size by ~80 `"h5"` writes a TENx-format HDF5 file next
  to the `.crb` that the Shiny runtime loads lazily, minimising RAM and
  startup time (recommended default for large datasets). The Shiny
  runtime re-resolves both backends relative to the `.crb`'s parent
  directory, so packaging the `.crb` with its sibling `<stem>.bpcells/`
  or `<stem>.h5` together is enough for portable deployment.

- spatial_images:

  Optional manifest in `spatial entry -> image label -> path` form.
  Spatial-entry names must match `SeuratObject::Images(seurat_file)`.
  Each entry may contain one or more arbitrarily named PNG, JPEG/JPG, or
  SVG files. A named character vector is path shorthand; a leaf may
  instead be
  `list(path = ..., bounds = c(xmin = ..., xmax = ..., ymin = ..., ymax = ...))`.
  Missing bounds are derived from the exported coordinates. Supplied
  images are embedded in the generated CRB; existing embedded images
  declared by the Seurat object are retained.

- verbose:

  Logical indicating whether to print progress messages; default:
  `TRUE`.

- cell_cycle:

  Character vector of column names in metadata containing cell cycle
  phase assignments; default: `NULL`.

- marker_file:

  Character string specifying the path to a marker gene table file.
  Supported formats: .csv, .tsv, .txt, .tab; default: `NULL`.

- marker_method:

  Character string specifying the name of the method used to identify
  marker genes (will be used as a label in Cerebro); default:
  `"Diff. Expression"`.

- add_most_expressed_genes:

  Logical indicating whether to calculate the most expressed genes for
  each group; default: `TRUE`.

- most_expressed_genes:

  Optional pre-calculated most expressed genes data. Can be either a
  data.frame (will be converted to list(unknown = ...)) or a list of
  data.frames. If list elements are unnamed, they will be assigned names
  like "unknown1", "unknown2", etc.; default: `NULL`.

- bcr_file:

  Character string specifying the path to a BCR data file (.rds format).
  The data will be merged into the unified `immune_repertoire` slot of
  the Seurat object before export; default: `NULL`.

- tcr_file:

  Character string specifying the path to a TCR data file (.rds format).
  The data will be merged into the unified `immune_repertoire` slot of
  the Seurat object before export; default: `NULL`.

## Value

This function does not return a value. It saves a Cerebro object (.crb
file) to the specified `result_dir`.

## Details

The function performs the following steps:

1.  Reads the Seurat object from `seurat_file`

2.  Renames grouping columns if `groups_naming` is provided

3.  Loads marker gene tables from `marker_file` if provided:

    - For Excel files with multiple sheets, each sheet becomes a
      separate group

    - For single-sheet files, data is split by the first column

4.  Exports the processed data using
    [`exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)

5.  Saves the result as `cerebro_<basename>.crb` in `result_dir`

6.  Cleans up memory by removing the Seurat object and calling garbage
    collection

Seurat uses the same named image collection for several
platform-specific structures: a name can identify a Visium slice, Xenium
or MERFISH field of view (FOV), Slide-seq puck, or another SpatialImage
subclass. It is a structural spatial-entry key, not necessarily a donor
or sample name. An entry can carry multiple user-named backgrounds, or
coordinates only.

## See also

[`exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(CerebroNexus)

input_dir <- system.file("extdata/examples", package = "CerebroNexus")
convertSeuratToCerebro(
  seurat_file = file.path(input_dir, "pbmc_seurat.rds"),
  result_dir = "output",
  assay = "RNA",
  slot = "data",
  experiment_name = "PBMC example",
  organism = "Human",
  groups = c("sample", "seurat_clusters"),
  groups_naming = list("seurat_clusters" = "cluster")
)
# Creates output/cerebro_pbmc_seurat.crb.
} # }
```
