# Convert a SingleCellExperiment object to a Cerebro file

One-step route from a `SingleCellExperiment` (SCE) object, or a file
holding one, to a `.crb` file that
[`createShinyApp`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
can serve. It is the SCE counterpart of
[`convertSeuratToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md):
both run the same preparation (group validation and renaming, marker
tables, most-expressed genes and mean expression) and then hand over to
an exporter built on the same core, so every storage option and feature
of the Seurat route is available here.

## Usage

``` r
convertSCEToCerebro(
  sce_file,
  result_dir,
  assay = "logcounts",
  experiment_name = NULL,
  organism = "Human",
  groups = NULL,
  groups_naming = NULL,
  max_group_levels = 100,
  nUMI = "nUMI",
  nGene = "nGene",
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
  extra_features = NULL,
  .source_name = NULL
)
```

## Arguments

- sce_file:

  A `SingleCellExperiment` object, or the path to a `.rds`
  ([`saveRDS()`](https://rdrr.io/r/base/readRDS.html)) or `.qs2`
  ([`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html)) file
  holding one, or to an `.h5ad` file (read with
  [`readH5adAsSCE`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md);
  see
  [`convertH5adToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md)).
  There is no SCE-specific file extension: an SCE is an ordinary R
  object, so it is stored with whichever of these serializers was used.

- result_dir:

  Directory the `.crb` is written to (created if missing). The file is
  named `cerebro_<basename>.crb`.

- assay:

  Name of the assay to export; default `"logcounts"`.

- experiment_name:

  Experiment name stored in the Cerebro object. Defaults to the file's
  base name, or `"SingleCellExperiment"` for an in-memory object.

- organism:

  Organism label (e.g. `"Human"`, `"Mouse"`).

- groups:

  Character vector of `colData` columns to use as grouping variables.
  Required; an error lists the usable columns when omitted.

- groups_naming:

  Named list renaming grouping variables (`old = "new"`); default
  `NULL`.

- max_group_levels:

  Groups with more unique values than this are excluded; default `100`.

- nUMI, nGene:

  `colData` columns holding transcripts and expressed genes per cell.
  When a column is missing it is computed from the `counts` assay (or
  from `assay` if there is no `counts`).

- add_all_meta_data, use_delayed_array, expression_matrix_mode, codec,
  verbose:

  As in
  [`exportFromSCE`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md).

- viewer_binary, viewer_binary_threshold:

  As in
  [`exportFromSCE`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md).

- cell_cycle, main_group, projections:

  As in
  [`exportFromSCE`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md).

- marker_file:

  Optional marker gene table (`.csv`, `.tsv`, `.txt`, `.tab`).

- marker_method:

  Label for the marker table; default `"Diff. Expression"`.

- add_most_expressed_genes:

  Compute the most expressed genes and mean expression per group;
  default `TRUE`.

- most_expressed_genes:

  Optional pre-computed most-expressed-genes table(s); see
  [`convertSeuratToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md).

- spatial_split_by:

  Optional `colData` column dividing the spatial coordinates into one
  section per level; see
  [`exportFromSCE`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md).

- extra_features:

  Alternative experiments (`altExp`, for example antibody counts
  `"ADT"`) to add to the expression data as extra features: their names,
  or `TRUE` for all. Features are named `<experiment>:<feature>`. They
  are taken from the assay of the same name as `assay` when it exists,
  otherwise from `X`, `counts` or the first assay, so check that this is
  on the same scale as the genes.

- .source_name:

  Internal. Base name used for the output file and the default
  experiment name when `sce_file` is an object that was read from a file
  by a caller (see
  [`convertH5adToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md)).

## Value

The path of the written `.crb`, invisibly.

## Details

Immune repertoire and HLA typing stored in `metadata(sce)` are exported
like their `@misc` counterparts on a Seurat object. Attaching BCR/TCR
files (`bcr_file`/`tcr_file`) is not available for SCE yet.

## See also

[`exportFromSCE`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md),
[`convertSeuratToCerebro`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md)

## Examples

``` r
if (FALSE) { # \dontrun{
convertSCEToCerebro(
  sce_file = system.file("extdata/examples/pbmc_SCE.rds",
    package = "CerebroNexus"),
  result_dir = "output",
  experiment_name = "PBMC (SCE)",
  organism = "Human",
  groups = c("sample", "cluster")
)
} # }
```
