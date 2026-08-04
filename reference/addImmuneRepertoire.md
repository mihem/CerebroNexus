# Add immune repertoire data to a Seurat object

Puts TCR and/or BCR data into `object@misc$immune_repertoire` in the
shape CerebroNexus reads: a list named by sample, each element a
data.frame whose `barcode` column matches the object's cell names.

## Usage

``` r
addImmuneRepertoire(
  object,
  tcr = NULL,
  bcr = NULL,
  sample_col = "orig.ident",
  sample_names = NULL,
  groups = NULL,
  from_metadata = TRUE,
  verbose = TRUE
)
```

## Arguments

- object:

  A `Seurat` object.

- tcr:

  TCR data, as either the named list
  [`scRepertoire::combineTCR()`](https://www.borch.dev/uploads/scRepertoire/reference/combineTCR.html)
  returns, the path to an `.rds` holding such a list, or a vector of
  Cell Ranger `filtered_contig_annotations.csv` paths (one per sample,
  assembled with scRepertoire).

- bcr:

  BCR data, in any of the forms accepted for `tcr`.

- sample_col:

  Metadata column identifying the sample, used when reading the
  repertoire out of `meta.data`; defaults to `"orig.ident"`. An explicit
  name must exist. Set to `NULL` to auto-detect `orig.ident`, `sample`,
  or `Sample`, in that order.

- sample_names:

  Sample names for the `.csv` form, in the order the paths are given.
  CSV inputs require either this argument or a completely named path
  vector; sample identities are never guessed from file paths.

- groups:

  Metadata columns to carry into the extracted data.frames when reading
  out of `meta.data`.

- from_metadata:

  When neither `tcr` nor `bcr` supplies data and the unified repertoire
  slot is absent or empty, read the repertoire out of scRepertoire's
  `meta.data` columns (`CTgene`, `CTnt`, `CTaa`, `CTstrict`) if they are
  there. Defaults to `TRUE`. An explicit zero-row input remains empty
  rather than falling through to metadata.

- verbose:

  Print progress messages; defaults to `TRUE`.

## Value

The `Seurat` object, with the repertoire in `@misc$immune_repertoire`.
Returned unchanged when there is no repertoire to add.

## Details

Before this function existed the slot had to be assigned by hand,
following a convention written down in a vignette with no function
behind it and nothing checking the result. The shape is checked here, so
a mistake is reported where it was made rather than surfacing as an
empty page.

Each non-empty sample table must contain `barcode`, `CTgene`, `CTnt`,
`CTaa`, and `CTstrict`. Barcodes identify cells globally: duplicate
rows, cross-sample reuse, and a cell present in both TCR and BCR inputs
are errors. Rows whose barcodes are not cells in `object` are removed
with a warning before storage; a complete mismatch is an error. Zero-row
sample tables are treated as absent.

## Examples

``` r
if (FALSE) { # \dontrun{
## from scRepertoire's output
combined <- scRepertoire::combineTCR(contig_list, samples = donor_ids)
seurat <- addImmuneRepertoire(seurat, tcr = combined)

## or straight from Cell Ranger
seurat <- addImmuneRepertoire(
  seurat,
  tcr = file.path(sample_dirs, "filtered_contig_annotations.csv"),
  sample_names = donor_ids
)

## or from meta.data, after scRepertoire::combineExpression()
seurat <- addImmuneRepertoire(seurat)

exportFromSeurat(seurat, file = "my_data.crb", experiment_name = "mine",
                 organism = "hg", groups = c("sample", "seurat_clusters"))
} # }
```
