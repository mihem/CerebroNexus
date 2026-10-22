# Immune Repertoire Analysis in CerebroNexus

## Overview

CerebroNexus includes a comprehensive **Immune Repertoire** module for
interactive exploration of T-cell and B-cell receptor clonotype data.
The module leverages the `scRepertoire` package and provides
visualizations covering clonal expansion, abundance, diversity, BCR
isotypes, clonotype sharing, and cross-sample comparisons.

The Immune Repertoire tab appears **conditionally** in the sidebar —
only when the loaded `.crb` file contains TCR or BCR clonotype
annotations. The bundled `example.crb` ships with real 10x
immune-repertoire data (the `sc5p_v2_hs_PBMC_10k` dataset, with 5’ gene
expression, TCR, and BCR from the same experiment), so the tab —
including TCR, BCR (isotype), and cross-sample comparisons — is
available out of the box. (This single 10x donor is randomly split into
three demo samples so cross-sample features have data; the sample labels
are not distinct biological donors.)

Grouping and sample-splitting in the module work for **any** grouping
variable present in the data set’s cell metadata (sample, condition,
cell type, etc.). The clonotype tables themselves only need the standard
scRepertoire columns; the module joins metadata onto them by barcode at
runtime.

## Preparing immune repertoire data

### Background: scRepertoire

The immune repertoire module is built on the
[scRepertoire](https://www.borch.dev/uploads/screpertoire/) package (≥
2.0), which is the standard tool for turning 10x Cell Ranger V(D)J
output into per-cell clonotype annotations. CerebroNexus depends on it
as a mandatory `Imports`, so a standard install already provides it (via
Bioconductor):

``` r
# Bioconductor
BiocManager::install("scRepertoire")
```

Installation and namespace loading are separate: `scRepertoire` is
required for standard installations and exported apps that expose the
complete immune module. The Viewer checks availability with
[`system.file()`](https://rdrr.io/r/base/system.file.html) and loads its
namespace only when an analysis calls it. Native Clonal Projection,
Clone Sharing, Definition, and supported native count summaries do not
trigger that namespace load; they still belong to the supported
installation with the dependency present. A first scRepertoire-backed
analysis may therefore incur a process-wide loading cost. Moving the
package between `Imports` and `Suggests` does not eliminate it. Nix
environments include the same required dependency.

Clonal Projection expansion categories use the complete repertoire for
the chosen receptor and clone definition; filtering or sampling
displayed cells does not redefine clone sizes. Clone Sharing can select
categorical cell metadata (such as patient or condition) without first
selecting it as a grouping variable. Only requested metadata columns are
joined to the receptor tables.

The full scRepertoire workflow is documented in its vignettes — [Loading
data](https://www.borch.dev/uploads/screpertoire/articles/loading) and
[Combining
contigs](https://www.borch.dev/uploads/screpertoire/articles/combining_contigs).
The steps below summarise just what CerebroNexus needs.

### Step 1 — load the V(D)J contigs

Read the `filtered_contig_annotations.csv` produced by Cell Ranger for
each sample. TCR and BCR are separate libraries, so load each one:

``` r
library(scRepertoire)

# one filtered_contig_annotations.csv per sample
tcr_contigs <- lapply(tcr_paths, read.csv) # T-cell receptor contigs
bcr_contigs <- lapply(bcr_paths, read.csv) # B-cell receptor contigs
```

### Step 2 — combine contigs into clonotypes

[`combineTCR()`](https://www.borch.dev/uploads/scRepertoire/reference/combineTCR.html)
and
[`combineBCR()`](https://www.borch.dev/uploads/scRepertoire/reference/combineBCR.html)
collapse contigs into per-cell clonotypes and add the standard `CTgene`
/ `CTnt` / `CTaa` / `CTstrict` columns. Use the matching function for
each receptor type:

``` r
combined_tcr <- combineTCR(tcr_contigs, samples = sample_names)
combined_bcr <- combineBCR(bcr_contigs, samples = sample_names)
```

### Step 3 — attach to the Seurat object and export

Hand both receptor types to
[`addImmuneRepertoire()`](https://mihem.github.io/CerebroNexus/reference/addImmuneRepertoire.md)
and export.
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
picks the slot up automatically. A sample is one biological sample
rather than one receptor type, so the T- and B-cell clonotypes of a
given sample are row-bound into a single table — cells are mutually
exclusive, a cell has a TCR *or* a BCR.
[`addImmuneRepertoire()`](https://mihem.github.io/CerebroNexus/reference/addImmuneRepertoire.md)
does that for you and refuses a barcode present in both inputs (usually
a doublet or an unresolved receptor assignment). It checks the result as
it stores it, so a wrong shape or barcodes that reach no cell are
reported here rather than turning into an empty Immune repertoire page
later:

``` r
library(CerebroNexus)

seurat_object <- addImmuneRepertoire(
  seurat_object,
  tcr = combined_tcr,
  bcr = combined_bcr
)

exportFromSeurat(
  seurat_object,
  file = "my_data.crb",
  experiment_name = "my_experiment",
  organism = "hg",
  groups = c("sample", "seurat_clusters")
)
```

One thing to watch: `combineTCR(samples = )` and
`combineBCR(samples = )` prefix every barcode with the sample name. The
cell names have to carry the same prefix —
[`SeuratObject::RenameCells()`](https://satijalab.github.io/seurat-object/reference/RenameCells.html)
— or no receptor can be tied to a cell.
[`addImmuneRepertoire()`](https://mihem.github.io/CerebroNexus/reference/addImmuneRepertoire.md)
stops with that diagnosis rather than storing a repertoire that reaches
nothing.

If filtering the Seurat object removed only some repertoire cells,
[`addImmuneRepertoire()`](https://mihem.github.io/CerebroNexus/reference/addImmuneRepertoire.md)
reports and removes those orphan rows before storage. They therefore
cannot inflate clone abundance or diversity. A sample with no remaining
cell is removed; a repertoire with no matching cell at all is an error.

For Cell Ranger files you may name the path vector instead of sharing
one `sample_names` vector between receptor types:

``` r
seurat_object <- addImmuneRepertoire(
  seurat_object,
  tcr = c(donorA = tcr_a, donorB = tcr_b),
  bcr = c(donorA = bcr_a)
)
```

CSV inputs require either a named path vector or an explicit
`sample_names` argument. CerebroNexus does not guess biological sample
identities from file or directory names.

Each data.frame must contain the scRepertoire clonotype columns
`barcode`, `CTgene`, `CTnt`, `CTaa`, and `CTstrict`. The `barcode`
values should match the cell barcodes in the Seurat object’s metadata.
Chains (e.g. TRA/TRB, IGH/IGK/IGL) are detected automatically from
`CTgene`, and grouping variables (sample, condition, cell type, …) are
read from the cell metadata at runtime — the clonotype tables themselves
only need the five columns above.

Each barcode may occur exactly once across the complete named list. A
duplicate row would be counted as another cell by clone-size and
diversity analyses; reusing one barcode in two list entries would also
assign one cell to two samples. Both are export errors rather than
warnings. Zero-row entries are treated as absent.

When extracting these columns from `meta.data`, an explicit `sample_col`
must exist. Use `sample_col = NULL` only when you want CerebroNexus to
auto-detect `orig.ident`, `sample`, or `Sample`.

### How the repertoire is stored

A `.crb` holds one unified repertoire: a named list with one table per
sample, with the TCR and BCR rows of a sample in the same table. The
Cerebro object has no separate `tcr_data` or `bcr_data` fields;
`getTCR()` and `getBCR()` derive their tables from the repertoire (rows
whose `CTgene` names a `TR[ABGD]` or `IG[HKL]` chain, samples without
such rows dropped), and `getImmuneRepertoire()` returns the whole list.
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
still accepts Seurat objects that keep receptors in
`object@misc$tcr_data` and `object@misc$bcr_data` and merges them into
the unified repertoire, so older preparation workflows keep working. A
`.crb` from an older release that stored receptors in those fields is a
retired format; convert it once with
[`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md).

A very large repertoire is kept out of the `.crb`.
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
and
[`saveCerebro()`](https://mihem.github.io/CerebroNexus/reference/saveCerebro.md)
write it to a sibling file `<stem>.immune.qs2` (`.rds` with
`codec = "rds"`) and record its name and checksum in the `.crb`, so the
Viewer lists samples and chains without reading it and loads the tables
when the Immune repertoire page needs them. The `immune_sidecar`
argument controls this:

- `"auto"` (default): externalise only for H5-backed
  (`expression_matrix_mode = "h5"`) CRBs whose repertoire has at least
  `immune_sidecar_threshold` rows (default `100000L`);
- `"always"`: externalise for any expression storage;
- `"never"`: keep the repertoire inside the `.crb`.

``` r
exportFromSeurat(
  seurat_object,
  file = "my_data.crb",
  experiment_name = "my_experiment",
  organism = "hg",
  groups = c("sample", "seurat_clusters"),
  expression_matrix_mode = "h5",
  immune_sidecar = "always"
)
```

The sidecar must travel with the `.crb`: keep the two files in one
directory when moving them, and
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
copies the sidecar next to the bundled `.crb` automatically.

## Module interface

### Settings panel

The settings panel provides three controls:

- **Clone call**: which clonotype identifier to use for analysis
  (`gene`, `nt`, `aa`, or `strict`)
- **Group by**: a metadata grouping variable for faceted comparisons
- **Chain**: filter to TCR chains (TRA, TRB, TRG, TRD), BCR chains (IGH,
  IGK, IGL), or all (`both`)

When multiple samples are present, additional controls appear for paired
scatter and multi-sample comparison.

### Visualization tabs

Each tab includes a contextual help panel explaining the biological
interpretation with example guidance.

#### Basic repertoire metrics

- **Clonal projection**: overlays clone-expansion level
  (Single/Small/Medium/Large/Hyperexpanded) on the cell projection,
  reusing the dataset’s UMAP/tSNE coordinates; a Receptor (TCR/BCR) and
  Projection selector drive it
- **Abundance**: ranks clonotypes by cell count; steep drop-off
  indicates oligoclonal dominance
- **Diversity**: Shannon entropy with bootstrap confidence intervals
- **Homeostasis**: categorizes clonotypes into size classes from Rare to
  Hyperexpanded
- **Isotype**: BCR isotype (IgM/IgD/IgG/IgA/IgE) composition per group

#### Comparison and sharing

- **Paired Scatter**: clone frequency comparison between two groups or
  paired sample-level metadata levels
- **Clone Sharing**: classifies each clonotype as Private (in a single
  unit), Public within-group, or Public cross-group, using a
  configurable sharing unit (default `sample`); with no group selected
  it degrades to Private / Shared

When \>= 2 samples are present, additional tabs become available: -
**Compare**: alluvial diagram tracking clonotypes across samples -
**SizeDist**: hierarchical clustering of samples by clone size
distribution
