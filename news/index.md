# Changelog

## cerebroAppLite 1.7.7

### Multiple data sets (multi-crb)

- **Dataset switcher**:
  [`createShinyApp()`](https://mihem.github.io/cerebroAppLite/reference/createShinyApp.md)
  now accepts a named vector of several `.crb` files and renders a
  “Select dataset:” dropdown in the sidebar, letting users move between
  data sets without restarting the app. Single-file usage is unchanged
  and shows no switcher. By default the smallest file is loaded first
  (`crb_pick_smallest_file`, default `TRUE`).
- **URL selection**: a data set can be opened directly via the URL,
  matched by the name given in `cerebro_data` or by file basename —
  either as a query string (`?dataset=TCR`) or as the last path segment
  (`/TCR`).
- **Demo data sets**: three genuinely distinct demo `.crb` files ship in
  `inst/extdata/v1.4/` — `demo_full_tcr_bcr.crb` (all cells, TCR + BCR),
  `demo_healthy_t.crb` (T + monocytes, TCR) and `demo_bcell_rich.crb`
  (B-cell rich, BCR). They differ in cell composition, so the UMAP and
  cell-type mix change as you switch, and clonotypes are assigned by
  lineage (TCR to T cells, BCR to B cells) rather than at random.
  Group-level analyses (marker genes, most-expressed genes, enriched
  pathways) are filtered to the cell types kept in each subset, so the
  demos are internally consistent. Built from the public 10x Genomics
  `vdj_v1_hs_pbmc3` dataset; see `data-raw/README.md` for the
  reproducible build. The bundled app (`shiny::runApp("inst")`) now
  opens on these three data sets so the switcher is visible out of the
  box; pass a named vector to
  [`createShinyApp()`](https://mihem.github.io/cerebroAppLite/reference/createShinyApp.md)
  for your own data (see
  [`vignette("multi_crb")`](https://mihem.github.io/cerebroAppLite/articles/multi_crb.md)).
  New vignette: *Loading multiple data sets (multi-crb) with a dataset
  switcher*.

### Immune repertoire

- **Clonal UMAP** no longer renders blank when the Immune repertoire tab
  is opened after visiting another tab (e.g. Main). The plotly renderer
  was gated on server-reported plot dimensions, which are not yet
  available when its output element is created on tab switch; plotly
  sizes itself client-side, so that gate was removed.

## cerebroAppLite 1.7.6

### Immune repertoire

- **Clone Sharing tab**: classifies every clonotype (V+J+CDR3 of the
  active chain) as Private (in a single unit), Public within-group, or
  Public cross-group, using a configurable “sharing unit” (any
  categorical metadata column, default `sample`) and the active group
  column. With no group selected it degrades to Private / Shared.
  Interactive plotly bars with on-bar count/percentage labels and a
  clean hover tooltip (one class per bar, no raw aesthetic names).
- **Definition** (clone-definition resolution waterfall) is available
  but hidden from the default tab strip: it is an exploratory check for
  choosing a clone-call resolution rather than a reader-facing figure.
  Uncomment its `tabPanel` to re-enable.

## cerebroAppLite 1.7.5

### Immune repertoire

- **Clonal UMAP**: axes now match the main projection style —
  `UMAP_1`/`UMAP_2` titles removed, with boxed/mirrored axes (showline +
  mirror) and autorange, so the Clonal UMAP sits visually consistent
  with the main projection tab.

## cerebroAppLite 1.7.4

### Immune repertoire

- **Clonal UMAP**: new first tab overlaying clone-expansion level
  (Single/Small/Medium/Large/Hyperexpanded) on the existing cell
  projection, reusing the dataset’s UMAP/tSNE coordinates. A Receptor
  selector (TCR/BCR, only the classes present in the data) and a
  Projection selector drive it. A “Show all cells” option (on by
  default) draws cells without the selected receptor as a grey
  background, so expanded clones are shown in context. Group filters
  subset which cells appear by any metadata column.
- **Generic display options**: font size and title for every IR plot,
  plus point size and opacity for the scatter-type plots (Clonal UMAP,
  Scatter), in an “Additional parameters” box. Changing them re-renders
  the plot.
- **Reworked layout**: the immune repertoire page now uses the same
  left-parameters / right-visualization layout as the main projection
  tab, with Main parameters, Additional parameters, and Group filters
  boxes on the left.
- **Parameter help**: the info button on each parameter box opens a
  dialog explaining, in plain language, exactly the controls shown on
  the current tab.
- Clone call is no longer shown on the Clonal UMAP tab, where it only
  adds noise.
- **More scRepertoire parameters wired up**: a generic “Order groups”
  control (Default / Alphanumeric) now reaches every plot whose
  scRepertoire function accepts `order.by`, and clonalHomeostasis gains
  a “Clone size thresholds” control (`cloneSize`). Both previously had
  no UI and were never passed.
- **CDR3 length is now faceted, not overlaid.** The Length tab
  previously passed `group.by` straight to
  [`scRepertoire::clonalLength`](https://www.borch.dev/uploads/scRepertoire/reference/clonalLength.html),
  which draws every group as coloured bars in a single panel. It now
  takes that function’s export table and redraws it with `facet_wrap`,
  so each group (sample, or the chosen metadata column’s levels) gets
  its own length-distribution panel on a shared axis — “Group results
  by: sample” produces one plot per sample instead of a single mixed
  plot.
- **Grouping unified on a single control.** The separate “Comparison
  units” selector has been removed from every tab: it re-split the
  repertoire list, which only duplicated — with a narrower, sample-only
  column set — what scRepertoire’s own `group.by` already does (it
  rbinds the list and re-splits on the chosen column). Comparison units
  are now defined solely by “Group results by” (“Compare by” on Scatter
  / Compare / Paired Scatter): None compares the loaded samples; a
  metadata column compares that column’s levels. This removes the case
  where setting one control had no visible effect because the other
  already expressed the same split.
- **Unified plot heights**: the immune repertoire tabs now share a
  single plot height (`ir_fill_plot` / `ir_fill_wrap` helpers) instead
  of repeating a per-tab pixel value.

## cerebroAppLite 1.7.3

### Immune repertoire

- **Immune repertoire**: new conditional tab for TCR/BCR clonotype
  analysis with 19 visualization methods driven by `scRepertoire`,
  covering clonal abundance, diversity, homeostasis, CDR3
  length/composition, V(D)J gene usage, k-mer motifs, and cross-sample
  comparison. Each method includes contextual help with biological
  interpretation guidance.
- **Sample splitting**: a sample-column dropdown lets users re-split the
  repertoire by any shared metadata column; all visualizations recompute
  against the chosen grouping instead of a fixed sample field.
- New utility wrapper: `getImmuneRepertoire()`.
- The bundled `example.crb` now carries real 10x immune-repertoire data
  (`sc5p_v2_hs_PBMC_10k`, 5’ gene expression + TCR + BCR from the same
  experiment), so the immune repertoire tab — including TCR, BCR
  (isotype/SHM), and cross-sample comparisons — works out of the box in
  a single combined dataset. (This single 10x donor is randomly
  partitioned into three demo samples so that cross-sample features have
  data; the sample labels do not represent distinct biological donors.)
- Immune repertoire grouping/splitting now works for **any** metadata
  column (sample, condition, cell type, …): grouping variables are taken
  from the data set’s metadata and joined onto the clonotype data by
  barcode, rather than only columns embedded in the IR table.

## cerebroAppLite 1.7.2

### Enhanced modules

- Added a Most expressed genes tab for exploring per-group gene
  expression summaries exported with `.crb` files.
- Added an Enriched pathways tab for browsing pathway enrichment
  results.
- Added an Extra material tab for exported tables and plots.
- Added utility wrappers for exporting most expressed genes, enriched
  pathways, extra tables, and extra plots.
- Added tests and vignettes covering the new Shiny modules and export
  helpers.

## cerebroAppLite 1.7.1

This maintenance release cleans up the package surface introduced by the
previous releases and refreshes documentation for the current codebase.

### Maintenance

- Removed unused internal Shiny sidebar/menu helpers and orphaned
  utility wrapper functions.
- Cleaned stale roxygen comments, generated Rd files, README wording,
  and internal comments so they describe the current package from its
  own perspective.
- Updated package metadata and regenerated documentation for the current
  public API.

## cerebroAppLite 1.7.0

### New features

- External HDF5 expression backend, symmetric to the bpcells backend:
  [`exportFromSeurat()`](https://mihem.github.io/cerebroAppLite/reference/exportFromSeurat.md)
  with `expression_matrix_mode = "h5"` writes the matrix via
  [`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html)
  to a TENx-format `.h5` next to the `.crb`. The runtime attach loads it
  back as a lazy
  [`HDF5Array::TENxMatrix`](https://rdrr.io/pkg/HDF5Array/man/TENxMatrix-class.html)
  seed and transposes via `DelayedArray::t()` (free); the in-memory
  `dgCMatrix` is never materialised, so RAM stays close to the `.crb`
  metadata size and attach is effectively instant
- Introduced
  [`createShinyApp()`](https://mihem.github.io/cerebroAppLite/reference/createShinyApp.md)
  for bundling a self-contained Shiny app from one or more `.crb` files
- [`createShinyApp()`](https://mihem.github.io/cerebroAppLite/reference/createShinyApp.md)
  now copies the `<stem>.h5` sibling alongside the `.crb` during app
  bundling, mirroring the existing `.bpcells/` handling
- Legacy `.crb` files (predating the `expression_backend` field) are
  auto-tagged as `h5` when the host app sets
  `Cerebro.options[["expression_matrix_h5"]]`, finally giving
  `inst/extdata/v1.4/example.h5` a runtime consumer
- [`convertSeuratToCerebro()`](https://mihem.github.io/cerebroAppLite/reference/convertSeuratToCerebro.md)
  accepts an in-memory Seurat object alongside the `.rds` path; output
  basename derives from `experiment_name` when no path is given
- [`createShinyApp()`](https://mihem.github.io/cerebroAppLite/reference/createShinyApp.md)
  opens a `...` passthrough so callers can forward extra options without
  signature churn
- [`exportFromSeurat()`](https://mihem.github.io/cerebroAppLite/reference/exportFromSeurat.md)
  with `expression_matrix_mode = "bpcells"` now auto-detects
  losslessly-integer values and calls
  `BPCells::convert_matrix_type("uint32_t")` before
  `write_matrix_dir()`, which triggers BPCells’s bit-packed integer
  storage on the typical scRNA-seq counts case. Shrinks the bpcells
  sibling ~5× on integer counts (e.g. 50k cells × 20k genes: 440 MB raw
  double → 78 MB bit-packed; PBMC All Samples 38,606 × 147,756: 2.6 GB →
  549 MB), and queries get ~1.5-1.7× faster as a side effect (smaller
  payload to read). Normalised float values
  (`slot = "data"`/`"scale.data"`) fall back to raw double to avoid
  silent precision loss

### Bug fixes

- Fixed
  [`exportFromSCE()`](https://mihem.github.io/cerebroAppLite/reference/exportFromSCE.md)
  projections: `reducedDims()` output is now coerced to `data.frame`
  before `addProjection()`, matching the Seurat path and clearing a
  latent runtime error for SCE inputs with non-PCA reductions
- Fixed `.attachExternalExpression` crashing on legacy `.crb` objects
  that predate the `getExpressionBackend()` method; such objects are now
  treated as embedded backend and skip the attach step
- Fixed “method not found” errors on the trajectory tab by renaming the
  corresponding `Cerebro_v1.3` methods to the names the Shiny server
  already calls (`getMethodsForTrajectories`, `getNamesOfTrajectories`)
- Fixed gene_expression plot chain freezing on gene picker changes:
  removed a stale
  [`isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html) wrapper and
  a reference to a non-existent `expression_projection_update_button`
  input; the existing 250 ms debounce on the data-to-plot reactive still
  throttles bursts

### Testing

- Extended testing
- Added an h5 round-trip test in `test-exportFromSeurat.R` verifying
  writer/reader bit-identity for the new HDF5 backend, plus an
  attach-level test asserting the runtime returns a lazy `DelayedMatrix`
  (not an in-memory `dgCMatrix`)
- Added `tests/README.md` documenting the layout (testthat unit,
  testthat shinytest2, smoke)

### Dependencies

- `rhdf5` removed from `Suggests`. The h5 backend now goes through
  [`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html)
  (writer) and
  [`HDF5Array::TENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/TENxMatrix-class.html)
  (lazy reader), which use rhdf5 internally; users no longer need to
  install or `requireNamespace` rhdf5 directly

### CI/CD

- Switched Nix environment to `fixed-date` to avoid constant rebuilding
- simplified workflow by removing `dev` and `sync-dev`

## cerebroAppLite 1.6.0

### Bug fixes

- Fixed all errors and warnings identified by R CMD CHECK, making the
  package ready for CRAN submission
- Fixed Seurat v5 API: replaced deprecated slot access (`@counts`,
  `@data`) with
  [`GetAssayData()`](https://satijalab.github.io/seurat-object/reference/AssayData.html)
  across multiple functions
- Fixed `addPercentMtRibo`, `calculatePercentGenes`,
  `getMostExpressedGenes`, `performGeneSetEnrichmentAnalysis`,
  `getMarkerGenes`, `getEnrichedPathways`, `exportFromSCE`,
  `exportFromSeurat`
- Fixed GSVA v2.x API compatibility: now uses `gsvaParam()` with version
  check for backward compatibility
- Fixed `class(x) == "..."` checks replaced with
  [`inherits()`](https://rdrr.io/r/base/class.html) across all relevant
  functions
- Fixed [`require()`](https://rdrr.io/r/base/library.html) replaced with
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) throughout
- Fixed cross-references and examples in documentation

### Testing

- Added unit tests for all core R functions
- Added shinytest2 integration tests for the full Cerebro interface,
  covering gene expression, group/marker genes, color management, and
  more
- Tests run in a reproducible Nix environment via GitHub Actions

### CI/CD

- Added Nix-based GitHub Actions workflows for R CMD CHECK, R tests,
  pkgdown, code style, and automatic `default.nix` updates
- Added `update-nix` workflow: regenerates `default.nix` weekly via
  `create_env.R` and opens a PR automatically; BPCells commit SHA is now
  auto-fetched from GitHub
- Added `style` workflow: formats R code via
  [air](https://github.com/posit-dev/air) on every PR
- Added `sync-dev` workflow: automatically merges master back into dev
  after every merge to keep branches in sync
- Switched Nix environment to `bleeding-edge` for always up-to-date CRAN
  packages
- Branch protection rulesets configured for both `master` and `dev` with
  required status checks and auto-merge

### Documentation

- Added pkgdown site at <https://mihem.github.io/cerebroAppLite/> with
  light/dark/auto theme switch, search, and all vignettes as articles
- Site automatically builds and deploys to GitHub Pages on push to
  master

## cerebroAppLite 1.5.3

- several bug fixes so that launchCerebro should work again

## cerebroAppLite 1.5.2

- allow plot settings (size, opacity, number of cells to show) to be
  different in gene expression and overview (useful for large datasets
  with slow gene expression)

## cerebroAppLite 1.5.1

- remove unused functions in group

## cerebroAppLite 1.5.0

- make compatible with Seuratv5, especially with BPCells Matrix

## cerebroAppLite 1.4.1

- timeout function added. This logs out the user after 600 second of
  inactivity (can be changed in `shiny_ui.R`). The JS function was taken
  from <https://stackoverflow.com/a/53207050/21417317>.
- add option to show up to 1000 cells in `Main`, which is useful for
  exports.

## cerebroAppLite 1.4.0

This is the first update of this cerebroApp fork. Its aim is to continue
a lightweight version of the excellent cerebroApp with only the main
function as the cerebroApp by Roman Hillje is sadly discontinued.

### Major changes

- remove enriched pathways, extra material, most expressed genes and
  trajectory functions since the goal of this fork is to continue with a
  lightweight version

### Minor changes

- `Load Data` is renamed to `Data info` and `Overview` to `Main`
- Preferences about WebGL and hover info are now show in the first tab
  called `Data info`
- more colorful boxes for the sample information
- different icons for tabs `Data info`, `Main`, `Groups` and
  `Marker Groups`
