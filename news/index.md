# Changelog

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
