## Submission

This is a new submission of CerebroNexus 8.0.0.

CerebroNexus extends the 'Cerebro' viewer (Hillje et al. 2020,
<doi:10.1093/bioinformatics/btz877>) to coordinated views of gene expression,
immune repertoires, trajectories, spatial images and HLA-TCR motifs.

## Test environments

`R CMD check --as-cran` on the built tarball, with `_R_CHECK_LIMIT_CORES_=true`:

* macOS (local), R 4.6.2
* Windows 11 x64 (local), R 4.6.1 (ucrt), Rtools 4.5, including the PDF manual
* Ubuntu (WSL, local), R 4.6.1
* Ubuntu (GitHub Actions), R 4.6.1
* R-devel: to be added (win-builder)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

The incoming check also lists https://mihem.github.io/CerebroNexus/ as moved
(301) to https://www.mheming.com/CerebroNexus/. The first is the package's
documented address and redirects to the same site.

Notes that depend on the machine and are not part of the package:

* Packages suggested but not available for checking ('anndataR', 'arrow'):
  both are optional and the package works without them.

## Size and data

* The tarball is 5.7 MB. About 2.6 MB is R code, the Viewer and Builder apps and
  tests; the rest is example data that the examples, tests and the Builder's
  example need (the largest are the Viewer's gene ID tables, 1.3 MB). The
  large demo data sets and two Builder examples are not shipped:
  `downloadDemoData()` downloads them on request into `tools::R_user_dir()` and
  checks size and MD5.
* Nothing is written outside `tempdir()` during examples and tests.

## Examples

* Every exported function has a runnable example. Examples that need a
  suggested package are guarded with `@examplesIf`; those that take more than a
  few seconds (loading 'SingleCellExperiment' or 'Seurat') are in `\donttest{}`.
* `extractMonocleTrajectory()` keeps `\dontrun{}`: it needs a monocle (version
  2) CellDataSet, and monocle is not on CRAN.

## Tests

* Under CRAN settings the slow Builder integration tests and the tests that need
  a browser, Node.js, Chrome, a server or a subprocess are skipped with
  `skip_on_cran()`. The full suite runs on GitHub Actions.
