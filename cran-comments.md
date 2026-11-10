## Submission

This is a new submission of CerebroNexus 8.0.0.

CerebroNexus extends the 'Cerebro' viewer (Hillje et al. 2020,
<doi:10.1093/bioinformatics/btz877>) to coordinated views of gene expression,
immune repertoires, trajectories, spatial images and HLA-TCR motifs.

## Test environments

`R CMD check --as-cran` on the built tarball, with `_R_CHECK_LIMIT_CORES_=true`:

* Windows 11 x64 (local), R 4.6.1 (ucrt), Rtools 4.5, including the PDF manual
* Ubuntu (WSL, local), R 4.6.1, also with `_R_CHECK_DEPENDS_ONLY_=true`
  (suggested packages not available)
* Ubuntu (GitHub Actions), R 4.6.1
* R-hub, R-devel (2026-11-09 r90865): Linux, Windows and macOS arm64
* win-builder, R-devel: TO RUN on the final tarball (`devtools::check_win_devel()`)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Reverse dependencies

None (new package).

## Size and data

* The tarball is about 6 MB, below the 10 MB guideline. About 2.6 MB is R code
  and the Viewer and Builder apps; the rest is example data used by the
  examples, tests and the Builder (the largest are the Viewer's gene ID tables,
  1.3 MB) and small AnnData test fixtures (0.5 MB). Large demo data sets and two
  Builder examples are not shipped: `downloadDemoData()` downloads them on
  request into `tools::R_user_dir()` and checks size and MD5. Test files that
  are skipped as a whole on CRAN are not shipped either.
* The source and license of every shipped data set are recorded in
  `data-raw/DATASETS.md` in the source repository.
* Nothing is written outside `tempdir()` during examples and tests.

## Examples

* Every exported function has a runnable example. Examples that need a
  suggested package check that it is installed; those that take more than a
  few seconds (loading 'SingleCellExperiment' or 'Seurat') are in `\donttest{}`.
* `extractMonocleTrajectory()` keeps `\dontrun{}`: it needs a monocle (version
  2) CellDataSet, and monocle is not on CRAN.

## Tests

* Under CRAN settings the slow Builder integration tests and the tests that need
  a browser, Node.js, Chrome, a server or a subprocess are skipped with
  `skip_on_cran()`. Core tests (CRB I/O, conversions, AnnData `.h5ad` and
  `.h5mu` fixtures) run; the tests that need the Zarr store fixtures cannot ship
  them (tarball paths over 100 bytes) and run from a git checkout. The full suite
  runs on GitHub Actions.
