## Submission

This is a new submission of CerebroNexus 8.0.0.

CerebroNexus extends the 'Cerebro' viewer (Hillje et al. 2020,
<doi:10.1093/bioinformatics/btz877>) to coordinated views of gene expression,
immune repertoires, trajectories, spatial images and HLA-TCR motifs.

## Test environments

* macOS (local), R 4.6.2: `R CMD check --as-cran` on the built tarball.
* Windows and Linux: to be added before submission.

## R CMD check results

`R CMD check --as-cran` on the tarball reports the "New submission" NOTE.

Notes that depend on the machine and are not part of the package:

* Packages suggested but not available for checking ('anndataR', 'arrow'):
  both are optional and the package works without them.

## Size and data

* The tarball is about 6 MB. The large demo data sets and the Trekker and Visium
  Builder examples are not shipped. `downloadDemoData()` downloads the demo data
  sets on request into `tools::R_user_dir()` and checks size and MD5.
* Nothing is written outside `tempdir()` during examples and tests.

## Tests

* Under CRAN settings the slow Builder integration tests and the tests that need
  a browser, Node.js, Chrome, a server or a subprocess are skipped with
  `skip_on_cran()`. The full suite runs on GitHub Actions.
