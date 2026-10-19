# Running tests locally

This directory holds the package's automated tests. `R CMD check` and CI (`.github/workflows/R-tests.yaml`, `R-cmd-check.yaml`) run these on every PR.

## Layout

```
tests/
├── testthat.R             # entry point picked up by R CMD check
├── testthat/              # actual unit + shinytest2 suite (committed)
│   ├── setup.R            # browser timeouts; respects the caller's CRAN settings
│   ├── helper-portability.R # Node/Chrome skips and a Windows-safe system2(env=)
│   ├── test-app-inst.R    # shinytest2 end-to-end smoke tests against inst/
│   ├── test-exportFromSeurat.R
│   └── test-r-functions.R
└── smoke/                 # local dev scratch (gitignored, see its own README)
```

The testthat suite is included in package builds. Benchmark scripts, Node modules
and local browser artifacts are excluded. `tests/smoke/` is a local sandbox for
hand-driven smoke tests; it does not run under `R CMD check`.

## Prerequisites

R `>= 3.5.1` and the package's runtime deps installed. For the full test suite you also need:

| Package | Why |
|---|---|
| `testthat (>= 3.0.0)` | core framework |
| `shinytest2 (>= 0.2.0)` | drives the Shiny app in `test-app-inst.R` |
| `chromote` | transitive dep of `shinytest2`; controls headless Chrome |
| Google Chrome / Chromium | actual browser binary `chromote` launches |
| Node.js | execute complete JavaScript scripts using built-in modules only |

For the full development suite, set `NOT_CRAN=true`. No npm installation is
needed. Node tests load complete scripts and exercise registered messages and
public APIs. Canvas and interaction tests use Chrome with the actual Viewer UI;
only external boundaries (Shiny, downloads and GPU submission) are instrumented.
The separate renderer fixture exercises actual WebGPU and WebGL2 upload logic.

Ordinary package checks leave `NOT_CRAN` unset: heavy browser tests skip, while
lightweight R/Shiny and available Node tests run. The R-CMD-check workflow builds
the source tarball and checks it outside the checkout, without `NODE_PATH`,
`NOT_CRAN` or `CI`. Node's global module search is disabled as well.

The suite uses testthat's third edition and runs test files in parallel
(`Config/testthat/parallel` in `DESCRIPTION`). Parallelism applies when testthat
loads the package itself: `devtools::test()`, `testthat::test_local()` and
`R CMD check`, but not `testthat::test_dir()` after a manual `load_all()`. Set
`TESTTHAT_CPUS` to choose the number of workers (testthat defaults to two; CI
uses four). Each file runs in one worker, so a file's shared AppDriver and
fixtures stay within that file. The slowest browser files are listed in
`Config/testthat/start-first` so they do not finish last.

On Linux/WSL, reproduce both modes from the repository root:

```bash
NOT_CRAN=true TESTTHAT_CPUS=4 Rscript -e \
  'testthat::test_local(".", stop_on_failure=TRUE)'

source_dir="$PWD"
check_dir=$(mktemp -d)
cd "$check_dir"
R CMD build "$source_dir"
env -u CI -u NOT_CRAN -u NODE_PATH NODE_OPTIONS=--no-global-search-paths \
  R CMD check --as-cran --no-manual CerebroNexus_*.tar.gz
```

`--no-manual` omits the PDF manual (which requires TeX); tests and HTML vignettes
still run. This portability check does not establish complete CRAN readiness.
To run the full suite against an installed package, opt in with `NOT_CRAN=true`.

The test packages are declared in `DESCRIPTION` `Suggests`. To install them:

```r
install.packages(c("testthat", "shinytest2", "chromote"))
# or pull the whole Suggests list:
devtools::install_dev_deps()
```

Verify Chrome is discoverable:

```r
chromote::find_chrome()
# Override if needed:
Sys.setenv(CHROMOTE_CHROME = "/path/to/Chromium")
```

## Running tests

Viewer UI regressions are organized by behavior: `test-viewer-ui-components.R`
checks generated controls, `test-app-visualization-layout.R` exercises all eight
visualization pages, and `test-app-viewport-layout.R` covers sharing, selection,
dragging and responsive interaction. IR's grouped/ungrouped sharing checks live
in `test-app-immune_repertoire.R`; missing-gene feedback lives in
`test-gene-validation.R`. Test observable output and interaction rather than R,
JavaScript or CSS source spelling. Keep shared-component coverage in one place.

`test-hla-consumer-lifecycle.R` and `test-trekker.R` execute page/subtab
transitions to check that hidden consumers stay idle. `test-trajectory-data-flow.R`
checks shared row mapping and deferred controls. `test-dataset-loading-progress.R`
checks server progress and readiness messages; the visualization browser test
also exercises the visible loading overlay. `test-ren-backend-benchmark.R` runs
the real command-line workers against tiny CRBs, including deliberately changed
values, dimensions and backend expectations. It tests correctness, not speed.

`test-hla-allele-state.R` compares actual network colours and overlap counts
after changing either allele picker. The browser layout test also checks
selection persistence across HLA tabs and drawer geometry at both widths.
`test-app-dataset-lifecycle.R` switches real datasets A/B/A, injects late frames,
holds and releases the reconnect handshake, and verifies recovery. Its current
frame positive control ensures rejected messages are not merely ignored by a
broken renderer. Trajectory tests change cell order, annotations and values,
then switch back to verify both cache invalidation and correct output.

Set `CEREBRO_LAYOUT_ARTIFACTS` to an output directory when running
`app-visualization-layout` to save screenshots and measured geometry for visual
review. The test waits for geometry to settle; it does not compare entire HTML
or freeze incidental pixel values into snapshots.
Paths into `inst/` live in `helper-paths.R`; shared file reads and AppDriver
helpers in `helper-viewer-driver.R`, Node runners in
`helper-viewer-execution.R`. Drawer
accessibility, reduced motion and panel placement are checked on rendered output.

`helper-viewer-server.R` loads real R declarations and subscriptions into small
Shiny sessions; assertions check their emitted values, invalidation and deferred
work. JavaScript fixtures load complete scripts through Node or Chrome,
independently of whitespace, comments or quote style. Neither helper asserts
that a production function has a particular body or sequence of statements.

Coverage includes actual HLA graph/table output, IR renderer arguments and cache
invalidation, Spatial RGB/hulls/backgrounds, first-frame and supplement transport,
stale dataset messages, Canvas geometry and PNG exports, login layout, benchmark
readiness/byte counters and shell failure cleanup. Expensive atlas downloads and
timing thresholds remain opt-in; small deterministic inputs exercise correctness.
Retained text checks concern published documentation, metadata, private output
contents and parity of bundled core files, rather than implementation spelling.

From the package root.

**Interactive R session (RStudio / VS Code R console / `R`)**:

```r
# Full suite (loads dev source via load_all)
devtools::test()

# A single file (matches tests/testthat/test-<filter>*.R)
devtools::test(filter = "app-inst")
devtools::test(filter = "exportFromSeurat")

# Just the currently open file
devtools::test_active_file()
```

**From the shell (CI / scripting / one-shot runs)**:

```bash
# Same as devtools::test() above, but driven from a shell prompt
Rscript -e 'Sys.setenv(NOT_CRAN="true"); devtools::test(filter="app-inst", reporter="summary")'

# Whole suite, default reporter
Rscript -e 'devtools::test()'

# What R CMD check effectively runs (uses installed package, not dev source)
Rscript -e 'testthat::test_dir("tests/testthat")'
```

Why the extra bits in that first shell one-liner:

- `Sys.setenv(NOT_CRAN="true")` — tells `skip_on_cran()` we are not on CRAN, so shinytest2 tests do not get skipped. The full CI workflow sets this explicitly; `tests/testthat/setup.R` preserves the caller’s environment.
- `filter="app-inst"` — only run `tests/testthat/test-app-inst.R`. Skips the non-shinytest2 files. Useful when debugging the Shiny end-to-end suite.
- `reporter="summary"` — one line per `test_that()` plus full diff on failure. Other useful options: `"minimal"` (compact `.EFWS` stream), `"progress"` (default), `"check"` (matches `R CMD check`).

Inside an `R CMD check` run, the suite is invoked through `tests/testthat.R`, which is just:

```r
library(testthat)
library(CerebroNexus)
test_check("CerebroNexus")
```

### Picking the right entry point

| Entry point | Requires the package to be installed? | `R/` source loaded from | `inst/` files read from |
|---|---|---|---|
| `devtools::test()` | no | dev source (via `pkgload::load_all`) | local `inst/` (`system.file()` is redirected by `load_all`) |
| `Rscript tests/testthat.R` | yes (`library(CerebroNexus)` aborts otherwise) | installed package | installed package |
| `testthat::test_dir("tests/testthat")` | yes (same reason) | installed package | installed package |
| `R CMD check` / CI | self-installs into a temp library | that temp install | that temp install |

While iterating on the package, use `devtools::test()` — it picks up edits to both `R/` and `inst/` immediately, no reinstall needed. Run `R CMD check` (or `testthat::test_dir()` after `devtools::install()`) before opening a PR to verify what CI will see.

## Pitfalls and FAQ

### Linux authentication deployment permissions

`test-viewer-auth-runtime.R` covers secret source precedence, content and
file-type errors, and the exact POSIX `0600` requirement. POSIX checks are
explicitly skipped on Windows; fault-injected read failures are not presented
as cross-UID integration coverage.

See the [authentication guide](../vignettes/control_access_to_cerebro_with_a_login_page.Rmd)
for deployment and Windows-to-Linux transfer limitations. Generated Apps retain
a brief offline checklist.


### How the app directory is resolved in browser tests

Browser tests call `viewer_app_test_path()` (`helper-paths.R`): the source
tree's `inst/` when it exists, otherwise the installed package root. Files whose
tests only navigate the default data set share one AppDriver through
`viewer_shared_app()` (`helper-viewer-driver.R`); a test that asserts pristine startup state or switches
data sets boots its own.

If you edit `inst/` and want the change reflected:

- Under `devtools::test()` — nothing to do, it reads `inst/` directly.
- Under `testthat::test_dir()` / `R CMD check` — reinstall:
  ```r
  devtools::install(".", quick = TRUE, upgrade = "never")
  ```

### Stale `inst/extdata/examples/example.crb` after R6 class changes

`example.crb` is a serialized `Cerebro` R6 object. R6 method tables are baked into the object at serialization time. If a method is added or renamed on the class definition under `R/`, an older `example.crb` will not have that method, and calling it from the Shiny app yields:

```
Error in <fn>: attempt to apply non-function
```

The Shiny session never reaches idle, and shinytest2 reports `Shiny app did not become stable in 15000ms`.

Whenever you add or rename methods on `Cerebro`, regenerate the example fixture by re-exporting from `inst/extdata/examples/pbmc_seurat.rds` with `exportFromSeurat()`, then commit the new `example.crb`.

### shinytest2 needs `NOT_CRAN=true`

`tests/testthat/setup.R` sets this so shinytest2 tests are not skipped by `skip_on_cran()`. When running tests outside `devtools::test()` (e.g. directly via `Rscript`), make sure the env var is set:

```bash
NOT_CRAN=true Rscript -e 'testthat::test_dir("tests/testthat", filter = "app-inst")'
```

### chromote leaves zombie processes after Ctrl-C

If you interrupt a shinytest2 run mid-flight, the headless Chrome and the spawned Shiny process can survive:

```bash
pkill -f chromote
pkill -f 'shiny::runApp'
```

---
