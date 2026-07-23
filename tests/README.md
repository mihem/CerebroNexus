# Running tests locally

This directory holds the package's automated tests. `R CMD check` and CI (`.github/workflows/R-tests.yaml`, `R-cmd-check.yaml`) run these on every PR.

## Layout

```
tests/
├── testthat.R             # entry point picked up by R CMD check
├── testthat/              # actual unit + shinytest2 suite (committed)
│   ├── setup.R            # sets NOT_CRAN=true so shinytest2 tests are not skipped
│   ├── test-app-inst.R    # shinytest2 end-to-end smoke tests against inst/
│   ├── test-exportFromSeurat.R
│   ├── test-r-functions.R
│   └── _snaps/            # testthat snapshot fixtures
└── smoke/                 # local dev scratch (gitignored, see its own README)
```

Only `tests/testthat/` is part of the package. `tests/smoke/` is a local sandbox for hand-driven smoke tests; it does not run under `R CMD check`.

## Prerequisites

R `>= 3.5.1` and the package's runtime deps installed. For the full test suite you also need:

| Package | Why |
|---|---|
| `testthat (>= 3.0.0)` | core framework |
| `shinytest2 (>= 0.2.0)` | drives the Shiny app in `test-app-inst.R` |
| `chromote` | transitive dep of `shinytest2`; controls headless Chrome |
| Google Chrome / Chromium | actual browser binary `chromote` launches |

Both `testthat` and `shinytest2` are declared in `DESCRIPTION` `Suggests`. To install everything in one shot:

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

- `Sys.setenv(NOT_CRAN="true")` — tells `skip_on_cran()` we are not on CRAN, so shinytest2 tests do not get skipped. `tests/testthat/setup.R` sets the same thing inside the R session, but some hooks fire earlier than setup; setting it at the shell level is the safe belt-and-braces.
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

### How `inst_dir` is resolved in `test-app-inst.R`

```r
inst_dir <- system.file(package = "CerebroNexus")
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}
```

In practice `system.file(package = "CerebroNexus")` always resolves to something with `app.R` at its root:

- Under `devtools::test()`, `pkgload::load_all()` redirects `system.file()` to `<project>/inst/` (the source tree itself acts as the "installed root").
- Under `Rscript tests/testthat.R` or `testthat::test_dir()`, `library(CerebroNexus)` has already loaded the installed package, so `system.file()` returns its install location.

The fallback to `testthat::test_path("../../inst")` is effectively dead code — none of the supported entry points reach it. (`library(CerebroNexus)` aborts before the test file is sourced when the package is not installed; under `devtools::test()` the first branch already succeeds.)

If you edit `inst/` and want the change reflected:

- Under `devtools::test()` — nothing to do, it reads `inst/` directly.
- Under `testthat::test_dir()` / `R CMD check` — reinstall:
  ```r
  devtools::install(".", quick = TRUE, upgrade = "never")
  ```

### Stale `inst/extdata/v1.4/example.crb` after R6 class changes

`example.crb` is a serialized `Cerebro_v1.3` R6 object. R6 method tables are baked into the object at serialization time. If a method is added or renamed on the class definition under `R/`, an older `example.crb` will not have that method, and calling it from the Shiny app yields:

```
Error in <fn>: attempt to apply non-function
```

The Shiny session never reaches idle, and shinytest2 reports `Shiny app did not become stable in 15000ms` (often with a misleading sidekick `the fixed layout requires the slimscroll plugin!` JS warning — that one is harmless AdminLTE noise).

Whenever you add or rename methods on `Cerebro_v1.3`, regenerate the example fixture by re-exporting from `inst/extdata/v1.4/pbmc_seurat.rds` with `exportFromSeurat()`, then commit the new `example.crb`.

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

### Snapshot tests under `_snaps/`

`testthat::expect_snapshot()` writes golden files into `tests/testthat/_snaps/`. Review diffs with:

```r
testthat::snapshot_review()
testthat::snapshot_accept()   # only after you've confirmed the new output is correct
```

---

