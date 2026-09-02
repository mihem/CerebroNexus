# Create a self-contained Shiny app from a Cerebro data file

## Overview

[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
bundles a CerebroNexus Shiny app into a single output directory. It
copies the Shiny sources shipped under `inst/viewer/`, the requested
`.crb` (or `.rds`) data file(s), and the `inst/extdata/` reference
files, and writes an `app.R` that wires everything together with a
pre-built `Cerebro.options` list. The result is a directory you can
serve directly with `shiny-server`, drop behind `rsconnect`/Posit
Connect, or launch locally with
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) — no
further calls into `CerebroNexus` are required at runtime.

The default descriptor-based workflow is self-contained: sibling H5 or
BPCells matrices are copied with the `.crb`. A runtime matrix override
is an explicit host-managed exception. It is not copied, so an app using
one still requires that absolute host path after deployment.

This is the recommended path when you want to hand a colleague a
runnable copy of Cerebro pre-loaded with a specific data set without
making them install the package themselves, pin the Shiny sources at a
known revision alongside the data, or deploy to a host that doesn’t run
R interactively (`shinyapps.io`, Docker images, etc.).

## Setup

The package ships an example `.crb` in `inst/extdata/examples/`, which
we use throughout this vignette.

``` r
library(CerebroNexus)

crb <- system.file("extdata/examples/example.crb", package = "CerebroNexus")
file.exists(crb)
#> [1] TRUE
```

## Quick start

`cerebro_data` must be a **named** character vector (or list). Names
must be non-missing and unique because they become the dataset labels
shown in the Load Data tab; the values are paths to `.crb` (or `.rds`)
files.

``` r
out_dir <- file.path(tempdir(), "cerebro_app")

createShinyApp(
  cerebro_data = c("PBMC example" = crb),
  result_dir   = out_dir,
  launch_browser = FALSE
)
```

After this call, `out_dir` contains the generated `app.R`, a copy of the
Shiny sources under `shiny/`, the `inst/extdata/` reference files, the
data file(s) under `private-data/`, and a `cerebro_config.rds` holding
the serialized `Cerebro.options`. The `private-data/` directory is
private to the R process: the generated app does not register it as an
HTTP resource.

Launch it locally:

``` r
shiny::runApp(out_dir)
```

## Common parameters

| argument | purpose |
|----|----|
| `cerebro_data` | named vector/list of `.crb` (or `.rds`) paths; labels and resolved source files must both be unique |
| `result_dir` | output directory (required; portable basename outside the reserved lock namespace) |
| `overwrite` | replace `result_dir` after a complete staged build; with `FALSE`, the destination must be absent or empty |
| `max_request_size` | finite positive numeric request cap in MB; defaults to `8000`; closed Viewers use at most 6 MiB |
| `port`, `host` | whole-number port `1`-`65535` and a non-empty host string; defaults to `8080` / `127.0.0.1` |
| `launch_browser`, `quiet`, `display_mode` | non-missing logical flags and exactly `auto`, `normal`, or `showcase` for [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) |
| `welcome_message` | text shown in the Load Data tab |
| `colors` | optional `dataset -> variable -> named level colours` list |
| `cerebro_options` | extra entries merged into `Cerebro.options`; matrix overrides must be absolute host paths (native paths must resolve outside `result_dir`) |
| `crb_pick_smallest_file` | forwarded into `Cerebro.options` |
| `show_upload_ui` | allow users to upload their own data; defaults to `FALSE` for generated apps |
| `initial_page` | stable Viewer page ID used once when the first dataset loads |
| `point_size`, `point_opacity`, `percentage_cells_to_show` | scalar or named per-dataset scatter defaults |
| `variable_to_compare` | forwarded into `Cerebro.options` |
| `extra_tables`, `extra_tables_sheets` | external delimited files or Excel workbooks, plus optional displayed sheet names |

When `show_upload_ui = FALSE`, the generated Viewer limits the effective
request size to the smaller of `max_request_size` and 6 MiB. This keeps
a closed deployment from accepting large request bodies even though no
upload control is displayed. Open Viewers use the supplied
`max_request_size`.

## Bundling multiple datasets (available since 2.0.0)

The names of `cerebro_data` are what the user picks from inside Cerebro.
You can also supply matching `colors` so each dataset gets a
deterministic palette.

``` r
crb_pbmc <- system.file("extdata/examples/example.crb", package = "CerebroNexus")
# crb_other <- "/path/to/another_dataset.crb"

createShinyApp(
  cerebro_data = c(
    "PBMC example"   = crb_pbmc
    # , "My dataset" = crb_other
  ),
  result_dir = file.path(tempdir(), "cerebro_app_multi"),
  colors = list(
    "PBMC example" = list(sample = c(pbmc_1 = "#1f77b4"))
  ),
  point_size = c("PBMC example" = 4),
  point_opacity = c("PBMC example" = 0.8),
  percentage_cells_to_show = c("PBMC example" = 75),
  welcome_message = "Welcome to my Cerebro deployment.",
  launch_browser = FALSE
)
```

Each `colors` dataset entry maps metadata variables to named level
colours. The three scatter options may be one value shared by all
datasets or a named numeric vector/list whose names exactly match
`cerebro_data`.

## Initial page

Use `initial_page` with a stable page ID such as `"linked_views"`,
`"extra_material"`, or `"spatial"`:

``` r
createShinyApp(
  cerebro_data = c("PBMC example" = crb),
  result_dir = file.path(tempdir(), "cerebro_app_spatial"),
  initial_page = "spatial",
  launch_browser = FALSE
)
```

Initial routing is a one-time decision for the first loaded dataset. If
that dataset does not support a conditional page, the Viewer stays on
its normal page; switching later to a compatible dataset does not
redirect the user after manual navigation. Restored shared links take
precedence over this default.

## External Extra material tables

`extra_tables` accepts a named collection of CSV, TSV, TXT, XLS, XLSX,
or XLSM files. Excel workbooks expose each non-empty sheet separately.
Use `extra_tables_sheets` to rename selected sheets while retaining
unmapped sheet names:

``` r
createShinyApp(
  cerebro_data = c("PBMC example" = crb),
  result_dir = file.path(tempdir(), "cerebro_app_tables"),
  extra_tables = list(
    "QC summary" = "results/qc.csv",
    "Annotations" = "results/annotations.xlsx"
  ),
  extra_tables_sheets = list(
    "Annotations" = list("Cell labels" = "Sheet1")
  ),
  launch_browser = FALSE
)
```

The build converts non-empty tables to private RDS assets without
recording their source paths. The Viewer first presents the file and
sheet choices, then loads only the selected sheet. Formula-like text is
neutralized and external table cells are HTML-escaped in the Viewer;
downloads use the same neutralized data.

## Sibling files: `.bpcells/` and `.h5`

If your `.crb` was exported with an external expression backend
(`expression_matrix_mode = "bpcells"` or `"h5"` in
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)),
its descriptor records the matrix location relative to the `.crb`. Fresh
exports normally use a sibling `<stem>.bpcells/` directory or
`<stem>.h5` file, but the descriptor remains authoritative if the `.crb`
is renamed.

[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
copies the tagged file or directory to the same relative location under
`result_dir/private-data/`. The location must be a portable relative
path; missing backends and conflicting bundle targets are errors rather
than silent overwrites. Before copying, the function resolves every
descriptor and sidecar and plans all data and spatial-image targets
together. Exact, parent/child, case-folded and symbolic-link collisions
fail during preflight.

Preflight reads the ordinary `expression_backend` field without invoking
its serialized getter. It combines that field with any deployment
override and writes a versioned, per-CRB attachment plan into
`cerebro_config.rds`. Configured CRBs consume that exact plan at runtime
instead of interpreting the serialized object a second time. Direct
launches and uploads also avoid the getter: they validate the ordinary
field at load time. A historical CRB may omit both the field and getter;
an object containing only one of them is an unsupported mixed format and
fails closed.

### Private data and spatial assets

Files explicitly passed through `spatial_images` are copied under
`spatial-assets/`. The server-side renderer reads them from disk and
embeds them as data URIs when drawing the plot; the directory is not
registered as an HTTP resource. Raw `.crb`, H5 and BPCells artifacts
stay under `private-data/`, which is likewise never registered with
`addResourcePath()`. The historical `data/` directory name is
deliberately not reused: a still-running older generated app can retain
its former `/data` resource mapping while the bundle directory is
replaced.

This boundary prevents ordinary HTTP download through the generated
Shiny app; it is not encryption or an operating-system access-control
boundary. Anyone with filesystem access to the deployment account can
still read the files. Missing optional spatial images are omitted with a
warning.

Generated bundles follow the standard Shiny deployment model of one app
per R process. They use process-global `Cerebro.options` and do not
claim isolation between separately sourced apps in the same process.

### Host-managed matrix overrides

The global `expression_matrix_h5` and `expression_matrix_BPCells`
overrides retain precedence over a `.crb` descriptor. An override must
be an absolute path. A path native to the build host is canonicalized
and rejected if it resolves inside `result_dir`. Existing native
components are resolved before an unresolved suffix, so aliases cannot
hide a future path inside the app. Unresolved filesystem entries fail
closed, as do Windows device or extended-length namespaces and
Windows-incompatible segments. A truly absent ordinary target is still
allowed because the deployment host may provision it later.

A non-native absolute path intended for a different host is preserved
lexically; the build cannot compare it with the destination host’s app
tree, so the deployer must verify that relationship. Native Windows
paths use link-aware physical resolution, including mapped-drive and UNC
aliases. The descriptor sidecar is not copied. The deployed app
therefore depends on that exact host path and is not self-contained.
Because an override is global, it may serve only one effective CRB
consumer in a multi-dataset app.

### Single-writer publication and recovery

Each target has one atomic sibling lock named
`.<result-basename>-build.lock`. A second build for the same canonical
target stops immediately; builds for other targets continue
independently. The lock contains PID, host, start time and target
metadata for diagnosis. It covers target inspection, preflight, stage
construction, publication and stage cleanup. Target ancestors are first
resolved to a physical path; the final target cannot be a symbolic link,
junction, unresolved filesystem entry, or reserved-lock alias. The stage
and lock are created beside that frozen target.

The function does not guess whether an existing lock is stale. If no
build is running after a process crash, inspect the lock metadata and
remove that exact lock directory manually before retrying. Do not remove
a lock owned by a live build.

The complete app is assembled in a private sibling stage. Preflight,
copy and configuration-write failures occur before the old deployment
moves, so it remains untouched. During publication, the old app is first
renamed to a unique backup and the completed stage is then renamed into
place. If that final rename fails, restoration is attempted. If
restoration also fails, the backup is kept and the error reports its
exact path; the function does not claim that the old deployment remained
in place. An unexpected destination created by another actor is never
deleted. `overwrite = FALSE` remains conservative: a non-empty or
unreadable destination is rejected rather than merged.

An abrupt process death, host restart or `SIGKILL` between the two
publication renames cannot run the restoration code. In that case
`result_dir` may be temporarily absent while the old app remains in a
sibling backup and the new app remains in its sibling stage. First
confirm that the recorded lock owner is no longer running. Then inspect
the candidates and restore the verified old backup to `result_dir`
before removing any leftover stage and, last, the exact lock. Do not
guess from the newest backup name when older cleanup backups may exist.

On POSIX systems, the staging directory is mode `0700` while data is
copied. Replacing an existing app retains its root permission bits;
platform-specific ACLs, ownership changes and security labels remain the
deployment system’s responsibility. Inputs are validated and then copied
without immutable snapshots. Every CRB, descriptor sidecar, spatial
image and source-path ancestor must remain trusted and unchanged for the
complete build. The target build lock does not protect these input
paths; concurrent producers should finish an immutable release directory
before
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
consumes it.

Launch settings are validated before target preparation and stored in a
typed internal manifest. The generated `app.R` reads this frozen
configuration rather than interpolating values into R source, and the
staged source is parsed before publication. `max_request_size` is
converted from MB and applied through `shiny.maxRequestSize` for the app
lifetime; the previous process option is restored when the app stops.

Finally, `.crb` and `.rds` files are serialized R objects. Supply only
trusted inputs. The structural checks require a minimum stable runtime
API and avoid invoking serialized methods or getter bindings during
preflight, but they do not perform semantic validation or turn
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) into a sandbox for
hostile files.

## Forwarding extra Cerebro options

Anything you want to surface in `Cerebro.options` that isn’t already a
top-level argument can go through `cerebro_options`.

``` r
createShinyApp(
  cerebro_data = c("PBMC example" = crb),
  result_dir   = file.path(tempdir(), "cerebro_app_opts"),
  cerebro_options = list(
    exclude_trivial_metadata = TRUE
  ),
  launch_browser = FALSE
)
```

`mode`, `cerebro_version`, `crb_file_to_load`, `cerebro_root`, and the
internal `.bundle_backend_plan` and `.bundle_run_options` are filled in
by the function itself. Duplicate entries for either internal key are
removed, and anything you supply for those keys is overwritten.

## Deploying the bundle

With descriptor-backed sidecars (the default), the output directory is
self-contained: shipping the folder is all that is required. If you
configured a host-managed matrix override, provision that absolute path
separately on the deployment host. Three common targets:

``` r
# 1. Local
shiny::runApp(out_dir)

# 2. shiny-server / Posit Connect
# Drop the directory under the server's app root (e.g. /srv/shiny-server/),
# or use the Posit Connect publishing UI pointing at `app.R`.

# 3. shinyapps.io
rsconnect::deployApp(appDir = out_dir)
```

## Reference

- [`?createShinyApp`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
- [`vignette("cerebronexus_workflow_seurat")`](https://mihem.github.io/CerebroNexus/articles/cerebronexus_workflow_seurat.md)
  for end-to-end Seurat → `.crb` → app
- [`vignette("host_cerebro_on_shinyapps")`](https://mihem.github.io/CerebroNexus/articles/host_cerebro_on_shinyapps.md)
  for shinyapps.io specifics
