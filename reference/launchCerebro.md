# Launch CerebroNexus

Launch the CerebroNexus Shiny application.

## Usage

``` r
launchCerebro(
  mode = "open",
  maxFileSize = 800,
  crb_file_to_load = NULL,
  welcome_message = NULL,
  point_size = 5,
  point_opacity = 1,
  percentage_cells_to_show = 100,
  projections_show_hover_info = TRUE,
  require_token = NULL,
  ...
)
```

## Arguments

- mode:

  Cerebro can run in `open` or `closed` mode. Open mode allows users to
  load their own data sets and session-only HLA typing. Closed mode
  shows only pre-loaded data sets and their stored HLA typing; dataset
  and HLA upload controls are hidden and their handlers reject uploads.
  View-configuration JSON imports remain available. Defaults to `open`.

- maxFileSize:

  Maximum size of input file; defaults to `800` (800 MB).

- crb_file_to_load:

  Path to `.crb` file to load on launch of Cerebro. Useful when
  using/hosting Cerebro in `closed` mode. To offer several data sets,
  pass a *named* character vector, e.g.
  `c(sampleA = "a.crb", sampleB = "b.crb")`: every name is the stable,
  unique data set identity used in links and saved state. The files are
  read when the App starts, so a file that cannot be read stops the
  running App with its error. Defaults to `NULL`.

- welcome_message:

  `string` with custom welcome message to display in the "Load data"
  tab. Can contain HTML formatting, e.g. `'<h3>Hi!</h3>'`. Defaults to
  `NULL`.

- point_size:

  Default point size for cell scatter views. This value can be changed
  in the UI; defaults to 5.

- point_opacity:

  Default point opacity for cell scatter views. This value can be
  changed in the UI; defaults to 1.

- percentage_cells_to_show:

  Default percentage of cells shown in every cell scatter view. This
  value can be changed independently in each page; defaults to 100.

- projections_show_hover_info:

  Show hover information in projections. This setting can be changed in
  the UI; defaults to TRUE.

- require_token:

  Require a fresh per-run access token. The default, `NULL`, enables it
  for open-mode Viewers bound to a loopback address (for example
  `127.0.0.1`). The authorized URL is printed in the R console and used
  when the App opens its browser. `TRUE` requires a token on any
  address; `FALSE` explicitly disables it, for example behind an
  authenticated reverse proxy. Loopback binding alone is not
  authentication. Tokens travel in the URL: keep it private and use
  HTTPS for remote access. Closed-mode and generated standalone Viewers
  are unchanged by default. This default is a breaking change for
  open-mode launches behind a reverse proxy that also binds to loopback.
  Set `require_token = FALSE` for an intentionally public or externally
  authenticated deployment; proxy headers are not used to disable
  protection automatically.

- ...:

  Further parameters that are used by
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html), e.g.
  `host` or `port`.

## Value

Shiny application.

## Details

To answer the first request sooner, start the App with the file
`start.R` of the installed package instead of calling this function:
`Rscript <package directory>/start.R --port=3838 data.crb`, with
`system.file(package = "CerebroNexus")` as the package directory, or
`source(system.file("start.R", package = "CerebroNexus")); cerebroStart("data.crb", port = 3838)`
in R. It starts a small web server before CerebroNexus is loaded, which
sends the page of the previous run, its files and the first view of the
Projection page, and then runs `launchCerebro()` behind it; the App
keeps the address it was given. The first start of an installation has
no page to send yet and starts as `launchCerebro()` does.

The cached HTML interface remains public, including the early page
served by `cerebroStart()`. Without the token it cannot start a working
Shiny session: uploads, analyses and session data remain inaccessible.
Cached plot previews contain data, so their URLs remain private. No
token needs to be entered manually; it is included in the browser URL
automatically. If you start the returned App yourself with
`shiny::runApp(..., launch.browser = TRUE)`, use the authorized URL
printed in the console instead of the bare address. To control browser
opening while retaining the automatic authorized URL, pass
`options = list(launch.browser = FALSE)` (or a browser function) to
`launchCerebro()`. `cerebroStart()` accepts `require_token` too; its
command-line equivalent is `--require-token=auto`,
`--require-token=true` or `--require-token=false`.

## Examples

``` r
if ( interactive() ) {
  launchCerebro(
    mode = "open",
    maxFileSize = 800
  )
}
```
