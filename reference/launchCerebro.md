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
  ...
)
```

## Arguments

- mode:

  Cerebro can be ran in `open` or `closed` mode, allowing the user to
  load their own data set (`open`) or only show a pre-loaded data set
  (`closed`, removes the "Load data" element); defaults to `open`.

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

## Examples

``` r
if ( interactive() ) {
  launchCerebro(
    mode = "open",
    maxFileSize = 800
  )
}
```
