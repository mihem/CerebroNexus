# Launch CerebroNexus

Launch the CerebroNexus Shiny application.

## Usage

``` r
launchCerebro(
  mode = "open",
  maxFileSize = 800,
  crb_file_to_load = NULL,
  expression_matrix_mode = "crb",
  expression_matrix_h5 = NULL,
  expression_matrix_BPCells = NULL,
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
  using/hosting Cerebro in `closed` mode. Defaults to `NULL`.

- expression_matrix_mode:

  Mode of expression matrix. Can be either crb, h5, or BPCells. Default
  is crb.

- expression_matrix_h5:

  Optional: Path to `.h5` file containing an expression matrix created
  with
  [`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html),
  with genes as columns and cells as rows, contrary to the conventional
  format of genes as rows and cells as columns. This format greatly
  favors performance for extracting expression values for a gene
  (column), rather than a cell (row), which is the primary action in
  Cerebro. Importantly, the matrix should be stored with "expression" as
  group name (see parameters of the
  [`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html)
  function). Saving the expression matrix in `TENxMatrix` format has the
  benefit of a low memory footprint since the expression values are
  directly read from disk. This is particularly useful when working with
  very large data sets and/or when startup of the CerebroNexus app is a
  priority (which is shorter because only the rest of the data that
  needs to be loaded tends to be very small). By default, this value is
  set to `NULL`, meaning that the expression matrix is expected to be
  part of the `.crb` file.

- expression_matrix_BPCells:

  Optional: Path to BPCells directory created with
  [`BPCells::write_matrix_dir()`](https://bnprks.github.io/BPCells/reference/matrix_io.html).
  This is a hopefully faster alternative to h5 with a similar approach.

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

  Show hover infos in projections. This setting can be changed in the
  UI; defaults to TRUE.

- ...:

  Further parameters that are used by
  [`shiny::runApp`](https://rdrr.io/pkg/shiny/man/runApp.html), e.g.
  `host` or `port`.

## Value

Shiny application.

## Examples

``` r
if ( interactive() ) {
  launchCerebro(
    mode = "open",
    maxFileSize = 800
  )
}
```
