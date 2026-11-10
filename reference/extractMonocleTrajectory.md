# Extract trajectory from Monocle and add to Seurat object.

This function takes a Monocle object, extracts a trajectory that was
calculated, and stores it in the specified Seurat object. Trajectory
info (state, pseudotime, projection and tree) will be stored in
`object@misc$trajectories$monocle2` under the specified name.

## Usage

``` r
extractMonocleTrajectory(
  monocle,
  seurat,
  trajectory_name,
  column_state = "State",
  column_pseudotime = "Pseudotime"
)
```

## Arguments

- monocle:

  Monocle object to extract trajectory from.

- seurat:

  Seurat object to transfer trajectory to.

- trajectory_name:

  Name of trajectory.

- column_state:

  Name of meta data column that holds info about the state of a cell;
  defaults to 'State'.

- column_pseudotime:

  Name of meta data column that holds info about the pseudotime of a
  cell; defaults to 'Pseudotime'.

## Value

Returns Seurat object with added trajectory. Trajectory info (state,
pseudotime, projection and tree) will be stored in
`object@misc$trajectories$monocle2`\` under the specified name.

## Examples

``` r
if (FALSE) { # \dontrun{
## Needs a monocle (version 2) CellDataSet; monocle is not on CRAN.
seurat <- extractMonocleTrajectory(
  monocle = monocle,
  seurat = seurat,
  trajectory_name = 'trajectory_1',
  column_state = 'State',
  column_pseudotime = 'Pseudotime'
)
} # }
```
