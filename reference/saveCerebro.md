# Save a Cerebro data file

Saves a Cerebro object as a thin CRB when it uses a BPCells sidecar and
stores spatial molecule tables in a sibling per-FOV sidecar. The input
object and existing sidecars are not modified.

## Usage

``` r
saveCerebro(
  object,
  file,
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 500000L
)
```

## Arguments

- object:

  A Cerebro object.

- file:

  Output `.crb` path.

- codec:

  Serialization codec. Defaults to `"qs2"`; use `"rds"` when direct
  compatibility with [`readRDS()`](https://rdrr.io/r/base/readRDS.html)
  is required.

- viewer_binary:

  Build the derived Viewer Pack `"auto"`matically for large datasets,
  `"always"`, or `"never"`.

- viewer_binary_threshold:

  Dataset-level cell threshold used by `viewer_binary = "auto"`.

## Value

The output path, invisibly.
