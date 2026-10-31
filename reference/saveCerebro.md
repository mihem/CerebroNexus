# Save a Cerebro data file

Saves a Cerebro object. An object whose expression matrix sits in an H5
sidecar is saved as a thin CRB: the cell names stay in the H5 file and
the CRB keeps only their order and a checksum. Spatial molecule tables
go to a sibling per-FOV sidecar, and a large immune repertoire to a
sibling file (see `immune_sidecar`). The input object and existing
sidecars are not modified.

## Usage

``` r
saveCerebro(
  object,
  file,
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  immune_sidecar = c("auto", "always", "never"),
  immune_sidecar_threshold = 100000L
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

- immune_sidecar:

  Store the immune repertoire in a sibling file `<stem>.immune.qs2`
  (`.rds` with `codec = "rds"`) instead of inside the CRB. `"auto"` does
  so for H5-backed CRBs whose repertoire has at least
  `immune_sidecar_threshold` rows; `"always"` and `"never"` override
  that. The Viewer then lists samples and chains without reading the
  repertoire, and loads it when the Immune repertoire page needs it. The
  sidecar must travel with the CRB. An object whose repertoire already
  sits in a sidecar keeps pointing to that file whatever the mode is (a
  copy saved under another name shares it); to move it back into the
  CRB, call `addImmuneRepertoire(object$getImmuneRepertoire())` first. A
  sidecar of the CRB being replaced that the new CRB no longer uses is
  removed.

- immune_sidecar_threshold:

  Number of repertoire rows (cells with receptor data) used by
  `immune_sidecar = "auto"`.

## Value

The output path, invisibly.
