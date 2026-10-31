# Build a derived Viewer Pack for an existing CRB

Build a derived Viewer Pack for an existing CRB

## Usage

``` r
buildViewerPack(
  file,
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  overwrite = FALSE
)
```

## Arguments

- file:

  Existing canonical CRB path.

- viewer_binary:

  One of \`"auto"\`, \`"always"\`, or \`"never"\`.

- viewer_binary_threshold:

  Dataset-level threshold used by \`"auto"\`.

- overwrite:

  Replace an existing valid or invalid pack.

## Value

The pack path invisibly, or \`NULL\` when disabled.
