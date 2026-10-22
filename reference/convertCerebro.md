# Convert a Cerebro data file

Re-serializes a CRB while reusing its existing expression sidecar. This
is also how a CRB from an older release is brought up to date: a missing
expression-backend descriptor is set to embedded, receptor data in the
retired `bcr_data` / `tcr_data` fields is merged into the unified immune
repertoire, and an H5-backed CRB written without a thin schema is
thinned. An object written before spatial, HLA typing, Trekker or
enriched-pathways support is rebuilt on the current class, so the
accessors it lacked exist. A spatial entry with the singular
`histology_image` fields becomes `histology_images`, and a molecule
table embedded in a spatial entry moves into the per-FOV sidecar next to
the CRB. A CRB whose expression storage is older than the formats this
version reads (an unknown expression backend, thin schema 1 or 2) cannot
be converted; re-export it with
[`exportFromSeurat`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md).

## Usage

``` r
convertCerebro(input, output = input, codec = c("qs2", "rds"))
```

## Arguments

- input:

  Input `.crb` path.

- output:

  Output `.crb` path in the same directory as `input`. Defaults to
  replacing `input`.

- codec:

  Serialization codec. Defaults to `"qs2"`; use `"rds"` when direct
  compatibility with [`readRDS()`](https://rdrr.io/r/base/readRDS.html)
  is required.

## Value

The output path, invisibly.
