# Convert a Cerebro data file

Re-serializes a CRB while reusing its existing expression sidecar.

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
