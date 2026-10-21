# Read a Cerebro data file

Reads a CRB written by
[`saveCerebro()`](https://mihem.github.io/CerebroNexus/reference/saveCerebro.md)
or the export functions, as RDS or qs2. An H5-backed expression matrix
is attached lazily and a thin CRB gets its cell names back from the H5
file; spatial molecule tables stay lazy until their FOV is requested. A
CRB from an older release that this version no longer reads (for example
one without an expression-backend descriptor) is rejected with a pointer
to
[`convertCerebro`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md).

## Usage

``` r
readCerebro(file)
```

## Arguments

- file:

  Input `.crb` path.

## Value

A current Cerebro object.
