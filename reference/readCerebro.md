# Read a Cerebro data file

Auto-detects legacy RDS, thin RDS, and thin qs2 CRBs. Thin expression
data are hydrated from their sibling sidecar; spatial molecule tables
stay lazy until their FOV is requested.

## Usage

``` r
readCerebro(file)
```

## Arguments

- file:

  Input `.crb` path.

## Value

A current Cerebro object.
