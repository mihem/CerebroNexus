# Bounds of the display-only node size multiplier.

A dense network can read as one blob at the default radii, and a sparse
one as specks. The multiplier is presentation only: it scales every
radius and the cap by the same factor, so it never changes what a node's
area means.

## Usage

``` r
HLA_NODE_SCALE_MIN

HLA_NODE_SCALE_MAX
```
