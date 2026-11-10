# Validate a Viewer Pack

Validate a Viewer Pack

## Usage

``` r
validateViewerPack(file, pack = .viewerPackPath(file), full = TRUE)
```

## Arguments

- file:

  Path to the canonical CRB.

- pack:

  Optional Viewer Pack directory.

- full:

  Verify every asset size and checksum.

## Value

A validation result.

## Examples

``` r
dir <- tempfile("cerebro-")
dir.create(dir)
file.copy(system.file("extdata/examples/example.crb", package = "CerebroNexus"), dir)
#> [1] TRUE
crb <- file.path(dir, "example.crb")
buildViewerPack(crb, viewer_binary = "always")
validateViewerPack(crb)$valid
#> [1] TRUE
unlink(dir, recursive = TRUE)
```
