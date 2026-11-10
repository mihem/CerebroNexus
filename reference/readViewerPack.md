# Read a valid Viewer Pack descriptor

Read a valid Viewer Pack descriptor

## Usage

``` r
readViewerPack(file, pack = .viewerPackPath(file))
```

## Arguments

- file:

  Path to the canonical CRB.

- pack:

  Optional Viewer Pack directory.

## Value

A descriptor or \`NULL\`.

## Examples

``` r
dir <- tempfile("cerebro-")
dir.create(dir)
file.copy(system.file("extdata/examples/example.crb", package = "CerebroNexus"), dir)
#> [1] TRUE
crb <- file.path(dir, "example.crb")
buildViewerPack(crb, viewer_binary = "always")
pack <- readViewerPack(crb)
names(pack)
#> [1] "valid"    "reason"   "manifest" "path"    
unlink(dir, recursive = TRUE)
```
