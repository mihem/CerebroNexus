# Download the larger demo data sets

The spatial (Visium, Slide-seq, MERFISH, Xenium) and HLA & TCR demo data
sets are not included in the package to keep it small.
`downloadDemoData()` downloads their `.crb` files from the CerebroNexus
GitHub repository (release tag `v7.0.0`) into the user cache,
`tools::R_user_dir("CerebroNexus", "cache")`, and checks the size and
MD5 sum of each file. Files that are already there are not downloaded
again.

The demo app shipped with the package,
`shiny::runApp(system.file(package = "CerebroNexus"))`, lists the
downloaded data sets next to the bundled one. The tissue images of the
Visium and Xenium demos are only part of the demo app in the source
repository.

## Usage

``` r
downloadDemoData(datasets = names(.demo_datasets))
```

## Arguments

- datasets:

  Names of the demo data sets to download; defaults to all of them:
  "Mouse brain (Visium)", "Mouse hippocampus (Slide-seq v2)", "Mouse
  ileum (MERFISH)", "Mouse brain (Xenium)" and "HLA & TCR".

## Value

A named character vector with the paths of the downloaded `.crb` files,
invisibly. It can be passed to `launchCerebro(crb_file_to_load = ...)`.

## Examples

``` r
if (interactive()) {
  files <- downloadDemoData("Mouse brain (Visium)")
  launchCerebro(mode = "closed", crb_file_to_load = files)
}
```
