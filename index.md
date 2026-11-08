![CerebroNexus](reference/figures/logo.svg)

[![R-CMD-check
(upstream)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml/badge.svg)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle:
stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)

**CerebroNexus** (**Ce**ll**Re**port**Bro**wser **Nexus**) is a
[Shiny](https://shiny.posit.co/) platform for exploring and sharing
single-cell and spatial transcriptomics data, with gene-expression,
immune-repertoire, trajectory, and HLA-TCR analyses. See the [full
documentation](https://mihem.github.io/CerebroNexus/).

[Try the live demo](https://osmzhlab.uni-muenster.de/shiny/demo/).

*CerebroNexus began as a fork of
[cerebroApp](https://github.com/romanhaa/cerebroApp) by Roman Hillje and
has since evolved with substantial new features and active development
by [mihem](https://github.com/mihem) and [Xuesong
Wang](https://github.com/duocang).*

Automated tests run in a reproducible Nix environment.

![CerebroNexus spatial data view](reference/figures/featured.png)

CerebroNexus spatial data view

## 1. Installation

``` r
remotes::install_github('mihem/CerebroNexus')
```

The Builder (below) also needs a few suggested packages:

``` r
install.packages(c("Seurat", "callr", "processx", "ps"))
```

## 2. Build an app for your own data

**Without writing code**, open the Builder in your browser:

``` r
CerebroNexus::launchCerebroBuilder()
```

Add your data files, choose the groups and content, and build a `.crb`
file or a standalone App. See [Build a data set by
pointing](https://mihem.github.io/CerebroNexus/articles/build_a_data_set_by_pointing.html).

**From a script:**

``` r
library(CerebroNexus)

# Seurat, SingleCellExperiment, AnnData (.h5ad / .zarr), MuData (.h5mu)
# or SpatialData
convertSeuratToCerebro(
  seurat_file = "my_seurat.rds",
  result_dir = "output",
  groups = c("sample", "cluster")
)
convertSCEToCerebro(sce, "output", groups = "cluster")
convertH5adToCerebro("my_data.h5ad", "output", groups = "cell_type")
convertSpatialDataToCerebro("my_data.zarr", "output", groups = "cell_type")

createShinyApp(
  cerebro_data = c("My dataset" = "output/cerebro_my_seurat.crb"),
  result_dir = "my_app"
)
```

No Python is needed to read AnnData or SpatialData; see the vignette
[Use SingleCellExperiment and AnnData
data](https://mihem.github.io/CerebroNexus/vignettes/use_sce_and_anndata_data.Rmd).

## License

MIT, see [LICENSE.md](https://mihem.github.io/CerebroNexus/LICENSE.md).
