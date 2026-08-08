![CerebroNexus](reference/figures/logo.svg)

[![R-CMD-check
(upstream)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml/badge.svg)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle:
stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)

CerebroNexus is a [Shiny](https://shiny.posit.co/) platform for
exploring and sharing single-cell and spatial transcriptomics data, with
gene-expression, immune-repertoire, trajectory, and HLA-TCR analyses.
See the [full documentation](https://mihem.github.io/CerebroNexus/).

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

## 2. Quick Start

``` r
library(CerebroNexus)

convertSeuratToCerebro(
  seurat_file = "my_seurat.rds",
  result_dir = "output",
  groups = c("sample", "cluster")
)

createShinyApp(
  cerebro_data = c("My dataset" = "output/cerebro_my_seurat.crb"),
  result_dir = "my_app"
)
```

### Optional login

To require a login, first create an encrypted database with
[`shinymanager::create_db()`](https://rdrr.io/pkg/shinymanager/man/create_db.html),
set its passphrase in an environment variable, and pass the database to
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md):

``` r
createShinyApp(
  cerebro_data = c("My dataset" = "output/cerebro_my_seurat.crb"),
  result_dir = "my_app",
  auth = list(
    credentials = "credentials.sqlite",
    passphrase_env = "CEREBRO_AUTH_PASSPHRASE"
  )
)
```

See [Control access with a login
page](https://www.mheming.com/CerebroNexus/articles/control_access_to_cerebro_with_a_login_page.html)
for multi-user setup and deployment examples.

## License

MIT, see [LICENSE.md](https://mihem.github.io/CerebroNexus/LICENSE.md).
