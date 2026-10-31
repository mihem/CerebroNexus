<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="man/figures/logo-dark.svg">
    <img src="man/figures/logo.svg" alt="CerebroNexus" width="380">
  </picture>
</p>

[![R-CMD-check (upstream)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml/badge.svg)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)


**CerebroNexus** (**Ce**ll**Re**port**Bro**wser **Nexus**) is a [Shiny](https://shiny.posit.co/) platform for exploring and sharing single-cell and spatial transcriptomics data, with gene-expression, immune-repertoire, trajectory, and HLA-TCR analyses. See the [full documentation](https://mihem.github.io/CerebroNexus/).

**Builder** prepares your data and packages an App. **Viewer** opens the result for interactive exploration and sharing.

[Install](#install) · [Builder](#run-builder) · [Viewer demos](#run-the-viewer-demos) · [Large datasets](#run-both-large-datasets) · [Project layout](#project-layout) · [Guides](#guides)

[Try the live demo](https://osmzhlab.uni-muenster.de/shiny/demo/).

*CerebroNexus began as a fork of [cerebroApp](https://github.com/romanhaa/cerebroApp) by Roman Hillje and has since evolved with substantial new features and active development by [mihem](https://github.com/mihem) and [Xuesong Wang](https://github.com/duocang).*

Automated tests run in a reproducible Nix environment.

![CerebroNexus spatial data view](man/figures/featured.png)

## Install

```r
install.packages("remotes")
remotes::install_github("mihem/CerebroNexus")
install.packages(c("Seurat", "shinymanager")) # Builder and login-enabled Apps
```

Seurat is needed for Builder; `shinymanager >= 1.1.0` is needed only for login-enabled Apps. Optional input/backend packages are listed in the [Builder guide](https://mihem.github.io/CerebroNexus/articles/build_a_data_set_by_pointing.html). Exported Apps still require R and the Viewer dependencies on the destination machine.

## Run Builder

```r
CerebroNexus::launchCerebroBuilder()
```

Builder opens in your browser and runs locally. Import trusted Seurat `.rds`, `.qs2` or `.qs` files, or choose **Load example datasets** to try it without your own data.

![CerebroNexus Builder configuring a bundled example dataset](man/figures/builder.png)

1. **Data** — add one or more datasets, or reopen a saved Builder project.
2. **Configure** — choose expression, groups, projections and optional analyses; inspect spatial views and align background images.
3. **Review** — check the included content and resolve blocking issues.
4. **Build** — export reusable CRB bundles or a standalone Viewer App, optionally protected by login.

**Save** keeps an editable Builder project for later work. **Build** creates a separate release inside the output folder you choose. A Viewer App release has this layout (simplified):

```text
release/
├── cerebro_app/
│   ├── app.R
│   ├── private-data/       # Datasets and required sidecars
│   ├── viewer/             # Bundled Viewer runtime
│   └── viewer-auth.env     # Login-enabled releases only; keep private
└── build-report.json
```

Run the exported App with `shiny::runApp("path/to/release/cerebro_app")`. Keep the entire App directory together when moving or sharing it; CRB-only releases instead contain the `.crb` files and their required sidecars.

On Linux, run `bash viewer/start-linux.sh` from the exported App directory to prepare login-file permissions and start Viewer, including after copying from Windows. For a service deployment, run `sudo bash viewer/start-linux.sh --prepare-only --user shiny` once with the actual service account, then start the service normally. See the bundled `viewer/AUTHENTICATION-DEPLOYMENT.md` for details.

## Convert from a script

```r
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
[Use SingleCellExperiment and AnnData data](vignettes/use_sce_and_anndata_data.Rmd).

## Run the Viewer demos

In R, launch the bundled demo app (PBMC, Spatial, Trekker and HLA/TCR):

```r
shiny::runApp(system.file("app.R", package = "CerebroNexus"), launch.browser = TRUE)
```

Switch datasets using **Sample data set**. For the basic Viewer example, use `CerebroNexus::launchCerebro()`. Use separate R sessions to run Builder and Viewer at the same time.

## Run both large datasets

For **Mouse 1M + Ren 1.46M**, alongside the bundled demos, run in a terminal with R and the package dependencies available:

```bash
git clone https://github.com/mihem/CerebroNexus.git
cd CerebroNexus
Rscript -e "install.packages(c('remotes', 'pkgload')); remotes::install_local('.', dependencies = TRUE)"
Rscript run-demo.R
```

The launcher uses the current checkout and prints the local URL, usually `http://127.0.0.1:7451`. First-time preparation downloads about **18 GB**; allow **100 GB free disk space**. Later runs reuse complete compatible caches. Keep each `.crb` with its `.bpcells/` directory.

Optional flags: `--prepare-only` prepares without launching, `--ren-only` skips Mouse, and `--no-browser` lets you open the printed URL yourself (useful in WSL).

## Project layout

| Path | Purpose |
| --- | --- |
| `R/` | Package API, data conversion and App export |
| `inst/builder/` | Builder interface and build workflow |
| `inst/viewer/` | Viewer interface and runtime |
| `inst/app.R`, `inst/extdata/examples/` | Bundled demo App and datasets |
| `run-demo.R` | Launcher for both large datasets and the bundled demos |
| `data-raw/` | Scripts for rebuilding demo data |
| `vignettes/`, `tests/` | User guides and tests |

## Guides

- [Builder workflow](https://mihem.github.io/CerebroNexus/articles/build_a_data_set_by_pointing.html)
- [Large datasets and preparation](https://mihem.github.io/CerebroNexus/articles/million_cell_crb_artifacts.html)
- [Scripted App export](https://mihem.github.io/CerebroNexus/articles/create_a_self_contained_shiny_app.html)
- [Login-protected Apps](https://mihem.github.io/CerebroNexus/articles/control_access_to_cerebro_with_a_login_page.html)

## License

MIT, see [LICENSE.md](LICENSE.md). 
