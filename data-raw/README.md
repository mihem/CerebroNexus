# data-raw — reproducible build of the demo immune-repertoire datasets

This directory regenerates the three demo `.crb` files shipped in
`inst/extdata/v1.4/` for the multi-crb + immune-repertoire demo:

| File | Sample | Cell composition | Immune repertoire |
|------|--------|------------------|-------------------|
| `demo_full_tcr_bcr.crb` | PBMC - Full (T+B)     | all cells (T + B + Mono) | TCR **and** BCR |
| `demo_healthy_t.crb`    | PBMC - Healthy (T/NK) | T + Monocytes            | TCR only        |
| `demo_bcell_rich.crb`   | PBMC - B-cell rich    | B + a few T cells        | BCR only        |

These are **genuinely distinct** data sets, not one data set with three IR
variants: each is a different cell subset of `example.crb`, so the UMAP and
cell-type composition change when you switch. Clonotypes are assigned **by
lineage** — TCR only to T cells, BCR only to B cells — so the repertoire is
biologically plausible rather than random noise.

## Data source (public, citable)

10x Genomics public dataset **`vdj_v1_hs_pbmc3`** — Human PBMC from a healthy
donor, Chromium 5' V(D)J, Cell Ranger 3.1.0. No de-identification is involved:
the data is public, and the only identity handling is a neutral sample name.

Download the filtered contig annotations into `vdj_10x/` (these CSVs are not
tracked in git — the built `.crb` demos are what ships):

```bash
mkdir -p data-raw/vdj_10x
BASE=https://cf.10xgenomics.com/samples/cell-vdj/3.1.0/vdj_v1_hs_pbmc3
curl -fL -o data-raw/vdj_10x/pbmc3_t_contig.csv \
  "$BASE/vdj_v1_hs_pbmc3_t_filtered_contig_annotations.csv"
curl -fL -o data-raw/vdj_10x/pbmc3_b_contig.csv \
  "$BASE/vdj_v1_hs_pbmc3_b_filtered_contig_annotations.csv"
```

## Rebuild

From the package root, with `cerebroAppLite` and `scRepertoire` (>= 2.0)
installed:

```bash
Rscript data-raw/build_ir_demos.R
```

The script (`build_ir_demos.R`):

1. `scRepertoire::loadContigs()` + `combineTCR()` / `combineBCR()` turn the 10x
   contig CSVs into clonotype pools (`CTgene`, `CTnt`, `CTaa`, `CTstrict`).
2. For each demo it takes a **cell subset** of `example.crb` (e.g. T + Mono for
   the healthy sample) and reconstructs a fresh `Cerebro_v1.3` with the
   expression matrix, metadata and projections filtered consistently.
3. Clonotypes are assigned **by lineage** (`set.seed` for reproducibility): TCR
   clonotypes go only to `T cells`, BCR only to `B cells`. The result is written
   into the `immune_repertoire` slot in the five-column layout
   (`barcode, CTgene, CTnt, CTaa, CTstrict`) the Shiny app's
   `immune_repertoire/data.R` expects; the app infers chain type from `CTgene`.
4. A verification pass asserts every TCR barcode lands on a T cell and every BCR
   barcode on a B cell.

Output overwrites the three `.crb` files in `inst/extdata/v1.4/`.

## Try the multi-dataset demo

```r
library(cerebroAppLite)
createShinyApp(
  cerebro_data = c(
    "PBMC - Full (T+B)"     = system.file("extdata/v1.4/demo_full_tcr_bcr.crb", package = "cerebroAppLite"),
    "PBMC - Healthy (T/NK)" = system.file("extdata/v1.4/demo_healthy_t.crb",    package = "cerebroAppLite"),
    "PBMC - B-cell rich"    = system.file("extdata/v1.4/demo_bcell_rich.crb",   package = "cerebroAppLite")
  )
)
```

A "Select dataset:" switcher appears in the sidebar; switching changes the UMAP,
the cell-type composition and the Immune Repertoire tab.

## Note

`data-raw/` is excluded from the built package via `.Rbuildignore`; it stays in
the repository for reproducibility only.
