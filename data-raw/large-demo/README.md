# Large demo data sets

These scripts download and prepare the large demo data sets that `run-demo.R` opens in the Viewer: the mouse brain E18 data set (all 1,306,127 cells of the 10x 1M neurons matrix), the Ren et al. COVID-19 atlas (1.46M cells with paired TCR/BCR data), and, on request, the NanoString CosMx NSCLC spatial data set (eight tissue sections), the Zheng et al. pan-cancer T-cell atlas (397,810 cells with TCR sequences and the CD8 exhaustion pseudotime), and three spatial data sets with many sections: the Allen whole mouse brain MERFISH atlas (3,938,808 cells, 59 sections) and the Kukanja et al. mouse EAE (in situ sequencing, 52 sections) and human MS (Xenium, 6 sections) spinal cord data. They are not shipped in `inst/extdata/examples/`, `data-raw/` is excluded from package builds, and they are not part of the test suite. `tests/testthat/test-run-demo.R`, `test-ren-demo.R`, `test-large-demos.R`, `test-million-cell-viewer.R` and `test-demo-h5-transpose.R` test them with small synthetic data.

## How to run

You normally do not run these scripts yourself. From the repository root, run the launcher:

```bash
Rscript run-demo.R                             # the mouse brain, then the Viewer in the browser
Rscript run-demo.R --data=ren                  # only the Ren atlas
Rscript run-demo.R --data=1m,ren,cosmx,tcell   # the four large single-cell data sets
Rscript run-demo.R --data=allen,eae,ms         # the three spatial data sets with many sections
Rscript run-demo.R --data=ren --prepare-only   # download and convert, do not start the Viewer
Rscript run-demo.R --data=dlpfc,mouse_brain,breast,prostate  # the four multi-section Visium data sets
Rscript run-demo.R --data=1m,tcell,dlpfc,breast              # any mix, all in one Viewer
```

`--data` takes a comma-separated list of `1m` (the mouse brain), `ren`, `cosmx`, `tcell`, `allen`, `eae`, `ms`, `dlpfc`, `mouse_brain`, `breast` and `prostate`; without it the launcher prepares the mouse brain.

The launcher loads this checkout with `pkgload`, sources the scripts below, prepares each data set (a complete cache is reused, so only the first run downloads and converts), builds the Viewer Pack, and starts the app on `http://127.0.0.1:7451`. It needs `pkgload`, `rhdf5`, `HDF5Array`, `DelayedArray`, `readxl` and `qs2`, plus the packages the Viewer itself uses.

## What `Rscript run-demo.R --data=ren` does the first time

There is no prompt: if the cache has no finished Ren data, the launcher starts downloading and converting right away, then opens the Viewer. The options only change which data sets are prepared (`--data=ren` skips the one-million-cell mouse data set; the small example data sets of the app are always there). The steps, in order:

1. **Check the cache.** If a finished Ren data set is already there, skip to step 7.
2. **Download five files** into `large-examples/ren/source/`: the 14 GB `.h5ad` archive, the cell annotation table, the TCR table, the BCR table and the clinical sample table. Each file is checked against its expected size and SHA256; a file that is already complete is not downloaded again. The log says `Preparing Ren source: <file>`.
3. **Unpack** the archive into a 61 GB `.h5ad` file.
4. **Convert the counts to an H5 file** (`Converting the Ren count matrix to an H5 sidecar...`). This is the long step. The log first counts up the cells it has read (`read cells 20,000 of 1,462,702`), then the gene blocks it has written (`wrote gene block 1 of 24`). Temporary files are used on the way and removed at the end. The result is a 28 GB `.h5` file.
5. **Build the data set object**: read the cell annotations and clinical data, the published UMAP, and the 503,432 paired TCR/BCR records, and save the `.crb` file and the immune repertoire file next to the `.h5` file. This takes a few minutes.
6. **Publish.** Everything is built in a temporary folder first and only moved to its final name when it is complete, so a run that is stopped halfway leaves no half-finished data set behind. Starting again continues from the downloaded files, but redoes the conversion.
7. **Build the Viewer Pack**, the extra files that make the first page of the Viewer open quickly. A pack built by an earlier version, without the immune repertoire's integer encoding (`immune/encoded.qs2`), is rebuilt (about 80 s for Ren); without it the Immune repertoire page encodes the repertoire at every start. For a `.crb` you prepared yourself, run `CerebroNexus::buildViewerPack("path/to/data.crb", overwrite = TRUE)` once.
8. **Start the Viewer** at `http://127.0.0.1:7451`. The first data set in the list is still the small PBMC example; choose "Ren et al. COVID-19 atlas" in the data set menu.

The next run finds the finished data set in step 1 and starts the Viewer without downloading or converting anything. You need about 105 GB of free disk for the Ren data set (the archive and the `.h5ad` together are 75 GB, the converted files 28 GB). The one-million-cell data set needs more on top of that.

To prepare a data set from an R session without starting the Viewer, source the helper and the script, then call its entry function:

```r
pkgload::load_all(".")
source("data-raw/large-demo/prepare_viewer_ren_data.R")
crb <- prepareViewerRenDemoData()   # returns the path of the prepared .crb
```

`prepareViewer1mBenchmarkData()` does the same for the mouse brain data set, and `prepareDemoViewerPack(crb)` (from `prepare_viewer_pack.R`) builds the Viewer Pack of a prepared `.crb`.

## Files

- `prepare_viewer_1m_data.R`: downloads the 10x 1M neurons matrix and builds the mouse brain E18 `.crb` from all 1,306,127 cells. Entry function: `prepareViewer1mBenchmarkData()`.
- `prepare_viewer_ren_data.R`: downloads the Ren et al. atlas (about 61 GB as `.h5ad`), the cell annotation, the clinical sample table and the TCR/BCR tables, and builds the Ren `.crb` with its immune repertoire. Entry function: `prepareViewerRenDemoData()`.
- `prepare_viewer_cosmx_data.R`: downloads the eight section archives of the CosMx NSCLC data set (11.3 GB), reads the expression and cell tables, and builds the CosMx `.crb` with one spatial entry per section. Entry function: `prepareViewerCosMxDemoData()`. Source, licence status and changes: [`cosmx.ATTRIBUTION.md`](cosmx.ATTRIBUTION.md).
- `prepare_viewer_tcell_data.R`: downloads the Zenodo files of the pan-cancer T-cell atlas (14.1 GB), joins the 70 per-data-set expression objects, computes the joint embedding and builds the `.crb` with the TCR repertoire and the CD8 trajectory. Entry function: `prepareViewerTcellDemoData()`. Source, licence and changes: [`tcell.ATTRIBUTION.md`](tcell.ATTRIBUTION.md).
- `prepare_viewer_allen_data.R`: downloads the cell table, the gene table and the log2 expression `.h5ad` of the Allen whole mouse brain MERFISH data set (8.6 GB), computes the joint embedding and builds the `.crb` with one spatial entry per section and the atlas's taxonomy colours. Entry function: `prepareViewerAllenDemoData()`. Source, licence and changes: [`allen.ATTRIBUTION.md`](allen.ATTRIBUTION.md).
- `prepare_viewer_kukanja_data.R`: downloads the AnnData archive of the Kukanja et al. EAE (`eae`, 0.57 GB) or MS (`ms`, 0.51 GB) data set and builds the `.crb` with one spatial entry per section, the authors' cell types, colours and UMAP. Entry function: `prepareViewerKukanjaDemoData("eae")` or `("ms")`. Source, licence and changes: [`kukanja.ATTRIBUTION.md`](kukanja.ATTRIBUTION.md).
- `prepare_builder_visium_data.R`: downloads four real multi-section 10x Visium data sets with the H&E photograph of every section (`dlpfc`: 12 sections of human prefrontal cortex with layer labels; `mouse_brain`: 4 sagittal sections; `breast`: 2 breast cancer sections; `prostate`: prostate cancer and normal), and writes one SingleCellExperiment file per data set as Builder input, with a computed PCA, UMAP and clusters. `Rscript run-demo.R --data=dlpfc` prepares that file and then builds it with the Builder's own steps: import, the Builder's settings, every section's picture, Spatial opening as the grid. The resulting CRB opens in the same Viewer as the other data sets. The launcher prints the file's path, so it can also be imported in the Builder (`launchCerebroBuilder()`). This build loads the Builder code the way its end-to-end tests do, so it also needs `testthat` and `withr`. The Viewer shows pictures only from its own `spatial-assets` folder, so each picture is linked or copied into `inst/spatial-assets/demo-visium/`, a folder Git ignores. Entry functions: `prepareBuilderVisiumDemoData(id)` (the Builder input) and `prepareViewerVisiumDemoData(id, repo_root)` (the built data set). Sources, licences and changes: [`visium.ATTRIBUTION.md`](visium.ATTRIBUTION.md).
- `download_helpers.R`: the cache folder, the checked download (size, then MD5 or SHA256, resumable) and the publish-by-rename shared by the two scripts above. `run-demo.R` sources it before them.
- `prepare_viewer_pack.R`: builds or reuses the Viewer Pack of a prepared `.crb`. Entry function: `prepareDemoViewerPack()`.
- The expression matrix of the mouse brain and Ren data sets is written as the cells x genes H5 sidecar by the package's own streaming writer (`R/h5ad_stream.R`, the one `convertH5adToCerebro()` uses for an h5ad with more than 2^31 - 1 non-zero values), not by a script of this folder: `HDF5Array::writeTENxMatrix()` can only write one gene after the other, and a source stored by cells is then scanned from start to end for every block of genes, which takes days for these atlases. The writer reads the CSR arrays directly, in several R processes.

## Where the data goes

Downloads and prepared files are written to the user cache directory (`tools::R_user_dir("CerebroNexus", "cache")`, under `large-examples/`), shared by all checkouts and worktrees. Set `CEREBRO_LARGE_CACHE` to use another folder, for example on a cluster, or to keep one copy of the data outside the cache. The Ren data set alone needs about 105 GB. The conversion of the counts to H5 runs on several R processes (the option `cerebro.h5ad_stream_workers`; each process needs about 3 to 5 GB of memory, an estimate, not a measurement). On a 24-core workstation, reading the 1.46M Ren cells took about 80 seconds on 8 processes.

Each data set has two folders there: `<name>/source/` with the downloads and `<name>/prepared/` with the `.crb`, its `.h5` (and repertoire) file and `demo-version`. The folder names do not change. `demo-version` is the stamp of the preparation: the CerebroNexus version and a hash of the functions of the data set's script (the "recipe"), plus when it was written. A run prepares the data set again when the stamp differs from what the checkout would write, that is after a new release or a change of the script; nothing has to be bumped by hand. Data prepared before this scheme (folders `cerebro-v2`, `cerebro-v3`) is not used: delete those folders.

## The CosMx and T-cell data sets

Both are built the way the Ren data set is: the sources are downloaded once into `large-examples/<name>/source/` (each file is checked against its published size and checksum; a complete file is not downloaded again and an interrupted download is resumed), the result is built in a temporary folder of the cache and renamed to `prepared/` only when it is complete, and the last file written is `demo-version`. A stamp that no longer matches (a new release, or a change of the script) makes the next run build the data set again.

| | `--data=cosmx` | `--data=tcell` |
|---|---|---|
| Environment variable read by the Viewer | `CEREBRO_COSMX_DEMO_CRB` | `CEREBRO_TCELL_DEMO_CRB` |
| Cache folder | `large-examples/cosmx/` | `large-examples/tcell/` |
| Data set menu entry | CosMx SMI NSCLC (8 sections) | Zheng et al. pan-cancer T cells (398K + TCR) |
| Pages it is meant to exercise | Spatial, Gene expression | Immune repertoire, Trajectory |
| Download | 8 files, 11.3 GB | 3 files, 14.1 GB |

Notes on the T-cell data set:

- The expression matrix is the authors' normalised matrix, joined over the union of the genes of 70 files and reduced to gene symbols that at least 20 cells express (36,625 genes). 10x data are log-normalised, Smart-seq2 data are log2(TPM + 1).
- The atlas publishes no single-cell embedding. The `harmony_umap` projection is computed by the script (2,000 variable genes chosen per data set, 30 principal components, Harmony across the 35 data sets, UMAP). On the 24-core test machine this step took about 8 minutes.
- The TCR table covers 168,901 cells of 86 patients; a repertoire sample is a patient and a tissue site (`patient (T)`, `patient (N)`, `patient (P)`), so Compare and Clone Sharing follow a patient's clones between sites.
- The trajectory (`published_pseudotime`, "Published pseudotime" in the Viewer) is the CD8 continuum with the authors' diffusion pseudotime, which belongs to mini-clusters; see the attribution file for exactly what is inherited and rescaled.
- The H5 file and the intermediate matrix are held in memory while the data set is built: the peak was about 52 GB of resident memory.

## The spatial data sets with many sections

They are built like the CosMx data set, one spatial entry per section, so that Spatial opens as the grid of sections. They are meant to exercise the Spatial grid, its enlarged view and the Linked views with many panels.

| | `--data=allen` | `--data=eae` | `--data=ms` |
|---|---|---|---|
| Environment variable read by the Viewer | `CEREBRO_ALLEN_DEMO_CRB` | `CEREBRO_EAE_DEMO_CRB` | `CEREBRO_MS_DEMO_CRB` |
| Cache folder | `large-examples/allen/` | `large-examples/eae/` | `large-examples/ms/` |
| Data set menu entry | Allen whole mouse brain MERFISH (59 sections, 3.9M) | Kukanja et al. mouse EAE ISS (52 sections) | Kukanja et al. human MS Xenium (6 sections) |
| Cells, genes, sections | 3,938,808, 550, 59 | 592,421, 239, 52 | 660,801, 266, 6 |
| Download | 3 files, 8.6 GB | 1 file, 0.57 GB | 1 file, 0.51 GB |
| Licence | CC BY-NC 4.0 | CC BY 4.0 | CC BY 4.0 |

Notes:

- Allen: the expression is the atlas's log2-normalised matrix of the cells that passed its quality control. The atlas publishes no embedding of these cells; the `umap` projection is computed by the script (30 principal components, UMAP).
- Kukanja: the AnnData archive is unpacked into the build folder (4.3 GB for `eae`, 3.5 GB for `ms`) and removed afterwards. The embedding is the authors' UMAP.

Benchmarks and smoke checks live in [CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code) (`topics/viewer_performance/` and `topics/expression_backends/`).
