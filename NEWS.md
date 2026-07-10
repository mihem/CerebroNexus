# cerebroAppLite 2.0.0

## Spatial analysis and overlay improvements

- **Multi-gene co-expression**: a new "Co-expression (RGB)" plot type maps up to
  three genes onto the red / green / blue channels, so each cell's colour blends
  the genes it expresses and spatial co-localisation reads as a mixed hue.
- **Spatial autocorrelation**: ImageFeaturePlot now reports the displayed gene's
  Moran's I — how spatially clustered its expression is (large slides are
  down-sampled for a responsive, stable score).
- **Region outlines**: an opt-in toggle outlines each colour group's spatial
  region with its convex hull.
- **Copy alignment as preset**: after hand-aligning a histology overlay, a button
  emits the matching `spatial_images_*` `Cerebro.options` lines to paste into an
  app so the dataset ships pre-aligned.
- **Honest single-source overlay scale**: the background scale is now applied
  once (a squared-scale bug is fixed), the image is clipped to the plot area so
  it no longer covers the axes, and the default view is evenly framed.
- **Overlay controls UX**: interacting with any Additional-parameters control
  collapses the Main-parameters box, and the Additional panel scrolls internally
  (hidden scrollbar with soft top/bottom fades), so the plot stays visible while
  adjusting Move/Rotate.
- **Fixes**: switching from an image-bearing platform to a bead-only one
  (Slide-seq) no longer leaves a stale tissue image behind, and the embedded-image
  option is offered only for datasets that actually carry one.

## Spatial transcriptomics (interactive tab + histology overlay)

- **Spatial tab**: the interactive Spatial projection is now wired into the app.
  It mounts conditionally (via `insertConditionalTab()`) whenever the loaded
  dataset carries spatial data, with plotly-based coloring, group filters, and
  box/lasso cell selection.
- **Histology background overlay**: `createShinyApp()` gains `spatial_images`
  plus per-dataset `spatial_images_flip_x`, `spatial_images_flip_y`,
  `spatial_images_scale_x`, `spatial_images_scale_y`, and `spatial_plot_rotation`
  parameters. Matched images are copied into the app bundle and shown behind the
  cells, controlled by a **Background image** dropdown and an **Image opacity**
  slider. Unmatched entries are ignored with a warning rather than an error.
- **Bundled demo**: the "Cortex - Spatial (synthetic)" demo pairs fully
  synthetic cortical-depth cell coordinates (illustrative cell-type labels such
  as Excitatory L2/3 … Oligodendrocyte) with a synthetic H&E cortex-section SVG
  whose layer bands align with the cells, so cell types visibly stratify across
  the cortex out of the box. Both the coordinates and the image are synthetic —
  no patient data.
- **Documentation**: added the `vignette("spatial_analysis")` guide.
- **Bundled demo set**: the app now opens on `demo_full_tcr_bcr.crb` (PBMC,
  TCR + BCR + trajectory) plus four real spatial sections (Visium, Slide-seq v2,
  MERFISH, Xenium), so the dataset switcher spans immune-repertoire, trajectory,
  and spatial content. The two narrower PBMC subsets (`demo_healthy_t.crb`,
  `demo_bcell_rich.crb`) are no longer shipped — the Full set is their superset;
  `data-raw/build_ir_demos.R` can still rebuild them for a multi-sample demo.

## Spatial transcriptomics (backend)

- **Spatial data layer**: the `Cerebro_v1.3` class gains a `spatial` field with
  `addSpatialData()`, `getSpatialData()`, and `availableSpatial()` accessors.
- **Export support**: `exportFromSeurat()` now extracts spatial coordinates and
  expression from Seurat v5 image slots (Visium / Xenium / FOV) via the internal
  `.getSpatialData()` helper, storing them per image in the exported `.crb`.
- **Utility wrappers**: added `availableSpatial()`, `getSpatialData()`, and
  `serverSideGeneSelector()` in the Shiny utility layer.
- **Demo dataset**: bundled a synthetic Xenium spatial demo
  (`demo_spatial.crb`, 1,000 cells) as a fifth demo dataset.

# cerebroAppLite 1.7.8

## Trajectory tab

- **Trajectory module**: restores the pseudotime trajectory explorer from the
  original cerebroApp v1.3 (projection coloured by state/pseudotime, states by
  group, expression metrics along pseudotime, per-state gene/transcript counts).
  The code is Roman Hillje's original implementation, restructured into the
  v1.4 sub-file layout with no functional change.
- **Conditional tab**: the Trajectory tab is inserted dynamically
  (`insertConditionalTab`) only for data sets whose `.crb` carries trajectory
  data — the same content-driven sidebar mechanism used by the Immune
  Repertoire and Extra material tabs.
- **Demo data**: the monocle2 pseudotime trajectory is now bundled inside the
  `demo_full_tcr_bcr.crb` demo (computed on its B-cell subset) instead of a
  separate `demo_trajectory.crb`, so one demo shows TCR + BCR + trajectory. The
  trajectory is reproducible via `data-raw/build_trajectory_demo.R`.

# cerebroAppLite 1.7.7

## Multiple data sets (multi-crb)

- **Dataset switcher**: `createShinyApp()` now accepts a named vector of several
  `.crb` files and renders a "Select dataset:" dropdown in the sidebar, letting
  users move between data sets without restarting the app. Single-file usage is
  unchanged and shows no switcher. By default the smallest file is loaded first
  (`crb_pick_smallest_file`, default `TRUE`).
- **URL selection**: a data set can be opened directly via the URL, matched by
  the name given in `cerebro_data` or by file basename — either as a query
  string (`?dataset=TCR`) or as the last path segment (`/TCR`).
- **Demo data sets**: three genuinely distinct demo `.crb` files ship in
  `inst/extdata/v1.4/` — `demo_full_tcr_bcr.crb` (all cells, TCR + BCR),
  `demo_healthy_t.crb` (T + monocytes, TCR) and `demo_bcell_rich.crb` (B-cell
  rich, BCR). They differ in cell composition, so the UMAP and cell-type mix
  change as you switch, and clonotypes are assigned by lineage (TCR to T cells,
  BCR to B cells) rather than at random. Group-level analyses (marker genes,
  most-expressed genes, enriched pathways) are filtered to the cell types kept
  in each subset, so the demos are internally consistent. Built from the public
  10x Genomics `vdj_v1_hs_pbmc3` dataset; see `data-raw/README.md` for the
  reproducible build. The bundled app (`shiny::runApp("inst")`) now opens on
  these three data sets so the switcher is visible out of the box; pass a named
  vector to `createShinyApp()` for your own data (see `vignette("multi_crb")`).
  New vignette: *Loading multiple data sets (multi-crb) with a dataset
  switcher*.

## Immune repertoire

- **Clonal UMAP** no longer renders blank when the Immune repertoire tab is
  opened after visiting another tab (e.g. Main). The plotly renderer was gated
  on server-reported plot dimensions, which are not yet available when its
  output element is created on tab switch; plotly sizes itself client-side, so
  that gate was removed.

# cerebroAppLite 1.7.6

## Immune repertoire

- **Clone Sharing tab**: classifies every clonotype (V+J+CDR3 of the active
  chain) as Private (in a single unit), Public within-group, or Public
  cross-group, using a configurable "sharing unit" (any categorical metadata
  column, default `sample`) and the active group column. With no group selected
  it degrades to Private / Shared. Interactive plotly bars with on-bar
  count/percentage labels and a clean hover tooltip (one class per bar, no raw
  aesthetic names).
- **Definition** (clone-definition resolution waterfall) is available but hidden
  from the default tab strip: it is an exploratory check for choosing a
  clone-call resolution rather than a reader-facing figure. Uncomment its
  `tabPanel` to re-enable.

# cerebroAppLite 1.7.5

## Immune repertoire

- **Clonal UMAP**: axes now match the main projection style — `UMAP_1`/`UMAP_2`
  titles removed, with boxed/mirrored axes (showline + mirror) and autorange,
  so the Clonal UMAP sits visually consistent with the main projection tab.

# cerebroAppLite 1.7.4

## Immune repertoire

- **Clonal UMAP**: new first tab overlaying clone-expansion level
  (Single/Small/Medium/Large/Hyperexpanded) on the existing cell projection,
  reusing the dataset's UMAP/tSNE coordinates. A Receptor selector (TCR/BCR,
  only the classes present in the data) and a Projection selector drive it.
  A "Show all cells" option (on by default) draws cells without the selected
  receptor as a grey background, so expanded clones are shown in context.
  Group filters subset which cells appear by any metadata column.
- **Generic display options**: font size and title for every IR plot, plus
  point size and opacity for the scatter-type plots (Clonal UMAP, Scatter),
  in an "Additional parameters" box. Changing them re-renders the plot.
- **Reworked layout**: the immune repertoire page now uses the same
  left-parameters / right-visualization layout as the main projection tab, with
  Main parameters, Additional parameters, and Group filters boxes on the left.
- **Parameter help**: the info button on each parameter box opens a dialog
  explaining, in plain language, exactly the controls shown on the current tab.
- Clone call is no longer shown on the Clonal UMAP tab, where it only adds
  noise.
- **More scRepertoire parameters wired up**: a generic "Order groups" control
  (Default / Alphanumeric) now reaches every plot whose scRepertoire function
  accepts `order.by`, and clonalHomeostasis gains a "Clone size thresholds"
  control (`cloneSize`). Both previously had no UI and were never passed.
- **CDR3 length is now faceted, not overlaid.** The Length tab previously
  passed `group.by` straight to `scRepertoire::clonalLength`, which draws every
  group as coloured bars in a single panel. It now takes that function's export
  table and redraws it with `facet_wrap`, so each group (sample, or the chosen
  metadata column's levels) gets its own length-distribution panel on a shared
  axis — "Group results by: sample" produces one plot per sample instead of a
  single mixed plot.
- **Grouping unified on a single control.** The separate "Comparison units"
  selector has been removed from every tab: it re-split the repertoire list,
  which only duplicated — with a narrower, sample-only column set — what
  scRepertoire's own `group.by` already does (it rbinds the list and re-splits
  on the chosen column). Comparison units are now defined solely by "Group
  results by" ("Compare by" on Scatter / Compare / Paired Scatter): None
  compares the loaded samples; a metadata column compares that column's levels.
  This removes the case where setting one control had no visible effect because
  the other already expressed the same split.
- **Unified plot heights**: the immune repertoire tabs now share a single plot
  height (`ir_fill_plot` / `ir_fill_wrap` helpers) instead of repeating a
  per-tab pixel value.

# cerebroAppLite 1.7.3

## Immune repertoire

- **Immune repertoire**: new conditional tab for TCR/BCR clonotype analysis with 19
  visualization methods driven by `scRepertoire`, covering clonal abundance, diversity,
  homeostasis, CDR3 length/composition, V(D)J gene usage, k-mer motifs, and cross-sample
  comparison. Each method includes contextual help with biological interpretation guidance.
- **Sample splitting**: a sample-column dropdown lets users re-split the repertoire
  by any shared metadata column; all visualizations recompute against the chosen
  grouping instead of a fixed sample field.
- New utility wrapper: `getImmuneRepertoire()`.
- The bundled `example.crb` now carries real 10x immune-repertoire data
  (`sc5p_v2_hs_PBMC_10k`, 5' gene expression + TCR + BCR from the same
  experiment), so the immune repertoire tab — including TCR, BCR (isotype/SHM),
  and cross-sample comparisons — works out of the box in a single combined
  dataset. (This single 10x donor is randomly partitioned into three demo
  samples so that cross-sample features have data; the sample labels do not
  represent distinct biological donors.)
- Immune repertoire grouping/splitting now works for **any** metadata column
  (sample, condition, cell type, ...): grouping variables are taken from the
  data set's metadata and joined onto the clonotype data by barcode, rather
  than only columns embedded in the IR table.

# cerebroAppLite 1.7.2

## Enhanced modules

- Added a Most expressed genes tab for exploring per-group gene expression
  summaries exported with `.crb` files.
- Added an Enriched pathways tab for browsing pathway enrichment results.
- Added an Extra material tab for exported tables and plots.
- Added utility wrappers for exporting most expressed genes, enriched pathways,
  extra tables, and extra plots.
- Added tests and vignettes covering the new Shiny modules and export helpers.

# cerebroAppLite 1.7.1

This maintenance release cleans up the package surface introduced by the
previous releases and refreshes documentation for the current codebase.

## Maintenance

- Removed unused internal Shiny sidebar/menu helpers and orphaned utility
  wrapper functions.
- Cleaned stale roxygen comments, generated Rd files, README wording, and
  internal comments so they describe the current package from its own
  perspective.
- Updated package metadata and regenerated documentation for the current public
  API.

# cerebroAppLite 1.7.0

## New features

- External HDF5 expression backend, symmetric to the bpcells backend: `exportFromSeurat()` with `expression_matrix_mode = "h5"` writes the matrix via `HDF5Array::writeTENxMatrix()` to a TENx-format `.h5` next to the `.crb`. The runtime attach loads it back as a lazy `HDF5Array::TENxMatrix` seed and transposes via `DelayedArray::t()` (free); the in-memory `dgCMatrix` is never materialised, so RAM stays close to the `.crb` metadata size and attach is effectively instant
- Introduced `createShinyApp()` for bundling a self-contained Shiny app from one or more `.crb` files
- `createShinyApp()` now copies the `<stem>.h5` sibling alongside the `.crb` during app bundling, mirroring the existing `.bpcells/` handling
- Legacy `.crb` files (predating the `expression_backend` field) are auto-tagged as `h5` when the host app sets `Cerebro.options[["expression_matrix_h5"]]`, finally giving `inst/extdata/v1.4/example.h5` a runtime consumer
- `convertSeuratToCerebro()` accepts an in-memory Seurat object alongside the `.rds` path; output basename derives from `experiment_name` when no path is given
- `createShinyApp()` opens a `...` passthrough so callers can forward extra options without signature churn
- `exportFromSeurat()` with `expression_matrix_mode = "bpcells"` now auto-detects losslessly-integer values and calls `BPCells::convert_matrix_type("uint32_t")` before `write_matrix_dir()`, which triggers BPCells's bit-packed integer storage on the typical scRNA-seq counts case. Shrinks the bpcells sibling ~5× on integer counts (e.g. 50k cells × 20k genes: 440 MB raw double → 78 MB bit-packed; PBMC All Samples 38,606 × 147,756: 2.6 GB → 549 MB), and queries get ~1.5-1.7× faster as a side effect (smaller payload to read). Normalised float values (`slot = "data"`/`"scale.data"`) fall back to raw double to avoid silent precision loss

## Bug fixes

- Fixed `exportFromSCE()` projections: `reducedDims()` output is now coerced to `data.frame` before `addProjection()`, matching the Seurat path and clearing a latent runtime error for SCE inputs with non-PCA reductions
- Fixed `.attachExternalExpression` crashing on legacy `.crb` objects that predate the `getExpressionBackend()` method; such objects are now treated as embedded backend and skip the attach step
- Fixed "method not found" errors on the trajectory tab by renaming the corresponding `Cerebro_v1.3` methods to the names the Shiny server already calls (`getMethodsForTrajectories`, `getNamesOfTrajectories`)
- Fixed gene_expression plot chain freezing on gene picker changes: removed a stale `isolate()` wrapper and a reference to a non-existent `expression_projection_update_button` input; the existing 250 ms debounce on the data-to-plot reactive still throttles bursts

## Testing

- Extended testing
- Added an h5 round-trip test in `test-exportFromSeurat.R` verifying writer/reader bit-identity for the new HDF5 backend, plus an attach-level test asserting the runtime returns a lazy `DelayedMatrix` (not an in-memory `dgCMatrix`)
- Added `tests/README.md` documenting the layout (testthat unit, testthat shinytest2, smoke)

## Dependencies

- `rhdf5` removed from `Suggests`. The h5 backend now goes through `HDF5Array::writeTENxMatrix()` (writer) and `HDF5Array::TENxMatrix()` (lazy reader), which use rhdf5 internally; users no longer need to install or `requireNamespace` rhdf5 directly

## CI/CD

- Switched Nix environment to `fixed-date` to avoid constant rebuilding
- simplified workflow by removing `dev` and `sync-dev`

# cerebroAppLite 1.6.0

## Bug fixes

- Fixed all errors and warnings identified by R CMD CHECK, making the package ready for CRAN submission
- Fixed Seurat v5 API: replaced deprecated slot access (`@counts`, `@data`) with `GetAssayData()` across multiple functions
- Fixed `addPercentMtRibo`, `calculatePercentGenes`, `getMostExpressedGenes`, `performGeneSetEnrichmentAnalysis`, `getMarkerGenes`, `getEnrichedPathways`, `exportFromSCE`, `exportFromSeurat`
- Fixed GSVA v2.x API compatibility: now uses `gsvaParam()` with version check for backward compatibility
- Fixed `class(x) == "..."` checks replaced with `inherits()` across all relevant functions
- Fixed `require()` replaced with `requireNamespace()` throughout
- Fixed cross-references and examples in documentation

## Testing

- Added unit tests for all core R functions
- Added shinytest2 integration tests for the full Cerebro interface, covering gene expression, group/marker genes, color management, and more
- Tests run in a reproducible Nix environment via GitHub Actions

## CI/CD

- Added Nix-based GitHub Actions workflows for R CMD CHECK, R tests, pkgdown, code style, and automatic `default.nix` updates
- Added `update-nix` workflow: regenerates `default.nix` weekly via `create_env.R` and opens a PR automatically; BPCells commit SHA is now auto-fetched from GitHub
- Added `style` workflow: formats R code via [air](https://github.com/posit-dev/air) on every PR
- Added `sync-dev` workflow: automatically merges master back into dev after every merge to keep branches in sync
- Switched Nix environment to `bleeding-edge` for always up-to-date CRAN packages
- Branch protection rulesets configured for both `master` and `dev` with required status checks and auto-merge

## Documentation

- Added pkgdown site at <https://mihem.github.io/cerebroAppLite/> with light/dark/auto theme switch, search, and all vignettes as articles
- Site automatically builds and deploys to GitHub Pages on push to master

# cerebroAppLite 1.5.3

- several bug fixes so that launchCerebro should work again

# cerebroAppLite 1.5.2

- allow plot settings (size, opacity, number of cells to show) to be different in gene expression and overview (useful for large datasets with slow gene expression)

# cerebroAppLite 1.5.1

- remove unused functions in group

# cerebroAppLite 1.5.0

- make compatible with Seuratv5, especially with BPCells Matrix

# cerebroAppLite 1.4.1

- timeout function added. This logs out the user after 600 second of inactivity (can be changed in `shiny_ui.R`). The JS function was taken from https://stackoverflow.com/a/53207050/21417317.
- add option to show up to 1000 cells in `Main`, which is useful for exports.

# cerebroAppLite 1.4.0

This is the first update of this cerebroApp fork. Its aim is to continue a lightweight version of the excellent cerebroApp with only the main function as the cerebroApp by Roman Hillje is sadly discontinued.

## Major changes

- remove enriched pathways, extra material, most expressed genes and trajectory functions since the goal of this fork is to continue with a lightweight version

## Minor changes

- `Load Data` is renamed to `Data info` and `Overview` to `Main`
- Preferences about WebGL and hover info are now show in the first tab called `Data info`
- more colorful boxes for the sample information
- different icons for tabs `Data info`, `Main`, `Groups` and `Marker Groups`

# cerebroApp 1.3.1

Despite the minor version bump, this update contains substantial performance improvements in the Shiny app, specifically in the projections.

## Major changes

- Projections in the "Overview" and "Gene (set) expression" are now updated using the `Plotly.react()` Javascript function instead of redrawn from scratch inside R when changing the input variables. For the user, that means that (1) plots are drawn much quicker and (2) the current zoom/pan settings are maintained when switching plot parameters (coloring variable, point size/opacity, etc).

## Minor changes

- It is now possible to define several settings related to the projections shown in the "Overview" and "Gene (set) expression" tabs. For example, you can change the default point size and opacity, the default percentage of cells to show, and whether or not hover info should be activated in the projections. These settings are optional but useful when hosting a known data sets in `closed` mode, e.g. because you want to decrease the point size in a large data set. The respective parameters can be found in the description of the `launchCerebroV1.3()` function.
- Hover/tooltip info for cells in projections can be deactivated through a checkbox on the "About" tab. Deactivating hover info increases performance of projections.
- Hover/tooltip info for cells in the gene expression projection no longer contain the gene expression value. This is because preparing the hover info is an expensive computation with little return. As a result of removing the gene expression value, the hover info does not need to be recalculated every time a gene is added to or removed from the list of genes to show expression for. For the same reason, when plotting a trajectory, the state and pseudotime are not added to the hover info either.
- Internally, data for plotting in projections is rearranged, stored in different variables, and the final output is debounced to avoid unnecessary redrawing of the projections on initialization.
- The feature to show expression of multiple genes in separate panels has been matured. Up to 9 genes can be shown in a 3x3 panel matrix but all share the same color scale. While cells can be selected in any of the panels, the expression levels shown in the other UI element, e.g. table of selected cells or expression by group, refers to the mean expression of all selected genes (not just the one the cells were selected in).
- When coloring cells in projections by a caterogical variable, e.g. cell type, the dots in the legend are now larger and independent from the selected point size.
- Tables are now rendered server-side to improve performance for large tables.
- Cellular barcodes in tables of selected cells are formatted in monospace font.
- Columns in meta data tables, e.g. table of cells selected in projections, which are identified to contain percentage on a 0-100 scale are changed to a 0-1 scale to prevent non-sensical values such as 500%.
- Add comma to Y axis and hover info in bar chart of selected cells in projection ("Overview" tab).
- The `crb_file_to_load` parameter of the `launchCerebroV1.3()` function (or as part of `Cerebro.options`) can now be set to the name of a `Cerebro_v1.3` object. That means you can load the data set before launching Cerebro (with `readRDS()`) and make Cerebro initialize itself with it. This is particularly useful when hosting Cerebro in `closed` mode, preventing that each user session has to read the data set from disk.
- Update author info in "About" tab.

## Fixes

- Colors assigned to groups in bar chart of selected cells in projection ("Overview" tab) sometimes did not match those shown in the projection. This only applied to categorical grouping variables that are not registered as grouping variables.
- Update Enrichr API for `getEnrichedPathways()` function. Make it configurable in case of further changes to the API.

# cerebroApp 1.3.0

Because this is a relatively big release, I have prepared a dedicated article with release notes for cerebroApp v1.3 that you can find in the navigation bar.

## Major changes

- With data sets becoming more complex, users often have more than just the two grouping variables Cerebro was initially made to work with ('sample' and 'cluster'). To provide a more generalized interface, users can now specify multiple grouping variables (or a single one). Consequently, the 'Samples' and 'Clusters' tabs in the Cerebro interface have been replaced by the 'Groups' tab, where users can select one of the available grouping variables (with the same content as before). This can be useful when you cluster the cells with different methods/settings or have additional grouping variables, such as treatments, and want to provide the Cerebro user with both results.
- Data loaded into Cerebro is now stored in a dedicated class: `Cerebro_v1.3`.
- Due to the changes in data structure, files exported with cerebroApp v1.3 can only be visualized in Cerebro v1.3. Moreover, files exported with cerebroApp v1.2 and earlier cannot be loaded into Cerebro v1.3. I apologize for any inconvenience but I believe these changes will lead to more stability coming releases.
- Removed support for Seurat objects before v3.0. Users who need to continue working with older version of Seurat have two options: (1) use the `Seurat::UpdateSeuratObject()` function to update their Seurat object before exporting it for visualization in Cerebro; (2) use older Cerebro version. I apologize for the any trouble this may cause.
- The "Gene expression" and "Gene set expression" tabs have been merged into the new "Gene (set) expression)" which gives you access to both.
- The new "Extra material" tab allows you to export additional material related to the data set that you want to share with others. At the moment, only tables and plots (from ggplot2) are supported, but support for other types of content can be added upon user request in the future.

## New features

- It is now possible to export single cell data stored in `SingleCellExperiment` (SCE) objects.
- Gene (set) expression can now also be visualized in trajectories (generated by Monocle 2).
- `NA` values for cell assignment to one of the specified grouping variables will be replaced by `N/A` and put into a separate group ("N/A") when exporting the data.

# cerebroApp 1.2.2

## Fixes

- The title in the browser tab now correctly says "Cerebro" instead of containing some HTML code.
- Cluster trees should now be displayed correctly.
- `getEnrichedPathways()` no longer results in an error when marker genes are present but no database returns any enriched pathways, e.g. because there are too few marker genes. Thanks to @turkeyri for pointing it out and suggesting a solution!

# cerebroApp 1.2.1

## New features

- It is now possible to select cells in the dimensional reduction plots ('Overview', 'Gene expression', and 'Gene set expression' tabs) and retrieve additional info for them. For example, users can get tables of meta data or expression values and save them as a file for further analysis. Also, gene expression can be shown in the selected vs. non-selected cells.

## Minor changes

- Scales for expression levels by sample and cluster in "Gene expression" and "Gene set expression" tabs are now set to be from 0 to 1.2 times the highest value. This is to limit the violin plots which cannot be trimmed to the actual data range and will extend beyond, giving a false impression of negative values existing in the data.
- Hover info in expression by gene plot in "Gene expression" and "Gene set expression" tabs now show both the gene name and the mean expression value instead of just the gene name.

# cerebroApp 1.2.0

## New features

- New button for composition plots (e.g. samples by clusters or cell cycle) that allows to choose whether to scale by actual cell count (default) or percentage.
- New button for composition plots that allows to show/hide the respective table of numbers behind them.
- New tab "Color management": Users can now change the color assigned to each sample/cluster.
- "Gene expression" and "Gene set expression" panels: Users can now pick from a set of color scales and adjust the color range.
- The gene selection box in the "Gene expression" panel will now allow to view available genes and select them by clicking. It is not necessary anymore to hit Enter or Space to update the plot, this will be done automatically after providing new input.
- It is now possible to export assays other than `RNA` through the `assay` parameter in relevant functions.
- Launch old Cerebro interfaces through `version` parameter in `launchCerebro()`.
- We added a vignette which explains how to use cerebroApp and its functions.

## Minor changes

- Add citation info.
- Composition tables (e.g. samples by clusters or cell cycle) are now calculated in the Shiny app rather than being expected to be present in the `.crb` file.
- Fix log message in `exportFromSeurat()` when extracting trajectories.
- The gene set selection box in the "Gene set expression" tab will not crash anymore when typing a sequence of letters that doesn't match any gene set names.
- Remove dependency on pre-assigned colors in the `.crb` file. If no colors have been assigned to samples and clusters when loading a data set, they will be assigned then.
- Update examples of functions and include mini-Seurat object and example gene set (GMT file) to run the examples.
- Modify pre-loaded data set in Cerebro interface to contain more data.
- When attempting to download genes in GO term "cell surface" in the `getMarkerGenes()` function, it tries at max. 3 times to contact the biomaRt server and continues without if all attempts failed. Sometimes the server does not respond which gave an error in previous versions of the function.
- Plenty of changes to meet Bioconductor guidelines (character count per line, replace `.` in dplyr pipes with `rlang::.data`, etc.).
- Reduce package size by compressing reference files, e.g. gene name/ID conversion tables.

# cerebroApp 1.1.0

- Release along with manuscript revision.

## New features

- New function `extractMonocleTrajectory()`: Users can extract data from trajectories calculated with Monocle v2.
- New tab "Trajectories": Allows visualization of trajectories calculated with Monocle v2.

# cerebroApp 1.0.0

- Public release along with manuscript submission to bioRxiv.
