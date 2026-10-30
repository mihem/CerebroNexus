# CerebroNexus 5.4.0

## Viewer

- Share is offered on the grouped Immune repertoire Clonal projection too. An
  opened view comes back grouped, with its selection in its panel.
- Gene expression summaries taller than four screens keep updating; they used
  to need a quarter of themselves on screen.
- The spatial renderer takes a background image only from the resolved
  descriptor, and finds rotation and geometry by dataset id, not `.crb` path.
- Focus and Clear work the same on every page, and each page keeps its own
  selection and Focus while another page is open.
- The HLA & TCR Motifs network says why it draws nothing instead of spinning.
- Clicking a plot after brushing keeps the brushed outline.
- The Linked views panel button is now Enlarge / Restore, so Focus only zooms
  to the selection; the plot toolbar floats in the plot's top-right corner.
- Background tasks see libraries added at run time (`.libPaths()`, renv); with
  mirai or qs2 installed there, they used to stop silently.
- Linked views paints its first frame in the configured colours; it used to
  flash the default palette first.
- The Focus button turns off when a data set, projection or clone layout
  replaces the view under it, and follows a restored view.
- A restored view stops waiting for its render at the first click, key press or
  scroll, so a later render cannot undo what was done meanwhile.
  A filter or subsample change does not end that wait, and a selection
  emptied by one takes its brushed outline with it.
- Gene expression summaries keep updating however tall they are, and when the
  gate changes size in place.
- The plot toolbar of a short plot wraps into a second column and keeps clear
  of the minimap.
- Linked views, Spatial and the other plot pages follow the window while it is
  resized: the last plot no longer drops to a second row (a classic scrollbar,
  a fractional width or a drag inside a step did that), the height follows the
  window instead of moving in 24px steps, a plot is resized once per frame,
  and a page whose controls render late (Spatial) is refitted. A resize at the
  bottom of a scrolled page no longer grows the plots, and a last, shorter row
  of plots starts at the left edge.
- The Data info bars take the colour of their category, as on the other pages.
  Only hex, named and plain rgb()/hsl() colours are used as a bar fill.

## Changes

- `saveCerebro()`, `exportFromSeurat()` and `exportFromSCE()` build a Viewer Pack
  above 100,000 cells (was 500,000), so mid-sized data sets get one too.

# CerebroNexus 5.3.1

## Fixes

- Fixes `exportFromSCE()` failing on an object whose only reduction is PCA,
  and ignoring `use_delayed_array = TRUE`.
- Fixes `createShinyApp()` dropping the spatial molecule sidecars of every
  data set when one data set before them had none.
- Fixes `convertSeuratToCerebro()` failing when `most_expressed_genes` holds a
  non-table element, `getMarkerGenes()` failing with the default
  `organism = NULL`, and the gene lists of `performGeneSetEnrichmentAnalysis()`
  results carrying a stray space after every gene.
- Fixes the Linked views group filters on a group that is not the one coloured
  at start, for example `celltype` on the Trekker example: filtering it showed
  a stale picture or empty panels, because its per-cell values had not been
  loaded. The filter now loads them and then shows the cells that pass every
  filter; the colouring stays as it was (#222).

## Internal

- Internal clean-up with no change in behaviour: `exportFromSCE()` and
  `exportFromSeurat()` share their builders, and repeated code in the analysis
  helpers, `createShinyApp()` and the `Cerebro` class is merged.
- The expression backend types are listed once, in
  `inst/viewer/core/crb_contract.R`, and the export file-name stem is computed
  by one helper. A failed first load of the shared CRB contract is no longer
  cached as an empty environment.

# CerebroNexus 5.3.0

This release removes the remaining compatibility code for files, arguments and
dependency versions that no current release writes or needs. **Some calls and
stored files that 5.2.0 accepted now fail with an error that says what to use
instead**: see "Stored files" and "Calls that now fail" below.

## Stored files

`readCerebro()`, `launchCerebro()`, `createShinyApp()` and the Viewer now also
reject, with "uses a retired format", a `.crb` that

- was written by an exporter older than the `getHLATyping()` (2.3.0),
  `getTrekker()` (2.2.0) or `getMethodsForEnrichedPathways()` (after 1.7.0)
  accessors;
- holds a spatial entry with the singular `histology_image` /
  `histology_image_bounds` fields instead of `histology_images`;
- holds spatial molecule tables inside the `.crb` instead of in the
  `<stem>.spatial/` sidecar, or has a `getSpatialData()` without
  `hydrate_molecules`.

`convertCerebro()` upgrades all of these in place: it rebuilds the object on the
current class, converts a singular image to `histology_images` and moves molecule
tables into the sidecar.
The Viewer no longer carries guards for objects that lack an accessor, so a
current `.crb` is the only supported input.

## Calls that now fail

- **Seurat layers.** A requested layer that does not exist is an error in
  `exportFromSeurat()`, `convertSeuratToCerebro()`, `calculatePercentGenes()`,
  `getMostExpressedGenes()`, `addPercentMtRibo()` and
  `performGeneSetEnrichmentAnalysis()`; no other layer is substituted. Before,
  an RNA assay with only `counts` exported at the default `slot = "data"`
  silently exported `counts`. The message lists the layers that exist; the fix is
  `slot = "counts"` (or running `NormalizeData()`).
- **`createShinyApp()` spatial arguments.** `spatial_images_flip_x/_y`,
  `spatial_images_scale_x/_y` and `spatial_images_offset_x/_y` are removed; use
  `spatial_image_settings`. `spatial_images = c(Dataset = path)` and a
  per-dataset numeric `spatial_plot_rotation` are rejected with the nested form
  to use: `spatial_images = list(Dataset = list(<spatial entry> = c(label =
  path)))`, `spatial_plot_rotation = list(Dataset = list(<spatial entry> = 90))`.
  `expression_point_size`, `expression_point_opacity` and
  `expression_percentage_cells_to_show` in `cerebro_options` are rejected (they
  never had an effect); use `point_size`, `point_opacity` and
  `percentage_cells_to_show`.
- **`addSpatialData()`** and `@misc$cerebro_spatial_images` reject the singular
  `histology_image` fields.
- **Seurat v4 Visium images** (`VisiumV1`) are rejected by `exportFromSeurat()`
  and `convertSeuratToCerebro()`. Rebuild the image as a `VisiumV2` image
  (`Load10X_Spatial()`) or drop it from `object@images`; non-spatial Seurat v4
  objects still export. `SlideSeq` and `STARmap` images, which Seurat 5 still
  builds (`ReadSlideSeq()`, `LoadCurioSeeker()`, `LoadSTARmap()`), keep
  exporting.
- **`launchCerebro()`** needs unique, non-empty names when `crb_file_to_load`
  has more than one file, for example `c(sampleA = "a.crb", sampleB = "b.crb")`.
  Unnamed multi-file launches no longer get `legacy-N` ids.
- **Links.** `?dataset=` and `/name` links match only the dataset id (the name
  given in `cerebro_data` or `crb_file_to_load`), no longer the file name. A
  `?dataset=` link that matches no id opens another dataset with a warning that
  says so.
- `getMethodsWithEnrichedPathways()` is removed; use
  `getMethodsForEnrichedPathways()`.
- The display setting `gene_plotting_order = "Random"` is rejected; it never
  shuffled cells. Use `"Input order"`.

## Viewer

- Viewer Packs are checked by their schema version only. The
  `projection_identity_version` and `violin_summary_version` markers are no
  longer written or checked; every release with Viewer Packs (5.0.0 and later)
  wrote both, so no released pack is affected. A pack built by development code
  before 5.0.0 is no longer refused and has to be rebuilt with
  `buildViewerPack()`.
- Saved specialist views whose canvas viewport uses `x0/x1/y0/y1` are rejected
  on import; views saved by the current Viewer are unaffected.
- The Data info page no longer rebuilds category summaries that older code had
  truncated to ten entries.

## Dependencies

- Minimum versions are raised so the compatibility code could go: R 4.4.0,
  GSVA 2.0.0 (the `gsvaParam()` API only), msigdbr 24.1.0 (the
  `collection`/`msigdbr_collections()` API only) and ggplot2 3.4.0 (`linewidth`).
  R 4.4.0 follows from GSVA: GSVA 2.0.0 is part of Bioconductor 3.20, which
  needs R 4.4.
- `%||%` now comes from rlang; `R/utils-compat.R` is removed.

## Immune repertoire

- The Clonal projection grouped by a metadata column is no longer a static
  72 dpi image. Every group is a panel of the same interactive canvas as the
  ungrouped view, titled with its group and the number of cells that carry the
  receptor: zoom, hover and box or lasso select work in each panel, and a
  selection lists exactly the cells drawn in the panel it was made in. A column
  with more than 12 groups among the cells shown is not drawn (filter its groups
  or choose another column); a notice says so, and also when no cell or only
  some cells have a value in the column.

## Example data

- The example data sets in `inst/extdata/examples/` keep at most 100 genes
  (before: up to 1,500) and a random subset of their cells (1,000 to 3,000).
  Marker genes come first, but the 100 genes cannot hold every gene the
  analysis tables name: in `example.crb` 89 of the 1,911 marker genes, 32 of the
  182 most expressed genes and 45 of the 721 enriched-pathway genes are in the
  matrix, so those tables list genes that Gene expression cannot show. They stay
  embedded; `demo_spatial_merfish.crb` was converted to the current format.
- Every example is reproduced by running `data-raw/subsample_demos.R` and then
  `data-raw/shrink_demos.R` on the examples of 5.2.0; see `data-raw/README.md`.
  The Trekker example keeps all 50 nuclei with a positioning-evidence image.
- The three Xenium background images were recompressed without loss, and the
  unused `example.h5` was removed.

# CerebroNexus 5.2.1

## Fixes

- Share on a single-plot page (Projection, Spatial, Immune repertoire and the
  other pages with a Share button) works in a session that has not opened
  Linked views yet. The Share dialog's download and open requests are answered
  by the Linked views server, which was loaded only when that page was opened,
  so both requests timed out with "Download did not finish" or "Open did not
  finish".

# CerebroNexus 5.2.0

This release removes the BPCells expression storage and several retired storage
forms. **A `.crb` written by an older release may no longer open**: see
"Migrating existing files" below. External expression is now stored only in an
H5 file next to the `.crb`, and the large demo data sets can be rebuilt as H5 in
minutes.

## Storage

- `expression_matrix_mode` of `exportFromSeurat()` and `convertSeuratToCerebro()`
  is now `"embedded"` or `"h5"`. The BPCells storage mode is removed; the `h5`
  backend was the faster reader in the 2026-10-18 scale run. The recorded
  benchmark results, including the BPCells measurements, are kept as history in
  `inst/extdata/benchmarks/` and the "Large-dataset benchmarks" article.
- An H5-backed `.crb` is now thin (CRB schema version 3). The cell names are
  kept once, in the H5 file (`/expression/genes`), and the `.crb` stores only a
  checksum and the cell count instead of repeating them in `meta_data` and every
  projection. The Viewer still defers the cell names until a page needs them, so
  opening a data set reads no more than before. Opening compares only the cell
  count; the Viewer refuses an H5 file whose cell names do not match the `.crb`
  when the names are first read, which is at the latest before any gene
  expression is read. H5 `.crb` files from 5.1.x, which carry
  their cell names, keep opening.
- The immune repertoire of a large H5-backed data set is stored in a sibling
  file `<stem>.immune.qs2` (`.rds` with `codec = "rds"`) that the package
  writes itself: `exportFromSeurat()` and `saveCerebro()` gain
  `immune_sidecar = c("auto", "always", "never")` and
  `immune_sidecar_threshold` (100,000 rows). The Viewer lists samples and chains
  from the descriptor and loads the file when the Immune repertoire page needs
  it; `createShinyApp()` copies it into the bundle.
- The host-side overrides `expression_matrix_h5`, `expression_matrix_BPCells`
  and `expression_matrix_mode` are removed from `launchCerebro()`, and the
  matching `cerebro_options` entries from `createShinyApp()`, which now stops
  with an error when `cerebro_options` still contains `expression_matrix_h5` or
  `expression_matrix_BPCells`. A `.crb` finds its matrix through its own
  descriptor, relative to its own directory.
- `Cerebro` no longer has the `bcr_data` and `tcr_data` fields or
  `addBCRData()` / `addTCRData()`. `getBCR()` and `getTCR()` now return the BCR
  or TCR rows of the unified immune repertoire. `exportFromSeurat()` still
  accepts `@misc$tcr_data` / `@misc$bcr_data` as input and merges them into that
  repertoire.
- The Nix environment (`create_env.R`, `default.nix`) no longer provides
  BPCells. Reading a Seurat object whose layers are BPCells matrices still needs
  BPCells installed; exporting such an object is not supported (bring every
  layer into memory first).

## Migrating existing files

`readCerebro()`, `launchCerebro()`, `createShinyApp()` and the Viewer stop with
"uses a retired format" for a `.crb` that has no expression-backend descriptor,
uses thin schema version 1 or 2, still holds `bcr_data` / `tcr_data`, keeps its
immune repertoire in a retired sidecar layout, or is a pre-spatial
`Cerebro_v1.3` object.

- `convertCerebro("x.crb")` upgrades a missing descriptor, `bcr_data` /
  `tcr_data` and a pre-spatial object in place, and rewrites a 5.1.x H5 `.crb`
  as a thin one.
- A `.crb` whose matrix is stored in a retired external layout (the BPCells
  directory, thin schema 1 or 2) cannot be converted. Re-export it from the
  source object with `exportFromSeurat(expression_matrix_mode = "h5")`, and
  delete the old matrix directory afterwards.
- The shipped example `demo_hla_tcr_dextramer.crb` had no descriptor and was
  converted.

## Large demo data

- The scripts that download and prepare the one-million-cell mouse brain and the
  Ren et al. COVID-19 atlas moved from `tests/demo/` to `data-raw/large-demo/`,
  with a README that says how to run them (`Rscript run-demo.R`). They are not
  part of the test suite. `run-demo.R`, `.Rbuildignore` and the tests that read
  the scripts follow.
- Both scripts write the expression matrix as the H5 file next to the `.crb`
  (and the Ren immune repertoire as `<stem>.immune.qs2`), through the new helper
  `h5_transposed_write.R`. `HDF5Array::writeTENxMatrix()` can only write one
  gene after the other, so a source stored by cells was scanned from start to
  end for every block of genes; the first mouse brain build did not finish in
  26 minutes. The helper transposes in two passes instead: pass 1 reads the
  cells once, in chunks and on several R processes (an AnnData matrix stored as
  CSR is read from its arrays), and writes one scratch file per chunk; pass 2
  assembles blocks of genes from those files and appends them to the H5 file.
  The layout is the one `writeTENxMatrix()` writes. On an 8-core machine with 4
  processes the mouse brain (1,000,000 cells) took about 8 minutes and the Ren
  atlas (1,462,702 cells) about 7 minutes, with the sources already local.
  `CEREBRO_DEMO_WORKERS` sets the number of processes (default 2; each needs
  about 3 to 5 GB of memory, an estimate).
- The mouse brain demo now uses all 1,306,127 cells of the 10x matrix instead
  of 1,000,000 evenly spaced ones, and is labelled "10x E18 mouse brain
  (1.3M)". Its cached analysis is stored as `mouse_brain_e18_sketch.rds`. The
  file and function names still say `1m`.
- The mouse brain script reads `CEREBRO_LARGE_CACHE` like the Ren script, so an
  existing download is found. It prepares the data in `1m/cerebro-v2/` with a
  `demo-version` stamp, as the Ren script does in its `cerebro-v2` folder, so a
  mouse brain built by an earlier release (1,000,000 cells, folder `cerebro`) is
  not taken for the 1.3M data set. The earlier files are left in place.
- `CEREBRO_1M_DEMO_CRB` and `CEREBRO_REN_DEMO_CRB` must name a `.crb` with an
  adjacent `.h5` file (before: an adjacent `.bpcells` directory). The demo data
  built by an earlier release has to be built again.

## Fixes

- `convertSeuratToCerebro()` accepts `h5_compression_level`, `immune_sidecar`
  and `immune_sidecar_threshold` and passes them to `exportFromSeurat()`; before,
  it always wrote an uncompressed H5 file and could not control the sidecar.
- Re-exporting with `exportFromSeurat()` or saving with `saveCerebro()` removes an
  immune repertoire sidecar of the replaced CRB that the new CRB no longer uses
  (a file another CRB in the directory still points to stays), and
  `saveCerebro()` no longer leaves a staged sidecar behind when a later step
  fails. An object whose repertoire already sits in a sidecar keeps pointing to
  it whatever `immune_sidecar` is; see `?saveCerebro`.
- `immune_sidecar` and `immune_sidecar_threshold` come after `verbose` in
  `exportFromSeurat()`, so the position of the arguments before them is the same
  as in 5.1.3.
- The Viewer checks the H5 file's cell names against the `.crb` before it reads
  gene expression, so expression of another data set with the same number of
  cells is no longer painted onto a thin `.crb`. The refusal is written to the
  log.
- The "uses a retired format" error says to re-export the file with
  `exportFromSeurat()` when `convertCerebro()` cannot repair it (a BPCells or thin
  schema 1 or 2 `.crb`); before, it sent every such file to `convertCerebro()`,
  which then refused it.
- `convertCerebro()` stops on an `expression_backend` that is not a descriptor
  instead of writing the file as embedded.
- `convertCerebro()` no longer rejects an `output` in the same directory on
  Windows because of a path-separator mismatch.
- A background read of a `.crb` that does not finish within 30 s (for example
  when the worker R process cannot load its packages) no longer leaves the
  dataset switch waiting forever; the Viewer reads the file in the foreground.
  The 30 s count from when the switch starts to wait, not from when the read
  began, and the end of an abandoned read no longer discards a newer one.

## Documentation

- The H5, thin-CRB and immune-sidecar changes are described in the vignettes on
  H5 expression, self-contained apps, shinyapps.io hosting, data integrity,
  million-cell artifacts, immune repertoire analysis, the Cerebro class, Seurat
  v5 layered assays and the Seurat workflow. The benchmark article keeps its
  BPCells measurements as history and says that BPCells storage was removed in
  5.2.0.

# CerebroNexus 5.1.4

- Fixes Data info taking 13 to 21 seconds to open a large dataset when the app
  has no prebuilt dataset catalog, as in `run-demo.R`; the 1.46-million-cell
  Ren et al. atlas now opens in about a second. The page counts categories
  without copying the metadata, takes the gene count and projection names from
  the first-frame cache instead of reading every cell name, publishes the
  projection resources after it is painted, and renders category bars as one
  string. The Viewer also reserves heap at start-up when a large CRB is
  configured, so opening it no longer triggers a full garbage collection.

# CerebroNexus 5.1.3

- The "Large-dataset benchmarks" article now reports the 2026-10-18 Scale run
  that compares embedded, BPCells and H5 storage from 1k cells up to each full
  dataset: setup, full-size medians and both figures, read from a small summary
  table in `inst/extdata/benchmarks/`.
- The expression-backend benchmark, the Viewer benchmarks and smoke checks,
  their results and the paper figures moved to
  [duocang/CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code).
  `tests/bench/` is gone; the data preparation used by `run-demo.R` and its
  tests is now in `tests/demo/`.
- `exportFromSeurat()` writes the H5 expression file at gzip level 0 (stored
  without compression; the shuffle and deflate filters are still applied),
  because export and read speed matter more than disk size. The file is about
  7 times larger than at HDF5Array's default level 6, e.g. about 69 GiB instead
  of 10 GiB for 1.5 million human prefrontal cortex cells. The new
  `h5_compression_level` argument sets the level; use 6 to save disk space.
- BPCells is no longer installed with the package: `DESCRIPTION` drops the
  BPCells remote. Reading BPCells-backed Seurat objects still needs BPCells
  installed.
- Fixes the HLA motif network's Focus button showing as off right after a
  selection was focused, when the server's selection reply arrived after the
  click.

# CerebroNexus 5.1.2

- Fixes embedded background images above about 750 KB being cut short, which
  left their last rows transparent.
- Fixes Spatial sessions ending when a gene was chosen before Gene expression
  had been opened, or when a dataset switch left a gene the new dataset lacks;
  a session that ends can no longer stop the whole app.
- Moves the expression colour scales shared by Spatial and Gene expression to
  `viewer/core/`, and the deferred-page test now also catches calls made by
  string or function reference.

# CerebroNexus 5.1.1

- Background images now open at full opacity (1.0) instead of 50% in Spatial,
  Linked views and Trekker. A configured or saved `image_opacity` still applies.
- Fixes background images drifting away from their cells in a rotated scene,
  such as the MERFISH demo. The image rotates in data space about the origin,
  the same way `spatial_plot_rotation` rotates the cell coordinates.
- Declares only the packages the code uses: `httr` is now a Suggests entry,
  and the unused `formattable` and `reshape2` are no longer listed. The Nix
  environment (`create_env.R`, `default.nix`) drops `data.table`, `formattable`
  and `shinyvalidate`, and `create_env.R` groups its packages by role.
- Internal clean-up with no change in behaviour: shared helpers replace
  duplicated code in Spatial, Linked views, Trekker and `cell_views.js`, and
  unused reactives, outputs and CSS rules are removed.

# CerebroNexus 5.1.0

- Adds Viewer Packs: `buildViewerPack()` writes a sidecar directory next to a
  `.crb` file so that large data sets skip re-reading the object on every
  session, and `saveCerebro()` builds one automatically above 500,000 cells.
- Spatial scenes are grouped by sample and ROI, and background images are
  delivered per session: the browser receives a private URL for the file it is
  allowed to see instead of a data URI or a filesystem path. The Background
  image picker holds one removable chip and is hidden for data sets that
  configure no image.
- Background images open at 50% opacity unless `image_opacity` is set in
  `spatial_image_settings` or saved with an embedded image's alignment; the
  setting applies in Spatial, Linked views and Trekker.
- Linked views selects the section whose background is adjusted from a picker
  instead of a tab strip, with one switch that shows or hides every background
  image.
- Ordinary projection views share one author configuration: legacy per-page
  overrides such as `expression_point_size` no longer shadow it, while
  `spatial_*` settings still apply to Spatial.
- The data set selector drives one data set context, so progress, results and
  hover metadata started for a previous data set are discarded instead of being
  applied to the new one.
- Keeps the "UMAP layout" shortcut on Immune repertoire once its plot is painted.
  The values Shiny reports for re-created tab and grouping controls are no longer
  mistaken for edits, which used to hide the shortcut for the rest of the visit.
- Fixes the page loader staying on "Preparing the current view…" after a dataset
  switch rebuilt a page's result once its value had already arrived, which left
  Extra material tables rendered but unreachable until another page was opened.
- Fixes an uncaught client error for an output that fails without a bound
  element. It aborted the rest of Shiny's message frame, so a page that became
  unavailable for a dataset announced the return to Data info without
  performing it.
- Keeps Gene expression selections stable while switching datasets, rejects
  late gene-search results, and confirms a finished plot only for the current
  dataset generation. Defers updates to off-screen expression summaries until
  they are visible, while retaining already computed plots during scrolling.
- Makes visualization parameter bars fill available space automatically, giving
  multi-select fields more room and wrapping controls on narrower screens.
  Groups selectors and Settings fields also fill their available space. Removes
  JavaScript chip-width measurements in favour of responsive CSS.
- Adds Composite RGB and Separate channels layouts to Gene expression. RGB
  starts with the first three available selected genes, retains manual channel
  assignments across display-mode switches, and preserves the full gene list.
  Genes chosen in the channels are added to the gene list when leaving RGB.
  Separate channels retains the composite's per-channel intensity scaling.
- Fixes switching from mean expression to packed multi-gene panels getting stuck
  before the first paint completes.
- Speeds up loading and switching data sets. Data info shows a catalog data set
  without reading the whole `.crb`, each data set is loaded once per request,
  cell fingerprints and Viewer Pack checksums are computed once per process,
  and HDF5 expression is attached on first use.
- Pages stop computing in the background once left: Overview, Gene expression
  and Trajectory no longer rebuild their frames or read expression for a page
  that is not shown.
- Reduces work on common interactions with large data: Overview reuses
  downloaded cell IDs for recolours and applies "% of cells" in the browser when
  a Viewer Pack is present; selections are identified by cell IDs instead of
  coordinate strings; Gene expression keeps expression values across plot
  option changes and refreshes summaries only when their inputs change;
  Trajectory and Spatial apply point size and opacity without a new frame.
- Spatial computes its first frame once, normalizes sections and ROI scopes
  once per input, and serves embedded background images by private URL instead
  of resending them with every frame.
- Trajectory colours its default (main group) view from the Viewer Pack, and
  Linked views loads further projections from the Viewer Pack over HTTP instead
  of the websocket.
- Speeds up Immune repertoire Clone Sharing parsing and stops tab visits from
  recomputing settled results; toolbar choices are kept when switching tabs.
- Fixes HLA/TCR motifs reusing the previous chain's segments after a chain
  switch, and an uncaught client error when Gene expression loads before Shiny
  connects.
- The browser benchmarks in `tests/bench` run installed candidates by default
  (`VIEWER_BENCH_LOAD=load_all` keeps the source-tree mode) and no longer stall
  on Data info.

# CerebroNexus 5.0.2

- Completes the source-test cleanup across Viewer modules, transport, dataset
  lifecycle, authentication and benchmark tools. Tests now execute renderers,
  Shiny consumers, browser interactions and command-line stages; JavaScript
  fixtures execute complete scripts instead of matching source formatting (#163).
- Reuses the shared Share button in Linked Views, removes duplicate toolbar and
  button styling, and simplifies overlay dragging and responsive sizing.
- Replaces compact-layout source assertions with component output and browser
  checks across all eight visualization pages, including sharing, selection
  clearing, HLA focus, IR sharing visibility and drag cancellation (#163).
- Exercises dataset loading progress, hidden HLA/Trekker consumers, deferred
  Trajectory panels and benchmark correctness with runtime tests instead of
  assertions about source spelling (#163).
- Keeps Share buttons disabled while a dataset switch or reconnect waits for
  a current plot. Adds runtime coverage for HLA allele synchronization,
  reordered Trajectory data, stale frames and reconnect recovery.
- Keeps settings drawers open during global Shiny value notifications; only
  replacement of their containing output closes them.
- Honors disabled per-cell hover while still loading cell IDs on demand.
- Removes the optional JavaScript parser dependency from tests and keeps
  browser checks in development mode while package checks respect CRAN settings.

# CerebroNexus 5.0.1

- Reduces the nine bundled demo CRBs from 30.26 to 21.22 MiB (about 30%) through
  stronger build-time compression and a maximum of 1,500 expression genes per
  demo. Smaller panels remain whole; available demonstration markers,
  myelination genes and Trekker Moran genes are protected.
- Preserves all demo cells, UMAP and spatial coordinates, metadata, precomputed
  analyses, images and exact expression values for retained genes. Gene search
  and new gene-set analyses are limited to the exported panel. User-supplied
  datasets and application codec defaults are unaffected.
- Shares the final export policy across active demo builders, uses the public
  CRB reader/writer and fixes Spatial background-image insertion to use the
  named image collection.
- Replaces formatting-sensitive Viewer test assertions with parsed R expression
  checks and a behavioral check for Spatial's empty-selection early return.

# CerebroNexus 5.0.0

## Viewer

- Adds a shared accelerated renderer with WebGPU and WebGL2 fallback for large
  projections, Linked Views, Gene Expression, Spatial, Trajectory and repertoire
  views, preserving zoom, hover and coordinated selection.
- Uses compact binary transport, canonical cell indices, reusable geometry and
  deferred metadata to reduce large-dataset transfer and preparation costs.
- Fixes remounted views, Gene selection summaries and tables, progress lifecycle,
  pinned tooltips and stale-result rejection when switching datasets.
- Refines navigation, typography, responsive layouts, loading and empty states;
  improves keyboard access, focus, status announcements and reduced motion.
- Restores colored sidebar icons and provides a default-on Linked cell hover
  switch in the toolbar, independent of zoom, selection and Data Info settings.
- Avoids repeated IR preparation and metadata joins, reuses Groups counts, and
  computes exact native Abundance, SizeDist and Homeostasis summaries where
  supported while preserving analytical fallbacks.
- Defers inactive-page work and optional trajectory/IR content; bounds process
  caches and shared prefetch tasks without changing expression precision.
- Includes an opt-in million-cell mouse-brain demo and reproducible benchmarks.
  Historical timing series retain their original source and readiness boundaries;
  they are not a universal page-loading guarantee.

## Data format and I/O

- Adds `saveCerebro()`, `readCerebro()` and `convertCerebro()` with automatic legacy
  RDS compatibility and qs2 support. Thin CRB v2 and Viewer Packs preserve lazy
  backend access, stable cell fingerprints and portable first-frame assets.
- Supports gene-major BPCells storage and batch expression reads, restoring
  canonical cell order after storage-order access; existing backends remain
  compatible.
- Supports lazy large-repertoire sidecars and compact repeated metadata, including
  the Ren atlas's paired TCR/BCR repertoire and published t-SNE projection.
- Streams delayed-matrix exports without whole-matrix dense realization, and
  bounds external enrichment request time and error handling.

## Packaging

- Removes application-specific container recipes and the direct OpenSSL package
  dependency. Dataset bundle filenames remain unchanged.
- Consolidates this PR's development NEWS entries into one 5.0.0 release; earlier
  released versions below are unchanged. Nix remains the CI environment.

# CerebroNexus 4.4.3

## Viewer

- Visualization pages use compact, consistent controls and selection actions
  to leave more room for plots without layout shifts.
- Legends, action buttons, and Immune Repertoire/HLA tabs share consistent
  styling.

# CerebroNexus 4.4.2

## Viewer

- Plotly charts use Linked views-style toolbars that switch from horizontal to
  vertical when their panel is narrow; Sankey diagrams no longer show
  Cartesian-only controls.
- Viewer tabs keep a stable size on hover, gene selectors remain above their
  content, and table pagination follows the amber application theme.
- Composition Sankey diagrams render their data after switching from bar
  charts.
- Gene-expression detail and summary panels now follow the active gene mode
  and cell selection, including consistent RGB violin colours.

# CerebroNexus 4.4.1

## Viewer

- Ordinary data tables escape HTML content by default, including the gene ID
  conversion table.
- Gene-set selection uses the public `msigdbr` API and caches only compact
  catalogues and resolved gene lists across Viewer sessions.
- Unsafe server-side PDF export controls were removed; compatible plots use
  browser-side PNG downloads from the plot toolbar. Portable views also offer
  PNG download through Share view.

# CerebroNexus 4.4.0

## Viewer

- Generated Viewers can bundle private CSV, TSV and Excel workbooks as lazy
  Extra material tables with optional display names for workbook sheets;
  formula-like cells and column names are neutralized before export.
- `createShinyApp()` now applies nested per-dataset colour palettes and rejects
  flat vectors or invalid colour values that generated Viewers cannot interpret.
- Generated Viewers can open on a configured initial page, including pages that
  are available only for the loaded dataset, without shifting historical
  positional arguments.
- Closed Viewers cap request bodies at 6 MiB, while open Viewers retain their
  configured upload limit.
- Manual colour choices remain scoped to the dataset where they were made.

# CerebroNexus 4.3.6

## Viewer

- Linked and specialist views can be downloaded as validated portable JSON and
  opened in another compatible Cerebro session while preserving panels,
  filters, viewports and selections.

# CerebroNexus 4.3.5

## Viewer

- Projection, Spatial, Trekker, expression, trajectory and repertoire pages
  now share the coordinated Canvas renderer and consistent selection controls.
- Generated Viewers preserve legacy Spatial image settings and row-name-only
  cell identities throughout linked selections and cell details.
- Linked views combine projections, Spatial sections, Trekker and clonal
  layouts in one coordinated workspace with shared selection, colouring,
  filtering and cell details.

# CerebroNexus 4.3.4

## Viewer

- Viewer cache messages identify CRB files by basename without logging their
  absolute server paths.

# CerebroNexus 4.3.3

## Viewer

- Generated Viewers now disable user data uploads by default. Set
  `show_upload_ui = TRUE` in `createShinyApp()` to opt in. Closed Viewers also
  ignore upload input submitted outside the rendered UI.

# CerebroNexus 4.3.2

## Viewer

- Generated Viewers decrypt hashed credentials into memory and no longer
  require write access to authentication databases at runtime. Account and
  password changes remain administrator-managed through rebuilt deployments.

# CerebroNexus 4.3.1

## Viewer

- Generated Viewers no longer require write access to authentication databases
  at runtime. Read-only deployments can update accounts or passwords in the
  private source database and deploy the change through a rebuilt Viewer.

# CerebroNexus 4.3.0

## Data and conversion

- Spatial backgrounds now use the public hierarchy `dataset -> spatial entry ->
  image label` in `createShinyApp()`, while conversion starts at `spatial entry
  -> image label`. A spatial entry is a Seurat `Images()` name (for example a
  Visium slice, Xenium/MERFISH FOV, or Slide-seq puck), not a required donor
  field. Each entry may contain multiple arbitrarily named embedded and external
  PNG/JPEG/SVG backgrounds, or remain coordinates-only. Optional per-image
  bounds and display settings use the same stable keys; ambiguous legacy
  dataset-to-path calls now fail instead of guessing a spatial target.
- Existing pre-spatial `Cerebro_v1.3` files remain bundleable as coordinate-only
  datasets. Legacy per-dataset `spatial_images` vectors with one unambiguous
  spatial entry are migrated to separately labelled images; calls with zero or
  multiple spatial entries still fail rather than guessing.

# CerebroNexus 4.2.0

## Breaking changes

- The public R6 data-class constructor is now `Cerebro$new()`. The
  version-suffixed constructor was removed; existing `.crb` files remain
  readable and all bundled examples have been reserialized with the new class.

# CerebroNexus 4.1.0

## Viewer

- `createShinyApp()` can optionally protect a generated Viewer with an existing
  encrypted shinymanager SQLite database and an environment-variable key. The
  encrypted database is copied into the private app bundle; its passphrase and
  plaintext login passwords are not. Apps remain public when `auth` is omitted.
- Authenticated Viewers use a responsive CerebroNexus login page with accessible
  controls and mobile-friendly spacing.
- Viewer sessions now close server-side on logout or authentication timeout,
  and forced-password-change pages cannot start the protected Viewer server.
- Authentication preflight validates the complete shinymanager database,
  requires a non-trivial database passphrase, and verifies private POSIX modes.

# CerebroNexus 4.0

## Breaking changes

- `launchCerebro()` is now the single application launcher. The obsolete
  version selector and deprecated version-specific launch exports were removed
  because CerebroNexus ships one Viewer implementation.

## Internal

- Viewer runtime sources now live under `inst/viewer/`, and bundled examples
  live under `inst/extdata/examples/`. Removing the historical Viewer version
  from internal paths does not change the application or data formats.

# CerebroNexus 3.2.0

## Documentation

- **Draft article: "Expression backend benchmark".** Introduces a protocol for comparing `embedded`, `bpcells`, and `h5` on two real public million-cell matrices. It reports independent-process medians and ranges, distinguishes a fresh-process first query from uncontrolled cold-disk access, and limits scale conclusions to the recorded host and source/tier points. A complete publication-profile run has not yet been performed on this development head.
- **A complete methodology accompanies the results.** It defines the experimental unit, balanced backend order, expression-density-stratified query panel, run profiles, correctness fingerprints, provenance, failure handling, and interpretation boundaries.

## Internal

- **A reproducible benchmark harness now lives in `tests/bench/`.** It reads public HDF5 cell blocks through the `rhdf5` ROS3 driver, rotates backend order across repeated exports, validates row and block values against the source matrix, and records Git, dependency, host, and SHA-256 source provenance.
- **Benchmark result publication is failure-safe.** Runs are staged and validated before entering an immutable result directory; the `CURRENT` pointer changes last, so an interruption cannot erase the previous evidence.
- **Benchmark plans are checked against the host before bulk transfer.** The harness reports estimated peak memory, R's vector limit, sparse-index capacity, and free-disk budget for every source/tier. Normal comparison profiles exclude deliberate memory-boundary tiers; those are isolated in the explicit `stress` profile.
- `rhdf5` is now listed in `Suggests` for the benchmark harness and its reader contract test.

# CerebroNexus 3.1.0

## Documentation

- A new data-integrity guide explains the shared resolve/validate/stage pattern
  used for layered assays, export replacement, and immune-repertoire identity.

## Export

- `addImmuneRepertoire()` now accepts scRepertoire lists, one `.rds`, explicitly
  named Cell Ranger CSVs, or existing scRepertoire metadata.
  `convertSeuratToCerebro()` delegates to the same API users can call before
  `exportFromSeurat()`. TCR and BCR rows are merged by sample rather than
  leaving duplicate names that hide one receptor type.
- Export now validates one named data.frame per sample, the five required
  scRepertoire columns as one-dimensional row vectors, globally unique non-empty
  barcodes, and barcode overlap with the Seurat cells. Complete mismatch and
  ambiguous identity are errors. Valid partial overlap is normalized before
  storage: orphan rows and samples are removed with a warning, so they cannot
  inflate clone statistics. CSV sample identities must be explicit, and an
  explicit `sample_col` no longer falls back silently when misspelled.
- A valid unified repertoire takes precedence over legacy `tcr_data` and
  `bcr_data`. When only legacy slots exist, they are validated and merged into
  the unified field while remaining available through `getTCR()` / `getBCR()`.
  Existing serialized CRBs must be re-exported to receive this migration.
- CRBs and external H5/BPCells sidecars are staged with owner-only POSIX modes,
  so late validation errors leave an existing export unchanged and replacement
  preserves an existing CRB's mode. An existing sidecar is replaced only when
  the published CRB identifies it as its own backend; otherwise the export stops
  without touching either file. Backend switches remove the previous owned
  sidecar after commit. Ordinary R errors use best-effort rollback. Readers must
  be stopped before replacement; process termination and concurrent writers
  remain outside the transaction guarantee.

# CerebroNexus 3.0.5

## Testing / CI

- **The app tests reuse one Shiny process where booting a fresh one buys
  nothing.** Every `shinytest2` recording started its own app, and starting the
  app cost far more than the assertions did: on a CI runner roughly 88% of the
  suite's wall clock went to cold starts rather than to its ~8,600 assertions.
  Recordings that only navigate the plot tabset and read the DOM now share one
  `AppDriver` per file without changing a single assertion. Local timings
  estimate that this can save about 1.5 minutes per CI run. The
  immune-repertoire file takes this furthest: eleven of its sixteen tests share
  one driver, cutting that file's local runtime from roughly 170 seconds to
  90--120 seconds across measured runs.
- Sharing is applied only where a reused app is still a fair test. These keep
  their own driver deliberately: the `app$expect_values()` snapshots, whose files
  are named after the driver; the scRepertoire lazy-loading contracts and others
  that assert what a pristine app has *not* loaded; tests that read an input's
  initial value; and the one that leaves a modal open. Tests that navigate the
  dashboard sidebar and then read that tab's output keep their own driver where
  the reused output remains suspended and reads back `NULL`.
- Production smoke tests now build each synthetic and real-data app bundle once
  per test file instead of rebuilding identical artifacts for every assertion.
  Consumers remain read-only and browser checks still use independent Shiny
  sessions; locally this reduced the smoke file from about 38 to 29 seconds.

# CerebroNexus 3.0.4

## Export

- **`createShinyApp()` now follows each `.crb` backend descriptor.** H5 files
  and BPCells directories are copied from the recorded portable relative
  location rather than guessed from the current `.crb` name, so renamed data
  files remain portable. Missing sidecars, trailing-slash H5 locations, symbolic
  links, case-folded or parent/child target collisions, duplicate labels or
  data sources that resolve to the same canonical CRB, unsupported serialized
  objects and unsafe multi-dataset overrides now fail during preflight.
  Validation requires the minimum stable runtime API on a locked R6 structure
  without invoking serialized methods, getter, active or lazy bindings;
  `.crb`/`.rds` inputs nevertheless remain trusted serialized R objects, not
  sandboxed content.
- **Configured CRBs now use the exact backend decision validated at build
  time.** `createShinyApp()` derives a versioned per-CRB attachment plan from the
  ordinary `expression_backend` field and the deployment override, then stores
  it in the generated configuration. The standalone runtime consumes that plan
  instead of calling a serialized getter. Direct launches and uploads likewise
  read and validate the ordinary field without invoking the getter; partially
  upgraded objects that contain only the field or only the getter fail closed.
- **Bundle publication now has an explicit private/public boundary and recovery
  protocol.** Raw `.crb`, H5 and BPCells artifacts stay in the non-HTTP
  `private-data/` tree; the historical `data/` name is not reused because a
  still-running older app may retain its former `/data` HTTP mapping. Files
  explicitly supplied through `spatial_images` are copied to `spatial-assets/`,
  read by the server-side renderer and embedded as data URIs; the directory is
  not registered as an HTTP resource. Client-selected backgrounds must match
  the current dataset's configured allowlist and resolve canonically inside
  `spatial-assets/` before the renderer reads them. Each
  canonical target has one atomic build lock covering
  preflight through cleanup. Builds use a private sibling stage, retain the
  existing root mode, fail closed on unreadable destinations, never delete a
  foreign target, restore a previous app after a failed final rename when
  possible, and retain the exact backup path when restoration fails.
  CRBs, sidecars, spatial images and their source-path ancestors must remain
  unchanged during a build. Abrupt process death is not crash-atomic; recovery
  requires verifying and restoring the previous backup before stale stage/lock
  cleanup.
- **Generated app launch settings now fail closed before publication.** Upload
  size, port, host, browser, quiet and display-mode values are strictly
  validated and frozen in a typed configuration rather than interpolated into
  R source. The staged `app.R` is parsed before it can replace an existing app.
  The upload limit now actually applies through
  `shiny.maxRequestSize` for the app lifetime and restores the process option
  when the app stops; previous bundles documented this limit but did not apply
  it.
- **Host-managed matrix overrides are explicit exceptions to self-contained
  bundles.** They must be absolute and serve only one effective CRB consumer.
  Native paths are resolved component by component and rejected inside
  `result_dir`; unresolved filesystem entries, unsafe Windows aliases, and
  device namespaces fail closed. Non-native paths for another host are
  preserved lexically and cannot be compared with the local app tree.
  Overrides are not copied, and absent ordinary targets are not rejected, so
  the deployment host must provide them.
- **The H5 guide now starts with the one-step export workflow.**
  `exportFromSeurat(..., expression_matrix_mode = "h5")` creates the `.crb`
  and its H5 sidecar together. The manual conversion now writes an H5 backend
  descriptor before saving, producing the same portable, self-describing pair.
# CerebroNexus 3.0.3

## Export

- **Split Seurat v5 objects no longer export a single sample.** `split()` names
  an assay's layers `<root>.<level>`, but the old fallback accepted the first
  readable layer and could pair one sample's matrix with every sample's
  metadata. Resolution is now driven by the requested layer: an exact physical
  name remains authoritative, otherwise candidate cell memberships must prove
  one unique complete partition before CerebroNexus joins it on a local copy.
  This works for sample names and arbitrary roots, ignores unrelated overlapping
  layers, protects every custom layer matched by Seurat's own prefix search, and
  reports ambiguous or incomplete covers instead of guessing. Incomplete
  requested-prefix noise is returned as a structured resolution failure, so it
  cannot short-circuit an explicitly enabled, complete compatibility fallback.
- **Every complete-matrix consumer shares the same coverage contract.**
  `exportFromSeurat()`, `convertSeuratToCerebro()`,
  `calculatePercentGenes()`, `getMostExpressedGenes()`,
  `addPercentMtRibo()`, and `performGeneSetEnrichmentAnalysis()` now resolve
  layered assays through one gateway. The resulting matrix must contain exactly
  the object's cells and is reordered once before analysis or storage.
  `embedded`, `h5`, and `bpcells` therefore fail identically on partial data,
  and conversion errors are propagated rather than printed and swallowed.
- **Spatial export reuses the validated expression resolution.** Each image
  intersects coordinates with the already-resolved matrix instead of resolving
  and joining the assay again. Spatial payloads keep the requested and physical
  layer names separately, so a warned `data` to `counts` compatibility fallback
  cannot serialize counts while labelling them as normalized data.
- **A disk-backed source assay says so when it is refused.** Reading a Seurat
  object whose layers are BPCells or DelayedArray matrices is still not
  supported. Every selected partition member is inspected, the refusal names
  the disk-backed layer, shows how to bring every layer into memory, and points
  out that this is unrelated to `expression_matrix_mode`, which controls how
  the exported `.crb` stores its matrix rather than how the source object holds
  it.
- **A new layered-assay vignette documents the full contract.** It includes
  troubleshooting, storage guidance, custom-root examples, and four colour
  diagrams covering the original failure, membership-based partition proof,
  resolver decisions, and public entry points.
- **Prefix protection and partition proof are separate.** Seurat's broad
  `Layers(search = "^root")` result remains the authority for everything
  `JoinLayers()` might consume, but only `<root>.*` layers may prove the
  requested partition. A complete nested custom root such as
  `data.imputed.s1/s2` is semantically ambiguous when `data` is requested and
  now fails closed instead of becoming normalized data silently.
- **Pathological exact-cover search has deterministic resource budgets.**
  Conflict indexing, visited nodes, and recursion depth are bounded
  independently. Highly overlapping prefix noise now stops before exhausting
  memory or R's call stack, and the diagnostic asks the user to rename noise or
  join the intended layers.
  `convertSeuratToCerebro()` also hands its already validated resolution to
  `exportFromSeurat()`, avoiding a second joined matrix and the corresponding
  peak-memory duplication.

# CerebroNexus 3.0.2

## Interface and documentation

- **The wordmark renders identically on every machine.** The sidebar logo was an
  SVG built from `<text>` elements carrying a `font-family` list, so the browser
  substituted whatever font it happened to have and the mark changed width and
  weight from one machine to the next. It is now vector outlines (Fredoka, SIL
  OFL) kept in `inst/shiny/v1.4/www/cerebronexus.svg`, so it no longer depends on
  any installed font. The `Nexus` half also moves from the old Bootstrap blue to
  the amber accent used throughout the rest of the interface.
- **The README documents the analysis modules.** Its feature section previously
  covered only the export API, so Immune repertoire, Spatial, Trekker, HLA & TCR
  Motifs and Trajectory went unmentioned. Each now states what a `.crb` must
  carry for its tab to appear, with a link to the corresponding guide.

# CerebroNexus 3.0.1

## Faster startup

- **Deferred `scRepertoire` loading.** The Immune Repertoire settings render on
  the first flush (they are `suspendWhenHidden = FALSE`), which previously forced
  `scRepertoire` — and, through its imports, ~90 packages and several seconds of
  `lazyLoadDBfetch` — to load at startup even when the tab was never opened.
  Availability is now probed with `system.file()` (a disk-path lookup, no
  namespace load); the namespace is loaded lazily on the first
  scRepertoire-backed plot — self-made plots (Clone Sharing, Definition) and the
  default Clonal UMAP never trigger it. Repertoire figures are unchanged — they
  are still computed by `scRepertoire`.
- **True lazy loading — no background prewarm.** `scRepertoire` is loaded only
  when the first scRepertoire-backed plot is drawn. There is intentionally no
  background prewarm: `later::later()` is cooperative scheduling on Shiny's
  single R thread, so loading the ~90-package tree "in the background" would
  still block the event loop and freeze every session in the process for
  several seconds. The trade-off is explicit and honest — app startup never
  pays for `scRepertoire`, and a repertoire user waits once (several seconds —
  the full namespace load) on their first scRepertoire plot. Because a namespace
  is process-wide, it then stays warm for every session in that R worker (not
  only the one that triggered it — which also means that first load briefly
  blocks the other sessions sharing the process). Measured: startup drops ~50%
  (median 8.2 s → 4.0 s) while the first Abundance plot rises correspondingly
  (1.4 s → 5.2 s), so the two roughly cancel — the cost is moved off startup,
  not removed.
- **Heavy dependencies load on demand.** All dependencies stay mandatory, so a
  standard install gives every feature out of the box; other heavy packages
  (`GSVA`, `biomaRt`, `httr`, `qvalue`, `future.apply`, `pbapply`) are accessed
  via `requireNamespace()` and loaded only when the feature that needs them is
  first used, not at package/app startup. (`scRepertoire` is loaded the same
  way — see above.) Dropped the genuinely unused `ape` and `readr`, and synced
  the nix environment. `viridis` is also no longer a dependency, but that was a
  *substitution*, not removal of dead code: the two `viridis::scale_fill_viridis()`
  calls became `ggplot2::scale_fill_viridis_c()` (the same viridis palette via
  viridisLite). The continuous interpolation differs very slightly — the palette
  midpoint shifts `#21908D` → `#2B9089` — so expression colour maps are
  near-identical but not byte-for-byte.
- **Cached static assets and single projection-engine load.** Static UI assets
  are now cached and the shared projection engine is loaded once rather than per
  tab, reducing repeated work on startup and tab switches.
- **No more Immune Repertoire plot flashing** when switching between the tab's
  sub-tabs (the theme fade-in animation no longer re-runs on already-rendered
  plots).

# CerebroNexus 3.0.0

## Renamed package and application

- Unified the R package, Shiny application, and GitHub repository under the
  `CerebroNexus` name. Install from `mihem/CerebroNexus` and load the package
  with `library(CerebroNexus)`.
- Kept the established Cerebro data model and public API, including `.crb`
  files, `Cerebro`, `Cerebro.options`, `launchCerebro()`, and
  `convertSeuratToCerebro()`.
- Updated the HLA export metadata field to `CerebroNexus_version`.

# Version 2.3.0

## HLA & TCR Motifs

- **New page: HLA & TCR Motifs.** A standalone top-level tab (peer of Immune
  Repertoire) that draws a CDR3 motif network — every unique CDR3 is a node and
  an edge joins two equal-length CDR3s at Hamming distance 1 — alongside
  donor-level HLA context. It appears conditionally, only when the loaded `.crb`
  carries a TRA/TRB chain. Three sub-tabs: **Motif Network** (colour by motif
  cluster, cell type, MHC context, HLA carrier status, or sample of origin),
  **HLA Associations** (descriptive carrier vs. non-carrier overlap — no p-value,
  no restriction claim), and **Data & QC** (coverage, normalized typing, and a
  session-only HLA upload).
- **HLA typing on the data class.** `Cerebro` gained an optional
  `hla_typing` slot with `addHLATyping()` / `getHLATyping()`; the getter is
  backward-compatible with older `.crb` files. Typing accepts a canonical long
  table, a wide `sample` + `HLA-*_1/_2` table, or a named list, and carries
  provenance (`genotyped` / `imputed` / `synthetic` / `unknown`) so a synthetic
  or imputed genotype is never treated as directly typed.
- **Export picks it up automatically.** `exportFromSeurat()` now reads
  `object@misc$hla_typing` (with `object@misc$hla_typing_source_type`), parallel
  to the existing `immune_repertoire` slot.
- **Declared contracts for bulk data.** A `.crb` may declare
  `observation_unit`, `receptor_key`, and `tcr_selection` in `technical_info`, so
  the page states honestly when rows are analysis units rather than cells, when a
  receptor is keyed by V gene + CDR3, and when a carrier contrast is a positive
  control rather than independent evidence.
- **One demo data set.** `demo_hla_tcr_dextramer.crb`: 12,000 real CD8+ T cells,
  every one with a paired αβ clonotype, plus the donors' published HLA
  genotypes, from 10x Genomics' dextramer cohort (Zhang et al., *Sci Adv* 2021,
  CC-BY). The repertoire is antigen-selected, which is what makes its motif
  network legible on measured sequences, where an unselected repertoire gives a
  handful of disconnected dots. Class I only (sorted CD8+), so the Class I ×
  Class II pair scope stays hidden on this demo and appears when a data set
  carries Class II typing plus a lineage column.
  The per-cell `dextramer_*` columns are 10x's **raw binder calls for a
  reagent**, not validated peptide specificity: staining is heavily
  cross-reactive here, and a `restriction_in_genotype` (`yes` / `no` /
  `unknown`) column ships beside them so that is visible in the app rather than
  only in the vignette. `unknown` is not padding: table S1 publishes one HLA-B
  allele for two donors, and absence from an incompletely called locus is not
  evidence of absence. The HLA association contrasts use the published genotypes
  and are therefore not circular — though the repertoire was still captured by a
  reagent panel, so ascertainment and donor/panel confounding remain, which the
  caveat above the tables now states.
- **Three new vignettes.** *"HLA & TCR Motifs: from synthetic data to an
  interactive app"* (single-cell, runnable end to end), *"HLA Associations on
  bulk TCRβ with real donor HLA"* (bring your own bulk cohort, with its
  positive-control caveat), and *"Antigen-selected single-cell TCR"* (the
  shipped demo's full download → `.crb` pipeline).

# Version 2.2.0

## Trekker single-cell spatial mapping

- **New page: Trekker.** A standalone top-level tab (peer of Spatial) for Curio
  Bioscience / Takara Bio Trekker single-cell spatial-mapping output — real single
  nuclei × whole transcriptome, with positions inferred from bead spatial barcodes
  and (usually) no matched histology image. It appears conditionally, only when the
  loaded `.crb` carries a `trekker` slot.
- **Physical space and transcriptome space, linked.** Because every nucleus has
  both a spatial and a UMAP position, the two scatterplots are shown side by side
  and cross-linked: a box- or lasso-select in one pane highlights the same nuclei
  in the other, and hovering a nucleus rings the same cell in both panes so its
  location reads off instantly. A toolbar matching the app's plotly modebar
  (box / lasso select, pan, zoom, reset, download) sits over the panes. The View
  control switches between side-by-side, a single enlarged Spatial-only or
  UMAP-only pane, and a Transition view that animates each nucleus from its UMAP
  position to its physical position. Colour by cell type, cluster, or any
  of the whole-transcriptome genes, and use the **Group filters** panel (the same
  per-grouping pickers as the projection tabs) to restrict the view to selected
  cell types or clusters.
- **Positioning evidence is auditable.** Trekker positions are inferred, so the page
  surfaces the vendor's per-nucleus positioning-evidence images (the bead-barcode
  cloud plus a UMI knee plot) for the sampled nuclei, and a Cell inspector reports
  each nucleus's identity together with the **real** physical-neighbour cell-type
  counts within a chosen radius (not a deconvolution estimate).
- **Colour the physical map by any per-cell value — analysis, not just a view.**
  Beyond cell type / cluster / gene, the page colours by **cross-space metrics**
  computed on the full positioned set — spatial-neighbourhood purity and
  expression-vs-physical neighbourhood concordance (who forms tight anatomical
  domains vs. who is dispersed or infiltrating, with a per-cell-type summary and
  the honest reminder that in a healthy brain low microglial purity is baseline
  tiling, not activation) — and by **any numeric per-cell meta column** the object
  carries (pseudotime, a signature/module score, velocity magnitude, a signaling
  score), so an existing single-cell analysis result gains a physical-space
  projection with no page change. The demo ships a myelination signature score as
  a worked example. Only Trekker can do this: it needs true single-cell identity
  and true physical position for the same nuclei.
- **Canonical coordinates.** The panes use the vendor's Location CSV, the canonical
  coordinate authority. The generic `@images` slot is axis-transposed and the
  `SPATIAL` reduction is y-mirrored relative to it; the vignette documents that
  discrepancy so the tissue is never silently drawn rotated.
- **Honest QC.** Positioning QC is shown in the vendor's own field names (a missing
  metric stays missing); "confidently positioned" is disclosed as including
  vendor-salvaged multi-location nuclei (labelled `vendor_confidently_positioned`);
  values below the vendor's reference range are flagged without adjudicating sample
  usability; and Moran's I is the upstream vendor value, labelled and never mixed
  with Cerebro's own.
- **Data class.** `Cerebro` gained an optional `trekker` slot with
  `addTrekker()` / `getTrekker()`; the getter is backward-compatible with older
  `.crb` files that predate the field.
- **Demo data and vignette.** A real, down-sampled demo `.crb`
  (`demo_trekker.crb`, from the smallest official Trekker bundle: 2,532 nuclei ×
  all 21,374 genes, with positioning-evidence images embedded) and a runnable
  vignette, *"Trekker single-cell spatial mapping: from a vendor bundle to an
  interactive app"*, covering the registration-gated download, the bundle
  contents, and the reproducible build (`data-raw/build_trekker_demo.R`).

# Version 2.1.1

## Robustness and interface

- **Plots fill the viewport.** Projection and other plot panels grow to fit the
  available height through a single shared mechanism, so tall screens no longer
  leave large empty bands.
- **Unified info buttons and tidier styling.** The per-tab info buttons were
  consolidated onto one shared component and assorted inline CSS moved into the
  stylesheet.

## Fixes

- **Spatial axis sliders.** Guard against empty coordinate ranges so the axis
  range sliders no longer emit `Inf`/`-Inf` warnings on data sets without
  spatial coordinates.

## Testing / CI

- Raise the default shinytest2 `load_timeout` to 60s and wait for
  asynchronously inserted tabs before navigating, de-flaking the app tests on
  slower runners.
- Preserve the last shinytest2 output error when a retry times out, so failures
  report their original cause instead of an unexplained `NULL` value.

# Version 2.1.0

## Projection overhaul, unified interface, and cross-tab selection

- **Shared projection renderer**: the Overview, Gene expression, Trajectory and
  Clonal UMAP scatterplots now share one WebGL renderer instead of per-tab
  copies, so sizing, legend, hover and selection behave consistently across
  tabs. Each projection sizes itself to the available viewport and no longer
  flashes at the wrong size on first paint.
- **Cell selection**: box- and lasso-select persist across parameter changes,
  with a Clear button and a zoom-to-selection toggle on every projection tab.
  Hiding a group in the legend also excludes it from selected-cell counts, and
  the plot toolbar (lasso / box-select / zoom / pan / reset / PNG download) is
  available again.
- **Interactive Clonal Diversity**: the Clonal Diversity plot is now an
  interactive figure — hovering a point shows that group's bootstrap value.
- **Interface**: a lighter "Console" visual language with coloured sidebar
  icons, one warm palette shared by every chart (plotly and ggplot), and a fluid
  projection layout that reclaims the space freed by the removed top bar. A
  floating menu button keeps the sidebar reachable on phone-sized screens.
- **Render feedback**: a parameter change dims the projection while the new
  render is in flight, and the sliders are debounced so dragging no longer fires
  a render per step.
- **Fewer empty tabs**: the Marker genes and Most expressed genes sidebar items
  appear only for datasets that carry them (e.g. hidden for the spatial demos).
- **Fixes**: the spatial histology background is no longer cleared when another
  tab renders; the Clonal UMAP host reveals correctly after faceting is toggled;
  gene-expression multi-panel selection is restored; hidden-group state stays in
  sync with the server across re-renders; and the trajectory projection keeps
  its view on redraw.

# Version 2.0.1

## Robustness, performance, and deprecation cleanup

- **Fixes**: table rendering no longer errors on selected-cell slices whose
  `percent_mt` / `percent_ribo` columns are all `NA`, and the details table
  tolerates the transient `NULL` / `NA` a `materialSwitch` can emit while its UI
  re-renders; the group-centre helper returns an empty result instead of
  crashing when its grouping column is missing.
- **Plot caching**: projection hover-info, the groups and trajectory
  expression-metric violins, the groups composition bar/Sankey plot, and the
  Moran's I score are now cached per dataset (session-scoped, invalidated on
  dataset switch), so switching genes or re-rendering no longer recomputes them.
  The pseudotime plot selects `scattergl` up-front instead of converting the
  whole figure afterwards.
- **Deprecations**: replaced `aes_string()` with the `.data[[ ]]` pronoun and
  wrapped tidy-select group variables in `all_of()`, which also removes the
  per-call warning and roughly halves the composition cross-tabulation on large
  tables.

# Version 2.0.0

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

- **Spatial data layer**: the `Cerebro` class gains a `spatial` field with
  `addSpatialData()`, `getSpatialData()`, and `availableSpatial()` accessors.
- **Export support**: `exportFromSeurat()` now extracts spatial coordinates and
  expression from Seurat v5 image slots (Visium / Xenium / FOV) via the internal
  `.getSpatialData()` helper, storing them per image in the exported `.crb`.
- **Utility wrappers**: added `availableSpatial()`, `getSpatialData()`, and
  `serverSideGeneSelector()` in the Shiny utility layer.
- **Demo dataset**: bundled a synthetic Xenium spatial demo
  (`demo_spatial.crb`, 1,000 cells) as a fifth demo dataset.

# Version 1.7.8

## Trajectory tab

- **Trajectory module**: restores the pseudotime trajectory explorer from the
  original v1.3 implementation (projection coloured by state/pseudotime, states by
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

# Version 1.7.7

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

# Version 1.7.6

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

# Version 1.7.5

## Immune repertoire

- **Clonal UMAP**: axes now match the main projection style — `UMAP_1`/`UMAP_2`
  titles removed, with boxed/mirrored axes (showline + mirror) and autorange,
  so the Clonal UMAP sits visually consistent with the main projection tab.

# Version 1.7.4

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

# Version 1.7.3

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

# Version 1.7.2

## Enhanced modules

- Added a Most expressed genes tab for exploring per-group gene expression
  summaries exported with `.crb` files.
- Added an Enriched pathways tab for browsing pathway enrichment results.
- Added an Extra material tab for exported tables and plots.
- Added utility wrappers for exporting most expressed genes, enriched pathways,
  extra tables, and extra plots.
- Added tests and vignettes covering the new Shiny modules and export helpers.

# Version 1.7.1

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

# Version 1.7.0

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
- Fixed "method not found" errors on the trajectory tab by renaming the corresponding `Cerebro` methods to the names the Shiny server already calls (`getMethodsForTrajectories`, `getNamesOfTrajectories`)
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

# Version 1.6.0

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

- Added pkgdown site at <https://mihem.github.io/CerebroNexus/> with light/dark/auto theme switch, search, and all vignettes as articles
- Site automatically builds and deploys to GitHub Pages on push to master

# Version 1.5.3

- several bug fixes so that launchCerebro should work again

# Version 1.5.2

- allow plot settings (size, opacity, number of cells to show) to be different in gene expression and overview (useful for large datasets with slow gene expression)

# Version 1.5.1

- remove unused functions in group

# Version 1.5.0

- make compatible with Seuratv5, especially with BPCells Matrix

# Version 1.4.1

- timeout function added. This logs out the user after 600 second of inactivity (can be changed in `shiny_ui.R`). The JS function was taken from https://stackoverflow.com/a/53207050/21417317.
- add option to show up to 1000 cells in `Main`, which is useful for exports.

# Version 1.4.0

This was the first update of the lightweight fork that continued the main functionality after its predecessor was discontinued.

## Major changes

- remove enriched pathways, extra material, most expressed genes and trajectory functions since the goal of this fork is to continue with a lightweight version

## Minor changes

- `Load Data` is renamed to `Data info` and `Overview` to `Main`
- Preferences about WebGL and hover info are now show in the first tab called `Data info`
- more colorful boxes for the sample information
- different icons for tabs `Data info`, `Main`, `Groups` and `Marker Groups`

# Legacy release 1.3.1

Despite the minor version bump, this update contains substantial performance improvements in the Shiny app, specifically in the projections.

## Major changes

- Projections in the "Overview" and "Gene (set) expression" are now updated using the `Plotly.react()` Javascript function instead of redrawn from scratch inside R when changing the input variables. For the user, that means that (1) plots are drawn much quicker and (2) the current zoom/pan settings are maintained when switching plot parameters (coloring variable, point size/opacity, etc).

## Minor changes

- It became possible to define several settings related to the projections shown in the "Overview" and "Gene (set) expression" tabs, including default point size, opacity, sampling percentage, and hover information.
- Hover/tooltip info for cells in projections can be deactivated through a checkbox on the "About" tab. Deactivating hover info increases performance of projections.
- Hover/tooltip info for cells in the gene expression projection no longer contain the gene expression value. This is because preparing the hover info is an expensive computation with little return. As a result of removing the gene expression value, the hover info does not need to be recalculated every time a gene is added to or removed from the list of genes to show expression for. For the same reason, when plotting a trajectory, the state and pseudotime are not added to the hover info either.
- Internally, data for plotting in projections is rearranged, stored in different variables, and the final output is debounced to avoid unnecessary redrawing of the projections on initialization.
- The feature to show expression of multiple genes in separate panels has been matured. Up to 9 genes can be shown in a 3x3 panel matrix but all share the same color scale. While cells can be selected in any of the panels, the expression levels shown in the other UI element, e.g. table of selected cells or expression by group, refers to the mean expression of all selected genes (not just the one the cells were selected in).
- When coloring cells in projections by a caterogical variable, e.g. cell type, the dots in the legend are now larger and independent from the selected point size.
- Tables are now rendered server-side to improve performance for large tables.
- Cellular barcodes in tables of selected cells are formatted in monospace font.
- Columns in meta data tables, e.g. table of cells selected in projections, which are identified to contain percentage on a 0-100 scale are changed to a 0-1 scale to prevent non-sensical values such as 500%.
- Add comma to Y axis and hover info in bar chart of selected cells in projection ("Overview" tab).
- It became possible to supply a preloaded `Cerebro` object through the launch configuration, avoiding repeated disk reads in closed-mode hosting.
- Update author info in "About" tab.

## Fixes

- Colors assigned to groups in bar chart of selected cells in projection ("Overview" tab) sometimes did not match those shown in the projection. This only applied to categorical grouping variables that are not registered as grouping variables.
- Update Enrichr API for `getEnrichedPathways()` function. Make it configurable in case of further changes to the API.

# Legacy release 1.3.0

This historical release had a dedicated article; it is no longer part of the current documentation site.

## Major changes

- With data sets becoming more complex, users often have more than just the two grouping variables Cerebro was initially made to work with ('sample' and 'cluster'). To provide a more generalized interface, users can now specify multiple grouping variables (or a single one). Consequently, the 'Samples' and 'Clusters' tabs in the Cerebro interface have been replaced by the 'Groups' tab, where users can select one of the available grouping variables (with the same content as before). This can be useful when you cluster the cells with different methods/settings or have additional grouping variables, such as treatments, and want to provide the Cerebro user with both results.
- Data loaded into Cerebro is now stored in a dedicated class: `Cerebro`.
- This historical data-structure change required matching releases of the original exporter and viewer.
- Removed support for Seurat objects before v3.0. Users could update a Seurat object with `Seurat::UpdateSeuratObject()` before export.
- The "Gene expression" and "Gene set expression" tabs have been merged into the new "Gene (set) expression)" which gives you access to both.
- The new "Extra material" tab allows you to export additional material related to the data set that you want to share with others. At the moment, only tables and plots (from ggplot2) are supported, but support for other types of content can be added upon user request in the future.

## New features

- It is now possible to export single cell data stored in `SingleCellExperiment` (SCE) objects.
- Gene (set) expression can now also be visualized in trajectories (generated by Monocle 2).
- `NA` values for cell assignment to one of the specified grouping variables will be replaced by `N/A` and put into a separate group ("N/A") when exporting the data.

# Legacy release 1.2.2

## Fixes

- The title in the browser tab now correctly says "Cerebro" instead of containing some HTML code.
- Cluster trees should now be displayed correctly.
- `getEnrichedPathways()` no longer results in an error when marker genes are present but no database returns any enriched pathways, e.g. because there are too few marker genes. Thanks to @turkeyri for pointing it out and suggesting a solution!

# Legacy release 1.2.1

## New features

- It is now possible to select cells in the dimensional reduction plots ('Overview', 'Gene expression', and 'Gene set expression' tabs) and retrieve additional info for them. For example, users can get tables of meta data or expression values and save them as a file for further analysis. Also, gene expression can be shown in the selected vs. non-selected cells.

## Minor changes

- Scales for expression levels by sample and cluster in "Gene expression" and "Gene set expression" tabs are now set to be from 0 to 1.2 times the highest value. This is to limit the violin plots which cannot be trimmed to the actual data range and will extend beyond, giving a false impression of negative values existing in the data.
- Hover info in expression by gene plot in "Gene expression" and "Gene set expression" tabs now show both the gene name and the mean expression value instead of just the gene name.

# Legacy release 1.2.0

## New features

- New button for composition plots (e.g. samples by clusters or cell cycle) that allows to choose whether to scale by actual cell count (default) or percentage.
- New button for composition plots that allows to show/hide the respective table of numbers behind them.
- New tab "Color management": Users can now change the color assigned to each sample/cluster.
- "Gene expression" and "Gene set expression" panels: Users can now pick from a set of color scales and adjust the color range.
- The gene selection box in the "Gene expression" panel will now allow to view available genes and select them by clicking. It is not necessary anymore to hit Enter or Space to update the plot, this will be done automatically after providing new input.
- It is now possible to export assays other than `RNA` through the `assay` parameter in relevant functions.
- Launch old Cerebro interfaces through `version` parameter in `launchCerebro()`.
- A vignette was added to explain the original export functions.

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

# Legacy release 1.1.0

- Release along with manuscript revision.

## New features

- New function `extractMonocleTrajectory()`: Users can extract data from trajectories calculated with Monocle v2.
- New tab "Trajectories": Allows visualization of trajectories calculated with Monocle v2.

# Legacy release 1.0.0

- Public release along with manuscript submission to bioRxiv.
