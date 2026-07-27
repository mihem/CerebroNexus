# Changelog

## CerebroNexus 3.0.2

### Interface and documentation

- **The wordmark renders identically on every machine.** The sidebar
  logo was an SVG built from `<text>` elements carrying a `font-family`
  list, so the browser substituted whatever font it happened to have and
  the mark changed width and weight from one machine to the next. It is
  now vector outlines (Fredoka, SIL OFL) kept in
  `inst/shiny/v1.4/www/cerebronexus.svg`, so it no longer depends on any
  installed font. The `Nexus` half also moves from the old Bootstrap
  blue to the amber accent used throughout the rest of the interface.
- **The README documents the analysis modules.** Its feature section
  previously covered only the export API, so Immune repertoire, Spatial,
  Trekker, HLA & TCR Motifs and Trajectory went unmentioned. Each now
  states what a `.crb` must carry for its tab to appear, with a link to
  the corresponding guide.

## CerebroNexus 3.0.1

### Faster startup

- **Deferred `scRepertoire` loading.** The Immune Repertoire settings
  render on the first flush (they are `suspendWhenHidden = FALSE`),
  which previously forced `scRepertoire` — and, through its imports, ~90
  packages and several seconds of `lazyLoadDBfetch` — to load at startup
  even when the tab was never opened. Availability is now probed with
  [`system.file()`](https://rdrr.io/r/base/system.file.html) (a
  disk-path lookup, no namespace load); the namespace is loaded lazily
  on the first scRepertoire-backed plot — self-made plots (Clone
  Sharing, Definition) and the default Clonal UMAP never trigger it.
  Repertoire figures are unchanged — they are still computed by
  `scRepertoire`.
- **True lazy loading — no background prewarm.** `scRepertoire` is
  loaded only when the first scRepertoire-backed plot is drawn. There is
  intentionally no background prewarm:
  [`later::later()`](https://later.r-lib.org/reference/later.html) is
  cooperative scheduling on Shiny’s single R thread, so loading the
  ~90-package tree “in the background” would still block the event loop
  and freeze every session in the process for several seconds. The
  trade-off is explicit and honest — app startup never pays for
  `scRepertoire`, and a repertoire user waits once (several seconds —
  the full namespace load) on their first scRepertoire plot. Because a
  namespace is process-wide, it then stays warm for every session in
  that R worker (not only the one that triggered it — which also means
  that first load briefly blocks the other sessions sharing the
  process). Measured: startup drops ~50% (median 8.2 s → 4.0 s) while
  the first Abundance plot rises correspondingly (1.4 s → 5.2 s), so the
  two roughly cancel — the cost is moved off startup, not removed.
- **Heavy dependencies load on demand.** All dependencies stay
  mandatory, so a standard install gives every feature out of the box;
  other heavy packages (`GSVA`, `biomaRt`, `httr`, `qvalue`,
  `future.apply`, `pbapply`) are accessed via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) and loaded
  only when the feature that needs them is first used, not at
  package/app startup. (`scRepertoire` is loaded the same way — see
  above.) Dropped the genuinely unused `ape` and `readr`, and synced the
  nix environment. `viridis` is also no longer a dependency, but that
  was a *substitution*, not removal of dead code: the two
  [`viridis::scale_fill_viridis()`](https://sjmgarnier.github.io/viridis/reference/scale_viridis.html)
  calls became
  [`ggplot2::scale_fill_viridis_c()`](https://ggplot2.tidyverse.org/reference/scale_viridis.html)
  (the same viridis palette via viridisLite). The continuous
  interpolation differs very slightly — the palette midpoint shifts
  `#21908D` → `#2B9089` — so expression colour maps are near-identical
  but not byte-for-byte.
- **Cached static assets and single projection-engine load.** Static UI
  assets are now cached and the shared projection engine is loaded once
  rather than per tab, reducing repeated work on startup and tab
  switches.
- **No more Immune Repertoire plot flashing** when switching between the
  tab’s sub-tabs (the theme fade-in animation no longer re-runs on
  already-rendered plots).

## CerebroNexus 3.0.0

### Renamed package and application

- Unified the R package, Shiny application, and GitHub repository under
  the `CerebroNexus` name. Install from `mihem/CerebroNexus` and load
  the package with
  [`library(CerebroNexus)`](https://mihem.github.io/CerebroNexus/).
- Kept the established Cerebro data model and public API, including
  `.crb` files, `Cerebro_v1.3`, `Cerebro.options`,
  [`launchCerebro()`](https://mihem.github.io/CerebroNexus/reference/launchCerebro.md),
  and
  [`convertSeuratToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md).
- Updated the HLA export metadata field to `CerebroNexus_version`.

## Version 2.3.0

### HLA & TCR Motifs

- **New page: HLA & TCR Motifs.** A standalone top-level tab (peer of
  Immune Repertoire) that draws a CDR3 motif network — every unique CDR3
  is a node and an edge joins two equal-length CDR3s at Hamming distance
  1 — alongside donor-level HLA context. It appears conditionally, only
  when the loaded `.crb` carries a TRA/TRB chain. Three sub-tabs:
  **Motif Network** (colour by motif cluster, cell type, MHC context,
  HLA carrier status, or sample of origin), **HLA Associations**
  (descriptive carrier vs. non-carrier overlap — no p-value, no
  restriction claim), and **Data & QC** (coverage, normalized typing,
  and a session-only HLA upload).
- **HLA typing on the data class.** `Cerebro_v1.3` gained an optional
  `hla_typing` slot with `addHLATyping()` / `getHLATyping()`; the getter
  is backward-compatible with older `.crb` files. Typing accepts a
  canonical long table, a wide `sample` + `HLA-*_1/_2` table, or a named
  list, and carries provenance (`genotyped` / `imputed` / `synthetic` /
  `unknown`) so a synthetic or imputed genotype is never treated as
  directly typed.
- **Export picks it up automatically.**
  [`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
  now reads `object@misc$hla_typing` (with
  `object@misc$hla_typing_source_type`), parallel to the existing
  `immune_repertoire` slot.
- **Declared contracts for bulk data.** A `.crb` may declare
  `observation_unit`, `receptor_key`, and `tcr_selection` in
  `technical_info`, so the page states honestly when rows are analysis
  units rather than cells, when a receptor is keyed by V gene + CDR3,
  and when a carrier contrast is a positive control rather than
  independent evidence.
- **One demo data set.** `demo_hla_tcr_dextramer.crb`: 12,000 real CD8+
  T cells, every one with a paired αβ clonotype, plus the donors’
  published HLA genotypes, from 10x Genomics’ dextramer cohort (Zhang et
  al., *Sci Adv* 2021, CC-BY). The repertoire is antigen-selected, which
  is what makes its motif network legible on measured sequences, where
  an unselected repertoire gives a handful of disconnected dots. Class I
  only (sorted CD8+), so the Class I × Class II pair scope stays hidden
  on this demo and appears when a data set carries Class II typing plus
  a lineage column. The per-cell `dextramer_*` columns are 10x’s **raw
  binder calls for a reagent**, not validated peptide specificity:
  staining is heavily cross-reactive here, and a
  `restriction_in_genotype` (`yes` / `no` / `unknown`) column ships
  beside them so that is visible in the app rather than only in the
  vignette. `unknown` is not padding: table S1 publishes one HLA-B
  allele for two donors, and absence from an incompletely called locus
  is not evidence of absence. The HLA association contrasts use the
  published genotypes and are therefore not circular — though the
  repertoire was still captured by a reagent panel, so ascertainment and
  donor/panel confounding remain, which the caveat above the tables now
  states.
- **Three new vignettes.** *“HLA & TCR Motifs: from synthetic data to an
  interactive app”* (single-cell, runnable end to end), *“HLA
  Associations on bulk TCRβ with real donor HLA”* (bring your own bulk
  cohort, with its positive-control caveat), and *“Antigen-selected
  single-cell TCR”* (the shipped demo’s full download → `.crb`
  pipeline).

## Version 2.2.0

### Trekker single-cell spatial mapping

- **New page: Trekker.** A standalone top-level tab (peer of Spatial)
  for Curio Bioscience / Takara Bio Trekker single-cell spatial-mapping
  output — real single nuclei × whole transcriptome, with positions
  inferred from bead spatial barcodes and (usually) no matched histology
  image. It appears conditionally, only when the loaded `.crb` carries a
  `trekker` slot.
- **Physical space and transcriptome space, linked.** Because every
  nucleus has both a spatial and a UMAP position, the two scatterplots
  are shown side by side and cross-linked: a box- or lasso-select in one
  pane highlights the same nuclei in the other, and hovering a nucleus
  rings the same cell in both panes so its location reads off instantly.
  A toolbar matching the app’s plotly modebar (box / lasso select, pan,
  zoom, reset, download) sits over the panes. The View control switches
  between side-by-side, a single enlarged Spatial-only or UMAP-only
  pane, and a Transition view that animates each nucleus from its UMAP
  position to its physical position. Colour by cell type, cluster, or
  any of the whole-transcriptome genes, and use the **Group filters**
  panel (the same per-grouping pickers as the projection tabs) to
  restrict the view to selected cell types or clusters.
- **Positioning evidence is auditable.** Trekker positions are inferred,
  so the page surfaces the vendor’s per-nucleus positioning-evidence
  images (the bead-barcode cloud plus a UMI knee plot) for the sampled
  nuclei, and a Cell inspector reports each nucleus’s identity together
  with the **real** physical-neighbour cell-type counts within a chosen
  radius (not a deconvolution estimate).
- **Colour the physical map by any per-cell value — analysis, not just a
  view.** Beyond cell type / cluster / gene, the page colours by
  **cross-space metrics** computed on the full positioned set —
  spatial-neighbourhood purity and expression-vs-physical neighbourhood
  concordance (who forms tight anatomical domains vs. who is dispersed
  or infiltrating, with a per-cell-type summary and the honest reminder
  that in a healthy brain low microglial purity is baseline tiling, not
  activation) — and by **any numeric per-cell meta column** the object
  carries (pseudotime, a signature/module score, velocity magnitude, a
  signaling score), so an existing single-cell analysis result gains a
  physical-space projection with no page change. The demo ships a
  myelination signature score as a worked example. Only Trekker can do
  this: it needs true single-cell identity and true physical position
  for the same nuclei.
- **Canonical coordinates.** The panes use the vendor’s Location CSV,
  the canonical coordinate authority. The generic `@images` slot is
  axis-transposed and the `SPATIAL` reduction is y-mirrored relative to
  it; the vignette documents that discrepancy so the tissue is never
  silently drawn rotated.
- **Honest QC.** Positioning QC is shown in the vendor’s own field names
  (a missing metric stays missing); “confidently positioned” is
  disclosed as including vendor-salvaged multi-location nuclei (labelled
  `vendor_confidently_positioned`); values below the vendor’s reference
  range are flagged without adjudicating sample usability; and Moran’s I
  is the upstream vendor value, labelled and never mixed with Cerebro’s
  own.
- **Data class.** `Cerebro_v1.3` gained an optional `trekker` slot with
  `addTrekker()` / `getTrekker()`; the getter is backward-compatible
  with older `.crb` files that predate the field.
- **Demo data and vignette.** A real, down-sampled demo `.crb`
  (`demo_trekker.crb`, from the smallest official Trekker bundle: 2,532
  nuclei × all 21,374 genes, with positioning-evidence images embedded)
  and a runnable vignette, *“Trekker single-cell spatial mapping: from a
  vendor bundle to an interactive app”*, covering the registration-gated
  download, the bundle contents, and the reproducible build
  (`data-raw/build_trekker_demo.R`).

## Version 2.1.1

### Robustness and interface

- **Plots fill the viewport.** Projection and other plot panels grow to
  fit the available height through a single shared mechanism, so tall
  screens no longer leave large empty bands.
- **Unified info buttons and tidier styling.** The per-tab info buttons
  were consolidated onto one shared component and assorted inline CSS
  moved into the stylesheet.

### Fixes

- **Spatial axis sliders.** Guard against empty coordinate ranges so the
  axis range sliders no longer emit `Inf`/`-Inf` warnings on data sets
  without spatial coordinates.

### Testing / CI

- Raise the default shinytest2 `load_timeout` to 60s and wait for
  asynchronously inserted tabs before navigating, de-flaking the app
  tests on slower runners.
- Preserve the last shinytest2 output error when a retry times out, so
  failures report their original cause instead of an unexplained `NULL`
  value.

## Version 2.1.0

### Projection overhaul, unified interface, and cross-tab selection

- **Shared projection renderer**: the Overview, Gene expression,
  Trajectory and Clonal UMAP scatterplots now share one WebGL renderer
  instead of per-tab copies, so sizing, legend, hover and selection
  behave consistently across tabs. Each projection sizes itself to the
  available viewport and no longer flashes at the wrong size on first
  paint.
- **Cell selection**: box- and lasso-select persist across parameter
  changes, with a Clear button and a zoom-to-selection toggle on every
  projection tab. Hiding a group in the legend also excludes it from
  selected-cell counts, and the plot toolbar (lasso / box-select / zoom
  / pan / reset / PNG download) is available again.
- **Interactive Clonal Diversity**: the Clonal Diversity plot is now an
  interactive figure — hovering a point shows that group’s bootstrap
  value.
- **Interface**: a lighter “Console” visual language with coloured
  sidebar icons, one warm palette shared by every chart (plotly and
  ggplot), and a fluid projection layout that reclaims the space freed
  by the removed top bar. A floating menu button keeps the sidebar
  reachable on phone-sized screens.
- **Render feedback**: a parameter change dims the projection while the
  new render is in flight, and the sliders are debounced so dragging no
  longer fires a render per step.
- **Fewer empty tabs**: the Marker genes and Most expressed genes
  sidebar items appear only for datasets that carry them (e.g. hidden
  for the spatial demos).
- **Fixes**: the spatial histology background is no longer cleared when
  another tab renders; the Clonal UMAP host reveals correctly after
  faceting is toggled; gene-expression multi-panel selection is
  restored; hidden-group state stays in sync with the server across
  re-renders; and the trajectory projection keeps its view on redraw.

## Version 2.0.1

### Robustness, performance, and deprecation cleanup

- **Fixes**: table rendering no longer errors on selected-cell slices
  whose `percent_mt` / `percent_ribo` columns are all `NA`, and the
  details table tolerates the transient `NULL` / `NA` a `materialSwitch`
  can emit while its UI re-renders; the group-centre helper returns an
  empty result instead of crashing when its grouping column is missing.
- **Plot caching**: projection hover-info, the groups and trajectory
  expression-metric violins, the groups composition bar/Sankey plot, and
  the Moran’s I score are now cached per dataset (session-scoped,
  invalidated on dataset switch), so switching genes or re-rendering no
  longer recomputes them. The pseudotime plot selects `scattergl`
  up-front instead of converting the whole figure afterwards.
- **Deprecations**: replaced
  [`aes_string()`](https://ggplot2.tidyverse.org/reference/aes_.html)
  with the `.data[[ ]]` pronoun and wrapped tidy-select group variables
  in [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
  which also removes the per-call warning and roughly halves the
  composition cross-tabulation on large tables.

## Version 2.0.0

### Spatial analysis and overlay improvements

- **Multi-gene co-expression**: a new “Co-expression (RGB)” plot type
  maps up to three genes onto the red / green / blue channels, so each
  cell’s colour blends the genes it expresses and spatial
  co-localisation reads as a mixed hue.
- **Spatial autocorrelation**: ImageFeaturePlot now reports the
  displayed gene’s Moran’s I — how spatially clustered its expression is
  (large slides are down-sampled for a responsive, stable score).
- **Region outlines**: an opt-in toggle outlines each colour group’s
  spatial region with its convex hull.
- **Copy alignment as preset**: after hand-aligning a histology overlay,
  a button emits the matching `spatial_images_*` `Cerebro.options` lines
  to paste into an app so the dataset ships pre-aligned.
- **Honest single-source overlay scale**: the background scale is now
  applied once (a squared-scale bug is fixed), the image is clipped to
  the plot area so it no longer covers the axes, and the default view is
  evenly framed.
- **Overlay controls UX**: interacting with any Additional-parameters
  control collapses the Main-parameters box, and the Additional panel
  scrolls internally (hidden scrollbar with soft top/bottom fades), so
  the plot stays visible while adjusting Move/Rotate.
- **Fixes**: switching from an image-bearing platform to a bead-only one
  (Slide-seq) no longer leaves a stale tissue image behind, and the
  embedded-image option is offered only for datasets that actually carry
  one.

### Spatial transcriptomics (interactive tab + histology overlay)

- **Spatial tab**: the interactive Spatial projection is now wired into
  the app. It mounts conditionally (via `insertConditionalTab()`)
  whenever the loaded dataset carries spatial data, with plotly-based
  coloring, group filters, and box/lasso cell selection.
- **Histology background overlay**:
  [`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
  gains `spatial_images` plus per-dataset `spatial_images_flip_x`,
  `spatial_images_flip_y`, `spatial_images_scale_x`,
  `spatial_images_scale_y`, and `spatial_plot_rotation` parameters.
  Matched images are copied into the app bundle and shown behind the
  cells, controlled by a **Background image** dropdown and an **Image
  opacity** slider. Unmatched entries are ignored with a warning rather
  than an error.
- **Bundled demo**: the “Cortex - Spatial (synthetic)” demo pairs fully
  synthetic cortical-depth cell coordinates (illustrative cell-type
  labels such as Excitatory L2/3 … Oligodendrocyte) with a synthetic H&E
  cortex-section SVG whose layer bands align with the cells, so cell
  types visibly stratify across the cortex out of the box. Both the
  coordinates and the image are synthetic — no patient data.
- **Documentation**: added the
  [`vignette("spatial_analysis")`](https://mihem.github.io/CerebroNexus/articles/spatial_analysis.md)
  guide.
- **Bundled demo set**: the app now opens on `demo_full_tcr_bcr.crb`
  (PBMC, TCR + BCR + trajectory) plus four real spatial sections
  (Visium, Slide-seq v2, MERFISH, Xenium), so the dataset switcher spans
  immune-repertoire, trajectory, and spatial content. The two narrower
  PBMC subsets (`demo_healthy_t.crb`, `demo_bcell_rich.crb`) are no
  longer shipped — the Full set is their superset;
  `data-raw/build_ir_demos.R` can still rebuild them for a multi-sample
  demo.

### Spatial transcriptomics (backend)

- **Spatial data layer**: the `Cerebro_v1.3` class gains a `spatial`
  field with `addSpatialData()`, `getSpatialData()`, and
  `availableSpatial()` accessors.
- **Export support**:
  [`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
  now extracts spatial coordinates and expression from Seurat v5 image
  slots (Visium / Xenium / FOV) via the internal `.getSpatialData()`
  helper, storing them per image in the exported `.crb`.
- **Utility wrappers**: added `availableSpatial()`, `getSpatialData()`,
  and `serverSideGeneSelector()` in the Shiny utility layer.
- **Demo dataset**: bundled a synthetic Xenium spatial demo
  (`demo_spatial.crb`, 1,000 cells) as a fifth demo dataset.

## Version 1.7.8

### Trajectory tab

- **Trajectory module**: restores the pseudotime trajectory explorer
  from the original cerebroApp v1.3 (projection coloured by
  state/pseudotime, states by group, expression metrics along
  pseudotime, per-state gene/transcript counts). The code is Roman
  Hillje’s original implementation, restructured into the v1.4 sub-file
  layout with no functional change.
- **Conditional tab**: the Trajectory tab is inserted dynamically
  (`insertConditionalTab`) only for data sets whose `.crb` carries
  trajectory data — the same content-driven sidebar mechanism used by
  the Immune Repertoire and Extra material tabs.
- **Demo data**: the monocle2 pseudotime trajectory is now bundled
  inside the `demo_full_tcr_bcr.crb` demo (computed on its B-cell
  subset) instead of a separate `demo_trajectory.crb`, so one demo shows
  TCR + BCR + trajectory. The trajectory is reproducible via
  `data-raw/build_trajectory_demo.R`.

## Version 1.7.7

### Multiple data sets (multi-crb)

- **Dataset switcher**:
  [`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
  now accepts a named vector of several `.crb` files and renders a
  “Select dataset:” dropdown in the sidebar, letting users move between
  data sets without restarting the app. Single-file usage is unchanged
  and shows no switcher. By default the smallest file is loaded first
  (`crb_pick_smallest_file`, default `TRUE`).
- **URL selection**: a data set can be opened directly via the URL,
  matched by the name given in `cerebro_data` or by file basename —
  either as a query string (`?dataset=TCR`) or as the last path segment
  (`/TCR`).
- **Demo data sets**: three genuinely distinct demo `.crb` files ship in
  `inst/extdata/v1.4/` — `demo_full_tcr_bcr.crb` (all cells, TCR + BCR),
  `demo_healthy_t.crb` (T + monocytes, TCR) and `demo_bcell_rich.crb`
  (B-cell rich, BCR). They differ in cell composition, so the UMAP and
  cell-type mix change as you switch, and clonotypes are assigned by
  lineage (TCR to T cells, BCR to B cells) rather than at random.
  Group-level analyses (marker genes, most-expressed genes, enriched
  pathways) are filtered to the cell types kept in each subset, so the
  demos are internally consistent. Built from the public 10x Genomics
  `vdj_v1_hs_pbmc3` dataset; see `data-raw/README.md` for the
  reproducible build. The bundled app (`shiny::runApp("inst")`) now
  opens on these three data sets so the switcher is visible out of the
  box; pass a named vector to
  [`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
  for your own data (see
  [`vignette("multi_crb")`](https://mihem.github.io/CerebroNexus/articles/multi_crb.md)).
  New vignette: *Loading multiple data sets (multi-crb) with a dataset
  switcher*.

### Immune repertoire

- **Clonal UMAP** no longer renders blank when the Immune repertoire tab
  is opened after visiting another tab (e.g. Main). The plotly renderer
  was gated on server-reported plot dimensions, which are not yet
  available when its output element is created on tab switch; plotly
  sizes itself client-side, so that gate was removed.

## Version 1.7.6

### Immune repertoire

- **Clone Sharing tab**: classifies every clonotype (V+J+CDR3 of the
  active chain) as Private (in a single unit), Public within-group, or
  Public cross-group, using a configurable “sharing unit” (any
  categorical metadata column, default `sample`) and the active group
  column. With no group selected it degrades to Private / Shared.
  Interactive plotly bars with on-bar count/percentage labels and a
  clean hover tooltip (one class per bar, no raw aesthetic names).
- **Definition** (clone-definition resolution waterfall) is available
  but hidden from the default tab strip: it is an exploratory check for
  choosing a clone-call resolution rather than a reader-facing figure.
  Uncomment its `tabPanel` to re-enable.

## Version 1.7.5

### Immune repertoire

- **Clonal UMAP**: axes now match the main projection style —
  `UMAP_1`/`UMAP_2` titles removed, with boxed/mirrored axes (showline +
  mirror) and autorange, so the Clonal UMAP sits visually consistent
  with the main projection tab.

## Version 1.7.4

### Immune repertoire

- **Clonal UMAP**: new first tab overlaying clone-expansion level
  (Single/Small/Medium/Large/Hyperexpanded) on the existing cell
  projection, reusing the dataset’s UMAP/tSNE coordinates. A Receptor
  selector (TCR/BCR, only the classes present in the data) and a
  Projection selector drive it. A “Show all cells” option (on by
  default) draws cells without the selected receptor as a grey
  background, so expanded clones are shown in context. Group filters
  subset which cells appear by any metadata column.
- **Generic display options**: font size and title for every IR plot,
  plus point size and opacity for the scatter-type plots (Clonal UMAP,
  Scatter), in an “Additional parameters” box. Changing them re-renders
  the plot.
- **Reworked layout**: the immune repertoire page now uses the same
  left-parameters / right-visualization layout as the main projection
  tab, with Main parameters, Additional parameters, and Group filters
  boxes on the left.
- **Parameter help**: the info button on each parameter box opens a
  dialog explaining, in plain language, exactly the controls shown on
  the current tab.
- Clone call is no longer shown on the Clonal UMAP tab, where it only
  adds noise.
- **More scRepertoire parameters wired up**: a generic “Order groups”
  control (Default / Alphanumeric) now reaches every plot whose
  scRepertoire function accepts `order.by`, and clonalHomeostasis gains
  a “Clone size thresholds” control (`cloneSize`). Both previously had
  no UI and were never passed.
- **CDR3 length is now faceted, not overlaid.** The Length tab
  previously passed `group.by` straight to
  [`scRepertoire::clonalLength`](https://www.borch.dev/uploads/scRepertoire/reference/clonalLength.html),
  which draws every group as coloured bars in a single panel. It now
  takes that function’s export table and redraws it with `facet_wrap`,
  so each group (sample, or the chosen metadata column’s levels) gets
  its own length-distribution panel on a shared axis — “Group results
  by: sample” produces one plot per sample instead of a single mixed
  plot.
- **Grouping unified on a single control.** The separate “Comparison
  units” selector has been removed from every tab: it re-split the
  repertoire list, which only duplicated — with a narrower, sample-only
  column set — what scRepertoire’s own `group.by` already does (it
  rbinds the list and re-splits on the chosen column). Comparison units
  are now defined solely by “Group results by” (“Compare by” on Scatter
  / Compare / Paired Scatter): None compares the loaded samples; a
  metadata column compares that column’s levels. This removes the case
  where setting one control had no visible effect because the other
  already expressed the same split.
- **Unified plot heights**: the immune repertoire tabs now share a
  single plot height (`ir_fill_plot` / `ir_fill_wrap` helpers) instead
  of repeating a per-tab pixel value.

## Version 1.7.3

### Immune repertoire

- **Immune repertoire**: new conditional tab for TCR/BCR clonotype
  analysis with 19 visualization methods driven by `scRepertoire`,
  covering clonal abundance, diversity, homeostasis, CDR3
  length/composition, V(D)J gene usage, k-mer motifs, and cross-sample
  comparison. Each method includes contextual help with biological
  interpretation guidance.
- **Sample splitting**: a sample-column dropdown lets users re-split the
  repertoire by any shared metadata column; all visualizations recompute
  against the chosen grouping instead of a fixed sample field.
- New utility wrapper: `getImmuneRepertoire()`.
- The bundled `example.crb` now carries real 10x immune-repertoire data
  (`sc5p_v2_hs_PBMC_10k`, 5’ gene expression + TCR + BCR from the same
  experiment), so the immune repertoire tab — including TCR, BCR
  (isotype/SHM), and cross-sample comparisons — works out of the box in
  a single combined dataset. (This single 10x donor is randomly
  partitioned into three demo samples so that cross-sample features have
  data; the sample labels do not represent distinct biological donors.)
- Immune repertoire grouping/splitting now works for **any** metadata
  column (sample, condition, cell type, …): grouping variables are taken
  from the data set’s metadata and joined onto the clonotype data by
  barcode, rather than only columns embedded in the IR table.

## Version 1.7.2

### Enhanced modules

- Added a Most expressed genes tab for exploring per-group gene
  expression summaries exported with `.crb` files.
- Added an Enriched pathways tab for browsing pathway enrichment
  results.
- Added an Extra material tab for exported tables and plots.
- Added utility wrappers for exporting most expressed genes, enriched
  pathways, extra tables, and extra plots.
- Added tests and vignettes covering the new Shiny modules and export
  helpers.

## Version 1.7.1

This maintenance release cleans up the package surface introduced by the
previous releases and refreshes documentation for the current codebase.

### Maintenance

- Removed unused internal Shiny sidebar/menu helpers and orphaned
  utility wrapper functions.
- Cleaned stale roxygen comments, generated Rd files, README wording,
  and internal comments so they describe the current package from its
  own perspective.
- Updated package metadata and regenerated documentation for the current
  public API.

## Version 1.7.0

### New features

- External HDF5 expression backend, symmetric to the bpcells backend:
  [`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
  with `expression_matrix_mode = "h5"` writes the matrix via
  [`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html)
  to a TENx-format `.h5` next to the `.crb`. The runtime attach loads it
  back as a lazy
  [`HDF5Array::TENxMatrix`](https://rdrr.io/pkg/HDF5Array/man/TENxMatrix-class.html)
  seed and transposes via `DelayedArray::t()` (free); the in-memory
  `dgCMatrix` is never materialised, so RAM stays close to the `.crb`
  metadata size and attach is effectively instant
- Introduced
  [`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
  for bundling a self-contained Shiny app from one or more `.crb` files
- [`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
  now copies the `<stem>.h5` sibling alongside the `.crb` during app
  bundling, mirroring the existing `.bpcells/` handling
- Legacy `.crb` files (predating the `expression_backend` field) are
  auto-tagged as `h5` when the host app sets
  `Cerebro.options[["expression_matrix_h5"]]`, finally giving
  `inst/extdata/v1.4/example.h5` a runtime consumer
- [`convertSeuratToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md)
  accepts an in-memory Seurat object alongside the `.rds` path; output
  basename derives from `experiment_name` when no path is given
- [`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
  opens a `...` passthrough so callers can forward extra options without
  signature churn
- [`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
  with `expression_matrix_mode = "bpcells"` now auto-detects
  losslessly-integer values and calls
  `BPCells::convert_matrix_type("uint32_t")` before
  `write_matrix_dir()`, which triggers BPCells’s bit-packed integer
  storage on the typical scRNA-seq counts case. Shrinks the bpcells
  sibling ~5× on integer counts (e.g. 50k cells × 20k genes: 440 MB raw
  double → 78 MB bit-packed; PBMC All Samples 38,606 × 147,756: 2.6 GB →
  549 MB), and queries get ~1.5-1.7× faster as a side effect (smaller
  payload to read). Normalised float values
  (`slot = "data"`/`"scale.data"`) fall back to raw double to avoid
  silent precision loss

### Bug fixes

- Fixed
  [`exportFromSCE()`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md)
  projections: `reducedDims()` output is now coerced to `data.frame`
  before `addProjection()`, matching the Seurat path and clearing a
  latent runtime error for SCE inputs with non-PCA reductions
- Fixed `.attachExternalExpression` crashing on legacy `.crb` objects
  that predate the `getExpressionBackend()` method; such objects are now
  treated as embedded backend and skip the attach step
- Fixed “method not found” errors on the trajectory tab by renaming the
  corresponding `Cerebro_v1.3` methods to the names the Shiny server
  already calls (`getMethodsForTrajectories`, `getNamesOfTrajectories`)
- Fixed gene_expression plot chain freezing on gene picker changes:
  removed a stale
  [`isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html) wrapper and
  a reference to a non-existent `expression_projection_update_button`
  input; the existing 250 ms debounce on the data-to-plot reactive still
  throttles bursts

### Testing

- Extended testing
- Added an h5 round-trip test in `test-exportFromSeurat.R` verifying
  writer/reader bit-identity for the new HDF5 backend, plus an
  attach-level test asserting the runtime returns a lazy `DelayedMatrix`
  (not an in-memory `dgCMatrix`)
- Added `tests/README.md` documenting the layout (testthat unit,
  testthat shinytest2, smoke)

### Dependencies

- `rhdf5` removed from `Suggests`. The h5 backend now goes through
  [`HDF5Array::writeTENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/writeTENxMatrix.html)
  (writer) and
  [`HDF5Array::TENxMatrix()`](https://rdrr.io/pkg/HDF5Array/man/TENxMatrix-class.html)
  (lazy reader), which use rhdf5 internally; users no longer need to
  install or `requireNamespace` rhdf5 directly

### CI/CD

- Switched Nix environment to `fixed-date` to avoid constant rebuilding
- simplified workflow by removing `dev` and `sync-dev`

## Version 1.6.0

### Bug fixes

- Fixed all errors and warnings identified by R CMD CHECK, making the
  package ready for CRAN submission
- Fixed Seurat v5 API: replaced deprecated slot access (`@counts`,
  `@data`) with
  [`GetAssayData()`](https://satijalab.github.io/seurat-object/reference/AssayData.html)
  across multiple functions
- Fixed `addPercentMtRibo`, `calculatePercentGenes`,
  `getMostExpressedGenes`, `performGeneSetEnrichmentAnalysis`,
  `getMarkerGenes`, `getEnrichedPathways`, `exportFromSCE`,
  `exportFromSeurat`
- Fixed GSVA v2.x API compatibility: now uses `gsvaParam()` with version
  check for backward compatibility
- Fixed `class(x) == "..."` checks replaced with
  [`inherits()`](https://rdrr.io/r/base/class.html) across all relevant
  functions
- Fixed [`require()`](https://rdrr.io/r/base/library.html) replaced with
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) throughout
- Fixed cross-references and examples in documentation

### Testing

- Added unit tests for all core R functions
- Added shinytest2 integration tests for the full Cerebro interface,
  covering gene expression, group/marker genes, color management, and
  more
- Tests run in a reproducible Nix environment via GitHub Actions

### CI/CD

- Added Nix-based GitHub Actions workflows for R CMD CHECK, R tests,
  pkgdown, code style, and automatic `default.nix` updates
- Added `update-nix` workflow: regenerates `default.nix` weekly via
  `create_env.R` and opens a PR automatically; BPCells commit SHA is now
  auto-fetched from GitHub
- Added `style` workflow: formats R code via
  [air](https://github.com/posit-dev/air) on every PR
- Added `sync-dev` workflow: automatically merges master back into dev
  after every merge to keep branches in sync
- Switched Nix environment to `bleeding-edge` for always up-to-date CRAN
  packages
- Branch protection rulesets configured for both `master` and `dev` with
  required status checks and auto-merge

### Documentation

- Added pkgdown site at <https://mihem.github.io/CerebroNexus/> with
  light/dark/auto theme switch, search, and all vignettes as articles
- Site automatically builds and deploys to GitHub Pages on push to
  master

## Version 1.5.3

- several bug fixes so that launchCerebro should work again

## Version 1.5.2

- allow plot settings (size, opacity, number of cells to show) to be
  different in gene expression and overview (useful for large datasets
  with slow gene expression)

## Version 1.5.1

- remove unused functions in group

## Version 1.5.0

- make compatible with Seuratv5, especially with BPCells Matrix

## Version 1.4.1

- timeout function added. This logs out the user after 600 second of
  inactivity (can be changed in `shiny_ui.R`). The JS function was taken
  from <https://stackoverflow.com/a/53207050/21417317>.
- add option to show up to 1000 cells in `Main`, which is useful for
  exports.

## Version 1.4.0

This is the first update of this cerebroApp fork. Its aim is to continue
a lightweight version of the excellent cerebroApp with only the main
function as the cerebroApp by Roman Hillje is sadly discontinued.

### Major changes

- remove enriched pathways, extra material, most expressed genes and
  trajectory functions since the goal of this fork is to continue with a
  lightweight version

### Minor changes

- `Load Data` is renamed to `Data info` and `Overview` to `Main`
- Preferences about WebGL and hover info are now show in the first tab
  called `Data info`
- more colorful boxes for the sample information
- different icons for tabs `Data info`, `Main`, `Groups` and
  `Marker Groups`
