# Builder fixtures

The Builder gallery ships three synthetic inputs through one action:

- `inst/builder/fixtures/complete_viewer/complete_viewer_data.rds`
- three precomputed Marker gene attachments (`.csv`, `.tsv`, and `.xlsx`)
- two supplementary-table attachments (`.csv` and `.xlsx`)
- seven deterministic histology attachments (`.png` and `.jpg`)
- `inst/builder/fixtures/trekker_spatial/trekker_4_tissues.qs2`
- four Trekker tissue H&E images (`.jpg`)
- `inst/builder/fixtures/spatial_alignment/arrow_spatial.rds` and its `arrow_background.png`

These are Builder inputs, separate from the Viewer demo `.crb` outputs. From the repository root, rebuild all three inputs with:

```sh
Rscript data-raw/build_builder_fixtures.R
Rscript data-raw/build_builder_spatial_fixtures.R
```

The first script builds `complete_viewer/`. The second independently builds the Trekker and arrow inputs from explicit synthetic recipes, fixed seeds and mathematical image generation. It does not read existing fixture binaries, download data or require the vendor Trekker dataset. To rebuild one input, or validate in a scratch directory without changing the shipped files:

```sh
Rscript data-raw/build_builder_spatial_fixtures.R --fixture trekker --output /tmp/builder-fixtures
Rscript data-raw/build_builder_spatial_fixtures.R --fixture arrow --output /tmp/builder-fixtures
```

Use a writable path appropriate for your OS; for example `--output "C:/temp/builder-fixtures"` on Windows. Output always contains the corresponding `trekker_spatial/` or `spatial_alignment/` subdirectory. The original Complete Viewer script accepts an optional positional path to its own output directory.

Build dependencies are `Seurat`, `SeuratObject`, `Matrix`, `qs2`, `jpeg` and `future` for the two spatial fixtures; the Complete Viewer script additionally uses packages such as `ggplot2` and `writexl`. The reconstruction test uses `png` to inspect the generated background. These packages are included directly or transitively in the development environment. R must support the PNG graphics device. Fixture generation is offline once dependencies are installed.

The previous Trekker and arrow generator sources were not retained. The 2026-10-13 recipes replace those opaque inputs while preserving their gallery contracts; old/new binary equality is not claimed. Two fresh-process builds are checked byte-for-byte within one toolchain. Package versions, numerical libraries and graphics/font backends can change serialization, UMAP output or pixels across environments; the structural and identity assertions remain the portable acceptance criteria. No source timestamps or local file paths are embedded as provenance.

## Trekker-style and arrow recipes

The Trekker-style input contains 200 genes, 5,020 cells, eight synthetic cell types, four tissues of 1,250 cells each and 20 spatial outliers. Marker-enriched Poisson counts produce meaningful gene and group displays. PCA, UMAP and clustering are computed with fixed seeds and sequential execution, then the object retains a counts layer, `pca`, `umap`, `SPATIAL`, and one `SlideSeq` section named `slice1`. Its coordinate convention is explicitly `x = -SPATIAL_2`, `y = SPATIAL_1`. Four deterministic H&E-like illustrations are separately selectable images for that section; they are synthetic pictures, not measured tissue or vendor data.

The arrow input contains 30 genes and 159 cells: 119 outline points, 38 spine points and two extent anchors. The anchors bound the background at 800 × 500 and are grouped with the spine. The `Spatial` assay has counts and normalized data, the `umap` embedding is the spatial coordinates divided by 100, and `arrow_fov` holds the canonical cell identities. The generated background has directional labels and four differently coloured corners. The catalog, rather than the source object, applies the alignment preset. The cell coordinates are the background's pixel positions (x = column, y = row) and image rows point down, so the preset is flip y with no rotation, offset or scale, and the example opens exactly aligned.

`tests/testthat/test-builder-fixture-rebuild.R` checks two independent rebuilds, dimensions, canonical cell order, image shapes, SlideSeq orientation and marker-expression separation. The existing gallery/alignment tests verify that the committed inputs still load and restore the image preset. Source recipes stay in `data-raw/`; the installed Builder reads only the committed generated artifacts.

## Complete Viewer data Seurat

`complete_viewer_data.rds` follows the same inspection and build path as a
user-uploaded RDS. It contains:

- sparse RNA counts and normalized expression;
- patient, section, FOV, sample, condition, cell-type, cluster, region,
  cell-cycle, and QC metadata;
- PCA, UMAP, and t-SNE reductions;
- gene lists, Marker genes, Most expressed genes, mean expression, enrichment,
  a valid Monocle 2 trajectory, and embedded supplementary tables and plot;
- a six-sample immune repertoire with TRA, TRB, and IGH records plus canonical
  synthetic HLA typing;
- three patients represented by six measured Spatial FOVs with micron
  coordinate systems;
- a valid Trekker payload aligned to a subset of Seurat cell barcodes.

HLA and Trekker coverage here verifies detection, preservation, BuildPlan and
Viewer behavior for embedded payloads. It does not imply a dedicated HLA or
Trekker upload editor.

## Explicit attachments

The marker attachments exercise single-level CSV/TSV mapping and multi-sheet
XLSX mapping. The supplementary attachments exercise CSV plus multi-sheet XLSX
materialization. They are intentionally not embedded in the RDS, so tests must
pass through the same explicit Configure workflow used for local uploads.

The catalog maps histology files to explicit `section_id` and `fov_ids`; file
names are not the authority for that relationship:

- `section_a_1_he.png` and `section_a_1_dapi.png`
- `section_a_2_he.png` and `section_a_2_dapi.png`
- `section_b_1_he.png`, `section_b_1_if.jpg`, and `section_b_1_pas.png`

The remaining B sections and C section are coordinates-only. A photograph is
never a new FOV. The Complete Viewer example does not attach these images automatically;
they are files for exercising the same manual Add image workflow as user data.

`inst/builder/io.R` is the source of truth for the
`complete_viewer_data`, `trekker_spatial` and `spatial_alignment` records, their expected manifests,
conditional Viewer pages, and sidecar inventories.
