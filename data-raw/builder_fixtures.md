# Builder fixtures

The Builder gallery ships three inputs through one action, two synthetic and one real:

- `inst/builder/fixtures/complete_viewer/complete_viewer_data.rds`
- three precomputed Marker gene attachments (`.csv`, `.tsv`, and `.xlsx`)
- two supplementary-table attachments (`.csv` and `.xlsx`)
- seven deterministic histology attachments (`.png` and `.jpg`)
- `inst/builder/fixtures/trekker_spatial/trekker_4_tissues.qs2`
- four Trekker tissue H&E images (`.jpg`)
- `inst/builder/fixtures/spatial_alignment/visium_breast_cancer.rds` and its `visium_breast_cancer_he.jpg`, a crop of a real 10x Visium section (10x Genomics, CC BY 4.0)

These are Builder inputs, separate from the Viewer demo `.crb` outputs. From the repository root, rebuild them with:

```sh
Rscript data-raw/build_builder_fixtures.R
Rscript data-raw/build_builder_spatial_fixtures.R
Rscript data-raw/build_builder_visium_fixture.R \
  --matrix PATH/V1_Breast_Cancer_Block_A_Section_1_filtered_feature_bc_matrix.h5 \
  --spatial PATH/V1_Breast_Cancer_Block_A_Section_1_spatial.tar.gz
```

The first script builds `complete_viewer/`. The second builds the Trekker input from an explicit synthetic recipe, fixed seeds and mathematical image generation; it does not read existing fixture binaries, download data or require the vendor Trekker dataset. The third builds `spatial_alignment/` from two published 10x Genomics files that you download first (see below); it never downloads anything itself and takes their paths as arguments. To rebuild into a scratch directory without changing the shipped files:

```sh
Rscript data-raw/build_builder_spatial_fixtures.R --fixture trekker --output /tmp/builder-fixtures
Rscript data-raw/build_builder_visium_fixture.R --matrix ... --spatial ... --output /tmp/builder-fixtures
```

Use a writable path appropriate for your OS; for example `--output "C:/temp/builder-fixtures"` on Windows. Output always contains the corresponding `trekker_spatial/` or `spatial_alignment/` subdirectory. The original Complete Viewer script accepts an optional positional path to its own output directory.

Build dependencies are `Seurat`, `SeuratObject`, `Matrix`, `qs2`, `jpeg` and `future` for the Trekker fixture, plus `rhdf5`, `png` and `jsonlite` for the Visium fixture; the Complete Viewer script additionally uses packages such as `ggplot2` and `writexl`. These packages are included directly or transitively in the development environment. Fixture generation is offline once the dependencies (and, for Visium, the two input files) are present.

The previous Trekker generator source was not retained. The 2026-10-13 recipe replaces that opaque input while preserving its gallery contract; old/new binary equality is not claimed. Two fresh-process builds are checked byte-for-byte within one toolchain. Package versions, numerical libraries and graphics/font backends can change serialization, UMAP output or pixels across environments; the structural and identity assertions remain the portable acceptance criteria. No source timestamps or local file paths are embedded as provenance.

## Trekker-style recipe

The Trekker-style input contains 200 genes, 5,020 cells, eight synthetic cell types, four tissues of 1,250 cells each and 20 spatial outliers. Marker-enriched Poisson counts produce meaningful gene and group displays. PCA, UMAP and clustering are computed with fixed seeds and sequential execution, then the object retains a counts layer, `pca`, `umap`, `SPATIAL`, and one `SlideSeq` section named `slice1`. Its coordinate convention is explicitly `x = -SPATIAL_2`, `y = SPATIAL_1`. Four deterministic H&E-like illustrations are separately selectable images for that section; they are synthetic pictures, not measured tissue or vendor data.

`tests/testthat/test-builder-fixture-rebuild.R` checks two independent rebuilds, dimensions, canonical cell order, image shapes, SlideSeq orientation and marker-expression separation. The existing gallery/alignment tests verify that the committed inputs still load and restore the image preset. Source recipes stay in `data-raw/`; the installed Builder reads only the committed generated artifacts.

## Visium breast cancer recipe (Spatial alignment demo)

Source and licence: 10x Genomics, *Human Breast Cancer (Block A Section 1)*, Visium Spatial Gene Expression, Space Ranger 1.1.0, released under the Creative Commons Attribution 4.0 licence (CC BY 4.0, <https://creativecommons.org/licenses/by/4.0/>). Attribution: "10x Genomics, CC BY 4.0"; the gallery button and the example's catalog description carry it. The fixture is a cropped and reduced derivative of that dataset. Download the two inputs from the 10x Genomics dataset page (<https://www.10xgenomics.com/datasets>, `V1_Breast_Cancer_Block_A_Section_1`) or the scverse mirror (<https://exampledata.scverse.org/scanpy/visium/1.1.0/V1_Breast_Cancer_Block_A_Section_1/>):

- `V1_Breast_Cancer_Block_A_Section_1_filtered_feature_bc_matrix.h5` (28 MB)
- `V1_Breast_Cancer_Block_A_Section_1_spatial.tar.gz` (10 MB; `--spatial` also accepts its extracted directory)

The recipe analyses all 3,798 in-tissue spots of the section with Seurat (log-normalisation, 2,000 variable genes, 30 PCs, Louvain clusters at resolution 0.5 and a uwot UMAP on 20 PCs, fixed seed 20261104, sequential execution), then keeps the 300 most variable genes and the 405 spots whose whole spot circle lies inside a 560 × 440 pixel crop of `tissue_hires_image.png` (columns 1201–1760, rows 881–1320): the right edge of the section with the pink tumour nodule, stroma, fat and the glass beyond the tissue edge. The object keeps the real counts and the normalised data of those genes (normalised against each spot's whole library), the whole-transcriptome `nCount_Spatial`/`nFeature_Spatial`, the clusters (`seurat_clusters`; levels absent from the crop are dropped, a few have a single spot), the `umap` coordinates of the full-section embedding and a `sample` column. The `visium_breast` field of view holds the spot centres in the crop's pixel space (x = column, y = row: the published `pxl_*_in_fullres` positions times `tissue_hires_scalef`, minus the crop origin). The crop is written as a JPEG (quality 0.82). Together the two files are about 240 kB.

The catalog, not the object, applies the alignment preset: the image spans its own pixel extent (0–560 × 0–440) and image rows point down, so the preset is flip y with no rotation, offset or scale, and the example opens exactly aligned. `test-builder-seurat-omnibus.R` checks that preset and that the stain under each spot follows its expression cluster much better than under a flipped or shifted registration. The raw inputs are not committed, so the rebuild check in `test-builder-fixture-rebuild.R` runs only when `CEREBRO_VISIUM_BREAST_DIR` names a directory containing the two files; it then compares two rebuilds byte-for-byte and against the committed counts, spots and coordinates.

Until 2026-11 this example was a synthetic arrow picture; that input and its recipe were removed because no test depended on its picture.

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
