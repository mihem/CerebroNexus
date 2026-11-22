# Attribution — Xenium Prime 5K mouse brain demo

`run-demo.R --data=xenium-prime` downloads and prepares 10x Genomics' public
**Xenium Prime fresh-frozen mouse brain, coronal** data set
(`Xenium_Prime_Mouse_Brain_Coronal_FF`, Xenium Onboard Analysis 3.0.0).
No original or derived data files are committed or included in the package.

## Source

- Dataset page: <https://www.10xgenomics.com/datasets/xenium-prime-ffpe-mouse-brain>.
  Although the page slug says FFPE, the downloaded bundle and this demo are
  explicitly the **FF** coronal sample.
- Original outs ZIP:
  <https://cf.10xgenomics.com/samples/xenium/3.0.0/Xenium_Prime_Mouse_Brain_Coronal_FF/Xenium_Prime_Mouse_Brain_Coronal_FF_outs.zip>
  (13,315,451,681 bytes).
- SHA256 of the downloaded original ZIP, checked on 2026-11-21:
  `d624a46a468596969b7021dc12b0fa09690b92f741e6d51642183da8e45a3729`.
  This is a locally verified checksum, not claimed to be published by 10x.
- The four-stain display is inspired by the
  [Spatialscape Prime 5K example](https://github.com/MicTott/spatialscape-examples/tree/main/xenium-mouse-brain).
  The preparation code here reads the original 10x files, not Spatialscape's
  expression bundle or imagery.

Users download directly from 10x into their own cache. Consult the data set
page and 10x's current data-use/licensing terms before redistributing prepared
files. The software's MIT licence does not replace the source data's terms.

## Contents and transformations

- All 63,173 cells and 5,006 `Gene Expression` features; negative-control
  features are excluded. Stable barcodes join the matrix, cell table,
  published graph clusters and UMAP. Duplicate gene symbols, if any, receive
  `make.unique()` suffixes rather than being silently dropped.
- Expression is `log1p(count / total Gene Expression counts * 10000)` per cell
  in an H5 sidecar. `nUMI` and `nGene` come from the raw panel counts.
- The published analysis covers 63,162 cells. The remaining 11 cells are
  retained spatially as `Unassigned`, with missing UMAP coordinates. No
  embedding positions or cell types are invented.
- Coordinates remain in micrometres, with y negated for the Viewer's
  upward-growing axis. The full source pixel dimensions and physical pixel
  sizes define image bounds; no visual registration or hand-tuned offsets
  are applied.
- All four `morphology_focus_0000`–`0003.ome.tif` files are kept together while
  converting: they are one OME image with four measured channels, not four
  unrelated background alternatives.
- The image is read with Bio-Formats (`RBioFormats`). Only one pyramid level
  is decoded, the smallest whose long side is at least 4,096 px (level 4 of 8,
  4,269 x 2,989 px); the 34,154 x 23,912 px scan is never read in full. Each
  channel is linearly stretched between the 0.5th and 99.9th percentiles of its
  positive intensities, then clipped and additively combined: DAPI `#4c8dff`,
  ATP1A1/CD45/E-Cadherin `#3fd14a`, 18S `#ff4fd8`, alphaSMA/Vimentin
  `#ffd23f`. The physical bounds come from the full-resolution size and pixel
  size, not from the level decoded. The JSON records the exact windows and
  dimensions used.
- Transcript points, cell/nucleus boundaries and the 3D morphology stack are
  not imported. The composite is embedded in the CRB and drawn by CerebroNexus'
  existing background-image renderer.
