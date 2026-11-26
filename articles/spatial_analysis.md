# Spatial Transcriptomics Analysis

## Overview

The **Spatial** tab provides interactive visualization of spatial
transcriptomics data (e.g., Xenium, Visium). It appears *conditionally*
— only when the loaded `.crb` file contains spatial coordinates. Cells
are plotted at their spatial positions, optionally overlaid on a
histological background image, with interactive controls for color
mapping, point aesthetics, group filtering, and selection-based
analysis.

## Quick start

The Spatial tab surfaces automatically once a dataset carrying spatial
data is loaded. To also show a histological image behind the cells, pass
its path through
[`createShinyApp()`](https://www.mheming.com/CerebroNexus/reference/createShinyApp.md).
Images are declared per dataset, per spatial entry (the name of the
Seurat image, `availableSpatial()`), and per image label; alignment goes
in `spatial_image_settings` under the same three names:

``` r
library(CerebroNexus)

createShinyApp(
  cerebro_data = c("My spatial sample" = "path/to/sample_spatial.crb"),
  result_dir = "cerebro_app",
  spatial_images = list(
    "My spatial sample" = list(
      slice1 = c("H&E" = "path/to/sample_histology.png")
    )
  ),
  spatial_image_settings = list(
    "My spatial sample" = list(
      slice1 = list("H&E" = list(flip_y = TRUE, scale_x = 0.9))
    )
  )
)
```

The dataset names of `spatial_images` and `spatial_image_settings` must
match the names of `cerebro_data`, and the spatial entry names must
match those listed by `availableSpatial()` of the CRB. When the app
runs:

1.  If the dataset contains spatial data, **Spatial** appears in the
    sidebar.
2.  The **Background image** dropdown lets you toggle the histology
    overlay.
3.  The **Image opacity** slider blends the image against the cells.
4.  Lasso or box-select cells to drive the selected-cells plot and
    table.

## Visualization panels

### Projection

The main spatial view shows cells positioned at their coordinates,
optionally on a histological background. Cells are coloured by a
user-selected metadata variable. Hover to see cell metadata; zoom and
pan with the Canvas toolbar.

### Main parameters

- **Sections**: which image/FOV to plot when several are present;
  several give the section grid (below).
- **Display mode**: *Groups* (colour by a metadata column, Seurat’s
  `ImageDimPlot`), *Gene expression* (`ImageFeaturePlot`) or *RGB
  co-expression* (three genes, one per colour channel).
- **Colour by**: metadata variable used for cell coloring.
- **Background image**: histology overlay (only shown when a matching
  `spatial_images` entry exists for the loaded dataset).
- **Background image** in the settings: opacity, position, scale,
  rotation and flips of the picture of each section on screen, and *Copy
  alignment as preset* for the `spatial_image_settings` code.

### Additional parameters

- **Point size / opacity**: control visual density.
- **Show % of cells**: randomly downsample for performance.
- **Point border**: toggle cell outlines.
- **Color scale**: continuous or categorical color mapping.
- **Show group label**: overlay group centroids.

### Group filters

Select which metadata groups to display. Cells passing all active
filters are shown in the projection.

### Selected cells

Lasso or box-select cells in the projection to view:

- **Plot of selected cells**: bar plot of group composition
  (categorical) or violin/box plot (continuous).
- **Table of selected cells**: interactive DT table with metadata,
  exportable to CSV or Excel.

### Section grid

When a dataset has more than one spatial entry (one per tissue section,
FOV or capture area), the **Sections** field takes one or more of them:
one section shows the single view, several a grid with one panel per
section. Clicking a panel’s title shows only that section. The grid
needs no change to the data or the export.

![The Spatial grid of the Allen whole mouse brain MERFISH atlas (59
coronal sections, 3.9 million cells), cells coloured by class, with the
Sections, Display mode, Colour by, Columns and Embedding fields above
the panels.](img/spatial-grid-allen.png)

The Spatial grid of the Allen whole mouse brain MERFISH atlas (59
coronal sections, 3.9 million cells), cells coloured by class, with the
Sections, Display mode, Colour by, Columns and Embedding fields above
the panels.

- **Same colours, same scale**: every section is coloured by the same
  metadata column or gene. A group has one colour in every section, and
  a gene has one colour scale whose range covers all sections, so the
  same value has the same colour in each section.
- **Own image**: each section starts with the first histology image
  bound to it (embedded, or from `spatial_images`), and the **Background
  images** field lists the images grouped by section, so a section’s
  image can be changed or removed. A section without an image shows its
  cells without one.
- **Dark pictures**: over a dark picture, such as a fluorescence image,
  points are drawn with a light outline so they stay visible. It is on
  by default and can be switched off with *Outline points on dark
  pictures* in the page’s settings.
- **Order** and **Columns**: the panels follow the order of the sections
  in the field; remove a section to hide it, and add it back to move it
  to the end. **Columns** fixes the number of columns; *Auto* fits them
  to the window. Changes among several sections only rearrange the drawn
  panels and do not reload data.
- **Embedding**: adds a UMAP (or another projection) panel. Selecting
  cells in it highlights them in every section, and selecting cells in a
  section highlights them in the embedding. The selected-cells plot and
  table list the cells of all sections.
- **Sync zoom**: zooming or panning one section moves the others to the
  same relative region.
- **Panel shape** (top right of the page): *Fit tissue* shapes the
  panels to the sections and picks the columns that show the most
  tissue; *Square* draws square panels. Every panel has the same size,
  so all sections are drawn at one scale.
- **Opening in the grid**: a data set with several sections opens
  Spatial in the grid. In the Builder, the Spatial card of the Viewer
  display settings has *Open Spatial with*, which can be set to *The
  first section*. Readers can always pick one section or all of them.

Each panel zooms and pans on its own and shows the usual tooltips. RGB
co-expression works in the grid: *RGB layout* shows one composite panel
per section, or with *Separate channels* one panel per section and
channel. The ROI controls and region outlines apply to a single section
and are not available in the grid.

## Data preparation

Spatial data must be embedded in the `.crb` file by the export pipeline.
[`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
**automatically detects** spatial data on the Seurat object
(`object@images`, covering Visium / Xenium / FOV) and stores the
coordinates and expression — no special flag is required.

``` r
library(CerebroNexus)

seurat_obj <- qs2::qs_read("path/to/seurat_object.qs2")

exportFromSeurat(
  object = seurat_obj,
  assay = "SCT",
  slot = "data",
  organism = "hg",
  experiment_name = "My Spatial Dataset",
  groups = c("sample_id", "condition", "cell_type"),
  file = "my_spatial_data.crb"
)
```

If the Seurat object carries images, the resulting `.crb` gains a
spatial projection and the Spatial tab becomes available when it is
loaded.

### Xenium and 10x Atera objects

Objects from Seurat’s imaging readers
([`LoadXenium()`](https://satijalab.org/seurat/reference/ReadXenium.html),
and in Seurat 6
[`LoadAtera()`](https://satijalab.org/seurat/reference/ReadAtera.html)
for 10x Atera) keep their counts in an assay named after the platform,
not `RNA`, and Seurat names the per-cell totals after it
(`nCount_Xenium`, `nCount_Atera`). Pass the assay and these two columns;
the Builder picks them on its own. The readers add no dimensional
reduction, so run
e.g. [`RunPCA()`](https://satijalab.org/seurat/reference/RunPCA.html)
and [`RunUMAP()`](https://satijalab.org/seurat/reference/RunUMAP.html)
first.

[`LoadAtera()`](https://satijalab.org/seurat/reference/ReadAtera.html)
needs the `blosc`, `jsonlite` and `data.table` packages, and by default
reads the cell centroids only. Whatever the FOV holds is exported, so
ask for the rest when reading:

- `segmentations = "cell"` (or `"nucleus"`) for the cell outlines;
- `molecule.coordinates = TRUE` together with `genes` for transcript
  positions. Without `genes` Seurat only keeps a handle to the
  transcript file, and no molecules reach the FOV or the `.crb`.

The morphology image that `LoadAtera(morphology.image = TRUE)` stores in
[`Misc()`](https://satijalab.github.io/seurat-object/reference/Misc.html)
is not exported; supply a background through `spatial_images` like any
other.

``` r
atera <- Seurat::LoadAtera(
  "path/to/atera_outs",
  segmentations = "cell",
  molecule.coordinates = TRUE,
  genes = c("CD19", "ATOX1")
)
atera <- Seurat::NormalizeData(atera)
atera <- Seurat::FindVariableFeatures(atera)
atera <- Seurat::ScaleData(atera)
atera <- Seurat::RunPCA(atera)
atera <- Seurat::RunUMAP(atera, dims = 1:30)

convertSeuratToCerebro(
  atera,
  result_dir = "output",
  assay = "Atera",
  nUMI = "nCount_Atera",
  nGene = "nFeature_Atera",
  organism = "hg",
  experiment_name = "Atera kidney",
  groups = "azimuth_broad"
)
```

### Placing histological images

Histology images are **not** stored inside the `.crb`; they are passed
to
[`createShinyApp()`](https://www.mheming.com/CerebroNexus/reference/createShinyApp.md),
which copies them into the generated app bundle and references them by
dataset, spatial entry, and image label. JPG, PNG, and SVG are
supported. A plain `c(Dataset = path)` vector is rejected.

``` r
createShinyApp(
  cerebro_data = c("My spatial sample" = "my_spatial_data.crb"),
  result_dir = "cerebro_app",
  spatial_images = list(
    "My spatial sample" = list(
      slice1 = c("H&E" = "path/to/histology.png")
    )
  )
)
```

### Multiple samples with alignment

When several capture areas live in one app, give each its own image and
use the flip / scale / rotation parameters to align each image with its
coordinate system. Every dataset name must match a name of
`cerebro_data`:

``` r
createShinyApp(
  cerebro_data = c(
    "Ctrl sample" = "ctrl_spatial.crb",
    "MS sample" = "ms_spatial.crb"
  ),
  result_dir = "cerebro_app",
  spatial_images = list(
    "Ctrl sample" = list(slice1 = c("H&E" = "ctrl_histology.png")),
    "MS sample" = list(slice1 = c("H&E" = "ms_histology.png"))
  ),
  spatial_image_settings = list(
    "Ctrl sample" = list(
      slice1 = list("H&E" = list(flip_y = TRUE, scale_x = 0.90, scale_y = 1.00))
    ),
    "MS sample" = list(
      slice1 = list("H&E" = list(flip_y = TRUE, scale_x = 0.95, scale_y = 1.45))
    )
  ),
  spatial_plot_rotation = list(
    "Ctrl sample" = list(slice1 = -61),
    "MS sample" = list(slice1 = 205)
  )
)
```

## Spatial parameters reference

| Parameter | Type | Default | Description |
|----|----|----|----|
| `spatial_images` | nested list | `NULL` | `dataset -> spatial entry -> image label -> path` (JPG, PNG, or SVG), or a descriptor `list(path = , bounds = )`. Dataset names must match `cerebro_data`. |
| `spatial_image_settings` | nested list | `NULL` | `dataset -> spatial entry -> image label -> list(...)` with `flip_x`, `flip_y` (`TRUE` mirrors the image), `scale_x`, `scale_y` (1.0 = no scaling), `offset_x`, `offset_y`, `rotation`, and `image_opacity`. Offsets, scales and flips act in the image’s own unrotated frame; `rotation` is applied last, about the coordinate origin, like `spatial_plot_rotation`. |
| `spatial_plot_rotation` | nested list | `NULL` | `dataset -> spatial entry -> degrees`: rotation of cell coordinates (e.g. -90 to 270), for when capture-area and image orientation differ. A background image does not follow it on its own: give the image the same `rotation`, and keep its offsets and flips as found on the unrotated section. |

A dataset, spatial entry, or image label that does not exist raises an
error before anything is bundled, so a typo never produces a silently
unaligned app. Missing settings default to a no-op (no flip, scale 1.0,
no rotation).

## See also

- [`vignette("import_your_analysis")`](https://www.mheming.com/CerebroNexus/articles/import_your_analysis.md)
  for converting Seurat, AnnData and SpatialData data to a `.crb` file.
- [Share data sets as a Shiny
  app](https://www.mheming.com/CerebroNexus/articles/create_a_self_contained_shiny_app.md)
  for app bundling options and hosting several datasets in one app.
- [Trekker single-cell spatial
  mapping](https://www.mheming.com/CerebroNexus/articles/trekker_spatial_mapping.md)
  for Trekker data, which has its own page.
