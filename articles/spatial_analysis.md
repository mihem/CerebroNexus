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
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md).
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

- **Spatial data**: which image/FOV to plot when several are present.
- **Plot type**: `ImageDimPlot` (categorical) or `ImageFeaturePlot`
  (gene).
- **Color cells by**: metadata variable used for cell coloring.
- **Background image**: histology overlay (only shown when a matching
  `spatial_images` entry exists for the loaded dataset).
- **Image opacity**: blend factor for the background image.

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

## Data preparation

Spatial data must be embedded in the `.crb` file by the export pipeline.
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
**automatically detects** spatial data on the Seurat object
(`object@images`, covering Visium / Xenium / FOV) and stores the
coordinates and expression — no special flag is required.

``` r
library(CerebroNexus)

seurat_obj <- qs::qread("path/to/seurat_object.qs")

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

### Placing histological images

Histology images are **not** stored inside the `.crb`; they are passed
to
[`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md),
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
| `spatial_image_settings` | nested list | `NULL` | `dataset -> spatial entry -> image label -> list(...)` with `flip_x`, `flip_y` (`TRUE` mirrors the image), `scale_x`, `scale_y` (1.0 = no scaling), `offset_x`, `offset_y`, `rotation`, and `image_opacity`. |
| `spatial_plot_rotation` | nested list | `NULL` | `dataset -> spatial entry -> degrees`: rotation of cell coordinates (e.g. -90 to 270), for when capture-area and image orientation differ. |

A dataset, spatial entry, or image label that does not exist raises an
error before anything is bundled, so a typo never produces a silently
unaligned app. Missing settings default to a no-op (no flip, scale 1.0,
no rotation).

## See also

- [`vignette("cerebronexus_workflow_seurat")`](https://mihem.github.io/CerebroNexus/articles/cerebronexus_workflow_seurat.md)
  for the complete export workflow.
- [`vignette("create_a_self_contained_shiny_app")`](https://mihem.github.io/CerebroNexus/articles/create_a_self_contained_shiny_app.md)
  for app bundling options.
- [`vignette("multi_crb")`](https://mihem.github.io/CerebroNexus/articles/multi_crb.md)
  for hosting several datasets in one app.
