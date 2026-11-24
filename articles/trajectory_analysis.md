# Trajectory Analysis

## Overview

The **Trajectory** tab provides interactive exploration of pseudotime
trajectories (e.g., from Monocle 2). It appears *conditionally* — only
when the loaded `.crb` file contains trajectory data.

This module restores trajectory functionality from an earlier
implementation. The code is the original implementation by Roman Hillje,
restructured into the current Viewer sub-file layout with no functional
changes.

## Quick start

``` r
library(CerebroNexus)
launchCerebro()
```

1.  Launch CerebroNexus and load a `.crb` file with trajectory data
2.  If trajectory data is present, **Trajectory** appears in the sidebar
3.  Select a trajectory method and name from the dropdowns
4.  Explore cells along pseudotime, expression metrics, and state
    distributions

## Visualization panels

### Projection

Shows cells positioned along the trajectory in a 2D projection. Cells
are coloured by pseudotime state. Hover to see cell metadata.

### Distribution along pseudotime

Density or scatter plots showing how cells are distributed along
pseudotime. Optionally colour by metadata group to compare
distributions.

### Expression metrics

Gene expression plotted against pseudotime. Select a gene to see how its
expression changes along the trajectory. A smooth trend line helps
identify gradual expression changes characteristic of developmental
processes.

### States by group

Stacked bar chart showing the proportion of cells from each metadata
group within each pseudotime state. Useful for identifying which groups
are enriched in specific trajectory branches.

### Statistics

- **Number of expressed genes by state**: boxplot of detected gene
  counts per cell, faceted by state
- **Number of transcripts by state**: boxplot of UMI counts per cell,
  faceted by state
- **Selected cells table**: interactive DT table of cells in the current
  selection or state

## Data preparation

Trajectories must be computed with Monocle 2 (or compatible) and
attached to the Seurat object with
[`extractMonocleTrajectory()`](https://www.mheming.com/CerebroNexus/reference/extractMonocleTrajectory.md)
before the export. The function returns the Seurat object with the
trajectory stored in it, and
[`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
writes it to the `.crb` file.

``` r
library(CerebroNexus)

seurat_object <- extractMonocleTrajectory(
  monocle_object,
  seurat_object,
  trajectory_name = "my_trajectory"
)

exportFromSeurat(
  seurat_object,
  file = "my_data.crb",
  experiment_name = "my_experiment",
  organism = "hg",
  groups = c("sample", "seurat_clusters"),
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA"
)
```

`monocle_object` is a Monocle 2 `CellDataSet` after `orderCells()`. With
monocle 2.38.0 and a current igraph (tested with 2.3.4), `orderCells()`
stops with “The `neimode` argument of `dfs()` … is now defunct”.
[`data-raw/build_trajectory_demo.R`](https://github.com/mihem/CerebroNexus/blob/master/data-raw/build_trajectory_demo.R)
contains the two compatibility patches that the bundled demo trajectory
is built with.

## See also

- [`vignette("cerebronexus_workflow_seurat")`](https://www.mheming.com/CerebroNexus/articles/cerebronexus_workflow_seurat.md)
  for exporting a Seurat object with marker genes and other tables
- [`vignette("import_your_analysis")`](https://www.mheming.com/CerebroNexus/articles/import_your_analysis.md)
  for PAGA, scVelo and pseudotime trajectories from scanpy (`.h5ad`)
