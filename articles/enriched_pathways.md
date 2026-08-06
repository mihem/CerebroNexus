# Enriched Pathways

## Overview

The **Enriched pathways** tab displays functional enrichment results
from methods such as Enrichr, GSVA, or any tool producing gene set
enrichment tables. It appears *conditionally* in the sidebar — only when
the loaded `.crb` file contains pathway enrichment data.

Results are shown in a paginated, searchable DT table with automatic
formatting: percentages as progress bars, p-values as colour tiles, and
log fold-change values on a diverging colour scale.

## Quick start

``` r
library(CerebroNexus)
launchCerebro()
```

1.  Launch CerebroNexus and load a `.crb` file with enrichment results
2.  If enrichment data is present, **Enriched pathways** appears in the
    sidebar
3.  Choose an enrichment method (e.g., `cerebro_seurat_enrichr`)
4.  Choose a grouping variable (e.g., `seurat_clusters`)
5.  Browse, search, sort, and download the results

## Table formatting

Columns are auto-detected and formatted:

- **Percentages**: colour-filled progress bars
- **P-values**: red (significant) to white
- **Log fold-change**: blue (negative) to red (positive)
- **Long text**: hidden behind a toggle to keep the table compact

## Data export

Enrichment results are passed to
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
via the `enriched_pathways` parameter.

``` r
exportFromSeurat(seurat_object,
  file = "my_data.crb",
  enriched_pathways = list(
    method = "cerebro_seurat_enrichr",
    results = enrichment_df
  )
)
```

See
[`vignette("overview_of_cerebro_v1.3_class")`](https://mihem.github.io/CerebroNexus/articles/overview_of_cerebro_v1.3_class.md)
for the expected format.

## See also

[`vignette("overview_of_cerebro_v1.3_class")`](https://mihem.github.io/CerebroNexus/articles/overview_of_cerebro_v1.3_class.md)
for the Cerebro data model.
