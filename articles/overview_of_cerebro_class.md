# Overview of the Cerebro class

The `Cerebro` data class formalizes the data structure to provide
stability and improve backward compatibility across CerebroNexus
releases.

While the data is organized similar to how it was done before, now they
live in their own class with setter and getter methods for the different
slots. Also, unnecessary prefixes and suffixes have been removed,
allowing direct connection between results, e.g. tables of marker genes,
and the grouping variable they belong to / were derived from. Moreover,
additional layers have been added to allow storing results derived from
different methods.

Let’s look at some details. Overall, the object is organized as such:

    <Cerebro_object>
    ├── version (package_version)
    ├── experiment (named list)
    │   ├── experiment_name (string)
    │   ├── organism (string)
    │   ├── date_of_analysis (string)
    │   └── date_of_export (string)
    ├── technical_info (named list)
    ├── parameters (named list)
    ├── groups (named list)
    │   └── <grouping_variable> (vector of group levels)
    ├── cell_cycle (vector)
    ├── gene_lists (named list)
    │   └── <gene_list_name> (vector of genes)
    ├── expression (matrix-like; attached from the H5 file when an H5-backed .crb is read)
    ├── expression_backend (list: type "embedded" or "h5", location)
    ├── meta_data (data.frame)
    ├── projections (named list)
    │   └── <projection_name> (data.frame)
    ├── trees (named list)
    │   └── <grouping_variable> (phylo)
    ├── most_expressed_genes (named list)
    │   └── <grouping_variable> (data.frame)
    ├── marker_genes (named list)
    │   └── <method> (named list)
    │       └── <table_name> (data.frame)
    ├── enriched_pathways (named list)
    │   └── <method> (named list)
    │       └── <table_name> (data.frame)
    ├── trajectories (named list)
    │   └── <method> (named list)
    │       └── <trajectory_name> (depends on method, most likely a list or data frame)
    ├── immune_repertoire (named list)
    │   └── <sample> (data.frame, one row per cell with a receptor)
    └── extra_material (named list)
        ├── tables (named list)
        │   └── <name> (data.frame)    
        └── plots (named list)
            └── <name> (ggplot-object)

Interaction with the Cerebro object will be done through internal
methods, providing additional control for the correct format of the
object.

The expression matrix is either embedded in the `.crb` or kept in an H5
file next to it; `expression_backend` records which
(`setExpressionBackend()` accepts only `"embedded"` and `"h5"`), and
[`saveCerebro()`](https://mihem.github.io/CerebroNexus/reference/saveCerebro.md)
/
[`readCerebro()`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md)
take care of the rest. T-cell and B-cell receptors live together in
`immune_repertoire`; there are no separate `tcr_data` / `bcr_data`
fields, and `getTCR()` / `getBCR()` return the TCR or BCR rows of that
repertoire. A very large repertoire can sit in a sibling
`<stem>.immune.qs2` file that `getImmuneRepertoire()` loads on demand. A
`.crb` written by an older release that does not follow this layout is
rejected with a pointer to
[`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md).

As long as you follow the described scheme, you can export and visualize
any data frame that is stored in the `marker_genes` or
`enriched_pathways` slots. I have prepared a vignette with some more
details that you can find
[here](https://mihem.github.io/CerebroNexus/articles/export_and_visualize_custom_tables_and_plots.md).

Please make sure to use names for items in lists, otherwise it will
likely result in an error.

Information stored in the `technical_info` and `parameters` slots will
be presented in the “Analysis info” tab. Here you can add strings,
numbers, and lists that with you think are relevant to share.

Calling the Cerebro object will give an overview of the information it
currently contains:

    class: Cerebro
    exporter package version: 4.3.0
    experiment name: pbmc_Seurat
    organism: hg
    date of analysis: 2020-02-19
    date of export: 2020-09-08
    number of cells: 5,697
    number of genes: 15,907
    grouping variables (3): sample, seurat_clusters, cell_type_singler_blueprintencode_main
    cell cycle variables (1): cell_cycle_seurat
    projections (2): UMAP, UMAP_3D
    trees (3): sample, seurat_clusters, cell_type_singler_blueprintencode_main
    most expressed genes: sample, seurat_clusters, cell_type_singler_blueprintencode_main
    marker genes:
      - cerebro_seurat (3): sample, seurat_clusters, cell_type_singler_blueprintencode_main
    enriched pathways:
      - cerebro_seurat_enrichr (3): sample, seurat_clusters, cell_type_singler_blueprintencode_main, 
      - cerebro_GSVA (3): sample, seurat_clusters, cell_type_singler_blueprintencode_main
    trajectories:
      - monocle2 (1): highly_variable_genes

For more details, please check the `Cerebro` reference page.
