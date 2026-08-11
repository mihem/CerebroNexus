# R6 class in which data sets will be stored for visualization in Cerebro.

A `Cerebro` object is an R6 class that contains several types of data
that can be visualized in Cerebro.

## Value

A new `Cerebro` object.

## Public fields

- `version`:

  Exporter package version that was used to create the object.

- `experiment`:

  `list` that contains meta data about the data set, including
  experiment name, species, date of export.

- `technical_info`:

  `list` that contains technical information about the analysis,
  including the R session info.

- `parameters`:

  `list` that contains important parameters that were used during the
  analysis, e.g. cut-off values for cell filtering.

- `groups`:

  `list` that contains specified grouping variables and and the group
  levels (subgroups) that belong to each of them. For each grouping
  variable, a corresponding column with the same name must exist in the
  meta data.

- `cell_cycle`:

  `vector` that contains the name of columns in the meta data that
  contain cell cycle assignments.

- `gene_lists`:

  `list` that contains gene lists, e.g. mitochondrial and/or ribosomal
  genes.

- `expression`:

  `matrix`-like object that holds transcript counts.

- `expression_backend`:

  `list` describing how/where the expression matrix is stored. For step
  7.1 every newly exported object tags itself
  `list(type = "embedded", location = NULL)`; future step 7.2 will
  introduce `type = "h5"` / `"bpcells"` with an external `location`.
  Older `.crb` files (serialised before this field existed) load with
  `expression_backend = NULL`; `getExpressionBackend()` treats that as
  `"embedded"` for backward compatibility.

- `meta_data`:

  `data.frame` that contains cell meta data.

- `projections`:

  `list` that contains projections/dimensional reductions.

- `most_expressed_genes`:

  `list` that contains a `data.frame` holding the most expressed genes
  for each grouping variable that was specified during the call to
  [`getMostExpressedGenes`](https://mihem.github.io/CerebroNexus/reference/getMostExpressedGenes.md).

- `mean_expression`:

  `list` that contains a `data.frame` holding the mean expression per
  gene for each grouping variable.

- `marker_genes`:

  `list` that contains a `list` for every method that was used to
  calculate marker genes, and a `data.frame` for each grouping variable,
  e.g. those that were specified during the call to
  [`getMarkerGenes`](https://mihem.github.io/CerebroNexus/reference/getMarkerGenes.md).

- `enriched_pathways`:

  `list` that contains a `list` for every method that was used to
  calculate marker genes, and a `data.frame` for each grouping variable,
  e.g. those that were specified during the call to
  [`getEnrichedPathways`](https://mihem.github.io/CerebroNexus/reference/getEnrichedPathways.md)
  or
  [`performGeneSetEnrichmentAnalysis`](https://mihem.github.io/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md).

- `trees`:

  `list` that contains a phylogenetic tree (class `phylo`) for grouping
  variables.

- `trajectories`:

  `list` that contains a `list` for every method that was used to
  calculate trajectories, and, depending on the method, a `data.frame`
  or `list` for each specific trajectory, e.g. those extracted with
  [`extractMonocleTrajectory`](https://mihem.github.io/CerebroNexus/reference/extractMonocleTrajectory.md).

- `extra_material`:

  `list` that can contain additional material related to the data set;
  tables should be stored in `data.frame` format in a named `list`
  called \`tables\`

- `immune_repertoire`:

  `list` of data.frames (one per sample) containing scRepertoire columns
  (CTgene, CTnt, CTaa, CTstrict, etc.).

- `hla_typing`:

  canonical HLA typing `data.frame` (long: one row per sample x locus x
  copy) with data provenance, or NULL. Consumed by the HLA & TCR Motifs
  page for donor-level HLA context. Optional; older .crb files simply
  lack it.

- `bcr_data`:

  `list` that contains BCR data (kept for backward compatibility with
  older .crb files).

- `tcr_data`:

  `list` that contains TCR data (kept for backward compatibility with
  older .crb files).

- `spatial`:

  `list` that contains spatial data (coordinates and expression).

- `trekker`:

  `list` with Trekker single-cell spatial-mapping content: canonical and
  variant (transposed / y-mirrored) coordinates, UMAP coordinates,
  per-nucleus cluster/cell-type, positioning QC in the vendor's original
  field names, the upstream (vendor) Moran's I table, and per-nucleus
  positioning-evidence images (base64 `data:` URIs). Consumed by the
  Trekker page. Optional; older .crb files simply lack it.

## Methods

### Public methods

- [`Cerebro$new()`](#method-Cerebro-initialize)

- [`Cerebro$setVersion()`](#method-Cerebro-setVersion)

- [`Cerebro$getVersion()`](#method-Cerebro-getVersion)

- [`Cerebro$checkIfGroupExists()`](#method-Cerebro-checkIfGroupExists)

- [`Cerebro$checkIfColumnExistsInMetadata()`](#method-Cerebro-checkIfColumnExistsInMetadata)

- [`Cerebro$addExperiment()`](#method-Cerebro-addExperiment)

- [`Cerebro$getExperiment()`](#method-Cerebro-getExperiment)

- [`Cerebro$addParameters()`](#method-Cerebro-addParameters)

- [`Cerebro$getParameters()`](#method-Cerebro-getParameters)

- [`Cerebro$addTechnicalInfo()`](#method-Cerebro-addTechnicalInfo)

- [`Cerebro$getTechnicalInfo()`](#method-Cerebro-getTechnicalInfo)

- [`Cerebro$addGroup()`](#method-Cerebro-addGroup)

- [`Cerebro$getGroups()`](#method-Cerebro-getGroups)

- [`Cerebro$getGroupLevels()`](#method-Cerebro-getGroupLevels)

- [`Cerebro$setMetaData()`](#method-Cerebro-setMetaData)

- [`Cerebro$getMetaData()`](#method-Cerebro-getMetaData)

- [`Cerebro$addGeneList()`](#method-Cerebro-addGeneList)

- [`Cerebro$getGeneLists()`](#method-Cerebro-getGeneLists)

- [`Cerebro$setExpression()`](#method-Cerebro-setExpression)

- [`Cerebro$setExpressionBackend()`](#method-Cerebro-setExpressionBackend)

- [`Cerebro$getExpressionBackend()`](#method-Cerebro-getExpressionBackend)

- [`Cerebro$getCellNames()`](#method-Cerebro-getCellNames)

- [`Cerebro$getGeneNames()`](#method-Cerebro-getGeneNames)

- [`Cerebro$getMeanExpressionForGenes()`](#method-Cerebro-getMeanExpressionForGenes)

- [`Cerebro$getMeanExpressionForCells()`](#method-Cerebro-getMeanExpressionForCells)

- [`Cerebro$getExpressionMatrix()`](#method-Cerebro-getExpressionMatrix)

- [`Cerebro$getExpressionRow()`](#method-Cerebro-getExpressionRow)

- [`Cerebro$getExpressionBlock()`](#method-Cerebro-getExpressionBlock)

- [`Cerebro$setCellCycle()`](#method-Cerebro-setCellCycle)

- [`Cerebro$getCellCycle()`](#method-Cerebro-getCellCycle)

- [`Cerebro$addProjection()`](#method-Cerebro-addProjection)

- [`Cerebro$availableProjections()`](#method-Cerebro-availableProjections)

- [`Cerebro$getProjection()`](#method-Cerebro-getProjection)

- [`Cerebro$addTree()`](#method-Cerebro-addTree)

- [`Cerebro$getTree()`](#method-Cerebro-getTree)

- [`Cerebro$addMostExpressedGenes()`](#method-Cerebro-addMostExpressedGenes)

- [`Cerebro$getGroupsWithMostExpressedGenes()`](#method-Cerebro-getGroupsWithMostExpressedGenes)

- [`Cerebro$getMostExpressedGenes()`](#method-Cerebro-getMostExpressedGenes)

- [`Cerebro$addMeanExpression()`](#method-Cerebro-addMeanExpression)

- [`Cerebro$getGroupsWithMeanExpression()`](#method-Cerebro-getGroupsWithMeanExpression)

- [`Cerebro$getMeanExpression()`](#method-Cerebro-getMeanExpression)

- [`Cerebro$addMarkerGenes()`](#method-Cerebro-addMarkerGenes)

- [`Cerebro$getMethodsForMarkerGenes()`](#method-Cerebro-getMethodsForMarkerGenes)

- [`Cerebro$getGroupsWithMarkerGenes()`](#method-Cerebro-getGroupsWithMarkerGenes)

- [`Cerebro$getMarkerGenes()`](#method-Cerebro-getMarkerGenes)

- [`Cerebro$addEnrichedPathways()`](#method-Cerebro-addEnrichedPathways)

- [`Cerebro$getMethodsWithEnrichedPathways()`](#method-Cerebro-getMethodsWithEnrichedPathways)

- [`Cerebro$getMethodsForEnrichedPathways()`](#method-Cerebro-getMethodsForEnrichedPathways)

- [`Cerebro$getGroupsWithEnrichedPathways()`](#method-Cerebro-getGroupsWithEnrichedPathways)

- [`Cerebro$getEnrichedPathways()`](#method-Cerebro-getEnrichedPathways)

- [`Cerebro$addTrajectory()`](#method-Cerebro-addTrajectory)

- [`Cerebro$getMethodsForTrajectories()`](#method-Cerebro-getMethodsForTrajectories)

- [`Cerebro$getNamesOfTrajectories()`](#method-Cerebro-getNamesOfTrajectories)

- [`Cerebro$getTrajectory()`](#method-Cerebro-getTrajectory)

- [`Cerebro$getBCR()`](#method-Cerebro-getBCR)

- [`Cerebro$getTCR()`](#method-Cerebro-getTCR)

- [`Cerebro$addBCRData()`](#method-Cerebro-addBCRData)

- [`Cerebro$addTCRData()`](#method-Cerebro-addTCRData)

- [`Cerebro$getImmuneRepertoire()`](#method-Cerebro-getImmuneRepertoire)

- [`Cerebro$addImmuneRepertoire()`](#method-Cerebro-addImmuneRepertoire)

- [`Cerebro$getHLATyping()`](#method-Cerebro-getHLATyping)

- [`Cerebro$addHLATyping()`](#method-Cerebro-addHLATyping)

- [`Cerebro$addSpatialData()`](#method-Cerebro-addSpatialData)

- [`Cerebro$getSpatialData()`](#method-Cerebro-getSpatialData)

- [`Cerebro$availableSpatial()`](#method-Cerebro-availableSpatial)

- [`Cerebro$getTrekker()`](#method-Cerebro-getTrekker)

- [`Cerebro$addTrekker()`](#method-Cerebro-addTrekker)

- [`Cerebro$addExtraMaterial()`](#method-Cerebro-addExtraMaterial)

- [`Cerebro$addExtraTable()`](#method-Cerebro-addExtraTable)

- [`Cerebro$addExtraPlot()`](#method-Cerebro-addExtraPlot)

- [`Cerebro$getExtraMaterial()`](#method-Cerebro-getExtraMaterial)

- [`Cerebro$getExtraMaterialCategories()`](#method-Cerebro-getExtraMaterialCategories)

- [`Cerebro$checkForExtraTables()`](#method-Cerebro-checkForExtraTables)

- [`Cerebro$getNamesOfExtraTables()`](#method-Cerebro-getNamesOfExtraTables)

- [`Cerebro$getExtraTable()`](#method-Cerebro-getExtraTable)

- [`Cerebro$checkForExtraPlots()`](#method-Cerebro-checkForExtraPlots)

- [`Cerebro$getNamesOfExtraPlots()`](#method-Cerebro-getNamesOfExtraPlots)

- [`Cerebro$getExtraPlot()`](#method-Cerebro-getExtraPlot)

- [`Cerebro$print()`](#method-Cerebro-print)

- [`Cerebro$clone()`](#method-Cerebro-clone)

------------------------------------------------------------------------

### `Cerebro$new()`

Create a new `Cerebro` object.

#### Usage

    Cerebro$new()

#### Returns

A new `Cerebro` object.

------------------------------------------------------------------------

### `Cerebro$setVersion()`

Set the exporter package version that was used to generate this object.

#### Usage

    Cerebro$setVersion(version)

#### Arguments

- `version`:

  Version to set.

------------------------------------------------------------------------

### `Cerebro$getVersion()`

Get the exporter package version that was used to generate this object.

#### Usage

    Cerebro$getVersion()

#### Returns

Version as `package_version` class.

------------------------------------------------------------------------

### `Cerebro$checkIfGroupExists()`

Safety function that will check if a provided group name is present in
the `groups` field.

#### Usage

    Cerebro$checkIfGroupExists(group_name)

#### Arguments

- `group_name`:

  Group name to be tested

------------------------------------------------------------------------

### `Cerebro$checkIfColumnExistsInMetadata()`

Safety function that will check if a provided group name is present in
the meta data.

#### Usage

    Cerebro$checkIfColumnExistsInMetadata(group_name)

#### Arguments

- `group_name`:

  Group name to be tested.

------------------------------------------------------------------------

### `Cerebro$addExperiment()`

Add information to `experiment` field.

#### Usage

    Cerebro$addExperiment(field, content)

#### Arguments

- `field`:

  Name of the information, e.g. `organism`.

- `content`:

  Actual information, e.g. `hg`.

------------------------------------------------------------------------

### `Cerebro$getExperiment()`

Retrieve information from `experiment` field.

#### Usage

    Cerebro$getExperiment()

#### Returns

`list` of all entries in the `experiment` field.

------------------------------------------------------------------------

### `Cerebro$addParameters()`

Add information to `parameters` field.

#### Usage

    Cerebro$addParameters(field, content)

#### Arguments

- `field`:

  Name of the information, e.g. `number_of_PCs`.

- `content`:

  Actual information, e.g. `30`.

------------------------------------------------------------------------

### `Cerebro$getParameters()`

Retrieve information from `parameters` field.

#### Usage

    Cerebro$getParameters()

#### Returns

`list` of all entries in the `parameters` field.

------------------------------------------------------------------------

### `Cerebro$addTechnicalInfo()`

Add information to `technical_info` field.

#### Usage

    Cerebro$addTechnicalInfo(field, content)

#### Arguments

- `field`:

  Name of the information, e.g. `R`.

- `content`:

  Actual information, e.g. `4.0.2`.

------------------------------------------------------------------------

### `Cerebro$getTechnicalInfo()`

Retrieve information from `technical_info` field.

#### Usage

    Cerebro$getTechnicalInfo()

#### Returns

`list` of all entries in the `technical_info` field.

------------------------------------------------------------------------

### `Cerebro$addGroup()`

Add group to the groups registered in the `groups` field.

#### Usage

    Cerebro$addGroup(group_name, levels)

#### Arguments

- `group_name`:

  Group name.

- `levels`:

  `vector` of group levels (subgroups).

------------------------------------------------------------------------

### `Cerebro$getGroups()`

Retrieve all names in the `groups` field.

#### Usage

    Cerebro$getGroups()

#### Returns

`vector` of registered groups.

------------------------------------------------------------------------

### `Cerebro$getGroupLevels()`

Retrieve group levels for a group registered in the `groups` field.

#### Usage

    Cerebro$getGroupLevels(group_name)

#### Arguments

- `group_name`:

  Group name for which to retrieve group levels.

#### Returns

`vector` of group levels.

------------------------------------------------------------------------

### `Cerebro$setMetaData()`

Set meta data for cells.

#### Usage

    Cerebro$setMetaData(table)

#### Arguments

- `table`:

  `data.frame` that contains meta data for cells. The number of rows
  must be equal to the number of rows of projections and the number of
  columns in the transcript count matrix.

------------------------------------------------------------------------

### `Cerebro$getMetaData()`

Retrieve meta data for cells.

#### Usage

    Cerebro$getMetaData()

#### Returns

`data.frame` containing meta data.

------------------------------------------------------------------------

### `Cerebro$addGeneList()`

Add a gene list to the `gene_lists`.

#### Usage

    Cerebro$addGeneList(name, genes)

#### Arguments

- `name`:

  Name of the gene list.

- `genes`:

  `vector` of genes.

------------------------------------------------------------------------

### `Cerebro$getGeneLists()`

Retrieve gene lists from the `gene_lists`.

#### Usage

    Cerebro$getGeneLists()

#### Returns

`list` of all entries in the `gene_lists` field.

------------------------------------------------------------------------

### `Cerebro$setExpression()`

Set transcript count matrix.

#### Usage

    Cerebro$setExpression(counts, backend = NULL)

#### Arguments

- `counts`:

  `matrix`-like object that contains transcript counts for cells in the
  data set. Number of columns must be equal to the number of rows in the
  `meta_data` field.

- `backend`:

  Optional backend tag. If left `NULL` the object is tagged `"embedded"`
  (the matrix lives inside the `.crb` itself). Callers exporting with
  step-7.2 external-storage modes should pass `setExpressionBackend()`
  directly instead of relying on this default.

------------------------------------------------------------------------

### `Cerebro$setExpressionBackend()`

Tag the object with information about how / where its expression matrix
is stored. In step 7.1 every newly exported `.crb` is tagged
`"embedded"` with a NULL location, meaning the matrix is carried inside
the serialised `.crb`. Later steps (7.2 exporter, 7.3 runtime attach)
will produce objects tagged `"h5"` or `"bpcells"` with an external
`location`.

#### Usage

    Cerebro$setExpressionBackend(type = "embedded", location = NULL)

#### Arguments

- `type`:

  Storage backend label. One of `"embedded"`, `"h5"`, `"bpcells"`. Step
  7.1 only recognises `"embedded"` at runtime; the other two are
  accepted here (so step 7.2 can set them) but will still need step-7.3
  runtime attach to be useful.

- `location`:

  Optional character path (absolute or relative to the parent directory
  of the serialized Cerebro object) where the external matrix lives.
  `NULL` when `type == "embedded"`.

------------------------------------------------------------------------

### `Cerebro$getExpressionBackend()`

Read the expression backend tag. Returns a `list(type, location)`. For
`.crb` files generated before the `expression_backend` field existed the
stored slot is `NULL`; this method graciously falls back to
`list(type = "embedded", location = NULL)` so that downstream code does
not need to special-case legacy objects.

#### Usage

    Cerebro$getExpressionBackend()

------------------------------------------------------------------------

### `Cerebro$getCellNames()`

Get names of all cells.

#### Usage

    Cerebro$getCellNames()

#### Returns

`vector` containing all cell names/barcodes.

------------------------------------------------------------------------

### `Cerebro$getGeneNames()`

Get names of all genes in transcript count matrix.

#### Usage

    Cerebro$getGeneNames()

#### Returns

`vector` containing all gene names in transcript count matrix.

------------------------------------------------------------------------

### `Cerebro$getMeanExpressionForGenes()`

Retrieve mean expression across all cells in the data set for a set of
genes.

#### Usage

    Cerebro$getMeanExpressionForGenes(genes)

#### Arguments

- `genes`:

  Names of genes to extract; no default.

#### Returns

`data.frame` containing specified gene names and their respective mean
expression across all cells in the data set.

------------------------------------------------------------------------

### `Cerebro$getMeanExpressionForCells()`

Retrieve (mean) expression for a single gene or a set of genes for a
given set of cells.

#### Usage

    Cerebro$getMeanExpressionForCells(cells = NULL, genes = NULL)

#### Arguments

- `cells`:

  Names/barcodes of cells to extract; defaults to `NULL`, which will
  return all cells.

- `genes`:

  Names of genes to extract; defaults to `NULL`, which will return all
  genes.

#### Returns

`vector` containing (mean) expression across all specified genes in each
specified cell.

------------------------------------------------------------------------

### `Cerebro$getExpressionMatrix()`

Retrieve transcript count matrix.

#### Usage

    Cerebro$getExpressionMatrix(cells = NULL, genes = NULL)

#### Arguments

- `cells`:

  Names/barcodes of cells to extract; defaults to `NULL`, which will
  return all cells.

- `genes`:

  Names of genes to extract; defaults to `NULL`, which will return all
  genes.

#### Returns

Dense transcript count matrix for specified cells and genes.

------------------------------------------------------------------------

### `Cerebro$getExpressionRow()`

Retrieve a single row of the expression matrix as a named numeric vector
WITHOUT going through the dense helper. Prefer this over
`getExpressionMatrix(genes = gene)` on large or sparse backends where
materialising a 1 x N dense matrix first is wasteful.

#### Usage

    Cerebro$getExpressionRow(gene, cells = NULL)

#### Arguments

- `gene`:

  Name of a single gene. Must exist in the matrix.

- `cells`:

  Names/barcodes of cells to extract; `NULL` returns all cells.

#### Returns

Named `numeric` vector, one entry per requested cell.

------------------------------------------------------------------------

### `Cerebro$getExpressionBlock()`

Retrieve a genes x cells sub-matrix in the backend's NATIVE form (sparse
/ lazy). Callers that need a dense base matrix must apply
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) themselves. Use this
to keep sparse-aware downstream operations
([`Matrix::rowMeans`](https://rdrr.io/pkg/Matrix/man/colSums-methods.html),
[`Matrix::colMeans`](https://rdrr.io/pkg/Matrix/man/colSums-methods.html),
etc.) fast instead of densifying just to aggregate.

#### Usage

    Cerebro$getExpressionBlock(genes, cells = NULL)

#### Arguments

- `genes`:

  Non-empty character vector of gene names.

- `cells`:

  Names/barcodes of cells to extract; `NULL` returns all cells.

#### Returns

A sub-matrix of the same concrete class as `self$expression`:
`dgCMatrix` stays `dgCMatrix`, `RleMatrix` yields `DelayedMatrix`,
`IterableMatrix` stays `IterableMatrix`.

------------------------------------------------------------------------

### `Cerebro$setCellCycle()`

Add columns containing cell cycle assignments to the `cell_cycle` field.

#### Usage

    Cerebro$setCellCycle(cols)

#### Arguments

- `cols`:

  `vector` of columns names containing cell cycle assignments.

------------------------------------------------------------------------

### `Cerebro$getCellCycle()`

Retrieve column names containing cell cycle assignments.

#### Usage

    Cerebro$getCellCycle()

#### Returns

`vector` of column names in meta data.

------------------------------------------------------------------------

### `Cerebro$addProjection()`

Add projections (dimensional reductions).

#### Usage

    Cerebro$addProjection(name, projection)

#### Arguments

- `name`:

  Name of the projection.

- `projection`:

  `data.frame` containing positions of cells in projection.

------------------------------------------------------------------------

### `Cerebro$availableProjections()`

Get list of available projections (dimensional reductions).

#### Usage

    Cerebro$availableProjections()

#### Returns

`vector` of projections / dimensional reductions that are available.

------------------------------------------------------------------------

### `Cerebro$getProjection()`

Retrieve data for a specific projection.

#### Usage

    Cerebro$getProjection(name)

#### Arguments

- `name`:

  Name of projection.

#### Returns

`data.frame` containing the positions of cells in the projection.

------------------------------------------------------------------------

### `Cerebro$addTree()`

Add phylogenetic tree to `trees` field.

#### Usage

    Cerebro$addTree(group_name, tree)

#### Arguments

- `group_name`:

  Group name that this tree belongs to.

- `tree`:

  Phylogenetic tree as `phylo` object.

------------------------------------------------------------------------

### `Cerebro$getTree()`

Retrieve phylogenetic tree for a specific group.

#### Usage

    Cerebro$getTree(group_name)

#### Arguments

- `group_name`:

  Group name for which to retrieve phylogenetic tree.

#### Returns

Phylogenetic tree as `phylo` object.

------------------------------------------------------------------------

### `Cerebro$addMostExpressedGenes()`

Add table of most expressed genes.

#### Usage

    Cerebro$addMostExpressedGenes(group_name, table)

#### Arguments

- `group_name`:

  Name of grouping variable that the most expressed genes belong to.
  Must be registered in the `groups` field.

- `table`:

  `data.frame` that contains the most expressed genes.

------------------------------------------------------------------------

### `Cerebro$getGroupsWithMostExpressedGenes()`

Retrieve names of grouping variables for which most expressed genes are
available.

#### Usage

    Cerebro$getGroupsWithMostExpressedGenes()

#### Returns

`vector` of grouping variables for which most expressed genes are
available.

------------------------------------------------------------------------

### `Cerebro$getMostExpressedGenes()`

Retrieve table of most expressed genes for a specific grouping variable.

#### Usage

    Cerebro$getMostExpressedGenes(group_name)

#### Arguments

- `group_name`:

  Name of grouping variable for which to retrieve most expressed genes.

#### Returns

`data.frame` containing the most expressed genes.

------------------------------------------------------------------------

### `Cerebro$addMeanExpression()`

Add table of mean expression per gene.

#### Usage

    Cerebro$addMeanExpression(group_name, table)

#### Arguments

- `group_name`:

  Name of grouping variable that the mean expression belongs to. Must be
  registered in the `groups` field.

- `table`:

  `data.frame` that contains the mean expression per gene.

------------------------------------------------------------------------

### `Cerebro$getGroupsWithMeanExpression()`

Retrieve names of grouping variables for which mean expression data is
available.

#### Usage

    Cerebro$getGroupsWithMeanExpression()

#### Returns

`vector` of grouping variables for which mean expression is available.

------------------------------------------------------------------------

### `Cerebro$getMeanExpression()`

Retrieve table of mean expression for a specific grouping variable.

#### Usage

    Cerebro$getMeanExpression(group_name)

#### Arguments

- `group_name`:

  Name of grouping variable for which to retrieve mean expression.

#### Returns

`data.frame` containing the mean expression per gene.

------------------------------------------------------------------------

### `Cerebro$addMarkerGenes()`

Add table of marker genes.

#### Usage

    Cerebro$addMarkerGenes(method, name, table)

#### Arguments

- `method`:

  Name of method that was used to generate the marker genes.

- `name`:

  Name of table. This name will be used to select the table in Cerebro.
  It is recommended to use the grouping variable, e.g. `sample`.

- `table`:

  `data.frame` that contains the marker genes.

------------------------------------------------------------------------

### `Cerebro$getMethodsForMarkerGenes()`

Retrieve names of methods that were used to generate marker genes.

#### Usage

    Cerebro$getMethodsForMarkerGenes()

#### Returns

`vector` of names of methods that were used to generate marker genes.

------------------------------------------------------------------------

### `Cerebro$getGroupsWithMarkerGenes()`

Retrieve grouping variables for which marker genes were generated using
a specified method.

#### Usage

    Cerebro$getGroupsWithMarkerGenes(method)

#### Arguments

- `method`:

  Name of method.

#### Returns

`vector` of grouping variables for which marker genes were calculated
using the specified method.

------------------------------------------------------------------------

### `Cerebro$getMarkerGenes()`

Retrieve table of marker genes for specific method and grouping
variable.

#### Usage

    Cerebro$getMarkerGenes(method, name)

#### Arguments

- `method`:

  Name of method.

- `name`:

  Name of table.

#### Returns

`data.frame` that contains marker genes for the specified combination of
method and grouping variable.

------------------------------------------------------------------------

### `Cerebro$addEnrichedPathways()`

Add table of enriched pathways.

#### Usage

    Cerebro$addEnrichedPathways(method, group_name, table)

#### Arguments

- `method`:

  Name of method that was used to calculate enriched pathways.

- `group_name`:

  Name of grouping variable that the enriched pathways belong to. Must
  be registered in the `groups` field.

- `table`:

  `data.frame` that contains the enriched pathways.

------------------------------------------------------------------------

### `Cerebro$getMethodsWithEnrichedPathways()`

Retrieve names of methods for which enriched pathways are available.

#### Usage

    Cerebro$getMethodsWithEnrichedPathways()

#### Returns

`vector` of methods for which enriched pathways are available.

------------------------------------------------------------------------

### `Cerebro$getMethodsForEnrichedPathways()`

Alias of `getMethodsWithEnrichedPathways()`, kept for backwards
compatibility with the Shiny app, which calls this name.

#### Usage

    Cerebro$getMethodsForEnrichedPathways()

#### Returns

`vector` of methods for which enriched pathways are available.

------------------------------------------------------------------------

### `Cerebro$getGroupsWithEnrichedPathways()`

Retrieve names of grouping variables for which enriched pathways are
available for a specific method.

#### Usage

    Cerebro$getGroupsWithEnrichedPathways(method)

#### Arguments

- `method`:

  Name of method for which to retrieve grouping variables.

#### Returns

`vector` of grouping variables for which enriched pathways are
available.

------------------------------------------------------------------------

### `Cerebro$getEnrichedPathways()`

Retrieve table of enriched pathways for a specific method and grouping
variable.

#### Usage

    Cerebro$getEnrichedPathways(method, group_name)

#### Arguments

- `method`:

  Name of method for which to retrieve enriched pathways.

- `group_name`:

  Name of grouping variable for which to retrieve enriched pathways.

#### Returns

`data.frame` containing the enriched pathways.

------------------------------------------------------------------------

### `Cerebro$addTrajectory()`

Add trajectory to `trajectories` field.

#### Usage

    Cerebro$addTrajectory(method, trajectory_name, trajectory)

#### Arguments

- `method`:

  Name of method that was used to calculate trajectory.

- `trajectory_name`:

  Name of trajectory.

- `trajectory`:

  Trajectory data as `data.frame` or `list`.

------------------------------------------------------------------------

### `Cerebro$getMethodsForTrajectories()`

Retrieve names of methods for which trajectories are available.

#### Usage

    Cerebro$getMethodsForTrajectories()

#### Returns

`vector` of methods for which trajectories are available.

------------------------------------------------------------------------

### `Cerebro$getNamesOfTrajectories()`

Retrieve names of trajectories for a specific method.

#### Usage

    Cerebro$getNamesOfTrajectories(method)

#### Arguments

- `method`:

  Name of method for which to retrieve trajectories.

#### Returns

`vector` of trajectories for the specified method.

------------------------------------------------------------------------

### `Cerebro$getTrajectory()`

Retrieve trajectory data for a specific method and trajectory name.

#### Usage

    Cerebro$getTrajectory(method, trajectory_name)

#### Arguments

- `method`:

  Name of method for which to retrieve trajectory.

- `trajectory_name`:

  Name of trajectory to retrieve.

#### Returns

Trajectory data as `data.frame` or `list`.

------------------------------------------------------------------------

### `Cerebro$getBCR()`

Retrieve BCR data

#### Usage

    Cerebro$getBCR()

#### Returns

BCR data stored in the object.

------------------------------------------------------------------------

### `Cerebro$getTCR()`

Retrieve TCR data

#### Usage

    Cerebro$getTCR()

#### Returns

TCR data stored in the object.

------------------------------------------------------------------------

### `Cerebro$addBCRData()`

Add BCR data.

#### Usage

    Cerebro$addBCRData(data)

#### Arguments

- `data`:

  `list` that contains BCR data.

------------------------------------------------------------------------

### `Cerebro$addTCRData()`

Add TCR data.

#### Usage

    Cerebro$addTCRData(data)

#### Arguments

- `data`:

  `list` that contains TCR data.

------------------------------------------------------------------------

### `Cerebro$getImmuneRepertoire()`

Get immune repertoire data. Returns the unified `immune_repertoire`
field if available; otherwise falls back to merging legacy `bcr_data`
and `tcr_data`.

#### Usage

    Cerebro$getImmuneRepertoire()

#### Returns

Named list of data.frames (one per sample), or empty list.

------------------------------------------------------------------------

### `Cerebro$addImmuneRepertoire()`

Set immune repertoire data.

#### Usage

    Cerebro$addImmuneRepertoire(data)

#### Arguments

- `data`:

  Named list of data.frames (one per sample) containing scRepertoire
  columns.

------------------------------------------------------------------------

### `Cerebro$getHLATyping()`

Get HLA typing data (canonical long `data.frame`), or an empty table
when none is stored. Safe on older objects that predate the field.

#### Usage

    Cerebro$getHLATyping()

#### Returns

A canonical HLA typing `data.frame` (possibly zero-row).

------------------------------------------------------------------------

### `Cerebro$addHLATyping()`

Set HLA typing data. Accepts a canonical long `data.frame`, a wide
`data.frame` (sample + HLA-\*\_1/\_2 columns), or a named `list` (sample
-\> allele vector). Non-canonical inputs are normalized; a table that
already has the canonical columns is validated (unrecognisable alleles
dropped, locus re-derived, copy and provenance coerced) rather than
stored verbatim, so a canonical-looking table cannot smuggle invalid
values into downstream analysis.

#### Usage

    Cerebro$addHLATyping(
      data,
      source_type = "unknown",
      typing_method = NA_character_,
      source_reference = NA_character_
    )

#### Arguments

- `data`:

  HLA typing in any accepted form.

- `source_type`:

  Provenance of the genotype: one of "genotyped", "imputed",
  "synthetic", "unknown".

- `typing_method`:

  Optional assay/software string.

- `source_reference`:

  Optional traceable reference (file/cohort/model).

------------------------------------------------------------------------

### `Cerebro$addSpatialData()`

Add spatial data.

#### Usage

    Cerebro$addSpatialData(name, data)

#### Arguments

- `name`:

  Name of the spatial data entry (e.g. image name).

- `data`:

  `list` containing 'coordinates' (data.frame) and 'expression' (sparse
  matrix). It may optionally carry an embedded histology image as
  'image' (a base64 `data:` URI string) plus 'image_bounds' (named list
  xmin/xmax/ymin/ymax in coordinate space) so the Spatial tab can render
  the real tissue background without an external file.

------------------------------------------------------------------------

### `Cerebro$getSpatialData()`

Retrieve spatial data.

#### Usage

    Cerebro$getSpatialData(name)

#### Arguments

- `name`:

  Name of the spatial data entry.

#### Returns

`list` containing 'coordinates' and 'expression'.

------------------------------------------------------------------------

### `Cerebro$availableSpatial()`

Get list of available spatial data entries.

#### Usage

    Cerebro$availableSpatial()

#### Returns

`vector` of spatial data entries that are available.

------------------------------------------------------------------------

### `Cerebro$getTrekker()`

Get Trekker single-cell spatial-mapping data, or `NULL` when none is
stored. Safe on older objects that predate the field (the slot is read
through a `tryCatch` so the getter never errors on a legacy .crb).

#### Usage

    Cerebro$getTrekker()

#### Returns

A `list` with the Trekker page's content, or `NULL`.

------------------------------------------------------------------------

### `Cerebro$addTrekker()`

Set Trekker single-cell spatial-mapping data.

#### Usage

    Cerebro$addTrekker(data)

#### Arguments

- `data`:

  `list` carrying the Trekker page content (coordinates, cell metadata,
  positioning QC, upstream Moran's I, and positioning evidence). See the
  Trekker page module for the expected structure.

------------------------------------------------------------------------

### `Cerebro$addExtraMaterial()`

Add content to extra material field.

#### Usage

    Cerebro$addExtraMaterial(category, name, content)

#### Arguments

- `category`:

  Name of category. At the moment, only `tables` and `plots` are valid
  categories. Tables must be in `data.frame` format and plots must be
  created with `ggplot2`.

- `name`:

  Name of material, will be used to select it in Cerebro.

- `content`:

  Data that should be added.

------------------------------------------------------------------------

### `Cerebro$addExtraTable()`

Add table to \`extra_material\` slot.

#### Usage

    Cerebro$addExtraTable(name, table)

#### Arguments

- `name`:

  Name of material, will be used to select it in Cerebro.

- `table`:

  Table that should be added, must be `data.frame`.

------------------------------------------------------------------------

### `Cerebro$addExtraPlot()`

Add plot to \`extra_material\` slot.

#### Usage

    Cerebro$addExtraPlot(name, plot)

#### Arguments

- `name`:

  Name of material, will be used to select it in Cerebro.

- `plot`:

  Plot that should be added, must be created with `ggplot2` (class:
  `ggplot`).

------------------------------------------------------------------------

### `Cerebro$getExtraMaterial()`

Retrieve extra material from `extra_material` field.

#### Usage

    Cerebro$getExtraMaterial()

#### Returns

`list` of all entries in the `extra_material` field.

------------------------------------------------------------------------

### `Cerebro$getExtraMaterialCategories()`

Get names of categories for which extra material is available.

#### Usage

    Cerebro$getExtraMaterialCategories()

#### Returns

`vector` with names of available categories.

------------------------------------------------------------------------

### `Cerebro$checkForExtraTables()`

Check whether there are tables in the extra materials.

#### Usage

    Cerebro$checkForExtraTables()

#### Returns

`logical` indicating whether there are tables in the extra materials.

------------------------------------------------------------------------

### `Cerebro$getNamesOfExtraTables()`

Get names of tables in extra materials.

#### Usage

    Cerebro$getNamesOfExtraTables()

#### Returns

`vector` containing names of tables in extra materials.

------------------------------------------------------------------------

### `Cerebro$getExtraTable()`

Get table from extra materials.

#### Usage

    Cerebro$getExtraTable(name)

#### Arguments

- `name`:

  Name of table.

#### Returns

Requested table in `data.frame` format.

------------------------------------------------------------------------

### `Cerebro$checkForExtraPlots()`

Check whether there are plots in the extra materials.

#### Usage

    Cerebro$checkForExtraPlots()

#### Returns

`logical` indicating whether there are plots in the extra materials.

------------------------------------------------------------------------

### `Cerebro$getNamesOfExtraPlots()`

Get names of plots in extra materials.

#### Usage

    Cerebro$getNamesOfExtraPlots()

#### Returns

`vector` containing names of plots in extra materials.

------------------------------------------------------------------------

### `Cerebro$getExtraPlot()`

Get plot from extra materials.

#### Usage

    Cerebro$getExtraPlot(name)

#### Arguments

- `name`:

  Name of plot.

#### Returns

Requested plot made with `ggplot2`.

------------------------------------------------------------------------

### `Cerebro$print()`

Show overview of object and the data it contains. Print overview of
available marker gene results for `self$print()` function. Print
overview of available enriched pathway results for `self$print()`
function. Print overview of available trajectories for `self$print()`
function. Print overview of extra material for `self$print()` function.

#### Usage

    Cerebro$print()

------------------------------------------------------------------------

### `Cerebro$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Cerebro$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
