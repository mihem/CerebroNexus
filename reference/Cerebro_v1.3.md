# R6 class in which data sets will be stored for visualization in Cerebro.

A `Cerebro_v1.3` object is an R6 class that contains several types of
data that can be visualized in Cerebro.

## Value

A new `Cerebro_v1.3` object.

## Public fields

- `version`:

  cerebroApp version that was used to create the object.

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

- [`Cerebro_v1.3$new()`](#method-Cerebro_v1.3-new)

- [`Cerebro_v1.3$setVersion()`](#method-Cerebro_v1.3-setVersion)

- [`Cerebro_v1.3$getVersion()`](#method-Cerebro_v1.3-getVersion)

- [`Cerebro_v1.3$checkIfGroupExists()`](#method-Cerebro_v1.3-checkIfGroupExists)

- [`Cerebro_v1.3$checkIfColumnExistsInMetadata()`](#method-Cerebro_v1.3-checkIfColumnExistsInMetadata)

- [`Cerebro_v1.3$addExperiment()`](#method-Cerebro_v1.3-addExperiment)

- [`Cerebro_v1.3$getExperiment()`](#method-Cerebro_v1.3-getExperiment)

- [`Cerebro_v1.3$addParameters()`](#method-Cerebro_v1.3-addParameters)

- [`Cerebro_v1.3$getParameters()`](#method-Cerebro_v1.3-getParameters)

- [`Cerebro_v1.3$addTechnicalInfo()`](#method-Cerebro_v1.3-addTechnicalInfo)

- [`Cerebro_v1.3$getTechnicalInfo()`](#method-Cerebro_v1.3-getTechnicalInfo)

- [`Cerebro_v1.3$addGroup()`](#method-Cerebro_v1.3-addGroup)

- [`Cerebro_v1.3$getGroups()`](#method-Cerebro_v1.3-getGroups)

- [`Cerebro_v1.3$getGroupLevels()`](#method-Cerebro_v1.3-getGroupLevels)

- [`Cerebro_v1.3$setMetaData()`](#method-Cerebro_v1.3-setMetaData)

- [`Cerebro_v1.3$getMetaData()`](#method-Cerebro_v1.3-getMetaData)

- [`Cerebro_v1.3$addGeneList()`](#method-Cerebro_v1.3-addGeneList)

- [`Cerebro_v1.3$getGeneLists()`](#method-Cerebro_v1.3-getGeneLists)

- [`Cerebro_v1.3$setExpression()`](#method-Cerebro_v1.3-setExpression)

- [`Cerebro_v1.3$setExpressionBackend()`](#method-Cerebro_v1.3-setExpressionBackend)

- [`Cerebro_v1.3$getExpressionBackend()`](#method-Cerebro_v1.3-getExpressionBackend)

- [`Cerebro_v1.3$getCellNames()`](#method-Cerebro_v1.3-getCellNames)

- [`Cerebro_v1.3$getGeneNames()`](#method-Cerebro_v1.3-getGeneNames)

- [`Cerebro_v1.3$getMeanExpressionForGenes()`](#method-Cerebro_v1.3-getMeanExpressionForGenes)

- [`Cerebro_v1.3$getMeanExpressionForCells()`](#method-Cerebro_v1.3-getMeanExpressionForCells)

- [`Cerebro_v1.3$getExpressionMatrix()`](#method-Cerebro_v1.3-getExpressionMatrix)

- [`Cerebro_v1.3$getExpressionRow()`](#method-Cerebro_v1.3-getExpressionRow)

- [`Cerebro_v1.3$getExpressionBlock()`](#method-Cerebro_v1.3-getExpressionBlock)

- [`Cerebro_v1.3$setCellCycle()`](#method-Cerebro_v1.3-setCellCycle)

- [`Cerebro_v1.3$getCellCycle()`](#method-Cerebro_v1.3-getCellCycle)

- [`Cerebro_v1.3$addProjection()`](#method-Cerebro_v1.3-addProjection)

- [`Cerebro_v1.3$availableProjections()`](#method-Cerebro_v1.3-availableProjections)

- [`Cerebro_v1.3$getProjection()`](#method-Cerebro_v1.3-getProjection)

- [`Cerebro_v1.3$addTree()`](#method-Cerebro_v1.3-addTree)

- [`Cerebro_v1.3$getTree()`](#method-Cerebro_v1.3-getTree)

- [`Cerebro_v1.3$addMostExpressedGenes()`](#method-Cerebro_v1.3-addMostExpressedGenes)

- [`Cerebro_v1.3$getGroupsWithMostExpressedGenes()`](#method-Cerebro_v1.3-getGroupsWithMostExpressedGenes)

- [`Cerebro_v1.3$getMostExpressedGenes()`](#method-Cerebro_v1.3-getMostExpressedGenes)

- [`Cerebro_v1.3$addMeanExpression()`](#method-Cerebro_v1.3-addMeanExpression)

- [`Cerebro_v1.3$getGroupsWithMeanExpression()`](#method-Cerebro_v1.3-getGroupsWithMeanExpression)

- [`Cerebro_v1.3$getMeanExpression()`](#method-Cerebro_v1.3-getMeanExpression)

- [`Cerebro_v1.3$addMarkerGenes()`](#method-Cerebro_v1.3-addMarkerGenes)

- [`Cerebro_v1.3$getMethodsForMarkerGenes()`](#method-Cerebro_v1.3-getMethodsForMarkerGenes)

- [`Cerebro_v1.3$getGroupsWithMarkerGenes()`](#method-Cerebro_v1.3-getGroupsWithMarkerGenes)

- [`Cerebro_v1.3$getMarkerGenes()`](#method-Cerebro_v1.3-getMarkerGenes)

- [`Cerebro_v1.3$addEnrichedPathways()`](#method-Cerebro_v1.3-addEnrichedPathways)

- [`Cerebro_v1.3$getMethodsWithEnrichedPathways()`](#method-Cerebro_v1.3-getMethodsWithEnrichedPathways)

- [`Cerebro_v1.3$getMethodsForEnrichedPathways()`](#method-Cerebro_v1.3-getMethodsForEnrichedPathways)

- [`Cerebro_v1.3$getGroupsWithEnrichedPathways()`](#method-Cerebro_v1.3-getGroupsWithEnrichedPathways)

- [`Cerebro_v1.3$getEnrichedPathways()`](#method-Cerebro_v1.3-getEnrichedPathways)

- [`Cerebro_v1.3$addTrajectory()`](#method-Cerebro_v1.3-addTrajectory)

- [`Cerebro_v1.3$getMethodsForTrajectories()`](#method-Cerebro_v1.3-getMethodsForTrajectories)

- [`Cerebro_v1.3$getNamesOfTrajectories()`](#method-Cerebro_v1.3-getNamesOfTrajectories)

- [`Cerebro_v1.3$getTrajectory()`](#method-Cerebro_v1.3-getTrajectory)

- [`Cerebro_v1.3$getBCR()`](#method-Cerebro_v1.3-getBCR)

- [`Cerebro_v1.3$getTCR()`](#method-Cerebro_v1.3-getTCR)

- [`Cerebro_v1.3$addBCRData()`](#method-Cerebro_v1.3-addBCRData)

- [`Cerebro_v1.3$addTCRData()`](#method-Cerebro_v1.3-addTCRData)

- [`Cerebro_v1.3$getImmuneRepertoire()`](#method-Cerebro_v1.3-getImmuneRepertoire)

- [`Cerebro_v1.3$addImmuneRepertoire()`](#method-Cerebro_v1.3-addImmuneRepertoire)

- [`Cerebro_v1.3$getHLATyping()`](#method-Cerebro_v1.3-getHLATyping)

- [`Cerebro_v1.3$addHLATyping()`](#method-Cerebro_v1.3-addHLATyping)

- [`Cerebro_v1.3$addSpatialData()`](#method-Cerebro_v1.3-addSpatialData)

- [`Cerebro_v1.3$getSpatialData()`](#method-Cerebro_v1.3-getSpatialData)

- [`Cerebro_v1.3$availableSpatial()`](#method-Cerebro_v1.3-availableSpatial)

- [`Cerebro_v1.3$getTrekker()`](#method-Cerebro_v1.3-getTrekker)

- [`Cerebro_v1.3$addTrekker()`](#method-Cerebro_v1.3-addTrekker)

- [`Cerebro_v1.3$addExtraMaterial()`](#method-Cerebro_v1.3-addExtraMaterial)

- [`Cerebro_v1.3$addExtraTable()`](#method-Cerebro_v1.3-addExtraTable)

- [`Cerebro_v1.3$addExtraPlot()`](#method-Cerebro_v1.3-addExtraPlot)

- [`Cerebro_v1.3$getExtraMaterial()`](#method-Cerebro_v1.3-getExtraMaterial)

- [`Cerebro_v1.3$getExtraMaterialCategories()`](#method-Cerebro_v1.3-getExtraMaterialCategories)

- [`Cerebro_v1.3$checkForExtraTables()`](#method-Cerebro_v1.3-checkForExtraTables)

- [`Cerebro_v1.3$getNamesOfExtraTables()`](#method-Cerebro_v1.3-getNamesOfExtraTables)

- [`Cerebro_v1.3$getExtraTable()`](#method-Cerebro_v1.3-getExtraTable)

- [`Cerebro_v1.3$checkForExtraPlots()`](#method-Cerebro_v1.3-checkForExtraPlots)

- [`Cerebro_v1.3$getNamesOfExtraPlots()`](#method-Cerebro_v1.3-getNamesOfExtraPlots)

- [`Cerebro_v1.3$getExtraPlot()`](#method-Cerebro_v1.3-getExtraPlot)

- [`Cerebro_v1.3$print()`](#method-Cerebro_v1.3-print)

- [`Cerebro_v1.3$clone()`](#method-Cerebro_v1.3-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Cerebro_v1.3` object.

#### Usage

    Cerebro_v1.3$new()

#### Returns

A new `Cerebro_v1.3` object.

------------------------------------------------------------------------

### Method `setVersion()`

Set the version of `cerebroApp` that was used to generate this object.

#### Usage

    Cerebro_v1.3$setVersion(version)

#### Arguments

- `version`:

  Version to set.

------------------------------------------------------------------------

### Method `getVersion()`

Get the version of `cerebroApp` that was used to generate this object.

#### Usage

    Cerebro_v1.3$getVersion()

#### Returns

Version as `package_version` class.

------------------------------------------------------------------------

### Method `checkIfGroupExists()`

Safety function that will check if a provided group name is present in
the `groups` field.

#### Usage

    Cerebro_v1.3$checkIfGroupExists(group_name)

#### Arguments

- `group_name`:

  Group name to be tested

------------------------------------------------------------------------

### Method `checkIfColumnExistsInMetadata()`

Safety function that will check if a provided group name is present in
the meta data.

#### Usage

    Cerebro_v1.3$checkIfColumnExistsInMetadata(group_name)

#### Arguments

- `group_name`:

  Group name to be tested.

------------------------------------------------------------------------

### Method `addExperiment()`

Add information to `experiment` field.

#### Usage

    Cerebro_v1.3$addExperiment(field, content)

#### Arguments

- `field`:

  Name of the information, e.g. `organism`.

- `content`:

  Actual information, e.g. `hg`.

------------------------------------------------------------------------

### Method `getExperiment()`

Retrieve information from `experiment` field.

#### Usage

    Cerebro_v1.3$getExperiment()

#### Returns

`list` of all entries in the `experiment` field.

------------------------------------------------------------------------

### Method `addParameters()`

Add information to `parameters` field.

#### Usage

    Cerebro_v1.3$addParameters(field, content)

#### Arguments

- `field`:

  Name of the information, e.g. `number_of_PCs`.

- `content`:

  Actual information, e.g. `30`.

------------------------------------------------------------------------

### Method `getParameters()`

Retrieve information from `parameters` field.

#### Usage

    Cerebro_v1.3$getParameters()

#### Returns

`list` of all entries in the `parameters` field.

------------------------------------------------------------------------

### Method `addTechnicalInfo()`

Add information to `technical_info` field.

#### Usage

    Cerebro_v1.3$addTechnicalInfo(field, content)

#### Arguments

- `field`:

  Name of the information, e.g. `R`.

- `content`:

  Actual information, e.g. `4.0.2`.

------------------------------------------------------------------------

### Method `getTechnicalInfo()`

Retrieve information from `technical_info` field.

#### Usage

    Cerebro_v1.3$getTechnicalInfo()

#### Returns

`list` of all entries in the `technical_info` field.

------------------------------------------------------------------------

### Method `addGroup()`

Add group to the groups registered in the `groups` field.

#### Usage

    Cerebro_v1.3$addGroup(group_name, levels)

#### Arguments

- `group_name`:

  Group name.

- `levels`:

  `vector` of group levels (subgroups).

------------------------------------------------------------------------

### Method `getGroups()`

Retrieve all names in the `groups` field.

#### Usage

    Cerebro_v1.3$getGroups()

#### Returns

`vector` of registered groups.

------------------------------------------------------------------------

### Method `getGroupLevels()`

Retrieve group levels for a group registered in the `groups` field.

#### Usage

    Cerebro_v1.3$getGroupLevels(group_name)

#### Arguments

- `group_name`:

  Group name for which to retrieve group levels.

#### Returns

`vector` of group levels.

------------------------------------------------------------------------

### Method `setMetaData()`

Set meta data for cells.

#### Usage

    Cerebro_v1.3$setMetaData(table)

#### Arguments

- `table`:

  `data.frame` that contains meta data for cells. The number of rows
  must be equal to the number of rows of projections and the number of
  columns in the transcript count matrix.

------------------------------------------------------------------------

### Method `getMetaData()`

Retrieve meta data for cells.

#### Usage

    Cerebro_v1.3$getMetaData()

#### Returns

`data.frame` containing meta data.

------------------------------------------------------------------------

### Method `addGeneList()`

Add a gene list to the `gene_lists`.

#### Usage

    Cerebro_v1.3$addGeneList(name, genes)

#### Arguments

- `name`:

  Name of the gene list.

- `genes`:

  `vector` of genes.

------------------------------------------------------------------------

### Method `getGeneLists()`

Retrieve gene lists from the `gene_lists`.

#### Usage

    Cerebro_v1.3$getGeneLists()

#### Returns

`list` of all entries in the `gene_lists` field.

------------------------------------------------------------------------

### Method `setExpression()`

Set transcript count matrix.

#### Usage

    Cerebro_v1.3$setExpression(counts, backend = NULL)

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

### Method `setExpressionBackend()`

Tag the object with information about how / where its expression matrix
is stored. In step 7.1 every newly exported `.crb` is tagged
`"embedded"` with a NULL location, meaning the matrix is carried inside
the serialised `.crb`. Later steps (7.2 exporter, 7.3 runtime attach)
will produce objects tagged `"h5"` or `"bpcells"` with an external
`location`.

#### Usage

    Cerebro_v1.3$setExpressionBackend(type = "embedded", location = NULL)

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

### Method `getExpressionBackend()`

Read the expression backend tag. Returns a `list(type, location)`. For
`.crb` files generated before the `expression_backend` field existed the
stored slot is `NULL`; this method graciously falls back to
`list(type = "embedded", location = NULL)` so that downstream code does
not need to special-case legacy objects.

#### Usage

    Cerebro_v1.3$getExpressionBackend()

------------------------------------------------------------------------

### Method `getCellNames()`

Get names of all cells.

#### Usage

    Cerebro_v1.3$getCellNames()

#### Returns

`vector` containing all cell names/barcodes.

------------------------------------------------------------------------

### Method `getGeneNames()`

Get names of all genes in transcript count matrix.

#### Usage

    Cerebro_v1.3$getGeneNames()

#### Returns

`vector` containing all gene names in transcript count matrix.

------------------------------------------------------------------------

### Method `getMeanExpressionForGenes()`

Retrieve mean expression across all cells in the data set for a set of
genes.

#### Usage

    Cerebro_v1.3$getMeanExpressionForGenes(genes)

#### Arguments

- `genes`:

  Names of genes to extract; no default.

#### Returns

`data.frame` containing specified gene names and their respective mean
expression across all cells in the data set.

------------------------------------------------------------------------

### Method `getMeanExpressionForCells()`

Retrieve (mean) expression for a single gene or a set of genes for a
given set of cells.

#### Usage

    Cerebro_v1.3$getMeanExpressionForCells(cells = NULL, genes = NULL)

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

### Method `getExpressionMatrix()`

Retrieve transcript count matrix.

#### Usage

    Cerebro_v1.3$getExpressionMatrix(cells = NULL, genes = NULL)

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

### Method `getExpressionRow()`

Retrieve a single row of the expression matrix as a named numeric vector
WITHOUT going through the dense helper. Prefer this over
`getExpressionMatrix(genes = gene)` on large or sparse backends where
materialising a 1 x N dense matrix first is wasteful.

#### Usage

    Cerebro_v1.3$getExpressionRow(gene, cells = NULL)

#### Arguments

- `gene`:

  Name of a single gene. Must exist in the matrix.

- `cells`:

  Names/barcodes of cells to extract; `NULL` returns all cells.

#### Returns

Named `numeric` vector, one entry per requested cell.

------------------------------------------------------------------------

### Method `getExpressionBlock()`

Retrieve a genes x cells sub-matrix in the backend's NATIVE form (sparse
/ lazy). Callers that need a dense base matrix must apply
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) themselves. Use this
to keep sparse-aware downstream operations
([`Matrix::rowMeans`](https://rdrr.io/pkg/Matrix/man/colSums-methods.html),
[`Matrix::colMeans`](https://rdrr.io/pkg/Matrix/man/colSums-methods.html),
etc.) fast instead of densifying just to aggregate.

#### Usage

    Cerebro_v1.3$getExpressionBlock(genes, cells = NULL)

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

### Method `setCellCycle()`

Add columns containing cell cycle assignments to the `cell_cycle` field.

#### Usage

    Cerebro_v1.3$setCellCycle(cols)

#### Arguments

- `cols`:

  `vector` of columns names containing cell cycle assignments.

------------------------------------------------------------------------

### Method `getCellCycle()`

Retrieve column names containing cell cycle assignments.

#### Usage

    Cerebro_v1.3$getCellCycle()

#### Returns

`vector` of column names in meta data.

------------------------------------------------------------------------

### Method `addProjection()`

Add projections (dimensional reductions).

#### Usage

    Cerebro_v1.3$addProjection(name, projection)

#### Arguments

- `name`:

  Name of the projection.

- `projection`:

  `data.frame` containing positions of cells in projection.

------------------------------------------------------------------------

### Method `availableProjections()`

Get list of available projections (dimensional reductions).

#### Usage

    Cerebro_v1.3$availableProjections()

#### Returns

`vector` of projections / dimensional reductions that are available.

------------------------------------------------------------------------

### Method `getProjection()`

Retrieve data for a specific projection.

#### Usage

    Cerebro_v1.3$getProjection(name)

#### Arguments

- `name`:

  Name of projection.

#### Returns

`data.frame` containing the positions of cells in the projection.

------------------------------------------------------------------------

### Method `addTree()`

Add phylogenetic tree to `trees` field.

#### Usage

    Cerebro_v1.3$addTree(group_name, tree)

#### Arguments

- `group_name`:

  Group name that this tree belongs to.

- `tree`:

  Phylogenetic tree as `phylo` object.

------------------------------------------------------------------------

### Method `getTree()`

Retrieve phylogenetic tree for a specific group.

#### Usage

    Cerebro_v1.3$getTree(group_name)

#### Arguments

- `group_name`:

  Group name for which to retrieve phylogenetic tree.

#### Returns

Phylogenetic tree as `phylo` object.

------------------------------------------------------------------------

### Method `addMostExpressedGenes()`

Add table of most expressed genes.

#### Usage

    Cerebro_v1.3$addMostExpressedGenes(group_name, table)

#### Arguments

- `group_name`:

  Name of grouping variable that the most expressed genes belong to.
  Must be registered in the `groups` field.

- `table`:

  `data.frame` that contains the most expressed genes.

------------------------------------------------------------------------

### Method `getGroupsWithMostExpressedGenes()`

Retrieve names of grouping variables for which most expressed genes are
available.

#### Usage

    Cerebro_v1.3$getGroupsWithMostExpressedGenes()

#### Returns

`vector` of grouping variables for which most expressed genes are
available.

------------------------------------------------------------------------

### Method [`getMostExpressedGenes()`](https://mihem.github.io/CerebroNexus/reference/getMostExpressedGenes.md)

Retrieve table of most expressed genes for a specific grouping variable.

#### Usage

    Cerebro_v1.3$getMostExpressedGenes(group_name)

#### Arguments

- `group_name`:

  Name of grouping variable for which to retrieve most expressed genes.

#### Returns

`data.frame` containing the most expressed genes.

------------------------------------------------------------------------

### Method `addMeanExpression()`

Add table of mean expression per gene.

#### Usage

    Cerebro_v1.3$addMeanExpression(group_name, table)

#### Arguments

- `group_name`:

  Name of grouping variable that the mean expression belongs to. Must be
  registered in the `groups` field.

- `table`:

  `data.frame` that contains the mean expression per gene.

------------------------------------------------------------------------

### Method `getGroupsWithMeanExpression()`

Retrieve names of grouping variables for which mean expression data is
available.

#### Usage

    Cerebro_v1.3$getGroupsWithMeanExpression()

#### Returns

`vector` of grouping variables for which mean expression is available.

------------------------------------------------------------------------

### Method `getMeanExpression()`

Retrieve table of mean expression for a specific grouping variable.

#### Usage

    Cerebro_v1.3$getMeanExpression(group_name)

#### Arguments

- `group_name`:

  Name of grouping variable for which to retrieve mean expression.

#### Returns

`data.frame` containing the mean expression per gene.

------------------------------------------------------------------------

### Method `addMarkerGenes()`

Add table of marker genes.

#### Usage

    Cerebro_v1.3$addMarkerGenes(method, name, table)

#### Arguments

- `method`:

  Name of method that was used to generate the marker genes.

- `name`:

  Name of table. This name will be used to select the table in Cerebro.
  It is recommended to use the grouping variable, e.g. `sample`.

- `table`:

  `data.frame` that contains the marker genes.

------------------------------------------------------------------------

### Method `getMethodsForMarkerGenes()`

Retrieve names of methods that were used to generate marker genes.

#### Usage

    Cerebro_v1.3$getMethodsForMarkerGenes()

#### Returns

`vector` of names of methods that were used to generate marker genes.

------------------------------------------------------------------------

### Method `getGroupsWithMarkerGenes()`

Retrieve grouping variables for which marker genes were generated using
a specified method.

#### Usage

    Cerebro_v1.3$getGroupsWithMarkerGenes(method)

#### Arguments

- `method`:

  Name of method.

#### Returns

`vector` of grouping variables for which marker genes were calculated
using the specified method.

------------------------------------------------------------------------

### Method [`getMarkerGenes()`](https://mihem.github.io/CerebroNexus/reference/getMarkerGenes.md)

Retrieve table of marker genes for specific method and grouping
variable.

#### Usage

    Cerebro_v1.3$getMarkerGenes(method, name)

#### Arguments

- `method`:

  Name of method.

- `name`:

  Name of table.

#### Returns

`data.frame` that contains marker genes for the specified combination of
method and grouping variable.

------------------------------------------------------------------------

### Method `addEnrichedPathways()`

Add table of enriched pathways.

#### Usage

    Cerebro_v1.3$addEnrichedPathways(method, group_name, table)

#### Arguments

- `method`:

  Name of method that was used to calculate enriched pathways.

- `group_name`:

  Name of grouping variable that the enriched pathways belong to. Must
  be registered in the `groups` field.

- `table`:

  `data.frame` that contains the enriched pathways.

------------------------------------------------------------------------

### Method `getMethodsWithEnrichedPathways()`

Retrieve names of methods for which enriched pathways are available.

#### Usage

    Cerebro_v1.3$getMethodsWithEnrichedPathways()

#### Returns

`vector` of methods for which enriched pathways are available.

------------------------------------------------------------------------

### Method `getMethodsForEnrichedPathways()`

Alias of `getMethodsWithEnrichedPathways()`, kept for backwards
compatibility with the Shiny app, which calls this name.

#### Usage

    Cerebro_v1.3$getMethodsForEnrichedPathways()

#### Returns

`vector` of methods for which enriched pathways are available.

------------------------------------------------------------------------

### Method `getGroupsWithEnrichedPathways()`

Retrieve names of grouping variables for which enriched pathways are
available for a specific method.

#### Usage

    Cerebro_v1.3$getGroupsWithEnrichedPathways(method)

#### Arguments

- `method`:

  Name of method for which to retrieve grouping variables.

#### Returns

`vector` of grouping variables for which enriched pathways are
available.

------------------------------------------------------------------------

### Method [`getEnrichedPathways()`](https://mihem.github.io/CerebroNexus/reference/getEnrichedPathways.md)

Retrieve table of enriched pathways for a specific method and grouping
variable.

#### Usage

    Cerebro_v1.3$getEnrichedPathways(method, group_name)

#### Arguments

- `method`:

  Name of method for which to retrieve enriched pathways.

- `group_name`:

  Name of grouping variable for which to retrieve enriched pathways.

#### Returns

`data.frame` containing the enriched pathways.

------------------------------------------------------------------------

### Method `addTrajectory()`

Add trajectory to `trajectories` field.

#### Usage

    Cerebro_v1.3$addTrajectory(method, trajectory_name, trajectory)

#### Arguments

- `method`:

  Name of method that was used to calculate trajectory.

- `trajectory_name`:

  Name of trajectory.

- `trajectory`:

  Trajectory data as `data.frame` or `list`.

------------------------------------------------------------------------

### Method `getMethodsForTrajectories()`

Retrieve names of methods for which trajectories are available.

#### Usage

    Cerebro_v1.3$getMethodsForTrajectories()

#### Returns

`vector` of methods for which trajectories are available.

------------------------------------------------------------------------

### Method `getNamesOfTrajectories()`

Retrieve names of trajectories for a specific method.

#### Usage

    Cerebro_v1.3$getNamesOfTrajectories(method)

#### Arguments

- `method`:

  Name of method for which to retrieve trajectories.

#### Returns

`vector` of trajectories for the specified method.

------------------------------------------------------------------------

### Method `getTrajectory()`

Retrieve trajectory data for a specific method and trajectory name.

#### Usage

    Cerebro_v1.3$getTrajectory(method, trajectory_name)

#### Arguments

- `method`:

  Name of method for which to retrieve trajectory.

- `trajectory_name`:

  Name of trajectory to retrieve.

#### Returns

Trajectory data as `data.frame` or `list`.

------------------------------------------------------------------------

### Method `getBCR()`

Retrieve BCR data

#### Usage

    Cerebro_v1.3$getBCR()

#### Returns

BCR data stored in the object.

------------------------------------------------------------------------

### Method `getTCR()`

Retrieve TCR data

#### Usage

    Cerebro_v1.3$getTCR()

#### Returns

TCR data stored in the object.

------------------------------------------------------------------------

### Method `addBCRData()`

Add BCR data.

#### Usage

    Cerebro_v1.3$addBCRData(data)

#### Arguments

- `data`:

  `list` that contains BCR data.

------------------------------------------------------------------------

### Method `addTCRData()`

Add TCR data.

#### Usage

    Cerebro_v1.3$addTCRData(data)

#### Arguments

- `data`:

  `list` that contains TCR data.

------------------------------------------------------------------------

### Method `getImmuneRepertoire()`

Get immune repertoire data. Returns the unified `immune_repertoire`
field if available; otherwise falls back to merging legacy `bcr_data`
and `tcr_data`.

#### Usage

    Cerebro_v1.3$getImmuneRepertoire()

#### Returns

Named list of data.frames (one per sample), or empty list.

------------------------------------------------------------------------

### Method [`addImmuneRepertoire()`](https://mihem.github.io/CerebroNexus/reference/addImmuneRepertoire.md)

Set immune repertoire data.

#### Usage

    Cerebro_v1.3$addImmuneRepertoire(data)

#### Arguments

- `data`:

  Named list of data.frames (one per sample) containing scRepertoire
  columns.

------------------------------------------------------------------------

### Method `getHLATyping()`

Get HLA typing data (canonical long `data.frame`), or an empty table
when none is stored. Safe on older objects that predate the field.

#### Usage

    Cerebro_v1.3$getHLATyping()

#### Returns

A canonical HLA typing `data.frame` (possibly zero-row).

------------------------------------------------------------------------

### Method `addHLATyping()`

Set HLA typing data. Accepts a canonical long `data.frame`, a wide
`data.frame` (sample + HLA-\*\_1/\_2 columns), or a named `list` (sample
-\> allele vector). Non-canonical inputs are normalized; a table that
already has the canonical columns is validated (unrecognisable alleles
dropped, locus re-derived, copy and provenance coerced) rather than
stored verbatim, so a canonical-looking table cannot smuggle invalid
values into downstream analysis.

#### Usage

    Cerebro_v1.3$addHLATyping(
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

### Method `addSpatialData()`

Add spatial data.

#### Usage

    Cerebro_v1.3$addSpatialData(name, data)

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

### Method `getSpatialData()`

Retrieve spatial data.

#### Usage

    Cerebro_v1.3$getSpatialData(name)

#### Arguments

- `name`:

  Name of the spatial data entry.

#### Returns

`list` containing 'coordinates' and 'expression'.

------------------------------------------------------------------------

### Method `availableSpatial()`

Get list of available spatial data entries.

#### Usage

    Cerebro_v1.3$availableSpatial()

#### Returns

`vector` of spatial data entries that are available.

------------------------------------------------------------------------

### Method `getTrekker()`

Get Trekker single-cell spatial-mapping data, or `NULL` when none is
stored. Safe on older objects that predate the field (the slot is read
through a `tryCatch` so the getter never errors on a legacy .crb).

#### Usage

    Cerebro_v1.3$getTrekker()

#### Returns

A `list` with the Trekker page's content, or `NULL`.

------------------------------------------------------------------------

### Method `addTrekker()`

Set Trekker single-cell spatial-mapping data.

#### Usage

    Cerebro_v1.3$addTrekker(data)

#### Arguments

- `data`:

  `list` carrying the Trekker page content (coordinates, cell metadata,
  positioning QC, upstream Moran's I, and positioning evidence). See the
  Trekker page module for the expected structure.

------------------------------------------------------------------------

### Method `addExtraMaterial()`

Add content to extra material field.

#### Usage

    Cerebro_v1.3$addExtraMaterial(category, name, content)

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

### Method `addExtraTable()`

Add table to \`extra_material\` slot.

#### Usage

    Cerebro_v1.3$addExtraTable(name, table)

#### Arguments

- `name`:

  Name of material, will be used to select it in Cerebro.

- `table`:

  Table that should be added, must be `data.frame`.

------------------------------------------------------------------------

### Method `addExtraPlot()`

Add plot to \`extra_material\` slot.

#### Usage

    Cerebro_v1.3$addExtraPlot(name, plot)

#### Arguments

- `name`:

  Name of material, will be used to select it in Cerebro.

- `plot`:

  Plot that should be added, must be created with `ggplot2` (class:
  `ggplot`).

------------------------------------------------------------------------

### Method `getExtraMaterial()`

Retrieve extra material from `extra_material` field.

#### Usage

    Cerebro_v1.3$getExtraMaterial()

#### Returns

`list` of all entries in the `extra_material` field.

------------------------------------------------------------------------

### Method `getExtraMaterialCategories()`

Get names of categories for which extra material is available.

#### Usage

    Cerebro_v1.3$getExtraMaterialCategories()

#### Returns

`vector` with names of available categories.

------------------------------------------------------------------------

### Method `checkForExtraTables()`

Check whether there are tables in the extra materials.

#### Usage

    Cerebro_v1.3$checkForExtraTables()

#### Returns

`logical` indicating whether there are tables in the extra materials.

------------------------------------------------------------------------

### Method `getNamesOfExtraTables()`

Get names of tables in extra materials.

#### Usage

    Cerebro_v1.3$getNamesOfExtraTables()

#### Returns

`vector` containing names of tables in extra materials.

------------------------------------------------------------------------

### Method `getExtraTable()`

Get table from extra materials.

#### Usage

    Cerebro_v1.3$getExtraTable(name)

#### Arguments

- `name`:

  Name of table.

#### Returns

Requested table in `data.frame` format.

------------------------------------------------------------------------

### Method `checkForExtraPlots()`

Check whether there are plots in the extra materials.

#### Usage

    Cerebro_v1.3$checkForExtraPlots()

#### Returns

`logical` indicating whether there are plots in the extra materials.

------------------------------------------------------------------------

### Method `getNamesOfExtraPlots()`

Get names of plots in extra materials.

#### Usage

    Cerebro_v1.3$getNamesOfExtraPlots()

#### Returns

`vector` containing names of plots in extra materials.

------------------------------------------------------------------------

### Method `getExtraPlot()`

Get plot from extra materials.

#### Usage

    Cerebro_v1.3$getExtraPlot(name)

#### Arguments

- `name`:

  Name of plot.

#### Returns

Requested plot made with `ggplot2`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Show overview of object and the data it contains. Print overview of
available marker gene results for `self$print()` function. Print
overview of available enriched pathway results for `self$print()`
function. Print overview of available trajectories for `self$print()`
function. Print overview of extra material for `self$print()` function.

#### Usage

    Cerebro_v1.3$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Cerebro_v1.3$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
