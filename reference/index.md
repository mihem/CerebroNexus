# Package index

## Launch Cerebro

- [`launchCerebro()`](https://mihem.github.io/CerebroNexus/reference/launchCerebro.md)
  : Launch CerebroNexus

## Export data

- [`exportFromSCE()`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md)
  : Export SingleCellExperiment (SCE) object to Cerebro.
- [`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
  : Export Seurat object to Cerebro.
- [`exportToH5ad()`](https://mihem.github.io/CerebroNexus/reference/exportToH5ad.md)
  : Export a Cerebro file to AnnData (h5ad)

## Convert data

- [`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md)
  : Convert a Cerebro data file
- [`convertH5adToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md)
  : Convert an AnnData (h5ad) file to a Cerebro file
- [`convertSCEToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md)
  : Convert a SingleCellExperiment object to a Cerebro file
- [`convertSeuratToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md)
  : Convert Seurat Object to Cerebro Format
- [`convertSpatialDataToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSpatialDataToCerebro.md)
  : Convert a SpatialData store to a Cerebro object
- [`readH5adAsSCE()`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md)
  : Read an h5ad file as a SingleCellExperiment
- [`readSpatialDataAsSCE()`](https://mihem.github.io/CerebroNexus/reference/readSpatialDataAsSCE.md)
  : Read a SpatialData store as a SingleCellExperiment

## Pre-processing

- [`addPercentMtRibo()`](https://mihem.github.io/CerebroNexus/reference/addPercentMtRibo.md)
  : Add percentage of mitochondrial and ribosomal transcripts.
- [`calculatePercentGenes()`](https://mihem.github.io/CerebroNexus/reference/calculatePercentGenes.md)
  : Calculate percentage of transcripts of gene list.
- [`getMostExpressedGenes()`](https://mihem.github.io/CerebroNexus/reference/getMostExpressedGenes.md)
  : Get most expressed genes for specified grouping variables in Seurat
  object.
- [`getMarkerGenes()`](https://mihem.github.io/CerebroNexus/reference/getMarkerGenes.md)
  : Get marker genes for specified grouping variables in Seurat object.
- [`getEnrichedPathways()`](https://mihem.github.io/CerebroNexus/reference/getEnrichedPathways.md)
  : Get enriched pathways based on marker genes from EnrichR.
- [`performGeneSetEnrichmentAnalysis()`](https://mihem.github.io/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md)
  : Perform gene set enrichment analysis with GSVA.
- [`extractMonocleTrajectory()`](https://mihem.github.io/CerebroNexus/reference/extractMonocleTrajectory.md)
  : Extract trajectory from Monocle and add to Seurat object.

## Internal

- [`Cerebro`](https://mihem.github.io/CerebroNexus/reference/Cerebro.md)
  : R6 class in which data sets will be stored for visualization in
  Cerebro.
- [`addImmuneRepertoire()`](https://mihem.github.io/CerebroNexus/reference/addImmuneRepertoire.md)
  : Add immune repertoire data to a Seurat object
- [`addPercentMtRibo()`](https://mihem.github.io/CerebroNexus/reference/addPercentMtRibo.md)
  : Add percentage of mitochondrial and ribosomal transcripts.
- [`buildViewerPack()`](https://mihem.github.io/CerebroNexus/reference/buildViewerPack.md)
  : Build a derived Viewer Pack for an existing CRB
- [`calculatePercentGenes()`](https://mihem.github.io/CerebroNexus/reference/calculatePercentGenes.md)
  : Calculate percentage of transcripts of gene list.
- [`convertCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertCerebro.md)
  : Convert a Cerebro data file
- [`convertH5adToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertH5adToCerebro.md)
  : Convert an AnnData (h5ad) file to a Cerebro file
- [`convertSCEToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSCEToCerebro.md)
  : Convert a SingleCellExperiment object to a Cerebro file
- [`convertSeuratToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSeuratToCerebro.md)
  : Convert Seurat Object to Cerebro Format
- [`convertSpatialDataToCerebro()`](https://mihem.github.io/CerebroNexus/reference/convertSpatialDataToCerebro.md)
  : Convert a SpatialData store to a Cerebro object
- [`createShinyApp()`](https://mihem.github.io/CerebroNexus/reference/createShinyApp.md)
  : Create a self-contained CerebroNexus Shiny app folder
- [`.read_GMT_file()`](https://mihem.github.io/CerebroNexus/reference/dot-read_GMT_file.md)
  : Read GMT file.
- [`.send_enrichr_query()`](https://mihem.github.io/CerebroNexus/reference/dot-send_enrichr_query.md)
  : Gene enrichment using Enrichr.
- [`exportFromSCE()`](https://mihem.github.io/CerebroNexus/reference/exportFromSCE.md)
  : Export SingleCellExperiment (SCE) object to Cerebro.
- [`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
  : Export Seurat object to Cerebro.
- [`exportToH5ad()`](https://mihem.github.io/CerebroNexus/reference/exportToH5ad.md)
  : Export a Cerebro file to AnnData (h5ad)
- [`extractMonocleTrajectory()`](https://mihem.github.io/CerebroNexus/reference/extractMonocleTrajectory.md)
  : Extract trajectory from Monocle and add to Seurat object.
- [`getEnrichedPathways()`](https://mihem.github.io/CerebroNexus/reference/getEnrichedPathways.md)
  : Get enriched pathways based on marker genes from EnrichR.
- [`getMarkerGenes()`](https://mihem.github.io/CerebroNexus/reference/getMarkerGenes.md)
  : Get marker genes for specified grouping variables in Seurat object.
- [`getMostExpressedGenes()`](https://mihem.github.io/CerebroNexus/reference/getMostExpressedGenes.md)
  : Get most expressed genes for specified grouping variables in Seurat
  object.
- [`hla_normalize_typing()`](https://mihem.github.io/CerebroNexus/reference/hla_normalize_typing.md)
  : Normalize any accepted HLA input into the canonical long table
- [`launchCerebro()`](https://mihem.github.io/CerebroNexus/reference/launchCerebro.md)
  : Launch CerebroNexus
- [`performGeneSetEnrichmentAnalysis()`](https://mihem.github.io/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md)
  : Perform gene set enrichment analysis with GSVA.
- [`readCerebro()`](https://mihem.github.io/CerebroNexus/reference/readCerebro.md)
  : Read a Cerebro data file
- [`readH5adAsSCE()`](https://mihem.github.io/CerebroNexus/reference/readH5adAsSCE.md)
  : Read an h5ad file as a SingleCellExperiment
- [`readSpatialDataAsSCE()`](https://mihem.github.io/CerebroNexus/reference/readSpatialDataAsSCE.md)
  : Read a SpatialData store as a SingleCellExperiment
- [`readViewerPack()`](https://mihem.github.io/CerebroNexus/reference/readViewerPack.md)
  : Read a valid Viewer Pack descriptor
- [`saveCerebro()`](https://mihem.github.io/CerebroNexus/reference/saveCerebro.md)
  : Save a Cerebro data file
- [`validateViewerPack()`](https://mihem.github.io/CerebroNexus/reference/validateViewerPack.md)
  : Validate a Viewer Pack
