# Package index

## Launch Cerebro

- [`launchCerebro()`](https://www.mheming.com/CerebroNexus/reference/launchCerebro.md)
  : Launch CerebroNexus
- [`launchCerebroBuilder()`](https://www.mheming.com/CerebroNexus/reference/launchCerebroBuilder.md)
  : Build a Cerebro data set from a Seurat object, by pointing and
  clicking.
- [`downloadDemoData()`](https://www.mheming.com/CerebroNexus/reference/downloadDemoData.md)
  : Download the larger demo data sets

## Export data

- [`exportFromSCE()`](https://www.mheming.com/CerebroNexus/reference/exportFromSCE.md)
  : Export SingleCellExperiment (SCE) object to Cerebro.
- [`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
  : Export Seurat object to Cerebro.
- [`exportToH5ad()`](https://www.mheming.com/CerebroNexus/reference/exportToH5ad.md)
  : Export a Cerebro file to AnnData (h5ad)

## Convert data

- [`convertCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertCerebro.md)
  : Convert a Cerebro data file
- [`convertH5adToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertH5adToCerebro.md)
  : Convert an AnnData (h5ad) file to a Cerebro file
- [`convertSCEToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSCEToCerebro.md)
  : Convert a SingleCellExperiment object to a Cerebro file
- [`convertSCEToSeurat()`](https://www.mheming.com/CerebroNexus/reference/convertSCEToSeurat.md)
  : Convert a SingleCellExperiment into a Seurat object
- [`convertSeuratToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSeuratToCerebro.md)
  : Convert Seurat Object to Cerebro Format
- [`convertSpatialDataToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSpatialDataToCerebro.md)
  : Convert a SpatialData store to a Cerebro object
- [`readH5adAsSCE()`](https://www.mheming.com/CerebroNexus/reference/readH5adAsSCE.md)
  : Read an h5ad file as a SingleCellExperiment
- [`readSpatialDataAsSCE()`](https://www.mheming.com/CerebroNexus/reference/readSpatialDataAsSCE.md)
  : Read a SpatialData store as a SingleCellExperiment

## Pre-processing

- [`addPercentMtRibo()`](https://www.mheming.com/CerebroNexus/reference/addPercentMtRibo.md)
  : Add percentage of mitochondrial and ribosomal transcripts.
- [`calculatePercentGenes()`](https://www.mheming.com/CerebroNexus/reference/calculatePercentGenes.md)
  : Calculate percentage of transcripts of gene list.
- [`getMostExpressedGenes()`](https://www.mheming.com/CerebroNexus/reference/getMostExpressedGenes.md)
  : Get most expressed genes for specified grouping variables in Seurat
  object.
- [`getMarkerGenes()`](https://www.mheming.com/CerebroNexus/reference/getMarkerGenes.md)
  : Get marker genes for specified grouping variables in Seurat object.
- [`getEnrichedPathways()`](https://www.mheming.com/CerebroNexus/reference/getEnrichedPathways.md)
  : Get enriched pathways based on marker genes from EnrichR.
- [`performGeneSetEnrichmentAnalysis()`](https://www.mheming.com/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md)
  : Perform gene set enrichment analysis with GSVA.
- [`extractMonocleTrajectory()`](https://www.mheming.com/CerebroNexus/reference/extractMonocleTrajectory.md)
  : Extract trajectory from Monocle and add to Seurat object.

## Internal

- [`Cerebro`](https://www.mheming.com/CerebroNexus/reference/Cerebro.md)
  : R6 class in which data sets will be stored for visualization in
  Cerebro.
- [`addImmuneRepertoire()`](https://www.mheming.com/CerebroNexus/reference/addImmuneRepertoire.md)
  : Add immune repertoire data to a Seurat object
- [`addPercentMtRibo()`](https://www.mheming.com/CerebroNexus/reference/addPercentMtRibo.md)
  : Add percentage of mitochondrial and ribosomal transcripts.
- [`buildViewerPack()`](https://www.mheming.com/CerebroNexus/reference/buildViewerPack.md)
  : Build a derived Viewer Pack for an existing CRB
- [`calculatePercentGenes()`](https://www.mheming.com/CerebroNexus/reference/calculatePercentGenes.md)
  : Calculate percentage of transcripts of gene list.
- [`convertCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertCerebro.md)
  : Convert a Cerebro data file
- [`convertH5adToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertH5adToCerebro.md)
  : Convert an AnnData (h5ad) file to a Cerebro file
- [`convertSCEToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSCEToCerebro.md)
  : Convert a SingleCellExperiment object to a Cerebro file
- [`convertSCEToSeurat()`](https://www.mheming.com/CerebroNexus/reference/convertSCEToSeurat.md)
  : Convert a SingleCellExperiment into a Seurat object
- [`convertSeuratToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSeuratToCerebro.md)
  : Convert Seurat Object to Cerebro Format
- [`convertSpatialDataToCerebro()`](https://www.mheming.com/CerebroNexus/reference/convertSpatialDataToCerebro.md)
  : Convert a SpatialData store to a Cerebro object
- [`createShinyApp()`](https://www.mheming.com/CerebroNexus/reference/createShinyApp.md)
  : Create a self-contained CerebroNexus Shiny app folder
- [`.read_GMT_file()`](https://www.mheming.com/CerebroNexus/reference/dot-read_GMT_file.md)
  : Read GMT file.
- [`.send_enrichr_query()`](https://www.mheming.com/CerebroNexus/reference/dot-send_enrichr_query.md)
  : Gene enrichment using Enrichr.
- [`downloadDemoData()`](https://www.mheming.com/CerebroNexus/reference/downloadDemoData.md)
  : Download the larger demo data sets
- [`exportFromSCE()`](https://www.mheming.com/CerebroNexus/reference/exportFromSCE.md)
  : Export SingleCellExperiment (SCE) object to Cerebro.
- [`exportFromSeurat()`](https://www.mheming.com/CerebroNexus/reference/exportFromSeurat.md)
  : Export Seurat object to Cerebro.
- [`exportToH5ad()`](https://www.mheming.com/CerebroNexus/reference/exportToH5ad.md)
  : Export a Cerebro file to AnnData (h5ad)
- [`extractMonocleTrajectory()`](https://www.mheming.com/CerebroNexus/reference/extractMonocleTrajectory.md)
  : Extract trajectory from Monocle and add to Seurat object.
- [`getEnrichedPathways()`](https://www.mheming.com/CerebroNexus/reference/getEnrichedPathways.md)
  : Get enriched pathways based on marker genes from EnrichR.
- [`getMarkerGenes()`](https://www.mheming.com/CerebroNexus/reference/getMarkerGenes.md)
  : Get marker genes for specified grouping variables in Seurat object.
- [`getMostExpressedGenes()`](https://www.mheming.com/CerebroNexus/reference/getMostExpressedGenes.md)
  : Get most expressed genes for specified grouping variables in Seurat
  object.
- [`hla_normalize_typing()`](https://www.mheming.com/CerebroNexus/reference/hla_normalize_typing.md)
  : Normalize any accepted HLA input into the canonical long table
- [`launchCerebro()`](https://www.mheming.com/CerebroNexus/reference/launchCerebro.md)
  : Launch CerebroNexus
- [`launchCerebroBuilder()`](https://www.mheming.com/CerebroNexus/reference/launchCerebroBuilder.md)
  : Build a Cerebro data set from a Seurat object, by pointing and
  clicking.
- [`performGeneSetEnrichmentAnalysis()`](https://www.mheming.com/CerebroNexus/reference/performGeneSetEnrichmentAnalysis.md)
  : Perform gene set enrichment analysis with GSVA.
- [`readCerebro()`](https://www.mheming.com/CerebroNexus/reference/readCerebro.md)
  : Read a Cerebro data file
- [`readH5adAsSCE()`](https://www.mheming.com/CerebroNexus/reference/readH5adAsSCE.md)
  : Read an h5ad file as a SingleCellExperiment
- [`readSpatialDataAsSCE()`](https://www.mheming.com/CerebroNexus/reference/readSpatialDataAsSCE.md)
  : Read a SpatialData store as a SingleCellExperiment
- [`readViewerPack()`](https://www.mheming.com/CerebroNexus/reference/readViewerPack.md)
  : Read a valid Viewer Pack descriptor
- [`saveCerebro()`](https://www.mheming.com/CerebroNexus/reference/saveCerebro.md)
  : Save a Cerebro data file
- [`validateViewerPack()`](https://www.mheming.com/CerebroNexus/reference/validateViewerPack.md)
  : Validate a Viewer Pack
