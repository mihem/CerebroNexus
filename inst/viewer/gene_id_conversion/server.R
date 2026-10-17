##----------------------------------------------------------------------------##
## Table of gene IDs and symbols.
##----------------------------------------------------------------------------##
output[["gene_info"]] <- DT::renderDataTable({
  if (input[["geneIdConversion_organism"]] == "mouse") {
    conversion_table <- read.table(
      paste0(Cerebro.options$cerebro_root, "/extdata/mm10_gene_ID_name.tsv.gz"),
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  } else if (input[["geneIdConversion_organism"]] == "human") {
    conversion_table <- read.table(
      paste0(Cerebro.options$cerebro_root, "/extdata/hg38_gene_ID_name.tsv.gz"),
      sep = "\t",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  }
  DT::datatable(
    style = "bootstrap",
    conversion_table,
    filter = "none",
    selection = "multiple",
    escape = TRUE,
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      scrollX = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "excel", "csv", "print"),
      lengthMenu = c(15, 30, 50, 100),
      pageLength = 50
    )
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(input, "geneIdConversion_info", geneIdConversion_info)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
geneIdConversion_info <- list(
  title = "Gene ID/symbol conversion",
  text = p(
    "Map Gencode, Ensembl and Havana IDs to gene symbols and types. References: mouse M16; human 27."
  )
)
