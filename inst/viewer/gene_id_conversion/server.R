##----------------------------------------------------------------------------##
## Table of gene IDs and symbols.
##----------------------------------------------------------------------------##
output[["gene_info"]] <- DT::renderDataTable({
  ## read once per process (viewerGeneIdTable() in shiny_server.R)
  conversion_table <- viewerGeneIdTable(input[["geneIdConversion_organism"]])
  req(conversion_table)
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
