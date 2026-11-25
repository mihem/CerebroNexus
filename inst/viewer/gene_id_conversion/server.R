##----------------------------------------------------------------------------##
## Table of gene IDs and symbols.
##----------------------------------------------------------------------------##
output[["gene_info_UI"]] <- renderUI({
  DT::dataTableOutput("gene_info")
})

output[["gene_info"]] <- viewerLazyRender(function() {
  DT::renderDataTable({
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
        deferRender = TRUE,
        autoWidth = FALSE,
        dom = "Bfrtip",
        buttons = c("copy", "excel", "csv", "print"),
        lengthMenu = c(15, 30, 50, 100),
        pageLength = 50
      )
    )
  })
})
if (exists("viewer_hidden_output_options", inherits = TRUE)) {
  outputOptions(output, "gene_info_UI", priority = 10)
  outputOptions(output, "gene_info", suspendWhenHidden = FALSE)
}

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
