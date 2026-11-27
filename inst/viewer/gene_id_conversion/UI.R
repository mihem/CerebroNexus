##----------------------------------------------------------------------------##
## Tab: Gene ID/symbol conversion
##----------------------------------------------------------------------------##
tab_gene_id_conversion <- tabItem(
  tabName = "geneIdConversion",
  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Convert gene ID <-> gene symbol"),
        cerebroInfoButton("geneIdConversion_info")
      ),
      tagList(
        selectInput(
          "geneIdConversion_organism",
          "Organism:",
          choices = c("mouse", "human")
        ),
        uiOutput("gene_info_UI", class = "cerebro-result-slot")
      )
    )
  )
)
