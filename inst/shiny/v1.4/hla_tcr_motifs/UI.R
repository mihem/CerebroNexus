##----------------------------------------------------------------------------##
## Tab: HLA & TCR Motifs
##
## A standalone top-level page (peer to Immune Repertoire) that rebuilds the
## CDR3 Hamming-1 motif network and layers donor-level HLA context onto it.
##
## Subtitle is a hard constraint from the design: everything on this page is
## exploratory HLA CONTEXT and association, never inferred restriction.
##
## Layout mirrors the other module pages: a left column of parameter boxes and
## a right column with a visualization tab strip (Motif Network / HLA
## Associations / Data & QC).
##----------------------------------------------------------------------------##

tab_hla_tcr_motifs <- tabItem(
  tabName = "hla_tcr_motifs",
  fluidRow(
    class = "cerebro-viz-row",
    ## ---- Left column: parameter boxes ---------------------------------- ##
    column(
      width = 3,
      offset = 0,
      class = "cerebro-param-col",
      style = "padding: 0px;",
      tagList(
        cerebroBox(
          title = tagList(
            "Parameters",
            cerebroInfoButton("hla_parameters_info")
          ),
          content = uiOutput("hla_parameters_ui")
        ),
        cerebroBox(
          title = tagList(
            "Evidence status",
            cerebroInfoButton("hla_status_info")
          ),
          content = uiOutput("hla_status_ui")
        )
      )
    ),
    ## ---- Right column: visualization tab strip ------------------------- ##
    column(
      width = 9,
      offset = 0,
      style = "padding: 0px;",
      cerebroBox(
        title = tagList(
          "HLA & TCR Motifs",
          cerebroInfoButton("hla_visualizations_info"),
          tags$span(
            style = "font-size: 12px; font-weight: normal; color: #888;",
            "Exploratory HLA context and association — not inferred restriction."
          )
        ),
        content = tabsetPanel(
          id = "hla_tabs",
          tabPanel(
            "Motif Network",
            br(),
            shinycssloaders::withSpinner(
              visNetwork::visNetworkOutput(
                "hla_plot_motifNetwork",
                height = "640px"
              )
            ),
            uiOutput("hla_node_details"),
            uiOutput("hla_motif_note"),
            # A picture cannot be recomputed or audited; the tables and their
            # manifest can. See output$hla_export_analysis.
            downloadButton(
              "hla_export_analysis",
              "Download analysis (tables + manifest)",
              class = "btn-sm"
            )
          ),
          tabPanel(
            "HLA Associations",
            br(),
            uiOutput("hla_associations_ui")
          ),
          tabPanel(
            "Data & QC",
            br(),
            uiOutput("hla_data_qc_ui")
          )
        )
      )
    )
  )
)
