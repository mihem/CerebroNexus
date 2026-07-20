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
            "Additional parameters",
            cerebroInfoButton("hla_additional_parameters_info")
          ),
          content = uiOutput("hla_additional_params_ui"),
          collapsed = TRUE
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
      # cerebro-viz-col: every other viz page carries this so the plot column
      # absorbs the row's slack and can shrink below its content (min-width:0)
      # instead of forcing overflow. HLA was the one page that omitted it, so the
      # careful flex rules in custom.css never applied here.
      class = "cerebro-viz-col",
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
            # The legend and the network share one positioning context so the
            # modebar can float at its top-right: it lands on the legend's row
            # when a legend is shown (reclaiming that otherwise-empty right side),
            # and at the plot's top-right when the legend is hidden (the legend
            # collapses to zero height). A modebar matching the app's plotly one
            # is drawn by www/hla_motifs.js (visNetwork's own green nav buttons
            # are turned off in visualizations.R for consistency).
            tags$div(
              class = "hla-motif-tab",
              tags$div(class = "hla-modebar", id = "hla-modebar"),
              uiOutput("hla_legend_ui"),
              # Fill the viewport instead of a hardcoded 640px: the wrapper is
              # sized to (viewport - its live top - a bottom gap) by
              # fill_height.js, and the network renders at height:100% inside it.
              # The legend above is a sibling, so when it wraps the wrapper's top
              # moves and the height re-measures itself. See www/fill_height.js.
              tags$div(
                class = "hla-plot-wrap",
                tags$div(
                  class = "cerebro-fill",
                  shinycssloaders::withSpinner(
                    visNetwork::visNetworkOutput(
                      "hla_plot_motifNetwork",
                      height = "100%"
                    )
                  )
                )
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
            "Network data",
            br(),
            radioButtons(
              "hla_table_grain",
              "Rows:",
              choices = c(
                "By motif (node)" = "node",
                "By cell" = "cell"
              ),
              selected = "node",
              inline = TRUE
            ),
            tags$p(
              class = "text-muted",
              style = "font-size: 12px;",
              paste(
                "The rows behind the network shown on Motif Network, under the",
                "current chain / scope / allele / min-size filters. 'By motif' is",
                "one row per CDR3 node; 'By cell' is one row per cell."
              )
            ),
            DT::dataTableOutput("hla_network_table"),
            br(),
            downloadButton(
              "hla_network_download",
              "Download CSV",
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
