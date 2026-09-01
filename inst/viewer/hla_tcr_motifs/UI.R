##----------------------------------------------------------------------------##
## Tab: HLA & TCR Motifs
##
## A standalone top-level page (peer to Immune Repertoire) that rebuilds the
## CDR3 Hamming-1 motif network and layers donor-level HLA context onto it.
##
## Subtitle is a hard constraint from the design: everything on this page is
## exploratory HLA CONTEXT and association, never inferred restriction.
##
## Primary analysis choices stay above the visualization. Secondary analysis,
## display, and evidence controls use the same fixed settings drawer as the
## other specialist pages.
##----------------------------------------------------------------------------##

tab_hla_tcr_motifs <- tabItem(
  tabName = "hla_tcr_motifs",
  cerebroVizPageHeader(
    "HLA & TCR Motifs",
    "hla_visualizations_info",
    "Exploratory HLA context and association — not inferred restriction."
  ),
  fluidRow(
    class = "cerebro-viz-row cerebro-viz-top-layout",
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-toolbar-col",
      div(
        class = "cerebro-viz-toolbar",
        div(
          class = "cerebro-viz-primary",
          uiOutput("hla_parameters_ui")
        ),
        cerebroSettingsButton("hla_more_button", "hla_more"),
        cerebroSettingsDrawer(
          "hla_more",
          cerebroSettingsSection(
            "Appearance",
            uiOutput("hla_additional_params_ui"),
            cerebroInfoButton("hla_additional_parameters_info")
          ),
          cerebroSettingsSection(
            "Analysis",
            uiOutput("hla_more_parameters_ui")
          ),
          cerebroSettingsSection(
            "Evidence status",
            div(
              class = "cerebro-settings-full",
              uiOutput("hla_status_ui")
            ),
            cerebroInfoButton("hla_status_info")
          )
        )
      )
    ),
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-col",
      cerebroSelectionStatus(
        "hla_motif_network",
        "hla_selected_count",
        client_actions = FALSE
      ),
      cerebroBox(
        title = NULL,
        collapsible = FALSE,
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
                ),
                uiOutput(
                  "hla_motif_network_composition",
                  class = "cerebro-selection-composition-slot"
                )
              )
            ),
            tags$div(
              id = "hla-node-details",
              class = "well well-sm hla-node-details",
              style = "display:none"
            ),
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
            # Rendered server-side: the second grain is one row per OBSERVATION
            # UNIT, which is a cell only when the data set says so. A bulk
            # repertoire's rows are analysis units, so the label has to follow
            # the declared unit rather than being hard-coded here.
            uiOutput("hla_table_grain_ui"),
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
