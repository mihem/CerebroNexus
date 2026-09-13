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
        cerebroToolbarActions(
          cerebroSettingsButton("hla_more_button", "hla_more"),
          cerebroShareButton("hla_motif_network")
        ),
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
        ),
        cerebroSelectionStatus(
          "hla_motif_network",
          "hla_selected_count",
          portable = FALSE
        )
      )
    ),
    column(
      width = 12,
      offset = 0,
      class = "cerebro-viz-col",
      shiny::tagAppendAttributes(
        tabsetPanel(
          id = "hla_tabs",
          tabPanel(
            "Motif Network",
            # The legend remains a full-width row above the shared Canvas.
            tags$div(
              class = "hla-motif-tab",
              uiOutput("hla_legend_ui", class = "hla-legend-row"),
              cerebroCellViewOutput("hla_motif_network")
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
            uiOutput("hla_associations_ui")
          ),
          tabPanel(
            "Data & QC",
            uiOutput("hla_data_qc_ui")
          )
        ),
        class = "cerebro-analysis-tabs"
      )
    )
  )
)
