##----------------------------------------------------------------------------##
## Tab: Trekker — UI.
##
## Layout mirrors the other module pages: a left column of parameter boxes
## (cerebroBox) and a right column with the visualization boxes. All controls are
## standard app widgets (selectInput / sliderInput / materialSwitch / selectize),
## rendered server-side in `trekker_parameters_ui` / `trekker_coordsource_ui`, so
## the page uses exactly the same components and theme as every other tab.
##
## The two dual-linked scatter panes are the only bespoke element (a WebGL-free
## canvas pair with morphing + lassoing); their DOM is filled by www/trekker.js
## from the `trekker_data` message. Every custom id is `tk-`-prefixed and every
## custom style rule is scoped under `.trekker-page` (www/trekker.css).
##----------------------------------------------------------------------------##

tab_trekker <- tabItem(
  tabName = "trekker",
  ## ---- Dataset metadata strip (populated by trekker.js) ------------------ ##
  div(
    class = "trekker-page",
    div(
      class = "tk-meta",
      tags$span(class = "tk-badge", id = "tk-b-assay", "Trekker"),
      tags$span(class = "tk-badge tk-badge-gray", id = "tk-b-tile", "Tile —"),
      tags$span(class = "tk-badge tk-badge-soft", "No histology image"),
      div(class = "tk-sub", id = "tk-subline", "—")
    )
  ),
  fluidRow(
    class = "cerebro-viz-row",
    ## ---- Left column: parameter boxes ------------------------------------ ##
    column(
      width = 3,
      offset = 0,
      class = "cerebro-param-col",
      cerebroBox(
        title = tagList(
          "Parameters",
          cerebroInfoButton("trekker_parameters_info")
        ),
        content = uiOutput("trekker_parameters_ui")
      ),
      cerebroBox(
        title = tagList(
          "Coordinate source",
          cerebroInfoButton("trekker_coordsource_info")
        ),
        content = uiOutput("trekker_coordsource_ui")
      )
    ),
    ## ---- Right column: visualization boxes ------------------------------- ##
    column(
      width = 9,
      offset = 0,
      class = "cerebro-viz-col",
      cerebroBox(
        title = tagList(
          "Physical space and transcriptome space",
          cerebroInfoButton("trekker_viz_info"),
          tags$span(
            class = "tk-note",
            id = "tk-vnote",
            "same nuclei · two coordinate systems"
          )
        ),
        content = div(
          class = "trekker-page",
          div(
            class = "tk-panes",
            id = "tk-panes",
            div(
              class = "tk-pane",
              id = "tk-pane-sp",
              tags$h4(
                class = "tk-pane-h",
                tags$span("Spatial"),
                tags$span(class = "tk-u", id = "tk-u-sp", "µm · Location CSV")
              ),
              tags$canvas(id = "tk-cv-sp"),
              div(class = "tk-tip", id = "tk-tip-sp")
            ),
            div(
              class = "tk-pane",
              id = "tk-pane-um",
              tags$h4(
                class = "tk-pane-h",
                tags$span("UMAP"),
                tags$span(class = "tk-u", "whole transcriptome")
              ),
              tags$canvas(id = "tk-cv-um"),
              div(class = "tk-tip", id = "tk-tip-um")
            )
          ),
          div(
            class = "tk-selbar",
            id = "tk-selbar",
            style = "display:none",
            tags$span(id = "tk-seltext", "—"),
            tags$button(
              id = "tk-selclear",
              class = "tk-linkbtn",
              "Clear selection"
            )
          ),
          div(class = "tk-legend", id = "tk-legend"),
          div(
            class = "tk-cbar",
            id = "tk-cbar",
            style = "display:none",
            tags$span(id = "tk-cb0", "0"),
            div(class = "tk-grad", id = "tk-grad"),
            tags$span(id = "tk-cb1", "1"),
            tags$span(
              class = "tk-cbar-note",
              id = "tk-cbar-note",
              "SCT normalized"
            )
          ),
          div(class = "tk-fieldsummary", id = "tk-fieldsummary"),
          div(
            class = "tk-hint",
            HTML(
              "Drag to lasso-select in either pane and the other highlights in ",
              "sync. Click a single nucleus to open the Cell inspector below."
            )
          )
        )
      ),
      cerebroBox(
        title = tagList(
          "Cell inspector",
          cerebroInfoButton("trekker_inspector_info")
        ),
        content = div(
          class = "trekker-page",
          div(
            class = "tk-inspbody",
            id = "tk-inspbody",
            div(
              class = "tk-empty",
              "Click a nucleus to see its identity, physical neighbourhood, and positioning evidence."
            )
          )
        )
      ),
      cerebroBox(
        title = tagList(
          "Data and QC",
          cerebroInfoButton("trekker_qc_info")
        ),
        content = div(
          class = "trekker-page",
          div(class = "tk-grid", id = "tk-stats"),
          div(
            class = "tk-two",
            div(
              tags$h4(class = "tk-sub-h", "Positioning class distribution"),
              tags$table(
                class = "tk-table",
                tags$thead(tags$tr(
                  tags$th("Spatial locations"),
                  tags$th(class = "num", "Nuclei"),
                  tags$th(class = "num", "Share"),
                  tags$th("Handling")
                )),
                tags$tbody(id = "tk-postbl")
              ),
              div(class = "tk-flag", id = "tk-salvflag")
            ),
            div(
              tags$h4(class = "tk-sub-h", "Provenance"),
              tags$dl(class = "tk-kv", id = "tk-prov"),
              div(class = "tk-flag", id = "tk-rangeflag")
            )
          )
        )
      ),
      cerebroBox(
        title = tagList(
          "Positioning evidence",
          cerebroInfoButton("trekker_evidence_info")
        ),
        content = div(
          class = "trekker-page",
          div(class = "tk-exgrid", id = "tk-exgrid"),
          div(
            class = "tk-hint",
            HTML(
              "Left = every bead barcode associated with the nucleus (coloured by nUMI); ",
              "<code>*</code> marks the adopted centroid, <code>-</code> a rejected candidate. ",
              "Right = the bead barcodes as a UMI knee plot. \"Why is this nucleus here\" is a ",
              "question the other spatial platforms never face — their positions are not inferred."
            )
          )
        )
      ),
      cerebroBox(
        title = tagList(
          "Spatial autocorrelation — Moran's I",
          cerebroInfoButton("trekker_moran_info")
        ),
        content = div(
          class = "trekker-page",
          tags$table(
            class = "tk-table",
            tags$thead(tags$tr(
              tags$th(class = "num", "#"),
              tags$th("Gene"),
              tags$th(class = "num", "Moran's I"),
              tags$th("Show in plot")
            )),
            tags$tbody(id = "tk-morantbl")
          ),
          div(
            class = "tk-hint",
            HTML(
              "Source: <code>..._variable_features_spatial_moransi.txt</code> (the upstream, ",
              "vendor pipeline value). Cerebro's own Moran's I uses Euclidean 6-NN — a ",
              "different algorithm — so the two are labelled separately and never mixed."
            )
          )
        )
      )
    )
  ),
  ## ---- Full-size positioning-evidence zoom dialog ----------------------- ##
  tags$dialog(
    id = "tk-zoom",
    tags$button(
      class = "tk-zoom-x",
      onclick = "document.getElementById('tk-zoom').close()",
      "Close"
    ),
    tags$img(id = "tk-zoomimg")
  )
)
