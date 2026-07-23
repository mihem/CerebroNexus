##----------------------------------------------------------------------------##
## Tab: Trekker — UI.
##
## Layout mirrors the other module pages: a left column of parameter boxes
## (cerebroBox) and a right column with the visualization boxes. All controls are
## standard app widgets (selectInput / sliderInput / materialSwitch / selectize),
## rendered server-side in `trekker_parameters_ui` / `trekker_group_filters_ui`,
## so the page uses exactly the same components and theme as every other tab.
##
## The two dual-linked scatter panes are the only bespoke element (a WebGL-free
## canvas pair with a modebar-style toolbar — pan / zoom / box + lasso select —
## and morphing); their DOM is filled by www/trekker.js from the `trekker_data`
## message. Every custom id is `tk-`-prefixed and every custom style rule is
## scoped under `.trekker-page` (www/trekker.css).
##----------------------------------------------------------------------------##

tab_trekker <- tabItem(
  tabName = "trekker",
  ## ---- Dataset metadata strip (populated by trekker.js) ------------------ ##
  div(
    class = "trekker-page",
    div(
      class = "tk-meta",
      tags$span(class = "tk-badge", id = "tk-b-assay", "Trekker"),
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
          "Group filters",
          cerebroInfoButton("trekker_group_filters_info")
        ),
        content = uiOutput("trekker_group_filters_ui")
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
          cerebroInfoButton("trekker_viz_info")
        ),
        content = div(
          class = "trekker-page",
          div(
            class = "tk-plot-wrap",
            div(class = "tk-modebar", id = "tk-modebar"),
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
              "Use the toolbar (top-right) to box- or lasso-select, pan, or zoom ",
              "(the scroll wheel zooms too); a selection in either pane highlights ",
              "the same nuclei in the other. Click a single nucleus to open the ",
              "Cell inspector below."
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
        collapsed = TRUE,
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
              "vendor pipeline value). CerebroNexus's own Moran's I uses Euclidean 6-NN — a ",
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
