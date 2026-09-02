##----------------------------------------------------------------------------##
## Tab: Linked views — UI.
##
## The workspace that makes the paper's central claim demonstrable: a selection
## in ANY panel propagates as a coordinated highlight to every other panel,
## across modalities — expression (UMAP), physical space (Spatial), AND the
## immune axis (Clonal) — with a live composition + top-clonotype readout.
##
## Layout is deliberately different from the other tabs: NO left param column.
## A horizontal control bar sits above a full-width panel grid, so the linked
## views get the room the standard param/viz split does not. All controls carry
## `cv-` ids and are wired client-side by www/cell_views.js; the server sends a
## single per-dataset bundle. Custom styles are scoped under `.coordviews-page`.
##----------------------------------------------------------------------------##

## Per-panel hover modebar (plotly-style). `panel` is the JS panel key ("A"/"B").
## Lasso is the default active drag mode. All buttons are wired client-side.
## The label goes in `data-tip`, NOT `title`: the native tooltip's delay is the
## browser's to decide (~1s in Chrome) and cannot be configured, which is far too
## slow for a toolbar you sweep across. CSS draws it instead (see .cv-tbtn::after)
## on a 350ms delay. `aria-label` then carries the accessible name that `title`
## used to provide — these buttons have no text of their own.
cv_panebar <- function(panel) {
  tbtn <- function(act, tip, ic, active = FALSE) {
    tags$button(
      type = "button",
      class = if (active) "cv-tbtn is-on" else "cv-tbtn",
      `data-act` = act,
      `data-panel` = panel,
      `data-tip` = tip,
      `aria-label` = tip,
      icon(ic)
    )
  }
  div(
    class = "cv-panebar",
    tbtn("box", "Box select", "vector-square"),
    tbtn("lasso", "Lasso select", "draw-polygon", active = TRUE),
    ## Pan is its own mode (and is also reachable from any mode via middle-drag
    ## or shift-drag) — without it a zoomed panel can only be reset, never moved.
    tbtn("pan", "Pan · or shift-drag", "up-down-left-right"),
    ## Only meaningful for a 3-D embedding, so JS reveals it on the panels whose
    ## space carries a third dimension and leaves it hidden everywhere else.
    tags$button(
      type = "button",
      class = "cv-tbtn cv-orbit-btn",
      `data-act` = "orbit",
      `data-panel` = panel,
      style = "display:none",
      `data-tip` = "Rotate · 3-D embedding",
      `aria-label` = "Rotate the 3-D embedding",
      icon("cube")
    ),
    tbtn("zin", "Zoom in", "search-plus"),
    tbtn("zout", "Zoom out", "search-minus"),
    ## Zoom THIS panel to the selection. The top bar's button does the expression
    ## panel only; getting in close on the tissue or the clonal layout needs a
    ## per-panel one. JS reveals it while a selection exists.
    tags$button(
      type = "button",
      class = "cv-tbtn cv-zsel-btn",
      `data-act` = "zsel",
      `data-panel` = panel,
      style = "display:none",
      `data-tip` = "Zoom this panel to the selection",
      `aria-label` = "Zoom this panel to the selection",
      icon("crop-simple")
    ),
    ## A house, not the four-corner "expand" glyph that was here: that one reads
    ## as fullscreen everywhere else. This is the same mark plotly puts on its
    ## "Reset axes" button, which is where these users are coming from.
    tbtn("reset", "Reset view", "house"),
    tags$button(
      type = "button",
      class = "cv-tbtn cv-clear-btn",
      `data-act` = "clear",
      `data-panel` = panel,
      style = "display:none",
      `data-tip` = "Clear selection",
      `aria-label` = "Clear selection",
      icon("eraser")
    ),
    tbtn("png", "Download PNG", "download")
  )
}

## One panel slot. Four initial slots avoid DOM churn for the common case;
## www/cell_views.js clones more when several spatial sections push the linked
## workspace beyond four panels, then lays all visible slots out responsively.
## Every head carries a (hidden) Trekker info button, shown by JS on whichever
## panel ends up holding the Trekker space.
cv_pane <- function(key) {
  low <- tolower(key)
  div(
    class = "cv-pane cv-hidden",
    div(
      class = "cv-pane-head",
      tags$span(class = "cv-ptitle", id = paste0("cv-title-", low), "—"),
      tags$span(
        class = "cv-role-badge",
        id = paste0("cv-role-", low),
        style = "display:none"
      ),
      tags$button(
        type = "button",
        class = "cv-tbtn",
        id = paste0("cv-tk-info-", low),
        `data-act` = "trekker-info",
        style = "display:none",
        `data-tip` = "Trekker coordinate source, QC & Moran's I",
        `aria-label` = "Trekker coordinate source, QC and Moran's I",
        icon("circle-info")
      ),
      cv_panebar(key),
      ## Focus is a primary workspace action, not an advanced plotting tool.
      ## Keep it labelled and visible; the title click and canvas double-click
      ## remain shortcuts rather than the only way to discover the capability.
      tags$button(
        type = "button",
        class = "cv-tbtn cv-focus-btn",
        `data-act` = "focus",
        `data-panel` = key,
        `data-tip` = "Make this the focus",
        `aria-label` = "Make this the focus",
        `aria-pressed` = "false",
        icon("expand"),
        tags$span(class = "cv-focus-label", "Focus")
      )
    ),
    ## The canvas gets a positioned wrapper so the minimap can sit at the
    ## CANVAS's bottom-left rather than the pane's. It matters because the canvas
    ## is not always flush with the pane: in the three-space layout the UMAP pane
    ## is stretched over two rows and its canvas centres itself in the extra
    ## height. Anchoring in CSS also means the minimap follows the canvas's
    ## width/height transition for free, instead of being re-measured per frame.
    div(
      class = "cv-canvas-wrap",
      tags$canvas(id = paste0("cv-cv-", low)),
      ## Read-only overview: the whole space in miniature with a frame marking
      ## the visible part. Only shown while the panel is zoomed or panned.
      tags$canvas(class = "cv-mini", id = paste0("cv-mini-", low)),
      ## Inside the wrapper, so the coordinates the hover code computes (which
      ## are relative to the CANVAS) are the coordinates this is positioned by.
      ## As a child of the pane it was offset by the header's height.
      tags$button(
        type = "button",
        class = "cv-moran-badge cv-moran-corner",
        id = paste0("cv-moran-", low),
        `data-act` = "moran-info",
        `data-panel` = key,
        style = "display:none"
      ),
      div(class = "cv-tip", id = paste0("cv-tip-", low))
    )
  )
}

tab_coordinated_views <- tabItem(
  tabName = "coordinated_views",
  div(
    class = "coordviews-page linked-views-page",
    ## ---- header + info -------------------------------------------------- ##
    div(
      style = "display:flex;align-items:baseline;gap:10px;margin-bottom:2px;",
      tags$h3(
        style = "font-size:18px;font-weight:650;margin:0;",
        "Linked views"
      ),
      cerebroInfoButton("coordinated_views_info")
    ),
    div(
      class = "cv-meta",
      id = "cv-meta",
      "Load a single-cell data set to explore its modalities together."
    ),
    ## ---- horizontal control bar (the layout fix) ------------------------ ##
    div(
      class = "cv-topbar",
      div(
        class = "cv-ctl",
        tags$label("Colour by"),
        tags$select(id = "cv-pick-color")
      ),
      ## Projections are a multi-select. Every selected embedding receives its
      ## own linked card in the same responsive grid as Spatial, Trekker and TCR.
      ## All coordinates already travel in the bundle, so this stays client-side.
      div(
        class = "cv-ctl cv-multiselect-ctl",
        id = "cv-proj-ctl",
        tags$label("Projection"),
        tags$select(id = "cv-pick-proj", multiple = "multiple")
      ),
      ## Spatial sections are a multi-select. Every selected section receives an
      ## independent linked canvas; all sections still share colour, filtering,
      ## brushing and hover state.
      div(
        class = "cv-ctl cv-multiselect-ctl",
        id = "cv-spatial-ctl",
        style = "display:none",
        tags$label("Spatial data"),
        tags$select(id = "cv-pick-spatial", multiple = "multiple")
      ),
      ## Mean/per-gene expression picker. Per-gene mode expands the linked
      ## spaces into a gene x space grid; the client enforces a 12-panel cap.
      div(
        class = "cv-ctl",
        id = "cv-gene-ctl",
        style = "display:none",
        tags$label("Gene(s)"),
        selectizeInput(
          "coordviews_gene",
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          options = list(
            maxOptions = 1000,
            placeholder = "select genes...",
            create = FALSE,
            plugins = list("remove_button"),
            loadThrottle = 300
          )
        )
      ),
      div(
        class = "cv-ctl",
        id = "cv-gene-panel-color-ctl",
        style = "display:none",
        tags$label("Panel colours"),
        tags$select(
          id = "cv-gene-panel-color",
          tags$option(value = "shared", "Shared scale"),
          tags$option(value = "distinct", "Distinct colours")
        )
      ),
      ## Three-gene co-expression: one gene per RGB channel, shown only in
      ## "Co-expression (RGB)" mode.
      div(
        class = "cv-ctl cv-rgb-ctl",
        id = "cv-rgb-ctl",
        style = "display:none",
        tags$label("Co-expression (R / G / B)"),
        div(
          class = "cv-rgb-row",
          selectizeInput(
            "coordviews_gene_r",
            label = NULL,
            choices = NULL,
            options = list(
              maxOptions = 1000,
              placeholder = "red gene...",
              create = FALSE,
              loadThrottle = 300
            )
          ),
          selectizeInput(
            "coordviews_gene_g",
            label = NULL,
            choices = NULL,
            options = list(
              maxOptions = 1000,
              placeholder = "green gene...",
              create = FALSE,
              loadThrottle = 300
            )
          ),
          selectizeInput(
            "coordviews_gene_b",
            label = NULL,
            choices = NULL,
            options = list(
              maxOptions = 1000,
              placeholder = "blue gene...",
              create = FALSE,
              loadThrottle = 300
            )
          )
        )
      ),
      ## Clonal-panel layout switch. Shown only when the data set carries an
      ## immune axis (a "clone" space). The clone panel's X/Y are abstract, so
      ## unlike UMAP/Spatial it draws labelled axes and can be re-laid-out
      ## client-side between representations without touching the server.
      div(
        ## cv-collapse (opacity 0) as the initial hidden state so the FIRST reveal
        ## also fades in, matching subsequent switches (revealEl toggles it).
        class = "cv-ctl cv-collapse",
        id = "cv-clone-layout-ctl",
        style = "display:none",
        tags$label("Clonal layout"),
        div(
          class = "cv-seg",
          id = "cv-clone-layout",
          tags$button(
            type = "button",
            class = "cv-seg-btn is-on",
            `data-mode` = "stack",
            tags$span("Rank", tags$br(), "stack")
          ),
          tags$button(
            type = "button",
            class = "cv-seg-btn",
            `data-mode` = "bands",
            tags$span("Expansion", tags$br(), "bands")
          )
        )
      ),
      ## Reveals the shared advanced-settings drawer.
      cerebroSettingsButton(
        "cv-more-btn",
        "cv-more"
      ),
      ## Right-aligned global filter/subsample readout. Cohort actions belong to
      ## the Active cohort bar below, not to this settings row.
      div(
        class = "cv-topbar-right",
        ## Live "showing N / M cells" readout — hidden unless a filter/subsample
        ## reduces the view, so filtering is visible even when the panels are
        ## coloured by a different variable than the one being filtered.
        tags$span(class = "cv-shown", id = "cv-shown")
      ),

      ## ---- shared advanced-settings drawer ----------------------------- ##
      ## The app-wide drawer owns viewport placement, scrolling, spacing and
      ## responsive layout. Linked views supplies only its page-specific controls.
      cerebroSettingsDrawer(
        "cv-more",
        div(
          class = "coordviews-page",
          cerebroSettingsSection(
            "Appearance",
            tagList(
              sliderInput(
                "cv-ps",
                label = "Point size",
                min = 1,
                max = 20,
                step = 1,
                value = 5
              ),
              sliderInput(
                "cv-opacity",
                label = "Point opacity",
                min = 0.1,
                max = 1,
                step = 0.1,
                value = 1
              ),
              checkboxInput(
                "cv-labels",
                "Group labels",
                value = TRUE
              ),
              checkboxInput(
                "cv-borders",
                "Draw border around cells",
                value = FALSE
              ),
              checkboxInput(
                "cv-square-plots",
                "Keep plots square",
                value = FALSE
              )
            )
          ),
          cerebroSettingsSection(
            "Data",
            tagList(
              sliderInput(
                "cv-pct",
                label = "Show % of cells",
                min = 10,
                max = 100,
                step = 10,
                value = 100
              ),
              shiny::tagAppendAttributes(
                selectInput(
                  "cv-clip",
                  label = "Colour range",
                  choices = c(
                    "Full range" = "0",
                    "1-99%" = "0.01",
                    "2-98%" = "0.02",
                    "5-95%" = "0.05"
                  ),
                  selected = "0.01",
                  selectize = FALSE
                ),
                id = "cv-clip-ctl",
                style = "display:none"
              )
            )
          ),
          shiny::tagAppendAttributes(
            cerebroSettingsSection(
              "Background image",
              div(
                class = "cv-ctl cv-bg-ctl cerebro-settings-full",
                id = "cv-img-pick-ctl",
                style = "display:none",
                div(class = "cv-bg-space-tabs", id = "cv-bg-space-tabs"),
                div(
                  class = "cv-bg-settings",
                  div(
                    class = "cv-bg-display",
                    tags$div(class = "cv-bg-display-title", "Display"),
                    tags$select(
                      id = "cv-bg-image-select",
                      class = "form-control"
                    )
                  ),
                  uiOutput("coordviews_image_ui")
                )
              )
            ),
            style = "display:none"
          ),
          shiny::tagAppendAttributes(
            cerebroSettingsSection(
              "Spatial mapping",
              div(
                class = "cv-trekker cerebro-settings-contents",
                id = "cv-trekker-ctl",
                style = "display:none",
                sliderInput(
                  "cv-dissolve",
                  label = "Dissolve least-confident (%)",
                  min = 0,
                  max = 95,
                  step = 5,
                  value = 0
                ),
                shiny::tagAppendAttributes(
                  sliderInput(
                    "cv-niche",
                    label = "Niche radius (µm)",
                    min = 50,
                    max = 500,
                    step = 25,
                    value = 250
                  ),
                  id = "cv-niche-wrap"
                ),
                checkboxInput(
                  "cv-evidence",
                  label = "Mark positioning evidence",
                  value = FALSE
                )
              )
            ),
            style = "display:none"
          ),
          cerebroSettingsSection(
            "Group filters",
            div(
              class = "cv-ctl cv-filters cerebro-group-filters",
              div(class = "cv-filters-row", id = "cv-filters-row")
            )
          )
        )
      )
    ),

    ## ---- linked-workspace guide / active cohort -------------------------- ##
    ## The quiet opening guide makes the two defining interactions discoverable.
    ## The guide stays visible; the active cohort appears beneath it.
    div(
      class = "cv-status-slot",
      div(
        class = "cv-workspace-guide cv-collapse",
        id = "cv-workspace-guide",
        tags$span(
          class = "cv-workspace-kicker",
          icon("link"),
          "Linked workspace"
        ),
        tags$span(
          class = "cv-workspace-guide-text",
          id = "cv-workspace-guide-text",
          "Drag in any view to create an active cohort. Use Focus to enlarge one lens while keeping the others linked."
        ),
        div(
          class = "cv-workspace-actions",
          tags$button(
            type = "button",
            class = "cv-workspace-overview",
            id = "cv-workspace-overview",
            style = "display:none",
            icon("table-cells-large"),
            "Back to overview"
          ),
          tags$button(
            type = "button",
            id = "cv-config-open",
            class = "cv-config-open",
            `data-view-id` = "linked_views",
            disabled = "disabled",
            `aria-disabled` = "true",
            `aria-haspopup` = "dialog",
            `aria-controls` = "cv-config-dialog",
            title = "The linked workspace is waiting for its plots",
            icon("share-alt"),
            tags$span("Share view")
          )
        )
      ),

      ## Active cohort: the shared state all lenses are describing.
      div(
        class = paste(
          "cv-selbar cv-collapse",
          "cerebro-selection-status-active"
        ),
        id = "cv-selbar",
        div(
          class = "cv-selcopy",
          tags$span(
            class = "cerebro-selection-status-kicker",
            id = "cv-sel-kicker",
            "Active cohort"
          ),
          tags$span(
            class = "cerebro-selection-status-count",
            id = "cv-seltext",
            "—"
          ),
          tags$span(
            class = "cerebro-selection-status-profile",
            id = "cv-selprofile",
            ""
          ),
          tags$span(
            class = "cerebro-selection-status-origin",
            id = "cv-selorigin",
            ""
          ),
          tags$span(
            class = "cerebro-selection-status-origin",
            id = "cv-selcoverage",
            ""
          )
        ),
        ## Actions sit with the cohort they affect, rather than in the unrelated
        ## global-control row. They remain hidden until a selection/niche exists.
        div(
          class = paste(
            "cv-selactions cv-collapse",
            "cerebro-selection-actions"
          ),
          id = "cv-selactions",
          style = "display:none",
          tags$button(
            type = "button",
            class = "cerebro-config-open",
            `data-view-id` = "linked_views",
            disabled = "disabled",
            `aria-disabled` = "true",
            `aria-haspopup` = "dialog",
            `aria-controls` = "cv-config-dialog",
            title = "The linked workspace is waiting for its plots",
            icon("share-alt"),
            tags$span("Share view")
          ),
          div(
            class = "cv-sel-action-row",
            tags$button(
              id = "cv-zoom",
              class = paste(
                "btn btn-xs btn-default",
                "cerebro-selection-action-zoom"
              ),
              "Zoom to selection"
            ),
            tags$button(
              id = "cv-clear",
              class = paste(
                "btn btn-xs btn-default",
                "cerebro-selection-action-clear"
              ),
              "Clear selection"
            )
          )
        )
      )
    ),

    ## ---- legend (categorical) or colourbar (continuous gene) ------------ ##
    div(class = "cv-legend", id = "cv-legend"),
    div(
      class = "cv-cbar",
      id = "cv-cbar",
      style = "display:none",
      tags$span(id = "cv-cb0", "0"),
      div(class = "cv-grad", id = "cv-grad"),
      tags$span(id = "cv-cb1", "1"),
      tags$span(class = "cv-cbar-note", id = "cv-cbar-note", "expression")
    ),
    ## ---- panel grid ----------------------------------------------------- ##
    ## Every selected/present space gets its OWN panel: selected projections,
    ## selected Spatial sections, Trekker and Clonal. cell_views.js
    ## assigns spaces, hides unused slots, creates extras as needed, and wraps
    ## panels automatically while keeping each canvas at least 300px wide.
    div(
      class = "cv-panes",
      cv_pane("A"),
      cv_pane("B"),
      cv_pane("C"),
      cv_pane("D"),
      ## ---- single-cell detail card ------------------------------------- ##
      ## Clicking a cell promotes its hover tooltip into this: the same facts,
      ## uncut, parked in the middle of the panel grid instead of chasing the
      ## pointer — so a barcode can be copied and a full CDR3 read while the
      ## panels stay usable. Absolutely positioned, so it never enters the grid.
      ## The outer element owns the centring, the inner one owns the fly-in
      ## transform; keeping those on separate elements means neither has to
      ## reconstruct the other's translate.
      div(
        class = "cv-card-pos",
        id = "cv-card-pos",
        div(
          class = "cv-card",
          id = "cv-card",
          role = "dialog",
          `aria-label` = "Cell details",
          div(
            class = "cv-card-head",
            div(
              tags$div(class = "cv-card-title", id = "cv-card-title", "—"),
              tags$div(class = "cv-card-bc", id = "cv-card-bc", "")
            ),
            tags$button(
              type = "button",
              class = "cv-card-x",
              id = "cv-card-x",
              `aria-label` = "Close",
              HTML("&times;")
            )
          ),
          div(class = "cv-card-body", id = "cv-card-body")
        )
      )
    ),

    ## Selection results appear only after there is something to show, leaving
    ## the initial workspace entirely available to the visualisations.
    div(class = "cv-readout", id = "cv-readout", style = "display:none"),

    ## ---- Trekker insights ------------------------------------------------- ##
    ## One discoverable, default-collapsed analysis region replaces the old
    ## page's three vertically stacked boxes. It is client-driven: the selected
    ## cell, QC and upstream Moran values are already in the Linked views bundle.
    div(
      class = "cv-tk-insights",
      id = "cv-tk-insights",
      style = "display:none",
      tags$button(
        type = "button",
        class = "cv-tk-insights-toggle",
        id = "cv-tk-insights-toggle",
        `aria-expanded` = "false",
        tags$span(
          tags$span(class = "cv-tk-insights-kicker", "Trekker"),
          tags$strong("Trekker insights"),
          tags$small(
            "Cell inspector, positioning quality and spatial autocorrelation"
          )
        ),
        icon("chevron-down")
      ),
      div(
        class = "cv-tk-insights-body trekker-page",
        id = "cv-tk-insights-body",
        style = "display:none",
        div(
          class = "cv-tk-tabs",
          role = "tablist",
          `aria-label` = "Trekker insights",
          tags$button(
            type = "button",
            class = "cv-tk-tab is-active",
            id = "cv-tk-tab-cell",
            `data-tk-tab` = "cell",
            role = "tab",
            `aria-selected` = "true",
            "Cell inspector"
          ),
          tags$button(
            type = "button",
            class = "cv-tk-tab",
            id = "cv-tk-tab-qc",
            `data-tk-tab` = "qc",
            role = "tab",
            `aria-selected` = "false",
            "Data and QC"
          ),
          tags$button(
            type = "button",
            class = "cv-tk-tab",
            id = "cv-tk-tab-moran",
            `data-tk-tab` = "moran",
            role = "tab",
            `aria-selected` = "false",
            "Spatial autocorrelation — Moran's I"
          )
        ),
        div(
          class = "cv-tk-panel-stage",
          id = "cv-tk-panel-stage",
          div(
            class = "cv-tk-panel is-active",
            id = "cv-tk-panel-cell",
            role = "tabpanel",
            `aria-labelledby` = "cv-tk-tab-cell",
            div(
              class = "cv-tk-cell-empty",
              id = "cv-tk-cell-empty",
              "Click a nucleus in any linked cell view to inspect its identity, ",
              "physical neighbourhood and positioning evidence."
            ),
            div(
              class = "cv-tk-cell-content",
              id = "cv-tk-cell-content",
              style = "display:none",
              tags$h4(id = "cv-tk-cell-title", "—"),
              tags$div(class = "cv-tk-cell-bc", id = "cv-tk-cell-bc"),
              div(class = "cv-card-body", id = "cv-tk-cell-body")
            )
          ),
          div(
            class = "cv-tk-panel",
            id = "cv-tk-panel-qc",
            role = "tabpanel",
            `aria-labelledby` = "cv-tk-tab-qc",
            style = "display:none",
            div(class = "tk-grid", id = "cv-tk-stats"),
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
                  tags$tbody(id = "cv-tk-postbl")
                ),
                div(class = "tk-flag", id = "cv-tk-salvflag")
              ),
              div(
                tags$h4(class = "tk-sub-h", "Provenance"),
                tags$dl(class = "tk-kv", id = "cv-tk-prov"),
                div(class = "tk-flag", id = "cv-tk-rangeflag")
              )
            )
          ),
          div(
            class = "cv-tk-panel",
            id = "cv-tk-panel-moran",
            role = "tabpanel",
            `aria-labelledby` = "cv-tk-tab-moran",
            style = "display:none",
            tags$table(
              class = "tk-table",
              tags$thead(tags$tr(
                tags$th(class = "num", "#"),
                tags$th("Gene"),
                tags$th(class = "num", "Moran's I")
              )),
              tags$tbody(id = "cv-tk-morantbl")
            )
          )
        )
      )
    ),

    ## Detailed selected-cell plot and table are rendered only while a selection
    ## exists, so the landing surface stays compact without dropping the result.
    div(
      class = "cv-secondary-analysis",
      shiny::uiOutput("coordviews_selected_cells_UI")
    ),

    ## A portable Linked views configuration contains the cohort's cell
    ## barcodes and this workspace's controls, but never the source data, image
    ## pixels, expression values, file paths, or Builder project identity.
    tags$dialog(
      id = "cv-config-dialog",
      class = "cv-config-dialog",
      `aria-labelledby` = "cv-config-title",
      `aria-describedby` = "cv-config-privacy",
      tags$button(
        type = "button",
        id = "cv-config-close",
        class = "cv-config-close",
        `aria-label` = "Close workspace JSON",
        HTML("&times;")
      ),
      tags$div(class = "cv-config-kicker", "View configuration"),
      tags$h4(id = "cv-config-title", "Share view"),
      tags$p(
        id = "cv-config-privacy",
        class = "cv-config-privacy",
        "Download this display state and its selected cell barcodes as JSON, ",
        "then open it in another compatible CerebroNexus session. Source data stays ",
        "on this device."
      ),
      tags$section(
        class = "cv-config-region cv-config-image",
        `aria-labelledby` = "cv-config-image-title",
        tags$div(
          class = "cv-config-region-head cv-config-region-head-actions",
          tags$div(
            tags$h5(id = "cv-config-image-title", "Image"),
            tags$p(
              id = "cv-config-png-help",
              "Download the current view as an image."
            )
          ),
          tags$button(
            type = "button",
            id = "cv-config-png",
            class = "cv-config-action",
            icon("image"),
            tags$span("Download PNG")
          )
        )
      ),
      tags$section(
        class = "cv-config-region cv-config-transfer",
        `aria-labelledby` = "cv-config-transfer-title",
        tags$div(
          class = "cv-config-region-head cv-config-region-head-actions",
          tags$div(
            tags$h5(id = "cv-config-transfer-title", "View JSON"),
            tags$p("Download or open a validated view configuration.")
          ),
          div(
            class = "cv-config-actions",
            tags$button(
              type = "button",
              id = "cv-config-download",
              class = "cv-config-action",
              icon("download"),
              tags$span("Download JSON")
            ),
            div(
              class = "cv-config-upload",
              tags$button(
                type = "button",
                id = "cv-config-upload-button",
                class = "btn-file",
                icon("folder-open"),
                tags$span("Open JSON")
              ),
              tags$input(
                id = "coordviews_config_upload",
                class = "cv-config-file-input",
                type = "file",
                accept = "application/json,.json"
              )
            )
          )
        )
      ),
      tags$p(
        id = "cv-config-status",
        class = "cv-config-status",
        role = "status",
        `aria-live` = "polite"
      )
    ),
    tags$dialog(
      id = "cv-moran-modal",
      class = "cv-insight-modal",
      tags$button(
        class = "cv-insight-x",
        onclick = "document.getElementById('cv-moran-modal').close()",
        `aria-label` = "Close",
        HTML("&times;")
      ),
      tags$div(class = "cv-insight-kicker", "Spatial pattern"),
      tags$h4(id = "cv-moran-modal-title"),
      tags$div(class = "cv-moran-modal-value", id = "cv-moran-modal-value"),
      tags$p(
        "Moran's I measures whether nearby cells carry similar values. ",
        "Positive values indicate spatial clustering, values near zero indicate ",
        "little spatial structure, and negative values indicate neighbouring ",
        "cells tend to differ. The estimate uses each cell's six nearest spatial ",
        "neighbours and a stable sample of at most 1,000 positioned cells."
      )
    ),

    tags$dialog(
      id = "cv-evidence-modal",
      class = "cv-evidence-modal",
      tags$button(
        class = "cv-insight-x",
        onclick = "document.getElementById('cv-evidence-modal').close()",
        `aria-label` = "Close",
        HTML("&times;")
      ),
      tags$div(class = "cv-insight-kicker", "Positioning evidence"),
      tags$div(class = "cv-evidence-modal-cell", id = "cv-evidence-modal-cell"),
      tags$img(id = "cv-evidence-modal-img", alt = "Positioning evidence")
    )
  )
)
