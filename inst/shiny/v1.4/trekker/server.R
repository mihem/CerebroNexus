##----------------------------------------------------------------------------##
## Tab: Trekker — server.
##
## Sourced into the main server scope (try_source with local = parent.frame()),
## so `input`, `output`, `session` and `data_set` are in scope.
##
## Responsibilities:
##   1. render the parameter controls (standard app widgets), so the page uses
##      the same components/theme as every other tab;
##   2. push the loaded .crb's `trekker` slot to the client (www/trekker.js);
##   3. answer whole-transcriptome gene-colouring requests, returning a 0-255
##      vector aligned to the page's nuclei (via the slot's `barcodes` order).
##
## The controls are Shiny inputs; www/trekker.js listens to `shiny:inputchanged`
## and updates the canvases client-side (instant), so dragging a slider does not
## round-trip to the server. Only gene expression needs the server (it holds the
## matrix), so that one path is a request/response.
##----------------------------------------------------------------------------##

## Pure helpers (trekker_gene_suggest, trekker_numeric_meta_cols) live in a
## side-effect-free file so they can be unit-tested; source it here so the
## bare-name calls below resolve at runtime (packaged or via runApp('inst')).
source(
  paste0(Cerebro.options[["cerebro_root"]], "/shiny/v1.4/trekker/helpers.R"),
  local = TRUE
)

trekker_slot <- reactive({
  req(data_set())
  tryCatch(data_set()$getTrekker(), error = function(e) NULL)
})

trekker_gene_names <- reactive({
  tryCatch(rownames(data_set()$expression), error = function(e) character(0))
})

## Numeric, non-constant per-cell meta columns (differentiator 3): any existing
## per-cell analysis output the object carries -- pseudotime, a signature/module
## score (this demo ships a myelination score), velocity magnitude, a signaling
## score -- can colour the physical map with no page change.
trekker_meta_cols <- reactive({
  md <- tryCatch(data_set()$getMetaData(), error = function(e) NULL)
  trekker_numeric_meta_cols(md)
})

##----------------------------------------------------------------------------##
## Parameter controls (standard widgets, rendered only when Trekker data exists)
##----------------------------------------------------------------------------##
output[["trekker_parameters_ui"]] <- renderUI({
  tk <- req(trekker_slot())
  suggest <- trekker_gene_suggest(tk, trekker_gene_names())
  default_gene <- if (length(suggest)) suggest[1] else trekker_gene_names()[1]
  ## Data-driven "Colour by": every continuous field carried in the slot (the
  ## cross-space metrics now; per-cell analysis outputs later) becomes a physical-
  ## map colouring with no UI change. This is the substrate the differentiators
  ## plug into.
  field_choices <- if (length(tk$fields)) {
    stats::setNames(
      names(tk$fields),
      vapply(tk$fields, function(f) f$label, character(1))
    )
  } else {
    character(0)
  }
  meta_cols <- trekker_meta_cols()
  meta_default <- if ("Myelination" %in% meta_cols) {
    "Myelination"
  } else if (length(meta_cols)) {
    meta_cols[1]
  } else {
    NULL
  }
  meta_choice <- if (length(meta_cols)) {
    c("Meta / analysis field" = "meta")
  } else {
    character(0)
  }
  tagList(
    selectInput(
      "trekker_view",
      label = "View",
      choices = c(
        "Side by side" = "pair",
        "Spatial only" = "sp",
        "UMAP only" = "um",
        "Transition" = "morph"
      )
    ),
    selectInput(
      "trekker_mode",
      label = "Colour by",
      choices = c(
        "Cell type" = "celltype",
        "Cluster" = "cluster",
        "Gene expression" = "gene",
        meta_choice,
        field_choices
      )
    ),
    conditionalPanel(
      condition = "input.trekker_mode == 'meta'",
      selectInput(
        "trekker_meta_pick",
        label = "Meta / analysis field",
        choices = meta_cols,
        selected = meta_default
      )
    ),
    conditionalPanel(
      condition = "input.trekker_mode == 'gene'",
      ## Suggested genes (upstream Moran + canonical markers) are offered as
      ## options; `create = TRUE` lets the user type ANY of the ~21k measured
      ## genes (the server validates it), which keeps this lightweight instead of
      ## rendering 21k <option> nodes -- same pattern as the Gene expression tab.
      selectizeInput(
        "trekker_gene_pick",
        label = "Gene",
        choices = suggest,
        selected = default_gene,
        multiple = FALSE,
        options = list(create = TRUE, placeholder = "search or type a gene...")
      )
    ),
    conditionalPanel(
      condition = "input.trekker_view == 'morph'",
      sliderInput(
        "trekker_morph",
        label = "Transition (UMAP → Spatial)",
        min = 0,
        max = 1,
        value = 0,
        step = 0.01
      )
    ),
    sliderInput(
      "trekker_ps",
      label = "Point size",
      min = 0.6,
      max = 6,
      value = 2.2,
      step = 0.2
    ),
    sliderInput(
      "trekker_nr",
      label = "Niche radius (um)",
      min = 50,
      max = 500,
      value = 250,
      step = 25
    ),
    ## Differentiator 1: position uncertainty as an interactive axis. Dissolves
    ## the least-confidently-positioned nuclei (by the vendor's adopted-cluster
    ## UMI share) so the tissue sharpens to only its well-supported positions.
    sliderInput(
      "trekker_conf",
      label = "Dissolve least-confident positions (%)",
      min = 0,
      max = 95,
      value = 0,
      step = 5
    ),
    shinyWidgets::materialSwitch(
      "trekker_evtoggle",
      label = "Mark nuclei with positioning evidence",
      value = TRUE,
      status = "primary",
      right = TRUE
    )
  )
})

## Group filters (same widget family as the projection tabs): one pickerInput per
## categorical grouping variable, all levels selected by default. www/trekker.js
## reads the selections and hides nuclei outside them, so you can isolate a cell
## type or cluster without having to colour by it.
output[["trekker_group_filters_ui"]] <- renderUI({
  tk <- req(trekker_slot())
  celltypes <- sort(unique(as.character(tk$celltype)))
  clusters <- sort(unique(tk$clusters))
  tagList(
    shinyWidgets::pickerInput(
      "trekker_group_filter_celltype",
      label = "Cell type",
      choices = celltypes,
      selected = celltypes,
      options = list("actions-box" = TRUE),
      multiple = TRUE
    ),
    shinyWidgets::pickerInput(
      "trekker_group_filter_cluster",
      label = "Cluster",
      choices = clusters,
      selected = clusters,
      options = list("actions-box" = TRUE),
      multiple = TRUE
    )
  )
})

## Render the controls eagerly (not only when the tab is shown) so they exist and
## stay in sync with the canvas as soon as a Trekker data set is loaded.
outputOptions(output, "trekker_parameters_ui", suspendWhenHidden = FALSE)
outputOptions(output, "trekker_group_filters_ui", suspendWhenHidden = FALSE)

##----------------------------------------------------------------------------##
## Push the slot to the client on (re)connect or data-set change
##----------------------------------------------------------------------------##
observe({
  input[["trekker_ready"]]
  tk <- trekker_slot()
  if (is.null(tk)) {
    return()
  }
  tk$gene_suggest <- trekker_gene_suggest(tk, trekker_gene_names())
  session$sendCustomMessage("trekker_data", tk)
})

##----------------------------------------------------------------------------##
## Gene colouring (whole transcriptome): send a 0-255 vector aligned to the
## page's nuclei whenever gene mode is active and a gene is selected.
##----------------------------------------------------------------------------##
observeEvent(
  list(input[["trekker_mode"]], input[["trekker_gene_pick"]]),
  {
    tk <- trekker_slot()
    if (is.null(tk) || is.null(input[["trekker_mode"]])) {
      return()
    }
    if (input[["trekker_mode"]] != "gene") {
      return()
    }
    g <- input[["trekker_gene_pick"]]
    if (is.null(g) || !nzchar(g)) {
      return()
    }
    if (!(g %in% trekker_gene_names())) {
      session$sendCustomMessage("trekker_geneval", list(gene = g, ok = FALSE))
      return()
    }
    mat <- tryCatch(
      data_set()$getExpressionMatrix(cells = tk$barcodes, genes = g),
      error = function(e) NULL
    )
    if (is.null(mat)) {
      session$sendCustomMessage("trekker_geneval", list(gene = g, ok = FALSE))
      return()
    }
    v <- as.numeric(mat)
    mx <- suppressWarnings(max(v, na.rm = TRUE))
    q <- if (is.finite(mx) && mx > 0) {
      as.integer(round(v / mx * 255))
    } else {
      rep(0L, length(v))
    }
    session$sendCustomMessage(
      "trekker_geneval",
      list(gene = g, ok = TRUE, v = q, max = round(mx, 3))
    )
  },
  ignoreInit = TRUE
)

## Meta / analysis-field colouring (differentiator 3): send the picked per-cell
## meta column, aligned to the page's nuclei, min-max scaled to 0-255. Same
## request/response shape as gene colouring; the client renders it identically.
observeEvent(
  list(input[["trekker_mode"]], input[["trekker_meta_pick"]]),
  {
    tk <- trekker_slot()
    if (is.null(tk) || is.null(input[["trekker_mode"]])) {
      return()
    }
    if (input[["trekker_mode"]] != "meta") {
      return()
    }
    col <- input[["trekker_meta_pick"]]
    if (is.null(col) || !nzchar(col)) {
      return()
    }
    md <- tryCatch(data_set()$getMetaData(), error = function(e) NULL)
    if (is.null(md) || !(col %in% names(md))) {
      session$sendCustomMessage("trekker_served", list(ok = FALSE))
      return()
    }
    key <- if ("cell_barcode" %in% names(md)) {
      md[["cell_barcode"]]
    } else {
      rownames(md)
    }
    raw <- suppressWarnings(as.numeric(md[[col]][match(tk$barcodes, key)]))
    if (all(is.na(raw))) {
      session$sendCustomMessage("trekker_served", list(ok = FALSE))
      return()
    }
    mn <- min(raw, na.rm = TRUE)
    mx <- max(raw, na.rm = TRUE)
    rng <- if (mx > mn) mx - mn else 1
    q <- as.integer(round((raw - mn) / rng * 255))
    q[is.na(q)] <- 0L
    session$sendCustomMessage(
      "trekker_served",
      list(
        ok = TRUE,
        v = q,
        min = round(mn, 3),
        max = round(mx, 3),
        label = col,
        desc = paste0(
          "A per-cell value carried by the loaded object, projected onto ",
          "physical coordinates (an existing single-cell analysis, spatialized)."
        )
      )
    )
  },
  ignoreInit = TRUE
)

## Moran's I "Show in plot" link: switch to gene mode and select the gene. The
## widget updates drive the observers above, which send the expression vector.
observeEvent(input[["trekker_moran_gene"]], {
  g <- input[["trekker_moran_gene"]]
  req(g, nzchar(g))
  updateSelectInput(session, "trekker_mode", selected = "gene")
  updateSelectizeInput(session, "trekker_gene_pick", selected = g)
})

##----------------------------------------------------------------------------##
## Info modals (same pattern as the other tabs)
##----------------------------------------------------------------------------##
observeEvent(input[["trekker_parameters_info"]], {
  showModal(modalDialog(
    title = "Parameters",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    HTML(
      "<ul>
        <li><b>View:</b> <i>Side by side</i> shows the spatial and UMAP scatter
          together; <i>Spatial only</i> / <i>UMAP only</i> show a single enlarged
          pane; <i>Transition</i> animates each nucleus along the slider from its
          UMAP position to its physical position (it is a visual blend between the
          two spaces, not a morphology view).</li>
        <li><b>Colour by:</b> cell type, cluster, or a single gene's expression
          (any of the whole-transcriptome genes).</li>
        <li><b>Point size / Niche radius:</b> display size, and the radius (in um)
          used for the physical-neighbour counts in the Cell inspector.</li>
        <li><b>Mark nuclei with positioning evidence:</b> ring the 50 nuclei that
          carry an official positioning-evidence image.</li>
      </ul>"
    )
  ))
})

observeEvent(input[["trekker_group_filters_info"]], {
  showModal(modalDialog(
    title = "Group filters",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    HTML(
      "Choose which cells are shown, by the group(s) they belong to. For each
      grouping variable (cell type, cluster) you can activate or deactivate
      levels; only nuclei that pass every filter are drawn in both panes and
      counted in the selection. This isolates a population without having to
      colour by it. The Cell inspector's physical-neighbour counts deliberately
      use the whole tissue — a nucleus's real neighbours don't change when you
      hide some from view. Use the box's <i>Select all / Deselect all</i> actions
      to toggle a whole variable at once."
    )
  ))
})

observeEvent(input[["trekker_qc_info"]], {
  showModal(modalDialog(
    title = "Data and QC",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    HTML(
      "Positioning QC in the vendor's own field names; a missing metric stays
      <i>missing</i>, never replaced by 0. <b>Confidently positioned</b> is not the
      same as \"exactly one location\": a fraction are vendor-salvaged
      multi-location nuclei, so the set is labelled
      <code>vendor_confidently_positioned</code>. The app reports when a value is
      below the vendor's reference range but does not adjudicate sample
      usability."
    )
  ))
})

observeEvent(input[["trekker_moran_info"]], {
  showModal(modalDialog(
    title = "Moran's I (upstream)",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    HTML(
      "Spatial autocorrelation from the upstream Trekker pipeline
      (<code>..._variable_features_spatial_moransi.txt</code>). It is computed
      differently from CerebroNexus's own Moran's I (Euclidean 6-NN), so the two are
      labelled separately and are not interchangeable."
    )
  ))
})

## Inline SVG schematic for the panel guide (same self-contained, annotated
## style as the HLA & TCR Motifs guide): the same nuclei in two coordinate
## systems, and what a linked selection shows.
trekker_viz_figure <- function() {
  pal <- c("#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A")
  dot <- function(x, y, ci, ring = FALSE) {
    paste0(
      if (ring) {
        paste0(
          "<circle cx='",
          x,
          "' cy='",
          y,
          "' r='7.5' fill='none' stroke='#111' stroke-width='1.6'/>"
        )
      } else {
        ""
      },
      "<circle cx='",
      x,
      "' cy='",
      y,
      "' r='4.2' fill='",
      pal[ci + 1],
      "'/>"
    )
  }
  left <- paste0(
    dot(60, 88, 0),
    dot(86, 72, 0),
    dot(152, 86, 0),
    dot(140, 60, 0),
    dot(122, 150, 0),
    dot(170, 120, 3),
    dot(78, 142, 2),
    dot(56, 110, 4),
    dot(180, 152, 1),
    dot(112, 98, 1, TRUE),
    dot(132, 116, 1, TRUE),
    dot(106, 128, 2, TRUE)
  )
  right <- paste0(
    dot(290, 78, 0),
    dot(306, 66, 0),
    dot(282, 94, 0),
    dot(316, 88, 0),
    dot(300, 118, 0),
    dot(360, 70, 3),
    dot(346, 126, 4),
    dot(300, 150, 0),
    dot(405, 105, 1, TRUE),
    dot(418, 118, 1, TRUE),
    dot(330, 165, 2, TRUE)
  )
  HTML(paste0(
    "<svg viewBox='0 0 460 210' role='img' ",
    "style='width:100%;max-width:460px;height:auto;display:block;",
    "margin:2px auto 16px' xmlns='http://www.w3.org/2000/svg'>",
    "<style>",
    ".tg-lbl{font:600 12px system-ui,sans-serif;fill:#1c1c1e}",
    ".tg-ann{font:600 11px system-ui,sans-serif;fill:#c2410c}",
    ".tg-panel{fill:#fafafa;stroke:#e0e0e0;stroke-width:1}",
    ".tg-box{fill:rgba(47,111,214,.08);stroke:#2f6fd6;stroke-width:1.3;",
    "stroke-dasharray:4 3}",
    ".tg-arrow{stroke:#c2410c;stroke-width:1.5;fill:none;marker-end:url(#tgar)}",
    "</style>",
    "<defs><marker id='tgar' markerWidth='8' markerHeight='8' refX='6' ",
    "refY='3' orient='auto'><path d='M0,0 L6,3 L0,6 Z' fill='#c2410c'/>",
    "</marker></defs>",
    "<rect class='tg-panel' x='14' y='42' width='196' height='150' rx='8'/>",
    "<rect class='tg-panel' x='250' y='42' width='196' height='150' rx='8'/>",
    "<text class='tg-lbl' x='18' y='36'>Spatial (µm)</text>",
    "<text class='tg-lbl' x='254' y='36'>UMAP</text>",
    left,
    right,
    "<rect class='tg-box' x='92' y='84' width='64' height='60' rx='6'/>",
    "<path class='tg-arrow' d='M160,114 C210,96 214,108 250,108 ",
    "300,108 372,108 398,110'/>",
    "<text class='tg-ann' x='40' y='184'>lasso a spatial region</text>",
    "<text class='tg-ann' x='250' y='184'>",
    "→ the same nuclei (here, two identities)</text>",
    "</svg>"
  ))
}

observeEvent(input[["trekker_viz_info"]], {
  showModal(modalDialog(
    title = "Physical space and transcriptome space",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    tagList(
      trekker_viz_figure(),
      tags$p(
        "Every Trekker nucleus has ",
        tags$b("both"),
        " a physical position (inferred from its bead spatial barcodes) and a ",
        "transcriptomic position (its UMAP embedding). The two scatter plots show ",
        "the same nuclei in these two coordinate systems, side by side."
      ),
      tags$p(
        tags$b("Linked selection"),
        " — box- or lasso-drag in either pane and the same nuclei highlight ",
        "in the other. Select an anatomical region on the left to see which ",
        "transcriptomic identities live there, or a UMAP cluster on the right to ",
        "see where in the tissue it sits."
      ),
      tags$p(
        tags$b("Transition"),
        " — the slider animates each nucleus from its UMAP position to its ",
        "physical position (a visual blend between the two spaces, not a ",
        "morphology view), so you can watch transcriptomic neighbourhoods ",
        "resolve into (or disperse across) physical space."
      ),
      tags$p(
        tags$b("Colour by"),
        " — cell type or cluster; any of the ~21k genes; the cross-space ",
        "metrics (spatial purity, cross-space concordance); any per-cell meta / ",
        "analysis value the object carries (pseudotime, a signature score, ...); ",
        "or the per-nucleus positioning confidence. One control turns any per-cell ",
        "value into a physical-map colouring."
      ),
      tags$p(
        tags$b("Inspect"),
        " — click a single nucleus to open the Cell inspector below."
      ),
      tags$p(
        style = "color:#6b6b70;",
        tags$b("Why this matters. "),
        "Linked spatial + UMAP views are the floor, not the ceiling — a ",
        "general viewer does that. What only Trekker enables is analysis ",
        tags$i("across"),
        " the two spaces: because the same real single cells carry both ",
        "coordinate systems, you can ask whether a cell's transcriptomic ",
        "neighbours are its physical neighbours, and project any single-cell ",
        "result onto the tissue."
      )
    )
  ))
})

## Niche schematic: a picked nucleus, the radius, and the real neighbours in it.
trekker_inspector_figure <- function() {
  pal <- c("#636EFA", "#EF553B", "#00CC96", "#AB63FA")
  dot <- function(x, y, ci, r = 4.2) {
    paste0(
      "<circle cx='",
      x,
      "' cy='",
      y,
      "' r='",
      r,
      "' fill='",
      pal[ci + 1],
      "'/>"
    )
  }
  near <- paste0(
    dot(112, 78, 0),
    dot(182, 92, 1),
    dot(150, 58, 0),
    dot(120, 132, 2),
    dot(190, 128, 1),
    dot(148, 142, 3),
    dot(96, 112, 1),
    dot(178, 60, 0)
  )
  far <- paste0(
    dot(52, 58, 0),
    dot(246, 66, 1),
    dot(250, 152, 2),
    dot(64, 162, 0)
  )
  HTML(paste0(
    "<svg viewBox='0 0 300 200' role='img' ",
    "style='width:100%;max-width:300px;height:auto;display:block;",
    "margin:2px auto 16px' xmlns='http://www.w3.org/2000/svg'>",
    "<style>.tg-ann{font:600 11px system-ui,sans-serif;fill:#c2410c}</style>",
    "<circle cx='150' cy='100' r='70' fill='rgba(249,115,22,.05)' ",
    "stroke='#f97316' stroke-width='1.3' stroke-dasharray='4 3'/>",
    far,
    near,
    "<circle cx='150' cy='100' r='8.5' fill='none' stroke='#f97316' ",
    "stroke-width='2.2'/>",
    dot(150, 100, 0, 5),
    "<text class='tg-ann' x='150' y='190' text-anchor='middle'>",
    "real cell-type counts within the niche radius</text>",
    "</svg>"
  ))
}

observeEvent(input[["trekker_inspector_info"]], {
  showModal(modalDialog(
    title = "Cell inspector",
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    tagList(
      trekker_inspector_figure(),
      tags$p("Click a nucleus in either pane to inspect it."),
      tags$p(
        tags$b("Identity"),
        " — cell type, cluster, physical (x, y) in um, UMAP position, the ",
        "active colour value, and the vendor bead statistics behind the position ",
        "(adopted-cluster UMI share, bead noise, spatial-barcode count)."
      ),
      tags$p(
        tags$b("Physical neighbourhood"),
        " — the ",
        tags$b("real"),
        " cell-type counts within the chosen niche radius: counts of actual ",
        "single nuclei, not a spot-deconvolution estimate. A Visium spot is ",
        "internally mixed, so it cannot give this."
      ),
      tags$p(
        tags$b("Positioning evidence"),
        " — if this nucleus is one of the vendor's evidence set, its ",
        "bead-barcode cloud and UMI knee plot are shown: the auditable answer to ",
        "“why is this nucleus here?”."
      )
    )
  ))
})
