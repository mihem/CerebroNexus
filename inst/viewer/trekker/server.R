##----------------------------------------------------------------------------##
## Tab: Trekker — server.
##
## Sourced into the main server scope (try_source with local = parent.frame()),
## so `input`, `output`, `session` and `data_set` are in scope.
##
## R owns Trekker-specific controls and payload preparation. cell_views.js owns
## every canvas, viewport, toolbar and linked-selection interaction.
##----------------------------------------------------------------------------##

## Pure helpers (trekker_gene_suggest, trekker_numeric_meta_cols) live in a
## side-effect-free file so they can be unit-tested; source it here so the
## bare-name calls below resolve at runtime (packaged or via runApp('inst')).
source(cerebro_root_path("viewer/trekker/helpers.R"), local = TRUE)

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
output[["trekker_main_parameters_ui"]] <- renderUI({
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
      class = "trekker-transition-control",
      sliderInput(
        "trekker_morph",
        label = "Transition (UMAP → Spatial)",
        min = 0,
        max = 1,
        value = 0,
        step = 0.01,
        ticks = FALSE
      )
    )
  )
})

## Build the dedicated page as a two-panel specialist payload. Rendering,
## viewport state, selection and toolbars all stay in cell_views.js.
observe({
  req(identical(input[["sidebar"]], "trekker"))
  render_request <- input[["trekker_projection_render_request"]]
  req(is.list(render_request), is.list(render_request$dataset_context))
  dataset_context <- viewer_loaded_dataset_context()
  req(viewerDatasetContextEqual(
    render_request$dataset_context,
    dataset_context
  ))
  tk <- req(trekker_slot())
  mode <- input[["trekker_mode"]] %||% "celltype"
  view <- input[["trekker_view"]] %||% "pair"
  keys <- as.character(tk$barcodes)
  cluster <- as.character(tk$clusters)
  cluster_index <- suppressWarnings(as.integer(cluster)) + 1L
  cell_type <- if (
    length(tk$celltype) &&
      length(cluster_index) == length(keys) &&
      all(!is.na(cluster_index)) &&
      all(cluster_index >= 1L & cluster_index <= length(tk$celltype))
  ) {
    as.character(tk$celltype[cluster_index])
  } else {
    as.character(tk$celltype)[seq_along(keys)]
  }
  categorical <- mode %in% c("celltype", "cluster")
  group <- if (identical(mode, "cluster")) cluster else cell_type
  color <- NULL
  color_label <- mode
  if (identical(mode, "gene")) {
    gene <- input[["trekker_gene_pick"]]
    if (!is.null(gene) && gene %in% trekker_gene_names()) {
      color <- as.numeric(tryCatch(
        viewerExpressionRow(data_set(), keys, gene),
        error = function(e) rep(NA_real_, length(keys))
      ))
      color_label <- gene
    }
  } else if (identical(mode, "meta")) {
    md <- tryCatch(data_set()$getMetaData(), error = function(e) NULL)
    column <- input[["trekker_meta_pick"]]
    md_keys <- if (!is.null(md) && "cell_barcode" %in% names(md)) {
      md[["cell_barcode"]]
    } else if (!is.null(md)) {
      rownames(md)
    } else {
      character()
    }
    color <- if (!is.null(md) && !is.null(column) && column %in% names(md)) {
      suppressWarnings(as.numeric(md[[column]][match(keys, md_keys)]))
    } else {
      rep(NA_real_, length(keys))
    }
    color_label <- column
  } else if (!categorical && !is.null(tk$fields[[mode]]$v)) {
    color <- as.numeric(tk$fields[[mode]]$v)
    color_label <- tk$fields[[mode]]$label %||% mode
  }
  if (!categorical && is.null(color)) {
    color <- rep(0, length(keys))
  }
  alignment <- tk$histology_alignment %||% list()
  background <- viewerTrekkerBackground(tk)
  image_preset <- if (is.list(background$external$preset)) {
    background$external$preset
  } else {
    list(
      # Match Spatial and Linked views: a saved opacity applies, else 1.
      opacity = {
        saved <- suppressWarnings(as.numeric(alignment$image_opacity))
        if (
          length(saved) == 1L && is.finite(saved) && saved >= 0 && saved <= 1
        ) {
          saved
        } else {
          0.5
        }
      },
      offsetX = alignment$offset_x %||% 0,
      offsetY = alignment$offset_y %||% 0,
      scaleX = alignment$scale_x %||% 1,
      scaleY = alignment$scale_y %||% 1,
      flipX = isTRUE(alignment$flip_x),
      flipY = isTRUE(alignment$flip_y),
      rotation = alignment$rotation %||% 0
    )
  }
  spatial_panel <- list(
    id = "trekker",
    label = if (identical(view, "morph")) "UMAP → Spatial" else "Spatial",
    selection_key = keys,
    x = as.numeric(tk$x),
    y = as.numeric(tk$y),
    spatial = TRUE,
    background_image = background$uri,
    image_bounds = as.list(background$bounds),
    image_preset = image_preset
  )
  if (identical(view, "morph")) {
    spatial_panel$from_x <- as.numeric(tk$ux)
    spatial_panel$from_y <- as.numeric(tk$uy)
    spatial_panel$to_x <- as.numeric(tk$x)
    spatial_panel$to_y <- as.numeric(tk$y)
    spatial_panel$transition <- input[["trekker_morph"]] %||% 0
  }
  umap_panel <- list(
    id = "umap",
    label = "UMAP",
    selection_key = keys,
    x = as.numeric(tk$ux),
    y = as.numeric(tk$uy),
    projection = TRUE
  )
  panels <- switch(
    view,
    sp = list(spatial_panel),
    um = list(umap_panel),
    morph = list(spatial_panel),
    list(spatial_panel, umap_panel)
  )
  levels <- if (categorical) sort(unique(group)) else character()
  appearance <- current_scatter_defaults()
  cerebroCellViewRender(
    "trekker_projection",
    meta = list(
      color_type = if (categorical) "categorical" else "continuous",
      color_variable = color_label,
      traces = levels,
      group_colors = cerebro_group_colors(length(levels)),
      shared_filters = categorical
    ),
    data = list(
      selection_key = keys,
      panels = panels,
      group = group,
      color = color,
      point_size = appearance$point_size,
      point_opacity = appearance$point_opacity,
      percentage_cells_to_show = appearance$percentage_cells_to_show
    ),
    dataset_context = dataset_context
  )
})

output[["trekker_number_of_selected_cells"]] <- renderUI({
  selected <- input[["trekker_projection_persistent_selection"]] %||%
    character()
  if (
    length(selected) &&
      (!is.list(selected) ||
        !viewerDatasetContextEqual(
          selected[["dataset_context"]],
          viewer_loaded_dataset_context()
        ))
  ) {
    selected <- character()
  }
  tags$span(
    format(cerebroSelectionCount(selected), big.mark = ","),
    " cells selected"
  )
})

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
