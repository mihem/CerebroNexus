##----------------------------------------------------------------------------##
## HLA & TCR Motifs — descriptive feature x HLA overlap
##
## This tab locks one node or motif from the already-built graph, then reports
## observed donor/sample overlap with one allele. It performs no enrichment test
## and never labels co-occurrence as restriction.
##----------------------------------------------------------------------------##

## A small info marker for a control label: a one-line, plain-language hover hint
## (native title tooltip, no modal). The "info" panel buttons open a dialog; a
## couple of controls just need a short "what is this?" instead.
hla_label_hint <- function(label, hint) {
  tagList(
    label,
    # data-tip (not title): a CSS bubble in hla_motifs.css shows it instantly on
    # hover, instead of the browser's slow ~1s native title tooltip.
    tags$span(
      icon("info-circle"),
      class = "hla-hint",
      `data-tip` = hint
    )
  )
}

## Deliberately hla_global_motif_graph(), never the drawn hla_motif_graph():
## under the allele scope the drawn graph is built from one allele's carriers,
## and a feature nominated by the exposure cannot then be tested against it.
## See hla_global_motif_graph in data.R.
hla_feature_catalog <- reactive({
  g <- hla_global_motif_graph()
  if (!hla_motif_graph_ok(g)) {
    return(NULL)
  }
  va <- igraph::vertex_attr(g)
  data.frame(
    node_id = as.character(va$name),
    cdr3 = as.character(va$cdr3),
    v_gene = as.character(va$v_gene),
    motif_group = as.character(va$motif_group),
    stringsAsFactors = FALSE
  )
})

hla_selected_feature_members <- reactive({
  catalog <- hla_feature_catalog()
  if (is.null(catalog) || nrow(catalog) == 0) {
    return(NULL)
  }
  feature_type <- hla_param("hla_feature_type", "motif")
  feature_id <- input$hla_feature_id
  ## req(), not return(NULL): switching Feature type rebuilds the feature
  ## selectInput (it lives in a renderUI), so for one reactive cycle
  ## input$hla_feature_id still holds the OLD type's id while feature_type has
  ## already flipped to the new one. Selecting on the new type's column then
  ## matches nothing, and the three tables rendered a degenerate "all-absent"
  ## intermediate before the id caught up -- the visible flicker. Holding here
  ## until id and type agree (a non-empty match) lets the tables render once,
  ## straight to the final result. A genuinely empty catalog returned NULL above.
  req(feature_id, nzchar(feature_id))
  members <- if (identical(feature_type, "node")) {
    catalog[catalog$node_id == feature_id, , drop = FALSE]
  } else {
    catalog[catalog$motif_group == feature_id, , drop = FALSE]
  }
  req(nrow(members) > 0)
  members
})

output$hla_feature_selector_ui <- renderUI({
  catalog <- hla_feature_catalog()
  if (is.null(catalog) || nrow(catalog) == 0) {
    return(tags$p(class = "text-muted", "No drawable motif feature available."))
  }
  feature_type <- hla_param("hla_feature_type", "motif")
  if (identical(feature_type, "node")) {
    labels <- if (isTRUE(hla_param("hla_by_v", hla_by_v_default()))) {
      paste0(catalog$cdr3, " [", catalog$v_gene, "]")
    } else {
      catalog$cdr3
    }
    choices <- stats::setNames(catalog$node_id, labels)
  } else {
    # Largest motif first. Motif ids are generated in length-bin order, so the
    # natural order opens on an arbitrary two-CDR3 component; a bigger component
    # is the more informative thing to land on and rests on more observations.
    sizes <- table(catalog$motif_group)
    groups <- names(sort(sizes, decreasing = TRUE))
    labels <- sprintf("%s (%d CDR3)", groups, as.integer(sizes[groups]))
    choices <- stats::setNames(groups, labels)
  }
  selectizeInput(
    "hla_feature_id",
    hla_label_hint(
      "Locked feature:",
      paste(
        "The one motif family or CDR3 that the tables below compare between",
        "carriers and non-carriers of the chosen HLA allele. Type to search."
      )
    ),
    choices = choices,
    options = list(placeholder = "Type to search a motif or CDR3...")
  )
})

hla_overlap_table <- reactive({
  typing <- hla_active_typing()
  members <- hla_selected_feature_members()
  # The page's shared allele, not this tab's picker. Reading the picker directly
  # meant these numbers came from a control that only exists while this tab is
  # open, and that answered independently of the allele the network was
  # coloured by — the exact mismatch hla_color_allele() exists to prevent.
  allele <- hla_color_allele()
  seg <- hla_segments()
  if (
    !hla_has_typing() ||
      is.null(members) ||
      is.null(seg) ||
      is.null(allele) ||
      !nzchar(allele)
  ) {
    return(NULL)
  }
  hla_descriptive_feature_overlap(
    typing = typing,
    segments = seg,
    samples = names(getImmuneRepertoire()),
    allele = allele,
    feature_cdr3 = members$cdr3,
    feature_v_gene = if (isTRUE(hla_param("hla_by_v", hla_by_v_default()))) {
      members$v_gene
    } else {
      NULL
    }
  )
})

output$hla_associations_ui <- renderUI({
  if (!hla_has_typing()) {
    return(tags$p(
      class = "text-muted",
      paste(
        "No HLA typing loaded. Provide HLA typing in Data & QC to inspect",
        "feature-specific descriptive overlap."
      )
    ))
  }
  # Same labelling/order as the network's picker: an alphabetical list of every
  # allele makes the user guess which ones can show a contrast at all.
  alleles <- hla_allele_choices()
  tagList(
    # A circular selection is the one thing a reader cannot detect from the
    # numbers, so it is stated first and in the stronger style.
    if (!is.null(hla_selection_caveat())) {
      tags$div(
        class = "alert alert-warning",
        style = "font-size: 13px;",
        tags$b(paste0(hla_selection_caveat()$headline, " ")),
        hla_selection_caveat()$body
      )
    },
    tags$div(
      class = "alert alert-info",
      style = "font-size: 13px;",
      tags$b("Descriptive overlap only."),
      paste(
        "The selected node/motif is locked to the Hamming-1 graph over ALL",
        "cells, whatever the network scope. Counts are donor-level only with",
        "complete donor mapping, otherwise sample-level. No p-value or",
        "restriction claim is produced."
      )
    ),
    # Under ANY allele-selected scope the tab is deliberately NOT showing
    # features from the network on screen. Silently listing a different motif
    # set than the one drawn would be its own trap, so name the divergence and
    # the reason. Keyed on "not all", never on one scope's name: the guard in
    # hla_global_motif_graph covers every scoped view, and an explanation that
    # covers fewer of them leaves the rest diverging in silence.
    if (!identical(hla_scope_mode(), "all")) {
      built_from <- if (identical(hla_scope_mode(), "pair")) {
        sprintf(
          "carriers of %s and %s",
          hla_pair_allele_i() %||% "the Class I allele",
          hla_pair_allele_ii() %||% "the Class II allele"
        )
      } else {
        sprintf("carriers of %s", hla_color_allele() %||% "the selected allele")
      }
      tags$div(
        class = "alert alert-warning",
        style = "font-size: 13px;",
        tags$b("Features here ignore the network scope. "),
        paste0(
          "The network is currently built from ",
          built_from,
          ", so a motif taken from it would have been picked BY the very ",
          "allele this tab then compares carriers on — the carrier side would ",
          "look enriched whatever the biology. The list below is therefore ",
          "taken from the all-cells graph and may not match the network on ",
          "screen."
        )
      )
    },
    fluidRow(
      column(
        4,
        # Same two-line labels as the network's picker (allele on one line, the
        # carrier split under it): it is the same list, so it must not read as a
        # different one here.
        selectizeInput(
          "hla_association_allele",
          "HLA allele:",
          choices = alleles,
          # The page's shared allele, NOT alleles[1]. This tab renders the first
          # time the user opens it, which may be long after they picked an
          # allele on the network — seeding from the top of the list would
          # silently answer about a different allele than the one on screen.
          #
          # isolate(): this UI already re-renders on an allele change (the scope
          # caveat below names the allele), and the observer in data.R keeps the
          # picker in step, so reading it live here would only add a second
          # reason to rebuild the tab.
          selected = isolate(hla_color_allele()),
          options = list(render = HLA_TWO_LINE_RENDER)
        )
      ),
      column(
        3,
        # Stacked, not inline: inline puts both radios on one line, and at this
        # column width the second wraps under the first with its circle indented
        # by however much text preceded it. Stacked, the circles share a left
        # edge at any width.
        radioButtons(
          "hla_feature_type",
          hla_label_hint(
            "Feature type:",
            paste(
              "What the HLA comparison is run on: a whole motif family (a group",
              "of near-identical CDR3s that cluster together), or one single",
              "CDR3 sequence on its own."
            )
          ),
          choices = c("Motif component" = "motif", "CDR3 node" = "node"),
          selected = "motif",
          inline = FALSE
        )
      ),
      column(5, uiOutput("hla_feature_selector_ui"))
    ),
    tags$div(
      class = "hla-result-card",
      tags$h4("Observed overlap summary"),
      DT::dataTableOutput("hla_overlap_summary")
    ),
    tags$div(
      class = "hla-result-card",
      tags$h4(sprintf(
        "Per-unit breadth and %s fraction",
        hla_unit_noun()
      )),
      # State the denominator. These are fractions of what this data set
      # contains, which is not the donor's repertoire whenever the receptors
      # were selected (see the positive-control notice above).
      tags$p(
        class = "text-muted",
        style = "font-size: 12px;",
        sprintf(
          paste(
            "Fractions are of the %ss and clonotypes present IN THIS DATA SET",
            "for each unit%s - not of the unit's full repertoire."
          ),
          hla_unit_noun(),
          hla_selection_caveat()$subset_phrase %||% ""
        )
      ),
      DT::dataTableOutput("hla_overlap_table")
    ),
    tags$div(
      class = "hla-result-card",
      tags$h4("Analysis-unit × HLA allele matrix"),
      tags$p(
        class = "text-muted",
        style = "font-size: 12px;",
        "1 = carrier, 0 = locus-typed non-carrier, blank = locus untyped."
      ),
      DT::dataTableOutput("hla_allele_matrix")
    )
  )
})

## The two overlap tables update on every Feature-type/feature switch. Rendering
## them the normal (reactive) way tore the whole DataTable down and rebuilt it
## each time -- an abrupt full redraw. Instead each table is rendered ONCE from a
## constant empty skeleton (so the widget always exists with the right columns)
## and every data change is pushed through DT::replaceData() on a proxy, which
## swaps the rows in place without rebuilding the widget. Their dimensions are
## stable (summary = 3 status rows; breadth = one row per analysis unit), so
## replaceData is always a clean row-for-row swap.

## ---- Observed overlap summary ---------------------------------------------- ##
hla_overlap_summary_data <- reactive({
  empty <- data.frame(
    "HLA status" = character(),
    "Analysis units" = integer(),
    "Units with feature" = integer(),
    "Observed feature prevalence" = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  df <- hla_overlap_table()
  if (is.null(df) || nrow(df) == 0) {
    return(empty)
  }
  statuses <- c("carrier", "non-carrier", "untyped")
  do.call(
    rbind,
    lapply(statuses, function(status) {
      d <- df[df$hla_status == status, , drop = FALSE]
      data.frame(
        `HLA status` = status,
        `Analysis units` = nrow(d),
        `Units with feature` = sum(d$feature_present),
        `Observed feature prevalence` = if (nrow(d) > 0) {
          sprintf("%.1f%%", 100 * mean(d$feature_present))
        } else {
          "—"
        },
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
  )
})

output$hla_overlap_summary <- DT::renderDataTable(
  # scrollX: this table grows a column per carrier status and its headers are
  # named after the data set's own unit, so its width is not something this
  # layout can assume. Without it the table pushes the page sideways.
  DT::datatable(
    data.frame(
      "HLA status" = character(),
      "Analysis units" = integer(),
      "Units with feature" = integer(),
      "Observed feature prevalence" = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    rownames = FALSE,
    class = "display nowrap",
    options = list(dom = "t", scrollX = TRUE)
  )
)
hla_overlap_summary_proxy <- DT::dataTableProxy("hla_overlap_summary")
observeEvent(hla_overlap_summary_data(), {
  DT::replaceData(
    hla_overlap_summary_proxy,
    hla_overlap_summary_data(),
    resetPaging = FALSE,
    rownames = FALSE
  )
})

## ---- Per-unit breadth and cell fraction ------------------------------------ ##
## Empty skeleton mirrors the columns hla_descriptive_feature_overlap() returns,
## in order, so replaceData() lines up row values with the rendered header.
hla_overlap_breadth_skeleton <- function() {
  data.frame(
    analysis_unit = character(),
    unit_type = character(),
    hla_status = character(),
    feature_present = logical(),
    n_cells = integer(),
    n_feature_cells = integer(),
    n_unique_clonotypes = integer(),
    n_feature_clonotypes = integer(),
    unique_clonotype_fraction = numeric(),
    cell_fraction = numeric(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

hla_overlap_breadth_data <- reactive({
  df <- hla_overlap_table()
  if (is.null(df) || nrow(df) == 0) {
    return(hla_overlap_breadth_skeleton())
  }
  df$unique_clonotype_fraction <- round(df$unique_clonotype_fraction, 4)
  df$cell_fraction <- round(df$cell_fraction, 4)
  df
})

output$hla_overlap_table <- DT::renderDataTable({
  # The core keeps neutral column names; the header names them for what this
  # data set actually holds, so bulk rows are never presented as cells. noun is
  # stable per data set, so this render fires once, not on Feature-type switches.
  noun <- hla_unit_noun()
  headers <- colnames(hla_overlap_breadth_skeleton())
  headers[headers == "n_cells"] <- sprintf("n_%ss", noun)
  headers[headers == "n_feature_cells"] <- sprintf("n_feature_%ss", noun)
  headers[headers == "cell_fraction"] <- sprintf("%s_fraction", noun)
  DT::datatable(
    hla_overlap_breadth_skeleton(),
    rownames = FALSE,
    colnames = headers,
    class = "display nowrap",
    options = list(pageLength = 15, dom = "tip", scrollX = TRUE)
  )
})
hla_overlap_table_proxy <- DT::dataTableProxy("hla_overlap_table")
observeEvent(hla_overlap_breadth_data(), {
  DT::replaceData(
    hla_overlap_table_proxy,
    hla_overlap_breadth_data(),
    resetPaging = FALSE,
    rownames = FALSE
  )
})

output$hla_allele_matrix <- DT::renderDataTable({
  if (!hla_has_typing()) {
    return(NULL)
  }
  mat <- hla_unit_allele_matrix(
    hla_active_typing(),
    samples = names(getImmuneRepertoire())
  )
  DT::datatable(
    mat,
    rownames = FALSE,
    class = "display nowrap",
    options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
  )
})
