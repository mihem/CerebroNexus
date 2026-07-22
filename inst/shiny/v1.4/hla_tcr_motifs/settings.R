##----------------------------------------------------------------------------##
## HLA & TCR Motifs — parameter + status panels
##----------------------------------------------------------------------------##

## ---- Two-line option renderer ----------------------------------------- ##
## selectize draws each option/item as one run of text, so a label long enough
## to wrap breaks wherever it runs out of room — "re-colours" or "non-carrier"
## split across two lines. This renders "name|explanation" as a name plus a
## smaller, muted second line, so the break is a decision. escape() is
## selectize's own HTML escaper; the labels are ours, but rendering them raw
## would make any future label an injection point.
##
## Shared by every picker on this page whose label is "what it is" plus "what it
## means": network scope, and both allele pickers.
HLA_TWO_LINE_RENDER <- I(
  "{
    option: function(item, escape) {
      var p = item.label.split('|');
      return '<div class=\"option\" style=\"padding:6px 10px;line-height:1.35;\">' +
             '<div>' + escape(p[0]) + '</div>' +
             (p[1] ? '<div style=\"font-size:11px;color:#8a8a90;\">' +
                     escape(p[1]) + '</div>' : '') +
             '</div>';
    },
    item: function(item, escape) {
      var p = item.label.split('|');
      return '<div class=\"item\" style=\"line-height:1.35;\">' +
             '<div>' + escape(p[0]) + '</div>' +
             (p[1] ? '<div style=\"font-size:11px;color:#8a8a90;\">' +
                     escape(p[1]) + '</div>' : '') +
             '</div>';
    }
  }"
)

## ---- Left-column parameters ------------------------------------------- ##
output$hla_parameters_ui <- renderUI({
  chains <- hla_tcr_chains()
  # The panel is rebuilt only when the DATA changes (which controls exist), so
  # its own inputs are read under isolate() below: reading them live made every
  # scope / colour / checkbox change tear the whole panel down (finding #8). The
  # colour-by choices come from the shared hla_color_by_choices() reactive and
  # are kept current in place by the observer under this renderUI.
  tagList(
    if (length(chains) > 1) {
      selectInput(
        "hla_chain",
        "Chain:",
        choices = chains,
        selected = isolate(hla_active_chain())
      )
    } else {
      tags$p(
        tags$b("Chain: "),
        if (length(chains) == 1) chains[1] else "none"
      )
    },
    # Scope decides WHICH CELLS the graph is built from; colour only decides how
    # the built graph is painted. Needs an allele this page can interpret, since
    # every scope other than "all" is defined by who carries one.
    # Two lines per option, not one long label: at this column width the single
    # label wrapped wherever it happened to run out of room, splitting
    # "re-colours" across lines. The name goes on line one and the explanation
    # on a smaller second line, so the break is chosen rather than accidental.
    # Labels carry "name|explanation"; HLA_TWO_LINE_RENDER splits on the bar.
    if (hla_has_analyzable_allele()) {
      scope_choices <- c(
        "All cells|one graph; the allele only re-colours it" = "all",
        "One HLA allele|rebuild the graph on its carriers" = "allele"
      )
      # Offered only when both classes have an allele to pick AND a lineage
      # exists to sort cells between them; without either, the pair is not a
      # narrower view of anything.
      if (hla_pair_available()) {
        scope_choices <- c(
          scope_choices,
          "Class I x Class II pair|one allele of each; both classes at once" = "pair"
        )
      }
      selectizeInput(
        "hla_scope",
        "Network scope:",
        choices = scope_choices,
        selected = isolate(hla_param("hla_scope", "all")),
        options = list(render = HLA_TWO_LINE_RENDER)
      )
    },
    # Same two-line labels as the pickers above: the derived colourings each
    # need a clause explaining what the colour means, and at this column width
    # one long label wrapped mid-phrase — "(pick allele" / "below)".
    selectizeInput(
      "hla_color_by",
      "Colour nodes by:",
      choices = isolate(hla_color_by_choices()),
      selected = isolate(hla_param("hla_color_by", "cluster")),
      options = list(render = HLA_TWO_LINE_RENDER)
    ),
    # The page's single allele. It drives the carrier colouring AND the allele
    # scope, so it is shown for either. Under "all" scope, changing it only
    # re-colours the cached graph; under "allele" scope it is a build parameter
    # and rebuilds the Hamming distance matrix.
    conditionalPanel(
      condition = paste(
        "input.hla_color_by == 'hla_carrier'",
        "|| input.hla_scope == 'allele'"
      ),
      uiOutput("hla_color_allele_ui")
    ),
    # The pair needs two alleles, one per class, so it gets its own pair of
    # pickers rather than bending the page's single allele into both roles.
    conditionalPanel(
      condition = "input.hla_scope == 'pair'",
      uiOutput("hla_pair_allele_ui")
    ),
    uiOutput("hla_scope_status"),
    sliderInput(
      "hla_min_nodes",
      "Minimum motif size (nodes):",
      min = 2,
      max = 10,
      value = hla_default_min_nodes(),
      step = 1
    ),
    # Live read-out of what the current threshold actually shows, so the effect
    # of moving the slider is visible without guessing.
    uiOutput("hla_motif_readout"),
    checkboxInput(
      "hla_by_v",
      "Split motifs by V gene",
      value = isolate(isTRUE(hla_param("hla_by_v", hla_by_v_default())))
    ),
    checkboxInput(
      "hla_show_isolated",
      "Show unconnected CDR3s",
      value = isolate(isTRUE(hla_param("hla_show_isolated", FALSE)))
    ),
    # A declared grouping that is missing from the list above looks like a bug
    # or like bad data. It is neither: it has too many levels to read as colour.
    if (length(hla_color_cols_dropped()) > 0) {
      tags$p(
        class = "text-muted",
        style = "font-size: 11px;",
        sprintf(
          paste(
            "%s not offered above: more than %d levels, which colour cannot",
            "show on a network. Still on the node tooltips."
          ),
          paste(hla_color_cols_dropped(), collapse = ", "),
          HLA_MAX_COLOR_LEVELS
        )
      )
    },
    tags$p(
      class = "text-muted",
      style = "font-size: 11px;",
      "Edges use Hamming distance 1 (fixed)."
    )
  )
})

## Keep the colour-by picker's options current WITHOUT rebuilding the panel.
## The choices depend on scope (pair scope swaps "MHC context" for "Pair class")
## and on the data; the picker is rendered once from isolate(hla_color_by_choices())
## above, then updated in place here -- the discipline the allele pickers use.
## Fires on scope AND dataset change; ignoreInit because the render already
## seeded the current choices.
observeEvent(
  hla_color_by_choices(),
  {
    choices <- hla_color_by_choices()
    # Keep the current pick if the new scope still offers it, else fall back to
    # "Motif cluster" (""). Downstream tolerates a stale value, but the picker must
    # not display one it no longer lists. isolate(): reading the pick must not make
    # this observer depend on it, or every colour change would re-push the choices.
    sel <- isolate(hla_param("hla_color_by", "cluster"))
    if (!(sel %in% unname(choices))) {
      sel <- "cluster"
    }
    updateSelectizeInput(
      session,
      "hla_color_by",
      choices = choices,
      selected = sel,
      options = list(render = HLA_TWO_LINE_RENDER)
    )
  },
  ignoreInit = TRUE
)

## ---- Additional parameters (collapsed by default) --------------------- ##
## Display-only controls: nothing here rebuilds the graph.
##
## The box ships collapsed, and Shiny suspends a hidden output — but
## shinydashboard's collapse animation never triggers a recalculation, so the
## control stayed empty even after the user opened the box. Unsuspend it. Safe
## here precisely because this UI is static: it reads no data set, so it cannot
## drag reactive work into a hidden panel (cf. the spatial_images regression).
output$hla_additional_params_ui <- renderUI({
  tagList(
    radioButtons(
      "hla_legend_mode",
      "Legend:",
      choices = c(
        "Auto" = "auto",
        "Always show" = "always",
        "Hide" = "never"
      ),
      selected = hla_param("hla_legend_mode", "auto"),
      inline = TRUE
    ),
    tags$p(
      class = "text-muted",
      style = "font-size: 11px;",
      sprintf(
        paste(
          "Auto hides it only when colouring by motif cluster with more than",
          "%d motifs, where the numbers are arbitrary and the swatches map to",
          "nothing. Every other colouring always keeps its key."
        ),
        HLA_MOTIF_MAX_LEGEND_CLUSTERS
      )
    ),
    sliderInput(
      "hla_node_scale",
      "Node size:",
      min = HLA_NODE_SCALE_MIN,
      max = HLA_NODE_SCALE_MAX,
      value = 1,
      step = 0.1
    ),
    tags$p(
      class = "text-muted",
      style = "font-size: 11px;",
      paste(
        "Display only: it scales every node and the size cap by the same",
        "factor, so a node's area still reads as its count. A dense network",
        "can otherwise draw as one blob, a sparse one as specks."
      )
    )
  )
})

outputOptions(output, "hla_additional_params_ui", suspendWhenHidden = FALSE)

## ---- Allele picker for carrier colouring ------------------------------ ##
## Labelled with the carrier split and ordered by informativeness, so the user
## is not choosing blind out of a long alphabetical list: an allele that only
## one sample carries cannot show a contrast, and an allele nobody lacks cannot
## either. See hla_allele_choices() in data.R.
output$hla_color_allele_ui <- renderUI({
  choices <- hla_allele_choices()
  if (length(choices) == 0) {
    return(tags$p(class = "text-muted", "No HLA alleles available."))
  }
  selectizeInput(
    "hla_color_allele",
    "HLA allele to colour by:",
    choices = choices,
    # Seeded from the page's shared allele, so appearing for the first time
    # ADOPTS whatever the Associations picker already chose rather than
    # overwriting it with choices[1].
    #
    # isolate(): this control must not re-render when the allele changes. It is
    # one of the two things that CHANGES the allele, so reading it live would
    # make the picker tear itself down and rebuild on every pick. Once rendered,
    # the observer above keeps it in step.
    selected = isolate(hla_color_allele()),
    options = list(render = HLA_TWO_LINE_RENDER)
  )
})

## ---- The pair scope's two allele pickers ------------------------------ ##
## One picker per class, each offering only that class's alleles: the pair is
## defined by the two classes, and a picker that let both sides be Class I would
## be offering a graph this page cannot build (see
## hla_scope_segments_by_allele_pair).
output$hla_pair_allele_ui <- renderUI({
  class_i <- hla_class_allele_choices("Class I")
  class_ii <- hla_class_allele_choices("Class II")
  if (length(class_i) == 0 || length(class_ii) == 0) {
    return(tags$p(
      class = "text-muted",
      "No Class I / Class II allele pair available."
    ))
  }
  tagList(
    selectizeInput(
      "hla_pair_allele_i",
      "Class I allele (CD8 side):",
      choices = class_i,
      selected = hla_pair_allele_i(),
      options = list(render = HLA_TWO_LINE_RENDER)
    ),
    selectizeInput(
      "hla_pair_allele_ii",
      "Class II allele (CD4 side):",
      choices = class_ii,
      selected = hla_pair_allele_ii(),
      options = list(render = HLA_TWO_LINE_RENDER)
    ),
    # DQ/DP are stored and normalized but never offered (HLA_MVP_LOCI): a lone
    # DQB1 / DPB1 allele is not an independently interpretable molecule. Say so
    # here, or a user looking for their DQ allele reads its absence as a bug.
    tags$p(
      class = "text-muted",
      style = "font-size: 11px;",
      sprintf(
        paste(
          "Class II here means %s only. DQ and DP are alpha/beta heterodimers",
          "and need pairing rules this version does not have."
        ),
        paste(intersect(HLA_MVP_LOCI, HLA_CLASS_II_LOCI), collapse = " / ")
      )
    )
  )
})

## ---- Evidence-status panel -------------------------------------------- ##
output$hla_status_ui <- renderUI({
  t <- hla_active_typing()
  n_ir_samples <- length(getImmuneRepertoire())
  session_on <- !is.null(hla_session_typing()) &&
    is.data.frame(hla_session_typing()) &&
    nrow(hla_session_typing()) > 0
  channel <- if (session_on) "session upload" else "stored .crb"

  ## The lineage column decides each cell's MHC context and gates the Class I x
  ## Class II scope, so when it was INFERRED rather than declared the page is
  ## resting on a guess about which column holds the CD4/CD8 label. Say so, and
  ## name the column, rather than presenting it as a stated fact.
  lineage_note <- local({
    col <- hla_celltype_col()
    if (is.na(col) || isTRUE(hla_celltype_col_declared())) {
      return(NULL)
    }
    tags$p(
      class = "text-warning",
      style = "font-size: 12px;",
      sprintf(
        paste(
          "Lineage column inferred: '%s'. MHC context and the Class I x Class II",
          "scope rest on it. Declare technical_info$lineage_column to pin it."
        ),
        col
      )
    )
  })

  if (!hla_has_typing()) {
    return(tagList(
      tags$p(tags$b("HLA context: "), "none loaded"),
      tags$p(
        class = "text-muted",
        style = "font-size: 12px;",
        paste(
          "Motif network uses cell-type / sample colouring only. Provide HLA",
          "typing in the Data & QC tab to enable donor-level HLA context."
        )
      ),
      lineage_note
    ))
  }

  typed_samples <- length(unique(t$sample))
  src <- paste(unique(t$source_type), collapse = ", ")
  ir_samples <- names(getImmuneRepertoire())
  covered <- sum(ir_samples %in% unique(t$sample))
  tagList(
    tags$p(
      tags$b("HLA context source: "),
      channel,
      sprintf(" (%s)", src)
    ),
    tags$p(sprintf(
      "Coverage: %d / %d IR samples typed.",
      covered,
      n_ir_samples
    )),
    if (any(t$source_type %in% c("synthetic", "unknown"))) {
      tags$p(
        class = "text-warning",
        style = "font-size: 12px;",
        "Contains synthetic / unknown-provenance typing: descriptive context only."
      )
    },
    lineage_note,
    ## Typing loaded, nothing to analyse. The controls are gone by now, so this
    ## is the only place the user can learn whether the file was for another
    ## cohort, was DQ/DP-only, or simply holds no contrast.
    if (!is.null(hla_no_allele_reason())) {
      tagList(
        tags$p(
          class = "text-warning",
          style = "font-size: 12px;",
          tags$b("No allele can be analysed here.")
        ),
        tags$p(
          class = "text-muted",
          style = "font-size: 12px;",
          hla_no_allele_reason()
        )
      )
    },
    ## The typing warning above covers the HLA side only. Colouring the network
    ## by carrier status is itself an association display, so a declared
    ## selection caveat has to reach every tab, not just HLA Associations.
    if (!is.null(hla_selection_caveat())) {
      tags$p(
        class = "text-warning",
        style = "font-size: 12px;",
        tags$b(hla_selection_caveat()$headline)
      )
    },
    tags$p(
      class = "text-muted",
      style = "font-size: 12px;",
      paste(
        "Alleles shown for a motif are candidate co-occurrences, not confirmed",
        "TCR restrictions."
      )
    )
  )
})

## ---- What the pair scope kept, and on which side ---------------------- ##
## The pair drops more than any other scope — a donor carrying neither allele
## contributes nothing, and a Class I cell of a donor who carries only the
## Class II allele is dropped too. Report the split, or a thin network reads as
## a weak signal rather than as a small denominator.
hla_pair_scope_status <- function() {
  full <- hla_segments()
  scoped <- hla_scoped_segments()
  a_i <- hla_pair_allele_i()
  a_ii <- hla_pair_allele_ii()
  if (is.null(full) || is.null(a_i) || is.null(a_ii)) {
    return(NULL)
  }
  noun <- hla_unit_noun()
  if (is.null(scoped) || nrow(scoped) == 0) {
    return(tags$p(
      class = "text-danger",
      style = "font-size: 12px;",
      sprintf(
        paste(
          "Nothing in scope: no typed carrier of %s has a CD8 %s, and no",
          "carrier of %s has a CD4 %s."
        ),
        a_i,
        noun,
        a_ii,
        noun
      )
    ))
  }
  n_i <- sum(scoped$pair_allele == a_i)
  n_ii <- sum(scoped$pair_allele == a_ii)
  tagList(
    tags$p(
      class = "text-muted",
      style = "font-size: 12px;",
      sprintf(
        "Scope: %s of %s %ss — %s on %s (CD8), %s on %s (CD4).",
        format(nrow(scoped), big.mark = ","),
        format(nrow(full), big.mark = ","),
        noun,
        format(n_i, big.mark = ","),
        a_i,
        format(n_ii, big.mark = ","),
        a_ii
      )
    ),
    tags$p(
      class = "text-muted",
      style = "font-size: 12px;",
      paste(
        "Each",
        noun,
        "is shown under the allele its OWN lineage could present on and its",
        "donor carries. A CDR3 coloured",
        sprintf("\"%s\"", HLA_PAIR_MIXED_LABEL),
        "was seen in both compartments — which is convergence across lineages,",
        "not evidence that either allele restricts it."
      )
    )
  )
}

## ---- What the current scope actually kept ----------------------------- ##
## A scope silently dropping most of the data is the failure mode here: the user
## sees a smaller network and has no way to tell whether the allele is rare, the
## class filter bit, or the lineage was Unknown. State the counts.
output$hla_scope_status <- renderUI({
  if (identical(hla_scope_mode(), "pair")) {
    return(hla_pair_scope_status())
  }
  if (!identical(hla_scope_mode(), "allele")) {
    return(NULL)
  }
  full <- hla_segments()
  scoped <- hla_scoped_segments()
  allele <- hla_color_allele()
  if (is.null(full) || is.null(allele)) {
    return(NULL)
  }
  n_full <- nrow(full)
  n_scoped <- if (is.null(scoped)) 0L else nrow(scoped)
  cls <- hla_locus_class(hla_allele_locus(allele))
  has_ctx <- "mhc_context" %in% colnames(full)
  noun <- hla_unit_noun()
  tagList(
    tags$p(
      class = if (n_scoped == 0) "text-danger" else "text-muted",
      style = "font-size: 12px;",
      sprintf(
        "Scope: %s of %s %ss — carriers of %s%s.",
        format(n_scoped, big.mark = ","),
        format(n_full, big.mark = ","),
        noun,
        allele,
        if (has_ctx && cls %in% c("Class I", "Class II")) {
          sprintf(", %s lineage only", cls)
        } else {
          ""
        }
      )
    ),
    # A bulk repertoire has no lineage to match on. Saying so beats letting the
    # user read a carrier-only scope as class-matched.
    if (!has_ctx) {
      tags$p(
        class = "text-warning",
        style = "font-size: 12px;",
        paste(
          "No lineage available, so this scope is carriers only and is NOT",
          "class-matched."
        )
      )
    },
    if (n_scoped == 0) {
      tags$p(
        class = "text-danger",
        style = "font-size: 12px;",
        sprintf(
          "Nothing in scope: no typed carrier of %s has a %s-lineage cell here.",
          allele,
          cls
        )
      )
    },
    # The scope removes the comparison group. That is the whole reason the
    # carrier colouring on the "All cells" scope exists, so say it here rather
    # than let a carrier-only network read as evidence.
    tags$p(
      class = "text-muted",
      style = "font-size: 12px;",
      paste(
        "Every donor in this scope is a carrier, so recurrence across donors",
        "here cannot be told apart from an ordinary public TCR. Use the",
        "\"All cells\" scope with HLA carrier colouring for that contrast."
      )
    )
  )
})
