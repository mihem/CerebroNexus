##----------------------------------------------------------------------------##
## HLA & TCR Motifs — data layer (reactive composition)
##
## Core algorithms live in the package (R/hla_motif_core.R, R/hla_typing.R);
## this file only wires reactives. Nothing here recomputes a distance matrix on
## a display-only change — the graph reactive is keyed on the build parameters
## and the active dataset, colour/legend changes are handled in the renderer.
##----------------------------------------------------------------------------##

## ---- Optional-dependency gate ----------------------------------------- ##
## visNetwork + stringdist are Imports, so they are present in a normal install.
## The gate stays as a defensive guard for unusual environments.
hla_has_deps <- function() {
  requireNamespace("stringdist", quietly = TRUE) &&
    requireNamespace("igraph", quietly = TRUE) &&
    requireNamespace("visNetwork", quietly = TRUE)
}

## ---- Per-session cache wrapper (module-local, no cross-module coupling) ##
## cache = "session" keeps caches isolated per user; the selected dataset is
## appended to every key at the call site so switching datasets invalidates it.
hla_bindCache <- function(x, ..., cache = "session") {
  if (utils::packageVersion("shiny") >= "1.6.0") {
    shiny::bindCache(x, ..., cache = cache)
  } else {
    x
  }
}

## ---- Metadata-annotated IR data (barcode-joined) ---------------------- ##
## Reuse the same join contract as the IR module: the IR tables carry only
## scRepertoire columns; biological grouping lives in cell metadata, attached
## here by barcode so the page can colour by any metadata column.
hla_ir_annotated <- reactive({
  data <- getImmuneRepertoire()
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(NULL)
  }
  md <- tryCatch(getMetaData(), error = function(e) NULL)
  has_md <- !is.null(md) && "cell_barcode" %in% colnames(md)
  meta_cols <- if (has_md) {
    setdiff(colnames(md), "cell_barcode")
  } else {
    character(0)
  }
  nm <- names(data)
  out <- lapply(seq_along(data), function(i) {
    df <- data[[i]]
    if (is.null(df)) {
      return(df)
    }
    if (has_md && "barcode" %in% colnames(df)) {
      add <- setdiff(meta_cols, colnames(df))
      if (length(add) > 0) {
        idx <- match(df$barcode, md$cell_barcode)
        for (col in add) {
          df[[col]] <- md[[col]][idx]
        }
      }
    }
    # `sample` is STRUCTURAL here, not a metadata column this page hopes to
    # find: HLA typing is matched to the repertoire by the names of this very
    # list (hla_analysis_unit_map is handed names(getImmuneRepertoire())), so
    # the list name is what "sample" has to mean for every join on this page.
    # Taking it from a metadata column of the same name would leave the page
    # broken on any object that names it differently, and quietly inconsistent
    # on one where the column and the list disagree.
    if (!is.null(nm) && nzchar(nm[i])) {
      df$sample <- nm[i]
    }
    df
  })
  names(out) <- nm
  out
})

## ---- TCR chains available (TRA / TRB only for this page) --------------- ##
hla_tcr_chains <- reactive({
  intersect(
    tryCatch(hla_detect_chains(getImmuneRepertoire()), error = function(e) {
      character(0)
    }),
    c("TRA", "TRB")
  )
})

## ---- Active chain (default TRB when present) --------------------------- ##
hla_active_chain <- reactive({
  ch <- input$hla_chain
  chains <- hla_tcr_chains()
  if (!is.null(ch) && nzchar(ch) && ch %in% chains) {
    return(ch)
  }
  if ("TRB" %in% chains) {
    return("TRB")
  }
  if (length(chains) > 0) {
    return(chains[1])
  }
  "TRB"
})

## ---- Read a build/display parameter with a default -------------------- ##
hla_param <- function(id, default = NULL) {
  v <- input[[id]]
  if (is.null(v)) default else v
}

## ---- Columns the receptor table actually carries ---------------------- ##
## Declared or not. This is a question about the DATA ("is the lineage here?"),
## which is why it is separate from the colour list below: what the user may
## colour by is a curation decision, what this page can compute is not. Tying
## the two together would make MHC context vanish for any data set whose lineage
## column exists but was never declared a grouping.
hla_available_cols <- reactive({
  data <- hla_ir_annotated()
  if (is.null(data)) {
    return(character(0))
  }
  Reduce(intersect, lapply(data, colnames))
})

## ---- Metadata columns offered for node colouring ---------------------- ##
## The data set's DECLARED grouping variables — the same list, in the same
## order, that the Groups page offers as "Choose a grouping variable".
##
## This used to infer the list instead: every metadata column that happened to
## be a string with more than one value. Inference cannot tell a grouping from a
## leftover, so it offered whatever the upstream pipeline had lying around
## (`orig.ident`, `RNA_snn_res.0.6`, ...) as if those were biology, and the same
## data set could offer different colourings on two pages. getGroups() is the
## object's own answer to "what are the groupings here"; there is no reason for
## this page to hold a second opinion.
##
## Intersected with what actually reached the receptor table: a declared group
## whose values did not survive the barcode join has nothing to colour with.
hla_color_meta_cols <- reactive({
  groups <- tryCatch(getGroups(), error = function(e) character(0))
  if (length(groups) == 0) {
    return(character(0))
  }
  intersect(groups, hla_available_cols())
})

## Colouring by a column with more distinct values than this is unreadable: the
## legend is suppressed and every point gets a near-arbitrary hue, so the control
## would promise a grouping it cannot show. Such columns are not offered.
HLA_MAX_COLOR_LEVELS <- 24L

## ---- Level count of each declared grouping, as the receptors see it ----- ##
hla_color_col_levels <- reactive({
  cols <- hla_color_meta_cols()
  data <- hla_ir_annotated()
  if (length(cols) == 0 || is.null(data)) {
    return(stats::setNames(integer(0), character(0)))
  }
  vapply(
    cols,
    function(col) {
      v <- unlist(lapply(data, function(df) {
        if (col %in% colnames(df)) as.character(df[[col]]) else character(0)
      }))
      length(unique(v[!is.na(v) & nzchar(v)]))
    },
    integer(1)
  )
})

## ---- Colour columns that can actually be read -------------------------- ##
## A grouping with a level per donor (`sample` in the 100-donor bulk cohort)
## yields ~100 hues over a network of a few hundred nodes: every node gets its
## own colour and the picture stops being a grouping at all. Capped for the
## colour picker only; the column is still on the node tooltips, and
## "Sample of origin" is the readable way to ask that question here.
##
## This is the ONE place this page's list is narrower than the Groups page's.
## hla_color_cols_dropped() exists so the difference is stated on screen rather
## than looking like the grouping was never declared.
hla_usable_color_cols <- reactive({
  cols <- hla_color_meta_cols()
  if (length(cols) == 0) {
    return(character(0))
  }
  if (is.null(hla_ir_annotated())) {
    return(cols)
  }
  n_levels <- hla_color_col_levels()
  cols[n_levels[cols] <= HLA_MAX_COLOR_LEVELS]
})

hla_color_cols_dropped <- reactive({
  setdiff(hla_color_meta_cols(), hla_usable_color_cols())
})

## ---- Does the source key its receptors on V gene + CDR3? --------------- ##
## Node identity defaults to the CDR3 alone, which is right when the source
## reports a full rearrangement. Some sources (bulk V-family + CDR3, e.g. the
## Emerson/pubtcrs cohort) define a receptor as the PAIR: merging on CDR3 alone
## would fuse receptors the source counts as distinct and double-count a donor
## across them. The .crb declares this in `technical_info$receptor_key`;
## "v_gene+cdr3" makes split-by-V the default. The user can still override.
hla_source_keys_on_v <- reactive({
  ti <- tryCatch(data_set()$technical_info, error = function(e) NULL)
  is.list(ti) && identical(ti$receptor_key, "v_gene+cdr3")
})

hla_by_v_default <- reactive({
  isTRUE(hla_source_keys_on_v())
})

## ---- Default minimum motif size --------------------------------------- ##
## A fixed default of 2 keeps every 2-node component, which on a real repertoire
## means thousands of nodes in hundreds of tiny motifs: physics is disabled, the
## layout collapses to a ring and the first thing the user sees is a hairball.
## Scale the default to the data instead, so the first view is a readable set of
## the larger motifs; the slider still exposes the full range down to 2.
hla_default_min_nodes <- reactive({
  seg <- hla_segments()
  if (is.null(seg) || nrow(seg) == 0) {
    return(2L)
  }
  n_cdr3 <- length(unique(seg$cdr3))
  if (n_cdr3 > 2000) {
    6L
  } else if (n_cdr3 > 500) {
    4L
  } else {
    2L
  }
})

## ---- HLA alleles offered for carrier colouring ------------------------ ##
## Labelled with the carrier / non-carrier split and ordered by how much
## contrast they can show. An allele carried by everyone (or by nobody) in the
## cohort colours the whole network one shade, so those sort last.
hla_allele_choices <- reactive({
  if (!hla_has_typing()) {
    return(character(0))
  }
  typing <- hla_active_typing()
  samples <- names(getImmuneRepertoire())
  summ <- hla_allele_carrier_summary(typing, samples = samples)
  if (is.null(summ) || nrow(summ) == 0) {
    return(character(0))
  }
  # Offer ONLY the loci this version can interpret (HLA_MVP_LOCI: A/B/C/DRB1).
  # DQ and DP are alpha/beta heterodimers: a lone DQB1 or DPB1 allele is not an
  # independently interpretable unit without pairing/phasing, so presenting one
  # in an allele picker would invite exactly the over-reading the page exists to
  # avoid. Other loci stay stored and visible in Data & QC, just not offered
  # here as an analysis axis.
  summ <- summ[summ$locus %in% HLA_MVP_LOCI, , drop = FALSE]
  if (nrow(summ) == 0) {
    return(character(0))
  }
  # Informativeness = the size of the smaller of the two groups: that is the
  # most donors a carrier/non-carrier contrast could ever rest on.
  contrast <- pmin(summ$n_carrier, summ$n_noncarrier)
  ord <- order(-contrast, -summ$n_carrier, summ$allele)
  summ <- summ[ord, , drop = FALSE]
  # "allele|counts": the bar is where HLA_TWO_LINE_RENDER breaks the label, so
  # the allele gets a line of its own and the carrier split reads as the
  # annotation it is. At this column width one long label wrapped wherever it
  # ran out of room, splitting "non-carrier" across lines.
  stats::setNames(
    summ$allele,
    sprintf(
      "%s|%d carrier / %d non-carrier",
      summ$allele,
      summ$n_carrier,
      summ$n_noncarrier
    )
  )
})

## ---- The page's single allele ------------------------------------------ ##
## ONE allele for the whole page. The network's colour and the Associations
## tables must answer the same question: two independent pickers let the user
## colour by one allele while reading another's numbers, and nothing on screen
## would reveal the mismatch.
##
## The value is held HERE, not in either picker's input, and that is the whole
## point. It used to live in input$hla_color_allele, with the two pickers kept
## in sync by updateSelectInput. But that picker sits in a conditionalPanel, so
## Shiny suspends it whenever the network is neither scoped to an allele nor
## coloured by carrier status — and an update addressed to a control that is not
## rendered is silently dropped. Picking an allele on the Associations tab in
## that state therefore wrote to nothing: hla_color_allele() kept answering with
## choices[1], and the moment the network's picker did appear it was seeded from
## choices[1] and dragged the Associations picker back with it. Measured: the
## user's HLA-B*07:02 replaced by HLA-A*02:01, no error, no notice.
##
## A reactiveVal cannot be suspended, so both pickers can always write it and
## either can be absent.
hla_allele_state <- reactiveVal(NULL)

## Either picker writes the shared value. Guarded on nzchar() because a
## selectize that is being torn down reports "" on its way out, which must not
## erase the selection.
observeEvent(input$hla_color_allele, {
  a <- input$hla_color_allele
  if (!is.null(a) && nzchar(a)) {
    hla_allele_state(a)
  }
})
observeEvent(input$hla_association_allele, {
  a <- input$hla_association_allele
  if (!is.null(a) && nzchar(a)) {
    hla_allele_state(a)
  }
})

## Keep whichever pickers ARE on screen showing it. Both can be visible at once
## (the parameter column is shared by every tab, so an allele-scoped network and
## the Associations tab put their pickers side by side), so this is not
## redundant with seeding them at render time. Updates to a picker that is not
## rendered are dropped, which is now harmless: the value does not live there.
##
## Terminates rather than ping-pongs: an update only fires when the picker
## disagrees, the picker then reports the value this observer just sent, and
## reactiveVal() does not invalidate when set to the value it already holds.
observeEvent(hla_allele_state(), {
  a <- hla_allele_state()
  if (is.null(a)) {
    return()
  }
  if (!identical(a, input$hla_color_allele)) {
    updateSelectInput(session, "hla_color_allele", selected = a)
  }
  if (!identical(a, input$hla_association_allele)) {
    updateSelectInput(session, "hla_association_allele", selected = a)
  }
})

## ---- Allele currently colouring the network --------------------------- ##
## Falls back to the most informative allele so the first carrier render is
## meaningful before any picker has reported a value.
##
## Normalised against the CURRENT choices on every read, so an allele held over
## from a data set that had it never leaks into one that does not.
hla_color_allele <- reactive({
  choices <- hla_allele_choices()
  if (length(choices) == 0) {
    return(NULL)
  }
  a <- hla_allele_state()
  if (!is.null(a) && nzchar(a) && a %in% choices) {
    return(a)
  }
  unname(choices[1])
})

## ---- What one row of the data actually is ----------------------------- ##
## Read the data set's DECLARED observation unit (see getObservationUnit in
## utility_functions.R) rather than inferring it. An earlier version guessed
## "not a cell" from an empty expression matrix, which is a proxy: it would
## relabel any data set that merely ships without expression.
hla_unit_noun <- reactive({
  getObservationUnit()$singular
})

## ---- Column the CD4/CD8 lineage is read from -------------------------- ##
## MHC context (CD8 -> Class I, CD4/Treg -> Class II) needs to know which column
## holds the lineage label. Two ways to know, in this order:
##
##   1. the data set DECLARES it in `technical_info$lineage_column`, the same
##      contract style as observation_unit / receptor_key;
##   2. failing that, ASK THE VALUES -- but only of the DECLARED grouping
##      variables. hla_lineage_context() matches on the label itself (CD8 / CD4
##      / Treg), never on the column name, so the declared group that resolves
##      the most cells to a real lineage is the lineage column. Restricting to
##      declared groups is what stops an identifier (a `sample` value of
##      "CD8_case") or a covariate ("anti-CD4") from being taken for biology.
##
## What this deliberately no longer does is match names: `cell_type_fine` then
## `cell_type` worked for the bundled demos and quietly produced "Unknown" for
## everyone whose annotation lives in `celltype`, `annotation`, `azimuth_l2`,
## `predicted.id`... This is a general-purpose viewer; the columns are the
## user's to name.
##
## Ties break toward the column with MORE distinct labels: between a coarse
## "T cells / B cells" and a fine "CD8 TEM / CD4 naive", both may resolve the
## same cells, and the finer one carries the lineage more precisely.
hla_celltype_col <- reactive({
  data <- hla_ir_annotated()
  cols <- hla_available_cols()
  if (is.null(data) || length(cols) == 0) {
    return(NA_character_)
  }

  declared <- tryCatch(
    data_set()$technical_info$lineage_column,
    error = function(e) NULL
  )
  if (
    is.character(declared) && length(declared) >= 1 && declared[1] %in% cols
  ) {
    return(declared[1])
  }

  # Inference is limited to the data set's DECLARED grouping variables. Scoring
  # every available column let an identifier -- a `sample` value like "CD8_case",
  # or a treatment such as "anti-CD4" -- win the lineage role and silently change
  # HLA scope filtering. A grouping variable is the user's own "this is biology";
  # an id or a covariate is not, so it is never a candidate here.
  candidates <- hla_color_meta_cols()
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  score <- vapply(
    candidates,
    function(col) {
      v <- unlist(lapply(data, function(df) {
        if (col %in% colnames(df)) as.character(df[[col]]) else character(0)
      }))
      # Scored in the core so the rule is unit-tested: values that read as an
      # experimental condition ("anti-CD4", "CD8_case") carry a lineage token
      # but are NOT a lineage, and counting them would let a treatment column
      # take the role and change which cells the Class I / Class II scope keeps.
      hla_lineage_column_score(v)
    },
    numeric(1)
  )
  # Below the bar, do not infer at all: the pair scope then stays unavailable
  # rather than resting on a guess (hla_pair_available() gates on this).
  if (max(score) < HLA_LINEAGE_MIN_SHARE) {
    return(NA_character_)
  }
  best <- candidates[score == max(score)]
  if (length(best) == 1) {
    return(best)
  }
  best_levels <- vapply(
    best,
    function(col) {
      v <- unlist(lapply(data, function(df) {
        if (col %in% colnames(df)) as.character(df[[col]]) else character(0)
      }))
      length(unique(v[!is.na(v) & nzchar(v)]))
    },
    integer(1)
  )
  best[which.max(best_levels)]
})

## ---- Was the lineage column DECLARED, or inferred from its values? ----- ##
## The Class I / Class II scope filters cells by the lineage read off this
## column, so an inferred one is a guess that changes what the page analyses. It
## must say so in the UI rather than read as a stated fact.
hla_celltype_col_declared <- reactive({
  cols <- hla_available_cols()
  declared <- tryCatch(
    data_set()$technical_info$lineage_column,
    error = function(e) NULL
  )
  is.character(declared) && length(declared) >= 1 && declared[1] %in% cols
})

## ---- Parsed segments for the active chain (+ per-cell MHC context) ----- ##
hla_segments <- reactive({
  data <- hla_ir_annotated()
  chain <- hla_active_chain()
  if (is.null(data)) {
    return(NULL)
  }
  seg <- hla_parse_ir_segments(data, chain)
  if (is.null(seg) || nrow(seg) == 0) {
    return(seg)
  }
  ct_col <- hla_celltype_col()
  if (!is.na(ct_col) && ct_col %in% colnames(seg)) {
    seg$mhc_context <- hla_lineage_context(seg[[ct_col]])
  }
  seg
})

## ---- Metadata columns to carry onto nodes (for tooltip / colouring) ---- ##
hla_node_meta_cols <- reactive({
  # `sample` is always carried: every HLA join on this page is by sample, and
  # hla_ir_annotated() guarantees the column from the repertoire's own list
  # names. The lineage column comes from hla_celltype_col(), which finds it in
  # the data rather than assuming what it is called.
  #
  # EVERY colourable column is carried, not just the one currently selected. The
  # graph is cached on this set, so making it independent of hla_color_by means a
  # colour switch never re-keys the cache: the column is already on the node, and
  # the renderer recolours it in place instead of rebuilding (see the
  # visNetworkProxy observer in visualizations.R).
  ct <- hla_celltype_col()
  cols <- c(
    "sample",
    if (!is.na(ct)) ct else character(0),
    hla_color_meta_cols()
  )
  unique(intersect(cols, hla_available_cols()))
})

## ---- Colour-by choices (scope-aware) ---------------------------------- ##
## What the network can be coloured by depends on the DATA (which meta columns
## exist, whether a lineage column is present, how many samples) and on the
## current scope. Held as ONE reactive so the picker is rendered once from an
## isolate() of it and then kept current in place by an observer -- building
## these choices inside the picker's own renderUI made every scope change tear
## the whole parameter panel down (finding #8).
hla_color_by_choices <- reactive({
  meta_cols <- hla_usable_color_cols()
  # "cluster", not "": selectize treats an empty-string value as NO selection,
  # so the default option was silently dropped from the dropdown -- the picker
  # showed nothing and, once another colouring was picked, motif cluster could
  # never be chosen again. "cluster" is the node attribute this colouring reads,
  # so the graph builder resolves it exactly as the old fallback did.
  choices <- c(
    "Motif cluster" = "cluster",
    stats::setNames(meta_cols, meta_cols)
  )
  # In the pair scope every node already carries its candidate allele, and that
  # IS the lineage split — so "MHC context" would be the same picture under a
  # vaguer name. Offer the pair class instead.
  if (identical(hla_scope_mode(), "pair")) {
    choices <- c(
      choices,
      "Pair class|which allele, or both" = "pair_allele"
    )
  } else if (!is.na(hla_celltype_col())) {
    # "MHC context" is a derived node attribute (CD8->Class I / CD4->Class II /
    # Unknown), offered only when a lineage column exists to derive it from.
    choices <- c(
      choices,
      "MHC context|CD8 -> Class I, CD4 -> Class II" = "mhc_context"
    )
  }
  # Carrier status of ONE allele is the colouring this page exists for: it is
  # what connects the network to the HLA context. Deliberately named for what it
  # shows (who carries the allele), never as if the allele restricted the TCR.
  #
  # Gated on an allele this page can actually put on screen, not merely on the
  # typing table being non-empty: with typing that matches no sample, or only
  # DQ/DP, this control used to appear and then have nothing to offer.
  if (hla_has_analyzable_allele()) {
    choices <- c(
      choices,
      "HLA carrier status|pick the allele below" = "hla_carrier"
    )
  }
  # Sample of origin, with every CDR3 seen in >1 sample collapsed to "Shared".
  # Distinct from colouring by the plain `sample` column, which shows the node's
  # MODAL sample and so hides the cross-sample recurrence an HLA screen looks
  # for. Offered only when the repertoire actually has more than one sample.
  if (length(names(getImmuneRepertoire())) > 1) {
    choices <- c(
      choices,
      "Sample of origin|seen in more than one = black" = "sample_origin"
    )
  }
  choices
})

## ---- Network scope ----------------------------------------------------- ##
## "all"    -> one graph over every cell; an allele only re-colours it.
## "allele" -> the graph is REBUILT on the cells that could bear on the page's
##             allele (its carriers, class-matched). This is a different graph,
##             not a different colour: edges never join a carrier's CDR3 to a
##             non-carrier's, which the global graph does by construction.
## "pair"   -> the graph is rebuilt on ONE Class I allele and ONE Class II
##             allele at once; each cell is assigned the one its own lineage
##             could use, and a CDR3 seen on both sides is the thing to look at.
## Scoping needs typing, so it collapses to "all" without it.
## Falls back on hla_has_analyzable_allele(), not hla_has_typing(): removing the
## selector does not clear input$hla_scope, so a session that scoped to an
## allele and then lost its usable typing would keep reporting "allele" with no
## control on screen and no allele behind it.
HLA_SCOPE_MODES <- c("all", "allele", "pair")
hla_scope_mode <- reactive({
  m <- hla_param("hla_scope", "all")
  if (!hla_has_analyzable_allele() || !(m %in% HLA_SCOPE_MODES)) {
    return("all")
  }
  # The pair scope needs BOTH classes to be offerable, and a lineage to split
  # cells by. Without either it is not a narrower view, it is undefined — so it
  # collapses rather than silently drawing something else.
  if (identical(m, "pair") && !hla_pair_available()) {
    return("all")
  }
  m
})

## ---- Can a Class I x Class II pair be formed at all? ------------------- ##
hla_pair_available <- reactive({
  !is.na(hla_celltype_col()) &&
    length(hla_class_allele_choices("Class I")) > 0 &&
    length(hla_class_allele_choices("Class II")) > 0
})

## The allele choices of one MHC class, in the same order and with the same
## labels as the page's single-allele picker.
hla_class_allele_choices <- function(class) {
  choices <- hla_allele_choices()
  if (length(choices) == 0) {
    return(choices)
  }
  keep <- vapply(
    unname(choices),
    function(a) identical(hla_locus_class(hla_allele_locus(a)), class),
    logical(1)
  )
  choices[keep]
}

## ---- The two alleles of the pair scope -------------------------------- ##
## The Class I side reuses the page's single allele when that allele IS class I,
## so switching into the pair scope keeps the allele the user was already
## looking at rather than silently jumping to another one.
hla_pair_allele_i <- reactive({
  choices <- hla_class_allele_choices("Class I")
  if (length(choices) == 0) {
    return(NULL)
  }
  picked <- input$hla_pair_allele_i
  if (!is.null(picked) && nzchar(picked) && picked %in% choices) {
    return(picked)
  }
  current <- hla_color_allele()
  if (
    !is.null(current) &&
      current %in% choices
  ) {
    return(current)
  }
  unname(choices[1])
})

hla_pair_allele_ii <- reactive({
  choices <- hla_class_allele_choices("Class II")
  if (length(choices) == 0) {
    return(NULL)
  }
  picked <- input$hla_pair_allele_ii
  if (!is.null(picked) && nzchar(picked) && picked %in% choices) {
    return(picked)
  }
  unname(choices[1])
})

## The scope reuses the page's single allele rather than adding a picker: see
## hla_color_allele() — one allele must answer for the whole page, or the user
## scopes to one allele while reading another's numbers.
hla_scoped_segments <- reactive({
  seg <- hla_segments()
  mode <- hla_scope_mode()
  if (is.null(seg) || nrow(seg) == 0 || identical(mode, "all")) {
    return(seg)
  }
  ctx <- if ("mhc_context" %in% colnames(seg)) "mhc_context" else NULL

  if (identical(mode, "pair")) {
    a_i <- hla_pair_allele_i()
    a_ii <- hla_pair_allele_ii()
    if (is.null(a_i) || is.null(a_ii)) {
      return(seg)
    }
    out <- hla_scope_segments_by_allele_pair(
      seg,
      hla_active_typing(),
      allele_i = a_i,
      allele_ii = a_ii,
      context_col = ctx
    )
    # NULL means the pair is not analysable at all. Falling back to the whole
    # repertoire would answer a different question under the pair's label, so
    # return nothing and let hla_scope_status say why.
    return(out)
  }

  allele <- hla_color_allele()
  if (is.null(allele)) {
    return(seg)
  }
  out <- hla_scope_segments_by_allele(
    seg,
    hla_active_typing(),
    allele,
    context_col = ctx
  )
  if (is.null(out)) seg else out
})

## Cache key fragment: constant while unscoped, so changing the allele still
## re-colours the cached global graph instead of rebuilding it. Only in "allele"
## scope does the allele become a build parameter.
##
## The allele NAME is not enough to key on. In allele scope the graph is built
## from that allele's carriers, so the CARRIER SET is the real build parameter —
## and two different typings can name the same allele while disagreeing about
## who carries it. Without the carrier set in the key, uploading a second typing
## and keeping the allele selected would serve the graph cached from the old
## carriers while the colours and the Associations table came from the new
## typing: one screen, two cohorts, no error.
##
## Fingerprinted only in allele scope, and only the carrier set rather than the
## whole typing table: hla_scope_segments_by_allele reads typing through
## hla_carriers_of() and nowhere else, and in "all" scope typing never touches
## the build at all (colour is applied downstream at render). Keying wider than
## that would rebuild the Hamming graph on typing edits that cannot change it.
hla_scope_key <- reactive({
  mode <- hla_scope_mode()
  if (identical(mode, "all")) {
    return("all")
  }
  # Same rule for both scoped modes: name the alleles AND fingerprint whose
  # carriage they resolve to.
  fingerprint <- function(allele) {
    allele <- allele %||% ""
    carriers <- if (nzchar(allele)) {
      hla_carriers_of(hla_active_typing(), allele)
    } else {
      character(0)
    }
    paste0(allele, "|carriers:", paste(sort(carriers), collapse = ","))
  }
  if (identical(mode, "pair")) {
    return(paste0(
      "pair:",
      fingerprint(hla_pair_allele_i()),
      "|x|",
      fingerprint(hla_pair_allele_ii()),
      # The lineage decides which side each cell lands on, so it is a build
      # parameter too: the same two alleles over a different lineage column are
      # a different graph.
      "|lineage:",
      hla_celltype_col() %||% ""
    ))
  }
  paste0("allele:", fingerprint(hla_color_allele()))
})

## ---- Have the build parameters reported yet? --------------------------- ##
## Every build/display control on this page is created by output$hla_parameters_ui
## (its choices depend on the data set, so it cannot be static UI). That has a
## consequence worth stating: on the flush that first draws this page the inputs
## DO NOT EXIST, so hla_param() serves its fallbacks, and the browser reports the
## real values one flush later.
##
## Left ungated, the page therefore builds and draws the whole network twice on
## first open — once against the fallbacks, then again for real. Both passes
## agree today (the slider's `value` and the hla_param() fallback share one
## expression), so bindCache spares the second Hamming build and the visible cost
## is the network being torn down and re-stabilised the moment it appears. That
## agreement is a coincidence maintained by hand, though: change either default
## without the other and the first pass becomes a full wasted build of a graph
## nobody ever sees.
##
## So wait for the controls to report instead. The slider and the colour picker
## are created unconditionally, so either being non-NULL proves the panel
## rendered and reported. Checked with is.null(), NOT req(input$hla_color_by):
## that input's default value is "" (colour by motif cluster), which req() treats
## as missing — the network would then never draw until the user picked a
## colouring.
##
## The allele pickers need the same wait, one level deeper, and this is easy to
## miss because they are not on screen when the problem starts. They live in
## their own uiOutputs inside conditionalPanels, so Shiny suspends them until the
## panel appears — which is the very moment the user picks the scope that needs
## them. Selecting "One HLA allele" therefore ran the whole build against
## hla_color_allele()'s fallback, drew it, and redrew it ~130ms later when the
## picker finally reported. Measured: two widgets on one scope change, which is
## the flash.
##
## Guarded on hla_allele_choices() being non-empty, not just on the mode: a data
## set with no analysable allele renders that uiOutput as a bare "no alleles"
## message and creates no input at all, so requiring one would wait forever.
hla_params_ready <- reactive({
  if (is.null(input$hla_color_by) || is.null(input$hla_min_nodes)) {
    return(FALSE)
  }
  if (length(hla_allele_choices()) == 0) {
    return(TRUE)
  }
  mode <- hla_scope_mode()
  # The page's single allele is a build parameter in "allele" scope and a display
  # parameter under carrier colouring; either way the graph or its colours wait
  # on it.
  needs_allele <- identical(mode, "allele") ||
    identical(hla_param("hla_color_by", "cluster"), "hla_carrier")
  if (needs_allele && is.null(input$hla_color_allele)) {
    return(FALSE)
  }
  if (
    identical(mode, "pair") &&
      (is.null(input$hla_pair_allele_i) || is.null(input$hla_pair_allele_ii))
  ) {
    return(FALSE)
  }
  TRUE
})

## ---- One-way readiness latch ------------------------------------------ ##
## hla_params_ready() reads input$hla_color_by (to wait for it to register), so
## it invalidates on EVERY colour change even though its answer stays TRUE. The
## renderer must not re-run on a colour change, so it gates on this latch
## instead: it flips FALSE -> TRUE once params are ready and, being a
## reactiveVal, never notifies its readers again when set to the value it already
## holds. Structure changes reach the renderer through hla_motif_graph_cached(),
## not through here.
hla_ready_latch <- reactiveVal(FALSE)
observe({
  if (isTRUE(hla_params_ready())) {
    hla_ready_latch(TRUE)
  }
})

## ---- The motif graph (heavy; keyed on build parameters) --------------- ##
## Only build parameters (chain, min_nodes, split-by-V, show-isolated, scope)
## and the dataset re-trigger this; colour is applied downstream in the renderer.
hla_build_graph_raw_from <- function(seg) {
  if (!hla_has_deps() || is.null(seg) || nrow(seg) == 0) {
    return(NULL)
  }
  # In the pair scope the per-cell candidate allele IS the context, and it is
  # summarised by hla_pair_class_summary so a CDR3 on both sides reads as
  # "Both classes" rather than as whichever side had more cells. The plain MHC
  # context adds nothing there: the assignment already came from the lineage.
  if ("pair_allele" %in% colnames(seg)) {
    ctx_col <- "pair_allele"
    ctx_summary <- hla_pair_class_summary
  } else {
    ctx_col <- if ("mhc_context" %in% colnames(seg)) "mhc_context" else NULL
    ctx_summary <- hla_context_summary
  }
  # Builds the EXPENSIVE full graph only (distance matrix + core layout). The
  # min_nodes / show_isolated filter is applied later by hla_finalize_motif_graph
  # so that sweeping those never rebuilds this.
  hla_build_motif_graph_raw(
    seg,
    by_v = isTRUE(hla_param("hla_by_v", hla_by_v_default())),
    meta_cols = hla_node_meta_cols(),
    context_col = ctx_col,
    context_summary = ctx_summary
  )
}

## Debounce the minimum-size slider. Dragging it emits a stream of intermediate
## values; without this each one would trigger a finalize. Only the value the
## slider settles on recomputes. show_isolated is a checkbox (one event), so it
## needs no debounce.
hla_min_nodes_debounced <- shiny::debounce(
  reactive({
    as.integer(hla_param("hla_min_nodes", hla_default_min_nodes()))
  }),
  millis = 250
)

## The EXPENSIVE half, cached on the BUILD parameters alone (chain, by_v,
## metadata, scope, dataset) and NOT on min_nodes / show_isolated. Sweeping the
## threshold therefore never rebuilds the Hamming distance matrix or the layout:
## it reuses this cached full graph. The gate lives in an UNCACHED wrapper
## (hla_motif_graph below), never inside a bindCache body, because a req() stop
## there would be a value the cache stores under the current key and replays.
hla_motif_graph_raw_cached <- reactive({
  hla_build_graph_raw_from(hla_scoped_segments())
}) %>%
  hla_bindCache(
    hla_active_chain(),
    hla_param("hla_by_v", hla_by_v_default()),
    paste(hla_node_meta_cols(), collapse = ","),
    hla_scope_key(),
    available_crb_files$selected
  )

## The CHEAP half: filter the cached full graph by minimum size / isolated and
## relabel clusters. Cached on those two so returning to a value is instant; on a
## new value the raw build above is a cache hit and only this cheap step runs.
hla_motif_graph_cached <- reactive({
  hla_finalize_motif_graph(
    hla_motif_graph_raw_cached(),
    min_nodes = hla_min_nodes_debounced(),
    show_isolated = isTRUE(hla_param("hla_show_isolated", FALSE))
  )
}) %>%
  hla_bindCache(
    hla_active_chain(),
    hla_param("hla_by_v", hla_by_v_default()),
    hla_min_nodes_debounced(),
    hla_param("hla_show_isolated", FALSE),
    paste(hla_node_meta_cols(), collapse = ","),
    hla_scope_key(),
    available_crb_files$selected
  )

hla_motif_graph <- reactive({
  req(hla_params_ready())
  hla_motif_graph_cached()
})

## ---- The allele-INDEPENDENT graph (features for Associations) ---------- ##
## Associations compares carriers against non-carriers of an allele. If the
## motif it compares was itself discovered in a graph built from that allele's
## carriers, the allele picked the feature and is then asked whether it explains
## it — the carriers' side is guaranteed to look enriched, whatever the biology.
## The comparison must therefore only ever see motifs found WITHOUT reference to
## the allele, so this graph is always built from the unscoped segments.
##
## The scoped views stay available as exploratory VIEWS; they just cannot also
## be the thing that nominates the feature.
##
## Tested as "is the scope 'all'", NOT as "is the scope something other than
## 'allele'". Those read the same until a scope is added: the pair scope is just
## as allele-selected, and a not-equal test would have handed its graph straight
## back to Associations while looking untouched.
##
## Costs a second Hamming build in a scoped view only: when the scope is "all",
## the drawn graph is already allele-independent and is reused as-is.
## Same split as hla_motif_graph above, and for the same reason: the gate must
## stay outside the cache. Associations reads this, so without the gate the page
## would build the allele-independent graph on the fallback parameters too.
## The scope decision — build from UNSCOPED segments unless the scope is already
## "all" (where the drawn graph is allele-independent and is reused) — lives on
## the RAW build, so it is the raw graph that stays allele-independent. Cached on
## the build parameters alone, mirroring hla_motif_graph_raw_cached.
hla_global_motif_graph_raw_cached <- reactive({
  if (identical(hla_scope_mode(), "all")) {
    return(hla_motif_graph_raw_cached())
  }
  hla_build_graph_raw_from(hla_segments())
}) %>%
  hla_bindCache(
    hla_active_chain(),
    hla_param("hla_by_v", hla_by_v_default()),
    paste(hla_node_meta_cols(), collapse = ","),
    "all",
    available_crb_files$selected
  )

## Filtered to the same minimum size / isolated rule as the drawn graph.
hla_global_motif_graph_cached <- reactive({
  hla_finalize_motif_graph(
    hla_global_motif_graph_raw_cached(),
    min_nodes = hla_min_nodes_debounced(),
    show_isolated = isTRUE(hla_param("hla_show_isolated", FALSE))
  )
}) %>%
  hla_bindCache(
    hla_active_chain(),
    hla_param("hla_by_v", hla_by_v_default()),
    hla_min_nodes_debounced(),
    hla_param("hla_show_isolated", FALSE),
    paste(hla_node_meta_cols(), collapse = ","),
    "all",
    available_crb_files$selected
  )

hla_global_motif_graph <- reactive({
  req(hla_params_ready())
  hla_global_motif_graph_cached()
})

## ---- How many motif clusters the current view holds -------------------- ##
## Only used to explain the suppressed legend, so it reads the built graph
## rather than re-deriving anything.
hla_motif_n_clusters <- reactive({
  g <- hla_motif_graph()
  if (!hla_motif_graph_ok(g)) {
    return(0L)
  }
  cl <- tryCatch(igraph::vertex_attr(g, "cluster"), error = function(e) NULL)
  if (is.null(cl)) 0L else length(unique(cl))
})

## ---- Selection provenance of the receptor set -------------------------- ##
## A data set may have been assembled by SELECTING receptors on the very HLA
## association the page then displays (a positive control). The carrier/
## non-carrier contrast is then true but circular: it was put there by the
## selection, and re-computing overlap on it is not independent evidence.
##
## This cannot be inferred from the data, so the .crb must declare it in
## `technical_info$tcr_selection`. Recognised values:
##   "association-conditioned" -> receptors were chosen using a published HLA
##                                association; the sequences and genotypes are
##                                real but the contrast is circular
##   "synthetic"               -> receptors AND their HLA association were both
##                                fabricated. Strictly weaker than the above:
##                                there is no measurement anywhere underneath
##   "unselected"              -> receptors were not chosen using HLA
## `technical_info$tcr_selection_detail` carries the human-readable specifics.
## Anything else (or absent) is treated as unknown and stays silent, so this
## never invents a caveat for a data set that did not declare one.
## Each recognised value carries its OWN wording. "Positive control" and "a
## selected subset of receptors" are true of association-conditioned data (real
## sequences, real genotypes, circular selection) and false of a synthetic
## fixture, where nothing was selected because nothing was measured — so the
## headline and the subset phrase travel with the value instead of being
## hard-coded at the call site.
HLA_SELECTION_CAVEATS <- list(
  "association-conditioned" = list(
    headline = "Positive control - the contrast below is built in.",
    body = paste(
      "This data set's receptors were selected using a published HLA",
      "association, so a carrier/non-carrier difference here is expected by",
      "construction and is not independent evidence."
    ),
    subset_phrase = ", which holds a selected subset of receptors"
  ),
  "synthetic" = list(
    headline = "Fabricated fixture - nothing below is a measurement.",
    body = paste(
      "This data set's receptor sequences and their HLA association were both",
      "constructed, so every contrast shown here was put there on purpose. It",
      "demonstrates the page and is not evidence of anything."
    ),
    subset_phrase = ", whose receptors are fabricated"
  ),
  ## Weaker than "association-conditioned" and easy to mistake for clean: the
  ## receptors were NOT chosen using an HLA association, so the contrast is not
  ## circular -- but they were still chosen, by a reagent panel, and that is a
  ## different problem with the same direction of travel. Independent genotypes
  ## fix circularity; they do not fix which cells were captured, and a panel is
  ## itself HLA-biased (its reagents are restricted by particular alleles), so
  ## carriers of a well-covered allele contribute more receptors than carriers
  ## of one the panel barely probes. Saying "the genotypes are independent" and
  ## stopping there would leave that unsaid.
  ##
  ## This value used to be unrecognised, which meant the shipped demo declared a
  ## selection and the page showed NOTHING above the tables.
  "antigen-selected" = list(
    headline = "Selected repertoire - read the contrast as suggestive, not as a test.",
    body = paste(
      "The receptors here were captured by binding to a reagent panel, not",
      "sampled from the donors' repertoires. The HLA genotypes come from a",
      "published table and were measured independently of these cells, so a",
      "carrier contrast is not circular -- but which receptors exist to compare",
      "was still decided by the panel, which is itself restricted by particular",
      "alleles and applied to four donors. Ascertainment and donor/panel",
      "confounding remain."
    ),
    subset_phrase = ", which holds only receptors that bound a reagent in the panel"
  )
)

## Returns NULL, or a list(headline, body, subset_phrase). `body` may be
## overridden per data set via technical_info$tcr_selection_detail.
hla_selection_caveat <- reactive({
  ti <- tryCatch(data_set()$technical_info, error = function(e) NULL)
  if (!is.list(ti)) {
    return(NULL)
  }
  sel <- ti$tcr_selection
  if (
    !is.character(sel) ||
      length(sel) != 1L ||
      !(sel %in% names(HLA_SELECTION_CAVEATS))
  ) {
    return(NULL)
  }
  entry <- HLA_SELECTION_CAVEATS[[sel]]
  entry$body <- ti$tcr_selection_detail %||% entry$body
  entry
})

## ---- Stored HLA typing (canonical long table) ------------------------- ##
hla_stored_typing <- reactive({
  getHLATyping()
})

## ---- Session HLA override (from Data & QC upload) --------------------- ##
## Session-only; never written back to the .crb. NULL until the user uploads
## and activates a session source.
hla_session_typing <- reactiveVal(NULL)

## Drop the session override the moment the data set changes. HLA is matched to
## samples by exact name, so an override uploaded for one data set could
## silently re-attach to same-named samples in the next one and present another
## cohort's genotypes as this one's. Uploading again after switching is a small
## cost; showing the wrong donor's HLA is not recoverable by the reader.
observeEvent(available_crb_files$selected, ignoreInit = TRUE, {
  if (!is.null(hla_session_typing())) {
    hla_session_typing(NULL)
    showNotification(
      "Data set changed - the uploaded HLA typing was cleared.",
      type = "warning"
    )
  }
})

## ---- Active typing (session override wins over stored) ---------------- ##
hla_active_typing <- reactive({
  sess <- hla_session_typing()
  if (!is.null(sess) && is.data.frame(sess) && nrow(sess) > 0) {
    return(sess)
  }
  hla_stored_typing()
})

## ---- Does the active typing cover anything? --------------------------- ##
hla_has_typing <- reactive({
  t <- hla_active_typing()
  is.data.frame(t) && nrow(t) > 0
})

## ---- Is any allele actually usable as an analysis axis? ---------------- ##
## "The typing table has rows" is a much weaker fact than "this page can put an
## allele on screen", and the analysis controls need the stronger one. A table
## can be non-empty and still offer nothing: typed for another cohort (no sample
## matches the repertoire), or typed only at DQ/DP (valid, stored, but not an
## independently interpretable unit here — see hla_allele_choices).
##
## Gating the carrier colouring and the allele scope on the weaker fact left
## both controls on screen with nothing behind them: the picker said "no alleles
## available" and the scope quietly fell back to the whole network, which looks
## exactly like a scope that found everything.
hla_has_analyzable_allele <- reactive({
  length(hla_allele_choices()) > 0
})

## Why there is nothing to analyse, when there is typing but no usable allele.
## Hiding the controls without saying why just moves the confusion.
hla_no_allele_reason <- reactive({
  if (!hla_has_typing() || hla_has_analyzable_allele()) {
    return(NULL)
  }
  typing <- hla_active_typing()
  ir_samples <- names(getImmuneRepertoire())
  matched <- intersect(unique(as.character(typing$sample)), ir_samples)
  if (length(matched) == 0) {
    return(sprintf(
      paste(
        "None of the %d typed sample names match this data set's %d sample",
        "names. Matching is exact, never guessed."
      ),
      length(unique(typing$sample)),
      length(ir_samples)
    ))
  }
  in_scope <- typing[typing$sample %in% matched, , drop = FALSE]
  loci <- sort(unique(as.character(in_scope$locus)))
  if (!any(loci %in% HLA_MVP_LOCI)) {
    return(sprintf(
      paste(
        "The matched samples are typed only at %s. This page can interpret %s;",
        "DQ and DP are alpha/beta heterodimers and are not independently",
        "interpretable without pairing."
      ),
      paste(loci, collapse = ", "),
      paste(HLA_MVP_LOCI, collapse = ", ")
    ))
  }
  paste(
    "The matched, interpretable alleles are carried by every sample or by none,",
    "so no carrier contrast exists in this data set."
  )
})
