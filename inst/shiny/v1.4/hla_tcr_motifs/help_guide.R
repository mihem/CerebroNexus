## ---- Illustrated guide (panel-level info button) ----------------------- ##
## Content for the "info" modal on the HLA & TCR Motifs panel. Mirrors the
## Immune Repertoire guide (help_guide.R there): a left tab rail + right content
## pane, each tab carrying an annotated inline SVG schematic plus an element key.
##
## Tabs are NOT one-per-page-tab. Three of them explain the three page tabs; the
## other three explain the things this page is actually misread on:
##   * scope   — a per-allele view REBUILDS the graph; it does not re-colour it
##   * colour  — "Mixed" names two orthogonal axes on this page
##   * limits  — what the page cannot show, stated as its own tab
##
## SVG is inline (no external asset), matching the IR guide's self-contained rule.

hla_guide_svg_style <- paste(
  ".hg-lbl{font:600 12px system-ui,sans-serif;fill:#1c1c1e}",
  ".hg-sub{font:11px system-ui,sans-serif;fill:#6b6b70}",
  # .hg-ann: an annotation calling out a part of the figure.
  ".hg-ann{font:600 11px system-ui,sans-serif;fill:#c2410c}",
  ".hg-node{stroke:#333;stroke-width:1}",
  ".hg-edge{stroke:#bbb;stroke-width:1.2}",
  ".hg-arrow{stroke:#c2410c;stroke-width:1.4;fill:none;marker-end:url(#hgarrow)}",
  ".hg-cut{stroke:#c2410c;stroke-width:1.6;stroke-dasharray:4 3}",
  # Residues are drawn one <text> per character at a fixed pitch rather than as
  # one string, so the columns line up whatever font the browser resolves and a
  # substituted residue can be highlighted exactly in its column.
  ".hg-aa{font:600 13px ui-monospace,SFMono-Regular,Menlo,monospace;",
  "fill:#1c1c1e;text-anchor:middle}",
  ".hg-mm{font:700 13px ui-monospace,SFMono-Regular,Menlo,monospace;",
  "fill:#c2410c;text-anchor:middle}",
  ".hg-seqlbl{font:600 12px system-ui,sans-serif;fill:#6b6b70}",
  # For the single sentence this page is most often misread on. Louder than
  # .hg-ann on purpose: a table difference read as significance is THE failure
  # mode here.
  ".hg-warn{font:800 13px system-ui,sans-serif;fill:#b03030}",
  sep = ""
)

## One CDR3 rendered as positioned characters; `hl` are 1-based columns to mark.
hla_guide_seq <- function(seq, y, x0 = 118, dx = 15, hl = integer(0)) {
  chars <- strsplit(seq, "", fixed = TRUE)[[1]]
  bands <- if (length(hl) == 0) {
    ""
  } else {
    paste0(
      vapply(
        hl,
        function(i) {
          paste0(
            "<rect x='",
            x0 + (i - 1) * dx - 7,
            "' y='",
            y - 13,
            "' width='14' height='18' rx='3' fill='#fdeae0'/>"
          )
        },
        character(1)
      ),
      collapse = ""
    )
  }
  paste0(
    bands,
    paste0(
      vapply(
        seq_along(chars),
        function(i) {
          paste0(
            "<text class='",
            if (i %in% hl) "hg-mm" else "hg-aa",
            "' x='",
            x0 + (i - 1) * dx,
            "' y='",
            y,
            "'>",
            chars[i],
            "</text>"
          )
        },
        character(1)
      ),
      collapse = ""
    )
  )
}

hla_guide_svg_defs <- paste0(
  "<defs><marker id='hgarrow' markerWidth='8' markerHeight='8' refX='6' ",
  "refY='3' orient='auto'><path d='M0,0 L6,3 L0,6 Z' fill='#c2410c'/>",
  "</marker></defs>"
)

hla_guide_svg <- function(body, viewbox = "0 0 460 260") {
  HTML(paste0(
    "<svg viewBox='",
    viewbox,
    "' role='img' ",
    "style='width:100%;max-width:460px;height:auto;display:block;margin:0 auto' ",
    "xmlns='http://www.w3.org/2000/svg'><style>",
    hla_guide_svg_style,
    "</style>",
    hla_guide_svg_defs,
    body,
    "</svg>"
  ))
}

## Colours reused from the live network so the schematic and the plot agree.
## These MUST track HLA_CARRIER_COLORS / HLA_CONTEXT_COLORS in visualizations.R:
## an earlier version invented its own hues for the MHC-context axis and so
## taught colours the app never draws (it renders #636EFA/#EF553B/#00CC96 there).
## test-hla-app-contract.R pins them against the renderer.
HLA_GUIDE_CARRIER <- "#d6432f"
HLA_GUIDE_NONCARRIER <- "#3b6fb6"
HLA_GUIDE_MIXED <- "#b07aa1"
HLA_GUIDE_UNTYPED <- "#b8bcc4"
## Context axis: deliberately disjoint hues from the carrier axis, because the
## two axes are independent and look-alike colours imply a link that is not there.
HLA_GUIDE_CLASS_I <- "#e08214"
HLA_GUIDE_CLASS_II <- "#0f9b8e"
HLA_GUIDE_CTX_MIXED <- "#8a6d3b"
HLA_GUIDE_SHARED <- "#222222"
## Sample-origin hues: the renderer's first three from hla_distinct_colors().
## Unlike every other scale on this page these are ARBITRARY — which sample gets
## which is decided per data set — so they are named only to keep the schematic
## and the app in step, never because a hue means something.
HLA_GUIDE_SAMPLE <- c("#636EFA", "#EF553B", "#00CC96")
## A node drawn where colour is NOT the subject (the motif and association
## figures). Neutral on purpose: it must not read as a level of any scale.
HLA_GUIDE_NEUTRAL <- "#7b9fd4"

## ---- Schematic: a mismatch, at the sequence level --------------------- ##
## Circles cannot teach what "Hamming distance 1" means on a CDR3, so this shows
## three real-shaped TRB CDR3s of equal length. B is one substitution from A, C
## is one from B, and A-C therefore differ at two positions: an edge each for
## A-B and B-C, none between A and C, and all three in one motif. That is the
## transitivity the max-mismatch readout exists to disclose, and the same two
## varying columns are exactly what the consensus writes as "x".
hla_guide_svg_mismatch <- hla_guide_svg(
  paste0(
    "<text class='hg-lbl' x='14' y='20'>Three CDR3s, same length</text>",
    # column guides for the two varying positions
    "<line x1='237' y1='30' x2='237' y2='128' stroke='#f0cdb8' ",
    "stroke-dasharray='2 3'/>",
    "<line x1='282' y1='30' x2='282' y2='128' stroke='#f0cdb8' ",
    "stroke-dasharray='2 3'/>",
    # rows
    "<text class='hg-seqlbl' x='14' y='52'>A</text>",
    hla_guide_seq("CASSLGQAYEQYF", 52),
    "<text class='hg-seqlbl' x='14' y='84'>B</text>",
    hla_guide_seq("CASSLGQAYEQFF", 84, hl = 12),
    "<text class='hg-seqlbl' x='14' y='116'>C</text>",
    hla_guide_seq("CASSLGQAHEQFF", 116, hl = 9),
    # per-edge annotations
    "<text class='hg-ann' x='330' y='70'>1 substitution</text>",
    "<text class='hg-sub' x='330' y='84'>&#8594; edge A&#8211;B</text>",
    "<text class='hg-ann' x='330' y='102'>1 substitution</text>",
    "<text class='hg-sub' x='330' y='116'>&#8594; edge B&#8211;C</text>",
    # the transitive point
    "<text class='hg-ann' x='14' y='150'>A vs C differ at BOTH columns = 2 mismatches</text>",
    "<text class='hg-sub' x='14' y='166'>&#8594; no edge A&#8211;C, yet all three are ONE motif.</text>",
    "<text class='hg-sub' x='14' y='182'>&#8594; the tooltip reports max mismatch 2, so you can tell.</text>",
    # consensus
    "<line x1='14' y1='198' x2='446' y2='198' stroke='#ececec'/>",
    "<text class='hg-seqlbl' x='14' y='226'>consensus</text>",
    hla_guide_seq("CASSLGQAxEQxF", 226, hl = c(9, 12)),
    "<text class='hg-sub' x='330' y='226'>x = varies here</text>",
    # the length rule
    "<text class='hg-sub' x='14' y='252'>A CDR3 of a different length is never compared at all:</text>",
    "<text class='hg-sub' x='14' y='268'>Hamming distance is undefined between unequal lengths.</text>"
  ),
  viewbox = "0 0 460 280"
)

## ---- Schematic: what a motif is --------------------------------------- ##
## One hub + 1-mismatch variants, plus a chain out to a node at distance 3, so
## the transitive-membership point and the max-mismatch readout are both visible.
hla_guide_svg_network <- hla_guide_svg(paste0(
  # edges
  "<line class='hg-edge' x1='150' y1='120' x2='95' y2='75'/>",
  "<line class='hg-edge' x1='150' y1='120' x2='95' y2='165'/>",
  "<line class='hg-edge' x1='150' y1='120' x2='150' y2='55'/>",
  "<line class='hg-edge' x1='150' y1='120' x2='215' y2='120'/>",
  "<line class='hg-edge' x1='215' y1='120' x2='275' y2='90'/>",
  "<line class='hg-edge' x1='275' y1='90' x2='330' y2='120'/>",
  # hub (big = many cells) + leaves
  "<circle class='hg-node' cx='150' cy='120' r='16' fill='",
  HLA_GUIDE_NEUTRAL,
  "'/>",
  "<circle class='hg-node' cx='95' cy='75' r='6' fill='",
  HLA_GUIDE_NEUTRAL,
  "'/>",
  "<circle class='hg-node' cx='95' cy='165' r='6' fill='",
  HLA_GUIDE_NEUTRAL,
  "'/>",
  "<circle class='hg-node' cx='150' cy='55' r='6' fill='",
  HLA_GUIDE_NEUTRAL,
  "'/>",
  "<circle class='hg-node' cx='215' cy='120' r='8' fill='",
  HLA_GUIDE_NEUTRAL,
  "'/>",
  "<circle class='hg-node' cx='275' cy='90' r='7' fill='",
  HLA_GUIDE_NEUTRAL,
  "'/>",
  "<circle class='hg-node' cx='330' cy='120' r='7' fill='",
  HLA_GUIDE_NEUTRAL,
  "'/>",
  # Isolated node, filtered out. Parked top-right: at bottom-right it collided
  # with the "3 mismatches from the hub" annotation.
  "<circle class='hg-node' cx='418' cy='58' r='5' fill='#cfcfcf'/>",
  "<text class='hg-sub' x='372' y='80'>filtered out</text>",
  # annotations
  "<path class='hg-arrow' d='M150,190 L150,140'/>",
  "<text class='hg-ann' x='84' y='205'>AREA &#8733; cells</text>",
  "<text class='hg-sub' x='60' y='219'>(4x the area = 4x the cells)</text>",
  "<path class='hg-arrow' d='M182,42 L166,105'/>",
  "<text class='hg-ann' x='176' y='36'>edge = 1 mismatch</text>",
  "<path class='hg-arrow' d='M300,190 L322,133'/>",
  "<text class='hg-ann' x='250' y='205'>3 mismatches from the hub</text>",
  "<text class='hg-sub' x='250' y='220'>same motif; max mismatch says so</text>",
  "<text class='hg-lbl' x='20' y='24'>Node = one unique CDR3</text>"
))

## ---- Schematic: scope rebuilds, colour repaints ------------------------ ##
## This tab sits BEFORE "Colour" in the rail, so it cannot lean on that tab to
## say what a carrier is: it carries its own key, and names a concrete allele so
## "carries" has something to refer to. The key deliberately says "from a donor
## who carries", i.e. the DONOR's genotype — every node here comes from one
## donor. A CDR3 seen in several donors can be both at once, which is the
## "Mixed" the Colour tab covers.
hla_guide_svg_scope <- hla_guide_svg(
  paste0(
    "<text class='hg-lbl' x='14' y='18'>Scope decides which cells build the graph</text>",
    # Key: what the two colours mean, before either panel uses them.
    "<circle class='hg-node' cx='22' cy='40' r='7' fill='",
    HLA_GUIDE_CARRIER,
    "'/>",
    "<text class='hg-sub' x='36' y='44'>CDR3 from a donor who CARRIES HLA-A*02:01</text>",
    "<circle class='hg-node' cx='22' cy='62' r='7' fill='",
    HLA_GUIDE_NONCARRIER,
    "'/>",
    "<text class='hg-sub' x='36' y='66'>CDR3 from a donor who does NOT carry it</text>",
    "<line x1='14' y1='80' x2='446' y2='80' stroke='#ececec'/>",
    # divider between the two panels
    "<line x1='232' y1='92' x2='232' y2='288' stroke='#e2e2e2'/>",
    # ---- left: one graph over everything ----
    "<text class='hg-lbl' x='14' y='108'>All cells: one graph</text>",
    "<line class='hg-edge' x1='62' y1='138' x2='118' y2='164'/>",
    "<line class='hg-edge' x1='118' y1='164' x2='78' y2='204'/>",
    "<line class='hg-edge' x1='118' y1='164' x2='168' y2='200'/>",
    "<circle class='hg-node' cx='62' cy='138' r='8' fill='",
    HLA_GUIDE_CARRIER,
    "'/>",
    "<circle class='hg-node' cx='118' cy='164' r='8' fill='",
    HLA_GUIDE_CARRIER,
    "'/>",
    "<circle class='hg-node' cx='78' cy='204' r='8' fill='",
    HLA_GUIDE_NONCARRIER,
    "'/>",
    "<circle class='hg-node' cx='168' cy='200' r='8' fill='",
    HLA_GUIDE_NONCARRIER,
    "'/>",
    # Point at the midpoint of the red(118,164)-blue(78,204) edge.
    "<path class='hg-arrow' d='M62,252 L94,188'/>",
    "<text class='hg-ann' x='14' y='268'>this edge joins a carrier</text>",
    "<text class='hg-ann' x='14' y='282'>to a non-carrier</text>",
    # ---- right: rebuilt on the allele's carriers ----
    "<text class='hg-lbl' x='250' y='108'>Scoped to HLA-A*02:01</text>",
    "<line class='hg-edge' x1='292' y1='138' x2='348' y2='164'/>",
    "<circle class='hg-node' cx='292' cy='138' r='8' fill='",
    HLA_GUIDE_CARRIER,
    "'/>",
    "<circle class='hg-node' cx='348' cy='164' r='8' fill='",
    HLA_GUIDE_CARRIER,
    "'/>",
    # Cut line ABOVE the ghosts: what is kept stays above it, what the scope
    # removed sits below. With the line under them it read as if the ghosts had
    # survived and something else was cut.
    "<line class='hg-cut' x1='264' y1='186' x2='420' y2='186'/>",
    "<circle cx='308' cy='212' r='8' fill='none' stroke='#ccc' ",
    "stroke-dasharray='3 3'/>",
    "<circle cx='392' cy='208' r='8' fill='none' stroke='#ccc' ",
    "stroke-dasharray='3 3'/>",
    "<text class='hg-ann' x='250' y='244'>non-carriers gone,</text>",
    "<text class='hg-ann' x='250' y='258'>edges recomputed</text>",
    "<text class='hg-sub' x='250' y='278'>fewer nodes is expected, not a bug</text>"
  ),
  viewbox = "0 0 460 296"
)

## ---- Schematic: one worked node, read on both axes -------------------- ##
## The key below states what each level means, but a key cannot show HOW a node
## gets its label. This walks one CDR3's actual cells through both axes and
## lands on Carrier for one and Mixed for the other — the same node, two labels,
## which is the whole point of the tab. Verified against the real core:
## hla_node_carrier_status("donor_03,donor_07", ...) is "Carrier" and
## hla_context_summary(c("Class I","Class II","Class I")) is "Mixed".
hla_guide_svg_worked <- hla_guide_svg(
  paste0(
    "<text class='hg-lbl' x='14' y='18'>One CDR3, read on two different axes</text>",
    "<text class='hg-seqlbl' x='14' y='44'>node</text>",
    hla_guide_seq("CASSLGQAYEQYF", 44, x0 = 76, dx = 14),
    "<text class='hg-sub' x='268' y='44'>&#8212; found in 3 cells:</text>",
    # the three cells behind the node
    "<text class='hg-sub' x='34' y='72'>cell</text>",
    "<text class='hg-sub' x='96' y='72'>donor</text>",
    "<text class='hg-sub' x='188' y='72'>HLA-A*02:01</text>",
    "<text class='hg-sub' x='310' y='72'>lineage</text>",
    "<line x1='28' y1='79' x2='400' y2='79' stroke='#e2e2e2'/>",
    "<text class='hg-sub' x='34' y='98'>1</text>",
    "<text class='hg-sub' x='96' y='98'>donor_03</text>",
    "<text class='hg-sub' x='188' y='98'>carries</text>",
    "<text class='hg-sub' x='310' y='98'>CD8</text>",
    "<text class='hg-sub' x='34' y='118'>2</text>",
    "<text class='hg-sub' x='96' y='118'>donor_07</text>",
    "<text class='hg-sub' x='188' y='118'>carries</text>",
    "<text class='hg-sub' x='310' y='118'>CD4</text>",
    "<text class='hg-sub' x='34' y='138'>3</text>",
    "<text class='hg-sub' x='96' y='138'>donor_07</text>",
    "<text class='hg-sub' x='188' y='138'>carries</text>",
    "<text class='hg-sub' x='310' y='138'>CD8</text>",
    # two arrows down into two verdicts
    "<path class='hg-arrow' d='M110,152 L110,182'/>",
    "<path class='hg-arrow' d='M330,152 L330,182'/>",
    "<line x1='230' y1='166' x2='230' y2='286' stroke='#e2e2e2'/>",
    # left verdict: carrier axis
    "<text class='hg-lbl' x='14' y='202'>Carrier axis (HLA-A*02:01)</text>",
    "<text class='hg-sub' x='14' y='220'>looks at the DONORS: 03 and 07.</text>",
    "<text class='hg-sub' x='14' y='236'>Both carry it, none lacks it.</text>",
    "<circle class='hg-node' cx='24' cy='262' r='9' fill='",
    HLA_GUIDE_CARRIER,
    "'/>",
    "<text class='hg-ann' x='40' y='266'>&#8594; Carrier</text>",
    # right verdict: context axis
    "<text class='hg-lbl' x='250' y='202'>MHC context axis (lineage)</text>",
    "<text class='hg-sub' x='250' y='220'>looks at the CELLS: CD8, CD4, CD8.</text>",
    "<text class='hg-sub' x='250' y='236'>Class I and Class II both present.</text>",
    "<circle class='hg-node' cx='260' cy='262' r='9' fill='",
    HLA_GUIDE_MIXED,
    "'/>",
    "<text class='hg-ann' x='276' y='266'>&#8594; Mixed</text>",
    "<line x1='14' y1='282' x2='446' y2='282' stroke='#ececec'/>",
    "<text class='hg-ann' x='14' y='302'>The SAME node: Carrier on one axis, Mixed on the other.</text>"
  ),
  viewbox = "0 0 460 312"
)

## ---- Schematic: the two "Mixed" axes ---------------------------------- ##
## The single most confusable thing on the page, so it gets a side-by-side.
## The hues here are the renderer's actual scales (HLA_CARRIER_COLORS /
## HLA_CONTEXT_COLORS) and are disjoint on purpose: when both axes shared
## red/blue/purple, "red on the left" and "red on the right" invited a reader to
## connect a carrier to a CD8 cell, which is exactly the inference the tab exists
## to prevent. Only the no-information grey is shared, because that IS the same
## statement on both axes.
hla_guide_svg_colour <- hla_guide_svg(
  paste0(
    # left: carrier axis
    "<text class='hg-lbl' x='14' y='20'>Carrier status of ONE allele</text>",
    "<text class='hg-sub' x='14' y='36'>asks about the DONOR\'s genotype</text>",
    "<circle class='hg-node' cx='30' cy='62' r='9' fill='",
    HLA_GUIDE_CARRIER,
    "'/>",
    "<text class='hg-sub' x='48' y='66'>seen only in carriers</text>",
    "<circle class='hg-node' cx='30' cy='92' r='9' fill='",
    HLA_GUIDE_NONCARRIER,
    "'/>",
    "<text class='hg-sub' x='48' y='96'>seen only in non-carriers</text>",
    "<circle class='hg-node' cx='30' cy='122' r='9' fill='",
    HLA_GUIDE_MIXED,
    "'/>",
    "<text class='hg-sub' x='48' y='126'>seen in BOTH</text>",
    "<text class='hg-ann' x='48' y='140'>= &quot;Mixed&quot;</text>",
    "<circle class='hg-node' cx='30' cy='162' r='9' fill='",
    HLA_GUIDE_UNTYPED,
    "'/>",
    "<text class='hg-sub' x='48' y='166'>no carrying donor was typed</text>",
    "<text class='hg-sub' x='48' y='180'>= &quot;Untyped&quot;</text>",
    # divider
    "<line x1='232' y1='14' x2='232' y2='196' stroke='#e2e2e2'/>",
    # right: MHC context axis
    "<text class='hg-lbl' x='250' y='20'>MHC context (lineage)</text>",
    "<text class='hg-sub' x='250' y='36'>asks about the CELL\'s lineage</text>",
    "<circle class='hg-node' cx='266' cy='62' r='9' fill='",
    HLA_GUIDE_CLASS_I,
    "'/>",
    "<text class='hg-sub' x='284' y='66'>CD8 cells only &#8594; Class I</text>",
    "<circle class='hg-node' cx='266' cy='92' r='9' fill='",
    HLA_GUIDE_CLASS_II,
    "'/>",
    "<text class='hg-sub' x='284' y='96'>CD4 / Treg only &#8594; Class II</text>",
    "<circle class='hg-node' cx='266' cy='122' r='9' fill='",
    HLA_GUIDE_CTX_MIXED,
    "'/>",
    "<text class='hg-sub' x='284' y='126'>cells of BOTH lineages</text>",
    "<text class='hg-ann' x='284' y='140'>= &quot;Mixed&quot; too</text>",
    "<circle class='hg-node' cx='266' cy='162' r='9' fill='",
    HLA_GUIDE_UNTYPED,
    "'/>",
    "<text class='hg-sub' x='284' y='166'>lineage not CD4/CD8</text>",
    "<text class='hg-sub' x='284' y='180'>= &quot;Unknown&quot;</text>",
    # The crosstalk warning. Three short lines, not one long one: at 11px a
    # single line ran past the viewBox and was clipped mid-sentence.
    "<rect x='14' y='204' width='432' height='46' rx='5' fill='#fdeae0' ",
    "stroke='#f0cdb8'/>",
    "<text class='hg-ann' x='26' y='219'>The two colour scales are INDEPENDENT.</text>",
    "<text class='hg-sub' x='26' y='233'>A hue on the left has no relation to a hue on the right.</text>",
    "<text class='hg-sub' x='26' y='245'>Only the grey means the same thing on both.</text>",
    # the misconceptions, named
    "<text class='hg-ann' x='14' y='272'>&#10007; &quot;Mixed lineage&quot; does NOT mean some cells came from a carrier.</text>",
    "<text class='hg-ann' x='14' y='288'>&#10007; &quot;Mixed carrier status&quot; does NOT mean some cells are CD4.</text>",
    "<text class='hg-sub' x='14' y='306'>Same word. Orthogonal axes. Read the legend title.</text>"
  ),
  viewbox = "0 0 460 316"
)

## ---- Schematic: sample origin / Shared -------------------------------- ##
## The per-sample hues here are the renderer's own (hla_distinct_colors), not a
## prettier palette: an earlier version drew RColorBrewer Set2, which the app
## never uses. They are also the ONLY arbitrary scale on this page — which hue a
## sample gets depends on the data set — so the schematic says so outright. Black
## is the one fixed, meaningful colour on this axis, and that is the point.
hla_guide_svg_shared <- hla_guide_svg(
  paste0(
    "<text class='hg-lbl' x='14' y='18'>Sample origin: only BLACK has a fixed meaning</text>",
    # key: the arbitrary sample hues
    "<circle class='hg-node' cx='26' cy='44' r='7' fill='",
    HLA_GUIDE_SAMPLE[1],
    "'/>",
    "<circle class='hg-node' cx='48' cy='44' r='7' fill='",
    HLA_GUIDE_SAMPLE[2],
    "'/>",
    "<circle class='hg-node' cx='70' cy='44' r='7' fill='",
    HLA_GUIDE_SAMPLE[3],
    "'/>",
    "<text class='hg-sub' x='90' y='41'>one hue per sample, assigned per data set.</text>",
    "<text class='hg-sub' x='90' y='55'>Arbitrary: no order, no meaning, not comparable across data sets.</text>",
    # key: the one fixed level
    "<circle class='hg-node' cx='26' cy='84' r='9' fill='",
    HLA_GUIDE_SHARED,
    "'/>",
    "<text class='hg-ann' x='90' y='81'>the IDENTICAL CDR3 seen in &#8805;2 samples</text>",
    "<text class='hg-sub' x='90' y='95'>= &quot;Shared&quot; &#8212; a public clonotype</text>",
    "<line x1='14' y1='112' x2='446' y2='112' stroke='#ececec'/>",
    # the motif: a black hub among private, single-sample nodes
    "<line class='hg-edge' x1='150' y1='172' x2='96' y2='142'/>",
    "<line class='hg-edge' x1='150' y1='172' x2='212' y2='142'/>",
    "<line class='hg-edge' x1='150' y1='172' x2='150' y2='222'/>",
    "<line class='hg-edge' x1='212' y1='142' x2='262' y2='186'/>",
    "<circle class='hg-node' cx='96' cy='142' r='7' fill='",
    HLA_GUIDE_SAMPLE[1],
    "'/>",
    "<circle class='hg-node' cx='212' cy='142' r='7' fill='",
    HLA_GUIDE_SAMPLE[2],
    "'/>",
    "<circle class='hg-node' cx='150' cy='222' r='7' fill='",
    HLA_GUIDE_SAMPLE[3],
    "'/>",
    "<circle class='hg-node' cx='262' cy='186' r='7' fill='",
    HLA_GUIDE_SAMPLE[1],
    "'/>",
    "<circle class='hg-node' cx='150' cy='172' r='13' fill='",
    HLA_GUIDE_SHARED,
    "'/>",
    "<text class='hg-ann' x='300' y='140'>identical CDR3 found</text>",
    "<text class='hg-ann' x='300' y='154'>across &#8805;2 samples</text>",
    "<path class='hg-arrow' d='M296,164 L170,174'/>",
    "<text class='hg-sub' x='300' y='186'>bigger, too: it is in</text>",
    "<text class='hg-sub' x='300' y='200'>more cells (see area)</text>",
    "<line x1='14' y1='240' x2='446' y2='240' stroke='#ececec'/>",
    # what is ordinary vs what is not
    "<text class='hg-lbl' x='14' y='262'>Expected</text>",
    "<text class='hg-sub' x='84' y='262'>a family of DIFFERENT CDR3s from different donors.</text>",
    "<text class='hg-sub' x='84' y='276'>Convergent recombination makes near-neighbours independently.</text>",
    "<text class='hg-lbl' x='14' y='298'>Notable</text>",
    "<text class='hg-sub' x='84' y='298'>the IDENTICAL CDR3 recurring. That is what black marks.</text>"
  ),
  viewbox = "0 0 460 310"
)

## ---- Schematic: HLA Associations -------------------------------------- ##
## Two traps live in this one panel and both get drawn rather than described:
##   1. a big prevalence gap read as significance (there is no test at all);
##   2. "prevalence" and "fraction" read as the same word — they are different
##      numbers in different tables (between units vs within one unit).
## The numbers shown are arithmetically consistent: 11/15 = 73%, 3/15 = 20%.
hla_guide_svg_assoc <- hla_guide_svg(
  paste0(
    "<text class='hg-lbl' x='14' y='18'>Freeze one motif, then count units</text>",
    # the frozen feature
    "<line class='hg-edge' x1='62' y1='72' x2='104' y2='58'/>",
    "<line class='hg-edge' x1='62' y1='72' x2='104' y2='92'/>",
    "<circle class='hg-node' cx='62' cy='72' r='9' fill='",
    HLA_GUIDE_NEUTRAL,
    "'/>",
    "<circle class='hg-node' cx='104' cy='58' r='6' fill='",
    HLA_GUIDE_NEUTRAL,
    "'/>",
    "<circle class='hg-node' cx='104' cy='92' r='6' fill='",
    HLA_GUIDE_NEUTRAL,
    "'/>",
    "<rect x='40' y='40' width='88' height='66' fill='none' ",
    "stroke='#c2410c' stroke-dasharray='4 3' rx='4'/>",
    "<text class='hg-ann' x='40' y='124'>frozen motif</text>",
    "<path class='hg-arrow' d='M136,72 L178,72'/>",
    # the table it produces
    "<rect x='192' y='40' width='240' height='22' fill='#f4f4f5' stroke='#ddd'/>",
    "<text class='hg-sub' x='198' y='55'>HLA status</text>",
    "<text class='hg-sub' x='288' y='55'>units</text>",
    "<text class='hg-sub' x='330' y='55'>with it</text>",
    "<text class='hg-sub' x='386' y='55'>prevalence</text>",
    "<rect x='192' y='62' width='240' height='22' fill='#fff' stroke='#ddd'/>",
    "<text class='hg-sub' x='198' y='77'>carrier</text>",
    "<text class='hg-sub' x='288' y='77'>15</text>",
    "<text class='hg-sub' x='330' y='77'>11</text>",
    "<text class='hg-sub' x='386' y='77'>73%</text>",
    "<rect x='192' y='84' width='240' height='22' fill='#fff' stroke='#ddd'/>",
    "<text class='hg-sub' x='198' y='99'>non-carrier</text>",
    "<text class='hg-sub' x='288' y='99'>15</text>",
    "<text class='hg-sub' x='330' y='99'>3</text>",
    "<text class='hg-sub' x='386' y='99'>20%</text>",
    "<rect x='192' y='106' width='240' height='22' fill='#fff' stroke='#ddd'/>",
    "<text class='hg-sub' x='198' y='121'>untyped</text>",
    "<text class='hg-sub' x='288' y='121'>0</text>",
    "<text class='hg-sub' x='330' y='121'>&#8212;</text>",
    "<text class='hg-sub' x='386' y='121'>&#8212;</text>",
    # THE trap
    "<rect x='14' y='142' width='432' height='48' rx='5' fill='#fdeae0' ",
    "stroke='#e0a58a'/>",
    "<text class='hg-warn' x='26' y='162'>73% vs 20% is a difference, NOT a p-value.</text>",
    "<text class='hg-sub' x='26' y='178'>No hypothesis test is run. No odds ratio. Nothing is corrected for multiplicity.</text>",
    "<line x1='14' y1='206' x2='446' y2='206' stroke='#ececec'/>",
    # the vocabulary trap
    "<text class='hg-lbl' x='14' y='228'>Two words, two different numbers</text>",
    "<text class='hg-ann' x='14' y='250'>prevalence</text>",
    "<text class='hg-sub' x='96' y='250'>ACROSS units: how many donors carry the motif at all.</text>",
    "<text class='hg-sub' x='96' y='264'>The table above. 11 of 15 = 73%.</text>",
    "<text class='hg-ann' x='14' y='286'>fraction</text>",
    "<text class='hg-sub' x='96' y='286'>WITHIN one unit: how much of that donor\'s repertoire</text>",
    "<text class='hg-sub' x='96' y='300'>IN THIS DATA SET the motif is. A different table.</text>"
  ),
  viewbox = "0 0 460 312"
)

## ---- Schematic: Data & QC --------------------------------------------- ##
## Three things get drawn because they are the ones that bite: an upload is
## session-only, resolution is preserved rather than padded (so the carrier call
## is field-wise), and source type is declared rather than sniffed.
hla_guide_svg_qc <- hla_guide_svg(
  paste0(
    "<text class='hg-lbl' x='14' y='18'>Two ways in, one canonical table</text>",
    "<rect x='14' y='34' width='128' height='36' fill='#f4f4f5' stroke='#ddd' ",
    "rx='6'/>",
    "<text class='hg-sub' x='24' y='50'>stored in the .crb</text>",
    "<text class='hg-sub' x='24' y='64'>addHLATyping()</text>",
    "<rect x='14' y='82' width='128' height='36' fill='#fff8ec' stroke='#e0a58a' ",
    "rx='6'/>",
    "<text class='hg-sub' x='24' y='98'>uploaded this session</text>",
    "<text class='hg-ann' x='24' y='112'>SESSION-ONLY</text>",
    "<path class='hg-arrow' d='M148,54 L188,66'/>",
    "<path class='hg-arrow' d='M148,100 L188,80'/>",
    "<rect x='196' y='46' width='128' height='56' fill='#fff' stroke='#c2410c' ",
    "rx='6'/>",
    "<text class='hg-sub' x='206' y='64'>normalize</text>",
    "<text class='hg-sub' x='206' y='80'>NNNN / NA &#8594; missing</text>",
    "<text class='hg-sub' x='206' y='96'>resolution kept as-is</text>",
    "<path class='hg-arrow' d='M330,74 L372,74'/>",
    "<text class='hg-sub' x='378' y='66'>sample</text>",
    "<text class='hg-sub' x='378' y='80'>&#8594; donor</text>",
    "<text class='hg-sub' x='378' y='94'>(optional)</text>",
    # the session-only trap
    "<rect x='14' y='128' width='432' height='30' rx='5' fill='#fdeae0' ",
    "stroke='#e0a58a'/>",
    "<text class='hg-warn' x='26' y='146'>An upload NEVER writes back to the .crb.</text>",
    "<text class='hg-sub' x='26' y='156'>It is dropped the moment the data set changes.</text>",
    # resolution: the reason the carrier call is field-wise
    "<line x1='14' y1='172' x2='446' y2='172' stroke='#ececec'/>",
    "<text class='hg-lbl' x='14' y='192'>Resolution is preserved, never padded</text>",
    "<text class='hg-sub' x='14' y='210'>HLA-A*02 stays HLA-A*02. It is never expanded to HLA-A*02:00.</text>",
    "<text class='hg-sub' x='14' y='226'>So carrier calls compare FIELD BY FIELD, not string to string:</text>",
    "<text class='hg-ann' x='26' y='244'>typed A*02:01, asked about A*02</text>",
    "<text class='hg-sub' x='250' y='244'>&#8594; carrier (it refines it)</text>",
    "<text class='hg-ann' x='26' y='260'>typed A*02, asked about A*02:01</text>",
    "<text class='hg-sub' x='250' y='260'>&#8594; untyped (cannot tell)</text>",
    "<text class='hg-ann' x='26' y='276'>typed A*03:01, asked about A*02:01</text>",
    "<text class='hg-sub' x='250' y='276'>&#8594; non-carrier</text>",
    "<text class='hg-warn' x='14' y='300'>An untyped donor is NEVER a non-carrier.</text>"
  ),
  viewbox = "0 0 460 312"
)

## ---- Schematic: the ceiling ------------------------------------------- ##
## The whole page rests on this: a carrier match narrows the restricting element
## to a SET, never to the allele you picked. Drawn with the donor's actual six
## class I alleles so the size of that set is visible rather than asserted.
hla_guide_svg_limits <- hla_guide_svg(
  paste0(
    "<text class='hg-lbl' x='14' y='20'>What a carrier match does and does not pin down</text>",
    "<rect x='14' y='36' width='176' height='110' fill='#fafafa' stroke='#ddd' ",
    "rx='6'/>",
    "<text class='hg-sub' x='26' y='56'>One donor carries, at class I:</text>",
    "<text class='hg-aa' x='60' y='76' style='text-anchor:start'>A*02:01</text>",
    "<text class='hg-aa' x='130' y='76' style='text-anchor:start'>A*01:01</text>",
    "<text class='hg-aa' x='60' y='96' style='text-anchor:start'>B*07:02</text>",
    "<text class='hg-aa' x='130' y='96' style='text-anchor:start'>B*44:02</text>",
    "<text class='hg-aa' x='60' y='116' style='text-anchor:start'>C*07:01</text>",
    "<text class='hg-aa' x='130' y='116' style='text-anchor:start'>C*07:02</text>",
    "<text class='hg-warn' x='26' y='138'>six alleles, all class I</text>",
    "<path class='hg-arrow' d='M198,90 L240,90'/>",
    "<circle class='hg-node' cx='276' cy='90' r='14' fill='",
    HLA_GUIDE_CARRIER,
    "'/>",
    "<text class='hg-sub' x='300' y='78'>this CD8 cell\'s TCR is</text>",
    "<text class='hg-sub' x='300' y='92'>restricted by ONE of them</text>",
    "<text class='hg-warn' x='300' y='110'>&#8212; unknown which</text>",
    "<line x1='14' y1='160' x2='446' y2='160' stroke='#ececec'/>",
    "<text class='hg-ann' x='14' y='182'>Scoping to A*02:01 keeps ALL six alleles\' receptors.</text>",
    "<text class='hg-sub' x='14' y='198'>The scope narrows the DONORS, never the restricting element.</text>",
    # class II is worse, not better
    "<text class='hg-lbl' x='14' y='224'>Class II is looser still</text>",
    "<text class='hg-sub' x='14' y='242'>DQ and DP are heterodimers whose alpha and beta chains pair in</text>",
    "<text class='hg-sub' x='14' y='256'>CIS and in TRANS, so a heterozygote assembles more distinct class II</text>",
    "<text class='hg-sub' x='14' y='270'>molecules than the alleles it carries. A CD4 cell narrows even less.</text>",
    "<text class='hg-warn' x='14' y='296'>Candidate co-occurrence. Never confirmed restriction.</text>"
  ),
  viewbox = "0 0 460 308"
)

## ---- Prose helpers ---------------------------------------------------- ##
## The tab bodies used to be one pre-line string each, so a whole topic arrived
## as an undifferentiated 13px wall and ALL-CAPS was standing in for headings.
## These give the prose the same three levels the schematics already use:
## a heading, body text, and one loud callout reserved for the sentence the tab
## exists to prevent being misread.

## Section heading inside a tab body.
hla_guide_h <- function(txt) {
  tags$p(
    style = paste0(
      "font-weight:700;font-size:12px;letter-spacing:.02em;",
      "margin:16px 0 5px;color:#1c1c1e;"
    ),
    txt
  )
}

## Body paragraph. 1.55 line-height: these are dense sentences in a modal.
hla_guide_p <- function(...) {
  tags$p(
    style = "font-size:13px;line-height:1.55;margin:0 0 8px;color:#33333a;",
    ...
  )
}

## The one thing on a tab that must not be skimmed. Same amber as the modal's
## top banner and the schematics' warning bands, so "amber" means one thing.
hla_guide_warn <- function(...) {
  tags$div(
    class = "alert alert-warning",
    style = "font-size:13px;line-height:1.5;padding:9px 11px;margin:10px 0;",
    ...
  )
}

## A literal the user will see in the app: an allele, a column, a field value.
hla_guide_code <- function(txt) {
  tags$code(
    style = paste0(
      "font-size:12px;background:#f4f4f5;border:1px solid #ececec;",
      "border-radius:3px;padding:1px 4px;color:#1c1c1e;"
    ),
    txt
  )
}

## An inline term being defined.
hla_guide_term <- function(txt) {
  tags$b(style = "color:#1c1c1e;", txt)
}

## ---- Per-tab element keys --------------------------------------------- ##
hla_guide_li <- function(...) {
  tags$ul(class = "hg-elements", lapply(list(...), tags$li))
}

HLA_GUIDE_TABS <- c(
  "Motif Network",
  "Network scope",
  "Colour",
  "Sample origin",
  "HLA Associations",
  "Data & QC",
  "Limits"
)

hla_guide_content <- list(
  "Motif Network" = list(
    # Two figures: the rule at the sequence level, then what the picture adds on
    # top of it. One combined diagram would have to teach both at once.
    svg = tagList(
      tags$p(
        style = "font-weight:600;font-size:12px;margin:0 0 6px 2px;",
        "1 — What counts as an edge"
      ),
      hla_guide_svg_mismatch,
      tags$p(
        style = "font-weight:600;font-size:12px;margin:16px 0 6px 2px;",
        "2 — What the picture adds"
      ),
      hla_guide_svg_network
    ),
    summary = "The Hamming-1 CDR3 network: which receptors are one substitution apart.",
    detail = tagList(
      hla_guide_h("What a node is"),
      hla_guide_p(
        "One unique CDR3 amino-acid string — or one ",
        hla_guide_code("V gene + CDR3"),
        " pair when \"Split motifs by V gene\" is on. A node is not one cell: it",
        "stands for every cell carrying that CDR3."
      ),
      hla_guide_h("What an edge is"),
      hla_guide_p(
        "Two ",
        hla_guide_term("equal-length"),
        " CDR3s at Hamming distance",
        " exactly 1 — they differ at a single position. A ",
        hla_guide_term("motif"),
        " is a connected component of those edges."
      ),
      hla_guide_h("Membership is transitive"),
      hla_guide_p(
        "If A–B and B–C are each 1 mismatch, A and C sit in the same motif even",
        "though they differ at 2 positions. The tooltip reports the component's ",
        hla_guide_term("max mismatch"),
        " so that is never hidden."
      ),
      hla_guide_warn(
        tags$b("Insertions and deletions are invisible here. "),
        "CDR3s of different lengths are never compared, because Hamming distance",
        "is undefined between them. This network sees substitutions only."
      )
    ),
    elements = hla_guide_li(
      tagList(tags$b("Node"), " - one unique CDR3."),
      tagList(
        tags$b("Node area"),
        " - proportional to the number of analysis units carrying it, so twice",
        "the area means twice the cells. It is the AREA that carries the number,",
        "not the width: radius grows as the square root of the count. Very large",
        "clones hit a size cap and stop growing - the tooltip always gives the",
        "exact count."
      ),
      tagList(tags$b("Edge"), " - exactly 1 substitution, equal length only."),
      tagList(
        tags$b("Max mismatch"),
        " - the largest Hamming distance inside the motif: a max mismatch of 5",
        "means its two most different members differ at 5 positions. It is",
        "deliberately not called a diameter - on a network that word means the",
        "longest shortest-path in hops, which is a different, larger number."
      ),
      tagList(
        tags$b("Minimum motif size"),
        " - components smaller than this are hidden. Unconnected CDR3s stay",
        "hidden unless \"Show unconnected CDR3s\" is ticked."
      )
    )
  ),
  "Network scope" = list(
    svg = hla_guide_svg_scope,
    summary = "Scope changes the graph. Colour only changes the paint.",
    detail = tagList(
      hla_guide_h("First, the word \"carrier\""),
      hla_guide_p(
        "Everyone inherits two alleles at each HLA locus, so for a given allele",
        " — say ",
        hla_guide_code("HLA-A*02:01"),
        " — a donor either has a copy",
        " or does not. A ",
        hla_guide_term("carrier"),
        " has at least one. A ",
        hla_guide_term("non-carrier"),
        " was typed at that locus and has none."
      ),
      hla_guide_p(
        "\"Typed\" is load-bearing. A donor nobody typed is ",
        hla_guide_term("untyped"),
        ": absence of evidence, not evidence of absence, and never counted as a",
        "non-carrier. Carrying says nothing about dose — one copy and two copies",
        "are both carriers. And status is always relative to ",
        hla_guide_term("one"),
        " allele: the same cell is a carrier cell for A*02:01 and a non-carrier",
        "cell for any allele its donor lacks."
      ),
      hla_guide_h("\"All cells\" — one graph, repainted"),
      hla_guide_p(
        "Built from every cell; picking an allele only re-colours it. That graph",
        "contains edges joining a carrier's CDR3 to a non-carrier's, because it",
        "was never built with the allele in mind."
      ),
      hla_guide_h("\"One HLA allele\" — a different graph"),
      hla_guide_p(
        "Rebuilt on the cells that could bear on that allele: donors who carry",
        "it, and only cells whose lineage matches the allele's MHC class (a class",
        "II allele cannot restrict a CD8 cell's receptor). Distances are",
        "recomputed inside the subset, so no edge crosses into a non-carrier.",
        tags$b(" Expect fewer nodes — that is the point, not a fault.")
      ),
      hla_guide_p(
        "The status line under the picker states how many units survived, so a",
        "small network is never left ambiguous between \"rare allele\", \"class",
        "filter\" and \"lineage unknown\"."
      ),
      hla_guide_warn(
        tags$b("An allele scope has no comparison group. "),
        "Inside it every donor is a carrier, so recurrence across donors cannot",
        "be told apart from an ordinary public TCR. Use \"All cells\" with",
        "carrier colouring when you need that contrast."
      )
    ),
    elements = hla_guide_li(
      tagList(
        tags$b("Carrier / non-carrier"),
        " - of ONE named allele: the donor has at least one copy of it, or was",
        "typed at that locus and has none. Untyped is neither. The schematic",
        "colours a CDR3 by the donor it came from; a CDR3 seen in several donors",
        "can be both at once, which is the \"Mixed\" on the Colour tab."
      ),
      tagList(
        tags$b("All cells"),
        " - one cached graph; changing allele repaints it instantly."
      ),
      tagList(
        tags$b("One HLA allele"),
        " - carriers x matching lineage; the distance matrix is rebuilt, so it",
        "takes a moment."
      ),
      tagList(
        tags$b("Cells with Unknown lineage"),
        " - dropped by the class filter, never assumed into a class."
      ),
      tagList(
        tags$b("Bulk data"),
        " - has no lineage, so an allele scope is carriers-only and is labelled",
        "as NOT class-matched."
      )
    )
  ),
  "Colour" = list(
    svg = tagList(
      tags$p(
        style = "font-weight:600;font-size:12px;margin:0 0 6px 2px;",
        "1 — How one node gets its colour"
      ),
      hla_guide_svg_worked,
      tags$p(
        style = "font-weight:600;font-size:12px;margin:16px 0 6px 2px;",
        "2 — What each level means"
      ),
      hla_guide_svg_colour
    ),
    summary = "\"Mixed\" names two different things. Check the legend title.",
    detail = tagList(
      hla_guide_h("A node's colour is a summary, not a cell's property"),
      hla_guide_p(
        "A node is one unique CDR3, not one cell. Its colour summarises ",
        tags$b("every"),
        " cell carrying that CDR3 — often several cells, from several donors. So",
        " \"Mixed\" is always a statement about a set."
      ),
      hla_guide_h("Axis 1 — HLA carrier status"),
      hla_guide_p(
        "Of the typed donors this CDR3 was seen in, do they all carry the chosen",
        "allele? ",
        hla_guide_term("Carrier"),
        " = at least one carrier and no typed non-carrier. ",
        hla_guide_term("Non-carrier"),
        " = the reverse. ",
        hla_guide_term("Mixed"),
        " = both."
      ),
      hla_guide_h("Axis 2 — MHC context"),
      hla_guide_p(
        "Something unrelated: which lineage were the cells? CD8 gives Class I,",
        "CD4 and Treg give Class II, anything else stays Unknown. ",
        hla_guide_term("Mixed"),
        " here means the CDR3 was found in BOTH compartments."
      ),
      hla_guide_warn(
        tags$b("The two axes are orthogonal. "),
        "A node can be Carrier on one and Mixed on the other at the same time —",
        "the worked example above is exactly that. Their colour scales share no",
        "hue on purpose: nothing connects a colour on one axis to a colour on",
        "the other."
      ),
      hla_guide_h("Why one donor has the same CDR3 in a CD4 and a CD8 cell"),
      hla_guide_p(
        "Because the node key is the CDR3 (or V gene + CDR3), which is ",
        tags$b("coarser than the receptor"),
        ": a T cell's specificity comes from a paired alpha and beta chain, and",
        "two cells with different alpha chains can carry the identical beta",
        "CDR3. Short, germline-proximal junctions also recur by convergent",
        "recombination within one person. A Mixed-lineage node is ordinary, and",
        "it is not evidence that a clone changed lineage."
      ),
      hla_guide_h("One asymmetry worth knowing"),
      hla_guide_p(
        "Carrier status can only read \"Mixed\" for a CDR3 seen in ≥2 donors.",
        "Where most CDR3s are private to one donor, most nodes simply inherit",
        "that donor's status, and the background sits near the allele's carrier",
        "frequency by construction."
      )
    ),
    elements = hla_guide_li(
      tagList(
        tags$b("Carrier"),
        " - no evidence against, NOT \"every donor carries it\". The tooltip",
        "gives the carrier / non-carrier / untyped counts; read those, because a",
        "colour cannot show whether a call rests on one donor or ten."
      ),
      tagList(
        tags$b("Untyped"),
        " - no carrying sample was typed at that locus, or the typing is too",
        "coarse to answer. Absence of typing, not absence of the allele."
      ),
      tagList(
        tags$b("Motif cluster"),
        " - an arbitrary palette to separate components; the numbers carry no",
        "order or meaning."
      )
    )
  ),
  "Sample origin" = list(
    svg = hla_guide_svg_shared,
    summary = "Black marks the IDENTICAL CDR3 turning up in ≥2 samples.",
    detail = tagList(
      hla_guide_h("What the colours do"),
      hla_guide_p(
        "A node keeps its sample's hue when it was seen in exactly one sample,",
        "and turns black (",
        hla_guide_term("Shared"),
        ") when the identical CDR3 was seen in ≥2."
      ),
      hla_guide_warn(
        tags$b("The hues themselves mean nothing. "),
        "They are handed out per data set, so the same sample can be blue here",
        "and red in the next object, and a hue never compares across data sets.",
        "Black is the only level on this axis with a fixed meaning — which is",
        "why it is black rather than one more colour."
      ),
      hla_guide_h("Not the same as colouring by sample"),
      hla_guide_p(
        "The plain ",
        hla_guide_code("sample"),
        " column shows a node's MOST COMMON sample, which paints a CDR3 seen in",
        "three samples as if it were private to one — hiding the very recurrence",
        "you came for."
      ),
      hla_guide_h("Biological context"),
      hla_guide_p(
        "A motif family made of ",
        tags$b("different"),
        " sequences from different donors is expected: convergent recombination",
        "produces near-neighbours independently in unrelated people. The notable",
        "case is the ",
        tags$b("identical"),
        " sequence recurring — a ",
        hla_guide_term("public clonotype"),
        "."
      ),
      hla_guide_p(
        "Observing a public clonotype is not, on its own, evidence of an HLA",
        "association. ",
        tags$i(
          "That is not the same as saying public clonotypes are unrelated to HLA"
        ),
        " — many are, and finding them is the point of a screen. It means",
        "sharing has an HLA-independent explanation available: short junctions",
        "and germline-proximal rearrangements have a high generation",
        "probability, so unrelated donors produce them repeatedly whatever their",
        "genotype. Cross-donor sharing is where a screen ",
        tags$b("starts"),
        "; the carrier contrast is what could take it further."
      )
    ),
    elements = hla_guide_li(
      tagList(
        tags$b("Coloured dot"),
        " - a CDR3 private to that one sample. The hue is arbitrary."
      ),
      tagList(
        tags$b("Black dot"),
        " - the identical CDR3 in ≥2 samples: a public clonotype."
      ),
      tagList(
        tags$b("Node area"),
        " - shared nodes tend to look bigger because they sit in more cells; that",
        "is the area encoding, not a second meaning of black."
      ),
      tagList(
        tags$b("Legend"),
        " - one entry per sample, so it is unreadable past a handful. This",
        "colouring suits cohorts of a few samples, not dozens."
      )
    )
  ),
  "HLA Associations" = list(
    svg = hla_guide_svg_assoc,
    summary = "Descriptive overlap for one frozen motif or node. No test is run.",
    detail = tagList(
      hla_guide_h("What it does"),
      hla_guide_p(
        "Pick one motif component or one CDR3 node from the graph the Motif",
        "Network tab has already built, then read how its presence splits across",
        "carriers and non-carriers of one allele."
      ),
      hla_guide_h("Counting units"),
      hla_guide_p(
        "A ",
        hla_guide_term("unit"),
        " is a donor when donor mapping is complete, and a sample otherwise; the",
        "tab states which is active. Two samples from one donor are aggregated,",
        "or that donor would count twice and a cohort of ten people could",
        "present itself as twenty."
      ),
      hla_guide_warn(
        tags$b("No statistics, on purpose. "),
        "No p-value, no odds ratio, no multiplicity correction. A gap in the",
        "prevalence column is an observation, not a result: the motif and the",
        "allele were both chosen ",
        tags$i("after"),
        " looking at the network, so any test computed here would be a",
        "post-selection test over an unstated number of comparisons. Export the",
        "tables and test a prespecified hypothesis on donor-level data instead."
      ),
      hla_guide_h("Two words that are not synonyms"),
      hla_guide_p(
        hla_guide_term("Prevalence"),
        " is measured ACROSS units: of the 15 carriers, how many carry this",
        "motif at all. The per-unit table's ",
        hla_guide_term("fractions"),
        " are measured WITHIN one unit: of that donor's clonotypes in this data",
        "set, how many are this motif. A motif can be present in every carrier",
        "(prevalence 100%) while being a vanishing fraction of each one's",
        "repertoire."
      ),
      hla_guide_h("And the denominator"),
      hla_guide_p(
        "Those fractions are over what ",
        tags$b("this data set"),
        " holds per unit, never over the donor's real repertoire. A subsetted",
        "object or a shallowly sequenced sample changes them without anything",
        "biological changing, so they do not compare across cohorts."
      ),
      hla_guide_h("Circularity"),
      hla_guide_p(
        "When a data set declares that its receptors were selected using an HLA",
        "association — or constructed outright — this tab says so first and in",
        "the stronger style. That declaration is the only way to know: ",
        tags$b(
          "a circular contrast looks exactly like a real one in the numbers"
        ),
        ", and no amount of reading the table can reveal it."
      )
    ),
    elements = hla_guide_li(
      tagList(
        tags$b("Analysis unit"),
        " - a donor when donor mapping is complete, a sample otherwise. The tab",
        "labels the active one."
      ),
      tagList(
        tags$b("Prevalence"),
        " - ACROSS units: the proportion of units of that HLA status in which the",
        "frozen feature appears at all."
      ),
      tagList(
        tags$b("Per-unit fraction"),
        " - WITHIN one unit: how much of that unit's content in THIS data set the",
        "feature accounts for. Not the same number as prevalence, and not",
        "comparable across cohorts."
      ),
      tagList(
        tags$b("Unit x allele matrix"),
        " - 1 carrier, 0 locus-typed non-carrier, blank locus untyped. Blank is",
        "absence of typing, not absence of the allele."
      ),
      tagList(
        tags$b("Before quoting any gap"),
        " - check the typing coverage, the source type, and whether a selection",
        "caveat is showing. A gap on selected receptors is a consequence of the",
        "selection."
      )
    )
  ),
  "Data & QC" = list(
    svg = hla_guide_svg_qc,
    summary = "Where the typing came from, and what it cost to normalize.",
    detail = tagList(
      hla_guide_h("Two ways in"),
      hla_guide_p(
        "Typing is either stored in the ",
        hla_guide_code(".crb"),
        " at build time via ",
        hla_guide_code("addHLATyping()"),
        ", or uploaded here as a CSV/TSV."
      ),
      hla_guide_warn(
        tags$b("An upload is session-only. "),
        "It never writes back to the ",
        hla_guide_code(".crb"),
        " and is dropped the moment the data set changes. Nothing you do on this",
        "tab edits the file."
      ),
      hla_guide_h("Accepted shapes"),
      hla_guide_p(
        "A long table with ",
        hla_guide_code("sample / locus / allele"),
        " columns, or a wide one with a ",
        hla_guide_code("sample"),
        " column plus ",
        hla_guide_code("HLA-*_1 / HLA-*_2"),
        " columns. Donor mapping is ",
        tags$b("optional"),
        ": without it, counting simply stays at sample level and the page says so."
      ),
      hla_guide_h("Resolution is kept, not padded"),
      hla_guide_p(
        "Alleles normalize to ",
        hla_guide_code("HLA-<locus>*<fields>"),
        ", and whatever resolution the lab reported survives: ",
        hla_guide_code("HLA-A*02"),
        " stays ",
        hla_guide_code("HLA-A*02"),
        " and is never expanded to ",
        hla_guide_code("HLA-A*02:00"),
        ". ",
        hla_guide_code("NNNN"),
        ", blanks and ",
        hla_guide_code("NA"),
        " become missing."
      ),
      hla_guide_p(
        "That decides who lands in your comparison group, so carrier calls",
        "compare ",
        tags$b("field by field"),
        ", not string to string:"
      ),
      tags$ul(
        class = "hg-elements",
        tags$li(
          "typed ",
          hla_guide_code("A*02:01"),
          ", asked about ",
          hla_guide_code("A*02"),
          " → ",
          tags$b("carrier"),
          " (the typing refines the question)."
        ),
        tags$li(
          "typed ",
          hla_guide_code("A*02"),
          ", asked about ",
          hla_guide_code("A*02:01"),
          " → ",
          tags$b("untyped"),
          " — A*02:01 is an A*02, so this donor may well have it. Calling them a",
          "non-carrier would quietly bias the contrast."
        ),
        tags$li(
          "typed ",
          hla_guide_code("A*03:01"),
          ", asked about ",
          hla_guide_code("A*02:01"),
          " → ",
          tags$b("non-carrier"),
          "."
        )
      ),
      hla_guide_h("Source type is declared, never sniffed"),
      hla_guide_p(
        hla_guide_code("genotyped"),
        ", ",
        hla_guide_code("imputed"),
        ", ",
        hla_guide_code("synthetic"),
        " or ",
        hla_guide_code("unknown"),
        ". Anything but genotyped is surfaced on every tab, because once a",
        "genotype is a string in a table an imputed or fabricated one looks",
        "exactly like a measured one — and they do not carry the same weight."
      ),
      hla_guide_warn(
        tags$b("Coverage is the number to read first. "),
        "An allele contrast resting on two typed donors is not a contrast."
      )
    ),
    elements = hla_guide_li(
      tagList(
        tags$b("Coverage"),
        " - typed samples over immune-repertoire samples. Read it before",
        "anything else."
      ),
      tagList(
        tags$b("Untyped is not non-carrier"),
        " - untyped units are excluded from carrier calls, never counted as",
        "lacking the allele. That applies to a locus nobody typed AND to typing",
        "too coarse to answer the question asked."
      ),
      tagList(
        tags$b("Normalization preview"),
        " - what each raw token became. Anything unrecognisable is reported as a",
        "QC warning rather than dropped in silence."
      ),
      tagList(
        tags$b("Donor mapping"),
        " - optional. It lifts counting from sample level to donor level;",
        "incomplete mapping keeps it at sample level and says so."
      ),
      tagList(
        tags$b("Source type"),
        " - genotyped / imputed / synthetic / unknown, declared by whoever built",
        "the data. The page cannot verify it, only repeat it."
      )
    )
  ),
  "Limits" = list(
    svg = hla_guide_svg_limits,
    summary = "The ceiling on everything this page can tell you.",
    detail = tagList(
      hla_guide_warn(
        tags$b("This page shows candidate co-occurrence, not restriction. "),
        "It reports that a motif and an allele a donor happens to carry turn up",
        "together. That is not the same claim, and no setting on this page",
        "closes the gap."
      ),
      hla_guide_h("Why, structurally"),
      hla_guide_p(
        "A donor carries up to ",
        tags$b("six"),
        " class I alleles: two each at HLA-A, -B and -C. Knowing a CD8 cell came",
        "from an ",
        hla_guide_code("A*02:01"),
        " carrier narrows its restricting element to one of those six — it does",
        "not identify A*02:01. Scoping the network to A*02:01 keeps every one of",
        "that donor's class-I-restricted receptors, including those restricted",
        "by the other five. ",
        tags$b("The scope narrows the donors, never the restricting element.")
      ),
      hla_guide_h("Class II is looser, not tighter"),
      hla_guide_p(
        "DR is the tractable one: DRA is essentially monomorphic, so DRB1 alone",
        "names the molecule. DQ and DP are not — they are heterodimers whose",
        "alpha and beta chains pair both ",
        tags$i("in cis"),
        " and ",
        tags$i("in trans"),
        ", so a heterozygote assembles more distinct class II molecules than the",
        "alleles it carries, and \"carrier of ",
        hla_guide_code("DQB1*02:01"),
        "\" names a donor set rather than a molecule. A CD4 cell therefore",
        "narrows even less than a CD8 one."
      ),
      hla_guide_h("What the lineage map does and does not do"),
      hla_guide_p(
        "CD8 to class I and CD4/Treg to class II is solid immunology. It",
        "constrains the ",
        tags$b("class"),
        ". It says nothing about ",
        tags$b("which allele"),
        " within that class."
      ),
      hla_guide_h("What would establish restriction"),
      hla_guide_p(
        "Orthogonal experimental evidence: peptide-HLA multimer staining, a",
        "functional assay against the specific allele, or single-allele",
        "presenting lines. Nothing computed on this page substitutes for a",
        "wet-lab measurement, and ",
        tags$b(
          "no amount of extra cohort makes co-occurrence into restriction"
        ),
        " — it is a type error, not a power problem."
      ),
      hla_guide_h("Why there are no statistics"),
      hla_guide_p(
        "Deliberate. A p-value over motifs × alleles chosen after looking at",
        "the network is a post-selection test over an unstated number of",
        "comparisons — a multiplicity problem, not a result. Export the tables",
        "and test a prespecified hypothesis on donor-level data instead."
      )
    ),
    elements = hla_guide_li(
      tagList(
        tags$b("Can show"),
        " - which motifs exist, who carries which alleles, and how the two",
        "co-occur."
      ),
      tagList(
        tags$b("Cannot show"),
        " - that an allele restricts a receptor; that a difference is",
        "statistically significant; that a motif is antigen-specific."
      ),
      tagList(
        tags$b("Check before quoting anything"),
        " - typing coverage; source type (genotyped / imputed / synthetic); any",
        "declared selection caveat; and how many donors the call actually rests",
        "on."
      ),
      tagList(
        tags$b("To go further"),
        " - export the tables, then test a prespecified hypothesis on",
        "donor-level data, and confirm restriction with multimer or functional",
        "evidence."
      )
    )
  )
)

hla_guide_tab_content <- function(tab) {
  spec <- hla_guide_content[[tab]]
  if (is.null(spec)) {
    return(NULL)
  }
  tagList(
    if (!is.null(spec$svg)) {
      div(
        style = paste0(
          "background:#fafafa;border:1px solid #ececec;border-radius:8px;",
          "padding:12px;margin-bottom:14px;"
        ),
        spec$svg
      )
    },
    if (!is.null(spec$summary)) {
      tags$p(style = "font-weight:600;font-size:14px;", spec$summary)
    },
    # `detail` is structured markup (headings / paragraphs / callouts), not a
    # pre-line string, so it must NOT be wrapped in white-space:pre-line.
    if (!is.null(spec$detail)) {
      div(style = "margin-bottom:12px;", spec$detail)
    },
    if (!is.null(spec$elements)) {
      tagList(
        tags$hr(),
        tags$p(
          style = "font-weight:600;font-size:13px;margin-bottom:4px;",
          "How to read it"
        ),
        spec$elements
      )
    }
  )
}

## ---- Panel-level info button ------------------------------------------ ##
observeEvent(input$hla_visualizations_info, {
  panels <- lapply(HLA_GUIDE_TABS, function(tab) {
    tabPanel(tab, div(class = "hla-guide-pane", hla_guide_tab_content(tab)))
  })
  showModal(modalDialog(
    title = "HLA & TCR Motifs — guide",
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    tags$style(HTML(paste0(
      ".hla-guide .nav-tabs{float:left;border-bottom:none;border-right:1px solid ",
      "#ececec;width:150px;margin-right:16px;}",
      ".hla-guide .nav-tabs>li{float:none;margin:0;}",
      ".hla-guide .nav-tabs>li>a{border:none;border-radius:6px;",
      "margin:2px 6px 2px 0;color:#1c1c1e;font-size:13px;padding:6px 10px;}",
      ".hla-guide .nav-tabs>li.active>a{background:#f0f4ff;color:#2f6fd6;",
      "font-weight:600;}",
      ".hla-guide .tab-content{overflow:auto;}",
      ".hla-guide .hg-elements{font-size:13px;padding-left:18px;margin:0;}",
      ".hla-guide .hg-elements li{margin-bottom:5px;}",
      ".hla-guide:after{content:'';display:table;clear:both;}"
    ))),
    div(
      class = "hla-guide",
      style = "min-height:360px;",
      # The boundary is the one thing a reader must not skim past, and buried in
      # a grey paragraph it was skimmable. It gets the alert style the page uses
      # for its other non-negotiables.
      tags$div(
        class = "alert alert-warning",
        style = "font-size:13px;margin-bottom:12px;",
        tags$b("This page reports co-occurrence, not restriction. "),
        "It shows how TCR motifs and the HLA alleles donors happen to carry",
        "turn up together. That cannot establish that an allele restricts a",
        "receptor — see the \"Limits\" topic for why, and for what would."
      ),
      tags$p(
        style = "margin-bottom:12px;font-size:13px;color:#6b6b70;",
        "Pick a topic on the left."
      ),
      do.call(tabsetPanel, c(list(id = "hla_guide_tabs"), panels))
    )
  ))
})
