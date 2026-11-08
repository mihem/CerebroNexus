## ---- Illustrated per-tab guide (panel-level info button) --------------- ##
## Content for the "info" modal on the Immune Repertoire panel. Each visible tab
## gets an annotated static schematic (inline SVG, so it is self-contained and
## can point arrows at the axes / blocks / legend) plus an "elements" block that
## spells out what the axes, block height, colour and legend mean for THAT plot.
## The prose reuses ir_tab_help[[tab]]$detail; this file adds the schematic and
## the element key. Built as a left tab list + right content pane.

## Small helper: a labelled arrow annotation is just text; the SVGs below embed
## their own arrows. Keeping the schematic markup inline (not files) avoids any
## external asset and keeps the strict-CSP-safe, self-contained rule.

## Shared SVG bits ------------------------------------------------------------
ir_guide_svg_style <- paste(
  ".ig-lbl{font:600 12px system-ui,sans-serif;fill:#1c1c1e}",
  ".ig-sub{font:11px system-ui,sans-serif;fill:#6b6b70}",
  ".ig-ann{font:600 11px system-ui,sans-serif;fill:#c2410c}",
  ".ig-axis{stroke:#333;stroke-width:1.4}",
  ".ig-arrow{stroke:#c2410c;stroke-width:1.4;fill:none;marker-end:url(#igarrow)}",
  sep = ""
)

ir_guide_svg_defs <- paste0(
  "<defs><marker id='igarrow' markerWidth='8' markerHeight='8' refX='6' ",
  "refY='3' orient='auto'><path d='M0,0 L6,3 L0,6 Z' fill='#c2410c'/>",
  "</marker></defs>"
)

## Wrap raw SVG body into a responsive, self-contained figure.
ir_guide_svg <- function(body, viewbox = "0 0 460 260") {
  HTML(paste0(
    "<svg viewBox='",
    viewbox,
    "' role='img' ",
    "style='width:100%;max-width:460px;height:auto;display:block;margin:0 auto' ",
    "xmlns='http://www.w3.org/2000/svg'><style>",
    ir_guide_svg_style,
    "</style>",
    ir_guide_svg_defs,
    body,
    "</svg>"
  ))
}

## Compare schematic: two stacked bars + one ribbon, with arrows calling out the
## block height, the ribbon, the Y axis and the legend.
ir_guide_svg_compare <- ir_guide_svg(paste0(
  # Y axis
  "<line class='ig-axis' x1='60' y1='20' x2='60' y2='210'/>",
  "<text class='ig-sub' x='18' y='120' transform='rotate(-90 18,120)'>Proportion</text>",
  # bar 1 (sample_1) stacked segments
  "<rect x='90' y='70' width='70' height='45' fill='#d94a7a' stroke='#333'/>",
  "<rect x='90' y='115' width='70' height='35' fill='#1f9e89' stroke='#333'/>",
  "<rect x='90' y='150' width='70' height='60' fill='#6a9a1f' stroke='#333'/>",
  # bar 2 (sample_2)
  "<rect x='300' y='40' width='70' height='55' fill='#b05fd0' stroke='#333'/>",
  "<rect x='300' y='95' width='70' height='30' fill='#1f9e89' stroke='#333'/>",
  "<rect x='300' y='125' width='70' height='85' fill='#c98a1f' stroke='#333'/>",
  # ribbon linking the teal clone between bars
  "<path d='M160,115 C220,120 240,90 300,95 L300,125 C240,120 220,150 160,150 Z' ",
  "fill='#1f9e89' opacity='0.45'/>",
  # x labels
  "<text class='ig-sub' x='108' y='226'>sample_1</text>",
  "<text class='ig-sub' x='308' y='226'>sample_2</text>",
  # annotations
  "<path class='ig-arrow' d='M235,60 L150,95'/>",
  "<text class='ig-ann' x='236' y='56'>block height = size</text>",
  "<path class='ig-arrow' d='M245,175 L235,120'/>",
  "<text class='ig-ann' x='210' y='190'>ribbon = same clone</text>",
  # legend swatch
  "<rect x='392' y='60' width='14' height='10' fill='#d94a7a' stroke='#333'/>",
  "<text class='ig-sub' x='410' y='69'>clone (0.017)</text>",
  "<path class='ig-arrow' d='M420,50 L405,60'/>",
  "<text class='ig-ann' x='398' y='46'>legend + total</text>"
))

## Clonal UMAP: a cell projection with a few points highlighted (expanded
## clones) over a grey background of the rest.
ir_guide_svg_clonalumap <- ir_guide_svg(paste0(
  "<text class='ig-sub' x='190' y='20'>cell projection (UMAP / tSNE)</text>",
  # grey background cells
  paste0(
    vapply(
      list(
        c(120, 90),
        c(150, 130),
        c(100, 160),
        c(180, 100),
        c(210, 150),
        c(160, 190),
        c(240, 120),
        c(130, 200),
        c(270, 170),
        c(200, 80)
      ),
      function(p) {
        sprintf("<circle cx='%d' cy='%d' r='5' fill='#cfcfcf'/>", p[1], p[2])
      },
      character(1)
    ),
    collapse = ""
  ),
  # expanded-clone cells in a blue ramp (bigger = more expanded)
  "<circle cx='190' cy='150' r='6' fill='#8bb8de'/>",
  "<circle cx='210' cy='170' r='6' fill='#5e9bc7'/>",
  "<circle cx='175' cy='175' r='6' fill='#2f6fd6'/>",
  "<circle cx='195' cy='190' r='6' fill='#1d4ea0'/>",
  "<path class='ig-arrow' d='M320,150 L215,175'/>",
  "<text class='ig-ann' x='322' y='150'>colour = expansion</text>",
  "<path class='ig-arrow' d='M300,110 L250,120'/>",
  "<text class='ig-ann' x='250' y='95'>grey = other cells</text>"
))

## Abundance: a rank-abundance curve (steep drop-off).
ir_guide_svg_abundance <- ir_guide_svg(paste0(
  "<line class='ig-axis' x1='60' y1='20' x2='60' y2='210'/>",
  "<line class='ig-axis' x1='60' y1='210' x2='420' y2='210'/>",
  "<text class='ig-sub' x='18' y='130' transform='rotate(-90 18,130)'>cells / clone</text>",
  "<text class='ig-sub' x='210' y='232'>clone rank (1 = most common)</text>",
  "<path d='M70,40 C120,150 200,195 420,205' stroke='#2f6fd6' stroke-width='2.5' fill='none'/>",
  "<path class='ig-arrow' d='M180,70 L95,80'/>",
  "<text class='ig-ann' x='150' y='60'>steep = a few dominant clones</text>",
  "<path class='ig-arrow' d='M300,150 L360,200'/>",
  "<text class='ig-ann' x='250' y='145'>flat tail = many rare clones</text>"
))

## Diversity: box/points per group, higher = more diverse.
ir_guide_svg_diversity <- ir_guide_svg(paste0(
  "<line class='ig-axis' x1='60' y1='20' x2='60' y2='210'/>",
  "<line class='ig-axis' x1='60' y1='210' x2='420' y2='210'/>",
  "<text class='ig-sub' x='18' y='130' transform='rotate(-90 18,130)'>diversity index</text>",
  # group A box (high)
  "<rect x='120' y='60' width='60' height='45' fill='#1f9e89' opacity='0.5' stroke='#333'/>",
  "<line class='ig-axis' x1='150' y1='45' x2='150' y2='120'/>",
  # group B box (low)
  "<rect x='280' y='120' width='60' height='40' fill='#c98a1f' opacity='0.5' stroke='#333'/>",
  "<line class='ig-axis' x1='310' y1='105' x2='310' y2='175'/>",
  "<text class='ig-sub' x='132' y='226'>group A</text>",
  "<text class='ig-sub' x='292' y='226'>group B</text>",
  "<path class='ig-arrow' d='M215,70 L182,80'/>",
  "<text class='ig-ann' x='210' y='60'>higher = more diverse</text>"
))

## Homeostasis: two stacked bars split into clone-size classes.
ir_guide_svg_homeostasis <- ir_guide_svg(paste0(
  "<line class='ig-axis' x1='60' y1='20' x2='60' y2='210'/>",
  "<text class='ig-sub' x='18' y='140' transform='rotate(-90 18,140)'>fraction of repertoire</text>",
  # bar A
  "<rect x='110' y='40' width='70' height='40' fill='#1d4ea0' stroke='#333'/>",
  "<rect x='110' y='80' width='70' height='50' fill='#2f6fd6' stroke='#333'/>",
  "<rect x='110' y='130' width='70' height='80' fill='#8bb8de' stroke='#333'/>",
  # bar B
  "<rect x='300' y='90' width='70' height='30' fill='#1d4ea0' stroke='#333'/>",
  "<rect x='300' y='120' width='70' height='40' fill='#2f6fd6' stroke='#333'/>",
  "<rect x='300' y='160' width='70' height='50' fill='#8bb8de' stroke='#333'/>",
  "<text class='ig-sub' x='128' y='226'>group A</text>",
  "<text class='ig-sub' x='318' y='226'>group B</text>",
  "<path class='ig-arrow' d='M240,55 L182,60'/>",
  "<text class='ig-ann' x='236' y='52'>top = hyperexpanded</text>",
  "<path class='ig-arrow' d='M240,180 L182,175'/>",
  "<text class='ig-ann' x='236' y='196'>bottom = rare clones</text>"
))

## Isotype: stacked composition bars (IgM/IgG/IgA...).
ir_guide_svg_isotype <- ir_guide_svg(paste0(
  "<line class='ig-axis' x1='60' y1='20' x2='60' y2='210'/>",
  "<text class='ig-sub' x='18' y='140' transform='rotate(-90 18,140)'>isotype fraction</text>",
  "<rect x='110' y='40' width='70' height='90' fill='#d94a7a' stroke='#333'/>",
  "<rect x='110' y='130' width='70' height='80' fill='#6a9a1f' stroke='#333'/>",
  "<rect x='300' y='40' width='70' height='40' fill='#d94a7a' stroke='#333'/>",
  "<rect x='300' y='80' width='70' height='130' fill='#6a9a1f' stroke='#333'/>",
  "<text class='ig-sub' x='120' y='226'>timepoint 1</text>",
  "<text class='ig-sub' x='310' y='226'>timepoint 2</text>",
  "<rect x='392' y='60' width='14' height='10' fill='#d94a7a' stroke='#333'/>",
  "<text class='ig-sub' x='410' y='69'>IgM</text>",
  "<rect x='392' y='80' width='14' height='10' fill='#6a9a1f' stroke='#333'/>",
  "<text class='ig-sub' x='410' y='89'>IgG</text>",
  "<path class='ig-arrow' d='M250,120 L305,140'/>",
  "<text class='ig-ann' x='200' y='115'>class-switch: IgM → IgG</text>"
))

## Paired Scatter: clone proportions in group A (x) vs group B (y).
ir_guide_svg_pairedscatter <- ir_guide_svg(paste0(
  "<line class='ig-axis' x1='70' y1='20' x2='70' y2='210'/>",
  "<line class='ig-axis' x1='70' y1='210' x2='420' y2='210'/>",
  "<text class='ig-sub' x='30' y='120' transform='rotate(-90 30,120)'>group B</text>",
  "<text class='ig-sub' x='220' y='232'>group A</text>",
  "<line x1='70' y1='210' x2='320' y2='40' stroke='#cfcfcf' stroke-dasharray='4 3'/>",
  # points
  "<circle cx='150' cy='150' r='5' fill='#2f6fd6'/>",
  "<circle cx='200' cy='120' r='5' fill='#2f6fd6'/>",
  "<circle cx='120' cy='90' r='5' fill='#d94a7a'/>",
  "<circle cx='260' cy='170' r='5' fill='#c98a1f'/>",
  "<path class='ig-arrow' d='M300,70 L260,55'/>",
  "<text class='ig-ann' x='250' y='60'>on the line = equal in both</text>",
  "<path class='ig-arrow' d='M170,60 L128,88'/>",
  "<text class='ig-ann' x='150' y='50'>above = higher in B</text>"
))

## Clone Sharing: a small matrix of private / public cells.
ir_guide_svg_clonesharing <- ir_guide_svg(paste0(
  "<text class='ig-sub' x='150' y='24'>clonotype sharing across units</text>",
  # grid
  paste0(
    vapply(
      0:2,
      function(r) {
        paste0(
          vapply(
            0:3,
            function(c) {
              shared <- (r == 0 && c < 2) || (r == 1 && c == 2)
              fill <- if (shared) "#2f6fd6" else "#e6e6e6"
              sprintf(
                "<rect x='%d' y='%d' width='34' height='34' fill='%s' stroke='#fff' stroke-width='2'/>",
                120 + c * 38,
                60 + r * 38,
                fill
              )
            },
            character(1)
          ),
          collapse = ""
        )
      },
      character(1)
    ),
    collapse = ""
  ),
  "<path class='ig-arrow' d='M330,80 L200,74'/>",
  "<text class='ig-ann' x='300' y='76'>filled = present</text>",
  "<path class='ig-arrow' d='M330,180 L235,150'/>",
  "<text class='ig-ann' x='250' y='185'>one unit = private</text>"
))

## SizeDist: a fitted clone-size distribution curve.
ir_guide_svg_sizedist <- ir_guide_svg(paste0(
  "<line class='ig-axis' x1='60' y1='20' x2='60' y2='210'/>",
  "<line class='ig-axis' x1='60' y1='210' x2='420' y2='210'/>",
  "<text class='ig-sub' x='18' y='130' transform='rotate(-90 18,130)'>number of clones</text>",
  "<text class='ig-sub' x='230' y='232'>clone size</text>",
  # bars
  paste0(
    vapply(
      1:8,
      function(i) {
        h <- c(150, 110, 80, 60, 45, 34, 26, 20)[i]
        sprintf(
          "<rect x='%d' y='%d' width='26' height='%d' fill='#8bb8de' stroke='#333'/>",
          75 + (i - 1) * 34,
          210 - h,
          h
        )
      },
      character(1)
    ),
    collapse = ""
  ),
  # fitted curve
  "<path d='M88,60 C150,120 250,185 380,200' stroke='#c2410c' stroke-width='2' fill='none'/>",
  "<path class='ig-arrow' d='M300,90 L250,150'/>",
  "<text class='ig-ann' x='250' y='85'>fitted size distribution</text>"
))

## Elements key per tab: named list of "what each visual channel means". Only
## the currently-shown tabs need entries; others fall back to the prose only.
ir_guide_elements <- list(
  "Clonal UMAP" = tags$ul(
    class = "ig-elements",
    tags$li(
      tags$b("Position: "),
      "the same cell projection (UMAP/tSNE) as the other tabs — each dot is one cell."
    ),
    tags$li(
      tags$b("Colour: "),
      "clone-expansion level of that cell; deeper blue = more expanded clone. ",
      "Grey dots are cells without the selected receptor."
    ),
    tags$li(
      tags$b("What to look for: "),
      "clusters of deep-blue cells mark where expanded clones sit."
    )
  ),
  "Abundance" = tags$ul(
    class = "ig-elements",
    tags$li(tags$b("X axis: "), "clonotype rank — 1 is the most common clone."),
    tags$li(tags$b("Y axis: "), "how many cells carry that clone."),
    tags$li(
      tags$b("Curve shape: "),
      "a steep drop = a few clones dominate; a flat tail = many equally-rare clones."
    )
  ),
  "Diversity" = tags$ul(
    class = "ig-elements",
    tags$li(tags$b("X axis: "), "each group / sample."),
    tags$li(
      tags$b("Y axis: "),
      "a diversity index (Shannon, inverse-Simpson, …). Higher = more even, ",
      "more diverse repertoire; lower = dominated by a few clones."
    ),
    tags$li(
      tags$b("Box / points: "),
      "the bootstrap spread of the index for that group."
    )
  ),
  "Homeostasis" = tags$ul(
    class = "ig-elements",
    tags$li(tags$b("X axis: "), "each group / sample."),
    tags$li(
      tags$b("Block height: "),
      "the fraction of the repertoire taken up by each clone-size class ",
      "(rare → hyperexpanded); every bar sums to 1."
    ),
    tags$li(
      tags$b("Colour: "),
      "the clone-size class; the top band is the most-expanded clones, the ",
      "bottom the rarest."
    )
  ),
  "Isotype" = tags$ul(
    class = "ig-elements",
    tags$li(
      tags$b("X axis: "),
      "each sample / timepoint (BCR only — TCR has no isotype)."
    ),
    tags$li(
      tags$b("Block height: "),
      "the fraction of B cells of each antibody isotype; every bar sums to 1."
    ),
    tags$li(
      tags$b("Colour / legend: "),
      "the isotype (IgM, IgD, IgG, IgA, IgE). A shift from IgM to IgG/IgA ",
      "between timepoints indicates class-switching."
    )
  ),
  "Paired Scatter" = tags$ul(
    class = "ig-elements",
    tags$li(tags$b("X axis: "), "a clone's proportion in the first group."),
    tags$li(
      tags$b("Y axis: "),
      "the same clone's proportion in the second group."
    ),
    tags$li(
      tags$b("Diagonal: "),
      "clones on the dashed line are equally frequent in both; above it = ",
      "higher in the second group, below = higher in the first."
    ),
    tags$li(
      tags$b("Facets: "),
      "with a Pair-by column the plot splits into one panel per facet level."
    )
  ),
  "Clone Sharing" = tags$ul(
    class = "ig-elements",
    tags$li(
      tags$b("Cells: "),
      "each clonotype against each sharing unit; filled = the clone is present there."
    ),
    tags$li(
      tags$b("Classes: "),
      "private (one unit only), public within a group, or public across groups."
    ),
    tags$li(
      tags$b("Group by: "),
      "sets what counts as within-group vs. cross-group sharing."
    )
  ),
  "SizeDist" = tags$ul(
    class = "ig-elements",
    tags$li(tags$b("X axis: "), "clone size (number of cells per clone)."),
    tags$li(tags$b("Y axis: "), "how many clones have that size."),
    tags$li(
      tags$b("Curve: "),
      "a fitted size distribution; a long heavy tail means a few very large clones."
    )
  ),
  "Compare" = tags$ul(
    class = "ig-elements",
    tags$li(tags$b("X axis: "), "the groups being compared (e.g. samples)."),
    tags$li(
      tags$b("Y axis / block height: "),
      "a clonotype's size — its proportion of the repertoire (Proportion on) ",
      "or its raw clone count (off). Taller block = bigger clone."
    ),
    tags$li(
      tags$b("Ribbon width: "),
      "the same clone's size in each group; a ribbon that narrows/widens shows ",
      "the clone shrinking/growing between groups."
    ),
    tags$li(
      tags$b("Colour: "),
      "one distinct colour per clonotype, consistent across groups."
    ),
    tags$li(
      tags$b("Legend: "),
      "each clone by colour and name, with its total size in parentheses, ",
      "largest first. Blocks stack largest-at-the-bottom in the same order in ",
      "every group, so ribbons stay parallel."
    )
  )
)

## Ordered list of the tabs shown in the guide (matches the visible tab strip).
IR_GUIDE_TABS <- c(
  "Clonal UMAP",
  "Abundance",
  "Diversity",
  "Homeostasis",
  "Isotype",
  "Paired Scatter",
  "Clone Sharing",
  "Compare",
  "SizeDist"
)

## Map a guide tab to its schematic SVG (NULL -> no schematic yet).
ir_guide_schematic <- function(tab) {
  switch(
    tab,
    "Clonal UMAP" = ir_guide_svg_clonalumap,
    "Abundance" = ir_guide_svg_abundance,
    "Diversity" = ir_guide_svg_diversity,
    "Homeostasis" = ir_guide_svg_homeostasis,
    "Isotype" = ir_guide_svg_isotype,
    "Paired Scatter" = ir_guide_svg_pairedscatter,
    "Clone Sharing" = ir_guide_svg_clonesharing,
    "Compare" = ir_guide_svg_compare,
    "SizeDist" = ir_guide_svg_sizedist,
    NULL
  )
}

## Render the content pane for one tab: schematic + prose + element key.
ir_guide_tab_content <- function(tab) {
  help <- ir_tab_help[[tab]]
  schematic <- ir_guide_schematic(tab)
  elements <- ir_guide_elements[[tab]]
  tagList(
    if (!is.null(schematic)) {
      div(
        style = paste0(
          "background:#fafafa;border:1px solid #ececec;border-radius:8px;",
          "padding:12px;margin-bottom:14px;"
        ),
        schematic
      )
    },
    if (!is.null(help$summary)) {
      tags$p(style = "font-weight:600;font-size:14px;", help$summary)
    },
    if (!is.null(help$detail)) {
      div(
        style = "font-size:13px;white-space:pre-line;margin-bottom:12px;",
        help$detail
      )
    },
    if (!is.null(elements)) {
      tagList(
        tags$hr(),
        tags$p(
          style = "font-weight:600;font-size:13px;margin-bottom:4px;",
          "How to read it"
        ),
        elements
      )
    }
  )
}

## ---- Panel-level info button: illustrated, tabbed guide --------------- ##
## One tabPanel per visible visualization, each with its annotated schematic,
## prose and element key. Rendered as a left tab rail + right content pane via
## the .ir-guide CSS (below), so the modal reads like a small doc site.
observeEvent(input$ir_visualizations_info, {
  panels <- lapply(intersect(IR_GUIDE_TABS, ir_visible_tabs()), function(tab) {
    label <- if (identical(tab, "Clonal UMAP")) "Clonal projection" else tab
    tabPanel(
      label,
      value = tab,
      div(class = "ir-guide-pane", ir_guide_tab_content(tab))
    )
  })
  showModal(modalDialog(
    title = "Immune Repertoire visualizations — guide",
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    tags$style(HTML(paste0(
      # vertical tab rail on the left, content on the right
      ".ir-guide .nav-tabs{float:left;border-bottom:none;border-right:1px solid ",
      "#ececec;width:150px;margin-right:16px;}",
      ".ir-guide .nav-tabs>li{float:none;margin:0;}",
      ".ir-guide .nav-tabs>li>a{border:none;border-radius:6px;margin:2px 6px 2px 0;",
      "color:#1c1c1e;font-size:13px;padding:6px 10px;}",
      ".ir-guide .nav-tabs>li.active>a{background:#f0f4ff;color:#2f6fd6;",
      "font-weight:600;}",
      ".ir-guide .tab-content{overflow:auto;}",
      ".ir-guide .ig-elements{font-size:13px;padding-left:18px;margin:0;}",
      ".ir-guide .ig-elements li{margin-bottom:5px;}",
      ".ir-guide:after{content:'';display:table;clear:both;}"
    ))),
    div(
      class = "ir-guide",
      style = "min-height:360px;",
      tags$p(
        style = "margin-bottom:12px;font-size:13px;color:#6b6b70;",
        "Pick a visualization on the left. Each panel explains what the plot",
        "shows and how to read its axes, blocks, colours and legend. Controls",
        "on the left of the app (clone call, chain, grouping, top clones,",
        "proportion vs. counts) drive every plot."
      ),
      do.call(tabsetPanel, c(list(id = "ir_guide_tabs"), panels))
    )
  ))
})
