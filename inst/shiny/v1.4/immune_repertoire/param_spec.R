## ---------------------------------------------------------------------------
## IR_PARAM_SPEC — per-function analysis parameters (scRepertoire 2.6.2)
##
## Drives the dynamic "function-specific" settings panel: each visualization
## tab shows exactly the analysis parameters of its scRepertoire function.
## Pure output/style params (input.data, exportTable, palette) are excluded;
## order.by and specific-clone selectors are deferred.
##
## Each entry: tab label -> list of param specs. Each param spec:
##   id      : input id (ir_p_<param>)
##   label   : UI label
##   type    : "select" | "numeric" | "checkbox"
##   choices : for select — either a character vector, or one of the dynamic
##             tokens "<<groups>>" (metadata grouping columns) /
##             "<<genes>>" (gene-segment families)
##   value   : default value (must match the scRepertoire default)
##   min/max/step : for numeric
## ---------------------------------------------------------------------------

## ---- Global control visibility ----------------------------------------- ##
IR_GLOBAL_CONTROL_IDS <- c("ir_cloneCall", "ir_chain", "ir_groupBy")
IR_GLOBAL_CONTROL_HIDDEN <- list(
  # Clonal UMAP colours by clone size and uses its own Receptor selector; the
  # global Clone call is intentionally fixed there so hidden state cannot affect
  # the plot. The other tabs either do not use cloneCall or enforce their own.
  ir_cloneCall = c(
    "Clonal UMAP",
    "Isotype",
    "SHM Proxy",
    "Gene usage",
    "vizGenes",
    "percentGenes",
    "percentVJ",
    "AA %",
    "Entropy",
    "Property"
  ),
  ir_chain = c("vizGenes", "Clonal UMAP"),
  ir_groupBy = c("Clonal UMAP")
)

ir_global_control_visible <- function(id, tab) {
  hidden <- IR_GLOBAL_CONTROL_HIDDEN[[id]]
  if (is.null(hidden)) {
    hidden <- character(0)
  }
  is.null(tab) || !(tab %in% hidden)
}

ir_visible_global_ids <- function(tab) {
  IR_GLOBAL_CONTROL_IDS[vapply(
    IR_GLOBAL_CONTROL_IDS,
    ir_global_control_visible,
    logical(1),
    tab = tab
  )]
}

IR_SCATTER_SPEC <- list(
  list(
    id = "ir_p_graph",
    label = "Graph:",
    type = "select",
    choices = c("proportion", "count"),
    value = "proportion"
  ),
  list(
    id = "ir_p_dot_size",
    label = "Dot size:",
    type = "select",
    choices = c("total", "x", "y"),
    value = "total"
  )
)

IR_SCALE_SPEC <- list(
  list(
    id = "ir_p_scale",
    label = "Scale (proportion):",
    type = "checkbox",
    value = FALSE
  )
)

IR_PARAM_SPEC <- list(
  # Clonal UMAP: overlay clone expansion on the cell projection. Receptor and
  # projection choices are dynamic (resolved in settings.R via the
  # "<<receptors>>" / "<<projections>>" tokens), defaulting to the first option.
  "Clonal UMAP" = list(
    list(
      id = "ir_p_umap_receptor",
      label = "Receptor:",
      type = "select",
      choices = "<<receptors>>",
      value = NULL
    ),
    list(
      id = "ir_p_umap_projection",
      label = "Projection:",
      type = "select",
      choices = "<<projections>>",
      value = NULL
    ),
    list(
      id = "ir_p_umap_show_all",
      label = "Show all cells (grey background)",
      type = "checkbox",
      value = TRUE
    )
  ),

  "Diversity" = list(
    list(
      id = "ir_p_metric",
      label = "Metric:",
      type = "select",
      choices = c(
        "shannon",
        "inv.simpson",
        "gini.simpson",
        "norm.entropy",
        "pielou",
        "ace",
        "chao1",
        "gini",
        "d50",
        "hill0",
        "hill1",
        "hill2"
      ),
      value = "shannon"
    ),
    list(
      id = "ir_p_x_axis",
      label = "X axis (metadata):",
      type = "select",
      choices = "<<groups>>",
      value = ""
    ),
    list(
      id = "ir_p_n_boots",
      label = "Bootstrap iterations:",
      type = "numeric",
      value = 20,
      min = 3,
      max = 100,
      step = 1
    )
  ),

  "Scatter" = IR_SCATTER_SPEC,

  "Paired Scatter" = IR_SCATTER_SPEC,

  "vizGenes" = list(
    list(
      id = "ir_p_vg_x_axis",
      label = "X axis (gene):",
      type = "select",
      choices = "<<genes>>",
      value = "TRBV"
    ),
    list(
      id = "ir_p_vg_plot",
      label = "Plot:",
      type = "select",
      choices = c("heatmap", "barplot"),
      value = "heatmap"
    ),
    list(
      id = "ir_p_vg_summary",
      label = "Summary:",
      type = "select",
      choices = c("percent", "proportion", "count"),
      value = "percent"
    )
  ),

  "Overlap" = list(
    list(
      id = "ir_p_overlap_method",
      label = "Method:",
      type = "select",
      choices = c("overlap", "morisita", "jaccard", "cosine", "raw"),
      value = "overlap"
    )
  ),

  "K-mer" = list(
    list(
      id = "ir_p_motif_length",
      label = "Motif length:",
      type = "numeric",
      value = 3,
      min = 1,
      max = 6,
      step = 1
    ),
    list(
      id = "ir_p_min_depth",
      label = "Min depth:",
      type = "numeric",
      value = 3,
      min = 1,
      max = 20,
      step = 1
    ),
    list(
      id = "ir_p_top_motifs",
      label = "Top motifs:",
      type = "select",
      choices = c("10", "20", "25", "30", "50", "75", "100"),
      value = "30"
    )
  ),

  "Homeostasis" = list(
    list(
      id = "ir_p_clone_size",
      label = "Clone size thresholds:",
      type = "text",
      value = "0.0001, 0.001, 0.01, 0.1, 1"
    )
  ),

  ## ---- scale-only plots -------------------------------------------------
  "Abundance" = IR_SCALE_SPEC,
  "Length" = IR_SCALE_SPEC,
  "Quant" = IR_SCALE_SPEC,

  ## ---- clonal structure -------------------------------------------------
  "Proportion" = list(
    list(
      id = "ir_p_clonal_split",
      label = "Clonal split (comma-separated):",
      type = "text",
      value = "10, 100, 1000, 10000, 30000, 100000"
    )
  ),
  "Rarefaction" = list(
    list(
      id = "ir_p_rare_plot_type",
      label = "Plot type:",
      type = "select",
      choices = c(
        "Sample-size" = "1",
        "Coverage" = "2",
        "Sample completeness" = "3"
      ),
      value = "1"
    ),
    list(
      id = "ir_p_hill_numbers",
      label = "Hill number (q):",
      type = "select",
      choices = c(
        "Richness (q=0)" = "0",
        "Shannon (q=1)" = "1",
        "Simpson (q=2)" = "2"
      ),
      value = "0"
    ),
    list(
      id = "ir_p_rare_n_boots",
      label = "Bootstrap iterations:",
      type = "numeric",
      value = 20,
      min = 3,
      max = 100,
      step = 1
    )
  ),
  "SizeDist" = list(
    list(
      id = "ir_p_sd_method",
      label = "Clustering method:",
      type = "select",
      choices = c(
        "ward.D2",
        "ward.D",
        "single",
        "complete",
        "average",
        "mcquitty",
        "median",
        "centroid"
      ),
      value = "ward.D2"
    ),
    list(
      id = "ir_p_sd_threshold",
      label = "Threshold:",
      type = "numeric",
      value = 1,
      min = 1,
      max = 20,
      step = 1
    )
  ),
  "Compare" = list(
    list(
      id = "ir_p_compare_graph",
      label = "Graph:",
      type = "select",
      choices = c("alluvial", "area"),
      value = "alluvial"
    ),
    list(
      id = "ir_p_compare_prop",
      label = "Proportion (vs counts):",
      type = "checkbox",
      value = TRUE
    ),
    list(
      id = "ir_p_top_clones",
      label = "Top clones:",
      type = "numeric",
      value = 10,
      min = 1,
      max = 50,
      step = 1
    )
  ),

  ## ---- gene usage -------------------------------------------------------
  "Gene usage" = list(
    list(
      id = "ir_p_gu_genes",
      label = "Genes:",
      type = "select",
      choices = "<<genes>>",
      value = "TRBV"
    ),
    list(
      id = "ir_p_gu_plot_type",
      label = "Plot type:",
      type = "select",
      choices = c("heatmap", "barplot"),
      value = "heatmap"
    ),
    list(
      id = "ir_p_gu_summary",
      label = "Summary:",
      type = "select",
      choices = c("percent", "proportion", "count"),
      value = "percent"
    )
  ),
  "percentGenes" = list(
    list(
      id = "ir_p_pg_gene",
      label = "Gene segment:",
      type = "select",
      choices = c("Vgene", "Dgene", "Jgene"),
      value = "Vgene"
    ),
    list(
      id = "ir_p_pg_summary",
      label = "Summary:",
      type = "select",
      choices = c("percent", "proportion", "count"),
      value = "percent"
    )
  ),
  "percentVJ" = list(
    list(
      id = "ir_p_vj_summary",
      label = "Summary:",
      type = "select",
      choices = c("percent", "proportion", "count"),
      value = "percent"
    )
  ),

  ## ---- CDR3 amino-acid composition --------------------------------------
  "AA %" = list(
    list(
      id = "ir_p_aa_length",
      label = "AA length:",
      type = "numeric",
      value = 20,
      min = 5,
      max = 40,
      step = 1
    )
  ),
  "Entropy" = list(
    list(
      id = "ir_p_pe_aa_length",
      label = "AA length:",
      type = "numeric",
      value = 20,
      min = 5,
      max = 40,
      step = 1
    ),
    list(
      id = "ir_p_pe_method",
      label = "Method:",
      type = "select",
      choices = c(
        "shannon",
        "inv.simpson",
        "gini.simpson",
        "norm.entropy",
        "pielou",
        "hill0",
        "hill1",
        "hill2"
      ),
      value = "norm.entropy"
    )
  ),
  # Property: method choices are detected at runtime (immApex availability),
  # resolved via the "<<property_methods>>" token in the settings panel. The id
  # stays ir_property_method so the existing renderer keeps working.
  "Property" = list(
    list(
      id = "ir_property_method",
      label = "Property method:",
      type = "select",
      choices = "<<property_methods>>",
      value = NULL
    ),
    list(
      id = "ir_p_pp_aa_length",
      label = "AA length:",
      type = "numeric",
      value = 20,
      min = 5,
      max = 40,
      step = 1
    )
  )
)

## ---------------------------------------------------------------------------
## IR_DISPLAY_SPEC — generic display/style parameters, applied to all IR plots
##
## Parallel to IR_PARAM_SPEC but for pure presentation (font size, title, and
## for scatter-type plots point size / opacity). Rendered into a collapsible
## "Display options" panel (see ir_display_panel in settings.R) and applied via
## ir_apply_display(). Each param spec has the same shape as IR_PARAM_SPEC.
##
## Two reusable groups keep per-tab declarations DRY:
##   IR_DISPLAY_BASE    — applies to every plot (font size, title)
##   IR_DISPLAY_SCATTER — extra params only meaningful for point clouds
## ir_display_params_for(tab) assembles the applicable set per tab.
## ---------------------------------------------------------------------------

IR_DISPLAY_BASE <- list(
  list(
    id = "ir_d_base_size",
    label = "Font size:",
    type = "slider",
    value = 12,
    min = 6,
    max = 30,
    step = 1
  ),
  list(
    id = "ir_d_title",
    label = "Title:",
    type = "text",
    value = ""
  )
)

IR_DISPLAY_SCATTER <- list(
  list(
    id = "ir_d_point_size",
    label = "Point size:",
    type = "slider",
    value = 1,
    min = 0.1,
    max = 6,
    step = 0.1
  ),
  list(
    id = "ir_d_alpha",
    label = "Point opacity:",
    type = "slider",
    value = 0.8,
    min = 0.1,
    max = 1,
    step = 0.05
  )
)

## Legend controls — applicable to every plot (each has a legend). Font size and
## key/point size apply on both the ggplot and plotly paths; position also lets
## the legend be hidden.
IR_DISPLAY_LEGEND <- list(
  list(
    id = "ir_d_legend_size",
    label = "Legend font size:",
    type = "slider",
    value = 12,
    min = 6,
    max = 30,
    step = 1
  ),
  list(
    id = "ir_d_legend_key",
    label = "Legend point size:",
    type = "slider",
    value = 3,
    min = 1,
    max = 12,
    step = 0.5
  ),
  list(
    id = "ir_d_legend_pos",
    label = "Legend position:",
    type = "select",
    choices = c(
      "Right" = "right",
      "Bottom" = "bottom",
      "Top" = "top",
      "Left" = "left",
      "Hidden" = "none"
    ),
    value = "right"
  )
)

## Tabs whose plots are point clouds (scatter-type): get the scatter extras.
IR_SCATTER_TABS <- c("Clonal UMAP", "Scatter", "Paired Scatter")

## ---------------------------------------------------------------------------
## order.by — a generic "Order groups" control, reused across every tab whose
## scRepertoire function accepts order.by. Declared once here and appended to
## the analysis params of the applicable tabs (see ir_param_panel). Default
## (empty) keeps scRepertoire's own ordering; "alphanumeric" sorts the group
## axis. Low-risk: NULL/"" maps to the API default, so nothing changes unless
## the user opts in.
## ---------------------------------------------------------------------------
IR_ORDER_BY_PARAM <- list(
  id = "ir_p_order_by",
  label = "Order groups:",
  type = "select",
  choices = c("Default" = "", "Alphanumeric" = "alphanumeric"),
  value = ""
)

## Tabs whose scRepertoire function accepts order.by.
IR_ORDER_BY_TABS <- c(
  "Abundance",
  "Length",
  "Diversity",
  "Homeostasis",
  "Compare",
  "vizGenes",
  "Gene usage",
  "percentGenes",
  "percentVJ",
  "AA %",
  "Entropy",
  "Property"
)

## Assemble the display params applicable to a given tab.
ir_display_params_for <- function(tab) {
  params <- IR_DISPLAY_BASE
  if (!is.null(tab) && tab %in% IR_SCATTER_TABS) {
    params <- c(params, IR_DISPLAY_SCATTER)
  }
  # Every plot has a legend, so the legend controls apply to all tabs.
  params <- c(params, IR_DISPLAY_LEGEND)
  params
}

IR_DESC_SUMMARY <- "How values are scaled: percent or proportion (share within each group) or raw count."
IR_DESC_HEATMAP_BARPLOT <- "Heatmap (compact overview of many genes/groups) or barplot (easier to read exact values for few genes)."
IR_DESC_AA_LENGTH <- "CDR3 length (in amino acids) to analyse position-by-position. Sequences of a different length are excluded."

## ---------------------------------------------------------------------------
## IR_PARAM_DESC — plain-language help for every control, keyed by input id.
##
## Single source of truth for the info dialogs (see ir_param_help_cards in
## settings.R). Kept central (not per IR_PARAM_SPEC entry) because many params
## are reused across tabs (scale, bootstrap, summary, aa_length ...), so each is
## explained once. Covers the global controls, the per-tab analysis params, and
## the display options. Written for a biologist who is not a scRepertoire user:
## say what the control does and how reading the plot changes, not the API.
## ---------------------------------------------------------------------------
IR_PARAM_DESC <- list(
  ## ---- Global controls ----
  ir_cloneCall = "How a 'clone' is defined when counting cells. gene = same V(D)J genes; nt = identical CDR3 nucleotide sequence; aa = identical CDR3 amino-acid sequence; strict = same genes AND same CDR3 nucleotides (most specific). Stricter definitions split near-identical cells into separate clones.",
  ir_chain = "Which receptor chain to analyse. 'All chains' combines them; otherwise restrict to one chain (e.g. TRB for the T-cell beta chain, IGH for the B-cell heavy chain). Choose a single chain when a plot should reflect just that chain's diversity or genes.",
  ir_groupBy = "Metadata column that defines the comparison units. None uses the loaded samples (the repertoire list elements); choosing a column (sample, condition, treatment, cell type, ...) makes that column's levels the units scRepertoire compares. On Paired Scatter this is shown as Compare by and directly defines the X/Y candidates.",

  ## ---- Clonal UMAP ----
  ir_p_umap_receptor = "Which receptor to colour by: TCR (T cells) or BCR (B cells). Only the types present in your data are offered.",
  ir_p_umap_projection = "The cell map to draw on — the same UMAP/tSNE projections used elsewhere in the app. Pick which one to overlay the clones on.",
  ir_p_umap_show_all = "When on, every cell is drawn: cells without the selected receptor appear light grey, so the coloured (expanded) clones stand out in context. When off, only cells carrying the receptor are shown.",

  ## ---- Diversity ----
  ir_p_metric = "The diversity index. shannon/norm.entropy balance richness and evenness; inv.simpson/gini.simpson emphasise the dominant clones; chao1/ace estimate unseen clones; d50 is how many clones make up half the cells. Higher usually means a broader, more even repertoire.",
  ir_p_x_axis = "A metadata column to spread the groups along the x-axis (e.g. condition), so diversity is compared across that variable.",
  ir_p_n_boots = "How many bootstrap resamples to average over for the diversity estimate and its spread. More iterations give a smoother, more stable estimate but take longer.",

  ## ---- Scatter ----
  ir_p_graph = "Whether the axes show each clone's proportion (share of the repertoire) or raw count. Proportion makes samples of different sizes comparable.",
  ir_p_dot_size = "What the dot size encodes: the clone's total size, or its size on the x or the y sample only.",

  ## ---- vizGenes / gene usage ----
  ir_p_vg_x_axis = "Which gene-segment family to put on the x-axis (e.g. TRBV for TCR beta V genes).",
  ir_p_vg_plot = IR_DESC_HEATMAP_BARPLOT,
  ir_p_vg_summary = IR_DESC_SUMMARY,
  ir_p_gu_genes = "Which gene-segment family to summarise (e.g. TRBV, IGHV).",
  ir_p_gu_plot_type = IR_DESC_HEATMAP_BARPLOT,
  ir_p_gu_summary = IR_DESC_SUMMARY,
  ir_p_pg_gene = "Which gene segment to break down: V, D or J gene.",
  ir_p_pg_summary = IR_DESC_SUMMARY,
  ir_p_vj_summary = IR_DESC_SUMMARY,

  ## ---- Overlap ----
  ir_p_overlap_method = "How clonotype sharing between two groups is scored. overlap/jaccard/cosine/morisita differ in how they weight clone sizes; raw is the count of shared clones. Higher means the groups share more of their repertoire.",

  ## ---- K-mer ----
  ir_p_motif_length = "Length (in amino acids) of the short CDR3 sub-sequences (k-mers) to count. Longer motifs are more specific but rarer.",
  ir_p_min_depth = "Minimum number of times a motif must occur to be kept, filtering out noise.",
  ir_p_top_motifs = "How many of the most frequent motifs to display.",

  ## ---- scale-only / structure ----
  ir_p_scale = "Show proportions (share of the repertoire) instead of raw cell counts, so samples of different sizes are comparable.",
  ir_p_clonal_split = "The size thresholds (comma-separated) that bin clones into proportion categories, from rare to expanded.",

  ## ---- Rarefaction ----
  ir_p_rare_plot_type = "What the rarefaction curve shows: diversity vs sample size, vs sequencing coverage, or sample completeness.",
  ir_p_hill_numbers = "Which diversity order (Hill number q) to plot: q=0 counts clones (richness), q=1 weights by Shannon, q=2 emphasises the dominant clones.",
  ir_p_rare_n_boots = "Bootstrap resamples for the rarefaction confidence band. More is smoother but slower.",

  ## ---- SizeDist ----
  ir_p_sd_method = "Linkage method for clustering samples by their clone-size distribution (ward.D2 is a common default).",
  ir_p_sd_threshold = "Minimum clone size considered when fitting the distribution.",

  ## ---- Compare ----
  ir_p_compare_graph = "alluvial (ribbons tracking clones between groups) or area (stacked bands).",
  ir_p_compare_prop = "Plot each clone's proportion instead of raw counts, so groups of different sizes are comparable.",
  ir_p_top_clones = "How many of the largest clones to track across the groups.",

  ## ---- CDR3 amino-acid composition ----
  ir_p_aa_length = IR_DESC_AA_LENGTH,
  ir_p_pe_aa_length = IR_DESC_AA_LENGTH,
  ir_p_pe_method = "Which entropy/diversity measure to compute at each CDR3 position.",
  ir_property_method = "The amino-acid property scale to profile along the CDR3 (e.g. Atchley, Kidera) — captures physico-chemical character such as hydrophobicity.",
  ir_p_pp_aa_length = IR_DESC_AA_LENGTH,

  ## ---- Display options ----
  ir_d_base_size = "Base font size for the plot's text (axis labels, legend, title).",
  ir_d_title = "A custom title shown above the plot. Leave blank for none.",
  ir_d_point_size = "Diameter of the scatter points.",
  ir_d_alpha = "Point opacity (0 = transparent, 1 = solid). Lower values help when points overlap heavily.",
  ir_d_legend_size = "Font size of the legend text.",
  ir_d_legend_key = "Size of the point/marker shown for each legend entry.",
  ir_d_legend_pos = "Where to place the legend, or hide it.",

  ## ---- Homeostasis ----
  ir_p_clone_size = "The upper bounds (as a fraction of the repertoire) that bin clones into Rare / Small / Medium / Large / Hyperexpanded. Five increasing numbers, comma-separated. Leave as-is for scRepertoire's defaults.",

  ## ---- Generic ----
  ir_p_order_by = "The order the groups appear along the axis. Default keeps scRepertoire's own ordering; Alphanumeric sorts them by name."
)
