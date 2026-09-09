## ---- Help text formatter ---------------------------------------------- ##
.format_detail <- function(txt) {
  lines <- strsplit(txt, "\n")[[1]]
  out <- list()
  ul_buf <- character(0)
  is_first_para <- TRUE

  ## -- inline markup: 'term' -> bold accent; em-dash split in bullets ----
  .inline <- function(s) {
    # Replace 'quoted' terms with styled <b>
    s <- gsub("'([^']+)'", "<b style='color:#0f6cbd;'>\\1</b>", s)
    HTML(s)
  }

  .make_li <- function(raw) {
    txt <- sub("^\\s*\u2022\\s*", "", raw)
    # Split on em-dash: bold the key part, normal the explanation
    if (grepl("\u2014", txt)) {
      parts <- strsplit(txt, "\\s*\u2014\\s*", perl = TRUE)[[1]]
      tagList(
        tags$li(
          style = "margin: 4px 0; line-height: 1.5;",
          tags$strong(.inline(parts[1])),
          if (length(parts) > 1) {
            tagList(
              " \u2014 ",
              tags$span(
                style = "color: var(--neutral-secondary);",
                .inline(paste(parts[-1], collapse = " \u2014 "))
              )
            )
          }
        )
      )
    } else {
      tags$li(style = "margin: 4px 0; line-height: 1.5;", .inline(txt))
    }
  }

  flush_ul <- function() {
    if (length(ul_buf) > 0) {
      items <- lapply(ul_buf, .make_li)
      out[[length(out) + 1L]] <<- do.call(
        tags$ul,
        c(
          items,
          list(
            style = "padding-left: 22px; margin: 8px 0; list-style-type: disc;"
          )
        )
      )
      ul_buf <<- character(0)
    }
  }

  for (ln in lines) {
    if (grepl("^\\s*\u2022", ln)) {
      ## bullet line
      ul_buf <- c(ul_buf, ln)
    } else if (trimws(ln) == "") {
      ## blank line -> flush
      flush_ul()
    } else if (grepl(":$", trimws(ln))) {
      ## section header (e.g. "What to look for:")
      flush_ul()
      out[[length(out) + 1L]] <- tags$p(
        style = "margin: 12px 0 4px 0; font-weight: 600; color: #0f6cbd; font-size: 14px;",
        sub(":$", "", trimws(ln))
      )
    } else {
      ## regular paragraph
      flush_ul()
      if (is_first_para) {
        out[[length(out) + 1L]] <- tags$p(
          style = "margin: 6px 0; line-height: 1.6; font-size: 14px;",
          .inline(ln)
        )
        is_first_para <- FALSE
      } else {
        out[[length(out) + 1L]] <- tags$p(
          style = "margin: 6px 0; line-height: 1.6; color: var(--neutral-primary);",
          .inline(ln)
        )
      }
    }
  }
  flush_ul()
  do.call(tagList, out)
}

## ---- Help text for each visualization tab ----------------------------- ##
ir_tab_help <- list(
  "Clone Sharing" = list(
    short = "Cross-group clonotype sharing",
    summary = "Classifies each clonotype as private to one unit, public within a group, or shared across groups.",
    detail = paste(
      "Some clonotypes appear in only one sample; others are found in several, and a few are shared across different conditions. This plot summarises that sharing.",
      "",
      "Each clonotype (V+J+CDR3 of the selected chain) is labelled:",
      "• Private — seen in only one 'sharing unit' (default: sample).",
      "• Public (within-group) — in ≥ 2 units, all in the same 'Group by' group.",
      "• Public (cross-group) — spanning ≥ 2 groups.",
      "",
      "Controls:",
      "• Sharing unit — the smallest unit across which sharing is counted.",
      "• Group by — the grouping used for within/cross classification.",
      "",
      "With no 'Group by' selected the classes collapse to Private / Public.",
      sep = "\n"
    )
  ),
  Abundance = list(
    short = "Clonal abundance distribution",
    summary = "Ranks clonotypes by cell count. Steep drop-off indicates oligoclonal dominance; gradual decline indicates diverse repertoire.",
    detail = paste(
      "Every T or B cell carries a unique receptor sequence (called a 'clonotype').",
      "Some clonotypes are found in many cells (expanded clones), while most appear only once or twice (rare clones).",
      "",
      "This plot ranks all clonotypes by how many cells carry them.",
      "The X-axis is the rank (1 = most common clone), and the Y-axis is the number of cells.",
      "",
      "What to look for:",
      "\u2022 A steep drop-off means a few clones dominate \u2014 this often happens after infection or in tumors where certain T/B cells multiply rapidly.",
      "\u2022 A flat, gradual curve means many clones are roughly equal in size \u2014 typical of a resting, diverse immune system.",
      "\u2022 Compare samples: if one sample has a much steeper curve, that sample likely experienced stronger clonal expansion.",
      sep = "\n"
    )
  ),
  "Clonal UMAP" = list(
    short = "Clonal expansion on the cell UMAP",
    summary = "Overlays each cell's clone-expansion level onto the existing cell projection (UMAP/tSNE), so you can see where expanded T/B clones sit.",
    detail = paste(
      "This reuses the cell projection you already computed (the same UMAP as the other tabs) and colours each cell by how large its clonotype is.",
      "",
      "Clones are binned into expansion levels by the number of cells sharing them:",
      "• Single (0 < X <= 1)      — the clonotype appears in one cell",
      "• Small (1 < X <= 5)",
      "• Medium (5 < X <= 20)",
      "• Large (20 < X <= 100)",
      "• Hyperexpanded (100 < X) — strongly expanded clones",
      "",
      "Controls:",
      "• Receptor: choose TCR or BCR (only the types present in your data are offered).",
      "• Projection: which dimensional reduction to plot on.",
      "• Clone call: how clonotype identity is defined (gene/nt/aa/strict).",
      "• Display options: point size and opacity for the scatter.",
      "",
      "What to look for: expanded (orange/gold) cells clustering in a region suggest a localized clonal response — e.g. an activated/effector population.",
      sep = "\n"
    )
  ),
  Diversity = list(
    short = "Repertoire diversity",
    summary = "Quantifies clonotype richness and evenness using Shannon entropy. Higher values reflect broader, more balanced repertoires.",
    detail = paste(
      "Diversity measures how many different clonotypes exist AND how evenly they are distributed.",
      "Think of it like species diversity in an ecosystem \u2014 a forest with 100 equally common tree species is more 'diverse' than one with 100 species where a single species makes up 99%.",
      "",
      "How the plot works — bootstrap resampling:",
      "\u2022 For each sample, scRepertoire randomly draws clonotypes (with replacement) to create a 'resampled' dataset of the same size, then calculates Shannon entropy. This process is repeated many times (controlled by 'Bootstrap iterations').",
      "\u2022 The result is a distribution of diversity values — not a single number, but a range that reflects how stable the estimate is given the number of clonotypes in that sample.",
      "",
      "What the dots (jitter points) actually represent:",
      "\u2022 Each jitter point is ONE bootstrap replicate \u2014 one diversity value computed from one random resampling of the original clonotype pool. They are NOT independent biological observations.",
      "\u2022 The boxplot summarises the bootstrap distribution: median line is the point estimate, box is the middle 50% (IQR), whiskers show the range.",
      "\u2022 A narrow box means the diversity estimate is stable (bootstrap repeatedly gives similar values). A wide box means the estimate is more uncertain \u2014 typical for samples with few clonotypes.",
      "",
      "When 'Group by' and 'X axis' are the SAME column (e.g. both set to 'sample'):",
      "\u2022 Each x-axis position has exactly one group. All jitter points at that position belong to that single group \u2014 they show the bootstrap uncertainty of that group's diversity estimate.",
      "\u2022 This is correct behaviour, not a bug: the jitter is visualising the spread of bootstrap replicates, not displaying multiple independent measurements.",
      "",
      "When 'Group by' and 'X axis' are DIFFERENT (e.g. Group by = cell_type, X axis = condition):",
      "\u2022 Each x-axis position contains multiple groups side-by-side.",
      "\u2022 Jitter points are colour-coded by Group by, making it easy to compare diversity across categories at each condition.",
      "",
      "What to look for:",
      "\u2022 Higher values = more diverse repertoire (many clonotypes, evenly distributed).",
      "\u2022 Lower values = less diverse (dominated by a few expanded clones).",
      "\u2022 After vaccination or infection, diversity often drops temporarily as specific clones expand.",
      "\u2022 In autoimmune diseases, you may see persistently low diversity in the affected tissue.",
      "\u2022 Bootstrapping helps you judge whether differences between samples are meaningful or just random variation \u2014 non-overlapping boxplots suggest a genuine difference.",
      sep = "\n"
    )
  ),
  Homeostasis = list(
    short = "Clonal homeostasis",
    summary = "Categorises clonotypes into size classes (Rare to Hyperexpanded). Shifts toward larger classes indicate active clonal expansion.",
    detail = paste(
      "This plot groups all clonotypes into size categories based on how many cells carry them:",
      "\u2022 Rare: appears in very few cells",
      "\u2022 Small: slightly more common",
      "\u2022 Medium: moderately expanded",
      "\u2022 Large: substantially expanded",
      "\u2022 Hyperexpanded: found in a very large number of cells",
      "",
      "Each bar shows the proportion of cells belonging to each category for a given sample.",
      "",
      "What to look for:",
      "\u2022 A healthy resting repertoire is mostly 'Rare' and 'Small' clones.",
      "\u2022 After immune activation (infection, vaccination), you'll see more cells shifting into 'Large' and 'Hyperexpanded' categories.",
      "\u2022 Comparing samples side-by-side reveals which conditions drive more clonal expansion.",
      "\u2022 In cancer, tumor-infiltrating lymphocytes often show a high proportion of hyperexpanded clones (indicating anti-tumor response or exhaustion).",
      sep = "\n"
    )
  ),
  Compare = list(
    short = "Clonotype tracking",
    summary = "Alluvial diagram tracking top clonotypes across samples. Shared ribbons represent public or persistent clones.",
    detail = paste(
      "This alluvial (flow) diagram tracks the top clonotypes across your selected samples.",
      "Each coloured ribbon represents a clonotype, and its height represents its proportion.",
      "",
      "What to look for:",
      "\u2022 Ribbons that flow across multiple samples = 'public' or shared clonotypes, present in both samples.",
      "\u2022 Ribbons that appear in only one sample = 'private' clonotypes, unique to that sample.",
      "\u2022 In longitudinal studies (same patient, different time points), shared clonotypes represent persistent immune memory.",
      "\u2022 In different patients, shared clonotypes suggest convergent immune responses to the same antigen.",
      "\u2022 The height of each ribbon shows how dominant that clone is \u2014 a thick ribbon across both samples means a highly expanded public clone.",
      sep = "\n"
    )
  ),
  SizeDist = list(
    short = "Clone size distribution clustering",
    summary = "Hierarchical clustering of samples by clone size distribution. Samples that cluster together share similar repertoire architecture.",
    detail = paste(
      "This dendrogram clusters samples based on how similar their clone size distributions are.",
      "It uses Ward's hierarchical clustering method \u2014 samples that branch together early have the most similar patterns.",
      "",
      "What to look for:",
      "\u2022 Samples that cluster together share similar 'shapes' of repertoire \u2014 e.g., both dominated by a few big clones, or both having many small clones.",
      "\u2022 If disease samples cluster separately from healthy samples, it suggests disease systematically changes the repertoire structure.",
      "\u2022 If replicates or time points from the same patient cluster together, it confirms biological consistency.",
      "\u2022 Long branch lengths between clusters mean large differences in repertoire structure.",
      "\u2022 This provides a global summary of repertoire 'shape' without focusing on specific clonotypes.",
      sep = "\n"
    )
  ),
  Isotype = list(
    short = "BCR isotype distribution",
    summary = "Stacked-bar of IgM/IgD/IgG1-4/IgA1-2/IgE proportions per sample or timepoint. Class-switch readout unique to BCR.",
    detail = paste(
      "After antigen stimulation in germinal centres, B cells can switch their antibody isotype (class-switch recombination) from IgM/IgD to IgG, IgA, or IgE.",
      "This plot shows the proportion of each isotype in every sample or timepoint.",
      "",
      "What to look for:",
      "\u2022 A high IgM/IgD fraction indicates na\u00efve or unswitched B cells.",
      "\u2022 Increased IgG (especially IgG1/IgG3) suggests T-cell-dependent immune activation.",
      "\u2022 IgA enrichment is typical in mucosal tissues or chronic inflammation.",
      "\u2022 Shifts from IgM-dominant to IgG/IgA-dominant between timepoints indicate ongoing germinal centre maturation.",
      "\u2022 This analysis is BCR-specific \u2014 TCR data does not have isotype information.",
      sep = "\n"
    )
  ),
  `Paired Scatter` = list(
    short = "Paired or manual clone scatter",
    summary = "Compares clonotype frequencies between two comparison units, such as samples, conditions, or cell types. Off-diagonal clones expanded or contracted.",
    detail = paste(
      "Every dot is a clonotype; X and Y are the selected comparison units.",
      "Use Compare by to decide what the selectable X/Y groups are. None uses the original repertoire samples; sample, condition, cell type, or treatment use that metadata column's levels.",
      "If a 2-level sample-level metadata column is available, Pair by can compare those paired levels and optionally facet by another sample-level column.",
      "",
      "What to look for:",
      "\u2022 Dots on the diagonal \u2014 stable clones, unchanged by treatment.",
      "\u2022 Dots above the diagonal \u2014 clones enriched in the Y selection.",
      "\u2022 Dots below the diagonal \u2014 clones enriched in the X selection.",
      "\u2022 Dots along only one axis \u2014 clones unique to one side of the comparison.",
      "\u2022 With paired Pre/Post data, facet by subject_id to compare subjects side by side.",
      sep = "\n"
    )
  )
)

## ---- Collapsible help panel ------------------------------------------- ##
output$ir_help_panel <- renderUI({
  tab <- input$ir_tabs
  if (is.null(tab)) {
    return(NULL)
  }
  info <- ir_tab_help[[tab]]
  if (is.null(info)) {
    return(NULL)
  }
  example_button <- if (ir_help_has_example(tab)) {
    actionButton(
      "ir_help_example_btn",
      label = tags$span(icon("lightbulb"), " Example"),
      class = "btn-xs",
      style = "white-space: nowrap; margin-top: 2px; background: #0f6cbd; color: #fff; border: none;"
    )
  } else {
    NULL
  }
  div(
    style = "background: #e5f0fa; border-left: 4px solid #0f6cbd; padding: 8px 12px; margin-bottom: 10px; font-size: 13px; border-radius: 2px; display: flex; align-items: flex-start; gap: 10px;",
    div(
      style = "flex: 1;",
      tags$strong(info$short),
      tags$p(
        style = "margin: 4px 0 0 0; color: var(--neutral-secondary);",
        info$summary
      )
    ),
    example_button
  )
})

## ---- Demo data (lazy, cached) ----------------------------------------- ##
ir_demo_data <- reactiveVal(NULL)

.get_demo_data <- function() {
  if (!is.null(ir_demo_data())) {
    return(ir_demo_data())
  }
  data("contig_list", package = "scRepertoire", envir = environment())
  demo <- scRepertoire::combineTCR(
    contig_list[1:2],
    samples = c("Healthy", "Disease")
  )
  ir_demo_data(demo)
  demo
}

## ---- BCR demo data (synthetic — scRepertoire has no built-in BCR) ---- ##
ir_bcr_demo_data <- reactiveVal(NULL)

.get_bcr_demo_data <- function() {
  if (!is.null(ir_bcr_demo_data())) {
    return(ir_bcr_demo_data())
  }
  demo <- ir_make_bcr_demo_data()
  ir_bcr_demo_data(demo)
  demo
}

## ---- TCR demo data (synthetic — for SELF-MADE plots, no scRepertoire) - ##
ir_tcr_demo_data <- reactiveVal(NULL)

.get_tcr_demo_data <- function() {
  if (!is.null(ir_tcr_demo_data())) {
    return(ir_tcr_demo_data())
  }
  demo <- ir_make_tcr_demo_data()
  ir_tcr_demo_data(demo)
  demo
}

## ---- Example modal ---------------------------------------------------- ##
observeEvent(input$ir_help_example_btn, {
  tab <- input$ir_tabs
  if (is.null(tab)) {
    return()
  }
  info <- ir_tab_help[[tab]]
  demo_kind <- ir_help_demo_kind(tab)
  if (is.null(info) || identical(demo_kind, "none")) {
    return()
  }
  demo_note <- switch(
    demo_kind,
    local_bcr = paste(
      "Generated from a synthetic BCR demo",
      "(2 samples: Pre- vs Post-vaccination)."
    ),
    local_tcr = paste(
      "Generated from a synthetic TCR demo",
      "(2 samples: Healthy vs Disease) — no scRepertoire needed."
    ),
    backed = paste(
      "Generated from scRepertoire built-in demo data",
      "(2 TCR samples: Healthy vs Disease)."
    )
  )

  showModal(modalDialog(
    title = paste0("Example: ", tab),
    size = "l",
    easyClose = TRUE,
    fade = TRUE,
    div(
      div(
        style = "font-size: 14px; margin-bottom: 12px;",
        .format_detail(info$detail)
      ),
      tags$hr(),
      tags$p(
        style = "color: var(--neutral-tertiary); font-size: 12px;",
        demo_note
      ),
      plotOutput("ir_demo_plot", height = "450px")
    ),
    footer = modalButton("Close")
  ))
})

## ---- Demo plot renderer ----------------------------------------------- ##
output$ir_demo_plot <- renderPlot({
  req_plot_space("ir_demo_plot")
  tab <- input$ir_tabs
  if (is.null(tab)) {
    return()
  }
  demo_kind <- ir_help_demo_kind(tab)
  if (identical(demo_kind, "none")) {
    return()
  }
  ## Keep the explicit load/error boundary used by live backed renderers.
  ## Local BCR/TCR examples never cross it.
  if (identical(demo_kind, "backed")) {
    req_scRepertoire()
  }
  tryCatch(
    {
      demo <- switch(
        demo_kind,
        local_bcr = .get_bcr_demo_data(),
        local_tcr = .get_tcr_demo_data(),
        backed = .get_demo_data()
      )
      p <- switch(
        tab,
        "Abundance" = scRepertoire::clonalAbundance(demo, cloneCall = "gene"),
        "Diversity" = ir_plot_clonal_diversity(
          data = demo,
          clone_call = "gene",
          chain = "TRB",
          group_by = NULL,
          metric = "shannon",
          x_axis = NULL,
          n_boots = 20,
          palette = "inferno"
        ),
        "Homeostasis" = scRepertoire::clonalHomeostasis(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          palette = "inferno"
        ),
        "Compare" = scRepertoire::clonalCompare(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          samples = names(demo),
          top.clones = 5,
          graph = "alluvial",
          palette = "inferno"
        ),
        "Paired Scatter" = scRepertoire::clonalScatter(
          demo,
          cloneCall = "gene",
          chain = "TRB",
          x.axis = names(demo)[1],
          y.axis = names(demo)[2],
          palette = "inferno"
        ),
        "SizeDist" = scRepertoire::clonalSizeDistribution(
          demo,
          cloneCall = "gene",
          method = "ward.D2"
        ),
        "Clone Sharing" = ir_build_sharing_plot(
          # Ensure a `sample` column (the sharing unit) on each demo frame,
          # keyed by the list element name. scRepertoire 2.6.2's combineTCR()
          # already adds one, so this is a defensive/idempotent guard that keeps
          # the demo working regardless of the combineTCR version's behaviour.
          Map(
            function(df, nm) {
              df$sample <- nm
              df
            },
            demo,
            names(demo)
          ),
          chain = "TRB",
          unit_col = "sample",
          group_by = NULL
        ),
        "Isotype" = bcr_isotype_plot(demo, group_col = "sample"),
        stop("No example renderer registered for: ", tab)
      )
      if (is.null(p)) {
        stop("Example renderer returned no plot for: ", tab)
      }
      if (inherits(p, "gg")) {
        print(p)
      }
    },
    error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating example:\n", e$message), cex = 0.9)
    }
  )
})

## The panel-level "info" button opens an illustrated, tabbed guide; that modal
## and its content live in help_guide.R (sourced right after this file).
