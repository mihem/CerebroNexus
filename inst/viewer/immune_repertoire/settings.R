## ---- Main parameters (left column, box 1) ----------------------------- ##
## Core controls needed to select a plot: the global cloneCall / chain /
## group-by (shown only on tabs they apply to) plus the current tab's
## function-specific analysis parameters (IR_PARAM_SPEC). Scatter / Compare
## sample selectors live here too, scoped to their tabs.
output$ir_main_params_UI <- renderUI({
  if (!has_scRepertoire()) {
    return(ir_scRepertoire_missing_ui())
  }
  raw <- ir_data_raw()
  if (is.null(raw)) {
    return(div(
      class = "alert alert-warning",
      "No immune repertoire data available. Import data with TCR/BCR annotations first."
    ))
  }

  chains_present <- detect_chains(raw)
  tcr_present <- intersect(chains_present, c("TRA", "TRB", "TRG", "TRD"))
  bcr_present <- intersect(chains_present, c("IGH", "IGK", "IGL"))
  # Build a flat named vector of chain choices. A nested list mixing a
  # top-level scalar ("both") with sub-lists (TCR/BCR optgroups) renders
  # incorrectly under selectize (only the scalar survives), so keep it flat.
  chain_choices <- c("All chains" = "both")
  if (length(tcr_present) > 0) {
    chain_choices <- c(chain_choices, setNames(tcr_present, tcr_present))
  }
  if (length(bcr_present) > 0) {
    chain_choices <- c(chain_choices, setNames(bcr_present, bcr_present))
  }

  # Grouping options come from the data set's declared grouping variables
  # joined onto the IR data by barcode (see ir_data_annotated), so users can
  # group by ANY metadata column (sample, condition, treatment, cell type, ...)
  # rather than only columns embedded in the IR table itself.
  annotated <- ir_data_annotated()
  data_cols <- if (!is.null(annotated)) {
    names(annotated[[1]])
  } else {
    names(raw[[1]])
  }
  groups <- tryCatch(getGroups(), error = function(e) character(0))
  available_groups <- intersect(groups, data_cols)

  # Global control visibility comes from IR_GLOBAL_CONTROL_HIDDEN
  # (param_spec.R), so the UI and help dialogs cannot drift.
  tab <- input$ir_tabs

  # Collect only the controls that apply to the current tab, then flow them into
  # rows so a hidden control never leaves a blank gap.
  controls <- list()
  if (ir_global_control_visible("ir_cloneCall", tab)) {
    controls <- c(
      controls,
      list(selectInput(
        "ir_cloneCall",
        "Clone call:",
        choices = c("gene", "nt", "aa", "strict"),
        selected = "gene",
        selectize = FALSE
      ))
    )
  }
  if (ir_global_control_visible("ir_chain", tab)) {
    controls <- c(
      controls,
      list(selectInput(
        "ir_chain",
        "Chain:",
        choices = chain_choices,
        selected = "both",
        selectize = FALSE
      ))
    )
  }
  if (ir_global_control_visible("ir_groupBy", tab)) {
    # group.by is the single grouping control: None compares the loaded samples
    # (the repertoire list elements); a metadata column makes that column's
    # levels the comparison units. scRepertoire rbinds + re-splits internally
    # (.groupList), so this fully defines what a plot compares.
    prev_gb <- isolate(input$ir_groupBy)
    default_gb <- if (!is.null(prev_gb) && prev_gb %in% available_groups) {
      prev_gb
    } else {
      ""
    }
    group_label <- if (
      !is.null(tab) && tab %in% c("Paired Scatter", "Scatter", "Compare")
    ) {
      "Compare by:"
    } else {
      "Group results by:"
    }
    controls <- c(
      controls,
      list(selectInput(
        "ir_groupBy",
        group_label,
        choices = c("None" = "", available_groups),
        selected = default_gb,
        selectize = FALSE
      ))
    )
  }
  tagList(
    tags$style(
      "#ir_chain + .selectize-control .selectize-dropdown-content { max-height: none; }"
    ),
    # Global controls, flowed two-per-row so no hidden control leaves a gap.
    ir_flow_controls(controls),
    # High-frequency analysis parameters for the current tab.
    uiOutput("ir_primary_param_panel"),
    # Scatter / Compare sample selectors only on their own tabs.
    conditionalPanel(
      condition = "input.ir_tabs == 'Scatter'",
      uiOutput("ir_scatter_settings")
    ),
    conditionalPanel(
      condition = "input.ir_tabs == 'Compare'",
      uiOutput("ir_compare_settings")
    ),
    conditionalPanel(
      condition = "input.ir_tabs == 'Clone Sharing'",
      selectInput(
        "ir_sharing_unit",
        "Sharing unit:",
        choices = ir_sharing_unit_choices(),
        selected = if ("sample" %in% ir_sharing_unit_choices()) {
          "sample"
        } else {
          NULL
        },
        selectize = FALSE
      )
    )
  )
})

## ---- Appearance controls (settings drawer) ---------------------------- ##
## Only render the section when the active plot has effective controls.
output$ir_appearance_section_UI <- renderUI({
  if (!has_scRepertoire() || is.null(ir_data_raw())) {
    return(NULL)
  }
  tab <- input$ir_tabs
  has_display <- exists("ir_display_params_for") &&
    length(ir_display_params_for(tab)) > 0
  group_by <- ir_param("ir_p_umap_group_by", "")
  has_cell_options <- identical(tab, "Clonal UMAP") &&
    (is.null(group_by) || !nzchar(group_by))
  if (!has_display && !has_cell_options) {
    return(NULL)
  }
  cerebroSettingsSection(
    "Appearance",
    uiOutput("ir_additional_params_UI"),
    cerebroInfoButton("ir_additional_parameters_info")
  )
})

## Scatter point controls and shared Canvas checkboxes.
output$ir_additional_params_UI <- renderUI({
  if (!has_scRepertoire() || is.null(ir_data_raw())) {
    return(NULL)
  }
  tagList(
    uiOutput("ir_display_panel", class = "cerebro-settings-contents"),
    uiOutput("ir_cell_view_options_UI", class = "cerebro-settings-contents")
  )
})

## ---- Helper: flow controls across the shared top parameter area -------- ##
ir_flow_controls <- function(controls) {
  ir_flow_controls_inline(controls)
}

## ---- Helper: lay controls out side-by-side, wrapping only when needed -- ##
## For the wide right-hand visualization area (not the narrow left column):
## controls sit in one row and share the width, wrapping to the next line only
## when they no longer fit. Each item has a sensible minimum width.
ir_flow_controls_inline <- function(controls, min_width = "160px") {
  controls <- Filter(Negate(is.null), controls)
  if (length(controls) == 0) {
    return(NULL)
  }
  items <- lapply(controls, function(ctrl) {
    div(
      class = "ir-flow-control-item",
      style = sprintf("flex: 1 1 %s; min-width: %s;", min_width, min_width),
      ctrl
    )
  })
  div(
    class = "ir-flow-controls",
    style = "display: flex; flex-wrap: wrap; gap: 0 12px; align-items: flex-end;",
    do.call(tagList, items)
  )
}

## ---- Scatter sample selectors (Scatter tab only) --------------------- ##
output$ir_scatter_settings <- renderUI({
  available_samples <- ir_compare_groups()
  if (length(available_samples) < 2) {
    return(helpText(
      "Clonal scatter compares two groups. Use 'Group by' above to",
      "divide the data into at least two groups."
    ))
  }
  fluidRow(
    column(
      6,
      # selectize = FALSE: see ir_chain — selectize widgets rendered inside a
      # hidden conditionalPanel drop all but the selected option.
      selectInput(
        "ir_scatter_x",
        "Scatter: X axis",
        choices = available_samples,
        selected = available_samples[1],
        selectize = FALSE
      )
    ),
    column(
      6,
      selectInput(
        "ir_scatter_y",
        "Scatter: Y axis",
        choices = available_samples,
        selected = available_samples[2],
        selectize = FALSE
      )
    )
  )
})

## ---- Compare sample selector (Compare tab only) ---------------------- ##
output$ir_compare_settings <- renderUI({
  available_samples <- ir_compare_groups()
  if (length(available_samples) < 2) {
    return(helpText(
      "Clonal comparison needs at least two groups. Use 'Group by'",
      "above to divide the data."
    ))
  }
  fluidRow(
    column(
      12,
      # Tag/token multi-select (selectize). This UI is rendered inside a hidden
      # conditionalPanel (Compare tab not active), so selectize.js initialises
      # while the container is display:none — measuring zero width and rendering
      # its dropdown/tokens wrong, and historically dropping non-selected
      # choices. The inline script below re-syncs and refreshes the selectize
      # instance the first time the control becomes visible (Shiny fires
      # `shiny:visualchange` on visibility changes), which fixes the layout and
      # restores all choices without falling back to the plain listbox.
      selectInput(
        "ir_compare_samples",
        "Groups to compare (select ≥ 2):",
        choices = available_samples,
        multiple = TRUE,
        selected = available_samples[1:min(2, length(available_samples))],
        selectize = TRUE
      ),
      tags$script(HTML(
        "(function() {
           var el = document.getElementById('ir_compare_samples');
           if (!el) return;
           function refresh() {
             if (el.selectize && el.offsetParent !== null) {
               el.selectize.sync();
               el.selectize.refreshOptions(false);
             }
           }
           $(el).on('shiny:visualchange', refresh);
           // Also refresh once shortly after render in case it is already shown.
           setTimeout(refresh, 0);
         })();"
      ))
    )
  )
})

## ---- Available gene-segment families (for vizGenes x.axis) ------------ ##
ir_gene_families <- reactive({
  raw <- ir_data_raw()
  fams <- c(
    "TRAV",
    "TRAJ",
    "TRBV",
    "TRBD",
    "TRBJ",
    "TRGV",
    "TRGJ",
    "TRDV",
    "TRDJ",
    "IGHV",
    "IGHD",
    "IGHJ",
    "IGKV",
    "IGKJ",
    "IGLV",
    "IGLJ"
  )
  if (is.null(raw)) {
    return(fams)
  }
  all_ct <- paste(unlist(lapply(raw, function(d) d$CTgene)), collapse = ";")
  present <- fams[vapply(fams, function(f) grepl(f, all_ct), logical(1))]
  if (length(present) == 0) fams else present
})

## ---- Function-specific parameter panels (driven by IR_PARAM_SPEC) ----- ##
## The top bar gets plot-defining controls; low-frequency controls listed in
## IR_MORE_PARAM_IDS render in the drawer. Dynamic choices are shared so the
## two surfaces cannot drift.
ir_analysis_spec <- function(tab, more = FALSE) {
  if (
    is.null(tab) || !exists("IR_PARAM_SPEC") || is.null(IR_PARAM_SPEC[[tab]])
  ) {
    return(list())
  }
  spec <- IR_PARAM_SPEC[[tab]]

  # Append the generic "Order groups" control on tabs whose scRepertoire
  # function accepts order.by (declared once in param_spec.R).
  if (
    exists("IR_ORDER_BY_TABS") &&
      exists("IR_ORDER_BY_PARAM") &&
      tab %in% IR_ORDER_BY_TABS
  ) {
    spec <- c(spec, list(IR_ORDER_BY_PARAM))
  }

  is_more <- vapply(
    spec,
    function(p) p$id %in% IR_MORE_PARAM_IDS,
    logical(1)
  )
  spec[is_more == more]
}

ir_analysis_panel <- function(more = FALSE) {
  spec <- ir_analysis_spec(input$ir_tabs, more)
  if (length(spec) == 0) {
    if (more) {
      return(helpText("No additional analysis settings for this plot."))
    }
    return(NULL)
  }

  groups <- tryCatch(getGroups(), error = function(e) character(0))
  genes <- ir_gene_families()

  controls <- lapply(spec, function(p) {
    if (identical(p$type, "numeric")) {
      return(numericInput(
        p$id,
        p$label,
        value = p$value,
        min = p$min,
        max = p$max,
        step = p$step
      ))
    }
    if (identical(p$type, "checkbox")) {
      return(checkboxInput(p$id, p$label, value = isTRUE(p$value)))
    }
    if (identical(p$type, "text")) {
      return(textInput(p$id, p$label, value = p$value))
    }
    # select
    choices <- p$choices
    selected <- p$value
    if (identical(choices, "<<groups>>")) {
      choices <- c("None" = "", groups)
    } else if (identical(choices, "<<genes>>")) {
      choices <- genes
    } else if (identical(choices, "<<property_methods>>")) {
      # detected at runtime (immApex availability)
      choices <- names(available_property_methods())
      if (is.null(selected) || !selected %in% choices) {
        selected <- choices[1]
      }
    } else if (identical(choices, "<<receptors>>")) {
      # TCR / BCR — only the receptor classes present in the data.
      choices <- ir_receptor_types()
      if (is.null(selected) || !selected %in% choices) {
        selected <- if (length(choices) > 0) choices[1] else NULL
      }
    } else if (identical(choices, "<<projections>>")) {
      # cell projections (UMAP/tSNE) available in the data set; default first.
      choices <- tryCatch(availableProjections(), error = function(e) {
        character(0)
      })
      if (is.null(selected) || !selected %in% choices) {
        selected <- if (length(choices) > 0) choices[1] else NULL
      }
    }
    selectInput(
      p$id,
      p$label,
      choices = choices,
      selected = selected,
      selectize = FALSE
    )
  })

  if (more) {
    do.call(tagList, controls)
  } else {
    ir_flow_controls(controls)
  }
}

output$ir_primary_param_panel <- renderUI({
  ir_analysis_panel(more = FALSE)
})

output$ir_more_analysis_UI <- renderUI({
  if (!has_scRepertoire() || is.null(ir_data_raw())) {
    return(NULL)
  }
  ir_analysis_panel(more = TRUE)
})

## ---- Reactive: number of samples -------------------------------------- ##
n_samples <- reactive({
  data <- ir_data()
  if (is.null(data)) 0L else length(data)
})

## ---- Scatter display options ------------------------------------------ ##
## Renders the effective display controls applicable to the current tab. The
## settings drawer owns the section and scrolling, so this stays flat.
output$ir_display_panel <- renderUI({
  tab <- input$ir_tabs
  if (!exists("ir_display_params_for")) {
    return(NULL)
  }
  spec <- ir_display_params_for(tab)
  if (length(spec) == 0) {
    return(NULL)
  }
  if (identical(tab, "Clonal UMAP")) {
    appearance <- current_scatter_defaults()
    spec <- lapply(spec, function(p) {
      if (identical(p$id, "ir_d_point_size")) {
        p$value <- appearance$point_size
        p$max <- 20
        p$step <- 1
      }
      if (identical(p$id, "ir_d_alpha")) {
        p$value <- appearance$point_opacity
        p$step <- 0.1
      }
      if (identical(p$id, "ir_d_percentage_cells_to_show")) {
        p$value <- appearance$percentage_cells_to_show
      }
      p
    })
  }

  controls <- lapply(spec, function(p) {
    if (identical(p$type, "slider")) {
      return(sliderInput(
        p$id,
        p$label,
        min = p$min,
        max = p$max,
        step = p$step,
        value = p$value
      ))
    }
    if (identical(p$type, "numeric")) {
      return(numericInput(
        p$id,
        p$label,
        value = p$value,
        min = p$min,
        max = p$max,
        step = p$step
      ))
    }
    if (identical(p$type, "select")) {
      return(selectInput(
        p$id,
        p$label,
        choices = p$choices,
        selected = p$value,
        selectize = FALSE
      ))
    }
    # text
    textInput(p$id, p$label, value = p$value)
  })

  do.call(tagList, controls)
})

output$ir_cell_view_options_UI <- renderUI({
  if (!identical(input$ir_tabs, "Clonal UMAP")) {
    return(NULL)
  }
  group_by <- ir_param("ir_p_umap_group_by", "")
  if (!is.null(group_by) && nzchar(group_by)) {
    return(NULL)
  }
  tagList(
    checkboxInput(
      "ir_clonalUMAP_group_labels",
      "Group labels",
      value = TRUE
    ),
    checkboxInput(
      "ir_clonalUMAP_point_border",
      "Draw border around cells",
      value = FALSE
    ),
    checkboxInput(
      "ir_clonalUMAP_keep_square",
      "Keep plots square",
      value = FALSE
    )
  )
})

## ---- Reactive: current display parameter values ----------------------- ##
## Collects the live display-control values for the current tab, falling back
## to each param's declared default when the input is absent (e.g. a control
## not rendered on the current tab).
ir_display_params <- reactive({
  tab <- input$ir_tabs
  spec <- if (exists("ir_display_params_for")) {
    ir_display_params_for(tab)
  } else {
    list()
  }
  if (identical(tab, "Clonal UMAP")) {
    appearance <- current_scatter_defaults()
    spec <- lapply(spec, function(p) {
      if (identical(p$id, "ir_d_point_size")) {
        p$value <- appearance$point_size
        p$max <- 20
        p$step <- 1
      }
      if (identical(p$id, "ir_d_alpha")) {
        p$value <- appearance$point_opacity
        p$step <- 0.1
      }
      if (identical(p$id, "ir_d_percentage_cells_to_show")) {
        p$value <- appearance$percentage_cells_to_show
      }
      p
    })
  }
  vals <- list()
  for (p in spec) {
    v <- input[[p$id]]
    vals[[p$id]] <- if (
      is.null(v) || (is.character(v) && !nzchar(v) && p$type != "text")
    ) {
      p$value
    } else {
      v
    }
  }
  vals
})

## ---- Group filters (settings drawer) ---------------------------------- ##
## Shared group-filter chips for the Clonal UMAP. Other tabs show a short note.
output$ir_group_filters_UI <- renderUI({
  if (!has_scRepertoire() || is.null(ir_data_raw())) {
    return(NULL)
  }
  if (!identical(input$ir_tabs, "Clonal UMAP")) {
    return(helpText("Group filters apply to the Clonal UMAP tab."))
  }
  groups <- tryCatch(getGroups(), error = function(e) character(0))
  if (length(groups) == 0) {
    return(helpText("No grouping columns available to filter by."))
  }
  filters <- lapply(groups, function(g) {
    lvls <- tryCatch(getGroupLevels(g), error = function(e) character(0))
    colors <- tryCatch(
      unname(reactive_colors()[[g]][lvls]),
      error = function(e) NULL
    )
    if (is.null(colors) || length(colors) != length(lvls)) {
      colors <- cerebro_group_colors(length(lvls))
    }
    groupFilterControl(
      paste0("ir_group_filter_", g),
      g,
      lvls,
      colors
    )
  })
  div(
    class = "cerebro-group-filters",
    div(class = "cv-filters-row", filters)
  )
})

## ---- Barcodes to show in the Clonal UMAP (from Group filters) ---------- ##
## Reads the per-group controls and returns the barcodes whose cells pass
## every active filter. Returns NULL when no filtering is in effect (every
## level of every group still selected) so the renderer shows all cells.
## Overrides the NULL default defined in data.R.
ir_umap_cells_to_show <- reactive({
  groups <- tryCatch(getGroups(), error = function(e) character(0))
  md <- tryCatch(getMetaData(), error = function(e) NULL)
  if (
    length(groups) == 0 ||
      is.null(md) ||
      !("cell_barcode" %in% colnames(md))
  ) {
    return(NULL)
  }
  keep <- rep(TRUE, nrow(md))
  any_filter <- FALSE
  for (g in groups) {
    sel <- input[[paste0("ir_group_filter_", g)]]
    if (is.null(sel) || !(g %in% colnames(md))) {
      next
    }
    all_lvls <- tryCatch(getGroupLevels(g), error = function(e) character(0))
    # Only treat it as an active filter when the user has deselected something.
    if (length(sel) < length(all_lvls)) {
      any_filter <- TRUE
      keep <- keep & (as.character(md[[g]]) %in% sel)
    }
  }
  if (!any_filter) {
    return(NULL)
  }
  as.character(md$cell_barcode[keep])
})

## ---- Info dialogs: explain the parameters shown on the current tab ---- ##
## The top bar and drawer info buttons explain the controls visible on the
## current tab. Text comes from IR_PARAM_DESC so it cannot drift from the UI.

## Render a list of param ids as styled help cards (bold name + plain text).
ir_param_help_cards <- function(ids) {
  ids <- ids[ids %in% names(IR_PARAM_DESC)]
  if (length(ids) == 0) {
    return(tags$p(
      style = "color:#888;",
      "No adjustable parameters on this tab."
    ))
  }
  cards <- lapply(ids, function(id) {
    label <- IR_PARAM_LABELS[[id]] %||% id
    div(
      class = "ir-help-card",
      div(class = "ir-help-card-title", label),
      div(class = "ir-help-card-body", IR_PARAM_DESC[[id]])
    )
  })
  ## Card styling lives in www/custom.css (.ir-help-card*), driven by theme
  ## tokens; no inline <style> needed here.
  div(class = "ir-help-cards", do.call(tagList, cards))
}

## Lookup: input id -> human label (from IR_PARAM_SPEC / display / globals).
IR_PARAM_LABELS <- local({
  labs <- list(
    ir_cloneCall = "Clone call",
    ir_chain = "Chain",
    ir_groupBy = "Group by"
  )
  for (tab in names(IR_PARAM_SPEC)) {
    for (p in IR_PARAM_SPEC[[tab]]) {
      labs[[p$id]] <- sub(":\\s*$", "", p$label)
    }
  }
  for (p in IR_DISPLAY_SCATTER) {
    labs[[p$id]] <- sub(":\\s*$", "", p$label)
  }
  labs[[IR_ORDER_BY_PARAM$id]] <- sub(":\\s*$", "", IR_ORDER_BY_PARAM$label)
  labs
})

## Main parameters info: global controls + top-bar analysis params.
observeEvent(input$ir_main_parameters_info, {
  tab <- input$ir_tabs
  spec_ids <- vapply(
    ir_analysis_spec(tab, more = FALSE),
    function(p) p$id,
    character(1)
  )
  ids <- c(ir_visible_global_ids(tab), spec_ids)
  showModal(modalDialog(
    title = paste0("Main parameters", if (!is.null(tab)) paste0(" — ", tab)),
    ir_param_help_cards(ids),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"
  ))
})

observeEvent(input$ir_more_analysis_info, {
  tab <- input$ir_tabs
  ids <- vapply(
    ir_analysis_spec(tab, more = TRUE),
    function(p) p$id,
    character(1)
  )
  showModal(modalDialog(
    title = paste0("Analysis settings", if (!is.null(tab)) paste0(" — ", tab)),
    ir_param_help_cards(ids),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"
  ))
})

## Additional parameters info: the display options for this tab.
observeEvent(input$ir_additional_parameters_info, {
  tab <- input$ir_tabs
  ids <- if (exists("ir_display_params_for")) {
    vapply(ir_display_params_for(tab), function(p) p$id, character(1))
  } else {
    character(0)
  }
  showModal(modalDialog(
    title = "Additional parameters — display options",
    ir_param_help_cards(ids),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"
  ))
})

## Group filters info.
observeEvent(input$ir_group_filters_info, {
  showModal(modalDialog(
    title = "Group filters",
    tags$p(
      "Restrict the Clonal UMAP to a subset of cells by metadata column ",
      "(sample, condition, cell type, ...). Deselect levels to hide those ",
      "cells; with everything selected, all cells are shown."
    ),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"
  ))
})

## Keep drawer controls alive while the drawer is closed.
outputOptions(output, "ir_additional_params_UI", suspendWhenHidden = FALSE)
outputOptions(output, "ir_appearance_section_UI", suspendWhenHidden = FALSE)
outputOptions(output, "ir_group_filters_UI", suspendWhenHidden = FALSE)
outputOptions(output, "ir_display_panel", suspendWhenHidden = FALSE)
outputOptions(output, "ir_cell_view_options_UI", suspendWhenHidden = FALSE)
outputOptions(output, "ir_more_analysis_UI", suspendWhenHidden = FALSE)
