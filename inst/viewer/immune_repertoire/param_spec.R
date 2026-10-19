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
##             "<<receptors>>" / "<<projections>>"
##   value   : default value (must match the scRepertoire default)
##   min/max/step : for numeric
## ---------------------------------------------------------------------------

## ---- Global control visibility ----------------------------------------- ##
IR_GLOBAL_CONTROL_HIDDEN <- list(
  # Clonal UMAP colours by clone size and uses its own Receptor selector; the
  # global Clone call is intentionally fixed there so hidden state cannot affect
  # the plot. The other tabs either do not use cloneCall or enforce their own.
  ir_cloneCall = c("Clonal UMAP", "Isotype", "Clone Sharing"),
  ir_chain = c("Clonal UMAP"),
  ir_groupBy = c("Clonal UMAP")
)

ir_global_control_visible <- function(id, tab) {
  hidden <- IR_GLOBAL_CONTROL_HIDDEN[[id]]
  if (is.null(hidden)) {
    hidden <- character(0)
  }
  is.null(tab) || !(tab %in% hidden)
}

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
      id = "ir_p_umap_group_by",
      label = "Group results by:",
      type = "select",
      choices = "<<groups>>",
      value = ""
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

  "Paired Scatter" = list(
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
  ),

  "Homeostasis" = list(
    list(
      id = "ir_p_clone_size",
      label = "Clone size thresholds:",
      type = "text",
      value = "0.0001, 0.001, 0.01, 0.1, 1"
    )
  ),

  "Abundance" = list(
    list(
      id = "ir_p_scale",
      label = "Scale (proportion):",
      type = "checkbox",
      value = FALSE
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
  )
)

## ---------------------------------------------------------------------------
## IR display parameters that have a consistent consumer on scatter plots.
##
## Font and legend controls were removed: they affected only some ggplot paths
## and were no-ops on shared Canvas / several Plotly views. More settings should
## expose only controls that behave consistently wherever they are shown.
## ---------------------------------------------------------------------------

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

## Tabs whose plots are point clouds (scatter-type): get the scatter extras.
IR_SCATTER_TABS <- c("Clonal UMAP", "Paired Scatter")

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
IR_ORDER_BY_TABS <- c("Abundance", "Diversity", "Homeostasis", "Compare")

## Low-frequency analysis controls live in More settings. Everything else in
## IR_PARAM_SPEC remains in the top bar because it directly defines the plot.
IR_MORE_PARAM_IDS <- c("ir_p_umap_show_all", "ir_p_n_boots", "ir_p_order_by")

## Assemble the display params applicable to a given tab.
ir_display_params_for <- function(tab) {
  params <- list()
  if (!is.null(tab) && tab %in% IR_SCATTER_TABS) {
    params <- c(params, IR_DISPLAY_SCATTER)
  }
  if (identical(tab, "Clonal UMAP")) {
    params <- c(
      params,
      list(list(
        id = "ir_d_percentage_cells_to_show",
        label = "Show % of cells:",
        type = "slider",
        value = 100,
        min = 10,
        max = 100,
        step = 10
      ))
    )
  }
  params
}

## ---------------------------------------------------------------------------
## IR_PARAM_DESC — plain-language help for the settings-drawer controls,
## keyed by input id.
##
## Single source of truth for the drawer info dialogs (see ir_param_help_cards
## in settings.R): the low-frequency analysis params (IR_MORE_PARAM_IDS) and
## the display options. Written for a biologist who is not a scRepertoire user:
## say what the control does and how reading the plot changes, not the API.
## ---------------------------------------------------------------------------
IR_PARAM_DESC <- list(
  ir_p_umap_show_all = "When on, every cell is drawn: cells without the selected receptor appear light grey, so the coloured (expanded) clones stand out in context. When off, only cells carrying the receptor are shown.",
  ir_p_n_boots = "How many bootstrap resamples to average over for the diversity estimate and its spread. More iterations give a smoother, more stable estimate but take longer.",
  ir_p_order_by = "The order the groups appear along the axis. Default keeps scRepertoire's own ordering; Alphanumeric sorts them by name.",

  ## ---- Display options ----
  ir_d_point_size = "Diameter of the scatter points.",
  ir_d_alpha = "Point opacity (0 = transparent, 1 = solid). Lower values help when points overlap heavily.",
  ir_d_percentage_cells_to_show = "Percentage of cells sampled for the clonal projection. Lower values can improve performance for large datasets."
)
