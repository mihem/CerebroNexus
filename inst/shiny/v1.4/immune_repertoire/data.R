## ---- Reactive: raw repertoire data (as stored in crb) ------------------ ##
ir_data_raw <- reactive({
  req(!is.null(data_set()))
  data <- getImmuneRepertoire()
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(NULL)
  }
  data
})

## ---- Standard scRepertoire columns (not usable as grouping) ----------- ##
ir_scr_cols <- c(
  "barcode",
  "CTgene",
  "CTnt",
  "CTaa",
  "CTstrict",
  "clonalProportion",
  "clonalFrequency",
  "cloneSize",
  "Frequency",
  "frequency",
  "cloneType"
)

## ---- Join cell metadata onto every IR row by barcode ------------------ ##
## The IR data.frames carry only scRepertoire columns (barcode, CT*). Any
## biological grouping (sample, condition, treatment, cell type, ...) lives in
## the data set's cell metadata. We attach it here by `cell_barcode` so the
## module can group/split by ANY metadata column, not just whatever columns a
## data producer happened to embed in the IR table.
ir_data_annotated <- reactive({
  data <- ir_data_raw()
  if (is.null(data)) {
    return(NULL)
  }
  md <- tryCatch(getMetaData(), error = function(e) NULL)
  if (is.null(md) || !("cell_barcode" %in% colnames(md))) {
    return(data) # nothing to join; fall back to raw IR data
  }
  # metadata columns that don't already exist in the IR tables
  meta_cols <- setdiff(colnames(md), "cell_barcode")
  lapply(data, function(df) {
    if (is.null(df) || !("barcode" %in% colnames(df))) {
      return(df)
    }
    add <- setdiff(meta_cols, colnames(df))
    if (length(add) == 0) {
      return(df)
    }
    idx <- match(df$barcode, md$cell_barcode)
    n_miss <- sum(is.na(idx))
    if (n_miss > 0) {
      warning(sprintf(
        paste0(
          "[IR] %d / %d clonotype barcodes not found in cell metadata; ",
          "grouping/splitting by metadata columns may be incomplete. ",
          "Check that IR barcodes match the cell barcodes (e.g. the '-1' suffix)."
        ),
        n_miss,
        length(idx)
      ))
    }
    for (col in add) {
      df[[col]] <- md[[col]][idx]
    }
    df
  })
})

## ---- Reactive: repertoire data --------------------------------------- ##
## Returns the metadata-annotated repertoire list as loaded. Grouping is not
## done here: scRepertoire's own group.by rbinds the list and re-splits on the
## chosen column (.groupList), so an in-app re-split would only duplicate — with
## a narrower, sample-only column set — what group.by already does. Comparison
## units are therefore expressed solely through ir_groupBy / group.by.
ir_data <- reactive({
  ir_data_annotated()
})

## ---- Helper: read a dynamic function-specific parameter --------------- ##
## The function-specific controls (IR_PARAM_SPEC) are rendered into a dynamic
## panel, so an input may be absent on tabs where the parameter doesn't apply.
## Returns `default` when the input is missing/empty.
ir_param <- function(id, default = NULL) {
  v <- input[[id]]
  if (is.null(v)) default else v
}

## ---- Resolve the generic "Order groups" (order.by) value -------------- ##
## Maps the ir_p_order_by control to scRepertoire's order.by argument: the
## empty default becomes NULL (scRepertoire's own ordering), otherwise the
## chosen value (e.g. "alphanumeric") is passed through.
ir_order_by <- function() {
  v <- input[["ir_p_order_by"]]
  if (is.null(v) || !nzchar(v)) NULL else v
}

## ---- Parse the Homeostasis clone-size thresholds ---------------------- ##
## clonalHomeostasis' cloneSize is a *named* numeric vector of upper bounds
## (Rare < Small < ... < Hyperexpanded). The UI takes them as a comma-separated
## list of 5 increasing numbers; this builds the named vector. Returns NULL on
## anything malformed so scRepertoire falls back to its own default.
IR_CLONE_SIZE_NAMES <- c(
  "Rare",
  "Small",
  "Medium",
  "Large",
  "Hyperexpanded"
)
IR_CLONE_SIZE_DEFAULT <- setNames(
  c(1e-04, 0.001, 0.01, 0.1, 1),
  IR_CLONE_SIZE_NAMES
)
ir_clone_size <- function() {
  v <- input[["ir_p_clone_size"]]
  if (is.null(v) || !nzchar(v)) {
    return(IR_CLONE_SIZE_DEFAULT)
  }
  nums <- suppressWarnings(as.numeric(trimws(strsplit(v, ",")[[1]])))
  if (
    length(nums) != length(IR_CLONE_SIZE_NAMES) ||
      any(is.na(nums)) ||
      is.unsorted(nums, strictly = TRUE)
  ) {
    return(IR_CLONE_SIZE_DEFAULT)
  }
  setNames(nums, IR_CLONE_SIZE_NAMES)
}

## ---- Comparable groups for Scatter / Compare ------------------------- ##
## clonalScatter (x.axis/y.axis) and clonalCompare (samples) operate on the
## *names of the groups that group.by produces*. With group.by = None the
## groups are the list elements (samples); with group.by = <column> they are
## that column's levels. This reactive returns those group names so the Scatter
## X/Y and Compare selectors stay in sync with the active grouping.
ir_compare_groups <- reactive({
  data <- ir_data()
  if (is.null(data)) {
    return(character(0))
  }
  gb <- input$ir_groupBy
  if (is.null(gb) || !nzchar(gb)) {
    return(names(data))
  }
  vals <- unique(unlist(lapply(data, function(df) {
    if (gb %in% colnames(df)) as.character(df[[gb]]) else character(0)
  })))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0) names(data) else sort(vals)
})

## ---- Reactive: parameters --------------------------------------------- ##
ir_params <- reactive({
  gb <- input$ir_groupBy
  if (is.null(gb) || gb == "") {
    gb <- NULL
  }
  list(
    cloneCall = input$ir_cloneCall,
    chain = input$ir_chain,
    groupBy = gb
  )
})

## ---- Reactive: number of groups for faceted plots --------------------- ##
n_groups <- reactive({
  gb <- ir_params()$groupBy
  if (is.null(gb)) {
    return(1L)
  }
  data <- ir_data()
  if (is.null(data)) {
    return(1L)
  }
  lvls <- unique(unlist(lapply(data, function(df) {
    if (gb %in% names(df)) unique(as.character(df[[gb]])) else character(0)
  })))
  max(1L, length(lvls))
})

## ---- Dynamic gene parameter for vizGenes/percentGeneUsage ------------- ##
default_gene_family <- reactive({
  chains <- detect_chains(ir_data())
  tcr_chains <- intersect(chains, c("TRA", "TRB", "TRG", "TRD"))
  bcr_chains <- intersect(chains, c("IGH", "IGK", "IGL"))
  if (length(tcr_chains) > 0 && "TRB" %in% tcr_chains) {
    return("TRBV")
  }
  if (length(tcr_chains) > 0) {
    return(paste0(tcr_chains[1], "V"))
  }
  if (length(bcr_chains) > 0 && "IGH" %in% bcr_chains) {
    return("IGHV")
  }
  if (length(bcr_chains) > 0) {
    return(paste0(bcr_chains[1], "V"))
  }
  "TRBV"
})

## ---- Resolve chain: for functions that don't accept "both" ------------ ##
specific_chain <- reactive({
  ch <- input$ir_chain
  if (is.null(ch) || ch == "both") {
    chains <- detect_chains(ir_data())
    if ("TRB" %in% chains) {
      return("TRB")
    }
    if (length(chains) > 0) {
      return(chains[1])
    }
    return("TRB")
  }
  ch
})

## ---- Count unique genes for dynamic plot height ----------------------- ##
n_genes <- reactive({
  data <- ir_data()
  if (is.null(data)) {
    return(0L)
  }
  gene_family <- default_gene_family()
  # Gather all gene values across samples
  all_genes <- unique(unlist(lapply(data, function(df) {
    # CTgene has format like "TRBV1.TRBJ2" — extract the gene family portion
    ct <- as.character(df$CTgene)
    ct <- ct[!is.na(ct)]
    # Split by "." and keep segments matching the gene family prefix
    segments <- unlist(strsplit(ct, "[._]"))
    segments[grepl(paste0("^", gene_family), segments, ignore.case = TRUE)]
  })))
  length(all_genes)
})

ir_plot_height <- function(facet_mode = c("none", "grid", "wrap")) {
  facet_mode <- match.arg(facet_mode)
  n <- n_genes()
  ng <- n_groups()
  base_h <- max(450, min(n * 25, 2500))
  if (ng <= 1 || facet_mode == "none") {
    return(base_h)
  }
  if (facet_mode == "grid") {
    # facet_grid(Group ~ .): each group stacked vertically
    return(base_h * ng)
  }
  # facet_wrap: ggplot default ncol = ceiling(sqrt(n))
  ncol <- ceiling(sqrt(ng))
  nrow <- ceiling(ng / ncol)
  base_h * nrow
}

##----------------------------------------------------------------------------##
## Clonal UMAP data layer
##----------------------------------------------------------------------------##

## ---- Chains that define each receptor class --------------------------- ##
IR_TCR_CHAINS <- c("TRA", "TRB", "TRG", "TRD")
IR_BCR_CHAINS <- c("IGH", "IGK", "IGL")

## ---- Which receptor classes are present in the data ------------------- ##
## Returns a named vector ("TCR" / "BCR") of the receptor types actually
## detected, so the Clonal UMAP selector only offers what exists. The names
## are the labels shown to the user; values feed ir_umap_chains().
ir_receptor_types <- reactive({
  chains <- tryCatch(detect_chains(ir_data()), error = function(e) character(0))
  types <- character(0)
  if (length(intersect(chains, IR_TCR_CHAINS)) > 0) {
    types <- c(types, "TCR" = "TCR")
  }
  if (length(intersect(chains, IR_BCR_CHAINS)) > 0) {
    types <- c(types, "BCR" = "BCR")
  }
  types
})

## ---- Chains belonging to the selected receptor type ------------------- ##
ir_umap_chains <- function(receptor) {
  if (identical(receptor, "BCR")) IR_BCR_CHAINS else IR_TCR_CHAINS
}

## ---- Clone-size bin breaks / labels (scRepertoire cloneSize defaults) -- ##
## A clone's size = number of cells carrying that clonotype (within the
## selected receptor). Cells are binned into the standard expansion levels.
IR_CLONE_BINS <- c(0, 1, 5, 20, 100, Inf)
IR_CLONE_LABELS <- c(
  "Single (0 < X <= 1)",
  "Small (1 < X <= 5)",
  "Medium (5 < X <= 20)",
  "Large (20 < X <= 100)",
  "Hyperexpanded (100 < X)"
)

## ---- Which CT* column a cloneCall maps to ----------------------------- ##
ir_clonecall_col <- function(cloneCall) {
  switch(
    cloneCall %||% "gene",
    "gene" = "CTgene",
    "nt" = "CTnt",
    "aa" = "CTaa",
    "strict" = "CTstrict",
    "CTgene"
  )
}

## ---- Clonal UMAP data: coords + per-cell expansion level --------------- ##
## Joins the chosen projection's UMAP coordinates (barcode-indexed) with each
## cell's clone-expansion level, restricted to the selected receptor (TCR/BCR).
## Returns a data.frame (x, y, expansion, barcode) or NULL when it cannot be
## built (no projection, no data for the receptor, no overlapping barcodes).
##
##   projection : a name from availableProjections()
##   receptor   : "TCR" | "BCR"
##   cloneCall  : "gene" | "nt" | "aa" | "strict" (clone identity column)
##   show_all   : when TRUE, also include every other cell in the projection
##                with expansion = NA (drawn as a grey background by the
##                renderer), so the receptor cells are shown in context.
##   cells      : optional character vector of barcodes to restrict to (e.g.
##                from the Group filters); NULL = all cells in the projection.
ir_clonal_umap_data <- function(
  projection,
  receptor,
  cloneCall = "gene",
  show_all = TRUE,
  cells = NULL
) {
  if (is.null(projection) || !nzchar(projection)) {
    return(NULL)
  }
  if (
    !(projection %in%
      tryCatch(availableProjections(), error = function(e) character(0)))
  ) {
    return(NULL)
  }
  coords <- tryCatch(getProjection(projection), error = function(e) NULL)
  if (is.null(coords) || nrow(coords) == 0) {
    return(NULL)
  }
  # Restrict to the requested cells (group filters) up front, so both the
  # coloured receptor cells and the grey background respect the filter.
  if (!is.null(cells)) {
    coords <- coords[rownames(coords) %in% cells, , drop = FALSE]
    if (nrow(coords) == 0) {
      return(NULL)
    }
  }

  data <- ir_data_annotated()
  if (is.null(data)) {
    return(NULL)
  }
  clone_col <- ir_clonecall_col(cloneCall)
  keep_chains <- ir_umap_chains(receptor)

  # Flatten the per-sample IR list into one barcode -> clonotype table,
  # restricted to rows whose CTstrict/CTgene references one of the receptor's
  # chains. Each row is one cell (scRepertoire keeps one row per barcode).
  rows <- lapply(data, function(df) {
    if (is.null(df) || !all(c("barcode", clone_col) %in% colnames(df))) {
      return(NULL)
    }
    chain_ref <- if ("CTstrict" %in% colnames(df)) {
      as.character(df$CTstrict)
    } else {
      as.character(df[[clone_col]])
    }
    in_receptor <- vapply(
      chain_ref,
      function(s) {
        any(vapply(
          keep_chains,
          function(ch) grepl(ch, s, fixed = TRUE),
          logical(1)
        ))
      },
      logical(1)
    )
    df <- df[in_receptor, , drop = FALSE]
    if (nrow(df) == 0) {
      return(NULL)
    }
    data.frame(
      barcode = as.character(df$barcode),
      clone = as.character(df[[clone_col]]),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  has_receptor <- !is.null(rows) && nrow(rows) > 0
  if (has_receptor) {
    # Clone size = number of cells sharing the clonotype; bin into expansion levels.
    rows <- rows[!is.na(rows$clone) & nzchar(rows$clone), , drop = FALSE]
  }
  if (!has_receptor || nrow(rows) == 0) {
    # No receptor cells. With show_all we can still draw the grey background;
    # otherwise there is nothing to plot.
    if (!isTRUE(show_all)) {
      return(NULL)
    }
    rows <- data.frame(
      barcode = character(0),
      clone = character(0),
      stringsAsFactors = FALSE
    )
    has_receptor <- FALSE
  } else {
    sizes <- table(rows$clone)
    rows$size <- as.integer(sizes[rows$clone])
    rows$expansion <- cut(
      rows$size,
      breaks = IR_CLONE_BINS,
      labels = IR_CLONE_LABELS,
      right = TRUE,
      include.lowest = TRUE
    )
  }

  coord_bc <- rownames(coords)

  # Coloured layer: receptor cells with an expansion level, joined to coords.
  if (has_receptor) {
    idx <- match(rows$barcode, coord_bc)
    ok <- !is.na(idx)
    rows <- rows[ok, , drop = FALSE]
    idx <- idx[ok]
  } else {
    idx <- integer(0)
  }
  if (length(idx) == 0 && !isTRUE(show_all)) {
    return(NULL)
  }
  coloured <- if (length(idx) > 0) {
    xy <- coords[idx, 1:2, drop = FALSE]
    data.frame(
      x = as.numeric(xy[[1]]),
      y = as.numeric(xy[[2]]),
      expansion = factor(rows$expansion, levels = IR_CLONE_LABELS),
      barcode = rows$barcode,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  # Background layer: every other cell in the projection, expansion = NA, so the
  # renderer can draw them in grey. Only when show_all is requested.
  background <- NULL
  if (isTRUE(show_all)) {
    bg_mask <- !(coord_bc %in%
      (if (length(idx) > 0) rows$barcode else character(0)))
    if (any(bg_mask)) {
      xy_bg <- coords[bg_mask, 1:2, drop = FALSE]
      background <- data.frame(
        x = as.numeric(xy_bg[[1]]),
        y = as.numeric(xy_bg[[2]]),
        expansion = factor(NA, levels = IR_CLONE_LABELS),
        barcode = coord_bc[bg_mask],
        stringsAsFactors = FALSE
      )
    }
  }

  out <- rbind(background, coloured)
  if (is.null(out) || nrow(out) == 0) {
    return(NULL)
  }
  out
}
