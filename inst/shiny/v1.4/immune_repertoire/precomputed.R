##----------------------------------------------------------------------------##
## Precomputed repertoire tables — render straight from the scRepertoire tables
## stored in the .crb (see computeRepertoireMetrics), so the figures show
## scRepertoire's own numbers with no runtime computation. The visualization
## renderers look a table up with ir_pc_lookup(); when one exists (by-sample
## view, clone def / chain inside the precomputed grid) they draw it with the
## ir_pc_plot_* helpers below, otherwise they fall back to the native irn_*.
##----------------------------------------------------------------------------##

## Precomputed store for the loaded data set (empty list when none / old .crb).
ir_precomputed <- reactive({
  getImmuneRepertoirePrecomputed()
})

## Look a stored table up by metric + clone definition + chain (+ overlap
## method). Returns NULL when absent so the caller can fall back to native.
ir_pc_lookup <- function(
  metric,
  cloneCall = NULL,
  chain = NULL,
  method = NULL
) {
  store <- ir_precomputed()
  if (length(store) == 0) {
    return(NULL)
  }
  parts <- c(metric, cloneCall, chain, method)
  key <- paste(parts, collapse = "|")
  store[[key]]
}

## ---- shared plotting helpers ---------------------------------------------- ##

## Melt a samples x category matrix to long, keeping the column order.
.ir_pc_melt <- function(m, category = "category") {
  m <- as.matrix(m)
  cats <- colnames(m)
  out <- data.frame(
    sample = rep(rownames(m), times = ncol(m)),
    category = factor(rep(cats, each = nrow(m)), levels = cats),
    value = as.numeric(m),
    stringsAsFactors = FALSE
  )
  names(out)[2] <- category
  out
}

ir_pc_plot_homeostasis <- function(tbl) {
  df <- .ir_pc_melt(tbl, "band")
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$sample, y = .data$value, fill = .data$band)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = irn_pal(nlevels(df$band), IR_PALETTE)) +
    ggplot2::labs(x = NULL, y = "Relative abundance", fill = "Clone size") +
    irn_theme()
}

ir_pc_plot_proportion <- function(tbl) {
  df <- .ir_pc_melt(tbl, "bin")
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$sample, y = .data$value, fill = .data$bin)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = irn_pal(nlevels(df$bin), IR_PALETTE)) +
    ggplot2::labs(x = NULL, y = "Clones", fill = "Rank bin") +
    irn_theme()
}

ir_pc_plot_quant <- function(tbl, scale = FALSE) {
  df <- as.data.frame(tbl)
  df$values <- factor(df$values, levels = df$values)
  if (isTRUE(scale)) {
    df$y <- df$contigs / df$total * 100
    ylab <- "Percent of unique clonotypes"
  } else {
    df$y <- df$contigs
    ylab <- "Unique clonotypes"
  }
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$values, y = .data$y, fill = .data$values)
  ) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = irn_pal(nrow(df), IR_PALETTE)) +
    ggplot2::labs(x = NULL, y = ylab) +
    irn_theme()
}

ir_pc_plot_overlap <- function(tbl) {
  m <- as.matrix(tbl)
  samples <- rownames(m)
  df <- expand.grid(
    row = factor(samples, levels = samples),
    col = factor(samples, levels = rev(samples)),
    stringsAsFactors = FALSE
  )
  df$value <- mapply(
    function(r, c) m[as.character(r), as.character(c)],
    df$row,
    df$col
  )
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$row, y = .data$col, fill = .data$value)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(is.na(.data$value), "", sprintf("%.2f", .data$value))
      ),
      size = 3.5
    ) +
    ggplot2::scale_fill_gradient(
      low = "#f7fbff",
      high = "#08519c",
      na.value = "grey92"
    ) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Overlap") +
    irn_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

ir_pc_plot_length <- function(tbl, scale = FALSE) {
  df <- as.data.frame(tbl)
  df$values <- factor(df$values)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$length, fill = .data$values)
  ) +
    ggplot2::geom_bar(
      position = if (isTRUE(scale)) "fill" else "stack",
      width = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values = irn_pal(nlevels(df$values), IR_PALETTE),
      name = "Group"
    ) +
    ggplot2::labs(
      x = "CDR3 length",
      y = if (isTRUE(scale)) "Proportion" else "Number of CDR3"
    ) +
    irn_theme()
}

ir_pc_plot_percentAA <- function(tbl) {
  df <- as.data.frame(tbl)
  df$group <- factor(df$group)
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$Position,
      y = .data$Frequency,
      fill = .data$AminoAcid
    )
  ) +
    ggplot2::geom_col(width = 1) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$group)) +
    ggplot2::scale_fill_manual(
      values = irn_pal(length(unique(df$AminoAcid)), IR_PALETTE),
      name = "AA"
    ) +
    ggplot2::labs(x = "Position", y = "Relative frequency") +
    irn_theme()
}
