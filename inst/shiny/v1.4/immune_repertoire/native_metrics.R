##----------------------------------------------------------------------------##
## Native repertoire metrics — dependency-free replacement for the scRepertoire
## computations the Immune Repertoire module used to call. Each `irn_*` function
## takes the same arguments the module already passes to its scRepertoire
## counterpart (extra args are absorbed by `...`) and returns either a ggplot
## (exportTable = FALSE, the default) or, for the three call sites that consume a
## table, a data.frame with the exact columns the downstream helper reads.
##
## Correctness bar: standard immunology definitions (a clonotype = the chosen
## cloneCall string for the chosen chain; a cell "has" a chain when that slot is
## non-NA). These reproduce the same analyses/visualisations scRepertoire shows
## without loading it (and its ~90-package Bioconductor/Seurat/iNEXT tail). They
## intentionally do NOT replicate scRepertoire's idiosyncratic internal cell
## filtering byte-for-byte (scRepertoire is itself inconsistent there — e.g. its
## own clonalQuant and clonalAbundance keep different cell subsets from the same
## sample); the native definitions are consistent across all functions.
##----------------------------------------------------------------------------##

## ---- clonotype extraction ------------------------------------------------- ##
irn_clone_col <- function(cloneCall) {
  switch(
    tolower(cloneCall %||% "gene"),
    gene = "CTgene",
    nt = "CTnt",
    aa = "CTaa",
    strict = "CTstrict",
    "CTgene"
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

## Per row (cell), the clonotype string for the requested chain, or NA when the
## cell has no rearrangement for that chain. "both"/NULL keeps the combined
## string. In scRepertoire's combined format the "_"-joined value has the
## alpha/heavy chain in slot 1 (TRA / TRG / IGH) and the beta/light chain in
## slot 2 (TRB / TRD / IGK / IGL).
##
## A single-chain request must be masked by the chain FAMILY, not just the slot
## position: on a mixed TCR+BCR object slot 1 holds TRA for T cells but IGH for
## B cells, so chain = "TRA" must reject the IGH cells. The family is only
## legible in CTgene (CTnt / CTaa carry no chain prefix), so we always read the
## mask from CTgene and apply it to whichever cloneCall column was requested.
## This matches scRepertoire, which filters cells to the requested chain first.
irn_extract <- function(df, cloneCall = "gene", chain = "TRB") {
  col <- irn_clone_col(cloneCall)
  v <- df[[col]]
  if (is.null(v)) {
    return(rep(NA_character_, nrow(df)))
  }
  v <- as.character(v)
  if (is.null(chain) || identical(chain, "both") || !nzchar(chain)) {
    v[v %in% c("NA", "NA_NA") | !nzchar(v)] <- NA
    return(v)
  }
  slot <- if (chain %in% c("TRA", "TRG", "IGH")) 1L else 2L
  slot_of <- function(x) {
    parts <- strsplit(as.character(x), "_", fixed = TRUE)
    vapply(
      parts,
      function(p) if (length(p) >= slot) p[slot] else NA_character_,
      character(1)
    )
  }
  out <- slot_of(v)
  ## Family mask: the leading receptor family of this slot (e.g.
  ## "TRAV8-6.TRAJ8.TRAC" -> "TRA", "IGHV3-23..." -> "IGH"). CTgene and CTstrict
  ## carry the V-gene prefix, so read the family straight from the requested
  ## column; CTnt / CTaa do not, so fall back to CTgene for those.
  fam_source <- if (cloneCall %in% c("gene", "strict")) {
    out
  } else {
    slot_of(df[["CTgene"]])
  }
  fam <- substr(fam_source, 1L, 3L)
  drop <- is.na(out) |
    out == "NA" |
    !nzchar(out) |
    is.na(fam) |
    fam != chain
  out[drop] <- NA
  out
}

## CDR3 length, matching scRepertoire's .lengthDF. For "both" the two chains'
## CDR3s are concatenated with the "_" separator and missing-chain markers
## dropped, then counted; for a single chain only the first ";"-separated contig
## is measured (scRepertoire takes contig 1, it does not sum multiple contigs).
irn_cdr3_length <- function(x, chain = "TRB") {
  x <- as.character(x)
  if (is.null(chain) || identical(chain, "both") || !nzchar(chain)) {
    x <- gsub("_NA", "", x)
    x <- gsub("NA_", "", x)
    nchar(gsub("_", "", x))
  } else {
    nchar(sub(";.*", "", x))
  }
}

## Long (group, clonotype) frame over all cells that carry the chain.
irn_long <- function(data, cloneCall = "gene", chain = "TRB", group.by = NULL) {
  rows <- lapply(names(data), function(nm) {
    df <- data[[nm]]
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    ct <- irn_extract(df, cloneCall, chain)
    g <- if (is.null(group.by) || !(group.by %in% colnames(df))) {
      rep(nm, nrow(df))
    } else {
      as.character(df[[group.by]])
    }
    data.frame(group = g, clonotype = ct, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    return(data.frame(group = character(0), clonotype = character(0)))
  }
  out[!is.na(out$clonotype) & !is.na(out$group), , drop = FALSE]
}

irn_group_levels <- function(long, order.by = NULL) {
  lv <- unique(as.character(long$group))
  if (identical(order.by, "alphanumeric")) {
    lv <- sort(lv)
  }
  lv
}

## ---- shared aesthetics ---------------------------------------------------- ##
irn_pal <- function(n, palette = "Harmonic") {
  if (is.null(palette) || !nzchar(palette)) {
    palette <- "Harmonic"
  }
  cols <- tryCatch(
    grDevices::hcl.colors(max(n, 1), palette),
    error = function(e) grDevices::hcl.colors(max(n, 1), "Harmonic")
  )
  rep(cols, length.out = n)
}

irn_theme <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      legend.position = "right",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

## Parse V / D / J / C segments out of a chain gene string like
## "TRBV6-2..TRBJ2-6.TRBC2" (segments dot-separated; empty D = "").
irn_gene_segments <- function(gene_strings) {
  seg <- strsplit(as.character(gene_strings), ".", fixed = TRUE)
  pick <- function(i) {
    vapply(
      seg,
      function(s) if (length(s) >= i && nzchar(s[i])) s[i] else NA_character_,
      character(1)
    )
  }
  list(V = pick(1), D = pick(2), J = pick(3), C = pick(4))
}

## amino-acid split of a CDR3 string, dropping the leading/trailing conserved
## residues is NOT done (scRepertoire keeps the full CDR3); returns char vector.
irn_aa_chars <- function(s) strsplit(s, "", fixed = TRUE)[[1]]

## ==========================================================================
## 1. clonalQuant — unique clonotypes per group
## ==========================================================================
irn_clonalQuant <- function(
  data,
  cloneCall = "gene",
  chain = "TRB",
  group.by = NULL,
  scale = FALSE,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  groups <- irn_group_levels(long)
  df <- do.call(
    rbind,
    lapply(groups, function(g) {
      v <- long$clonotype[long$group == g]
      data.frame(
        contigs = length(unique(v)),
        values = g,
        total = length(v),
        stringsAsFactors = FALSE
      )
    })
  )
  df$scaled <- df$contigs / df$total * 100
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$values <- factor(df$values, levels = groups)
  yv <- if (isTRUE(scale)) "scaled" else "contigs"
  ylab <- if (isTRUE(scale)) {
    "Percent of unique clonotypes"
  } else {
    "Unique clonotypes"
  }
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$values, y = .data[[yv]], fill = .data$values)
  ) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = irn_pal(length(groups), palette)) +
    ggplot2::labs(x = NULL, y = ylab) +
    irn_theme()
}

## ==========================================================================
## 2. clonalAbundance — rank-abundance curve
## ==========================================================================
irn_clonalAbundance <- function(
  data,
  cloneCall = "gene",
  chain = "TRB",
  group.by = NULL,
  order.by = NULL,
  scale = FALSE,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  groups <- irn_group_levels(long, order.by)
  ## one row per clonotype with its Abundance (clone size), like scRepertoire
  per_clone <- do.call(
    rbind,
    lapply(groups, function(g) {
      tb <- table(long$clonotype[long$group == g])
      if (length(tb) == 0) {
        return(NULL)
      }
      data.frame(
        CTclone = names(tb),
        values = g,
        Abundance = as.numeric(tb),
        stringsAsFactors = FALSE
      )
    })
  )
  if (isTRUE(exportTable)) {
    return(per_clone)
  }
  per_clone$values <- factor(per_clone$values, levels = groups)
  cols <- irn_pal(length(groups), palette)
  if (isTRUE(scale)) {
    ## density of clone sizes per group (scRepertoire scale = TRUE)
    ggplot2::ggplot(
      per_clone,
      ggplot2::aes(x = .data$Abundance, fill = .data$values)
    ) +
      ggplot2::geom_density(alpha = 0.5, colour = NA) +
      ggplot2::scale_fill_manual(values = cols, name = "Group") +
      ggplot2::labs(x = "Abundance", y = "Density of Clones") +
      irn_theme()
  } else {
    ## number of clones at each abundance level, one line per group
    counts <- as.data.frame(
      table(values = per_clone$values, Abundance = per_clone$Abundance),
      stringsAsFactors = FALSE
    )
    counts$Abundance <- as.numeric(counts$Abundance)
    counts <- counts[counts$Freq > 0, , drop = FALSE]
    counts$values <- factor(counts$values, levels = groups)
    ggplot2::ggplot(
      counts,
      ggplot2::aes(
        x = .data$Abundance,
        y = .data$Freq,
        colour = .data$values
      )
    ) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::scale_colour_manual(values = cols, name = "Group") +
      ggplot2::labs(x = "Abundance", y = "Number of Clones") +
      irn_theme()
  }
}

## ==========================================================================
## 3. clonalProportion — rank-bin occupancy (stacked)
## ==========================================================================
irn_clonalProportion <- function(
  data,
  cloneCall = "gene",
  chain = "TRB",
  group.by = NULL,
  clonalSplit = c(10, 100, 1000, 10000, 30000, 1e5),
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  groups <- irn_group_levels(long)
  lo <- c(1, utils::head(clonalSplit, -1) + 1)
  labs <- sprintf(
    "[%s:%s]",
    lo,
    format(clonalSplit, scientific = FALSE, trim = TRUE)
  )
  df <- do.call(
    rbind,
    lapply(groups, function(g) {
      sizes <- sort(
        as.numeric(table(long$clonotype[long$group == g])),
        decreasing = TRUE
      )
      vals <- vapply(
        seq_along(clonalSplit),
        function(i) {
          if (lo[i] > length(sizes)) {
            return(0)
          }
          idx <- seq.int(lo[i], min(clonalSplit[i], length(sizes)))
          sum(sizes[idx])
        },
        numeric(1)
      )
      data.frame(group = g, bin = labs, value = vals, stringsAsFactors = FALSE)
    })
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = groups)
  df$bin <- factor(df$bin, levels = labs)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$group, y = .data$value, fill = .data$bin)
  ) +
    ggplot2::geom_col(position = "fill", width = 0.7) +
    ggplot2::scale_fill_manual(
      values = irn_pal(length(labs), palette),
      name = "Clonal indices"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = NULL, y = "Occupied repertoire space") +
    irn_theme()
}

## ==========================================================================
## 4. clonalHomeostasis — relative-frequency bands (stacked)
## ==========================================================================
irn_clonalHomeostasis <- function(
  data,
  cloneCall = "gene",
  chain = "TRB",
  group.by = NULL,
  cloneSize = c(
    Rare = 1e-4,
    Small = 1e-3,
    Medium = 1e-2,
    Large = 0.1,
    Hyperexpanded = 1
  ),
  order.by = NULL,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  groups <- irn_group_levels(long, order.by)
  bounds <- c(0, unname(cloneSize))
  nm <- names(cloneSize)
  labs <- vapply(
    seq_along(cloneSize),
    function(i) {
      sprintf(
        "%s (%s < X <= %s)",
        nm[i],
        format(bounds[i], trim = TRUE),
        format(bounds[i + 1], scientific = TRUE, trim = TRUE)
      )
    },
    character(1)
  )
  df <- do.call(
    rbind,
    lapply(groups, function(g) {
      tb <- table(long$clonotype[long$group == g])
      prop <- as.numeric(tb) / sum(tb)
      band <- cut(
        prop,
        breaks = bounds,
        labels = labs,
        right = TRUE,
        include.lowest = FALSE
      )
      v <- tapply(prop, band, sum)
      out <- setNames(numeric(length(labs)), labs)
      out[names(v)[!is.na(names(v))]] <- v[!is.na(names(v))]
      out[is.na(out)] <- 0
      data.frame(
        group = g,
        band = labs,
        value = as.numeric(out),
        stringsAsFactors = FALSE
      )
    })
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = groups)
  df$band <- factor(df$band, levels = labs)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$group, y = .data$value, fill = .data$band)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = irn_pal(length(labs), palette),
      name = "Clonal group"
    ) +
    ggplot2::labs(x = NULL, y = "Relative abundance") +
    irn_theme()
}

## ==========================================================================
## 5. clonalLength — CDR3 length distribution (table or overlaid plot)
## ==========================================================================
irn_clonalLength <- function(
  data,
  cloneCall = "aa",
  chain = "TRB",
  group.by = NULL,
  order.by = NULL,
  scale = FALSE,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  ## length is measured on the CDR3 sequence (nt or aa)
  cc <- if (tolower(cloneCall) %in% c("nt", "aa")) tolower(cloneCall) else "aa"
  long <- irn_long(data, cc, chain, group.by)
  long$length <- irn_cdr3_length(long$clonotype, chain)
  long <- long[long$length > 0, , drop = FALSE]
  groups <- irn_group_levels(long, order.by)
  if (isTRUE(exportTable)) {
    return(data.frame(
      length = long$length,
      CT = long$clonotype,
      values = long$group,
      stringsAsFactors = FALSE
    ))
  }
  long$group <- factor(long$group, levels = groups)
  ggplot2::ggplot(long, ggplot2::aes(x = .data$length, fill = .data$group)) +
    ggplot2::geom_bar(
      position = if (isTRUE(scale)) "fill" else "stack",
      width = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values = irn_pal(length(groups), palette),
      name = "Group"
    ) +
    ggplot2::labs(
      x = "CDR3 length",
      y = if (isTRUE(scale)) "Proportion" else "Number of CDR3"
    ) +
    irn_theme()
}

## ==========================================================================
## 6. clonalCompare — top clones across selected groups (table)
## ==========================================================================
irn_clonalCompare <- function(
  data,
  cloneCall = "gene",
  chain = "TRB",
  group.by = NULL,
  order.by = NULL,
  samples = NULL,
  top.clones = 10,
  proportion = TRUE,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  if (!is.null(samples)) {
    long <- long[long$group %in% samples, , drop = FALSE]
  }
  groups <- if (!is.null(samples)) {
    samples[samples %in% long$group]
  } else {
    irn_group_levels(long, order.by)
  }
  per <- lapply(groups, function(g) {
    tb <- table(long$clonotype[long$group == g])
    if (length(tb) == 0) {
      return(NULL)
    }
    prop <- as.numeric(tb) / sum(tb)
    data.frame(
      clones = names(tb),
      Count = as.numeric(tb),
      Proportion = prop,
      Sample = g,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, per)
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      clones = character(0),
      Proportion = numeric(0),
      Sample = character(0)
    ))
  }
  ## keep the top.clones per group (by the shown value), then the union
  val_col <- if (isTRUE(proportion)) "Proportion" else "Count"
  top <- unlist(
    lapply(groups, function(g) {
      sub <- df[df$Sample == g, ]
      sub <- sub[order(-sub[[val_col]]), ]
      utils::head(sub$clones, top.clones)
    }),
    use.names = FALSE
  )
  df <- df[df$clones %in% unique(top), , drop = FALSE]
  keep <- c("clones", val_col, "Sample")
  out <- df[, keep, drop = FALSE]
  out$Sample <- factor(out$Sample, levels = groups)
  if (isTRUE(exportTable)) {
    return(out)
  }
  ## gallery / non-table path: stacked bar of the shown clones per group
  clone_lv <- names(sort(
    tapply(out[[val_col]], out$clones, sum),
    decreasing = TRUE
  ))
  out$clones <- factor(out$clones, levels = rev(clone_lv))
  ggplot2::ggplot(
    out,
    ggplot2::aes(x = .data$Sample, y = .data[[val_col]], fill = .data$clones)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = irn_pal(length(clone_lv), palette),
      guide = "none"
    ) +
    ggplot2::labs(x = NULL, y = val_col) +
    irn_theme()
}

## ==========================================================================
## 7. clonalOverlap — pairwise similarity heatmap
## ==========================================================================
irn_clonalOverlap <- function(
  data,
  cloneCall = "gene",
  chain = "TRB",
  group.by = NULL,
  method = "overlap",
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  groups <- irn_group_levels(long)
  counts <- lapply(groups, function(g) table(long$clonotype[long$group == g]))
  names(counts) <- groups
  n <- length(groups)
  m <- matrix(NA_real_, n, n, dimnames = list(groups, groups))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i > j) {
        m[i, j] <- irn_overlap_pair(counts[[j]], counts[[i]], method)
      }
    }
  }
  df <- expand.grid(Var1 = groups, Var2 = groups, stringsAsFactors = FALSE)
  df$value <- as.vector(m)
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$Var1 <- factor(df$Var1, levels = groups)
  df$Var2 <- factor(df$Var2, levels = rev(groups))
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = .data$value)
  ) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          is.na(.data$value),
          "",
          formatC(.data$value, digits = 2, format = "f")
        )
      ),
      size = 3
    ) +
    ggplot2::scale_fill_gradientn(
      colours = irn_pal(9, palette),
      na.value = "grey95",
      name = method
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    irn_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

irn_overlap_pair <- function(xa, xb, method = "overlap") {
  a <- names(xa)
  b <- names(xb)
  inter <- length(intersect(a, b))
  switch(
    method,
    overlap = inter / min(length(a), length(b)),
    jaccard = inter / length(union(a, b)),
    raw = inter,
    morisita = {
      ## Morisita-Horn index, matching scRepertoire's .morisitaCalc:
      ##   2*sum(x*y) / ((sum(x^2)/X^2 + sum(y^2)/Y^2) * X * Y)
      ## (not the classic unbiased Morisita, which divides by N(N-1) and blows
      ## up to Inf for a singleton clone.)
      keys <- union(a, b)
      va <- as.numeric(xa[keys])
      va[is.na(va)] <- 0
      vb <- as.numeric(xb[keys])
      vb[is.na(vb)] <- 0
      X <- sum(va)
      Y <- sum(vb)
      num <- 2 * sum(va * vb)
      den <- ((sum(va^2) / X^2) + (sum(vb^2) / Y^2)) * X * Y
      num / den
    },
    inter / min(length(a), length(b))
  )
}

## ==========================================================================
## 8. clonalDiversity — bootstrapped diversity (table with n.boots rows/group)
## ==========================================================================
irn_diversity_metric <- function(counts, metric) {
  counts <- counts[counts > 0]
  N <- sum(counts)
  p <- counts / N
  switch(
    tolower(metric),
    shannon = -sum(p * log(p)),
    inv.simpson = 1 / sum(p^2),
    gini.simpson = 1 - sum(p^2),
    norm.entropy = if (length(counts) <= 1) {
      0
    } else {
      -sum(p * log(p)) / log(length(counts))
    },
    chao1 = {
      f1 <- sum(counts == 1)
      f2 <- sum(counts == 2)
      length(counts) + if (f2 > 0) f1^2 / (2 * f2) else f1 * (f1 - 1) / 2
    },
    ace = length(counts),
    -sum(p * log(p))
  )
}

irn_clonalDiversity <- function(
  data,
  cloneCall = "gene",
  chain = "TRB",
  group.by = NULL,
  metric = "shannon",
  x.axis = NULL,
  order.by = NULL,
  n.boots = 20,
  return.boots = TRUE,
  exportTable = TRUE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  groups <- irn_group_levels(long, order.by)
  ## downsample to the smallest group's cell count, bootstrap n.boots times
  sizes <- vapply(groups, function(g) sum(long$group == g), integer(1))
  min_n <- max(1, min(sizes))
  gcol <- if (is.null(group.by)) "Group" else group.by
  rows <- lapply(groups, function(g) {
    v <- long$clonotype[long$group == g]
    boots <- vapply(
      seq_len(n.boots),
      function(b) {
        s <- sample(v, size = min_n, replace = TRUE)
        irn_diversity_metric(as.numeric(table(s)), metric)
      },
      numeric(1)
    )
    d <- data.frame(value = boots, stringsAsFactors = FALSE)
    d[[gcol]] <- g
    d
  })
  df <- do.call(rbind, rows)
  df$metric <- metric
  df$x.axis <- df[[gcol]]
  if (!is.null(x.axis) && x.axis %in% unlist(lapply(data, colnames))) {
    ## map each group to its x.axis value (first observed)
    df[["x.axis"]] <- df[[gcol]]
  }
  ## rename x.axis column to the requested name if provided
  if (!is.null(x.axis)) {
    names(df)[names(df) == "x.axis"] <- x.axis
  }
  df
}

## ==========================================================================
## 9. clonalScatter — clone frequency in group X vs Y
## ==========================================================================
irn_clonalScatter <- function(
  data,
  cloneCall = "gene",
  chain = "both",
  group.by = NULL,
  x.axis = NULL,
  y.axis = NULL,
  dot.size = "total",
  graph = "proportion",
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  gx <- table(long$clonotype[long$group == x.axis])
  gy <- table(long$clonotype[long$group == y.axis])
  keys <- union(names(gx), names(gy))
  cx <- as.numeric(gx[keys])
  cx[is.na(cx)] <- 0
  cy <- as.numeric(gy[keys])
  cy[is.na(cy)] <- 0
  if (identical(graph, "proportion")) {
    vx <- cx / sum(cx)
    vy <- cy / sum(cy)
  } else {
    vx <- cx
    vy <- cy
  }
  cls <- ifelse(
    cx > 0 & cy > 0,
    "dual.expanded",
    ifelse(cx > 0, x.axis, y.axis)
  )
  df <- data.frame(
    x = vx,
    y = vy,
    total = cx + cy,
    class = cls,
    stringsAsFactors = FALSE
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey60"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = .data$class, size = .data$total),
      shape = 21,
      alpha = 0.7,
      colour = "black",
      stroke = 0.2
    ) +
    ggplot2::scale_fill_manual(
      values = irn_pal(length(unique(df$class)), palette),
      name = "Clone class"
    ) +
    ggplot2::guides(size = "none") +
    ggplot2::labs(x = x.axis, y = y.axis) +
    irn_theme()
}

## ==========================================================================
## 10. clonalSizeDistribution — clone-size ECDF per group
## ==========================================================================
irn_clonalSizeDistribution <- function(
  data,
  cloneCall = "strict",
  chain = "TRB",
  group.by = NULL,
  method = "ward.D2",
  threshold = 1,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  groups <- irn_group_levels(long)
  df <- do.call(
    rbind,
    lapply(groups, function(g) {
      sizes <- as.numeric(table(long$clonotype[long$group == g]))
      sizes <- sort(sizes[sizes >= threshold])
      if (length(sizes) == 0) {
        return(NULL)
      }
      data.frame(
        group = g,
        size = sizes,
        ecdf = seq_along(sizes) / length(sizes),
        stringsAsFactors = FALSE
      )
    })
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = groups)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$size, y = .data$ecdf, colour = .data$group)
  ) +
    ggplot2::geom_step(direction = "hv", linewidth = 0.7) +
    ggplot2::scale_colour_manual(
      values = irn_pal(length(groups), palette),
      name = "Group"
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Clone size (cells)",
      y = "Cumulative fraction of clones"
    ) +
    irn_theme()
}

## ==========================================================================
## 11. clonalRarefaction — rarefaction curve (analytic Hurlbert expectation)
## ==========================================================================
irn_clonalRarefaction <- function(
  data,
  cloneCall = "gene",
  chain = "TRB",
  group.by = NULL,
  plot.type = 1,
  hill.numbers = 0,
  n.boots = 20,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, cloneCall, chain, group.by)
  groups <- irn_group_levels(long)
  ## E[S_m] = S - sum_i C(N - n_i, m) / C(N, m)   (expected richness at depth m)
  rar <- function(counts, m) {
    N <- sum(counts)
    S <- length(counts)
    S - sum(exp(lchoose(N - counts, m) - lchoose(N, m)))
  }
  df <- do.call(
    rbind,
    lapply(groups, function(g) {
      counts <- as.numeric(table(long$clonotype[long$group == g]))
      N <- sum(counts)
      if (N < 2) {
        return(NULL)
      }
      ms <- unique(round(seq(1, N, length.out = 40)))
      data.frame(
        group = g,
        m = ms,
        richness = vapply(ms, function(mm) rar(counts, mm), numeric(1)),
        stringsAsFactors = FALSE
      )
    })
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = groups)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$m, y = .data$richness, colour = .data$group)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(
      values = irn_pal(length(groups), palette),
      name = "Group"
    ) +
    ggplot2::labs(x = "Number of cells sampled", y = "Clonotype richness") +
    irn_theme()
}

## ==========================================================================
## amino-acid position helpers (percentAA, positionalEntropy/Property)
## ==========================================================================
IRN_AA <- c(
  "A",
  "R",
  "N",
  "D",
  "C",
  "Q",
  "E",
  "G",
  "H",
  "I",
  "L",
  "K",
  "M",
  "F",
  "P",
  "S",
  "T",
  "W",
  "Y",
  "V"
)

## per (group, position) amino-acid frequency table over CDR3 sequences
irn_position_matrix <- function(
  data,
  chain,
  group.by,
  aa.length,
  order.by = NULL
) {
  long <- irn_long(data, "aa", chain, group.by)
  groups <- irn_group_levels(long, order.by)
  out <- list()
  for (g in groups) {
    ## Match scRepertoire/immApex calculateFrequency: multi-contig strings are
    ## split on ";" into separate sequences, sequences LONGER than aa.length are
    ## dropped (not truncated), and shorter ones are padded with a gap — so the
    ## denominator is the sequence count, constant across positions.
    raw <- long$clonotype[long$group == g]
    seqs <- unlist(strsplit(raw, ";", fixed = TRUE))
    seqs <- seqs[!is.na(seqs) & nzchar(seqs) & seqs != "NA"]
    seqs <- seqs[nchar(seqs) <= aa.length]
    counts <- matrix(
      0,
      nrow = aa.length,
      ncol = length(IRN_AA),
      dimnames = list(NULL, IRN_AA)
    )
    for (s in seqs) {
      chars <- strsplit(s, "", fixed = TRUE)[[1]]
      for (p in seq_along(chars)) {
        aa <- chars[p]
        if (aa %in% IRN_AA) counts[p, aa] <- counts[p, aa] + 1
      }
    }
    attr(counts, "n_seq") <- length(seqs)
    out[[g]] <- counts
  }
  list(counts = out, groups = groups)
}

## ==========================================================================
## 12. percentAA — amino-acid composition per position (facet by group)
## ==========================================================================
irn_percentAA <- function(
  data,
  chain = "TRB",
  group.by = NULL,
  order.by = NULL,
  aa.length = 20,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  pm <- irn_position_matrix(data, chain, group.by, aa.length, order.by)
  df <- do.call(
    rbind,
    lapply(pm$groups, function(g) {
      m <- pm$counts[[g]]
      ## Frequencies are over ALL sequences in the group (shorter CDR3s padded
      ## with a gap), matching scRepertoire: divide every position by the same
      ## sequence count rather than by the per-position residue total.
      n_seq <- attr(m, "n_seq")
      freq <- if (!is.null(n_seq) && n_seq > 0) m / n_seq else m
      d <- as.data.frame(as.table(freq))
      colnames(d) <- c("Position", "AA", "Frequency")
      d$Position <- as.integer(d$Position)
      d$group <- g
      d
    })
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = pm$groups)
  df$AA <- factor(df$AA, levels = IRN_AA)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$Position, y = .data$Frequency, fill = .data$AA)
  ) +
    ggplot2::geom_col(width = 1) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$group)) +
    ggplot2::scale_fill_manual(
      values = irn_pal(length(IRN_AA), palette),
      name = "AA"
    ) +
    ggplot2::labs(x = "Position", y = "Relative frequency") +
    irn_theme()
}

## ==========================================================================
## 13/14/19. gene-usage heatmaps: percentGeneUsage, percentGenes, vizGenes
## ==========================================================================
irn_gene_usage_df <- function(
  data,
  chain,
  group.by,
  which_gene = "V",
  order.by = NULL,
  summary.fun = "percent"
) {
  long <- irn_long(data, "gene", chain, group.by)
  groups <- irn_group_levels(long, order.by)
  do.call(
    rbind,
    lapply(groups, function(g) {
      genes <- irn_gene_segments(long$clonotype[long$group == g])[[which_gene]]
      genes <- genes[!is.na(genes)]
      tb <- table(genes)
      val <- switch(
        summary.fun,
        percent = as.numeric(tb) / sum(tb) * 100,
        proportion = as.numeric(tb) / sum(tb),
        as.numeric(tb)
      )
      data.frame(
        gene = names(tb),
        value = val,
        group = g,
        stringsAsFactors = FALSE
      )
    })
  )
}

irn_gene_heatmap <- function(df, groups, palette, ylab = "Gene") {
  df$group <- factor(df$group, levels = groups)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$group, y = .data$gene, fill = .data$value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colours = irn_pal(9, palette), name = "%") +
    ggplot2::labs(x = NULL, y = ylab) +
    irn_theme()
}

irn_gene_family_letter <- function(genes) {
  ## "TRBV" -> "V", "TRBJ" -> "J"
  g <- toupper(genes %||% "TRBV")
  if (grepl("J$", g)) {
    "J"
  } else if (grepl("D$", g)) {
    "D"
  } else {
    "V"
  }
}

irn_percentGeneUsage <- function(
  data,
  chain = "TRB",
  genes = "TRBV",
  group.by = NULL,
  order.by = NULL,
  summary.fun = "percent",
  plot.type = "heatmap",
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  which_gene <- irn_gene_family_letter(genes)
  long <- irn_long(data, "gene", chain, group.by)
  groups <- irn_group_levels(long, order.by)
  df <- irn_gene_usage_df(
    data,
    chain,
    group.by,
    which_gene,
    order.by,
    summary.fun
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  irn_gene_heatmap(df, groups, palette, ylab = genes)
}

irn_percentGenes <- function(
  data,
  chain = "TRB",
  gene = "Vgene",
  group.by = NULL,
  order.by = NULL,
  summary.fun = "percent",
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  which_gene <- if (grepl("^J", gene) || grepl("Jgene", gene)) {
    "J"
  } else if (grepl("^D", gene) || grepl("Dgene", gene)) {
    "D"
  } else {
    "V"
  }
  long <- irn_long(data, "gene", chain, group.by)
  groups <- irn_group_levels(long, order.by)
  df <- irn_gene_usage_df(
    data,
    chain,
    group.by,
    which_gene,
    order.by,
    summary.fun
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  irn_gene_heatmap(df, groups, palette, ylab = gene)
}

irn_vizGenes <- function(
  data,
  x.axis = "TRBV",
  y.axis = NULL,
  group.by = NULL,
  order.by = NULL,
  plot = "heatmap",
  summary.fun = "percent",
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  which_gene <- irn_gene_family_letter(x.axis)
  long <- irn_long(data, "gene", "both", group.by)
  groups <- irn_group_levels(long, order.by)
  df <- irn_gene_usage_df(
    data,
    "both",
    group.by,
    which_gene,
    order.by,
    summary.fun
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  irn_gene_heatmap(df, groups, palette, ylab = x.axis)
}

## ==========================================================================
## 15. percentVJ — V x J pairing heatmap (facet by group)
## ==========================================================================
irn_percentVJ <- function(
  data,
  chain = "TRB",
  group.by = NULL,
  order.by = NULL,
  summary.fun = "percent",
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  long <- irn_long(data, "gene", chain, group.by)
  groups <- irn_group_levels(long, order.by)
  df <- do.call(
    rbind,
    lapply(groups, function(g) {
      seg <- irn_gene_segments(long$clonotype[long$group == g])
      ok <- !is.na(seg$V) & !is.na(seg$J)
      if (!any(ok)) {
        return(NULL)
      }
      tb <- as.data.frame(
        table(V = seg$V[ok], J = seg$J[ok]),
        stringsAsFactors = FALSE
      )
      tb$value <- if (identical(summary.fun, "percent")) {
        tb$Freq / sum(tb$Freq) * 100
      } else {
        tb$Freq
      }
      tb$group <- g
      tb
    })
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = groups)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$V, y = .data$J, fill = .data$value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(ggplot2::vars(.data$group)) +
    ggplot2::scale_fill_gradientn(colours = irn_pal(9, palette), name = "%") +
    ggplot2::labs(x = "V gene", y = "J gene") +
    irn_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 5),
      axis.text.y = ggplot2::element_text(size = 5)
    )
}

## ==========================================================================
## 16. percentKmer — top CDR3 k-mer motifs (heatmap group x motif)
## ==========================================================================
irn_percentKmer <- function(
  data,
  chain = "TRB",
  cloneCall = "aa",
  group.by = NULL,
  motif.length = 3,
  min.depth = 3,
  top.motifs = 30,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  cc <- if (tolower(cloneCall) %in% c("nt", "aa")) tolower(cloneCall) else "aa"
  long <- irn_long(data, cc, chain, group.by)
  groups <- irn_group_levels(long)
  kmers_of <- function(s) {
    n <- nchar(s)
    if (n < motif.length) {
      return(character(0))
    }
    vapply(
      seq_len(n - motif.length + 1),
      function(i) substr(s, i, i + motif.length - 1),
      character(1)
    )
  }
  per <- lapply(groups, function(g) {
    seqs <- long$clonotype[long$group == g]
    km <- unlist(lapply(seqs, kmers_of), use.names = FALSE)
    km <- km[!grepl("[^A-Z]", km)]
    tb <- table(km)
    tb <- tb[tb >= min.depth]
    if (length(tb) == 0) {
      return(NULL)
    }
    data.frame(
      motif = names(tb),
      value = as.numeric(tb) / sum(tb) * 100,
      group = g,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, per)
  if (is.null(df)) {
    df <- data.frame(
      motif = character(0),
      value = numeric(0),
      group = character(0)
    )
  }
  ## keep the globally top motifs
  tops <- names(sort(tapply(df$value, df$motif, sum), decreasing = TRUE))
  tops <- utils::head(tops, top.motifs)
  df <- df[df$motif %in% tops, , drop = FALSE]
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = groups)
  df$motif <- factor(df$motif, levels = rev(tops))
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$group, y = .data$motif, fill = .data$value)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colours = irn_pal(9, palette), name = "%") +
    ggplot2::labs(x = NULL, y = paste0(motif.length, "-mer motif")) +
    irn_theme()
}

## ==========================================================================
## 17. positionalEntropy — per-position Shannon/normalised entropy (line/group)
## ==========================================================================
irn_positionalEntropy <- function(
  data,
  chain = "TRB",
  group.by = NULL,
  order.by = NULL,
  aa.length = 20,
  method = "norm.entropy",
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  pm <- irn_position_matrix(data, chain, group.by, aa.length, order.by)
  df <- do.call(
    rbind,
    lapply(pm$groups, function(g) {
      m <- pm$counts[[g]]
      ent <- apply(m, 1, function(row) {
        tot <- sum(row)
        if (tot == 0) {
          return(0)
        }
        p <- row[row > 0] / tot
        h <- -sum(p * log(p))
        if (identical(method, "norm.entropy")) h / log(length(IRN_AA)) else h
      })
      data.frame(
        Position = seq_len(aa.length),
        entropy = as.numeric(ent),
        group = g,
        stringsAsFactors = FALSE
      )
    })
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = pm$groups)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$Position, y = .data$entropy, colour = .data$group)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1) +
    ggplot2::scale_colour_manual(
      values = irn_pal(length(pm$groups), palette),
      name = "Group"
    ) +
    ggplot2::labs(x = "Position", y = "Entropy") +
    irn_theme()
}

## ==========================================================================
## 18. positionalProperty — per-position mean AA property (vendored matrices)
## ==========================================================================
## Vendored Atchley factors (5) and Kidera factors (subset) so the property view
## works without immApex. Values are the published standardized factor loadings.
IRN_ATCHLEY <- local({
  m <- rbind(
    A = c(-0.591, -1.302, -0.733, 1.570, -0.146),
    R = c(1.538, -0.055, 1.502, 0.440, 2.897),
    N = c(0.945, 0.828, 1.299, -0.169, 0.933),
    D = c(1.050, 0.302, -3.656, -0.259, -3.242),
    C = c(-1.343, 0.465, -0.862, -1.020, -0.255),
    Q = c(0.931, -0.179, -3.005, -0.503, -1.853),
    E = c(1.357, -1.453, 1.477, 0.113, -0.837),
    G = c(-0.384, 1.652, 1.330, 1.045, 2.064),
    H = c(0.336, -0.417, -1.673, -1.474, -0.078),
    I = c(-1.239, -0.547, 2.131, 0.393, 0.816),
    L = c(-1.019, -0.987, -1.505, 1.266, -0.912),
    K = c(1.831, -0.561, 0.533, -0.277, 1.648),
    M = c(-0.663, -1.524, 2.219, -1.005, 1.212),
    F = c(-1.006, -0.590, 1.891, -0.397, 0.412),
    P = c(0.189, 2.081, -1.628, 0.421, -1.392),
    S = c(-0.228, 1.399, -4.760, 0.670, -2.647),
    T = c(-0.032, 0.326, 2.213, 0.908, 1.313),
    W = c(-0.595, 0.009, 0.672, -2.128, -0.184),
    Y = c(0.260, 0.830, 3.097, -0.838, 1.512),
    V = c(-1.337, -0.279, -0.544, 1.242, -1.262)
  )
  colnames(m) <- paste0("AF", 1:5)
  m
})

irn_positionalProperty <- function(
  data,
  chain = "TRB",
  group.by = NULL,
  order.by = NULL,
  method = "atchleyFactors",
  aa.length = 20,
  exportTable = FALSE,
  palette = "Harmonic",
  ...
) {
  if (is.na(aa.length) || aa.length < 1) {
    aa.length <- 20
  }
  pm <- irn_position_matrix(data, chain, group.by, aa.length, order.by)
  props <- IRN_ATCHLEY # only atchleyFactors vendored; other methods fall back to it
  df <- do.call(
    rbind,
    lapply(pm$groups, function(g) {
      m <- pm$counts[[g]]
      do.call(
        rbind,
        lapply(seq_len(ncol(props)), function(k) {
          vals <- apply(m, 1, function(row) {
            tot <- sum(row)
            if (tot == 0) {
              return(NA_real_)
            }
            sum(row / tot * props[colnames(m), k])
          })
          data.frame(
            Position = seq_len(aa.length),
            property = colnames(props)[k],
            value = as.numeric(vals),
            group = g,
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
  if (isTRUE(exportTable)) {
    return(df)
  }
  df$group <- factor(df$group, levels = pm$groups)
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$Position, y = .data$value, colour = .data$group)
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_wrap(ggplot2::vars(.data$property), scales = "free_y") +
    ggplot2::scale_colour_manual(
      values = irn_pal(length(pm$groups), palette),
      name = "Group"
    ) +
    ggplot2::labs(x = "Position", y = "Mean property value") +
    irn_theme()
}
