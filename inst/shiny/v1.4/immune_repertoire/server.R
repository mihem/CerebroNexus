##----------------------------------------------------------------------------##
## Tab: Immune Repertoire server — entry point
##----------------------------------------------------------------------------##

## The immune-repertoire analytics are now computed natively (native_metrics.R),
## so scRepertoire is no longer required at runtime. Kept as a function (always
## TRUE) so the existing req()/guard call sites need no change.
has_scRepertoire <- function() {
  TRUE
}

## ---- Missing-dependency notice ---------------------------------------- ##
## scRepertoire is a Suggests dependency (GitHub/Bioconductor). When it is
## not installed the immune repertoire UI cannot render, so instead of a
## silent blank panel we show an explicit prompt telling the user how to
## enable the feature. Shown in both the settings and visualizations boxes.
ir_scRepertoire_missing_ui <- function() {
  div(
    class = "alert alert-warning",
    tags$b("scRepertoire is required for immune repertoire analysis."),
    tags$br(),
    "Please install it to use the TCR/BCR features:",
    tags$pre(
      "BiocManager::install(\"scRepertoire\")"
    )
  )
}

## ---- Container size guard: prevent "figure margins too large" during
## ---- tab switches when the output container has zero/tiny dimensions. req()
## ---- silently halts rendering; Shiny re-triggers once the container has real
## ---- space. Both width AND height must clear the floor: base-graphics and
## ---- grid prints (e.g. clonalRarefaction via ggiNEXT) call plot.new(), which
## ---- throws "figure margins too large" when either dimension leaves no room
## ---- for the fixed margins (~1 inch). 72px ≈ 1 inch at the default device
## ---- resolution, so require a comfortable margin above that.
req_plot_space <- function(output_id, min_px = 80L) {
  cd <- shiny::getDefaultReactiveDomain()$clientData
  w <- cd[[paste0("output_", output_id, "_width")]]
  h <- cd[[paste0("output_", output_id, "_height")]]
  shiny::req(isTRUE(w >= min_px), isTRUE(h >= min_px))
}

## ---- Apply generic display options to a ggplot ------------------------ ##
## Reads the IR_DISPLAY_SPEC values (see ir_display_params()) and applies the
## tab-agnostic ones — font size and title — to a ggplot. Point size / opacity
## are scatter-specific and handled directly by the scatter renderers (so we
## don't reach into ggplot layer internals here). Non-ggplot input (base-R
## plots) is returned unchanged.
ir_apply_display <- function(p, params = NULL) {
  if (!inherits(p, "ggplot")) {
    return(p)
  }
  if (is.null(params)) {
    params <- tryCatch(ir_display_params(), error = function(e) list())
  }
  base_size <- suppressWarnings(as.numeric(params[["ir_d_base_size"]]))
  if (length(base_size) == 1 && !is.na(base_size) && base_size > 0) {
    p <- p + ggplot2::theme(text = ggplot2::element_text(size = base_size))
  }
  title <- params[["ir_d_title"]]
  if (is.character(title) && length(title) == 1 && nzchar(title)) {
    p <- p + ggplot2::labs(title = title)
  }
  # Legend: font size, key/point size and position (or hidden).
  legend_size <- suppressWarnings(as.numeric(params[["ir_d_legend_size"]]))
  if (length(legend_size) == 1 && !is.na(legend_size) && legend_size > 0) {
    p <- p +
      ggplot2::theme(legend.text = ggplot2::element_text(size = legend_size))
  }
  legend_key <- suppressWarnings(as.numeric(params[["ir_d_legend_key"]]))
  if (length(legend_key) == 1 && !is.na(legend_key) && legend_key > 0) {
    p <- p +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          override.aes = list(size = legend_key)
        ),
        fill = ggplot2::guide_legend(
          override.aes = list(size = legend_key)
        )
      )
  }
  legend_pos <- params[["ir_d_legend_pos"]]
  if (
    is.character(legend_pos) && length(legend_pos) == 1 && nzchar(legend_pos)
  ) {
    p <- p + ggplot2::theme(legend.position = legend_pos)
  }
  p
}

## ---- Muffle known-harmless upstream warnings -------------------------- ##
## scRepertoire::clonalRarefaction delegates bootstrapping to iNEXT, whose
## internals (iNEXT:::invChat -> matrix(apply(Abun.Mat, 2, ...))) emit
## "data length [N] is not a sub-multiple or multiple of the number of rows"
## whenever bootstrap resamples yield unequal qD vector lengths. This is benign
## iNEXT noise we cannot fix at source, and it floods the console. Muffle ONLY
## these specific patterns; every other warning still propagates so real issues
## stay visible.
IR_NOISE_WARNINGS <- paste(
  "is not a sub-multiple or multiple of the number of rows",
  "aes_string\\(\\) was deprecated",
  sep = "|"
)
ir_quiet_inext <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(IR_NOISE_WARNINGS, conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

safeRenderPlot <- function(expr, plot_name = "unknown") {
  tryCatch(
    {
      # Evaluate the plot expression, then apply the generic display options
      # (font size / title) to any ggplot it produced. This single hook covers
      # every renderer that funnels through safeRenderPlot, so individual
      # renderers don't each need to call ir_apply_display(). Non-ggplot
      # results (base-R plots) pass through unchanged.
      result <- force(expr)
      ir_apply_display(result)
    },
    error = function(e) {
      # validate()/need()/req() raise a "shiny.silent.error"; re-raise it so
      # Shiny renders the usual grey placeholder instead of our error plot.
      # (Using a single handler avoids the re-raised condition being caught by
      # a sibling error handler in the same tryCatch.)
      if (inherits(e, "shiny.silent.error")) {
        stop(e)
      }
      # "figure margins too large" / "plot region too large" come from base
      # graphics plot.new() when Shiny opens a near-zero PNG device — which
      # happens for a plotOutput on a hidden/not-yet-laid-out tab (the browser
      # reports a CSS height to clientData that does not match the tiny device
      # Shiny actually opens, so the upstream width/height guard can't catch
      # it). This is a transient layout state, not a real failure: swallow it
      # silently (Shiny shows its grey placeholder and re-renders once the
      # container has real space) instead of dumping a scary stack trace to the
      # console on every tab switch.
      if (grepl("figure margins too large|plot region too large", e$message)) {
        shiny::req(FALSE)
      }
      message("[IR ERROR] Plot '", plot_name, "' failed: ", e$message)

      # scRepertoire raises cryptic internal errors (e.g. "get1index",
      # "subscript out of bounds", "less than one element") when the current
      # selection leaves a group/sample empty or single-valued. These are not
      # actionable to the user, so translate them into a plain empty-state
      # message instead of dumping the raw R error onto the plot.
      msg <- conditionMessage(e)
      is_empty_selection <- grepl(
        "get1index|subscript out of bounds|less than one element|undefined columns|replacement has|non-conformable|missing value",
        msg,
        ignore.case = TRUE
      )
      # clonalSizeDistribution MLE fitting fails on gene/nt/aa clone calls
      # (the distribution is too noisy for log-normal MLE); the error reads
      # "initial parameter values are invalid" or "NA/NaN ... sigmau".
      is_mle_failure <- grepl(
        "initial parameter|NA/NaN.*sigmau|non-finite|optimization failed",
        msg,
        ignore.case = TRUE
      )
      label <- if (is_empty_selection) {
        paste0(
          "No data to display for the current selection.\n",
          "Try a different Chain, Clone call, or Group by."
        )
      } else if (is_mle_failure) {
        paste0(
          "Clone size distribution fitting failed.\n",
          "The strict clone definition is required for this plot.\n",
          "No action needed — strict is already enforced."
        )
      } else {
        paste0("Could not render this plot:\n", msg)
      }
      # Render an empty-state message robustly even on small devices. ggplot
      # avoids base-graphics margin issues ("figure margins too large").
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = label,
          size = 4.5,
          colour = "#666666",
          hjust = 0.5,
          vjust = 0.5
        ) +
        ggplot2::theme_void()
    }
  )
}

## ---- BCR-specific helper: extract isotype ------------------------------- ##
bcr_extract_isotype <- function(combined_BCR) {
  dplyr::bind_rows(lapply(combined_BCR, function(df) {
    if (is.null(df) || !"CTgene" %in% colnames(df)) {
      return(NULL)
    }
    parts <- strsplit(df$CTgene, "_", fixed = TRUE)
    igh <- vapply(
      parts,
      function(p) {
        if (length(p) == 0) {
          return(NA_character_)
        }
        hit <- grep("^IGH", p, value = TRUE)
        if (length(hit) == 0) NA_character_ else hit[1]
      },
      character(1)
    )
    isotype <- ifelse(
      !is.na(igh) & grepl("\\.", igh),
      sub("^.*\\.", "", igh),
      NA_character_
    )
    isotype <- ifelse(grepl("^IGH[ADEGM]", isotype), isotype, NA_character_)
    tibble::add_column(tibble::as_tibble(df), isotype = isotype)
  }))
}

## ---- BCR-specific: isotype distribution plot ---------------------------- ##
bcr_isotype_plot <- function(combined, group_col = "sample") {
  iso <- bcr_extract_isotype(combined)

  if (is.null(iso) || nrow(iso) == 0L) {
    return(NULL)
  }
  iso <- iso[!is.na(iso$isotype) & !is.na(iso[[group_col]]), , drop = FALSE]
  if (nrow(iso) == 0L) {
    return(NULL)
  }

  iso$isotype <- factor(
    iso$isotype,
    levels = c(
      "IGHM",
      "IGHD",
      "IGHG1",
      "IGHG2",
      "IGHG3",
      "IGHG4",
      "IGHA1",
      "IGHA2",
      "IGHE"
    )
  )
  iso[[group_col]] <- factor(
    iso[[group_col]],
    levels = unique(iso[[group_col]])
  )

  ## Low-saturation isotype palette (was high-saturation Set1). Same low-sat
  ## family used app-wide via cerebro_group_colors(); named per isotype so the
  ## mapping is stable across plots.
  pal <- setNames(
    cerebro_group_colors(9),
    c(
      "IGHM",
      "IGHD",
      "IGHG1",
      "IGHG2",
      "IGHG3",
      "IGHG4",
      "IGHA1",
      "IGHA2",
      "IGHE"
    )
  )

  ggplot2::ggplot(iso, ggplot2::aes(x = .data[[group_col]], fill = isotype)) +
    ggplot2::geom_bar(position = "fill", colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(
      values = pal,
      na.value = "grey80",
      name = "Isotype",
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(x = group_col, y = "Proportion") +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}

## ---- BCR-specific: SHM proxy plot --------------------------------------- ##
bcr_shm_proxy_plot <- function(
  combined_BCR,
  group_col = "sample",
  clone_call_col = "CTstrict"
) {
  diversity <- dplyr::bind_rows(lapply(combined_BCR, function(df) {
    needed <- c(clone_call_col, "CTnt", group_col)
    if (is.null(df) || !all(needed %in% colnames(df))) {
      return(NULL)
    }
    parts <- strsplit(df$CTnt, "_", fixed = TRUE)
    igh_nt <- vapply(
      parts,
      function(p) {
        if (length(p) == 0) {
          return(NA_character_)
        }
        p[1]
      },
      character(1)
    )

    tib <- tibble::as_tibble(df[, group_col, drop = FALSE])
    tib$.clone <- df[[clone_call_col]]
    tib$.cdr3_nt <- igh_nt
    tib <- tib[!is.na(tib$.clone) & !is.na(tib$.cdr3_nt), , drop = FALSE]
    if (nrow(tib) == 0L) {
      return(NULL)
    }

    tib %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(
        group_col,
        ".clone"
      )))) %>%
      dplyr::summarise(
        n_cells = dplyr::n(),
        n_unique_cdr3nt = dplyr::n_distinct(.cdr3_nt),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_cells >= 2L)
  }))

  if (is.null(diversity) || nrow(diversity) == 0L) {
    return(NULL)
  }

  diversity[[group_col]] <- factor(
    diversity[[group_col]],
    levels = unique(diversity[[group_col]])
  )

  ggplot2::ggplot(
    diversity,
    ggplot2::aes(x = .data[[group_col]], y = n_unique_cdr3nt)
  ) +
    ggplot2::geom_boxplot(outlier.size = 0.4, fill = "#4c72a6", alpha = 0.55) +
    ggplot2::scale_y_continuous(
      trans = "log1p",
      breaks = c(1, 2, 3, 5, 10, 20, 50, 100)
    ) +
    ggplot2::labs(
      x = group_col,
      y = "Unique CDR3-H3 nt per clone (size >=2)"
    ) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}

## ---- Helper: per-sample metadata (columns constant within each sample) -- ##
sample_level_meta <- function(data) {
  shared_cols <- Reduce(intersect, lapply(data, colnames))
  meta_cols <- setdiff(shared_cols, ir_scr_cols)
  constant <- vapply(
    meta_cols,
    function(col) {
      all(vapply(
        data,
        function(df) {
          length(unique(df[[col]])) == 1L
        },
        logical(1)
      ))
    },
    logical(1)
  )
  meta_cols <- meta_cols[constant]
  if (length(meta_cols) == 0L) {
    return(NULL)
  }
  meta <- data.frame(
    .sample_name = names(data),
    stringsAsFactors = FALSE
  )
  for (col in meta_cols) {
    meta[[col]] <- vapply(
      data,
      function(df) as.character(df[[col]][1]),
      character(1)
    )
  }
  meta
}

detect_chains <- function(data) {
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(character(0))
  }
  sample_dfs <- data[seq_len(min(length(data), 3))]
  all_ct <- unlist(lapply(sample_dfs, function(df) {
    if ("CTgene" %in% names(df)) as.character(df$CTgene) else character(0)
  }))
  chains <- character(0)
  if (any(grepl("TRA", all_ct))) {
    chains <- c(chains, "TRA")
  }
  if (any(grepl("TRB", all_ct))) {
    chains <- c(chains, "TRB")
  }
  if (any(grepl("TRG", all_ct))) {
    chains <- c(chains, "TRG")
  }
  if (any(grepl("TRD", all_ct))) {
    chains <- c(chains, "TRD")
  }
  if (any(grepl("IGH", all_ct))) {
    chains <- c(chains, "IGH")
  }
  if (any(grepl("IGK", all_ct))) {
    chains <- c(chains, "IGK")
  }
  if (any(grepl("IGL", all_ct))) {
    chains <- c(chains, "IGL")
  }
  chains
}

## ---- bindCache fallback for Shiny < 1.6.0 ---------------------------- ##
## cache = "session" ensures caches are NOT shared across users/sessions.
## available_crb_files$selected is appended to every key so switching datasets
## invalidates the cache (prevents stale plots from the previous dataset).
ir_bindCache <- function(x, ..., cache = "session") {
  if (utils::packageVersion("shiny") >= "1.6.0") {
    # Keep only truly global plot state here. Plot-specific parameters belong
    # in each renderer's own bindCache call.
    shiny::bindCache(
      x,
      ...,
      input$ir_d_base_size,
      input$ir_d_title,
      input$ir_d_legend_size,
      input$ir_d_legend_key,
      input$ir_d_legend_pos,
      available_crb_files$selected,
      cache = cache
    )
  } else {
    x
  }
}

source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/param_spec.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/data.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/settings.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/tabs.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/help.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/help_guide.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/paired_scatter_helpers.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/length_helpers.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/compare_helpers.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/native_metrics.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/precomputed.R"
  ),
  local = TRUE
)
source(
  paste0(
    Cerebro.options[["cerebro_root"]],
    "/shiny/v1.4/immune_repertoire/visualizations.R"
  ),
  local = TRUE
)
