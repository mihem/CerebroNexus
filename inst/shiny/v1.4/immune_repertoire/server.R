##----------------------------------------------------------------------------##
## Tab: Immune Repertoire server — entry point
##----------------------------------------------------------------------------##

local({
  has_scRepertoire <- function() {
    requireNamespace("scRepertoire", quietly = TRUE)
  }

  safeRenderPlot <- function(expr, plot_name = "unknown") {
    tryCatch(
      {
        expr
      },
      error = function(e) {
        message("[IR ERROR] Plot '", plot_name, "' failed: ", e$message)
        plot.new()
        text(
          0.5,
          0.5,
          paste("Error in", plot_name, ":\n", e$message),
          cex = 0.8
        )
      }
    )
  }

  ## ---- BCR-specific helper: extract isotype ------------------------------- ##
  bcr_extract_isotype <- function(combined_BCR) {
    purrr::map_dfr(combined_BCR, function(df) {
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
    })
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

    pal <- c(
      IGHM = "#E41A1C",
      IGHD = "#FF7F00",
      IGHG1 = "#377EB8",
      IGHG2 = "#4DAF4A",
      IGHG3 = "#984EA3",
      IGHG4 = "#A65628",
      IGHA1 = "#F781BF",
      IGHA2 = "#999999",
      IGHE = "#FFFF33"
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
    diversity <- purrr::map_dfr(combined_BCR, function(df) {
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
    })

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
      ggplot2::geom_boxplot(outlier.size = 0.4, fill = "#9DC8E2", alpha = 0.7) +
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
    scr_cols <- c(
      "barcode",
      "CTgene",
      "CTnt",
      "CTaa",
      "CTstrict",
      "clonalProportion",
      "clonalFrequency",
      "cloneSize"
    )
    shared_cols <- Reduce(intersect, lapply(data, colnames))
    meta_cols <- setdiff(shared_cols, scr_cols)
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
      "/shiny/v1.4/immune_repertoire/visualizations.R"
    ),
    local = TRUE
  )
})
