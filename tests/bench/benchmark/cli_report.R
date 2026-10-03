# Commands: report, figure.
if (command %in% c("report", "figure")) {
  .bench_cli_handled <- TRUE
if (identical(command, "report")) {
  # Generate an uncertainty-aware Markdown report from a staged or published run.
  #
  # Usage: Rscript cli.R report <result_dir>

  args <- command_args
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }

  if (!length(args)) {
    stop("need <result_dir>", call. = FALSE)
  }
  result_dir <- normalizePath(args[1], mustWork = TRUE)

  read_if <- function(name) {
    path <- file.path(result_dir, name)
    if (file.exists(path)) {
      utils::read.csv(path, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  }

  probe <- read_if("00_probe.csv")
  exports <- read_if("10_export.csv")
  access <- read_if("20_access.csv")
  crashes <- read_if("crashes.csv")
  manifest <- read_if("run_manifest.csv")
  source_manifest <- read_if("source_manifest.csv")

  if (is.null(manifest)) {
    stop(
      "run_manifest.csv is required for a scientific benchmark report",
      call. = FALSE
    )
  }
  manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
  profile <- bench_profile(manifest_values[["profile"]])

  interval <- function(row, metric, digits = 2L) {
    bench_format_interval(
      row[[paste0(metric, "_median")]],
      row[[paste0(metric, "_min")]],
      row[[paste0(metric, "_max")]],
      row[[paste0(metric, "_n")]],
      digits = digits
    )
  }

  out <- c(
    "# Expression-backend benchmark on real public datasets",
    "",
    sprintf("**Run:** `%s`  ", manifest_values[["run_id"]]),
    sprintf("**Profile:** `%s`  ", profile$name),
    sprintf("**Git:** `%s`", manifest_values[["git_sha"]]),
    "",
    paste0("> **Evidence status.** ", bench_evidence_notice(profile)),
    ""
  )

  if (!is.null(probe) && nrow(probe)) {
    out <- c(
      out,
      "## Sources",
      "",
      "| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |",
      "|---|---:|---:|---:|---:|---:|:--:|"
    )
    for (i in seq_len(nrow(probe))) {
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %.3e | %.0f | %.1f GB | %s |",
          probe$label[i],
          format(probe$n_cells[i], big.mark = ","),
          format(probe$n_genes[i], big.mark = ","),
          probe$nnz[i],
          probe$nnz_per_cell[i],
          probe$dgc_gb_full[i],
          if (probe$dgc_representable[i]) "yes" else "**no**"
        )
      )
    }
    out <- c(out, "")
  }

  if (!is.null(exports) && nrow(exports)) {
    if (!"peak_rss_mb" %in% names(exports)) {
      exports$peak_rss_mb <- NA_real_
    }
    for (name in c("shell_secs", "serialize_secs")) {
      if (!name %in% names(exports)) {
        exports[[name]] <- NA_real_
      }
    }
    exports$r_peak_mb[exports$r_peak_mb > 4e6] <- NA_real_
    export_summary <- bench_summarise_metrics(
      exports,
      group = c("source", "n_cells", "backend"),
      metrics = c(
        "crb_mb",
        "sibling_mb",
        "total_mb",
        "export_secs",
        "shell_secs",
        "serialize_secs",
        "r_peak_mb",
        "peak_rss_mb"
      )
    )
    export_summary <- export_summary[
      order(
        export_summary$source,
        export_summary$n_cells,
        export_summary$backend
      ),
      ,
      drop = FALSE
    ]
    out <- c(
      out,
      "## Export",
      "",
      "Values are median [minimum-maximum], followed by the number of independent export processes.",
      "",
      paste0(
        "| source | cells | backend | total MB | backend build s | shell s | ",
        "CRB serialization s | peak R heap MB | peak process RSS MB |"
      ),
      "|---|---:|---|---:|---:|---:|---:|---:|---:|"
    )
    for (i in seq_len(nrow(export_summary))) {
      row <- export_summary[i, , drop = FALSE]
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %s | %s | %s | %s | %s | %s |",
          row$source,
          format(row$n_cells, big.mark = ","),
          row$backend,
          interval(row, "total_mb", 1L),
          interval(row, "export_secs", 1L),
          interval(row, "shell_secs", 2L),
          interval(row, "serialize_secs", 2L),
          interval(row, "r_peak_mb", 0L),
          interval(row, "peak_rss_mb", 0L)
        )
      )
    }
    out <- c(out, "")

    failed <- exports[exports$status != "OK", , drop = FALSE]
    if (nrow(failed)) {
      out <- c(
        out,
        "### Caught export failures",
        "",
        "| source | cells | backend | repeat | status |",
        "|---|---:|---|---:|---|"
      )
      for (i in seq_len(nrow(failed))) {
        out <- c(
          out,
          sprintf(
            "| %s | %s | %s | %d | %s |",
            failed$source[i],
            format(failed$n_cells[i], big.mark = ","),
            failed$backend[i],
            failed$export_repeat[i],
            substr(failed$status[i], 1L, 100L)
          )
        )
      }
      out <- c(out, "")
    }
  }

  if (!is.null(access) && nrow(access)) {
    if (!"peak_rss_mb" %in% names(access)) {
      access$peak_rss_mb <- NA_real_
    }
    if (!"startup_secs" %in% names(access)) {
      access$startup_secs <- access$load_secs + access$attach_secs
    }
    if (!"subset_row_secs" %in% names(access)) {
      access$subset_row_secs <- NA_real_
      access$subset_block_secs <- NA_real_
    }
    access_summary <- bench_summarise_metrics(
      access,
      group = c("source", "n_cells", "backend"),
      metrics = c(
        "startup_secs",
        "rss_mb",
        "peak_rss_mb",
        "first_query_secs",
        "hot_p50_secs",
        "hot_p95_secs",
        "block_secs",
        "subset_row_secs",
        "subset_block_secs"
      )
    )
    access_summary <- access_summary[
      order(
        access_summary$source,
        access_summary$n_cells,
        access_summary$backend
      ),
      ,
      drop = FALSE
    ]
    out <- c(
      out,
      "## Runtime access",
      "",
      paste0(
        "The first-query metric is the first backend getter call in a fresh R process. ",
        "The operating-system file cache is uncontrolled, so it is not a cold-disk measurement."
      ),
      "",
      paste0(
        "| source | cells | backend | startup s | RSS MB | ",
        "peak process RSS MB | first query s | warmed p50 s | warmed p95 s | ",
        "12-gene full block s | shuffled 100k row s | shuffled 100k block s |"
      ),
      "|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
    )
    for (i in seq_len(nrow(access_summary))) {
      row <- access_summary[i, , drop = FALSE]
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
          row$source,
          format(row$n_cells, big.mark = ","),
          row$backend,
          interval(row, "startup_secs", 2L),
          interval(row, "rss_mb", 0L),
          interval(row, "peak_rss_mb", 0L),
          interval(row, "first_query_secs", 4L),
          interval(row, "hot_p50_secs", 4L),
          interval(row, "hot_p95_secs", 4L),
          interval(row, "block_secs", 3L),
          interval(row, "subset_row_secs", 4L),
          interval(row, "subset_block_secs", 3L)
        )
      )
    }
    out <- c(
      out,
      "",
      sprintf(
        "Correctness: %d/%d access processes matched both source-matrix fingerprints.",
        sum(access$correctness == "OK", na.rm = TRUE),
        nrow(access)
      ),
      ""
    )
  }

  if (
    !bench_is_full_profile(profile) &&
      !is.null(exports) &&
      nrow(exports)
  ) {
    usable <- exports[
      exports$backend == "embedded" &
        exports$status == "OK" &
        is.finite(exports$r_peak_mb) &
        !is.na(exports$nnz),
      ,
      drop = FALSE
    ]
    if (nrow(usable)) {
      ceiling_points <- bench_summarise_metrics(
        usable,
        group = c("source", "n_cells", "nnz"),
        metrics = "r_peak_mb"
      )
      ceiling_points$bytes_per_nnz <-
        ceiling_points$r_peak_mb_median * 2^20 / ceiling_points$nnz
      estimate <- stats::median(ceiling_points$bytes_per_nnz)
      out <- c(
        out,
        "## Host-specific scale-limit estimate",
        "",
        sprintf(
          paste0(
            "Across %d distinct source/tier points, the median observed peak was ",
            "%.1f bytes per non-zero (range %.1f-%.1f). This is a descriptive ",
            "estimate for this exporter and host, not a universal memory law."
          ),
          nrow(ceiling_points),
          estimate,
          min(ceiling_points$bytes_per_nnz),
          max(ceiling_points$bytes_per_nnz)
        ),
        ""
      )
    }
  }

  if (!is.null(crashes) && nrow(crashes)) {
    out <- c(
      out,
      "## Processes killed outright",
      "",
      "| source | cells | backend | repeat | stage | exit |",
      "|---|---:|---|---:|---|---:|"
    )
    for (i in seq_len(nrow(crashes))) {
      out <- c(
        out,
        sprintf(
          "| %s | %s | %s | %d | %s | %d |",
          crashes$source[i],
          format(crashes$n_cells[i], big.mark = ","),
          crashes$backend[i],
          crashes$export_repeat[i],
          crashes$stage[i],
          crashes$exit_code[i]
        )
      )
    }
    out <- c(out, "")
  }

  out <- c(out, "## Provenance", "")
  provenance_keys <- c(
    "generated_at",
    "git_branch",
    "git_dirty",
    "package_version",
    "r_version",
    "os",
    "cpu",
    "logical_cores",
    "benchmark_threads",
    "slurm_job_id",
    "slurm_node_list",
    "slurm_cpus_per_task",
    "slurm_memory_per_node",
    "memory_mb",
    "r_vector_limit_mb"
  )
  out <- c(out, "| key | value |", "|---|---|")
  for (key in provenance_keys[provenance_keys %in% names(manifest_values)]) {
    out <- c(out, sprintf("| %s | %s |", key, manifest_values[[key]]))
  }
  if (!is.null(source_manifest)) {
    for (i in seq_len(nrow(source_manifest))) {
      out <- c(
        out,
        sprintf(
          "| source `%s` | %s bytes; SHA-256 `%s` |",
          source_manifest$source[i],
          format(source_manifest$bytes[i], big.mark = ","),
          source_manifest$sha256[i]
        )
      )
    }
  }
  out <- c(out, "")

  path <- file.path(result_dir, "summary.md")
  writeLines(out, path, useBytes = TRUE)
  cat(paste(out, collapse = "\n"), "\n")
  message("\nwrote ", path)
} else if (identical(command, "figure")) {
  # Generate uncertainty-aware figures from the current benchmark run.
  #
  # Usage: Rscript cli.R figure <result_dir> <out_dir>

  args <- command_args
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }

  if (!length(args)) {
    stop("need <result_dir>", call. = FALSE)
  }
  result_dir <- normalizePath(args[1], mustWork = TRUE)
  if (length(args) < 2L) {
    stop("need <result_dir> <out_dir>", call. = FALSE)
  }
  out_dir <- args[2]

  suppressPackageStartupMessages({
    library(ggplot2)
    library(patchwork)
  })

  manifest <- utils::read.csv(
    file.path(result_dir, "run_manifest.csv"),
    stringsAsFactors = FALSE
  )
  manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
  profile <- bench_profile(manifest_values[["profile"]])
  bench_require_evidence_profile(profile)

  exports <- utils::read.csv(
    file.path(result_dir, "10_export.csv"),
    stringsAsFactors = FALSE
  )
  access <- utils::read.csv(
    file.path(result_dir, "20_access.csv"),
    stringsAsFactors = FALSE
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  exports$r_peak_mb[exports$r_peak_mb > 4e6] <- NA_real_
  if (!"startup_secs" %in% names(access)) {
    access$startup_secs <- access$load_secs + access$attach_secs
  }
  export_summary <- bench_summarise_metrics(
    exports,
    group = c("source", "n_cells", "backend"),
    metrics = c("total_mb", "export_secs", "r_peak_mb")
  )
  access_summary <- bench_summarise_metrics(
    access,
    group = c("source", "n_cells", "backend"),
    metrics = c(
      "startup_secs",
      "rss_mb",
      "first_query_secs",
      "hot_p50_secs",
      "block_secs"
    )
  )

  pretty_source <- c(
    mouse_brain_e18 = "10x mouse brain E18",
    human_pfc_hbcc = "human PFC cross-disorder (HBCC)",
    human_pfc_mssm = "human PFC cross-disorder (MSSM)"
  )
  add_source_label <- function(x) {
    label <- unname(pretty_source[x$source])
    label[is.na(label)] <- x$source[is.na(label)]
    x$src <- label
    x
  }
  export_summary <- add_source_label(export_summary)
  access_summary <- add_source_label(access_summary)

  backend_levels <- c("embedded", "bpcells", "h5")
  backend_cols <- c(embedded = "#B4553F", bpcells = "#D9A03C", h5 = "#3F7F93")
  export_summary$backend <- factor(
    export_summary$backend,
    levels = backend_levels
  )
  access_summary$backend <- factor(
    access_summary$backend,
    levels = backend_levels
  )

  base <- theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 10.5),
      plot.subtitle = element_text(size = 8.5, colour = "grey35")
    )

  k_cells <- function(x) paste0(x / 1000, "k")

  panel_line <- function(df, metric, title, subtitle, ylab, log_y = TRUE) {
    median <- paste0(metric, "_median")
    minimum <- paste0(metric, "_min")
    maximum <- paste0(metric, "_max")
    p <- ggplot(
      df,
      aes(
        .data[["n_cells"]],
        .data[[median]],
        colour = .data[["backend"]],
        group = .data[["backend"]]
      )
    ) +
      geom_errorbar(
        aes(ymin = .data[[minimum]], ymax = .data[[maximum]]),
        width = 0,
        linewidth = 0.45
      ) +
      geom_point(size = 1.9) +
      facet_wrap(~src) +
      scale_colour_manual(values = backend_cols) +
      scale_x_continuous(labels = k_cells) +
      labs(title = title, subtitle = subtitle, x = "cells", y = ylab) +
      base
    if (length(unique(df$n_cells)) > 1L) {
      p <- p + geom_line(linewidth = 0.6)
    }
    if (log_y) {
      p <- p + scale_y_log10()
    }
    p
  }

  p_hot <- panel_line(
    access_summary,
    "hot_p50_secs",
    "Interactive single-gene latency",
    "warmed expression lookup; median and range; log scale",
    "seconds"
  )
  p_block <- panel_line(
    access_summary,
    "block_secs",
    "Marker-panel block latency",
    "12 genes across all cells; median and range; log scale",
    "seconds"
  )
  p_rss <- panel_line(
    access_summary,
    "rss_mb",
    "Resident memory after load and attach",
    "one process per measurement; median and range",
    "MB",
    log_y = FALSE
  )

  p_disk <- ggplot(
    export_summary,
    aes(
      factor(.data[["n_cells"]]),
      .data[["total_mb_median"]],
      fill = .data[["backend"]]
    )
  ) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    geom_errorbar(
      aes(
        ymin = .data[["total_mb_min"]],
        ymax = .data[["total_mb_max"]]
      ),
      position = position_dodge(width = 0.75),
      width = 0.15,
      linewidth = 0.4
    ) +
    facet_wrap(~src, scales = "free_x") +
    guides(fill = "none") +
    scale_fill_manual(values = backend_cols) +
    scale_x_discrete(labels = function(x) k_cells(as.numeric(x))) +
    labs(
      title = "Total on-disk footprint",
      subtitle = "CRB plus external sibling; median and range",
      x = "cells",
      y = "MB"
    ) +
    base

  overview <- (p_hot / p_block / p_rss / p_disk) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "bottom")

  ggsave(
    file.path(out_dir, "expression_backend_benchmark_overview.png"),
    overview,
    width = 8,
    height = 11,
    dpi = 150,
    bg = "white"
  )

  if (bench_is_full_profile(profile)) {
    message(
      sprintf(
        "wrote full-source benchmark figure from %s",
        manifest_values[["run_id"]]
      )
    )
    quit(status = 0L)
  }

  usable <- exports[
    exports$backend == "embedded" &
      exports$status == "OK" &
      is.finite(exports$r_peak_mb) &
      !is.na(exports$nnz),
    ,
    drop = FALSE
  ]
  points <- bench_summarise_metrics(
    usable,
    group = c("source", "n_cells", "nnz"),
    metrics = "r_peak_mb"
  )
  points <- add_source_label(points)
  points$bytes_per_nnz <- points$r_peak_mb_median * 2^20 / points$nnz
  bytes_per_nnz <- stats::median(points$bytes_per_nnz)
  limit_mb <- suppressWarnings(as.numeric(manifest_values[[
    "r_vector_limit_mb"
  ]]))
  if (!is.finite(limit_mb)) {
    limit_mb <- max(points$r_peak_mb_max) * 1.1
  }
  ceiling_nnz <- limit_mb * 2^20 / bytes_per_nnz

  failures <- exports[
    exports$backend == "embedded" &
      grepl("^FAILED", exports$status) &
      !is.na(exports$nnz),
  ]
  failures <- failures[!duplicated(failures[c("source", "n_cells", "nnz")]), ]
  failures <- add_source_label(failures)
  plot_points <- rbind(
    data.frame(
      nnz = points$nnz,
      value = points$r_peak_mb_median,
      minimum = points$r_peak_mb_min,
      maximum = points$r_peak_mb_max,
      src = points$src,
      outcome = "built"
    ),
    data.frame(
      nnz = failures$nnz,
      value = rep(limit_mb * 1.06, nrow(failures)),
      minimum = rep(limit_mb * 1.06, nrow(failures)),
      maximum = rep(limit_mb * 1.06, nrow(failures)),
      src = failures$src,
      outcome = rep("could not be built", nrow(failures))
    )
  )

  ceiling <- ggplot(
    plot_points,
    aes(
      .data[["nnz"]],
      .data[["value"]],
      colour = .data[["src"]],
      shape = .data[["outcome"]]
    )
  ) +
    geom_abline(
      slope = bytes_per_nnz / 2^20,
      intercept = 0,
      colour = "grey55",
      linetype = "22",
      linewidth = 0.5
    ) +
    geom_errorbar(
      aes(ymin = .data[["minimum"]], ymax = .data[["maximum"]]),
      width = 0,
      linewidth = 0.45
    ) +
    geom_hline(yintercept = limit_mb, colour = "#A6342A", linewidth = 0.5) +
    geom_vline(xintercept = ceiling_nnz, colour = "grey55", linewidth = 0.4) +
    geom_point(size = 2.6, stroke = 0.9) +
    coord_cartesian(ylim = c(0, limit_mb * 1.15)) +
    scale_x_continuous(labels = function(x) sprintf("%.1fe9", x / 1e9)) +
    scale_shape_manual(values = c(built = 16, `could not be built` = 4)) +
    labs(
      title = "Host-specific export scale estimate",
      subtitle = sprintf(
        "%d distinct source/tier points; median %.1f B/nnz (range %.1f-%.1f)",
        nrow(points),
        bytes_per_nnz,
        min(points$bytes_per_nnz),
        max(points$bytes_per_nnz)
      ),
      x = "non-zeros in the tier",
      y = "peak R heap (MB)"
    ) +
    base

  ggsave(
    file.path(out_dir, "expression_backend_benchmark_ceiling.png"),
    ceiling,
    width = 8,
    height = 4.4,
    dpi = 150,
    bg = "white"
  )

  message(
    sprintf(
      "wrote benchmark figures from %s (%d scale points)",
      manifest_values[["run_id"]],
      nrow(points)
    )
  )
}
}
