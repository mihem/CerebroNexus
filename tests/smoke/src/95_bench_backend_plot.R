#!/usr/bin/env Rscript
# 95 — Visualize backend benchmark results from 93 (server) + 94 (web).
#
# Reads result/93_bench_backend_compare/{summary.csv, web_load.csv} and writes
# a single PNG (and PDF) with five panels:
#   1. Disk footprint (stacked: crb + sibling)
#   2. Startup time   (stacked: load + attach)
#   3. RSS after load + attach
#   4. Query latency  (cold / hot p50 / hot p95 / bulk, grouped)
#   5. Web load time  (URL -> "cell count visible" in the browser; full-width)
#
# Depends on: 93_bench_backend_compare.R having written summary.csv. web_load.csv
# (from 94_bench_web_load.R) is optional (if missing, panel 5 is skipped).
# Output: result/93_bench_backend_compare/{summary.png, summary.pdf}

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(scales)
})

result_dir <- "result/93_bench_backend_compare"
csv_path <- file.path(result_dir, "summary.csv")
if (!file.exists(csv_path)) {
  stop(sprintf("summary.csv not found at %s -- run 93_bench_backend_compare.R first.", csv_path))
}

df <- read.csv(csv_path, stringsAsFactors = FALSE)
df$backend <- factor(df$backend, levels = c("embedded", "bpcells", "h5"))

pal <- c(embedded = "#1f77b4", bpcells = "#2ca02c", h5 = "#d62728")

# Shared theme + axis style
theme_set(
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
)

# 1. Disk footprint -- stacked
disk_long <- rbind(
  data.frame(backend = df$backend, part = "crb",     mb = df$disk_crb_mb,
             stringsAsFactors = FALSE),
  data.frame(backend = df$backend,
             part = "sibling",
             mb = ifelse(is.na(df$disk_sibling_mb), 0, df$disk_sibling_mb),
             stringsAsFactors = FALSE)
)
disk_long$part <- factor(disk_long$part, levels = c("sibling", "crb"))

p1 <- ggplot(disk_long, aes(x = backend, y = mb, fill = part)) +
  geom_col(width = 0.7) +
  geom_text(
    data = df,
    aes(x = backend, y = disk_total_mb, label = sprintf("%.0f MB", disk_total_mb)),
    inherit.aes = FALSE, vjust = -0.4, size = 3.3
  ) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c(crb = "#9ecae1", sibling = "#3182bd")) +
  labs(title = "Disk footprint", y = "megabytes", x = NULL, fill = NULL)

# 2. Startup -- stacked load + attach
start_long <- rbind(
  data.frame(backend = df$backend, phase = "load",   secs = df$load_secs,
             stringsAsFactors = FALSE),
  data.frame(backend = df$backend, phase = "attach", secs = df$attach_secs,
             stringsAsFactors = FALSE)
)
start_long$phase <- factor(start_long$phase, levels = c("attach", "load"))
df$start_total <- df$load_secs + df$attach_secs

p2 <- ggplot(start_long, aes(x = backend, y = secs, fill = phase)) +
  geom_col(width = 0.7) +
  geom_text(
    data = df,
    aes(x = backend, y = start_total, label = sprintf("%.1f s", start_total)),
    inherit.aes = FALSE, vjust = -0.4, size = 3.3
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c(load = "#fdae6b", attach = "#e6550d")) +
  labs(title = "Startup time (load + attach)", y = "seconds", x = NULL, fill = NULL)

# 3. RSS bar
p3 <- ggplot(df, aes(x = backend, y = rss_mb, fill = backend)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.0f MB", rss_mb)),
            vjust = -0.4, size = 3.3) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(title = "Memory after load + attach", y = "RSS (MB)", x = NULL)

# 4. Query latency -- grouped bars
lat_long <- rbind(
  data.frame(backend = df$backend, op = "cold (1 gene)",   secs = df$cold_secs,
             stringsAsFactors = FALSE),
  data.frame(backend = df$backend, op = "hot p50",         secs = df$hot_p50_secs,
             stringsAsFactors = FALSE),
  data.frame(backend = df$backend, op = "hot p95",         secs = df$hot_p95_secs,
             stringsAsFactors = FALSE),
  data.frame(backend = df$backend, op = "bulk (50 genes)", secs = df$bulk_secs,
             stringsAsFactors = FALSE)
)
lat_long$op <- factor(lat_long$op,
                      levels = c("cold (1 gene)", "hot p50", "hot p95", "bulk (50 genes)"))

p4 <- ggplot(lat_long, aes(x = op, y = secs, fill = backend)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", secs)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 2.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = pal) +
  labs(title = "Query latency", y = "seconds", x = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 12, hjust = 0.7))

# 5. Web load time (URL -> cell count visible) -- optional, only if web_load.csv exists
web_csv <- file.path(result_dir, "web_load.csv")
p5 <- NULL
if (file.exists(web_csv)) {
  web <- read.csv(web_csv, stringsAsFactors = FALSE)
  web$backend <- factor(web$backend, levels = c("embedded", "bpcells", "h5"))
  web$cell_count_visible_s <- web$cell_count_visible_ms / 1000

  p5 <- ggplot(web, aes(x = backend, y = cell_count_visible_s, fill = backend)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = sprintf("%.1f s", cell_count_visible_s)),
              vjust = -0.4, size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_fill_manual(values = pal, guide = "none") +
    labs(
      title = "Web load time: URL -> cell count visible (callr + chromote, cold)",
      y = "seconds", x = NULL
    )
}

n_genes_str <- format(df$n_genes[1], big.mark = ",")
n_cells_str <- format(df$n_cells[1], big.mark = ",")
caption <- sprintf(
  "PBMC All Samples (%s genes x %s cells); generated by 95_bench_backend_plot.R",
  n_genes_str, n_cells_str
)

grid <- (p1 | p2) / (p3 | p4)
combined <- if (is.null(p5)) grid else grid / p5 + plot_layout(heights = c(1, 1, 0.7))
combined <- combined +
  plot_annotation(
    title    = "Expression backend comparison: embedded vs bpcells vs h5",
    caption  = caption,
    theme    = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.caption = element_text(colour = "grey50", size = 9, hjust = 0)
    )
  )

png_path <- file.path(result_dir, "summary.png")
pdf_path <- file.path(result_dir, "summary.pdf")
plot_height <- if (is.null(p5)) 8 else 11
ggsave(png_path, combined, width = 11, height = plot_height, dpi = 150)
ggsave(pdf_path, combined, width = 11, height = plot_height)

cat(sprintf("wrote %s\n", png_path))
cat(sprintf("wrote %s\n", pdf_path))
