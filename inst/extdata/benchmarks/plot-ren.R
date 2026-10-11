# Shared by the vignette and repository figure-reproduction command.
plotRenBackendBenchmark <- function(data) {
  stopifnot(
    all(data$correctness == "OK"),
    setequal(data$backend, c("bpcells", "h5"))
  )
  metrics <- c(
    startup_seconds = "Load source + CRB",
    first_gene_seconds = "First gene",
    hot_gene_p50_seconds = "Repeated gene",
    twelve_gene_seconds = "12-gene block"
  )
  old <- graphics::par(
    mfrow = c(1, 4),
    mar = c(4, 4, 3, 1),
    oma = c(0, 0, 3, 0)
  )
  on.exit(graphics::par(old))
  for (metric in names(metrics)) {
    medians <- vapply(
      c("bpcells", "h5"),
      function(backend) {
        stats::median(data[data$backend == backend, metric])
      },
      numeric(1)
    )
    mid <- graphics::barplot(
      medians,
      names.arg = c("BPCells", "HDF5"),
      col = c("#D9A03C", "#3F7F93"),
      border = NA,
      ylim = c(0, max(medians) * 1.27),
      main = metrics[[metric]],
      ylab = "Seconds"
    )
    graphics::text(mid, medians, sprintf("%.3f", medians), pos = 3, cex = .9)
  }
  graphics::mtext(
    paste0(
      "Ren v2: median of ",
      length(unique(data$round)),
      " fresh-process rounds (historical measurements)"
    ),
    outer = TRUE,
    side = 3,
    line = 1,
    cex = 1.1
  )
  invisible(NULL)
}
