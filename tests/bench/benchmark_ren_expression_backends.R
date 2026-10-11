# Focused, non-publication comparison of two already prepared Ren v2 CRBs.
#
# Usage:
#   Rscript tests/bench/benchmark_ren_expression_backends.R \
#     REN_BPCELLS.crb REN_H5.crb OUTPUT_PREFIX [ROUNDS]
#
# Produces OUTPUT_PREFIX.tsv and OUTPUT_PREFIX.png. Each measurement runs in a
# new R process. The query panel and value fingerprints are frozen in a separate
# planning process, so measurements compare the same genes and reject CRBs that
# do not contain the same expression values. This is deliberately narrower than
# the publication matrix: it answers the maintainer's Ren-specific BPCells/H5
# question without claiming controlled cold-disk latency or universal results.

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script <- if (length(script_arg)) {
  normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("tests/bench/benchmark_ren_expression_backends.R")
}
repo <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)

fingerprint <- function(value) {
  path <- tempfile("ren-backend-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  realized <- if (isS4(value)) {
    as.matrix(value)
  } else {
    value
  }
  saveRDS(
    list(dim = dim(realized), value = as.numeric(realized)),
    path,
    version = 3
  )
  unname(as.character(tools::md5sum(path)))
}

load_crb <- function(path) {
  suppressPackageStartupMessages(devtools::load_all(repo, quiet = TRUE))
  CerebroNexus::readCerebro(path)
}

if (length(args) && identical(args[[1L]], "--plan")) {
  if (length(args) != 3L) {
    stop("worker --plan needs <crb> <plan>", call. = FALSE)
  }
  object <- load_crb(normalizePath(args[[2L]], mustWork = TRUE))
  genes <- object$getGeneNames()
  if (length(genes) < 2L) {
    stop("the Ren CRB needs at least two genes", call. = FALSE)
  }
  take <- unique(as.integer(round(seq(1, length(genes), length.out = 12L))))
  panel <- genes[take]
  row <- object$getExpressionRow(panel[[1L]])
  block <- object$getExpressionBlock(panel)
  plan <- list(
    cells = length(object$getCellNames()),
    genes = length(genes),
    panel = panel,
    row_fingerprint = fingerprint(row),
    block_fingerprint = fingerprint(block)
  )
  saveRDS(plan, args[[3L]], version = 3)
  quit(save = "no", status = 0L)
}

if (length(args) && identical(args[[1L]], "--measure")) {
  if (length(args) != 6L) {
    stop(
      "worker --measure needs <crb> <backend> <plan> <round> <result>",
      call. = FALSE
    )
  }
  crb <- normalizePath(args[[2L]], mustWork = TRUE)
  expected_backend <- args[[3L]]
  plan <- readRDS(args[[4L]])
  round <- as.integer(args[[5L]])
  started <- proc.time()[["elapsed"]]
  object <- load_crb(crb)
  startup <- unname(proc.time()[["elapsed"]] - started)
  backend <- object$getExpressionBackend()$type
  if (!identical(backend, expected_backend)) {
    stop(
      "expected ",
      expected_backend,
      " CRB, found ",
      backend,
      call. = FALSE
    )
  }
  if (
    length(object$getCellNames()) != plan$cells ||
      length(object$getGeneNames()) != plan$genes
  ) {
    stop("the BPCells and H5 CRBs have different dimensions", call. = FALSE)
  }
  timed <- function(expr) {
    started <- proc.time()[["elapsed"]]
    value <- force(expr)
    list(seconds = unname(proc.time()[["elapsed"]] - started), value = value)
  }
  first <- timed(object$getExpressionRow(plan$panel[[1L]]))
  invisible(lapply(plan$panel, object$getExpressionRow))
  hot <- vapply(
    rep(plan$panel, 3L),
    function(gene) timed(object$getExpressionRow(gene))$seconds,
    numeric(1)
  )
  # getExpressionBlock() returns a lazy view for both external backends.
  # Include materialization here; otherwise this only times view construction.
  block <- timed(as.matrix(object$getExpressionBlock(plan$panel)))
  row_ok <- identical(fingerprint(first$value), plan$row_fingerprint)
  block_ok <- identical(fingerprint(block$value), plan$block_fingerprint)
  result <- data.frame(
    round = round,
    backend = backend,
    cells = plan$cells,
    genes = plan$genes,
    startup_seconds = startup,
    first_gene_seconds = first$seconds,
    hot_gene_p50_seconds = unname(stats::median(hot)),
    hot_gene_p95_seconds = unname(stats::quantile(hot, 0.95)),
    twelve_gene_seconds = block$seconds,
    correctness = if (row_ok && block_ok) "OK" else "MISMATCH",
    stringsAsFactors = FALSE
  )
  saveRDS(result, args[[6L]], version = 3)
  quit(save = "no", status = if (row_ok && block_ok) 0L else 2L)
}

if (length(args) < 3L || length(args) > 4L) {
  stop(
    paste(
      "usage: Rscript benchmark_ren_expression_backends.R",
      "REN_BPCELLS.crb REN_H5.crb OUTPUT_PREFIX [ROUNDS]"
    ),
    call. = FALSE
  )
}

crbs <- c(
  bpcells = normalizePath(args[[1L]], mustWork = TRUE),
  h5 = normalizePath(args[[2L]], mustWork = TRUE)
)
prefix <- normalizePath(args[[3L]], mustWork = FALSE)
rounds <- if (length(args) == 4L) as.integer(args[[4L]]) else 5L
if (is.na(rounds) || rounds < 1L) {
  stop("ROUNDS must be a positive integer", call. = FALSE)
}
dir.create(dirname(prefix), recursive = TRUE, showWarnings = FALSE)
plan <- tempfile("ren-backend-plan-", fileext = ".rds")

run_worker <- function(worker_args) {
  stdout <- tempfile("ren-backend-stdout-")
  stderr <- tempfile("ren-backend-stderr-")
  on.exit(unlink(c(stdout, stderr)), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script), worker_args),
    stdout = stdout,
    stderr = stderr
  )
  if (!identical(status, 0L)) {
    stop(
      paste(
        c(readLines(stdout, warn = FALSE), readLines(stderr, warn = FALSE)),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }
}

run_worker(c("--plan", shQuote(crbs[["bpcells"]]), shQuote(plan)))
rows <- list()
at <- 0L
for (round in seq_len(rounds)) {
  order <- if (round %% 2L) c("bpcells", "h5") else c("h5", "bpcells")
  for (backend in order) {
    result <- tempfile("ren-backend-result-", fileext = ".rds")
    run_worker(c(
      "--measure",
      shQuote(crbs[[backend]]),
      backend,
      shQuote(plan),
      round,
      shQuote(result)
    ))
    at <- at + 1L
    rows[[at]] <- readRDS(result)
    unlink(result)
    message(
      "Completed round ",
      round,
      ": ",
      backend,
      "; first gene ",
      sprintf("%.3fs", rows[[at]]$first_gene_seconds),
      "; materialized block ",
      sprintf("%.3fs", rows[[at]]$twelve_gene_seconds)
    )
  }
}
results <- do.call(rbind, rows)
if (!all(results$correctness == "OK")) {
  stop(
    "at least one backend returned different expression values",
    call. = FALSE
  )
}
tsv <- paste0(prefix, ".tsv")
png <- paste0(prefix, ".png")
utils::write.table(results, tsv, sep = "\t", quote = FALSE, row.names = FALSE)

metrics <- c(
  startup_seconds = "Startup",
  first_gene_seconds = "First gene",
  hot_gene_p50_seconds = "Warm gene p50",
  twelve_gene_seconds = "12-gene block (materialized)"
)
long <- do.call(
  rbind,
  lapply(names(metrics), function(metric) {
    data.frame(
      backend = results$backend,
      metric = metrics[[metric]],
      seconds = results[[metric]],
      stringsAsFactors = FALSE
    )
  })
)
summary <- stats::aggregate(seconds ~ backend + metric, long, stats::median)
summary$metric <- factor(summary$metric, levels = unname(metrics))
plot <- ggplot2::ggplot(
  summary,
  ggplot2::aes(x = backend, y = seconds, fill = backend)
) +
  ggplot2::geom_col(width = 0.65) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3fs", seconds)),
    vjust = -0.25,
    size = 3.5
  ) +
  ggplot2::facet_wrap(~metric, scales = "free_y", nrow = 1) +
  ggplot2::scale_fill_manual(values = c(bpcells = "#D9A03C", h5 = "#3F7F93")) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
  ggplot2::labs(
    title = "Ren v2 expression backend comparison",
    subtitle = paste0(
      "Median of ",
      rounds,
      " independent warm-cache processes; identical values required"
    ),
    x = NULL,
    y = "Seconds"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(legend.position = "none")
ggplot2::ggsave(png, plot, width = 12, height = 4.5, dpi = 160)
unlink(plan)
message("Wrote ", tsv)
message("Wrote ", png)
