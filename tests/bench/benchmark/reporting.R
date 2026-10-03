# ---- reporting.R ----
# Aggregation and formatting helpers for benchmark reports.

bench_summarise_metrics <- function(x, group, metrics) {
  missing <- setdiff(c(group, metrics, "status"), names(x))
  if (length(missing)) {
    stop("missing summary columns: ", paste(missing, collapse = ", "))
  }
  ok <- x[x$status == "OK", , drop = FALSE]
  if (!nrow(ok)) {
    return(data.frame())
  }
  key <- interaction(ok[group], drop = TRUE, lex.order = TRUE)
  rows <- split(seq_len(nrow(ok)), key)
  out <- lapply(rows, function(index) {
    group_row <- ok[index[1], group, drop = FALSE]
    group_row$rows_n <- length(index)
    for (metric in metrics) {
      values <- ok[[metric]][index]
      values <- values[is.finite(values)]
      group_row[[paste0(metric, "_median")]] <- if (length(values)) {
        stats::median(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_min")]] <- if (length(values)) {
        min(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_max")]] <- if (length(values)) {
        max(values)
      } else {
        NA_real_
      }
      group_row[[paste0(metric, "_n")]] <- length(values)
    }
    group_row
  })
  rownames(out) <- NULL
  do.call(rbind, out)
}

bench_format_interval <- function(median, minimum, maximum, n, digits = 2L) {
  if (
    !is.finite(median) || !is.finite(minimum) || !is.finite(maximum) || n < 1L
  ) {
    return("--")
  }
  sprintf(
    paste0("%.", digits, "f [%.", digits, "f-%.", digits, "f], n=%d"),
    median,
    minimum,
    maximum,
    as.integer(n)
  )
}

bench_evidence_notice <- function(profile) {
  if (isTRUE(profile$evidence_grade)) {
    paste0(
      "Benchmark evidence: backend comparisons use independent ",
      "process repeats and correctness fingerprints."
    )
  } else {
    paste0(
      "Exploratory evidence only: the ",
      profile$name,
      " profile is useful for harness validation but must not support the ",
      "user-facing performance conclusions."
    )
  }
}
