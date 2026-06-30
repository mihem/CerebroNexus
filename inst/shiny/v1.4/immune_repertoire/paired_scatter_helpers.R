## ---- Paired Scatter helpers ------------------------------------------- ##

ir_paired_scatter_choices <- function(meta) {
  empty <- list(
    mode = "unavailable",
    compare_candidates = character(0),
    facet_candidates = character(0),
    sample_choices = character(0)
  )
  if (
    is.null(meta) || nrow(meta) < 2L || !(".sample_name" %in% colnames(meta))
  ) {
    return(empty)
  }

  meta_cols <- setdiff(colnames(meta), ".sample_name")
  n_levels <- function(col) {
    vals <- unique(as.character(meta[[col]]))
    vals <- vals[!is.na(vals)]
    length(vals)
  }
  compare_candidates <- meta_cols[vapply(meta_cols, n_levels, integer(1)) == 2L]
  facet_candidates <- meta_cols[vapply(meta_cols, n_levels, integer(1)) >= 2L]
  sample_choices <- as.character(meta$.sample_name)
  sample_choices <- sample_choices[
    !is.na(sample_choices) & nzchar(sample_choices)
  ]

  mode <- if (length(compare_candidates) > 0L) {
    "paired"
  } else if (length(sample_choices) >= 2L) {
    "manual"
  } else {
    "unavailable"
  }

  list(
    mode = mode,
    compare_candidates = compare_candidates,
    facet_candidates = facet_candidates,
    sample_choices = sample_choices
  )
}

ir_paired_scatter_default_facet <- function(
  meta,
  compare_col,
  facet_candidates
) {
  if (
    is.null(meta) ||
      is.null(compare_col) ||
      !nzchar(compare_col) ||
      !(compare_col %in% colnames(meta)) ||
      length(facet_candidates) == 0L
  ) {
    return("")
  }

  for (fc in facet_candidates[facet_candidates != compare_col]) {
    ok <- all(vapply(
      unique(meta[[fc]]),
      function(lv) {
        subs <- meta[meta[[fc]] == lv, , drop = FALSE]
        length(unique(subs[[compare_col]])) >= 2L
      },
      logical(1)
    ))
    if (ok) {
      return(fc)
    }
  }
  ""
}
