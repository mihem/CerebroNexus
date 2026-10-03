# ---- protocol.R ----
# Pure protocol helpers for the real-data expression-backend benchmark.

bench_profile <- function(name = Sys.getenv("BENCH_PROFILE", "quick")) {
  requested_name <- name
  aliases <- c(panel_c1 = "preview", panel_c2 = "full")
  if (name %in% names(aliases)) {
    name <- unname(aliases[[name]])
  }
  profiles <- list(
    quick = list(
      name = "quick",
      export_repeats = 1L,
      access_repeats = 1L,
      query_genes = 12L,
      hot_iterations = 1L,
      include_scale_tiers = FALSE,
      comparison_tier_mode = "smallest",
      evidence_grade = FALSE
    ),
    standard = list(
      name = "standard",
      export_repeats = 3L,
      access_repeats = 1L,
      query_genes = 12L,
      hot_iterations = 2L,
      include_scale_tiers = TRUE,
      comparison_tier_mode = "all",
      evidence_grade = FALSE
    ),
    scale = list(
      name = "scale",
      export_repeats = 5L,
      access_repeats = 2L,
      query_genes = 12L,
      hot_iterations = 3L,
      include_scale_tiers = TRUE,
      comparison_tier_mode = "all",
      evidence_grade = TRUE
    ),
    preview = list(
      name = "preview",
      export_repeats = 3L,
      access_repeats = 2L,
      query_genes = 12L,
      hot_iterations = 3L,
      include_scale_tiers = FALSE,
      comparison_tier_mode = "all",
      evidence_grade = TRUE
    ),
    full = list(
      name = "full",
      export_repeats = 5L,
      access_repeats = 2L,
      query_genes = 12L,
      hot_iterations = 3L,
      include_scale_tiers = FALSE,
      comparison_tier_mode = "all",
      evidence_grade = TRUE
    ),
    stress = list(
      name = "stress",
      export_repeats = 1L,
      access_repeats = 1L,
      query_genes = 12L,
      hot_iterations = 1L,
      include_scale_tiers = TRUE,
      comparison_tier_mode = "all",
      evidence_grade = FALSE
    )
  )
  profile <- profiles[[name]]
  if (is.null(profile)) {
    stop(
      "unknown benchmark profile: ",
      name,
      paste0(
        "; expected quick, standard, scale, preview, full, or stress"
      ),
      call. = FALSE
    )
  }
  if (!identical(requested_name, name)) {
    profile$name <- requested_name
    profile$canonical_name <- name
  }
  profile
}

bench_is_full_profile <- function(profile) {
  profile$name %in% c("full", "panel_c2")
}

bench_scale_schedule <- function(specs) {
  sources <- intersect(c("mouse_brain_e18", "human_pfc_hbcc"), names(specs))
  if (length(sources) != 2L) {
    stop(
      "scale profile requires the mouse and human sources",
      call. = FALSE
    )
  }
  embedded_specs <- external_specs <- specs
  for (source in sources) {
    embedded_specs[[source]]$tiers <-
      embedded_specs[[source]]$comparison_tiers <-
        specs[[source]]$comparison_tiers[
          specs[[source]]$comparison_tiers <= 500e3
        ]
    external_specs[[source]]$tiers <-
      external_specs[[source]]$comparison_tiers <-
        specs[[source]]$comparison_tiers[
          specs[[source]]$comparison_tiers > 500e3
        ]
  }
  rbind(
    bench_schedule(
      embedded_specs,
      "scale",
      sources = sources,
      backends = c("embedded", "bpcells", "h5")
    ),
    bench_schedule(
      external_specs,
      "scale",
      sources = sources,
      backends = c("bpcells", "h5")
    )
  )
}

bench_fixed_schedule <- function(
  specs,
  profile = c("preview", "full"),
  schedule_name = profile
) {
  if (length(profile) != 1L || !profile %in% c("preview", "full")) {
    stop("fixed profile must be preview or full", call. = FALSE)
  }
  sources <- intersect(c("mouse_brain_e18", "human_pfc_hbcc"), names(specs))
  if (length(sources) != 2L) {
    stop(
      "fixed profiles require the mouse and human benchmark sources",
      call. = FALSE
    )
  }
  field <- if (profile == "preview") "preview_cells" else "full_cells"
  missing <- sources[vapply(
    specs[sources],
    function(x) is.null(x[[field]]),
    logical(1)
  )]
  if (length(missing)) {
    stop("fixed-profile source metadata is missing ", field, call. = FALSE)
  }
  panel_specs <- specs[sources]
  for (source in sources) {
    tier <- panel_specs[[source]][[field]]
    panel_specs[[source]]$tiers <- tier
    panel_specs[[source]]$comparison_tiers <- tier
  }
  bench_schedule(
    panel_specs,
    schedule_name,
    sources = sources,
    backends = if (profile == "preview") {
      c("embedded", "bpcells", "h5")
    } else {
      c("bpcells", "h5")
    }
  )
}

bench_schedule <- function(
  specs,
  profile = "quick",
  sources = names(specs),
  backends = c("embedded", "bpcells", "h5")
) {
  if (is.character(profile)) {
    profile <- bench_profile(profile)
  }
  unknown <- setdiff(sources, names(specs))
  if (length(unknown)) {
    stop("unknown benchmark source: ", paste(unknown, collapse = ", "))
  }

  rows <- list()
  at <- 0L
  for (source in sources) {
    spec <- specs[[source]]
    comparison_tiers <- spec$comparison_tiers
    if (is.null(comparison_tiers)) {
      comparison_tiers <- min(spec$tiers)
    }
    if (identical(profile$comparison_tier_mode, "smallest")) {
      comparison_tiers <- min(comparison_tiers)
    }
    tiers <- if (isTRUE(profile$include_scale_tiers)) {
      spec$tiers
    } else {
      comparison_tiers
    }
    for (n_cells in tiers) {
      comparison <- n_cells %in% comparison_tiers
      n_repeats <- if (comparison) profile$export_repeats else 1L
      for (export_repeat in seq_len(n_repeats)) {
        shift <- (export_repeat - 1L) %% length(backends)
        order <- backends[
          ((seq_along(backends) + shift - 1L) %% length(backends)) + 1L
        ]
        for (position in seq_along(order)) {
          at <- at + 1L
          rows[[at]] <- data.frame(
            profile = profile$name,
            source = source,
            n_cells = as.numeric(n_cells),
            comparison = comparison,
            export_repeat = as.integer(export_repeat),
            order_position = as.integer(position),
            backend = order[position],
            access_repeats = if (comparison) profile$access_repeats else 1L,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, rows)
}

bench_stratified_gene_panel <- function(
  genes,
  nnz,
  n_genes = 50L
) {
  if (length(genes) != length(nnz)) {
    stop("genes and nnz must have the same length", call. = FALSE)
  }
  active <- data.frame(
    gene = as.character(genes),
    nnz = as.numeric(nnz),
    stringsAsFactors = FALSE
  )
  active <- active[
    is.finite(active$nnz) & active$nnz > 0 & nzchar(active$gene),
  ]
  if (!nrow(active)) {
    stop("no expressed genes are available for the query panel", call. = FALSE)
  }
  active <- active[order(active$nnz, active$gene), , drop = FALSE]
  n_take <- min(as.integer(n_genes), nrow(active))
  selected <- unique(round(seq(1, nrow(active), length.out = n_take)))
  panel <- active[selected, , drop = FALSE]

  first <- which.min(abs(panel$nnz - stats::median(active$nnz)))
  panel <- panel[c(first, setdiff(seq_len(nrow(panel)), first)), , drop = FALSE]
  panel$role <- c("first", rep("hot", nrow(panel) - 1L))
  rownames(panel) <- NULL
  panel
}

bench_serialized_fingerprint <- function(payload) {
  raw <- serialize(
    payload,
    connection = NULL,
    ascii = FALSE,
    xdr = TRUE,
    version = 3
  )
  path <- tempfile("cerebro-bench-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, open = "wb")
  writeBin(raw, con)
  close(con)
  as.character(unname(tools::md5sum(path)))
}

bench_numeric_fingerprint <- function(x) {
  dimensions <- dim(x)
  values <- if (is.null(dimensions)) {
    as.numeric(x)
  } else {
    as.numeric(as.matrix(x))
  }
  bench_serialized_fingerprint(list(
    dim = dimensions,
    values = values
  ))
}

bench_subset_cells <- function(n_cells, limit = 100000L) {
  if (
    length(n_cells) != 1L ||
      !is.finite(n_cells) ||
      n_cells < 1L ||
      n_cells != as.integer(n_cells)
  ) {
    stop("n_cells must be one positive integer", call. = FALSE)
  }
  count <- min(as.integer(n_cells), as.integer(limit))
  rev(unique(as.integer(round(seq.int(1, n_cells, length.out = count)))))
}

.bench_result_key <- function(x) {
  paste(
    x$source,
    sprintf("%.0f", as.numeric(x$n_cells)),
    x$backend,
    as.integer(x$export_repeat),
    sep = "|"
  )
}

bench_validate_results <- function(
  schedule,
  exports,
  access,
  crashes = data.frame(),
  profile = bench_profile("quick")
) {
  expected <- .bench_result_key(schedule)
  export_keys <- if (nrow(exports)) .bench_result_key(exports) else character()
  crash_keys <- if (
    nrow(crashes) &&
      all(
        c("source", "n_cells", "backend", "export_repeat") %in% names(crashes)
      )
  ) {
    export_crashes <- if ("stage" %in% names(crashes)) {
      crashes[crashes$stage == "export", , drop = FALSE]
    } else {
      crashes
    }
    .bench_result_key(export_crashes)
  } else {
    character()
  }
  outcome <- c(export_keys, crash_keys)
  counts <- table(factor(outcome, levels = expected))
  if (any(counts == 0L)) {
    stop(
      "missing export outcome for scheduled cell: ",
      names(counts)[counts == 0L][1],
      call. = FALSE
    )
  }
  if (any(counts > 1L) || anyDuplicated(expected)) {
    stop("duplicate export outcome for scheduled cell", call. = FALSE)
  }

  successful <- exports[
    identical(exports$status, "OK") | exports$status == "OK",
    ,
    drop = FALSE
  ]
  required <- schedule$comparison &
    !(identical(profile$name, "scale") &
      schedule$backend == "embedded")
  comparison_keys <- expected[required]
  if (!all(comparison_keys %in% .bench_result_key(successful))) {
    stop(
      "comparison tier did not complete every required backend",
      call. = FALSE
    )
  }

  if (nrow(access)) {
    required_access <- c(
      "status",
      "correctness",
      "row_fingerprint",
      "reference_row_fingerprint",
      "block_fingerprint",
      "reference_block_fingerprint"
    )
    subset_access <- c(
      "subset_row_fingerprint",
      "reference_subset_row_fingerprint",
      "subset_block_fingerprint",
      "reference_subset_block_fingerprint"
    )
    if (!all(required_access %in% names(access))) {
      stop("access results are missing required columns", call. = FALSE)
    }
    has_subset <- subset_access %in% names(access)
    if (any(has_subset) && !all(has_subset)) {
      stop("access subset results are incomplete", call. = FALSE)
    }
    if (bench_is_full_profile(profile) && !all(has_subset)) {
      stop("full-source access results require subset metrics", call. = FALSE)
    }
    optional_failure <-
      identical(profile$name, "scale") &
      access$backend == "embedded" &
      !is.na(access$status) &
      access$status != "OK"
    checked_access <- access[!optional_failure, , drop = FALSE]
    if (
      any(is.na(checked_access$status)) ||
        any(checked_access$status != "OK")
    ) {
      stop("access process failed", call. = FALSE)
    }
    mismatch <-
      checked_access$correctness != "OK" |
      checked_access$row_fingerprint !=
        checked_access$reference_row_fingerprint |
      checked_access$block_fingerprint !=
        checked_access$reference_block_fingerprint
    if (all(has_subset)) {
      mismatch <- mismatch |
        checked_access$subset_row_fingerprint !=
          checked_access$reference_subset_row_fingerprint |
        checked_access$subset_block_fingerprint !=
          checked_access$reference_subset_block_fingerprint
    }
    if (any(is.na(mismatch) | mismatch)) {
      stop("backend correctness fingerprint mismatch", call. = FALSE)
    }
  }

  successful_keys <- .bench_result_key(successful)
  access_keys <- if (nrow(access)) .bench_result_key(access) else character()
  for (i in seq_len(nrow(successful))) {
    key <- successful_keys[i]
    planned <- schedule[expected == key, , drop = FALSE]
    wanted <- planned$access_repeats[1]
    observed <- sum(access_keys == key)
    if (observed != wanted) {
      stop(
        sprintf(
          "missing access measurement for %s: expected %d, observed %d",
          key,
          wanted,
          observed
        ),
        call. = FALSE
      )
    }
  }
  TRUE
}

bench_require_evidence_profile <- function(profile) {
  if (is.character(profile)) {
    profile <- bench_profile(profile)
  }
  if (!isTRUE(profile$evidence_grade)) {
    stop(
      "the user-facing report requires an evidence-grade benchmark profile",
      call. = FALSE
    )
  }
  TRUE
}
