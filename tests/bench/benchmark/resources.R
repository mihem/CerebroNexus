# ---- resource_planning.R ----
# Pure host-resource planning helpers for the real-data benchmark.

bench_free_disk_bytes <- function(
  path,
  override = Sys.getenv("BENCH_FREE_DISK_BYTES", unset = "")
) {
  override <- suppressWarnings(as.numeric(override))
  if (is.finite(override) && override > 0) {
    return(override)
  }
  lines <- tryCatch(
    system2("df", c("-Pk", shQuote(path)), stdout = TRUE, stderr = TRUE),
    error = function(error) character()
  )
  fields <- if (length(lines)) {
    strsplit(trimws(tail(lines, 1L)), "[[:space:]]+")[[1L]]
  } else {
    character()
  }
  available_kb <- suppressWarnings(as.numeric(fields[4L]))
  if (!is.finite(available_kb)) {
    stop("could not determine free disk space", call. = FALSE)
  }
  available_kb * 1024
}

bench_assess_resources <- function(
  inventory,
  plan,
  memory_mb,
  vector_limit_mb,
  free_disk_bytes,
  bytes_per_nnz = 64,
  fixed_memory_mb = 1024,
  memory_fraction = 0.70,
  disk_fraction = 0.80
) {
  required_inventory <- c("source", "nnz_per_cell", "source_bytes")
  required_plan <- c("source", "n_cells")
  if (!all(required_inventory %in% names(inventory))) {
    stop("inventory is missing resource columns", call. = FALSE)
  }
  if (!all(required_plan %in% names(plan))) {
    stop("run plan is missing source or cell count", call. = FALSE)
  }
  if (anyDuplicated(inventory$source)) {
    stop("inventory sources must be unique", call. = FALSE)
  }

  planned <- unique(plan[required_plan])
  matched <- match(planned$source, inventory$source)
  if (anyNA(matched)) {
    stop(
      "resource inventory does not cover source: ",
      planned$source[is.na(matched)][1],
      call. = FALSE
    )
  }
  source <- inventory[matched, , drop = FALSE]
  limit_mb <- min(memory_mb, vector_limit_mb, na.rm = TRUE)
  memory_budget_mb <- limit_mb * memory_fraction
  disk_budget_bytes <- free_disk_bytes * disk_fraction
  estimated_nnz <- planned$n_cells * source$nnz_per_cell
  estimated_peak_mb <- fixed_memory_mb + estimated_nnz * bytes_per_nnz / 2^20
  memory_ok <- is.finite(estimated_peak_mb) &
    estimated_peak_mb <= memory_budget_mb
  index_ok <- is.finite(estimated_nnz) &
    estimated_nnz <= .Machine$integer.max
  disk_ok <- is.finite(source$source_bytes) &
    source$source_bytes <= disk_budget_bytes

  reason <- vapply(
    seq_len(nrow(planned)),
    function(i) {
      failures <- character()
      if (!memory_ok[i]) {
        failures <- c(failures, "estimated memory exceeds safe budget")
      }
      if (!index_ok[i]) {
        failures <- c(failures, "estimated nnz exceeds 32-bit sparse index")
      }
      if (!disk_ok[i]) {
        failures <- c(failures, "source download exceeds safe disk budget")
      }
      if (length(failures)) paste(failures, collapse = "; ") else "safe"
    },
    character(1)
  )

  data.frame(
    source = planned$source,
    n_cells = planned$n_cells,
    estimated_nnz = estimated_nnz,
    estimated_peak_mb = round(estimated_peak_mb),
    memory_budget_mb = round(memory_budget_mb),
    source_bytes = source$source_bytes,
    disk_budget_bytes = disk_budget_bytes,
    memory_ok = memory_ok,
    index_ok = index_ok,
    disk_ok = disk_ok,
    safe = memory_ok & index_ok & disk_ok,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

bench_require_safe_plan <- function(
  assessment,
  allow_unsafe = identical(Sys.getenv("BENCH_ALLOW_UNSAFE"), "1")
) {
  unsafe <- assessment[!assessment$safe, , drop = FALSE]
  if (!nrow(unsafe) || isTRUE(allow_unsafe)) {
    return(TRUE)
  }
  details <- paste0(
    unsafe$source,
    " @ ",
    format(unsafe$n_cells, big.mark = ",", scientific = FALSE),
    " cells: ",
    unsafe$reason
  )
  stop(
    "unsafe benchmark plan for this machine:\n- ",
    paste(details, collapse = "\n- "),
    "\nChoose a smaller profile/tier, or set BENCH_ALLOW_UNSAFE=1 ",
    "only for an intentional stress run.",
    call. = FALSE
  )
}
