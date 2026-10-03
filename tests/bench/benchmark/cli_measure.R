# Commands: export, build-full, access, validate.
if (command %in% c("export", "build-full", "access", "validate")) {
  .bench_cli_handled <- TRUE
if (identical(command, "export")) {
  # Export one (source, tier, backend) cell of the grid.
  #
  # Usage: Rscript cli.R export <source> <n_cells> <backend> \
  #   <export_repeat> <order_position> <scratch> <result> <query_plan>
  #
  # Deliberately one process per grid cell rather than one per tier: the embedded
  # backend is expected to be killed by the OS (or to hit the 32-bit dgCMatrix
  # index limit) at the larger tiers, and that must not take the streaming
  # backends of the same tier down with it. Each process writes its own row, so an
  # aborted run keeps everything already measured.

  args <- command_args
  if (length(args) < 8) {
    stop(
      paste(
        "need <source> <n_cells> <backend> <export_repeat>",
        "<order_position> <scratch> <result> <query_plan>"
      )
    )
  }
  src_name <- args[1]
  n_cells <- as.numeric(args[2])
  backend <- args[3]
  export_repeat <- as.integer(args[4])
  order_position <- as.integer(args[5])
  scratch <- args[6]
  result <- args[7]
  query_plan_path <- args[8]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }
  # BENCH_LIB holds the branch under test, installed into the scratch directory so
  # the numbers describe this worktree rather than whatever version happens to be
  # in the user's global library.
  if (nzchar(Sys.getenv("BENCH_LIB"))) {
    .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
  }
  suppressPackageStartupMessages(library(CerebroNexus))

  spec <- BENCH_SOURCES[[src_name]]
  if (is.null(spec)) {
    stop("unknown source: ", src_name)
  }
  cached <- file.path(scratch, "sources", basename(sub("\\?.*$", "", spec$url)))
  if (file.exists(cached)) {
    spec$local_path <- cached
  }

  out_dir <- file.path(
    scratch,
    "export",
    sprintf("%s_%.0f_%s_r%d", src_name, n_cells, backend, export_repeat)
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  crb <- file.path(out_dir, "bench.crb")

  row <- data.frame(
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE", "quick"),
    source = src_name,
    label = spec$label,
    n_cells = n_cells,
    n_genes = NA_real_,
    nnz = NA_real_,
    backend = backend,
    export_repeat = export_repeat,
    order_position = order_position,
    status = "OK",
    read_secs = NA_real_,
    seurat_secs = NA_real_,
    export_secs = NA_real_,
    shell_secs = NA_real_,
    serialize_secs = NA_real_,
    crb_mb = NA_real_,
    sibling_mb = NA_real_,
    total_mb = NA_real_,
    rss_mb = NA_real_,
    peak_rss_mb = NA_real_,
    r_peak_mb = NA_real_,
    query_plan_fingerprint = NA_character_,
    stringsAsFactors = FALSE
  )

  fail <- function(stage, e) {
    row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(e))
    bench_append_row(result, row)
    bench_msg("FAILED at %s: %s", stage, conditionMessage(e))
    quit(status = 0) # the failure IS the measurement; do not fail the sweep
  }

  gc(reset = TRUE)
  bench_msg("%s / %.0f cells / %s: reading", src_name, n_cells, backend)

  m <- NULL
  row$read_secs <- tryCatch(
    bench_time(
      m <- if (identical(Sys.getenv("BENCH_PROFILE"), "scale")) {
        bench_materialize_source_tier(spec, cached, n_cells)
      } else {
        bench_read_subset(spec, n_cells, n_chunks = 4, verbose = TRUE)
      }
    ),
    error = function(e) fail("read", e)
  )
  row$n_genes <- nrow(m)
  row$nnz <- length(m@x)
  bench_msg("read done: %d x %d, nnz %.3e", nrow(m), ncol(m), length(m@x))

  query_plan <- tryCatch(
    readRDS(query_plan_path),
    error = function(e) fail("query plan", e)
  )
  if (query_plan$n_cells != ncol(m) || query_plan$n_genes != nrow(m)) {
    fail("query plan", simpleError("frozen query-plan dimensions changed"))
  }
  row$query_plan_fingerprint <- query_plan$query_plan_fingerprint

  obj <- NULL
  row$seurat_secs <- tryCatch(
    bench_time(obj <- bench_make_seurat(m)),
    error = function(e) fail("seurat", e)
  )
  rm(m)
  gc(verbose = FALSE)

  bench_msg("exporting %s", backend)
  row$export_secs <- tryCatch(
    bench_time(CerebroNexus::exportFromSeurat(
      object = obj,
      assay = "RNA",
      slot = "counts",
      file = crb,
      experiment_name = sprintf("%s_%.0f", src_name, n_cells),
      organism = spec$organism,
      groups = c("sample", "cluster"),
      nUMI = "nUMI",
      nGene = "nGene",
      expression_matrix_mode = backend,
      verbose = FALSE
    )),
    error = function(e) fail("export", e)
  )

  sibling <- switch(
    backend,
    bpcells = sub("\\.crb$", ".bpcells", crb),
    h5 = sub("\\.crb$", ".h5", crb),
    NULL
  )
  row$crb_mb <- bench_path_mb(crb)
  row$sibling_mb <- bench_path_mb(sibling)
  row$total_mb <- sum(c(row$crb_mb, row$sibling_mb), na.rm = TRUE)
  row$rss_mb <- bench_rss_mb()
  row$peak_rss_mb <- bench_peak_rss_mb()
  # gc() alternates (count, Mb) columns and the count named "max used" is NOT the
  # figure wanted; the Mb that follows it is. The column index is not fixed
  # either: an R with a vector memory limit set inserts a "limit (Mb)" column, so
  # the layout is 7 wide here and 6 wide elsewhere. The peak in MB is always the
  # last column.
  g <- gc()
  row$r_peak_mb <- sum(g[, ncol(g)], na.rm = TRUE)

  bench_append_row(result, row)
  bench_msg(
    "OK %s: total %.1f MB in %.1fs (peak R heap %.0f MB)",
    backend,
    row$total_mb,
    row$export_secs,
    row$r_peak_mb
  )
} else if (identical(command, "build-full")) {
  # Build one full-source out-of-core backend and its portable Cerebro shell.

  args <- command_args
  if (length(args) < 8L) {
    stop(
      paste(
        "need <source> <n_cells> <backend> <build_repeat>",
        "<order_position> <scratch> <result> <query_plan>"
      ),
      call. = FALSE
    )
  }
  src_name <- args[1L]
  n_cells <- as.numeric(args[2L])
  backend <- args[3L]
  build_repeat <- as.integer(args[4L])
  order_position <- as.integer(args[5L])
  scratch <- args[6L]
  result <- args[7L]
  query_plan_path <- args[8L]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }
  if (nzchar(Sys.getenv("BENCH_LIB"))) {
    .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
  }
  suppressPackageStartupMessages(library(CerebroNexus))

  spec <- BENCH_SOURCES[[src_name]]
  if (is.null(spec)) {
    stop("unknown source: ", src_name, call. = FALSE)
  }
  source_path <- file.path(
    scratch,
    "sources",
    basename(sub("\\?.*$", "", spec$url))
  )
  out_dir <- file.path(
    scratch,
    "export",
    sprintf("%s_%.0f_%s_r%d", src_name, n_cells, backend, build_repeat)
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  crb <- file.path(out_dir, "bench.crb")
  sibling <- if (backend == "bpcells") {
    file.path(out_dir, "bench.bpcells")
  } else {
    file.path(out_dir, "bench.h5")
  }

  row <- data.frame(
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE", "full"),
    source = src_name,
    label = spec$label,
    n_cells = n_cells,
    n_genes = NA_real_,
    nnz = NA_real_,
    backend = backend,
    export_repeat = build_repeat,
    order_position = order_position,
    status = "OK",
    read_secs = NA_real_,
    seurat_secs = 0,
    export_secs = NA_real_,
    shell_secs = NA_real_,
    serialize_secs = NA_real_,
    crb_mb = NA_real_,
    sibling_mb = NA_real_,
    total_mb = NA_real_,
    rss_mb = NA_real_,
    peak_rss_mb = NA_real_,
    r_peak_mb = NA_real_,
    query_plan_fingerprint = NA_character_,
    stringsAsFactors = FALSE
  )

  fail <- function(stage, error) {
    row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(error))
    bench_append_row(result, row)
    bench_msg("FAILED at %s: %s", stage, conditionMessage(error))
    quit(status = 0L)
  }

  source_matrix <- NULL
  row$read_secs <- tryCatch(
    bench_time(
      source_matrix <- bench_open_source_tier(spec, source_path, n_cells)
    ),
    error = function(error) fail("open", error)
  )
  row$n_cells <- ncol(source_matrix)
  row$n_genes <- nrow(source_matrix)
  if (row$n_cells != n_cells) {
    fail("open", simpleError("scheduled and source cell counts differ"))
  }

  query_plan <- tryCatch(
    readRDS(query_plan_path),
    error = function(error) fail("query plan", error)
  )
  if (
    query_plan$n_cells != ncol(source_matrix) ||
      query_plan$n_genes != nrow(source_matrix)
  ) {
    fail("query plan", simpleError("frozen query-plan dimensions changed"))
  }
  row$nnz <- query_plan$nnz
  row$query_plan_fingerprint <- query_plan$query_plan_fingerprint

  row$export_secs <- tryCatch(
    bench_time(bench_write_full_backend(source_matrix, backend, sibling)),
    error = function(error) fail("build", error)
  )
  obj <- NULL
  row$shell_secs <- tryCatch(
    bench_time(
      obj <- bench_make_full_shell(
        source_matrix,
        backend,
        basename(sibling),
        src_name,
        spec$organism,
        Sys.getenv("BENCH_RUN_ID")
      )
    ),
    error = function(error) fail("shell", error)
  )
  row$serialize_secs <- tryCatch(
    bench_time(saveCerebro(obj, crb)),
    error = function(error) fail("shell", error)
  )

  row$crb_mb <- bench_path_mb(crb)
  row$sibling_mb <- bench_path_mb(sibling)
  row$total_mb <- row$crb_mb + row$sibling_mb
  row$rss_mb <- bench_rss_mb()
  row$peak_rss_mb <- bench_peak_rss_mb()
  heap <- gc()
  row$r_peak_mb <- sum(heap[, ncol(heap)], na.rm = TRUE)
  bench_append_row(result, row)
  bench_msg(
    "OK %s: %.0f x %.0f, %.1f MB in %.1fs",
    backend,
    row$n_genes,
    row$n_cells,
    row$total_mb,
    row$export_secs
  )
} else if (identical(command, "access")) {
  # Measure hydrated startup, memory, and query latency for one exported .crb.
  #
  # Usage: Rscript cli.R access <source> <n_cells> <backend> \
  #   <export_repeat> <order_position> <access_repeat> <crb> <result> <query_plan>
  #
  # Runs in its own process so the resident-set reading describes this backend
  # only. Reads go through getExpressionRow() / getExpressionBlock() and the
  # reads use the public readCerebro() and expression getter paths.

  args <- command_args
  if (length(args) < 9) {
    stop(
      paste(
        "need <source> <n_cells> <backend> <export_repeat> <order_position>",
        "<access_repeat> <crb> <result> <query_plan>"
      )
    )
  }
  src_name <- args[1]
  n_cells <- as.numeric(args[2])
  backend <- args[3]
  export_repeat <- as.integer(args[4])
  order_position <- as.integer(args[5])
  access_repeat <- as.integer(args[6])
  crb <- args[7]
  result <- args[8]
  query_plan_path <- args[9]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }
  if (nzchar(Sys.getenv("BENCH_LIB"))) {
    .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
  }
  suppressPackageStartupMessages({
    library(CerebroNexus)
    library(Matrix)
  })

  row <- data.frame(
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE", "quick"),
    source = src_name,
    n_cells = n_cells,
    backend = backend,
    export_repeat = export_repeat,
    order_position = order_position,
    access_repeat = access_repeat,
    status = "OK",
    startup_secs = NA_real_,
    rss_mb = NA_real_,
    peak_rss_mb = NA_real_,
    first_query_secs = NA_real_,
    hot_p50_secs = NA_real_,
    hot_p95_secs = NA_real_,
    block_secs = NA_real_,
    subset_row_secs = NA_real_,
    subset_block_secs = NA_real_,
    subset_n_cells = NA_real_,
    n_hot = NA_integer_,
    correctness = NA_character_,
    row_fingerprint = NA_character_,
    reference_row_fingerprint = NA_character_,
    block_fingerprint = NA_character_,
    reference_block_fingerprint = NA_character_,
    subset_row_fingerprint = NA_character_,
    reference_subset_row_fingerprint = NA_character_,
    subset_block_fingerprint = NA_character_,
    reference_subset_block_fingerprint = NA_character_,
    query_plan_fingerprint = NA_character_,
    stringsAsFactors = FALSE
  )

  fail <- function(stage, e) {
    row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(e))
    bench_append_row(result, row)
    bench_msg("FAILED at %s: %s", stage, conditionMessage(e))
    quit(status = 0)
  }

  obj <- NULL
  query_plan <- tryCatch(
    readRDS(query_plan_path),
    error = function(e) fail("query plan", e)
  )
  row$startup_secs <- tryCatch(
    bench_time(obj <- readCerebro(crb)),
    error = function(e) fail("startup", e)
  )
  row$rss_mb <- bench_rss_mb()

  metrics <- tryCatch(
    bench_measure_backend(
      obj,
      query_plan,
      hot_iterations = bench_profile(Sys.getenv(
        "BENCH_PROFILE",
        "quick"
      ))$hot_iterations
    ),
    error = function(e) fail("correctness/access", e)
  )
  row$first_query_secs <- metrics$first_query_secs
  row$hot_p50_secs <- metrics$hot_p50_secs
  row$hot_p95_secs <- metrics$hot_p95_secs
  row$block_secs <- metrics$block_secs
  row$subset_row_secs <- metrics$subset_row_secs
  row$subset_block_secs <- metrics$subset_block_secs
  row$subset_n_cells <- metrics$subset_n_cells
  row$n_hot <- metrics$n_hot
  row$correctness <- metrics$correctness
  row$row_fingerprint <- metrics$row_fingerprint
  row$reference_row_fingerprint <- metrics$reference_row_fingerprint
  row$block_fingerprint <- metrics$block_fingerprint
  row$reference_block_fingerprint <- metrics$reference_block_fingerprint
  row$subset_row_fingerprint <- metrics$subset_row_fingerprint
  row$reference_subset_row_fingerprint <-
    metrics$reference_subset_row_fingerprint
  row$subset_block_fingerprint <- metrics$subset_block_fingerprint
  row$reference_subset_block_fingerprint <-
    metrics$reference_subset_block_fingerprint
  row$query_plan_fingerprint <- metrics$query_plan_fingerprint
  row$peak_rss_mb <- bench_peak_rss_mb()

  bench_append_row(result, row)
  bench_msg(
    "%s: hydrated startup %.2fs, rss %.0f MB, hot p50 %.4fs",
    backend,
    row$startup_secs,
    row$rss_mb,
    row$hot_p50_secs
  )
} else if (identical(command, "validate")) {
  # Validate a staged benchmark result set.

  args <- command_args
  if (length(args) < 1L) {
    stop("need <result_dir>", call. = FALSE)
  }
  result_dir <- args[1]
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }

  read_required <- function(name) {
    path <- file.path(result_dir, name)
    if (!file.exists(path)) {
      stop("missing staged result file: ", name, call. = FALSE)
    }
    utils::read.csv(path, stringsAsFactors = FALSE)
  }

  schedule <- read_required("05_schedule.csv")
  exports <- read_required("10_export.csv")
  access <- read_required("20_access.csv")
  crashes <- read_required("crashes.csv")
  manifest <- read_required("run_manifest.csv")
  source_manifest <- read_required("source_manifest.csv")
  resource_check <- read_required("resource_check.csv")
  preparation <- read_required("query_plan_manifest.csv")
  query_panel <- read_required("query_panel.csv")
  profile <- bench_profile(Sys.getenv("BENCH_PROFILE", "quick"))

  tier_key <- function(source, n_cells) {
    paste(source, sprintf("%.0f", as.numeric(n_cells)), sep = "\r")
  }

  resource_keys <- tier_key(resource_check$source, resource_check$n_cells)
  schedule_keys <- unique(tier_key(schedule$source, schedule$n_cells))
  if (!setequal(resource_keys, schedule_keys) || anyDuplicated(resource_keys)) {
    stop("resource_check.csv does not cover the scheduled tiers", call. = FALSE)
  }
  bench_require_safe_plan(resource_check)

  if (
    !identical(names(manifest), c("key", "value")) ||
      anyDuplicated(manifest$key)
  ) {
    stop("run manifest must contain unique key/value rows", call. = FALSE)
  }
  manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
  # Runs created before the manifest-name fix inherited the DCF column name and
  # wrote `package_version.Version`. Accept that exact legacy spelling so a fully
  # completed acquisition can be finalized without repeating timed measurements.
  if (
    !"package_version" %in% names(manifest_values) &&
      "package_version.Version" %in% names(manifest_values)
  ) {
    manifest_values[["package_version"]] <-
      manifest_values[["package_version.Version"]]
  }
  required_manifest <- c(
    "study_id",
    "run_id",
    "profile",
    "generated_at",
    "git_sha",
    "git_branch",
    "git_dirty",
    "package_version",
    "r_version",
    "r_platform",
    "os",
    "cpu",
    "logical_cores",
    "benchmark_threads",
    "scratch_df",
    "storage_description",
    "memory_mb",
    "r_vector_limit_mb",
    "package_Matrix",
    "package_rhdf5",
    "package_BPCells",
    "package_HDF5Array",
    "package_CerebroNexus"
  )
  if (!all(required_manifest %in% names(manifest_values))) {
    stop("run manifest is missing required provenance", call. = FALSE)
  }
  required_values <- manifest_values[required_manifest]
  if (any(is.na(required_values) | !nzchar(trimws(required_values)))) {
    stop(
      "run manifest contains blank required provenance: ",
      paste(
        names(required_values)[
          is.na(required_values) | !nzchar(trimws(required_values))
        ],
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  if (!identical(manifest_values[["profile"]], profile$name)) {
    stop(
      "run manifest profile does not match validation profile",
      call. = FALSE
    )
  }
  if (!grepl("^[0-9a-f]{40}$", manifest_values[["git_sha"]])) {
    stop("run manifest has an invalid Git SHA", call. = FALSE)
  }
  if (
    isTRUE(profile$evidence_grade) &&
      !identical(manifest_values[["git_dirty"]], "false")
  ) {
    stop(
      "benchmark evidence requires a clean Git worktree",
      call. = FALSE
    )
  }

  required_source_columns <- c("run_id", "source", "url", "bytes", "sha256")
  if (!all(required_source_columns %in% names(source_manifest))) {
    stop("source manifest is missing required columns", call. = FALSE)
  }
  if (!setequal(unique(schedule$source), unique(source_manifest$source))) {
    stop("source manifest does not cover the scheduled sources", call. = FALSE)
  }
  if (
    any(!is.finite(source_manifest$bytes) | source_manifest$bytes <= 0) ||
      any(!grepl("^[0-9a-fA-F]{64}$", source_manifest$sha256))
  ) {
    stop("source SHA-256 or byte size is invalid", call. = FALSE)
  }

  run_id <- manifest_values[["run_id"]]
  if (
    any(exports$run_id != run_id) ||
      any(access$run_id != run_id) ||
      any(source_manifest$run_id != run_id) ||
      any(preparation$run_id != run_id)
  ) {
    stop("result rows do not share the manifest run id", call. = FALSE)
  }

  plan_keys <- tier_key(schedule$source, schedule$n_cells)
  preparation_keys <- tier_key(preparation$source, preparation$n_cells)
  if (
    !setequal(unique(plan_keys), preparation_keys) ||
      anyDuplicated(preparation_keys) ||
      any(preparation$status != "OK") ||
      any(preparation$profile != profile$name)
  ) {
    stop("query-plan preparation does not cover the schedule", call. = FALSE)
  }
  required_panel_columns <- c(
    "run_id",
    "profile",
    "source",
    "n_cells",
    "panel_index",
    "gene",
    "nnz",
    "role",
    "query_plan_fingerprint",
    "reference_row_fingerprint",
    "reference_block_fingerprint"
  )
  subset_panel_columns <- c(
    "subset_n_cells",
    "subset_cells_fingerprint",
    "reference_subset_row_fingerprint",
    "reference_subset_block_fingerprint"
  )
  if (bench_is_full_profile(profile)) {
    required_panel_columns <- c(required_panel_columns, subset_panel_columns)
  }
  if (!all(required_panel_columns %in% names(query_panel))) {
    stop("query panel is missing required columns", call. = FALSE)
  }
  panel_keys <- tier_key(query_panel$source, query_panel$n_cells)
  panel_groups <- split(query_panel, panel_keys)
  validate_subset <- all(subset_panel_columns %in% names(query_panel))
  if (
    any(query_panel$run_id != run_id) ||
      any(query_panel$profile != profile$name) ||
      !setequal(names(panel_groups), unique(plan_keys)) ||
      any(vapply(panel_groups, nrow, integer(1)) != profile$query_genes) ||
      any(vapply(
        panel_groups,
        function(rows) {
          !identical(rows$panel_index, seq_len(nrow(rows))) ||
            anyDuplicated(rows$gene) ||
            any(is.na(rows$gene) | !nzchar(rows$gene)) ||
            sum(rows$role == "first") != 1L ||
            any(!rows$role %in% c("first", "hot")) ||
            any(!is.finite(rows$nnz) | rows$nnz <= 0) ||
            any(
              is.na(rows$query_plan_fingerprint) |
                !nzchar(rows$query_plan_fingerprint)
            ) ||
            any(
              is.na(rows$reference_row_fingerprint) |
                !nzchar(rows$reference_row_fingerprint)
            ) ||
            any(
              is.na(rows$reference_block_fingerprint) |
                !nzchar(rows$reference_block_fingerprint)
            ) ||
            length(unique(rows$query_plan_fingerprint)) != 1L ||
            length(unique(rows$reference_row_fingerprint)) != 1L ||
            length(unique(rows$reference_block_fingerprint)) != 1L ||
            (validate_subset &&
              (length(unique(rows$subset_n_cells)) != 1L ||
                any(
                  !is.finite(rows$subset_n_cells) | rows$subset_n_cells < 1L
                ) ||
                length(unique(rows$subset_cells_fingerprint)) != 1L ||
                length(unique(rows$reference_subset_row_fingerprint)) != 1L ||
                length(unique(rows$reference_subset_block_fingerprint)) != 1L))
        },
        logical(1)
      ))
  ) {
    stop("query panel does not match the fixed study protocol", call. = FALSE)
  }
  fingerprint_for <- function(data) {
    keys <- tier_key(data$source, data$n_cells)
    observed <- split(as.character(data$query_plan_fingerprint), keys)
    if (any(lengths(lapply(observed, unique)) != 1L)) {
      stop("query-plan fingerprint drifted within a tier", call. = FALSE)
    }
    sort(vapply(observed, function(x) unique(x)[1L], character(1)))
  }
  prepared_fingerprints <- fingerprint_for(preparation)
  successful_exports <- exports[
    !is.na(exports$status) & exports$status == "OK",
    ,
    drop = FALSE
  ]
  successful_access <- access[
    !is.na(access$status) & access$status == "OK",
    ,
    drop = FALSE
  ]
  fingerprints <- list(
    preparation = prepared_fingerprints,
    query_panel = fingerprint_for(query_panel),
    build = fingerprint_for(successful_exports),
    access = fingerprint_for(successful_access)
  )
  fingerprint_keys <- Reduce(union, lapply(fingerprints, names))
  fingerprint_matrix <- do.call(
    cbind,
    lapply(
      fingerprints,
      function(x) unname(x[match(fingerprint_keys, names(x))])
    )
  )
  fingerprint_mismatches <- fingerprint_keys[apply(
    fingerprint_matrix,
    1L,
    function(values) anyNA(values) || length(unique(values)) != 1L
  )]
  if (length(fingerprint_mismatches)) {
    stop(
      "query-plan fingerprint differs across preparation/build/access: ",
      paste(
        gsub("\r", " @ ", fingerprint_mismatches, fixed = TRUE),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  bench_validate_results(schedule, exports, access, crashes, profile)
  message(
    sprintf(
      "validated %d exports and %d access processes for profile %s",
      nrow(exports),
      nrow(access),
      profile$name
    )
  )
}
}
