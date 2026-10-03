# Commands: inspect, environment, plan, full-resources, resources, query-plan.
if (command %in% c("inspect", "environment", "plan", "full-resources", "resources", "query-plan")) {
  .bench_cli_handled <- TRUE
if (identical(command, "inspect")) {
  # Inspect every configured source over ROS3 and record what it contains.
  #
  # Usage: Rscript cli.R inspect <result>
  #
  # Costs a few HTTP range requests per source and no bulk transfer at all, so it
  # is safe to run before committing to a sweep. The `dgc_representable` column is
  # the interesting one: a source whose non-zero count exceeds 2^31 - 1 cannot be
  # held in a dgCMatrix at full size no matter how much RAM the host has.

  args <- command_args
  if (!length(args)) {
    stop("need <result.csv>", call. = FALSE)
  }
  result <- args[1]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }

  for (nm in bench_active_sources()) {
    spec <- BENCH_SOURCES[[nm]]
    bench_msg("probing %s", nm)
    p <- tryCatch(bench_probe(spec), error = function(e) {
      bench_msg("  FAILED: %s", conditionMessage(e))
      NULL
    })
    if (is.null(p)) {
      next
    }
    bench_append_row(
      result,
      data.frame(
        source = nm,
        label = p$label,
        kind = p$kind,
        n_cells = p$n_cells,
        n_genes = p$n_genes,
        nnz = p$nnz,
        nnz_per_cell = round(p$nnz_per_cell, 1),
        source_bytes = spec$expected_bytes,
        dgc_gb_full = round(p$dgc_gb_full, 2),
        dgc_representable = p$dgc_representable,
        tiers = paste(
          format(spec$tiers, scientific = FALSE, trim = TRUE),
          collapse = "|"
        ),
        stringsAsFactors = FALSE
      )
    )
    bench_msg(
      "  %s cells x %s genes, nnz %.4e (%.0f/cell), full dgCMatrix %.1f GB%s",
      format(p$n_cells, big.mark = ","),
      format(p$n_genes, big.mark = ","),
      p$nnz,
      p$nnz_per_cell,
      p$dgc_gb_full,
      if (p$dgc_representable) "" else " -- EXCEEDS the 32-bit index limit"
    )
  }
} else if (identical(command, "environment")) {
  # Write benchmark run provenance.

  args <- command_args
  if (length(args) < 1L) {
    stop("need <result.csv>", call. = FALSE)
  }
  result <- args[1]
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }
  repo <- normalizePath(file.path(here, "..", ".."))

  if (nzchar(Sys.getenv("BENCH_LIB"))) {
    .libPaths(c(Sys.getenv("BENCH_LIB"), .libPaths()))
  }

  capture_command <- function(command, args = character()) {
    out <- suppressWarnings(system2(
      command,
      vapply(args, shQuote, character(1)),
      stdout = TRUE,
      stderr = FALSE
    ))
    paste(trimws(out), collapse = " ")
  }

  git_value <- function(...) {
    capture_command("git", c("-C", repo, ...))
  }

  cpu_name <- function() {
    if (identical(Sys.info()[["sysname"]], "Darwin")) {
      value <- capture_command("sysctl", c("-n", "machdep.cpu.brand_string"))
      if (nzchar(value)) return(value)
    }
    if (file.exists("/proc/cpuinfo")) {
      lines <- readLines("/proc/cpuinfo", warn = FALSE)
      model <- sub(
        "^[^:]+:[[:space:]]*",
        "",
        grep("^model name", lines, value = TRUE)
      )
      if (length(model)) return(model[1])
    }
    Sys.info()[["machine"]]
  }

  memory_mb <- function() {
    if (identical(Sys.info()[["sysname"]], "Darwin")) {
      bytes <- suppressWarnings(as.numeric(capture_command(
        "sysctl",
        c("-n", "hw.memsize")
      )))
      if (is.finite(bytes)) return(bytes / 2^20)
    }
    if (file.exists("/proc/meminfo")) {
      line <- grep(
        "^MemTotal:",
        readLines("/proc/meminfo", warn = FALSE),
        value = TRUE
      )
      kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", line[1])))
      if (is.finite(kb)) return(kb / 1024)
    }
    NA_real_
  }

  package_version_or_na <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      return(NA_character_)
    }
    as.character(utils::packageVersion(package))
  }

  description <- read.dcf(file.path(repo, "DESCRIPTION"))
  tracked_status <- git_value(
    "status",
    "--porcelain",
    "--untracked-files=no",
    "--",
    ".",
    ":(exclude,glob)tests/bench/results/**"
  )
  untracked_status <- git_value(
    "ls-files",
    "--others",
    "--exclude-standard",
    "--",
    ".",
    ":(exclude,glob)tests/bench/results/**"
  )
  status <- paste0(tracked_status, untracked_status)
  scratch <- Sys.getenv("BENCH_SCRATCH")
  scratch_df <- if (nzchar(scratch) && dir.exists(scratch)) {
    capture_command("df", c("-P", scratch))
  } else {
    ""
  }
  manifest <- c(
    study_id = Sys.getenv("BENCH_STUDY_ID"),
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE", "quick"),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    git_sha = git_value("rev-parse", "HEAD"),
    git_branch = git_value("branch", "--show-current"),
    git_dirty = if (nzchar(status)) "true" else "false",
    package_version = unname(description[1, "Version"]),
    r_version = R.version.string,
    r_platform = R.version$platform,
    os = paste(Sys.info()[c("sysname", "release", "version")], collapse = " "),
    cpu = cpu_name(),
    logical_cores = as.character(parallel::detectCores(logical = TRUE)),
    benchmark_threads = Sys.getenv("BENCH_THREADS", "1"),
    slurm_job_id = Sys.getenv("SLURM_JOB_ID"),
    slurm_node_list = Sys.getenv("SLURM_NODELIST"),
    slurm_cpus_per_task = Sys.getenv("SLURM_CPUS_PER_TASK"),
    slurm_memory_per_node = Sys.getenv("SLURM_MEM_PER_NODE"),
    scratch_df = scratch_df,
    storage_description = Sys.getenv("BENCH_STORAGE_DESCRIPTION"),
    memory_mb = format(memory_mb(), scientific = FALSE, trim = TRUE),
    r_vector_limit_mb = format(mem.maxVSize(), scientific = FALSE, trim = TRUE)
  )
  packages <- c(
    "Matrix",
    "rhdf5",
    "Seurat",
    "SeuratObject",
    "BPCells",
    "HDF5Array",
    "CerebroNexus"
  )
  package_versions <- vapply(packages, package_version_or_na, character(1))
  # Provenance is recorded before the branch-under-test is installed into its
  # isolated library. Its authoritative version is therefore DESCRIPTION, not a
  # possibly absent or stale package on the caller's library path.
  package_versions[["CerebroNexus"]] <- unname(description[1, "Version"])
  manifest <- c(
    manifest,
    stats::setNames(
      package_versions,
      paste0("package_", packages)
    )
  )

  dir.create(dirname(result), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(
      key = names(manifest),
      value = unname(manifest),
      stringsAsFactors = FALSE
    ),
    result,
    row.names = FALSE,
    na = ""
  )
  message("wrote run provenance to ", result)
} else if (identical(command, "plan")) {
  # Write the deterministic benchmark schedule.

  args <- command_args
  if (length(args) < 1L) {
    stop("need <result.csv>", call. = FALSE)
  }
  result <- args[1]
  tsv_result <- if (length(args) >= 2L) args[2] else NULL
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }

  profile <- bench_profile(Sys.getenv("BENCH_PROFILE", "quick"))
  schedule <- if (identical(profile$name, "scale")) {
    bench_scale_schedule(BENCH_SOURCES)
  } else if (profile$name %in% c("preview", "full", "panel_c1", "panel_c2")) {
    fixed_profile <- if (profile$name %in% c("preview", "panel_c1")) {
      "preview"
    } else {
      "full"
    }
    bench_fixed_schedule(BENCH_SOURCES, fixed_profile, profile$name)
  } else {
    bench_schedule(
      BENCH_SOURCES,
      profile,
      sources = bench_active_sources()
    )
  }
  dir.create(dirname(result), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(schedule, result, row.names = FALSE)
  if (!is.null(tsv_result)) {
    tsv_schedule <- schedule
    tsv_schedule$n_cells <- sprintf("%.0f", tsv_schedule$n_cells)
    utils::write.table(
      tsv_schedule,
      tsv_result,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
  }
  message(
    sprintf(
      "wrote %d scheduled export cells for the %s profile to %s",
      nrow(schedule),
      profile$name,
      result
    )
  )
} else if (identical(command, "full-resources")) {
  # Preflight a scale or full-source out-of-core schedule.

  args <- command_args
  if (length(args) < 4L) {
    stop(
      "need <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>",
      call. = FALSE
    )
  }
  inventory <- utils::read.csv(args[1L], stringsAsFactors = FALSE)
  plan <- utils::read.csv(args[2L], stringsAsFactors = FALSE)
  manifest <- utils::read.csv(args[3L], stringsAsFactors = FALSE)
  output <- args[4L]
  values <- stats::setNames(as.character(manifest$value), manifest$key)

  planned <- unique(plan[c("source", "n_cells")])
  matched <- match(planned$source, inventory$source)
  if (anyNA(matched)) {
    stop("inventory does not cover the full-source plan", call. = FALSE)
  }
  source <- inventory[matched, , drop = FALSE]
  memory_mb <- min(
    as.numeric(values[["memory_mb"]]),
    as.numeric(values[["r_vector_limit_mb"]]),
    na.rm = TRUE
  )
  disk_free <- bench_free_disk_bytes(dirname(output))
  # Only 12 queried rows are materialised. Four GiB covers R, native buffers and
  # the full-cell metadata shell with a conservative margin.
  estimated_peak_mb <- 4096 + planned$n_cells * 12 * 8 / 2^20
  # One cached source and one staged backend coexist; 2.5x source bytes leaves a
  # compression-independent margin without pretending both backends coexist.
  required_disk <- source$source_bytes * 2.5
  assessment <- data.frame(
    source = planned$source,
    n_cells = planned$n_cells,
    estimated_nnz = source$nnz * planned$n_cells / source$n_cells,
    estimated_peak_mb = round(estimated_peak_mb),
    memory_budget_mb = round(memory_mb * 0.70),
    source_bytes = source$source_bytes,
    disk_budget_bytes = disk_free * 0.80,
    memory_ok = estimated_peak_mb <= memory_mb * 0.70,
    index_ok = TRUE,
    disk_ok = required_disk <= disk_free * 0.80,
    stringsAsFactors = FALSE
  )
  assessment$safe <- with(assessment, memory_ok & index_ok & disk_ok)
  assessment$reason <- ifelse(
    assessment$safe,
    "safe out-of-core plan",
    paste0(
      ifelse(assessment$memory_ok, "", "insufficient memory; "),
      ifelse(assessment$disk_ok, "", "insufficient disk")
    )
  )
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(assessment, output, row.names = FALSE)
  if (any(!assessment$safe)) {
    stop("unsafe full-source out-of-core plan", call. = FALSE)
  }
  message("full-source out-of-core resource plan is safe")
} else if (identical(command, "resources")) {
  # Refuse a benchmark plan that does not fit the current host.
  #
  # Usage: Rscript cli.R resources \
  #   <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>

  args <- command_args
  if (length(args) < 4L) {
    stop(
      "need <data_inventory.csv> <run_plan.csv> <run_manifest.csv> <output.csv>",
      call. = FALSE
    )
  }
  inventory_path <- args[1]
  plan_path <- args[2]
  manifest_path <- args[3]
  output_path <- args[4]
  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }

  inventory <- utils::read.csv(inventory_path, stringsAsFactors = FALSE)
  plan <- utils::read.csv(plan_path, stringsAsFactors = FALSE)
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  manifest_values <- stats::setNames(as.character(manifest$value), manifest$key)
  memory_mb <- suppressWarnings(as.numeric(manifest_values[["memory_mb"]]))
  vector_limit_mb <- suppressWarnings(
    as.numeric(manifest_values[["r_vector_limit_mb"]])
  )
  if (
    !is.finite(memory_mb) ||
      memory_mb <= 0 ||
      is.na(vector_limit_mb) ||
      vector_limit_mb <= 0
  ) {
    stop("run manifest has no usable memory limits", call. = FALSE)
  }

  assessment <- bench_assess_resources(
    inventory,
    plan,
    memory_mb = memory_mb,
    vector_limit_mb = vector_limit_mb,
    free_disk_bytes = bench_free_disk_bytes(dirname(output_path))
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(assessment, output_path, row.names = FALSE)

  for (i in seq_len(nrow(assessment))) {
    message(sprintf(
      "%s @ %s cells: %s (estimated %.0f MB; budget %.0f MB)",
      assessment$source[i],
      format(assessment$n_cells[i], big.mark = ",", scientific = FALSE),
      toupper(if (assessment$safe[i]) "safe" else "unsafe"),
      assessment$estimated_peak_mb[i],
      assessment$memory_budget_mb[i]
    ))
  }
  bench_require_safe_plan(assessment)
} else if (identical(command, "query-plan")) {
  # Prepare one immutable query plan before any timed backend build.

  args <- command_args
  if (length(args) < 6L) {
    stop(
      paste(
        "need <source> <n_cells> <scratch> <query_plan>",
        "<result> <query_panel_result>"
      ),
      call. = FALSE
    )
  }
  src_name <- args[1L]
  n_cells <- as.numeric(args[2L])
  scratch <- args[3L]
  query_plan_path <- args[4L]
  result <- args[5L]
  query_panel_result <- args[6L]

  here <- Sys.getenv("BENCH_ROOT", "")
  if (!nzchar(here)) {
    here <- normalizePath("tests/bench")
  }

  spec <- BENCH_SOURCES[[src_name]]
  if (is.null(spec)) {
    stop("unknown source: ", src_name, call. = FALSE)
  }
  source_path <- file.path(
    scratch,
    "sources",
    basename(sub("\\?.*$", "", spec$url))
  )
  if (!file.exists(source_path)) {
    stop("cached benchmark source is missing", call. = FALSE)
  }

  row <- data.frame(
    run_id = Sys.getenv("BENCH_RUN_ID"),
    profile = Sys.getenv("BENCH_PROFILE"),
    source = src_name,
    n_cells = n_cells,
    n_genes = NA_real_,
    nnz = NA_real_,
    source_prepare_secs = NA_real_,
    query_plan_secs = NA_real_,
    peak_rss_mb = NA_real_,
    subset_n_cells = NA_real_,
    subset_cells_fingerprint = NA_character_,
    query_plan_fingerprint = NA_character_,
    status = "OK",
    stringsAsFactors = FALSE
  )

  fail <- function(stage, error) {
    row$status <- sprintf("FAILED(%s): %s", stage, conditionMessage(error))
    bench_append_row(result, row)
    bench_msg(
      "query-plan preparation failed at %s: %s",
      stage,
      conditionMessage(error)
    )
    quit(status = 1L)
  }

  source_matrix <- NULL
  profile <- Sys.getenv("BENCH_PROFILE")
  row$source_prepare_secs <- tryCatch(
    bench_time({
      source_matrix <- if (profile %in% c("scale", "full", "panel_c2")) {
        bench_open_source_tier(spec, source_path, n_cells)
      } else {
        spec$local_path <- source_path
        bench_read_subset(spec, n_cells, n_chunks = 4L, verbose = FALSE)
      }
    }),
    error = function(error) fail("source", error)
  )
  if (ncol(source_matrix) != n_cells) {
    fail(
      "source",
      simpleError("prepared source cell count differs from schedule")
    )
  }
  row$n_genes <- nrow(source_matrix)
  plan <- NULL
  row$query_plan_secs <- tryCatch(
    bench_time({
      plan <- if (profile %in% c("scale", "full", "panel_c2")) {
        bench_build_lazy_query_plan(
          source_matrix,
          bench_profile(profile)$query_genes
        )
      } else {
        bench_build_query_plan(
          source_matrix,
          bench_profile(profile)$query_genes
        )
      }
    }),
    error = function(error) fail("query plan", error)
  )
  row$nnz <- plan$nnz
  row$query_plan_fingerprint <- plan$query_plan_fingerprint
  row$subset_n_cells <- length(plan$subset_cells)
  row$subset_cells_fingerprint <- plan$subset_cells_fingerprint
  row$peak_rss_mb <- bench_peak_rss_mb()

  dir.create(dirname(query_plan_path), recursive = TRUE, showWarnings = FALSE)
  staged <- tempfile("query-plan-", tmpdir = dirname(query_plan_path))
  tryCatch(
    saveRDS(plan, staged, version = 3),
    error = function(error) fail("write", error)
  )
  if (!file.rename(staged, query_plan_path)) {
    unlink(staged)
    fail("write", simpleError("could not publish frozen query plan"))
  }
  panel_rows <- data.frame(
    run_id = row$run_id,
    profile = row$profile,
    source = row$source,
    n_cells = row$n_cells,
    panel_index = seq_len(nrow(plan$panel)),
    gene = plan$panel$gene,
    nnz = plan$panel$nnz,
    role = plan$panel$role,
    query_plan_fingerprint = plan$query_plan_fingerprint,
    reference_row_fingerprint = plan$reference_row_fingerprint,
    reference_block_fingerprint = plan$reference_block_fingerprint,
    subset_n_cells = length(plan$subset_cells),
    subset_cells_fingerprint = plan$subset_cells_fingerprint,
    reference_subset_row_fingerprint = plan$reference_subset_row_fingerprint,
    reference_subset_block_fingerprint = plan$reference_subset_block_fingerprint,
    stringsAsFactors = FALSE
  )
  bench_append_row(query_panel_result, panel_rows)
  bench_append_row(result, row)
  bench_msg(
    "prepared %s / %.0f cells query plan in %.1fs",
    src_name,
    n_cells,
    row$source_prepare_secs + row$query_plan_secs
  )
}
}
