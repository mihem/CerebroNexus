validate_page_profile <- function(profile, rounds) {
  if (is.na(rounds) || rounds < 1L) {
    stop("ROUNDS must be a positive integer.", call. = FALSE)
  }
  if (identical(profile, "publication") && rounds < 5L) {
    stop("publication requires at least 5 rounds", call. = FALSE)
  }
  invisible(TRUE)
}

page_budget_pass <- function(elapsed_ms, budget_ms) {
  is.finite(elapsed_ms) && elapsed_ms < budget_ms
}

requires_ready_event <- function(page_name, visit, warmed) {
  TRUE
}

validate_benchmark_mode <- function(mode) {
  if (!mode %in% c("timing", "memory")) {
    stop("VIEWER_BENCH_MODE must be timing or memory.", call. = FALSE)
  }
  invisible(mode)
}

escape_tsv_controls <- function(value) {
  value <- gsub("\r", "\\r", value, fixed = TRUE)
  value <- gsub("\n", "\\n", value, fixed = TRUE)
  gsub("\t", "\\t", value, fixed = TRUE)
}

tsv_field_count <- function(line) {
  length(regmatches(line, gregexpr("\t", line, fixed = TRUE))[[1L]]) + 1L
}

write_validated_tsv <- function(data, file) {
  character_columns <- vapply(data, is.character, logical(1))
  data[character_columns] <- lapply(
    data[character_columns],
    escape_tsv_controls
  )
  utils::write.table(data, file, row.names = FALSE, sep = "\t", quote = FALSE)
  lines <- readLines(file, warn = FALSE)
  valid <- length(lines) == nrow(data) + 1L &&
    all(vapply(lines, tsv_field_count, integer(1)) == ncol(data))
  if (!valid) {
    stop("TSV output has an invalid field count: ", file, call. = FALSE)
  }
  invisible(file)
}

sha256_file <- function(path) {
  if (exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)) {
    return(unname(tools::sha256sum(path)))
  }
  command <- Sys.which("shasum")
  if (!nzchar(command)) {
    stop("No SHA-256 implementation is available.")
  }
  sub(" .*", "", system2(command, c("-a", "256", path), stdout = TRUE)[[1L]])
}

sha256_directory <- function(path) {
  files <- sort(list.files(path, recursive = TRUE, full.names = TRUE))
  files <- files[file.info(files)$isdir %in% FALSE]
  relative <- substring(files, nchar(normalizePath(path)) + 2L)
  inventory <- paste(
    relative,
    vapply(files, sha256_file, character(1)),
    sep = "\t"
  )
  inventory_file <- tempfile("benchmark-directory-inventory-")
  on.exit(unlink(inventory_file), add = TRUE)
  writeLines(inventory, inventory_file, useBytes = TRUE)
  sha256_file(inventory_file)
}

read_benchmark_crb_payload <- function(file) {
  connection <- file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  magic <- readBin(connection, "raw", n = 4L)
  if (identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))) {
    return(qs2::qs_read(file))
  }
  readRDS(file)
}

benchmark_trajectory_contract <- function(crb, method, name) {
  if (
    !is.character(method) ||
      length(method) != 1L ||
      is.na(method) ||
      !nzchar(method) ||
      !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name)
  ) {
    stop("Trajectory method and name must be non-empty strings.", call. = FALSE)
  }
  object <- read_benchmark_crb_payload(crb)
  trajectory <- tryCatch(
    object$getTrajectory(method, name),
    error = function(error) {
      stop(
        "Benchmark trajectory `",
        method,
        " / ",
        name,
        "` is unavailable: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  metadata <- trajectory[["meta"]]
  if (
    !is.data.frame(metadata) ||
      !all(c("DR_1", "DR_2", "pseudotime") %in% colnames(metadata))
  ) {
    stop("Benchmark trajectory metadata is incomplete.", call. = FALSE)
  }
  renderable <- !is.na(metadata[["pseudotime"]])
  data.frame(
    method = method,
    name = name,
    metadata_rows = nrow(metadata),
    renderable_rows = sum(renderable),
    excluded_pseudotime_rows = sum(!renderable),
    stringsAsFactors = FALSE
  )
}

benchmark_spatial_contract <- function(crb) {
  object <- read_benchmark_crb_payload(crb)
  spatial_names <- tryCatch(
    object$availableSpatial(),
    error = function(error) character()
  )
  spatial_names <- as.character(spatial_names)
  spatial_names <- spatial_names[!is.na(spatial_names) & nzchar(spatial_names)]
  if (!length(spatial_names)) {
    return(NULL)
  }

  name <- spatial_names[[1L]]
  spatial <- tryCatch(
    object$getSpatialData(name, hydrate_molecules = FALSE),
    error = function(error) {
      stop(
        "Benchmark Spatial entry `",
        name,
        "` is unavailable: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  coordinates <- spatial[["coordinates"]]
  valid <- is.data.frame(coordinates) &&
    all(c("x", "y") %in% colnames(coordinates)) &&
    is.numeric(coordinates[["x"]]) &&
    is.numeric(coordinates[["y"]]) &&
    nrow(coordinates) > 0L &&
    all(is.finite(coordinates[["x"]])) &&
    all(is.finite(coordinates[["y"]]))
  if (!valid) {
    stop(
      "Benchmark Spatial entry `",
      name,
      "` must contain finite numeric x/y coordinates.",
      call. = FALSE
    )
  }

  data.frame(
    name = name,
    renderable_rows = nrow(coordinates),
    stringsAsFactors = FALSE
  )
}

benchmark_artifact_provenance <- function(crb) {
  crb <- normalizePath(crb, mustWork = TRUE)
  object <- read_benchmark_crb_payload(crb)
  backend <- if (
    is.environment(object) &&
      exists("expression_backend", envir = object, inherits = FALSE)
  ) {
    get("expression_backend", envir = object, inherits = FALSE)
  } else {
    NULL
  }
  sidecar <- if (
    is.list(backend) &&
      identical(backend$type, "bpcells") &&
      is.character(backend$location) &&
      length(backend$location) == 1L
  ) {
    normalizePath(file.path(dirname(crb), backend$location), mustWork = TRUE)
  } else {
    NA_character_
  }
  pack_manifest <- file.path(
    dirname(crb),
    paste0(tools::file_path_sans_ext(basename(crb)), ".viewer"),
    "manifest.json"
  )
  if (file.exists(pack_manifest) && !dir.exists(pack_manifest)) {
    pack_manifest <- normalizePath(pack_manifest, mustWork = TRUE)
  } else {
    pack_manifest <- NA_character_
  }
  data.frame(
    artifact = crb,
    artifact_sha256 = sha256_file(crb),
    bpcells_sidecar = sidecar,
    bpcells_sidecar_sha256 = if (is.na(sidecar)) {
      NA_character_
    } else {
      sha256_directory(sidecar)
    },
    viewer_pack_manifest = pack_manifest,
    viewer_pack_manifest_sha256 = if (is.na(pack_manifest)) {
      NA_character_
    } else {
      sha256_file(pack_manifest)
    },
    stringsAsFactors = FALSE
  )
}

build_balanced_schedule <- function(candidates, page_names, rounds) {
  rows <- list()
  index <- 1L
  for (round in seq_len(rounds)) {
    shift <- (round - 1L) %% length(candidates)
    order <- candidates[c(
      seq.int(shift + 1L, length(candidates)),
      if (shift) seq_len(shift) else integer()
    )]
    for (page_name in page_names) {
      for (visit in c("first", "repeat")) {
        for (position in seq_along(order)) {
          rows[[index]] <- data.frame(
            schedule_position = index,
            round = round,
            candidate_position = position,
            candidate = order[[position]],
            page = page_name,
            visit = visit,
            stringsAsFactors = FALSE
          )
          index <- index + 1L
        }
      }
    }
  }
  do.call(rbind, rows)
}
