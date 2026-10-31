## Parent-authored portable build report.

.builder_report_json <- function(value, pretty = FALSE) {
  jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = pretty
  )
}

.builder_report_identity <- function(report) {
  payload <- report
  attr(payload, "class") <- NULL
  payload$identity <- NULL
  payload <- jsonlite::fromJSON(
    .builder_report_json(payload),
    simplifyVector = FALSE
  )
  canonical <- function(value) {
    if (is.null(value)) {
      return("null")
    }
    value_names <- names(value)
    if (is.list(value) || length(value) != 1L || !is.null(value_names)) {
      if (!length(value)) {
        return("[]")
      }
      entries <- lapply(seq_along(value), function(index) {
        name <- if (is.null(value_names)) "" else value_names[[index]]
        paste0(encodeString(name, quote = '"'), ":", canonical(value[[index]]))
      })
      return(paste0("[", paste(entries, collapse = ","), "]"))
    }
    if (is.character(value)) {
      return(paste0("s:", encodeString(value, quote = '"')))
    }
    if (is.logical(value)) {
      return(if (isTRUE(value)) "b:true" else "b:false")
    }
    if (is.numeric(value)) {
      return(paste0("n:", format(value, scientific = FALSE, trim = TRUE)))
    }
    stop(
      "The portable Builder report contains an unsupported value.",
      call. = FALSE
    )
  }
  path <- tempfile("builder-report-identity-", fileext = ".json")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeLines(canonical(payload), path, useBytes = TRUE)
  unname(tools::md5sum(path))
}

.builder_report_strings <- function(value) {
  if (is.character(value)) {
    return(value)
  }
  if (is.list(value)) {
    return(unlist(lapply(value, .builder_report_strings), use.names = FALSE))
  }
  character()
}

.builder_report_normalize_sensitive_tokens <- function(values) {
  values <- tolower(as.character(values))
  values <- gsub("[^\\p{L}\\p{N}]+", " ", values, perl = TRUE)
  trimws(values)
}

.builder_report_restricted_strings <- function(value, path = character()) {
  restricted <- any(grepl(
    "warning|diagnostic|provenance|operational|runtime|source|host|pid|raw",
    path,
    ignore.case = TRUE
  ))
  if (is.character(value)) {
    return(if (restricted) value else character())
  }
  if (!is.list(value)) {
    return(character())
  }
  value_names <- names(value)
  unlist(
    lapply(seq_along(value), function(index) {
      name <- if (is.null(value_names)) "" else value_names[[index]]
      .builder_report_restricted_strings(value[[index]], c(path, name))
    }),
    use.names = FALSE
  )
}

.builder_report_sensitive_key_paths <- function(value, path = character()) {
  if (!is.list(value)) {
    return(character())
  }
  value_names <- names(value)
  if (is.null(value_names)) {
    value_names <- rep("", length(value))
  }
  unlist(
    lapply(seq_along(value), function(index) {
      key <- value_names[[index]]
      next_path <- c(path, key)
      normalized <- .builder_report_normalize_sensitive_tokens(key)
      sensitive <- nzchar(normalized) &&
        grepl(
          "^(private diagnostics?|diagnostics?|host|pid|source|raw values?)( |$)",
          normalized
        )
      c(
        if (sensitive) paste(next_path[nzchar(next_path)], collapse = "/"),
        .builder_report_sensitive_key_paths(value[[index]], next_path)
      )
    }),
    use.names = FALSE
  )
}

.builder_report_safe_member <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value) &&
    !startsWith(value, "/") &&
    !grepl("^[A-Za-z]:", value) &&
    !grepl("\\", value, fixed = TRUE) &&
    all(!strsplit(value, "/", fixed = TRUE)[[1L]] %in% c("", ".", ".."))
}

.builder_report_text <- function(value) {
  is.character(value) &&
    !is.object(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

.builder_report_character_vector <- function(value, empty = TRUE) {
  is.character(value) &&
    !is.object(value) &&
    !anyNA(value) &&
    (empty || length(value) > 0L) &&
    is.null(names(value))
}

.builder_report_count <- function(value) {
  is.numeric(value) &&
    !is.object(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= 0 &&
    value == floor(value)
}

.builder_report_json_character_vector <- function(value) {
  if (is.character(value)) {
    return(unname(value))
  }
  if (
    is.list(value) &&
      is.null(names(value)) &&
      all(vapply(value, .builder_report_text, logical(1)))
  ) {
    return(unname(vapply(value, identity, character(1))))
  }
  value
}

.builder_report_json_normalize <- function(report) {
  vector_fields <- c(
    "dataset_order",
    "viewer_bundle_assets",
    "private_assets",
    "output_members",
    "warnings"
  )
  for (field in vector_fields) {
    report[[field]] <- .builder_report_json_character_vector(report[[field]])
  }
  if (is.list(report$datasets) && !is.object(report$datasets)) {
    dataset_vector_fields <- c(
      "artifact_members",
      "methods",
      "groups",
      "projections",
      "metadata_columns",
      "pages"
    )
    report$datasets <- lapply(report$datasets, function(dataset) {
      if (!is.list(dataset) || is.object(dataset)) {
        return(dataset)
      }
      for (field in dataset_vector_fields) {
        dataset[[field]] <- .builder_report_json_character_vector(
          dataset[[field]]
        )
      }
      if (is.list(dataset$metadata) && !is.object(dataset$metadata)) {
        for (field in c("retained", "excluded", "forced")) {
          dataset$metadata[[field]] <- .builder_report_json_character_vector(
            dataset$metadata[[field]]
          )
        }
      }
      dataset
    })
  }
  if (is.list(report$content) && !is.object(report$content)) {
    report$content <- lapply(report$content, function(entry) {
      if (is.list(entry) && !is.object(entry)) {
        entry$pages <- .builder_report_json_character_vector(entry$pages)
      }
      entry
    })
  }
  report
}

.builder_report_nested_schema_valid <- function(report) {
  dataset_fields <- c(
    "id",
    "name",
    "artifact_members",
    "organism",
    "methods",
    "groups",
    "projections",
    "metadata_columns",
    "metadata",
    "cell_count",
    "feature_count",
    "expression_backend",
    "expression_storage",
    "spatial_image_storage",
    "pages"
  )
  content_fields <- c("status", "disposition", "pages")
  valid_vector <- function(value, empty = TRUE, unique = FALSE) {
    .builder_report_character_vector(value, empty) &&
      (!unique || !anyDuplicated(value))
  }
  valid_dataset <- function(dataset) {
    metadata_valid <- is.list(dataset$metadata) &&
      !is.object(dataset$metadata) &&
      identical(names(dataset$metadata), c("retained", "excluded", "forced")) &&
      valid_vector(dataset$metadata$retained, unique = TRUE) &&
      valid_vector(dataset$metadata$excluded, unique = TRUE) &&
      valid_vector(dataset$metadata$forced, unique = TRUE) &&
      !length(intersect(
        dataset$metadata$retained,
        dataset$metadata$excluded
      )) &&
      all(dataset$metadata$forced %in% dataset$metadata$retained)
    expression_storage_valid <- is.list(dataset$expression_storage) &&
      !is.object(dataset$expression_storage) &&
      identical(names(dataset$expression_storage), "mode") &&
      identical(
        dataset$expression_storage$mode,
        dataset$expression_backend
      )
    spatial_storage <- dataset$spatial_image_storage
    spatial_storage_valid <- is.list(spatial_storage) &&
      !is.object(spatial_storage) &&
      identical(
        names(spatial_storage),
        c("mode", "image_count", "section_count")
      ) &&
      .builder_report_text(spatial_storage$mode) &&
      identical(spatial_storage$mode, "external") &&
      .builder_report_count(spatial_storage$image_count) &&
      .builder_report_count(spatial_storage$section_count)
    is.list(dataset) &&
      !is.object(dataset) &&
      identical(names(dataset), dataset_fields) &&
      .builder_report_text(dataset$id) &&
      .builder_report_text(dataset$name) &&
      valid_vector(dataset$artifact_members, empty = FALSE, unique = TRUE) &&
      all(vapply(
        dataset$artifact_members,
        .builder_report_safe_member,
        logical(1)
      )) &&
      (is.null(dataset$organism) || .builder_report_text(dataset$organism)) &&
      valid_vector(dataset$methods, unique = TRUE) &&
      valid_vector(dataset$groups, unique = TRUE) &&
      valid_vector(dataset$projections, unique = TRUE) &&
      valid_vector(dataset$metadata_columns, unique = TRUE) &&
      metadata_valid &&
      .builder_report_count(dataset$cell_count) &&
      .builder_report_count(dataset$feature_count) &&
      .builder_report_text(dataset$expression_backend) &&
      dataset$expression_backend %in% c("embedded", "h5") &&
      expression_storage_valid &&
      spatial_storage_valid &&
      valid_vector(dataset$pages, unique = TRUE)
  }
  valid_content <- function(content) {
    is.list(content) &&
      !is.object(content) &&
      identical(names(content), content_fields) &&
      (is.null(content$status) || .builder_report_text(content$status)) &&
      (is.null(content$disposition) ||
        .builder_report_text(content$disposition)) &&
      valid_vector(content$pages, unique = TRUE)
  }
  datasets <- report$datasets
  content <- report$content
  if (
    !valid_vector(report$dataset_order, empty = FALSE, unique = TRUE) ||
      !is.list(datasets) ||
      is.object(datasets) ||
      length(datasets) != length(report$dataset_order) ||
      !is.list(content) ||
      is.object(content) ||
      !valid_vector(report$viewer_bundle_assets, unique = TRUE) ||
      !valid_vector(report$private_assets, unique = TRUE) ||
      !valid_vector(report$warnings, unique = TRUE)
  ) {
    return(FALSE)
  }
  if (!all(vapply(datasets, valid_dataset, logical(1)))) {
    return(FALSE)
  }
  if (!all(vapply(content, valid_content, logical(1)))) {
    return(FALSE)
  }
  dataset_names <- names(datasets)
  content_names <- names(content)
  valid_dataset_names <- is.null(dataset_names) ||
    identical(dataset_names, report$dataset_order)
  valid_content_names <- !length(content) ||
    (!is.null(content_names) &&
      !anyNA(content_names) &&
      all(nzchar(content_names)) &&
      !anyDuplicated(content_names))
  valid_artifact_members <- unlist(
    lapply(datasets, `[[`, "artifact_members"),
    use.names = FALSE
  )
  valid_dataset_names &&
    identical(
      vapply(datasets, `[[`, character(1), "id"),
      report$dataset_order
    ) &&
    all(valid_artifact_members %in% report$output_members) &&
    all(valid_artifact_members %in% report$private_assets) &&
    !length(intersect(
      report$viewer_bundle_assets,
      report$private_assets
    )) &&
    all(vapply(
      c(report$viewer_bundle_assets, report$private_assets),
      .builder_report_safe_member,
      logical(1)
    )) &&
    valid_content_names &&
    "build-report.json" %in% report$output_members &&
    identical(
      "cerebro_app" %in% report$output_members,
      report$artifact_mode %in%
        c(
          "crbs_and_private_app",
          "private_app",
          "authenticated_private_app"
        )
    ) &&
    identical(
      "cerebro_app/viewer-auth.env" %in% report$output_members,
      identical(report$artifact_mode, "authenticated_private_app")
    ) &&
    identical(
      "cerebro_app/private-data/auth/credentials.sqlite" %in%
        report$output_members,
      identical(report$artifact_mode, "authenticated_private_app")
    )
}

.builder_report_validate <- function(report, require_class = FALSE) {
  if (
    !is.list(report) ||
      (isTRUE(require_class) && !inherits(report, "builder_build_report"))
  ) {
    stop("A portable Builder report is required.", call. = FALSE)
  }
  expected <- c(
    "schema_version",
    "identity",
    "plan_revision",
    "artifact_mode",
    "dataset_order",
    "datasets",
    "content",
    "viewer_bundle_assets",
    "private_assets",
    "output_members",
    "warnings"
  )
  if (
    !identical(names(report), expected) ||
      !.builder_report_count(report$schema_version) ||
      !identical(report$schema_version, 1L) ||
      !.builder_report_text(report$identity) ||
      !grepl("^[0-9a-f]{32}$", report$identity) ||
      !.builder_report_count(report$plan_revision) ||
      report$plan_revision < 1 ||
      !.builder_report_text(report$artifact_mode) ||
      !report$artifact_mode %in%
        c(
          "crbs_only",
          "crbs_and_private_app",
          "private_app",
          "authenticated_private_app"
        ) ||
      !.builder_report_character_vector(report$output_members, empty = FALSE) ||
      anyDuplicated(report$output_members) ||
      !all(vapply(
        report$output_members,
        .builder_report_safe_member,
        logical(1)
      )) ||
      !identical(
        sort(report$output_members, method = "radix"),
        report$output_members
      ) ||
      !.builder_report_nested_schema_valid(report)
  ) {
    stop("The portable Builder report schema is invalid.", call. = FALSE)
  }
  sensitive_key_paths <- .builder_report_sensitive_key_paths(report)
  if (length(sensitive_key_paths)) {
    stop(
      "The portable Builder report contains a sensitive structural field.",
      call. = FALSE
    )
  }
  strings <- .builder_report_strings(report)
  restricted_strings <- .builder_report_restricted_strings(report)
  normalized_tokens <- .builder_report_normalize_sensitive_tokens(
    restricted_strings
  )
  absolute <- grepl("(^|[^[:alnum:]])/[[:graph:]]+", strings) |
    grepl("(^|[^[:alnum:]])[A-Za-z]:[/\\]", strings) |
    grepl("\\\\", strings)
  private_runtime <- grepl(
    "[.]cerebro-builder|builder-stage|control-root|backup-root|lock-root",
    restricted_strings,
    ignore.case = TRUE
  )
  operational_identity <- grepl(
    paste0(
      "(^|[^[:alnum:]_])",
      "(pid|host|source|raw([_ -]?[[:alnum:]_]+)?|",
      "private[ _-]?diagnostic|diagnostic)",
      "[[:space:]]*[:=]"
    ),
    restricted_strings,
    ignore.case = TRUE
  )
  sensitive_phrase <- grepl(
    "(^|[^[:alnum:]])(raw values?|private diagnostics?|diagnostics?)([[:space:]:=]|$)",
    normalized_tokens
  )
  sensitive_identity_token <- grepl(
    "(^| )(host|pid|source) [^ ]+",
    normalized_tokens
  )
  if (
    any(absolute) ||
      any(private_runtime) ||
      any(operational_identity) ||
      any(sensitive_phrase) ||
      any(sensitive_identity_token)
  ) {
    stop("The portable Builder report failed redaction.", call. = FALSE)
  }
  if (!identical(.builder_report_identity(report), report$identity)) {
    stop(
      "The portable Builder report identity does not match its payload.",
      call. = FALSE
    )
  }
  invisible(report)
}

builder_build_report <- function(plan, result) {
  if (
    !inherits(plan, "builder_build_plan") ||
      !is.list(plan) ||
      !identical(plan$readiness, "ready") ||
      !is.list(result) ||
      !identical(result$state, "success") ||
      !isTRUE(result$publishable)
  ) {
    stop(
      "A frozen plan and verified successful result are required.",
      call. = FALSE
    )
  }
  stage <- normalizePath(result$stage, winslash = "/", mustWork = TRUE)
  built <- unname(result$built %||% character())
  expected_files <- vapply(plan$items, `[[`, character(1), "filename")
  if (
    length(built) != length(expected_files) ||
      !all(file.exists(built)) ||
      !all(vapply(built, .pathWithin, logical(1), parent = stage)) ||
      !identical(basename(built), expected_files) ||
      length(result$verifications) != length(expected_files) ||
      !all(vapply(
        result$verifications,
        function(value) isTRUE(value$valid),
        logical(1)
      ))
  ) {
    stop(
      "The verified build result does not match the frozen plan.",
      call. = FALSE
    )
  }
  if (
    isTRUE(plan$make_app) &&
      (!is.character(result$app_dir) ||
        length(result$app_dir) != 1L ||
        !dir.exists(result$app_dir) ||
        !.pathWithin(result$app_dir, stage) ||
        !is.list(result$app_verification) ||
        !isTRUE(result$app_verification$valid))
  ) {
    stop(
      "The verified App result does not match the frozen plan.",
      call. = FALSE
    )
  }
  datasets <- lapply(seq_along(plan$items), function(index) {
    item <- plan$items[[index]]
    verification <- result$verifications[[index]]
    list(
      id = item$id,
      name = item$name,
      artifact_members = unname(
        if (isTRUE(plan$make_app)) {
          file.path(
            "cerebro_app",
            "private-data",
            c(
              item$filename,
              item$sidecars %||% character()
            )
          )
        } else {
          c(item$filename, item$sidecars %||% character())
        }
      ),
      organism = item$organism %||% NULL,
      methods = unname(item$analyses %||% character()),
      groups = unname(item$included_groups %||% character()),
      projections = unname(item$included_projections %||% character()),
      metadata_columns = unname(
        verification$metadata %||%
          item$metadata_policy$retained %||%
          item$metadata_policy$included %||%
          character()
      ),
      metadata = list(
        retained = unname(
          item$metadata_policy$retained %||%
            item$metadata_policy$included %||%
            character()
        ),
        excluded = unname(item$metadata_policy$excluded %||% character()),
        forced = unname(item$metadata_policy$forced %||% character())
      ),
      cell_count = as.integer(
        verification$cell_count %||%
          length(verification$cells %||% character())
      ),
      feature_count = as.integer(
        verification$feature_count %||%
          length(verification$features %||% character())
      ),
      expression_backend = item$expression_backend,
      expression_storage = list(mode = item$expression_backend),
      spatial_image_storage = list(
        mode = "external",
        image_count = as.integer(item$spatial_alignment$image_count %||% 0L),
        section_count = as.integer(
          item$spatial_alignment$section_count %||% 0L
        )
      ),
      pages = unname(
        item$viewer_page_expectations$visible_conditional %||% character()
      )
    )
  })
  content <- lapply(plan$manifest %||% list(), function(entry) {
    disposition <- entry$disposition %||% NULL
    if (
      is.character(disposition) &&
        !is.object(disposition) &&
        length(disposition) == 1L &&
        is.na(disposition)
    ) {
      disposition <- NULL
    }
    list(
      status = entry$status %||% NULL,
      disposition = disposition,
      pages = unname(entry$pages %||% character())
    )
  })
  targets <- plan$output_release$targets %||% character()
  members <- vapply(
    targets,
    .builder_release_relative,
    character(1),
    root = plan$out_dir
  )
  app_members <- if (isTRUE(plan$make_app)) {
    unlist(
      lapply(plan$items, function(item) {
        file.path(
          "cerebro_app",
          "private-data",
          c(item$filename, item$sidecars %||% character())
        )
      }),
      use.names = FALSE
    )
  } else {
    character()
  }
  auth_members <- if (isTRUE(plan$make_app) && isTRUE(plan$app_auth$enabled)) {
    c(
      "cerebro_app/private-data/auth/credentials.sqlite",
      "cerebro_app/viewer-auth.env"
    )
  } else {
    character()
  }
  members <- sort(
    unique(c(members, app_members, auth_members, "build-report.json")),
    method = "radix"
  )
  report <- list(
    schema_version = 1L,
    identity = paste(rep("0", 32L), collapse = ""),
    plan_revision = as.integer(plan$revision),
    artifact_mode = if (!isTRUE(plan$make_app)) {
      "crbs_only"
    } else if (isTRUE(plan$app_auth$enabled)) {
      "authenticated_private_app"
    } else {
      "private_app"
    },
    dataset_order = unname(plan$dataset_order),
    datasets = datasets,
    content = content,
    viewer_bundle_assets = unname(
      plan$viewer_bundle_assets %||% character()
    ),
    private_assets = unname(sort(
      unique(c(
        if (isTRUE(plan$make_app)) {
          character()
        } else {
          plan$private_assets %||% character()
        },
        app_members,
        auth_members
      )),
      method = "radix"
    )),
    output_members = members,
    warnings = unname(as.character(unique(.builder_report_strings(
      plan$acknowledgements %||% list()
    ))))
  )
  report$identity <- .builder_report_identity(report)
  class(report) <- c("builder_build_report", "list")
  .builder_report_validate(report, require_class = TRUE)
  report
}

builder_read_build_report <- function(
  path,
  .read_json = function(path) {
    jsonlite::read_json(path, simplifyVector = FALSE)
  }
) {
  value <- tryCatch(
    .read_json(path),
    error = function(error) {
      stop("The build report JSON could not be read or parsed.", call. = FALSE)
    }
  )
  if (!is.list(value)) {
    stop("The build report JSON could not be read or parsed.", call. = FALSE)
  }
  value <- .builder_report_json_normalize(value)
  .builder_report_validate(value)
  structure(value, class = c("builder_build_report", "list"))
}

builder_write_build_report <- function(
  stage,
  report,
  .move = file.rename,
  .write_json = jsonlite::write_json,
  .read_json = function(path) {
    jsonlite::read_json(path, simplifyVector = FALSE)
  }
) {
  stage <- normalizePath(stage, winslash = "/", mustWork = TRUE)
  .builder_report_validate(report, require_class = TRUE)
  target <- file.path(stage, "build-report.json")
  target_link <- Sys.readlink(target)
  if (
    file.exists(target) ||
      dir.exists(target) ||
      (!is.na(target_link) && nzchar(target_link))
  ) {
    stop("The assigned stage already contains a build report.", call. = FALSE)
  }
  temporary <- tempfile(".build-report-", tmpdir = stage, fileext = ".json")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  tryCatch(
    .write_json(
      unclass(report),
      temporary,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA,
      pretty = TRUE
    ),
    error = function(error) {
      stop("The build report JSON could not be written.", call. = FALSE)
    }
  )
  if (!file.exists(temporary) || !isTRUE(.move(temporary, target))) {
    stop("The build report could not be committed atomically.", call. = FALSE)
  }
  reread <- tryCatch(
    builder_read_build_report(target, .read_json = .read_json),
    error = function(error) {
      unlink(target, force = TRUE)
      stop(conditionMessage(error), call. = FALSE)
    }
  )
  if (!identical(reread$identity, report$identity)) {
    unlink(target, force = TRUE)
    stop("The build report JSON round-trip identity changed.", call. = FALSE)
  }
  target
}
