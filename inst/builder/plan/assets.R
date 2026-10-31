.builder_plan_asset_claim <- function(source, target, artifact) {
  structure(
    list(
      source = source,
      target = target,
      artifact = artifact
    ),
    class = c("builder_asset_claim", "list")
  )
}

.builder_plan_normalize_asset_claim <- function(claim) {
  fields <- c("source", "target", "artifact")
  if (
    !inherits(claim, "builder_asset_claim") ||
      !is.list(claim) ||
      length(claim) != length(fields) ||
      .builder_plan_has_reference(claim)
  ) {
    return(NULL)
  }
  claim_names <- attr(claim, "names", exact = TRUE)
  if (
    !is.character(claim_names) ||
      length(claim_names) != length(fields) ||
      anyNA(claim_names) ||
      any(!nzchar(claim_names)) ||
      anyDuplicated(claim_names) ||
      !setequal(claim_names, fields)
  ) {
    return(NULL)
  }
  plain_claim <- claim
  attributes(plain_claim) <- list(names = claim_names)
  values <- plain_claim[fields]
  values <- lapply(values, function(value) {
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        .builder_plan_has_reference(value)
    ) {
      return(NULL)
    }
    attributes(value) <- NULL
    if (!nzchar(trimws(value))) {
      return(NULL)
    }
    value
  })
  if (any(vapply(values, is.null, logical(1)))) {
    return(NULL)
  }
  .builder_plan_asset_claim(
    values$source,
    values$target,
    values$artifact
  )
}

.builder_plan_asset_set <- function(value) {
  if (is.null(value)) {
    value <- character()
  }
  if (is.character(value)) {
    valid <- .builder_plan_character_set(value)
    claims <- if (valid) {
      .builder_plan_legacy_asset_claims(value)
    } else {
      list()
    }
    return(list(valid = valid, targets = value, claims = claims))
  }
  if (inherits(value, "builder_asset_claim")) {
    value <- list(value)
  }
  if (!is.list(value)) {
    return(list(valid = FALSE, targets = character(), claims = list()))
  }
  if (!length(value)) {
    return(list(valid = TRUE, targets = character(), claims = list()))
  }
  claims <- lapply(value, .builder_plan_normalize_asset_claim)
  valid <- !any(vapply(claims, is.null, logical(1)))
  if (!valid) {
    return(list(valid = FALSE, targets = character(), claims = list()))
  }
  if (length(.builder_plan_dedupe_claims(claims)) != length(claims)) {
    return(list(valid = FALSE, targets = character(), claims = list()))
  }
  list(
    valid = TRUE,
    targets = vapply(claims, `[[`, character(1), "target"),
    claims = claims
  )
}

.builder_plan_internal_asset_claims <- function(paths, entry) {
  lapply(paths, function(path) {
    .builder_plan_asset_claim(
      paste0("builder_dataset:", entry$id),
      path,
      path
    )
  })
}

.builder_plan_legacy_asset_claims <- function(paths) {
  lapply(paths, function(path) {
    structure(
      list(
        source = NULL,
        target = path,
        artifact = NULL
      ),
      class = c("builder_asset_claim", "list")
    )
  })
}

.builder_plan_dedupe_claims <- function(claims) {
  out <- list()
  for (claim in claims) {
    duplicate <- any(vapply(out, identical, logical(1), y = claim))
    if (!duplicate) {
      out[[length(out) + 1L]] <- claim
    }
  }
  out
}

.builder_plan_claim_target_conflict <- function(claims) {
  targets <- vapply(claims, `[[`, character(1), "target")
  any(vapply(
    unique(targets),
    function(target) {
      matching <- claims[targets == target]
      if (length(matching) < 2L) {
        return(FALSE)
      }
      incomplete <- any(vapply(
        matching,
        function(claim) {
          !builder_has_text(claim$source) ||
            !builder_has_text(claim$artifact)
        },
        logical(1)
      ))
      incomplete ||
        any(!vapply(matching[-1L], identical, logical(1), y = matching[[1L]]))
    },
    logical(1)
  ))
}

.builder_plan_source_snapshot_identity <- function(entry) {
  source_identity <- .builder_plan_entry_source_identity(entry)
  candidates <- Filter(
    Negate(is.null),
    list(entry$snapshot, entry$snapshot_identity)
  )
  owned_available <- exists(
    ".builder_snapshot_owned",
    mode = "function",
    inherits = TRUE
  )
  if (owned_available) {
    for (snapshot in candidates) {
      valid_time <- is.list(snapshot) &&
        inherits(snapshot$created_at, "POSIXt") &&
        length(snapshot$created_at) == 1L &&
        !is.na(snapshot$created_at) &&
        is.finite(as.numeric(snapshot$created_at))
      valid_bytes <- is.list(snapshot) &&
        is.numeric(snapshot$closure_bytes) &&
        length(snapshot$closure_bytes) == 1L &&
        !is.na(snapshot$closure_bytes) &&
        is.finite(snapshot$closure_bytes) &&
        snapshot$closure_bytes >= 0
      valid_md5 <- is.list(snapshot) &&
        builder_has_text(snapshot$object_md5 %||% "") &&
        grepl("^[[:xdigit:]]{32}$", snapshot$object_md5)
      valid_shape <- is.list(snapshot) &&
        is.null(attr(snapshot, "class", exact = TRUE)) &&
        builder_has_text(snapshot$path %||% "") &&
        builder_has_text(snapshot$object_file %||% "") &&
        identical(
          snapshot$object_file,
          file.path(snapshot$path, "object.rds")
        ) &&
        builder_has_text(snapshot$owner_token %||% "") &&
        valid_time &&
        valid_md5 &&
        valid_bytes &&
        !.builder_plan_has_reference(snapshot)
      owned <- valid_shape &&
        isTRUE(tryCatch(
          .builder_snapshot_owned(snapshot, verify_content = FALSE),
          error = function(error) FALSE
        ))
      if (owned) {
        return(c(
          list(
            available = TRUE,
            snapshot = snapshot,
            source = source_identity
          ),
          snapshot
        ))
      }
    }
  }
  list(
    available = FALSE,
    snapshot = NULL,
    source = source_identity,
    reason = paste0(
      "No Builder-owned snapshot with a matching owner marker and MD5 ",
      "was available to planning."
    )
  )
}

.builder_plan_backend <- function(settings, filename) {
  recommendations <- settings$recommendations
  backend <- settings$expression_backend
  if (is.null(backend) && is.null(recommendations)) {
    backend <- "embedded"
  }
  if (
    !builder_has_text(backend %||% "") ||
      !backend %in% c("embedded", "h5")
  ) {
    return(list(valid = FALSE, error_code = "invalid_expression_backend"))
  }
  stem <- tools::file_path_sans_ext(basename(filename))
  sidecars <- switch(
    backend,
    embedded = character(),
    h5 = paste0(stem, ".h5")
  )
  supplied <- settings$sidecars
  if (!is.null(supplied) && !identical(supplied, sidecars)) {
    return(list(valid = FALSE, error_code = "invalid_backend_sidecars"))
  }
  list(
    valid = TRUE,
    error_code = NULL,
    mode = backend,
    sidecars = sidecars,
    dataset_file = filename
  )
}

.builder_plan_analysis_graph <- function(analyses, has_marker_genes) {
  nodes <- lapply(analyses, function(id) {
    dependencies <- if (identical(id, "enriched_pathways")) {
      if ("marker_genes" %in% analyses || isTRUE(has_marker_genes)) {
        "marker_genes"
      } else {
        character()
      }
    } else {
      character()
    }
    list(id = id, dependencies = dependencies)
  })
  names(nodes) <- analyses
  nodes
}

.builder_plan_artifact_identity <- function(
  entry,
  included_groups,
  included_projections,
  analyses = character(),
  included_trajectories = list(),
  cell_cycle = character()
) {
  profile <- if (is.list(entry$dataset_profile)) {
    entry$dataset_profile
  } else {
    list()
  }
  identity <- if (is.list(profile$identity)) profile$identity else list()
  settings <- if (is.list(entry$settings)) entry$settings else list()
  feature_record <- function() {
    assays <- profile$assays
    if (!is.list(assays) || !length(assays)) {
      return(identity$features)
    }
    assay <- settings$assay
    layer <- settings$layer
    if (
      !is.character(assay) ||
        length(assay) != 1L ||
        is.na(assay) ||
        !is.character(layer) ||
        length(layer) != 1L ||
        is.na(layer)
    ) {
      stop("invalid_artifact_identity", call. = FALSE)
    }
    record <- assays[[assay]]$layers[[layer]]$features
    if (!is.list(record)) {
      stop("invalid_artifact_identity", call. = FALSE)
    }
    record
  }
  axis_ids <- function(axis) {
    record <- if (identical(axis, "features")) {
      feature_record()
    } else {
      identity[[axis]]
    }
    if (!is.list(record)) {
      stop("invalid_artifact_identity", call. = FALSE)
    }
    compact <- builder_axis_identity_normalize(record$axis_identity)
    if (!is.null(compact)) {
      return(compact)
    }
    values <- record$canonical_ids %||% record$ids
    if (is.null(values)) {
      stop("invalid_artifact_identity", call. = FALSE)
    }
    if (!is.character(values) || anyNA(values)) {
      stop("invalid_artifact_identity", call. = FALSE)
    }
    builder_axis_identity(unname(values))
  }
  levels <- if (is.list(entry$levels)) entry$levels else list()
  group_levels <- lapply(included_groups, function(group) {
    values <- levels[[group]] %||% character()
    if (!is.character(values) || anyNA(values)) character() else unname(values)
  })
  names(group_levels) <- included_groups
  policy <- settings$metadata_policy
  source_metadata <- if (is.list(policy)) {
    policy$retained %||% policy$included %||% character()
  } else {
    character()
  }
  nUMI <- settings$nUMI %||% entry$profile$nUMI
  nGene <- settings$nGene %||% entry$profile$nGene
  ## "nUMI" and "nGene" are the CRB's own QC columns, filled from the chosen
  ## nUMI/nGene source columns; source columns of those names are not
  ## exported (.addExportMetaData()).
  additional_metadata <- setdiff(
    source_metadata,
    c("cell_barcode", "nUMI", "nGene", included_groups, cell_cycle, nUMI, nGene)
  )
  generated_metadata <- if ("percent_mt_ribo" %in% analyses) {
    c("percent_mt", "percent_ribo")
  } else {
    character()
  }
  metadata <- make.unique(c(
    "cell_barcode",
    included_groups,
    "nUMI",
    "nGene",
    setdiff(cell_cycle, included_groups),
    additional_metadata,
    setdiff(generated_metadata, source_metadata)
  ))
  spatial <- if (is.list(profile$spatial)) profile$spatial else list()
  spatial_sections <- spatial$sections %||% character()
  if (!is.character(spatial_sections) || anyNA(spatial_sections)) {
    spatial_sections <- character()
  }
  list(
    schema_version = 3L,
    cells = axis_ids("cells"),
    features = axis_ids("features"),
    group_levels = group_levels,
    projections = unname(included_projections),
    trajectories = included_trajectories,
    source_metadata = unname(source_metadata),
    metadata = unname(metadata),
    spatial_sections = unname(spatial_sections)
  )
}

.builder_plan_release_manifest <- function(manifests) {
  ids <- unique(unlist(lapply(manifests, names), use.names = FALSE))
  out <- lapply(ids, function(id) {
    entries <- lapply(manifests, function(manifest) manifest[[id]])
    present <- !vapply(entries, is.null, logical(1))
    entries <- entries[present]
    dataset_ids <- names(manifests)[present]
    visible <- vapply(
      entries,
      function(entry) isTRUE(entry$page_visible),
      logical(1)
    )
    candidates <- if (any(visible)) which(visible) else seq_along(entries)
    rank <- c(
      blocking = 5L,
      checking = 4L,
      attention = 3L,
      valid = 2L,
      not_applicable = 1L
    )
    statuses <- vapply(entries, `[[`, "", "status")
    exemplar <- candidates[[which.max(unname(rank[statuses[candidates]]))]]
    record <- entries[[exemplar]]
    if (.builder_plan_has_reference(record)) {
      stop("unsafe_reference", call. = FALSE)
    }
    record$page_visible <- any(visible)
    record$dataset_ids <- dataset_ids
    record$dataset_entries <- entries
    if (length(entries) > 1L) {
      record$evidence <- lapply(entries, `[[`, "evidence")
      names(record$evidence) <- dataset_ids
    }
    record
  })
  names(out) <- ids
  out
}
