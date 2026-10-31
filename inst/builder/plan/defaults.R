##----------------------------------------------------------------------------##
## Turn UI entries into a validated, deterministic build plan.
##
## Pure helpers only. The Shiny process creates the plan; the worker executes
## it without having to reinterpret UI state.
##----------------------------------------------------------------------------##

`%||%` <- function(a, b) if (is.null(a)) b else a

builder_safe_stem <- function(
  value,
  fallback = "dataset",
  max_chars = 48L
) {
  original <- enc2utf8(as.character(value)[[1L]])
  value <- iconv(value, to = "ASCII//TRANSLIT", sub = "")
  value <- tolower(trimws(value))
  value <- gsub("[^a-z0-9]+", "-", value)
  value <- gsub("(^-+|-+$)", "", value)
  value <- if (!nzchar(value)) fallback else value
  max_chars <- as.integer(max_chars)
  if (nchar(value, type = "chars") <= max_chars) {
    return(value)
  }
  digest <- substr(
    as.character(digest::digest(
      charToRaw(original),
      algo = "md5",
      serialize = FALSE
    )),
    1L,
    8L
  )
  paste0(substr(value, 1L, max_chars - 9L), "-", digest)
}

builder_item_filename <- function(item, index, total) {
  width <- max(2L, nchar(as.character(total)))
  paste0(
    sprintf(paste0("%0", width, "d"), index),
    "-",
    builder_safe_stem(item$settings$name, max_chars = 24L),
    "-",
    builder_safe_stem(
      item$id,
      fallback = paste0("ds", index),
      max_chars = 20L
    ),
    ".crb"
  )
}

builder_profile_has <- function(profile, key) {
  any(vapply(
    profile$extras %||% list(),
    function(x) identical(x$key, key) && isTRUE(x$found),
    logical(1)
  ))
}

builder_default_first <- function(values, default) {
  if (is.null(default) || !length(default) || !default %in% values) {
    return(values)
  }
  c(default, values[values != default])
}

builder_trajectory_default_first <- function(values, default) {
  if (!is.list(values) || !length(values)) {
    return(values)
  }
  if (
    !is.list(default) ||
      !builder_has_text(default$method) ||
      !builder_has_text(default$name) ||
      !default$method %in% names(values) ||
      !default$name %in% values[[default$method]]
  ) {
    return(values)
  }
  method <- default$method
  ordered <- c(
    stats::setNames(
      list(builder_default_first(values[[method]], default$name)),
      method
    ),
    values[names(values) != method]
  )
  ordered
}

builder_normalize_analyses <- function(selected, has_marker_genes = FALSE) {
  if (
    !exists(
      ".builder_state_normalize_analyses",
      mode = "function",
      inherits = TRUE
    )
  ) {
    stop("state_authority_unavailable", call. = FALSE)
  }
  .builder_state_normalize_analyses(selected, has_marker_genes)
}

builder_resolve_colors <- function(settings, levels) {
  groups <- settings$included_groups %||% settings$groups %||% character()
  palette <- settings$palette %||% "cerebro"
  overrides <- builder_settings_color_overrides(settings)
  out <- list()
  for (group in groups) {
    group_levels <- levels[[group]] %||% character()
    if (length(group_levels)) {
      out[[group]] <- builder_level_colors(
        group_levels,
        palette,
        overrides[[group]]
      )
    }
  }
  out
}

.builder_plan_partition_alignments <- function(images) {
  if (
    exists("builder_partition_alignments", mode = "function", inherits = TRUE)
  ) {
    return(builder_partition_alignments(images %||% list()))
  }
  images <- images %||% list()
  is_trekker <- vapply(
    names(images),
    function(name) {
      identical(name, "trekker") ||
        identical(images[[name]]$section_kind %||% "", "trekker")
    },
    logical(1)
  )
  list(
    spatial = images[!is_trekker],
    trekker = if (any(is_trekker)) images[[which(is_trekker)[[1L]]]] else NULL
  )
}

.builder_plan_flatten_spatial_images <- function(images) {
  if (
    exists(
      "builder_image_collection_flatten",
      mode = "function",
      inherits = TRUE
    )
  ) {
    return(builder_image_collection_flatten(images %||% list()))
  }
  images <- images %||% list()
  flattened <- list()
  for (section_id in names(images)) {
    section <- images[[section_id]]
    legacy_record <- is.list(section) &&
      any(
        c("source_path", "project_asset", "source_uri", "uri") %in%
          names(section)
      )
    records <- if (legacy_record) {
      source <- section[["source", exact = TRUE]]
      stats::setNames(
        list(section),
        if (is.list(source)) source$name %||% section_id else section_id
      )
    } else {
      section
    }
    labels <- names(records)
    if (
      is.null(labels) ||
        anyNA(labels) ||
        any(!nzchar(labels)) ||
        anyDuplicated(labels)
    ) {
      stop("Spatial image labels must be unique and non-empty.", call. = FALSE)
    }
    for (image_label in names(records)) {
      flattened[[length(flattened) + 1L]] <- c(
        list(section_id = section_id, image_label = image_label),
        records[[image_label]]
      )
    }
  }
  flattened
}

.builder_plan_spatial_image_count <- function(images) {
  as.integer(length(.builder_plan_flatten_spatial_images(images)))
}

.builder_plan_spatial_point_appearance <- function(stored, images) {
  out <- if (is.list(stored) && !is.object(stored)) stored else list()
  for (record in .builder_plan_flatten_spatial_images(images)) {
    section <- record$section_id
    if (!is.null(out[[section]])) {
      next
    }
    opacity <- suppressWarnings(as.numeric(record$point_opacity))
    size <- suppressWarnings(as.numeric(record$point_size))
    if (
      length(opacity) == 1L &&
        !is.na(opacity) &&
        is.finite(opacity) &&
        opacity >= 0 &&
        opacity <= 1 &&
        length(size) == 1L &&
        !is.na(size) &&
        is.finite(size) &&
        size > 0 &&
        size <= 20
    ) {
      out[[section]] <- list(
        point_opacity = unname(opacity),
        point_size = unname(size)
      )
    }
  }
  out
}

#' Organism suggested by the gene-name guess, or NULL when it is not confident.
#'
#' Only "hg" and "mm" count: "other" means the identifiers matched neither
#' human nor mouse symbols, so the user has to choose.
builder_detected_organism <- function(profile, dataset_profile = NULL) {
  guess <- profile$organism_guess %||% dataset_profile$organism$code
  if (builder_has_text(guess) && guess %in% c("hg", "mm")) guess else NULL
}

## The same file added twice would show two identical names in the rail and in
## the generated Viewer's dataset menu; number the later copy instead.
builder_unique_dataset_name <- function(name, existing) {
  existing <- existing[!is.na(existing)]
  if (!length(name) || !name %in% existing) {
    return(name)
  }
  index <- 2L
  repeat {
    candidate <- paste0(name, " (", index, ")")
    if (!candidate %in% existing) {
      return(candidate)
    }
    index <- index + 1L
  }
}

builder_default_settings <- function(
  profile,
  name,
  recommendations = NULL,
  dataset_profile = NULL
) {
  assay <- profile$default_assay
  assay_profile <- profile$assay_profiles[[assay]] %||%
    list(
      default_layer = profile$default_layer,
      nUMI = profile$nUMI,
      nGene = profile$nGene
    )
  settings <- list(
    name = name,
    organism = builder_detected_organism(profile, dataset_profile),
    assay = assay,
    layer = assay_profile$default_layer,
    nUMI = if (builder_has_text(assay_profile$nUMI)) {
      assay_profile$nUMI
    } else {
      NULL
    },
    nGene = if (builder_has_text(assay_profile$nGene)) {
      assay_profile$nGene
    } else {
      NULL
    },
    groups = profile$group_preselect,
    reductions = profile$reduction_preselect,
    analyses = character(),
    tables = list(),
    images = list(),
    spatial_coordinate_transforms = list(),
    spatial_percentage_cells_to_show = 100,
    spatial_point_appearance = list(),
    spatial_roi_settings = list(),
    spatial_image_storage = "external",
    palette = "cerebro",
    color_overrides = list(),
    group_color_overrides = list()
  )
  if (!is.null(recommendations)) {
    settings$groups <- recommendations$groups$included %||% character()
    settings$reductions <- recommendations$projections$included %||% character()
    settings$default_group <- recommendations$groups$value
    settings$default_projection <- recommendations$projections$value
    settings$metadata_policy <- recommendations$metadata
    settings$nomenclature <- recommendations$nomenclature$value
    settings$expression_backend <- recommendations$backend$value
    settings$recommendations <- recommendations
  }
  entry <- builder_upgrade_viewer_content_entry(list(
    profile = profile,
    dataset_profile = dataset_profile,
    settings = settings
  ))
  entry$settings
}

builder_plan_error <- function(
  message,
  code = "invalid_plan",
  details = list()
) {
  structure(
    list(error = message, error_code = code, details = details),
    class = c("builder_plan_failure", "list")
  )
}

builder_has_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_plan_has_reference <- function(value, depth = 0L) {
  if (depth > 50L) {
    return(TRUE)
  }
  if (
    is.environment(value) ||
      is.function(value) ||
      isS4(value) ||
      is.language(value) ||
      is.symbol(value) ||
      typeof(value) %in% c("externalptr", "weakref") ||
      inherits(value, "connection")
  ) {
    return(TRUE)
  }
  value_attributes <- attributes(value)
  if (
    !is.null(value_attributes) &&
      any(vapply(
        value_attributes,
        .builder_plan_has_reference,
        logical(1),
        depth = depth + 1L
      ))
  ) {
    return(TRUE)
  }
  if (is.list(value) || is.pairlist(value)) {
    return(any(vapply(
      value,
      .builder_plan_has_reference,
      logical(1),
      depth = depth + 1L
    )))
  }
  FALSE
}

.builder_plan_revision <- function(revision, entries) {
  if (is.null(revision)) {
    entry_revisions <- vapply(
      entries,
      function(entry) {
        value <- entry$revision
        if (
          is.numeric(value) &&
            length(value) == 1L &&
            !is.na(value) &&
            is.finite(value) &&
            value >= 0 &&
            value <= .Machine$integer.max &&
            value == floor(value)
        ) {
          as.integer(value)
        } else {
          0L
        }
      },
      integer(1)
    )
    return(max(c(1L, entry_revisions)))
  }
  if (
    !is.numeric(revision) ||
      length(revision) != 1L ||
      is.na(revision) ||
      !is.finite(revision) ||
      revision < 1 ||
      revision > .Machine$integer.max ||
      revision != floor(revision)
  ) {
    return(NULL)
  }
  as.integer(revision)
}

.builder_plan_character_set <- function(value) {
  is.character(value) &&
    !anyNA(value) &&
    all(nzchar(trimws(value))) &&
    !anyDuplicated(value)
}

.builder_plan_entry_source_identity <- function(entry) {
  profile <- entry$dataset_profile
  if (is.null(profile) && inherits(entry$profile, "builder_dataset_profile")) {
    profile <- entry$profile
  }
  source <- profile$source %||% entry$source
  if (is.list(source)) {
    source[c("type", "location", "fingerprint", "format")]
  } else {
    NULL
  }
}
