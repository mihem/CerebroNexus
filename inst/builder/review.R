## Builder Review owns a small, immutable projection of the checked
## configuration. BuildPlan creation is deliberately kept out of this layer.

.builder_review_snapshot_fields <- c(
  "revision",
  "identity",
  "readiness",
  "error",
  "dataset_order",
  "items",
  "make_app",
  "viewer_page_expectations",
  "output_release",
  "warnings"
)

.builder_review_revision_valid <- function(value) {
  (is.integer(value) && length(value) == 1L && !is.na(value) && value >= 0L) ||
    (is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value))
}

.builder_review_identity_valid <- function(identity) {
  if (
    !is.list(identity) ||
      is.object(identity) ||
      !identical(
        names(identity),
        c("schema_version", "dataset_order", "checks")
      ) ||
      !identical(identity$schema_version, 1L) ||
      !is.character(identity$dataset_order) ||
      !length(identity$dataset_order) ||
      anyNA(identity$dataset_order) ||
      any(!nzchar(identity$dataset_order)) ||
      anyDuplicated(identity$dataset_order) ||
      !is.character(identity$checks) ||
      !identical(names(identity$checks), identity$dataset_order) ||
      anyNA(identity$checks) ||
      any(!nzchar(identity$checks))
  ) {
    return(FALSE)
  }
  TRUE
}

builder_review_configuration_identity <- function(
  entries,
  identity_cache = NULL
) {
  if (!is.list(entries) || !length(entries)) {
    stop("Review identity requires checked datasets.", call. = FALSE)
  }
  ids <- vapply(
    entries,
    function(entry) {
      value <- if (is.list(entry)) entry$id %||% "" else ""
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        ""
      } else {
        value
      }
    },
    character(1)
  )
  if (any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Review identity requires unique dataset ids.", call. = FALSE)
  }
  if (!exists("builder_project_check_identity", mode = "function")) {
    stop("Dataset check identity is unavailable.", call. = FALSE)
  }
  checks <- vapply(
    entries,
    builder_project_check_identity,
    character(1),
    identity_cache = identity_cache
  )
  names(checks) <- ids
  list(
    schema_version = 1L,
    dataset_order = ids,
    checks = checks
  )
}

builder_review_snapshot <- function(
  revision,
  identity,
  items,
  make_app = FALSE,
  warnings = character()
) {
  if (
    !.builder_review_revision_valid(revision) ||
      !.builder_review_identity_valid(identity) ||
      !is.list(items) ||
      is.object(items) ||
      !length(items) ||
      !is.logical(make_app) ||
      length(make_app) != 1L ||
      is.na(make_app) ||
      !is.character(warnings) ||
      anyNA(warnings)
  ) {
    stop("A valid lightweight review snapshot is required.", call. = FALSE)
  }
  item_ids <- vapply(
    items,
    function(item) {
      value <- if (is.list(item)) item$id %||% "" else ""
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        ""
      } else {
        value
      }
    },
    character(1)
  )
  if (!identical(item_ids, identity$dataset_order)) {
    stop(
      "Review snapshot items must follow checked dataset order.",
      call. = FALSE
    )
  }
  runtime <- unique(vapply(
    items,
    function(item) {
      as.character(item$estimated_runtime %||% "no optional analysis runtime")
    },
    character(1)
  ))
  bytes <- sum(vapply(
    items,
    function(item) {
      value <- suppressWarnings(as.double(item$estimated_disk_bytes %||% 0))
      if (
        length(value) != 1L || is.na(value) || !is.finite(value) || value < 0
      ) {
        0
      } else {
        value
      }
    },
    numeric(1)
  ))
  snapshot <- list(
    revision = revision,
    identity = identity,
    readiness = "ready",
    error = NULL,
    dataset_order = identity$dataset_order,
    items = unname(items),
    make_app = isTRUE(make_app),
    viewer_page_expectations = lapply(
      items,
      function(item) item$viewer_page_expectations %||% list()
    ),
    output_release = list(
      estimated_runtime = paste(runtime, collapse = "; "),
      estimated_disk_bytes = bytes
    ),
    warnings = unique(warnings[nzchar(trimws(warnings))])
  )
  structure(snapshot, class = c("builder_review_snapshot", "list"))
}

.builder_review_default_first <- function(values, selected = NULL) {
  values <- unique(as.character(values %||% character()))
  values <- values[!is.na(values) & nzchar(values)]
  if (
    is.character(selected) &&
      length(selected) == 1L &&
      !is.na(selected) &&
      selected %in% values
  ) {
    return(c(selected, setdiff(values, selected)))
  }
  values
}

.builder_review_item_filename <- function(entry, index, total) {
  if (exists("builder_item_filename", mode = "function")) {
    return(builder_item_filename(entry, index, total))
  }
  label <- as.character(entry$settings$name %||% entry$id)
  slug <- tolower(gsub("[^a-zA-Z0-9]+", "-", label))
  slug <- gsub("(^-+|-+$)", "", slug)
  if (!nzchar(slug)) {
    slug <- paste0("dataset-", index)
  }
  if (nchar(slug, type = "chars") > 24L) {
    digest <- substr(
      as.character(digest::digest(
        charToRaw(enc2utf8(label)),
        algo = "md5",
        serialize = FALSE
      )),
      1L,
      8L
    )
    slug <- paste0(substr(slug, 1L, 15L), "-", digest)
  }
  paste0(if (total > 1L) sprintf("%02d-", index) else "", slug, ".crb")
}

.builder_review_backend <- function(settings, filename) {
  if (exists(".builder_plan_backend", mode = "function")) {
    backend <- .builder_plan_backend(settings, filename)
    if (is.list(backend) && isTRUE(backend$valid)) {
      return(list(mode = backend$mode, sidecars = backend$sidecars))
    }
  }
  list(
    mode = as.character(settings$expression_backend %||% "embedded"),
    sidecars = character()
  )
}

.builder_review_spatial_alignment <- function(saved, settings) {
  alignment <- saved$spatial_alignment %||% NULL
  if (
    is.list(alignment) &&
      !is.null(alignment$section_count)
  ) {
    return(alignment)
  }
  sections <- unique(as.character(
    saved$artifact_identity$spatial_sections %||% character()
  ))
  sections <- sections[!is.na(sections) & nzchar(sections)]
  if (!length(sections)) {
    return(NULL)
  }
  images <- settings$images %||% list()
  image_sections <- intersect(names(images) %||% character(), sections)
  image_count <- sum(vapply(
    images[image_sections],
    function(records) if (is.list(records)) length(records) else 0L,
    integer(1)
  ))
  list(
    section_count = as.integer(length(sections)),
    image_count = as.integer(image_count),
    points_only = setdiff(sections, image_sections)
  )
}

.builder_review_item_from_entry <- function(entry, state, index, total) {
  saved <- if (identical(entry$load_state %||% "loaded", "artifact_ready")) {
    entry$project_artifact$plan_item %||% list()
  } else {
    list()
  }
  settings <- entry$settings %||% list()
  profile <- entry$profile %||% list()
  included_groups <- .builder_review_default_first(
    settings$included_groups %||%
      settings$recommendations$groups$included %||%
      settings$groups,
    settings$default_group
  )
  included_projections <- .builder_review_default_first(
    settings$included_projections %||%
      settings$recommendations$projections$included %||%
      settings$reductions,
    settings$default_projection
  )
  if (!length(included_groups)) {
    included_groups <- saved$included_groups %||% character()
  }
  if (!length(included_projections)) {
    included_projections <- saved$included_projections %||% character()
  }
  initial_projections <- .builder_review_default_first(
    settings$initial_projections %||%
      saved$initial_projections %||%
      settings$default_projection %||%
      saved$default_projection %||%
      character(),
    settings$default_projection %||% saved$default_projection
  )
  included_trajectories <- settings$included_trajectories %||%
    saved$included_trajectories %||%
    list()
  if (!is.list(included_trajectories)) {
    included_trajectories <- list()
  }
  filename <- saved$filename %||%
    .builder_review_item_filename(entry, index, total)
  backend <- if (length(saved)) {
    list(
      mode = saved$expression_backend %||% "embedded",
      sidecars = saved$sidecars %||% character()
    )
  } else {
    .builder_review_backend(settings, filename)
  }
  colors <- if (length(saved$colors %||% list())) {
    saved$colors
  } else if (exists("builder_resolve_colors", mode = "function")) {
    builder_resolve_colors(settings, entry$levels %||% list())
  } else {
    list()
  }
  color_overrides <- if (
    exists("builder_settings_color_overrides", mode = "function")
  ) {
    builder_settings_color_overrides(settings)
  } else {
    settings$color_overrides %||% list()
  }
  selected_overrides <- color_overrides[intersect(
    names(color_overrides) %||% character(),
    included_groups
  )]
  runtime_costs <- c(
    percent_mt_ribo = "seconds",
    most_expressed = "seconds",
    marker_genes = "minutes",
    enriched_pathways = "network-dependent"
  )
  analyses <- state$analyses %||% settings$analyses %||% character()
  runtime <- unname(runtime_costs[intersect(analyses, names(runtime_costs))])
  item <- list(
    id = entry$id,
    name = as.character(settings$name %||% saved$name %||% entry$id),
    filename = filename,
    organism = settings$organism %||% saved$organism %||% "",
    groups = settings$groups %||% saved$groups %||% character(),
    included_groups = included_groups,
    cell_cycle = settings$cell_cycle_columns %||%
      saved$cell_cycle %||%
      character(),
    reductions = settings$reductions %||% saved$reductions %||% character(),
    included_projections = included_projections,
    initial_projections = initial_projections,
    included_trajectories = included_trajectories,
    analyses = analyses,
    artifact_identity = list(
      group_levels = saved$artifact_identity$group_levels %||%
        entry$levels %||%
        list()
    ),
    cell_count = as.integer(
      profile$n_cells %||% saved$cell_count %||% 0L
    ),
    gene_count = as.integer(
      profile$n_genes %||% saved$gene_count %||% 0L
    ),
    spatial_alignment = .builder_review_spatial_alignment(saved, settings),
    estimated_runtime = if (length(runtime)) {
      paste(unique(runtime), collapse = ", ")
    } else {
      saved$estimated_runtime %||% "no optional analysis runtime"
    },
    estimated_disk_bytes = as.double(
      entry$snapshot$closure_bytes %||%
        saved$estimated_disk_bytes %||%
        0
    ),
    colors = colors,
    group_color_overrides = selected_overrides,
    color_custom_count = as.integer(sum(vapply(
      selected_overrides,
      length,
      integer(1)
    ))),
    default_group = settings$default_group %||%
      saved$default_group %||%
      settings$groups[[1L]],
    default_projection = settings$default_projection %||%
      saved$default_projection %||%
      settings$reductions[[1L]],
    default_trajectory = settings$default_trajectory %||%
      saved$default_trajectory %||%
      NULL,
    viewer_display = builderViewerDisplayDefaults(settings$viewer_display),
    overview_point_size = settings$overview_point_size %||%
      saved$overview_point_size %||%
      5,
    overview_point_opacity = settings$overview_point_opacity %||%
      saved$overview_point_opacity %||%
      1,
    metadata_policy = state$metadata_policy %||%
      saved$metadata_policy %||%
      list(),
    expression_backend = backend$mode,
    sidecars = backend$sidecars,
    spatial_image_storage = "external",
    manifest = state$manifest %||% saved$manifest %||% list(),
    viewer_page_expectations = state$page_expectations %||%
      saved$viewer_page_expectations %||%
      list(),
    acknowledgements = state$acknowledgements %||%
      saved$acknowledgements %||%
      character(),
    readiness = "ready"
  )
  item
}

builder_review_snapshot_from_entries <- function(
  entries,
  states,
  identity,
  make_app = FALSE,
  warnings = character()
) {
  if (
    !is.list(entries) ||
      !is.list(states) ||
      !length(entries) ||
      length(entries) != length(states)
  ) {
    stop("Review snapshot requires loaded dataset facts.", call. = FALSE)
  }
  ready <- vapply(
    states,
    function(state) {
      is.list(state) &&
        (identical(state$readiness, "ready") ||
          identical(state$readiness, "artifact_ready"))
    },
    logical(1)
  )
  if (!all(ready)) {
    stop("Every reviewed dataset must be ready.", call. = FALSE)
  }
  revisions <- vapply(
    entries,
    function(entry) {
      as.integer(entry$revision %||% 0L)
    },
    integer(1)
  )
  revision <- paste(revisions, collapse = ".")
  items <- Map(
    function(entry, state, index) {
      .builder_review_item_from_entry(entry, state, index, length(entries))
    },
    entries,
    states,
    seq_along(entries)
  )
  builder_review_snapshot(
    revision = revision,
    identity = identity,
    items = items,
    make_app = make_app,
    warnings = warnings
  )
}

builder_review_snapshot_valid <- function(snapshot) {
  if (
    !inherits(snapshot, "builder_review_snapshot") ||
      !is.list(snapshot) ||
      !identical(names(snapshot), .builder_review_snapshot_fields) ||
      !.builder_review_revision_valid(snapshot$revision) ||
      !.builder_review_identity_valid(snapshot$identity) ||
      !identical(snapshot$dataset_order, snapshot$identity$dataset_order) ||
      !identical(snapshot$readiness, "ready") ||
      !is.null(snapshot$error) ||
      !is.list(snapshot$items) ||
      length(snapshot$items) != length(snapshot$dataset_order)
  ) {
    return(FALSE)
  }
  item_ids <- vapply(
    snapshot$items,
    function(item) {
      if (is.list(item)) as.character(item$id %||% "") else ""
    },
    character(1)
  )
  identical(item_ids, snapshot$dataset_order)
}

builder_review_snapshot_identity <- function(snapshot) {
  if (!builder_review_snapshot_valid(snapshot)) {
    stop("A valid lightweight review snapshot is required.", call. = FALSE)
  }
  snapshot$identity
}
