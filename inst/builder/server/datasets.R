## Builder server: datasets.

## -- the rail ------------------------------------------------------------
last_dataset_rail_patch <- reactiveVal(NULL)
dataset_rail_row_cache <- new.env(parent = emptyenv())
observe({
  next_patch <- builder_dataset_rail_patch(
    store(),
    current(),
    checked_dataset_ids(),
    dataset_checkpoint_states(),
    row_cache = dataset_rail_row_cache,
    state_cache = builder_dataset_state_cache
  )
  if (identical(next_patch, isolate(last_dataset_rail_patch()))) {
    return()
  }
  last_dataset_rail_patch(next_patch)
  session$sendCustomMessage("builder_dataset_rail_patch", next_patch)
})

observeEvent(
  input$builder_dataset_rail_sync,
  {
    session$sendCustomMessage(
      "builder_dataset_rail_patch",
      builder_dataset_rail_patch(
        store(),
        current(),
        checked_dataset_ids(),
        dataset_checkpoint_states(),
        row_cache = dataset_rail_row_cache,
        force_html = TRUE,
        state_cache = builder_dataset_state_cache
      )
    )
  },
  ignoreInit = TRUE
)

last_import_rail_patch <- reactiveVal(NULL)
observe({
  next_patch <- builder_import_rail_patch(
    imports()$entries,
    active_import_id(),
    length(store()$datasets %||% list())
  )
  if (identical(next_patch, isolate(last_import_rail_patch()))) {
    return()
  }
  last_import_rail_patch(next_patch)
  session$sendCustomMessage("builder_import_rail_patch", next_patch)
})

observeEvent(
  input$builder_import_rail_sync,
  {
    session$sendCustomMessage(
      "builder_import_rail_patch",
      builder_import_rail_patch(
        imports()$entries,
        active_import_id(),
        length(store()$datasets %||% list())
      )
    )
  },
  ignoreInit = TRUE
)

## -- keep the current entry's settings in step with Core -----------------
core_setting_inputs <- c(
  name = "core-name",
  organism = "core-organism",
  assay = "core-assay",
  layer = "core-layer",
  nUMI = "core-nUMI",
  nGene = "core-nGene",
  expression_backend = "core-backend"
)
observeEvent(
  input[["core-assay"]],
  {
    id <- current()
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id)
    ) {
      return()
    }
    e <- isolate(entry_of(id))
    req(e)
    assay <- builder_core_resolve_assay(
      e$profile,
      input[["core-assay"]],
      fallback = e$settings$assay
    )
    if (is.null(assay)) {
      return()
    }
    if (!identical(assay, input[["core-assay"]])) {
      freezeReactiveValue(input, "core-assay")
      updateSelectInput(
        session,
        "core-assay",
        choices = builder_core_available_assays(e$profile),
        selected = assay
      )
      return()
    }
    controls <- builder_core_assay_controls(
      e$profile,
      e$settings,
      assay
    )
    if (is.null(controls)) {
      return()
    }
    for (field in names(controls)) {
      freezeReactiveValue(input, paste0("core-", field))
      updateSelectInput(
        session,
        paste0("core-", field),
        choices = controls[[field]]$choices,
        selected = controls[[field]]$selected
      )
    }
  },
  ignoreInit = TRUE
)
observe({
  id <- current()
  rendered_for <- input[["core-rendered_for"]]
  if (is.null(id) || !identical(rendered_for, id)) {
    return()
  }
  values <- lapply(core_setting_inputs, function(input_id) input[[input_id]])
  if (any(vapply(values, is.null, logical(1)))) {
    return()
  }
  entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
  req(entry)
  next_settings <- entry$settings
  profile <- entry$profile %||% list()
  stored_assay <- entry$settings$assay
  stored_layer <- entry$settings$layer
  stored_layers <- profile$assay_profiles[[stored_assay]]$layers %||%
    character()
  layer_missing <- builder_stage_has_text(stored_layer) &&
    !stored_layer %in% stored_layers
  for (setting in names(core_setting_inputs)) {
    next_settings[[setting]] <- values[[setting]]
  }
  resolved_assay <- builder_core_resolve_assay(
    entry$profile,
    next_settings$assay,
    fallback = stored_assay
  )
  if (is.null(resolved_assay)) {
    return()
  }
  if (!identical(resolved_assay, next_settings$assay)) {
    next_settings$assay <- resolved_assay
    freezeReactiveValue(input, "core-assay")
    updateSelectInput(
      session,
      "core-assay",
      choices = builder_core_available_assays(entry$profile),
      selected = resolved_assay
    )
  }
  assay_controls <- builder_core_assay_controls(
    entry$profile,
    next_settings,
    resolved_assay
  )
  if (is.null(assay_controls)) {
    return()
  }
  for (field in names(assay_controls)) {
    next_settings[[field]] <- assay_controls[[field]]$selected
  }
  if (
    layer_missing &&
      identical(values$assay, stored_assay) &&
      identical(values$layer, stored_layer)
  ) {
    next_settings$layer <- stored_layer
  }
  if (!next_settings$organism %in% c("hg", "mm")) {
    next_settings$analyses <- setdiff(
      next_settings$analyses %||% character(),
      "percent_mt_ribo"
    )
  }
  entry$settings <- next_settings
  replace_entry(entry)
})

observeEvent(
  input[["core-acknowledge_content"]],
  {
    request <- input[["core-acknowledge_content"]]
    scalar_text <- function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    }
    if (
      !is.list(request) ||
        !scalar_text(request$dataset) ||
        !scalar_text(request$capability) ||
        !scalar_text(request$token) ||
        !identical(request$dataset, isolate(current())) ||
        !identical(input[["core-rendered_for"]], request$dataset)
    ) {
      return()
    }
    entry <- isolate(entry_of(request$dataset))
    if (
      !builder_content_action_owner_is_current(
        request,
        entry,
        isolate(builder_project_open_generation())
      )
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(entry)
    state <- try(builder_dataset_state(entry), silent = TRUE)
    if (inherits(state, "try-error")) {
      return()
    }
    record <- state$manifest[[request$capability]] %||% list()
    action <- record$required_action %||% list()
    if (
      !identical(record$status %||% "", "attention") ||
        !identical(action$type %||% "", "acknowledge") ||
        !identical(action$token %||% "", request$token)
    ) {
      return()
    }
    entry$settings$acknowledgements <- unique(c(
      entry$settings$acknowledgements %||% character(),
      request$token
    ))
    if (isTRUE(replace_entry(entry))) {
      configure_workbench_surface(NULL)
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["core-content_disposition"]],
  {
    request <- input[["core-content_disposition"]]
    scalar_text <- function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    }
    allowed_capabilities <- c(
      .builder_profile_content_ids(),
      "hla_tcr_motifs"
    )
    if (
      !is.list(request) ||
        !scalar_text(request$dataset) ||
        !scalar_text(request$capability) ||
        !request$capability %in% allowed_capabilities ||
        !scalar_text(request$disposition) ||
        !request$disposition %in% c("filtered", "auto") ||
        !identical(request$dataset, isolate(current())) ||
        !identical(input[["core-rendered_for"]], request$dataset)
    ) {
      return()
    }
    entry <- isolate(entry_of(request$dataset))
    if (
      !builder_content_action_owner_is_current(
        request,
        entry,
        isolate(builder_project_open_generation())
      )
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(entry)
    dispositions <- entry$settings$content_dispositions %||% list()
    if (!is.list(dispositions) || is.object(dispositions)) {
      dispositions <- list()
    }
    state <- try(builder_dataset_state(entry), silent = TRUE)
    record <- if (inherits(state, "try-error")) {
      list()
    } else {
      state$manifest[[request$capability]] %||% list()
    }
    if (
      identical(request$disposition, "filtered") &&
        (!is.list(record) || !isTRUE(record$evidence$detected))
    ) {
      return()
    }
    if (identical(request$disposition, "auto")) {
      dispositions[[request$capability]] <- NULL
    } else {
      dispositions[[request$capability]] <- request$disposition
    }
    entry$settings$content_dispositions <- dispositions
    if (isTRUE(replace_entry(entry))) {
      configure_workbench_surface(NULL)
    }
  },
  ignoreInit = TRUE
)

group_catalog_for_entry <- function(entry) {
  builder_group_catalog_model(list(
    metadata_catalog = entry$dataset_profile$viewer_content$metadata %||%
      entry$profile$viewer_content$metadata %||%
      list(),
    group_choices = unname(entry$profile$group_candidates %||% character()),
    included_groups = entry$settings$included_groups %||%
      entry$settings$groups %||%
      character(),
    default_group = entry$settings$default_group %||% NULL,
    suggested_groups = entry$profile$group_preselect %||%
      entry$settings$included_groups %||%
      character(),
    metadata_policy = entry$settings$metadata_policy %||%
      entry$settings$recommendations$metadata %||%
      list(),
    levels = entry$levels %||% list()
  ))
}

send_group_state <- function(entry, message = NULL) {
  current_entry <- isolate(entry_of(entry$id)) %||% entry
  session$sendCustomMessage(
    "builder_group_state",
    list(
      dataset = entry$id,
      generation = as.integer(
        current_entry$revision %||% entry$revision %||% 0L
      ),
      included = unname(entry$settings$included_groups %||% character()),
      default = entry$settings$default_group %||% NULL,
      message = message
    )
  )
}

group_focus_value <- function(value) {
  if (is.list(value)) value$group %||% NULL else value
}

projection_catalog_for_entry <- function(entry) {
  catalog <- entry$dataset_profile$viewer_content$projections %||%
    entry$profile$viewer_content$projections %||%
    list()
  if (is.list(catalog) && length(catalog)) {
    return(catalog)
  }
  ids <- unname(as.character(entry$profile$reductions %||% character()))
  fallback <- lapply(ids, function(id) {
    list(
      id = id,
      name = id,
      kind = if (grepl("pca", id, ignore.case = TRUE)) "pca" else "other",
      dimensions = 2L,
      cell_count = entry$profile$n_cells %||% 0L,
      available = TRUE
    )
  })
  names(fallback) <- ids
  fallback
}

trajectory_catalog_for_entry <- function(entry) {
  entry$dataset_profile$viewer_content$trajectories %||%
    entry$profile$viewer_content$trajectories %||%
    list()
}

selectable_trajectory_selection <- function(catalog) {
  selected <- list()
  for (record in catalog %||% list()) {
    if (
      !is.list(record) ||
        !isTRUE(record$selectable) ||
        !builder_stage_has_text(record$method %||% "") ||
        !builder_stage_has_text(record$name %||% "")
    ) {
      next
    }
    selected[[record$method]] <- unique(c(
      selected[[record$method]],
      record$name
    ))
  }
  selected
}

send_projection_state <- function(entry, message = NULL) {
  current_entry <- isolate(entry_of(entry$id)) %||% entry
  session$sendCustomMessage(
    "builder_projection_state",
    list(
      dataset = entry$id,
      generation = as.integer(
        current_entry$revision %||% entry$revision %||% 0L
      ),
      included = unname(
        entry$settings$included_projections %||% character()
      ),
      default = entry$settings$default_projection %||% NULL,
      initial = unname(
        entry$settings$initial_projections %||%
          entry$settings$default_projection %||%
          character()
      ),
      point_size = entry$settings$overview_point_size %||% 5,
      point_opacity = entry$settings$overview_point_opacity %||% 1,
      percentage_cells_to_show = entry$settings[[
        "overview_percentage_cells_to_show"
      ]] %||%
        builder_default_percentage_cells_to_show(entry$profile$n_cells),
      message = message
    )
  )
}

send_trajectory_state <- function(entry, message = NULL) {
  current_entry <- isolate(entry_of(entry$id)) %||% entry
  included <- list()
  for (method in names(entry$settings$included_trajectories %||% list())) {
    for (name in entry$settings$included_trajectories[[method]]) {
      included[[length(included) + 1L]] <- list(
        method = method,
        name = name
      )
    }
  }
  session$sendCustomMessage(
    "builder_trajectory_state",
    list(
      dataset = entry$id,
      generation = as.integer(
        current_entry$revision %||% entry$revision %||% 0L
      ),
      included = included,
      default = entry$settings$default_trajectory %||% NULL,
      message = message
    )
  )
}

lapply(
  c("immune_repertoire", "hla_tcr_motifs"),
  function(capability) {
    observeEvent(
      input[[paste0("core-immune_source_", capability)]],
      {
        id <- current()
        value <- input[[paste0("core-immune_source_", capability)]]
        if (
          is.null(id) ||
            !identical(input[["core-rendered_for"]], id) ||
            !builder_stage_has_text(value %||% "")
        ) {
          return()
        }
        entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
        req(entry)
        fact <- entry$dataset_profile$content$immune_repertoire %||%
          entry$profile$content$immune_repertoire %||%
          list()
        state <- try(
          builder_cached_dataset_state(entry, builder_dataset_state_cache),
          silent = TRUE
        )
        if (inherits(state, "try-error")) {
          return()
        }
        selectors <- builder_immune_source_choices_model(
          list(
            immune_source_fact = fact,
            content_sources = entry$settings$content_sources %||% list()
          ),
          state$manifest %||% list()
        )
        matching <- Filter(
          function(selector) identical(selector$capability, capability),
          selectors
        )
        if (length(matching) != 1L) {
          return()
        }
        updated <- builder_apply_immune_source_choice(
          entry,
          matching[[1L]],
          value
        )
        if (!identical(updated$settings, entry$settings)) {
          replace_entry(updated)
        }
      },
      ignoreInit = TRUE
    )
  }
)

observeEvent(
  input[["core-projection_action"]],
  {
    id <- current()
    action <- input[["core-projection_action"]]
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        !is.list(action) ||
        !identical(action$action, "set")
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    catalog <- projection_catalog_for_entry(entry)
    available <- names(catalog)[vapply(
      catalog,
      function(item) is.list(item) && isTRUE(item$available),
      logical(1)
    )]
    included <- unique(as.character(unlist(
      action$included %||% character(),
      use.names = FALSE
    )))
    included <- available[available %in% included]
    if (!length(included)) {
      send_projection_state(
        entry,
        "Keep at least one projection selected."
      )
      return()
    }
    default <- action$default
    if (!builder_stage_has_text(default %||% "") || !default %in% included) {
      default <- included[[1L]]
    }
    initial <- unique(as.character(unlist(
      action$initial %||% character(),
      use.names = FALSE
    )))
    initial <- included[included %in% initial]
    if (!length(initial)) {
      initial <- default
    }
    if (!default %in% initial) {
      initial <- c(default, initial)
    }
    entry$settings$included_projections <- included
    entry$settings$reductions <- included
    entry$settings$default_projection <- default
    entry$settings$initial_projections <- initial
    if (isTRUE(replace_entry(entry))) {
      send_projection_state(entry)
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["core-point_size"]],
  {
    id <- current()
    value <- suppressWarnings(as.numeric(input[["core-point_size"]]))
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 1 ||
        value > 20
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    entry$settings$overview_point_size <- value
    if (isTRUE(replace_entry(entry))) {
      send_projection_state(entry)
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["core-point_opacity"]],
  {
    id <- current()
    value <- suppressWarnings(as.numeric(input[["core-point_opacity"]]))
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 0.1 ||
        value > 1
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    entry$settings$overview_point_opacity <- value
    if (isTRUE(replace_entry(entry))) {
      send_projection_state(entry)
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["core-percentage_cells_to_show"]],
  {
    id <- current()
    value <- suppressWarnings(as.numeric(
      input[["core-percentage_cells_to_show"]]
    ))
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 10 ||
        value > 100
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    entry$settings$overview_percentage_cells_to_show <- value
    if (isTRUE(replace_entry(entry))) {
      send_projection_state(entry)
    }
  },
  ignoreInit = TRUE
)

parse_trajectory_action <- function(value, selectable) {
  records <- value %||% list()
  if (!is.list(records)) {
    return(list())
  }
  selected <- list()
  for (record in records) {
    if (
      !is.list(record) ||
        !builder_stage_has_text(record$method %||% "") ||
        !builder_stage_has_text(record$name %||% "") ||
        !record$method %in% names(selectable) ||
        !record$name %in% selectable[[record$method]]
    ) {
      next
    }
    selected[[record$method]] <- unique(c(
      selected[[record$method]],
      record$name
    ))
  }
  selected
}

observeEvent(
  input[["core-trajectory_action"]],
  {
    id <- current()
    action <- input[["core-trajectory_action"]]
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        !is.list(action) ||
        !identical(action$action, "set")
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    selectable <- selectable_trajectory_selection(
      trajectory_catalog_for_entry(entry)
    )
    included <- parse_trajectory_action(action$included, selectable)
    default <- action$default
    if (
      !is.list(default) ||
        !builder_stage_has_text(default$method %||% "") ||
        !builder_stage_has_text(default$name %||% "") ||
        !default$method %in% names(included) ||
        !default$name %in% included[[default$method]]
    ) {
      default <- .builder_state_first_trajectory(included)
    } else {
      default <- list(method = default$method, name = default$name)
    }
    entry$settings$included_trajectories <- included
    entry$settings["default_trajectory"] <- list(default)
    if (isTRUE(replace_entry(entry))) {
      send_trajectory_state(entry)
    }
  },
  ignoreInit = TRUE
)

observe({
  req(isTRUE(worker_available()))
  id <- current()
  req(id)
  entry <- builder_upgrade_viewer_content_entry(entry_of(id))
  req(entry)
  req(!identical(entry$load_state %||% "loaded", "artifact_ready"))
  catalog <- projection_catalog_for_entry(entry)
  ids <- names(catalog)[vapply(
    catalog,
    function(item) is.list(item) && isTRUE(item$available),
    logical(1)
  )]
  contract <- builder_projection_preview_contract(entry, ids)
  cache <- isolate(projection_previews())
  if (builder_preview_cache_hit(cache, id, contract)) {
    return()
  }
  if (!length(ids)) {
    projection_previews(builder_preview_cache_store(
      builder_preview_cache_begin(cache, id, contract),
      id,
      list()
    ))
    return()
  }
  queued <- enqueue(list(
    kind = "projection_previews",
    id = id,
    dataset_revision = entry$revision,
    projections = ids,
    group = entry$settings$default_group %||% NULL,
    max_cells = BUILDER_PREVIEW_MAX,
    replaces = "viewer-projection-gallery"
  ))
  if (isTRUE(queued)) {
    projection_previews(builder_preview_cache_begin(cache, id, contract))
  }
})

observe({
  req(isTRUE(worker_available()))
  id <- current()
  req(id)
  entry <- builder_upgrade_viewer_content_entry(entry_of(id))
  req(entry)
  req(!identical(entry$load_state %||% "loaded", "artifact_ready"))
  trajectories <- selectable_trajectory_selection(
    trajectory_catalog_for_entry(entry)
  )
  contract <- builder_trajectory_preview_contract(entry, trajectories)
  cache <- isolate(trajectory_previews())
  if (builder_preview_cache_hit(cache, id, contract)) {
    return()
  }
  if (!length(trajectories)) {
    trajectory_previews(builder_preview_cache_store(
      builder_preview_cache_begin(cache, id, contract),
      id,
      list()
    ))
    return()
  }
  queued <- enqueue(list(
    kind = "trajectory_previews",
    id = id,
    dataset_revision = entry$revision,
    trajectories = trajectories,
    max_cells = BUILDER_PREVIEW_MAX,
    replaces = "viewer-trajectory-gallery"
  ))
  if (isTRUE(queued)) {
    trajectory_previews(builder_preview_cache_begin(cache, id, contract))
  }
})

observeEvent(
  input[["core-metadata_action"]],
  {
    id <- current()
    action <- input[["core-metadata_action"]]
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        !is.list(action) ||
        !identical(action$action, "set-retention")
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    policy <- entry$settings$metadata_policy %||%
      entry$settings$recommendations$metadata
    req(is.list(policy))
    retained <- unique(as.character(unlist(
      action$retained %||% character(),
      use.names = FALSE
    )))
    records <- policy$columns %||% list()
    source_columns <- entry$dataset_profile$metadata$columns %||% list()
    supported <- names(records)[vapply(
      names(records),
      function(column_id) {
        record <- records[[column_id]]
        source <- source_columns[[column_id]]
        identical(column_id, "cell_barcode") ||
          (is.list(source) && isTRUE(source$supported)) ||
          isTRUE(record$forced)
      },
      logical(1)
    )]
    retained <- intersect(retained, supported)
    entry$settings$metadata_policy <- builder_metadata_policy_set_retained(
      policy,
      retained
    )
    changed <- replace_entry(entry)
    if (isTRUE(changed)) {
      group_preview_ui_revision(
        isolate(group_preview_ui_revision()) + 1L
      )
      send_group_state(entry)
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["core-group_action"]],
  {
    id <- current()
    action <- input[["core-group_action"]]
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        !is.list(action) ||
        !action$action %in% c("set", "set-groups")
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    catalog <- group_catalog_for_entry(entry)
    eligible <- vapply(
      Filter(function(item) isTRUE(item$eligible), catalog$items),
      `[[`,
      character(1),
      "id"
    )
    included <- unique(as.character(unlist(
      action$included %||% character(),
      use.names = FALSE
    )))
    included <- eligible[eligible %in% included]
    if (!length(included)) {
      send_group_state(
        entry,
        "Keep at least one Viewer Group selected."
      )
      return()
    }
    default <- action$default
    if (!builder_stage_has_text(default %||% "") || !default %in% included) {
      default <- included[[1L]]
    }
    current_included <- entry$settings$included_groups %||% character()
    removed <- setdiff(current_included, included)
    next_overrides <- builder_settings_color_overrides(entry$settings)
    for (group in removed) {
      next_overrides[[group]] <- NULL
    }
    entry$settings$included_groups <- included
    entry$settings$groups <- included
    entry$settings$default_group <- default
    entry$settings$group_color_overrides <- next_overrides
    policy <- entry$settings$metadata_policy %||%
      entry$settings$recommendations$metadata
    if (is.list(policy)) {
      entry$settings$metadata_policy <- builder_metadata_policy_set_groups(
        policy,
        included
      )
    }
    changed <- replace_entry(entry)
    if (isTRUE(changed)) {
      group_preview_ui_revision(
        isolate(group_preview_ui_revision()) + 1L
      )
      send_group_state(entry)
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["core-cell_cycle"]],
  {
    id <- current()
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id)
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    metadata <- entry$dataset_profile$viewer_content$metadata %||%
      entry$profile$viewer_content$metadata %||%
      list()
    available <- builder_cell_cycle_candidate_ids(metadata)
    selected <- unname(as.character(
      input[["core-cell_cycle"]] %||% character()
    ))
    selected <- available[available %in% selected]
    if (
      identical(selected, entry$settings$cell_cycle_columns %||% character())
    ) {
      return()
    }
    entry$settings$cell_cycle_columns <- selected
    replace_entry(entry)
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE
)

group_color_ui_revision <- reactiveVal(0L)
group_preview_ui_revision <- reactiveVal(0L)

output[["core-group_detail"]] <- renderUI({
  group_preview_ui_revision()
  id <- current()
  rendered_for <- input[["core-rendered_for"]]
  if (is.null(id) || !identical(rendered_for, id)) {
    return(NULL)
  }
  entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
  req(entry)
  catalog <- group_catalog_for_entry(entry)
  focus <- group_focus_value(input[["core-group_focus"]]) %||%
    entry$settings$default_group %||%
    catalog$focus
  builder_group_detail_ui(
    "core",
    builder_group_detail_model(catalog, focus)
  )
})

output[["core-metadata_preview"]] <- renderUI({
  group_preview_ui_revision()
  id <- current()
  req(!is.null(id), identical(input[["core-rendered_for"]], id))
  entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
  req(entry)
  catalog <- group_catalog_for_entry(entry)
  builder_metadata_preview_ui(catalog$metadata_preview)
})

output[["core-group_colors"]] <- renderUI({
  group_color_ui_revision()
  id <- current()
  rendered_for <- input[["core-rendered_for"]]
  if (is.null(id) || !identical(rendered_for, id)) {
    return(NULL)
  }
  entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
  req(entry)
  catalog <- group_catalog_for_entry(entry)
  group <- group_focus_value(input[["core-group_focus"]]) %||%
    entry$settings$default_group %||%
    catalog$focus %||%
    ""
  if (!group %in% (entry$settings$included_groups %||% character())) {
    return(NULL)
  }
  levels <- entry$levels[[group]] %||% character()
  model <- builder_group_colors_model(
    group,
    levels,
    entry$settings$palette %||% "cerebro",
    builder_settings_color_overrides(entry$settings),
    builder_group_detail_model(catalog, group)$item$distribution %||% list()
  )
  builder_group_colors_ui("core", model)
})

output[["core-projection_gallery"]] <- renderUI({
  id <- current()
  rendered_for <- input[["core-rendered_for"]]
  if (is.null(id) || !identical(rendered_for, id)) {
    return(NULL)
  }
  entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
  req(entry)
  frames <- builder_preview_cache_frames(projection_previews(), id)
  group <- entry$settings$default_group %||% ""
  levels <- entry$levels[[group]] %||% character()
  colors <- if (length(levels)) {
    builder_level_colors(
      levels,
      entry$settings$palette %||% "cerebro",
      builder_settings_color_overrides(entry$settings)[[group]] %||%
        character()
    )
  } else {
    character()
  }
  model <- builder_projection_catalog_model(list(
    projection_catalog = projection_catalog_for_entry(entry),
    included_projections = entry$settings$included_projections,
    default_projection = entry$settings$default_projection,
    initial_projections = entry$settings$initial_projections,
    overview_point_size = entry$settings$overview_point_size,
    overview_point_opacity = entry$settings$overview_point_opacity,
    overview_percentage_cells_to_show = entry$settings[[
      "overview_percentage_cells_to_show"
    ]],
    projection_previews = frames,
    preview_colors = colors,
    preview_group = group
  ))
  builder_projection_catalog_ui("core", model)
})

output[["core-viewer_display"]] <- renderUI({
  id <- current()
  req(id, identical(input[["core-rendered_for"]], id))
  entry <- isolate(entry_of(id))
  req(entry)
  builder_viewer_display_ui("core", entry, builder_project_open_generation())
})

observeEvent(
  input[["core-viewer_display_action"]],
  {
    action <- input[["core-viewer_display_action"]]
    if (
      !is.list(action) ||
        !identical(action$id, current()) ||
        !is.numeric(action$epoch) ||
        length(action$epoch) != 1L ||
        !is.finite(action$epoch) ||
        !identical(
          as.double(action$epoch),
          as.double(builder_project_open_generation())
        ) ||
        !builder_operation_allowed("edit_dataset", notify = FALSE)
    ) {
      return()
    }
    display <- tryCatch(
      builderViewerDisplayDefaults(action$values),
      error = identity
    )
    if (inherits(display, "condition")) {
      builder_notify(
        "error",
        conditionMessage(display),
        target = list(selector = ".builder-viewer-display")
      )
      return()
    }
    entry <- isolate(entry_of(action$id))
    req(entry)
    entry$settings$viewer_display <- display
    replace_entry(entry)
  },
  ignoreInit = TRUE,
  # A field blur and Save/Review click can arrive in the same Shiny flush.
  # Commit the author's edit before those actions lock or freeze the settings.
  priority = 1000
)

output[["core-trajectory_gallery"]] <- renderUI({
  id <- current()
  rendered_for <- input[["core-rendered_for"]]
  if (is.null(id) || !identical(rendered_for, id)) {
    return(NULL)
  }
  entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
  req(entry)
  frames <- builder_preview_cache_frames(trajectory_previews(), id)
  model <- builder_trajectory_catalog_model(list(
    trajectory_catalog = trajectory_catalog_for_entry(entry),
    included_trajectories = entry$settings$included_trajectories,
    default_trajectory = entry$settings$default_trajectory,
    trajectory_previews = frames
  ))
  builder_trajectory_catalog_ui("core", model)
})

observeEvent(
  input[["core-group_color"]],
  {
    id <- current()
    change <- input[["core-group_color"]]
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        !is.list(change) ||
        !builder_stage_has_text(change$group %||% "") ||
        !builder_stage_has_text(change$level %||% "")
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    group <- as.character(change$group)
    level <- as.character(change$level)
    if (
      !group %in% (entry$settings$included_groups %||% character()) ||
        !level %in% (entry$levels[[group]] %||% character())
    ) {
      return()
    }
    current_overrides <- builder_settings_color_overrides(entry$settings)
    next_overrides <- builder_update_color_override(
      current_overrides,
      group,
      level,
      change$color
    )
    if (identical(next_overrides, current_overrides)) {
      return()
    }
    entry$settings$group_color_overrides <- next_overrides
    entry$settings$colors <- NULL
    replace_entry(entry)
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["core-reset_colors"]],
  {
    id <- current()
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id)
    ) {
      return()
    }
    entry <- builder_upgrade_viewer_content_entry(isolate(entry_of(id)))
    req(entry)
    group <- group_focus_value(input[["core-group_focus"]]) %||%
      entry$settings$default_group %||%
      ""
    if (!group %in% (entry$settings$included_groups %||% character())) {
      return()
    }
    current_overrides <- builder_settings_color_overrides(entry$settings)
    next_overrides <- builder_reset_color_overrides(current_overrides, group)
    if (identical(next_overrides, current_overrides)) {
      return()
    }
    entry$settings$group_color_overrides <- next_overrides
    entry$settings$colors <- NULL
    if (isTRUE(replace_entry(entry))) {
      group_color_ui_revision(isolate(group_color_ui_revision()) + 1L)
    }
  },
  ignoreInit = TRUE
)

## Assay-dependent controls above use the namespaced Core inputs.
analysis_checkbox_steps <- Filter(
  function(step) !identical(step$id, "marker_genes"),
  builder_analysis_steps()
)
invisible(lapply(analysis_checkbox_steps, function(step) {
  observeEvent(
    input[[paste0("enhance-analysis_", step$id)]],
    {
      id <- current()
      if (
        is.null(id) ||
          !identical(input[["enhance-rendered_for"]], id)
      ) {
        return()
      }
      entry <- isolate(entry_of(id))
      req(entry)
      selected <- entry$settings$analyses %||% character()
      requested <- isTRUE(input[[paste0("enhance-analysis_", step$id)]])
      analysis_profile <- builder_enhance_analysis_profile(
        entry$profile,
        entry$settings$organism,
        entry$settings$assay
      )
      blocked <- builder_step_blocked(step, analysis_profile, selected)
      if (requested && !is.null(blocked)) {
        return()
      }
      if (requested) {
        selected <- unique(c(selected, step$id))
      } else {
        selected <- setdiff(selected, step$id)
      }
      entry$settings$analyses <- builder_normalize_analyses(
        selected,
        builder_profile_has(entry$profile, "marker_genes")
      )
      replace_entry(entry)
    },
    ignoreInit = TRUE
  )
}))
