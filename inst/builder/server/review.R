## Builder server: review.

## -- the action bar ------------------------------------------------------
validate_review_inputs <- function(values) {
  next_options <- try(do.call(builder_review_options, values), silent = TRUE)
  if (inherits(next_options, "try-error")) {
    review_validation(list(
      ok = FALSE,
      error = conditionMessage(attr(next_options, "condition"))
    ))
    return(invisible(FALSE))
  }
  review_options(next_options)
  review_validation(list(ok = TRUE, error = NULL))
  invisible(TRUE)
}

observe({
  if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
    return()
  }
  current_options <- isolate(review_options())
  values <- list(
    welcome_message = input[["build_welcome_message"]],
    initial_page = input[["build_initial_page"]],
    point_size = current_options$point_size,
    variable_to_compare = current_options$variable_to_compare,
    host = input[["build_host"]],
    port = input[["build_port"]],
    max_request_size = current_options$max_request_size,
    display_mode = current_options$display_mode,
    launch_browser = FALSE,
    show_upload_ui = input[["build_show_upload_ui"]]
  )
  if (any(vapply(values, is.null, logical(1)))) {
    return()
  }
  validate_review_inputs(values)
})

observeEvent(
  input[["build_require_login"]],
  {
    if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
      return()
    }
    enabled <- isTRUE(input[["build_require_login"]])
    if (
      enabled && (!isTRUE(build_mode()) || !isTRUE(auth_capability()$available))
    ) {
      auth_enabled(FALSE)
      auth_accounts(builder_auth_empty_accounts())
      auth_validation(list(
        ok = FALSE,
        error = "Login is unavailable for the current App settings."
      ))
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
      return()
    }
    auth_enabled(enabled)
    if (!enabled) {
      auth_accounts(builder_auth_empty_accounts())
      auth_validation(list(ok = TRUE, error = NULL))
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
      return()
    }
    parsed <- builder_auth_validate_payload(TRUE, isolate(auth_accounts()))
    auth_validation(list(ok = parsed$ok, error = parsed$error))
  },
  ignoreInit = TRUE
)

observeEvent(
  input$builder_auth_accounts,
  {
    if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
      return()
    }
    payload <- input$builder_auth_accounts
    if (
      is.null(payload) ||
        !is.list(payload) ||
        is.object(payload) ||
        !identical(
          sort(names(payload)),
          c("accounts", "enabled", "nonce")
        ) ||
        !is.logical(payload$enabled) ||
        length(payload$enabled) != 1L ||
        is.na(payload$enabled) ||
        !is.numeric(payload$nonce) ||
        length(payload$nonce) != 1L ||
        is.na(payload$nonce) ||
        !is.finite(payload$nonce)
    ) {
      return()
    }
    if (
      isTRUE(payload$enabled) &&
        (!isTRUE(build_mode()) || !isTRUE(auth_capability()$available))
    ) {
      auth_enabled(FALSE)
      auth_accounts(builder_auth_empty_accounts())
      auth_validation(list(
        ok = FALSE,
        error = "Login accounts could not be saved."
      ))
      session$sendCustomMessage(
        "builder_auth_status",
        list(
          ok = FALSE,
          message = "Login accounts could not be saved.",
          nonce = payload$nonce
        )
      )
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
      return()
    }
    parsed <- builder_auth_validate_payload(
      isTRUE(payload$enabled),
      payload$accounts
    )
    if (!isTRUE(parsed$ok)) {
      auth_validation(list(
        ok = FALSE,
        error = "Login accounts could not be saved."
      ))
      session$sendCustomMessage(
        "builder_auth_status",
        list(
          ok = FALSE,
          message = "Login accounts could not be saved.",
          nonce = payload$nonce
        )
      )
      return()
    }
    auth_validation(list(ok = TRUE, error = NULL))
    auth_enabled(isTRUE(payload$enabled))
    auth_accounts(parsed$accounts)
    session$sendCustomMessage(
      "builder_auth_status",
      list(
        ok = TRUE,
        account_count = length(parsed$accounts),
        nonce = payload$nonce
      )
    )
    if (!isTRUE(payload$enabled)) {
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  build_mode(),
  {
    if (isTRUE(build_mode())) {
      return()
    }
    auth_enabled(FALSE)
    auth_accounts(builder_auth_empty_accounts())
    auth_validation(list(ok = TRUE, error = NULL))
    session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
  },
  ignoreInit = TRUE
)

freeze_plan_for_output <- function(
  out_dir,
  overwrite = FALSE,
  output_options = NULL,
  entries_override = NULL
) {
  pending <- imports()$entries
  if (length(pending)) {
    states <- vapply(pending, `[[`, character(1), "load_state")
    message <- if (any(states == "error")) {
      "Retry or remove datasets that could not load."
    } else {
      "Wait for all datasets to finish loading before building."
    }
    return(builder_plan_error(message, "imports_pending"))
  }
  all <- entries_override %||% sets()
  if (!length(all)) {
    return(builder_plan_error("No datasets yet.", "empty_release"))
  }
  review_identity <- tryCatch(
    builder_review_configuration_identity(
      all,
      builder_configuration_identity_cache
    ),
    error = function(error) error
  )
  if (inherits(review_identity, "condition")) {
    return(builder_plan_error(
      conditionMessage(review_identity),
      "invalid_review_identity"
    ))
  }
  explicit_output <- inherits(output_options, "builder_build_options")
  make_app <- if (explicit_output) {
    isTRUE(output_options$make_app) || builder_plan_requires_app(all)
  } else {
    builder_plan_requires_app(all)
  }
  validation <- review_validation()
  if (make_app && !isTRUE(validation$ok)) {
    return(builder_plan_error(
      validation$error %||% "Viewer App options are invalid.",
      "invalid_review_options"
    ))
  }
  login_enabled <- make_app && isTRUE(auth_enabled())
  parsed_auth <- builder_auth_validate_payload(
    login_enabled,
    if (login_enabled) auth_accounts() else builder_auth_empty_accounts()
  )
  if (!isTRUE(parsed_auth$ok)) {
    return(builder_plan_error(parsed_auth$error, "invalid_auth_accounts"))
  }
  build_entries <- if (
    exists("builder_project_build_entries", mode = "function")
  ) {
    builder_project_build_entries(all)
  } else {
    all
  }
  app_options <- if (make_app && explicit_output) {
    builder_review_options_for_plan(
      output_options$app,
      initial_dataset = output_options$initial_dataset
    )
  } else {
    builder_review_options_for_plan(builder_review_options())
  }
  builder_freeze_plan(
    entries = build_entries,
    out_dir = out_dir,
    make_app = make_app,
    overwrite = isTRUE(overwrite),
    review_identity = review_identity,
    app_options = app_options,
    app_auth = builder_auth_summary(login_enabled, parsed_auth$accounts),
    dataset_state_cache = builder_dataset_state_cache,
    file_verification_cache = builder_spatial_image_verification_cache,
    reuse_cache = builder_session_reuse_cache_for_build(),
    verification_cache = builder_session_verification_cache
  )
}

## Verified CRBs from this session's earlier builds. An open Project reuses
## its own registered artifacts instead, and its post-build registration
## only accepts those. Read-back results are keyed by CRB MD5 and apply to
## both kinds of reuse.
builder_session_reuse_cache <- new.env(parent = emptyenv())
builder_session_verification_cache <- new.env(parent = emptyenv())

builder_session_reuse_cache_for_build <- function() {
  project_open <- exists("builder_project", inherits = TRUE) &&
    is.function(builder_project) &&
    !is.null(isolate(builder_project()))
  if (project_open) NULL else builder_session_reuse_cache
}

freeze_materialized_plan_for_output <- function(
  out_dir,
  overwrite = FALSE,
  output_options = NULL,
  notify = TRUE
) {
  materialized <- alignment_server$materialize_coordinate_drafts(
    notify = notify
  )
  if (!isTRUE(materialized$ok)) {
    return(builder_plan_error(
      materialized$error %||% "Coordinate settings could not be saved.",
      "coordinate_materialization_failed"
    ))
  }
  freeze_plan_for_output(
    out_dir = out_dir,
    overwrite = overwrite,
    output_options = output_options,
    entries_override = materialized$all_entries
  )
}

current_review_snapshot <- reactive({
  if (length(imports()$entries) || !isTRUE(all_datasets_checked())) {
    return(NULL)
  }
  entries <- sets()
  if (!length(entries)) {
    return(NULL)
  }
  identity <- builder_review_configuration_identity(
    entries,
    builder_configuration_identity_cache
  )
  marks <- dataset_check_marks()[identity$dataset_order]
  if (!identical(unname(marks), unname(identity$checks))) {
    return(NULL)
  }
  states <- lapply(entries, function(entry) {
    tryCatch(
      builder_cached_dataset_state(entry, builder_dataset_state_cache),
      error = function(error) error
    )
  })
  if (any(vapply(states, inherits, logical(1), "condition"))) {
    return(NULL)
  }
  make_app <- builder_plan_requires_app(entries)
  tryCatch(
    builder_review_snapshot_from_entries(
      entries = entries,
      states = states,
      identity = identity,
      make_app = make_app
    ),
    error = function(error) NULL
  )
})
current_review_snapshot <- bindEvent(
  current_review_snapshot,
  dataset_check_marks(),
  dataset_revisions(),
  alignment_server$pending_drafts(),
  imports(),
  ignoreNULL = FALSE
)

review_report <- reactive({
  pending <- imports()$entries
  if (length(pending)) {
    states <- vapply(pending, `[[`, character(1), "load_state")
    if (any(states == "error")) {
      return(list(
        ok = FALSE,
        msg = "Retry or remove datasets that could not load."
      ))
    }
    return(list(
      ok = FALSE,
      msg = "Wait for all datasets to finish loading before building."
    ))
  }
  snapshot <- current_review_snapshot()
  if (!builder_review_snapshot_valid(snapshot)) {
    return(list(
      ok = FALSE,
      msg = "Check every dataset before continuing."
    ))
  }
  list(
    ok = TRUE,
    msg = paste0(
      length(snapshot$items),
      " dataset",
      if (length(snapshot$items) == 1L) "" else "s",
      " ready"
    )
  )
})

configure_readiness <- reactive({
  if (length(imports()$entries)) {
    return(list(
      can_continue = FALSE,
      message = "Wait for all datasets to finish loading.",
      snapshot = NULL
    ))
  }
  snapshot <- current_review_snapshot()
  ready <- builder_review_snapshot_valid(snapshot)
  list(
    can_continue = ready,
    message = if (ready) {
      count <- length(snapshot$items)
      paste0(
        count,
        " dataset",
        if (identical(count, 1L)) "" else "s",
        " ready"
      )
    } else {
      "Check every dataset before continuing."
    },
    snapshot = snapshot
  )
})

output[["enhance-analysis_modules"]] <- renderUI({
  contract <- enhance_contract()
  req(contract$id)
  entry <- isolate(entry_of(contract$id))
  req(entry)
  builder_enhance_modules_ui(
    "enhance",
    builder_enhance_modules(
      entry$profile,
      list(
        organism = contract$organism,
        analyses = entry$settings$analyses %||% character(),
        marker_imports = entry$settings$marker_imports %||% list()
      )
    )
  )
})

output[["enhance-table_list"]] <- renderUI({
  enhance_table_ui_revision()
  id <- current()
  req(id)
  entry <- isolate(entry_of(id))
  req(entry)
  table_action_owner <- builder_content_action_owner(
    entry,
    isolate(builder_project_open_generation())
  )
  tables <- entry$settings$tables %||% list()
  if (!length(tables)) {
    return(NULL)
  }
  files <- unique(vapply(
    tables,
    function(table) table$file_name %||% "",
    character(1)
  ))
  div(
    class = "enhance-table-list",
    `data-generation` = table_action_owner$generation,
    `data-project-epoch` = table_action_owner$project_epoch,
    `data-owner-token` = table_action_owner$owner_token,
    h4(paste(
      length(files),
      if (length(files) == 1L) "workbook" else "workbooks",
      "·",
      length(tables),
      if (length(tables) == 1L) "table" else "tables"
    )),
    lapply(seq_along(files), function(index) {
      filename <- files[[index]]
      keys <- names(tables)[vapply(
        tables,
        function(table) identical(table$file_name %||% "", filename),
        logical(1)
      )]
      records <- tables[keys]
      first <- records[[1L]]
      file_type <- first$file_type %||% toupper(tools::file_ext(filename))
      file_size <- builder_review_human_size(first$file_size %||% NA_real_)
      workbook_name <- first$workbook_name %||% filename
      tags$details(
        class = "enhance-workbook-item",
        `data-workbook-key` = filename,
        tags$summary(
          class = "enhance-workbook-summary",
          tags$span(
            class = "enhance-workbook-chevron",
            `aria-hidden` = "true",
            tags$svg(
              viewBox = "0 0 24 24",
              fill = "none",
              tags$path(
                d = "m9 18 6-6-6-6",
                stroke = "currentColor",
                `stroke-width` = "2.25",
                `stroke-linecap` = "round",
                `stroke-linejoin` = "round"
              )
            )
          ),
          div(
            class = "enhance-workbook-meta",
            div(
              class = "enhance-attachment-name enhance-workbook-name",
              workbook_name
            ),
            div(
              class = "enhance-attachment-editor",
              hidden = NA,
              tags$input(
                type = "text",
                class = "enhance-workbook-display-name",
                value = workbook_name,
                `data-workbook-key` = filename,
                autocomplete = "off",
                spellcheck = "false",
                `aria-label` = paste("Viewer workbook name for", filename)
              )
            ),
            span(
              class = "enhance-workbook-source",
              paste(
                file_type,
                file_size,
                paste(
                  length(keys),
                  if (length(keys) == 1L) "sheet" else "sheets"
                ),
                sep = " · "
              )
            )
          ),
          div(
            class = "enhance-workbook-controls",
            span(
              class = "builder-status builder-status--ready",
              "Ready"
            ),
            div(
              class = "enhance-attachment-actions",
              tags$button(
                type = "button",
                class = "enhance-attachment-edit enhance-workbook-edit",
                `data-workbook-key` = filename,
                `aria-label` = paste(
                  "Edit Viewer workbook name for",
                  workbook_name
                ),
                "Edit"
              ),
              tags$button(
                type = "button",
                class = "enhance-attachment-save enhance-workbook-save",
                `data-workbook-key` = filename,
                `aria-label` = paste(
                  "Save Viewer workbook name for",
                  workbook_name
                ),
                hidden = NA,
                "Save"
              ),
              tags$button(
                type = "button",
                class = "enhance-attachment-cancel enhance-workbook-cancel",
                `aria-label` = paste(
                  "Cancel editing Viewer workbook name for",
                  workbook_name
                ),
                hidden = NA,
                "Cancel"
              ),
              tags$button(
                type = "button",
                class = "enhance-workbook-remove",
                `data-workbook-key` = filename,
                `aria-label` = paste("Remove workbook", workbook_name),
                "Remove workbook"
              )
            )
          )
        ),
        div(
          class = "enhance-workbook-sheets",
          lapply(keys, function(key) {
            table <- tables[[key]]
            sheet_name <- table$sheet_name %||% table$sheet %||% key
            display_name <- table$display_name %||% sheet_name
            div(
              class = "enhance-sheet-item",
              div(
                class = "enhance-sheet-meta",
                div(
                  class = "enhance-attachment-name enhance-table-name",
                  display_name
                ),
                div(
                  class = "enhance-attachment-editor",
                  hidden = NA,
                  tags$input(
                    type = "text",
                    class = "enhance-table-display-name",
                    value = display_name,
                    `data-table-key` = key,
                    autocomplete = "off",
                    spellcheck = "false",
                    `aria-label` = paste("Viewer table name for", sheet_name)
                  )
                ),
                span(
                  class = "enhance-sheet-source",
                  paste("Source sheet:", sheet_name)
                )
              ),
              div(
                class = "enhance-attachment-actions",
                tags$button(
                  type = "button",
                  class = "enhance-attachment-edit enhance-table-edit",
                  `data-table-key` = key,
                  `aria-label` = paste(
                    "Edit Viewer table name for",
                    display_name
                  ),
                  "Edit"
                ),
                tags$button(
                  type = "button",
                  class = "enhance-attachment-save enhance-table-save",
                  `data-table-key` = key,
                  `aria-label` = paste(
                    "Save Viewer table name for",
                    display_name
                  ),
                  hidden = NA,
                  "Save"
                ),
                tags$button(
                  type = "button",
                  class = "enhance-attachment-cancel enhance-table-cancel",
                  `aria-label` = paste(
                    "Cancel editing Viewer table name for",
                    display_name
                  ),
                  hidden = NA,
                  "Cancel"
                ),
                tags$button(
                  type = "button",
                  class = "enhance-table-remove",
                  `data-table-key` = key,
                  `aria-label` = paste("Remove table", display_name),
                  "Remove"
                )
              )
            )
          })
        )
      )
    }),
    div(
      class = "enhance-table-editing-note",
      span(class = "builder-status builder-status--ready", "Editing is local."),
      " Select Save or press Enter to apply."
    )
  )
})

output[["inspect_stage"]] <- renderUI({
  dataset_check_marks()
  id <- current()
  req(id)
  entry <- isolate(entry_of(id))
  req(entry)
  state <- try(
    builder_cached_dataset_state(entry, builder_dataset_state_cache),
    silent = TRUE
  )
  attention <- if (inherits(state, "try-error")) {
    character()
  } else {
    state$attention_ids
  }
  if (
    identical(input[["core-rendered_for"]], id) &&
      builder_stage_has_text(input[["core-organism"]])
  ) {
    attention <- setdiff(attention, "settings_organism")
    if (!inherits(state, "try-error")) {
      state$attention_ids <- attention
    }
  }
  blockers <- if (inherits(state, "try-error")) {
    "Dataset state could not be validated."
  } else {
    state$blocking_ids
  }
  builder_inspect_stage_ui(
    "inspect",
    builder_inspect_model(
      profile = entry$profile,
      state = if (inherits(state, "try-error")) {
        list(
          attention_ids = attention,
          blocking_ids = blockers,
          manifest = list()
        )
      } else {
        state
      },
      format = entry$format,
      dataset_id = entry$id,
      settings = entry$settings
    )
  )
})

configure_actions_state <- reactiveVal(NULL)
observe({
  entries <- sets()
  ids <- vapply(entries, `[[`, character(1), "id")
  checked <- checked_dataset_ids()
  unchecked <- setdiff(ids, checked)
  current_checked <- current() %in% checked
  alignment_settled <- isTRUE(alignment_server$alignment_settled())
  readiness <- if (length(unchecked)) {
    list(can_continue = FALSE, message = NULL)
  } else {
    configure_readiness()
  }
  message <- if (!alignment_settled) {
    "Saving the latest spatial adjustment…"
  } else if (length(unchecked)) {
    builder_configure_check_message(
      length(ids) - length(unchecked),
      length(ids),
      length(imports()$entries %||% list())
    )
  } else {
    readiness$message
  }
  current_index <- match(current(), ids)
  current_entry <- if (length(current_index) == 1L && !is.na(current_index)) {
    entries[[current_index]]
  } else {
    NULL
  }
  if (
    is.list(current_entry) &&
      identical(current_entry$load_state %||% "loaded", "artifact_ready")
  ) {
    mode <- "artifact"
  } else {
    mode <- "dataset"
  }
  state <- list(
    mode = mode,
    message = message,
    can_continue = readiness$can_continue &&
      !length(unchecked) &&
      alignment_settled,
    check_ready = alignment_settled,
    dataset_checked = current_checked,
    remaining = length(unchecked)
  )
  if (!identical(state, isolate(configure_actions_state()))) {
    configure_actions_state(state)
  }
})

configure_dataset_name <- reactiveVal(NULL)
observe({
  entry <- entry_of(current())
  name <- if (is.null(entry)) "" else entry$settings$name %||% entry$id
  if (!identical(name, isolate(configure_dataset_name()))) {
    configure_dataset_name(name)
  }
})
output$configure_dataset_name <- renderText(configure_dataset_name())

output$configure_actions <- renderUI({
  state <- configure_actions_state()
  req(state)
  if (identical(state$mode, "artifact")) {
    return(builder_project_artifact_actions_ui(
      state$message,
      state$can_continue,
      dataset_checked = state$dataset_checked,
      remaining = state$remaining,
      check_ready = state$check_ready
    ))
  }
  builder_configure_actions_ui(
    state$message,
    state$can_continue,
    dataset_checked = state$dataset_checked,
    remaining = state$remaining,
    check_ready = state$check_ready
  )
})

open_next_unchecked_dataset <- function(id, entries, checked) {
  ids <- vapply(entries, `[[`, character(1), "id")
  index <- match(id, ids)
  if (is.na(index)) {
    return(invisible(FALSE))
  }
  unchecked <- setdiff(ids, checked)
  if (!length(unchecked)) {
    return(invisible(FALSE))
  }
  after <- if (index < length(ids)) {
    ids[(index + 1L):length(ids)]
  } else {
    character()
  }
  before <- if (index > 1L) ids[seq_len(index - 1L)] else character()
  target <- c(after, before)
  target <- target[target %in% unchecked][[1L]]
  alignment_server$request_dataset_switch(target, function() {
    current(target)
    result(NULL)
    session$sendCustomMessage(
      "builder_focus_dataset_start",
      list(
        dataset = target,
        generation = as.integer(isolate(entry_of(target))$revision %||% 0L)
      )
    )
  })
  invisible(TRUE)
}

observeEvent(input$complete_dataset_check, {
  if (
    exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("check_dataset"))
  ) {
    return()
  }
  id <- isolate(current())
  if (is.null(id)) {
    return()
  }
  entry <- isolate(entry_of(id))
  if (!is.list(entry)) {
    return()
  }
  entries <- isolate(sets())
  ids <- vapply(entries, `[[`, character(1), "id")
  if (!id %in% ids) {
    return()
  }
  marks <- isolate(dataset_check_marks())
  checked <- builder_project_checked_ids(
    entries,
    marks,
    identity_cache = builder_configuration_identity_cache
  )
  if (id %in% checked) {
    open_next_unchecked_dataset(id, entries, checked)
    return()
  }
  materialized <- isolate(alignment_server$materialize_coordinate_drafts(
    dataset = id,
    notify = TRUE,
    require_settled = TRUE
  ))
  if (!isTRUE(materialized$ok)) {
    return()
  }
  entries <- materialized$all_entries
  ids <- vapply(entries, `[[`, character(1), "id")
  index <- match(id, ids)
  if (is.na(index)) {
    return()
  }
  dataset_state <- try(
    builder_cached_dataset_state(
      entries[[index]],
      builder_dataset_state_cache
    ),
    silent = TRUE
  )
  unresolved <- c(
    if (
      !builder_stage_has_text(
        entries[[index]]$settings$organism %||% ""
      )
    ) {
      "settings_organism"
    },
    if (inherits(dataset_state, "try-error")) {
      "dataset"
    } else {
      c(
        dataset_state$blocking_ids %||% character(),
        dataset_state$attention_ids %||% character(),
        dataset_state$checking_ids %||% character()
      )
    }
  )
  unresolved <- unique(unresolved)
  if (length(unresolved)) {
    session$sendCustomMessage(
      "builder_focus_incomplete_setting",
      list(
        dataset = id,
        generation = as.integer(entries[[index]]$revision %||% 0L),
        blocker = unresolved[[1L]]
      )
    )
    return()
  }
  marks[[id]] <- builder_project_check_identity(
    entries[[index]],
    builder_configuration_identity_cache
  )
  dataset_check_marks(marks)
  checked <- builder_project_checked_ids(
    entries,
    marks,
    identity_cache = builder_configuration_identity_cache
  )
  open_next_unchecked_dataset(id, entries, checked)
})

observeEvent(
  input[["core-organism_apply_all"]],
  {
    if (
      exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
        !isTRUE(builder_operation_allowed("edit_dataset"))
    ) {
      return()
    }
    id <- isolate(current())
    organism <- input[["core-organism"]]
    if (
      is.null(id) ||
        !identical(input[["core-rendered_for"]], id) ||
        !builder_stage_has_text(organism)
    ) {
      return()
    }
    targets <- builder_core_organism_targets(
      isolate(sets()),
      organism,
      checked = c(isolate(checked_dataset_ids()), id)
    )
    changed <- sum(vapply(
      targets,
      function(entry) isTRUE(replace_entry(entry)),
      logical(1)
    ))
    label <- switch(
      organism,
      hg = "Human (hg)",
      mm = "Mouse (mm)",
      other = "Other",
      organism
    )
    builder_notify(
      "info",
      if (changed) {
        paste0(
          "Organism set to ",
          label,
          " on ",
          changed,
          if (changed == 1L) " other dataset." else " other datasets."
        )
      } else {
        "No other unchecked dataset needed a change."
      },
      target = list(field = "core-organism_apply_all"),
      duration = 4
    )
  },
  ignoreInit = TRUE
)

render_configure_workbench <- function() {
  configure_workbench_surface()
  id <- current()
  entry <- isolate(entry_of(id))
  if (is.null(entry)) {
    return(builder_empty_workbench_ui())
  }
  if (identical(entry$load_state %||% "loaded", "artifact_ready")) {
    project <- isolate(builder_project())
    return(builder_project_artifact_workbench_ui(
      entry,
      root = if (is.null(project)) NULL else project$root
    ))
  }
  state <- try(
    builder_cached_dataset_state(entry, builder_dataset_state_cache),
    silent = TRUE
  )
  attention <- if (inherits(state, "try-error")) {
    character()
  } else {
    state$attention_ids
  }
  blockers <- if (inherits(state, "try-error")) {
    "Dataset state could not be validated."
  } else {
    state$blocking_ids
  }
  if (!inherits(state, "try-error")) {
    entry <- state$entry
  }
  content_action_owner <- builder_content_action_owner(
    entry,
    isolate(builder_project_open_generation())
  )
  metadata_record <- if (inherits(state, "try-error")) {
    list()
  } else {
    state$manifest$metadata_policy %||% list()
  }
  settings <- entry$settings
  resolved_assay <- builder_core_resolve_assay(
    entry$profile,
    settings$assay
  )
  if (!is.null(resolved_assay)) {
    settings$assay <- resolved_assay
  }
  assay_profile <- if (is.null(resolved_assay)) {
    list(
      layers = entry$profile$layers,
      nUMI_choices = entry$profile$nUMI,
      nGene_choices = entry$profile$nGene
    )
  } else {
    entry$profile$assay_profiles[[resolved_assay]] %||%
      list(
        layers = entry$profile$layers,
        nUMI_choices = entry$profile$nUMI,
        nGene_choices = entry$profile$nGene
      )
  }
  core_model <- c(
    settings[c(
      "name",
      "organism",
      "included_groups",
      "default_group",
      "cell_cycle_columns",
      "included_projections",
      "default_projection",
      "initial_projections",
      "overview_point_size",
      "overview_point_opacity",
      "overview_percentage_cells_to_show",
      "spatial_percentage_cells_to_show",
      "included_trajectories",
      "default_trajectory",
      "assay",
      "layer",
      "nUMI",
      "nGene"
    )],
    list(
      id = entry$id,
      organism_choices = c(
        "Human (hg)" = "hg",
        "Mouse (mm)" = "mm",
        "Other" = "other"
      ),
      organism_detected = builder_detected_organism(
        entry$profile %||% list(),
        entry$dataset_profile
      ),
      group_choices = unname(entry$profile$group_candidates),
      suggested_groups = entry$profile$group_preselect %||%
        settings$included_groups %||%
        character(),
      metadata_catalog = entry$dataset_profile$viewer_content$metadata %||%
        entry$profile$viewer_content$metadata %||%
        list(),
      metadata_policy = if (inherits(state, "try-error")) {
        entry$settings$metadata_policy %||%
          entry$settings$recommendations$metadata %||%
          list()
      } else {
        state$metadata_policy %||% list()
      },
      analysis_manifest = if (inherits(state, "try-error")) {
        list()
      } else {
        state$manifest %||% list()
      },
      content_manifest = if (inherits(state, "try-error")) {
        list()
      } else {
        state$manifest %||% list()
      },
      immune_source_fact = entry$dataset_profile$content$immune_repertoire %||%
        entry$profile$content$immune_repertoire %||%
        list(),
      content_sources = settings$content_sources %||% list(),
      content_action_owner = content_action_owner,
      analysis_acknowledgements = if (inherits(state, "try-error")) {
        character()
      } else {
        state$acknowledgements %||% character()
      },
      projection_catalog = projection_catalog_for_entry(entry),
      trajectory_catalog = trajectory_catalog_for_entry(entry),
      levels = entry$levels %||% list(),
      projection_choices = entry$profile$reductions,
      assay_choices = entry$profile$assays,
      layer_choices = if (
        builder_has_text(settings$layer) &&
          !settings$layer %in% (assay_profile$layers %||% character())
      ) {
        stats::setNames(
          c(settings$layer, assay_profile$layers),
          c(paste0(settings$layer, " (unavailable)"), assay_profile$layers)
        )
      } else {
        assay_profile$layers
      },
      layer_attention = if (
        builder_has_text(settings$layer) &&
          !settings$layer %in% (assay_profile$layers %||% character())
      ) {
        if (!length(assay_profile$layers %||% character())) {
          paste0(
            "Saved layer `",
            settings$layer,
            "` is unavailable because assay `",
            settings$assay,
            "` has no expression layer covering every cell. ",
            "Reopen this dataset with its saved CRB, or use a compatible source."
          )
        } else {
          paste0(
            "Saved layer `",
            settings$layer,
            "` is unavailable. Select an available layer and check this dataset again."
          )
        }
      } else {
        ""
      },
      nUMI_choices = assay_profile$nUMI_choices,
      nGene_choices = assay_profile$nGene_choices,
      backend = settings$expression_backend %||% "embedded",
      backend_choices = c(
        "Embedded" = "embedded",
        "HDF5" = "h5"
      ),
      metadata_attention = if (length(attention)) {
        if ("metadata_policy" %in% attention) {
          "Review the suggested metadata retention choices before continuing."
        } else {
          ""
        }
      } else {
        ""
      },
      metadata_acknowledgement = if (inherits(state, "try-error")) {
        NULL
      } else {
        builder_acknowledgement_action(
          metadata_record,
          state$acknowledgements %||% character(),
          entry$id,
          "metadata_policy",
          content_action_owner
        )
      }
    )
  )
  enhance_model <- builder_enhance_model(
    id = entry$id,
    profile = entry$profile,
    state = if (inherits(state, "try-error")) list() else state,
    settings = entry$settings,
    modules = list(),
    active_section = shiny::isolate(active_slice()),
    active_image = shiny::isolate(alignment_server$active_image()),
    active_roi = shiny::isolate(alignment_server$active_roi())
  )
  div(
    class = "builder-stage builder-stage-shell builder-stage-configure",
    `data-workflow-stage` = "configure",
    builder_stage_header_ui(
      "Configure",
      tagList(
        "Configure ",
        # The workbench is not rebuilt for a rename; this span follows the
        # dataset name the way the rail does.
        tags$span(
          id = "configure_dataset_name",
          class = "shiny-text-output",
          entry$settings$name %||% entry$id
        )
      ),
      "Set essentials, then check this dataset."
    ),
    div(
      class = "builder-configure-inspect",
      uiOutput("inspect_stage")
    ),
    builder_core_stage_ui("core", core_model),
    builder_enhance_stage_ui(
      "enhance",
      enhance_model,
      dynamic_modules = TRUE,
      include_spatial = FALSE
    ),
    builder_core_advanced_ui("core", core_model),
    builder_views_stage_ui(
      "core",
      core_model,
      builder_spatial_stage_ui("enhance", enhance_model)
    ),
    uiOutput("configure_actions")
  )
}

render_review_workbench <- function() {
  snapshot <- workflow()$review_snapshot
  req(builder_review_snapshot_valid(snapshot))
  tagAppendAttributes(
    builder_review_stage_ui(
      "review",
      builder_review_model(snapshot),
      footer = builder_review_confirmation_ui()
    ),
    `data-workflow-stage` = "review"
  )
}

observeEvent(input$back_to_settings, {
  if (
    exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("navigate_workflow"))
  ) {
    return()
  }
  workflow(builder_reduce_workflow(
    isolate(workflow()),
    list(type = "back_to_settings")
  ))
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = "configure"))
    },
    once = TRUE
  )
})

observeEvent(input$confirm_review, {
  if (
    exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("navigate_workflow"))
  ) {
    return()
  }
  state <- isolate(workflow())
  if (!identical(state$stage, "review")) {
    return()
  }
  reviewed <- state$review_snapshot
  if (!builder_review_snapshot_valid(reviewed)) {
    workflow(builder_reduce_workflow(state, list(type = "invalidate")))
    result(NULL)
    session$onFlushed(
      function() {
        session$sendCustomMessage("builder_focus_stage", list(id = "configure"))
      },
      once = TRUE
    )
    return()
  }
  if (isTRUE(reviewed$make_app)) {
    build_mode(TRUE)
  }
  result(NULL)
  workflow(builder_reduce_workflow(
    state,
    list(type = "confirm_review", snapshot = reviewed)
  ))
  session$onFlushed(
    function() {
      session$sendCustomMessage("builder_focus_stage", list(id = "build"))
    },
    once = TRUE
  )
})

observe({
  state <- workflow()
  if (is.null(state$review_snapshot) && is.null(state$confirmation)) {
    return()
  }
  if (identical(isolate(build_flow())$stage, "building")) {
    return()
  }
  live <- current_review_snapshot()
  matches <- builder_review_snapshot_valid(live) &&
    identical(
      builder_review_snapshot_identity(state$review_snapshot),
      builder_review_snapshot_identity(live)
    )
  if (!isTRUE(matches)) {
    workflow(builder_reduce_workflow(state, list(type = "invalidate")))
    build_flow(list(stage = "idle", plan = NULL))
    result(NULL)
    session$sendCustomMessage(
      "builder_build_dialog",
      list(action = "close")
    )
    if (state$stage %in% c("review", "build")) {
      builder_notify(
        "warning",
        "Settings changed. Review the updated plan before building.",
        target = "review"
      )
    }
  }
})

observe({
  current_flow <- build_flow()
  current_protocol <- protocol()
  if (
    identical(current_flow$stage, "building") &&
      !is.null(current_protocol) &&
      builder_protocol_is_quiescent(current_protocol)
  ) {
    build_flow(list(stage = "idle", plan = NULL))
  }
})
