## Typed top-level Builder result models and actions.

builder_release_recovery_action <- function(recovery) {
  if (
    !is.list(recovery) ||
      !identical(recovery$state, "recovery_required")
  ) {
    return(NULL)
  }
  if (isTRUE(recovery$backup_exists)) {
    return("restore")
  }
  if (isTRUE(recovery$safe_to_abort)) {
    return("abort")
  }
  NULL
}

builder_mutations_locked <- function(flow, protocol) {
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (!is.list(flow) || !scalar_text(flow$stage)) {
    return(TRUE)
  }
  if (!is.null(protocol)) {
    if (!is.list(protocol) || !scalar_text(protocol$build_status)) {
      return(TRUE)
    }
    if (protocol$build_status %in% c("queued", "running", "cancelling")) {
      return(TRUE)
    }
  }
  flow$stage %in%
    c(
      "preparing",
      "building",
      "choosing_folder"
    )
}

builder_build_options <- function(
  make_app = FALSE,
  welcome_message = "Welcome to CerebroNexus!",
  initial_page = "data_info",
  point_size = 5,
  variable_to_compare = FALSE,
  host = "127.0.0.1",
  port = 8080L,
  max_request_size = 8000,
  display_mode = "normal",
  launch_browser = FALSE,
  show_upload_ui = FALSE,
  initial_dataset = NULL
) {
  if (!is.logical(make_app) || length(make_app) != 1L || is.na(make_app)) {
    stop("A valid Build output mode is required.", call. = FALSE)
  }
  app <- builder_review_options(
    welcome_message = welcome_message,
    initial_page = initial_page,
    point_size = point_size,
    variable_to_compare = variable_to_compare,
    host = host,
    port = port,
    max_request_size = max_request_size,
    display_mode = display_mode,
    launch_browser = launch_browser,
    show_upload_ui = show_upload_ui
  )
  if (
    !is.null(initial_dataset) &&
      (!is.character(initial_dataset) ||
        length(initial_dataset) != 1L ||
        is.na(initial_dataset) ||
        !nzchar(initial_dataset))
  ) {
    stop("A valid starting dataset is required.", call. = FALSE)
  }
  structure(
    list(
      make_app = make_app,
      app = app,
      initial_dataset = initial_dataset
    ),
    class = c("builder_build_options", "list")
  )
}

builder_build_auth_controls_ui <- function(auth) {
  if (!isTRUE(auth$enabled) || !isTRUE(auth$available)) {
    return(NULL)
  }
  htmltools::tags$div(
    class = "review-auth-controls",
    htmltools::tags$div(
      class = "builder-auth-account-copy",
      htmltools::tags$strong("Login accounts"),
      htmltools::tags$span(
        class = "review-auth-summary",
        if (identical(auth$account_count, 1L)) {
          "1 account configured"
        } else if (auth$account_count > 1L) {
          paste0(auth$account_count, " accounts configured")
        } else {
          "No accounts configured"
        }
      )
    ),
    htmltools::tags$button(
      type = "button",
      class = "btn builder-auth-open",
      "Manage accounts"
    ),
    if (builder_stage_has_text(auth$error %||% "")) {
      htmltools::tags$p(class = "hint review-auth-error", auth$error)
    }
  )
}

builder_build_options_ui <- function(
  options,
  app_available = TRUE,
  app_reason = NULL,
  app_required = FALSE,
  initial_page_choices = c("Data info" = "data_info"),
  dataset_choices = character(),
  auth = list(
    enabled = FALSE,
    account_count = 0L,
    error = NULL,
    available = TRUE
  ),
  auth_controls = NULL,
  controls_disabled = FALSE,
  app_settings = NULL
) {
  stopifnot(
    inherits(options, "builder_build_options"),
    is.logical(app_required),
    length(app_required) == 1L,
    !is.na(app_required),
    is.logical(controls_disabled),
    length(controls_disabled) == 1L,
    !is.na(controls_disabled)
  )
  tags <- htmltools::tags
  div <- tags$div
  h3 <- tags$h3
  p <- tags$p
  output_mode <- shiny::radioButtons(
    "build_output_mode",
    label = tags$span(class = "visually-hidden", "Output format"),
    choices = c(
      "Viewer app" = "app",
      "CRB files only" = "crb"
    ),
    selected = if (isTRUE(options$make_app)) "app" else "crb"
  )
  ## A ruled-out choice stays visible but cannot be picked; `reason` is the id
  ## of the sentence that says why, read with the choice by screen readers.
  disable_output_choice <- function(node, value, reason) {
    if (inherits(node, "shiny.tag")) {
      if (
        identical(node$name, "input") &&
          identical(node$attribs$value, value)
      ) {
        node$attribs$disabled <- "disabled"
        node$attribs$`aria-disabled` <- "true"
        node$attribs$`aria-describedby` <- reason
      }
      node$children <- lapply(
        node$children,
        disable_output_choice,
        value,
        reason
      )
      return(node)
    }
    if (is.list(node)) {
      original_attributes <- attributes(node)
      node <- lapply(node, disable_output_choice, value, reason)
      attributes(node) <- original_attributes
    }
    node
  }
  if (isTRUE(app_required)) {
    output_mode <- disable_output_choice(
      output_mode,
      "crb",
      "build_output_crb_reason"
    )
  }
  if (!isTRUE(app_available)) {
    output_mode <- disable_output_choice(
      output_mode,
      "app",
      "build_output_app_reason"
    )
  }
  tags$fieldset(
    class = "builder-build-options-fields",
    disabled = if (controls_disabled) "disabled",
    tags$section(
      class = "builder-stage-section builder-build-options",
      h3("Output"),
      output_mode,
      if (isTRUE(app_required)) {
        p(
          id = "build_output_crb_reason",
          class = "hint builder-app-required-reason",
          "Spatial images or ROI settings require CRB files + Viewer App output."
        )
      },
      if (!isTRUE(app_available)) {
        div(
          class = "builder-app-capability-warning",
          role = "alert",
          div(
            class = "builder-app-capability-icon",
            `aria-hidden` = "true",
            "!"
          ),
          div(
            class = "builder-app-capability-content",
            tags$strong(
              class = "builder-app-capability-title",
              "Viewer App unavailable"
            ),
            p(
              id = "build_output_app_reason",
              class = "builder-app-capability-reason",
              app_reason %||%
                "This installation cannot safely create a Viewer App."
            ),
            p(
              class = "builder-app-capability-note",
              "CRB-only export is still available."
            )
          )
        )
      }
    ),
    if (!is.null(app_settings)) {
      app_settings
    } else if (isTRUE(options$make_app)) {
      builder_build_app_settings_ui(
        options,
        initial_page_choices = initial_page_choices,
        dataset_choices = dataset_choices,
        auth = auth,
        auth_controls = auth_controls
      )
    }
  )
}

## The Viewer App section renders separately from the output-mode radio, so
## switching mode or starting dataset does not re-render (and unfocus) the
## control the user is on.
builder_build_app_settings_ui <- function(
  options,
  initial_page_choices = c("Data info" = "data_info"),
  dataset_choices = character(),
  auth = list(
    enabled = FALSE,
    account_count = 0L,
    error = NULL,
    available = TRUE
  ),
  auth_controls = NULL
) {
  stopifnot(inherits(options, "builder_build_options"))
  tags <- htmltools::tags
  div <- tags$div
  h3 <- tags$h3
  p <- tags$p
  span <- tags$span
  selected_dataset <- options$initial_dataset
  if (
    length(dataset_choices) &&
      (is.null(selected_dataset) || !selected_dataset %in% dataset_choices)
  ) {
    selected_dataset <- unname(dataset_choices[[1L]])
  }
  selected_page <- options$app$initial_page
  if (!selected_page %in% initial_page_choices) {
    selected_page <- unname(initial_page_choices[[1L]])
  }
  auth_available <- isTRUE(auth$available)
  require_login <- shiny::checkboxInput(
    "build_require_login",
    NULL,
    isTRUE(auth$enabled)
  )
  if (!auth_available) {
    require_login <- htmltools::tagQuery(require_login)$find("input")$addAttrs(
      disabled = "disabled"
    )$allTags()
  }
  if (is.null(auth_controls)) {
    auth_controls <- builder_build_auth_controls_ui(auth)
  }
  tags$section(
    class = "builder-stage-section builder-app-settings",
    div(
      class = "builder-app-settings-head",
      h3("Viewer App"),
      p("Choose what visitors see when the app opens.")
    ),
    tags$section(
      class = "builder-app-settings-group",
      p(class = "builder-app-settings-label", "Opening view"),
      div(
        class = "builder-app-opening-fields",
        div(
          class = "builder-app-field builder-app-field--wide",
          shiny::textInput(
            "build_welcome_message",
            "Welcome message",
            options$app$welcome_message
          )
        ),
        if (length(dataset_choices)) {
          div(
            class = "builder-app-field builder-app-field--dataset",
            shiny::selectInput(
              "build_initial_dataset",
              "Starting dataset",
              choices = dataset_choices,
              selected = selected_dataset
            )
          )
        },
        div(
          class = "builder-app-field",
          shiny::selectInput(
            "build_initial_page",
            "Starting page",
            choices = initial_page_choices,
            selected = selected_page
          )
        )
      )
    ),
    tags$section(
      class = "builder-app-settings-group",
      p(class = "builder-app-settings-label", "Access"),
      div(
        class = "builder-app-toggle",
        tags$label(
          class = "builder-app-toggle-copy",
          `for` = "build_show_upload_ui",
          tags$strong("Allow visitor uploads"),
          span("Visitors can open their own compatible data.")
        ),
        shiny::checkboxInput(
          "build_show_upload_ui",
          NULL,
          options$app$show_upload_ui
        )
      ),
      div(
        class = "builder-app-toggle",
        tags$label(
          class = "builder-app-toggle-copy",
          `for` = "build_require_login",
          tags$strong("Require login"),
          span("Restrict access to configured accounts.")
        ),
        require_login
      )
    ),
    if (!auth_available) {
      p(
        class = "hint builder-auth-dependency",
        auth$reason %||%
          "Login is unavailable. Install the required R package, then restart Builder."
      )
    },
    auth_controls,
    tags$details(
      class = "builder-app-connection-settings",
      tags$summary(
        span("Connection settings"),
        span(class = "builder-app-connection-summary", "Host · Port")
      ),
      div(
        class = "builder-app-network-fields",
        shiny::textInput("build_host", "Host", options$app$host),
        shiny::numericInput(
          "build_port",
          "Port",
          options$app$port,
          min = 1,
          max = 65535
        )
      )
    )
  )
}

.builder_result <- function(state, message = NULL, fields = list()) {
  if (
    !is.character(state) ||
      length(state) != 1L ||
      !state %in%
        c(
          "success",
          "needs_decision",
          "failure",
          "recovery_required"
        ) ||
      !is.list(fields)
  ) {
    stop("A supported typed build result is required.", call. = FALSE)
  }
  value <- c(list(state = state, message = message), fields)
  if (identical(state, "success")) {
    auth_enabled <- value$auth_enabled
    auth_env_file <- value$auth_env_file
    valid_enabled <- is.logical(auth_enabled) &&
      length(auth_enabled) == 1L &&
      !is.na(auth_enabled)
    valid_env <- is.null(auth_env_file) ||
      (is.character(auth_env_file) &&
        length(auth_env_file) == 1L &&
        !is.na(auth_env_file) &&
        nzchar(auth_env_file))
    if (
      !valid_enabled ||
        !valid_env ||
        (isTRUE(auth_enabled) && is.null(auth_env_file)) ||
        (!isTRUE(auth_enabled) && !is.null(auth_env_file))
    ) {
      stop("The result authentication fields are invalid.", call. = FALSE)
    }
  } else if (any(c("auth_enabled", "auth_env_file") %in% names(value))) {
    stop(
      "Only successful results may carry authentication fields.",
      call. = FALSE
    )
  }
  structure(
    value,
    class = c(paste0("builder_result_", state), "builder_result", "list")
  )
}

builder_result_success <- function(
  published = FALSE,
  built = character(),
  warnings = character(),
  app_dir = NULL,
  app_verified = FALSE,
  report_path = NULL,
  auth_enabled = FALSE,
  auth_env_file = NULL,
  ...
) {
  .builder_result(
    "success",
    fields = c(
      list(
        published = isTRUE(published),
        built = built,
        warnings = as.character(warnings),
        app_dir = app_dir,
        app_verified = isTRUE(app_verified),
        report_path = report_path,
        auth_enabled = auth_enabled,
        auth_env_file = auth_env_file
      ),
      list(...)
    )
  )
}

builder_result_needs_decision <- function(message, ...) {
  .builder_result("needs_decision", message, list(...))
}

builder_result_failure <- function(message, ...) {
  .builder_result("failure", message, c(list(error = message), list(...)))
}

builder_result_recovery_required <- function(message, ...) {
  .builder_result(
    "recovery_required",
    message,
    c(list(error = message), list(...))
  )
}

builder_result_recovery_followup <- function(result, message, recovery) {
  result <- if (is.list(result)) result else list()
  primary_message <- result$error %||% result$message %||% NULL
  if (!builder_stage_has_text(primary_message %||% "")) {
    primary_message <- message
  }
  builder_result_recovery_required(
    primary_message,
    recovery = recovery,
    recovery_message = message,
    cleanup_error = result$cleanup_error %||% NULL,
    recovery_error = result$recovery_error %||% NULL,
    settlement_error = result$settlement_error %||% NULL,
    diagnostics_path = result$diagnostics_path %||% NULL,
    failure_phase = result$failure_phase %||% NULL,
    failure_stage = result$failure_stage %||% NULL,
    failure_build_id = result$failure_build_id %||% NULL
  )
}

builder_release_error_result <- function(
  message,
  target,
  .recovery = builder_coordinator_recovery
) {
  recovery <- if (builder_stage_has_text(target %||% "")) {
    try(.recovery(target), silent = TRUE)
  } else {
    NULL
  }
  if (
    is.list(recovery) &&
      identical(recovery$state, "recovery_required")
  ) {
    return(builder_result_recovery_required(
      message,
      recovery = recovery,
      recovery_message = recovery$message %||% NULL
    ))
  }
  builder_result_failure(message)
}

builder_as_result <- function(result) {
  if (
    !is.list(result) ||
      !is.character(result$state) ||
      length(result$state) != 1L
  ) {
    stop("A typed build result is required.", call. = FALSE)
  }
  state <- result$state
  message <- result$error %||% result$message %||% NULL
  result$state <- NULL
  result$message <- NULL
  .builder_result(state, message, result)
}

builder_build_status_model <- function(result) {
  result <- builder_as_result(result)
  type <- result$state
  if (
    !type %in% c("success", "needs_decision", "failure", "recovery_required")
  ) {
    stop("The build result state is not supported.", call. = FALSE)
  }
  warnings <- as.character(result$warnings %||% character())
  primary_message <- result$error %||% result$message %||% NULL
  recovery <- result$recovery %||% NULL
  recovery_message <- result$recovery_message %||% NULL
  if (is.null(recovery_message) && is.list(recovery)) {
    recovery_message <- recovery$message %||% NULL
  }
  if (
    !builder_stage_has_text(recovery_message %||% "") ||
      identical(recovery_message, primary_message)
  ) {
    recovery_message <- NULL
  }
  model <- list(
    type = type,
    variant = if (identical(type, "success") && length(warnings)) {
      "warnings"
    } else {
      "default"
    },
    message = primary_message,
    warnings = warnings,
    built = as.character(result$built %||% character()),
    app_dir = if (
      identical(type, "success") &&
        isTRUE(result$published) &&
        isTRUE(result$app_verified) &&
        builder_stage_has_text(result$app_dir %||% "")
    ) {
      result$app_dir
    } else {
      NULL
    },
    release_dir = if (builder_stage_has_text(result$release$target %||% "")) {
      result$release$target
    } else if (builder_stage_has_text(result$app_dir %||% "")) {
      dirname(result$app_dir)
    } else if (length(result$built %||% character())) {
      dirname(result$built[[1L]])
    } else {
      NULL
    },
    auth_enabled = identical(type, "success") && isTRUE(result$auth_enabled),
    report_path = result$report_path %||% NULL,
    retry_closure = result$retry_closure %||% character(),
    failed_dataset_id = result$failed_dataset_id %||% NULL,
    restartable_worker = isTRUE(result$restartable_worker),
    recovery = recovery,
    recovery_message = recovery_message,
    cleanup_error = result$cleanup_error %||% NULL,
    recovery_error = result$recovery_error %||% NULL,
    settlement_error = result$settlement_error %||% NULL,
    failure_phase = if (
      builder_stage_has_text(result$failure_phase %||% "") &&
        result$failure_phase %in%
          c(
            "preflight",
            "stage",
            "dispatch",
            "datasets",
            "viewer",
            "verify",
            "publish"
          )
    ) {
      result$failure_phase
    } else {
      NULL
    },
    failure_stage = if (builder_stage_has_text(result$failure_stage %||% "")) {
      result$failure_stage
    } else {
      NULL
    },
    failure_build_id = if (
      builder_stage_has_text(result$failure_build_id %||% "")
    ) {
      result$failure_build_id
    } else {
      NULL
    },
    diagnostics_path = if (
      builder_stage_has_text(result$diagnostics_path %||% "")
    ) {
      result$diagnostics_path
    } else {
      NULL
    },
    failures = builder_build_text_lines(result$failures),
    failure_reasons = builder_build_failed_analysis_reasons(
      result$analysis_log
    ),
    analysis_log = builder_build_text_lines(result$analysis_log)
  )
  structure(model, class = c("builder_build_status", "list"))
}

builder_build_text_lines <- function(value) {
  value <- as.character(unlist(value %||% character(), use.names = FALSE))
  value[!is.na(value) & nzchar(trimws(value))]
}

## The analysis log marks a failed analysis with "✗"; its first line names the
## dataset, the analysis and the reason. Later lines are suggestions.
builder_build_failed_analysis_reasons <- function(log) {
  log <- builder_build_text_lines(log)
  failed <- log[grepl("\u2717", log, fixed = TRUE)]
  if (!length(failed)) {
    return(character())
  }
  reasons <- vapply(
    strsplit(failed, "\n", fixed = TRUE),
    function(lines) trimws(lines[[1L]]),
    character(1)
  )
  unique(sub("\\s*\u2717\\s*", " ", reasons))
}

.builder_open_app_child <- function(
  path,
  env_file,
  env_name,
  validate_database = NULL,
  run_app = shiny::runApp,
  port = NULL,
  host = NULL
) {
  if (!identical(env_name, "CEREBRO_AUTH_PASSPHRASE")) {
    stop("The authentication environment is invalid.", call. = FALSE)
  }
  if (!is.function(run_app)) {
    stop("The App launcher is invalid.", call. = FALSE)
  }
  if (is.null(validate_database)) {
    validate_database <- get0(
      ".viewerAuthValidateDatabase",
      mode = "function",
      inherits = TRUE
    )
    if (is.null(validate_database)) {
      validate_database <- tryCatch(
        utils::getFromNamespace(".viewerAuthValidateDatabase", "CerebroNexus"),
        error = function(error) NULL
      )
    }
  }
  if (!is.function(validate_database)) {
    stop("The authentication validator is invalid.", call. = FALSE)
  }
  path_is_link <- function(candidate) {
    link <- tryCatch(
      Sys.readlink(candidate),
      error = function(error) NA_character_
    )
    is.character(link) && length(link) == 1L && !is.na(link) && nzchar(link)
  }
  value <- NULL
  previous <- Sys.getenv(env_name, unset = NA_character_)
  installed <- FALSE
  on.exit(
    {
      if (installed) {
        if (is.na(previous)) {
          Sys.unsetenv(env_name)
        } else {
          do.call(Sys.setenv, stats::setNames(list(previous), env_name))
        }
      }
      value <- NULL
    },
    add = TRUE
  )
  if (!is.null(env_file)) {
    valid <- tryCatch(
      {
        app_dir <- normalizePath(path, winslash = "/", mustWork = TRUE)
        secret <- normalizePath(env_file, winslash = "/", mustWork = TRUE)
        env_info <- fs::file_info(env_file, fail = TRUE, follow = FALSE)
        if (
          !identical(secret, file.path(app_dir, "viewer-auth.env")) ||
            path_is_link(env_file) ||
            !identical(as.character(env_info$type), "file") ||
            !identical(as.double(env_info$hard_links), 1) ||
            (.Platform$OS.type != "windows" &&
              !identical(as.integer(file.info(env_file)$mode), 384L))
        ) {
          stop("invalid")
        }
        lines <- readLines(env_file, warn = FALSE, encoding = "UTF-8")
        pattern <- paste0("^", env_name, "=([0-9a-f]{64})$")
        if (length(lines) != 1L || !grepl(pattern, lines, perl = TRUE)) {
          stop("invalid")
        }
        value <- sub(paste0("^", env_name, "="), "", lines)
        database <- file.path(
          app_dir,
          "private-data",
          "auth",
          "credentials.sqlite"
        )
        if (
          path_is_link(database) ||
            !file.exists(database) ||
            dir.exists(database) ||
            !isTRUE(validate_database(database, value))
        ) {
          stop("invalid")
        }
        TRUE
      },
      error = function(error) FALSE,
      warning = function(warning) FALSE
    )
    if (!isTRUE(valid)) {
      value <- NULL
      stop("The authentication environment is invalid.", call. = FALSE)
    }
    do.call(Sys.setenv, stats::setNames(list(value), env_name))
    installed <- TRUE
  }
  ## The Builder chose the port and opens the URL in its own page's browser,
  ## which may not be on this machine.
  network <- Filter(Negate(is.null), list(port = port, host = host))
  do.call(run_app, c(list(path), network, list(launch.browser = FALSE)))
}

## Apps started by Open App, by App directory. processx kills a child process
## when its handle is garbage-collected, so every handle stays here until the
## Builder stops (builder_stop_opened_apps() is registered in
## builder_app_start()).
.builder_opened_apps <- new.env(parent = emptyenv())

## Host and port the App was built with (Review > Connection settings).
builder_app_run_options <- function(app_dir) {
  config_path <- file.path(app_dir, "cerebro_config.rds")
  config <- if (file.exists(config_path)) {
    tryCatch(readRDS(config_path), error = function(error) NULL)
  }
  options <- if (is.list(config)) {
    config[[".bundle_run_options"]]$shiny_app_options
  }
  port <- suppressWarnings(as.integer(options$port %||% NA_integer_))
  host <- options$host
  list(
    port = if (length(port) == 1L && !is.na(port) && port >= 1L) {
      port
    } else {
      8080L
    },
    host = if (builder_stage_has_text(host %||% "")) host else "127.0.0.1"
  )
}

builder_port_is_free <- function(host, port) {
  server <- tryCatch(
    suppressWarnings(httpuv::startServer(host, port, list())),
    error = function(error) NULL
  )
  if (is.null(server)) {
    return(FALSE)
  }
  httpuv::stopServer(server)
  TRUE
}

## The configured port when it is free, otherwise a free one.
builder_app_free_port <- function(host, port) {
  if (builder_port_is_free(host, port)) {
    return(as.integer(port))
  }
  as.integer(httpuv::randomPort(host = host))
}

## Changes when the App is rebuilt in place.
builder_app_identity <- function(app_dir) {
  info <- file.info(file.path(app_dir, "cerebro_config.rds"))
  paste(info$size, as.numeric(info$mtime))
}

## The running App started for this directory, or NULL. A stopped App, or one
## started before the App was rebuilt, is dropped.
builder_opened_app <- function(app_dir) {
  app <- .builder_opened_apps[[app_dir]]
  if (is.null(app)) {
    return(NULL)
  }
  if (
    isTRUE(app$process$is_alive()) &&
      identical(app$identity, builder_app_identity(app_dir))
  ) {
    return(app)
  }
  builder_stop_opened_app(app)
  rm(list = app_dir, envir = .builder_opened_apps)
  NULL
}

builder_stop_opened_app <- function(app) {
  if (isTRUE(app$process$is_alive())) {
    tryCatch(app$process$kill_tree(), error = function(error) {
      try(app$process$kill(), silent = TRUE)
    })
  }
  invisible(TRUE)
}

builder_stop_opened_apps <- function() {
  for (app_dir in ls(.builder_opened_apps, all.names = TRUE)) {
    builder_stop_opened_app(.builder_opened_apps[[app_dir]])
  }
  rm(
    list = ls(.builder_opened_apps, all.names = TRUE),
    envir = .builder_opened_apps
  )
  invisible(TRUE)
}

## "ready" once the App answers on its port, "stopped" when its process has
## ended, otherwise "starting".
builder_opened_app_state <- function(app) {
  if (!isTRUE(app$process$is_alive())) {
    return("stopped")
  }
  host <- if (app$host %in% c("0.0.0.0", "::", "")) "127.0.0.1" else app$host
  connected <- tryCatch(
    {
      connection <- suppressWarnings(
        socketConnection(host, app$port, open = "r+", timeout = 1)
      )
      close(connection)
      TRUE
    },
    error = function(error) FALSE
  )
  if (connected) "ready" else "starting"
}

## The last lines the App printed, for an App that stopped while starting.
builder_opened_app_log <- function(app, lines = 5L) {
  text <- tryCatch(
    readLines(app$log, warn = FALSE),
    error = function(error) character()
  )
  text <- trimws(text[nzchar(trimws(text))])
  utils::tail(text, lines)
}

## What the page needs to open the App: the host is left out when the App
## listens on every address, so the page uses the host it reached Builder by.
builder_opened_app_message <- function(app, state) {
  list(
    ok = identical(state, "ready"),
    state = state,
    port = app$port,
    host = if (app$host %in% c("0.0.0.0", "::", "")) NULL else app$host,
    configured_port = app$configured_port
  )
}

builder_open_final_app <- function(
  result,
  .verify_auth = function(app_dir, env_file) {
    builder_auth_verify_database_pair(
      file.path(app_dir, "private-data", "auth", "credentials.sqlite"),
      env_file
    )
  },
  .child = .builder_open_app_child,
  .validate_database = NULL,
  .run_app = NULL,
  .on_open = NULL,
  .open = function(path, env_file) {
    app <- builder_opened_app(path)
    if (is.null(app)) {
      if (!requireNamespace("callr", quietly = TRUE)) {
        stop("The callr package is required to open the App.", call. = FALSE)
      }
      run_options <- builder_app_run_options(path)
      port <- builder_app_free_port(run_options$host, run_options$port)
      args <- list(
        path = path,
        env_file = env_file,
        env_name = "CEREBRO_AUTH_PASSPHRASE",
        validate_database = .validate_database,
        port = port,
        host = run_options$host
      )
      environment(args$validate_database) <- globalenv()
      if (!is.null(.run_app)) {
        args$run_app <- .run_app
      }
      log <- tempfile("cerebro-app-", fileext = ".log")
      process <- callr::r_bg(
        .child,
        args = args,
        supervise = TRUE,
        stdout = log,
        stderr = "2>&1"
      )
      app <- list(
        process = process,
        app_dir = path,
        identity = builder_app_identity(path),
        host = run_options$host,
        port = port,
        configured_port = run_options$port,
        log = log
      )
      assign(path, app, envir = .builder_opened_apps)
    }
    if (is.function(.on_open)) {
      .on_open(app)
    }
    TRUE
  }
) {
  if (is.null(.validate_database)) {
    .validate_database <- get0(
      ".viewerAuthValidateDatabase",
      mode = "function",
      inherits = TRUE
    )
    if (is.null(.validate_database)) {
      .validate_database <- tryCatch(
        utils::getFromNamespace(".viewerAuthValidateDatabase", "CerebroNexus"),
        error = function(error) NULL
      )
    }
  }
  if (!is.function(.validate_database)) {
    stop("The authentication validator is invalid.", call. = FALSE)
  }
  result <- builder_as_result(result)
  if (
    !identical(result$state, "success") ||
      !isTRUE(result$published) ||
      !isTRUE(result$app_verified) ||
      !builder_stage_has_text(result$app_dir %||% "")
  ) {
    stop("A verified final App directory is required.", call. = FALSE)
  }
  app_dir <- normalizePath(result$app_dir, winslash = "/", mustWork = TRUE)
  env_file <- NULL
  if (isTRUE(result$auth_enabled)) {
    valid <- tryCatch(
      {
        env_file <- normalizePath(
          result$auth_env_file,
          winslash = "/",
          mustWork = TRUE
        )
        identical(env_file, file.path(app_dir, "viewer-auth.env")) &&
          isTRUE(.verify_auth(app_dir, env_file))
      },
      error = function(error) FALSE
    )
    if (!isTRUE(valid)) {
      stop(
        paste(
          "Authentication files are incomplete; rebuild the App",
          "or restore its matching viewer-auth.env."
        ),
        call. = FALSE
      )
    }
  }
  isTRUE(.open(app_dir, env_file))
}

builder_reveal_release <- function(result, .reveal = NULL) {
  result <- builder_as_result(result)
  model <- builder_build_status_model(result)
  if (!builder_stage_has_text(model$release_dir %||% "")) {
    stop("A final release directory is required.", call. = FALSE)
  }
  if (is.null(.reveal)) {
    .reveal <- function(path) {
      if (identical(Sys.info()[["sysname"]], "Darwin")) {
        identical(system2("open", path, stdout = FALSE, stderr = FALSE), 0L)
      } else if (.Platform$OS.type == "windows") {
        shell.exec(path)
        TRUE
      } else {
        identical(system2("xdg-open", path, stdout = FALSE, stderr = FALSE), 0L)
      }
    }
  }
  isTRUE(.reveal(model$release_dir))
}

builder_copy_result_path <- function(result, kind, .copy) {
  result <- builder_as_result(result)
  model <- builder_build_status_model(result)
  value <- switch(
    kind,
    release = model$release_dir,
    report = model$report_path,
    app = model$app_dir,
    NULL
  )
  if (!builder_stage_has_text(value %||% "")) {
    stop("The requested final result path is unavailable.", call. = FALSE)
  }
  isTRUE(.copy(value))
}

builder_build_pipeline_ui <- function(state) {
  states <- c(
    queued = "Queued",
    building = "Building",
    complete = "Complete",
    failure = "Failed"
  )
  if (
    !is.character(state) ||
      length(state) != 1L ||
      !state %in% names(states)
  ) {
    stop("A server-known build pipeline state is required.", call. = FALSE)
  }
  div(
    class = "builder-build-pipeline pipeline",
    `data-pipeline-state` = state,
    `aria-label` = paste("Build status:", states[[state]]),
    lapply(names(states), function(step) {
      div(
        class = "pipeline-step",
        `data-step` = step,
        span(class = "pipeline-light", `aria-hidden` = "true"),
        span(states[[step]])
      )
    })
  )
}

builder_build_stage_status_model <- function(
  flow,
  protocol,
  note,
  result,
  finalizing = FALSE,
  last_output_parent = NULL
) {
  flow_valid <-
    is.list(flow) &&
    is.character(flow$stage) &&
    length(flow$stage) == 1L &&
    !is.na(flow$stage)
  flow_stage <- if (flow_valid) {
    flow$stage
  } else {
    "idle"
  }
  build_status <- if (
    is.list(protocol) &&
      is.character(protocol$build_status) &&
      length(protocol$build_status) == 1L &&
      !is.na(protocol$build_status)
  ) {
    protocol$build_status
  } else {
    "idle"
  }
  state <- if (!is.null(result)) {
    "result"
  } else if (isTRUE(finalizing)) {
    "building"
  } else if (identical(flow_stage, "preparing")) {
    "preparing"
  } else if (identical(build_status, "queued")) {
    "queued"
  } else if (build_status %in% c("running", "cancelling")) {
    "building"
  } else if (identical(flow_stage, "checking_folder")) {
    "checking_folder"
  } else if (identical(flow_stage, "choosing_folder")) {
    "choosing_folder"
  } else {
    "ready"
  }
  message <- if (
    is.character(note) &&
      length(note) == 1L &&
      !is.na(note)
  ) {
    note
  } else {
    NULL
  }
  protocol_ready <- isTRUE(tryCatch(
    builder_protocol_is_quiescent(protocol),
    error = function(error) FALSE
  ))
  list(
    state = state,
    message = message,
    pipeline_state = switch(
      state,
      queued = "queued",
      building = "building",
      NULL
    ),
    can_build = identical(state, "ready") &&
      flow_valid &&
      identical(flow_stage, "idle") &&
      protocol_ready,
    last_output_parent = if (
      builder_stage_has_text(last_output_parent %||% "")
    ) {
      last_output_parent
    } else {
      NULL
    },
    result_model = if (is.null(result)) {
      NULL
    } else {
      builder_build_status_model(result)
    }
  )
}

## Live dataset/step line for the Build progress panel. The worker writes the
## detail record; the browser keeps the elapsed clock running between polls.
builder_build_progress_message <- function(detail, now = Sys.time()) {
  if (!is.list(detail) || !builder_stage_has_text(detail$phase %||% "")) {
    return(list(active = FALSE))
  }
  phase <- detail$phase
  index <- detail$dataset_index %||% NULL
  count <- detail$dataset_count %||% NULL
  name <- detail$dataset_name %||% NULL
  step <- detail$step_label %||% NULL
  parts <- switch(
    phase,
    datasets = c(
      if (is.numeric(index) && is.numeric(count) && count > 0L) {
        paste0("Dataset ", index, "/", count)
      },
      if (builder_stage_has_text(name %||% "")) name,
      if (builder_stage_has_text(step %||% "")) step
    ),
    viewer = "Packaging Viewer",
    verify = c(
      "Verifying output",
      if (builder_stage_has_text(step %||% "")) {
        step_index <- detail$step_index %||% NULL
        step_count <- detail$step_count %||% NULL
        if (
          is.numeric(step_index) &&
            is.numeric(step_count) &&
            step_count > 0L
        ) {
          paste0(step, " ", step_index, "/", step_count)
        } else {
          step
        }
      }
    ),
    publish = "Publishing release",
    character()
  )
  started <- suppressWarnings(as.numeric(detail$started_at %||% NA_real_))
  elapsed <- as.numeric(now) - started
  measured <- builder_build_progress_measure(detail)
  list(
    active = TRUE,
    phase = phase,
    text = paste(parts, collapse = " · "),
    elapsed_seconds = if (length(elapsed) == 1L && is.finite(elapsed)) {
      max(0, round(elapsed, 1))
    } else {
      NULL
    },
    percent = measured$percent,
    percent_text = measured$text
  )
}

## The share of the current step that is counted, as a whole percent: the
## datasets already built plus the finished steps of the one being written
## (opening, preparing, each analysis, each part of the CRB export, verifying),
## or the items a verification pass has finished. The steps are counted, not
## timed: a step that takes longer still moves the bar by one step. Steps
## without such a count (packaging the Viewer) get no percent.
builder_build_progress_measure <- function(detail) {
  whole <- function(value) {
    value <- suppressWarnings(as.numeric(value))
    length(value) == 1L && is.finite(value) && value == floor(value)
  }
  none <- list(percent = NULL, text = NULL)
  if (!is.list(detail)) {
    return(none)
  }
  if (identical(detail$phase, "datasets")) {
    index <- detail$dataset_index %||% NULL
    count <- detail$dataset_count %||% NULL
    steps_done <- detail$steps_done %||% NULL
    steps_total <- detail$steps_total %||% NULL
    stepped <- whole(steps_done) && whole(steps_total) && steps_total >= 1
    if (!whole(index) || !whole(count) || count < 1 || index < 1) {
      return(none)
    }
    if (count < 2 && !stepped) {
      return(none)
    }
    done <- min(index, count) - 1
    steps_done <- if (stepped) min(steps_done, steps_total) else 0
    share <- if (stepped) steps_done / steps_total else 0
    return(list(
      percent = as.integer(floor(100 * (done + share) / count)),
      text = paste(
        c(
          if (count >= 2) paste0(done, " of ", count, " datasets built"),
          if (stepped) {
            paste0(
              steps_done,
              " of ",
              steps_total,
              if (steps_total == 1) " step done" else " steps done"
            )
          }
        ),
        collapse = " · "
      )
    ))
  }
  if (identical(detail$phase, "verify")) {
    index <- detail$step_index %||% NULL
    count <- detail$step_count %||% NULL
    if (!whole(index) || !whole(count) || count < 1 || index < 0) {
      return(none)
    }
    ## A pass names each item before it checks it.
    done <- max(0, min(index, count) - 1)
    return(list(
      percent = as.integer(floor(100 * done / count)),
      text = paste0(done, " of ", count, " checked")
    ))
  }
  none
}

## `step = TRUE` adds the "Step k/N" label the Build card shows; the
## operation overlay names the step in its own header. The meter appears only
## while the current step reports a count (builder_build_progress_measure()).
builder_build_progress_detail_ui <- function(id = NULL, step = FALSE) {
  tags$p(
    id = id,
    class = "builder-build-progress-detail",
    `data-build-progress-detail` = "true",
    hidden = "hidden",
    if (isTRUE(step)) {
      tags$span(class = "builder-build-progress-step", hidden = "hidden")
    },
    tags$span(
      class = "builder-build-progress-text",
      role = "status",
      `aria-live` = "polite"
    ),
    # A ticking clock is visible only; announcing it every second is noise.
    tags$span(class = "builder-build-progress-elapsed", `aria-hidden` = "true"),
    tags$span(
      class = "builder-build-progress-measure",
      hidden = "hidden",
      tags$progress(
        class = "builder-build-progress-meter",
        max = "100",
        value = "0",
        `aria-label` = "Progress of the current build step"
      ),
      # The meter already exposes its value; the number is for sighted users.
      tags$span(
        class = "builder-build-progress-percent",
        `aria-hidden` = "true"
      )
    )
  )
}

builder_build_operation_overlay_model <- function(
  flow,
  protocol,
  note,
  result,
  phase = NULL,
  can_stop = FALSE
) {
  model <- builder_build_stage_status_model(
    flow = flow,
    protocol = protocol,
    note = note,
    result = result
  )
  active <- model$state %in%
    c(
      "choosing_folder",
      "checking_folder",
      "preparing",
      "queued",
      "building"
    )
  supported_phases <- c(
    "preflight",
    "stage",
    "dispatch",
    "datasets",
    "viewer",
    "verify",
    "publish"
  )
  reported_phase <- if (
    is.character(phase) &&
      length(phase) == 1L &&
      !is.na(phase) &&
      phase %in% supported_phases
  ) {
    phase
  } else {
    NULL
  }
  picker_waiting <- identical(model$state, "choosing_folder")
  phase <- if (!active || picker_waiting) {
    NULL
  } else if (!is.null(reported_phase)) {
    reported_phase
  } else if (model$state %in% c("checking_folder", "preparing")) {
    "preflight"
  } else if (grepl("Publishing release", model$message %||% "", fixed = TRUE)) {
    "publish"
  } else if (grepl("Verifying", model$message %||% "", fixed = TRUE)) {
    "verify"
  } else if (grepl("Packaging Viewer", model$message %||% "", fixed = TRUE)) {
    "viewer"
  } else if (identical(model$state, "queued")) {
    "stage"
  } else {
    "datasets"
  }
  list(
    active = active,
    picker_waiting = picker_waiting,
    phase = phase,
    cancelable = active &&
      !picker_waiting &&
      isTRUE(can_stop) &&
      phase %in%
        c("stage", "dispatch", "datasets", "viewer", "verify", "publish"),
    title = if (picker_waiting) {
      "Choose output folder"
    } else if (active) {
      "Building output"
    } else {
      NULL
    },
    message = if (picker_waiting) {
      "Choose a folder, or enter its path."
    } else if (active) {
      "Keep this page open."
    } else {
      NULL
    },
    detail = if (active) {
      switch(
        model$state,
        checking_folder = "Checking the selected output folder…",
        preparing = "Preparing build…",
        queued = model$message %||% "Build queued…",
        building = model$message %||% "Building output…"
      )
    } else {
      NULL
    }
  )
}

builder_build_stage_status_validate <- function(model) {
  if (
    !is.list(model) ||
      !is.character(model$state) ||
      length(model$state) != 1L ||
      is.na(model$state)
  ) {
    stop("A Build-stage status model is required.", call. = FALSE)
  }
  invisible(model)
}

builder_build_was_stopped <- function(model) {
  identical(model$type, "failure") &&
    is.character(model$message) &&
    length(model$message) == 1L &&
    !is.na(model$message) &&
    startsWith(model$message, "Build stopped by the user")
}

builder_build_stage_status_label <- function(model) {
  builder_build_stage_status_validate(model)
  switch(
    model$state,
    ready = if (isTRUE(model$can_build)) {
      "Ready to build"
    } else {
      model$message %||% ""
    },
    choosing_folder = "Choosing release parent…",
    checking_folder = "Checking output folder…",
    preparing = "Preparing build…",
    queued = "Build queued",
    building = "Build in progress",
    result = switch(
      model$result_model$type %||% "",
      success = "Build complete",
      needs_decision = "Build needs a decision",
      failure = if (builder_build_was_stopped(model$result_model)) {
        "Build stopped"
      } else {
        "Build failed"
      },
      recovery_required = "Build failed",
      "Build finished"
    ),
    stop("The Build-stage status is unsupported.", call. = FALSE)
  )
}

builder_build_stage_primary_action_ui <- function(
  model,
  controls_disabled = FALSE
) {
  builder_build_stage_status_validate(model)
  repeatable <- identical(model$state, "result") &&
    model$result_model$type %in% c("failure", "success")
  if (!identical(model$state, "ready") && !isTRUE(repeatable)) {
    return(NULL)
  }
  same_folder <- if (
    identical(model$state, "result") &&
      identical(model$result_model$type, "success") &&
      builder_stage_has_text(model$last_output_parent %||% "")
  ) {
    actionButton(
      "build_same_folder",
      "Build again to same folder",
      class = "btn",
      title = model$last_output_parent,
      disabled = isTRUE(controls_disabled)
    )
  }
  primary <- actionButton(
    "build",
    if (isTRUE(repeatable)) {
      if (identical(model$result_model$type, "failure")) {
        "Retry Build"
      } else {
        "Build again"
      }
    } else {
      "Build"
    },
    class = "btn btn-action",
    disabled = isTRUE(controls_disabled) ||
      (!isTRUE(repeatable) && !isTRUE(model$can_build))
  )
  if (is.null(same_folder)) primary else tagList(same_folder, primary)
}

builder_build_stage_status_body_ui <- function(model) {
  builder_build_stage_status_validate(model)
  switch(
    model$state,
    ready = if (
      !isTRUE(model$can_build) &&
        identical(model$message, "The background worker is not ready.")
    ) {
      div(
        class = "builder-build-waiting builder-build-blocked",
        span(model$message)
      )
    } else {
      NULL
    },
    choosing_folder = div(
      class = "builder-build-waiting",
      span(class = "spinner"),
      span("Choosing a parent folder for the new release…")
    ),
    checking_folder = div(
      class = "builder-build-waiting",
      span(class = "spinner"),
      span("Checking output folder…")
    ),
    preparing = div(
      class = "builder-build-waiting",
      span(class = "spinner"),
      span("Preparing build…")
    ),
    queued = tagList(
      builder_build_pipeline_ui("queued"),
      p(model$message %||% "Build queued…"),
      builder_build_progress_detail_ui(step = TRUE)
    ),
    building = tagList(
      builder_build_pipeline_ui("building"),
      p(model$message %||% "Building Viewer…"),
      builder_build_progress_detail_ui(step = TRUE)
    ),
    result = tagList(
      builder_build_status_ui(model$result_model),
      if (builder_stage_has_text(model$message %||% "")) {
        div(
          class = "builder-build-waiting builder-build-post-processing",
          span(class = "spinner"),
          span(model$message)
        )
      }
    ),
    stop("The Build-stage status is unsupported.", call. = FALSE)
  )
}

builder_build_status_ui <- function(model) {
  if (inherits(model, "builder_result")) {
    model <- builder_build_status_model(model)
  }
  stopifnot(inherits(model, "builder_build_status"))
  stopped <- builder_build_was_stopped(model)
  title <- switch(
    model$type,
    success = "Build complete",
    needs_decision = "Build needs a decision",
    failure = if (stopped) "Build stopped" else "Build failed",
    recovery_required = "Build failed"
  )
  icon <- switch(
    model$type,
    success = "check",
    needs_decision = "question",
    failure = "error",
    recovery_required = "recovery"
  )
  pipeline_state <- switch(
    model$type,
    success = "complete",
    failure = "failure",
    recovery_required = "failure",
    NULL
  )
  div(
    class = paste(
      "card result-card",
      model$type,
      model$variant,
      paste0(
        "is-",
        if (identical(model$type, "recovery_required")) {
          "recovery"
        } else {
          model$type
        }
      )
    ),
    h3(`data-icon` = icon, title),
    if (!is.null(pipeline_state) && !stopped) {
      builder_build_pipeline_ui(pipeline_state)
    },
    if (stopped) {
      p("Nothing was published. Retry when ready.")
    } else if (!is.null(model$message)) {
      p(model$message)
    },
    if (
      model$type %in%
        c("failure", "recovery_required") &&
        builder_stage_has_text(model$failure_phase %||% "")
    ) {
      phase_labels <- c(
        preflight = "release check",
        stage = "safe-stage creation",
        dispatch = "worker startup",
        datasets = "dataset build",
        viewer = "Viewer packaging",
        verify = "output verification",
        publish = "atomic publication"
      )
      p(
        class = "hint builder-failure-context",
        if (stopped) "Stopped during " else "Failed during ",
        strong(phase_labels[[model$failure_phase]] %||% model$failure_phase)
      )
    },
    if (
      model$type %in%
        c("failure", "recovery_required") &&
        builder_stage_has_text(model$failure_stage %||% "")
    ) {
      tags$details(
        tags$summary("Details"),
        if (stopped && !is.null(model$message)) p(model$message),
        p("Build ID: ", code(model$failure_build_id %||% "")),
        p(
          class = "hint builder-failure-stage",
          "Temporary output: ",
          code(model$failure_stage)
        )
      )
    },
    if (
      model$type %in%
        c("failure", "recovery_required") &&
        builder_stage_has_text(model$diagnostics_path %||% "")
    ) {
      tags$details(
        tags$summary("Diagnostic log"),
        p(class = "hint builder-diagnostics-path", code(model$diagnostics_path))
      )
    },
    if (
      model$type %in%
        c("failure", "recovery_required") &&
        builder_stage_has_text(model$settlement_error %||% "")
    ) {
      p(
        class = "hint builder-recovery-detail",
        paste0(
          "Release settlement also failed: ",
          model$settlement_error
        )
      )
    },
    if (
      identical(model$type, "failure") &&
        builder_stage_has_text(model$cleanup_error %||% "")
    ) {
      p(
        class = "hint builder-recovery-detail",
        paste0("Release cleanup also failed: ", model$cleanup_error)
      )
    },
    if (
      identical(model$type, "failure") &&
        builder_stage_has_text(model$recovery_error %||% "")
    ) {
      p(
        class = "hint builder-recovery-detail",
        paste0("Recovery status check also failed: ", model$recovery_error)
      )
    },
    if (identical(model$type, "success")) {
      p(
        class = "builder-auth-status",
        paste(
          "Login:",
          if (isTRUE(model$auth_enabled)) "Required" else "Not required"
        )
      )
    },
    if (identical(model$type, "needs_decision")) {
      reasons <- if (length(model$failure_reasons)) {
        model$failure_reasons
      } else {
        model$failures
      }
      div(
        class = "builder-recovery-action",
        if (length(reasons)) {
          tags$ul(
            class = "builder-failure-reasons",
            lapply(reasons, tags$li)
          )
        },
        if (length(model$analysis_log)) {
          tags$details(
            class = "builder-failure-log",
            tags$summary("Analysis log"),
            tags$pre(paste(model$analysis_log, collapse = "\n"))
          )
        },
        p(
          "Retry the failed optional work, or remove it, review, and build again."
        ),
        actionButton(
          "retry_failed_analysis",
          "Retry optional work",
          class = "btn btn-primary"
        ),
        if (builder_stage_has_text(model$failed_dataset_id %||% "")) {
          actionButton(
            "remove_failed_analysis",
            "Remove and review",
            class = "btn btn-quiet"
          )
        }
      )
    },
    if (identical(model$type, "failure") && isTRUE(model$restartable_worker)) {
      div(
        class = "builder-recovery-action",
        p(
          "Settings saved. Restart the worker, then retry."
        ),
        actionButton(
          "restart_worker",
          "Restart worker",
          class = "btn btn-primary"
        )
      )
    },
    if (identical(model$type, "recovery_required")) {
      recovery <- model$recovery
      phase_code <- if (
        is.list(recovery) &&
          builder_stage_has_text(recovery$phase %||% "")
      ) {
        recovery$phase
      } else {
        "unknown"
      }
      phase <- switch(
        phase_code,
        prepared = "preparing the release",
        locked = "checking the existing output",
        old_moving = "protecting the previous release",
        old_moving_in_place = "protecting the previous release",
        old_moved = "installing the new release",
        new_moving_in_place = "installing the new release",
        new_published = "finalizing the new release",
        recovery_required = "restoring the previous release",
        recovering_restore = "restoring the previous release",
        recovering_abort = "clearing the interrupted release",
        "an unknown publication step"
      )
      backup_exists <- is.list(recovery) && isTRUE(recovery$backup_exists)
      recovery_action <- builder_release_recovery_action(recovery)
      can_recover <- is.list(recovery) &&
        builder_stage_has_text(recovery$target %||% "") &&
        !is.null(recovery_action)
      div(
        class = "builder-recovery-action",
        h4("Recover interrupted release"),
        if (!is.null(model$recovery_message)) {
          p(class = "builder-recovery-summary", model$recovery_message)
        },
        if (builder_stage_has_text(model$cleanup_error %||% "")) {
          p(
            class = "hint builder-recovery-detail",
            paste0("Release cleanup also failed: ", model$cleanup_error)
          )
        },
        if (builder_stage_has_text(model$recovery_error %||% "")) {
          p(
            class = "hint builder-recovery-detail",
            paste0("Recovery status check also failed: ", model$recovery_error)
          )
        },
        p(
          paste0(
            "The earlier release stopped while ",
            phase,
            if (!identical(phase_code, "unknown")) {
              paste0(" (", phase_code, ")")
            } else {
              ""
            },
            "."
          )
        ),
        if (identical(recovery_action, "restore")) {
          p("The previous release is preserved and can be restored safely.")
        } else if (identical(recovery_action, "abort")) {
          p(paste(
            "Builder will verify that the existing output is unchanged",
            "before clearing the interrupted release."
          ))
        } else {
          p(paste(
            "Builder cannot recover this release automatically.",
            "Inspect the output and release-control directory manually."
          ))
        },
        p(
          class = "hint",
          "Close any running App or other process using this output before recovery."
        ),
        if (can_recover) {
          actionButton(
            "recover_builder_release",
            if (identical(recovery_action, "restore")) {
              "Restore previous release"
            } else {
              "Clear interrupted release"
            },
            class = "btn btn-primary"
          )
        }
      )
    },
    if (
      !is.null(model$app_dir) ||
        !is.null(model$release_dir) ||
        !is.null(model$report_path)
    ) {
      div(
        class = "builder-result-actions",
        if (!is.null(model$app_dir)) {
          actionButton("open_app", "Open App", class = "btn btn-primary")
        },
        if (!is.null(model$release_dir)) {
          actionButton(
            "reveal_folder",
            "Reveal Folder",
            class = "btn btn-quiet",
            `data-path` = model$release_dir
          )
        },
        if (!is.null(model$release_dir)) {
          actionButton(
            "copy_path",
            "Copy Path",
            class = "btn btn-quiet",
            `data-path` = model$release_dir
          )
        },
        if (!is.null(model$report_path)) {
          actionButton(
            "copy_report",
            "Copy Report",
            class = "btn btn-quiet",
            `data-report` = model$report_path
          )
        },
        if (!is.null(model$app_dir)) {
          ## builder.js shows the address once Open App has started the App.
          p(
            id = "builder-open-app-url",
            class = "hint builder-open-app-url",
            role = "status",
            hidden = NA
          )
        }
      )
    },
    if (length(model$warnings) || length(model$built)) {
      tags$details(
        class = "builder-result-details",
        open = if (!identical(model$type, "success")) "open",
        tags$summary("Build details"),
        builder_stage_text_items(model$warnings),
        if (length(model$built)) {
          builder_stage_text_items(basename(model$built))
        }
      )
    }
  )
}
