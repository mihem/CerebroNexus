.viewer_auth_error <- function(message) {
  stop(message, call. = FALSE)
}

.viewer_auth_validate_config <- function(config) {
  expected <- c(
    "credentials_path",
    "passphrase_env",
    "timeout_minutes"
  )
  valid <- is.list(config) &&
    identical(names(config), expected) &&
    identical(
      config$credentials_path,
      "private-data/auth/credentials.sqlite"
    ) &&
    is.character(config$passphrase_env) &&
    length(config$passphrase_env) == 1L &&
    !is.na(config$passphrase_env) &&
    grepl("^[A-Za-z_][A-Za-z0-9_]*$", config$passphrase_env) &&
    is.integer(config$timeout_minutes) &&
    length(config$timeout_minutes) == 1L &&
    !is.na(config$timeout_minutes) &&
    config$timeout_minutes >= 1L &&
    config$timeout_minutes <= 1440L
  if (!valid) {
    .viewer_auth_error("Invalid Viewer authentication configuration.")
  }
  config
}

.viewer_auth_require_provider <- function() {
  available <- requireNamespace("shinymanager", quietly = TRUE)
  version <- if (available) {
    tryCatch(
      utils::packageVersion("shinymanager"),
      error = function(condition) NULL
    )
  } else {
    NULL
  }
  if (is.null(version) || version < "1.1.0") {
    .viewer_auth_error(
      "Authentication requires shinymanager (>= 1.1.0)."
    )
  }
}

.viewer_auth_brand <- function(cerebro_root) {
  www <- file.path(cerebro_root, "viewer", "www")
  css <- file.path(www, "auth.css")
  logo <- file.path(www, "cerebronexus.svg")
  available <- isTRUE(utils::file_test("-f", css)) &&
    isTRUE(utils::file_test("-f", logo))
  if (!available) {
    .viewer_auth_error("Authentication branding assets are unavailable.")
  }
  svg <- paste(readLines(logo, warn = FALSE), collapse = "\n")
  list(
    head = shiny::includeCSS(css),
    top = shiny::tags$div(
      class = "cerebro-auth-brand",
      shiny::HTML(svg),
      shiny::tags$div(
        class = "cerebro-auth-eyebrow",
        "Secure viewer"
      )
    ),
    bottom = shiny::tags$div(
      class = "cerebro-auth-footer",
      "Protected access"
    )
  )
}

viewer_auth_apply <- function(ui, server, config, cerebro_root = ".") {
  if (is.null(config)) {
    return(list(ui = ui, server = server))
  }

  config <- .viewer_auth_validate_config(config)
  root <- tryCatch(
    normalizePath(cerebro_root, winslash = "/", mustWork = TRUE),
    error = function(condition) NULL
  )
  database <- if (is.null(root)) {
    NULL
  } else {
    tryCatch(
      normalizePath(
        file.path(root, config$credentials_path),
        winslash = "/",
        mustWork = TRUE
      ),
      error = function(condition) NULL
    )
  }
  accessible <- !is.null(database) &&
    isTRUE(utils::file_test("-f", database)) &&
    isTRUE(file.access(database, mode = 4L) == 0L) &&
    isTRUE(file.access(dirname(database), mode = 1L) == 0L)
  if (!accessible) {
    .viewer_auth_error(
      "Authentication credentials database is not accessible."
    )
  }

  passphrase <- Sys.getenv(config$passphrase_env, unset = NA_character_)
  on.exit(passphrase <- NULL, add = TRUE)
  if (
    length(passphrase) != 1L ||
      is.na(passphrase) ||
      !nzchar(passphrase)
  ) {
    .viewer_auth_error(paste0(config$passphrase_env, " is not set."))
  }

  .viewer_auth_require_provider()
  credentials <- suppressWarnings(suppressMessages(tryCatch(
    shinymanager::read_db_decrypt(
      conn = database,
      name = "credentials",
      passphrase = passphrase
    ),
    error = function(condition) NULL
  )))
  checker <- if (is.data.frame(credentials)) {
    suppressWarnings(suppressMessages(tryCatch(
      shinymanager::check_credentials(credentials),
      error = function(condition) NULL
    )))
  } else {
    NULL
  }
  passphrase <- NULL
  credentials <- NULL
  if (!is.function(checker)) {
    .viewer_auth_error(
      "Authentication database or passphrase is invalid."
    )
  }
  brand <- .viewer_auth_brand(root)

  secured_server <- function(input, output, session) {
    auth <- shinymanager::secure_server(
      check_credentials = checker,
      timeout = config$timeout_minutes,
      keep_token = FALSE,
      session = session
    )
    started <- shiny::reactiveVal(FALSE)
    revoked <- shiny::reactiveVal(FALSE)
    subject <- shiny::reactiveVal(NULL)
    last_activity <- shiny::reactiveVal(NULL)
    timeout_ms <- config$timeout_minutes * 60 * 1000
    now_ms <- function() {
      if (is.function(session$.now)) {
        return(as.numeric(session$.now()))
      }
      as.numeric(Sys.time()) * 1000
    }
    revoke <- function() {
      if (started() && !revoked()) {
        revoked(TRUE)
        session$reload()
        session$onFlushed(function() session$close(), once = TRUE)
      }
      invisible(NULL)
    }
    shiny::observe({
      user <- auth$user
      authorized <- is.character(user) &&
        length(user) == 1L &&
        !is.na(user) &&
        nzchar(user)
      if (authorized && !started()) {
        started(TRUE)
        subject(user)
        last_activity(now_ms())
        server(input, output, session)
      } else if (
        started() &&
          (!authorized || !identical(user, subject()))
      ) {
        revoke()
      }
    })
    shiny::observeEvent(
      input$.shinymanager_logout,
      {
        revoke()
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$.shinymanager_timeout,
      {
        if (started() && !revoked()) {
          current <- now_ms()
          if (current - last_activity() > timeout_ms) {
            revoke()
          } else {
            last_activity(current)
          }
        }
      },
      ignoreInit = TRUE
    )
    shiny::observe({
      last <- last_activity()
      shiny::req(!is.null(last), started(), !revoked())
      remaining <- timeout_ms - (now_ms() - last)
      if (remaining < 0) {
        revoke()
      } else {
        shiny::invalidateLater(as.integer(remaining) + 1L, session)
      }
    })
    invisible(auth)
  }

  secured_ui <- shinymanager::secure_app(
    ui,
    enable_admin = FALSE,
    head_auth = brand$head,
    tags_top = brand$top,
    tags_bottom = brand$bottom
  )
  list(
    ui = secured_ui,
    server = secured_server
  )
}
