.viewerAuthRequireProvider <- function() {
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
    stop(
      "Authentication requires shinymanager (>= 1.1.0).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.viewerAuthValidateDatabase <- function(path, passphrase) {
  table_names <- c("credentials", "pwd_mngt", "logs")
  tables <- setNames(
    lapply(table_names, function(name) {
      suppressWarnings(suppressMessages(tryCatch(
        shinymanager::read_db_decrypt(
          conn = path,
          name = name,
          passphrase = passphrase
        ),
        error = function(condition) NULL
      )))
    }),
    table_names
  )
  credentials <- tables$credentials
  pwd_mngt <- tables$pwd_mngt
  required <- list(
    credentials = c(
      "user",
      "password",
      "start",
      "expire",
      "admin",
      "is_hashed_password"
    ),
    pwd_mngt = c(
      "user",
      "must_change",
      "have_changed",
      "date_change",
      "n_wrong_pwd"
    ),
    logs = c("user", "server_connected", "token", "logout", "app")
  )
  valid_tables <- all(vapply(
    table_names,
    function(name) {
      table <- tables[[name]]
      is.data.frame(table) && all(required[[name]] %in% names(table))
    },
    logical(1)
  ))
  valid <- valid_tables && nrow(credentials) > 0L
  if (valid) {
    credential_users <- credentials$user
    password_rows <- pwd_mngt$user
    wrong_passwords <- pwd_mngt$n_wrong_pwd
    change_dates <- pwd_mngt$date_change
    valid_change_dates <- is.character(change_dates) &&
      {
        populated_dates <- !is.na(change_dates) & nzchar(change_dates)
        all(!is.na(suppressWarnings(as.Date(change_dates[populated_dates]))))
      }
    valid <- is.character(credential_users) &&
      !anyNA(credential_users) &&
      all(nzchar(credential_users)) &&
      !anyDuplicated(credential_users) &&
      is.character(credentials$password) &&
      !anyNA(credentials$password) &&
      all(nzchar(credentials$password)) &&
      is.logical(credentials$is_hashed_password) &&
      all(credentials$is_hashed_password %in% TRUE) &&
      is.character(password_rows) &&
      !anyNA(password_rows) &&
      all(nzchar(password_rows)) &&
      !anyDuplicated(password_rows) &&
      setequal(password_rows, credential_users) &&
      length(password_rows) == length(credential_users) &&
      is.character(pwd_mngt$must_change) &&
      all(pwd_mngt$must_change %in% c("TRUE", "FALSE")) &&
      is.character(pwd_mngt$have_changed) &&
      all(pwd_mngt$have_changed %in% c("TRUE", "FALSE")) &&
      valid_change_dates &&
      is.numeric(wrong_passwords) &&
      !is.object(wrong_passwords) &&
      all(is.finite(wrong_passwords)) &&
      all(wrong_passwords >= 0) &&
      all(wrong_passwords == floor(wrong_passwords))
  }
  if (!valid) {
    stop(
      paste0(
        "auth$credentials is not a complete compatible shinymanager ",
        "database, or its passphrase does not match."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.viewerAuthScalarString <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

.compileViewerAuth <- function(auth) {
  if (is.null(auth)) {
    return(NULL)
  }

  auth_names <- names(auth)
  required <- c("credentials", "passphrase_env")
  optional <- "timeout_minutes"
  valid_shape <- is.list(auth) &&
    !is.data.frame(auth) &&
    !is.null(auth_names) &&
    length(auth_names) == length(auth) &&
    !anyNA(auth_names) &&
    all(nzchar(auth_names)) &&
    !anyDuplicated(auth_names) &&
    all(required %in% auth_names) &&
    all(auth_names %in% c(required, optional))
  if (!valid_shape) {
    stop(
      paste0(
        "auth must be a named list containing credentials, ",
        "passphrase_env, and optional timeout_minutes."
      ),
      call. = FALSE
    )
  }

  credentials <- auth$credentials
  regular <- .viewerAuthScalarString(credentials) &&
    isTRUE(file.exists(credentials)) &&
    isTRUE(utils::file_test("-f", credentials)) &&
    isTRUE(file.access(credentials, mode = 4L) == 0L)
  if (!regular) {
    stop(
      "auth$credentials must be one readable SQLite file.",
      call. = FALSE
    )
  }
  credentials <- tryCatch(
    normalizePath(credentials, winslash = "/", mustWork = TRUE),
    error = function(condition) NULL
  )
  if (is.null(credentials)) {
    stop(
      "auth$credentials must be one readable SQLite file.",
      call. = FALSE
    )
  }

  passphrase_env <- auth$passphrase_env
  if (
    !.viewerAuthScalarString(passphrase_env) ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", passphrase_env)
  ) {
    stop(
      "auth$passphrase_env must be a valid environment variable name.",
      call. = FALSE
    )
  }
  passphrase <- Sys.getenv(passphrase_env, unset = NA_character_)
  on.exit(passphrase <- NULL, add = TRUE)
  if (
    length(passphrase) != 1L ||
      is.na(passphrase) ||
      !nzchar(passphrase)
  ) {
    stop(passphrase_env, " is not set.", call. = FALSE)
  }
  if (nchar(passphrase, type = "bytes") < 16L) {
    stop(
      passphrase_env,
      " must contain at least 16 characters.",
      call. = FALSE
    )
  }

  timeout_minutes <- if ("timeout_minutes" %in% auth_names) {
    auth$timeout_minutes
  } else {
    15
  }
  valid_timeout <- is.numeric(timeout_minutes) &&
    !is.logical(timeout_minutes) &&
    length(timeout_minutes) == 1L &&
    !is.na(timeout_minutes) &&
    is.finite(timeout_minutes) &&
    timeout_minutes == floor(timeout_minutes) &&
    timeout_minutes >= 1 &&
    timeout_minutes <= 1440
  if (!valid_timeout) {
    stop(
      "auth$timeout_minutes must be one whole number from 1 through 1440.",
      call. = FALSE
    )
  }

  .viewerAuthRequireProvider()
  .viewerAuthValidateDatabase(credentials, passphrase)

  list(
    credentials_path = "private-data/auth/credentials.sqlite",
    passphrase_env = passphrase_env,
    timeout_minutes = as.integer(timeout_minutes),
    source = credentials
  )
}
