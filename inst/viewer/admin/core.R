viewer_admin_scalar <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

viewer_admin_config <- function(options = list(), env = Sys.getenv) {
  if (!is.list(options)) {
    return(NULL)
  }
  config <- options[[".share_admin"]]
  if (is.null(config)) {
    return(NULL)
  }
  valid <- is.list(config) &&
    identical(sort(names(config)), c("account", "password_env")) &&
    viewer_admin_scalar(config$account) &&
    viewer_admin_scalar(config$password_env) &&
    grepl("^[A-Za-z_][A-Za-z0-9_]*$", config$password_env)
  if (!valid) {
    return(NULL)
  }
  password <- env(config$password_env, unset = NA_character_)
  if (
    !viewer_admin_scalar(password) ||
      nchar(password, type = "bytes") < 12L
  ) {
    return(NULL)
  }
  list(account = config$account, password = password)
}

viewer_admin_login <- function(user, password, config) {
  is.list(config) &&
    viewer_admin_scalar(user) &&
    viewer_admin_scalar(password) &&
    identical(user, config$account) &&
    identical(password, config$password)
}

VIEWER_ADMIN_LOGIN_LIMIT <- 5L
VIEWER_ADMIN_LOGIN_WINDOW_SECONDS <- 15L * 60L
VIEWER_ADMIN_LOGIN_KEYS <- 256L

## ponytail: process-wide throttling covers one Viewer worker; deployments with
## multiple workers should also rate-limit /admin at the reverse proxy.
.viewer_admin_login_attempts <- new.env(parent = emptyenv())

viewer_admin_login_cleanup <- function(now = Sys.time()) {
  cutoff <- as.numeric(now) - VIEWER_ADMIN_LOGIN_WINDOW_SECONDS
  keys <- ls(.viewer_admin_login_attempts, all.names = TRUE)
  for (key in keys) {
    attempts <- get(
      key,
      envir = .viewer_admin_login_attempts,
      inherits = FALSE
    )
    attempts <- attempts[attempts >= cutoff]
    if (length(attempts)) {
      assign(key, attempts, envir = .viewer_admin_login_attempts)
    } else {
      rm(list = key, envir = .viewer_admin_login_attempts)
    }
  }
  invisible(NULL)
}

viewer_admin_login_allowed <- function(key, now = Sys.time()) {
  viewer_admin_login_cleanup(now)
  attempts <- get0(
    key,
    envir = .viewer_admin_login_attempts,
    inherits = FALSE,
    ifnotfound = numeric()
  )
  length(attempts) < VIEWER_ADMIN_LOGIN_LIMIT
}

viewer_admin_login_failed <- function(key, now = Sys.time()) {
  viewer_admin_login_cleanup(now)
  attempts <- get0(
    key,
    envir = .viewer_admin_login_attempts,
    inherits = FALSE,
    ifnotfound = numeric()
  )
  assign(
    key,
    c(attempts, as.numeric(now)),
    envir = .viewer_admin_login_attempts
  )
  keys <- ls(.viewer_admin_login_attempts, all.names = TRUE)
  if (length(keys) > VIEWER_ADMIN_LOGIN_KEYS) {
    newest <- vapply(
      keys,
      function(item) max(get(item, envir = .viewer_admin_login_attempts)),
      numeric(1)
    )
    remove <- keys[order(newest)][
      seq_len(length(keys) - VIEWER_ADMIN_LOGIN_KEYS)
    ]
    rm(list = remove, envir = .viewer_admin_login_attempts)
  }
  invisible(NULL)
}

viewer_admin_login_succeeded <- function(key) {
  if (exists(key, envir = .viewer_admin_login_attempts, inherits = FALSE)) {
    rm(list = key, envir = .viewer_admin_login_attempts)
  }
  invisible(NULL)
}

viewer_admin_login_key <- function(session, account) {
  address <- tryCatch(session$request$REMOTE_ADDR, error = function(error) NULL)
  if (!viewer_admin_scalar(address)) {
    address <- "unknown"
  }
  paste(account, address, sep = "@")
}

viewer_admin_share_ttl <- function(options = list(), env = Sys.getenv) {
  if (is.null(viewer_admin_config(options, env = env))) {
    7L * 24L * 60L * 60L
  } else {
    90L * 24L * 60L * 60L
  }
}

viewer_admin_route <- function(path) {
  viewer_admin_scalar(path) &&
    grepl("/admin/?$", path) &&
    !grepl("/admin/.+", path)
}

viewer_admin_http_app <- function(app) {
  handler <- app$httpHandler
  app$httpHandler <- function(request) {
    if (
      identical(request$REQUEST_METHOD, "GET") &&
        viewer_admin_route(request$PATH_INFO)
    ) {
      request$PATH_INFO <- "/"
    }
    handler(request)
  }
  app
}

viewer_auth_context <- function(session) {
  closed <- list(authenticated = FALSE, user = NULL, is_admin = FALSE)
  if (is.null(session) || is.null(session$userData)) {
    return(closed)
  }
  context <- tryCatch(
    get("viewer_auth", envir = session$userData, inherits = FALSE),
    error = function(error) NULL
  )
  if (!is.list(context)) {
    return(closed)
  }
  user <- context$user
  authenticated <- isTRUE(context$authenticated) && viewer_admin_scalar(user)
  list(
    authenticated = authenticated,
    user = if (authenticated) user else NULL,
    is_admin = authenticated && isTRUE(context$is_admin)
  )
}

viewer_is_admin <- function(session) {
  isTRUE(viewer_auth_context(session)$is_admin)
}

viewer_admin_records <- function(rows, now = Sys.time()) {
  expected <- c(
    "token",
    "fingerprint",
    "dataset_label",
    "creator",
    "created_at",
    "expires_at",
    "revoked_at"
  )
  if (!is.data.frame(rows) || !all(expected %in% names(rows)) || !nrow(rows)) {
    return(list())
  }
  lapply(seq_len(nrow(rows)), function(index) {
    value <- function(name) {
      item <- rows[[name]][[index]]
      if (length(item) != 1L || is.na(item)) "" else as.character(item)
    }
    list(
      token = value("token"),
      fingerprint = value("fingerprint"),
      dataset_label = value("dataset_label"),
      creator = value("creator"),
      created_at = value("created_at"),
      expires_at = value("expires_at"),
      revoked_at = value("revoked_at"),
      status = if (nzchar(value("revoked_at"))) {
        "revoked"
      } else if (
        as.POSIXct(
          value("expires_at"),
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ) <=
          as.POSIXct(now, tz = "UTC")
      ) {
        "expired"
      } else {
        "active"
      }
    )
  })
}
